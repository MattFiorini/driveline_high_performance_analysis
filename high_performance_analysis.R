library(ggcorrplot)
library(tidyverse)
library(mice)

rawPerformanceData = read.csv("https://raw.githubusercontent.com/drivelineresearch/openbiomechanics/main/high_performance/data/hp_obp.csv", header = TRUE, sep = ",")

str(rawPerformanceData)

#splititng up data into 3 df's: 1 for hitter only, 1 for pitcher only and 1 for 2-way players

rawPerformanceData = rawPerformanceData %>% select(-test_date, -pitching_session_date, 
                                                -hitting_session_date,-playing_level, 
                                                -bat_speed_mph_group, -pitch_speed_mph_group)

duplicate_athletes = duplicated(rawPerformanceData$athlete_uid)

sum(duplicate_athletes)

#772 duplicates were identified. Will be isolated and the averages of them will be identified and
# consolidated together to avoid excess weight being placed on those athletes

all_duplicate_data = rawPerformanceData[rawPerformanceData$athlete_uid %in% rawPerformanceData$athlete_uid[duplicated(rawPerformanceData$athlete_uid)], ]

all_duplicate_data = all_duplicate_data %>%
  group_by(athlete_uid) %>%
  summarize(across(everything(), ~ mean(.x, na.rm = TRUE)))

# 448 ID's consolidated to their means, will re-add them back to the raw performance data, then
# split by position groups

duplicated_ids = rawPerformanceData$athlete_uid[duplicated(rawPerformanceData$athlete_uid)]

cleanPerformanceData =  rawPerformanceData[!rawPerformanceData$athlete_uid %in% duplicated_ids, ]

cleanPerformanceData = rbind(cleanPerformanceData, all_duplicate_data)

# re-confirming that the duplicates have been removed from the data set. NA's were left in the data set,
# they were removed during the mean calculation to avoid excessive manipulation
sum(duplicated(cleanPerformanceData$athlete_uid))

sapply(cleanPerformanceData, function(x) sum(is.na(x)))

#Need to remove NA's specifically for data points where athlete weight is null, Also understand if
# nulls on Right is consistent with values being present on left side (only tracking dominant side)

cleanPerformanceData %>% filter(is.na(ShoulderERL)) %>% summarise(ShouldERR_blank = sum(is.na(ShoulderERR)))
#indicates that 23 values are present where the right is known and left is null, while 280 remain null

#run line to identify what rows of data have significant NA values
na_athletes = cleanPerformanceData %>%
  rowwise() %>%
  mutate(na_count_row = sum(is.na(c_across(where(is.numeric))))) %>% 
  ungroup() %>% 
  group_by(athlete_uid) %>%
  mutate(na_count = sum(na_count_row)) %>%
  arrange(desc(na_count))

#visualize the distribution of the NA's
ggplot(na_athletes, aes(na_count)) + geom_histogram()

#distribution shows leftward leaning amount of null values, will confirm what the distribution
# of velocity is for IDs with greater than 20 NA's compared to the overall distribution to better
# determine potential impact that removing them will have on the end results

ggplot(na_athletes %>% filter(na_count > 20), aes(pitch_speed_mph)) + geom_histogram()
ggplot(na_athletes %>% filter(na_count > 20), aes(bat_speed_mph)) + geom_histogram()
ggplot(na_athletes, aes(pitch_speed_mph)) + geom_histogram()
ggplot(na_athletes, aes(bat_speed_mph)) + geom_histogram()

#determined that the distribution of the null values aligns with the rest of the data, will
# remove the rows that have 20 or more NA's present. This based off a large proportion of the
# data missing, potentially resulting in a negative representation of the larger data set findings
# if ML applied to missing variables

na_athletes = na_athletes %>% filter(na_count > 20) %>% select(athlete_uid)

cleanPerformanceData = cleanPerformanceData %>% anti_join(na_athletes, by = "athlete_uid")

sapply(cleanPerformanceData, function(x) sum(is.na(x)))

#will understand the difference between the NA values peak takeoff to see if there is a large
# difference in the distribution of velocity in both sets

cleanPerformanceData %>% filter(is.na(peak_takeoff_force_.n._mean_pp)) %>% 
  summarise(pitch_mean = mean(pitch_speed_mph, na.rm = T), pitch_median = median(pitch_speed_mph, na.rm = T), 
            pitch_std = sd(pitch_speed_mph, na.rm = T), bat_mean = mean(bat_speed_mph, na.rm = T), 
            bat_median = median(bat_speed_mph, na.rm = T), bat_std = sd(bat_speed_mph, na.rm = T))

cleanPerformanceData %>% filter(!is.na(peak_takeoff_force_.n._mean_pp)) %>% 
  summarise(pitch_mean = mean(pitch_speed_mph, na.rm = T), pitch_median = median(pitch_speed_mph, na.rm = T), 
            pitch_std = sd(pitch_speed_mph, na.rm = T), bat_mean = mean(bat_speed_mph, na.rm = T), 
            bat_median = median(bat_speed_mph, na.rm = T), bat_std = sd(bat_speed_mph, na.rm = T))

# the missing peak takeoff values indicate a larger dispersion with smaller values of data based on
# a larger standard deviation and lower means and medians for pitching data. Bat speed mirrors
# existing data with peak value known. Based on the lack of large difference between the sets with
# the values known vs unknown, the decision will be made to remove the variables from the data set.
# The remaining missing values will be filled using mice

cleanPerformanceData = cleanPerformanceData %>% select(-peak_takeoff_force_.n._mean_pp, 
                                                       -peak_eccentric_force_.n._mean_pp,
                                                       -peak_takeoff_force_asymmetry_.._l.r._mean_pp,
                                                       -peak_eccentric_force_asymmetry_.._l.r._mean_pp)

#isolate values that need to be imputed, will remove bat and pitch speed related variables

imputedData = cleanPerformanceData %>% select(-bat_speed_mph, -hitting_max_hss, -pitch_speed_mph,
                                              -pitching_max_hss)

#given the left leaning emphasis of the data, will use the default method for mice, pmm

imputedData = mice(data = imputedData,
                   seed = 2016,
                   method = "pmm",
                   m = 5,
                   maxit = 5)

imputedData = mice::complete(imputedData,1)

#left join the data back together to include imputed values

cleanPerformanceData = imputedData %>%
  left_join(cleanPerformanceData %>% select(athlete_uid, bat_speed_mph, hitting_max_hss,
                                            pitching_max_hss, pitch_speed_mph),
    by = "athlete_uid"
  )

names(cleanPerformanceData)

sapply(cleanPerformanceData, function(x) sum(is.na(x)))      
       
#adding column into the data set to identify if player is a PO, Hitter or 2-way
cleanPerformanceData = cleanPerformanceData %>%
  mutate(
    Classifier = case_when(
      is.na(pitch_speed_mph) & !is.na(bat_speed_mph) ~ "Hitter",
      !is.na(pitch_speed_mph) & is.na(bat_speed_mph) ~ "Pitcher",
      !is.na(pitch_speed_mph) & !is.na(bat_speed_mph) ~ "Two Way",
      TRUE ~ "Unknown" # Optional, handles cases where both are NA
    )
  )
cleanPerformanceData$Classifier = as.factor(cleanPerformanceData$Classifier)

### EDA to now understand the data based on each

numeric_df = cleanPerformanceData %>% select(where(is.numeric))

correlation_matrix = cor(numeric_df, use = "complete.obs")

ggcorrplot(correlation_matrix, method = "circle")
#far too noisy of a correlation matrix, will filter out correlations that are not significant,
#specifically the middle quartiles of correlation (-0.49 to 0.49)

cor_df = as.data.frame(as.table(correlation_matrix))

filtered_cor_df = cor_df %>%
  filter(!(Freq > -0.49 & Freq < 0.49))

# Plot the filtered correlation matrix using ggplot
ggplot(data = filtered_cor_df, aes(Var1, Var2, fill = Freq)) +
  geom_tile() +
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0) +
  theme_minimal() +
  labs(title = "Filtered Correlation Matrix", fill = "Correlation")

#drilling down into specific view for pitch and swing speed
pitch_speed_cor = correlation_matrix["pitch_speed_mph",]
bat_speed_cor = correlation_matrix["bat_speed_mph",]

sort(pitch_speed_cor, decreasing = TRUE)
sort(bat_speed_cor, decreasing = TRUE)
       

# initial observations from the data
# - appears t-spine has limited direct correlation to swing and throwing speed
# - counter movement depth has somewhat of an inverted relationship between swing speed, stronger
# with throwing speed, this in theory applies based on an athletes ability to quickly develop force with
# need to load
# - shoulder mobility is crtiically important for throwers, if an athlete demonstrates an ability
# to have excess mobility (large amount of both internal and external) that yields incredibly well
# for throwing speed
# - force production is critically important for both hitters and pitchers; the ability to produce
# power is critical for an increase in speed
# - body weight seemed to be more important for hitters compared to pitchers. While the correlation
# is still present, the relationship is stronger for swing speed compared to pitch speed
# - although body weight seems to be an important variable, interestingly relative strength has
# a more neutral relationship with both variables, specifically with swing speed. Raises the
# question of understanding the performance threshold for when gains in a certain variable begin to
# have mitigating returns

#to better understand how this idea of strength changes with each speed group, groups will be
# segmented off into different groups based on pitch and swing speed to better understand how the
# averages for these variables change group to group, and where are minimal increases in velocity being
# observed

#clean up environment
rm(var, pitch_speed_cor, duplicate_athletes, duplicated_ids, bat_speed_cor, rawPerformanceData, na_athletes, multipleLinearModel,
   imputedData, df, correlation_matrix, all_duplicate_data)

#running k-means clustering, will separate a pitching and hitting only data frame
# given the 2-way players, there will be some overlap but the data should still define the relationships 


pitching_data = numeric_df %>% select(-bat_speed_mph, -hitting_max_hss, -pitching_max_hss) %>% filter(!is.na(pitch_speed_mph))
hitting_data = numeric_df %>% select(-pitch_speed_mph,-pitching_max_hss, -hitting_max_hss) %>% filter(!is.na(bat_speed_mph))
# removing pitching and hitting max hss from each to keep variables consistent b/w both data sets

names(pitching_data)
pitching_data = pitching_data[,c(39,1:38)]
names(pitching_data)[1] <- "y"

names(hitting_data)
hitting_data = hitting_data[,c(39,1:38)]
names(hitting_data)[1] <- "y"

#initial clustering
set.seed(123)  # set this to replicate results

velo_df = data.frame() #accumulator for velocity results
velo_df
for(k in 1:20){
  
  # pitching set
  kmeans_pitch = kmeans(x=pitching_data[,2:ncol(pitching_data)], centers=k, nstart=25, iter.max=100)
  
  # hitting set
  kmeans_hit = kmeans(x=hitting_data[,2:ncol(hitting_data)], centers=k, nstart=25, iter.max=100)
  
  # combine cluster number and cost together, write to velo_df
  velo_df = rbind(velo_df, cbind(k, kmeans_pitch$tot.withinss
                                  , kmeans_hit$tot.withinss))
}

names(velo_df) = c("cluster","pitch","hit")
velo_df
par(mfrow=c(1,1))
plot(x=velo_df$cluster, y=velo_df$pitch, main="k-Means Elbow Plot"
     , col="blue", pch=19, type="b", cex.lab=1.2
     , xlab="Number of Clusters", ylab="SSE")
points(x=velo_df$cluster, y=velo_df$hit, col="red", pch=19)

#based on data in elbow plot, appears that cluster size of 5 appears to be the most appropriate

library(useful)
getwd()
setwd("C:\\Users\\Matt Fiorini\\OneDrive - purdue.edu\\R For Analytics - MGMT 59000\\Team Homework Assigments\\HW 3")
source('multiplot.R')
p1 = plot(kmeans_pitch_6, data=pitching_data)
p2 = plot(kmeans_hit_6, data=hitting_data)
multiplot(p1, p2)
rm(p1, p2)

kmeans_pitch_5 = kmeans(x=pitching_data[,2:ncol(pitching_data)], centers=5, nstart=25, iter.max=100)
kmeans_hit_5 = kmeans(x=hitting_data[,2:ncol(hitting_data)], centers=5, nstart=25, iter.max=100)
#clustering independant of velocity related variables to avoid data leakage into clusters

pitching_data$cluster = as.factor(kmeans_pitch_5$cluster)
hitting_data$cluster = as.factor(kmeans_hit_5$cluster)


ggplot(pitching_data %>%
  group_by(cluster) %>%
  summarise(mean_pitch_speed = mean(y)), aes(x = cluster, y = mean_pitch_speed, fill = as.factor(cluster))) +
  geom_bar(stat = "identity") +
  labs(
    title = "Mean Pitch Speed by Cluster",
    x = "Cluster",
    y = "Mean Pitch Speed (mph)",
    fill = "Cluster"
  ) +
  theme_minimal()

pitching_data %>%
  group_by(cluster) %>%
  summarise(mean_pitch_speed = mean(y))

#seeing larger difference between cluster 1 and 4; will observe the summary statistics for those
# clusters to identify the differences between the smallest and largest clusters


ggplot(hitting_data %>%
         group_by(cluster) %>%
         summarise(mean_swing_speed = mean(y)), aes(x = cluster, y = mean_swing_speed, fill = as.factor(cluster))) +
  geom_bar(stat = "identity") +
  labs(
    title = "Mean Swing Speed by Cluster",
    x = "Cluster",
    y = "Mean Swing Speed (mph)",
    fill = "Cluster"
  ) +
  theme_minimal()

hitting_data %>%
  group_by(cluster) %>%
  summarise(mean_swing_speed = mean(y))
# Seeing larger difference between cluster 2 and 5, will better understand the difference between
# each of the metrics in the cluster

#once metrics are observed, will run gradient boosting to simplify the number of variables,
# re-run clusters to understand if there is any simpler way to determine a proper relationship

pitching_data_clustercomp = pitching_data %>% filter(cluster %in% c("1", "4"))

summary_pitch_stats = pitching_data_clustercomp %>%
  group_by(cluster) %>%
  summarise(across(where(is.numeric), list(mean = ~mean(.x, na.rm = TRUE), 
                                           median = ~median(.x, na.rm = TRUE))))

hitting_data_clustercomp = hitting_data %>% filter(cluster %in% c("2", "5"))

summary_hit_stats = hitting_data_clustercomp %>%
  group_by(cluster) %>%
  summarise(across(where(is.numeric), list(mean = ~mean(.x, na.rm = TRUE), 
                                           median = ~median(.x, na.rm = TRUE))))
# are seeing some noticable differences between the cluster across the board, however the large
# difference may be more observable based on lower velocity athletes being included in the data set

# will filter to only have the upper quartile of velocity, and re-engage in clustering

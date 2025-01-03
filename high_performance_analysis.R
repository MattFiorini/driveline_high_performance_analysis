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

cleanPerformanceData = cleanPerformanceData[,c(1:38,40:44,39)]

names(cleanPerformanceData)

### EDA to now understand the data based on each

numeric_df = cleanPerformanceData %>% select(where(is.numeric))

for (col in names(numeric_df)) {
  # Create a histogram for the numeric column
  hist(numeric_df[[col]], 
       main = paste("Histogram of", col),  # Title with column name
       xlab = col,                         # X-axis label with column name
       col = "lightblue",                  # Color of the bars
       border = "black")                   # Border color of the bars
  
  # Pause to allow viewing each plot before moving to the next
  readline(prompt = "Press [Enter] to see the next histogram...")
}

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

#include segmentation based on pitch and swing speed, group together into upper, mid and low
# level swing speed based on the data

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

kmeans_pitch_5 = kmeans(x=pitching_data[,2:ncol(pitching_data)], centers=5, nstart=25, iter.max=100)
kmeans_hit_5 = kmeans(x=hitting_data[,2:ncol(hitting_data)], centers=5, nstart=25, iter.max=100)
#clustering independant of velocity related variables to avoid data leakage into clusters

library(useful)
getwd()
setwd("C:\\Users\\Matt Fiorini\\OneDrive - purdue.edu\\R For Analytics - MGMT 59000\\Team Homework Assigments\\HW 3")
source('multiplot.R')
p1 = plot(kmeans_pitch_5, data=pitching_data)
p2 = plot(kmeans_hit_5, data=hitting_data)
multiplot(p1, p2)
rm(p1, p2)

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

#seeing larger difference between cluster 2 and 5; will observe the summary statistics for those
# clusters to identify the differences between the smallest and largest clusters

t.test(y ~ cluster, data = pitching_data, subset = cluster %in% c(2, 5))

#t-test confirms that we can reject the null hypothesis, and accept the alternative, which is the that
# there is a significant statistical difference between throwing speed for the clusters with the
# biggest difference

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
# Seeing larger difference between cluster 4 and 5, will better understand the difference between
# each of the metrics in the cluster

t.test(y ~ cluster, data = hitting_data, subset = cluster %in% c(4, 5))

# t-test identifies a difference statistical difference between both clusters

# are seeing some noticable differences between the cluster across the board, however the large
# difference may be more observable based on lower velocity athletes being included in the data set

# will filter to only have the upper 30% of velocity, and re-engage in clustering

upper_pitching_data = pitching_data %>% filter(y >= quantile(pitching_data$y, 0.7)) %>% select(-cluster)
upper_hitting_data = hitting_data %>% filter(y >= quantile(hitting_data$y, 0.7)) %>% select(-cluster)

velo_df = data.frame() #accumulator for velocity results
velo_df
for(k in 1:20){
  
  # pitching set
  kmeans_pitch = kmeans(x=upper_pitching_data[,2:ncol(upper_pitching_data)], centers=k, nstart=25, iter.max=100)
  
  # hitting set
  kmeans_hit = kmeans(x=upper_hitting_data[,2:ncol(upper_hitting_data)], centers=k, nstart=25, iter.max=100)
  
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

#hitting with have 2 clusters, while throwing will have 4 clusters, given the elbow starting to break
# at approximately those points

kmeans_pitch_4 = kmeans(x=upper_pitching_data[,2:ncol(upper_pitching_data)], centers=4, nstart=25, iter.max=100)
kmeans_hit_2 = kmeans(x=upper_hitting_data[,2:ncol(upper_hitting_data)], centers=2, nstart=25, iter.max=100)


upper_pitching_data$cluster = as.factor(kmeans_pitch_4$cluster)
upper_hitting_data$cluster = as.factor(kmeans_hit_2$cluster)

upper_pitching_data %>%
  group_by(cluster) %>%
  summarise(mean_pitch_speed = mean(y), 
            median_pitch = median(y),
            sd_pitch = sd(y, na.rm = T),
            min_pitch = min(y),
            max_pitch = max(y),
            no_of_pitchers = n())

# 2 and 3 cluster have the largest difference, but still fairly small. Will test using t-test to
# identify the difference

t.test(y ~ cluster, data = upper_pitching_data, subset = cluster %in% c(1, 3))

#given p-value of 0.3644, we cannot reject the null. We will strictly observe the full data set

upper_hitting_data %>%
  group_by(cluster) %>%
  summarise(mean_swing_speed = mean(y), 
            median_swing = median(y),
            sd_swing = sd(y, na.rm = T),
            min_swing = min(y),
            max_swing = max(y),
            no_of_hitters = n())

t.test(y ~ cluster, data = upper_hitting_data, subset = cluster %in% c(1, 2))

# the t-test for the upper hitting data confirms that there is not a distinguishable difference
# between and a failure to reject the null. This confirms the consensus that cluster observations
# can only be made

rm(upper_hitting_data, upper_pitching_data, kmeans_pitch_4, kmeans_hit_2, velo_df, kmeans_hit, kmeans_pitch)
#removing to clear room

#Thorwing data
summary_stats = pitching_data %>% 
  filter(cluster %in% c("2", "5")) %>%
  group_by(cluster) %>%
  summarise(across(where(is.numeric), list(mean = ~mean(.x, na.rm = TRUE))))
print(summary_stats)

percent_difference_2_5 = summary_stats %>% 
  filter(cluster %in% c("2", "5")) %>%
  summarise(across(where(is.numeric), 
                   ~ (.[cluster == "2"] - .[cluster == "5"]) / .[cluster == "5"] * 100)) %>%
  mutate(across(everything(), abs)) %>%
  pivot_longer(cols = everything(), names_to = "variable", values_to = "value") %>%
  arrange(desc(value)) %>%
  slice_head(n=10)

ggplot(as.data.frame(percent_difference_2_5), aes(x = reorder(variable, value), y = value)) +
  geom_bar(stat = "identity", fill = "steelblue") + 
  coord_flip() +  # Flip coordinates for better readability
  labs(
    title = "Histogram of Pitching Variable Values",
    x = "Variable",
    y = "Value"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#hitting data
summary_stats_hit = hitting_data %>% 
  filter(cluster %in% c("4", "5")) %>%
  group_by(cluster) %>%
  summarise(across(where(is.numeric), list(mean = ~mean(.x, na.rm = TRUE))))
print(summary_stats_hit)

percent_difference_4_5 = summary_stats_hit %>% 
  filter(cluster %in% c("4", "5")) %>%
  summarise(across(where(is.numeric), 
                   ~ (.[cluster == "5"] - .[cluster == "4"]) / .[cluster == "4"] * 100)) %>%
  mutate(across(everything(), abs)) %>%
  pivot_longer(cols = everything(), names_to = "variable", values_to = "value") %>%
  arrange(desc(value)) %>%
  slice_head(n=10)

ggplot(as.data.frame(percent_difference_4_5), aes(x = reorder(variable, value), y = value)) +
  geom_bar(stat = "identity", fill = "steelblue") + 
  coord_flip() +  # Flip coordinates for better readability
  labs(
    title = "Histogram of Hitting Variable Values",
    x = "Variable",
    y = "Value"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# as a final means of validating our hypothesis that the separation of elite rotational athletes
# is the separation of body weight, relative strength, and potential asymetrical imbalances being present,
# and the ability to limit contact time during explosive movements, we'll compare athletes with velocity
# between 85-89 mph, and athletes velocity greater than 90 mph

pitch_85_to_90 = pitching_data %>% filter(y >= 85 & y < 90)
pitch_90_plus = pitching_data %>% filter(y >= 90)
combined_data = bind_rows(pitch_85_to_90, pitch_90_plus)

# separation by velocity does confirm that higher velocity is associated with individuals who have
# smaller contact time,greater amount of asymetry, produce greater power. Body weight did increase
# for elite throwers (90+) by a mean of 15 pounds, which is influenced by larger outliers, as the median
# reduced to 10 pounds of difference. 
# relative strength numbers were similar, with the 85-90 group exhibiting greater relative strength.

combined_data = combined_data %>% mutate(speed_label = ifelse(y > 90, "fast", "slow"))

#re-confirm that there is statistical significance between both velocity in both sets

t.test(y ~ speed_label, data = combined_data)

#given a p-value of less than 0.05, this allows us to recognize the statistical significance
# between the fast and slow group

#will create summary data to calculate the means and find the relative difference

summary_combined_data = combined_data %>% group_by(speed_label) %>%
  summarise(across(where(is.numeric), list(mean = ~mean(.x, na.rm = TRUE))))

percent_difference = summary_combined_data %>%
  summarise(across(where(is.numeric), 
                   ~ (.[speed_label == "fast"] - .[speed_label == "slow"]) / .[speed_label == "slow"] * 100)) %>%
  mutate(across(everything(), abs)) %>%
  pivot_longer(cols = everything(), names_to = "variable", values_to = "value") %>%
  arrange(desc(value)) %>%
  slice_head(n = 6)

ggplot(as.data.frame(percent_difference), aes(x = reorder(variable, value), y = value)) +
  geom_bar(stat = "identity", fill = "steelblue") + 
  coord_flip() +  # Flip coordinates for better readability
  labs(
    title = "Histogram of Throwing Variable Values",
    x = "Variable",
    y = "Value"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


common_throwing_variables = inner_join(percent_difference, percent_difference_2_5, by = "variable")
print(common_throwing_variables)
# the major common variables that are both present when comparing different groups are:
# cmj_stiffness_asymmetry_.._l.r._mean_cmj_mean
# p2_concentric_impulse_asymmetry_.._l.r._mean_cmj_mean
# p1_concentric_impulse_asymmetry_.._l.r._mean_sj_mean

# same comparison to be made with swing speed, will observe swing speed greater than 70 mph
# and between 65-70mph

swing_65_to_70 = hitting_data %>% filter(y >= 65 & y < 70)
swing_70_plus = hitting_data %>% filter(y >= 70)
combined_swing_data = bind_rows(swing_65_to_70, swing_70_plus)

combined_swing_data = combined_swing_data %>% mutate(speed_label = ifelse(y > 70, "fast", "slow"))

#re-confirm that there is statistical significance between both velocity in both sets

t.test(y ~ speed_label, data = combined_swing_data)

#given a p-value of less than 0.05, this allows us to recognize the statistical significance
# between the fast and slow group


summary_swing_combined_data = combined_swing_data %>% group_by(speed_label) %>%
  summarise(across(where(is.numeric), list(mean = ~mean(.x, na.rm = TRUE))))

percent_swing_difference = summary_swing_combined_data %>%
  summarise(across(where(is.numeric), 
                   ~ (.[speed_label == "fast"] - .[speed_label == "slow"]) / .[speed_label == "slow"] * 100)) %>%
  mutate(across(everything(), abs)) %>%
  pivot_longer(cols = everything(), names_to = "variable", values_to = "value") %>%
  arrange(desc(value)) %>%
  slice_head(n = 5)

ggplot(as.data.frame(percent_swing_difference), aes(x = reorder(variable, value), y = value)) +
  geom_bar(stat = "identity", fill = "steelblue") + 
  coord_flip() +  # Flip coordinates for better readability
  labs(
    title = "Histogram of Hitting Variable Values",
    x = "Variable",
    y = "Value"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

common_swing_variables = inner_join(percent_swing_difference, percent_difference_4_5, by = "variable")
print(common_swing_variables)

# common variables are not as large as with pitching, but there are still common variables, with the
# 3 most common being:
# p2_concentric_impulse_asymmetry_.._l.r._mean_cmj_mean
# eccentric_deceleration_impulse_.asymmetry._.._l.r._mean_cmj_mean
# p2_concentric_impulse_asymmetry_.._l.r._mean_sj_mean       

  
# Comparing similar variables both present in throwing and pitching differences between fast and slow rotational athletes
inner_join(common_swing_variables, common_throwing_variables, by = "variable")
# lone common variable is: p2_concentric_impulse_asymmetry_.._l.r._mean_cmj_mean

# this highlights the large imbalance that occurs with elite level athletes, and how the asymetrical
# can become more noticeable with elite rotational athletes. This is undertandable given the volume
# of reps that occur on one side compared to another.

# to identify variables that have a linear relationship with velocity and swing speed, we'll re-examine
# the correlation matrix that was initially computed

pitching_data = pitching_data %>% select(where(is.numeric))
hitting_data = hitting_data %>% select(where(is.numeric))

correlation_matrix_throw = cor(pitching_data, use = "complete.obs")
correlation_matrix_hit = cor(hitting_data, use = "complete.obs")

pitch_speed_cor = correlation_matrix_throw["y",]
bat_speed_cor = correlation_matrix_hit["y",]

sort(pitch_speed_cor, decreasing = TRUE)
sort(bat_speed_cor, decreasing = TRUE)

# as force production increases (peak power), velocity has a linear relationship. 

# final step of piecing together different performance attributes will be to model the data
# and identify the accuracy of predicting athletes swing and throwing speed based on the variables
# identified. This will be done using Recursive Feature Elimination

library(gbm)
library(xgboost)
library(caret)

#Feature Engineering to remove redundant variables and simplify 

# Highly Correlated Values

# Pitching

descCorThrow = cor(pitching_data[,2:ncol(pitching_data)])
highlyCorDescrThrow = findCorrelation(descCorThrow, cutoff = 0.8)
filteredDescThrow = pitching_data[,2:ncol(pitching_data)][,-highlyCorDescrThrow]
pitching_data = cbind(pitching_data$y, filteredDescThrow)
names(pitching_data)[1] = "y"

# Hitting

descCorHit = cor(hitting_data[,2:ncol(hitting_data)])
highlyCorDescrHit = findCorrelation(descCorHit, cutoff = 0.8)
filteredDescHit = hitting_data[,2:ncol(hitting_data)][,-highlyCorDescrHit]
hitting_data = cbind(hitting_data$y, filteredDescHit)
names(hitting_data)[1] = "y"

#throwing data
sizes = c(10, 15, 20, 25)
control = rfeControl(functions = rfFuncs, method = "cv", number = 10)
rfe_results_throw = rfe(x = pitching_data[,2:ncol(pitching_data)], y = pitching_data$y, sizes = sizes, rfeControl = control)
plot(rfe_results_throw, type = c("g", "o"))  # Plots performance across different subset sizes
#plot indicating that the lowest RMSE is at the 15 variable mark for pitchers
varImp(rfe_results_throw)
selected_throw_features = predictors(rfe_results_throw)
d = pitching_data[, selected_throw_features]
pitching_data = cbind(pitching_data$y, d)
names(pitching_data)[1] = "y"

#hitting data
rfe_results_hit = rfe(x = hitting_data[,2:ncol(hitting_data)], y = hitting_data$y, sizes = sizes, rfeControl = control)
plot(rfe_results_hit, type = c("g", "o"))  # Plots performance across different subset sizes
#plot indicating that the lowest RMSE is at the 10 variable mark for hitters
varImp(rfe_results_hit)
selected_hit_features = predictors(rfe_results_hit)
d = hitting_data[, selected_hit_features]
hitting_data = cbind(hitting_data$y, d)
names(hitting_data)[1] = "y"
       
#splitting both data sets up to understand how impactful these selected variables will be to the 
# ability to accurately predict throwing and swing speed

#throwing data
set.seed(19990)
trainPitch = createDataPartition(y = pitching_data$y, p = 0.8, list = F)
trPitch = pitching_data[trainPitch,]
tePitch = pitching_data[-trainPitch,]


#swing data
trainHit = createDataPartition(y = hitting_data$y, p = 0.8, list = F)
trHit = hitting_data[trainHit,]
teHit = hitting_data[-trainHit,]

ctrl = trainControl(method="cv", number = 10, classProbs = F, 
                    summaryFunction = defaultSummary, allowParallel = T)

mPitch = train(y ~ ., data = trPitch, method = "gbm", trControl = ctrl, metric = "RMSE", verbose = F)
mHit = train(y ~ ., data = trHit, method = "gbm", trControl = ctrl, metric = "RMSE", verbose = F)

#mThrow train
defaultSummary(data=data.frame(obs=trPitch$y, pred=predict(mPitch, newdata=trPitch))
               , model=mPitch)
#      RMSE  Rsquared       MAE 
# 4.5752074 0.7820216 3.3756521
#mThrow test
defaultSummary(data=data.frame(obs=tePitch$y, pred=predict(mPitch, newdata=tePitch))
               , model=mPitch)
#      RMSE  Rsquared       MAE 
# 5.9935098 0.6105431 4.3459839
#mHit train
defaultSummary(data=data.frame(obs=trHit$y, pred=predict(mHit, newdata=trHit))
               , model=mHit)
#      RMSE  Rsquared       MAE 
# 3.7807547 0.7640313 2.8146670
#mHit test
defaultSummary(data=data.frame(obs=teHit$y, pred=predict(mHit, newdata=teHit))
               , model=mHit)
#      RMSE  Rsquared       MAE 
# 3.1987575 0.7942578 2.3771203

# both models have underperformed, will reasses and try without the RFE transformations and solely 
# use feature engineering, will reset the pitching and hitting data frames to how they were prior
# to RFE and re-test

pitching_data = cbind(pitching_data$y, filteredDescThrow)
names(pitching_data)[1] = "y"

hitting_data = cbind(hitting_data$y, filteredDescHit)
names(hitting_data)[1] = "y"

set.seed(2025)
trainPitch = createDataPartition(y = pitching_data$y, p = 0.8, list = F)
trPitch = pitching_data[trainPitch,]
tePitch = pitching_data[-trainPitch,]

#swing data
trainHit = createDataPartition(y = hitting_data$y, p = 0.8, list = F)
trHit = hitting_data[trainHit,]
teHit = hitting_data[-trainHit,]

ctrl = trainControl(method="cv", number = 10, classProbs = F, 
                    summaryFunction = defaultSummary, allowParallel = T)

mPitch = train(y ~ ., data = trPitch, method = "gbm", trControl = ctrl, metric = "RMSE", verbose = F)
mHit = train(y ~ ., data = trHit, method = "gbm", trControl = ctrl, metric = "RMSE", verbose = F)

#mThrow train
defaultSummary(data=data.frame(obs=trPitch$y, pred=predict(mPitch, newdata=trPitch))
               , model=mPitch)
# RMSE Rsquared      MAE 
# 5.173742 0.722946 3.647921 
#mThrow test
defaultSummary(data=data.frame(obs=tePitch$y, pred=predict(mPitch, newdata=tePitch))
               , model=mPitch)
# RMSE  Rsquared       MAE 
# 5.2819907 0.6964376 4.4520396
#mHit train
defaultSummary(data=data.frame(obs=trHit$y, pred=predict(mHit, newdata=trHit))
               , model=mHit)
# RMSE  Rsquared       MAE 
# 3.4172693 0.8034829 2.5838670
#mHit test
defaultSummary(data=data.frame(obs=teHit$y, pred=predict(mHit, newdata=teHit))
               , model=mHit)
# RMSE  Rsquared       MAE 
# 4.0158504 0.7148143 2.7848097

# will try one final time without additional feature engineering to maintain critical relationships

pitching_data = numeric_df %>% select(-bat_speed_mph, -hitting_max_hss, -pitching_max_hss) %>% filter(!is.na(pitch_speed_mph))
hitting_data = numeric_df %>% select(-pitch_speed_mph,-pitching_max_hss, -hitting_max_hss) %>% filter(!is.na(bat_speed_mph))

names(pitching_data)
pitching_data = pitching_data[,c(39,1:38)]
names(pitching_data)[1] <- "y"

names(hitting_data)
hitting_data = hitting_data[,c(39,1:38)]
names(hitting_data)[1] <- "y"

set.seed(2030)
trainPitch = createDataPartition(y = pitching_data$y, p = 0.8, list = F)
trPitch = pitching_data[trainPitch,]
tePitch = pitching_data[-trainPitch,]

#swing data
trainHit = createDataPartition(y = hitting_data$y, p = 0.8, list = F)
trHit = hitting_data[trainHit,]
teHit = hitting_data[-trainHit,]

ctrl = trainControl(method="cv", number = 10, classProbs = F, 
                    summaryFunction = defaultSummary, allowParallel = T)

mPitch = train(y ~ ., data = trPitch, method = "gbm", trControl = ctrl, metric = "RMSE", verbose = F)
mHit = train(y ~ ., data = trHit, method = "gbm", trControl = ctrl, metric = "RMSE", verbose = F)

#mThrow train
defaultSummary(data=data.frame(obs=trPitch$y, pred=predict(mPitch, newdata=trPitch))
               , model=mPitch)
# RMSE  Rsquared       MAE 
# 4.8733649 0.7570516 3.5015916 
#mThrow test
defaultSummary(data=data.frame(obs=tePitch$y, pred=predict(mPitch, newdata=tePitch))
               , model=mPitch)
# RMSE  Rsquared       MAE 
# 4.8230834 0.7284779 3.8685593
#mHit train
defaultSummary(data=data.frame(obs=trHit$y, pred=predict(mHit, newdata=trHit))
               , model=mHit)
# RMSE  Rsquared       MAE 
# 3.3394354 0.8012979 2.4829433
#mHit test
defaultSummary(data=data.frame(obs=teHit$y, pred=predict(mHit, newdata=teHit))
               , model=mHit)
#      RMSE  Rsquared       MAE 
# 4.3126817 0.7337956 3.0243840

# able to achieve the most effective model for pitching by maintaining all of the attributes with
# limited feature engineering, where as the hitting model was the most accurate when RFE and feature
# engineering were applied

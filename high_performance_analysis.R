library(ggcorrplot)
library(tidyverse)
library(mice)
library(httr)
library(jsonlite)

# Function to connect gemini AI API
gemini = function(prompt, 
                   temperature=0.5,
                   max_output_tokens=1024,
                   api_key=Sys.getenv("GEMINI_API_KEY"),
                   model = "gemini-1.5-flash-latest") {
  
  if(nchar(api_key)<1) {
    api_key <- readline("Paste your API key here: ")
    Sys.setenv(GEMINI_API_KEY = api_key)
  }
  
  model_query = paste0(model, ":generateContent")
  
  response = POST(
    url = paste0("https://generativelanguage.googleapis.com/v1beta/models/", model_query),
    query = list(key = api_key),
    content_type_json(),
    encode = "json",
    body = list(
      contents = list(
        parts = list(
          list(text = prompt)
        )),
      generationConfig = list(
        temperature = temperature,
        maxOutputTokens = max_output_tokens
      )
    )
  )
  
  if(response$status_code>200) {
    stop(paste("Error - ", content(response)$error$message))
  }
  
  candidates = content(response)$candidates
  outputs = unlist(lapply(candidates, function(candidate) candidate$content$parts))
  
  return(outputs)
  
}

#test run for AI tool
prompt = "R code to remove duplicates using dplyr."
cat(gemini(prompt))


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


# To still understand how the relationships change, we'll observe the correlations and see the difference between upper 30% and lower 70% of data

lower_pitching_data = pitching_data %>% filter(y < quantile(pitching_data$y, 0.7)) %>% select(-cluster)
lower_hitting_data = hitting_data %>% filter(y < quantile(hitting_data$y, 0.7)) %>% select(-cluster)
upper_pitching_data = upper_pitching_data %>% select(-cluster)
upper_hitting_data = upper_hitting_data %>% select(-cluster)

upper_pitch_cor = sort(cor(upper_pitching_data, use = "everything")["y",], decreasing = T)
lower_pitch_cor = sort(cor(lower_pitching_data, use = "everything")["y",], decreasing = T)

upper_hit_cor = sort(cor(upper_hitting_data, use = "everything")["y",], decreasing = T)
lower_hit_cor = sort(cor(lower_hitting_data, use = "everything")["y",], decreasing = T)

cor_df = data.frame(
  Variable = names(lower_hit_cor),
  Lower_Hit_Cor = lower_hit_cor,
  Upper_Hit_Cor = upper_hit_cor[names(lower_hit_cor)],
  Lower_Pitch_Cor = lower_pitch_cor[names(lower_hit_cor)],
  Upper_Pitch_Cor = upper_pitch_cor[names(lower_hit_cor)]
)
cor_df

#correlation is not nearly as linear as velocity increases in athletes, will observe what variables see the
# largest change between lower and upper groups

pitch_diff = abs(lower_pitch_cor - upper_pitch_cor)
hit_diff = abs(lower_hit_cor - upper_hit_cor)

diff_df = data.frame(
  Variable = names(pitch_diff),
  Pitch_Diff = pitch_diff,
  Hit_Diff = hit_diff
)

largest_pitch_changes = diff_df[order(-diff_df$Pitch_Diff), ][1:10, 1:2]
largest_hit_changes = diff_df[order(-diff_df$Hit_Diff), ][1:10, c(1,3)]

cat("\nTop 5 variables with largest pitch correlation changes:\n")
print(largest_pitch_changes)

cat("\nTop 5 variables with largest hit correlation changes:\n")
print(largest_hit_changes)

pitch_var_change = diff_df[order(-diff_df$Pitch_Diff), ][1:10, 1]
hit_var_change = diff_df[order(-diff_df$Hit_Diff), ][1:10, 1]

for (var in pitch_var_change) {
  plot <- ggplot(pitching_data, aes_string(x = var, y = "y")) +
    geom_point(alpha = 0.6, color = "blue") +  # Scatterplot
    geom_smooth(method = "lm", color = "red", se = FALSE) +  # Correlation line
    labs(title = paste("Scatterplot of", var, "vs y"),
         x = var, 
         y = "y") +
    theme_minimal()
  
  print(plot)
  # Pause to allow viewing each plot before moving to the next
  readline(prompt = "Press [Enter] to see the next histogram...")
}
for (var in hit_var_change) {
  plot <- ggplot(hitting_data, aes_string(x = var, y = "y")) +
    geom_point(alpha = 0.6, color = "blue") +  # Scatterplot
    geom_smooth(method = "lm", color = "red", se = FALSE) +  # Correlation line
    labs(title = paste("Scatterplot of", var, "vs y"),
         x = var, 
         y = "y") +
    theme_minimal()
  
  print(plot)
  # Pause to allow viewing each plot before moving to the next
  readline(prompt = "Press [Enter] to see the next histogram...")
}       
# large differences between upper and lower, shows the influence of outliers on the data.       
       
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

# assess the relationship to Y for the pitching and hitting data sets
# Pitching
df_pitch = pitching_data %>%
  pivot_longer(
    cols = -y,              # Exclude 'y' from being reshaped
    names_to = "variable",  # Create a column for variable names
    values_to = "value"     # Create a column for values
  )

ggplot(df_pitch, aes(x = value, y = y)) +
  geom_point(alpha = 0.6) +   # Scatter points
  facet_wrap(~variable, scales = "free_x") +  # Separate plots for each variable
  labs(x = "Value of Variable", y = "Throwing Speed", title = "Scatterplots of Variables vs Throwing Speed") +
  theme_minimal()

#Hitting
df_hit = hitting_data %>%
  pivot_longer(
    cols = -y,              # Exclude 'y' from being reshaped
    names_to = "variable",  # Create a column for variable names
    values_to = "value"     # Create a column for values
  )

# Create scatterplots for each variable against 'y'
ggplot(df_pitch, aes(x = value, y = y)) +
  geom_point(alpha = 0.6) +   # Scatter points
  facet_wrap(~variable, scales = "free_x") +  # Separate plots for each variable
  labs(x = "Value of Variable", y = "Throwing Speed", title = "Scatterplots of Variables vs Throwing Speed") +
  theme_minimal()

#re-examine the distribution of variables to understand what kind of transformations can be applied
install.packages(e1071)
library(e1071)

skewness_values_pitch = sapply(pitching_data[, 2:ncol(pitching_data)], skewness, na.rm = TRUE)
skewness_values_hit = sapply(hitting_data[, 2:ncol(hitting_data)], skewness, na.rm = TRUE)

# Identify highly skewed variables
skewed_vars_pitch = names(skewness_values_pitch[abs(skewness_values_pitch) > 1])
skewed_vars_hit = names(skewness_values_hit[abs(skewness_values_hit) > 1])

# Output results
cat("Skewness values for Pitching:\n")
print(skewness_values_pitch)
cat("Skewness values for Hitting:\n")
print(skewness_values_pitch)

cat("\nHighly skewed Pitching variables (|skewness| > ", 1, "):\n", sep = "")
print(skewed_vars_pitch)
cat("\nHighly skewed Hitting variables (|skewness| > ", 1, "):\n", sep = "")
print(skewed_vars_hit)

#seeing rather large skewness with certain variables in both data sets but will keep them since
# we'll be implementing a tree-based model

#Feature Engineering to simplify the variables
# since body weight already included, will remove variables that indicates relative strength/force
# production to body weight. In addition, will calculate the average TSpineRom and will only
# keep the shoulder ER with the greatest value (as dominant side will typically have large ROM than
# non-throwing, and will keep subsequent IR variable)

#pitching data 
sum(ncol(pitching_data))

reduced_pitching_data = pitching_data %>%
  select(-contains("bm_.w.kg."))

# Process ShoulderER and ShoulderIR variables
reduced_pitching_data = reduced_pitching_data %>%
  mutate(
    DominantShoulderER = if_else(ShoulderERR > ShoulderERL, ShoulderERR, ShoulderERL),
    DominantShoulderER_Side = if_else(ShoulderERR > ShoulderERL, "R", "L"),
    DominantShoulderIR = if_else(DominantShoulderER_Side == "R", ShoulderIRR, ShoulderIRL),
    AvgTSpineRom = (TSpineRomR + TSpineRomL)/2
  ) %>%
  select(-ShoulderERR, -ShoulderERL, -ShoulderIRR, -ShoulderIRL, -DominantShoulderER_Side, 
         -TSpineRomR, -TSpineRomL)

sum(ncol(reduced_pitching_data))

#hitting data set
sum(ncol(hitting_data))

reduced_hitting_data = hitting_data %>%
  select(-contains("bm_.w.kg."))

# Process ShoulderER and ShoulderIR variables
reduced_hitting_data = reduced_hitting_data %>%
  mutate(
    DominantShoulderER = if_else(ShoulderERR > ShoulderERL, ShoulderERR, ShoulderERL),
    DominantShoulderER_Side = if_else(ShoulderERR > ShoulderERL, "R", "L"),
    DominantShoulderIR = if_else(DominantShoulderER_Side == "R", ShoulderIRR, ShoulderIRL),
    AvgTSpineRom = (TSpineRomR + TSpineRomL)/2
  ) %>%
  select(-ShoulderERR, -ShoulderERL, -ShoulderIRR, -ShoulderIRL, -DominantShoulderER_Side, 
         -TSpineRomR, -TSpineRomL)

sum(ncol(reduced_hitting_data))
#resulted in reduction of 5 variables from the data frames

# Feature Scalling
# Pitching
preProcValuesPitch = preProcess(reduced_pitching_data[,2:ncol(reduced_pitching_data)]
                            , method = c("center","scale","pca"))
summary(preProcValuesPitch)
reduced_pitching_data = predict(preProcValuesPitch, reduced_pitching_data)

loadingsPitch = preProcValuesPitch$rotation

# Hitting
preProcValuesHit = preProcess(reduced_hitting_data[,2:ncol(reduced_hitting_data)]
                            , method = c("center","scale","pca"))
summary(preProcValuesHit)
reduced_hitting_data = predict(preProcValuesHit, reduced_hitting_data)

loadingsHit = preProcValuesHit$rotation

#splitting both data sets up to understand how impactful these selected variables will be to the 
# ability to accurately predict throwing and swing speed

#reduced throwing data
set.seed(19990)
trainPitch1 = createDataPartition(y = reduced_pitching_data$y, p = 0.8, list = F)
trPitch1 = reduced_pitching_data[trainPitch,]
tePitch1 = reduced_pitching_data[-trainPitch,]

#reduced swing data
trainHit1 = createDataPartition(y = reduced_hitting_data$y, p = 0.8, list = F)
trHit1 = reduced_hitting_data[trainHit,]
teHit1 = reduced_hitting_data[-trainHit,]

ctrl = trainControl(method="cv", number = 10, classProbs = F, 
                    summaryFunction = defaultSummary, allowParallel = T)

mPitch1 = train(y ~ ., data = trPitch1, method = "gbm", trControl = ctrl, metric = "RMSE", verbose = F)
mHit1 = train(y ~ ., data = trHit1, method = "gbm", trControl = ctrl, metric = "RMSE", verbose = F)

#mThrow train
defaultSummary(data=data.frame(obs=trPitch1$y, pred=predict(mPitch1, newdata=trPitch1))
               , model=mPitch1)
#      RMSE  Rsquared       MAE 
# 4.3921749 0.8056028 3.2661287
#mThrow test
defaultSummary(data=data.frame(obs=tePitch1$y, pred=predict(mPitch1, newdata=tePitch1))
               , model=mPitch1)
#      RMSE  Rsquared       MAE 
# 5.0140508 0.7136857 4.1165287
#mHit train
defaultSummary(data=data.frame(obs=trHit1$y, pred=predict(mHit1, newdata=trHit1))
               , model=mHit1)
#      RMSE  Rsquared       MAE 
# 3.5961753 0.7713494 2.6659913 
#mHit test
defaultSummary(data=data.frame(obs=teHit1$y, pred=predict(mHit1, newdata=teHit1))
               , model=mHit1)
#      RMSE  Rsquared       MAE 
# 4.4812789 0.7180411 3.2210533
set.seed(2025)
trainPitch2 = createDataPartition(y = pitching_data$y, p = 0.8, list = F)
trPitch2 = pitching_data[trainPitch,]
tePitch2 = pitching_data[-trainPitch,]

#swing data
trainHit2 = createDataPartition(y = hitting_data$y, p = 0.8, list = F)
trHit2 = hitting_data[trainHit,]
teHit2 = hitting_data[-trainHit,]

mPitch2 = train(y ~ ., data = trPitch2, method = "gbm", trControl = ctrl, metric = "RMSE", verbose = F)
mHit2 = train(y ~ ., data = trHit2, method = "gbm", trControl = ctrl, metric = "RMSE", verbose = F)

#mThrow train
defaultSummary(data=data.frame(obs=trPitch2$y, pred=predict(mPitch2, newdata=trPitch2))
               , model=mPitch2)
#      RMSE  Rsquared       MAE 
# 5.1544506 0.7282005 3.7460377
#mThrow test
defaultSummary(data=data.frame(obs=tePitch2$y, pred=predict(mPitch2, newdata=tePitch2))
               , model=mPitch2)
#      RMSE  Rsquared       MAE 
# 4.8061714 0.7342857 3.9220766
#mHit train
defaultSummary(data=data.frame(obs=trHit2$y, pred=predict(mHit2, newdata=trHit2))
               , model=mHit2)
#      RMSE  Rsquared       MAE 
# 3.3386314 0.8019561 2.4970652
#mHit test
defaultSummary(data=data.frame(obs=teHit2$y, pred=predict(mHit2, newdata=teHit2))
               , model=mHit2)
#      RMSE  Rsquared       MAE 
# 4.3963371 0.7231484 3.0883645

# Appears that applying PCA to and reducing number of variables leads to a more accurate model
# for pitching and closely comparible for hitting. 

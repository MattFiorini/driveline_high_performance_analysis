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
       

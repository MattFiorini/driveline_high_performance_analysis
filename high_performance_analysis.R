install.packages("ggcorrplot")
library(ggcorrplot)
library(tidyverse)

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
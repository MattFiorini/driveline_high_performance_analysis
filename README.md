# driveline_high_performance_analysis
leveraging Driveline's high performance training data to better understand strength and conditioning metrics to both throwing and swing speed

This data analysis observes the difference between performance metrics and swing and throwing difference.

This analysis involves initial data cleaning, leveraging the mice package within R to impute the missing values. In addition, EDA was done through correlation matrix, and k-means clustering to understand the relationships between performance related data, absent of the influence of speed. This was then validated by segmenting the data based on throwing speed, between the upper quartile of velicty (for throwing, greater than 85mph, with group 1 being split into  85 < 90 and group 2 being >= 90).

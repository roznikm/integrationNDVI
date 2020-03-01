library(tidyverse)

df <- read_csv("/Users/uofm_research/Rprojects/integrationNDVI/StartingData/PrismDailyStates.csv")

df <- separate(df, `system:index`, c('Year', 'MonthDay'), 4)
df <- separate(df, 'MonthDay', c('Month', 'Day'), 2)
df$Day <- substr(df$Day, 0, 2)
df <- df[, -12]

write_csv(df, "/Users/uofm_research/Rprojects/integrationNDVI/PrismDailyStatesClean.csv")

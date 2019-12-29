library(tidyverse)

## Insert your file path to the data from your local computer
## Set wd using setwd('path) /Users/uofm_research/Rprojects/integrationNDVI/
file_path_read = './df250mCorn.csv'
file_path_write = './CornMaxNDVI.csv'
df <- read_csv(file_path_read)
df$date <- as.Date(with(df, paste(Year, Month, Day,sep="-")), "%Y-%m-%d")
df <- df %>% filter(Year > 2005)
years_geoid_df <- group_split(df, Year, GEOID)

calculate_max_ndvi = function(county_year) {
  max_ndvi <- county_year[which.max(county_year$NDVI), ]
  max_ndvi$NDVI <- max_ndvi$NDVI / 10000
  return(max_ndvi)
}

results <- map(years_geoid_df, calculate_max_ndvi)
results <- bind_rows(results)

selected_counties <- results %>% 
  group_by(GEOID) %>% 
  summarise(n=n()) %>% filter(n==13)

results_full_history <- results %>% 
                      filter(GEOID %in% selected_counties$GEOID)

write_csv(results_full_history, file_path_write)

## Test max NDVI 
mod <- lm(yield ~ NDVI + I(NDVI^2) + GEOID,data=results_full_history)
summary(mod)



library(tidyverse)

## Insert your file path to the data from your local computer
## Pick Crop Type
file_path_read <- '/Users/uofm_research/Rprojects/integrationNDVI/StartingData/df250mSpringWheat.csv'
file_path_write <- '/Users/uofm_research/Rprojects/integrationNDVI/SpringWheatIntNDVI.csv'
file_path_start_end_dates <- '/Users/uofm_research/Rprojects/integrationNDVI/StartingData/SpringWheatStartAndEndDates.csv'

start_end_dates <- read_csv(file_path_start_end_dates)
start_end_dates$STATEFP <- as.character(start_end_dates$STATEFP)
start_end_dates <- drop_na(start_end_dates)
states <- unique(start_end_dates$STATEFP)

df <- read_csv(file_path_read)
df$date <- as.Date(with(df, paste(Year, Month, Day,sep="-")), "%Y-%m-%d")
df <- df %>% filter(Year > 2005 & STATEFP %in% states)
years_geoid_df <- group_split(df, Year, GEOID)

calcIntegratedNdvi <- function(county_year) {
  if(sum(is.na(county_year$NDVI)) > 0)  {
    county_year_output <- county_year[1,]
    return(county_year_output)
  }
  county_year$NDVI <- county_year$NDVI /10000 
  ndviFunc <- splinefun(1:nrow(county_year), y = county_year$NDVI, method = "fmm", ties = mean)
  county_year_output <- county_year[1,]
  start_and_end <- start_end_dates %>% filter(STATEFP == county_year_output$STATEFP & Year == county_year_output$Year)
  start <- round(lubridate::yday(start_and_end$StartDate) / 16)
  end <- round(lubridate::yday(start_and_end$EndDate) / 16)
  county_year_output$NDVI <- integrate(ndviFunc,start,end)$value
  return(county_year_output)
}

results <- map(years_geoid_df, calcIntegratedNdvi)
results <- bind_rows(results) 
results <- drop_na(results)

selected_counties <- results %>% 
  group_by(GEOID) %>% 
  summarise(n=n()) %>% filter(n==13)

results_full_history <- results %>% 
  filter(GEOID %in% selected_counties$GEOID)

write_csv(results_full_history, file_path_write)

## Test int NDVI 
mod <- lm(yield ~ NDVI + I(NDVI^2) + GEOID, data=results_full_history)
summary(mod)




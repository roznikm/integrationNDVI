library(tidyverse)

## Insert your file path to the data from your local computer
## Set wd using setwd('path) /Users/uofm_research/Rprojects/MichaelWilton/
filePath = './df250mCorn.csv'
df <- read_csv(filePath)
df$date <- as.Date(with(df, paste(Year, Month, Day,sep="-")), "%Y-%m-%d")
counties <- unique(df$GEOID)
years <- unique(df$Year)
listResults <- list() 

for(j in 1:length(years)) {
  results <- tibble(GEOID=NA, intNDVI=NA, yield=NA, planted=NA, Year=NA)
  for(i in 1:length(counties)) {
    id <- counties[i]
    sing <- df %>% filter(GEOID ==id & Year == years[j])
    if(nrow(sing) < 1 || sum(is.na(sing$NDVI)) > 15) {
      results[i, 'GEOID'] <- sing$GEOID[1]
      results[i, 'intNDVI'] <- NA
      results[i, "yield"] <- sing$yield[1]
      results[i, "planted"] <- sing$planted[1]
      results[i, "Year"] <- sing$Year[1]
    } else {
      ind <- dplyr::select(sing, "NDVI", "date", "GEOID", "yield", "planted", "Year") 
      ggplot(ind, aes(y=NDVI, x= date)) + geom_point()
      ### add an index to be used in integration (can't use a date type for start or end)
      ind$index <- 1:nrow(ind)
      
      ## scale NDVI back so we can interpret results better
      ind$NDVI <- ind$NDVI/10000
      
      results[i, 1] <- ind$GEOID[1]
      results[i,2] <- max(ind$NDVI)
      results[i,3] <- ind$yield[1]
      results[i, 4] <- ind$planted[1]
      results[i, 5] <- ind$Year[1]
    }
  }
  listResults[[j]] <- drop_na(results)
}

resultsAllYears <- bind_rows(listResults)
countiesFullHistory <- resultsAllYears %>% 
  group_by(GEOID) %>% 
  summarise(n=n()) %>% filter(n==19)
resultsFullHistory <- resultsAllYears %>% filter(GEOID %in% countiesFullHistory$GEOID)

write_csv(resultsFullHistory, "University of Manitoba -M/1.Thesis/Corn/maxNDVI.csv")

mod <- lm(yield ~ intNDVI,data=resultsFullHistory)
summary(mod)

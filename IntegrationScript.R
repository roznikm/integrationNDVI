library(tidyverse)

## Insert your file path to the data from your local computer 
filePath = '/Users/lysaporth/Documents/SpatialTemporalRes/SpatialPaperData/FullFiles/df250mSoy.csv'
df <- read_csv(filePath)
df$date <- as.Date(with(df, paste(Year, Month, Day,sep="-")), "%Y-%m-%d")

sing <- df %>% filter(GEOID =='19001' & Year == 2017)
ind <- dplyr::select(sing, "NDVI", "date") 
ggplot(ind, aes(y=NDVI, x= date)) + geom_point()
### add an index to be used in integration (can't use a date type for start or end)
ind$index <- 1:nrow(ind)

## scale NDVI back so we can interpret results better
ind$NDVI <- ind$NDVI/10000
fnToint <- splinefun(ind$index, y = ind$NDVI, method = "fmm", ties = mean)

## Plot spline function to check it
plot(ind$index, ind$NDVI)
lines(spline(ind$index, ind$NDVI), col = 2)

## Results seem about right, you can double check by taking a sum of the NDVI values
## The integrated value should be similar
integrate(fnToint,7,18)

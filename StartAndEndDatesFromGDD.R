library(tidyverse)
#--------Preliminary Selections------
## Choose Corn, Soybean, WinterWheat, or SpringWheat 
## Depending on the Crop Type choose the crop progress % and accumulated_gdd level
## Corn (70, 2900), Soybean(70, 1992), WinterWheat(70, 1825), SpringWheat(70, 1825)
## Must also select the GDD base temperture in F
## Corn(50), Soybean(50), WinterWheat(32), SpringWheat (32)
crop <- "SpringWheat"
crop_progress_selection <- 70
accumulated_gdd_selection <- 1825
base_temp_selection <- 32
#-------------------------------------
prism <- read_csv("/Users/uofm_research/Rprojects/integrationNDVI/StartingData/PrismDailyStatesClean.csv")
prism$Date <- as.Date(with(prism, paste(Year, Month, Day,sep="-")), "%Y-%m-%d")
prism <- prism %>% select(-Month, -Day)
crop_progress <- read_csv(paste0("/Users/uofm_research/Rprojects/integrationNDVI/", crop, "PlantingProgress.csv"))
crop_progress <- drop_na(crop_progress)
if(crop == 'Corn' ) {
  crop_progress$Date <- lubridate::mdy(crop_progress$date_progress)
}
if(crop == 'Soybean' | crop == 'WinterWheat' | crop == 'SpringWheat') {
  crop_progress$Date <- lubridate::dmy(crop_progress$date_progress)
  crop_progress$Date <- as.Date(crop_progress$Date, '%y-%m-%d')
}
crop_progress$STATEFP <- str_pad(crop_progress$STATEFP, 2, 'left', pad=0)
crop_progress <- crop_progress %>% filter(Year > 2005)
crop_progress <- crop_progress %>% group_by(STATEFP) %>% filter(n() > 100) 

Start_EndDate_Finder = function(year, statefp, progress_percent = crop_progress_selection, accumulated_gdd = accumulated_gdd_selection) {
  weather <- prism %>% filter(STATEFP == statefp, Date >= paste0(year,"-01-01") & Date <= paste0(year, "-12-31"))
  progress <- crop_progress %>% filter(STATEFP == statefp, Date >= paste0(year ,"-01-01") & Date <= paste0(year, "-12-31"))
  start <- progress %>% filter(Progress > progress_percent)
  start <- start[1,]
  startDate <- start$Date
  if(is.na(startDate)) {
    startDate <- paste0(year, "-05-01")
    startDate <- lubridate::ymd(startDate)
  }
  weather <- weather %>% filter(Date > startDate)
  weather <- weather %>% mutate(GDD = ((tmean * 9/5) + 32) - base_temp_selection)
  weather$GDD <- replace(weather$GDD, weather$GDD < 0, 0) 
  weather$AccGDD <- cumsum(weather$GDD)
  end <- weather %>% filter(AccGDD > accumulated_gdd)
  end <- end[1,]
  end$StartDate <- startDate
  if(is.na(end$AccGDD)) {
    end <- weather[which.max(weather$AccGDD),]
    end$StartDate <- startDate
  } 
  return(end)
}
years <- tibble(year = unique(crop_progress$Year))
statefps <- tibble(statefp = unique(crop_progress$STATEFP))
years$fake <- 1
statefps$fake <- 1
my_cross_join <- full_join(years, statefps, by = "fake") %>%
  select(-fake)
year_list <- as.list(my_cross_join$year) 
statefp_list <- as.list(my_cross_join$statefp) 


start_and_end_dates <- map2(year_list, statefp_list, Start_EndDate_Finder)
start_and_end_dates <- bind_rows(start_and_end_dates)
start_and_end_dates <- start_and_end_dates %>% dplyr::select(NAME, STATEFP, Date, AccGDD, StartDate, Year)
names(start_and_end_dates)[3] <- 'EndDate'
write_csv(start_and_end_dates, paste0("/Users/uofm_research/Rprojects/integrationNDVI/", crop, "StartAndEndDates.csv"))

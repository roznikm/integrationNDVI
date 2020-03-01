library(tidyverse)
library(stargazer)

cornIntNdvi <- read_csv("/Users/uofm_research/Rprojects/integrationNDVI/CornIntNDVI.csv")
cornMaxNdvi <- read_csv("/Users/uofm_research/Rprojects/integrationNDVI/CornMaxNDVI.csv")

modInt <- lm(yield ~ NDVI + I(NDVI^2) + GEOID, data=cornIntNdvi)
summary(modInt)
modMax <- lm(yield ~ NDVI + I(NDVI^2) + GEOID, data=cornMaxNdvi)
summary(modMax)

stargazer(modInt, modMax, style='qje', omit='GEOID', type='latex', 
          dep.var.labels='Corn Yield',
          column.labels = c("Integrated NDVI","Max NDVI"),
          covariate.labels = c("NDVI", "NDVI-squared", "Constant"))
## Max NDVI performs much better R2 0.7852 vs 0.5779

soybeanIntNdvi <- read_csv("/Users/uofm_research/Rprojects/integrationNDVI/SoybeanIntNDVI.csv")
soybeanMaxNdvi <- read_csv("/Users/uofm_research/Rprojects/integrationNDVI/SoybeanMaxNDVI.csv")

modInt <- lm(yield ~ NDVI + I(NDVI^2) + GEOID, data=soybeanIntNdvi)
summary(modInt)
modMax <- lm(yield ~ NDVI + I(NDVI^2) + GEOID, data=soybeanMaxNdvi)
summary(modMax)
## Max NDVI outperforms 0.7713 to 0.5728
stargazer(modInt, modMax, style='qje', omit='GEOID', type='latex', 
          dep.var.labels='Soybean Yield',
          column.labels = c("Integrated NDVI","Max NDVI"),
          covariate.labels = c("NDVI", "NDVI-squared", "Constant"))

winterIntNdvi <- read_csv("/Users/uofm_research/Rprojects/integrationNDVI/WinterWheatIntNDVI.csv")
winterMaxNdvi <- read_csv("/Users/uofm_research/Rprojects/integrationNDVI/WinterWheatMaxNDVI.csv")

modInt <- lm(yield ~ NDVI + I(NDVI^2) + GEOID, data=winterIntNdvi)
summary(modInt)
modMax <- lm(yield ~ NDVI + I(NDVI^2) + GEOID, data=winterMaxNdvi)
summary(modMax)
### Winter Wheat Max outperforms 0.7908 to 0.753
stargazer(modInt, modMax, style='qje', omit='GEOID', type='latex', 
          dep.var.labels='Winter-Wheat Yield',
          column.labels = c("Integrated NDVI","Max NDVI"),
          covariate.labels = c("NDVI", "NDVI-squared", "Constant"))


springIntNdvi <- read_csv("/Users/uofm_research/Rprojects/integrationNDVI/SpringWheatIntNDVI.csv")
springMaxNdvi <- read_csv("/Users/uofm_research/Rprojects/integrationNDVI/SpringWheatMaxNDVI.csv")

modInt <- lm(yield ~ NDVI + I(NDVI^2) + GEOID, data=springIntNdvi)
summary(modInt)
modMax <- lm(yield ~ NDVI + I(NDVI^2) + GEOID, data=springMaxNdvi)
summary(modMax)
### Spring Wheat Max outperforms 0.4223 to 0.2214
stargazer(modInt, modMax, style='qje', omit='GEOID', type='latex', 
          dep.var.labels='Spring-Wheat Yield',
          column.labels = c("Integrated NDVI","Max NDVI"),
          covariate.labels = c("NDVI", "NDVI-squared", "Constant"))



library(readxl)
library(openxlsx)
library(tidyr)
library(tidyverse) # Modern data science library
library(plm) # Panel data analysis library
library(car) # Companion to applied regression
library(gplots) # Various programing tools for plotting data
library(tseries) # For timeseries analysis
library(lmtest) # For hetoroskedasticity analysis
library(dplyr)
library(showtext)


a <- loadWorkbook("Book1.xlsx")
sheetNames <- sheets(a)

for (i in 1:length(sheetNames))
{
  assign(sheetNames[i], readWorkbook(a, sheet = i))
}

# waste_recycled_muni <- gather(` 執行機關資源回收量`, "county", "waste_recycled_muni", -year)

waste_treatment <- gather(` 一般廢棄物妥善處理率`, "county", "waste_treatment", -year)
so2 <- gather(` 二氧化硫含量`, "county", "so2", -year)
ozone <- gather(` 空氣中臭氧濃度`, "county", "ozone", -year)
pm_total <- gather(` 空氣中總懸浮微粒濃度`, "county", "pm_total", -year)
pm2.5 <- gather(` 細懸浮微粒手動監測(PM2.5)濃度值`, "county", "pm2.5", -year)
lung_rate <- `癌症粗率（每10萬人口）`

temp1 <- merge(waste_treatment, so2, by = c(1, 2))
temp2 <- merge(temp1, ozone, by = c(1, 2))
temp3 <- merge(temp2, pm_total, by = c(1, 2))
View(temp3)
temp4 <- merge(temp3, pm2.5, by = c(1, 2))
temp5 <- merge(temp4, lung_rate, by = c(1, 2))

View(temp5)
temp5 %>% summarise_all(funs(class))

library(imputeTS)
library(zoo)
# # for_plot <- subset(temp5, select = -c(county))
# 
# temp5[] <- sapply(temp5, as.numeric)
# temp5 %>% summarise_all(funs(class))
# 
# temp5 <- na_interpolation(temp5)
# # final <- na.interpolation(temp5, option = "linear")

write.csv(temp5, "data.csv")





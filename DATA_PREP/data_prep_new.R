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


a <- loadWorkbook("DATA/raw_data/cancer.xlsx")
sheetNames <- sheets(a)

for (i in 1:length(sheetNames))
{
    assign(sheetNames[i], readWorkbook(a, sheet = i))
}

# waste_recycled_muni <- gather(` 執行機關資源回收量`, "county", "waste_recycled_muni", -year)
# 指標項：就業者之行業結構-工業(％)
# 定義：從事包括礦業及土石採取業、製造業、水電燃氣業與營造業之就業者占?
#   `就業
# 者之百分比。
# 公式：（工業就業人口數／總就業人口數）＊100
# 註記：自91年起金門縣於每年5月及11月各辦理1次人力資源抽樣調查；連江縣
# 於每年9月辦理1次普查。

factory_density <- read.csv("~/Documents/GitHub/School/bio_ia_air/DATA/raw_data/factory_density.csv")
factory_density <- gather(factory_density, "county/city", "Factory Density (Factory Count Per Squared Kilometer)", -year)
# waste_treatment <- gather(` 一般廢棄物妥善處理率`, "county", "waste_treatment", -year)
# so2 <- gather(` 二氧化硫含量`, "county", "so2", -year)
ozone <- gather(read.csv("~/Documents/GitHub/School/bio_ia_air/DATA/raw_data/ozone.csv"), "county/city", "Annual Average Ozone Concentration (ppb)", -year)
# pm_total <- gather(` 空氣中總懸浮微粒濃度`, "county", "pm_total", -year)
# pm2.5 <- gather(` 細懸浮微粒手動監測(PM2.5)濃度值`, "county", "pm2.5", -year)
lung_cancer <- lung_cancer_all
colnames(lung_cancer) <- c("year", "sex", "county", "cancer_type", "Age-Adjusted Lung Cancer (per 100,000 population) ", " ", " ", " ", " ")
# lung_cancer <- subset(lung_cancer, !(county/city %in% c("澎湖縣", "金門縣", "連江縣", "全國")))
lung_cancer <- subset(lung_cancer, sex %in% c("全"))
# lung_cancer <- subset(lung_cancer, year >= 2010)
# lung_cancer <- subset(lung_cancer, year <= 2020)
lung_cancer <- subset(lung_cancer, cancer_type %in% c("肺、支氣管及氣管"))

keep.cols <- names(lung_cancer) %in% c(" .1", " .2", " .3", " ", "sex", "cancer_type") # nolint
lung_cancer <- lung_cancer[!keep.cols]

temp1 <- merge(factory_density, ozone, by = c(1, 2), all=TRUE)
temp2 <- merge(temp1, lung_cancer, by = c(1, 2), all=TRUE)
temp2 <- subset(temp2, year >= 2009)
temp2 <- subset(temp2, year < 2019)
temp2 <- subset(temp2, !(`county/city` %in% c("澎湖縣", "金門縣", "連江縣", "全國", "臺灣地區", "總計")))

temp2 %>% summarise_all(funs(class))
temp2$`Factory Density (Factory Count Per Squared Kilometer)` <- as.numeric(temp2$`Factory Density (Factory Count Per Squared Kilometer)`)
temp2$`Annual Average Ozone Concentration (ppb)` <- as.numeric(temp2$`Annual Average Ozone Concentration (ppb)`)
temp2 %>% summarise_all(funs(class))

library(imputeTS)
library(zoo)
# # for_plot <- subset(temp5, select = -c(county))
#
# temp5[] <- sapply(temp5, as.numeric)
# temp5 %>% summarise_all(funs(class))
#
# temp5 <- na_interpolation(temp5)
final <- na.interpolation(temp2, option = "linear")
rownames(final) <- NULL
colnames(final)[1] <- "Year"
colnames(final)[2] <- "County/City"
write.csv(final, "~/Documents/GitHub/School/bio_ia_air/DATA/data.csv")

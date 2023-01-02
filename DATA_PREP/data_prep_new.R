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

factory_density <- read.csv("~/Documents/GitHub/School/bio_ia_air/DATA/raw_data/factory_density.csv")
factory_density <- gather(factory_density, "county/city", "Factory Density (Factory Count Per Squared Kilometer)", -year)
ozone <- gather(read.csv("~/Documents/GitHub/School/bio_ia_air/DATA/raw_data/ozone.csv"), "county/city", "Annual Average Ozone Concentration (ppb)", -year)

# Lung Cancer
lung_cancer <- cancer
colnames(lung_cancer) <- c("year", "sex", "county", "cancer_type", "Age-Adjusted Lung Cancer (per 100,000 population) ", " ", " ", " ", " ")
lung_cancer <- subset(lung_cancer, sex %in% c("全"))
lung_cancer <- subset(lung_cancer, cancer_type %in% c("肺、支氣管及氣管"))
keep.cols <- names(lung_cancer) %in% c(" .1", " .2", " .3", " ", "sex", "cancer_type") # nolint
lung_cancer <- lung_cancer[!keep.cols]

# Air Pollutants
pm <- gather(read.csv("~/Documents/GitHub/School/bio_ia_air/DATA/raw_data/pm.csv"), "county/city", "Particulate Matter Concentration (1.0×10^-9 kg/m-3)", -year)
so2 <- gather(read.csv("~/Documents/GitHub/School/bio_ia_air/DATA/raw_data/SO2.csv"), "county/city", "so2", -year)
no2 <- gather(read.csv("~/Documents/GitHub/School/bio_ia_air/DATA/raw_data/NO2.csv"), "county/city", "no2", -year)
co <- gather(read.csv("~/Documents/GitHub/School/bio_ia_air/DATA/raw_data/CO.csv"), "county/city", "co", -year)

# Densities
urban_density <- gather(read.csv("~/Documents/GitHub/School/bio_ia_air/DATA/raw_data/urban_density.csv"), "county/city", "urban_density", -year)
hospital_bed <- gather(read.csv("~/Documents/GitHub/School/bio_ia_air/DATA/raw_data/hospital_bed.csv"), "county/city", "hospital_bed", -year)
age65 <- gather(read.csv("~/Documents/GitHub/School/bio_ia_air/DATA/raw_data/age65.csv"), "county/city", "age65", -year)


lung_cancer <- lung_cancer %>% 
  mutate(county=str_replace(county,"台", "臺"))

temp1 <- merge(ozone, lung_cancer, by = c(1, 2), all=TRUE)
temp2 <- subset(temp1, year >= 2010)
temp2 <- subset(temp2, year <= 2019)
temp2 <- subset(temp2, !(`county/city` %in% c("澎湖縣", "金門縣", "連江縣", "全國", "臺灣地區", "總計")))

temp2 %>% summarise_all(funs(class))
# temp2$`Factory Density (Factory Count Per Squared Kilometer)` <- as.numeric(temp2$`Factory Density (Factory Count Per Squared Kilometer)`)
temp2$`Annual Average Ozone Concentration (ppb)` <- as.numeric(temp2$`Annual Average Ozone Concentration (ppb)`)
# temp2$so2 <- as.numeric(temp2$so2)
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

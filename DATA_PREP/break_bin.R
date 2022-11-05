# library(classInt)
df <- read.csv("~/Documents/GitHub/School/bio_ia_air/DATA/data.csv")
colnames(df)[4]<- "factory_density"
# classIntervals(df$factory_density, 6, style="fixed")

library(dplyr)
library(tidyverse)
#perform data binning on points variable
df %>% mutate(points_bin = cut(factory_density, breaks=6))

factory <- cut_interval(df$factory_density, n=6)
hist(df$factory_density, xlab="Factory Density (Factory Count Per Squared Kilometer)", main = "Histogram of Factory Density (Factory Count Per Squared Kilometer) \n across Taiwan's counties/cities between 2009 and 2018")

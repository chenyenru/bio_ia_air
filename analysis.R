library(dplyr)
library(ggplot)

data <- read.csv("final_merge.csv")

data_num <- as.data.frame(apply(data, 2, as.numeric))  # Convert all variable types to numeric
data_num <- subset(data_num, -c(county, factory_density))
data_num <- sapply(data_num, class)                                # Print classes of all columns

pairs(data)

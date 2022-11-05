library(tidyverse)
library(Hmisc)
library(ggplot2)
library(dplyr)
library(ggpubr)
library(EnvStats)

dataset = read.csv("DATA/data.csv")
colnames(dataset) <- c("x", "year", "location", "factory_density", "ann_avg_ozone", "age_adj_lung")

# dataset %>% 
#   mutate(bin = cut(factory_density, breaks = c(0, 2, 4, 6, 8, 10, 12, 14))) %>% 
#   count(bin)
  
BREAKS<-c(0, 2, 4, 6, 8, 10, 12, 14)
dataset$groups = cut(dataset$factory_density,breaks=BREAKS)

# View(dataset)

# Create Color Scale
# library(RColorBrewer)
# myColors <- brewer.pal(19,"Set1")
# names(myColors) <- levels(dataset$location)
colScale <- scale_colour_manual(name = "grp",values = myColors)

# Create the Label column that includes Sample Size
dataset <- dataset %>% left_join(dataset %>% group_by(groups) %>% summarise(N=n()))%>%
  mutate(Label=paste0('Interval: ', groups,' (Sample Size = ',N,')'))
  
  # ggplot(.,mapping=aes(x=Sepal.Length))+
  # geom_histogram(binwidth= 0.1)+
  # facet_wrap(~Label)


p <- ggplot(data = dataset, aes(x = ann_avg_ozone, y = age_adj_lung)) + 
  geom_point() +
  geom_smooth(method="lm", se = TRUE) + 
  stat_cor(label.y = 52) +
  stat_regline_equation(label.y = 50) +
  xlab("Annual Average Ozone Concentration (ppb)") + 
  ylab("Age-Adjusted Lung Cancer Incidence Rate (Death Counts per 100,000 People)") +
  labs(
    title = "Annual Average Ozone Concentration (ppb) vs. \n Age-Adjusted Lung Cancer Incidence Rate (Death Counts per 100,000 People)",
    subtitle = "By Factory Density (Factory Counts/km2)",
    caption = "Source: Taiwan Environmental Protection Agency"
  ) + 
  theme(
    axis.title = element_text(size=15),
    axis.text = element_text(size=14),
    plot.title = element_text(color = "#0099f9", size = 20, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 13, face = "bold", hjust = 0.5),
    plot.caption = element_text(face = "italic", hjust = 0)
  )
p + facet_wrap(~Label)

# p2 <- ggplot(data = dataset, aes(x = ann_avg_ozone, y = age_adj_lung)) + 
#   geom_point() +
#   geom_smooth(method="lm", se = FALSE) + 
#   stat_cor(label.y = 52) +
#   stat_regline_equation(label.y = 50) +
#   xlab("Annual Average Ozone Concentration (ppb)") + 
#   ylab("Age-Adjusted Lung Cancer Incidence Rate (Death Counts per 100,000 People)") +
#   labs(
#     title = "Annual Average Ozone Concentration (ppb) vs. \n Age-Adjusted Lung Cancer Incidence Rate (Death Counts per 100,000 People)",
#     subtitle = "By Factory Density (Factory Counts/km2)",
#     caption = "Source: Taiwan Environmental Protection Agency"
#   )
# p2
# # table(cut(dataset$factory_density, breaks = a, include.lowest = TRUE))
# 
# # dataset$groups <- as.numeric(cut2(dataset$factory_density, breaks = BREAKS, include.lowest = TRUE))
# 
# # dataset %>%
# #   group_by(category = cut(factory_density, c(0, 2, 4, 6, 8, 10, 12, 14), labels=paste0("i",1:7))) %>%
# #   summarise(
# #     mean = mean(factory_density),
# #     sd = sd(factory_density),
# #     count = n()
# #   )
# 
# groups_by_factory_density <- split(dataset,cut(dataset$factory_density, breaks = c(0, 2, 4, 6, 8, 10, 12, 14)))
# 
# 
# # hist(groups_by_factory_density$`(0,2]`$age_adj_lung)
# # par(mfrow = c(length(groups_by_factory_density), 1))
# # for (i in 1:length(groups_by_factory_density)){
# #   subgroup <- groups_by_factory_density[[i]]
# #   plot(x=subgroup$year, y=subgroup$age_adj_lung, xlab = "Year", ylab = "Age-Adjusted Lung Cancer Incidence Rate")
# # }
# 
# graph_scatter_with_line <- function(subgroup, num){
#   dev.new(width = 550, height = 330, unit = "px")
#   subgroup <- groups_by_factory_density[[num]]
#   # plot(x=subgroup$year, y=subgroup$age_adj_lung, xlab = "Year", ylab = "Age-Adjusted Lung Cancer Incidence Rate", col="blue")
#   
#   subgroup.summary <- subgroup %>% 
#     group_by(year) %>%
#     summarise(
#       len = mean(age_adj_lung),
#       sd = sd(age_adj_lung, na.rm = TRUE)
#   )
#   
#   ggplot(data=subgroup, aes(x=year, y=age_adj_lung)) +
#     geom_smooth(method="lm", se = FALSE) +
#     geom_point() +
#     stat_regline_equation()
#     geom_errorbar( aes(ymin = len-sd, ymax = len+sd),width = 0.2) 
# }
# 
# 
# graph_scatter_with_line(subgroup, 1)
# 
# 
# 
# 
# subgroup <- groups_by_factory_density[[1]]
# # plot(x=subgroup$year, y=subgroup$age_adj_lung, xlab = "Year", ylab = "Age-Adjusted Lung Cancer Incidence Rate", col="blue")
# 
# subgroup.summary <- subgroup %>% 
#   group_by(year) %>%
#   summarise(
#     len = mean(age_adj_lung),
#     sd = sd(age_adj_lung, na.rm = TRUE)
#   )
# 
# geom_errorbar(data=subgroup.summary, mapping = aes(ymin = len-sd, ymax = len+sd),width = 0.2)
# 
# ggplot(data=subgroup, aes(x=year, y=age_adj_lung)) +
#   geom_smooth(method="lm", se = FALSE) +
#   geom_point() +
#   stat_regline_equation() +
#   
#   f <- ggplot(
#     subgroup.summary, 
#     aes(x = year, y = age_adjg_lung, ymin = len-sd, ymax = len+sd)
#   )  



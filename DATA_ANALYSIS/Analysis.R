library(tidyverse)
library(Hmisc)
library(ggplot2)
library(GGally)
library(dplyr)
library(ggpubr)
library(EnvStats)
library(plm) # Panel data analysis library
library(lmtest) # For hetoroskedasticity analysis


# dataset = read.csv("DATA/data.csv")
# colnames(dataset) <- c("x", "year", "location", "ann_avg_ozone", "age_adj_lung")
# dataset$location <- str_replace(dataset$location, "南投縣", "Nantou County")
# dataset$location <- str_replace(dataset$location, "嘉義市", "Chiayi City")
# dataset$location <- str_replace(dataset$location, "嘉義縣", "Chiayi County")
# dataset$location <- str_replace(dataset$location, "基隆市", "Keelung County")
# dataset$location <- str_replace(dataset$location, "宜蘭縣", "Yilan County")
# dataset$location <- str_replace(dataset$location, "屏東縣", "Pingtung County")
# dataset$location <- str_replace(dataset$location, "彰化縣", "Changhua County")
# dataset$location <- str_replace(dataset$location, "新北市", "New Taipei City")
# dataset$location <- str_replace(dataset$location, "新竹市", "Hsinchu City")
# dataset$location <- str_replace(dataset$location, "新竹縣", "Hsinchu County")
# dataset$location <- str_replace(dataset$location, "桃園市", "Taoyuan City")
# dataset$location <- str_replace(dataset$location, "臺中市", "Taichung City")
# dataset$location <- str_replace(dataset$location, "臺北市", "Taipei City")
# dataset$location <- str_replace(dataset$location, "臺南市", "Tainang City")
# dataset$location <- str_replace(dataset$location, "花蓮縣", "Hualien County")
# dataset$location <- str_replace(dataset$location, "臺東縣", "Taitung County")
# dataset$location <- str_replace(dataset$location, "苗栗縣", "Miaoli County")
# dataset$location <- str_replace(dataset$location, "雲林縣", "Yunlin County")
# dataset$location <- str_replace(dataset$location, "高雄市", "Kaohsiung City")
# write.csv(dataset, "~/Documents/GitHub/School/bio_ia_air/DATA/data.csv")






pdf <- pdata.frame(dataset, c("location", "year"))

model <- (age_adj_lung ~ ann_avg_ozone + factory_density)
# model <- (age_adj_lung ~ ann_avg_ozone + urban_density + hospital_bed)
random <- plm(model, data = pdf, model = "random")
summary(random)
fixed <- plm(model, data = pdf, model = "within")
summary(fixed)

phtest(fixed, random)
par(las=2,                        # use perpendicular axis labels
    mar=c(10.1,10.1,6.1,2.1),      # create enough space for long x labels
    mgp=c(7.5,1,0)                  # move x axis legend down to avoid overlap
)
plotmeans(age_adj_lung ~ location, 
          main="Mean Age-Adjusted Lung Cancer Incidence Rate's\n Heterogeneity Across Taiwan's City/County", 
          ylab="Mean Age-Adjusted Lung Cancer Incidence Rate\n (Death Counts per 100,000 People) from 2011 to 2020", 
          data=dataset, 
          xlab="City/County")

plotmeans(age_adj_lung ~ year,
          main="Heterogeneity across years", 
          data=dataset,
          xlab="Year",
          ylab ="Mean Age-Adjusted Lung Cancer Incidence Rate (Death Counts per 100,000 People)")

plotmeans(ann_avg_ozone ~ location,
          main="Mean Annual Average Ozone Concentration's\n Heterogeneity Across Taiwan's City/County", 
          data=dataset,
          xlab="Year",
          ylab ="Mean Annual Average Ozone Concentration from 2011 to 2020 (ppb)")

library(foreign)
fixed.dum <- lm(age_adj_lung~ann_avg_ozone + factor(location), data = dataset)
summary(fixed.dum)


yhat <- fixed.dum$fitted
library(car)
dev.off()
scatterplot(dataset$age_adj_lung~dataset$ann_avg_ozone, 
            boxplots=FALSE, 
            xlab="Annual Average Ozone Concentration (ppb) ", 
            ylab="Age-Adjusted Lung Cancer Incidence Rate (Death Counts/100,000 People)",
            smooth=FALSE, regLine=FALSE, 
            main="Annual Average Ozone Concentration (ppb) vs. \n Age-Adjusted Lung Cancer Incidence Rate (Death Counts/100,000 People)")
dev.off()
par(las=2,                        # use perpendicular axis labels
    mar=c(4.1,4.1,22,2.1),      # create enough space for long x labels
    mgp=c(2.1,1,0)                  # move x axis legend down to avoid overlap
)
dev.off()
par(xpd=TRUE)
par(mar=c(5.1, 5.1, 4.1, 15.7))
scatterplot(dataset$age_adj_lung~dataset$ann_avg_ozone|dataset$location, 
            boxplots=FALSE, 
            xlab="Annual Average Ozone Concentration (ppb)", 
            ylab="Age-Adjusted Lung Cancer Incidence Rate (Deaths/100,000 People)",
            smooth=FALSE, regLine=FALSE,
            main="Annual Average Ozone Concentration (ppb) vs. \n Age-Adjusted Lung Cancer Incidence Rate (Death Counts/100,000 People)",
            legend =T, grid=F)

legend(title = "Locations", "bottomright", legend=unique(dataset$location), pch = seq_len(length(unique(dataset$location))), pt.cex=0.7, cex=0.7, col = seq_len(length(unique(dataset$location))), 
       ncol = 2,
       inset=c(-0.51,0)) 

dev.off()
# par(xpd=TRUE)
par(mar=c(5.1, 5.1, 4.1, 15.7))
scatterplot(yhat~dataset$ann_avg_ozone|dataset$location, boxplots=FALSE, 
            xlab="Annual Average Ozone Concentration (ppb) ", 
            ylab="Predicted Age-Adjusted Lung Cancer Incidence Rate\n(Deaths/100,000 People)",smooth=FALSE, legend=F,
            main="Annual Average Ozone Concentration (ppb) vs. \n Age-Adjusted Lung Cancer Incidence Rate (Deaths/100,000 People)", grid=F)

scatterplot(dataset$age_adj_lung~dataset$ann_avg_ozone|dataset$location, boxplots=FALSE, xlab="Annual Average Ozone Concentration (ppb) ", ylab="Age-Adjusted Lung Cancer Incidence Rate (Death Counts/100,000 People)",smooth=FALSE)

abline(lm(dataset$age_adj_lung~dataset$ann_avg_ozone), col="red", lwd=3)
legend("topleft", legend = "Regression Line when City/County is not considered", lwd=3, col = "red")
par(xpd=TRUE)
legend(title = "Locations", "bottomright", legend=unique(dataset$location), pch = seq_len(length(unique(dataset$location))), pt.cex=0.7, cex=0.7, col = seq_len(length(unique(dataset$location))), 
       ncol = 2,
       inset=c(-0.57,0)) 

ggplot(dataset, aes(x = ann_avg_ozone, y = age_adj_lung, color = location)) +
  geom_point()


# The Main One
ggplot(data=dataset, aes(x=ann_avg_ozone, y=age_adj_lung, group=1)) +
  geom_point() +
  geom_smooth(method="lm", se = TRUE) +
  stat_cor(label.y = 52) +
  stat_regline_equation(label.y = 50) +
  xlab("Annual Average Ozone Concentration (ppb)") + 
  ylab("Age-Adjusted Lung Cancer Incidence Rate (Deaths/100,000 People)") +
  labs(
    title = "Annual Average Ozone Concentration (ppb) vs. \n Age-Adjusted Lung Cancer Incidence Rate (Deaths/100,000 People)",
    caption = "Source: Taiwan Environmental Protection Agency"
  )+ 
  theme(
    axis.title = element_text(size=13),
    axis.text = element_text(size=11),
    plot.title = element_text(color = "#0099f9", size = 16, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 13, face = "bold", hjust = 0.5),
    plot.caption = element_text(face = "italic", hjust = 0)
  )

# Trying to find confounding variable
ggplot(data=dataset, aes(x=pm, y=age_adj_lung, group=1)) +
  geom_point() +
  geom_smooth(method="lm", se = TRUE) +
  stat_cor(label.y = 52) +
  stat_regline_equation(label.y = 50) +
  xlab("Factory Density (Factory/km^2)") + 
  ylab("Age-Adjusted Lung Cancer Incidence Rate (Deaths per 100,000 People)") +
  labs(
    title = "Factory Density (Factory/km^2) vs. \n Age-Adjusted Lung Cancer Incidence Rate (Death Counts per 100,000 People)",
    subtitle = "By Factory Density (Factory Counts/km2)",
    caption = "Source: Taiwan Environmental Protection Agency"
  )+ 
  theme(
    axis.title = element_text(size=15),
    axis.text = element_text(size=14),
    plot.title = element_text(color = "#0099f9", size = 20, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 13, face = "bold", hjust = 0.5),
    plot.caption = element_text(face = "italic", hjust = 0)
  )

ggplot(data=dataset, aes(x=pm, y=ann_avg_ozone, group=1)) +
  geom_point() +
  geom_smooth(method="lm", se = TRUE) +
  stat_cor(label.y = 52) +
  stat_regline_equation(label.y = 50) +
  xlab("Factory Density (Factory/km^2)") + 
  ylab("Age-Adjusted Lung Cancer Incidence Rate (Deaths per 100,000 People)") +
  labs(
    title = "Factory Density (Factory/km^2) vs. \n Age-Adjusted Lung Cancer Incidence Rate (Death Counts per 100,000 People)",
    subtitle = "By Factory Density (Factory Counts/km2)",
    caption = "Source: Taiwan Environmental Protection Agency"
  )+ 
  theme(
    axis.title = element_text(size=15),
    axis.text = element_text(size=14),
    plot.title = element_text(color = "#0099f9", size = 20, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 13, face = "bold", hjust = 0.5),
    plot.caption = element_text(face = "italic", hjust = 0)
  )

BREAKS<-c(0, 2, 4, 6, 8, 10, 12, 14)
dataset$groups = cut(dataset$factory_density,breaks=BREAKS)

# View(dataset)

# Create Color Scale
library(RColorBrewer)
myColors <- brewer.pal(19,"Set1")
names(myColors) <- levels(dataset$location)
colScale <- scale_colour_manual(name = "grp",values = myColors)

# Create the Label column that includes Sample Size
dataset <- dataset %>% left_join(dataset %>% group_by(groups) %>% summarise(N=n()))%>%
  mutate(Label=paste0('Interval: ', groups,' (Sample Size = ',N,')'))


p <- ggplot(data = dataset, aes(x = ann_avg_ozone, y = age_adj_lung)) + 
  geom_point() +
  geom_smooth(method="lm", se = TRUE) +
  stat_cor(label.y = 52) +
  stat_regline_equation(label.y = 50) +
  xlab("Annual Average Ozone Concentration (ppb)") + 
  ylab("Age-Adjusted Lung Cancer Incidence Rate (Deaths per 100,000 People)") +
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


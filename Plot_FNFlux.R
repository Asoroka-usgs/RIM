# 
# 
# 
# Purpose:  Plot Patuxent Flow Normalized Loads from Sciencebase
# 
# Author: Alex Soroka
# 
# Prior steps:  Get data on flow normalized loads from 
# https://www.sciencebase.gov/catalog/item/60d37347d34e12a1b0097243
# 



# load libraries
library(tidyverse)
library(dataRetrieval)



# read in data
data <- read.csv("D:/Datasets/RIM_Loads/RIMLoads.csv")


# rename sitenumber cause excel drops 0
data$site_no <- as.character(paste0("0",data$STAID))


# Grab patuxent only
sitedata <- data%>%
  filter(site_no == "01594440")

# set options for no scientific notation
options(scipen=10000) 


######################### nitrogen plotting ####################
unique(sitedata$PCODE)

# Codes of interest
N <- c("P00600", "P00631")


# select codes we want and filter db
dat <- sitedata%>%  
  filter(PCODE %in% N)


# rename the parms
dat$Parameter <- ifelse(dat$PCODE=="P00600","Total nitrogen","Dissolved nitrate plus nitrite")



p <- ggplot(dat, aes(Water.Year, FNLoad,shape=Parameter))+
  # ylim(0,NA)+
  xlab(paste0("Water Year"))+
  ylab(paste0("Flow normalized load in pounds per year"))+
  ggtitle(paste0("Patuxent near Bowie (USGS site 01594440) Nitrogen"))+
  # scale_fill_continuous(labels = c("Total nitrogen", "Dissolved nitrate plus nitrite"))+ 
  theme_bw()+
  scale_y_continuous(labels = scales::comma)+
  expand_limits(y = 0)+
  geom_point(size=3)+
  # labs(fill = "Parameter")+
  theme(legend.position = c(0.5, 0.8) )+
  theme(legend.title = element_blank())+
  guides(shape = guide_legend(reverse=TRUE))+
  # scale_fill_discrete(breaks = rev(levels(ToothGrowth$dose)))+ 
  # geom_vline(xintercept=2011,linetype="dotted")+
  geom_line()

p
# list[[i]]<-p

ggsave(paste0("~/RIM/Patuxent_QW_Nitrogen_Split.png"))

######################### phosphorous plotting ####################
unique(sitedata$PCODE)


# Select codes of interest 
N <- c("P00665", "P00671")


# select codes we want and filter db
dat <- sitedata%>%  
  filter(PCODE %in% N)


# rename the parms
dat$Parameter <- ifelse(dat$PCODE=="P00665","Total phosphorus","Dissolved orthophosphate")



p <- ggplot(dat, aes(Water.Year, FNLoad,shape=Parameter))+
  # ylim(0,NA)+
  xlab(paste0("Water Year"))+
  ylab(paste0("Flow normalized load in pounds per year"))+
  ggtitle(paste0("Patuxent near Bowie (USGS site 01594440) phosphorus"))+
  # scale_fill_continuous(labels = c("Total nitrogen", "Dissolved nitrate plus nitrite"))+ 
  theme_bw()+
  scale_y_continuous(labels = scales::comma)+
  expand_limits(y = 0)+
  geom_point(size=3)+
  # labs(fill = "Parameter")+
  theme(legend.position = c(0.5, 0.8) )+
  theme(legend.title = element_blank())+
  guides(shape = guide_legend(reverse=TRUE))+
  # scale_fill_discrete(breaks = rev(levels(ToothGrowth$dose)))+ 
  # geom_vline(xintercept=2011,linetype="dotted")+
  geom_line()

p


ggsave(paste0("~/RIM/Patuxent_QW_Phosphorus_Split.png"))



######################### Sediment plotting ####################
unique(sitedata$PCODE)

N <- c("P80154")


# select codes we want and filter db
dat <- sitedata%>%  
  filter(PCODE %in% N)

# We can rename the parms 
dat$Parameter <- "Sediment"


# make plot 
p <- ggplot(dat, aes(Water.Year, FNLoad,shape=Parameter))+
  # ylim(0,NA)+
  xlab(paste0("Water Year"))+
  ylab(paste0("Flow normalized load in pounds per year"))+
  ggtitle(paste0("Patuxent near Bowie (USGS site 01594440) suspended sediment "))+
  # scale_fill_continuous(labels = c("Total nitrogen", "Dissolved nitrate plus nitrite"))+ 
  theme_bw()+
  scale_y_continuous(labels = scales::comma)+
  expand_limits(y = 0)+
  geom_point(size=3)+
  # labs(fill = "Parameter")+
  theme(legend.position = "none" )+
  theme(legend.title = element_blank())+
  guides(shape = guide_legend(reverse=TRUE))+
  # scale_fill_discrete(breaks = rev(levels(ToothGrowth$dose)))+ 
  # geom_vline(xintercept=2011,linetype="dotted")+
  geom_line()

p

# save plot to file location
ggsave(paste0("~/RIM/Patuxent_QW_Sediment_Split.png"))

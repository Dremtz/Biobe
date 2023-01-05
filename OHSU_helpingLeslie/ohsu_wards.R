library(tidyverse)
library(dplyr)
library(ggplot2)
library(readxl)

setwd("C:\\Users\\Andreas\\OneDrive\\Desktop\\R\\OHSU_helpingLeslie")

warddata <- read_xlsx("C:\\Users\\Andreas\\OneDrive\\Desktop\\R\\OHSU_helpingLeslie\\Copy of COVID_patient_data_OHSU_sorted_forR_v5.xlsx")




warddata[, 14:51][is.na(warddata[, 14:51])] <- ""


warddata$all_coms <- paste(warddata$COM1, warddata$COM2, warddata$COM3, warddata$COM4, warddata$COM5, warddata$COM6, warddata$COM7, warddata$COM8, warddata$COM9, warddata$COM10, warddata$COM11, warddata$COM12, warddata$COM13)
                            
warddata$all_cs <- paste(warddata$CS1, warddata$CS2, warddata$CS3, warddata$CS4, warddata$CS5, warddata$CS6, warddata$CS7, warddata$CS8, warddata$CS9, warddata$CS10, warddata$CS11, warddata$CS12, warddata$CS13, warddata$CS14, warddata$CS15, warddata$CS16, warddata$CS17, warddata$CS18, warddata$CS19, warddata$CS20, warddata$CS21, warddata$CS22, warddata$CS23, warddata$CS24, warddata$CS25, warddata$CS26)

ggplot(warddata, aes(y = Ct_Value, x = all_coms)) +
  geom_point()

library(readxl)
library(effsize)

setwd('/Users/esbl/Dropbox (University of Oregon)/Projects/Active_Air_Cleaning _Projects/DAT_Project Data/R_Data/Genesis_DATA')

ms2 <- read.csv('/Users/esbl/Dropbox (University of Oregon)/Projects/Active_Air_Cleaning _Projects/DAT_Project Data/R_Data/Genesis_DATA/GE_SEPI_DATA/sepi_matchedpair.csv')

View(ttest)
ms2T0 <- ms2[grepl('T15', ms2$Time.Series), ]

t.test (ms2T0$Before, ms2T0$After, conf=0.95, paired = T)

boxplot(ms2T0$Before, ms2T0$After)

sepi <- read.csv('/Users/esbl/Dropbox (University of Oregon)/Projects/Active_Air_Cleaning _Projects/DAT_Project Data/R_Data/Genesis_DATA/GE_matchedpair.csv')

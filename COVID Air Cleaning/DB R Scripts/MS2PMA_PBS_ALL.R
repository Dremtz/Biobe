library(ggplot2)
library(ggsignif)
library(dplyr)
library(ggplot2)

setwd ("/Users/esbl/Dropbox (University of Oregon)/Projects/Active_Air_Cleaning _Projects/DAT_Project Data/R_Data/MS2_DATA")



####READ PMA PLATE 1#####
qp1 <- read.csv('/Users/esbl/Dropbox (University of Oregon)/Projects/Active_Air_Cleaning _Projects/DAT_Project Data/R_Data/MS2_DATA/DB_RAW_RESULTS_MS2_PMA1.csv', header = T,skip = 23, stringsAsFactors = F)

qp1[qp1 == "Undetermined"] <- NA

qp1.1 <- qp1 %>%
  dplyr::group_by(Sample) %>% #, run) %>%
  dplyr::summarise(Cq = mean(as.numeric(as.character(Cq)))) %>%
  as.data.frame()

# Make an object with just our standards
standards1 <- qp1.1[grepl('Standard', qp1.1$Sample), ]
standards1 <- standards1[!grepl('Standard 7', standards1$Sample),]
 
# Remove the standards from the data set with all the other samples    
qp1.1 <- qp1.1[!grepl('Standard', qp1.1$Sample), ]

# Describe our dilution curve and how many gene copies are in each standard
dil1 <- c(1,
         .1,
         .001,
         .0001, 
         .00001)
standards1 <- cbind(standards1, dil1)

copy.1 <- c(292000000,
            29200000.0,
            292000.000,
            29200.0000,
            2920.00000)

# Now we can combine everything and make our standard curve
dat1 <- data.frame('Sample'= standards1$Sample, 'copy' = copy.1, 'log.copy' = log10(copy.1), 'Ct' = standards1$Cq)

fit1 <- lm(Ct ~ log.copy, data = dat1)

line1 <- lm(Ct ~ log.copy, data = dat1)

ab1 <- coef(line1)

ggplot(dat1, aes(x = log.copy, y = Ct, colour= Sample)) +
  theme_classic() +
  xlab(expression('log'['10']*' Copy No. / µL standard')) +
  ylab(expression('C'['T'])) +
  labs(subtitle = paste("Adj R2 = ",signif(summary(fit1)$adj.r.squared, 5),
                        "Intercept =",signif(fit1$coef[[1]],5 ),
                        " Slope =",signif(fit1$coef[[2]], 5),
                        " P =",signif(summary(fit1)$coef[2,4], 5))) +
  geom_point(size = 2) +
  geom_line(aes(group=as.factor(Sample))) +  
  stat_smooth(method = 'lm', formula = y ~ x, level = 0, size = 0.75, col = "black")

convert <- function(y, b, a){
  x <- 10^((y - b) / a)
  x
}

qp1.1$PMAcounts <- convert(y = as.numeric(qp1.1$Cq), b = ab1[1], a = ab1[2])


NTC.1 <- qp1.1[grepl('ntc', qp1.1$Sample), ]

NTC.1 <- as.numeric(NTC.1$PMAcounts)

NTC.1

qp1.1

write.csv(qp1.1, "DB_RESULTS_MS2_PMA1.csv")



  
#####2 REPEAT FOR PMA PLATE 2#####
qp2 <- read.csv('/Users/esbl/Dropbox (University of Oregon)/Projects/Active_Air_Cleaning _Projects/DAT_Project Data/R_Data/MS2_DATA/DB_RAW_RESULTS_MS2_PMA2.csv', header = T,skip = 23, stringsAsFactors = F)

qp2[qp2 == "Undetermined"] <- NA

qp2.1 <- qp2 %>%
  dplyr::group_by(Sample) %>% #, run) %>%
  dplyr::summarise(Cq = mean(as.numeric(as.character(Cq)))) %>%
  as.data.frame()


# Make an object with just our standards
standards2 <- qp2.1[grepl('Standard', qp2.1$Sample), ]

# Remove the standards from the data set with all the other samples    
qp2.1 <- qp2.1[!grepl('Standard', qp2.1$Sample), ]

# Describe our dilution curve and how many gene copies are in each standard
dil2 <- c(1,
         .1,
         .01,
         .001,
         .0001, 
         .00001,
         .000001)

standards2 <- cbind(standards2, dil2)

copy.2 <- c(292000000,
            29200000.0,
            2920000.00,
            292000.000,
            29200.0000,
            2920.00000,
            292.000000)


# Now we can combine everything and make our standard curve
dat2 <- data.frame('Sample'= standards2$Sample, 'copy' = copy.2, 'log.copy' = log10(copy.2), 'Ct' = standards2$Cq)

fit2 <- lm(Ct ~ log.copy, data = dat2)

line2 <- lm(Ct ~ log.copy, data = dat2)

ab2 <- coef(line2)

ggplot(dat2, aes(x = log.copy, y = Ct, colour= Sample)) +
  theme_classic() +
  xlab(expression('log'['10']*' Copy No. / µL standard')) +
  ylab(expression('C'['T'])) +
  labs(subtitle = paste("Adj R2 = ",signif(summary(fit2)$adj.r.squared, 5),
                        "Intercept =",signif(fit2$coef[[1]],5 ),
                        " Slope =",signif(fit2$coef[[2]], 5),
                        " P =",signif(summary(fit2)$coef[2,4], 5))) +
  geom_point(size = 2) +
  geom_line(aes(group=as.factor(Sample))) +  
  stat_smooth(method = 'lm', formula = y ~ x, level = 0, size = 0.75, col = "black")

convert <- function(y, b, a){
  x <- 10^((y - b) / a)
  x
}

qp2.1$PMAcounts <- convert(y = as.numeric(qp2.1$Cq), b = ab2[1], a = ab2[2])


NTC.2 <- qp2.1[grepl('ntc', qp2.1$Sample), ]

NTC.2 <- as.numeric(NTC.2$PMAcounts)

NTC.2


write.csv(qp2.1, "DB_RESULTS_MS2_PMA2.csv")
qp1.3 = rbind(qp1.1, qp2.1)


meta = read.csv("/Users/esbl/Dropbox (University of Oregon)/Projects/Active_Air_Cleaning _Projects/DAT_Project Data/DBEngineering_AtmosAir_Metadata_R_PMA.csv")

DB_RESULTS_MS2_PMA_COMBINED <- cbind(meta, qp1.3[match(meta$Sample.Number, qp1.3$Sample), ])
DB_RESULTS_MS2_PMA_COMBINED$Sample <- NULL


write.csv(DB_RESULTS_MS2_PMA_COMBINED, "DB_RESULTS_MS2_PMA_COMBINED.csv")


DB_RESULTS_MS2_PMA_COMBINED[is.na(DB_RESULTS_MS2_PMA_COMBINED)] <- 0


#####3 PBS PLATE 1####


qp3 <- read.csv('/Users/esbl/Dropbox (University of Oregon)/Projects/Active_Air_Cleaning _Projects/DAT_Project Data/R_Data/MS2_DATA/DB_RAW__RESULTS_MS2_PBS1A.csv', header = T,skip = 23, stringsAsFactors = F)

qp3[qp3 == "Undetermined"] <- NA

qp3.1 <- qp3 %>%
  dplyr::group_by(Sample) %>% #, run) %>%
  dplyr::summarise(Cq = mean(as.numeric(as.character(Cq)))) %>%
  
  
  as.data.frame()

# Make an object with just our standards
standards3 <- qp3.1[grepl('Standard', qp3.1$Sample), ]


# Remove the standards from the data set with all the other samples    
qp3.1 <- qp3.1[!grepl('Standard', qp3.1$Sample), ]

# Describe our dilution curve and how many gene copies are in each standard
dil3 <- c(1,
         .1,
         .01,
         .0001, 
         .00001,
         .000001)
standards3 <- cbind(standards3, dil3)


copy.3 <- c(292000000,
             29200000.0,
             2920000.00,
             29200.0000,
             2920.00000,
             292.000000)

# Now we can combine everything and make our standard curve
dat3 <- data.frame('Sample'= standards3$Sample, 'copy' = copy.3, 'log.copy' = log10(copy.3), 'Ct' = standards3$Cq)

fit3 <- lm(Ct ~ log.copy, data = dat3)

line3 <- lm(Ct ~ log.copy, data = dat3)

ab3 <- coef(line1)

ggplot(dat3, aes(x = log.copy, y = Ct, colour= Sample)) +
  theme_classic() +
  xlab(expression('log'['10']*' Copy No. / µL standard')) +
  ylab(expression('C'['T'])) +
  labs(subtitle = paste("Adj R2 = ",signif(summary(fit3)$adj.r.squared, 5),
                        "Intercept =",signif(fit3$coef[[1]],5 ),
                        " Slope =",signif(fit3$coef[[2]], 5),
                        " P =",signif(summary(fit3)$coef[2,4], 5))) +
  geom_point(size = 2) +
  geom_line(aes(group=as.factor(Sample))) +  
  stat_smooth(method = 'lm', formula = y ~ x, level = 0, size = 0.75, col = "black")

convert <- function(y, b, a){
  x <- 10^((y - b) / a)
  x
}

qp3.1$PBScounts <- convert(y = as.numeric(qp3.1$Cq), b = ab3[1], a = ab3[2])


NTC.3 <- qp3.1[grepl('ntc', qp3.1$Sample), ]

NTC.3 <- as.numeric(NTC.3$PBScounts)

NTC.3

qp3.1[is.na(qp3.1)] <- 0

write.csv(qp3.1, "DB_RESULTS_MS2_PBS1.csv")




####4 PBS PLATE 2####
qp4 <- read.csv('/Users/esbl/Dropbox (University of Oregon)/Projects/Active_Air_Cleaning _Projects/DAT_Project Data/R_Data/MS2_DATA/DB_RAW_RESULTS_MS2_PBS2.csv', header = T,skip = 23, stringsAsFactors = F)

qp4[qp4 == "Undetermined"] <- NA

qp4.1 <- qp4 %>%
  dplyr::group_by(Sample) %>% #, run) %>%
  dplyr::summarise(Cq = mean(as.numeric(as.character(Cq)))) %>%
  as.data.frame()


# Make an object with just our standards
standards4 <- qp4.1[grepl('Standard', qp4.1$Sample), ]

# Remove the standards from the data set with all the other samples    
qp4.1 <- qp4.1[!grepl('Standard', qp4.1$Sample), ]

# Describe our dilution curve and how many gene copies are in each standard
dil4 <- c(1,
         .1,
         .01,
         .001,
         .0001, 
         .00001,
         .000001)

standards4 <- cbind(standards4, dil4)

copy.4 <- c(292000000,
             29200000.0,
             2920000.00,
             292000.000,
             29200.0000,
             2920.00000,
             292.000000)

# Now we can combine everything and make our standard curve
dat4 <- data.frame('Sample'= standards4$Sample, 'copy' = copy.4, 'log.copy' = log10(copy.4), 'Ct' = standards4$Cq)

fit4 <- lm(Ct ~ log.copy, data = dat4)

line4 <- lm(Ct ~ log.copy, data = dat4)

ab4 <- coef(line4)

ggplot(dat4, aes(x = log.copy, y = Ct, colour= Sample)) +
  theme_classic() +
  xlab(expression('log'['10']*' Copy No. / µL standard')) +
  ylab(expression('C'['T'])) +
  labs(subtitle = paste("Adj R2 = ",signif(summary(fit1)$adj.r.squared, 5),
                        "Intercept =",signif(fit4$coef[[1]],5 ),
                        " Slope =",signif(fit4$coef[[2]], 5),
                        " P =",signif(summary(fit4)$coef[2,4], 5))) +
  geom_point(size = 2) +
  geom_line(aes(group=as.factor(Sample))) +  
  stat_smooth(method = 'lm', formula = y ~ x, level = 0, size = 0.75, col = "black")

convert <- function(y, b, a){
  x <- 10^((y - b) / a)
  x
}

qp4.1$PBScounts <- convert(y = as.numeric(qp4.1$Cq), b = ab4[1], a = ab4[2])


NTC.4 <- qp4.1[grepl('ntc', qp4.1$Sample), ]

NTC.4 <- as.numeric(NTC.4$PBScounts)

NTC.4

write.csv(qp4.1, "DB_RESULTS_MS2_PBS2.csv")
qp4.3 = rbind(qp3.1, qp4.1)
qp4.3[is.na(qp4.3)] <- 0

#Bind to metadata sheet
meta = read.csv("/Users/esbl/Dropbox (University of Oregon)/Projects/Active_Air_Cleaning _Projects/DAT_Project Data/DBEngineering_AtmosAir_Metadata_R_PMA.csv")
DB_RESULTS_MS2_PBS_COMBINED <- cbind(meta, qp4.3[match(meta$Sample.Number, qp4.3$Sample), ])


DB_RESULTS_MS2_PBS_COMBINED$Sample <- NULL
write.csv(DB_RESULTS_MS2_PBS_COMBINED, "DB_RESULTS_MS2_PBS_COMBINED.csv")

#set NAs to 0
DB_RESULTS_MS2_PBS_COMBINED[is.na(DB_RESULTS_MS2_PBS_COMBINED)] <- 0
unique(DB_RESULTS_MS2_PBS_COMBINED$PBScounts)



###########BIND#############

qp4.3_final <- qp4.3%>%
  mutate(Sample.Number = Sample) %>%
  mutate(PBS_CQ = Cq) %>% 
  select(Sample.Number, PBS_CQ, PBScounts) 

#remove spaces - can probably delete this
'qp4.3_final$Sample.Number <- trimws(qp4.3_final$Sample.Number, which = c("left"))
DB_RESULTS_MS2_PMA_COMBINED$Sample.Number <- trimws(DB_RESULTS_MS2_PMA_COMBINED$Sample.Number, which = c("left"))'


MS2_bound <- inner_join(qp4.3_final, DB_RESULTS_MS2_PMA_COMBINED)
write.csv(MS2_bound, "DB_MS2_COMPLETE.csv")



MS2_COMPLETE_CLEANED <- MS2_bound[!grepl('1B|2A|3A|4A', MS2_bound$Sample.Day), ]
write.csv(MS2_COMPLETE_CLEANED, "DB_MS2_COMPLETE_CLEANED.csv")


#calculate the normilization
df_calc <- MS2_COMPLETE_CLEANED %>% 
  mutate(normalize = round((PBScounts/PMAcounts), 10)) 
df_calc[is.na(df_calc)] <- 0



# Packages
library(readxl)
library(dplyr)
library(mailR)
library(rJava)

# Where are we
setwd("C:\\Users\\ano\\Dropbox (University of Oregon)\\Projects\\PCEF\\DATA")


meta = read_xlsx("C:\\Users\\ano\\Dropbox (University of Oregon)\\Projects\\PCEF\\DATA\\PCEF METADATA.xlsx")

# What are our files? 
files = list.files("C:\\Users\\ano\\Dropbox (University of Oregon)\\Projects\\PCEF\\DATA\\Raw Data")

Cqs = data_frame()

for (f in files) {
  setwd("C:\\Users\\ano\\Dropbox (University of Oregon)\\Projects\\PCEF\\DATA\\Raw Data")
  qp1 <- read.csv(f, header = T,skip = 23, stringsAsFactors = F) 
  
  qp1[qp1 == "Undetermined"] <- 40
  
  S = qp1[grepl("S", qp1$Target), ]
  S = S[!grepl("MS2", S$Target), ]
  S = S[!grepl("16S", S$Target), ]
  N = qp1[grepl("N", qp1$Target), ]
  N = N[!grepl("RNaseP", N$Target), ]
  ORF = qp1[grepl("ORF1ab", qp1$Target), ]
  MS2 = qp1[grepl("MS2", qp1$Target), ]
  
  combined = data.frame("Sample" = S$Sample, 'S' = S$Cq, 'N' = N$Cq, 'ORF1ab' = ORF$Cq, 'MS2' = MS2$Cq)
  
  Cqs = rbind(Cqs, combined) }

Cqs$S = as.numeric(Cqs$S)
Cqs$N = as.numeric(Cqs$N)
Cqs$ORF1ab = as.numeric(Cqs$ORF1ab)
Cqs$MS2 = as.numeric(Cqs$MS2)

Cqs$N.temp = 0
Cqs$S.temp = 0
Cqs$ORF.temp = 0
Cqs$N.temp[Cqs$N<35]  <- 1
Cqs$S.temp[Cqs$S<35]  <- 1
Cqs$ORF.temp[Cqs$ORF1ab<35]  <- 1

Cqs = Cqs %>% mutate(temp = select(., N.temp, S.temp, ORF.temp) %>% rowSums(na.rm = TRUE))

Cqs$Result = "Negative"
Cqs$Result[Cqs$temp==2]  <- "Positive (2/3)"
Cqs$Result[Cqs$temp==3]  <- "Positive (3/3)"
Cqs$Ct = rowMeans(Cqs[,c("N", "ORF1ab", "S")], na.rm=TRUE)
Cqs$ORF1ab[Cqs$ORF1ab>35]  <- 'No Amp'
Cqs$S[Cqs$S>35]  <- 'No Amp'
Cqs$N[Cqs$N>35]  <- 'No Amp'
Cqs$MS2[Cqs$MS2>35]  <- 'No Amp'
#Cqs$Ct[Cqs$Ct>35]  <- 'Negative'

output3 = data.frame("Sample" = Cqs$Sample, 
                     'N' = Cqs$N, 
                     'ORF1ab' = Cqs$ORF1ab, 
                     'S' = Cqs$S, 
                     'MS2' = Cqs$MS2, 
                     'Result' = Cqs$Result, 
                     'Ct' = Cqs$Ct)

output3$N = as.numeric(output3$N)
output3$ORF1ab = as.numeric(output3$ORF1ab)
output3$S = as.numeric(output3$S)
output3$MS2 = as.numeric(output3$MS2)

is.num <- sapply(output3, is.numeric)
output3[is.num] <- lapply(output3[is.num], round, 1)
output3[is.na(output3)] <- "No Amp"


output3 <- cbind(meta, output3[match(meta$Sample, output3$Sample), ])
output3$Sample <- NULL

write.csv(output3, "C:\\Users\\ano\\Dropbox (University of Oregon)\\Projects\\PCEF\\RESULTS\\PCEF Results.csv")


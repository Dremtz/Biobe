# Packages
library(readxl)
library(dplyr)
library(mailR)
library(rJava)

# Where are we
setwd('/Users/esbl/Dropbox (University of Oregon)/Projects/Active_Air_Cleaning _Projects/DAT_Project Data')

currentDate <- Sys.Date()
meta = read_xlsx("/Users/esbl/Dropbox (University of Oregon)/Projects/Active_Air_Cleaning _Projects/DAT_Project Data/DB_covid_metadata.xlsx")
write.csv(meta, paste("/Users/esbl/Dropbox (University of Oregon)/Projects/Active_Air_Cleaning _Projects/DAT_Project Data/Metadata_record/Metadata-",currentDate,".csv",sep=""))

# What are our files? 
files = list.files('/Users/esbl/Dropbox (University of Oregon)/Projects/Active_Air_Cleaning _Projects/DAT_Project Data/R_Data/Covid Data/')

Cqs = data_frame()

for (f in files) {
  setwd('/Users/esbl/Dropbox (University of Oregon)/Projects/Active_Air_Cleaning _Projects/DAT_Project Data/R_Data/Covid Data/')
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

output1 = data.frame("Sample" = Cqs$Sample, 
                     'N' = Cqs$N, 
                     'ORF1ab' = Cqs$ORF1ab, 
                     'S' = Cqs$S, 
                     'MS2' = Cqs$MS2, 
                     'Result' = Cqs$Result, 
                     'Ct' = Cqs$Ct)

output1$N = as.numeric(output1$N)
output1$ORF1ab = as.numeric(output1$ORF1ab)
output1$S = as.numeric(output1$S)
output1$MS2 = as.numeric(output1$MS2)

is.num <- sapply(output1, is.numeric)
output1[is.num] <- lapply(output1[is.num], round, 1)
output1[is.na(output1)] <- "No Amp"


output1 <- cbind(meta, output1[match(meta$`Sample Number`, output1$Sample), ])
output1$Sample <- NULL

write.csv(output1, "/Users/esbl/Dropbox (University of Oregon)/Projects/Active_Air_Cleaning _Projects/Results/DB Results/DB_COVID_RESULTS.csv")

###UPDATE WITH NEW EMAIL INFO###
#send.mail(from="horves16@gmail.com",
#          to="kevinvdw@uoregon.edu",
#          subject="Smith's Detection RESULTS",
#          body="A new batch of Smith's Detection results are available. You can find the updated results sheet at #https://www.dropbox.com/s/v0kvpb6y7d75jt1/Results.csv?dl=0.",
#          html=T,
#          smtp=list(host.name = "smtp.gmail.com",
#                    port = 465,
#                    user.name = "horves16@gmail.com",
#                    passwd = "Pizzas16",
#                    ssl = T),
#          authenticate=T)
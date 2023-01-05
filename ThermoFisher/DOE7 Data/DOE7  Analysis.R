install.packages("polycor")
library(ggplot2)
library(tidyverse)
library(polycor)



setwd("C:\\Users\\ano\\Dropbox (University of Oregon)\\Projects\\ThermoFisher\\DOE7")

input <- read.csv("C:\\Users\\ano\\Dropbox (University of Oregon)\\Projects\\ThermoFisher\\DOE7\\DOE7 Results.csv", stringsAsFactors = FALSE)
input[input== '']= NA

inputclean <- input %>%
  filter(Trial.Day == '4' | Trial.Day == '5') %>%
  filter(Trial.Number != 'NA') %>%
  filter(Mesa.Result != 'NA') %>%
  filter(Sample != '57')

inputclean$pcrnumeric = '0'
inputclean$pcrnumeric[inputclean$Result == 'Positive (3/3)'] <- 1
inputclean$pcrnumeric[inputclean$Result == 'Positive (2/3)'] <- 1

inputclean$mesanumeric = '0'
inputclean$mesanumeric[inputclean$Mesa.Result =='Positive'] <-1


polychor(inputclean$pcrnumeric, inputclean$mesanumeric)


#POLYCHORIC CORELLATION COEFFICIENTS 
#Binary neg,pos 1,0                 0.875918
#Ordered w/ PCR 0,1,2               0.8149759

#0.9-1.0 "very highly correlated"
#0.7-0.9 "highly correlated"
#0.5-0.7 "moderately
#0.3-0.5 "low"
#<0.3 "little if any"
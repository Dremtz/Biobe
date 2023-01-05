require(plyr)
library(ggplot2)
library(ggsignif)
library(dplyr)
library(ggplot2)
library(reshape2)
library(scales)
library(wesanderson)


setwd ("/Users/esbl/Dropbox (University of Oregon)/Projects/Active_Air_Cleaning _Projects/DAT_Project Data/R_Data/MS2_DATA")

input <- read.csv('DB_MS2_COMPLETE_CLEANED.csv')
input_cleaned <- select(input, Sample.Number, Sample.Type, Sample.Day, Time.Series, Treatment, PMAcounts, PBScounts)
grouped <- ddply(input_cleaned, .(Sample.Type, Sample.Day, Treatment, Time.Series), summarise, pbs_sum = sum(PBScounts), pma_sum = sum(PMAcounts))

#NORMALIZE#######
df_calc <- grouped %>% 
  mutate(normalize = round((pma_sum/pbs_sum), 1000)) 
df_calc[is.na(df_calc)] <- 0
#set normalized values >1 to 1
df_calc$normalize[(df_calc$normalize > 1)] <- 1

pal = wes_palette("GrandBudapest2")

#PLOT ALL VIRUS BY TEST TYPE#######


#plot
ggplot(df_calc, aes(x = Treatment, y = normalize)) +
  geom_boxplot(outlier.shape = NA, aes(fill = Treatment), alpha = 0.5) +
  geom_jitter(aes(fill = Treatment, color = Time.Series,), size = 2, alpha = .7,) +
  geom_signif(comparisons = list(c("Control", "Experimental")),
              map_signif_level = FALSE, tip_length = 0.01,
              test = t.test, y_position = 1.) +
  facet_wrap(~Sample.Type) +
  theme_classic() +
  labs(title = "Combined Time Series By Test Type - Virus", y = "Viable/Total Virus") +
  scale_y_continuous(breaks = c(0, 0.25, 0.5, 0.75, 1), limits = c(-0.01, 1.1)) +
  scale_fill_manual(values = c(pal[2], pal[3])) +
  theme(legend.title = element_blank())


#PLOT DAILY PBS NEB TOTALS T0############

T0 <- input[grepl('35|33', input$Sample.Number), ]
T0.clean <- T0[!grepl('T', T0$Sample.Number), ]


ggplot(T0.clean, aes(x = factor(Sample.Day), y = PBScounts, fill = factor(Sample.Type))) +
  geom_bar(stat = "identity", alpha =.7, position = "dodge") +
  theme_classic() +
  labs(title = "Daily High Flow Air Samplers Capture Total - Virus", fill = " ", x = "Sample Day", y = " Gene Copies/Microiter") +
  scale_y_continuous(label = comma, limits = c(0, 20000000))



#PLOT by Time Series###############

ggplot(df_calc, aes(x = Treatment, y = normalize)) +
  geom_boxplot(outlier.shape = NA, aes(fill = Treatment), alpha = 0.5) +
  geom_jitter(aes(fill = Treatment, color = Sample.Type,), size = 1, alpha = .7,) +
  geom_signif(comparisons = list(c("Control", "Experimental")),
              map_signif_level = FALSE, tip_length = 0.01,
              test = t.test, y_position = 1) +
  facet_wrap(~Time.Series) +
  theme_classic() +
  labs(title = "All Test Types By Time Series - Virus", y = "Viable/Total Virus") +
  scale_y_continuous(breaks = c(0, 0.25, 0.5, 0.75, 1), limits = c(-0.01, 1.1)) +
  scale_fill_manual(values = c(pal[2], pal[3])) +
  theme(legend.title = element_blank())

########PLOT ALL TIME SERIES TOGETHER####
ggplot(df_calc, aes(x = Treatment, y = normalize)) +
  geom_boxplot(outlier.shape = NA, aes(fill = Treatment), alpha = 0.5) +
  geom_jitter(aes(fill = Treatment, color = Time.Series,), size = 2, alpha = .7,) +
  geom_signif(comparisons = list(c("Control", "Experimental")),
              map_signif_level = FALSE, tip_length = 0.01,
              test = t.test, y_position = 1) +
  theme_classic() +
  labs(title = "All Samples By Treatment - Virus", y = "Viable/Total Virus") +
  scale_y_continuous(breaks = c(0, 0.25, 0.5, 0.75, 1), limits = c(-0.01, 1.1)) +
  scale_fill_manual(values = c(pal[2], pal[3])) +
  theme(legend.title = element_blank())


####PLOT HIFLOW AND LOW FLOW####

hiflow <- df_calc[grepl('Aerosol Sense|Bobcat', df_calc$Sample.Type), ]

ggplot(hiflow, aes(x = Treatment, y = normalize)) +
  geom_boxplot(outlier.shape = NA, aes(fill = Treatment), alpha = 0.5) +
  geom_jitter(aes(fill = Treatment, color = Time.Series,), size = 2, alpha = .7,) +
  geom_signif(comparisons = list(c("Control", "Experimental")),
              map_signif_level = FALSE, tip_length = 0.01,
              test = t.test) +
  theme_classic() +
  labs(title = "High Flow Air Samplers Only - Virus", y = "Viable/Total Virus") +
  scale_fill_manual(values = c(pal[2], pal[3])) +
  theme(legend.title = element_blank())

loflow <- df_calc[grepl('SKC|Universal Spot|BSV 8-liter', df_calc$Sample.Type), ]

ggplot(loflow, aes(x = Treatment, y = normalize)) +
  geom_boxplot(outlier.shape = NA, aes(fill = Treatment), alpha = 0.5) +
  geom_jitter(aes(fill = Treatment, color = Time.Series,), size = 2, alpha = .7,) +
  geom_signif(comparisons = list(c("Control", "Experimental")),
              map_signif_level = FALSE, tip_length = 0.01,
              test = t.test, y_position = 1) +
  theme_classic() +
  labs(title = "Low Flow Air Samplers Only - Virus", y = "Viable/Total Virus") +
  scale_y_continuous(breaks = c(0, 0.25, 0.5, 0.75, 1), limits = c(-0.01, 1.1)) +
  scale_fill_manual(values = c(pal[2], pal[3])) +
  theme(legend.title = element_blank())

##########PLOT JUST VIABLE######## FIX#########

ggplot(input_cleaned, aes(x = Treatment, y = log10(PMAcounts),)) +
  geom_boxplot(outlier.shape = NA, aes(fill = Treatment), alpha = 0.5) +
  geom_jitter(aes(fill = Treatment, color = Sample.Type,), size = 2, alpha = .7,) +
  geom_signif(comparisons = list(c("Control", "Experimental")),
              map_signif_level = FALSE, tip_length = 0.01,
              test = t.test) +
  facet_wrap(~Time.Series) +
  theme_classic() +
  labs(title = "All Test Types By Time Series - Viable Virus Only", y = "Viable Virus Log10 Scale") +
  scale_fill_manual(values = c(pal[2], pal[3])) +
  theme(legend.title = element_blank())

View(input_cleaned)
####SETTLING PLATE ONLY####
plate_calc <- input_cleaned %>% 
  mutate(normalize = round((PMAcounts/PBScounts), 1000)) 
plate_calc[is.na(plate_calc)] <- 0

#set normalized values >1 to 1
plate_calc$normalize[(plate_calc$normalize > 1)] <- 1
plate_calc <- plate_calc[grepl('Settling Plate', plate_calc$Sample.Type), ]

ggplot(plate_calc, aes(x = Treatment, y = normalize)) +
  geom_boxplot(outlier.shape = NA, aes(fill = Treatment), alpha = 0.5) +
  geom_jitter(aes(fill = Treatment, color = Time.Series),height = .01, size = 2, alpha = .7,) +
  geom_signif(comparisons = list(c("Control", "Experimental")),
              map_signif_level = FALSE, tip_length = 0.01,
              test = t.test, y_position = 1) +
  theme_classic() +
  labs(title = "Settling Plates Only - Virus", y = "Viable/Total Virus") +
  scale_y_continuous(breaks = c(0, 0.25, 0.5, 0.75, 1), limits = c(-0.01, 1.1)) +
  scale_fill_manual(values = c(pal[2], pal[3])) +
  theme(legend.title = element_blank())

########CONTROL V T60#####

controlT0 <- df_calc[grepl('T0', df_calc$Time.Series), ]
controlT0 <- controlT0[grepl("Control", controlT0$Treatment),]

expT60 <- df_calc[grepl('T6', df_calc$Time.Series), ]
expT60 <- expT0[grepl("Experimental", expT0$Treatment),]

controlvexp <- rbind(controlT0, expT60)

ggplot(controlvexp, aes(x = Treatment, y = normalize)) +
  geom_boxplot(outlier.shape = NA, aes(fill = Treatment), alpha = 0.5) +
  geom_jitter(aes(fill = Treatment, color = Sample.Type,), size = 2, alpha = .7,) +
  geom_signif(comparisons = list(c("Control", "Experimental")),
              map_signif_level = FALSE, tip_length = 0.01,
              test = t.test, y_position = 1) +
  theme_classic() +
  labs(title = "Control T0 vs Experimental T60 - Virus", y = "Viable/Total Virus") +
  scale_y_continuous(breaks = c(0, 0.25, 0.5, 0.75, 1), limits = c(-0.01, 1.1)) +
  scale_fill_manual(values = c(pal[2], pal[3])) +
  theme(legend.title = element_blank())


###JUNK#####
#PLOT Nebulization Quantity PMA OVER PBS############

"ggplot() +
  geom_bar(T0.clean, mapping = aes(x = Sample.Day, y = PBScounts, fill = factor(Sample.Type)),  alpha = .3,  position = "dodge", stat = "identity") +
  geom_bar(T0.clean, mapping = aes(x = Sample.Day, y = PMAcounts, color = factor(Sample.Type)),fill = NA, position = "dodge", stat = "identity" ) +
  theme_classic() +
  labs(title = "Daily Nebulization Totals", fill = " ", x = "Sample Day", y = " Gene Copies/Liter") +
  scale_y_continuous(label = comma, limits = c(0, 25000000))"

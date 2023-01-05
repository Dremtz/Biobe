require(plyr)
library(ggplot2)
library(ggsignif)
library(dplyr)
library(ggplot2)
library(reshape2)
library(scales)
library(wesanderson)


setwd ("/Users/esbl/Dropbox (University of Oregon)/Projects/Active_Air_Cleaning _Projects/DAT_Project Data/R_Data/Genesis_DATA/GE_SEPI_DATA")


input <- read.csv('GE_SEPI_COMPLETE_CLEANED.csv')
input_cleaned <- select(input, Sample.Type, Sample.Day, Time.Series, Treatment, PMAcounts, PBScounts)

#grep just AS and group to average 2 daily samples and match input_cleaned
asgrep <- input_cleaned[grepl('AS', input_cleaned$Sample.Type),]
asgroup <- ddply(asgrep, .(Sample.Type, Sample.Day, Treatment, Time.Series), summarise, PBScounts = sum(PBScounts), PMAcounts = sum(PMAcounts))

#remove AS from input_clean and bind
input_drop <- input_cleaned[!grepl('AS', input_cleaned$Sample.Type),]
input_cleaned <- rbind(input_drop, asgroup)




#NORMALIZE#######
df_calc <- input_cleaned %>% 
  mutate(normalize = round((PMAcounts/PBScounts), 1000)) 
df_calc[is.na(df_calc)] <- 0
#set normalized values >1 to 1
df_calc$normalize[(df_calc$normalize > 1)] <- 1

pal = wes_palette("Darjeeling1")

#PLOT ALL Bacteria BY TEST TYPE#######


#plot
ggplot(df_calc, aes(x = Treatment, y = normalize)) +
  geom_boxplot(outlier.shape = NA, aes(fill = Treatment), alpha = 0.5) +
  geom_jitter(aes(fill = Treatment, color = Time.Series,), size = 2, alpha = .7,) +
  geom_signif(comparisons = list(c("Control", "Experimental")),
              map_signif_level = FALSE, tip_length = 0.01,
              test = t.test, y_position = 1.) +
  facet_wrap(~Sample.Type) +
  theme_classic() +
  labs(title = "Combined Time Series By Test Type - Bacteria", y = "Viable/Total Bacteria") +
  scale_y_continuous(breaks = c(0, 0.25, 0.5, 0.75, 1), limits = c(-0.01, 1.1)) +
  theme(legend.title = element_blank()) +
  scale_fill_manual(values = c(pal[2], pal[3]))


#PLOT DAILY PBS NEB TOTALS T0############

T0 <- input[grepl('35|33', input$Sample.Number), ]
T0.clean <- T0[!grepl('T', T0$Sample.Number), ]

ggplot(T0.clean, aes(x = factor(Sample.Day), y = PBScounts, fill = factor(Sample.Type))) +
  geom_bar(stat = "identity", alpha =.7, position = "dodge") +
  theme_classic() +
  labs(title = "Daily High Flow Air Sampler Capture Totals - Bacteria", fill = " ", x = "Sample Day", y = " Gene Copies/Microliter") +
  scale_y_continuous(label = comma) +
  scale_fill_manual(values = c(pal[2], pal[3]))


#PLOT by Time Series###############

ggplot(df_calc, aes(x = Treatment, y = normalize)) +
  geom_boxplot(outlier.shape = NA, aes(fill = Treatment), alpha = .5, ) +
  geom_jitter(aes(fill = Treatment, color = Sample.Type,), size = 2, alpha = .7,) +
  geom_signif(comparisons = list(c("Control", "Experimental")),
              map_signif_level = FALSE, tip_length = 0.01,
              test = t.test, y_position = 1) +
  facet_wrap(~Time.Series) +
  theme_classic() +
  labs(title = "All Test Types By Time Series - Bacteria - Ungrouped Settling Plates", y = "Viable/Total Bacteria") +
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
  labs(title = "All Samples By Treatment - Bacteria _ Ungrouped Settling Plates", y = "Viable/Total Bacteria") +
  scale_y_continuous(breaks = c(0, 0.25, 0.5, 0.75, 1), limits = c(-0.01, 1.1)) +
  scale_fill_manual(values = c(pal[2], pal[3])) +
  theme(legend.title = element_blank())


#### PLOT HIFLOW AND LOW FLOW####
hiflow <- df_calc[grepl('AS|Bobcat', df_calc$Sample.Type), ]
ggplot(hiflow, aes(x = Treatment, y = normalize)) +
  geom_boxplot(outlier.shape = NA, aes(fill = Treatment), alpha = 0.5) +
  geom_jitter(aes(fill = Treatment, color = Sample.Type,), size = 2, alpha = .7,) +
  geom_signif(comparisons = list(c("Control", "Experimental")),
              map_signif_level = FALSE, tip_length = 0.01,
              test = t.test) +
  theme_classic() +
  labs(title = "High Flow Air Samplers Only - Bacteria", y = "Ratio of Viable/Total Bacteria") +
  scale_fill_manual(values = c(pal[2], pal[3])) +
  theme(legend.title = element_blank())

loflow <- df_calc[grepl('SKC|USS|VSS', df_calc$Sample.Type), ]

ggplot(loflow, aes(x = Treatment, y = normalize)) +
  geom_boxplot(outlier.shape = NA, aes(fill = Treatment), alpha = 0.5) +
  geom_jitter(aes(fill = Treatment, color = Sample.Type,), size = 2, alpha = .7,) +
  geom_signif(comparisons = list(c("Control", "Experimental")),
              map_signif_level = FALSE, tip_length = 0.01,
              test = t.test) +
  theme_classic() +
  labs(title = "Low Flow Air Samplers Only - Bacteria", y = "Ratio of Viable/Total Bacteria") +
  scale_fill_manual(values = c(pal[2], pal[3])) +
  theme(legend.title = element_blank())





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
  labs(title = "Settling Plates Only - Bacteria", y = "Viable/Total Bacteria") +
  scale_y_continuous(breaks = c(0, 0.25, 0.5, 0.75, 1), limits = c(-0.01, 1.1)) +
  scale_fill_manual(values = c(pal[2], pal[3])) +
  theme(legend.title = element_blank()) +
  facet_wrap(~Time.Series)



##########PLOT JUST VIABLE######## FIX#########
ggplot(input_cleaned, aes(x = Treatment, y = log10(PMAcounts),)) +
  geom_boxplot(outlier.shape = NA, aes(fill = Treatment), alpha = 0.5) +
  geom_jitter(aes(fill = Treatment, color = Sample.Type,), size = 2, alpha = .7,) +
  geom_signif(comparisons = list(c("Control", "Experimental")),
              map_signif_level = FALSE, tip_length = 0.01,
              test = t.test, y_position = 3.15) +
  theme_classic() +
  labs(title = "All Test Types By Time Series - Viable Bacteria Only", y = "Viable Bacteria Log10 Scale") +
  scale_fill_manual(values = c(pal[2], pal[3])) +
  scale_y_continuous(breaks = c(-2, -1, 0, 1, 2, 3, 4), limits = c()) +
  theme(legend.title = element_blank())


########T0 V T60##############

controlT0 <- df_calc[grepl('T0', df_calc$Time.Series), ]
controlT0 <- controlT0[grepl("Control", controlT0$Treatment),]

expT60 <- df_calc[grepl('T6', df_calc$Time.Series), ]
expT60 <- expT60[grepl("Experimental", expT60$Treatment),]

controlvexp <- rbind(controlT0, expT60)

ggplot(controlvexp, aes(x = Treatment, y = normalize)) +
  geom_boxplot(outlier.shape = NA, aes(fill = Treatment), alpha = 0.5) +
  geom_jitter(aes(fill = Treatment, color = Sample.Type,), size = 2, alpha = .7,) +
  geom_signif(comparisons = list(c("Control", "Experimental")),
              map_signif_level = FALSE, tip_length = 0.01,
              test = t.test, y_position = 1) +
  theme_classic() +
  labs(title = "Control T0 vs Experimental T60 - Bacteria", y = "Viable/Total Bacteria") +
  scale_y_continuous(breaks = c(0, 0.25, 0.5, 0.75, 1), limits = c(-0.01, 1.1)) +
  scale_fill_manual(values = c(pal[2], pal[3])) +
  theme(legend.title = element_blank())












####JUNK CODE##########


#PLOT Nebulization Quantity PMA OVER PBS############


"ggplot() +
  geom_bar(T0.clean, mapping = aes(x = Sample.Day, y = PBScounts, fill = factor(Sample.Type)),  alpha = .3,  position = "dodge", stat = "identity") +
  geom_bar(T0.clean, mapping = aes(x = Sample.Day, y = PMAcounts, color = factor(Sample.Type)),fill = NA, position = "dodge", stat = "identity" ) +
  theme_classic() +
  labs(title = "Daily Nebulization Totals", fill = " ", x = "Sample Day", y = " Gene Copies/Liter") +
  scale_y_continuous(label = comma, limits = c(0, 250000))"

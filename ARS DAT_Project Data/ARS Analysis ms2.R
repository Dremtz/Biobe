require(plyr)
library(ggplot2)
library(ggsignif)
library(dplyr)
library(ggplot2)
library(reshape2)
library(scales)


setwd ("NEW FILEPATH")
input <- read.csv("NEW FILE", check.names = FALSE)
grouped <- ddply(input, .(Sample.Number, Collection.Time, Sample.Type, Location, wood.treatment), summarise, pbs_sum = sum(PBScounts), pma_sum = sum(PMAcounts))

#NORMALIZE#######
df_calc <- grouped %>% 
  mutate(normalize = round((pma_sum/pbs_sum), 1000)) 
df_calc[is.na(df_calc)] <- 0
#set normalized values >1 to 1
df_calc$normalize[(df_calc$normalize > 1)] <- 1


#ARS PLOT virus Viability T.test BY TEST TYPE#######

plot1 <- df_calc %>%
  filter(Sample.Type != 'AS')%>%
  filter(Sample.Type != 'VSS')%>%
  filter(Sample.Type != 'USS')%>%
  filter(Sample.Type != 'SKC1&2')%>%
  filter(wood.treatment != 'Air Sample') %>%
  ggplot(aes(x = Collection.Time, y = normalize)) +
    geom_boxplot(outlier.shape = NA) +
    geom_jitter() +
    geom_signif(comparisons = list(c("T0", "T360")),
                map_signif_level = FALSE, tip_length = 0.01,
                test = t.test, y_position = 1.) +
    theme_classic() +
    labs(title = "Combined Time Series By Test Type - virus", y = "Viable/Total virus") +
    scale_y_continuous(breaks = c(0, 0.25, 0.5, 0.75, 1), limits = c(-0.01, 1.1)) +
    theme(legend.title = element_blank())

plot1

#ARS PLOT virus Totals T.test BY TEST TYPE#######

#PMA COUNTS
plot2 <- input %>%
  filter(Sample.Type != 'AS')%>%
  filter(Sample.Type != 'VSS')%>%
  filter(Sample.Type != 'USS')%>%
  filter(Sample.Type != 'SKC1&2')%>%
  filter(wood.treatment != 'Air Sample') %>%
  ggplot(aes(x = Collection.Time, y = PMAcounts)) +
  geom_boxplot(outlier.shape = NA) +
  geom_point(position = position_jitter()) +
  geom_signif(comparisons = list(c("T0", "T360")),
              map_signif_level = FALSE, tip_length = 0.01,
              test = t.test) +
  theme_classic() +
  labs(title = "Active virus by Collection Time - PMA", y = "Active virus Count") +
  theme(legend.title = element_blank()) +
  scale_y_continuous(limits = c(0, 2000))
  


plot2


#PBS COUNTS
plot3 <- input %>%
  filter(Sample.Type != 'AS')%>%
  filter(Sample.Type != 'VSS')%>%
  filter(Sample.Type != 'USS')%>%
  filter(Sample.Type != 'SKC1&2')%>%
  filter(wood.treatment != 'Air Sample') %>%
  ggplot(aes(x = Collection.Time, y = PBScounts)) +
  geom_boxplot(outlier.shape = NA) +
  geom_point(position = position_jitter()) +
  geom_signif(comparisons = list(c("T0", "T360")),
              map_signif_level = FALSE, tip_length = 0.01,
              test = t.test) +
  theme_classic() +
  labs(title = "Total virus by Collection Time - PBS", y = "Total virus Count") +
  theme(legend.title = element_blank()) +
  scale_y_continuous(limits = c())

plot3


######MESSY PLOT, BASICALLY JUST SHOWING THAT ALL AIR SAMPLERS PICKED UP SOME 
#ARS PBS COUNTS AT T0

plot4 <- input %>%
  filter(Sample.Type != 'Settling Plate')%>%
  ggplot(aes(x = Sample.Type, y = log10(PBScounts))) +
  geom_point()

plot4


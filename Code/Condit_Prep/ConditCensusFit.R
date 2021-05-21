## condit inputs

library(tidyverse)
library(readr)
DrakeCensusFit <- read_tsv("Data/1981_2019_All_Counts/processed/ConditResults/DrakeCensusFit.csv")
DrakeCensusFit$Site <- "Drakes Beach"
HeadlandCensusFit <- read_tsv("Data/1981_2019_All_Counts/processed/ConditResults/HeadlandCensusFit.csv")
HeadlandCensusFit$Site <- "PR Headlands"
PtReyesCensusFit <- read_tsv("Data/1981_2019_All_Counts/processed/ConditResults/PtReyesCensusFit.csv")
PtReyesCensusFit$Site <- "All Point Reyes"
SouthCensusFit <- read_tsv("Data/1981_2019_All_Counts/processed/ConditResults/SouthCensusFit.csv")
SouthCensusFit$Site <- "South Beach"

#stack 'em
all <- bind_rows(DrakeCensusFit, HeadlandCensusFit, SouthCensusFit, PtReyesCensusFit)

# separate arrival and pop CIs
all.2 <- separate(all, CIpopulation, into = c("CIpopLow", "CIpopHi"), sep = "-", convert = TRUE)
all.3 <- separate(all.2, CIarrival, into = c("CIArriveLow", "CIArriveHi"), sep = "-", convert = TRUE)

## plot

p1 <- ggplot(all.3, aes(year, population, fill = Site)) +
  geom_line() + 
  geom_ribbon(aes(ymax = CIpopHi, ymin = CIpopLow), alpha = 0.5) +
  ylab("Predicted cow population size")
p1


p.arrival <- ggplot(all.3, aes(year, arrival, color = Site)) +
geom_pointrange(aes(ymax = CIArriveHi, ymin =  CIArriveLow)) + 
  geom_line() +
  ylab("Arrival Day in January") +
  theme_gray(base_size = 20) +
  scale_x_continuous(breaks = seq(1980, 2020, 2))
p.arrival

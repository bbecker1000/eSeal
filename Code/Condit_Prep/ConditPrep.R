
library(tidyverse)
library(lubridate)



library(readr)
Eseal_1981_2019 <- read_csv("Data/1981_2019_All_Counts/Eseal_1981_2019.csv", 
                            col_types = cols(Date = col_date(format = "%m/%d/%Y")))

#View(Eseal_1981_2019)

# If you can convert the day each year into a count of days since 1
# December, that's what I use. That is, 1=1Dec, 2=2Dec, etc until 32=1Jan
# etc and 63=1Feb. Then I want a season column, where season=2020 includes
# Dec 2019 plus Jan-Mar 2020. So 3 total columns of data: season, day,
# female count.

# just use data from Dec, Jan, Feb, March

# filter out all non-cow data
d1 <- Eseal_1981_2019 %>%
        filter(Age == "COW")

# Add a season (= YEAR column)
d1$season <- lubridate::year(d1$Date)
d1$month <- lubridate::month(d1$Date) 
hist(d2$month)
# filter out the few nov dates Dec-March
d2 <- d1 %>%
  filter(month != 11)

# Add a year to the december dates.  Called "season"
d2$season <- ifelse(d2$month == 12, d2$season + 1, d2$season)
View(d2)

# add day since Dec 1 within season
# Add column for Dec 1 of each year
# add day 1 column and month 12 column
d2$startDay = 1
d2$startMonth = 12
d2$StartSeasonDay = paste(d2$season-1, d2$startMonth, d2$startDay) %>% ymd() %>% as.Date()

#get days from StartSeasonDay
d2$int <- interval(d2$StartSeasonDay, d2$Date)
d2$day <- time_length(d2$int, "day")
# add 1 day since we want dec 1 to be 1 and not zero
d2$day <- d2$day + 1
# check data 
# View(d2) # looks good

#Clean up
d3 <- d2 %>% select("Location", "season", "day", "Count",)
# rename fields
d3 <- d3 %>% rename('female count' = Count) 

# final datasets will be:
# All PR
## Need to sum all sites by date. multi site Surveys are already coded to a common date so this should work.

PR_ALL_COWS <- d3 %>% 
               group_by(season, day) %>%
               summarize(FemaleCount = sum(`female count`))  

ggplot(PR_ALL_COWS, aes(day, FemaleCount)) +
  geom_point() +
  geom_smooth() +
  facet_wrap(. ~ season)
## ask Sarah about low points in 1995, 1998, 2017


# PR_ALL_COWS <- d3 %>% select(-Location)
write_csv(PR_ALL_COWS, path = "Data/1981_2019_All_Counts/processed/PR_Eseal_1981_2019_ALL_POOLED.csv")

# PR Headlands
PR_HEADLANDS_COWS <- d3 %>% filter(Location == "PR Headlands")
PR_HEADLANDS_COWS <- PR_HEADLANDS_COWS %>% select(-Location)
write_csv(PR_HEADLANDS_COWS, path = "Data/1981_2019_All_Counts/processed/PR_Eseal_1981_2019_PR_HEADLANDS.csv")

# Drakes Beach
PR_DrakesBeach_COWS <- d3 %>% filter(Location == "Drakes Beach")
PR_DrakesBeach_COWS <- PR_DrakesBeach_COWS %>% select(-Location)
write_csv(PR_DrakesBeach_COWS, path = "Data/1981_2019_All_Counts/processed/PR_Eseal_1981_2019_DrakesBeach.csv")

# South Beach
# Drakes Beach
PR_SouthBeach_COWS <- d3 %>% filter(Location == "South Beach")
PR_SouthBeach_COWS <- PR_SouthBeach_COWS %>% select(-Location)
write_csv(PR_SouthBeach_COWS, path = "Data/1981_2019_All_Counts/processed/PR_Eseal_1981_2019_SouthBeach.csv")


PR_HEADLANDS_COWS <- PR_HEADLANDS_COWS %>% rename(FemaleCount = 'female count')

ggplot(PR_HEADLANDS_COWS, aes(day, FemaleCount)) +
  geom_point() +
  geom_smooth() +
  facet_wrap(. ~ season)



















        




## how about some time series stuff.




library(tidyverse)
library(readr)
library(tsibble)

##%######################################################%##
#                                                          #
####                   data wrangling                   ####
#                                                          #
##%######################################################%##

## get all data

eSeal <- read_csv("C:/bbecker/Projects/eSeal/2018Analyses/Data/NES-Births_Lowry2014.csv")
head(eSeal)

head(eSeal.tot)
ggplot(eSeal.tot, aes(Year, tot)) +
  geom_line()

# to make a plot of just AN, SEFI, PORE, and PB
eSeal <- eSeal %>%
  select(c(1:4,6))

## make a tidy dataset
eSeal.tidy <- tidyr::gather(eSeal, key = "Site", value = count, 2:5) ## 2:11 when using all sites
head(eSeal.tidy)



## plot
ggplot(eSeal.tidy, aes(Year, count, group = Site, colour = Site)) +
  geom_line(size = 1.0) +
  geom_point() + 
  labs(y = "Estimated births") +
  facet_wrap(. ~ Site) +
  xlim(1996, 2010)
+ 
  scale_y_log10()


  scale_color_brewer(palette = "PiYG") 

## get site means for 1996 - 2010
  
  pop.means <- eSeal.tidy %>%
                filter(Year >= 1996) %>%
                  group_by(Site) %>%
                    summarize(avg = log(mean(count)))

## get first year with a value >0
year.one <- eSeal.tidy %>%
  group_by(Site) %>%
    filter(count != 0) %>%
      slice(which.min(count))

eSeal.tidy.index <- eSeal.tidy %>% filter(Site == "Ano Nuevo" & Year >= 1961 |
                      Site == "Farallon Islands" & Year >= 1972 |
                      Site == "Gorda-Cape San Martin" & Year >= 1981 |
                      Site == "Piedras Blancas" & Year >= 1992 |
                      Site == "Point Reyes" & Year >= 1981 |       
                      Site == "San Clemente Island" & Year >= 1977 |
                        Site == "San Miguel Island" & Year >= 1958 |
                        Site == "San Nicolas Island" & Year >= 1964 |
                        Site == "Santa Barbara Island" & Year >= 1964 |
                        Site == "Santa Rosa Island" & Year >= 1985    ) 

eSeal.tidy.index <- eSeal.tidy.index %>%
                        group_by(Site) %>%
                          mutate(start.year = row_number(Year))

## make a wide copy for MARSS

## drop Year
d1 <- select(eSeal.tidy.index, -Year)
## make wide
eSeal.wide <- d1 %>%
                spread(key = "Site", value = "count")
## now ready for eSeal_MARSS ch 7 if want to do "from colony start"



## plot with all same starting years
ggplot(eSeal.tidy.index, aes(x = start.year, y = count, color = Site)) +
  geom_line(size = 1.5) + 
  geom_point() + 
  scale_y_log10() +
  facet_wrap(. ~ Site)


## let's get lambda's
eSeal.tidy.index <- eSeal.tidy.index %>%
                      group_by(Site) %>%
                        mutate(lambda = count / lag(count))
## plot lambdas
ggplot(eSeal.tidy.index, aes(x = start.year, y = lambda, color = Site)) +
  geom_line(size = 1.5) +
  geom_point() +
  scale_y_continuous(limits = c(0, 5)) +
  scale_x_continuous(limits = c(0,10))

             
## start years for sites:

sites <- c("Ano Nuevo", 
             "Farallon Islands", 
             "Gorda-Cape San Martin",
              "Piedras Blancas",
              "Point Reyes",      
              "San Clemente Island",
              "San Miguel Island",
              "San Nicolas Island",
             "Santa Barbara Island",
              "Santa Rosa Island")
start.year <- as.numeric(c(1961, 1972, 1981, 1992 ,1981, 1977, 1958, 1964, 1964, 1985))
enso <- c("N", "N", "Y", "Y", "Y", "N", "Y", "N","N","N")
swell <- c("Y", "X", "Y", "Y", "Y", "N", "X", "N","N","N")
d1 <- as_tibble(cbind(sites, start.year, enso))
str(d1)
#remove san miguel and san nic since already established at first census.
d2 <- dplyr::filter(d1, sites != "San Miguel Island" & sites != "San Nicolas Island")


###---growthrates  Ugh !  won't deal with NAs
library(growthrates)
head(eSeal.tidy)

many_spline_fits <- all_splines(count ~ Year | Site,
                                data = eSeal.tidy, spar = 0.2)

library(growthcurver)
d <- growthdata





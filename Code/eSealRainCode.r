## Load eSeal Data
## old     eSealRain <- read.csv("Data/eSealRain.csv")
Cows <- read.csv("Data/eSealCows_81_17.csv")
Eseal_1981_2014 <- read.csv("Data/Eseal_1981_2014.csv")
eSealRain <- read.csv("Data/eSealRain.csv")



attach(eSealRain)
head(eSealRain)
library(lme4)
library(lattice)
library(effects)
library(ggplot2)
library(dplyr)
library(tidyr)


##----------------
library(readr)
library(lubridate)
## Load buoy Data dowloadsed from NOAA
## different formats/headers/data so had to do three different imports and then clean and bind
## first bring in 1982 - 1999 data and add a "19" to the year since only two digit
## make the columns all numbers or they act crazy!
COLUMNS <- cols(APD = col_number(), 
                     ATMP = col_skip(), BAR = col_skip(), 
                     DD = col_number(), DEWP = col_skip(), 
                     DPD = col_number(), MM = col_number(), 
                     MWD = col_number(), VIS = col_skip(), 
                     WD = col_number(), `WSPD GST` = col_skip(), 
                     WTMP = col_skip(), WVHT = col_number(), 
                     YY = col_number(), hh = col_number())

b1982 <-   read_table("Data/Buoy46026_Wind/1982.txt", col_types = COLUMNS)
b1983 <-   read_table("Data/Buoy46026_Wind/1983.txt", col_types = COLUMNS) 
b1984 <-   read_table("Data/Buoy46026_Wind/1984.txt", col_types = COLUMNS )
b1985 <-   read_table("Data/Buoy46026_Wind/1985.txt", col_types = COLUMNS )
b1986 <-   read_table("Data/Buoy46026_Wind/1986.txt", col_types = COLUMNS )
b1987 <-   read_table("Data/Buoy46026_Wind/1987.txt", col_types = COLUMNS )
b1988 <-   read_table("Data/Buoy46026_Wind/1988.txt", col_types = COLUMNS )
b1989 <-   read_table("Data/Buoy46026_Wind/1989.txt", col_types = COLUMNS )
b1990 <-   read_table("Data/Buoy46026_Wind/1990.txt", col_types = COLUMNS )
b1991 <-   read_table("Data/Buoy46026_Wind/1991.txt", col_types = COLUMNS )
b1992 <-   read_table("Data/Buoy46026_Wind/1992.txt", col_types = COLUMNS )
b1993 <-   read_table("Data/Buoy46026_Wind/1993.txt", col_types = COLUMNS )
b1994 <-   read_table("Data/Buoy46026_Wind/1994.txt", col_types = COLUMNS )
b1995 <-   read_table("Data/Buoy46026_Wind/1995.txt", col_types = COLUMNS )
b1996 <-   read_table("Data/Buoy46026_Wind/1996.txt", col_types = COLUMNS )
b1997 <-   read_table("Data/Buoy46026_Wind/1997.txt", col_types = COLUMNS )
b1998 <-   read_table("Data/Buoy46026_Wind/1998.txt", col_types = COLUMNS )


## bind rows
b82_98 <- dplyr::bind_rows(b1982, b1983, b1984, b1985, b1986, b1987, b1988, b1989, b1990,
                           b1991, b1992, b1993, b1994, b1995, b1996, b1997, b1998)

## change year to YYYY
b82_98$YY <- b82_98$YY + 1900
with(b82_98, hist(YY))
b82_98 <- dplyr::rename(b82_98,  YYYY = YY)

COLUMNS <- cols(APD = col_number(), 
                ATMP = col_skip(), BAR = col_skip(), 
                DD = col_number(), DEWP = col_skip(), 
                DPD = col_number(), MM = col_number(), 
                MWD = col_number(), VIS = col_skip(), 
                WD = col_number(), `WSPD GST` = col_skip(), 
                WTMP = col_skip(), WVHT = col_number(), 
                YYYY = col_number(), hh = col_number(),
                TIDE = col_skip())

## import the YYYY files
b1999 <-   read_table("Data/Buoy46026_Wind/1999.txt", col_types = COLUMNS)
b2000 <-   read_table("Data/Buoy46026_Wind/2000.txt", col_types = COLUMNS)
b2001 <-   read_table("Data/Buoy46026_Wind/2001.txt", col_types = COLUMNS)
b2002 <-   read_table("Data/Buoy46026_Wind/2002.txt", col_types = COLUMNS)
b2003 <-   read_table("Data/Buoy46026_Wind/2003.txt", col_types = COLUMNS)
b2004 <-   read_table("Data/Buoy46026_Wind/2004.txt", col_types = COLUMNS)
b2005 <-   read_table("Data/Buoy46026_Wind/2005.txt", col_types = COLUMNS)
b2006 <-   read_table("Data/Buoy46026_Wind/2006.txt", col_types = COLUMNS)

b99_06 <- dplyr::bind_rows(b1999, b2000, b2001, b2002, b2003, b2004,
                           b2005, b2006)

b82_06 <- dplyr::bind_rows(b82_98, b99_06)

## import the #YYYY files
## these were edited in excel, but could have done read_delim since were tab delimited!
b07_16 <- read_delim("Data/Buoy46026_Wind/2007-2016_trimmed.txt", 
                      "\t", escape_double = FALSE, trim_ws = TRUE)

## then make one file of all buoy data
b82_16 <- dplyr::bind_rows(b82_06, b07_16)
with(b82_16, hist(YYYY))  
## diff samples per year, so need to scale by samples in analysis?

## replace 99 and 999 with NA
b82_16[b82_16 == 99] <- NA
b82_16[b82_16 == 999] <- NA
## also a few 9s in the swell data in 2004-2006, bad data?
b82_16 <- dplyr::filter(b82_16, WVHT < 9)


### Fix dates
library(lubridate)
str(b82_16)

b82_16$datetime <- ymd_h(paste(
  b82_16$YYYY, b82_16$MM, b82_16$DD, b82_16$hh, sep="-"))




##-------------
##plot
with(b82_16, plot(datetime, WVHT))
p1 <- ggplot(b82_16, aes(datetime, WVHT)) + 
  geom_bin2d()
p1
  


## ok let's filter out the wave data to just dec-jan of each year
b82_16_dec_jan <- dplyr::filter(b82_16, MM == 1 | MM == 12)
## makde december surveys part of next year !
b82_16_dec_jan$SurveyYear <- ifelse(b82_16_dec_jan$MM == 12, b82_16_dec_jan$YYYY + 1, b82_16_dec_jan$YYYY)  ## double check this code!



p2 <- ggplot(b82_16_dec_jan, aes(SurveyYear, WVHT)) + 
  geom_point() +
  scale_y_continuous(breaks=seq(0,10,2)) +
  labs(x = "Year", y = "Wave Height (m)")
p2

## need to bin dec and jan together
b82_16_dec_jan$SurveyYear <- ifelse(b82_16_dec_jan$MM == 12, 
                                    b82_16_dec_jan$YYYY, 
                                    b82_16_dec_jan$YYYY - 1)
p3 <- ggplot(b82_16_dec_jan, aes(as.factor(SurveyYear), WVHT)) + 
  geom_boxplot() +
  scale_y_continuous(breaks=seq(0,10,2)) +
  labs(x = "Year", y = "Wave Height")
p3


##----------------------------------------------------------------
## Seal Data
Cows <- read.csv("Data/eSealCows_81_17.csv")

## get a lambda for each site by year
lambda <- select(Cows, 1, 6:9)
## wide to narrow
lambda_gathered <- gather(lambda, Site, Lambda, -Year)

## and the counts in a file by year
counts <- select(Cows, 1:5)
## wide to narrow
counts_gathered <- gather(counts, Site, Counts, -Year)
## for plotting, make eSeal counts on Jan 1 of each year.

lambda_plot <- ggplot(lambda_gathered, aes(Year, Lambda, colour = Site)) +
  geom_line(size = 2) + geom_point() + 
  scale_x_continuous(limits = c(1980, 2020))
lambda_plot

count_plot <- ggplot(counts_gathered, aes(Year, Counts * 1.12, colour = Site)) +
  geom_line(size = 3) + geom_point() + 
  scale_x_continuous(limits = c(1980, 2020))
count_plot


## MEI !!
MEI <- read.csv("Data/MEI.csv")

pMEI <- ggplot(MEI, aes(Year, MEI_DEC_JAN)) +
  geom_line(size = 3) + geom_point() + 
  scale_x_continuous(limits = c(1980, 2020))
pMEI
## lets put some plots together
library(ggpubr)
ggarrange(pMEI, lambda_plot, count_plot,
          ncol = 1, nrow = 3, align = "v")



##---what does the future hold?
## let's do 20 years
## use lambda since 2001
lambda_mean <- filter(lambda, Year > 2000) %>% summarize(avg = mean(Total_Lambda))
lambda_sd <- filter(lambda, Year > 2000) %>% summarize(avg = sd(Total_Lambda))

lambda_2001 <- filter(lambda, Year > 2000) 

hist((lambda_2001$Total_Lambda))


## loop
set.seed(2)
N0 = 1150  #initial population size
times = 20  #number of years into the future
N = vector(length = times)  #empty vector to store pop. sizes
N[1] = N0  #initial population size should be the first N

# lambdas--we only need 19 numbers because growth only
# happens between 2 years.
LAMBDA = rnorm(times - 1, mean = lambda_mean$avg, sd = lambda_sd$avg)

# start loop: Take previous year's N and multiply by lambda
for (t in 2:times) {
  N[t] = N[t - 1] * LAMBDA[t - 1]
}
plot(1:times, N, type = "b", las = 1)


## ok multiple loops

# multiple simulations
set.seed(2)
sims = 500
outmat = sapply(1:sims, function(x) {
  times = 20
  N0 = 1029
  N = vector(length = times)
  N[1] = N0
  LAMBDA = rnorm(times - 1, 1.07, 0.11)
  for (t in 2:times) {
    N[t] = N[t - 1] * LAMBDA[t - 1]
  }
  N
})
matplot(1:times, outmat, type = "l", las = 1, ylab = "Population Size", 
        xlab = "Years")
abline(h = 1029, lty = 2)

## try to plot with ggplot
outmat_tib <- as_tibble(outmat)

## gather the data
out_gathered <- gather(outmat_tib, Simulation, SimCount)

## add year field
out_gathered$Year <- rep(seq(1:NROW(outmat_tib)))

## give RealYear
out_gathered$RealYear <- out_gathered$Year + 2017 

## plot it

pSim <- ggplot(out_gathered, aes(RealYear, SimCount, group = Simulation)) + 
  geom_smooth(se = FALSE, size = 0.1, level = 0.5, alpha=0.01) +
  geom_hline(aes(yintercept = 1029)) + 
  labs(x = "Year", y = "Female elephant seals",
       title = "Projected Female Elephant Seal Population Size at Point Reyes National Seashore",
       subtitle = paste("500 Simulations:", "\u03BB = 0.07 \u00B1 0.11")) +
  ylim(0, 10000) + 
  scale_y_continuous(breaks=c(seq(0, 10000, by = 2000)))
  
pSim_plot <- pSim + geom_smooth(aes(group = 1), alpha = 1, level = 0.99)
pSim_plot

## make histogram of final values
max_counts <- filter(out_gathered, Year == 20)
hist(max_counts$SimCount)

max_hist <-ggplot(max_counts, aes(SimCount)) +
  labs(x = "Female elephant seals",
       title = "Projected Female Elephant Seal Population Size at Point Reyes National Seashore",
       subtitle = paste("500 Simulations:", "\u03BB = 0.07 \u00B1 0.11")) + 
  geom_histogram() 
max_hist



## ok, what if ENSOs more likely, include Lambda back to 97-98 ENSO

lambda_mean_enso <- filter(lambda, Year > 1997) %>% summarize(avg = mean(Total_Lambda))
lambda_sd_enso <- filter(lambda, Year > 1997) %>% summarize(avg = sd(Total_Lambda))

lambda_2001_enso <- filter(lambda, Year > 1997) 

hist((lambda_2001_enso$Total_Lambda))

# multiple simulations
set.seed(2)
sims = 500
outmat_enso = sapply(1:sims, function(x) {
  times = 20
  N0 = 1029
  N = vector(length = times)
  N[1] = N0
  LAMBDA = rnorm(times - 1, 1.068, 0.135)
  for (t in 2:times) {
    N[t] = N[t - 1] * LAMBDA[t - 1]
  }
  N
})
matplot(1:times, outmat_enso, type = "l", las = 1, ylab = "Population Size", 
        xlab = "Years")
abline(h = 1029, lty = 2)

## try to plot with ggplot
outmat_tib_enso <- as_tibble(outmat_enso)

## gather the data
out_gathered_enso <- gather(outmat_tib_enso, Simulation, SimCount)

## add year field
out_gathered_enso$Year <- rep(seq(1:NROW(outmat_tib_enso)))

## give RealYear
out_gathered_enso$RealYear <- out_gathered_enso$Year + 2017 

## plot it

pSim_enso <- ggplot(out_gathered_enso, aes(RealYear, SimCount, group = Simulation)) + 
  geom_smooth(se = FALSE, size = 0.1, level = 0.5, alpha=0.1) +
  geom_hline(aes(yintercept = 1029)) + 
  labs(x = "Year", y = "Female elephant seals",
       title = "Projected Female Elephant Seal Population Size at Point Reyes National Seashore with ENSO Events",
       subtitle = paste("100 Simulations:", "\u03BB = 0.07 \u00B1 0.13")) +
  ylim(0, 10000) + 
  scale_y_continuous(breaks=c(seq(0, 10000, by = 2000)))

pSim_enso_plot <- pSim_enso + geom_smooth(aes(group = 1), alpha = 1, level = 0.99)
pSim_enso_plot

library(ggpubr)
ggarrange(pSim_enso_plot, pSim_plot, 
          ncol = 1, nrow = 2, align = "v")




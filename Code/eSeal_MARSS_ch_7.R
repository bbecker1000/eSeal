##edited code from MARSS UserGuide chapter 7

## was from PORE phoca, now modify for eSeal all NA pops

library(MARSS)
library(tidyverse)
library(readr)

##%######################################################%##
#                                                          #
####                   data wrangling                   ####
#                                                          #
##%######################################################%##

## get all data

eSeal <- read_csv("C:/bbecker/Projects/eSeal/2018Analyses/Data/NES-Births_Lowry2014.csv")
head(eSeal)


##--------------------------------
## remove pre 1980 ----------------
# eSeal <- filter(eSeal, Year >= 1980)

## try filter on Lowry 2014 breakpoints
## first 1996 - 2010
eSeal <- filter(eSeal, Year >= 1996)

##------------------------
## use 0 start time for this one.  


##-------------------------------
## NEXT TRY Z -SCORING ALL THE DATA.



## now use names in the basic MARSS code 
dat <- t(eSeal)
## if using eSeal.ts data
## dat <- t(eSeal.wide)


###################################################
### code chunk number 11: Cs2_Code1
###################################################
#Code to fit the single population model with i.i.d. errors
#Read in data
## dat=t(harborSealWA) #Transpose since MARSS needs time ACROSS columns
years = dat[1,]     # get  years
n = nrow(dat)-1     # sites
dat = dat[2:nrow(dat),]  # remove years
dat <- log(dat)     ## log the data
dat[dat == 0] <- NA## replace zeros with NA
dat[dat == -Inf] <- NA  ##QC why we still have some -Inf
dat = zscore(dat)

legendnames = (unlist(dimnames(dat)[1]))  # site names


########################################################################

## Parameter key ########
## R = observation errors          equal
## U = growth parameter            unequal  
## Z = design matrix 
## Q = hidden state process        diagonal and unequal
## B = effect of column on row     unequal (these are the interactions)
## c = 
## d = process covariates

#####################################################################


###NOW TO MODELS ##########
#estimate parameters
## Single pop
Z.model = factor(c(1,1,1,1,1,1,1,1,1,1))
U.model="unequal"
Q.model="unconstrained"
R.model="unconstrained"  ### "diagonal and unequal" 
B.model="unconstrained"
kem_onepop = MARSS(dat, model=list(Z=Z.model, R=R.model), 
                                   control=list(maxit=1000, safe=TRUE))

#make figure
par(mfrow=c(1,1))
matplot(years, t(dat),xlab="",ylab="index of log abundance",
        pch=c("1","2","3","4","5","6","7","8","9","10"), ylim=c(3,10), bty="L")
lines(years,kem_onepop$states-1.96*kem_onepop$states.se,type="l",
      lwd=1,lty=2,col="red")
lines(years,kem_onepop$states+1.96*kem_onepop$states.se,type="l",
      lwd=1,lty=2,col="red")
lines(years,kem_onepop$states,type="l",lwd=2)
title("Observations and total population estimate",cex.main=.9)

coef(kem_onepop, type="vector")  #show the estimated parameter elements as a vector
coef(kem_onepop, type="matrix")$R
#show estimated elements for each parameter matrix as a list
coef(kem_onepop) 

kem_onepop$logLik   #show the log-likelihood
kem_onepop$AIC  #show the AIC

#plot residuals
plotdat = t(dat)
matrix.of.biases = matrix(coef(kem_onepop, type="matrix")$A,
                          nrow=nrow(plotdat),ncol=ncol(plotdat),byrow=T)
xs = matrix(kem_onepop$states,
            nrow=dim(plotdat)[1],ncol=dim(plotdat)[2],byrow=F)
resids = plotdat-matrix.of.biases-xs
par(mfrow=c(2,5))
for(i in 1:n){
  plot(resids[!is.na(resids[,i]),i],ylab="residuals")
  title(paste("One Population", legendnames[i]))
}


#####

###################################################
### code chunk number 23: Cs2_Code4
###################################################
## Independent pops
Z.model=factor(c(1,2,3,4,5,6,7,8,9,10))
U.model="unequal"
Q.model="unconstrained"
R.model="diagonal and equal"
B.model="unconstrained"
kem_ind=MARSS(dat, model=list(Z=Z.model, U=U.model, Q=Q.model, R=R.model, B=B.model),
               control=list(maxit=500, safe=TRUE)) 

#plot residuals  ## ANO NUEVO RESIDUAL ISSUES !!
plotdat = t(dat)
matrix.of.biases = matrix(coef(kem_ind,type="matrix")$A,
                          nrow=nrow(plotdat),ncol=ncol(plotdat),byrow=T)
par(mfrow=c(2,5))
for(i in 1:n){
  j=c(1,2,3,4,5,6,7,8,9,10)
  xs = kem_ind$states[j[i],]
  resids = plotdat[,i]-matrix.of.biases[,i]-xs
  plot(resids[!is.na(resids)],ylab="residuals")
  title(paste("Independent Populations", legendnames[i]))
}

kem_ind$AIC
####### THIS INDEPENDENT HAS BEST RESIDUALS SO FAR!! #######
B.0 = coef(kem_ind, type="matrix")$B[1:10,1:10]
rownames(B.0) = colnames(B.0) = c("PR","SEFI","AN","Gorda","PB", "SanMig", "SR", "SanNic", "SB", "SanClem")
print(B.0,digits=2)




coef(kem_ind, type="vector")  #show the estimated parameter elements as a vector
coef(kem_ind, type="matrix")$R
#show estimated elements for each parameter matrix as a list
coef(kem_ind) 

## try opepop plot
par(mfrow=c(1,1))
#make figure
matplot(years, t(dat),xlab="",ylab="index of log abundance",
        pch=c("1","2","3","4","5"), ylim=c(0,10), bty="L")
lines(years,kem_ind$states-1.96*kem_ind$states.se,type="l", ## need to reconcile vector lengths!
      lwd=1,lty=2,col="red")
lines(years,kem_ind$states+1.96*kem_ind$states.se,type="l",
      lwd=1,lty=2,col="red")
lines(years,kem_ind$states,type="l",lwd=2)
title("Observations and total population estimate",cex.main=.9)


### independent with ENSO----------------------
## MEI for DEC_JAN.  Year is January Year
MEI <- read_csv("C:/bbecker/Projects/eSeal/2018Analyses/Data/MEI.csv")
head(MEI)
# slice MEI to 1996 - 2010
MEI.96.10 <- filter(MEI, Year >= 1996 & Year <= 2010)
years <- MEI.96.10$Year #get year vector
MEI.96.10 <- MEI.96.10$MEI_DEC_JAN ## remove Year
MEI.96.10 <- t(MEI.96.10)
MEI.96.10.zscored <- zscore(MEI.96.10)  ## hardly any changes
## will use this as a c covariate on process and not (D) on observation
c = MEI.96.10.zscored
hist(c)
# and will have c be unconstrained.

# one pop
Z.model=factor(c(1,2,3,4,5,6,7,8,9,10))
U.model="unequal"
Q.model="unconstrained"
R.model="diagonal and equal"
B.model="unconstrained"
c = MEI.96.10.zscored
C.model = "unconstrained"

# independent with MEI
kem_ind.MEI = MARSS(dat, model=list(Z=Z.model, U=U.model, Q=Q.model, R=R.model, B=B.model, C=C.model, c=c), 
                         control=list(maxit=1000, safe=TRUE))

kem_ind.MEI$AIC

B.0 = coef(kem_ind.MEI, type="matrix")$B[1:10,1:10]
rownames(B.0) = colnames(B.0) = c("PR","SEFI","AN","Gorda","PB", "SanMig", "SR", "SanNic", "SB", "SanClem")
print(B.0,digits=2)

plotdat = t(dat)
matrix.of.biases = matrix(coef(kem_ind.MEI,type="matrix")$A,
                          nrow=nrow(plotdat),ncol=ncol(plotdat),byrow=T)
par(mfrow=c(2,5))
for(i in 1:n){
  j=c(1,2,3,4,5,6,7,8,9,10)
  xs = kem_ind.MEI$states[j[i],]
  resids = plotdat[,i]-matrix.of.biases[,i]-xs
  plot(resids[!is.na(resids)],ylab="residuals")
  title(paste("Independent+MEI", legendnames[i]))
}



####################################
## let's try mainland vs island model
###################################
#[1] "Point.Reyes"           "Farallon.Islands"      "Ano.Nuevo"            
#[4] "Gorda.Cape.San.Martin" "Piedras.Blancas"       "San.Miguel.Island"    
#[7] "Santa.Rosa.Island"     "San.Nicolas.Island"    "Santa.Barbara.Island" 
#[10] "San.Clemente.Island"  

#1 = mainland, 2 = island

Z.model=factor(c(1,2,2,1,1,2,2,2,2,2))
U.model="unequal"
Q.model="unconstrained"
R.model="unconstrained"  ### "diagonal and unequal" 
B.model="unconstrained"
kem_MI = MARSS(dat, model=list(Z=Z.model, U=U.model, Q=Q.model, R=R.model, B=B.model),
              control=list(maxit=1000, safe=TRUE))

#plot residuals
plotdat = t(dat)
matrix.of.biases = matrix(coef(kem_MI,type="matrix")$A,
                          nrow=nrow(plotdat),ncol=ncol(plotdat),byrow=T)
par(mfrow=c(2,5))
for(i in 1:n){
  j=c(1,2,2,1,1,2,2,2,2,2)
  xs = kem_MI$states[j[i],]
  resids = plotdat[,i]-matrix.of.biases[,i]-xs
  plot(resids[!is.na(resids)],ylab="residuals")
  title(paste("Bay vs Ocean", legendnames[i]))
}

## kem 4 still looks best residual and AIC wise.
coef(kem_MI, type="vector")  #show the estimated parameter elements as a vector
coef(kem_MI, type="matrix")$R
#show estimated elements for each parameter matrix as a list
coef(kem_MI) 

##########################################################
#Two subpopulations with different population parameters  
## (North and South)
##########################################################

# 1 = North, 2 = South
Z.model=factor(c(1,1,1,2,2,2,2,2,2,2))
U.model="unequal"
Q.model="unconstrained"
R.model="unconstrained"  ### "diagonal and unequal" 
B.model="unconstrained"
kem_NS = MARSS(dat, model=list(Z=Z.model, U=U.model, Q=Q.model, R=R.model, B=B.model),
              control=list(maxit=1000, safe=TRUE))

#plot residuals
plotdat = t(dat)
matrix.of.biases = matrix(coef(kem_NS,type="matrix")$A,
                          nrow=nrow(plotdat),ncol=ncol(plotdat),byrow=T)
par(mfrow=c(2,5))
for(i in 1:n){
  j=c(1,1,1,2,2,2,2,2,2,2)
  xs = kem_NS$states[j[i],]
  resids = plotdat[,i]-matrix.of.biases[,i]-xs
  plot(resids[!is.na(resids)],ylab="residuals")
  title(paste("North vs South", legendnames[i]))
}

coef(kem_NS, type="vector")  #show the estimated parameter elements as a vector
coef(kem_NS, type="matrix")$R
#show estimated elements for each parameter matrix as a list
coef(kem_NS) 


##########################################################
#Two subpopulations with different population parameters  
## (Channel Islands vs central Cal)
##########################################################

#[1] "Point.Reyes"           "Farallon.Islands"      "Ano.Nuevo"            
#[4] "Gorda.Cape.San.Martin" "Piedras.Blancas"       "San.Miguel.Island"    
#[7] "Santa.Rosa.Island"     "San.Nicolas.Island"    "Santa.Barbara.Island" 
#[10] "San.Clemente.Island"  

#1 = CI, 2 = CC

Z.model=factor(c(2,2,2,2,2,1,1,1,1,1))
U.model="unequal"
Q.model="unconstrained"
R.model="unconstrained"  ### "diagonal and unequal" 
B.model="unconstrained"
kem_CHIS = MARSS(dat, model=list(Z=Z.model, U=U.model, Q=Q.model, R=R.model, B=B.model),
              control=list(maxit=1000, safe=TRUE))

#plot residuals
plotdat = t(dat)
matrix.of.biases = matrix(coef(kem_CHIS,type="matrix")$A,
                          nrow=nrow(plotdat),ncol=ncol(plotdat),byrow=T)
par(mfrow=c(2,5))
for(i in 1:n){
  j=c(2,2,2,2,2,1,1,1,1,1)
  xs = kem_CHIS$states[j[i],]
  resids = plotdat[,i]-matrix.of.biases[,i]-xs
  plot(resids[!is.na(resids)],ylab="residuals")
  title(paste("CHIS vs Central Cal", legendnames[i]))
}

coef(kem_CHIS, type="vector")  #show the estimated parameter elements as a vector
coef(kem_CHIS, type="matrix")$R
#show estimated elements for each parameter matrix as a list
coef(kem_CHIS) 




##########################################################
# habitat /storm influences subpopulations with different population parameters  
## 
##########################################################

#[1] "Point.Reyes"           "Farallon.Islands"      "Ano.Nuevo"            
#[4] "Gorda.Cape.San.Martin" "Piedras.Blancas"       "San.Miguel.Island"    
#[7] "Santa.Rosa.Island"     "San.Nicolas.Island"    "Santa.Barbara.Island" 
#[10] "San.Clemente.Island"  

# 1 = storm, 2 = non storm, 3 = source  ## NEEDS UPDDATING !!

#Lost Coast (no data yet). Unknown                   NA
#Point Reyes.              Yes                       1
#Farallones storms         limited access            1
#Ano                       NEED INFO FROM SARAH      2
#Gorda Cape San Martin     NEED INFO FROM SARAH      2
#Peidras Blancas           yes                       1
#San Miguel                source                    3
#Santa Rosa                yes                       1
#San Nicolas               source                    3
#Santa Barbara             not likely                2
#San Clemente              not likely                2

Z.model=factor(c(1,1,2,2,1,2,1,2,2,2))  ## if two groups
U.model="unequal"
Q.model="unconstrained"
R.model="unconstrained"  ### "diagonal and unequal" 
B.model="unconstrained"
kem_habitat = MARSS(dat, model=list(Z=Z.model, U=U.model, Q=Q.model, R=R.model, B=B.model),
                control=list(maxit=1000, safe=TRUE))

par(mfrow = c(1,1))
matplot(years, t(dat),xlab="",ylab="index of log abundance",
        pch=c("1","2","3","4","5","6","7","8","9","10"), ylim=c(3,10), bty="L")
lines(years,kem_habitat$states-1.96*kem_habitat$states.se,type="l",
      lwd=1,lty=2,col="red")
lines(years,kem_habitat$states+1.96*kem_habitat$states.se,type="l",
      lwd=1,lty=2,col="red")
lines(years,kem_habitat$states,type="l",lwd=2)
title("Observations and total population estimate",cex.main=.9)

plot(years,kem_habitat$states[1,])


plotdat = t(dat)
matrix.of.biases = matrix(coef(kem_habitat,type="matrix")$A,
                          nrow=nrow(plotdat),ncol=ncol(plotdat),byrow=T)
par(mfrow=c(2,5))
for(i in 1:n){
  j=c(1,1,2,2,1,2,1,2,2,2)
  xs = kem_habitat$states[j[i],]
  resids = plotdat[,i]-matrix.of.biases[,i]-xs
  plot(resids[!is.na(resids)],ylab="residuals")
  title(paste("Storm and Source Pops", legendnames[i]))
}

## get parameters
coef(kem_habitat, type="matrix")$R
coef(kem_habitat, type="matrix")$Q
coef(kem_habitat, type="matrix")$U
coef(kem_habitat, type="matrix")$B


### based on log plots just looking at trends ----------------
# 1 = steady, 2 = highly variable

#Lost Coast (no data yet). Unknown                   NA
#Point Reyes.              Yes                       1
#Farallones storms         limited access            2
#Ano                       NEED INFO FROM SARAH      1
#Gorda Cape San Martin     NEED INFO FROM SARAH      2
#Peidras Blancas           yes                       1
#San Miguel                source                    1
#Santa Rosa                yes                       1
#San Nicolas               source                    1
#Santa Barbara             not likely                2
#San Clemente              not likely                2


Z.model=factor(c(1,2,1,2,1,1,1,1,2,2))  
U.model="unequal"
Q.model="unconstrained"
R.model="unconstrained"  ### "diagonal and unequal" 
B.model="unconstrained"
kem_habitat_trends = MARSS(dat, model=list(Z=Z.model, U=U.model, Q=Q.model, R=R.model, B=B.model),
                control=list(maxit=1000, safe=TRUE))

plotdat = t(dat)
matrix.of.biases = matrix(coef(kem_habitat_trends,type="matrix")$A,
                          nrow=nrow(plotdat),ncol=ncol(plotdat),byrow=T)
par(mfrow=c(2,5))
for(i in 1:n){
  j=c(1,2,1,2,1,1,1,1,2,2)
  xs = kem_habitat_trends$states[j[i],]
  resids = plotdat[,i]-matrix.of.biases[,i]-xs
  plot(resids[!is.na(resids)],ylab="residuals")
  title(paste("Storm and Source Pops", legendnames[i]))
}

## get parameters
coef(kem_habitat_trends, type="matrix")$R
coef(kem_habitat_trends, type="matrix")$Q
coef(kem_habitat_trends, type="matrix")$U
coef(kem_habitat_trends, type="matrix")$B

### Farallones, ANI, GOrda and SB vs all others independent. ----------------
## this makes most sense when starting from all time = zero

#Lost Coast (no data yet). Unknown                   NA
#Point Reyes.              Yes                       1
#Farallones storms         limited access            2
#Ano                       NEED INFO FROM SARAH      2
#Gorda Cape San Martin     NEED INFO FROM SARAH      2
#Peidras Blancas           yes                       3
#San Miguel                source                    4
#Santa Rosa                yes                       5
#San Nicolas               source                    6
#Santa Barbara             not likely                2
#San Clemente              not likely                7


Z.model=factor(c(1,2,2,2,3,4,5,6,2,7))  
U.model="unequal"
Q.model="unconstrained"
R.model="unconstrained"  ### "diagonal and unequal" 
B.model="unconstrained"
kem_A.F.G.SB = MARSS(dat, model=list(Z=Z.model, U=U.model, Q=Q.model, R=R.model, B=B.model),
                           control=list(maxit=1000, safe=TRUE))

plotdat = t(dat)
matrix.of.biases = matrix(coef(kem_A.F.G.SB,type="matrix")$A,
                          nrow=nrow(plotdat),ncol=ncol(plotdat),byrow=T)
par(mfrow=c(2,5))
for(i in 1:n){
  j=c(1,2,2,2,3,4,5,6,2,7)
  xs = kem_A.F.G.SB$states[j[i],]
  resids = plotdat[,i]-matrix.of.biases[,i]-xs
  plot(resids[!is.na(resids)],ylab="residuals")
  title(paste("kem_A.F.G.SB", legendnames[i]))
}



### Farallones, ANI, PORE and PB vs all others independent. ----------------
## this makes most sense when starting from 1980

#Lost Coast (no data yet). Unknown                   NA
#Point Reyes.              Yes                       1
#Farallones storms         limited access            1
#Ano                       NEED INFO FROM SARAH      1
#Gorda Cape San Martin     NEED INFO FROM SARAH      2
#Peidras Blancas           yes                       1
#San Miguel                source                    3
#Santa Rosa                yes                       4
#San Nicolas               source                    5
#Santa Barbara             not likely                6
#San Clemente              not likely                7


Z.model=factor(c(1,1,1,2,1,3,4,5,6,7))  
U.model="unequal"
Q.model="unconstrained"
R.model="unconstrained"  ### "diagonal and unequal" 
B.model="unconstrained"
kem_A.F.P.PB = MARSS(dat, model=list(Z=Z.model, U=U.model, Q=Q.model, R=R.model, B=B.model),
                     control=list(maxit=1000, safe=TRUE))

plotdat = t(dat)
matrix.of.biases = matrix(coef(kem_A.F.P.PB,type="matrix")$A,
                          nrow=nrow(plotdat),ncol=ncol(plotdat),byrow=T)
par(mfrow=c(2,5))
for(i in 1:n){
  j=c(1,1,1,2,1,3,4,5,6,7)
  xs = kem_A.F.P.PB$states[j[i],]
  resids = plotdat[,i]-matrix.of.biases[,i]-xs
  plot(resids[!is.na(resids)],ylab="residuals")
  title(paste("kem_A.F.G.SB", legendnames[i]))
}

### Farallones, ANI, PORE vs all others independent. ----------------
## this makes most sense when starting from 1980

#Lost Coast (no data yet). Unknown                   NA
#Point Reyes.              Yes                       1
#Farallones storms         limited access            1
#Ano                       NEED INFO FROM SARAH      1
#Gorda Cape San Martin     NEED INFO FROM SARAH      2
#Peidras Blancas           yes                       3
#San Miguel                source                    4
#Santa Rosa                yes                       5
#San Nicolas               source                    6
#Santa Barbara             not likely                7
#San Clemente              not likely                8


Z.model=factor(c(1,1,1,2,3,4,5,6,7,8))  
U.model="unequal"
Q.model="unconstrained"
R.model="unconstrained"  ### "diagonal and unequal" 
B.model="unconstrained"
kem_A.F.P = MARSS(dat, model=list(Z=Z.model, U=U.model, Q=Q.model, R=R.model, B=B.model),
                     control=list(maxit=1000, safe=TRUE))

plotdat = t(dat)
matrix.of.biases = matrix(coef(kem_A.F.P,type="matrix")$A,
                          nrow=nrow(plotdat),ncol=ncol(plotdat),byrow=T)
par(mfrow=c(2,5))
for(i in 1:n){
  j=c(1,1,1,2,3,4,5,6,7,8)
  xs = kem_A.F.P$states[j[i],]
  resids = plotdat[,i]-matrix.of.biases[,i]-xs
  plot(resids[!is.na(resids)],ylab="residuals")
  title(paste("kem_A.F.G.SB", legendnames[i]))
}



## AICs
models <- as.numeric(c(kem_onepop$AIC, kem_ind$AIC, kem_NS$AIC, kem_MI$AIC, kem_CHIS$AIC, 
                       kem_habitat$AIC, kem_habitat_trends$AIC, kem_A.F.G.SB$AIC, kem_A.F.P.PB$AIC, kem_A.F.P$AIC))
names <- c("one pop", "Independent", "NS", "MI", "CHIS", "Habitat", "Habitat 2", "A.F.G.SB", "kem_A.F.P.PB", "kem_A.F.P")
AICs <- as.data.frame(cbind(names, as.numeric(models)))
## AICs_Sorted <- AICs[order(AICs$models),]
AICs_Sorted <- dplyr::arrange(AICs, models)
AICs_Sorted

kem_onepop$AIC
kem_ind$AIC
kem_NS$AIC
kem_MI$AIC
kem_CHIS$AIC
kem_habitat$AIC
kem_habitat_trends$AIC
kem_A.F.G.SB$AIC
kem_A.F.P.PB$AIC
kem_A.F.P$AIC

## reminder--------------
########################################################################

## Parameter key ########
## R = observation errors          non-process error variance
## U = growth rate (exp)              
## Z = design matrix 
## Q = hidden state process /      process error variance    
## B = effect of column (j) on row (i)   these are the interactions
##            (subtract off 1 from the diagonals to get the effect of species i on itself 
                                          ##                     (think autoregressive).)
##              < 1 = DD, 1 = DI
## B in
#  terms of the interaction strengths between species; Bij
#  equals dfi /dXj, the change in the log population growth
#  rate of species i with respect to changes in the log
#  population abundance of species j. 
## c = 
## d = process covariates

#####################################################################
## OK, lets just look at the suspect sites ----------------------------------
# A.F.P.PB


eSeal <- read_csv("C:/bbecker/Projects/eSeal/2018Analyses/Data/NES-Births_Lowry2014.csv")
head(eSeal)

## try filter on Lowry 2014 breakpoints
## first 1996 - 2010
eSeal <- filter(eSeal, Year >= 1996)
## remove all but A.F.P.PB
eSeal <- select(eSeal, 1:4,6)
dat <- t(eSeal)
years = dat[1,]     # get  years
n = nrow(dat)-1     # sites
dat = dat[2:nrow(dat),]  # remove years
dat <- log(dat)     ## log the data
dat[dat == 0] <- NA## replace zeros with NA
dat[dat == -Inf] <- NA  ##QC why we still have some -Inf
legendnames = (unlist(dimnames(dat)[1]))  # sit


## MEI for DEC_JAN.  Year is January Year
MEI <- read_csv("C:/bbecker/Projects/eSeal/2018Analyses/Data/MEI.csv")
head(MEI)
# slice MEI to 1996 - 2010
MEI.96.10 <- filter(MEI, Year >= 1996 & Year <= 2010)
years <- MEI.96.10$Year #get year vector
MEI.96.10 <- MEI.96.10$MEI_DEC_JAN ## remove Year
MEI.96.10 <- t(MEI.96.10)
MEI.96.10.zscored <- zscore(MEI.96.10)  ## hardly any changes
## will use this as a c covariate on process and not (D) on observation
c = MEI.96.10.zscored
hist(c)
# and will have D be unconstrained.



# one pop
Z.model=factor(c(1,1,1,1))  
U.model="unequal"
Q.model="unconstrained"
R.model="diagonal and equal"
#R.model=matrix(0, nrow = 4, ncol = 4) #set observation error to zero for all sites
B.model="unconstrained"
c = MEI.96.10.zscored
C.model = "unconstrained"

## one pop
kem_A.F.P.PB = MARSS(dat, model=list(Z=Z.model, U=U.model, Q=Q.model, R=R.model, B=B.model), 
                     control=list(maxit=1000, safe=TRUE))
# one pop with MEI
kem_A.F.P.PB.MEI = MARSS(dat, model=list(Z=Z.model, U=U.model, Q=Q.model, R=R.model, B=B.model, C=C.model, c=c), 
                     control=list(maxit=1000, safe=TRUE))

# independent with MEI
Z.model=factor(c(1,2,3,4)) 
kem_A.F.P.PB.ind.MEI = MARSS(dat, model=list(Z=Z.model, U=U.model, Q=Q.model, R=R.model, B=B.model, C=C.model, c=c),
                     control=list(maxit=1000, safe=TRUE))
# independent no MEI
kem_A.F.P.PB.ind = MARSS(dat, model=list(Z=Z.model, U=U.model, Q=Q.model, R=R.model, B=B.model),
                             control=list(maxit=1000, safe=TRUE))

# PB independent 
Z.model=factor(c(1,1,1,2)) 
kem_A.F.P.PB.ind.1.1.1.2 = MARSS(dat, model=list(Z=Z.model, U=U.model, Q=Q.model, R=R.model, B=B.model),
                        control=list(maxit=1000, safe=TRUE))

## PORE is recipient
Z.model=factor(c(1,2,2,2)) 
kem_A.F.P.PB.ind.1.2.2.2 = MARSS(dat, model=list(Z=Z.model, U=U.model, Q=Q.model, R=R.model, B=B.model),
                                 control=list(maxit=1000, safe=TRUE))

kem_A.F.P.PB$AIC
kem_A.F.P.PB.MEI$AIC
kem_A.F.P.PB.ind$AIC
kem_A.F.P.PB.ind.MEI$AIC
kem_A.F.P.PB.ind.1.1.1.2$AIC
kem_A.F.P.PB.ind.1.2.2.2$AIC

## get parameters
summary(kem_A.F.P.PB.ind.MEI)
coef(kem_A.F.P.PB.ind.MEI, type="matrix")$R
coef(kem_A.F.P.PB.ind.MEI, type="matrix")$Q
exp(coef(kem_A.F.P.PB.ind.MEI, type="matrix")$U)  #to get lambda
coef(kem_A.F.P.PB.ind.MEI, type="matrix")$B
exp(coef(kem_A.F.P.PB.ind.MEI, type="matrix")$C) # exponentiate?
exp(kem_A.F.P.PB.ind.MEI$states)

## give B matrix site headers

## B in
#  terms of the interaction strengths between species; bij
#  equals dfi /dXj, the change in the log population growth
#  rate of species i with respect to changes in the log
#  population abundance of species j. T
B.0 = coef(kem_A.F.P.PB.ind.MEI, type="matrix")$B[1:4,1:4]
rownames(B.0) = colnames(B.0) = c("PR","SEFI","AN","PB")
print(exp(B.0),digits=2)  ## should these be exponentiated for interpretation?  like lambdas?

## best model residuals
plotdat = t(dat)
matrix.of.biases = matrix(coef(kem_A.F.P.PB.ind.MEI,type="matrix")$A,
                          nrow=nrow(plotdat),ncol=ncol(plotdat),byrow=T)
par(mfrow=c(1,4))
for(i in 1:n){
  j=c(1,2,3,4)
  xs = kem_A.F.P.PB.ind.MEI$states[j[i],]
  resids = plotdat[,i]-matrix.of.biases[,i]-xs
  plot(resids[!is.na(resids)],ylab="residuals")
  title(paste("4 site.ind.MEI", legendnames[i]))
}

## and CIs
CIs <- MARSSparamCIs(kem_A.F.P.PB.ind.MEI, method = "parametric", alpha = 0.8, nboot = 100) #takes about 40 min at 100 boots
## and prediction errors
pred.err <- MARSSkfss(kem_A.F.P.PB.ind)$Innov

## mean pop sizes
head(eSeal)



## -------------------------------------------------
## import tags 1998-2000 vs MARSS 1996 - 2010
tag_vs_marss <- read_csv("C:/bbecker/Projects/eSeal/2018Analyses/Data/tag_vs_marss_1996-2010.csv")
head(tag_vs_marss)

cor.test(tag_vs_marss$tag.recip.ratio, tag_vs_marss$ML.ratio, method = "pearson")
ggplot(tag_vs_marss, aes(x = tag.recip.ratio, y = ML.ratio)) +
  geom_point()


#### -----------------------------------------------------------------




##%######################################################%##
#                                                          #
####                 OK, now got models                 ####
####         working. On to hypothesis testing          ####
#                                                          #
##%######################################################%##


## play with glmm
library(tidyverse)
library(lme4)
eSeal <-
  read.csv("Data/NES-Births_Lowry2014.csv")
head(eSeal)

eSeal.gathered <- tidyr::gather(eSeal, Site, Count, 2:11)
head(eSeal.gathered)
# 53 years
eSeal.gathered$TwoGroups <- as.factor(c ( rep(1, 53),
                                rep(1, 53),
                                rep(2, 53),
                                rep(2, 53),
                                rep(1, 53),
                                rep(2, 53),
                                rep(1, 53),
                                rep(2, 53),
                                rep(2, 53),
                                rep(2, 53)))


eSeal.gathered$Indep <- as.factor(c ( rep(1, 53),
                                rep(2, 53),
                                rep(3, 53),
                                rep(4, 53),
                                rep(5, 53),
                                rep(6, 53),
                                rep(7, 53),
                                rep(8, 53),
                                rep(9, 53),
                                rep(10, 53)))   
head(eSeal.gathered)
str(eSeal.gathered)
## simple glmm.nb

m.2pop <- glmer(Count ~ Year + (1|TwoGroups), family = negative.binomial(1), data = eSeal.gathered)
m.Indep <- glmer(Count ~ Year + (1|Indep), family = negative.binomial(1), data = eSeal.gathered)

AIC(m.2pop, m.Indep)

                                
                                
                                
                                

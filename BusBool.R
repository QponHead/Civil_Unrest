# Libraries
library(foreign)
devtools::install_github("jason-morgan/boolean3",force=TRUE)
library(boolean3)
library(snow)
library(plyr)
library(ggplot2)

##### TUNING THE MODEL WITH A1 #####
# I Want to create a matrix that holds all of the xcel rules so that I can make a new df of
# changes in a1 and a2 and see how altering the a variables impacts the coeficients.

#***********************************#
# Create df with same rules as xcel #
#***********************************#

# variables
iterations <- 20
L <- 1000
sdinitE1 <- 0.2
sdinitE2 <- 0.1
slope <- 0.1
a1init <- 1
a2 <-0.05
b <- 0.5

#*************************#
# Population size Looping #

LLoop.vector <- c(5000,1000,500,100)
for(L in unique(LLoop.vector)){

#*************************#  
  
# Populate df
dat <- data.frame(1:L)
dat$Time.left <- sample(1:10,L, replace = T)
dat$Traffic <- sample(1:10,L, replace = T)
dat$E1 <- rnorm(L,0,sdinitE1)
dat$E2 <- rnorm(L,0,sdinitE2)
# Main Logit function with pmin & pmax parameters to keep within mathamatical bounds of 1 & 0
dat$Caught.Bus <- ifelse(pmax(10^-33, pmin(1, 1-slope*dat$Time.left+dat$E1)) > a1init, 1 , 
                         ifelse(pmin(1, pmax(0, 1-slope*dat$Time.left+dat$E1)) >= a2, 
                                ifelse(1-slope*dat$Traffic+dat$E2 > b, 1, 0), 0))
# English explination:
  # Caught Bus is a binary function of Time.Left and traffic
  # The parameters state that if the time you leave for the bus is in between a1 & a2, then
  # whether or not you catch the bus depends on the traffic. If traffic is above b, you
  # catch the bus. 
  # If you are above a1, you catch the bus no matter the traffic. If you are below, 
  # you cannot catch the bus no matter the traffic.
  # The E1 and E2 are error terms that add normal noise of  a set standard dev centered around 0.


#***************************************************************************#
# Loop through a1 and a2 / apply bool / pull Coefficient values into new df #
#***************************************************************************#

# make new df & populate
a1Var.df <- data.frame()
a1Bucket <- c()

for(i in c(1:iterations)){
  print(i)
  a1 <- a1init
  while(a1 >= a2)
  {
    # Run Bool
    dat$E1 <- rnorm(L,0,sdinitE1)
    dat$E2 <- rnorm(L,0,sdinitE2)
    dat$Caught.Bus <- ifelse(pmax(10^-33, pmin(1, 1-slope*dat$Time.left+dat$E1)) > a1, 1 , 
                             ifelse(pmin(1, pmax(0, 1-slope*dat$Time.left+dat$E1)) >= a2, 
                                    ifelse(1-slope*dat$Traffic+dat$E2 > b, 1, 0), 0))
    mod.1 <- boolprep(Caught.Bus ~ (a & b), a ~ Time.left, b ~ Traffic , data= dat, family = list(binomial(link = "logit"))) 
    result.1tmp <- boolean(mod.1, method="BFGS")
    
    # make new df
    Time.Left.a1Co <- result.1tmp$model.fit$BFGS$detail$Time.left
    Traffic.a1Co <- result.1tmp$model.fit$BFGS$detail$Traffic
    #Time.Left.a1pVal <- result.1tmp$model.fit$BFGS$detail$Time.left
    #Traffic.a1pVal <- result.1tmp$model.fit$BFGS$detail$Traffic
    a1Bucket <- as.data.frame(cbind(a1,Time.Left.a1Co,Traffic.a1Co))
    a1Var.df <- rbind(a1Bucket, a1Var.df)
      # plot the real data, not bool
        #noiz <- runif(dim(dat)[1], -.1, .1)
        plot(dat$Caught.Bus + noiz ~dat$Time.left, main = a1)
        #plot(dat$Caught.Bus + noiz ~dat$Traffic, main = a1)
    a1 <- round(a1 - .05, 2) 
  }
}
# Plot the variables coefficients against a1
plot(a1Var.df$Time.Left.a1Co ~ a1Var.df$a1, ylim=c(-2,0), main = L)
plot(a1Var.df$Traffic.a1Co ~ a1Var.df$a1, ylim=c(-5,7), main = L)
# Plot the pvalues aganist a1



}
##### TUNING THE MODEL WITH B #####

# variables
iterations <- 1
L <- 1000
sdinitE1 <- 0.2
sdinitE2 <- 0
slope <- 0.1
a1init <- 1
a1 <- a1init
a2 <-0
b <- 1

# Populate df
dat <- data.frame(1:L)
dat$Time.left <- sample(1:10,L, replace = T)
dat$Traffic <- sample(1:10,L, replace = T)
dat$E1 <- rnorm(L,0,sdinitE1)
dat$E2 <- rnorm(L,0,sdinitE2)

# Main Logit function with pmin & pmax parameters to keep within mathamatical bounds of 1 & 0
dat$Caught.Bus <- ifelse(pmax(10^-33, pmin(1, 1-slope*dat$Time.left+dat$E1)) > a1init, 1 , 
                         ifelse(pmin(1, pmax(0, 1-slope*dat$Time.left+dat$E1)) >= a2, 
                                ifelse(1-slope*dat$Traffic+dat$E2 > b, 1, 0), 0))

# make new df & populate
bVar.df <- data.frame()
bBucket <- c()

for(i in c(1:iterations)){
  print(i)
  while(b >= 0)
  {
    # Run Bool
    dat$E1 <- rnorm(L,0,sdinitE1)
    dat$E2 <- rnorm(L,0,sdinitE2)
    dat$Caught.Bus <- ifelse(pmax(10^-33, pmin(1, 1-slope*dat$Time.left+dat$E1)) > a1, 1 , 
                             ifelse(pmin(1, pmax(0, 1-slope*dat$Time.left+dat$E1)) >= a2, 
                                    ifelse(1-slope*dat$Traffic+dat$E2 > b, 1, 0), 0))
    mod.1 <- boolprep(Caught.Bus ~ (a & b), a ~ Time.left, b ~ Traffic , data= dat, family = list(binomial(link = "logit"))) 
    result.1tmp <- boolean(mod.1, method="BFGS")
    
    # make new df
    Time.Left.bCo <- result.1tmp$model.fit$BFGS$detail$Time.left
    Traffic.bCo <- result.1tmp$model.fit$BFGS$detail$Traffic
    bBucket <- as.data.frame(cbind(b,Time.Left.bCo,Traffic.bCo))
    bVar.df <- rbind(bBucket, bVar.df)
        #plot the real data, not bool
        noiz <- runif(dim(dat)[1], -.1, .1)
        #plot(dat$Caught.Bus + noiz ~dat$Time.left, main = b)
        plot(dat$Caught.Bus + noiz ~dat$Traffic, main = b)
    b <- round(b - .05, 2) 
  }
}
# Plot the variables coefficients against b
plot(bVar.df$Time.Left.bCo ~ bVar.df$b, main = L)
plot(bVar.df$Traffic.bCo ~ bVar.df$b, main = L)


##### ADDING INTEGRAL ANALYSIS! #####

# Notes from Bear model:
# a and b must be probabilities, which are mapped to a cumulative normal distribution,
# and are created from underlying variables in a regression-like manor.
# these variables are then multiplied together and represent the relationship that
# as one changes, the marginal impact of the other on Y also changes.


#****************************************#
# Integral function taken from Bear 2003 #
#****************************************#

# Variables

# t :: The Variable intitial input
# Bool.alpha :: the boolean predicted y intercept of the variable
# Bool.beta :: the boolean predicted Coefficient of the variable
# Variable.Int :: the normally distributed probability of the variable

# Probit Function
f <- function(x) {1/sqrt(2*pi)*exp(-x^2/2)}
Variable.Int <- integrate(f, lower = -Inf, upper = Bool.alpha - Bool.beta*t)
print(Variable.Int$value)

# Logit Function
Logit <- function (x) {(exp(x)) / (1 + exp(x))}
dat$Logit.Time.pr <- Logit(Bool.alpha.a + Bool.beta.a*(t))

    # Due to the way that the boolean Logit model works, 
    # Variable.Int.a * Variable.Int.b = the probability of your observed outcome.
    # In other words; the pr of Time.Left * pr of Traffic = pr Caught.Bus
    # So, if I see a probability < .5 the observed value in the Caught.Bus collumn
    # should be a 0.

#************************************************************************#
# Apply the Int funct to main df and then plot against observered values #
#************************************************************************#

# variables
iterations <- 1
L <- 1000
sdinitE1 <- 0.2 #default = 0.2
sdinitE2 <- 0.1 #default = 0.1
slope <- 0.1
a1init <- 1
a2 <-0.25
b <- 0.9

# Populate df & add probability Int collumns
dat <- data.frame(1:L)
dat$Time.left <- sample(1:10,L, replace = T)
dat$Logit.Time.pr <- NA
dat$Probit.Time.pr <- NA
dat$Traffic <- sample(1:10,L, replace = T)
dat$Logit.Traffic.pr <- NA
dat$Probit.Traffic.pr <- NA
dat$E1 <- rnorm(L,0,sdinitE1)
dat$E2 <- rnorm(L,0,sdinitE2)
dat$Caught.Logit.pr <-NA
dat$Caught.Probit.pr <-NA

# Main Logit function with pmin & pmax parameters to keep within mathamatical bounds of 1 & 0
dat$Caught.Bus <- ifelse(pmax(10^-33, pmin(1, 1-slope*dat$Time.left+dat$E1)) > a1init, 1 , 
                         ifelse(pmin(1, pmax(0, 1-slope*dat$Time.left+dat$E1)) >= a2, 
                                ifelse(1-slope*dat$Traffic+dat$E2 > b, 1, 0), 0))

# Run boolean model
mod.1 <- boolprep(Caught.Bus ~ (a & b), a ~ Time.left, b ~ Traffic , data= dat, family = list(binomial(link = "logit"))) 
result.1tmp <- boolean(mod.1, method="BFGS")
    
# Extract summary values from bool
    # Intercept and Coef for Time.left
    Bool.alpha.a <- result.1tmp$model.fit$BFGS$detail$X.Intercept.
    Bool.beta.a <- result.1tmp$model.fit$BFGS$detail$Time.left
    # Intercept and Coef for Traffic
    Bool.alpha.b <- result.1tmp$model.fit$BFGS$detail$X.Intercept..1
    Bool.beta.b <- result.1tmp$model.fit$BFGS$detail$Traffic
    # extract pvalues & X intercepts
    mySum <- summary(result.1tmp)
      Time.pVal <- format(round(as.numeric(mySum$methods$BFGS$coef$p.val[2]), 3), nsmall = 2)
      Traffic.pVal <- format(round(as.numeric(mySum$methods$BFGS$coef$p.val[4]), 3), nsmall = 2)
      Xinter.Time.pVal <- format(round(as.numeric(mySum$methods$BFGS$coef$p.val[1]), 3), nsmall = 2)
      Xinter.Traffic.pVal <- format(round(as.numeric(mySum$methods$BFGS$coef$p.val[3]), 3), nsmall = 2)
      
    
# Probit / Logit functions
Probit <- function(x) {1/sqrt(2*pi)*exp(-x^2/2)}
Logit <- function (x) {(exp(x)) / (1 + exp(x))}

# Apply Logit function to df
for(i in c(1:L)){
  dat$Logit.Time.pr[i] <- Logit(Bool.alpha.a + Bool.beta.a*(dat$Time.left[i]))
  dat$Logit.Traffic.pr[i] <- Logit(Bool.alpha.b + Bool.beta.b*(dat$Traffic[i]))
}
# Apply function to df
for(i in c(1:L)){
  Variable.Probit.a <- integrate(f, lower = -Inf, upper = Bool.alpha.a + Bool.beta.a*(dat$Time.left[i]))
  dat$Probit.Time.pr[i] <- Variable.Probit.a$value
  Variable.Probit.b <- integrate(f, lower = -Inf, upper = Bool.alpha.b + Bool.beta.b*(dat$Traffic[i]))
  dat$Probit.Traffic.pr[i] <- Variable.Probit.b$value
}
    # Add final collumn of multiplied values 
    # Format to be within 3 decimal places
    dat$Caught.Probit.pr <- format(round(as.numeric(dat$Probit.Time.pr) * as.numeric(dat$Probit.Traffic.pr), 3), nsmall = 2)
    dat$Caught.Logit.pr <- format(round(as.numeric(dat$Logit.Time.pr) * as.numeric(dat$Logit.Traffic.pr), 3), nsmall = 2)
      # make all new columns numeric
      dat$Probit.Time.pr <- as.numeric(dat$Probit.Time.pr)
      dat$Probit.Traffic.pr <- as.numeric(dat$Probit.Traffic.pr)
      dat$Caught.Probit.pr <- as.numeric(dat$Caught.Probit.pr)

#********#
# PLOTS! #
#********#
      
# Plot of a & b variables by Boolean Caught.bus with Overlay of Forced Integration probabilities
noiz <- runif(dim(dat)[1], -.1, .1)  

  #TIME LEFT LOGIT
  ggplot(data = dat, aes(x = dat$Time.left, y = dat$Caught.Bus + noiz)) +
    geom_point()+
    geom_line(aes(x = dat$Time.left, y = dat$Logit.Time.pr, col = "red"))+
    labs(x = paste("Time.left"), y = "Caught.Bus",
         title = "Boolean Model vs Time.left Logit",
         subtitle = paste("Coef" ,format(round(Bool.beta.a, 3),nsmall = 2), " Pval", Time.pVal, " Intrcpt.Pval", Xinter.Time.pVal,"     a1=", a1init),
         colour = "Logit.Time.left")
  #TRAFFIC LOGIT
  ggplot(data = dat, aes(x = dat$Traffic, y = dat$Caught.Bus + noiz)) +
    geom_point()+
    geom_line(aes(x = dat$Traffic, y = dat$Logit.Traffic.pr, col = "red"))+
    labs(x = paste("Traffic"), y = "Caught.Bus",
         title = "Boolean Model vs Traffic Logit",
         subtitle = paste("Coef" ,format(round(Bool.beta.b, 3),nsmall = 2), " Pval", Traffic.pVal, " Intrcpt.Pval", Xinter.Traffic.pVal,"     a1 =",a1init),
         colour = "Logit.Traffic.left")  
  
  #TIME LEFT PROBIT
  ggplot(data = dat, aes(x = dat$Time.left, y = dat$Caught.Bus + noiz)) +
  geom_point()+
  geom_line(aes(x = dat$Time.left, y = dat$Probit.Time.pr, col = "red"))+
    labs(x = paste("Time.left"), y = "Caught.Bus",
         title = "Boolean Model vs Time.left Forced Integration",
         subtitle = paste("Coef" ,format(round(Bool.beta.a, 3),nsmall = 2), " Pval", Time.pVal, " Intrcpt.Pval", Xinter.Time.pVal,"     a1=", a1init),
         colour = "Probit.Time.left")
  #TRAFFIC PROBIT
  ggplot(data = dat, aes(x = dat$Traffic, y = dat$Caught.Bus + noiz)) +
    geom_point()+
    geom_line(aes(x = dat$Traffic, y = dat$Probit.Traffic.pr, col = "red"))+
    labs(x = paste("Traffic"), y = "Caught.Bus",
         title = "Boolean Model vs Traffic Forced Integration",
         subtitle = paste("Coef" ,format(round(Bool.beta.b, 3),nsmall = 2), " Pval", Traffic.pVal, " Intrcpt.Pval", Xinter.Traffic.pVal,"     a1 =",a1init),
         colour = "Probit.Traffic.left")  
  

# look into SCAD database (are rows truly indepenent?) Based off of the month scale...yes.
  
# Do this!  
  
  # make hists of all variables
  # Make multivariate plot: make a matrix plot of all variable interactions
  
  # Randomize the civil unrest colonm and re-run the model to see if there is a significant change...
    # plot the coeficients against the coefficients
  
  # for the variable being analyzed; divide X values into managable parts from min to max
  #  plot against probability with average / median values of all other variables.
  # 1. 
  #
  #
  #
  #
  #
  
  
 
  
  
  
  
  
  
  

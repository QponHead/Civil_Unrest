# Libraries
rm(list=ls())
library(foreign)
devtools::install_github("jason-morgan/boolean3",force=TRUE)
library(boolean3)
library(snow)
library(ggplot2)
library(reshape2)

# Fetch dataframe
    setwd("/Users/angelokelvakis/Desktop/CivilUnrest_Data/")
    dat <- read.dta("use.dta")
      dat$lcgdp <- log(dat$cgdp)
      dat$lpop <- log(dat$pop)
      dat$lland_area <- log(dat$land_area)
      dat$tempsq<-(dat$lag8diff_mean_temp)^2
      dat$t100 <- dat$t*100
      dat$t2100 <- dat$t2*100
      dat$t3100 <- dat$t3*1000
      # Setup boolean model
        mod.1 <- boolprep(viol_dum ~ (a & b), a ~  agc_pc_gdp + agc_land_pc_totland + polity2 + polity2sq + imports_perc_gdp + cg + lcgdp + t100 + t2100 + t3100, b ~ lag8diff_mean_precip  + lag8sq_diff_mean_precip + lag8diff_var_precip +  muprec_varprec_8 + muprecsq_varprec_8 + mu3_8 + mu_mu2_varprec_8 + lag8diff_mean_temp + lag8diff_var_temp + world_food_prices_detrend4 + world_food_price_dq + t100 + t2100 + t3100, data= dat, family = list(binomial(link = "logit"))) 
        result.1tmp <- boolean(mod.1, method="BFGS")
        summary(result.1tmp)
        tmp <- summary(result.1tmp)$methods$BFGS$coef[1][,1]
        result.1 <- boolean(mod.1, method="nlminb", start= tmp)
        summary(result.1)

#---------------------------#
# ANALYZING THE JONES MODEL #
#---------------------------#

##### HISTOGRAMS OF THE VARIABLES #####
dat$nbscwar <- as.numeric(dat$nbscwar)
dat$land_area <- as.numeric(dat$land_area)
dat$polity2 <- as.numeric(dat$polity2)

# Practically magic...
ggplot(data = melt(dat), mapping = aes(x = value)) + 
  geom_histogram(bins = 10) + facet_wrap(~variable, scales = 'free_x')

##### Interaction Matricies #####
#*** why wont it let me make the graph???
pairs(dat$viol_dum[,1:5])

##### Randomized Bool Output #####
dat <- read.csv("/Users/angelokelvakis/Desktop/Jones.csv")
dat$lcgdp <- log(dat$cgdp)
dat$lpop <- log(dat$pop)
dat$lland_area <- log(dat$land_area)
dat$tempsq<-(dat$lag8diff_mean_temp)^2
dat$t100 <- dat$t*100
dat$t2100 <- dat$t2*100
dat$t3100 <- dat$t3*1000

  # Setup boolean model
  mod.1 <- boolprep(Rand.viol_dum ~ (a & b), a ~  agc_pc_gdp + agc_land_pc_totland + polity2 + polity2sq + imports_perc_gdp + cg + lcgdp + t100 + t2100 + t3100, b ~ lag8diff_mean_precip  + lag8sq_diff_mean_precip + lag8diff_var_precip +  muprec_varprec_8 + muprecsq_varprec_8 + mu3_8 + mu_mu2_varprec_8 + lag8diff_mean_temp + lag8diff_var_temp + world_food_prices_detrend4 + world_food_price_dq + t100 + t2100 + t3100, data= dat, family = list(binomial(link = "logit"))) 
  result.1tmp <- boolean(mod.1, method="BFGS")
  summary(result.1tmp)
  tmp <- summary(result.1tmp)$methods$BFGS$coef[1][,1]
  result.1 <- boolean(mod.1, method="nlminb", start= tmp)
  summary(result.1)
##### Single Variable manipulation #####
# Logit Function
Logit <- function (x) {(exp(x)) / (1 + exp(x))}
#------------#
# ROAD MAP ! #
#------------#
# 1. run model
# 2. grab coefficients 
# 3. average / median (make function where best stat summary can be used)
# 4. with coefficients & stat summary values for each variable, reverse Logit function
# 5. plot the probability from Logit function against X1 variable

#------------#
# CODE MAP ! #
#------------#

# Run Model
  setwd("/Users/angelokelvakis/Desktop/CivilUnrest_Data/")
  dat <- read.dta("use.dta")
  dat$lcgdp <- log(dat$cgdp)
  dat$lpop <- log(dat$pop)
  dat$lland_area <- log(dat$land_area)
  dat$tempsq<-(dat$lag8diff_mean_temp)^2
  dat$t100 <- dat$t*100
  dat$t2100 <- dat$t2*100
  dat$t3100 <- dat$t3*1000
  # Setup boolean model
  mod.1 <- boolprep(viol_dum ~ (a & b), a ~  agc_pc_gdp + agc_land_pc_totland + polity2 + polity2sq + imports_perc_gdp + cg + lcgdp + t100 + t2100 + t3100, b ~ lag8diff_mean_precip  + lag8sq_diff_mean_precip + lag8diff_var_precip +  muprec_varprec_8 + muprecsq_varprec_8 + mu3_8 + mu_mu2_varprec_8 + lag8diff_mean_temp + lag8diff_var_temp + world_food_prices_detrend4 + world_food_price_dq + t100 + t2100 + t3100, data= dat, family = list(binomial(link = "logit"))) 
  result.1tmp <- boolean(mod.1, method="BFGS")
  summary(result.1tmp)
  tmp <- summary(result.1tmp)$methods$BFGS$coef[1][,1]
  result.1 <- boolean(mod.1, method="nlminb", start= tmp)
  summary(result.1)
  Bool.Coef <- as.data.frame(result.1$model.fit$nlminb$par)
  drop <- c("p1","p12")
  Bool.Coef <- t(Bool.Coef[,!(names(Bool.Coef) %in% drop)])
  Bool.Coef <- c(Bool.Coef)
  Bool.names <- c("agc_pc_gdp" , "agc_land_pc_totland" , "polity2" , "polity2sq" , "imports_perc_gdp" , "cg" , "lcgdp" , "t100" , "t2100" , "t3100",  "lag8diff_mean_precip" , "lag8sq_diff_mean_precip" , "lag8diff_var_precip" , "muprec_varprec_8" , "muprecsq_varprec_8" , "mu3_8" , "mu_mu2_varprec_8" , "lag8diff_mean_temp" , "lag8diff_var_temp" , "world_food_prices_detrend4" , "world_food_price_dq" , "t100" , "t2100" , "t3100")
  # Grab significance from Jones paper for all variables' coefficients (not including y intercepts)
  Jones.Sig <- c(T,T,T,T,T,T,T,T,T,T,T,F,T,F,T,F,T,T,F,F,T,T,T,T)

  
pdf("/Users/angelokelvakis/Desktop/Jones_variableGraphs.pdf", width = 3.25, height = 3.25, paper = "letter", family = "ArialMT", useDingbats = FALSE)
  
  
#-----------------------------------------------------#
# FOOD INSECURITY MINI_MODEL TARGET VARIABLE ANALYSIS #
#-----------------------------------------------------#
  
# Grab Coefficeints
  FoodIns.Alpha <- result.1$model.fit$nlminb$detail$p12
  FoodIns.Coef <- c(Bool.Coef[11:24]) # PRESICE MATCH BETWEEN VARIABLES AND COEFICIENTS!!!!!!
  FoodIns.names <- c(Bool.names[11:24]) # PRESICE MATCH BETWEEN VARIABLES AND COEFICIENTS!!!!!!

# Grab Significance from Jones
  Jones.FoodIns.Sig <- c(Jones.Sig[11:24]) # PRESICE MATCH BETWEEN VARIABLES AND COEFICIENTS!!!!!!
  
# Stats Summary Function
  MySummary_median <- function(x){median(x, na.rm = T)}
  MySummary_mean <- function(x){mean(x, na.rm = T)}
  MySummary_mode <- function(x) {
                    ux <- unique(x)
                    ux[which.max(tabulate(match(x, ux)))]}
# Reverse Logit Function
  for(i in c(1:length(FoodIns.Coef))){
    TargetCof <- FoodIns.Coef[i]
    TargetSummary <- FoodIns.names[i]
    TargetSig <- Jones.FoodIns.Sig[i]
    VarSum.median <- FoodIns.Alpha
    VarSum.mean <- FoodIns.Alpha
    VarSum.mode <- FoodIns.Alpha
    for(j in c(1:length(FoodIns.Coef))){
      if(j != i){
        ThisCof <- FoodIns.Coef[j]
        ThisSummary <- FoodIns.names[j]
        nonNA.This.df <- dat[,ThisSummary]
        nonNA.This.df <- nonNA.This.df[!is.na(nonNA.This.df)]
        VarSum.median <- VarSum.median + ThisCof*MySummary_median(dat[,ThisSummary])
        VarSum.mean <- VarSum.mean + ThisCof*MySummary_mean(dat[,ThisSummary])
        VarSum.mode <- VarSum.mode + ThisCof*MySummary_mode(nonNA.This.df)
      }
      Foodvar <- as.numeric(na.omit(dat[,TargetSummary]))
    }
    par(new = F, lwd = .25)
    hist(Foodvar, main = "", ylab = "", xlab = "", xlim = c(min(Foodvar), max(Foodvar)), axes = F, col = "grey")
    axis(4)
    par(new = T, lwd = 1)
    curve(Logit(VarSum.median + TargetCof*x), min(Foodvar), max(Foodvar), ylim=c(0,1),main = paste(TargetSummary, "\n Coef:",format(round(TargetCof, 3),nsmall = 2),"  p<0.05:", TargetSig), ylab = "Food Insecurity Probability" )
    par(new = TRUE)
    curve(Logit(VarSum.mean + TargetCof*x), min(Foodvar), max(Foodvar), ylim=c(0,1),main = "", col = "red", ylab = "")
    par(new = TRUE)
    curve(Logit(VarSum.mode + TargetCof*x), min(Foodvar), max(Foodvar), ylim=c(0,1),main = "", col = "blue", ylab = "")
    par(new = F)
  }
#---------------------------------------------------------#
# STATE VULNERABILITY MINI_MODEL TARGET VARIABLE ANALYSIS #
#---------------------------------------------------------#

# Grab Coefficeints
StateVul.Alpha <- result.1$model.fit$nlminb$detail$p1
StateVul.Coef <- c(Bool.Coef[1:10]) # PRESICE MATCH BETWEEN VARIABLES AND COEFICIENTS!!!!!!
StateVul.names <- c(Bool.names[1:10]) # PRESICE MATCH BETWEEN VARIABLES AND COEFICIENTS!!!!!!

# Grab Significance from Jones
Jones.StateVul.Sig <- c(Jones.Sig[1:10]) # PRESICE MATCH BETWEEN VARIABLES AND COEFICIENTS!!!!!!

# Stats Summary Function
MySummary_median <- function(x){median(x, na.rm = T)}
MySummary_mean <- function(x){mean(x, na.rm = T)}
MySummary_mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]}
# Reverse Logit Function
for(i in c(1:length(StateVul.Coef))){
  TargetCof <- StateVul.Coef[i]
  TargetSummary <- StateVul.names[i]
  TargetSig <- Jones.StateVul.Sig[i]
  VarSum.median <- StateVul.Alpha
  VarSum.mean <- StateVul.Alpha
  VarSum.mode <- StateVul.Alpha
  for(j in c(1:length(StateVul.Coef))){
    if(j != i){
      ThisCof <- StateVul.Coef[j]
      ThisSummary <- StateVul.names[j]
      nonNA.This.df <- dat[,ThisSummary]
      nonNA.This.df <- nonNA.This.df[!is.na(nonNA.This.df)]
      VarSum.median <- VarSum.median + ThisCof*MySummary_median(dat[,ThisSummary])
      VarSum.mean <- VarSum.mean + ThisCof*MySummary_mean(dat[,ThisSummary])
      VarSum.mode <- VarSum.mode + ThisCof*MySummary_mode(nonNA.This.df)
    }
    Statevar <- as.numeric(na.omit(dat[,TargetSummary]))
  }
  par(new = F, lwd = .25)
  hist(Statevar, main = "", ylab = "", xlab = "", xlim = c(min(Statevar), max(Statevar)), axes = F, col = "grey")
  axis(4)
  par(new = T, lwd = 1)
  curve(Logit(VarSum.median + TargetCof*x), min(Statevar), max(Statevar), ylim=c(0,1),main = paste(TargetSummary, "\n Coef:",format(round(TargetCof, 3),nsmall = 2),"  p<0.05:", TargetSig), ylab = "State Vulnerability Probability" )
  par(new = TRUE)
  curve(Logit(VarSum.mean + TargetCof*x), min(Statevar), max(Statevar), ylim=c(0,1),main = "", col = "red", ylab = "")
  par(new = TRUE)
  curve(Logit(VarSum.mode + TargetCof*x), min(Statevar), max(Statevar), ylim=c(0,1),main = "", col = "blue", ylab = "")
  par(new = F)
}   


dev.off()


# Subtract some variables and see what happens to the coefficients...  
        
        
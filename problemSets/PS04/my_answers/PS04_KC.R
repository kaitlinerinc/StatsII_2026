#####################
# load libraries
# set wd
# clear global .envir
#####################

# remove objects
rm(list=ls())
# detach all libraries
detachAllPackages <- function() {
  basic.packages <- c("package:stats", "package:graphics", "package:grDevices", "package:utils", "package:datasets", "package:methods", "package:base")
  package.list <- search()[ifelse(unlist(gregexpr("package:", search()))==1, TRUE, FALSE)]
  package.list <- setdiff(package.list, basic.packages)
  if (length(package.list)>0)  for (package in package.list) detach(package,  character.only=TRUE)
}
detachAllPackages()

# load libraries
pkgTest <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[,  "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg,  dependencies = TRUE)
  sapply(pkg,  require,  character.only = TRUE)
}

# here is where you load any necessary packages
# ex: stringr
# lapply(c("stringr"),  pkgTest)

lapply(c("nnet", "MASS", "sampleSelection", "survival", "eha", "tidyverse", "ggfortify", "stargazer"),  pkgTest)

# set wd for current folder
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#####################
# Problem 1
#####################

#We're interested in modeling the historical causes of child mortality. We have data from
#26855 children born in Skelleftea, Sweden from 1850 to 1884. Using the "child" dataset in
#the eha library, fit a Cox Proportional Hazard model using mother's age and infant's gender
#as covariates. Present and interpret the output.

# load data on child mortality by mother's background and child gender
data("child")

child_surv <- with(child, Surv(enter, exit, event))
cox_mortality <- coxph(child_surv ~ sex + m.age, data = child)
summary(cox_mortality)

# There is a 0.082215 decrease in the expected log of the hazard for female babies compared to male, holding mother's age constant.
# The hazard ratio of female babies is 0.92 that of male babies, i.e. female babies are less likely to die (92 female babies die for every 100 male babies/female deaths are 8% lower)

# For every one unit increase in the mother's age, there is a 0.007617 increase in the expected log of the hazard for babies, holding sex constant.
# For every one unit increase in the mother's age, there is a 0.76% increase in the expected hazard for babies, holding sex constant.

#####################
# Problem 2
#####################

#We want to estimate how the amount of damage caused by a disaster relates to (1) whether disaster relief is provided and 
#(2) the amount of relief that is provided  conditional on a donation occurring when a disaster hits a given country. Estimate a 
#Heckman selection model using the disasters_response data in which binContribution is the outcome for the selection equation, 
#and originalContributionMillionUSDLogged is the outcome for the second equation. Use occurrences  + deathsEM + normalizedDamageEMLogged 
#as your input variables for both equations. Present and interpret the output.

# Load disaster data
disaster_data <- read.csv("https://raw.githubusercontent.com/ASDS-TCD/StatsII_2026/refs/heads/main/datasets/disaster_response.csv")

#The OLS model
base_heck <- lm(binContribution ~ occurrences + deathsEM + normalizedDamageEMLogged, data = disaster_data)
base_heck

#A one unit increase in occurrences is associated with a 0.3520 percentage point increase in the probability of donation, on average and holding all else constant.

#A one unit increase in deathsEM is associated with a 0.3854 percentage point increase in the probability of donation, on average and holding all else constant.

#A one unit increase in normalizedDamageEMLogged is associated with a 0.2549 percentage point increase in the probability of donation, on average and holding all else constant.

heckman_model <- heckit(selection = binContribution ~  occurrences + deathsEM + normalizedDamageEMLogged, 
                        outcome = originalContributionMillionUSDLogged ~ occurrences + deathsEM + normalizedDamageEMLogged, 
                        data = disaster_data)

summary(heckman_model)

#None of the covariates are statistically different than zero in outcome equation.

#The p-value of invMillsRatio is 0.0649, which is above our critical value of 0.05. Therefore,
#we fail to reject the null hypothesis that there is no selection bias happening in OLS that 
#needs to be accounted for with a Heckman selection model. Therefore, we can continue to use the OLS
#model

#This indicates that while there is a limited selection of instances in which donation occurred,
#it is not biasing the results in predicting the amount of the donation.




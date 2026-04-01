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

# load data on child mortality by mother's background and child gender
data("child")

child_surv <- with(child, Surv(enter, exit, event))

cox_mortality <- coxph(child_surv ~ sex + m.age, data = child)
summary(cox_mortality)

# There is a 0.08 decrease in the expected log of the hazard for female babies compared to male, holding mother's age constant.
# The hazard ratio of female babies is 0.92 that of male babies, i.e. female babies are less likely to die (92 female babies die for every 100 male babies/female deaths are 8% lower)

# For every one unit increase in the mother's age, there is a 0.34 increase in the expected log of the hazard for babies, holding sex constant.
# For every one unit increase in the mother's age, there is a 0.7% increase in the expected hazard for babies, holding sex constant.

#####################
# Problem 2
#####################

# load data
disaster_data <- read.csv("https://raw.githubusercontent.com/ASDS-TCD/StatsII_2026/refs/heads/main/datasets/disaster_response.csv")

base_heck <- lm(binContribution ~ occurrences + deathsEM + normalizedDamageEMLogged, data = disaster_data)

#A one unit increase in occurrences is associated with a0.003520

heckman_model <- heckit(selection = binContribution ~  occurrences + deathsEM + normalizedDamageEMLogged, 
                        outcome = originalContributionMillionUSDLogged ~ occurrences + deathsEM + normalizedDamageEMLogged, 
                        data = disaster_data)

#Considers the probability of selection to get a more unbiased sample
#There is a selection bias happening in OLS that the Heckman Selection Model accounts for
#If the p-value is significant, there is selection bias and you want to account for that

summary(heckman_model)

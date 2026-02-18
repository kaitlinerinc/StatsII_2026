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

lapply(c(),  pkgTest)

# set wd for current folder
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#####################
# Problem 1
#####################

# load data
load(url("https://github.com/ASDS-TCD/StatsII_2026/blob/main/datasets/climateSupport.RData?raw=true"))

#Switch to dummy variables
contrasts(climateSupport$countries) <- contr.treatment
contrasts(climateSupport$sanctions) <- contr.treatment

#Additive model
additive_model <- glm(choice ~ countries + sanctions,
                      data = climateSupport, 
                      family = binomial(link="logit"))

summary(additive_model)


#The global null hypothesis 

null_model <- glm(choice ~ 1, data = climateSupport, family = binomial)
anova(additive_model, null_model, test = "LRT")

#The p-value found from the ANOVA test is 2.2e-16. That is below our critical value of 0.05.
#So, we can reject the global null hypothesis that countries and sanctions have no
#significant relationship with choice


#2a.
five_percent_coefficient <- summary(additive_model)$coefficients["sanctions5%", "Estimate"] 
fifteen_percent_coefficient <- summary(additive_model)$coefficients["sanctions15%", "Estimate"] 
difference_vector <- c(five_percent_coefficient,fifteen_percent_coefficient)
coefficient_difference <- diff(difference_vector)
coefficient_difference

#For the policy in which nearly all countries participate [160 of 192], increasing sanctions from 5% to 15% 
#decreases the log-odds that an individual will support the policy by approximately 0.32511 on average, holding
#all other variables constant.

exp(coefficient_difference)
#For the policy in which nearly all countries participate [160 of 192], increasing sanctions from 5% to 15%
#decreases the odds of support for the policy by a factor of exp(-0.3251028) ≈ 0.7224531 on average. As
#1-0.7224 = 0.2776, this indicates that increasing sanctions from 5% to 15% decreases the odds of support for
#the policy by approximately 27.76%


#2b.

five_percent_coefficient <- summary(additive_model)$coefficients["sanctions5%", "Estimate"] 
fifteen_percent_coefficient <- summary(additive_model)$coefficients["sanctions15%", "Estimate"] 
difference_vector <- c(Five_percent_coefficient,Fifteen_percent_coefficient)
coefficient_difference <- diff(difference_vector)
coefficient_difference

#For the policy in which very few countries participate [20 of 192], increasing sanctions from 5% to 15% 
#decreases the log-odds that an individual will support the policy by approximately 0.3251028 on average, holding
#all other variables constant.

exp(coefficient_difference)

#For the policy in which very few countries participate [20 of 192], increasing sanctions from 5% to 15%
#decreases the odds of support for the policy by a factor of exp(-0.3251028) ≈ 0.7224479 on average, corresponding
#to a roughly 28% decrease in the odds of supporting the policy.

#The interpretation of the coefficient for the policy in which very few countries participate and the interpretation
#of the coefficient for the policy in which nearly all countries participate are the same because this is an
#additive model that assumes no relationship between countries and sanctions

#2c. 
#Plugging all the values in the equation on slide 39



predicted_probability <- exp(-0.27266 + 0.33636 * 1)/(1 + exp(-0.27266 + 0.33636 * 1))
predicted_probability



############################################################################################################

#3

#The answers to 2a and 2b would potentially change if we included an interaction term in this model because
#it would allow for a difference in the fact that the impact on policy decisions might be different
#for different numbers of participating countries and at different levels of sanctions for missing emission reduction targets

interaction_model <- glm(choice ~ countries * sanctions,
                         data = climateSupport,
                         family = binomial(link="logit"))

anova(additive_model, interaction_model, test = "LRT")

#The anova has a p-value of 0.3912 which is larger than the critical value of 
#0.05, this suggest that the difference between models is not significant. For the
#sake of parsimony it may be better not to include and interaction effect,
#unless we have theoretical backing.


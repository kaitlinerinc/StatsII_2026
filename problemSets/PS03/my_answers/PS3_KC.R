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

lapply(c("nnet", "MASS", "AER", "pscl"),  pkgTest)

# set wd for current folder
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#####################
# Problem 1
#####################

# load data
gdp_data <- read.csv("https://raw.githubusercontent.com/ASDS-TCD/StatsII_2026/main/datasets/gdpChange.csv", stringsAsFactors = F)
typeof(gdp_data$GDPWdiff)

#data wrangling


gdp_data$GDPWdiff_category <- factor(
  ifelse(gdp_data$GDPWdiff < 0, "negative",
  ifelse(gdp_data$GDPWdiff > 0, "positive", "no change")),
  levels = c("negative", "no change", "positive")
)

gdp_data$GDPWdiff_category <- relevel(gdp_data$GDPWdiff_category, ref = "no change")

#unordered
unord.log <- multinom(GDPWdiff_category ~ REG + OIL, data = gdp_data, Hess = TRUE)
summary(unord.log)

exp(coef(unord.log))


#ordered
# a) Perform an ordered (proportional odds) logistic regression

gdp_data$GDPWdiff_ordered <- factor(gdp_data$GDPWdiff_category, levels = c("negative", "no change", "positive"), ordered = TRUE)

ord.log <- polr(GDPWdiff_ordered ~ REG + OIL, data = gdp_data, Hess = TRUE)
summary(ord.log)

exp(coef(ord.log))

pp_ord <- data.frame(fitted(ord.log))

# Calculate a p value
ctable_ord <- coef(summary(ord.log))
p_ord <- pnorm(abs(ctable_ord[, "t value"]), lower.tail = FALSE) * 2
(ctable_ord <- cbind(ctable_ord, "p value" = p_ord))
ctable_ord

# Calculate confidence intervals
(ci <- confint(ord.log))

# convert to odds ratio
exp(cbind(OR = coef(ord.log), ci))
#####################
# Problem 2
#####################

# load data
mexico_elections <- read.csv("https://raw.githubusercontent.com/ASDS-TCD/StatsII_2026/main/datasets/MexicoMuniData.csv")


poisson_regression <- glm(PAN.visits.06 ~ competitive.district + marginality.06 + PAN.governor.06,
                          family="poisson"(link="log"),
                          data = mexico_elections)

summary(poisson_regression)
new_data <- data.frame(competitive.district = 1, marginality.06 = 0, PAN.governor.06 = 1)

poisson_regression_prediction <- predict(poisson_regression, newdata = new_data, type = "response")
poisson_regression_prediction


#An examination of the outcome variable revealed that there are a lot of zeros.
#The mean and the variance are not the same. We can further test this to see if this difference is problematic.
#Indicates overdispersion

hist(mexico_elections$PAN.visits.06)
table(mexico_elections$PAN.visits.06)


summary(mexico_elections$PAN.visits.06)
mean(mexico_elections$PAN.visits.06)
var(mexico_elections$PAN.visits.06)

dispersiontest(poisson_regression)

#It is more than one and has a p-value less than our critical value
#Reject the null hypothesis that the dispersion is less than or equal to 1. The data is overdispersed

zip_model <- zeroinfl(PAN.visits.06 ~ competitive.district + marginality.06 + PAN.governor.06,
                       dist="poisson",
                       data = mexico_elections)

summary(zip_model)

exp(zip_model$coefficients)

zip_prediction <- predict(zip_model, newdata = mexico_elections, competitive.district = 1, marginality.06 = 0, PAN.governor.06 = 1, type = "response")
mean(zip_prediction)


#(c) Provide the estimated mean number of visits from the winning PAN presidential candi-
#date for a hypothetical district that was competitive (competitive.district=1), had
#an average poverty level (marginality.06 = 0), and a PAN governor (PAN.governor.06=1).
  
exp(-2.08)

exp(-0.31)

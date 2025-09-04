#######################################################################################################################
######################################## Method ########################################################
# We follow a two-step process to determine the slope and standard deviation parameters:

## Step 1
# We measure the slope parameter. For that, we need to identify a survey with both height and weight values for individuals from our population of interest.
# Then, we run a regression analysis of log(height) on log(weight). The regression coefficient is the slope.

## Step 2 
# Now that we know the slope, we need to find the standard deviation to get our target obesity prevalence.
# If we note by f the obesity prevalence as a function of standard deviation, we need to solve the following equation:
# f(x) = obesity target
# This can be achieved either by trying different values of standard deviation manually until we reach the obesity target,
# or we can use a more systematic way using solvers with appropriate algorithms
# (As the function f is an increasing function, Newton-Raphson or Binary Search could work).


#######################################################################################################################
######################## Libraries and Packages Initialisation##########################################################

library('stargazer')
library(ggplot2)
library(dplyr) 
library(nnet)
library(zoo)
library(moments)
library(MASS)
library(tidyr)
library(corrplot)
library(haven)


rm(list = ls())

### Working directory ###
setwd("C:/Users/jzhu5/OneDrive - Imperial College London/Health-GPS_SHARED/FINCH/Parametres in healthgps-examples_JZ/")

#setwd("/Users/jasmine/Library/CloudStorage/OneDrive-ImperialCollegeLondon/Health-GPS_SHARED/FINCH/Parametres in healthgps-examples_JZ")

### File to use ###
file_name <- "HealthGPS_HeightWeight.csv"

### Import data ###
data <- read.csv(file_name)
str(data)
head(data, 40)


#######################################################################################################################
#################################### Filtering ######################################################################
lower_q = 0.01
upper_q = 1-lower_q

subdata <- data %>%
  filter(weight > quantile(weight, lower_q) & weight < quantile(weight, upper_q),
         height > quantile(height, lower_q) & height < quantile(height, upper_q))

summary(subdata)

#######################################################################################################################
#################################### Estimating slope and sd #############################################################

subdata_male <- subdata[subdata$sex==1,]
subdata_female <- subdata[subdata$sex==2,]

model_male <- lm(log_height ~ log_weight, data = subdata_male
                 #, weights = svyweight
                 )
summary(model_male)

male_res_sd <- sd(model_male$residuals)

model_female <- lm(log_height ~ log_weight, data = subdata_female
                   #, weights = svyweight
                   )
summary(model_female)

female_res_sd <- sd(model_female$residuals)


#######################################################################################################################
#################################### Export results on slope and sd #############################################################

slope <- data.frame(model_male$coefficients, model_female$coefficients)
write.csv(slope, "Outputs/height_slope.csv")

sd <- data.frame(male_res_sd,female_res_sd)
colnames(sd) <- c("male_sd","female_sd")
write.csv(sd, "Outputs/height_sd.csv")



############################################################################################
########################ToolBox############################################################

get_smooth_factor <- function(initial_factor, times) {
  if(times<1)
    return(initial_factor)
  
  res <- initial_factor
  
  for (j in 1:times) {
    tmp <- res
    
    for (p in 1:length(res)) {
      tmp[p] <- res[p]
    }
    
    sum <- 0
    for (i in 1:length(res)) {
      if (i == 1) {
        res[i] <- (2 * tmp[i] + tmp[i + 1]) / 3
      } else if (i == length(tmp)) {
        res[i] <- (tmp[i - 1] + 2 * tmp[i]) / 3
      } else {
        res[i] <- (tmp[i - 1] + tmp[i] + tmp[i + 1]) / 3
      }
      
      sum <- sum + res[i]
    }
  }
  
  return(res)
}


#######################################################################################################################
#################################### Factors mean of height and weight #############################################
## For ages 0-15, factors means are from UK-WHO growth charts https://www.rcpch.ac.uk/resources/uk-who-growth-charts-0-4-years
## For ages 16-110, factors mean are calculated with health surveys
## In factorsmean_weightheight_adult.csv, mean height and weight are weighted mean of four means

# Smoothing was applied
#meanheight_smooth = get_smooth_factor(meanheight, 50),
#meanweight_smooth = get_smooth_factor(meanweight, 50)

# To view the adults factors mean
 factorsmean <- read.csv("factorsmean_weightheight_adult.csv")



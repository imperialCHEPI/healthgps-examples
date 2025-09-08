#######################################################################################################################
#################################### Physical Activity Level #############################################################

rm(list = ls())


################## Libraries and Packages Initialisation################

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


### Working directory ###
setwd("C:/Users/jzhu5/OneDrive - Imperial College London/Health-GPS_SHARED/FINCH/Parametres in healthgps-examples_JZ/")
#setwd("C:/Users/dlaydon/OneDrive - Imperial College London/Health-GPS_SHARED/FINCH/Parametres in healthgps-examples_JZ/")

#setwd("/Users/jasmine/Library/CloudStorage/OneDrive-ImperialCollegeLondon/Health-GPS_SHARED/FINCH/Parametres in healthgps-examples_JZ/")


file_name <- "HealthGPS_PAL.csv"

### Import data ###
data <- read.csv(file_name)
str(data)


####################### Physical activity model ######################
df <- data[data$PAL>=1.4 & data$PAL<=2.5, ]
df$region <- as.factor(df$region)
df$ethnicity <- as.factor(df$ethnicity)

summary(df)

hist(df$PAL)

df$age2 <- df$age*df$age

reg_pal <- lm(PAL ~ age + age2 + female + ethnicity + inc + region, data = df)
summary(reg_pal)

ggplot(df, aes(x = factor(age), y = PAL)) +
  geom_boxplot(fill = "yellow", color = "red", width = 0.7)

sd_pal <- sd(reg_pal$residuals)

min_pal = min(df$PAL)
max_pal = max(df$PAL)
boundaries_pal = data.frame(min_pal, max_pal)


## Mean PAL ##
factors_mean <- df |>
  group_by(age, female) |>
  summarise(pal_mean = mean(PAL))

factors_mean <- factors_mean |>
  group_by(female) |>
  mutate(pal_mean_smooth = get_smooth_factor(pal_mean,150))

plot(factors_mean$age, factors_mean$pal_mean_smooth)

####################### Export outputs ######################

Coeffs = reg_pal$coefficients
if (! "region2" %in% names(Coeffs))
{
	## create dummy region variables
	DummyRegionCoeffs = rep(0, 3)
	names(DummyRegionCoeffs) = paste0("region", 2:4)
	Coeffs = c(Coeffs, DummyRegionCoeffs)
}

PhysicalActivityDFrame = data.frame(Value = c(Coeffs, stddev = sd_pal, min = min_pal, max = max_pal))
# fix rownames
rownames(PhysicalActivityDFrame)  = gsub("\\(Intercept\\)"	, "Intercept"	, rownames(PhysicalActivityDFrame) )
rownames(PhysicalActivityDFrame)  = gsub("inc"				, "income"		, rownames(PhysicalActivityDFrame) )
rownames(PhysicalActivityDFrame)  = gsub("female"				, "gender2"		, rownames(PhysicalActivityDFrame) )
rownames(PhysicalActivityDFrame)  = gsub("^age$"				, "age1"		, rownames(PhysicalActivityDFrame) )

###### FLAGGING THAT THIS MIGHT NEED TO BE CHANGED WHEN RECODING GENDER!!!!! #######
###### FLAGGING THAT THIS MIGHT NEED TO BE CHANGED WHEN RECODING GENDER!!!!! #######
###### FLAGGING THAT THIS MIGHT NEED TO BE CHANGED WHEN RECODING GENDER!!!!! #######
###### FLAGGING THAT THIS MIGHT NEED TO BE CHANGED WHEN RECODING GENDER!!!!! #######
###### FLAGGING THAT THIS MIGHT NEED TO BE CHANGED WHEN RECODING GENDER!!!!! #######
###### FLAGGING THAT THIS MIGHT NEED TO BE CHANGED WHEN RECODING GENDER!!!!! #######
#rownames(PhysicalActivityDFrame)  = gsub("female"			, "gender2"		, rownames(PhysicalActivityDFrame) )
###### FLAGGING THAT THIS MIGHT NEED TO BE CHANGED WHEN RECODING GENDER!!!!! #######
###### FLAGGING THAT THIS MIGHT NEED TO BE CHANGED WHEN RECODING GENDER!!!!! #######
###### FLAGGING THAT THIS MIGHT NEED TO BE CHANGED WHEN RECODING GENDER!!!!! #######
###### FLAGGING THAT THIS MIGHT NEED TO BE CHANGED WHEN RECODING GENDER!!!!! #######
###### FLAGGING THAT THIS MIGHT NEED TO BE CHANGED WHEN RECODING GENDER!!!!! #######
###### FLAGGING THAT THIS MIGHT NEED TO BE CHANGED WHEN RECODING GENDER!!!!! #######
###### FLAGGING THAT THIS MIGHT NEED TO BE CHANGED WHEN RECODING GENDER!!!!! #######

#write.csv(PhysicalActivityDFrame, "Outputs/physicalactivity_model.csv")
write.table(PhysicalActivityDFrame, "Outputs/physicalactivity_model.csv", sep = ",", col.names = F)


#write.csv(reg_pal$coefficients, "Outputs/physicalactivity_model.csv")

write.csv(sd_pal, "Outputs/physicalactivity_res_sd.csv")

write.csv(boundaries_pal, "Outputs/boundaries_pal.csv")

write.csv(factors_mean, "Outputs/pal_factorsmean.csv")

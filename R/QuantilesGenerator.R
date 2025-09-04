#######################################################################################################################
######################## Libraries and Packages Initialisation##########################################################

rm(list = ls())

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

### Working directory ###
setwd("C:/Users/jzhu5/OneDrive - Imperial College London/Health-GPS_SHARED/FINCH/Parametres in healthgps-examples_JZ/")
#setwd("/Users/jasmine/Library/CloudStorage/OneDrive-ImperialCollegeLondon/Health-GPS_SHARED/FINCH/Parametres in healthgps-examples_JZ")


############################################################################################
############################### Weight Quantiles using HSE ###########################

######################## Parameters ##########################################################

data <- read.csv("HealthGPS_HeightWeight.csv")
weighting <- read.csv("region_proportion.csv")


#Filter threshold 
#rate_to_filter <- 0.01 

#Physical Activity Parameters: mean and pa 
#pa_mean = 1.6
#pa_sd = 0.06

#Weight Parameters 
#num_weight_quantiles_to_sample = 10000
#bounds = c(15.5,18.5,20,25,30,35,40,45)

#BMI Prevalence by range from NCD RisC (e.g. the following is India 2022)
#prevalence_male = c(0.1247,0.1159,0.4763,0.2291,0.0439,0.0068,0.0029)
#prevalence_female = c(0.1369,0.1144,0.4211,0.2296,0.0744,0.0177,0.0055)

#######################################################################################################################
######################## Data Loading and Variable Extraction ##########################################################

# Attach the data frame for easier variable referencing
attach(data)

# Extracting variables from the 'data' data frame
female <- case_when(
  data$sex == 1 ~ 0,
  data$sex == 2 ~ 1
)
age <- round(data$age)
height <- data$height
weight <- data$weight
svyweight <- data$svyweight
region <- data$region
bmi <- weight/(height/100)^2


# Create 'subdata' dataframe
subdata_all <- data.frame(age, female, height, weight, bmi, svyweight, region)

# Remove rows with non-numeric NaN or empty values
#subdata <- subdata[complete.cases(subdata), ]
subdata <- subdata_all[subdata_all$height>0 & subdata_all$weight>0, ]

summary(subdata)

############################################################################################
######################## Filtering ##########################################################

# Set upper and lower quantiles
lower_q <- 0.01 
upper_q <-1-lower_q

# Filter 'subdata' based on conditions
df <- subdata %>%
  filter(
    height > quantile(height, lower_q) & height < quantile(height, upper_q),  # Height within quantiles
    weight > quantile(weight, lower_q) & weight < quantile(weight, upper_q),  # Weight within quantiles
  )

summary(df)

############################################################################################
######################## Merging region weights ##########################################################
age <- weighting$Age
female <- weighting$Gender
prob1 <- weighting$England
prob2 <- weighting$Wales
prob3 <- weighting$Scotland
prob4 <- weighting$Northern.Ireland

wide <- data.frame(age, female, prob1, prob2, prob3, prob4)
long <- wide |>
  pivot_longer(
    cols = starts_with("prob"),
    names_to = "region",
    names_prefix = "prob",
    values_to = "prob"
  ) |>
  mutate(region = as.integer(region)) 


df <- left_join(df, long, by = c("age","female","region"))

df$newweighting <- df$svyweight*df$prob

summary(df)

################################################################################################3###

sampled_bmi_male <- df[df$female==0, c("bmi", "newweighting")]
sampled_bmi_female <- df[df$female==1, c("bmi", "newweighting")]

bmi_male_mean <- weighted.mean(sampled_bmi_male$bmi, sampled_bmi_male$newweighting)
bmi_female_mean <- weighted.mean(sampled_bmi_female$bmi, sampled_bmi_female$newweighting)

# Plot density of BMI distribution by sex
plot(density(sampled_bmi_male$bmi), col = "blue", lwd = 3, main = "BMI distribution by sex")
lines(density(sampled_bmi_female$bmi), col = "red", lwd = 3)

# Adding legend
legend("topright", legend = c("Male", "Female"), col = c("blue", "red"), lty = 1, lwd = 2)

# Normalize BMI values by dividing each value by the mean of the respective sex
normalised_bmi_male <- sampled_bmi_male$bmi / bmi_male_mean
normalised_bmi_female <- sampled_bmi_female$bmi / bmi_female_mean

normalised_bmi_male_df <- data.frame(quantile = normalised_bmi_male)
normalised_bmi_female_df <- data.frame(quantile = normalised_bmi_female)


# Write normalised BMI values to CSV files
write.csv(normalised_bmi_male_df, "Outputs/weight_quantiles_NCDRisk_male.csv")
write.csv(normalised_bmi_female_df, "Outputs/weight_quantiles_NCDRisk_female.csv")

################################################################################################################################
###############################Energy to Physical Activity Quantiles###############################################################

# Generate normally distributed numbers with mean = 0 and std = 1
#number_samples = length(df$energy)

#random_numbers <- rnorm(number_samples, mean = 0, sd = 1)

# Physical activity follows a log normal distribution with mean = pa_mean and sd = pa_sd
#physicalactivity = pa_mean*exp(pa_sd*random_numbers-0.5*pa_sd*pa_sd)
#plot(density(physicalactivity), col="red", main="Physical Activity Density", lwd=3)

#######################################################################################################################
######################## Data Loading and Variable Extraction ##########################################################

file_name <- "C:/Users/jzhu5/OneDrive - Imperial College London/Health-GPS_SHARED/FINCH/Parametres in healthgps-examples_JZ/HealthGPS_initial_population_2598hh.csv"
data <- read.csv(file_name)

## Parameter pf pal sd
pal_sd =  0.268662431506871 

## number of EPA quantiles to sample
num_epa_quantiles_to_sample <- 10000


female <- case_when(
  data$sex_person == 1 ~ 0,
  data$sex_person == 2 ~ 1
)
sex <- as.factor(data$sex_person)
age <- data$age_person
age1 <- data$age_person
age2 <- data$age_person * data$age_person
inc <- data$hhinc_pc # household income per equalised person
ethnicity <- ifelse(data$ethnicity_person == 3, 2, 
                    ifelse(data$ethnicity_person == 4, 3, 
                           ifelse(data$ethnicity_person %in% c(2, 5), 4, data$ethnicity_person)))
ethnicity <- as.factor(ethnicity)
region <- data$country # Same as in HGPS, 1-EN 2-WA 3-SC 4-NI
region <- as.factor(region)
protein <- data$protein_w
carb <- data$carbs_w
fat <- data$fats_w
alcohol <- data$alcohol_w

subdata_all <- data.frame(sex, female, age, age1, age2, inc, ethnicity, region, carb, fat, protein, alcohol)

subdata <- subdata_all[!is.na(subdata_all$inc),]
str(subdata)

# Missing alcohol
sum(is.na(subdata$alcohol))

# Replace missing alcohol with 0
subdata$alcohol <- ifelse(is.na(subdata$alcohol), 0, subdata$alcohol)

subdata$energy <- 4 * subdata$carb + 9 * subdata$fat + 4 * subdata$protein + 7 * subdata$alcohol

############### Predict PAL with physical activity model from HSE ###############
df <- subdata

coef <- read.csv("C:/Users/jzhu5/OneDrive - Imperial College London/Health-GPS_SHARED/FINCH/Parametres in healthgps-examples_JZ/Outputs/physicalactivity_model.csv",
                 header = FALSE, stringsAsFactors = FALSE)

df$ethnicity2 <- as.numeric(df$ethnicity == 2)
df$ethnicity3 <- as.numeric(df$ethnicity == 3)
df$ethnicity4 <- as.numeric(df$ethnicity == 4)

df$region2 <- as.numeric(df$region == 2)
df$region3 <- as.numeric(df$region == 3)
df$region4 <- as.numeric(df$region == 4)

coef_vector <- setNames(coef$V2, coef$V1)

print(names(coef_vector))

df$pal_predicted <- coef_vector["Intercept"] + 
  coef_vector["age1"] * df$age + 
  coef_vector["age2"] * df$age2 + 
  coef_vector["gender2"] * df$female + 
  coef_vector["income"] * df$inc + 
  coef_vector["ethnicity2"] * df$ethnicity2 + 
  coef_vector["ethnicity3"] * df$ethnicity3 + 
  coef_vector["ethnicity4"] * df$ethnicity4 +
  coef_vector["region2"] * df$region2 + 
  coef_vector["region3"] * df$region3 + 
  coef_vector["region4"] * df$region4
  

summary(df$pal_predicted)


####################### Adding residuals to predicted PAL ###########################
df$pal_res <- rnorm(nrow(df), mean = 0, sd = pal_sd)

df$pal_with_res <- df$pal_predicted + df$pal_res

summary(df$pal_with_res)

###################### Only keep those with reasonable PAL #########################
df_pal <- df[df$pal_with_res>=1.4 & df$pal_with_res<=2.5, ]
summary(df_pal$pal_with_res)

#################### Calculate energy/physical activity ratio ######################
df_pal$energy_to_pa_ratio = df_pal$energy/df_pal$pal_with_res


result <- df_pal %>%
  group_by(age, sex) %>%
  summarize(
    energy_to_pa_ratio_mean = mean(energy_to_pa_ratio)
  )

merged_df <- merge(df_pal, result, by = c("sex", "age"), all.x = TRUE)

# Calculate normalised energy ratio by dividing energy_to_pa_ratio by energy_to_pa_ratio_mean
normalised_energy_to_pa_ratio <- with(merged_df, energy_to_pa_ratio / energy_to_pa_ratio_mean)

# Set a threshold (cap) at the 99th percentile of the normalised energy ratio
cap <- quantile(normalised_energy_to_pa_ratio , 0.99)

# Keep only values below the cap to filter extreme outliers
normalised_energy_to_pa_ratio  <- normalised_energy_to_pa_ratio [normalised_energy_to_pa_ratio  < cap]

#plot distribution of normalised energy to pa ratio
main_title <- "Distribution of Normalised Energy to Physical Activity Ratio"
plot(density(normalised_energy_to_pa_ratio ), lwd=3, col="red", main = main_title)

# Sample random elements from normalised_energy_ratio
#sampled_elements <- sample(normalised_energy_to_pa_ratio , num_epa_quantiles_to_sample)

# Order the sampled elements
#sampled_elements = sort(sampled_elements)

# Write the sampled elements to a CSV file named "energy_physicalactivity_quantiles.csv"
write.csv(normalised_energy_to_pa_ratio, "Outputs/energy_physicalactivity_quantiles.csv")

#####################################################################################
#####################################################################################
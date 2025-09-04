#######################################################################################################################
## Authors: Jingmin Zhu, Daniel Laydon
## With reference to Ali's scripts
## Date: 23 June 2025

### Working directory ###
setwd("C:/Users/jzhu5/OneDrive - Imperial College London/Health-GPS_SHARED/FINCH/Parametres in healthgps-examples_JZ")
#setwd("C:/Users/dlaydon/OneDrive - Imperial College London/Health-GPS_SHARED/FINCH/Parametres in healthgps-examples_JZ")
#setwd("/Users/jasmine/Library/CloudStorage/OneDrive-ImperialCollegeLondon/Health-GPS_SHARED/FINCH/Parametres in healthgps-examples_JZ")

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

#install.packages(c("stargazer", "moments"))

#######################################################################################################################
######################## Data Loading and Variable Extraction ##########################################################

# Load the CSV data into a data frame


#file_name <- "Health_GPS_ind_20231012.csv"
#file_name <- "/Users/jasmine/Library/CloudStorage/OneDrive-ImperialCollegeLondon/Health-GPS_SHARED/FINCH/Parametres in healthgps-examples_JZ/HealthGPS_initial_population_2598hh.csv"
#file_name <- "C:/Users/dlaydon/OneDrive - Imperial College London/Health-GPS_SHARED/FINCH/Parametres in healthgps-examples_JZ/HealthGPS_initial_population.csv"
#file_name <- "C:/Users/dlaydon/OneDrive - Imperial College London/Health-GPS_SHARED/FINCH/Parametres in healthgps-examples_JZ/HealthGPS_initial_population_2598hh.csv"
file_name <- "HealthGPS_initial_population.csv"

#data <- read.csv(file_name)
data <- read.csv(file.path(getwd(), file_name))
str(data)
head(data, 5)

# Attach the data frame for easier variable referencing - data extracted below - don't need this.
#attach(data)

# Extracting variables from the 'data' data frame
sex <- as.factor(data$sex_person) # 1-male 2-female
age <- data$age_person
age1 <- data$age_person
age2 <- data$age_person * data$age_person
age3 <- data$age_person * data$age_person * data$age_person
inc <- data$hhinc_pc # household income per equalised person
ethnicity <- ifelse(data$ethnicity_person == 3, 2, 
                    ifelse(data$ethnicity_person == 4, 3, 
                           ifelse(data$ethnicity_person %in% c(2, 5), 4, data$ethnicity_person)))
ethnicity <- as.factor(ethnicity)
region <- as.factor(data$country)
protein <- data$protein_w
carb <- data$carbs_w
fat <- data$fats_w
alcohol <- data$alcohol_w
energy_original <- data$calories_w
sodium <- data$sodium_w
polyunsats <- data$polysaturates_w
saturates <- data$saturates_w
monounsats <- data$monosaturates_w
totalsugar <- data$totalsugars_w
addedsugar <- data$addedsugars_w
fibre <- data$fibre_w # AOAC fibre
calcium <- data$calcium_w
iron <- data$iron_w
vitaminc <- data$vitaminc_w
copper <- data$copper_w
zinc <- data$zinc_w

proc_meat <- data$i_calories_weight * data$proc_meat_hh_defra/14 # use DEFRA food and individual weights of calories
red_meat <- data$i_calories_weight * data$red_meat_hh_defra/14 # use DEFRA food and individual weights of calories
fruit <- data$i_calories_weight * data$fruit_hh_defra/14 # use DEFRA food and individual weights of calories
vegetable_only <- data$i_calories_weight * data$vegetable_hh_defra/14 # use DEFRA food and individual weights of calories
legume <- data$i_calories_weight * data$legume_hh_defra/14 # use DEFRA food and individual weights of calories
vegetable <- vegetable_only + pmin(legume, 80)


# Calculate 'energy' based on the standard formula
energy <- 4 * carb + 9 * fat + 4 * protein + 7 * alcohol


############################################################################################
######################## Filtering ##########################################################

# Create 'subdata' dataframe
subdata_all <- data.frame(sex, age, age1, age2, age3, inc, ethnicity, region, carb, fat, 
                          protein, alcohol, energy, energy_original, sodium, polyunsats, 
                          saturates, monounsats, totalsugar, addedsugar, fibre, 
                          calcium, iron, vitaminc, copper, zinc, 
                          proc_meat, red_meat, fruit, vegetable, legume)
str(subdata_all)

# Remove rows with non-numeric NaN or empty values - flagging that we may not want to simply throw away incomplete data. UPDATE: Only gets rid of one row in >455k so fine.
# subdata <- subdata_all[complete.cases(subdata_all), ]
subdata <- subdata_all[!is.na(subdata_all$inc),]
str(subdata)
head(subdata)

sum(is.na(subdata$alcohol))
sum(is.na(subdata$addedsugar))

# Replace missing alcohol and addedsugar with 0
subdata$alcohol <- ifelse(is.na(subdata$alcohol), 0, subdata$alcohol)
subdata$addedsugar <- ifelse(is.na(subdata$addedsugar), 0, subdata$addedsugar)

subdata$energy <- 4 * subdata$carb + 9 * subdata$fat + 4 * subdata$protein + 7 * subdata$alcohol

df <- subdata
head(df)
str(df)
# Set lower and upper quantiles
#upper_q <- 0.99			
#lower_q <- 1-upper_q 

# Filter 'subdata' based on conditions - gets rid of >20k entries
#df <- subdata %>%
  #filter(
    #age < 100,  # Age less than 100
    #carb 	> quantile(carb		, lower_q) & carb 		< quantile(carb		, upper_q),  	# Carb within quantiles
    #fat 	> quantile(fat		, lower_q) & fat 		< quantile(fat		, upper_q),  	# Fat within quantiles
    #protein > quantile(protein	, lower_q) & protein 	< quantile(protein	, upper_q),  	# Protein within quantiles
    #sodium 	> quantile(sodium	, lower_q) & sodium 	< quantile(sodium	, upper_q),  	# Sodium within quantiles
    #energy 	> quantile(energy	, lower_q) & energy 	< quantile(energy	, upper_q)  	# Energy within quantiles
  #)
#str(df)
#head(df)


############################################################################################
######################## Calculate Mean by Age and Sex #######################################

# Calculate mean values for 'carb', 'fat', 'protein', 'sodium', and 'energy' by 'age' and 'sex'
result <- df %>%
  group_by(age, sex) %>%
  summarize(
    carb_mean 		= mean(carb),
    fat_mean 		= mean(fat),
    protein_mean 	= mean(protein),
    sodium_mean 	= mean(sodium),
    energy_mean 	= mean(energy, na.rm = TRUE),
    monounsats_mean = mean(monounsats),
    polyunsats_mean = mean(polyunsats),
    saturates_mean = mean(saturates),
    totalsugar_mean = mean(totalsugar),
    addedsugar_mean = mean(addedsugar[addedsugar != 0]),
    alcohol_mean = mean(alcohol[alcohol != 0]),
    fibre_mean = mean(fibre),
    calcium_mean = mean(calcium),
    iron_mean = mean(iron),
    vitaminc_mean = mean(vitaminc),
    copper_mean = mean(copper),
    zinc_mean = mean(zinc),
    fruit_mean = mean(fruit[fruit != 0]),
    vegetable_mean = mean(vegetable[vegetable != 0]),
    red_meat_mean = mean(red_meat[red_meat != 0]),
    proc_meat_mean = mean(proc_meat[proc_meat != 0]),
    legume_mean = mean(legume[legume != 0])
  )
str(result)
head(result)

# Merge the calculated mean values back to the original dataframe ('df')
merged_df <- merge(df, result, by = c("sex", "age"), all.x = TRUE)
str(merged_df)
head(merged_df, 20)
tail(merged_df, 20)


#########################################################################################
########################### Income model #################################

reg_income = lm(inc ~ sex + age1 + age2 + ethnicity + region, data = merged_df)
#summary(reg_income) # 6049 ind in 2598 hh used
min_income = min(merged_df$inc)
max_income = max(merged_df$inc)
sd_income <- sd(reg_income$residuals)

IncomeDFrame = data.frame(Value = c(reg_income$coefficients, min = min_income, max = max_income, stddev = sd_income))
# fix rownames
rownames(IncomeDFrame) = gsub("\\(Intercept\\)", "Intercept", rownames(IncomeDFrame))
rownames(IncomeDFrame) = gsub("sex2", "gender2", rownames(IncomeDFrame))
colnames(IncomeDFrame) = NULL

write.table(IncomeDFrame, "Outputs/income_model.csv", sep = ",", col.names = F)
#write.table( <yourdf>, sep=",",  col.names=FALSE)
## Create a data frame with the values
#income_range <- data.frame(Min_Income = min_income, Max_Income = max_income)
## Write to a CSV file
#write.csv(income_range, "Outputs/income_range.csv", row.names = FALSE)
#
#
## Write to a CSV file
#write.csv(sd_income, "Outputs/income_sd.csv", row.names = FALSE)

#incomemean <- merged_df |>
 # group_by(age, sex) |>
  #summarise(income_mean = mean(inc))
  
#write.table(incomemean, "Outputs/income_factorsmean.csv", row.names = FALSE)


############################################################################################
######################## BOX-COX Transformation: Carb #######################################

str(merged_df)

# Calculate Box-Cox transformation for 'carb' variable
x	<- merged_df$carb / merged_df$carb_mean

#BLAH = data.frame(Age = merged_df$age, Sex = merged_df$sex, Raw = merged_df$carb, AgeAndSexMeans = merged_df$carb_mean, Weird = x)
#head(BLAH, 50)

#plot(hist(x))
lm(x ~ 1) ## this gives "linear model" expressing x as function of no other variables.
boxcox_results 	<- boxcox(lm(x ~ 1)) 								
lambda_carb 	<- boxcox_results$x[which.max(boxcox_results$y)]

# Apply Box-Cox transformation to 'carb' and create a new variable 'new_x_exact'
merged_df$new_x_exact <- (x^lambda_carb - 1) / lambda_carb

# Fit a linear regression model for the transformed variable
#reg_carb <- lm(new_x_exact ~ sex + age1 + age2 + ethnicity + inc, data = merged_df)
reg_carb <- lm(new_x_exact ~ sex + age1 + age2 + ethnicity + inc + region, data = merged_df)
#SimpleRegCarb <- lm(carb ~ sex + age1 + age2 + ethnicity + inc + region, data = merged_df)

# Display summary statistics of the regression model
summary(reg_carb)
mean(residuals(reg_carb)^2)
#summary(SimpleRegCarb)

# Calculate the standard deviation of the residuals
sd_carb <- sd(reg_carb$residuals)

# Plot the density distribution of 'carb' variable
plot(density(merged_df$carb), col="red", lwd=3, main="Carbs: Original Distribution")

# Plot the density distribution of transformed 'carb' variable
plot(density(merged_df$new_x_exact), col="red", lwd=3, main="Carbs: Transformed Distribution")

# Plot the density distribution of standardized residuals for 'carb' regression
plot(density(scale(reg_carb$residuals)), col="red", main="Carb Residuals", lwd=3)

# Standardize 'carb' residuals
carb_transformed <- scale(reg_carb$residuals)

# Adjust the number of random values for comparison
num_values <- 1000000  
random_values <- rnorm(num_values)

# Plot the density distribution of standardized 'carb' residuals and random values from a normal distribution
main_title <- "Carbs: Transformed Distribution vs. Random Normal Distribution"
plot(density(carb_transformed), col="red", lwd=3, main = main_title)
lines(density(random_values), col="blue", lwd=3)

############################################################################################
######################## BOX-COX Transformation: Fat #########################################

merged_df$fat <- ifelse(merged_df$fat == 0, 0.00001, merged_df$fat)

# Calculate Box-Cox transformation for 'fat' variable
x <- merged_df$fat / merged_df$fat_mean
boxcox_results <- boxcox(lm(x ~ 1))
lambda_fat <- boxcox_results$x[which.max(boxcox_results$y)]

# Apply Box-Cox transformation to 'fat' and create a new variable 'new_x_exact'
merged_df$new_x_exact <- (x^lambda_fat - 1) / lambda_fat

# Plot the density of the transformed variable
plot(density(merged_df$new_x_exact))

# Fit a linear regression model for the transformed variable
#reg_fat <- lm(new_x_exact ~ sex + age1 + age2 + ethnicity + inc, data = merged_df)
reg_fat <- lm(new_x_exact ~ sex + age1 + age2 + ethnicity + inc + region, data = merged_df)

# Display summary statistics of the regression model
summary(reg_fat)

# Calculate the standard deviation of the residuals
sd_fat <- sd(reg_fat$residuals)

# Plot the density distribution of 'fat' variable
plot(density(merged_df$fat), col="red", lwd=3, main="Fat: Original Distribution")

# Plot the density distribution of transformed 'fat' variable
plot(density(merged_df$new_x_exact), col="red", lwd=3, main="Fat: Transformed Distribution")

# Plot the density distribution of standardized residuals for 'fat' regression
plot(density(scale(reg_fat$residuals)), col="red", main="Fat Residuals",lwd=3)

# Standardize 'fat' residuals
fat_transformed <- scale(reg_fat$residuals)

# Adjust the number of random values for comparison
num_values <- 1000000  
random_values <- rnorm(num_values)

# Plot the density distribution of standardized 'fat' residuals and random values from a normal distribution
main_title <- "Fat: Transformed Distribution vs. Random Normal Distribution"
plot(density(fat_transformed), col="red", lwd=3, main = main_title)
lines(density(random_values), col="blue", lwd=3)

############################################################################################
######################## BOX-COX Transformation: Protein ####################################

# Calculate Box-Cox transformation for 'protein' variable
x <- merged_df$protein / merged_df$protein_mean
boxcox_results <- boxcox(lm(x ~ 1))
lambda_protein <- boxcox_results$x[which.max(boxcox_results$y)]

# Apply Box-Cox transformation to 'protein' and create a new variable 'new_x_exact'
merged_df$new_x_exact <- (x^lambda_protein - 1) / lambda_protein

# Fit a linear regression model for the transformed variable
#reg_protein <- lm(new_x_exact ~ sex + age1 + age2 + ethnicity + inc, data = merged_df)
reg_protein <- lm(new_x_exact ~ sex + age1 + age2 + ethnicity + inc + region, data = merged_df)

# Display summary statistics of the regression model
summary(reg_protein)

# Calculate the standard deviation of the residuals
sd_protein <- sd(reg_protein$residuals)

# Plot the density distribution of 'protein' variable
plot(density(merged_df$protein), col="red", lwd=3, main="Protein: Original Distribution")

# Plot the density distribution of transformed 'protein' variable
plot(density(merged_df$new_x_exact), col="red", lwd=3, main="Protein: Transformed Distribution")

# Plot the density distribution of standardized residuals for 'protein' regression
plot(density(scale(reg_protein$residuals)), col="red", main="Protein Residuals",lwd=3)

# Standardize 'protein' residuals
protein_transformed <- scale(reg_protein$residuals)

# Adjust the number of random values for comparison
num_values <- 1000000  
random_values <- rnorm(num_values)

# Plot the density distribution of standardized 'protein' residuals and random values from a normal distribution
main_title <- "Protein: Transformed Distribution vs. Random Normal Distribution"
plot(density(protein_transformed), col="red", lwd=3, main = main_title)
lines(density(random_values), col="blue", lwd=3)
legend("topright", legend = c("transformed", "random normal"), col = c("red", "blue"), lty = 1)

############################################################################################
######################## BOX-COX Transformation: Sodium #######################################################

# Calculate Box-Cox transformation for 'sodium' variable
x <- merged_df$sodium / merged_df$sodium_mean
boxcox_results <- boxcox(lm(x ~ 1))
lambda_sodium <- boxcox_results$x[which.max(boxcox_results$y)]

# Apply Box-Cox transformation to 'sodium' and create a new variable 'new_x_exact'
merged_df$new_x_exact <- (x^lambda_sodium - 1) / lambda_sodium

# Fit a linear regression model for the transformed variable
#reg_sodium <- lm(new_x_exact ~ sex + age1 + age2 + ethnicity + inc, data = merged_df)
reg_sodium <- lm(new_x_exact ~ sex + age1 + age2 + ethnicity + inc + region, data = merged_df)

# Display summary statistics of the regression model
summary(reg_sodium)

# Calculate the standard deviation of the residuals
sd_sodium <- sd(reg_sodium$residuals)

# Plot the density distribution of 'sodium' variable
plot(density(merged_df$sodium), col="red", lwd=3, main="Sodium: Original Distribution")

# Plot the density distribution of transformed 'sodium' variable
plot(density(merged_df$new_x_exact), col="red", lwd=3, main="Sodium: Transformed Distribution")

# Plot the density distribution of standardized residuals for 'sodium' regression
plot(density(scale(reg_sodium$residuals)), col="red", main="Sodium Residuals",lwd=3)

# Standardize 'sodium' residuals
sodium_transformed <- scale(reg_sodium$residuals)

# Adjust the number of random values for comparison
num_values <- 1000000  
random_values <- rnorm(num_values)

# Plot the density distribution of standardized 'sodium' residuals and random values from a normal distribution
main_title <- "Sodium: Transformed Distribution vs. Random Normal Distribution"
plot(density(sodium_transformed), col="red", lwd=3, main = main_title)
lines(density(random_values), col="blue", lwd=3)



############################################################################################
######################## BOX-COX Transformation: Monounsaturated fats #######################################################

merged_df$monounsats <- ifelse(merged_df$monounsats == 0, 0.00001, merged_df$monounsats)

# Calculate Box-Cox transformation for 'monounsats' variable
x <- merged_df$monounsats / merged_df$monounsats_mean
boxcox_results <- boxcox(lm(x ~ 1))
lambda_monounsats <- boxcox_results$x[which.max(boxcox_results$y)]

# Apply Box-Cox transformation to 'monounsats' and create a new variable 'new_x_exact'
merged_df$new_x_exact <- (x^lambda_monounsats - 1) / lambda_monounsats

# Fit a linear regression model for the transformed variable
#reg_monounsats <- lm(new_x_exact ~ sex + age1 + age2 + ethnicity + inc, data = merged_df)
reg_monounsats <- lm(new_x_exact ~ sex + age1 + age2 + ethnicity + inc + region, data = merged_df)

# Display summary statistics of the regression model
summary(reg_monounsats)

# Calculate the standard deviation of the residuals
sd_monounsats <- sd(reg_monounsats$residuals)

# Plot the density distribution of 'monounsats' variable
plot(density(merged_df$monounsats), col="red", lwd=3, main="monounsats: Original Distribution")

# Plot the density distribution of transformed 'monounsats' variable
plot(density(merged_df$new_x_exact), col="red", lwd=3, main="monounsats: Transformed Distribution")

# Plot the density distribution of standardized residuals for 'monounsats' regression
plot(density(scale(reg_monounsats$residuals)), col="red", main="monounsats Residuals",lwd=3)

# Standardize 'monounsats' residuals
monounsats_transformed <- scale(reg_monounsats$residuals)

# Adjust the number of random values for comparison
num_values <- 1000000  
random_values <- rnorm(num_values)

# Plot the density distribution of standardized 'monounsats' residuals and random values from a normal distribution
main_title <- "Monounsats: Transformed Distribution vs. Random Normal Distribution"
plot(density(monounsats_transformed), col="red", lwd=3, main = main_title)
lines(density(random_values), col="blue", lwd=3)

############################################################################################
######################## BOX-COX Transformation: Polyunsaturated fats #######################################################

merged_df$polyunsats <- ifelse(merged_df$polyunsats == 0, 0.00001, merged_df$polyunsats)

# Calculate Box-Cox transformation for 'polyunsats' variable
x <- merged_df$polyunsats / merged_df$polyunsats_mean
boxcox_results <- boxcox(lm(x ~ 1))
lambda_polyunsats <- boxcox_results$x[which.max(boxcox_results$y)]

# Apply Box-Cox transformation to 'polyunsats' and create a new variable 'new_x_exact'
merged_df$new_x_exact <- (x^lambda_polyunsats - 1) / lambda_polyunsats

# Fit a linear regression model for the transformed variable
#reg_polyunsats <- lm(new_x_exact ~ sex + age1 + age2 + ethnicity + inc, data = merged_df)
reg_polyunsats <- lm(new_x_exact ~ sex + age1 + age2 + ethnicity + inc + region, data = merged_df)

# Display summary statistics of the regression model
summary(reg_polyunsats)

# Calculate the standard deviation of the residuals
sd_polyunsats <- sd(reg_polyunsats$residuals)

# Plot the density distribution of 'polyunsats' variable
plot(density(merged_df$polyunsats), col="red", lwd=3, main="polyunsats: Original Distribution")

# Plot the density distribution of transformed 'polyunsats' variable
plot(density(merged_df$new_x_exact), col="red", lwd=3, main="polyunsats: Transformed Distribution")

# Plot the density distribution of standardized residuals for 'polyunsats' regression
plot(density(scale(reg_polyunsats$residuals)), col="red", main="polyunsats Residuals",lwd=3)

# Standardize 'polyunsats' residuals
polyunsats_transformed <- scale(reg_polyunsats$residuals)

# Adjust the number of random values for comparison
num_values <- 1000000  
random_values <- rnorm(num_values)

# Plot the density distribution of standardized 'polyunsats' residuals and random values from a normal distribution
main_title <- "polyunsats: Transformed Distribution vs. Random Normal Distribution"
plot(density(polyunsats_transformed), col="red", lwd=3, main = main_title)
lines(density(random_values), col="blue", lwd=3)



############################################################################################
######################## BOX-COX Transformation: Saturated fats #######################################################

merged_df$saturates <- ifelse(merged_df$saturates == 0, 0.00001, merged_df$saturates)

# Calculate Box-Cox transformation for 'saturates' variable
x <- merged_df$saturates / merged_df$saturates_mean
boxcox_results <- boxcox(lm(x ~ 1))
lambda_saturates <- boxcox_results$x[which.max(boxcox_results$y)]

# Apply Box-Cox transformation to 'saturates' and create a new variable 'new_x_exact'
merged_df$new_x_exact <- (x^lambda_saturates - 1) / lambda_saturates

# Fit a linear regression model for the transformed variable
#reg_saturates <- lm(new_x_exact ~ sex + age1 + age2 + ethnicity + inc, data = merged_df)
reg_saturates <- lm(new_x_exact ~ sex + age1 + age2 + ethnicity + inc + region, data = merged_df)

# Display summary statistics of the regression model
summary(reg_saturates)

# Calculate the standard deviation of the residuals
sd_saturates <- sd(reg_saturates$residuals)

# Plot the density distribution of 'saturates' variable
plot(density(merged_df$saturates), col="red", lwd=3, main="saturates: Original Distribution")

# Plot the density distribution of transformed 'saturates' variable
plot(density(merged_df$new_x_exact), col="red", lwd=3, main="saturates: Transformed Distribution")

# Plot the density distribution of standardized residuals for 'saturates' regression
plot(density(scale(reg_saturates$residuals)), col="red", main="saturates Residuals",lwd=3)

# Standardize 'saturates' residuals
saturates_transformed <- scale(reg_saturates$residuals)

# Adjust the number of random values for comparison
num_values <- 1000000  
random_values <- rnorm(num_values)

# Plot the density distribution of standardized 'saturates' residuals and random values from a normal distribution
main_title <- "saturates: Transformed Distribution vs. Random Normal Distribution"
plot(density(saturates_transformed), col="red", lwd=3, main = main_title)
lines(density(random_values), col="blue", lwd=3)



############################################################################################
######################## BOX-COX Transformation: Total sugar #######################################################

# Calculate Box-Cox transformation for 'totalsugar' variable
x <- merged_df$totalsugar / merged_df$totalsugar_mean
boxcox_results <- boxcox(lm(x ~ 1))
lambda_totalsugar <- boxcox_results$x[which.max(boxcox_results$y)]

# Apply Box-Cox transformation to 'totalsugar' and create a new variable 'new_x_exact'
merged_df$new_x_exact <- (x^lambda_totalsugar - 1) / lambda_totalsugar

# Fit a linear regression model for the transformed variable
#reg_totalsugar <- lm(new_x_exact ~ sex + age1 + age2 + ethnicity + inc, data = merged_df)
reg_totalsugar <- lm(new_x_exact ~ sex + age1 + age2 + ethnicity + inc + region, data = merged_df)

# Display summary statistics of the regression model
summary(reg_totalsugar)

# Calculate the standard deviation of the residuals
sd_totalsugar <- sd(reg_totalsugar$residuals)

# Plot the density distribution of 'totalsugar' variable
plot(density(merged_df$totalsugar), col="red", lwd=3, main="totalsugar: Original Distribution")

# Plot the density distribution of transformed 'totalsugar' variable
plot(density(merged_df$new_x_exact), col="red", lwd=3, main="totalsugar: Transformed Distribution")

# Plot the density distribution of standardized residuals for 'totalsugar' regression
plot(density(scale(reg_totalsugar$residuals)), col="red", main="totalsugar Residuals",lwd=3)

# Standardize 'totalsugar' residuals
totalsugar_transformed <- scale(reg_totalsugar$residuals)

# Adjust the number of random values for comparison
num_values <- 1000000  
random_values <- rnorm(num_values)

# Plot the density distribution of standardized 'totalsugar' residuals and random values from a normal distribution
main_title <- "totalsugar: Transformed Distribution vs. Random Normal Distribution"
plot(density(totalsugar_transformed), col="red", lwd=3, main = main_title)
lines(density(random_values), col="blue", lwd=3)



############################################################################################
######################## BOX-COX Transformation: Fibre ####################################

merged_df$fibre <- ifelse(merged_df$fibre == 0, 0.00001, merged_df$fibre)

# Calculate Box-Cox transformation for 'fibre' variable
x 				<- merged_df$fibre / merged_df$fibre_mean
#plot(hist(x))
lm(x ~ 1) ## this gives "linear model" expressing x as function of no other variables.
boxcox_results 	<- boxcox(lm(x ~ 1)) 								
lambda_fibre 	<- boxcox_results$x[which.max(boxcox_results$y)]

# Apply Box-Cox transformation to 'fibre' and create a new variable 'new_x_exact'
merged_df$new_x_exact <- (x^lambda_fibre - 1) / lambda_fibre

# Fit a linear regression model for the transformed variable
#reg_fibre <- lm(new_x_exact ~ sex + age1 + age2 + ethnicity + inc, data = merged_df)
reg_fibre <- lm(new_x_exact ~ sex + age1 + age2 + ethnicity + inc + region, data = merged_df)

# Display summary statistics of the regression model
summary(reg_fibre)

# Calculate the standard deviation of the residuals
sd_fibre <- sd(reg_fibre$residuals)

# Plot the density distribution of 'fibre' variable
plot(density(merged_df$fibre), col="red", lwd=3, main="fibres: Original Distribution")

# Plot the density distribution of transformed 'fibre' variable
plot(density(merged_df$new_x_exact), col="red", lwd=3, main="fibres: Transformed Distribution")

# Plot the density distribution of standardized residuals for 'fibre' regression
plot(density(scale(reg_fibre$residuals)), col="red", main="fibre Residuals", lwd=3)

# Standardize 'fibre' residuals
fibre_transformed <- scale(reg_fibre$residuals)

# Adjust the number of random values for comparison
num_values <- 1000000  
random_values <- rnorm(num_values)

# Plot the density distribution of standardized 'fibre' residuals and random values from a normal distribution
main_title <- "fibres: Transformed Distribution vs. Random Normal Distribution"
plot(density(fibre_transformed), col="red", lwd=3, main = main_title)
lines(density(random_values), col="blue", lwd=3)



############################################################################################
######################## Two-stage modelling: Alcohol #######################################################
## Logistic regression
logistic_df <- merged_df |>
  mutate(alcohol_bi = ifelse(alcohol == 0, 1, 0))
str(logistic_df)
DFRAME = data.frame(Alchohol = logistic_df$alcohol, Binary = logistic_df$alcohol_bi)

logistic_alcohol <- glm(alcohol_bi ~ sex + age1 + age2 + ethnicity + inc + region, 
                        data = logistic_df,
                        family = binomial)
summary(logistic_alcohol)

sd_logistic_alcohol <- sd(logistic_alcohol$residuals)

## Non-zero only: Box-Cox transformation
merged_alcohol <- merged_df[merged_df$alcohol>0, ]

# Calculate Box-Cox transformation for 'alcohol' variable
x <- merged_alcohol$alcohol / merged_alcohol$alcohol_mean
boxcox_results <- boxcox(lm(x ~ 1))
lambda_alcohol <- boxcox_results$x[which.max(boxcox_results$y)]

# Apply Box-Cox transformation to 'alcohol' and create a new variable 'new_x_exact'
merged_alcohol$new_x_exact <- (x^lambda_alcohol - 1) / lambda_alcohol

# Fit a linear regression model for the transformed variable
#reg_alcohol <- lm(new_x_exact ~ sex + age1 + age2 + ethnicity + inc, data = merged_df)
reg_alcohol <- lm(new_x_exact ~ sex + age1 + age2 + ethnicity + inc + region, data = merged_alcohol)

# Display summary statistics of the regression model
summary(reg_alcohol)

# Calculate the standard deviation of the residuals
sd_alcohol <- sd(reg_alcohol$residuals)

# Plot the density distribution of 'alcohol' variable
plot(density(merged_alcohol$alcohol), col="red", lwd=3, main="Alcohol: Original Distribution")

# Plot the density distribution of transformed 'alcohol' variable
plot(density(merged_alcohol$new_x_exact), col="red", lwd=3, main="Alcohol: Transformed Distribution")

# Plot the density distribution of standardized residuals for 'alcohol' regression
plot(density(scale(reg_alcohol$residuals)), col="red", main="Alcohol Residuals",lwd=3)

# Standardize 'alcohol' residuals
alcohol_transformed <- scale(reg_alcohol$residuals)

# Adjust the number of random values for comparison
num_values <- 1000000  
random_values <- rnorm(num_values)

# Plot the density distribution of standardized 'alcohol' residuals and random values from a normal distribution
main_title <- "Alcohol: Transformed Distribution vs. Random Normal Distribution"
plot(density(alcohol_transformed), col="red", lwd=3, main = main_title)
lines(density(random_values), col="blue", lwd=3)



############################################################################################
######################## Two-stage modelling: Added sugar #######################################################
## Logistic regression
logistic_df <- merged_df |>
  mutate(addedsugar_bi = ifelse(addedsugar == 0, 1, 0))

logistic_addedsugar <- glm(addedsugar_bi ~ sex + age1 + age2 + ethnicity + inc + region, 
                        data = logistic_df,
                        family = binomial)
summary(logistic_addedsugar)

sd_logistic_addedsugar <- sd(logistic_addedsugar$residuals)

## Non-zero only: Box-Cox Transformation
merged_addedsugar <- merged_df[merged_df$addedsugar > 0, ]

# Calculate Box-Cox transformation for 'addedsugar' variable
x <- merged_addedsugar$addedsugar / merged_addedsugar$addedsugar_mean
boxcox_results <- boxcox(lm(x ~ 1))
lambda_addedsugar <- boxcox_results$x[which.max(boxcox_results$y)]

# Apply Box-Cox transformation to 'addedsugar' and create a new variable 'new_x_exact'
merged_addedsugar$new_x_exact <- (x^lambda_addedsugar - 1) / lambda_addedsugar

# Fit a linear regression model for the transformed variable
#reg_addedsugar <- lm(new_x_exact ~ sex + age1 + age2 + ethnicity + inc, data = merged_df)
reg_addedsugar <- lm(new_x_exact ~ sex + age1 + age2 + ethnicity + inc + region, data = merged_addedsugar)

# Display summary statistics of the regression model
summary(reg_addedsugar)

# Calculate the standard deviation of the residuals
sd_addedsugar <- sd(reg_addedsugar$residuals)

# Plot the density distribution of 'addedsugar' variable
plot(density(merged_addedsugar$addedsugar), col="red", lwd=3, main="addedsugar: Original Distribution")

# Plot the density distribution of transformed 'addedsugar' variable
plot(density(merged_addedsugar$new_x_exact), col="red", lwd=3, main="addedsugar: Transformed Distribution")

# Plot the density distribution of standardized residuals for 'addedsugar' regression
plot(density(scale(reg_addedsugar$residuals)), col="red", main="addedsugar Residuals",lwd=3)

# Standardize 'addedsugar' residuals
addedsugar_transformed <- scale(reg_addedsugar$residuals)

# Adjust the number of random values for comparison
num_values <- 1000000  
random_values <- rnorm(num_values)

# Plot the density distribution of standardized 'addedsugar' residuals and random values from a normal distribution
main_title <- "addedsugar: Transformed Distribution vs. Random Normal Distribution"
plot(density(addedsugar_transformed), col="red", lwd=3, main = main_title)
lines(density(random_values), col="blue", lwd=3)



############################################################################################
######################## Two-stage modelling: Fruit ####################################
## Logistic regression
logistic_df <- merged_df |>
  mutate(fruit_bi = ifelse(fruit == 0, 1, 0))

logistic_fruit <- glm(fruit_bi ~ sex + age1 + age2 + ethnicity + inc + region, 
                           data = logistic_df,
                           family = binomial)
summary(logistic_fruit)

sd_logistic_fruit <- sd(logistic_fruit$residuals)

## Non-zero
merged_fruit <- merged_df[merged_df$fruit>0,]

# Calculate Box-Cox transformation for 'fruit' variable
x 				<- merged_fruit$fruit / merged_fruit$fruit_mean
#plot(hist(x))
lm(x ~ 1) ## this gives "linear model" expressing x as function of no other variables.
boxcox_results 	<- boxcox(lm(x ~ 1)) 								
lambda_fruit 	<- boxcox_results$x[which.max(boxcox_results$y)]

# Apply Box-Cox transformation to 'fruit' and create a new variable 'new_x_exact'
merged_fruit$new_x_exact <- (x^lambda_fruit - 1) / lambda_fruit

# Fit a linear regression model for the transformed variable
#reg_fruit <- lm(new_x_exact ~ sex + age1 + age2 + ethnicity + inc, data = merged_df)
reg_fruit <- lm(new_x_exact ~ sex + age1 + age2 + ethnicity + inc + region, data = merged_fruit)

# Display summary statistics of the regression model
summary(reg_fruit)

# Calculate the standard deviation of the residuals
sd_fruit <- sd(reg_fruit$residuals)

# Plot the density distribution of 'fruit' variable
plot(density(merged_fruit$fruit), col="red", lwd=3, main="fruits: Original Distribution")

# Plot the density distribution of transformed 'fruit' variable
plot(density(merged_fruit$new_x_exact), col="red", lwd=3, main="fruits: Transformed Distribution")

# Plot the density distribution of standardized residuals for 'fruit' regression
plot(density(scale(reg_fruit$residuals)), col="red", main="fruit Residuals", lwd=3)

# Standardize 'fruit' residuals
fruit_transformed <- scale(reg_fruit$residuals)

# Adjust the number of random values for comparison
num_values <- 1000000  
random_values <- rnorm(num_values)

# Plot the density distribution of standardized 'fruit' residuals and random values from a normal distribution
main_title <- "fruits: Transformed Distribution vs. Random Normal Distribution"
plot(density(fruit_transformed), col="red", lwd=3, main = main_title)
lines(density(random_values), col="blue", lwd=3)



############################################################################################
######################## Two-stage modelling: Vegetable ####################################
## Logistic regression
logistic_df <- merged_df |>
  mutate(vegetable_bi = ifelse(vegetable == 0, 1, 0))

logistic_vegetable <- glm(vegetable_bi ~ sex + age1 + age2 + ethnicity + inc + region, 
                      data = logistic_df,
                      family = binomial)
summary(logistic_vegetable)

sd_logistic_vegetable <- sd(logistic_vegetable$residuals)

## Non-zero
merged_vegetable <- merged_df[merged_df$vegetable>0, ]


# Calculate Box-Cox transformation for 'vegetable' variable
x 				<- merged_vegetable$vegetable / merged_vegetable$vegetable_mean
#plot(hist(x))
lm(x ~ 1) ## this gives "linear model" expressing x as function of no other variables.
boxcox_results 	<- boxcox(lm(x ~ 1)) 								
lambda_vegetable 	<- boxcox_results$x[which.max(boxcox_results$y)]

# Apply Box-Cox transformation to 'vegetable' and create a new variable 'new_x_exact'
merged_vegetable$new_x_exact <- (x^lambda_vegetable - 1) / lambda_vegetable

# Fit a linear regression model for the transformed variable
#reg_vegetable <- lm(new_x_exact ~ sex + age1 + age2 + ethnicity + inc, data = merged_df)
reg_vegetable <- lm(new_x_exact ~ sex + age1 + age2 + ethnicity + inc + region, data = merged_vegetable)

# Display summary statistics of the regression model
summary(reg_vegetable)

# Calculate the standard deviation of the residuals
sd_vegetable <- sd(reg_vegetable$residuals)

# Plot the density distribution of 'vegetable' variable
plot(density(merged_vegetable$vegetable), col="red", lwd=3, main="vegetables: Original Distribution")

# Plot the density distribution of transformed 'vegetable' variable
plot(density(merged_vegetable$new_x_exact), col="red", lwd=3, main="vegetables: Transformed Distribution")

# Plot the density distribution of standardized residuals for 'vegetable' regression
plot(density(scale(reg_vegetable$residuals)), col="red", main="vegetable Residuals", lwd=3)

# Standardize 'vegetable' residuals
vegetable_transformed <- scale(reg_vegetable$residuals)

# Adjust the number of random values for comparison
num_values <- 1000000  
random_values <- rnorm(num_values)

# Plot the density distribution of standardized 'vegetable' residuals and random values from a normal distribution
main_title <- "vegetables: Transformed Distribution vs. Random Normal Distribution"
plot(density(vegetable_transformed), col="red", lwd=3, main = main_title)
lines(density(random_values), col="blue", lwd=3)



############################################################################################
######################## Two-stage modelling: Legume ####################################
## Logistic regression
logistic_df <- merged_df |>
  mutate(legume_bi = ifelse(legume == 0, 1, 0))

logistic_legume <- glm(legume_bi ~ sex + age1 + age2 + ethnicity + inc + region, 
                          data = logistic_df,
                          family = binomial)
summary(logistic_legume)

sd_logistic_legume <- sd(logistic_legume$residuals)

## Non-zero
merged_legume <- merged_df[merged_df$legume>0, ]


# Calculate Box-Cox transformation for 'legume' variable
x 				<- merged_legume$legume / merged_legume$legume_mean
#plot(hist(x))
lm(x ~ 1) ## this gives "linear model" expressing x as function of no other variables.
boxcox_results 	<- boxcox(lm(x ~ 1)) 								
lambda_legume 	<- boxcox_results$x[which.max(boxcox_results$y)]

# Apply Box-Cox transformation to 'legume' and create a new variable 'new_x_exact'
merged_legume$new_x_exact <- (x^lambda_legume - 1) / lambda_legume

# Fit a linear regression model for the transformed variable
#reg_legume <- lm(new_x_exact ~ sex + age1 + age2 + ethnicity + inc, data = merged_df)
reg_legume <- lm(new_x_exact ~ sex + age1 + age2 + ethnicity + inc + region, data = merged_legume)

# Display summary statistics of the regression model
summary(reg_legume)

# Calculate the standard deviation of the residuals
sd_legume <- sd(reg_legume$residuals)

# Plot the density distribution of 'legume' variable
plot(density(merged_legume$legume), col="red", lwd=3, main="legumes: Original Distribution")

# Plot the density distribution of transformed 'legume' variable
plot(density(merged_legume$new_x_exact), col="red", lwd=3, main="legumes: Transformed Distribution")

# Plot the density distribution of standardized residuals for 'legume' regression
plot(density(scale(reg_legume$residuals)), col="red", main="legume Residuals", lwd=3)

# Standardize 'legume' residuals
legume_transformed <- scale(reg_legume$residuals)

# Adjust the number of random values for comparison
num_values <- 1000000  
random_values <- rnorm(num_values)

# Plot the density distribution of standardized 'legume' residuals and random values from a normal distribution
main_title <- "legumes: Transformed Distribution vs. Random Normal Distribution"
plot(density(legume_transformed), col="red", lwd=3, main = main_title)
lines(density(random_values), col="blue", lwd=3)



############################################################################################
######################## Two-stage modelling: Red meat ####################################
## Logistic regression
logistic_df <- merged_df |>
  mutate(red_meat_bi = ifelse(red_meat == 0, 1, 0))

logistic_red_meat <- glm(red_meat_bi ~ sex + age1 + age2 + ethnicity + inc + region, 
                          data = logistic_df,
                          family = binomial)
summary(logistic_red_meat)

sd_logistic_red_meat <- sd(logistic_red_meat$residuals)

## Non-zero
merged_redmeat <- merged_df[merged_df$red_meat>0, ]


# Calculate Box-Cox transformation for 'red_meat' variable
x 				<- merged_redmeat$red_meat / merged_redmeat$red_meat_mean
#plot(hist(x))
lm(x ~ 1) ## this gives "linear model" expressing x as function of no other variables.
boxcox_results 	<- boxcox(lm(x ~ 1)) 								
lambda_red_meat 	<- boxcox_results$x[which.max(boxcox_results$y)]

# Apply Box-Cox transformation to 'red_meat' and create a new variable 'new_x_exact'
merged_redmeat$new_x_exact <- (x^lambda_red_meat - 1) / lambda_red_meat

# Fit a linear regression model for the transformed variable
#reg_red_meat <- lm(new_x_exact ~ sex + age1 + age2 + ethnicity + inc, data = merged_df)
reg_red_meat <- lm(new_x_exact ~ sex + age1 + age2 + ethnicity + inc + region, data = merged_redmeat)

# Display summary statistics of the regression model
summary(reg_red_meat)

# Calculate the standard deviation of the residuals
sd_red_meat <- sd(reg_red_meat$residuals)

# Plot the density distribution of 'red_meat' variable
plot(density(merged_redmeat$red_meat), col="red", lwd=3, main="red_meats: Original Distribution")

# Plot the density distribution of transformed 'red_meat' variable
plot(density(merged_redmeat$new_x_exact), col="red", lwd=3, main="red_meats: Transformed Distribution")

# Plot the density distribution of standardized residuals for 'red_meat' regression
plot(density(scale(reg_red_meat$residuals)), col="red", main="red_meat Residuals", lwd=3)

# Standardize 'red_meat' residuals
red_meat_transformed <- scale(reg_red_meat$residuals)

# Adjust the number of random values for comparison
num_values <- 1000000  
random_values <- rnorm(num_values)

# Plot the density distribution of standardized 'red_meat' residuals and random values from a normal distribution
main_title <- "red_meats: Transformed Distribution vs. Random Normal Distribution"
plot(density(red_meat_transformed), col="red", lwd=3, main = main_title)
lines(density(random_values), col="blue", lwd=3)



############################################################################################
######################## Two-stage modelling: Processed meat ####################################
## Logistic regression
logistic_df <- merged_df |>
  mutate(proc_meat_bi = ifelse(proc_meat == 0, 1, 0))

logistic_proc_meat <- glm(proc_meat_bi ~ sex + age1 + age2 + ethnicity + inc + region, 
                          data = logistic_df,
                          family = binomial)
summary(logistic_proc_meat)

sd_logistic_proc_meat <- sd(logistic_proc_meat$residuals)

## Non-zero
merged_processedmeat <- merged_df[merged_df$proc_meat>0, ]


# Calculate Box-Cox transformation for 'proc_meat' variable
x 				<- merged_processedmeat$proc_meat / merged_processedmeat$proc_meat_mean
#plot(hist(x))
lm(x ~ 1) ## this gives "linear model" expressing x as function of no other variables.
boxcox_results 	<- boxcox(lm(x ~ 1)) 								
lambda_proc_meat 	<- boxcox_results$x[which.max(boxcox_results$y)]

# Apply Box-Cox transformation to 'proc_meat' and create a new variable 'new_x_exact'
merged_processedmeat$new_x_exact <- (x^lambda_proc_meat - 1) / lambda_proc_meat

# Fit a linear regression model for the transformed variable
#reg_proc_meat <- lm(new_x_exact ~ sex + age1 + age2 + ethnicity + inc, data = merged_df)
reg_proc_meat <- lm(new_x_exact ~ sex + age1 + age2 + ethnicity + inc + region, data = merged_processedmeat)

# Display summary statistics of the regression model
summary(reg_proc_meat)

# Calculate the standard deviation of the residuals
sd_proc_meat <- sd(reg_proc_meat$residuals)

# Plot the density distribution of 'proc_meat' variable
plot(density(merged_processedmeat$proc_meat), col="red", lwd=3, main="proc_meats: Original Distribution")

# Plot the density distribution of transformed 'proc_meat' variable
plot(density(merged_processedmeat$new_x_exact), col="red", lwd=3, main="proc_meats: Transformed Distribution")

# Plot the density distribution of standardized residuals for 'proc_meat' regression
plot(density(scale(reg_proc_meat$residuals)), col="red", main="proc_meat Residuals", lwd=3)

# Standardize 'proc_meat' residuals
proc_meat_transformed <- scale(reg_proc_meat$residuals)

# Adjust the number of random values for comparison
num_values <- 1000000  
random_values <- rnorm(num_values)

# Plot the density distribution of standardized 'proc_meat' residuals and random values from a normal distribution
main_title <- "proc_meats: Transformed Distribution vs. Random Normal Distribution"
plot(density(proc_meat_transformed), col="red", lwd=3, main = main_title)
lines(density(random_values), col="blue", lwd=3)




############################################################################################
######################## BOX-COX Transformation: Calcium ####################################

# Calculate Box-Cox transformation for 'calcium' variable
x 				<- merged_df$calcium / merged_df$calcium_mean
#plot(hist(x))
lm(x ~ 1) ## this gives "linear model" expressing x as function of no other variables.
boxcox_results 	<- boxcox(lm(x ~ 1)) 								
lambda_calcium 	<- boxcox_results$x[which.max(boxcox_results$y)]

# Apply Box-Cox transformation to 'calcium' and create a new variable 'new_x_exact'
merged_df$new_x_exact <- (x^lambda_calcium - 1) / lambda_calcium

# Fit a linear regression model for the transformed variable
#reg_fibre <- lm(new_x_exact ~ sex + age1 + age2 + ethnicity + inc, data = merged_df)
reg_calcium <- lm(new_x_exact ~ sex + age1 + age2 + ethnicity + inc + region, data = merged_df)

# Display summary statistics of the regression model
summary(reg_calcium)

# Calculate the standard deviation of the residuals
sd_calcium <- sd(reg_calcium$residuals)

# Plot the density distribution
plot(density(merged_df$calcium), col="red", lwd=3, main="Calcium: Original Distribution")

# Plot the density distribution of transformed variable
plot(density(merged_df$new_x_exact), col="red", lwd=3, main="Calcium: Transformed Distribution")

# Plot the density distribution of standardized residuals for regression
plot(density(scale(reg_calcium$residuals)), col="red", main="Calcium Residuals", lwd=3)

# Standardize residuals
calcium_transformed <- scale(reg_calcium$residuals)

# Adjust the number of random values for comparison
num_values <- 1000000  
random_values <- rnorm(num_values)

# Plot the density distribution of standardized residuals and random values from a normal distribution
main_title <- "Calcium: Transformed Distribution vs. Random Normal Distribution"
plot(density(calcium_transformed), col="red", lwd=3, main = main_title)
lines(density(random_values), col="blue", lwd=3)



############################################################################################
######################## BOX-COX Transformation: Iron ####################################

# Calculate Box-Cox transformation for 'iron' variable
x 				<- merged_df$iron / merged_df$iron_mean
#plot(hist(x))
lm(x ~ 1) ## this gives "linear model" expressing x as function of no other variables.
boxcox_results 	<- boxcox(lm(x ~ 1)) 								
lambda_iron 	<- boxcox_results$x[which.max(boxcox_results$y)]

# Apply Box-Cox transformation and create a new variable 'new_x_exact'
merged_df$new_x_exact <- (x^lambda_iron - 1) / lambda_iron

# Fit a linear regression model for the transformed variable
#reg_fibre <- lm(new_x_exact ~ sex + age1 + age2 + ethnicity + inc, data = merged_df)
reg_iron <- lm(new_x_exact ~ sex + age1 + age2 + ethnicity + inc + region, data = merged_df)

# Display summary statistics of the regression model
summary(reg_iron)

# Calculate the standard deviation of the residuals
sd_iron <- sd(reg_iron$residuals)

# Plot the density distribution
plot(density(merged_df$iron), col="red", lwd=3, main="Iron: Original Distribution")

# Plot the density distribution of transformed variable
plot(density(merged_df$new_x_exact), col="red", lwd=3, main="Iron: Transformed Distribution")

# Plot the density distribution of standardized residuals for regression
plot(density(scale(reg_iron$residuals)), col="red", main="Iron Residuals", lwd=3)

# Standardize residuals
iron_transformed <- scale(reg_iron$residuals)

# Adjust the number of random values for comparison
num_values <- 1000000  
random_values <- rnorm(num_values)

# Plot the density distribution of standardized residuals and random values from a normal distribution
main_title <- "Iron: Transformed Distribution vs. Random Normal Distribution"
plot(density(iron_transformed), col="red", lwd=3, main = main_title)
lines(density(random_values), col="blue", lwd=3)


############################################################################################
######################## BOX-COX Transformation: Vitamin C ####################################

# Calculate Box-Cox transformation for 'vitaminc' variable
x 				<- merged_df$vitaminc / merged_df$vitaminc_mean
#plot(hist(x))
lm(x ~ 1) ## this gives "linear model" expressing x as function of no other variables.
boxcox_results 	<- boxcox(lm(x ~ 1)) 								
lambda_vitaminc 	<- boxcox_results$x[which.max(boxcox_results$y)]

# Apply Box-Cox transformation and create a new variable 'new_x_exact'
merged_df$new_x_exact <- (x^lambda_vitaminc - 1) / lambda_vitaminc

# Fit a linear regression model for the transformed variable
#reg_fibre <- lm(new_x_exact ~ sex + age1 + age2 + ethnicity + inc, data = merged_df)
reg_vitaminc <- lm(new_x_exact ~ sex + age1 + age2 + ethnicity + inc + region, data = merged_df)

# Display summary statistics of the regression model
summary(reg_vitaminc)

# Calculate the standard deviation of the residuals
sd_vitaminc <- sd(reg_vitaminc$residuals)

# Plot the density distribution
plot(density(merged_df$vitaminc), col="red", lwd=3, main="VitaminC: Original Distribution")

# Plot the density distribution of transformed variable
plot(density(merged_df$new_x_exact), col="red", lwd=3, main="VitaminC: Transformed Distribution")

# Plot the density distribution of standardized residuals for regression
plot(density(scale(reg_vitaminc$residuals)), col="red", main="VitaminC Residuals", lwd=3)

# Standardize residuals
vitaminc_transformed <- scale(reg_vitaminc$residuals)

# Adjust the number of random values for comparison
num_values <- 1000000  
random_values <- rnorm(num_values)

# Plot the density distribution of standardized residuals and random values from a normal distribution
main_title <- "VitaminC: Transformed Distribution vs. Random Normal Distribution"
plot(density(vitaminc_transformed), col="red", lwd=3, main = main_title)
lines(density(random_values), col="blue", lwd=3)



############################################################################################
######################## BOX-COX Transformation: Copper ####################################

merged_df$copper <- ifelse(merged_df$copper == 0, 0.00001, merged_df$copper)

# Calculate Box-Cox transformation for 'copper' variable
x 				<- merged_df$copper / merged_df$copper_mean
#plot(hist(x))
lm(x ~ 1) ## this gives "linear model" expressing x as function of no other variables.
boxcox_results 	<- boxcox(lm(x ~ 1)) 								
lambda_copper 	<- boxcox_results$x[which.max(boxcox_results$y)]

# Apply Box-Cox transformation and create a new variable 'new_x_exact'
merged_df$new_x_exact <- (x^lambda_copper - 1) / lambda_copper

# Fit a linear regression model for the transformed variable
reg_copper <- lm(new_x_exact ~ sex + age1 + age2 + ethnicity + inc + region, data = merged_df)

# Display summary statistics of the regression model
summary(reg_copper)

# Calculate the standard deviation of the residuals
sd_copper <- sd(reg_copper$residuals)

# Plot the density distribution
plot(density(merged_df$copper), col="red", lwd=3, main="Copper: Original Distribution")

# Plot the density distribution of transformed variable
plot(density(merged_df$new_x_exact), col="red", lwd=3, main="Copper: Transformed Distribution")

# Plot the density distribution of standardized residuals for regression
plot(density(scale(reg_copper$residuals)), col="red", main="Copper Residuals", lwd=3)

# Standardize residuals
copper_transformed <- scale(reg_copper$residuals)

# Adjust the number of random values for comparison
num_values <- 1000000  
random_values <- rnorm(num_values)

# Plot the density distribution of standardized residuals and random values from a normal distribution
main_title <- "Copper: Transformed Distribution vs. Random Normal Distribution"
plot(density(copper_transformed), col="red", lwd=3, main = main_title)
lines(density(random_values), col="blue", lwd=3)


############################################################################################
######################## BOX-COX Transformation: Zinc ####################################
merged_df$zinc <- ifelse(merged_df$zinc == 0, 0.0001, merged_df$zinc)

# Calculate Box-Cox transformation for 'zinc' variable
x 				<- merged_df$zinc / merged_df$zinc_mean
#plot(hist(x))
lm(x ~ 1) ## this gives "linear model" expressing x as function of no other variables.
boxcox_results 	<- boxcox(lm(x ~ 1)) 								
lambda_zinc 	<- boxcox_results$x[which.max(boxcox_results$y)]

# Apply Box-Cox transformation and create a new variable 'new_x_exact'
merged_df$new_x_exact <- (x^lambda_zinc - 1) / lambda_zinc

# Fit a linear regression model for the transformed variable
reg_zinc <- lm(new_x_exact ~ sex + age1 + age2 + ethnicity + inc + region, data = merged_df)

# Display summary statistics of the regression model
summary(reg_zinc)

# Calculate the standard deviation of the residuals
sd_zinc <- sd(reg_zinc$residuals)

# Plot the density distribution
plot(density(merged_df$zinc), col="red", lwd=3, main="Zinc: Original Distribution")

# Plot the density distribution of transformed variable
plot(density(merged_df$new_x_exact), col="red", lwd=3, main="Zinc: Transformed Distribution")

# Plot the density distribution of standardized residuals for regression
plot(density(scale(reg_zinc$residuals)), col="red", main="Zinc Residuals", lwd=3)

# Standardize residuals
zinc_transformed <- scale(reg_zinc$residuals)

# Adjust the number of random values for comparison
num_values <- 1000000  
random_values <- rnorm(num_values)

# Plot the density distribution of standardized residuals and random values from a normal distribution
main_title <- "Zinc: Transformed Distribution vs. Random Normal Distribution"
plot(density(zinc_transformed), col="red", lwd=3, main = main_title)
lines(density(random_values), col="blue", lwd=3)



############################################################################################
######################## Box-Cox Transformation Parameters #######################################################

#########################################################################################
############################## Nutrient Regression Residuals & Correlations ################################

# Create a dataframe 'nutrients_residuals' containing residuals from regression models for nutrients
##### Use only common rows because they have different length
common_ids <- Reduce(intersect, list(
  names(reg_carb$residuals),
  names(reg_fat$residuals),
  names(reg_protein$residuals),
  names(reg_alcohol$residuals),
  names(reg_sodium$residuals),
  names(reg_fibre$residuals),
  names(reg_monounsats$residuals),
  names(reg_polyunsats$residuals),
  names(reg_saturates$residuals),
  names(reg_totalsugar$residuals),
  names(reg_addedsugar$residuals),
  names(reg_fruit$residuals),
  names(reg_vegetable$residuals),
  names(reg_legume$residuals),
  names(reg_red_meat$residuals),
  names(reg_proc_meat$residuals),
  names(reg_calcium$residuals),
  names(reg_iron$residuals),
  names(reg_vitaminc$residuals),
  names(reg_copper$residuals),
  names(reg_zinc$residuals)
))
nutrients_residuals <- data.frame(
  carb_residuals = reg_carb$residuals[common_ids],
  fat_residuals = reg_fat$residuals[common_ids],
  protein_residuals = reg_protein$residuals[common_ids],
  alcohol_residuals = reg_alcohol$residuals[common_ids],
  sodium_residuals = reg_sodium$residuals[common_ids],
  fibre_residuals = reg_fibre$residuals[common_ids],
  monounsats_residuals = reg_monounsats$residuals[common_ids],
  polyunsats_residuals = reg_polyunsats$residuals[common_ids],
  saturates_residuals = reg_saturates$residuals[common_ids],
  totalsugar_residuals = reg_totalsugar$residuals[common_ids],
  addedsugar_residuals = reg_addedsugar$residuals[common_ids],
  fruit_residuals = reg_fruit$residuals[common_ids],
  vegetable_residuals = reg_vegetable$residuals[common_ids],
  legume_residuals = reg_legume$residuals[common_ids],
  red_meat_residuals = reg_red_meat$residuals[common_ids],
  proc_meat_residuals = reg_proc_meat$residuals[common_ids],
  calcium_residuals = reg_calcium$residuals[common_ids],
  iron_residuals = reg_iron$residuals[common_ids],
  vitaminc_residuals = reg_vitaminc$residuals[common_ids],
  copper_residuals = reg_copper$residuals[common_ids],
  zinc_residuals = reg_zinc$residuals[common_ids]
)


# Calculate the correlation matrix for residuals
correlation_matrix <- cor(nutrients_residuals)

# Plot the correlation matrix
rownames(correlation_matrix) <- c('carb','fat','protein','alcohol','sodium','fibre',
                                  'monounsats','polyunsats','saturates','totalsugar',
                                  'addedsugar','fruit','vegetable','legume','red_meat','proc_meat',
                                  'calcium','iron','vitaminc','copper','zinc')
colnames(correlation_matrix) <- c('carb','fat','protein','alcohol','sodium','fibre',
                                  'monounsats','polyunsats','saturates','totalsugar',
                                  'addedsugar','fruit','vegetable','legume','red_meat','proc_meat',
                                  'calcium','iron','vitaminc','copper','zinc')
main_title <- "Residuals: Correlation Plot"
corrplot(correlation_matrix, type="upper", title=main_title)

#########################################################################################
############################## Logistic Regression & Residuals ################################

logistic_parameters <- data.frame(
  FoodAlcohol = logistic_alcohol$coefficients,
  FoodAddedSugar = logistic_addedsugar$coefficients,
  FoodFruit = logistic_fruit$coefficients,
  FoodVegetable = logistic_vegetable$coefficients,
  FoodLegume = logistic_legume$coefficients,
  FoodRedMeat = logistic_red_meat$coefficients,
  FoodProcMeat = logistic_proc_meat$coefficients
)



######################################################################################
####################################### Output Files ###################################

ConvertToHGPSname = function(OldName)
{
	HGPSName = OldName ## default to old name.
		 if (OldName ==  "carb"			) HGPSName = "Carbohydrate"
	else if (OldName ==  "fat"			) HGPSName = "Fat"
	else if (OldName ==  "protein"		) HGPSName = "Protein"
	else if (OldName ==  "alcohol"		) HGPSName = "Alcohol"
	else if (OldName ==  "sodium"		) HGPSName = "Sodium"
	else if (OldName ==  "fibre"		) HGPSName = "Fibre"
	else if (OldName ==  "monounsats"	) HGPSName = "MonounsaturatedFat"
	else if (OldName ==  "polyunsats"	) HGPSName = "PolyunsaturatedFattyAcid"
	else if (OldName ==  "saturates"	) HGPSName = "SaturatedFat"
	else if (OldName ==  "totalsugar"	) HGPSName = "TotalSugar"
	else if (OldName ==  "addedsugar"	) HGPSName = "AddedSugar"
	else if (OldName ==  "fruit"		) HGPSName = "Fruit"
	else if (OldName ==  "vegetable"	) HGPSName = "Vegetable"
	else if (OldName ==  "legume"		) HGPSName = "Legume"
	else if (OldName ==  "red_meat"		) HGPSName = "RedMeat"
	else if (OldName ==  "proc_meat"	) HGPSName = "ProcessedMeat"
	else if (OldName ==  "calcium"	) HGPSName = "Calcium"
	else if (OldName ==  "iron"	) HGPSName = "Iron"
	else if (OldName ==  "vitaminc"	) HGPSName = "VitaminC"
	else if (OldName ==  "copper"	) HGPSName = "Copper"
	else if (OldName ==  "zinc"	) HGPSName = "Zinc"
	return(HGPSName)
}

#setwd("C:/Users/jzhu5/OneDrive - Imperial College London/Health-GPS_SHARED/FINCH/Parametres in healthgps-examples_JZ/Outputs")
setwd(file.path(getwd(), "Outputs"))

# Save the coefficients from regression models for transformed variables ('carb', 'fat', 'protein', 'sodium') using Box-Cox transformation
#write.csv(reg_carb$coefficients, "boxcox_carb_coefficients.csv")
#write.csv(reg_fat$coefficients, "boxcox_fat_coefficients.csv")
#write.csv(reg_protein$coefficients, "boxcox_protein_coefficients.csv")
#write.csv(reg_sodium$coefficients, "boxcox_sodium_coefficients.csv")
#write.csv(reg_fibre$coefficients, "boxcox_fibre_coefficients.csv")
#write.csv(reg_monounsats$coefficients, "boxcox_monounsats_coefficients.csv")
#write.csv(reg_polyunsats$coefficients, "boxcox_polyunsats_coefficients.csv")
#write.csv(reg_saturates$coefficients, "boxcox_saturates_coefficients.csv")
#write.csv(reg_totalsugar$coefficients, "boxcox_totalsugar_coefficients.csv")
#write.csv(reg_alcohol$coefficients, "boxcox_alcohol_coefficients.csv")
#write.csv(reg_addedsugar$coefficients, "boxcox_addedsugar_coefficients.csv")
#write.csv(reg_fruit$coefficients, "boxcox_fruit_coefficients.csv")
#write.csv(reg_vegetable$coefficients, "boxcox_vegetable_coefficients.csv")
#write.csv(reg_legume$coefficients, "boxcox_legume_coefficients.csv")
#write.csv(reg_red_meat$coefficients, "boxcox_red_meat_coefficients.csv")
#write.csv(reg_proc_meat$coefficients, "boxcox_proc_meat_coefficients.csv")

BoxCoxCoeffs_Combined = data.frame(
		
		FoodCarbohydrate				= c(reg_carb$coefficients			,  stddev = sd_carb			,	lambda = lambda_carb		)	,   
		FoodFat	 						= c(reg_fat$coefficients			,  stddev = sd_fat			,   lambda = lambda_fat			)	, 
		FoodProtein	 					= c(reg_protein$coefficients		,  stddev = sd_protein		,   lambda = lambda_protein		)	, 
		FoodSodium	 					= c(reg_sodium$coefficients			,  stddev = sd_sodium		,	lambda = lambda_sodium		)	,  
		FoodAlcohol	 					= c(reg_alcohol$coefficients		,  stddev = sd_alcohol		,   lambda = lambda_alcohol		)	, 
		FoodFibre	 					= c(reg_fibre$coefficients			,  stddev = sd_fibre		,	lambda = lambda_fibre		)	,   
		FoodMonounsaturatedFat	 		= c(reg_monounsats$coefficients		,  stddev = sd_monounsats	,	lambda = lambda_monounsats	)	, 
		FoodPolyunsaturatedFattyAcid	= c(reg_polyunsats$coefficients		,  stddev = sd_polyunsats	,	lambda = lambda_polyunsats	)	, 
		FoodSaturatedFat	 			= c(reg_saturates$coefficients		,  stddev = sd_saturates	,	lambda = lambda_saturates	)	, 
		FoodTotalSugar	 				= c(reg_totalsugar$coefficients		,  stddev = sd_totalsugar	,	lambda = lambda_totalsugar	)	, 
		FoodAddedSugar	 				= c(reg_addedsugar$coefficients		,  stddev = sd_addedsugar	,	lambda = lambda_addedsugar	)	, 
		FoodFruit	 					= c(reg_fruit$coefficients			,  stddev = sd_fruit		,	lambda = lambda_fruit		)	,   
		FoodVegetable	 				= c(reg_vegetable$coefficients		,  stddev = sd_vegetable	,	lambda = lambda_vegetable	)	, 
		FoodLegume	 					= c(reg_legume$coefficients			,  stddev = sd_legume		,	lambda = lambda_legume		)	,  
		FoodRedMeat	 					= c(reg_red_meat$coefficients		,  stddev = sd_red_meat		,   lambda = lambda_red_meat	)	, 
		FoodProcessedMeat				= c(reg_proc_meat$coefficients		,  stddev = sd_proc_meat	,	lambda = lambda_proc_meat	)	, 
		FoodCalcium 					= c(reg_calcium$coefficients		,  stddev = sd_calcium		,   lambda = lambda_calcium		)	, 
		FoodIron 						= c(reg_iron$coefficients			,  stddev = sd_iron			,   lambda = lambda_iron		)	, 
		FoodVitaminC 					= c(reg_vitaminc$coefficients		,  stddev = sd_vitaminc		,   lambda = lambda_vitaminc	)	, 
		FoodCopper 						= c(reg_copper$coefficients			,  stddev = sd_copper		,	lambda = lambda_copper		)	, 
		FoodZinc 						= c(reg_zinc$coefficients			,  stddev = sd_zinc			,   lambda = lambda_zinc		)	)  

#
#BoxCoxCoeffs_Combined = data.frame(
#		
#		FoodCarbohydrate				= reg_carb$coefficients			,     
#		FoodFat	 						= reg_fat$coefficients			,      
#		FoodProtein	 					= reg_protein$coefficients		,   
#		FoodSodium	 					= reg_sodium$coefficients		,    
#		FoodAlcohol	 					= reg_alcohol$coefficients		,   
#		FoodFibre	 					= reg_fibre$coefficients		,     
#		FoodMonounsaturatedFat	 		= reg_monounsats$coefficients	, 
#		FoodPolyunsaturatedFattyAcid	= reg_polyunsats$coefficients	, 
#		FoodSaturatedFat	 			= reg_saturates$coefficients	,  
#		FoodTotalSugar	 				= reg_totalsugar$coefficients	, 
#		FoodAddedSugar	 				= reg_addedsugar$coefficients	, 
#		FoodFruit	 					= reg_fruit$coefficients		,     
#		FoodVegetable	 				= reg_vegetable$coefficients	,  
#		FoodLegume	 					= reg_legume$coefficients		,    
#		FoodRedMeat	 					= reg_red_meat$coefficients		,  
#		FoodProcessedMeat				= reg_proc_meat$coefficients,
#		FoodCalcium 					= reg_calcium$coefficients,
#		FoodIron 						= reg_iron$coefficients,
#		FoodVitaminC 					= reg_vitaminc$coefficients,
#		FoodCopper 						= reg_copper$coefficients,
#		FoodZinc 						= reg_zinc$coefficients
#)  
#
#
#
### combine lambda and sd with coefficients
#lambda = c(lambda_carb,lambda_fat,lambda_protein,lambda_alcohol,lambda_sodium, lambda_fibre, 
#		lambda_monounsats, lambda_polyunsats, lambda_saturates, lambda_totalsugar, 
#		lambda_addedsugar, lambda_fruit, lambda_vegetable,lambda_legume,
#		lambda_red_meat,lambda_proc_meat, lambda_calcium, lambda_iron,
#		lambda_vitaminc, lambda_copper, lambda_zinc)
#sd = c(sd_carb,sd_fat,sd_protein,sd_alcohol,sd_sodium, sd_fibre, sd_monounsats, sd_polyunsats,
#		sd_saturates, sd_totalsugar,sd_addedsugar,sd_fruit,sd_vegetable,sd_legume,
#		sd_red_meat,sd_proc_meat, sd_calcium, sd_iron, sd_vitaminc, sd_copper, sd_zinc)
#
#boxcoxparameters = data.frame(lambda,sd)
#rownames(boxcoxparameters) = c("carb","fat","protein","alcohol","sodium", "fibre","monounsats",
#		"polyunsats","saturates","totalsugar","addedsugar","fruit",
#		"vegetable","legume","red_meat","proc_meat", "calcium",
#		"iron","vitaminc","copper","zinc")





# fix rownames
rownames(BoxCoxCoeffs_Combined) = gsub("\\(Intercept\\)", "Intercept", rownames(BoxCoxCoeffs_Combined))
rownames(BoxCoxCoeffs_Combined) = gsub("inc", "income", rownames(BoxCoxCoeffs_Combined))
rownames(BoxCoxCoeffs_Combined) = gsub("sex2", "gender2", rownames(BoxCoxCoeffs_Combined))
write.csv(BoxCoxCoeffs_Combined, "boxcox_coefficients.csv")

# fix rownames of logistic coefficients
rownames(logistic_parameters) = gsub("\\(Intercept\\)", "Intercept", rownames(logistic_parameters))
rownames(logistic_parameters) = gsub("inc", "income", rownames(logistic_parameters))
rownames(logistic_parameters) = gsub("sex2", "gender2", rownames(logistic_parameters))
write.csv(logistic_parameters, "logistic_regression.csv")


# Save Box-Cox transformation parameters to a CSV file
rownames(boxcoxparameters) = paste0("Food", sapply(rownames(boxcoxparameters), ConvertToHGPSname))
write.csv(boxcoxparameters, "boxcox_parameters.csv")

# Save correlation matrix to a CSV file
rownames(correlation_matrix) = paste0("Food", sapply(rownames(correlation_matrix), ConvertToHGPSname))
colnames(correlation_matrix) = paste0("Food", sapply(colnames(correlation_matrix), ConvertToHGPSname))
write.csv(correlation_matrix, "Finch_residual_risk_factor_correlation.csv")

#####################################################################################
########################### Boundaries of nutrients ##################################

# Calculate min and max values for nutrients and policy effects
#min_nutrients = c(min(df$carb), min(df$fat), min(df$protein), min(df$alcohol), min(df$sodium), 
#                  min(df$fibre), min(df$polyunsats), min(df$monounsats), min(df$saturates), min(df$totalsugar), min(df$addedsugar),
#                  min(df$fruit), min(df$vegetable), min(df$legume), min(df$red_meat), min(df$proc_meat))
#max_nutrients = c(max(df$carb), max(df$fat), max(df$protein), max(df$alcohol), max(df$sodium),
#                  max(df$fibre), max(df$polyunsats), max(df$monounsats), max(df$saturates), max(df$totalsugar), max(df$addedsugar),
#                  max(df$fruit), max(df$vegetable), max(df$legume), max(df$red_meat), max(df$proc_meat))
#boundaries_nutrients = data.frame(min_nutrients, max_nutrients)
#rownames(boundaries_nutrients) = c("carb", "fat", "protein", "alcohol", "sodium", "fibre", "polyunsats",
#		"monounsats", "saturates", "totalsugar", "addedsugar", "fruit",
#		"vegetable", "legume", "red_meat", "proc_meat")

boundaries_nutrients = t(sapply(df[, c("carb", "fat", "protein", "sodium", "alcohol", "fibre", 
							  "monounsats", "polyunsats", "saturates", "totalsugar", "addedsugar", "fruit", "vegetable", 
							  "legume", "red_meat", "proc_meat", "calcium", "iron", "vitaminc", "copper","zinc")], range))  
rownames(boundaries_nutrients) = sapply(rownames(boundaries_nutrients), ConvertToHGPSname)
colnames(boundaries_nutrients) = c("min", "max")

# Create dataframes for boundaries of effects and nutrients
write.csv(boundaries_nutrients, "ranges_riskfactors.csv")


#####################################################################################
#####################################################################################


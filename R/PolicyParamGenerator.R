rm(list = ls())

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

### Working directory ###
#setwd("C:/Users/jzhu5/OneDrive - Imperial College London/Health-GPS_SHARED/FINCH/Parametres in healthgps-examples_JZ")
setwd("C:/Users/dlaydon/OneDrive - Imperial College London/Health-GPS_SHARED/FINCH/Parametres in healthgps-examples_JZ")
#setwd("/Users/jasmine/Library/CloudStorage/OneDrive-ImperialCollegeLondon/Health-GPS_SHARED/FINCH/Parametres in healthgps-examples_JZ")

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
	else if (OldName ==  "calcium"		) HGPSName = "Calcium"
	else if (OldName ==  "iron"			) HGPSName = "Iron"
	else if (OldName ==  "vitaminc"		) HGPSName = "VitaminC"
	else if (OldName ==  "copper"		) HGPSName = "Copper"
	else if (OldName ==  "zinc"			) HGPSName = "Zinc"
	return(HGPSName)
}

#######################################################################################################################
######################## Parameters ##########################################################

#File to use
#file_name <- "C:/Users/jzhu5/OneDrive - Imperial College London/Health-GPS_SHARED/FINCH/Parametres in healthgps-examples_JZ/HealthGPS_initial_population_2598hh.csv"
#file_name <- "/Users/jasmine/Library/CloudStorage/OneDrive-ImperialCollegeLondon/Health-GPS_SHARED/FINCH/Parametres in healthgps-examples_JZ/HealthGPS_initial_population_2598hh.csv"
file_name <- file.path(getwd(), "HealthGPS_initial_population_2598hh.csv")
#Filter threshold
#filter <- 0.005

# Load the CSV data into a data frame
data <- read.csv(file_name)
str(data)
colnames(data)

# Scenario identifier
scenario_number="2"

#######################################################################################################################
######################## Data Loading and Variable Extraction ##########################################################


# Attach the data frame for easier variable referencing
attach(data)

# Construct variable names based on the scenario
scenario = paste0("S",scenario_number)

cals_variable = paste0("cals_", scenario,"_percentchange")
carb_variable = paste0("carbs_", scenario,"_percentchange")
fat_variable = paste0("fats_", scenario,"_percentchange")
protein_variable = paste0("protein_", scenario,"_percentchange")
sodium_variable = paste0("sodium_", scenario,"_percentchange")
alcohol_variable = paste0("alcohol_", scenario,"_percentchange")
fibre_variable = paste0("fibre_", scenario,"_percentchange")
saturates_variable = paste0("saturates_", scenario,"_percentchange")
monounsats_variable = paste0("monounsats_", scenario,"_percentchange")
polyunsats_variable = paste0("polyunsats_", scenario,"_percentchange")
totalsugar_variable = paste0("totalsugars_", scenario,"_percentchange")
addedsugar_variable = paste0("addedsugar_", scenario,"_percentchange")
fruit_variable = paste0("fruit_", scenario,"_percentchange")
vegetable_variable = paste0("vegetable_", scenario,"_percentchange")
legume_variable = paste0("legume_", scenario,"_percentchange")
redmeat_variable = paste0("redmeat_", scenario,"_percentchange")
procmeat_variable = paste0("procmeat_", scenario,"_percentchange")
calcium_variable = paste0("calcium_", scenario,"_percentchange")
iron_variable = paste0("iron_", scenario,"_percentchange")
vitaminc_variable = paste0("vitaminc_", scenario,"_percentchange")
copper_variable = paste0("copper_", scenario,"_percentchange")
zinc_variable = paste0("zinc_", scenario,"_percentchange")


# Access the variables in your dataframe
cals_effect = 100 * data[[cals_variable]]
carb_effect = 100 * data[[carb_variable]]
fat_effect = 100 * data[[fat_variable]]
protein_effect = 100 * data[[protein_variable]]
sodium_effect = 100 * data[[sodium_variable]]
alcohol_effect = 100 * data[[alcohol_variable]]
fibre_effect = 100 * data[[fibre_variable]]
saturates_effect = 100 * data[[saturates_variable]]
monounsats_effect = 100 * data[[monounsats_variable]]
polyunsats_effect = 100 * data[[polyunsats_variable]]
totalsugar_effect = 100 * data[[totalsugar_variable]]
addedsugar_effect = 100 * data[[addedsugar_variable]]
fruit_effect = 100 * data[[fruit_variable]]
vegetable_effect = 100 * data[[vegetable_variable]]
legume_effect = 100 * data[[legume_variable]]
redmeat_effect = 100 * data[[redmeat_variable]]
procmeat_effect = 100 * data[[procmeat_variable]]
calcium_effect = 100 * data[[calcium_variable]]
iron_effect = 100 * data[[iron_variable]]
vitaminc_effect = 100 * data[[vitaminc_variable]]
copper_effect = 100 * data[[copper_variable]]
zinc_effect = 100 * data[[zinc_variable]]

# Missing values in alcohol_effect
alcohol_scenario = paste0("alcohol_", scenario)
alcohol_effect <- ifelse(data$alcohol_baseline==0 & data[[alcohol_scenario]] == 0, 0, alcohol_effect)

# Extracting variables from the 'data' data frame
sex <- as.factor(data$sex_person)
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

alcohol <- ifelse(is.na(alcohol), 0, alcohol)


# Calculate 'energy' based on the standard formula
energy <- 4 * carb + 9 * fat + 4 * protein + 7 * alcohol

############################################################################################
########################Filtering##########################################################

# Create 'subdata' dataframe
subdata_all <- data.frame(sex, age, age1, age2, age3, inc, ethnicity,region, protein, carb, fat, alcohol,
                          sodium, polyunsats,monounsats, saturates, totalsugar, addedsugar, fibre, proc_meat,
                          red_meat, fruit, vegetable, legume, calcium, iron, vitaminc, copper, zinc,
                      energy, carb_effect,fat_effect,protein_effect,sodium_effect, alcohol_effect,
                      polyunsats_effect,saturates_effect, monounsats_effect, totalsugar_effect,
                      addedsugar_effect, fibre_effect, fruit_effect, vegetable_effect, legume_effect,
                      redmeat_effect, procmeat_effect, calcium_effect, iron_effect, vitaminc_effect,
                      copper_effect, zinc_effect)
# Check missing values
subdata_all[subdata_all == Inf] <- NA

colSums(is.na(subdata_all))

subdata_all$carb_effect <- ifelse(is.na(subdata_all$carb_effect) & subdata_all$carb == 0, 0, subdata_all$carb_effect)
subdata_all$fat_effect <- ifelse(is.na(subdata_all$fat_effect) & subdata_all$fat == 0, 0, subdata_all$fat_effect)
subdata_all$protein_effect <- ifelse(is.na(subdata_all$protein_effect) & subdata_all$protein == 0, 0, subdata_all$protein_effect)
subdata_all$sodium_effect <- ifelse(is.na(subdata_all$sodium_effect) & subdata_all$sodium == 0, 0, subdata_all$sodium_effect)
subdata_all$alcohol_effect <- ifelse(is.na(subdata_all$alcohol_effect) & subdata_all$alcohol == 0, 0, subdata_all$alcohol_effect)
subdata_all$polyunsats_effect <- ifelse(is.na(subdata_all$polyunsats_effect) & subdata_all$polyunsats == 0, 0, subdata_all$polyunsats_effect)
subdata_all$saturates_effect <- ifelse(is.na(subdata_all$saturates_effect) & subdata_all$saturates == 0, 0, subdata_all$saturates_effect)
subdata_all$monounsats_effect <- ifelse(is.na(subdata_all$monounsats_effect) & subdata_all$monounsats == 0, 0, subdata_all$monounsats_effect)
subdata_all$totalsugar_effect <- ifelse(is.na(subdata_all$totalsugar_effect) & subdata_all$totalsugar == 0, 0, subdata_all$totalsugar_effect)
subdata_all$addedsugar_effect <- ifelse(is.na(subdata_all$addedsugar_effect) & subdata_all$addedsugar == 0, 0, subdata_all$addedsugar_effect)
subdata_all$fibre_effect <- ifelse(is.na(subdata_all$fibre_effect) & subdata_all$fibre == 0, 0, subdata_all$fibre_effect)
subdata_all$calcium_effect <- ifelse(is.na(subdata_all$calcium_effect) & subdata_all$calcium == 0, 0, subdata_all$calcium_effect)
subdata_all$iron_effect <- ifelse(is.na(subdata_all$iron_effect) & subdata_all$iron == 0, 0, subdata_all$iron_effect)
subdata_all$vitaminc_effect <- ifelse(is.na(subdata_all$vitaminc_effect) & subdata_all$vitaminc == 0, 0, subdata_all$vitaminc_effect)
subdata_all$copper_effect <- ifelse(is.na(subdata_all$copper_effect) & subdata_all$copper == 0, 0, subdata_all$copper_effect)
subdata_all$zinc_effect <- ifelse(is.na(subdata_all$zinc_effect) & subdata_all$zinc == 0, 0, subdata_all$zinc_effect)

# Check missing values
colSums(is.na(subdata_all))

## Keep complete cases
subdata <- subdata_all[complete.cases(subdata_all), ]
# 135 obs deleted in s1
# 140 in s2, 137 in s3, 155 in s4, 157 in s5
# 146 in s6, 154 in s7

df <- subdata

# Set upper and lower quantiles
#upper_q <-1-filter
#lower_q <- filter

# Filter 'subdata' based on conditions
#df <- subdata %>%
#filter(
#age <= 100,  # Age less than 100
#carb > quantile(carb, lower_q) & carb < quantile(carb, upper_q),  # Carb within quantiles
#fat > quantile(fat, lower_q) & fat < quantile(fat, upper_q),  # Fat within quantiles
#protein > quantile(protein, lower_q) & protein < quantile(protein, upper_q),  # Protein within quantiles
#sodium > quantile(sodium, lower_q) & sodium < quantile(sodium, upper_q),  # Sodium within quantiles
#energy > quantile(energy, lower_q) & energy < quantile(energy, upper_q),  # Energy within quantiles
#carb_effect > quantile(carb_effect, lower_q) & carb_effect < quantile(carb_effect, upper_q),  # Carb effect within quantiles
#fat_effect > quantile(fat_effect, lower_q) & fat_effect < quantile(fat_effect, upper_q),  # Fat effect within quantiles
#protein_effect > quantile(protein_effect, lower_q) & protein_effect < quantile(protein_effect, upper_q),  # Protein effect within quantiles
#sodium_effect > quantile(sodium_effect, lower_q) & sodium_effect < quantile(sodium_effect, upper_q)  # Sodium effect within quantiles
#)

############################################################################################
######################## Data Visualization and Analysis ##########################################################

# Plot the density of carb and carb_effect with different colors for comparison
plot(density(df$carb), col="blue", lwd=3, main = "Carb Density")
plot(density(df$carb_effect, na.rm = TRUE), col="red", lwd=3, main = "Carb Effect Density")

# Plot the density of fat and fat_effect with different colors for comparison
plot(density(df$fat), col="blue", lwd=3, main = "Fat Density")
plot(density(df$fat_effect, na.rm = TRUE), col="red", lwd=3, main = "Fat Effect Density")

# Plot the density of protein and protein_effect with different colors for comparison
plot(density(df$protein), col="blue", lwd=3, main = "Protein Density")
plot(density(df$protein_effect, na.rm = TRUE), col="red", lwd=3, main = "Protein Effect Density")

# Plot the density of sodium and sodium_effect with different colors for comparison
plot(density(df$sodium), col="blue", lwd=3, main = "Sodium Density")
plot(density(df$sodium_effect, na.rm = TRUE), col="red", lwd=3, main = "Sodium Effect Density")

# Plot the density of alcohol and alcohol_effect with different colors for comparison
plot(density(df$alcohol), col="blue", lwd=3, main = "Alcohol Density")
plot(density(df$alcohol_effect, na.rm = TRUE), col="red", lwd=3, main = "Alcohol Effect Density")

# Calculate the percentage of remaining data after filtering
#percentage = dim(df)[1] / dim(data)[1]
#percentage

# Create dataframes for nutrients and effects
nutrients = data.frame(df$carb, df$fat, df$protein, df$alcohol, df$sodium, df$fibre, df$saturates,
                       df$monounsats, df$polyunsats, df$totalsugar, df$addedsugar, df$fruit, df$vegetable,
                       df$legume, df$red_meat, df$proc_meat, df$calcium, df$iron,
                       df$vitaminc, df$copper, df$zinc)
# Error in data.frame(df$carb, df$fat, df$protein, df$alcohol, df$sodium,  :
#   arguments imply differing number of rows: 5999, 0

str(df)
effects = data.frame(df$carb_effect, df$fat_effect, df$protein_effect, df$alcohol_effect, df$sodium_effect,
                     df$fibre_effect, df$saturates_effect, df$monounsats_effect, df$polyunsats_effect,
                     df$totalsugar_effect, df$addedsugar_effect, df$fruit_effect, df$vegetable_effect,
                     df$legume_effect, df$redmeat_effect, df$procmeat_effect,
                     df$calcium_effect, df$iron_effect, df$vitaminc_effect, df$copper_effect,
                     df$zinc_effect)

# Display summary statistics for nutrients and effects
summary(nutrients)
summary(effects)

# Create the heatmap for nutrient correlations
nutrients_cor = cor(nutrients)
effects_cor = cor(effects)

cross_cor = cor(nutrients, effects)

# Set row and column names for better interpretation in the heatmap
rownames(nutrients_cor) <- c('carb', 'fat', 'protein', 'alcohol','sodium','fibre','saturates','monounsats',
                             'polyunsats','totalsugar','addedsugar', 'fruit','vegetable','legume',
                             'red_meat','proc_meat','calcium','iron','vitaminc','copper','zinc')
colnames(nutrients_cor) <- c('carb', 'fat', 'protein', 'alcohol','sodium','fibre','saturates','monounsats',
                             'polyunsats','totalsugar','addedsugar', 'fruit','vegetable','legume',
                             'red_meat','proc_meat','calcium','iron','vitaminc','copper','zinc')

rownames(effects_cor) <- c('carb_effect', 'fat_effect', 'protein_effect', 'alcohol_effect', 'sodium_effect','fibre_effect',
                           'saturates_effect', 'monounsats_effect', 'polyunsats_effect', 'totalsugar_effect', 'addedsugar_effect',
                           'fruit_effect','vegetable_effect','legume_effect','redmeat_effect','procmeat_effect',
                           'calcium_effect','iron_effect','vitaminc_effect','copper_effect','zinc_effect')
colnames(effects_cor) <- c('carb_effect', 'fat_effect', 'protein_effect', 'alcohol_effect', 'sodium_effect','fibre_effect',
                           'saturates_effect', 'monounsats_effect', 'polyunsats_effect', 'totalsugar_effect', 'addedsugar_effect',
                           'fruit_effect','vegetable_effect','legume_effect','redmeat_effect','procmeat_effect',
                           'calcium_effect','iron_effect','vitaminc_effect','copper_effect','zinc_effect')

# Plot nutrient correlation heatmap
main_title <- "Nutrients: Correlation Plot"
corrplot(nutrients_cor, type="upper", title=main_title)

#pdf("Outputs/correlation_plot.pdf", width = 8, height = 6)
#corrplot(nutrients_cor, type = "upper", title = main_title)
#dev.off()

# Plot nutrient effects correlation heatmap
main_title <- "Nutrient Effects: Correlation Plot"
corrplot(effects_cor, type="upper", title=main_title)

#pdf("Outputs/effect_correlation_plot.pdf", width = 8, height = 6)
#corrplot(effects_cor, type = "upper", title = main_title)
#dev.off()

# Plot cross correlation heatmap
#main_title <- "Cross: Correlation Plot"
#pdf("Outputs/cross_correlation_plot.pdf", width = 8, height = 6)
#corrplot(cross_cor, type="full", title=main_title)
#dev.off()


############################################################################################
######################## Policy Effect: Modelling ###################################

# Transforming energy with logarithm
log_energy = log(df$energy)

# Regression Models for Policy Effects
# Model for carb_effect
reg_carb = lm(carb_effect ~ sex + age1 + age2 + ethnicity + inc + region + log_energy, data = df)
summary(reg_carb)

# Model for fat_effect
reg_fat = lm(fat_effect ~ sex + age1 + age2 + ethnicity + inc + region + log_energy, data = df)
summary(reg_fat)

# Model for protein_effect
reg_protein = lm(protein_effect ~ sex + age1 + age2 + ethnicity + inc + region + log_energy, data = df)
summary(reg_protein)

# Model for alcohol_effect
reg_alcohol = lm(alcohol_effect ~ sex + age1 + age2 + ethnicity + inc + region + log_energy, data = df)
summary(reg_alcohol)

table(alcohol_effect)
sum(table(alcohol_effect))
table(na.omit(alcohol_effect))
alcohol_effect[alcohol_effect == Inf] = NA
mean(alcohol_effect, na.rm = T)

length(which(!is.na(alcohol_effect) & alcohol_effect != 0))

1/alcohol_effect
png(filename = paste0("alcohol_effect_hist_", scenario, ".png"), units = "in", width = 8, height = 5, res = 100)
hist(alcohol_effect, main = paste0("Alcohol Effect Histogram ", scenario))
dev.off()

# Model for sodium_effect
reg_sodium = lm(sodium_effect ~ sex + age1 + age2 + ethnicity + inc + region + log_energy, data = df)
summary(reg_sodium)

# Model for fibre_effect
reg_fibre = lm(fibre_effect ~ sex + age1 + age2 + ethnicity + inc + region + log_energy, data = df)
summary(reg_fibre)

# Model for saturates_effect
reg_saturates = lm(saturates_effect ~ sex + age1 + age2 + ethnicity + inc + region + log_energy, data = df)
summary(reg_saturates)

# Model for monounsats_effect
reg_monounsats = lm(monounsats_effect ~ sex + age1 + age2 + ethnicity + inc + region + log_energy, data = df)
summary(reg_monounsats)

# Model for polyunsats_effect
reg_polyunsats = lm(polyunsats_effect ~ sex + age1 + age2 + ethnicity + inc + region + log_energy, data = df)
summary(reg_polyunsats)

# Model for totalsugar_effect
reg_totalsugar = lm(totalsugar_effect ~ sex + age1 + age2 + ethnicity + inc + region + log_energy, data = df)
summary(reg_totalsugar)

# Model for addedsugar_effect
reg_addedsugar = lm(addedsugar_effect ~ sex + age1 + age2 + ethnicity + inc + region + log_energy, data = df)
summary(reg_addedsugar)

# Model for fruit_effect
reg_fruit = lm(fruit_effect ~ sex + age1 + age2 + ethnicity + inc + region + log_energy, data = df)
summary(reg_fruit)

# Model for vegetable_effect
reg_vegetable = lm(vegetable_effect ~ sex + age1 + age2 + ethnicity + inc + region + log_energy, data = df)
summary(reg_vegetable)
mean(vegetable_effect)

png(filename = paste0("vegetable_effect_hist_", scenario, ".png"), units = "in", width = 8, height = 5, res = 100)
#hist(vegetable_effect)
mean(vegetable_effect)
hist(vegetable_effect, main = paste0("Vegetable Effect Histogram ", scenario))
dev.off()


# Model for legume_effect
reg_legume = lm(legume_effect ~ sex + age1 + age2 + ethnicity + inc + region + log_energy, data = df)
summary(reg_legume)

# Model for redmeat_effect
reg_redmeat = lm(redmeat_effect ~ sex + age1 + age2 + ethnicity + inc + region + log_energy, data = df)
summary(reg_redmeat)

# Model for procmeat_effect
reg_procmeat = lm(procmeat_effect ~ sex + age1 + age2 + ethnicity + inc + region + log_energy, data = df)
summary(reg_procmeat)

# Model for calcium_effect
reg_calcium = lm(calcium_effect ~ sex + age1 + age2 + ethnicity + inc + region + log_energy, data = df)
summary(reg_calcium)

# Model for iron_effect
reg_iron = lm(iron_effect ~ sex + age1 + age2 + ethnicity + inc + region + log_energy, data = df)
summary(reg_iron)

# Model for vitaminc_effect
reg_vitaminc = lm(vitaminc_effect ~ sex + age1 + age2 + ethnicity + inc + region + log_energy, data = df)
summary(reg_vitaminc)

# Model for copper_effect
reg_copper = lm(copper_effect ~ sex + age1 + age2 + ethnicity + inc + region + log_energy, data = df)
summary(reg_copper)

# Model for zinc_effect
reg_zinc = lm(zinc_effect ~ sex + age1 + age2 + ethnicity + inc + region + log_energy, data = df)
summary(reg_zinc)


############################################################################################
# Residuals Analysis
residuals_matrix = data.frame(reg_carb$residuals, reg_fat$residuals, reg_protein$residuals,
                              reg_alcohol$residuals, reg_sodium$residuals, reg_fibre$residuals,
                              reg_saturates$residuals, reg_monounsats$residuals, reg_polyunsats$residuals,
                              reg_totalsugar$residuals, reg_addedsugar$residuals, reg_fruit$residuals,
                              reg_vegetable$residuals, reg_legume$residuals,reg_redmeat$residuals,
                              reg_procmeat$residuals, reg_calcium$residuals, reg_iron$residuals,
                              reg_vitaminc$residuals, reg_copper$residuals, reg_zinc$residuals)
names(residuals_matrix) = c("FoodCarbohydrate", "FoodFat", "FoodProtein", "FoodAlcohol", "FoodSodium", "FoodFibre",
                            "FoodSaturatedFat", "FoodMonounsaturatedFat", "FoodPolyunsaturatedFattyAcid", "FoodTotalsugar",
                            "FoodAddedsugar", "FoodFruit","FoodVegetable","FoodLegume","FoodRedMeat","FoodProcessedMeat",
                            "FoodCalcium","FoodIron","FoodVitaminC","FoodCopper","FoodZinc")

residuals_correlation = round(cor(residuals_matrix), 2)
residuals_covariance = round(cov(residuals_matrix), 2)

# Write model coefficients and residuals covariance to CSV files
setwd("C:/Users/jzhu5/OneDrive - Imperial College London/Health-GPS_SHARED/FINCH/Parametres in healthgps-examples_JZ/Outputs")
#setwd("C:/Users/dlaydon/OneDrive - Imperial College London/Health-GPS_SHARED/FINCH/Parametres in healthgps-examples_JZ/Outputs")
#setwd("/Users/jasmine/Library/CloudStorage/OneDrive-ImperialCollegeLondon/Health-GPS_SHARED/FINCH/Parametres in healthgps-examples_JZ/Outputs")

#OutputDir = file.path(getwd(), "Output")
#setwd(OutputDir)

# Construct variable names based on the scenario
scenario = paste("scenario",scenario_number,sep="")
#write.csv(reg_carb$coefficients			, paste0(scenario, "_carb_policyeffect_model.csv"))
#write.csv(reg_fat$coefficients			, paste0(scenario, "_fat_policyeffect_model.csv"))
#write.csv(reg_protein$coefficients		, paste0(scenario, "_protein_policyeffect_model.csv"))
#write.csv(reg_alcohol$coefficients		, paste0(scenario, "_alcohol_policyeffect_model.csv"))
#write.csv(reg_sodium$coefficients		, paste0(scenario, "_sodium_policyeffect_model.csv"))
#write.csv(reg_fibre$coefficients		, paste0(scenario, "_fibre_policyeffect_model.csv"))
#write.csv(reg_saturates$coefficients	, paste0(scenario, "_saturates_policyeffect_model.csv"))
#write.csv(reg_monounsats$coefficients	, paste0(scenario, "_monounsats_policyeffect_model.csv"))
#write.csv(reg_polyunsats$coefficients	, paste0(scenario, "_polyunsats_policyeffect_model.csv"))
#write.csv(reg_totalsugar$coefficients	, paste0(scenario, "_totalsugar_policyeffect_model.csv"))
#write.csv(reg_addedsugar$coefficients	, paste0(scenario, "_addedsugar_policyeffect_model.csv"))
#write.csv(reg_cals$coefficients			, paste0(scenario, "_cals_policyeffect_model.csv"))


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

CombinedNutrientCoefficients = cbind(

	FoodCarbohydrate             = reg_carb$coefficients		,
	FoodFat                      = reg_fat$coefficients			,
	FoodProtein                  = reg_protein$coefficients		,
	FoodAlcohol                  = reg_alcohol$coefficients		,
	FoodSodium                   = reg_sodium$coefficients		,
	FoodFibre                    = reg_fibre$coefficients		,
	FoodMonounsaturatedFat       = reg_monounsats$coefficients	,
	FoodPolyunsaturatedFattyAcid = reg_polyunsats$coefficients	,
	FoodSaturatedFat             = reg_saturates$coefficients	,
	FoodTotalSugar               = reg_totalsugar$coefficients	,
	FoodAddedSugar               = reg_addedsugar$coefficients	,
	FoodFruit	 				 = reg_fruit$coefficients		,
	FoodVegetable	 			 = reg_vegetable$coefficients		,
	FoodLegume	 				 = reg_legume$coefficients		,
	FoodRedMeat	 				 = reg_redmeat$coefficients		,
	FoodProcessedMeat			 = reg_procmeat$coefficients,
	FoodCalcium = reg_calcium$coefficients,
	FoodIron = reg_iron$coefficients,
	FoodVitaminC = reg_vitaminc$coefficients,
	FoodCopper = reg_copper$coefficients,
	FoodZinc = reg_zinc$coefficients
)
## add in rows for min and max
MinMaxDFrame = sapply(df[, c("carb_effect", "fat_effect", "protein_effect", "alcohol_effect", "sodium_effect", "fibre_effect",
						"monounsats_effect", "polyunsats_effect", "saturates_effect", "totalsugar_effect", "addedsugar_effect",
						"fruit_effect", "vegetable_effect", "legume_effect", "redmeat_effect", "procmeat_effect",
						"calcium_effect","iron_effect","vitaminc_effect", "copper_effect", "zinc_effect")], range)
rownames(MinMaxDFrame) = c("min", "max")
colnames(MinMaxDFrame) = paste0("Food", sapply(colnames(MinMaxDFrame), ConvertToHGPSname))
colnames(MinMaxDFrame) == colnames(CombinedNutrientCoefficients)
data.frame(colnames(MinMaxDFrame) , colnames(CombinedNutrientCoefficients))

# fix rownames
rownames(CombinedNutrientCoefficients) 	= gsub("\\(Intercept\\)", "Intercept"	, rownames(CombinedNutrientCoefficients) )
rownames(CombinedNutrientCoefficients)  = gsub("inc"			, "income"		, rownames(CombinedNutrientCoefficients) )
rownames(CombinedNutrientCoefficients)  = gsub("sex2"			, "gender2"		, rownames(CombinedNutrientCoefficients) )
rownames(CombinedNutrientCoefficients)  = gsub("log_energy"		, "EnergyIntake", rownames(CombinedNutrientCoefficients) )

CombinedNutrientCoefficients = rbind(CombinedNutrientCoefficients, MinMaxDFrame)

write.csv(CombinedNutrientCoefficients , paste0(scenario, "_food_policyeffect_model.csv"))

write.csv(residuals_covariance, paste(scenario, sep = "_", "Finch_residual_policy_covariance.csv"), row.names = FALSE)

# Calculate min and max values for policy effects

#min_policy_effect = c(min(df$carb_effect), min(df$fat_effect), min(df$protein_effect), min(df$alcohol_effect), min(df$sodium_effect),
                     # min(df$fibre_effect), min(df$saturates_effect), min(df$monounsats_effect), min(df$polyunsats_effect),
                      #min(df$totalsugar_effect), min(df$addedsugar_effect), min(df$cals_effect))
#max_policy_effect = c(max(df$carb_effect), max(df$fat_effect), max(df$protein_effect), max(df$alcohol_effect), max(df$sodium_effect),
                      #max(df$fibre_effect), max(df$saturates_effect), max(df$monounsats_effect), max(df$polyunsats_effect),
                      #max(df$totalsugar_effect), max(df$addedsugar_effect), max(df$cals_effect))

# Create dataframes for boundaries of effects and nutrients
#boundaries_effect = data.frame(min_policy_effect, max_policy_effect)
#rownames(boundaries_effect) = c("carb", "fat", "protein", "alcohol", "sodium", "fibre", "saturates", "monounsats", "polyunsats",
 #                               "totalsugar", "addedsugar", "cals")

#						t(boundaries_effect)
#write.csv(boundaries_effect, paste(scenario, sep = "_", "boundaries_effect.csv"))

#####################################################################################
#####################################################################################

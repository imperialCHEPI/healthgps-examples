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
#setwd("C:/Users/jzhu5/OneDrive - Imperial College London/Health-GPS_SHARED/FINCH/Parametres in healthgps-examples_JZ/")
setwd("/Users/jasmine/Library/CloudStorage/OneDrive-ImperialCollegeLondon/Health-GPS_SHARED/FINCH/Parametres in healthgps-examples_JZ")


#######################################################################################################################
######################## Parameters ##########################################################

#File to use
file_name <- "HealthGPS_initial_population_2598hh.csv"

# (e.g., 0.5 for half the rows)
country = "UK"
rate_to_print <- 0.2

#Filter threshold 
rate_to_filter <- 0 

#Smoothing parameters
smoothing = TRUE
repeated_smoothing = 150

#######################################################################################################################
######################## Data Loading and Variable Extraction ##########################################################

# Load the CSV data into a data frame
data <- read.csv(file_name)

# Attach the data frame for easier variable referencing
attach(data)

# Extracting variables from the 'data' data frame
sex <- as.factor(data$sex_person) ## 1-male 2-female
gender <- ifelse(sex == 1, 0, 1) ## 1-female 0-male
age <- data$age_person
age1 <- data$age_person
age2 <- data$age_person * data$age_person
age3 <- data$age_person * data$age_person * data$age_person
inc <- data$hhinc_pc # household income per equalised person
ethnicity <- ifelse(data$ethnicity_person == 3, 2, 
                    ifelse(data$ethnicity_person == 4, 3, 
                           ifelse(data$ethnicity_person %in% c(2, 5), 4, data$ethnicity_person)))
ethnicity <- as.factor(ethnicity)
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
########################Filtering##########################################################

# Create 'subdata' dataframe
subdata_all <- data.frame(gender, age, age1, age2, age3, inc, ethnicity, carb, fat, protein, alcohol,
                          energy, sodium, polyunsats, saturates, monounsats, totalsugar, 
                          addedsugar, fibre, proc_meat, red_meat, fruit, vegetable, legume,
                          calcium, iron, vitaminc, copper, zinc)

sum(is.na(subdata_all$alcohol))
sum(is.na(subdata_all$addedsugar))

subdata <- subdata_all

subdata$alcohol <- ifelse(is.na(subdata$alcohol), 0, subdata$alcohol)
subdata$addedsugar <- ifelse(is.na(subdata$addedsugar), 0, subdata$addedsugar)


subdata$energy <- 4 * subdata$carb + 9 * subdata$fat + 4 * subdata$protein + 7 * subdata$alcohol


df <- subdata

# Set upper and lower quantiles
#lower_q <- rate_to_filter 
#upper_q <-1-lower_q

# Filter 'subdata' based on conditions
#df <- subdata %>%
 # filter(
  #  age <= 110,  # Age less than 100
   # carb > quantile(carb, lower_q) & carb < quantile(carb, upper_q),  # Carb within quantiles
    #fat > quantile(fat, lower_q) & fat < quantile(fat, upper_q),  # Fat within quantiles
    #protein > quantile(protein, lower_q) & protein < quantile(protein, upper_q),  # Protein within quantiles
    #sodium > quantile(sodium, lower_q) & sodium < quantile(sodium, upper_q),  # Sodium within quantiles
    #energy > quantile(energy, lower_q) & energy < quantile(energy, upper_q)  # Energy within quantiles
  #)

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



############################################################################################
########################Calculate Means and Create CSV Files#######################################

# Calculate mean values for 'carb', 'fat', 'protein', 'sodium', and 'energy' based on 'age' and 'sex'
result <- df %>%
  group_by(age, gender) %>%
  summarize(
    carb_mean = mean(carb),
    fat_mean = mean(fat),
    protein_mean = mean(protein),
    sodium_mean = mean(sodium),
    energy_mean 	= mean(energy, na.rm = TRUE),
    monounsats_mean = mean(monounsats),
    polyunsats_mean = mean(polyunsats),
    saturates_mean = mean(saturates),
    totalsugar_mean = mean(totalsugar),
    addedsugar_mean = mean(addedsugar[addedsugar!=0]),
    #alcohol_mean = mean(alcohol, na.rm = TRUE), #ALCOHOL will be imported from NDNS
    fibre_mean = mean(fibre),
    fruit_mean = mean(fruit[fruit!=0]),
    vegetable_mean = mean(vegetable[vegetable!=0]),
    red_meat_mean = mean(red_meat[red_meat!=0]),
    proc_meat_mean = mean(proc_meat[proc_meat!=0]),
    legume_mean = mean(legume[legume!=0]),
    calcium_mean = mean(calcium),
    iron_mean = mean(iron),
    vitaminc_mean = mean(vitaminc),
    copper_mean = mean(copper),
    zinc_mean = mean(zinc)
  )

# Merge the calculated mean values back to the original dataframe ('df')
#result$sex = ifelse(result$sex=="1","Male","Female")

# Reorder the rows based on the 'sex' column
result <- result[order(result$gender), ]

# Remove the "_mean" suffix from column names
colnames(result) <- sub("_mean$", "", colnames(result))

# Change the "energy" to "energyintake"
colnames(result) <- sub("energy", "energyintake", colnames(result))

# Split the data frame based on 'sex'
male_data <- result[result$gender == 0, ]
female_data <- result[result$gender == 1, ]

# List of columns to smooth
columns_to_smooth <- c("carb", "fat", "protein", "sodium","energyintake",
                       "polyunsats", "saturates", "monounsats",
                       "totalsugar", "addedsugar", "fibre", "proc_meat", 
                       "red_meat", "fruit", "vegetable", "legume", "calcium",
                       "iron","vitaminc","copper","zinc")

# Apply the smoothing function to selected columns
male_data[, columns_to_smooth] <- lapply(male_data[, columns_to_smooth], get_smooth_factor, repeated_smoothing)
female_data[, columns_to_smooth] <- lapply(female_data[, columns_to_smooth], get_smooth_factor, repeated_smoothing)

# Variables to plot
variables_to_plot <- c("carb", "fat", "protein", "sodium", "energyintake",
                       "polyunsats", "saturates", "monounsats",
                       "totalsugar", "addedsugar", "fibre", "proc_meat", 
                       "red_meat", "fruit", "vegetable", "legume",
                       "calcium","iron","vitaminc","copper","zinc")

# Loop through each variable and create a separate plot
for (variable in variables_to_plot) {
  # Plotting male_data
  plot(male_data$age, male_data[[variable]], type="l", lwd=3, col="blue", main=paste("Average ", variable, " by sex and age"), xlab="Age", ylab=variable)
  
  # Plotting female_data
  lines(female_data$age, female_data[[variable]], col="red", lwd=3)
  
  # Adding legend
  legend("topright", legend=c("Male", "Female"), col=c("blue", "red"), lwd=3)
}


######################### Extrapolate nutrients for ages 81-110 #################

all_ages <- data.frame(age = 0:110)

male_data <- merge(all_ages, male_data, by = "age", all.x = TRUE)
male_data$gender[is.na(male_data$gender)] <- male_data$gender[1]

female_data <- merge(all_ages, female_data, by = "age", all.x = TRUE)
female_data$gender[is.na(female_data$gender)] <- female_data$gender[1]


extrapolation <- function(df, var) {
  missing_ages <- 81:110
  extrapolated_values <- spline(df$age[1:80], df[[var]][1:80], xout = missing_ages, method = "natural")$y
  df[[var]][missing_ages + 1] <- extrapolated_values
  return(df)
}

## Applying extrapolation
columns_to_extra <- c("carb", "fat", "protein", "sodium",
                       "polyunsats", "saturates", "monounsats",
                       "totalsugar", "addedsugar", "fibre", "proc_meat", 
                       "red_meat", "fruit", "vegetable", "legume",
                      "calcium","iron","vitaminc","copper","zinc")

for (var in columns_to_extra) {
  male_data <- extrapolation(male_data, var)
  female_data <- extrapolation(female_data, var)
}


########################################################################################
############################# Import Alcohol intake from NDNS ####################################

#file_name <- "C:/Users/jzhu5/OneDrive - Imperial College London/Health-GPS_SHARED/FINCH/Parametres in healthgps-examples_JZ/ndns_rp_yr9-11a_personleveldietarydata_uk_20210831.dta"
file_name <- "/Users/jasmine/Library/CloudStorage/OneDrive-ImperialCollegeLondon/Health-GPS_SHARED/FINCH/Parametres in healthgps-examples_JZ/ndns_rp_yr9-11a_personleveldietarydata_uk_20210831.dta"
  
data <- read_dta(file_name)

age <- data$AgeR
sex <- data$Sex
surveyyear <- data$SurveyYear
alcohol <- data$Alcoholg

subdata_all <- data.frame(age, sex, surveyyear, alcohol)
subdata <- subdata_all[subdata_all$surveyyear == 11,]
summary(subdata)

alcohol<- subdata |>
  group_by(age, sex) |>
  summarise(alcohol_mean = mean(alcohol[alcohol != 0], na.rm = TRUE), .groups = "drop")

## Separate female and male
alcohol_male <- alcohol[alcohol$sex==1, ]
alcohol_female <- alcohol[alcohol$sex==2,]

alcohol_male <- alcohol_male[, -2]
alcohol_female <- alcohol_female[, -2]


## Function to impute missing alcohol_mean values
alcohol_impute <- function(data) {
  for (i in which(is.na(data$alcohol_mean))) {
    age_i <- data$age[i]
    # Find nearest lower and higher non-NA values
    lower <- max(data$age[!is.na(data$alcohol_mean) & data$age < age_i], na.rm = TRUE)
    upper <- min(data$age[!is.na(data$alcohol_mean) & data$age > age_i], na.rm = TRUE)
    
    if (is.finite(lower) && is.finite(upper)) {
      val_lower <- data$alcohol_mean[data$age == lower]
      val_upper <- data$alcohol_mean[data$age == upper]
      data$alcohol_mean[i] <- mean(c(val_lower, val_upper), na.rm = TRUE)
    } else if (is.finite(lower)) {
      data$alcohol_mean[i] <- data$alcohol_mean[data$age == lower]
    } else if (is.finite(upper)) {
      data$alcohol_mean[i] <- data$alcohol_mean[data$age == upper]
    } else {
      data$alcohol_mean[i] <- 0 
    }
  }
  return(data[order(data$age), ])
}



#### MALE ####
## Generate a full data frame
alcohol_male_full <- data.frame(age = 0:110)
alcohol_male_full <- left_join(alcohol_male_full, alcohol_male, by = "age")
alcohol_male_full$alcohol_mean[alcohol_male_full$age<16] <- 0 ## Assume alcohol as 0 under age 16
alcohol_male_full$alcohol_mean[alcohol_male_full$age>85] <- alcohol_male_full$alcohol_mean[alcohol_male_full$age == 85]


alcohol_male_full <- alcohol_impute(alcohol_male_full)

age_mask <- alcohol_male_full$age >= 16 & alcohol_male_full$age <= 110

alcohol_male_full$alcohol_mean_smooth[age_mask] <- get_smooth_factor(alcohol_male_full$alcohol_mean[age_mask], 150)
alcohol_male_full$alcohol_mean_smooth <- ifelse(alcohol_male_full$age<16, alcohol_male_full$alcohol_mean, alcohol_male_full$alcohol_mean_smooth)

## Compare by plotting
ggplot(alcohol_male_full, aes(x = age)) +
  geom_line(aes(y = alcohol_mean, color = "Raw Mean"), size = 1) +
  geom_line(aes(y = alcohol_mean_smooth, color = "Smoothed Mean"), size = 1.5) +
  labs(
    title = "Alcohol Consumption by Age",
    x = "Age",
    y = "Alcohol Mean",
    color = "Legend"
  ) +
  theme_light()


#### FEMALE ####
## Generate a full data frame
alcohol_female_full <- data.frame(age = 0:110)
alcohol_female_full <- left_join(alcohol_female_full, alcohol_female, by = "age")
alcohol_female_full$alcohol_mean[alcohol_female_full$age<16] <- 0 ## Assume alcohol as 0 under age 16
alcohol_female_full$alcohol_mean[alcohol_female_full$age>85] <- alcohol_female_full$alcohol_mean[alcohol_female_full$age == 85]


alcohol_female_full <- alcohol_impute(alcohol_female_full)

age_mask <- alcohol_female_full$age >= 16 & alcohol_female_full$age <= 110

alcohol_female_full$alcohol_mean_smooth[age_mask] <- get_smooth_factor(alcohol_female_full$alcohol_mean[age_mask], 150)
alcohol_female_full$alcohol_mean_smooth <- ifelse(alcohol_female_full$age<16, alcohol_female_full$alcohol_mean, alcohol_female_full$alcohol_mean_smooth)

## Compare by plotting
ggplot(alcohol_female_full, aes(x = age)) +
  geom_line(aes(y = alcohol_mean, color = "Raw Mean"), size = 1) +
  geom_line(aes(y = alcohol_mean_smooth, color = "Smoothed Mean"), size = 1.5) +
  labs(
    title = "Alcohol Consumption by Age",
    x = "Age",
    y = "Alcohol Mean",
    color = "Legend"
  ) +
  theme_light()



## Add to factors mean files
male_data <- left_join(male_data, 
                       dplyr::select(alcohol_male_full, age, alcohol = alcohol_mean_smooth),
                       by = "age")

female_data <- left_join(female_data, 
                       dplyr::select(alcohol_female_full, age, alcohol = alcohol_mean_smooth),
                       by = "age")


###########################################################################################
## Update energy intake
male_data$energyintake <- 4*male_data$carb + 9*male_data$fat + 4*male_data$protein + 7*male_data$alcohol
female_data$energyintake <- 4*female_data$carb + 9*female_data$fat + 4*female_data$protein + 7*female_data$alcohol


###########################################################################################
########################## Import height and weight ########################################
#heightweight <- read.csv("factorsmean_weightheight.csv")

#heightweight_male <- heightweight[heightweight$gender==0, ]
#heightweight_female <- heightweight[heightweight$gender==1, ]

#heightweight_male <- heightweight_male[,-1]
#heightweight_female <- heightweight_female[,-1]

#male_data <- left_join(male_data, heightweight_male, by = "age")
#female_data <- left_join(female_data, heightweight_female, by = "age")


##############################################################################################
################## Import physical activity ####################################################
pal_mean <- read.csv("factorsmean_pal.csv")
pal_male <- pal_mean[pal_mean$female == 0,]
pal_female <- pal_mean[pal_mean$female == 1,]


pal_male <- pal_male[, -2]
pal_female <- pal_female[, -2]

pal_male_full <- data.frame(age = 0:110)
pal_female_full <- data.frame(age = 0:110)

pal_male_full <- left_join(pal_male_full, pal_male, by = "age")
pal_male_full <- pal_male_full[, -2]

pal_female_full <- left_join(pal_female_full, pal_female, by = "age")
pal_female_full <- pal_female_full[, -2]


pal_male_full <- pal_male_full %>%
  mutate(pal = case_when(
    age <= 15             ~ pal_mean_smooth[age == 16],
    age >= 93             ~ pal_mean_smooth[age == 92],
    TRUE                  ~ pal_mean_smooth
  ))

pal_female_full <- pal_female_full %>%
  mutate(pal = case_when(
    age <= 15             ~ pal_mean_smooth[age == 16],
    age >= 93             ~ pal_mean_smooth[age == 92],
    TRUE                  ~ pal_mean_smooth
  ))

impute_pal_mean <- function(data, col = "pal") {
  # Make sure it's ordered by age
  data <- data[order(data$age), ]
  
  # Loop through rows with NA in target column
  for (i in which(is.na(data[[col]]))) {
    age_i <- data$age[i]
    
    # Find nearest lower and upper non-NA ages
    lower_ages <- data$age[!is.na(data[[col]]) & data$age < age_i]
    upper_ages <- data$age[!is.na(data[[col]]) & data$age > age_i]
    
    lower <- if (length(lower_ages) > 0) max(lower_ages) else NA
    upper <- if (length(upper_ages) > 0) min(upper_ages) else NA
    
    # Impute based on available values
    if (!is.na(lower) && !is.na(upper)) {
      val_lower <- data[[col]][data$age == lower]
      val_upper <- data[[col]][data$age == upper]
      data[[col]][i] <- mean(c(val_lower, val_upper), na.rm = TRUE)
    } else if (!is.na(lower)) {
      data[[col]][i] <- data[[col]][data$age == lower]
    } else if (!is.na(upper)) {
      data[[col]][i] <- data[[col]][data$age == upper]
    } else {
      data[[col]][i] <- 0  # fallback default
    }
  }
  
  return(data)
}

pal_male_full <- impute_pal_mean(pal_male_full)
pal_female_full <- impute_pal_mean(pal_female_full)


pal_male_full$pal_mean <- get_smooth_factor(pal_male_full$pal, 150)
pal_female_full$pal_mean <- get_smooth_factor(pal_female_full$pal, 150)


## Compare by plotting
ggplot(pal_male_full, aes(x = age)) +
  geom_line(aes(y = pal, color = "Raw Mean"), size = 1) +
  geom_line(aes(y = pal_mean, color = "Smoothed Mean"), size = 1.5) +
  theme_light()


## Add to factors mean files
male_data <- left_join(male_data, 
                       dplyr::select(pal_male_full, age, physicalactivity = pal_mean),
                       by = "age")

female_data <- left_join(female_data, 
                         dplyr::select(pal_female_full, age, physicalactivity = pal_mean),
                         by = "age")


###################################################################################################
################################### Steady State adjustment #######################################

female_data <- female_data |>
  mutate(BMR = 9.99*weight + 6.25*height - 4.92*age - 161,
         steadystate = BMR*physicalactivity,
         ratio = steadystate/energyintake) ## Female

male_data <- male_data |>
  mutate(BMR = 9.99*weight + 6.25*height - 4.92*age + 5,
         steadystate = BMR*physicalactivity,
         ratio = steadystate/energyintake) ## Male


## Adjust risk factors with ratio
male_data <- male_data |>
  mutate(carb = carb*ratio,
         fat = fat*ratio,
         protein = protein*ratio,
         sodium = sodium*ratio,
         monounsats = monounsats*ratio,
         polyunsats = polyunsats*ratio,
         saturates = saturates*ratio,
         totalsugar = totalsugar*ratio,
         addedsugar = addedsugar*ratio,
         alcohol = alcohol*ratio,
         fibre = fibre*ratio,
         fruit = fruit*ratio,
         vegetable = vegetable*ratio,
         red_meat = red_meat*ratio,
         proc_meat = proc_meat*ratio,
         legume = legume*ratio,
         calcium = calcium*ratio,
         iron = iron*ratio,
         vitaminc = vitaminc*ratio,
         copper = copper*ratio,
         zinc = zinc*ratio
  )
female_data <- female_data |>
  mutate(carb = carb*ratio,
         fat = fat*ratio,
         protein = protein*ratio,
         sodium = sodium*ratio,
         monounsats = monounsats*ratio,
         polyunsats = polyunsats*ratio,
         saturates = saturates*ratio,
         totalsugar = totalsugar*ratio,
         addedsugar = addedsugar*ratio,
         alcohol = alcohol*ratio,
         fibre = fibre*ratio,
         fruit = fruit*ratio,
         vegetable = vegetable*ratio,
         red_meat = red_meat*ratio,
         proc_meat = proc_meat*ratio,
         legume = legume*ratio,
         calcium = calcium*ratio,
         iron = iron*ratio,
         vitaminc = vitaminc*ratio,
         copper = copper*ratio,
         zinc = zinc*ratio
         )



#####################################################################################################
#################################### Import income from ONS ###########################################

income <- read.csv("factorsmean_income.csv")

male_data <- left_join(male_data, income, by = "age")
female_data <- left_join(female_data, income, by = "age")


#####################################################################################################
################################### Export factors mean ###########################################

ConvertToHGPSname <- function(old_names) {
  name_map <- c(
    carb = "FoodCarbohydrate",
    fat = "FoodFat",
    protein = "FoodProtein",
    alcohol = "FoodAlcohol",
    sodium = "FoodSodium",
    fibre = "FoodFibre",
    monounsats = "FoodMonounsaturatedFat",
    polyunsats = "FoodPolyunsaturatedFattyAcid",
    saturates = "FoodSaturatedFat",
    totalsugar = "FoodTotalSugar",
    addedsugar = "FoodAddedSugar",
    fruit = "FoodFruit",
    vegetable = "FoodVegetable",
    legume = "FoodLegume",
    red_meat = "FoodRedMeat",
    proc_meat = "FoodProcessedMeat",
    energyintake = "oldenergy",
    age = "Age",
    gender = "Gender",
    height = "Height",
    weight = "Weight",
    physicalactivity = "PhysicalActivity",
    steadystate = "EnergyIntake",
    income = "Income",
    calcium = "FoodCalcium",
    iron = "FoodIron",
    vitaminc = "FoodVitaminC",
    copper = "FoodCopper",
    zinc = "FoodZinc"
  )
  
  return(ifelse(old_names %in% names(name_map), name_map[old_names], old_names))
}

colnames(male_data) <- ConvertToHGPSname(colnames(male_data))
colnames(female_data) <- ConvertToHGPSname(colnames(female_data))


write.csv(male_data, "Outputs/Finch.FactorsMean.Male.csv", row.names = FALSE)
write.csv(female_data, "Outputs/Finch.FactorsMean.Female.csv", row.names = FALSE)



### Packages ###
library(dplyr)
library(tidyr) 
library(ggplot2)
library(haven)


####################################################################################
####################### Ethnicity ##################################################

### Working directory ###
setwd("C:/Users/jzhu5/OneDrive - Imperial College London/Project/FINCH_UK/Microsimulation/Prepare inputs")

eth <- read.csv("2-Ethnicity/Total_UK.csv")
eth_ew <- eth[eth$country == "England and Wales",]
eth_ni <- eth[eth$country == "Northern Ireland",]
eth_sc <- eth[eth$country == "Scotland",]

eth_ew <- eth_ew |>
  mutate(ethnic_common = case_when(
    ethnic_group == 4 ~ 1,
    ethnic_group == 1 ~ 2,
    ethnic_group == 2 ~ 3,
    ethnic_group == 3 ~ 4,
    ethnic_group == 5 ~ 4,
    ethnic_group == -8 ~ 4),
    adult = case_when(
      age < 18 ~ 0,
      age >=18 ~ 1))

eth_ew_summary <- eth_ew |>
  group_by(adult, sex, ethnic_common) |>
  summarise(count_sum_ew = sum(count))

eth_ni <- eth_ni |>
  mutate(ethnic_common = case_when(
    ethnic_group >4 ~ 4,
    TRUE ~ ethnic_group),
    sex = case_when(
      sex == "Male" ~ 1,
      sex == "Female" ~ 2),
    adult = case_when(
      age < 18 ~ 0,
      age >=18 ~ 1))

eth_ni_summary <- eth_ni |>
  group_by(adult, sex, ethnic_common) |>
  summarise(count_sum_ni = sum(count))


eth_sc <- eth_sc |>
  mutate(ethnic_common = case_when(
    ethnic_group == 2 ~ 1,
    ethnic_group == 12 ~ 2,
    ethnic_group %in% c(18,21) ~ 3,
    ethnic_group %in% c(11,25) ~ 4),
    sex = case_when(
      sex == "Male" ~ 1,
      sex == "Female" ~ 2),
    adult = case_when(
      age_group %in% c("0 - 4", "10 - 14", "15", "16 - 17") ~ 0,
      TRUE ~ 1))

eth_sc <- eth_sc[!is.na(eth_sc$ethnic_common),]
eth_sc_summary <- eth_sc |>
  group_by(adult, sex, ethnic_common) |>
  summarise(count_sum_sc = sum(count))

eth_ew_summary$sex <- as.numeric(eth_ew_summary$sex)
eth_summary <- left_join(eth_ew_summary, eth_ni_summary, by = c("adult", "sex", "ethnic_common"))
eth_summary <- left_join(eth_summary, eth_sc_summary, by = c("adult", "sex", "ethnic_common"))

eth_summary <- eth_summary |>
  mutate(count_uk = count_sum_ew + count_sum_ni + count_sum_sc)

eth_summary <- eth_summary |>
  group_by(adult, sex) |>
  mutate(count_denominator = sum(count_uk))

eth_summary <- eth_summary |>
  mutate(count_uk_percent = count_uk/count_denominator)

write.csv(eth_summary, "2-Ethnicity/HGPS_eth_prevalence.csv")


################################################################################
### Policy ### Test ###
lcfs_ind <- read_dta("Mario/Health-GPS data_modified.dta")
lcfs_hh <- read.csv("UNIBO/HealthGPS_reference.csv")

### Calculate policy impact - percentage changes for each household ###
# 1-Identify unique variable names
all_vars <- colnames(lcfs_hh)
base_vars <- grep("_baseline$", all_vars, value = TRUE)
var_names <- sub("_baseline$", "", base_vars)  # Extract base variable names
# 2-Loop through variable names and calculate change
for (var in var_names) {
  base_col <- paste0(var, "_baseline")
  s1_col <- paste0(var, "_S1")
  
  if (base_col %in% all_vars & s1_col %in% all_vars) {
    change_col <- paste0(var, "_S1_percentchange")
    lcfs_hh[[change_col]] <- (lcfs_hh[[s1_col]] - lcfs_hh[[base_col]])/lcfs_hh[[base_col]]
  }
} ## S1
for (var in var_names) {
  base_col <- paste0(var, "_baseline")
  s2_col <- paste0(var, "_S2")
  
  if (base_col %in% all_vars & s2_col %in% all_vars) {
    change_col <- paste0(var, "_S2_percentchange")
    lcfs_hh[[change_col]] <- (lcfs_hh[[s2_col]] - lcfs_hh[[base_col]])/lcfs_hh[[base_col]]
  }
} ## S2

lcfs_hh_sub <- lcfs_hh[, grep("case|_percentchange", names(lcfs_hh))]

lcfs_ind_more <- left_join(lcfs_ind,lcfs_hh_sub, by = "case")

lcfs_ind_more <- lcfs_ind_more |>
  mutate(age_person_sq = age_person*age_person,
         age_person_cb = age_person*age_person*age_person,
         sex_person = as.factor(sex_person),
         ethnicity_person = as.factor(ethnicity_person),
         log_calories_w = log(calories_w),
         log_fats_w = log(fats_w),
         log_carbs_w = log(carbs_w),
         log_protein_w = log(protein_w),
         log_fibre_w = log(fibre_w))

lcfs_ind_more[lcfs_ind_more == -Inf] <- NA
cals <- lm(data = lcfs_ind_more, 
           fats_S1_percentchange ~ age_person + age_person_sq + age_person_cb + sex_person + 
             hhsize + ethnicity_person + log_fats_w + log_carbs_w + log_protein_w + log_fibre_w)
summary(cals)

################################################################################
### Comparing matched dataset and demand model simulated ###
matched <- read_dta("Health-GPS data_modified.dta")
demand <- read.csv("UNIBO/HealthGPS_reference.csv")

matched_hh <- matched |>
  group_by(case) |>
  summarise(HH_energy_intake = mean(HH_energy_intake), # HH EI using DEFRA nutrients
            hhEnergykcal = mean(hhEnergykcal)) # HH EI using matched NDNS nutrients

demand_cal <- demand[demand$wave=="Master", c("case","Year","hhsize_OECD","cals_baseline")]
demand_cal$hhcals_baseline <- demand_cal$hhsize_OECD*demand_cal$cals_baseline
both <- left_join(matched_hh,demand_cal, by = "case")
ggplot(both, aes(HH_energy_intake, hhcals_baseline)) +
  geom_point() +
  geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed", size = 1)


demand_cal1819 <- demand[demand$Year<2020, c("case","Year","cals_baseline")]
both1819 <- left_join(matched_hh,demand_cal1819, by = "case")
ggplot(both1819, aes(HH_energy_intake, cals_baseline)) +
  geom_point() +
  geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed", size = 1)


################################################################################
### Merge to get initial population ###
matched <- read_dta("Mario/Health-GPS data_modified.dta")
demand <- read.csv("UNIBO/HealthGPS_reference.csv")

demand_master <- demand[demand$wave=="Master",] # Keep hh from Master wave only
### Calculate policy impact - percentage changes for each household ###
# 1-Identify unique variable names
all_vars <- colnames(demand_master)
base_vars <- grep("_baseline$", all_vars, value = TRUE)
var_names <- sub("_baseline$", "", base_vars)  # Extract base variable names
# 2-Loop through variable names and calculate change
for (var in var_names) {
  base_col <- paste0(var, "_baseline")
  s1_col <- paste0(var, "_S1")
  
  if (base_col %in% all_vars & s1_col %in% all_vars) {
    change_col <- paste0(var, "_S1_percentchange")
    demand_master[[change_col]] <- (demand_master[[s1_col]] - demand_master[[base_col]])/demand_master[[base_col]]
  }
} ## Under S1
for (var in var_names) {
  base_col <- paste0(var, "_baseline")
  s2_col <- paste0(var, "_S2")
  
  if (base_col %in% all_vars & s2_col %in% all_vars) {
    change_col <- paste0(var, "_S2_percentchange")
    demand_master[[change_col]] <- (demand_master[[s2_col]] - demand_master[[base_col]])/demand_master[[base_col]]
  }
} ## Under S2
for (var in var_names) {
  base_col <- paste0(var, "_baseline")
  s3_col <- paste0(var, "_S3")
  
  if (base_col %in% all_vars & s3_col %in% all_vars) {
    change_col <- paste0(var, "_S3_percentchange")
    demand_master[[change_col]] <- (demand_master[[s3_col]] - demand_master[[base_col]])/demand_master[[base_col]]
  }
} ## Under S3
for (var in var_names) {
  base_col <- paste0(var, "_baseline")
  s4_col <- paste0(var, "_S4")
  
  if (base_col %in% all_vars & s4_col %in% all_vars) {
    change_col <- paste0(var, "_S4_percentchange")
    demand_master[[change_col]] <- (demand_master[[s4_col]] - demand_master[[base_col]])/demand_master[[base_col]]
  }
} ## Under S4
for (var in var_names) {
  base_col <- paste0(var, "_baseline")
  s5_col <- paste0(var, "_S5")
  
  if (base_col %in% all_vars & s5_col %in% all_vars) {
    change_col <- paste0(var, "_S5_percentchange")
    demand_master[[change_col]] <- (demand_master[[s5_col]] - demand_master[[base_col]])/demand_master[[base_col]]
  }
} ## Under S5

both <- left_join(matched, demand_master, by="case")

################################################################################
####### Add food consumptions ##################################################

setwd("C:/Users/jzhu5/OneDrive - Imperial College London/Project/FINCH_UK/Matched_LCF_NDNS")
efsdiary <- read.csv("LCFS/EFSdiary.csv")  ## Quantity and expenditure for each hh and product

case <- efsdiary$case
maffcode <- efsdiary$MAFFCODE
quantity <- efsdiary$quantity

df <- data.frame(case, maffcode, quantity)

# Create dummy variables based on maffcode conditions
df$dum_fruit <- (df$maffcode >= 21001 & df$maffcode <= 24502)

df$dum_vegetable <- ((df$maffcode >= 15501 & df$maffcode <= 18401 & 
                  df$maffcode != 16801 & df$maffcode != 16901) | 
                 df$maffcode %in% c(19101, 19501, 19602, 19801, 
                                    19901, 20101, 20801))

df$dum_legume <- df$maffcode %in% c(16801, 16901, 18501, 18803, 
                                     19201, 20301, 20401)

#df$dum_Bread <- ((df$maffcode >= 25102 & df$maffcode <= 26309 & 
#                  df$maffcode != 26303) | df$maffcode == 27101)

#df$dum_Cereals <- df$maffcode %in% c(26401, 28702, 28703, 29502, 
#                                    29919, 30101)

df$dum_red_meat <- (df$maffcode >= 3102 & df$maffcode <= 4605)

#df$dum_Meat_fresh <- (df$maffcode %in% c(4607, 5101) | 
#                        (df$maffcode >= 7401 & df$maffcode <= 7801))

df$dum_proc_meat <- ((df$maffcode >= 5502 & df$maffcode <= 7102 & 
                        df$maffcode != 5904) | 
                       (df$maffcode >= 7901 & df$maffcode <= 9403 & 
                          df$maffcode != 8901))

#df$dum_Fish <- ((df$maffcode >= 10201 & df$maffcode <= 11703) | 
#                  df$maffcode == 11901 | df$maffcode == 12001)

#df$dum_Milk <- ((df$maffcode >= 402 & df$maffcode <= 901) | 
#                  df$maffcode %in% c(1201, 1502, 1503) | 
#                  (df$maffcode >= 1605 & df$maffcode <= 1608))

#df$dum_Yoghurt <- df$maffcode %in% c(1301, 1302)

#df$dum_DairyCheeseEggs <- ((df$maffcode >= 1701 & df$maffcode <= 2301) | 
#                             df$maffcode == 12901)

#df$dum_BreakCer_L <- df$maffcode %in% c(28101, 28202, 28203, 28205)

#df$dum_BreakCer_H <- (df$maffcode == 28204)

#df$dum_SavSnack <- df$maffcode %in% c(20002, 27403, 29909)

#df$dum_OilFats <- (df$maffcode >= 13501 & df$maffcode <= 14807)

#df$dum_Confect_L <- df$maffcode %in% c(26303, 28502, 28503, 33303)

#df$dum_Confect_H <- (df$maffcode %in% c(1603, 26701, 27001, 27402, 
#                                        27702, 28601, 29402, 29907) | 
#                       (df$maffcode >= 32901 & df$maffcode <= 33302) | 
#                       (df$maffcode >= 35001 & df$maffcode <= 35401))

#df$dum_ReadyM_L <- df$maffcode %in% c(8901, 12103, 18802, 29501, 
#                                      29601, 31801, 19702)

#df$dum_ReadyM_H <- df$maffcode %in% c(20601, 31901)

#df$dum_NonAlcohol_excl <- df$maffcode %in% c(31401, 24801, 19603)

#df$dum_NonAlcohol_L <- df$maffcode %in% c(34401, 34301)

#df$dum_NonAlcohol_H <- df$maffcode %in% c(31201, 31301, 34001, 34101)

#df$dum_Alcohol_L <- ((df$maffcode >= 38102 & df$maffcode <= 38302) | 
#                       df$maffcode == 38403)

#df$dum_Alcohol_H <- ((df$maffcode >= 38501 & df$maffcode <= 38901) | 
#                       df$maffcode == 38402)

#df$dum_Miscell <- (df$maffcode %in% c(1102, 1103, 29001, 29101, 
#                                      29915, 31501, 32801, 24503) | 
#                     (df$maffcode >= 30401 & df$maffcode <= 30901) | 
#                     (df$maffcode >= 32302 & df$maffcode <= 32703) | 
#                     (df$maffcode >= 33401 & df$maffcode <= 33901) | 
#                     (df$maffcode >= 15001 & df$maffcode <= 15401))

#df$dum_Takeaway_L <- df$maffcode %in% c(5904, 9505, 12305, 19703, 
#                                        26311, 28704, 29503, 29602, 
#                                        31001, 32001, 32201)

#df$dum_Takeaway_H <- ((df$maffcode >= 9501 & df$maffcode <= 9504) | 
#                        df$maffcode %in% c(9506, 11801, 12304, 20604, 
#                                           26310, 27002, 29916, 32101, 
#                                           32704, 33304, 35501))

# Aggregate expenditure for EO
#df$dum_EO_exp <- (df$maffcode >= 500101 & df$maffcode <= 500902)


# Sum quantity for each category per individual
df_summary <- df %>%
  group_by(case) %>%
  summarise(
    fruit_hh_defra = sum(quantity * dum_fruit, na.rm = TRUE),
    vegetable_hh_defra = sum(quantity * dum_vegetable, na.rm = TRUE),
    legume_hh_defra = sum(quantity * dum_legume, na.rm = TRUE),
    red_meat_hh_defra = sum(quantity * dum_red_meat, na.rm = TRUE),
    proc_meat_hh_defra = sum(quantity * dum_proc_meat, na.rm = TRUE)
  )

population <- read.csv("HealthGPS_initial_population.csv")
population_update <- left_join(population, df_summary, by="case")

write.csv(population_update, "HealthGPS_initial_population.csv")





nutrient ~ age + age2 + age3 + sex + ethnicity + income + log_fat + log_carbs + 
  log_protein + log_alcohol + log_nutrient

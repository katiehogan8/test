# libraries
library(readxl)
library(writexl)
library(dplyr)
library(tidyverse)
library(tidyr)
library(ggplot2)
library(epiDisplay)
library(haven)
library(readr)
library(gtsummary)
library(gt)
library(weights)
library(survey)

#### IMPORT DATA ####
dynata_q1 <- read.csv("/Users/kathrynhogan/Library/CloudStorage/OneDrive-JohnsHopkins/2. Hopkins Year 1 Research/Thesis/accidda_aim/accidda_contact_mobility/dynata_12092024.csv", header = TRUE)
dynata_q2 <- read_xlsx("/Users/kathrynhogan/Library/CloudStorage/OneDrive-JohnsHopkins/2. Hopkins Year 1 Research/Thesis/accidda_aim/Merged Wave 2/Input Data/dynata_w2.xlsx")

# yougov_q1 <- read.csv("/Users/kathrynhogan/Library/CloudStorage/OneDrive-JohnsHopkins/2. Hopkins Year 1 Research/Thesis/accidda_aim/accidda_contact_mobility/yougov_dec2024.csv", header = TRUE) # wave 2 data includes wave 1 data
yougov <- read.csv("/Users/kathrynhogan/Library/CloudStorage/OneDrive-JohnsHopkins/2. Hopkins Year 1 Research/Thesis/accidda_aim/Merged Wave 2/Input Data/yougov_w2.csv")

#### RENAME DYNATA COL USING DATA DICTIONARY ####
# dynata data
relabel_names_dynata <- read.csv("/Users/kathrynhogan/Library/CloudStorage/OneDrive-JohnsHopkins/2. Hopkins Year 1 Research/Thesis/accidda_aim/accidda_contact_mobility/Dynata Data Dictionary_apw_cl.csv", header = TRUE)

orig_col_names <- colnames(dynata_q1)
replace_col_names <- function(orig_col_names, matching_file){
  old_names = matching_file[,1] ## first column is the old name
  replace_names = matching_file[,2] ## second column is the new name 
  n = length(orig_col_names)
  update_col_names <- rep(NA, n)
  for(ii in 1:n){
    x = orig_col_names[ii]
    if(is.element(x, old_names) == TRUE){
      update_name = replace_names[which(x == old_names)]
      if(update_name == ''){
        update_col_names[ii] = x
      }
      else{
        update_col_names[ii] = replace_names[which(x == old_names)]
      }
    }
    else{
      update_col_names[ii] = x
    }
  }
  return(update_col_names)
}
new_col_names <- replace_col_names(orig_col_names, relabel_names_dynata)

# replace colnames with new names for dynata
colnames(dynata_q1) <- new_col_names
colnames(dynata_q1) 

relabeled_status <- data.frame(
  original_name = orig_col_names,
  relabeled_to = new_col_names,
  was_renamed = orig_col_names != new_col_names
)

# view only columns that were not renamed
relabeled_status[relabeled_status$was_renamed == FALSE, ]

#### RENAME DYNATA Q2 COL USING DATA DICTIONARY #### 
orig_col_names <- colnames(dynata_q2)
new_col_names <- replace_col_names(orig_col_names, relabel_names_dynata)

# replace colnames with new names for dynata
colnames(dynata_q2) <- new_col_names
colnames(dynata_q2) 

relabeled_status <- data.frame(
  original_name = orig_col_names,
  relabeled_to = new_col_names,
  was_renamed = orig_col_names != new_col_names
)

# view only columns that were not renamed
relabeled_status[relabeled_status$was_renamed == FALSE, ]

#### MERGE DYNATA Q1 AND Q2 #### 
### add a column to identify as yougov or dynata data
dynata_q1$source <- "dynata"
dynata_q2$source <- "dynata"
yougov$source <- "yougov"
dynata_q1$wave <- "1"
dynata_q2$wave <- "2" # yougov already has wave variable

### look for column alignment between the two
intersect(names(dynata_q1), names(dynata_q2))  # shared columns
setdiff(names(dynata_q1), names(dynata_q2))    # only in dynata
setdiff(names(dynata_q2), names(dynata_q1))    # only in yougov

# make sure the variables that overlap are the same class before merge
common_cols <- intersect(names(dynata_q1), names(dynata_q2))
for (col in common_cols) {
  dynata_q1[[col]] <- as.character(dynata_q1[[col]])
  dynata_q2[[col]] <- as.character(dynata_q2[[col]])
}

dynata <- bind_rows(dynata_q1, dynata_q2)

#### RENAME YOUGOV COLUMNS #### 
# yougov data
relabel_names_yougov <- read.csv("/Users/kathrynhogan/Library/CloudStorage/OneDrive-JohnsHopkins/2. Hopkins Year 1 Research/Thesis/accidda_aim/accidda_contact_mobility/YouGov Data Dictionary.csv", header = TRUE)
orig_col_names <- colnames(yougov)
new_col_names <- replace_col_names(orig_col_names, relabel_names_yougov)

### replace colnames with new names for yougov
colnames(yougov) <- new_col_names

# view only columns that were not renamed
relabeled_status[relabeled_status$was_renamed == FALSE, ]

#### REMOVE COLUMNS THAT WON'T BE USED #### 
# remove columns from dynata that are empty or won't be used
# Remove columns by name and pattern
dynata <- dynata %>%
  select(
    -matches("^hQualityScoreAnalyzer\\d+$"),  # regex pattern for hQualityScoreAnalyzer1-99
    -county_freetext, # too many people answered this as "country" and put USA
    -rid,
    -date,
    -STATE,
    -q1a_start,
    -hBot_start,
    -region4,
    -region9,
    -q1a_mid,
    -hBot_mid,
    -q1a_end,
    -hBot_end,
    -dGender,
    -dState,
    -consent_participate,
    -status,
    -markers,
    -psid,
    -LOI,
    -DMA,
    -starts_with("dmaData") # clean out the dmaData columns from Dynata
  )

# remove yougov columns that won't be used
yougov <- yougov %>%
  select(
    -weight,
    -child18,
    -genhealth,
    -marstat,
    -presvote24post,
    -presvote20post,
    -inputstate,
    -votereg,
    -ideo5,
    -newsint,
    -religpew,
    -pew_churatd,
    -pew_bornagain,
    -pew_religimp,
    -pew_prayer,
    -age9) %>%
  mutate(age_group = case_when(
    age >= 18 & age < 35 ~ "1",
    age > 34 & age < 55 ~ "2",
    age > 54 & age < 65 ~ "3",
    age > 64 ~ "4"))

# rename zip code in yougov to match dynata
yougov <- rename(yougov, "zipcode" = "inputzip")

#### MERGE YOUGOV AND DYNATA ####
### look for column alignment between the two
intersect(names(dynata), names(yougov))  # shared columns
setdiff(names(dynata), names(yougov))    # only in dynata
setdiff(names(yougov), names(dynata))    # only in yougov

# make sure the variables that overlap are the same class before merge
common_cols <- intersect(names(dynata), names(yougov))
for (col in common_cols) {
  dynata[[col]] <- as.character(dynata[[col]])
  yougov[[col]] <- as.character(yougov[[col]])
}

combined_df <- bind_rows(dynata, yougov)

# rename distinct yougov or dynata variables
combined_df <- combined_df %>%
  rename(
    caseid_yg = caseid,
    record_dyn = record,
    uuid_dyn = uuid
  )

# make sure state is assigned as md for yougov
table(combined_df$state_residence, useNA = "ifany")
combined_df$state_residence <- ifelse(
  is.na(combined_df$state_residence), "MD",
  combined_df$state_residence
)

# write.csv(combined_df, "wave1_2.csv")

#### IMPORT DATA ####
# combined_df <- read.csv("wave1_2.csv", header = TRUE)

#### CLEAN DATA SO BOTH SOURCES MATCH ####
###### MODULE 1 #####
# make source and wave factors
combined_df$wave <- factor(combined_df$wave,
                                levels = c("1", "2"),
                                labels = c("1",
                                           "2"))

# make age numeric
combined_df$age <- as.numeric(combined_df$age)

# make state_residence factor
combined_df$state_residence <- as.factor(combined_df$state_residence)

# check zip code
table(is.na(combined_df$zipcode))

# clean age groups turn into factor
combined_df$age_group <- factor(combined_df$age_group,
                                levels = c("1", "2", "3", "4"),
                                labels = c("18-34",
                                           "35-54",
                                           "55-64",
                                           "65+"))
table(combined_df$age_group)
class(combined_df$age_group)

# clean gender
combined_df$gender <- case_when(
  combined_df$gender %in% c("1", 1, "Woman") ~ "1",
  combined_df$gender %in% c("2", 2, "Man") ~ "2",
  combined_df$gender %in% c("3", 3, "Transgender/Trans woman") ~ "3",
  combined_df$gender %in% c("4", 4, "Transgender/Trans man") ~ "4",
  combined_df$gender %in% c("5", 5, "Non-Binary") ~ "5",
  combined_df$gender %in% c("6", 6, "Other") ~ "6",
  combined_df$gender %in% c("98", 98, "Prefer not to say") ~ "98",
  TRUE ~ NA_character_
)

# create factor with labels
combined_df$gender <- factor(combined_df$gender,
                             levels = c("1", "2", "3", "4", "5", "6", "98"),
                             labels = c("Woman", "Man", "Transgender/Trans woman", "
                                        Transgender/Trans man", "Non-Binary", 
                                        "Other", "Prefer not to say"))

# clean race categories
vars <- c("race_white", "race_black", "race_hispanic", "race_native", 
          "race_asian", "race_other", "race_nr")
lapply(combined_df[vars], table, useNA = "ifany")

combined_df[vars] <- lapply(combined_df[vars], function(x) {
  ifelse(x == "selected" | x == "1", 1,
         ifelse(x == "not selected" | x == "0", 0, NA_character_))
  
  factor(x,
         levels = c("0", "1"),
         labels = c(
           "No",
           "Yes"))
})

# clean neighborhood
vars <- c("neighborhood")
lapply(combined_df[vars], table, useNA = "ifany")

# clean
combined_df$neighborhood <- case_when(
  combined_df$neighborhood %in% c("1", 1, "Urban") ~ "1",
  combined_df$neighborhood %in% c("2", 2, "Rural") ~ "2",
  combined_df$neighborhood %in% c("3", 3, "Suburban") ~ "3",
  combined_df$neighborhood %in% c("4", 4, "Other") ~ "4",
  combined_df$neighborhood %in% c("98", 98, "Prefer not to say") ~ "98",
  TRUE ~ NA_character_
)

# create factor with labels
combined_df$neighborhood <- factor(combined_df$neighborhood,
                                   levels = c("1", "2", "3", "4", "98"),
                                   labels = c("Urban", "Rural", "Suburban", "Other",
                                              "Prefer not to say"))

# clean language spoken at home
# check current categories
vars <- c("lang_eng", "lang_spa", "lang_fre", "lang_other", "lang_nr")
lapply(combined_df[vars], table, useNA = "ifany")

# clean
combined_df[vars] <- lapply(combined_df[vars], function(x) {
  ifelse(x == "selected" | x == "1", 1,
         ifelse(x == "not selected" | x == "0", 0, NA_character_))
  
  factor(x,
         levels = c("0", "1"),
         labels = c(
           "No",
           "Yes"))
})

# clean the numb_hh_memb variables
# check
vars <- names(combined_df)[23:31]
lapply(combined_df[vars], table, useNA = "ifany")

# clean vars that start w numb_hh
vars <- grep("^numb_hh_memb", names(combined_df), value = TRUE)
combined_df[vars] <- lapply(combined_df[vars], function(i) {
  x <- ifelse(i == "not asked" | i == "skipped", NA_integer_, i)
  as.numeric(x)
})

# clean EDUC_LEVEL
vars <- c("educ_level")
lapply(combined_df[vars], table, useNA = "ifany")

# clean
combined_df$educ_level <- case_when(
  combined_df$educ_level %in% c("1", 1, "Less than high school degree") ~ "1",
  combined_df$educ_level %in% c("2", 2, "High school degree or equivalent (e.g., GED)") ~ "2",
  combined_df$educ_level %in% c("3", 3, "Some college but no degree") ~ "3",
  combined_df$educ_level %in% c("4", 4, "Associate degree") ~ "4",
  combined_df$educ_level %in% c("5", 5, "Bachelor degree") ~ "5",
  combined_df$educ_level %in% c("6", 6, "Graduate degree") ~ "6",
  combined_df$educ_level %in% c("98", 98, "Prefer not to say") ~ "98",
  TRUE ~ NA_character_
)

# create factor with labels
combined_df$educ_level <- factor(combined_df$educ_level,
                                 levels = c("1", "2", "3", "4", "5", "6", "98"),
                                 labels = c("Less than high school degree", 
                                            "High school degree or equivalent (e.g., GED)", 
                                            "Some college but no degree", 
                                            "Associate degree",
                                            "Bachelor degree",
                                            "Graduate degree",
                                            "Prefer not to say"))
# clean WORK_OUTSIDE_HH
vars <- c("work_outside_hh")
lapply(combined_df[vars], table, useNA = "ifany")

# clean
combined_df$work_outside_hh <- case_when(
  combined_df$work_outside_hh %in% c("1", 1, "Yes") ~ "1",
  combined_df$work_outside_hh %in% c("2", 2, "No") ~ "2",
  combined_df$work_outside_hh %in% c("3", 3, "Unemployed") ~ "3",
  combined_df$work_outside_hh %in% c("4", 4, "Retired") ~ "4",
  combined_df$work_outside_hh %in% c("98", 98, "Prefer not to say") ~ "98",
  TRUE ~ NA_character_
)

# create factor with labels
combined_df$work_outside_hh <- factor(combined_df$work_outside_hh,
                                      levels = c("1", "2", "3", "4", "98"),
                                      labels = c("Yes", "No", "Unemployed",
                                                 "Retired",
                                                 "Prefer not to say"))
# clean OCC
vars <- c("occ")
lapply(combined_df[vars], table, useNA = "ifany")

# clean
combined_df$occ <- case_when(
  combined_df$occ %in% c("1", 1, "Private sector employee: for-profit organization") ~ "1",
  combined_df$occ %in% c("2", 2, "Private sector employee: non-profit organization") ~ "2",
  combined_df$occ %in% c("3", 3, "Government employee") ~ "3",
  combined_df$occ %in% c("4", 4, "Self-employed") ~ "4",
  combined_df$occ %in% c("5", 5, "Unemployed") ~ "5",
  combined_df$occ %in% c("6", 6, "Not in labor force") ~ "6",
  combined_df$occ %in% c("7", 7, "Retired") ~ "7",
  combined_df$occ %in% c("98", 98, "Prefer not to say") ~ "98",
  TRUE ~ NA_character_
)

# create factor with labels
combined_df$occ <- factor(combined_df$occ,
                          levels = c("1", "2", "3", "4", "5", "6", "7", "98"),
                          labels = c("Private sector employee: for-profit organization",
                                     "Private sector employee: non-profit organization",
                                     "Government employee",
                                     "Self-employed",
                                     "Unemployed",
                                     "Not in labor force",
                                     "Retired",
                                     "Prefer not to say"))
# clean POLICITAL_PARTY
vars <- c("political_party")
lapply(combined_df[vars], table, useNA = "ifany")

# clean
combined_df$political_party <- case_when(
  combined_df$political_party %in% c("1", 1, "Republican") ~ "1",
  combined_df$political_party %in% c("2", 2, "Democrat") ~ "2",
  combined_df$political_party %in% c("3", 3, "Independent") ~ "3",
  combined_df$political_party %in% c("4", 4, "Something else") ~ "4",
  combined_df$political_party %in% c("98", 98, "Prefer not to say") ~ "98",
  TRUE ~ NA_character_
)

# create factor with labels
combined_df$political_party <- factor(combined_df$political_party,
                                      levels = c("1", "2", "3", "4", "98"),
                                      labels = c("Republican", 
                                                 "Democrat", 
                                                 "Independent",
                                                 "Something else",
                                                 "Prefer not to say"))
# clean HH_INCOME
vars <- c("hh_income")
lapply(combined_df[vars], table, useNA = "ifany")

# clean
combined_df$hh_income <- case_when(
  combined_df$hh_income %in% c("1", 1, "Less than $10,000") ~ "1",
  combined_df$hh_income %in% c("2", 2, "$10,000 to $19,999") ~ "2",
  combined_df$hh_income %in% c("3", 3, "$20,000 to $29,999") ~ "3",
  combined_df$hh_income %in% c("4", 4, "$30,000 to $39,999") ~ "4",
  combined_df$hh_income %in% c("5", 5, "$40,000 to $49,999") ~ "5",
  combined_df$hh_income %in% c("6", 6, "$50,000 to $69,999") ~ "6",
  combined_df$hh_income %in% c("7", 7, "$70,000 to $84,999") ~ "7",
  combined_df$hh_income %in% c("8", 8, "$85,000 to $99,999") ~ "8",
  combined_df$hh_income %in% c("9", 9, "$100,000 to 149,999") ~ "9",
  combined_df$hh_income %in% c("10", 10, "$150,000 to $199,999") ~ "10",
  combined_df$hh_income %in% c("11", 11, "$200,000 or more") ~ "11",
  combined_df$hh_income %in% c("98", 98, "Prefer not to say") ~ "98",
  TRUE ~ NA_character_
)

# create factor with labels
combined_df$hh_income <- factor(combined_df$hh_income,
                                levels = c("1", "2", "3", "4", "5", 
                                           "6", "7", "8", "9", "10",
                                           "11", "98"),
                                labels = c("Less than $10,000", 
                                           "$10,000 to $19,999",
                                           "$20,000 to $29,999",
                                           "$30,000 to $39,999",
                                           "$40,000 to $49,999",
                                           "$50,000 to $69,999",
                                           "$70,000 to $84,999",
                                           "$85,000 to $99,999",
                                           "$100,000 to 149,999",
                                           "$150,000 to $199,999",
                                           "$200,000 or more",
                                           "Prefer not to say"))

# clean variables with yes/no/prefer not to respond
vars <- c("own_smartphone", "jail", "no_food", "no_transport",
          "sleep_outside", "worry_house", "unsafe", "paid_sick_leave")
lapply(combined_df[vars], table, useNA = "ifany")

# clean
combined_df[vars] <- lapply(combined_df[vars], function(x) {
  x <- case_when(
    x %in% c("1", 1, "Yes") ~ "1",
    x %in% c("0", 0, "No") ~ "0",
    x %in% c("98", 98, "Prefer not to say") ~ "98",
    TRUE ~ NA_character_
  )
  
  factor(x,
         levels = c("0", "1", "98"),
         labels = c(
           "No",
           "Yes",
           "Prefer not to say"
         ))
})

# clean HEALTH_INS
vars <- c("health_ins")
lapply(combined_df[vars], table, useNA = "ifany")

# clean
combined_df$health_ins <- case_when(
  combined_df$health_ins %in% c("0", 0, "No") ~ "0",
  combined_df$health_ins %in% c("1", 1, "Yes, through an employer or union (by me or a family member)") ~ "1",
  combined_df$health_ins %in% c("2", 2, "Yes, purchased directly from an insurance company (by me or a family member)") ~ "2",
  combined_df$health_ins %in% c("3", 3, "Yes, through Medicare") ~ "3",
  combined_df$health_ins %in% c("4", 4, "Yes, through Medicaid") ~ "4",
  combined_df$health_ins %in% c("5", 5, "Yes, through any other type of insurance") ~ "5",
  combined_df$health_ins %in% c("98", 98, "Prefer not to say") ~ "98",
  TRUE ~ NA_character_
)

# create factor with labels
combined_df$health_ins <- factor(combined_df$health_ins,
                                 levels = c("0", "1", "2", "3", 
                                            "4", "5", "98"),
                                 labels = c("No", 
                                            "Yes, through an employer or union (by me or a family member)", 
                                            "Yes, purchased directly from an insurance company (by me or a family member)",
                                            "Yes, through Medicare",
                                            "Yes, through Medicaid",
                                            "Yes, through any other type of insurance",
                                            "Prefer not to say"))

# clean PAY_BASICS
vars <- c("pay_basics")
lapply(combined_df[vars], table, useNA = "ifany")
# clean
combined_df$pay_basics <- case_when(
  combined_df$pay_basics %in% c("0", 0, "Not hard at all") ~ "0",
  combined_df$pay_basics %in% c("1", 1, "Not very hard") ~ "1",
  combined_df$pay_basics %in% c("2", 2, "Somewhat hard") ~ "2",
  combined_df$pay_basics %in% c("3", 3, "Hard") ~ "3",
  combined_df$pay_basics %in% c("4", 4, "Very hard") ~ "4",
  combined_df$pay_basics %in% c("98", 98, "Prefer not to say") ~ "98",
  TRUE ~ NA_character_
)

# create factor with labels
combined_df$pay_basics <- factor(combined_df$pay_basics,
                                 levels = c("0", "1", "2", "3", 
                                            "4", "98"),
                                 labels = c("Not hard at all", 
                                            "Not very hard", 
                                            "Somewhat hard",
                                            "Hard",
                                            "Very hard",
                                            "Prefer not to say"))


###### MODULE 2 #####
# clean REG_TRAVEL
vars <- c("reg_travel")
lapply(combined_df[vars], table, useNA = "ifany")


# clean
combined_df$reg_travel <- ifelse(
  combined_df$reg_travel %in% c("Yes", "1"), 1,
  ifelse(combined_df$reg_travel %in% c("No", "0"), 0, 
         ifelse(combined_df$reg_travel %in% c("Prefer not to say",
                                              "98"), 98, NA_character_)))
# as factor
combined_df$reg_travel <- factor(combined_df$reg_travel,
                                 levels = c("0", "1", "98"),
                                 labels = c("No", "Yes", "Prefer not to say"))

# clean the REG_TRAVEL_TRANSPORT
vars <- names(combined_df[,63:72])
vars <- c("reg_travel_car","reg_travel_rv", "reg_travel_train","reg_travel_bus",
          "reg_travel_plane", "reg_travel_boat", "reg_travel_walk",  "reg_travel_other",
          "reg_travel_nr")
lapply(combined_df[vars], table, useNA = "ifany")
combined_df[vars] <- lapply(combined_df[vars], function(x) {
  x <- case_when(
    x %in% c("1", 1, "Yes") ~ "1",
    x %in% c("0", 0, "No") ~ "0",
    TRUE ~ NA_character_
  )
  
  factor(x,
         levels = c("0", "1"),
         labels = c(
           "No",
           "Yes"))
})

# clean the OTHER_TRAVEL
vars <- c("other_travel")
lapply(combined_df[vars], table, useNA = "ifany")
combined_df[vars] <- lapply(combined_df[vars], function(x) {
  x <- case_when(
    x %in% c("1", 1, "Yes") ~ "1",
    x %in% c("0", 0, "No") ~ "0",
    x %in% c("98") ~ "98",
    TRUE ~ NA_character_
  )
  
  factor(x,
         levels = c("0", "1", "98"),
         labels = c(
           "No",
           "Yes",
           "Prefer not to respond"
         ))
})

# clean NUMB_DAYS_REG_TRAVEL
vars <- c("numb_days_reg_travel")
lapply(combined_df[vars], table, useNA = "ifany")
combined_df$numb_days_reg_travel <- as.numeric(combined_df$numb_days_reg_travel)
# only in yougov

# clean OTHER_TRAVEL_CITY_1
vars <- c("other_travel_city1")
lapply(combined_df[vars], table, useNA = "ifany")
# needs manual cleaning

# clean REG_TRAVEL_CAR
vars <- c("reg_travel_car1", "reg_travel_rv1", "reg_travel_train1", "reg_travel_bus1",
          "reg_travel_plane1", "reg_travel_boat1", "reg_travel_walk1", "reg_travel_other1")
lapply(combined_df[vars], table, useNA = "ifany")

# replace the trailing 1 with 2, 3, 4, and 5
suffixes <- 2:5
expanded_vars <- unlist(lapply(suffixes, function(i) gsub("1$", i, vars)))

# combine original + expanded
other_travel_vars <- c("travel_car1", "travel_rv1", "travel_train1", "travel_bus1",
                       "travel_plane1", "travel_boat1", "travel_walk1", "travel_other1",
                       "travel_pr1")

expanded_other_travel_vars <- unlist(lapply(suffixes, function(i) gsub("1$", i, other_travel_vars)))
vars <- c(vars, expanded_vars, other_travel_vars, expanded_other_travel_vars)
lapply(combined_df[vars], table, useNA = "ifany")

# these are all yougov, clean them to 0/1
combined_df[vars] <- lapply(combined_df[vars], function(i) {
  x <- ifelse(i == "not asked", NA_character_,
              ifelse(i == "not selected" | i == "0", 0,
                     ifelse(i == "selected" | i == "1", 1, NA_character_)))
  factor(x, 
         levels = c("0", "1"),
         labels = c("No", "Yes"))
})

# clean TRAVEL_DUR1-5 and TRAVEL_NUMB_PEOPLE1-5
vars <- c("travel_dur1", "travel_numb_people1", "travel_dur2", "travel_numb_people2",
          "travel_dur3", "travel_numb_people3", "travel_dur4", "travel_numb_people4",
          "travel_dur5", "travel_numb_people5")
lapply(combined_df[vars], table, useNA = "ifany")


# clean
combined_df[vars] <- lapply(combined_df[vars], function(i) {
  x <- ifelse(i == "not asked", NA_integer_, i)
  as.numeric(x)
})

# clean TRAVEL_ALONE1-5
vars <- paste0("travel_alone", 1:5)
lapply(combined_df[vars], table, useNA = "ifany")

# clean each column
combined_df[vars] <- lapply(combined_df[vars], function(x) {
  ifelse(
    x %in% c("Yes", "1"), 1,
    ifelse(x %in% c("No", "0"), 0,
           ifelse(x %in% c("Prefer not to say", "98"), 98,
                  ifelse(x == "not asked", NA_character_, NA_character_)))
  )
  
  factor(x, 
         levels = c("0", "1", "98"),
         labels = c("No", "Yes", "Prefer not to say"))
})

# clean TRAVEL_WHO1-5
vars <- paste0("travel_who", 1:5)
lapply(combined_df[vars], table, useNA = "ifany")


# clean
combined_df[vars] <- lapply(combined_df[vars], function(x) {
  x <- case_when(
    x %in% c("1", 1, "People you live with") ~ "1",
    x %in% c("2", 2, "People you don’t live with") ~ "2",
    x %in% c("3", 3, "Both people you live with and don’t live with") ~ "3",
    x %in% c("98", 98, "Prefer not to say") ~ "98",
    TRUE ~ NA_character_
  )
  
  factor(x,
         levels = c("1", "2", "3", "98"),
         labels = c(
           "People you live with",
           "People you don’t live with",
           "Both people you live with and don’t live with",
           "Prefer not to say"
         ))
})

# clean SOCIAL_10
vars <- c("social_10", "social_100")
lapply(combined_df[vars], table, useNA = "ifany")

# clean
combined_df[vars] <- lapply(combined_df[vars], function(x) {
  x <- case_when(
    x %in% c("0", 0, "Never") ~ "0",
    x %in% c("1", 1, "Once or twice") ~ "1",
    x %in% c("2", 2, "3-7 times") ~ "2",
    x %in% c("3", 3, "More than 7 times") ~ "3",
    x %in% c("98", 98, "Prefer not to say") ~ "98",
    TRUE ~ NA_character_
  )
  
  factor(x,
         levels = c("0", "1", "2", "3", "98"),
         labels = c("Never", "Once or twice",
                    "3-7 times", "More than 7 times",
                    "Prefer not to say"))
})

# clean CONTACT variables
# determine all the varnames
names(combined_df[,155:246])
vars <- names(combined_df)[grepl("^any_physical_|^non_physical_", names(combined_df))]
length(vars)

# remove the questions about any physical or non-physical contact (Q4)
# clean them separately
vars_any_contact <- c("any_physical_0_1", "any_physical_1_4", "any_physical_5_9", 
                      "any_physical_10_17", "any_physical_18_29", "any_physical_30_39", 
                      "any_physical_40_49", "any_physical_50_59", "any_physical_60_plus", 
                      "any_physical_none",  "non_physical_0_1", "non_physical_1_4", 
                      "non_physical_5_9", "non_physical_10_17", "non_physical_18_29", 
                      "non_physical_30_39", "non_physical_40_49", "non_physical_50_59", 
                      "non_physical_60_plus", "non_physical_none")
lapply(combined_df[vars_any_contact], table, useNA = "ifany")

# clean
combined_df[vars_any_contact] <- lapply(combined_df[vars_any_contact], function(y) {
  ifelse(y == "selected" | y == "1", 1,
         ifelse(y == "not selected" | y == "0", 0, NA_integer_))
})

# clean all the other contact variables
vars <- setdiff(vars, vars_any_contact)
lapply(combined_df[vars], table, useNA = "ifany")


# check what the non-numeric values are
non_numeric_summary <- lapply(combined_df[vars], function(col) {
  col_chr <- as.character(col)
  is_num <- grepl("^\\s*\\d+(\\.\\d+)?\\s*$", col_chr)
  unique(col_chr[!is_num & !is.na(col_chr)])
})

non_numeric_summary

# clean to remove any non numeric values
# all other value, text, blanks or ranges of numbers to 
combined_df[vars] <- lapply(combined_df[vars], function(y) {
  x <- ifelse(y == "__NA__" | y == "", NA_integer_,
              ifelse(y == "⁰" | y == "0", 0,
                     ifelse(y == "`1", 1, y))) # random symbol that was in there, assume was 0
  x <- as.numeric(x) # will turn non-numeric into NAs, expect 15
})

# clean VAX variables with yes/no/not sure 0, 1, 2, 98
# determine all the varnames
vars <- c("vax_hx_covid", "vax_flu_this_year", "vax_hx_rsv", "vax_hx_mmr",
          "covid_hx", "covid_test", "covid_test_positive", "flu_test",
          "flu_test_positive", "rsv_test", "rsv_test_positive", "hospitalized_covid",
          "hospitalized_flu", "hospitalized_rsv", "vax_hx_flu_past_2_yrs",
          "past_2_wks_covid_symptoms", "past_2_wks_covid_exposed",
          "past_2_wks_covid_need_test", "covid_antibodies",
          "covid_antibodies_positive",  "med_covid",
          "willing_med_covid", "septic", "water_sewer_bill")
lapply(combined_df[vars], table, useNA = "ifany")

# clean these variables (no, yes, not sure, prefer not to say)
# clean
combined_df[vars] <- lapply(combined_df[vars], function(x) {
  x <- case_when(
    x %in% c("1", 1, "Yes") ~ "1",
    x %in% c("0", 0, "No") ~ "0",
    x %in% c("2", 2, "Not sure", "Not Sure") ~ "2",
    x %in% c("not asked") ~ NA_character_,
    x %in% c("98", 98, "Prefer not to say") ~ "98",
    TRUE ~ NA_character_
  )
  
  factor(x,
         levels = c("0", "1", "2", "98"),
         labels = c(
           "No",
           "Yes",
           "Not sure",
           "Prefer not to say"
         ))
})

#### MODULES 3, 4, AND 5 ####
# clean VAX_HX_COVID_DOSES
vars <- c("vax_hx_covid_doses")
lapply(combined_df[vars], table, useNA = "ifany")


# clean 
# clean VAX_HX_COVID_DOSES
combined_df$vax_hx_covid_doses <- case_when(
  combined_df$vax_hx_covid_doses %in% c("1", 1, "5 or more") ~ "1",
  combined_df$vax_hx_covid_doses %in% c("2", 2, "4") ~ "2",
  combined_df$vax_hx_covid_doses %in% c("3", 3, "3") ~ "3",
  combined_df$vax_hx_covid_doses %in% c("4", 4, "2") ~ "4",
  combined_df$vax_hx_covid_doses %in% c("5", 5, "1") ~ "5",
  combined_df$vax_hx_covid_doses %in% c("6", 6, "Not sure") ~ "6",
  combined_df$vax_hx_covid_doses %in% c("not asked") ~ NA_character_,
  combined_df$vax_hx_covid_doses %in% c("98", 98, "Prefer not to say") ~ "98",
  TRUE ~ NA_character_
)

# create factor with labels
combined_df$vax_hx_covid_doses <- factor(combined_df$vax_hx_covid_doses,
                                         levels = c("1", "2", "3", "4", "5", "6", "98"),
                                         labels = c("5 or more", "4", "3", "2", "1", 
                                                    "Not sure", "Prefer not to say"))

# clean VAX_HX_COVID_LAST_TIME
# define vars
vars <- c("vax_hx_covid_last_time", "covid_hx_recent", "covid_need_test_recent",
          "covid_test_positive_recent", "flu_test_positive_recent",
          "rsv_test_positive_recent")

# look at distributions before cleaning
lapply(combined_df[vars], table, useNA = "ifany")

# clean
combined_df[vars] <- lapply(combined_df[vars], function(x) {
  x <- case_when(
    x %in% c("1", 1, "This summer (June-Aug 2024)") ~ "1",
    x %in% c("2", 2, "This spring (March-May 2024)") ~ "2",
    x %in% c("3", 3, "This winter (December 2023-February 2024)") ~ "3",
    x %in% c("4", 4, "Before December 2023") ~ "4",
    x %in% c("5", 5, "Not sure") ~ "5",
    x %in% c("6", 6, "This fall (Sep 2024-now)") ~ "6",
    x %in% c("98", 98, "Prefer not to say") ~ "98",
    x %in% c("not asked") ~ NA_character_,
    TRUE ~ NA_character_
  )
  
  factor(x,
         levels = c("1", "2", "3", "4", "5", "6", "98"),
         labels = c(
           "This summer (June-Aug 2024)",
           "This spring (March-May 2024)",
           "This winter (December 2023-February 2024)",
           "Before December 2023",
           "Not sure",
           "This fall (Sep 2024-now)",
           "Prefer not to say"
         ))
})

# clean covid_new_vax_willing
vars <- c("covid_new_vax_willing")
lapply(combined_df[vars], table, useNA = "ifany")

combined_df$covid_new_vax_willing <- case_when(
  combined_df$covid_new_vax_willing %in% c("1", 1, "Extremely willing") ~ "1",
  combined_df$covid_new_vax_willing %in% c("2", 2, "Willing") ~ "2",
  combined_df$covid_new_vax_willing %in% c("3", 3, "Not willing") ~ "3",
  combined_df$covid_new_vax_willing %in% c("4", 4, "Extremely not willing") ~ "4",
  combined_df$covid_new_vax_willing %in% c("98", 98, "Prefer not to say") ~ "98",
  TRUE ~ NA_character_
)

# create factor with labels
combined_df$covid_new_vax_willing <- factor(combined_df$covid_new_vax_willing,
                                            levels = c("1", "2", "3", "4", "98"),
                                            labels = c("Extremely willing", 
                                                       "Willing", 
                                                       "Not willing",
                                                       "Extremely not willing",
                                                       "Prefer not to say"))



# clean the how likely are you to wear a mask next time table questions
vars <- grep("^mask_", names(combined_df), value = TRUE)
vars_remove <- c("mask_covid")
vars <- setdiff(vars, vars_remove)
lapply(combined_df[vars], table, useNA = "ifany")

# clean
combined_df[vars] <- lapply(combined_df[vars], function(x) {
  x <- case_when(
    x %in% c("1") ~ "1",
    x %in% c("2") ~ "2",
    x %in% c("3") ~ "3",
    x %in% c("4") ~ "4",
    x %in% c("5") ~ "5",
    x %in% c("6") ~ "6",
    x %in% c("7") ~ "98",
    TRUE ~ NA_character_
  )
  
  factor(x,
         levels = c("1", "2", "3", "4", "5", "6", "98"),
         labels = c(
           "Extremely unlikely",
           "Unlikely",
           "Likely",
           "Extremely Likely",
           "No plans to go",
           "Don't know",
           "Prefer not to say"
         ))
})

# clean the why did you want or need to get tested for COVID-19 table
vars <- c("why_covid_test_symptoms", "why_covid_test_someone_symptoms", 
          "why_covid_test_someone_pos", "why_covid_test_close_contact_positive", 
          "why_covid_test_medical", "why_covid_test_visit_elder", 
          "why_covid_test_school_work", "why_covid_test_gathering",
          "why_covid_test_curious", "why_covid_test_other")
# clean these variables (no, yes, not sure, prefer not to say)
# clean
combined_df[vars] <- lapply(combined_df[vars], function(x) {
  x <- case_when(
    x %in% c("1", 1, "Yes") ~ "1",
    x %in% c("0", 0, "No") ~ "0",
    TRUE ~ NA_character_
  )
  
  factor(x,
         levels = c("0", "1"),
         labels = c(
           "No",
           "Yes"
         ))
})

# clean COVID_INSTANCES
vars <- c("covid_instances")
lapply(combined_df[vars], table, useNA = "ifany")
combined_df$covid_instances <- case_when(
  combined_df$covid_instances %in% c("1", 1, "Once") ~ "1",
  combined_df$covid_instances %in% c("2", 2, "Twice") ~ "2",
  combined_df$covid_instances %in% c("3", 3, "Three times or more") ~ "3",
  combined_df$covid_instances %in% c("4", 4, "Not sure") ~ "4",
  combined_df$covid_instances %in% c("98", 98, "Prefer not to say") ~ "98",
  TRUE ~ NA_character_
)

# create factor with labels
combined_df$covid_instances <- factor(combined_df$covid_instances,
                                      levels = c("1", "2", "3", "4", "98"),
                                      labels = c("Once", 
                                                 "Twice", 
                                                 "Three times or more",
                                                 "Not sure",
                                                 "Prefer not to say"))
# clean PAST_2_WKS_COVID_TEST
vars <- c("past_2_wks_covid_test", "covid_need_test")
lapply(combined_df[vars], table, useNA = "ifany")

# clean these variables (no, yes, not sure, prefer not to say)
# clean
combined_df[vars] <- lapply(combined_df[vars], function(x) {
  x <- case_when(
    x %in% c("1", 1, "Yes") ~ "1",
    x %in% c("0", 0, "No") ~ "0",
    x %in% c("98") ~ "98",
    TRUE ~ NA_character_
  )
  
  factor(x,
         levels = c("0", "1", "98"),
         labels = c(
           "No",
           "Yes",
           "Prefer not to respond"
         ))
})

# clean HH_COVID_POSITIVEns type questions
vars <- c("hh_covid_positive", "hh_covid_positive_2_wks",
          "hh_flu_positive_2_wks", "hh_rsv_positive_2_wks")
lapply(combined_df[vars], table, useNA = "ifany")

# clean these variables (no, yes, not sure, prefer not to say)
# clean
combined_df[vars] <- lapply(combined_df[vars], function(x) {
  x <- case_when(
    x %in% c("0", 0, "No") ~ "0",
    x %in% c("1", 1, "Yes, one child") ~ "1",
    x %in% c("2", 2, "Yes, one adult") ~ "2",
    x %in% c("3", 3, "Yes, more than one person") ~ "3",
    x %in% c("4", 4, "Not sure") ~ "4",
    x %in% c("95", 95, "Not applicable/I live alone") ~ "95",
    x %in% c("98", 98, "Prefer not to say") ~ "98",
    TRUE ~ NA_character_
  )
  
  factor(x,
         levels = c("0", "1", "2", "3", "4", "95", "98"),
         labels = c(
           "No",
           "Yes, one child",
           "Yes, one adult",
           "Yes, more than one person",
           "Not sure",
           "Not applicable/I live alone",
           "Prefer not to respond"
         ))
})

#### MODULE 5 ####
# clean MED_COVID to numeric for percentage 0-100
vars <- c("chance_covid", "chance_inf", "chance_resp", "chance_gi",
          "chance_sti")
lapply(combined_df[vars], table, useNA = "ifany")
lapply(combined_df[vars], summary, useNA = "ifany")
combined_df[vars] <- lapply(combined_df[vars], function(x) {
  as.numeric(x)
})

# clean MASK_COVID
vars <- c("mask_covid")
lapply(combined_df[vars], table, useNA = "ifany")
combined_df[vars] <- lapply(combined_df[vars], function(x) {
  x <- case_when(
    x %in% c("1", 1, "Yes, a lot") ~ "1",
    x %in% c("2", 2, "Yes, some") ~ "2",
    x %in% c("3", 3, "Not sure") ~ "3",
    x %in% c("4", 4, "No, it does nothing") ~ "4",
    x %in% c("5", 5, "No, it increases the spread") ~ "5",
    x %in% c("98", 98, "Prefer not to say") ~ "98",
    TRUE ~ NA_character_
  )
  
  factor(x,
         levels = c("1", "2", "3", "4", "5", "98"),
         labels = c(
           "Yes, a lot",
           "Yes, some",
           "Not sure",
           "No, it does nothing",
           "No, it increases the spread",
           "Prefer not to say"
         ))
})

# clean MASK_COVID
vars <- c("mask_covid")
lapply(combined_df[vars], table, useNA = "ifany")
combined_df[vars] <- lapply(combined_df[vars], function(x) {
  x <- case_when(
    x %in% c("1", 1, "Yes, a lot") ~ "1",
    x %in% c("2", 2, "Yes, some") ~ "2",
    x %in% c("3", 3, "Not sure") ~ "3",
    x %in% c("4", 4, "No, it does nothing") ~ "4",
    x %in% c("5", 5, "No, it increases the spread") ~ "5",
    x %in% c("98", 98, "Prefer not to say") ~ "98",
    TRUE ~ NA_character_
  )
  
  factor(x,
         levels = c("1", "2", "3", "4", "5", "98"),
         labels = c(
           "Yes, a lot",
           "Yes, some",
           "Not sure",
           "No, it does nothing",
           "No, it increases the spread",
           "Prefer not to say"
         ))
})

# clean table of agree/disagree in module 5
# clean MED_COVID to numeric for percentage 0-100
vars <- c("agree_resp_work","agree_mask", "agree_test", "agree_other_resp_work",
          "agree_self_resp_work_test", "agree_self_test", "agree_sick_time",
          "agree_self_resp_work_symp")
lapply(combined_df[vars], table, useNA = "ifany")
combined_df[vars] <- lapply(combined_df[vars], function(x) {
  x <- case_when(
    x %in% c("1", 1, "Strongly Agree") ~ "1",
    x %in% c("2", 2, "Agree") ~ "2",
    x %in% c("3", 3, "Disagree") ~ "3",
    x %in% c("4", 4, "Strongly Disagree") ~ "4",
    x %in% c("98", 98, "Prefer not to say") ~ "98",
    TRUE ~ NA_character_
  )
  
  factor(x,
         levels = c("1", "2", "3", "4", "98"),
         labels = c(
           "Strongly Agree",
           "Agree",
           "Disagree",
           "Strongly Disagree",
           "Prefer not to say"
         ))
})

# clean have you ever been tested for the following conditions
vars <- c("test_diabetes", "test_hcv", "test_hbv", "test_hiv", "test_hyper",
          "test_sti")
lapply(combined_df[vars], table, useNA = "ifany")
combined_df[vars] <- lapply(combined_df[vars], function(x) {
  x <- case_when(
    x %in% c("1", 1, "Yes") ~ "1",
    x %in% c("0", 0, "No") ~ "0",
    x %in% c("98") ~ "98",
    TRUE ~ NA_character_
  )
  
  factor(x,
         levels = c("0", "1", "98"),
         labels = c(
           "No",
           "Yes",
           "Prefer not to respond"
         ))
})

# clean these ones
vars <- c("trust_white", "trust_elect_gov",
          "trust_nat_health", "trust_local_health", 
          "trust_social_media", "trust_print_news", "trust_cable_news",
          "trust_broad_news", "trust_religion", "trust_pcp", "trust_prof_med",
          "trust_family_friends", "trust_handle_white", "trust_handle_elect_gov",
          "trust_handle_nat_health", "trust_handle_local_health", "trust_handle_religion",
          "trust_science", "fed_gov_ph")
lapply(combined_df[vars], table, useNA = "ifany")
combined_df[vars] <- lapply(combined_df[vars], function(x) {
  x <- case_when(
    x %in% c("1", 1, "Strongly Agree") ~ "1",
    x %in% c("2", 2, "Agree") ~ "2",
    x %in% c("3", 3, "Disagree") ~ "3",
    x %in% c("4", 4, "Strongly Disagree") ~ "4",
    x %in% c("98", 98, "Prefer not to say") ~ "98",
    x %in% c("99", 99, "Don't know enough") ~ "99",
    TRUE ~ NA_character_)
  
  factor(x,
         levels = c("1", "2", "3", "4", "98", "99"),
         labels = c(
           "Strongly Agree",
           "Agree",
           "Disagree",
           "Strongly Disagree",
           "Prefer not to say",
           "Don't know enough"))
})

#### MODULE 6 ####
vars <- c("h5n1_heard", "dengue_heard", "dengue_had", "mosq_around_home",
          "syph_heard", "syph_had", "mpox_heard", "mpox_had", "mosq_home")
lapply(combined_df[vars], table, useNA = "ifany")
combined_df[vars] <- lapply(combined_df[vars], function(x) {
  x <- case_when(
    x %in% c("1", 1, "Yes") ~ "1",
    x %in% c("0", 0, "No") ~ "0",
    x %in% c("2", 2, "Not sure") ~ "2",
    x %in% c("98") ~ "98",
    TRUE ~ NA_character_
  )
  
  factor(x,
         levels = c("0", "1", "2", "98"),
         labels = c(
           "No",
           "Yes",
           "Not sure",
           "Prefer not to respond"
         ))
})

# clean H5N1_SOURCE questions
# clean vars that start w h5n1_source
vars <- grep("^h5n1_source", names(combined_df), value = TRUE)

# clean vars that start with mosq
vars_mosq <- c("mosq_spray", "mosq_screens", "mosq_pest_control", "mosq_avoid_outside",
               "mosq_unlisted","mosq_ns", "mosq_nr", "mosq_unsure")
# clean vars about residing in place other than US
vars_five_years_another <- grep("^five_years_another_place", names(combined_df), value = TRUE)

# clean sex partner not sure / prefer not to respond
vars_sex_partner <- c("numb_sex_part_ns", "numb_sex_part_nr", "new_sex_part_ns",
                      "new_sex_part_nr")
# combine var list
vars <- c(vars, vars_mosq, vars_five_years_another, vars_sex_partner)
combined_df[vars] <- lapply(combined_df[vars], function(x) {
  x <- case_when(
    x %in% c("1", 1, "Yes") ~ "1",
    x %in% c("0", 0, "No") ~ "0",
    TRUE ~ NA_character_
  )
  
  factor(x,
         levels = c("0", "1"),
         labels = c(
           "No",
           "Yes"
         ))
})

# change numb_sex_part and new_sex_part to numeric variables
combined_df$numb_sex_part <- as.numeric(combined_df$numb_sex_part)
combined_df$new_sex_part <- as.numeric(combined_df$new_sex_part)

# clean RESIDENCE BESIDES US
vars <- grep("^five_years_another_place", names(combined_df), value = TRUE)
combined_df[vars] <- lapply(combined_df[vars], function(x) {
  x <- case_when(
    x %in% c("1", 1, "Yes") ~ "1",
    x %in% c("0", 0, "No") ~ "0",
    TRUE ~ NA_character_
  )
  
  factor(x,
         levels = c("0", "1"),
         labels = c(
           "No",
           "Yes"
         ))
})

# clean all the reg_travel questions at end
vars <- c("reg_travel_num", "numb_days_reg_travel1", "numb_days_reg_travel2",
          "numb_days_reg_travel3", "numb_days_reg_travel4", "numb_days_reg_travel5")
lapply(combined_df[vars], table, useNA = "ifany")
combined_df[vars] <- lapply(combined_df[vars], function(x) {
  i <- ifelse(x == "not asked" | x == "skipped" | x == "No Data", NA_integer_, x)
  as.numeric(i)
  
})

# clean transportation travel questions at end
vars <- c("reg_travel_car1", "reg_travel_rv1", "reg_travel_train1", "reg_travel_bus1",
          "reg_travel_plane1", "reg_travel_boat1", "reg_travel_walk1", 
          "reg_travel_other1", "reg_travel_car2",
          "reg_travel_rv2", "reg_travel_train2", "reg_travel_bus2",
          "reg_travel_plane2", "reg_travel_boat2", "reg_travel_walk2")
lapply(combined_df[vars], table, useNA = "ifany")
combined_df[vars] <- lapply(combined_df[vars], function(x) {
  x <- case_when(
    x %in% c("1", 1, "Yes") ~ "1",
    x %in% c("0", 0, "No") ~ "0",
    TRUE ~ NA_character_
  )
  
  factor(x,
         levels = c("0", "1"),
         labels = c(
           "No",
           "Yes"
         ))
})

# clean reg_travel_nr1 because it was in a different format
vars <- c("reg_travel_nr1")
lapply(combined_df[vars], table, useNA = "ifany")
combined_df[vars] <- lapply(combined_df[vars], function(x) {
  x <- case_when(
    x %in% c("1", 1, "selected") ~ "1",
    x %in% c("0", 0, "not selected") ~ "0",
    x %in% c("not asked") ~ "NA_character_",
    TRUE ~ NA_character_
  )
  
  factor(x,
         levels = c("0", "1"),
         labels = c(
           "No",
           "Yes"
         ))
})

vars <- names(combined_df[,1:50])
lapply(combined_df[vars], table, useNA = "ifany")

# clean LANG_USED
vars <- c("lang_used")
lapply(combined_df[vars], table, useNA = "ifany")

# clean
combined_df$lang_used <- case_when(
  combined_df$lang_used %in% c("1", 1, "English") ~ "1",
  combined_df$lang_used %in% c("2", 2, "Spanish") ~ "2",
  TRUE ~ NA_character_
)

# create factor with labels
combined_df$lang_used <- factor(combined_df$lang_used,
                                      levels = c("1", "2"),
                                      labels = c("English", 
                                                 "Spanish"))

# clean raise_poultry
vars <- c("raise_poultry")
lapply(combined_df[vars], table, useNA = "ifany")
combined_df[vars] <- lapply(combined_df[vars], function(x) {
  x <- case_when(
    x %in% c("0", 0) ~ "0",
    x %in% c("1", 1) ~ "1",
    x %in% c("2", 2) ~ "2",
    x %in% c("3", 3) ~ "3",
    x %in% c("4", 4) ~ "4",
    x %in% c("5", 5) ~ "5",
    
    x %in% c("not asked") ~ "NA_character_",
    TRUE ~ NA_character_
  )
  
  factor(x,
         levels = c("0", "1", "2", "3", "4", "5"),
         labels = c(
           "No",
           "Yes, <10 birds",
           "Yes, 10-24 birds",
           "Yes, 25-199 birds",
           "Yes, 200-999 birds",
           "Yes, 1000+ birds"))
})

# clean paid leave
vars <- c("use_paid_leave")
lapply(combined_df[vars], table, useNA = "ifany")
combined_df[vars] <- lapply(combined_df[vars], function(x) {
  x <- case_when(
    x %in% c("1", 1) ~ "1",
    x %in% c("2", 2) ~ "2",
    x %in% c("3", 3) ~ "3",
    x %in% c("4", 4) ~ "4",
    x %in% c("5", 5) ~ "5",
    x %in% c("6", 6) ~ "6",
    x %in% c("7", 7) ~ "7",
    x %in% c("not asked") ~ "NA_character_",
    TRUE ~ NA_character_
  )
  
  factor(x,
         levels = c("1", "2", "3", "4", "5", "6", "7"),
         labels = c(
           "Highly Likely",
           "Somewhat Likely",
           "Not very Likely",
           "Not at all",
           "Don't know",
           "Prefer not to say", 
           "Not applicable"))
})

# rearrange columns after merge and clean
cols_to_move <- c(
  "reg_travel_num", "numb_days_reg_travel1", "numb_days_reg_travel2", "numb_days_reg_travel3",
  "numb_days_reg_travel4", "numb_days_reg_travel5",
  "reg_travel_car1", "reg_travel_rv1", "reg_travel_train1", "reg_travel_bus1",
  "reg_travel_plane1", "reg_travel_boat1", "reg_travel_walk1", "reg_travel_other1", "reg_travel_nr1",
  "reg_travel_car2", "reg_travel_rv2", "reg_travel_train2", "reg_travel_bus2",
  "reg_travel_plane2", "reg_travel_boat2", "reg_travel_walk2", "reg_travel_other2", "reg_travel_nr2",
  "reg_travel_car3", "reg_travel_rv3", "reg_travel_train3", "reg_travel_bus3",
  "reg_travel_plane3", "reg_travel_boat3", "reg_travel_walk3", "reg_travel_other3", "reg_travel_nr3",
  "reg_travel_car4", "reg_travel_rv4", "reg_travel_train4", "reg_travel_bus4",
  "reg_travel_plane4", "reg_travel_boat4", "reg_travel_walk4", "reg_travel_other4", "reg_travel_nr4",
  "reg_travel_car5", "reg_travel_rv5", "reg_travel_train5", "reg_travel_bus5",
  "reg_travel_plane5", "reg_travel_boat5", "reg_travel_walk5", "reg_travel_other5", "reg_travel_nr5",
  "other_travel_num"
)

combined_df <- combined_df %>% 
  relocate("source") %>%
  relocate("wave") %>%
  relocate("caseid_yg", .after = "uuid_dyn") %>%
  relocate("starttime", "endtime", .after = "caseid_yg") %>%
  relocate(all_of(cols_to_move), .after = "reg_travel_nr")

# export merged wave 1 data
saveRDS(combined_df, "wave1and2_merged.rds") # preserve factors 

# read in later
wave1_2 <- readRDS("wave1and2_merged.rds")

# export merged wave 1 in csv (won't preserve attributes and factors etc)
write.csv(combined_df, "wave1and2_merged.csv", row.names = FALSE)

# double check all the variables
library(skimr)
skim(combined_df)

# checking specific variables
vars <- names(combined_df[,450:500])
lapply(combined_df[vars], table, useNA = "ifany")


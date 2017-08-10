#### Center on Reinventing Public Education #### 
# Title: Denver Census Clean
# Description: Cleaning census data for Denver
# Created by: Kevin Cha on 03-29-17
# Updated by: Kevin Cha on 08-10-17
# Data from: https://www.denvergov.org/opendata
# Link to Github: https://github.com/CRPE-UWB/Local
# Notes:
# -Should have 145 rows (Y)
# -1st 3 columns need to be right (Y)
# -Check if lose any data (Y)
# -Pull out only the variables I bolded in metadata files. (Y)
# -Remove all margin of errors (Y)
# -Add in subname variable (FAM, POV, OCP) that lets us know what the variable is. (Y)
# -Save the cleaned file into Drive 'cleaned files' folder.  (Y)
# -Merge the files together by census tract. Note any issues with merging and take care to watch for any data that we might lose when merging (Y)

# Set up --------------------------------------------------------------------------------------------------------
rm(list=ls())

## Setwd()
setwd("/Users/crpe/Documents/census_clean_v1") # MAC
setwd("C:/Users/phato_000/Documents/CRPE/denver/census_clean_v1") # PC

library(plyr)
library(dplyr)
library(tidyr)
library(stringr)
library(data.table)

# Read in Files --------------------------------------------------------------------------------------------------------
# Datasets
age_sex <- read.csv("data/age_and_sex.csv", stringsAsFactors = FALSE)
demograph_est <- read.csv("data/demographic.csv", stringsAsFactors = FALSE)
education <- read.csv("data/education.csv", stringsAsFactors = FALSE)
housing_char <- read.csv("data/selected_housing_characteristics.csv", stringsAsFactors = FALSE)
household_fam <- read.csv("data/household_and_families.csv", stringsAsFactors = FALSE)
med_income <- read.csv("data/median_household_income.csv", stringsAsFactors = FALSE)
occupation <- read.csv("data/occupation.csv", stringsAsFactors = FALSE)
poverty <- read.csv("data/poverty.csv", stringsAsFactors = FALSE)

# Lists --------------------------------------------------------------------------------------------------------
age_want <- c("GEO.id","GEO.id2","GEO.display.label","HC01_EST_VC01","HC01_MOE_VC01","HC02_EST_VC01","HC02_MOE_VC01","HC03_EST_VC01","HC03_MOE_VC01",
              "HC01_EST_VC03","HC01_MOE_VC03","HC02_EST_VC03","HC02_MOE_VC03","HC03_EST_VC03","HC03_MOE_VC03","HC01_EST_VC04","HC01_MOE_VC04",
              "HC02_EST_VC04","HC02_MOE_VC04","HC03_EST_VC04","HC03_MOE_VC04","HC01_EST_VC05","HC01_MOE_VC05","HC02_EST_VC05","HC02_MOE_VC05",
              "HC03_EST_VC05","HC03_MOE_VC05","HC01_EST_VC06","HC01_MOE_VC06","HC02_EST_VC06","HC02_MOE_VC06","HC03_EST_VC06","HC03_MOE_VC06",
              "HC01_EST_VC07","HC01_MOE_VC07","HC02_EST_VC07","HC02_MOE_VC07","HC03_EST_VC07","HC03_MOE_VC07","HC01_EST_VC08","HC01_MOE_VC08",
              "HC02_EST_VC08","HC02_MOE_VC08","HC03_EST_VC08","HC03_MOE_VC08","HC01_EST_VC09","HC01_MOE_VC09","HC02_EST_VC09","HC02_MOE_VC09",
              "HC03_EST_VC09","HC03_MOE_VC09","HC01_EST_VC10","HC01_MOE_VC10","HC02_EST_VC10","HC02_MOE_VC10","HC03_EST_VC10","HC03_MOE_VC10",
              "HC01_EST_VC11","HC01_MOE_VC11","HC02_EST_VC11","HC02_MOE_VC11","HC03_EST_VC11","HC03_MOE_VC11","HC01_EST_VC12","HC01_MOE_VC12",
              "HC02_EST_VC12","HC02_MOE_VC12","HC03_EST_VC12","HC03_MOE_VC12","HC01_EST_VC13","HC01_MOE_VC13","HC02_EST_VC13","HC02_MOE_VC13",
              "HC03_EST_VC13","HC03_MOE_VC13","HC01_EST_VC14","HC01_MOE_VC14","HC02_EST_VC14","HC02_MOE_VC14","HC03_EST_VC14","HC03_MOE_VC14",
              "HC01_EST_VC15","HC01_MOE_VC15","HC02_EST_VC15","HC02_MOE_VC15","HC03_EST_VC15","HC03_MOE_VC15","HC01_EST_VC16","HC01_MOE_VC16",
              "HC02_EST_VC16","HC02_MOE_VC16","HC03_EST_VC16","HC03_MOE_VC16","HC01_EST_VC17","HC01_MOE_VC17","HC02_EST_VC17","HC02_MOE_VC17",
              "HC03_EST_VC17","HC03_MOE_VC17","HC01_EST_VC18","HC01_MOE_VC18","HC02_EST_VC18","HC02_MOE_VC18","HC03_EST_VC18","HC03_MOE_VC18",
              "HC01_EST_VC19","HC01_MOE_VC19","HC02_EST_VC19","HC02_MOE_VC19","HC03_EST_VC19","HC03_MOE_VC19","HC01_EST_VC20", "HC01_MOE_VC20", 
              "HC02_EST_VC20", "HC02_MOE_VC20", "HC03_EST_VC20", "HC03_MOE_VC20")


dem_want <- c("GEO.id","GEO.id2","GEO.display.label","HC01_VC43","HC02_VC43","HC03_VC43","HC01_VC45","HC02_VC45",
              "HC03_VC45","HC04_VC45","HC01_VC49","HC02_VC49","HC03_VC49","HC04_VC49","HC01_VC50","HC02_VC50",
              "HC03_VC50","HC04_VC50","HC01_VC51","HC02_VC51","HC03_VC51","HC04_VC51","HC01_VC56","HC02_VC56",
              "HC03_VC56","HC04_VC56","HC01_VC57","HC02_VC57","HC03_VC57","HC04_VC57","HC01_VC58","HC02_VC58",
              "HC03_VC58","HC04_VC58","HC01_VC59","HC02_VC59","HC03_VC59","HC04_VC59","HC01_VC60","HC02_VC60",
              "HC03_VC60","HC04_VC60","HC01_VC61","HC02_VC61","HC03_VC61","HC04_VC61","HC01_VC62","HC02_VC62",
              "HC03_VC62","HC04_VC62","HC01_VC63","HC02_VC63","HC03_VC63","HC04_VC63","HC01_VC64","HC02_VC64",
              "HC03_VC64","HC04_VC64","HC01_VC65","HC02_VC65","HC03_VC65","HC04_VC65","HC01_VC66","HC02_VC66",
              "HC03_VC66","HC04_VC66","HC01_VC67","HC02_VC67","HC03_VC67","HC04_VC67","HC01_VC68","HC02_VC68",
              "HC03_VC68","HC04_VC68","HC01_VC69","HC02_VC69","HC03_VC69","HC04_VC69","HC01_VC70","HC02_VC70",
              "HC03_VC70","HC04_VC70","HC01_VC87","HC02_VC87","HC03_VC87","HC04_VC87","HC01_VC89","HC02_VC89",
              "HC03_VC89","HC04_VC89","HC01_VC90","HC02_VC90","HC03_VC90","HC04_VC90","HC01_VC91","HC02_VC91",
              "HC03_VC91","HC04_VC91","HC01_VC92","HC02_VC92","HC03_VC92","HC04_VC92")


edu_want <-c("GEO.id","GEO.id2","GEO.display.label","HC01_EST_VC02","HC01_MOE_VC02","HC02_EST_VC02","HC02_MOE_VC02","HC01_EST_VC03",
             "HC01_MOE_VC03","HC02_EST_VC03","HC02_MOE_VC03","HC01_EST_VC04","HC01_MOE_VC04","HC02_EST_VC04","HC02_MOE_VC04","HC01_EST_VC08",
             "HC01_MOE_VC08","HC02_EST_VC08","HC02_MOE_VC08","HC01_EST_VC11","HC01_MOE_VC11","HC02_EST_VC11","HC02_MOE_VC11","HC01_EST_VC14",
             "HC01_MOE_VC14","HC02_EST_VC14","HC02_MOE_VC14","HC03_EST_VC14","HC03_MOE_VC14","HC04_EST_VC14","HC04_MOE_VC14","HC05_EST_VC14",
             'HC05_MOE_VC14',"HC06_EST_VC14","HC06_MOE_VC14","HC01_EST_VC15","HC01_MOE_VC15",'HC02_EST_VC15',"HC02_MOE_VC15","HC03_EST_VC15",
             "HC03_MOE_VC15","HC04_EST_VC15","HC04_MOE_VC15","HC05_EST_VC15","HC05_MOE_VC15","HC06_EST_VC15","HC06_MOE_VC15","HC01_EST_VC20",
             "HC01_MOE_VC20","HC02_EST_VC20","HC02_MOE_VC20","HC03_EST_VC20","HC03_MOE_VC20","HC04_EST_VC20","HC04_MOE_VC20","HC05_EST_VC20",
             "HC05_MOE_VC20","HC06_EST_VC20","HC06_MOE_VC20","HC01_EST_VC21","HC01_MOE_VC21","HC02_EST_VC21","HC02_MOE_VC21","HC03_EST_VC21",
             "HC03_MOE_VC21","HC04_EST_VC21","HC04_MOE_VC21","HC05_EST_VC21","HC05_MOE_VC21","HC06_EST_VC21","HC06_MOE_VC21","HC01_EST_VC22",
             "HC01_MOE_VC22","HC02_EST_VC22","HC02_MOE_VC22","HC03_EST_VC22","HC03_MOE_VC22","HC04_EST_VC22","HC04_MOE_VC22","HC05_EST_VC22",
             "HC05_MOE_VC22","HC06_EST_VC22","HC06_MOE_VC22","HC01_EST_VC24","HC01_MOE_VC24","HC02_EST_VC24","HC02_MOE_VC24","HC03_EST_VC24",
             "HC03_MOE_VC24","HC04_EST_VC24","HC04_MOE_VC24","HC05_EST_VC24","HC05_MOE_VC24","HC06_EST_VC24","HC06_MOE_VC24","HC01_EST_VC25",
             "HC01_MOE_VC25","HC02_EST_VC25","HC02_MOE_VC25","HC03_EST_VC25","HC03_MOE_VC25","HC04_EST_VC25",'HC04_MOE_VC25',"HC05_EST_VC25",
             "HC05_MOE_VC25","HC06_EST_VC25","HC06_MOE_VC25","HC01_EST_VC26","HC01_MOE_VC26","HC02_EST_VC26","HC02_MOE_VC26","HC03_EST_VC26",
             "HC03_MOE_VC26","HC04_EST_VC26","HC04_MOE_VC26","HC05_EST_VC26","HC05_MOE_VC26","HC06_EST_VC26","HC06_MOE_VC26","HC01_EST_VC28",
             "HC01_MOE_VC28","HC02_EST_VC28","HC02_MOE_VC28","HC03_EST_VC28","HC03_MOE_VC28","HC04_EST_VC28","HC04_MOE_VC28","HC05_EST_VC28",
             "HC05_MOE_VC28","HC06_EST_VC28","HC06_MOE_VC28","HC01_EST_VC29","HC01_MOE_VC29","HC02_EST_VC29","HC02_MOE_VC29","HC03_EST_VC29",
             "HC03_MOE_VC29","HC04_EST_VC29","HC04_MOE_VC29","HC05_EST_VC29","HC05_MOE_VC29","HC06_EST_VC29","HC06_MOE_VC29","HC01_EST_VC30",
             "HC01_MOE_VC30","HC02_EST_VC30","HC02_MOE_VC30","HC03_EST_VC30","HC03_MOE_VC30","HC04_EST_VC30","HC04_MOE_VC30","HC05_EST_VC30",
             "HC05_MOE_VC30","HC06_EST_VC30","HC06_MOE_VC30","HC01_EST_VC32","HC01_MOE_VC32","HC02_EST_VC32","HC02_MOE_VC32","HC03_EST_VC32",
             "HC03_MOE_VC32","HC04_EST_VC32","HC04_MOE_VC32","HC05_EST_VC32","HC05_MOE_VC32","HC06_EST_VC32","HC06_MOE_VC32","HC01_EST_VC33",
             "HC01_MOE_VC33","HC02_EST_VC33","HC02_MOE_VC33","HC03_EST_VC33","HC03_MOE_VC33","HC04_EST_VC33","HC04_MOE_VC33","HC05_EST_VC33",
             "HC05_MOE_VC33","HC06_EST_VC33","HC06_MOE_VC33","HC01_EST_VC34","HC01_MOE_VC34","HC02_EST_VC34","HC02_MOE_VC34","HC03_EST_VC34",
             "HC03_MOE_VC34","HC04_EST_VC34","HC04_MOE_VC34","HC05_EST_VC34","HC05_MOE_VC34","HC06_EST_VC34","HC06_MOE_VC34")

fam_want <- c("GEO.id","GEO.id2","GEO.display.label","HC01_EST_VC02","HC01_MOE_VC02","HC02_EST_VC02","HC02_MOE_VC02","HC03_EST_VC02","HC03_MOE_VC02","HC04_EST_VC02","HC04_MOE_VC02",
              "HC05_EST_VC02","HC05_MOE_VC02","HC01_EST_VC03","HC01_MOE_VC03","HC02_EST_VC03","HC02_MOE_VC03","HC03_EST_VC03","HC03_MOE_VC03",
              "HC04_EST_VC03","HC04_MOE_VC03","HC05_EST_VC03","HC05_MOE_VC03","HC01_EST_VC06","HC01_MOE_VC06","HC02_EST_VC06","HC02_MOE_VC06",
              "HC03_EST_VC06","HC03_MOE_VC06","HC04_EST_VC06","HC04_MOE_VC06","HC05_EST_VC06","HC05_MOE_VC06","HC01_EST_VC10","HC01_MOE_VC10",
              "HC02_EST_VC10","HC02_MOE_VC10","HC03_EST_VC10","HC03_MOE_VC10","HC04_EST_VC10","HC04_MOE_VC10","HC05_EST_VC10","HC05_MOE_VC10",
              "HC01_EST_VC15","HC01_MOE_VC15","HC02_EST_VC15","HC02_MOE_VC15","HC03_EST_VC15","HC03_MOE_VC15","HC04_EST_VC15","HC04_MOE_VC15",
              "HC05_EST_VC15","HC05_MOE_VC15","HC01_EST_VC17","HC01_MOE_VC17","HC02_EST_VC17","HC02_MOE_VC17","HC03_EST_VC17","HC03_MOE_VC17",
              "HC04_EST_VC17","HC04_MOE_VC17","HC05_EST_VC17","HC05_MOE_VC17","HC01_EST_VC19","HC01_MOE_VC19","HC02_EST_VC19","HC02_MOE_VC19",
              "HC03_EST_VC19","HC03_MOE_VC19","HC04_EST_VC19","HC04_MOE_VC19","HC05_EST_VC19","HC05_MOE_VC19")

hou_want <- c("GEO.id","GEO.id2","GEO.display.label","HC01_VC03","HC02_VC03","HC03_VC03","HC04_VC03","HC01_VC04",
              "HC02_VC04","HC03_VC04","HC04_VC04","HC01_VC05","HC02_VC05","HC03_VC05","HC04_VC05","HC01_VC26",
              "HC02_VC26","HC03_VC26","HC04_VC26","HC01_VC27","HC02_VC27","HC03_VC27","HC04_VC27","HC01_VC28",
              "HC02_VC28","HC03_VC28","HC04_VC28","HC01_VC29","HC02_VC29","HC03_VC29","HC04_VC29","HC01_VC30",
              "HC02_VC30","HC03_VC30","HC04_VC30","HC01_VC31","HC02_VC31","HC03_VC31","HC04_VC31","HC01_VC32",
              "HC02_VC32","HC03_VC32","HC04_VC32","HC01_VC33","HC02_VC33","HC03_VC33","HC04_VC33","HC01_VC34",
              "HC02_VC34","HC03_VC34","HC04_VC34","HC01_VC35","HC02_VC35","HC03_VC35","HC04_VC35",'HC01_VC36',
              "HC02_VC36","HC03_VC36","HC04_VC36","HC01_VC65","HC02_VC65",'HC03_VC65',"HC04_VC65","HC01_VC66",
              "HC02_VC66","HC03_VC66","HC04_VC66","HC01_VC69","HC02_VC69","HC03_VC69","HC04_VC69","HC01_VC70",
              "HC02_VC70","HC03_VC70","HC04_VC70","HC01_VC119","HC02_VC119","HC03_VC119","HC04_VC119","HC01_VC120",
              "HC02_VC120","HC03_VC120","HC04_VC120","HC01_VC121","HC02_VC121","HC03_VC121","HC04_VC121","HC01_VC122",
              "HC02_VC122","HC03_VC122","HC04_VC122","HC01_VC123","HC02_VC123",'HC03_VC123',"HC04_VC123",
              "HC01_VC124","HC02_VC124","HC03_VC124","HC04_VC124","HC01_VC125","HC02_VC125","HC03_VC125","HC04_VC125",
              "HC01_VC126","HC02_VC126","HC03_VC126","HC04_VC126","HC01_VC127","HC02_VC127","HC03_VC127","HC04_VC127",
              "HC01_VC128","HC02_VC128","HC03_VC128","HC04_VC128","HC01_VC132","HC02_VC132","HC03_VC132","HC04_VC132",
              "HC01_VC133","HC02_VC133","HC03_VC133","HC04_VC133","HC01_VC134","HC02_VC134","HC03_VC134","HC04_VC134")

med_want <- c("GEO.id","GEO.id2","GEO.display.label","HD01_VD01","HD02_VD01")

ocp_want <- c("GEO.id","GEO.id2","GEO.display.label","HC01_EST_VC01","HC01_MOE_VC01","HC01_EST_VC02","HC01_MOE_VC02","HC01_EST_VC03",
              "HC01_MOE_VC03","HC01_EST_VC04","HC01_MOE_VC04","HC01_EST_VC05","HC01_MOE_VC05","HC01_EST_VC06","HC01_MOE_VC06","HC01_EST_VC07",
              "HC01_MOE_VC07","HC01_EST_VC08","HC01_MOE_VC08","HC01_EST_VC09","HC01_MOE_VC09","HC01_EST_VC10","HC01_MOE_VC10","HC01_EST_VC11",
              "HC01_MOE_VC11","HC01_EST_VC12","HC01_MOE_VC12","HC01_EST_VC13","HC01_MOE_VC13","HC01_EST_VC14","HC01_MOE_VC14","HC01_EST_VC15",
              "HC01_MOE_VC15","HC01_EST_VC16","HC01_MOE_VC16","HC01_EST_VC17","HC01_MOE_VC17","HC01_EST_VC18","HC01_MOE_VC18","HC01_EST_VC19",
              "HC01_MOE_VC19","HC01_EST_VC20","HC01_MOE_VC20","HC01_EST_VC21","HC01_MOE_VC21","HC01_EST_VC22","HC01_MOE_VC22","HC01_EST_VC23",
              "HC01_MOE_VC23","HC01_EST_VC24","HC01_MOE_VC24","HC01_EST_VC25","HC01_MOE_VC25","HC01_EST_VC26","HC01_MOE_VC26","HC01_EST_VC27",
              "HC01_MOE_VC27","HC01_EST_VC28","HC01_MOE_VC28","HC01_EST_VC29","HC01_MOE_VC29","HC01_EST_VC30","HC01_MOE_VC30","HC01_EST_VC31",
              "HC01_MOE_VC31","HC01_EST_VC32","HC01_MOE_VC32","HC01_EST_VC33","HC01_MOE_VC33","HC01_EST_VC34","HC01_MOE_VC34","HC01_EST_VC35",
              "HC01_MOE_VC35","HC01_EST_VC36","HC01_MOE_VC36")

pov_want <- c("GEO.id","GEO.id2","GEO.display.label","HC01_EST_VC01",
              "HC01_MOE_VC01","HC02_EST_VC01","HC02_MOE_VC01","HC03_EST_VC01","HC03_MOE_VC01")

# Functions --------------------------------------------------------------------------------------------------------

# Function 1: Adds 'MOE' to Columns' Names (it was a good idea i swear)
some_moe <- function(df) {
  for (i in 1:ncol(df)) {
    if (i > 3 & i %% 2) {
      colnames(df)[i] <- 'MOE'
    }
    next
  }
  return(df)
}

# Function 3: Gets Rid of 'MOE' Columns 
no_moe <- function(df) {
  return(df[, -grep("MOE", colnames(df))])
}

# Function 4: Add Code to Front of Col Names
add_code <- function(df, code) {
  for (i in 4:ncol(df)) {
    colnames(df)[i] <- paste(code, colnames(df)[i], sep="_")
    next
  }
  return(df)
}

# Function 5: gets rid of 1st column,1st row
no_more_first <- function(df) {
  df <- df[-1,-1]
  return(df)
}

# Function 6: retreieve decimal numeric values from alphanumeric strings and converts it from decimal to whole
# Note: only works to if the numbers are all together ie xx33232swe => GOOD    x332xx56 => BAD cause only gets 332
dec_num_only_2 <- function(x) {
  # sets to look for decimal number
  regexp <- "[[:digit:]]+\\.*[[:digit:]]*"
  x <- str_extract(x, regexp)
  # gets rid of decimal number
  x <- gsub("[.]","",x) 
  return(x)
}

# Function 7: cleaning colmbined
# Note: cause im lazy
lets_clean <- function(df) {
  df <- no_more_first(df)
  df[,3] <- dec_num_only_2(df[,3])
  df[,1] <- gsub("[a-zA-Z]","",df[,1])
  df[df == '-'] <- NA
  df[df == '(X)'] <- NA
  return(df)
}


# Cleaning Time --------------------------------------------------------------------------------------------------------
# Selects which columns to keep
age_sex <- select(age_sex, age_want)
education <- select(education, edu_want)
demograph_est <- select(demograph_est, dem_want) 
housing_char <- select(housing_char, hou_want) 
household_fam <- select(household_fam, fam_want) 
med_income <- select(med_income, med_want) 
occupation <- select(occupation, ocp_want) 
poverty <- select(poverty, pov_want) 

# Prereq: Check if columns have 'MOE' in name
demograph_est <- some_moe(demograph_est)
housing_char <- some_moe(housing_char)
med_income <- some_moe(med_income)

# Gets rid of 'MOE' columns
age_sex <- no_moe(age_sex)
demograph_est <- no_moe(demograph_est) # columns: 102 => 53
education <- no_moe(education) # columns: 191 => 97
housing_char <- no_moe(housing_char) # columns: 127 => 65
household_fam <- no_moe(household_fam) # columns: 73 => 38
med_income <- no_moe(med_income) # columns: 5 => 4
occupation <- no_moe(occupation) # columns: 75 => 39
poverty <- no_moe(poverty) # columns: 9 => 6

# Adds Code to front
age_sex <- add_code(age_sex, "AGE")
demograph_est <- add_code(demograph_est, "DEM")
education <- add_code(education, "EDU")
housing_char <- add_code(housing_char, "HOU")
household_fam <- add_code(household_fam, "FAM")
med_income <- add_code(med_income, "INC")
occupation <- add_code(occupation, "OCP")
poverty <- add_code(poverty, "POV")

# Clean.
age_sex <- lets_clean(age_sex)
demograph_est <- lets_clean(demograph_est)
education <- lets_clean(education)
housing_char <- lets_clean(housing_char)
household_fam <- lets_clean(household_fam)
med_income <- lets_clean(med_income)
occupation <- lets_clean(occupation)
poverty <- lets_clean(poverty)


# Merging --------------------------------------------------------------------------------------------------------
final <- left_join(education, poverty, by=c( 'GEO.id2', 'GEO.display.label'))
final <- left_join(final, occupation, by=c('GEO.id2', 'GEO.display.label'))
final <- left_join(final, med_income, by=c( 'GEO.id2', 'GEO.display.label'))
final <- left_join(final, housing_char, by=c('GEO.id2', 'GEO.display.label'))
final <- left_join(final, demograph_est, by=c( 'GEO.id2', 'GEO.display.label'))
final <- left_join(final, household_fam, by=c('GEO.id2', 'GEO.display.label'))
final <- left_join(final, age_sex, by=c('GEO.id2', 'GEO.display.label'))


# Write .csv Files --------------------------------------------------------------------------------------------------------
write.csv(education, file="clean_data/education.csv")
write.csv(poverty, file="clean_data/poverty.csv")
write.csv(occupation, file="clean_data/occupation.csv")
write.csv(med_income, file="clean_data/medium_household_income.csv")
write.csv(housing_char, file="clean_data/selected_housing_characteristics.csv")
write.csv(demograph_est, file="clean_data/demographic_estimate.csv")
write.csv(household_fam, file="clean_data/household_and_families.csv")
write.csv(final, file="clean_data/census_final_v1.csv")

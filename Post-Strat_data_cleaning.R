#### Preamble ####
# Purpose: Prepare and clean the survey data downloaded from [...UPDATE ME!!!!!]
# Author: Rohan Alexander and Sam Caetano [CHANGE THIS TO YOUR NAME!!!!]
# Data: 22 October 2020
# Contact: rohan.alexander@utoronto.ca [PROBABLY CHANGE THIS ALSO!!!!]
# License: MIT
# Pre-requisites: 
# - Need to have downloaded the ACS data and saved it to inputs/data
# - Don't forget to gitignore it!


library(haven)
library(tidyverse)

raw_data <- read_dta("~/Desktop/STA304/STA304-PS3/usa_00002.dta.gz")


# Add the labels
raw_data <- labelled::to_factor(raw_data)

# Filtering variables and categorizing certain 
# variables into categories that are easier for analysis
reduced_data <- raw_data %>% 
  dplyr::select(region,
         statefip,
         sex, 
         age, 
         race, 
         marst,
         educ) %>% 
  mutate(race = case_when(
    race == "white" ~ "white",
    race == "japanese" ~ "asian",
    race == "chinese" ~ "asian",
    race == "black/african american/negro" ~ "black/african american/negro",
    race == "american indian or alaska native" ~ "others",
    race == "three or more major races" ~ "others",
    race == "other asian or pacific islander" ~ "others",
    race == "two major races" ~ "others",
    race == "other race, nec" ~ "others")) %>% 
  mutate(edu_level = case_when(
    educ == "n/a or no schooling" ~ 0,
    educ == "nursery school to grade 4" ~ 1,
    educ == "grade 5, 6, 7, or 8" ~ 2,
    educ == "grade 9" ~ 3,
    educ == "grade 10" ~ 4,
    educ == "grade 11" ~ 5,
    educ == "grade 12" ~ 6,
    educ == "1 year of college" ~ 7,
    educ == "2 years of college" ~ 8,
    educ == "4 years of college" ~ 9,
    educ == "5+ years of college" ~ 10)) %>% 
  mutate(gender = case_when(
    sex == "male" ~ "Male",
    sex == "female" ~ "Female"
  ))
         
# splitting cells by genders, races, and education levels

reduced_data <- 
  reduced_data %>%
  count(gender, race, edu_level) %>%
  group_by(gender, race, edu_level)


# Saving the census data as a csv file in my
# working directory
write_csv(reduced_data, "~/Desktop/STA304/STA304-PS3/census_data.csv")



         
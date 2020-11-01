#### Preamble ####
# Purpose: Prepare and clean the survey data downloaded from [...UPDATE ME!!!!!]
# Author: Rohan Alexander and Sam Caetano [CHANGE THIS TO YOUR NAME!!!!]
# Data: 22 October 2020
# Contact: rohan.alexander@utoronto.ca [PROBABLY CHANGE THIS ALSO!!!!]
# License: MIT
# Pre-requisites: 
# - Need to have downloaded the data from X and save the folder that you're 
# interested in to inputs/data 
# - Don't forget to gitignore it!


library(haven)
library(tidyverse)

raw_data <- read_dta("~/Desktop/STA304/STA304-PS3/ns20200625/ns20200625.dta")

# Add the labels
raw_data <- labelled::to_factor(raw_data)
# Filtering variables and categorizing certain 
# variables into categories that are easier for analysis
# In accordance with post-strat dataset
reduced_data <- 
  raw_data %>% 
  select(interest,
         registration,
         vote_2016,
         vote_intention,
         vote_2020,
         ideo5,
         employment,
         foreign_born,
         gender,
         census_region,
         hispanic,
         race_ethnicity,
         household_income,
         education,
         state,
         congress_district,
         age)

# setting up logistic estimators for both candidates
reduced_data<-
  reduced_data %>%
  mutate(vote_trump = 
           ifelse(vote_2020 == "Donald Trump", 1, 0)) %>% 
  mutate(vote_biden = 
           ifelse(vote_2020 == "Joe Biden", 1, 0)) %>% 
  mutate(race = case_when(
    race_ethnicity == "White" ~ "white",
    race_ethnicity == "Asian (Asian Indian)" ~ "asian",
    race_ethnicity == "Asian (Vietnamese)" ~ "asian",
    race_ethnicity == "Asian (Chinese)" ~ "asian",
    race_ethnicity == "Asian (Korean)" ~ "asian",
    race_ethnicity == "Asian (Japanese)" ~ "asian",
    race_ethnicity == "Asian (Filipino)" ~ "asian",
    race_ethnicity == "Asian (Other)" ~ "asian",
    race_ethnicity == "Black, or African American" ~ "black/african american/negro",
    race_ethnicity == "Pacific Islander (Native Hawaiian)" ~ "others",
    race_ethnicity == "American Indian or Alaska Native" ~ "others",
    race_ethnicity == "Pacific Islander (Other)" ~ "others",
    race_ethnicity == "Pacific Islander (Samoan)" ~ "others",
    race_ethnicity == "Pacific Islander (Guamanian)" ~ "others",
    race_ethnicity == "Some other race" ~ "others")) %>% 
  mutate(edu_level = case_when(
    education == "3rd Grade or less" ~ 0,
    education == "Middle School - Grades 4 - 8" ~ 1,
    education == "High school graduate" ~ 3,
    education == "Completed some high school" ~ 2,
    education == "Other post high school vocational training" ~ 4,
    education == "Completed some college, but no degree" ~ 5,
    education == "Associate Degree" ~ 6,
    education == "College Degree (such as B.A., B.S.)" ~ 7,
    education == "Completed some graduate, but no degree" ~ 8,
    education == "Masters degree" ~ 9,
    education == "Doctorate degree" ~ 10,
  ))

# Saving the survey/sample data as a csv file in my
# working directory
write_csv(reduced_data, "~/Desktop/STA304/STA304-PS3/survey_data.csv")


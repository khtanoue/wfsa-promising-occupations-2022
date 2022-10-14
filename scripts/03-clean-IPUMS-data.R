## Clean IPUMS extract
## Kara Haberstock Tanoue
## 10.13.2022

## Libraries
library(tidyverse)
library(stringr)
library(purrr)
library(forcats)
library(ipumsr)
library(survey)
library(srvyr) #tidy style version of survey package

##### Read in IPUMS extract and promising occupations list #####

# created custom IPUMS extract for this project
# can view codebook for this extract in documentation folder

if (!require("ipumsr")) stop("Reading IPUMS data into R requires the ipumsr package. It can be installed using the following command: install.packages('ipumsr')")

setwd("raw-data/ipums")
ddi <- read_ipums_ddi("usa_00008.xml")
data <- read_ipums_micro(ddi)
setwd("../..")

## get promising occupations list (just occ codes and titles)
azpromocc21<-read_csv("clean-data/az_promising_occupations21.csv", col_select = c(OCC_CODE, OCC_TITLE))

## get all occ codes list
soccodes18<-read_csv("raw-data/ipums/OCCCODES2018.csv")

##### initial cleaning of IPUMS extract ####

## Definitions
# Working age: ages 18-64
# Full time employment: employed, usually working more than 35 hrs per week, worked 40+ weeks last year
# Part time employment: employed, usually working more than 3 hrs per week, worked at least 14 weeks last year
# Mothers: females with one or more own children in the household
# Mothers of young children: females with one or more own children under age 5 in the household
# Single mothers: Mothers who are separated, divorced, widowed, or never married

## flags we want:
# full-time, year-round employed adults ages 18-64
# full-time, year-round employed females ages 18-64
# full-time, year-round employed mothers ages 18-64
# full-time, year-round employed mothers of children under 5 ages 18-64
# full-time, year-round employed single mothers ages 18-64
# full-time, year-round employed single mothers of children under 5 ages 18-64

# want to end up with top 50 occupations for each of these flagged groups


pums2020 <- data %>% 
  filter(YEAR == 2020) %>% 
  # create easy binary vars for key filters
  mutate(WORKINGAGE = ifelse(AGE >= 18 & AGE < 65, 1, 0),
         FTEMP = ifelse(EMPSTAT == 1 & UHRSWORK >=35 & WKSWORK2 >= 4, 1, 0),
         PTEMP = ifelse(EMPSTAT == 1 & UHRSWORK >=3 & WKSWORK2 >= 2, 1, 0),
         MOM = ifelse(NCHILD >= 1 & SEX == 2, 1, 0), 
         MOMYC = ifelse(NCHLT5 >=1 & SEX == 2, 1, 0),
         SMOM = ifelse(MOM == 1 & MARST >= 3, 1, 0),
         SMOMYC = ifelse(MOMYC == 1 & MARST >= 3, 1, 0),
         CNT = 1,
         EDATT_RC = case_when(EDUCD == 999 ~ "Missing",
                               EDUCD >= 101 ~ "Bach or more",
                               EDUCD >= 90 ~ "Some college",
                               EDUCD >= 81 ~ "Associate",
                               EDUCD >= 65 ~ "Some college",
                               EDUCD >= 62 ~ "HS or GED",
                               TRUE ~ "Less than HS")) %>% 
  left_join(soccodes18, by = c("OCCSOC" = "SOCCODE18"))

# ## Test out survey object and ensure that counts match verification estiamtes
# pums20svy <- pums2020 %>% 
#   # create survey object with person weights specified (srvyr pkg)
#   as_survey_design(ids = 1, weights = PERWT)
# 
# # check count
# svytotal(~CNT, pums20svy)
# # checks out! (see PUMS estimates for verification here: https://www.census.gov/programs-surveys/acs/microdata/documentation.html)

##### Get top occupation lists #####
# top jobs:  full-time, year-round employed adults ages 18-64
topjobs <- pums2020 %>% 
  filter(WORKINGAGE == 1 & FTEMP == 1) %>% 
  as_survey_design(ids = 1, weights = PERWT) %>% 
  group_by(OCCSOC) %>% 
  summarize(EMPCNT = survey_total(),
            MN_INC = survey_mean(INCTOT)) %>% 
  arrange(desc(EMPCNT)) %>% 
  left_join(soccodes18, by = c("OCCSOC" = "SOCCODE18")) %>% 
  slice(1:50)

## attempts to pull a median income are currently failing-- will need to circle back

# top jobs:  full-time, year-round employed females ages 18-64

topfemjobs <- pums2020 %>% 
  filter(WORKINGAGE == 1 & FTEMP == 1 & SEX == 2) %>% 
  as_survey_design(ids = 1, weights = PERWT) %>% 
  group_by(OCCSOC) %>% 
  summarize(EMPCNT = survey_total(),
            MN_INC = survey_mean(INCTOT)) %>% 
  arrange(desc(EMPCNT)) %>% 
  left_join(soccodes18, by = c("OCCSOC" = "SOCCODE18")) %>% 
  slice(1:50)

# top jobs:  full-time, year-round employed mothers ages 18-64

topmomjobs <- pums2020 %>% 
  filter(WORKINGAGE == 1 & FTEMP == 1 & MOM == 1) %>% 
  as_survey_design(ids = 1, weights = PERWT) %>% 
  group_by(OCCSOC) %>% 
  summarize(EMPCNT = survey_total(),
            MN_INC = survey_mean(INCTOT)) %>% 
  arrange(desc(EMPCNT)) %>% 
  left_join(soccodes18, by = c("OCCSOC" = "SOCCODE18")) %>% 
  slice(1:50)

# top jobs:  full-time, year-round employed mothers of young children ages 18-64

topmomycjobs <- pums2020 %>% 
  filter(WORKINGAGE == 1 & FTEMP == 1 & MOMYC == 1) %>% 
  as_survey_design(ids = 1, weights = PERWT) %>% 
  group_by(OCCSOC) %>% 
  summarize(EMPCNT = survey_total(),
            MN_INC = survey_mean(INCTOT)) %>% 
  arrange(desc(EMPCNT)) %>% 
  left_join(soccodes18, by = c("OCCSOC" = "SOCCODE18")) %>% 
  slice(1:50)

# top jobs:  full-time, year-round employed single mothers ages 18-64

topsingmomjobs <- pums2020 %>% 
  filter(WORKINGAGE == 1 & FTEMP == 1 & SMOM == 1) %>% 
  as_survey_design(ids = 1, weights = PERWT) %>% 
  group_by(OCCSOC) %>% 
  summarize(EMPCNT = survey_total(),
            MN_INC = survey_mean(INCTOT)) %>% 
  arrange(desc(EMPCNT)) %>% 
  left_join(soccodes18, by = c("OCCSOC" = "SOCCODE18")) %>% 
  slice(1:50)

# top jobs:  full-time, year-round employed single mothers of young children ages 18-64

topmomycjobs <- pums2020 %>% 
  filter(WORKINGAGE == 1 & FTEMP == 1 & SMOMYC == 1) %>% 
  as_survey_design(ids = 1, weights = PERWT) %>% 
  group_by(OCCSOC) %>% 
  summarize(EMPCNT = survey_total(),
            MN_INC = survey_mean(INCTOT)) %>% 
  arrange(desc(EMPCNT)) %>% 
  left_join(soccodes18, by = c("OCCSOC" = "SOCCODE18")) %>% 
  slice(1:50)

##### Educational attainment for women
edatttab <- pums2020 %>% 
  filter(WORKINGAGE == 1 & FTEMP == 1) %>% 
  mutate(FEM = ifelse(SEX == 2, 1, 0)) %>% 
  as_survey_design(ids = 1, weights = PERWT) %>% 
  group_by(EDATT_RC) %>% 
  summarize(TOTCNT = survey_total(),
            WFEMCNT = survey_total(FEM),
            WMOMCNT = survey_total(MOM),
            WMOMYCCNT = survey_total(MOMYC),
            WSMOMCNT = survey_total(SMOM),
            WSMOMYCCNT = survey_total(SMOMYC)) %>% 
  mutate(pTot = TOTCNT/sum(TOTCNT),
         pWFEM = WFEMCNT/sum(WFEMCNT),
         pWMOM = WMOMCNT/sum(WMOMCNT),
         pWMOMYC = WMOMYCCNT/sum(WMOMYCCNT),
         pWSMOM = WSMOMCNT/sum(WSMOMCNT),
         pWSMOMYC = WSMOMYCCNT/sum(WSMOMYCCNT))
  

##### Get FT/PT work breakdown for top jobs for women, moms, single moms without 4-year degree


#### Re-slice top jobs for workers without a 4-yr degree


#### Get employment, education, & % female stats for promising occupations ####




#### check educational attainment for promising occupations-- remove jobs where more than (50%? 75%-- look to see) of workers have a bachelor's degree or more



#### Outputs:

# Table of top occupations for women with gender breakdowns and median earnings (workers, men, women)

# Merged table of top occupations for mothers, mothers of young children, & single mothers of young children with counts, median earnings

# Table with FT/PT work split for top occupation for women/mothers without a bachelor's degree

# Promising occupation table with % with bachelors degree or higher, employment counts by gender, and median earnings added





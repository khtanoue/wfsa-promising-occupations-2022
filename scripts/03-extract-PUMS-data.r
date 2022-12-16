## Get PUMS extract
## Kara Haberstock Tanoue
## 11.01.2022

## Libraries
library(tidyverse)
library(tidycensus)
library(srvyr) #tidy style version of survey package

##### Read in PUMS extract #####
# Use get_pums from the tidycensus library to download a pums extract with key variables of interest:

# Get a Census API Key:  https://api.census.gov/data/key_signup.html
census_api<- read_lines("api-keys/census_api_kt.txt")



# Variables we want:
# SERIALNO housing unique identifier; SPORDER person number; PUMA puma code;
# ADJINC adjustment factor for income and earnings; PWGTP person weight; AGEP age;
# CIT citizenship status; COW class of worker; FER gave birth in last year; 
# LANX language other than English at home; MAR marital status; MIL military service;
# PAP public assistance income in past year; SCH school enrollment; SCHL educational attainment; 
# SEMP self-employment income; SEX sex; WAGP wage/salary income; WKHP usual hrs per week in last year;
# WKL when last worked; WKW weeks worked in past year; WRK worked last week; ESR employment status recode;
# HICOV health insurance coverage recode; MSP married recode; NATIVITY nativity; OCCP occupation recode;
# PAOC presence & age of own childre; PERNP total person's earnings; PINCP total person's income; 
# POVPIP income to poverty ratio recode; RAC1P recoded race; HISP recoded hispanic detail; 
# SOCP SOC occupation recode; PWGTP1-80 replicate weights

##### Get 2020 PUMS ####

data<- get_pums(
  variables = c("SERIALNO",
                "FES",
                "HHT2",
                "HINCP",
                "ADJINC",
                "HUPAOC",
                "NOC",
                "PWGTP",
                "SPORDER",
                "PUMA",
                "AGEP",
                "CIT",
                "NATIVITY",
                "COW",
                "FER",
                "INTP",
                "OIP",
                "PAP",
                "RETP",
                "SEMP",
                "SSIP",
                "SSP",
                "WAGP",
                "PERNP",
                "PINCP",
                "MAR",
                "SCH",
                "SCHG",
                "SCHL",
                "SEX",
                "ESR",
                "WKHP",
                "WKL",
                "WKW",
                "WKWN",
                "JWAP",
                "JWDP",
                "MSP",
                "OCCP",
                "SOCP",
                "PAOC",
                "LANX",
                "MIL",
                "HICOV",
                "POVPIP",
                "RAC1P",
                "HISP"
                ),
  state = "AZ",
  year = 2020,
  survey = "acs5",
  rep_weights = "person",
  key = census_api
)

write_csv(data, "raw-data/pums2020extract.csv")


#### Get 2015 PUMS #####

data<- get_pums(
  variables = c("SERIALNO",
                "FES",
                "HHT2",
                "HINCP",
                "ADJINC",
                "HUPAOC",
                "NOC",
                "PWGTP",
                "SPORDER",
                "PUMA",
                "AGEP",
                "CIT",
                "NATIVITY",
                "COW",
                "FER",
                "INTP",
                "OIP",
                "PAP",
                "RETP",
                "SEMP",
                "SSIP",
                "SSP",
                "WAGP",
                "PERNP",
                "PINCP",
                "MAR",
                "SCH",
                "SCHG",
                "SCHL",
                "SEX",
                "ESR",
                "WKHP",
                "WKL",
                "WKW",
                "WKWN",
                "JWAP",
                "JWDP",
                "MSP",
                "OCCP",
                "SOCP",
                "PAOC",
                "LANX",
                "MIL",
                "HICOV",
                "POVPIP",
                "RAC1P",
                "HISP"
  ),
  state = "AZ",
  year = 2015,
  survey = "acs5",
  rep_weights = "person",
  key = census_api

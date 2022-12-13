## Extract and clean BLS data
## Kara Haberstock Tanoue
## 10.06.2022

## Libraries
library(tidyverse)
library(stringr)
library(purrr)
library(forcats)

##### Read in CSVs ####

# get list of BLS files in raw-data
BLS_files<-list.files("raw-data/bls/")

# specify df names
dfnames<-c("azproj", "azoews", "azprojcty", "typed")

# read in CSVs
for (i in 1:length(BLS_files)) {
  assign(dfnames[i], read_csv(paste0("raw-data/bls/", BLS_files[i])))
}

##### Initial Clean #####

## AZ Occcupation & Wage Survey Data
azoews_clean <- azoews %>% 
  # replace * (wage estimate not available) with NA
  mutate(across(where(is.character), ~na_if(.,"*"))) %>% 
  # replace ** (employment estimate not available) with NA
  mutate(across(where(is.character), ~na_if(.,"**"))) %>% 
  # replace # (hourly wage exceeds $100/hr) with $101
  mutate(across(c(H_MEAN, H_PCT10:H_PCT90), ~str_replace_all(.,"#", "101.00"))) %>% 
  # replace # (annual wage exceeds 208000) with 209000
  mutate(across(c(A_MEAN, A_PCT10:A_PCT90), ~str_replace_all(.,"#", "209000"))) %>%
  # parse data columns into numerics
  mutate(across(TOT_EMP:A_PCT90 & where(is.character), ~parse_number(.)))

#str(azoews_clean)


## AZ Occupation Projections Data
azproj_clean <- azproj %>% 
  rename(state = "Area Name",
         OCC_CODE = "SOC Code2",
         OCC_TITLEAZ = "Occupation Title") %>% 
  mutate(avg_annualopenings = total_openings/10)
  
#str(azproj_clean)


azproj_county<- azprojcty %>% 
  rename(state = "Area Name",
         OCC_CODE = "SOC Code2",
         OCC_TITLEAZ = "Occupation Title") %>% 
  mutate(avg_annualopenings = total_openings/10)

## BLS general education requirements for entry level occupations
typed_clean <- typed %>% 
  rename(OCC_CODE = "2021 National Employment Matrix code",
         OCC_TITLE21 = "2021 National Employment Matrix title",
         ED_REQ = "Typical education needed for entry",
         EXP_REQ = "Work experience in a related occupation",
         TRN_REQ = "Typical on-the-job training needed to attain competency in the occupation")


str(typed_clean)

#### Merge datasets into one for state and one for counties ####

bls_dfs <- list(azoews_clean, azproj_clean, typed_clean)
az_occdata_may21 <- bls_dfs %>% 
  reduce(full_join, by = c("OCC_CODE")) %>% 
  select(-state, -NAICS, -NAICS_TITLE, -I_GROUP,
         -OWN_CODE, -OCC_TITLEAZ,-PCT_TOTAL, -PCT_RPT, -O_GROUP)



#### Export clean dataset #####
write_csv(az_occdata_may21, "clean-data/az_occdata_may21.csv")
write_csv(azproj_county, "clean-data/azcty_proj_sept21.csv")


## Filter BLS data to create promising occupations list
## Kara Haberstock Tanoue
## 10.06.2022

## Libraries
library(tidyverse)
library(stringr)
library(purrr)
library(forcats)

##### Read in cleaned bls data #####
az_occdata0521 <- read_csv("clean-data/az_occdata_may21.csv")

#str(az_occdata0521)

##### filter to promising occupations #####

promocc21 <- az_occdata0521 %>% 
  mutate(ED_REQ.FACTOR = factor(ED_REQ, levels = c("No formal educational credential",
                                                   "High school diploma or equivalent",
                                                   "Some college, no degree",
                                                   "Associate's degree",
                                                   "Bachelor's degree",
                                                   "Master's degree",
                                                   "Postsecondary nondegree award",
                                                   "Doctoral or professional degree" )),
         EXP_REQ.FACTOR = factor(EXP_REQ, levels = c("None",
                                                   "Less than 5 years",
                                                   "5 years or more")),
         TRN_REQ.FACTOR = factor(TRN_REQ, levels = c("None",
                                                        "Short-term on-the-job training",
                                                        "Moderate-term on-the-job training",
                                                        "Long-term on-the-job training",
                                                        "Apprenticeship",
                                                        "Internship/residency"))) %>% 
  filter(as.integer(ED_REQ.FACTOR) <= 4 &
           as.integer(EXP_REQ.FACTOR) <= 2 &
           (as.integer(TRN_REQ.FACTOR) <= 3 | TRN_REQ == "Apprenticeship") &
           H_MEDIAN >= 20 &
           avg_annualopenings >= 150 &
           percchange_2030 > 0)  

# Current filter criteria applied:
#   no more than associates ed req
#   no more than 5 years work exp req
#   no more than moderate-term or apprenticeship training req
#   20 or more hourly median wage
#   150 or more avg annual openings 2020-2030
#   positive job growth 2020-2030
  

#str(promocc21)

##### Output promising occupation list #####
write_csv(promocc21, "clean-data/az_promising_occupations21.csv")



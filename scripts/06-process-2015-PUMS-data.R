## Process PUMS extract for 2015
## Kara Haberstock Tanoue
## 12.15.2022

##### Libraries ######

library(tidyverse)
library(stringr)
library(purrr)
library(forcats)
library(ipumsr)
library(survey)
library(tidycensus)
library(srvyr) #tidy style version of survey package

###### 2015 PUMS Data ######

##### Read in 2015 PUMS data ####
data<- read_csv("raw-data/pums2015extract.csv",
                na = c("b", "bb", "bbb",
                       "bbbb", "bbbbb", "bbbbbb",
                       "bbbbbbb", "bbbbbbbb")) %>% 
  mutate(SCHL = as.numeric(SCHL)) # coerce schl to numeric

## get promising occupations list (just occ codes and titles)
azpromocc21<-read_csv("clean-data/az_promising_occupations21.csv", col_select = c(OCC_CODE, OCC_TITLE)) %>% 
  mutate(OCC_CODE = str_remove(OCC_CODE, "-"))

## get all 2010 occ codes list
soccodes10<-read_csv("raw-data/ipums/OCCODES2010_crosswalk.csv", col_select = c(Census_OccCode_2010,
                                                                                SOCCODE10_CENSUS,
                                                                                SOCCODE10,
                                                                                OCC_TITLE),
                     col_types = "c") %>% 
  mutate(SOCCODE10 = str_remove(SOCCODE10, "-")) 

## get the missing 2012 occ codes (see note under initial PUMS processing section)
soccodes12add<-read_csv("raw-data/ipums/socp12_missing.csv", col_types = "c") %>% 
  mutate(SOCCODE10 = str_remove(SOCCODE10, "-"))

soccodes1012<-soccodes10 %>% 
  bind_rows(soccodes12add)

occ_compare<- soccodes1012 %>%
  full_join(azpromocc21, by = c("SOCCODE10" = "OCC_CODE")) %>%
  rename(OCC_TI_SOC = OCC_TITLE.x,
         OCC_TI_PROM = OCC_TITLE.y)

#### Manually checked to find jobs where we need to collapse codes (due to limitations in PUMS data) ####
# # Computer Support Specialists (151150) = 151231, 151232 (network support, user support)
# # Drafters (173010) = 173011 (Architectural and Civil Drafters), 173012 (Electrical and Electronics Drafters)
# # Engineering Techs (173020) = 173022, 173023, 173026 (Civil engineering, Electrical engineering, Industrial engineering)
# # Misc legal support workers (232090) = 232093 (Title Examiners/Abstractors/Seachers)
# # Other teachers and instructors (253000) = 253021 (Self-enrichment teachers)
# # Broadcast/sound techs (274010) = 274011 (Audio and Video Technicians)
# # Dental hygienists (291292) = 292021 (Dental Hygienists)
# # Diagnostic techs (292030) = 292032 (sonographers), 292034 (rad techs), 292035 (mri techs)
# # Health practice techs (292050) = 292055 (surg techs)
# # Occupational therapy assistants and aides (312010) = 312011 (Occupational Therapy Assistants)
# # Physical therapy assistants and aides (312020) = 312021 (Physical Therapy Assistants)
# # Rec & fitness workers (399030) = 399031 (Exercise/Group fitness instructors)
# # Other services sales reps (413099) = 413091 (service sales reps)
# # Sales reps, wholesale & manufacturing (414010) = 414012 (Sales reps, wholesale & manufacturing except tech/sci) 
# # Real estate brokers & sales agents (419020) = 419022 (Real estate sales agents)
# # Dispatchers (435030) = 435031 (Public safety telecommunicators)
# # Cement masons, concrete finishers, terrazo workers (472050) = 472051 (Cement masons & concrete finishers)
# # Construction equipment operators (472070) = 472073 (Operating Engineers and Other Construction Equipment Operators)
# # Drywall Installers, Ceiling Tile Installers, and Tapers (472080) = 472081 (Drywall & Ceiling Tile Installers)
# # Pipelayers, plumbers, pipefitters, steamfitters (472150) = 472151 (pipelayers), 472152 (plumbers, pipefitters, steamfitters)
# # Mining machine operators (475040) = 475041 (Continuous mining machine operators)
# # Radio and Telecommunications Equipment Installers and Repairers (492020) = 492022 (Radio and Telecommunications Equipment Installers & Repairers except line installers)
# # Control valve installers and repairers (499010) = 499012 (Control valve installers and repairers except mechanical door)
# # Welding, soldering, brazing workers (514120) = 514121 (Welders, cutters, solderers, brazers)
# # Computer numerically controlled tool operators and programmers (519160) = 519199 (other production workers)
# # Supervisors of transportation and material moving workers (531000) = 531047 (first line supervisors of transportation and material moving workers)
# # Driver/Sales Workers and Truck Drivers (533030) = 533032 (Heavy and Tractor-Trailer Truck Drivers)
# # Bus Drivers (533020) = 533052 (Bus Drivers, Transit and Intercity)

#####

missingAZpromocc21 <- c('151231', '151232', '173011', '173012', '173022', '173023', '173026', '232093', '253021',
                        '274011', '292021', '292032', '292034', '292035', '292055', '312011', '312021', '399031', '413091', '414012', '419022',
                        '435031', '472051', '472073', '472081', '472151', '472152', '475041',
                        '492022', '499012', '514121', '519199',
                        '531047', '533032', '533052' )
equivSOC10<- c('151150', '151150', '173010', '173010', '173020', '173020', '173020', '232090', '253000',
               '274010', '291292', '292030', '292030', '292030', '292050', '312010', '312020', '399030', '413099', '414010', '419020',
               '435030',  '472050', '472070', '472080', '472150', '472150', '475040',
               '492020', '499010', '514120',  '519160',
               '531000', '533030', '533020')

updateAZpromocc<- missingAZpromocc21 %>%
  bind_cols(equivSOC10) %>%
  rename(OCC_CODE = "...1",
         SOCCODE10 = "...2")

azpromocc21<- azpromocc21 %>% 
  left_join(updateAZpromocc, by = "OCC_CODE") %>% 
  mutate(SOCCODE10 = ifelse(is.na(SOCCODE10)==TRUE, OCC_CODE, SOCCODE10))

azpromocc21data<- read_csv("clean-data/az_promising_occupations21.csv") %>% 
  mutate(OCC_CODE = str_remove(OCC_CODE, "-")) %>% 
  left_join(azpromocc21, by = c("OCC_CODE", "OCC_TITLE"))

soccodes10uniq<- soccodes1012 %>% 
  distinct(SOCCODE10, .keep_all = TRUE) %>% 
  mutate(SOCCODE10 = str_remove(SOCCODE10, "-"),
         SOCCODE10_CENSUS = str_remove(SOCCODE10_CENSUS, "-")) %>% 
  select(-Census_OccCode_2010)

soccodes10census<-soccodes10uniq %>% 
  distinct(SOCCODE10_CENSUS, .keep_all = TRUE) %>% 
  mutate(SOCCODE10_CENSUS = str_remove(SOCCODE10_CENSUS, "-"))



##### initial cleaning of PUMS extract ####

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

pums2015 <- data %>% 
  # create easy binary vars for key filters
  mutate(WORKINGAGE = ifelse(AGEP >= 18 & AGEP < 65, 1, 0),
         FTEMP = ifelse(ESR == 1 & WKHP >=35 & WKW <= 3, 1, 0),
         PTEMP = ifelse(ESR == 1 & WKHP>0 & WKW <= 5 & FTEMP == 0, 1, 0),
         FEM = ifelse(SEX == 2, 1, 0),
         MAL = ifelse(SEX == 1, 1, 0),
         MOM = ifelse(PAOC <= 3 & SEX == 2, 1, 0), 
         MOMYC = ifelse((PAOC == 1 | PAOC == 3) & SEX == 2, 1, 0),
         SMOM = ifelse(MOM == 1 & MAR >= 2, 1, 0),
         SMOMYC = ifelse(MOMYC == 1 & MAR >= 2, 1, 0),
         EDATT_RC = case_when(SCHL == 20 ~ "Associate",
                              SCHL >= 21 ~ "Bach or more",
                              SCHL >= 18 ~ "Some college",
                              SCHL >= 16 ~ "HS or GED",
                              TRUE ~ "Less than HS"),
         LESSBACH = ifelse(SCHL <= 20, 1, 0),
         BACHMORE = ifelse(SCHL >= 21, 1, 0)) %>%
  # adjust income variables for internal consistency (2015 dollars)
  mutate(PINCP = PINCP*ADJINC,
         PERNP = PERNP*ADJINC,
         PERNP = WAGP*ADJINC) %>% 
  # adjust income variables from 2015 to 2020 dollars: 
  mutate(PINCP = PINCP*1.09257667,
         PERNP = PERNP*1.09257667,
         PERNP = WAGP*1.09257667) %>% 
  mutate(SOCP = if_else(SOCP10 ==  "N.A.//", SOCP12, SOCP10)) %>% 
  left_join(soccodes10uniq, by = c("SOCP10" = "SOCCODE10"))

### Need to figure out how to adjust dollars from 2015 to 2020
# adjustment factor for 2015 to 2020: 1.09257667 (https://www.census.gov/programs-surveys/acs/guidance/comparing-acs-data/2020/5-year-comparison.html)

# checked for matches between new SOCP field and the 2010 SOCCODES
# socpcheck<-unique(pums2015$SOCP)
# socp10list<-unique(soccodes10uniq$SOCCODE10)
# socpmissing<- socpcheck %in% socp10list
# socpcheck[socpmissing == FALSE]
# speed is being prioritized here so I went and found the 5 missing fields in the PUMS documentation
# and made them into a little helper field to create a merged soccode file (see read-in above)
# missing fields from 2012 were ( "472140" "4990XX" "472XXX" "514030" "5140XX" "516040")


## Test out survey object and ensure that counts match verification estimates
# pums15svy <- pums2015 %>%
#   # create survey object with person weights specified (srvyr pkg)
#   to_survey(type = "person", 
#             class = "srvyr", 
#             design = "rep_weights") %>%
#   survey_count()

# # checks out! (see PUMS estimates for verification here: https://www.census.gov/programs-surveys/acs/microdata/documentation.html)

#### Get basic population counts ####
pums2015_poptab<- pums2015 %>% 
  filter(WORKINGAGE == 1) %>% 
  to_survey(type = "person", 
            class = "srvyr", 
            design = "rep_weights") %>%
  summarise(WAPOPCNT = survey_total(na.rm = TRUE),
            FTCNT = survey_total(FTEMP, na.rm = TRUE),
            PTCNT = survey_total(PTEMP, na.rm = TRUE),
            FEMCNT = survey_total(FEM, na.rm = TRUE),
            MLCNT = survey_total(MAL, na.rm = TRUE),
            MOMCNT = survey_total(MOM, na.rm = TRUE), 
            MOMYCCNT = survey_total(MOMYC, na.rm = TRUE),
            SMOMCNT = survey_total(SMOM, na.rm = TRUE),
            SMOMYCCNT = survey_total(SMOMYC, na.rm = TRUE)) %>% 
  pivot_longer(everything())




pums2015_FTtab<- pums2015 %>% 
  filter(WORKINGAGE == 1 & FTEMP == 1) %>% 
  to_survey(type = "person", 
            class = "srvyr", 
            design = "rep_weights") %>%
  summarise(WAFTCNT = survey_total(na.rm = TRUE),
            FEMFTCNT = survey_total(FEM, na.rm = TRUE),
            MLFTCNT = survey_total(MAL, na.rm = TRUE),
            MOMFTCNT = survey_total(MOM, na.rm = TRUE), 
            MOMYCFTCNT = survey_total(MOMYC, na.rm = TRUE),
            SMOMFTCNT = survey_total(SMOM, na.rm = TRUE),
            SMOMYCFTCNT = survey_total(SMOMYC, na.rm = TRUE)) %>% 
  pivot_longer(everything())

pums2015_inschool<- pums2015 %>% 
  filter(WORKINGAGE == 1 & FTEMP == 1 & SCH >= 2) %>% 
  to_survey(type = "person", 
            class = "srvyr", 
            design = "rep_weights") %>%
  summarise(WASCHLCNT = survey_total(na.rm = TRUE),
            FEMSCHLCNT = survey_total(FEM, na.rm = TRUE),
            MLSCHLCNT = survey_total(MAL, na.rm = TRUE),
            MOMSCHLCNT = survey_total(MOM, na.rm = TRUE), 
            MOMYCSCHLCNT = survey_total(MOMYC, na.rm = TRUE),
            SMOMSCHLCNT = survey_total(SMOM, na.rm = TRUE),
            SMOMYCSCHLCNT = survey_total(SMOMYC, na.rm = TRUE)) %>% 
  pivot_longer(everything())

pums_poptab<- pums2015_poptab %>% 
  rbind(pums2015_FTtab, pums2015_inschool)

##### Get top occupation lists #####
# top jobs:  full-time, year-round employed adults ages 18-64
ftpums2015 <- pums2015 %>% 
  filter(WORKINGAGE == 1 & FTEMP == 1) 


topjobs <- ftpums2015 %>% 
  to_survey(type = "person", 
            class = "srvyr", 
            design = "rep_weights") %>% 
  group_by(SOCP) %>% 
  summarise(EMPCNT = survey_total(na.rm = TRUE),
            MN_INC = survey_mean(PINCP,na.rm = TRUE),
            MED_INC = survey_median(PINCP, 
                                    na.rm = TRUE),
            MED_WG = survey_median(PERNP, 
                                   na.rm = TRUE)) %>% 
  arrange(desc(EMPCNT)) %>% 
  left_join(soccodes10uniq, by = c("SOCP" = "SOCCODE10")) %>% 
  slice(1:50)

# top jobs:  full-time, year-round employed females ages 18-64

topfemjobs <- ftpums2015 %>% 
  filter(SEX == 2) %>% 
  to_survey(type = "person", 
            class = "srvyr", 
            design = "rep_weights") %>% 
  group_by(SOCP) %>%
  summarise(EMPCNT = survey_total(na.rm = TRUE),
            MN_INC = survey_mean(PINCP, na.rm = TRUE),
            MED_INC = survey_median(PINCP, 
                                    na.rm = TRUE),
            MED_WG = survey_median(PERNP, 
                                   na.rm = TRUE))%>% 
  arrange(desc(EMPCNT)) %>% 
  left_join(soccodes10uniq, by = c("SOCP" = "SOCCODE10")) %>% 
  slice(1:50)
# Note-- will be warnings generated; this is due to small numbers of observations for some occupations

# top jobs:  full-time, year-round employed mothers ages 18-64

topmomjobs <- ftpums2015 %>% 
  filter(MOM == 1) %>% 
  to_survey(type = "person", 
            class = "srvyr", 
            design = "rep_weights") %>% 
  group_by(SOCP) %>%
  summarise(EMPCNT = survey_total(na.rm = TRUE),
            MN_INC = survey_mean(PINCP, na.rm = TRUE),
            MED_INC = survey_median(PINCP, 
                                    na.rm = TRUE),
            MED_WG = survey_median(PERNP, 
                                   na.rm = TRUE))%>% 
  arrange(desc(EMPCNT)) %>% 
  left_join(soccodes10uniq, by = c("SOCP" = "SOCCODE10")) %>% 
  slice(1:50)
# Note-- will be warnings generated; this is due to small numbers of observations for some occupations

# top jobs:  full-time, year-round employed mothers of young children ages 18-64


topmomycjobs <- ftpums2015 %>% 
  filter(MOMYC == 1) %>% 
  to_survey(type = "person", 
            class = "srvyr", 
            design = "rep_weights") %>% 
  group_by(SOCP) %>%
  summarise(EMPCNT = survey_total(na.rm = TRUE),
            MN_INC = survey_mean(PINCP,na.rm = TRUE),
            MED_INC = survey_median(PINCP, 
                                    na.rm = TRUE),
            MED_WG = survey_median(PERNP, 
                                   na.rm = TRUE))%>% 
  arrange(desc(EMPCNT)) %>% 
  left_join(soccodes10uniq, by = c("SOCP" = "SOCCODE10")) %>% 
  slice(1:50)
# Note-- will be warnings generated; this is due to small numbers of observations for some occupations

# top jobs:  full-time, year-round employed single mothers ages 18-64

topsingmomjobs <- ftpums2015 %>% 
  filter(SMOM == 1) %>% 
  to_survey(type = "person", 
            class = "srvyr", 
            design = "rep_weights") %>% 
  group_by(SOCP) %>%
  summarise(EMPCNT = survey_total(na.rm = TRUE),
            MN_INC = survey_mean(PINCP, na.rm = TRUE),
            MED_INC = survey_median(PINCP, 
                                    na.rm = TRUE),
            MED_WG = survey_median(PERNP, 
                                   na.rm = TRUE))%>% 
  arrange(desc(EMPCNT)) %>% 
  left_join(soccodes10uniq, by = c("SOCP" = "SOCCODE10")) %>% 
  slice(1:50)
# Note-- will be warnings generated; this is due to small numbers of observations for some occupations


# top jobs:  full-time, year-round employed single mothers of young children ages 18-64

topsingmomycjobs<- ftpums2015 %>% 
  filter(SMOMYC == 1) %>% 
  to_survey(type = "person", 
            class = "srvyr", 
            design = "rep_weights") %>% 
  group_by(SOCP) %>%
  summarise(EMPCNT = survey_total(na.rm = TRUE),
            MN_INC = survey_mean(PINCP, na.rm = TRUE),
            MED_INC = survey_median(PINCP, 
                                    na.rm = TRUE),
            MED_WG = survey_median(PERNP, 
                                   na.rm = TRUE))%>% 
  arrange(desc(EMPCNT)) %>% 
  left_join(soccodes10uniq, by = c("SOCP" = "SOCCODE10")) %>% 
  slice(1:50)
# Note-- will be warnings generated; this is due to small numbers of observations for some occupations

#### Re-slice top jobs for workers without a 4-yr degree #####
# top jobs:  full-time, year-round employed adults ages 18-64
ftpums2015 <- pums2015 %>% 
  filter(WORKINGAGE == 1 & FTEMP == 1 & LESSBACH == 1) 

toplbjobs <- ftpums2015 %>% 
  to_survey(type = "person", 
            class = "srvyr", 
            design = "rep_weights") %>% 
  group_by(SOCP) %>% 
  summarise(EMPCNT = survey_total(na.rm = TRUE),
            MN_INC = survey_mean(PINCP,na.rm = TRUE),
            MED_INC = survey_median(PINCP, 
                                    na.rm = TRUE),
            MED_WG = survey_median(PERNP, 
                                   na.rm = TRUE)) %>% 
  arrange(desc(EMPCNT)) %>% 
  left_join(soccodes10uniq, by = c("SOCP" = "SOCCODE10")) %>%
  slice(1:50)

# top jobs:  full-time, year-round employed females ages 18-64

toplbfemjobs <- ftpums2015 %>% 
  filter(SEX == 2) %>% 
  to_survey(type = "person", 
            class = "srvyr", 
            design = "rep_weights") %>% 
  group_by(SOCP) %>%
  summarise(EMPCNT = survey_total(na.rm = TRUE),
            MN_INC = survey_mean(PINCP, na.rm = TRUE),
            MED_INC = survey_median(PINCP, 
                                    na.rm = TRUE),
            MED_WG = survey_median(PERNP, 
                                   na.rm = TRUE))%>% 
  arrange(desc(EMPCNT)) %>% 
  left_join(soccodes10uniq, by = c("SOCP" = "SOCCODE10")) %>% 
  slice(1:50)
# Note-- will be warnings generated; this is due to small numbers of observations for some occupations

# top jobs:  full-time, year-round employed mothers ages 18-64

toplbmomjobs <- ftpums2015 %>% 
  filter(MOM == 1) %>% 
  to_survey(type = "person", 
            class = "srvyr", 
            design = "rep_weights") %>% 
  group_by(SOCP) %>%
  summarise(EMPCNT = survey_total(na.rm = TRUE),
            MN_INC = survey_mean(PINCP, na.rm = TRUE),
            MED_INC = survey_median(PINCP, 
                                    na.rm = TRUE),
            MED_WG = survey_median(PERNP, 
                                   na.rm = TRUE))%>% 
  arrange(desc(EMPCNT)) %>% 
  left_join(soccodes10uniq, by = c("SOCP" = "SOCCODE10")) %>%
  slice(1:50)
# Note-- will be warnings generated; this is due to small numbers of observations for some occupations

# top jobs:  full-time, year-round employed mothers of young children ages 18-64


toplbmomycjobs <- ftpums2015 %>% 
  filter(MOMYC == 1) %>% 
  to_survey(type = "person", 
            class = "srvyr", 
            design = "rep_weights") %>% 
  group_by(SOCP) %>%
  summarise(EMPCNT = survey_total(na.rm = TRUE),
            MN_INC = survey_mean(PINCP,na.rm = TRUE),
            MED_INC = survey_median(PINCP, 
                                    na.rm = TRUE),
            MED_WG = survey_median(PERNP, 
                                   na.rm = TRUE))%>% 
  arrange(desc(EMPCNT)) %>% 
  left_join(soccodes10uniq, by = c("SOCP" = "SOCCODE10")) %>% 
  slice(1:50)
# Note-- will be warnings generated; this is due to small numbers of observations for some occupations

# top jobs:  full-time, year-round employed single mothers ages 18-64

toplbsingmomjobs <- ftpums2015 %>% 
  filter(SMOM == 1) %>% 
  to_survey(type = "person", 
            class = "srvyr", 
            design = "rep_weights") %>% 
  group_by(SOCP) %>%
  summarise(EMPCNT = survey_total(na.rm = TRUE),
            MN_INC = survey_mean(PINCP, na.rm = TRUE),
            MED_INC = survey_median(PINCP, 
                                    na.rm = TRUE),
            MED_WG = survey_median(PERNP, 
                                   na.rm = TRUE),
  )%>% 
  arrange(desc(EMPCNT)) %>% 
  left_join(soccodes10uniq, by = c("SOCP" = "SOCCODE10")) %>%
  slice(1:50)
# Note-- will be warnings generated; this is due to small numbers of observations for some occupations


# top jobs:  full-time, year-round employed single mothers of young children ages 18-64

toplbsingmomycjobs<- ftpums2015 %>% 
  filter(SMOMYC == 1) %>% 
  to_survey(type = "person", 
            class = "srvyr", 
            design = "rep_weights") %>% 
  group_by(SOCP) %>%
  summarise(EMPCNT = survey_total(na.rm = TRUE),
            MN_INC = survey_mean(PINCP, na.rm = TRUE),
            MED_INC = survey_median(PINCP, 
                                    na.rm = TRUE),
            MED_WG = survey_median(PERNP, 
                                   na.rm = TRUE))%>% 
  arrange(desc(EMPCNT)) %>% 
  left_join(soccodes10uniq, by = c("SOCP" = "SOCCODE10")) %>%
  slice(1:50)
# Note-- will be warnings generated; this is due to small numbers of observations for some occupations

##### Educational attainment for FT working women #####
ftpums2015 <- pums2015 %>% 
  filter(WORKINGAGE == 1 & FTEMP == 1) 

edatttab <- ftpums2015 %>% 
  to_survey(type = "person", 
            class = "srvyr", 
            design = "rep_weights") %>% 
  group_by(EDATT_RC) %>% 
  summarize(TOTCNT = survey_total(na.rm = TRUE),
            WFEMCNT = survey_total(FEM, na.rm = TRUE),
            WMOMCNT = survey_total(MOM, na.rm = TRUE),
            WMOMYCCNT = survey_total(MOMYC, na.rm = TRUE),
            WSMOMCNT = survey_total(SMOM,na.rm = TRUE),
            WSMOMYCCNT = survey_total(SMOMYC, na.rm = TRUE)) %>% 
  mutate(pTot = TOTCNT/sum(TOTCNT),
         pWFEM = WFEMCNT/sum(WFEMCNT),
         pWMOM = WMOMCNT/sum(WMOMCNT),
         pWMOMYC = WMOMYCCNT/sum(WMOMYCCNT),
         pWSMOM = WSMOMCNT/sum(WSMOMCNT),
         pWSMOMYC = WSMOMYCCNT/sum(WSMOMYCCNT))

##### Get FT/PT work breakdown for top jobs for women, moms, single moms without 4-year degree #####

alltopjobs <- topjobs %>% 
  bind_rows(topfemjobs, 
            topmomjobs, 
            topmomycjobs,
            topsingmomjobs,
            topsingmomycjobs,
            toplbjobs,
            toplbfemjobs,
            toplbmomjobs,
            toplbmomycjobs,
            toplbsingmomjobs,
            toplbsingmomycjobs) %>%
  select(SOCP) %>% 
  distinct(SOCP)

ftpttab<- pums2015 %>% 
  filter(ESR == 1 & WORKINGAGE == 1) %>% 
  filter(SOCP %in% alltopjobs$SOCP) %>% 
  to_survey(type = "person", 
            class = "srvyr", 
            design = "rep_weights") %>% 
  group_by(SOCP) %>% 
  summarize(TOTCNT = survey_total(na.rm = TRUE),
            FTCNT = survey_total(FTEMP, na.rm = TRUE),
            PTCNT = survey_total(PTEMP, na.rm = TRUE)) %>% 
  mutate(pFT = FTCNT/TOTCNT,
         pPT = PTCNT/TOTCNT) %>% 
  left_join(soccodes10uniq, by = c("SOCP" = "SOCCODE10"))

ftptfemtab<- pums2015 %>% 
  filter(ESR == 1 & WORKINGAGE == 1 & SEX == 2) %>% 
  filter(SOCP %in% alltopjobs$SOCP) %>% 
  to_survey(type = "person", 
            class = "srvyr", 
            design = "rep_weights") %>% 
  group_by(SOCP) %>% 
  summarize(TOTCNT = survey_total(),
            FTCNT = survey_total(FTEMP),
            PTCNT = survey_total(PTEMP)) %>% 
  mutate(pFT = FTCNT/TOTCNT,
         pPT = PTCNT/TOTCNT) %>% 
  left_join(soccodes10uniq, by = c("SOCP" = "SOCCODE10"))

#### Create a women's top job wide table (see alt format in the outputs section) ####
ftpums2015 <- pums2015 %>% 
  filter(WORKINGAGE == 1 & FTEMP == 1) 

topfemjobs_wide<- ftpums2015 %>% 
  filter(SOCP %in% alltopjobs$SOCP) %>% 
  to_survey(type = "person", 
            class = "srvyr", 
            design = "rep_weights") %>% 
  group_by(SOCP) %>% 
  summarize(TOTCNT = survey_total(na.rm = TRUE),
            FEMCNT = survey_total(FEM, na.rm = TRUE),
            MALCNT = survey_total(MAL, na.rm = TRUE),
            MOMCNT = survey_total(MOM, na.rm = TRUE),
            MOMYCCNT = survey_total(MOMYC, na.rm = TRUE),
            SMOMCNT = survey_total(SMOM, na.rm = TRUE),
            SMOMYCCNT = survey_total(SMOMYC, na.rm = TRUE),
            LBACHCNT = survey_total(LESSBACH, na.rm = TRUE),
            BACHMCNT = survey_total(BACHMORE, na.rm = TRUE)) %>% 
  mutate(pFem = FEMCNT/TOTCNT,
         pMom = MOMCNT/TOTCNT,
         pMomYC = MOMYCCNT/TOTCNT,
         pSMomYC = SMOMYCCNT/TOTCNT) %>% 
  left_join(soccodes10uniq, by = c("SOCP" = "SOCCODE10"))


mommedinctopjobs<- ftpums2015 %>% 
  filter(SOCP %in% alltopjobs$SOCP) %>% 
  filter(MOM == 1) %>% 
  to_survey(type = "person", 
            class = "srvyr", 
            design = "rep_weights") %>% 
  group_by(SOCP) %>%
  summarize(EMPCNT = survey_total(na.rm = TRUE),
            MN_INC = survey_mean(PINCP,na.rm = TRUE),
            MED_INC = survey_median(PINCP, 
                                    na.rm = TRUE),
            MED_WG = survey_median(PERNP, 
                                   na.rm = TRUE)) %>%
  left_join(soccodes10uniq, by = c("SOCP" = "SOCCODE10")) %>% 
  rename_with(~paste0(., "_mom"), -c("SOCP", "OCC_TITLE"))

momycmedinctopjobs<- ftpums2015 %>% 
  filter(SOCP %in% alltopjobs$SOCP) %>% 
  filter(MOMYC == 1) %>% 
  to_survey(type = "person", 
            class = "srvyr", 
            design = "rep_weights") %>% 
  group_by(SOCP) %>%
  summarize(EMPCNT = survey_total(na.rm = TRUE),
            MN_INC = survey_mean(PINCP,na.rm = TRUE),
            MED_INC = survey_median(PINCP, 
                                    na.rm = TRUE),
            MED_WG = survey_median(PERNP, 
                                   na.rm = TRUE)) %>%
  left_join(soccodes10uniq, by = c("SOCP" = "SOCCODE10")) %>% 
  rename_with(~paste0(., "_momyc"), -c("SOCP", "OCC_TITLE"))

smomycmedinctopjobs<- ftpums2015 %>% 
  filter(SOCP %in% alltopjobs$SOCP) %>% 
  filter(SMOMYC == 1) %>% 
  to_survey(type = "person", 
            class = "srvyr", 
            design = "rep_weights") %>% 
  group_by(SOCP) %>%
  summarize(EMPCNT = survey_total(na.rm = TRUE),
            MN_INC = survey_mean(PINCP,na.rm = TRUE),
            MED_INC = survey_median(PINCP, 
                                    na.rm = TRUE),
            MED_WG = survey_median(PERNP, 
                                   na.rm = TRUE)) %>%
  left_join(soccodes10uniq, by = c("SOCP" = "SOCCODE10")) %>% 
  rename_with(~paste0(., "_smomyc"), -c("SOCP", "OCC_TITLE"))

topfemjobs_wide<-topfemjobs_wide %>% 
  left_join(mommedinctopjobs, by = c("SOCP", "OCC_TITLE")) %>% 
  left_join(momycmedinctopjobs, by = c("SOCP", "OCC_TITLE")) %>% 
  left_join(smomycmedinctopjobs, by = c("SOCP", "OCC_TITLE")) %>%
  write_csv("clean-data/top_women_jobs_wide_v2.csv")


momyc_childcare<- ftpums2015 %>% 
  filter(SOCP %in% alltopjobs$SOCP) %>% 
  filter(MOMYC == 1 & SOCP == 399011)   

###### Get percent female, income for men, women, and all for top jobs #####
ftpums2015 <- pums2015 %>% 
  filter(WORKINGAGE == 1 & FTEMP == 1) 

pfemfttab<- ftpums2015 %>% 
  filter(SOCP %in% alltopjobs$SOCP) %>% 
  to_survey(type = "person", 
            class = "srvyr", 
            design = "rep_weights") %>% 
  group_by(SOCP) %>%
  summarize(EMPCNT = survey_total(),
            FEMCNT = survey_total(FEM),
            MALCNT = survey_total(MAL),
            MN_INC = survey_mean(PINCP,na.rm = TRUE),
            MED_INC = survey_median(PINCP, 
                                    na.rm = TRUE),
            MED_WG = survey_median(PERNP, 
                                   na.rm = TRUE)) %>%
  mutate(pFEM = FEMCNT/EMPCNT,
         pMAL = MALCNT/EMPCNT) %>% 
  left_join(soccodes10uniq, by = c("SOCP" = "SOCCODE10"))

fmedinctopjobstab<- ftpums2015 %>% 
  filter(SOCP %in% alltopjobs$SOCP) %>% 
  filter(SEX == 2) %>% 
  to_survey(type = "person", 
            class = "srvyr", 
            design = "rep_weights") %>% 
  group_by(SOCP) %>%
  summarize(EMPCNT = survey_total(na.rm = TRUE),
            MN_INC = survey_mean(PINCP,na.rm = TRUE),
            MED_INC = survey_median(PINCP, 
                                    na.rm = TRUE),
            MED_WG = survey_median(PERNP, 
                                   na.rm = TRUE)) %>%
  left_join(soccodes10uniq, by = c("SOCP" = "SOCCODE10"))

ftpums2015 <- pums2015 %>% 
  filter(WORKINGAGE == 1 & FTEMP == 1) 

mmedinctopjobstab<- ftpums2015 %>% 
  filter(SOCP %in% alltopjobs$SOCP) %>% 
  filter(SEX == 1) %>% 
  to_survey(type = "person", 
            class = "srvyr", 
            design = "rep_weights") %>% 
  group_by(SOCP) %>%
  summarize(EMPCNT = survey_total(na.rm = TRUE),
            MN_INC = survey_mean(PINCP,na.rm = TRUE),
            MED_INC = survey_median(PINCP, 
                                    na.rm = TRUE),
            MED_WG = survey_median(PERNP, 
                                   na.rm = TRUE)) %>%
  left_join(soccodes10uniq, by = c("SOCP" = "SOCCODE10"))

## Repeat for workers sans bachelors degree

ftpums2015 <- pums2015 %>% 
  filter(WORKINGAGE == 1 & FTEMP == 1 & LESSBACH == 1) 

pfemftLBtab<- ftpums2015 %>% 
  filter(SOCP %in% alltopjobs$SOCP) %>% 
  to_survey(type = "person", 
            class = "srvyr", 
            design = "rep_weights") %>% 
  group_by(SOCP) %>%
  summarize(EMPCNT = survey_total(),
            FEMCNT = survey_total(FEM),
            MALCNT = survey_total(MAL),
            MN_INC = survey_mean(PINCP,na.rm = TRUE),
            MED_INC = survey_median(PINCP, 
                                    na.rm = TRUE),
            MED_WG = survey_median(PERNP, 
                                   na.rm = TRUE)) %>%
  mutate(pFEM = FEMCNT/EMPCNT,
         pMAL = MALCNT/EMPCNT) %>% 
  left_join(soccodes10uniq, by = c("SOCP" = "SOCCODE10"))

fmedinctopjobsLBtab<- ftpums2015 %>% 
  filter(SOCP %in% alltopjobs$SOCP) %>% 
  filter(SEX == 2) %>% 
  to_survey(type = "person", 
            class = "srvyr", 
            design = "rep_weights") %>% 
  group_by(SOCP) %>%
  summarize(EMPCNT = survey_total(na.rm = TRUE),
            MN_INC = survey_mean(PINCP,na.rm = TRUE),
            MED_INC = survey_median(PINCP, 
                                    na.rm = TRUE),
            MED_WG = survey_median(PERNP, 
                                   na.rm = TRUE)) %>%
  left_join(soccodes10uniq, by = c("SOCP" = "SOCCODE10"))

ftpums2015 <- pums2015 %>% 
  filter(WORKINGAGE == 1 & FTEMP == 1 & LESSBACH == 1) 

mmedinctopjobsLBtab<- ftpums2015 %>% 
  filter(SOCP %in% alltopjobs$SOCP) %>% 
  filter(SEX == 1) %>% 
  to_survey(type = "person", 
            class = "srvyr", 
            design = "rep_weights") %>% 
  group_by(SOCP) %>%
  summarize(EMPCNT = survey_total(na.rm = TRUE),
            MN_INC = survey_mean(PINCP,na.rm = TRUE),
            MED_INC = survey_median(PINCP, 
                                    na.rm = TRUE),
            MED_WG = survey_median(PERNP, 
                                   na.rm = TRUE)) %>%
  left_join(soccodes10uniq, by = c("SOCP" = "SOCCODE10"))

#### Get employment, education, & % female stats for promising occupations ####
promocctab <- pums2015 %>% 
  filter(SOCP %in% azpromocc21$SOCCODE10) %>% 
  to_survey(type = "person", 
            class = "srvyr", 
            design = "rep_weights") %>%  
  group_by(SOCP) %>% 
  summarize(TOTCNT = survey_total(na.rm = TRUE),
            FTCNT = survey_total(FTEMP, na.rm = TRUE),
            PTCNT = survey_total(PTEMP, na.rm = TRUE),
            FEMCNT = survey_total(FEM, na.rm = TRUE),
            MALCNT = survey_total(MAL, na.rm = TRUE),
            MOMCNT = survey_total(MOM, na.rm = TRUE),
            MOMYCCNT = survey_total(MOMYC, na.rm = TRUE),
            SMOMCNT = survey_total(SMOM, na.rm = TRUE),
            SMOMYCCNT = survey_total(SMOMYC, na.rm = TRUE),
            LBACHCNT = survey_total(LESSBACH, na.rm = TRUE),
            BACHMCNT = survey_total(BACHMORE, na.rm = TRUE)) %>% 
  mutate(pFT = FTCNT/TOTCNT,
         pPT = PTCNT/TOTCNT,
         pFEM = FEMCNT/TOTCNT,
         pMAL = MALCNT/TOTCNT,
         pMOM = MOMCNT/TOTCNT,
         pMOMYC = MOMYCCNT/TOTCNT,
         pSMOM = SMOMCNT/TOTCNT,
         pSMOMYC = SMOMYCCNT/TOTCNT,
         pLBACH = LBACHCNT/TOTCNT,
         pBACHM = BACHMCNT/TOTCNT) %>% 
  left_join(soccodes10uniq, by = c("SOCP" = "SOCCODE10"))

ftpums2015 <- pums2015 %>% 
  filter(WORKINGAGE == 1 & FTEMP == 1) 

promoccFTtab <- ftpums2015 %>% 
  filter(SOCP %in% azpromocc21$SOCCODE10) %>% 
  to_survey(type = "person", 
            class = "srvyr", 
            design = "rep_weights") %>%  
  group_by(SOCP) %>% 
  summarize(TOTCNT = survey_total(na.rm = TRUE),
            FTCNT = survey_total(FTEMP, na.rm = TRUE),
            PTCNT = survey_total(PTEMP, na.rm = TRUE),
            FEMCNT = survey_total(FEM, na.rm = TRUE),
            MALCNT = survey_total(MAL, na.rm = TRUE),
            MOMCNT = survey_total(MOM, na.rm = TRUE),
            MOMYCCNT = survey_total(MOMYC, na.rm = TRUE),
            SMOMCNT = survey_total(SMOM, na.rm = TRUE),
            SMOMYCCNT = survey_total(SMOMYC, na.rm = TRUE),
            LBACHCNT = survey_total(LESSBACH, na.rm = TRUE),
            BACHMCNT = survey_total(BACHMORE, na.rm = TRUE),
            MED_INC = survey_median(PINCP, na.rm = TRUE),
            MED_WG = survey_median(PERNP, na.rm = TRUE)) %>% 
  mutate(pFT = FTCNT/TOTCNT,
         pPT = PTCNT/TOTCNT,
         pFEM = FEMCNT/TOTCNT,
         pMAL = MALCNT/TOTCNT,
         pMOM = MOMCNT/TOTCNT,
         pMOMYC = MOMYCCNT/TOTCNT,
         pSMOM = SMOMCNT/TOTCNT,
         pSMOMYC = SMOMYCCNT/TOTCNT,
         pLBACH = LBACHCNT/TOTCNT,
         pBACHM = BACHMCNT/TOTCNT) %>% 
  left_join(soccodes10uniq, by = c("SOCP" = "SOCCODE10"))

### Get female and male median wages for promising occupations
ftpums2015 <- pums2015 %>% 
  filter(WORKINGAGE == 1 & FTEMP == 1)

fmedincpromocctab<- ftpums2015 %>% 
  filter(SOCP %in% azpromocc21$SOCCODE10 & SEX == 2) %>% 
  to_survey(type = "person", 
            class = "srvyr", 
            design = "rep_weights") %>% 
  group_by(SOCP) %>%
  summarize(EMPCNT = survey_total(na.rm = TRUE),
            MN_INC = survey_mean(PINCP,na.rm = TRUE),
            MED_INC = survey_median(PINCP, 
                                    na.rm = TRUE),
            MED_WG = survey_median(PERNP, 
                                   na.rm = TRUE)) %>%
  left_join(soccodes10uniq, by = c("SOCP" = "SOCCODE10"))

ftpums2015 <- pums2015 %>% 
  filter(WORKINGAGE == 1 & FTEMP == 1) 

mmedincpromocctab<- ftpums2015 %>% 
  filter(SOCP %in% azpromocc21$SOCCODE10 & SEX == 1) %>% 
  to_survey(type = "person", 
            class = "srvyr", 
            design = "rep_weights") %>% 
  group_by(SOCP) %>%
  summarize(EMPCNT = survey_total(na.rm = TRUE),
            MN_INC = survey_mean(PINCP,na.rm = TRUE),
            MED_INC = survey_median(PINCP, 
                                    na.rm = TRUE),
            MED_WG = survey_median(PERNP, 
                                   na.rm = TRUE)) %>%
  left_join(soccodes10uniq, by = c("SOCP" = "SOCCODE10"))


# Check to make sure that there aren't missing OCC codes
# '%!in%' <- Negate('%in%')
# p1<-unique(azpromocc21$SOCCODE18)
# p2<-unique(promocctab$SOCP)
# p3<- p1[p1 %!in% p2 == TRUE]

#### Outputs for 2015 data: #####

# Pop tab with all the denominators
pums_poptab<- pums_poptab %>% 
  write_csv("clean-data/pums15_poptab.csv")

# Table of top occupations for women with gender breakdowns and median earnings (workers, men, women)

### merge together pfemfttab, fmedinctopjobs, mmedinctopjobs
fmedinctopjobstab<- fmedinctopjobstab %>% 
  rename_with(~paste0(., "_fem"), -c("SOCP", "OCC_TITLE"))

mmedinctopjobstab<- mmedinctopjobstab %>% 
  rename_with(~paste0(., "_ml"), -c("SOCP", "OCC_TITLE"))

pfem_medinc_jobs<- pfemfttab %>% 
  left_join(fmedinctopjobstab, by = c("SOCP", "OCC_TITLE")) %>% 
  left_join(mmedinctopjobstab, by = c("SOCP", "OCC_TITLE")) %>% 
  write_csv("clean-data/sex_medinc_top_jobs2015.csv")

#repeat for the less than bachelors crowd

fmedinctopjobsLBtab<- fmedinctopjobsLBtab %>% 
  rename_with(~paste0(., "_fem"), -c("SOCP", "OCC_TITLE"))

mmedinctopjobsLBtab<- mmedinctopjobsLBtab %>% 
  rename_with(~paste0(., "_ml"), -c("SOCP", "OCC_TITLE"))

pfem_medinc_LBjobs<- pfemftLBtab %>% 
  left_join(mmedinctopjobsLBtab, by = c("SOCP", "OCC_TITLE")) %>% 
  left_join(fmedinctopjobsLBtab, by = c("SOCP", "OCC_TITLE")) %>% 
  write_csv("clean-data/sex_medinc_top_jobsLB2015.csv")


# Merged long table of top occupations for mothers, mothers of young children, & single mothers of young children with counts, median earnings

alltopjobslong <- topjobs %>% 
  bind_rows(topfemjobs, 
            topmomjobs, 
            topmomycjobs,
            topsingmomjobs,
            topsingmomycjobs,
            toplbjobs,
            toplbfemjobs,
            toplbmomjobs,
            toplbmomycjobs,
            toplbsingmomjobs,
            toplbsingmomycjobs, .id = "category") %>% 
  mutate(category = recode(category, "1" = "all",
                           "2" = "female", 
                           "3" = "mom", 
                           "4" = "mom young children",
                           "5" = "single mom",
                           "6" = "single mom young children",
                           "7" = "all less bach",
                           "8" = "female less bach",
                           "9" = "mom less bach",
                           "10" = "mom young children lb",
                           "11" = "single mom lb",
                           "12" = "single mom young children lb")) %>% 
  write_csv("clean-data/top_women_jobs_long2015.csv")

## Merged wide table of top jobs for women
# Ideally this would have been a for loop and using a purr:reduce to merge all the tables together
# but given a very impending deadline, currently just the fast and working copy/paste

topfemjobs<-topfemjobs %>% 
  rename_with(~paste0(., "_fem"), -c("SOCP", "OCC_TITLE"))

topmomjobs<-topmomjobs %>% 
  rename_with(~paste0(., "_mom"), -c("SOCP", "OCC_TITLE"))

topmomycjobs<-topmomycjobs %>% 
  rename_with(~paste0(., "_momyc"), -c("SOCP", "OCC_TITLE"))

topsingmomjobs<-topsingmomjobs %>% 
  rename_with(~paste0(., "_smom"), -c("SOCP", "OCC_TITLE"))

topsingmomycjobs<-topsingmomycjobs %>% 
  rename_with(~paste0(., "_smomyc"), -c("SOCP", "OCC_TITLE"))

toplbjobs<-toplbjobs %>% 
  rename_with(~paste0(., "_lb"), -c("SOCP", "OCC_TITLE"))

toplbfemjobs<-toplbfemjobs %>% 
  rename_with(~paste0(., "_lbfem"), -c("SOCP", "OCC_TITLE"))

toplbmomjobs<-toplbmomjobs %>% 
  rename_with(~paste0(., "_lbmom"), -c("SOCP", "OCC_TITLE"))

toplbmomycjobs<-toplbmomycjobs %>% 
  rename_with(~paste0(., "_lbmomyc"), -c("SOCP", "OCC_TITLE"))

toplbsingmomjobs<-toplbsingmomjobs %>% 
  rename_with(~paste0(., "_lbsingmom"), -c("SOCP", "OCC_TITLE"))

toplbsingmomycjobs<-toplbsingmomycjobs %>% 
  rename_with(~paste0(., "_lbsingmomyc"), -c("SOCP", "OCC_TITLE"))

alltopjobswide<- topjobs %>% 
  left_join(topfemjobs, by = c("SOCP", "OCC_TITLE")) %>% 
  left_join(topmomjobs, by = c("SOCP", "OCC_TITLE")) %>%
  left_join(topmomycjobs, by = c("SOCP", "OCC_TITLE")) %>%
  left_join(topsingmomjobs, by = c("SOCP", "OCC_TITLE")) %>%
  left_join(topsingmomycjobs, by = c("SOCP", "OCC_TITLE")) %>%
  left_join(toplbjobs, by = c("SOCP", "OCC_TITLE")) %>%
  left_join(toplbmomjobs, by = c("SOCP", "OCC_TITLE")) %>%
  left_join(toplbmomycjobs, by = c("SOCP", "OCC_TITLE")) %>%
  left_join(toplbmomycjobs, by = c("SOCP", "OCC_TITLE")) %>%
  left_join(toplbsingmomjobs, by = c("SOCP", "OCC_TITLE")) %>%
  left_join(toplbsingmomycjobs, by = c("SOCP", "OCC_TITLE")) %>%
  write_csv("clean-data/top_women_jobs_wide2015.csv")

# Educational Attainment table
#### write ed att table
edatttab %>% write_csv("clean-data/ed_attainment_women2015.csv")

# Table with FT/PT work split for top occupation for women/mothers without a bachelor's degree
### merge ftpttab and ftptfemtab


ftptfemtab<- ftptfemtab %>% 
  rename_with(~paste0(., "_fem"), -c("SOCP", "OCC_TITLE"))

ftptsplittab<-ftpttab %>% 
  left_join(ftptfemtab, by = c("SOCP", "OCC_TITLE")) %>% 
  write_csv("clean-data/ftptsplit_top_jobs2015.csv")


# Promising occupation table with % with bachelors degree or higher, employment counts by gender, and median earnings added
### merge together promocctab, promoccfttab


promoccFTtab<- promoccFTtab %>% 
  rename_with(~paste0(., "_ft"), -c("SOCP", "OCC_TITLE"))

fmedincpromocctab<- fmedincpromocctab %>% 
  rename_with(~paste0(., "_femft"), -c("SOCP", "OCC_TITLE"))

mmedincpromocctab<- mmedincpromocctab %>% 
  rename_with(~paste0(., "_mlft"), -c("SOCP", "OCC_TITLE"))

promocc_table<- promocctab %>% 
  left_join(promoccFTtab, by = c("SOCP", "OCC_TITLE")) %>% 
  left_join(fmedincpromocctab, by = c("SOCP", "OCC_TITLE")) %>% 
  left_join(mmedincpromocctab, by = c("SOCP", "OCC_TITLE")) %>% 
  write_csv("clean-data/promising_occupations_tab2015.csv")



# Promising occupation table with BLS data joined back in
promocc_table_pumsbls<-promocc_table %>% 
  full_join(azpromocc21data, by = c("SOCP" = "SOCCODE10")) %>% 
  write_csv("clean-data/PUMSBLS_promising_occupations_tab2015.csv")




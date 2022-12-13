## Manipulate BLS data to create Table 1 from White Paper
## Kara Haberstock Tanoue
## 10.06.2022

## Libraries
library(tidyverse)
library(stringr)
library(purrr)
library(forcats)


##### Read in cleaned bls data #####
azcty_proj <- read_csv("clean-data/azcty_proj_sept21.csv")

str(azcty_proj)

#### Get Ranking by # of total openings between 2020 and 2030 ####
azcty_proj_ranked <- azcty_proj %>%
  filter(!grepl("-0000", OCC_CODE)) %>% #filter out the grouped occupations (SOC codes ending in -0000)
  arrange(area, desc(total_openings)) %>% # sort by area, then by total openings in descending order
  group_by(area) %>% # group by area
  mutate(openings_rank = rank(-total_openings)) %>% #generate a ranking variable
  filter(openings_rank <= 40) #filter to top 40 for each region



#### Restructure the data into table 1 ####
# To create table 1, we want to unfurl all the ranks, areas, and occupations, then put them back together
azcty_opnrank<- azcty_proj_ranked %>% 
  select(area, OCC_CODE, OCC_TITLEAZ, openings_rank) %>% 
  pivot_wider(names_from = area,
              values_from = openings_rank) %>% 
  write_csv("clean-data/occupation_rank_top40openings.csv")



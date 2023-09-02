# 0_read_stats_sections_extra.R
# read statistical methods section from ANZCTR, copied from 0_read_stats_sections.R
# additional sections from recent years
# handy document: https://www.anzctr.org.au/docs/ANZCTR%20Data%20field%20explanation.pdf
# data downloaded from https://www.anzctr.org.au/TrialSearch.aspx using empty search, then "download all to XML" button 
# September 2023
library(readxl) # Not in XML format
library(stringr)
library(janitor)
library(dplyr)
censor.date = as.Date('2022-10-18') # date that these data were downloaded by me from ANZCTR

# get the data from Excel
raw = read_excel(path = 'data/download_anzctr.xlsx', sheet='TRIAL') %>%
  clean_names() %>%
  select(trial_id, submit_date, study_type, assignment, target_sample_size, statistical_methods) %>%
  filter(!is.na(statistical_methods))

# small fixes
studies = mutate(raw, 
                 submit_date = as.Date(submit_date, origin='1970-01-01'),
                 target_sample_size = as.numeric(target_sample_size)) %>% 
  rename('number' = 'trial_id', # to match
         'submitdate' = 'submit_date',
         'stats_section' = 'statistical_methods',
         'samplesize_target' = 'target_sample_size')

# check for duplicates, should be none
table(duplicated(studies$number))

# save
save(studies, censor.date, file='data/0_Stats_Sections_Extra.RData')

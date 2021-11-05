# 1_find_sample_size.R
# find stats sections that mention sample size
# November 2021
library(dplyr)
library(stringr)
library(textclean)

# get non-empty stats sections from 0_read_stats_sections.R
load('data/Stats_Sections.RData')
studies = mutate(studies,
                 stats_section = tolower(stats_section), # transform to lower case
                 stats_section = replace_non_ascii(stats_section)) %>% # remove non-ASCII
  filter(!is.na(stats_section)) # catch any empty
N = nrow(studies)

## key-words for sample size calculations
# beta must be whole word to avoid running into other words
# removed '0\\.05' as it just flagged p-value statements
keywords = c('power','sample size','alpha','\\bbeta\\b','type.?i','type.?ii','type.?1','type.?2','type.?one','type.?two')
keywords = paste(keywords, collapse='|')
# key words for more complex sample size
complex = c('longitudinal', 'repeated measure', 'spatial')
complex = paste(complex, collapse='|')

# big loop
update = NULL
for (k in 1:N){
  
  # get the study details
  this_study = studies[k,]

  # is there a sample size?
  any = str_detect(string = this_study$stats_section, pattern = keywords)
  if(any == FALSE){
    eframe = data.frame(number = this_study$number, reason='No sample size in stats methods')
    excluded = bind_rows(excluded, eframe)
    next # skip to next study
  }
  
  # extract the paragraph(s) with the sample size
  text = this_study$stats_section
  list = str_split(text, pattern='\\n') # split into paragraphs
  index = str_detect(list[[1]], pattern=keywords)
  single_paragraph = paste(list[[1]][index], paste = ' ')
  stats_section = str_squish(single_paragraph)
  # remove commas from large numbers:
  stats_section = gsub(",(?=\\d{3,})", "", stats_section, perl = TRUE) # strip commas in numbers
  
  # any mention of complex sample sizes?
  any_complex = str_detect(string = stats_section, pattern = complex)
  
  ## store data 
  frame = select(this_study, -stats_section) %>%
    mutate(stats_section = stats_section,
           complex = any_complex)
  update = bind_rows(update, frame)
  frame =  NULL # tidy up

}

# data edits
studies = mutate(update, # rename
     ID = 1:n()) # simple ID per row

# save
save(studies, excluded, censor.date, file='data/Stats_Sections_Processed.RData')


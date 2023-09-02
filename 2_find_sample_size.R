# 2_find_sample_size.R
# find stats sections that mention sample size
# September 2023
library(dplyr)
library(stringr)

# which data to use
data_sources = c('original','extra')
data_source = data_sources[2]
source('1_which_data.R')

# get non-empty and processed stats sections from 1_process_anzctr_stats_section.R
studies = readRDS(rds_file) 
N = nrow(studies)

## key-words for sample size calculations
# beta must be whole word to avoid running into other words (also power and alpha)
# removed '0\\.05' as it just flagged p-value statements
keywords = c('\\bpower\\b','sample.size','\\balpha\\b','\\bbeta\\b','type.?i','type.?ii','type.?1','type.?2','type.?one','type.?two')
keywords = paste(keywords, collapse='|')
# key words for more complex study designs for the sample size calculations
complex = c('longitudinal', 'repeated.measures?', 'spatial', 'simulation', 'equivalence', 'non.?inferiority', 'cluster', 'stepped.wedge', 'intra.?class.correlation')
complex = paste(complex, collapse='|')
# key words for pilot study
pilot = c('pilot', 'feasibility') # exploratory?
pilot = paste(pilot, ' study', sep='')
pilot = paste(pilot, collapse='|')

# big loop
update = excluded = NULL
for (k in 1:N){
  
  # get one study at a time
  this_study = studies[k,]

  # is there a sample size?
  any = str_detect(string = this_study$text_data_clean, pattern = keywords)
  if(any == FALSE){ 
    # add number of words, suspect that many will be very small
    eframe = data.frame(number = this_study$number, words = this_study$words, reason='No sample size in stats methods')
    excluded = bind_rows(excluded, eframe)
    next # skip to next study
  }
  
  # extract the paragraph(s) with the sample size
  text = this_study$text_data_clean
  list = str_split(text, pattern='\\n') # split into paragraphs
  index = str_detect(list[[1]], pattern=keywords)
  single_paragraph = paste(list[[1]][index], paste = ' ')
  stats_section = str_squish(single_paragraph)
  
  # any mention of sample sizes based on more complex study designs?
  any_complex = str_detect(string = stats_section, pattern = complex)
  
  # any mention of pilot 
  any_pilot = str_detect(string = stats_section, pattern = pilot)
  
  ## store data 
  frame = select(this_study, -starts_with('text_data_')) %>%
    mutate(stats_section = stats_section,
           complex = any_complex,
           pilot = any_pilot)
  update = bind_rows(update, frame)
  frame =  NULL # tidy up

}

# data edits
studies = mutate(update, # rename
     ID = 1:n()) # simple ID per row

# save
save(studies, excluded, file = file_processed)

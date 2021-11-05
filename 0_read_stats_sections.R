# 0_read_stats_sections.R
# read statistical methods section from ANZCTR, adapted from stats_section repository
# handy document: https://www.anzctr.org.au/docs/ANZCTR%20Data%20field%20explanation.pdf
# data downloaded from https://www.anzctr.org.au/TrialSearch.aspx using empty search, then "download all to XML" button 
# November 2021
library(XML)
library(stringr)
library(dplyr)
#source('99_functions.R')
censor.date = as.Date('2020-02-01') # date that these data were downloaded by me from ANZCTR

# list of files, one file per study
folder = "U:/Research/Projects/ihbi/aushsi/aushsi_barnetta/meta.research/ANZCTR/data/anzdata/" # use existing data for now
list.of.files = dir(folder, pattern='.xml')
N = length(list.of.files) # number of files

# big loop
studies = excluded = NULL
for (k in 1:N){ # loop through files
  data <- xmlParse(paste(folder, list.of.files[k], sep=''))
  xml_data <- xmlToList(data)
  
  ## trial number and submitted date
  number = xml_data$actrnumber
  if(is.null(number) == TRUE) { 
    number = xml_data$nctid # use secondary ID
  } 
  submitdate = ifelse(is.null(xml_data$submitdate), NA, as.Date(xml_data$submitdate, '%d/%m/%Y'))
  
  ## stats section 
  stats_section = ifelse(is.null(xml_data$trial_design$statisticalmethods), NA, xml_data$trial_design$statisticalmethods)
  if(is.na(stats_section) == TRUE){
    eframe = data.frame(number=number, reason='Missing stats section')
    excluded = bind_rows(excluded, eframe)
    next # skip to next study
  }
  
  # get study type
  study_type = ifelse(is.null(xml_data$trial_design$studytype), NA, xml_data$trial_design$studytype)
  # Intervention assignment, useful for knowing number of groups
  assignment = ifelse(is.null(xml_data$trial_design$assignment), NA, xml_data$trial_design$assignment)
  
  # sample size
  samplesize_target = ifelse(is.null(xml_data$recruitment$samplesize), NA, xml_data$recruitment$samplesize)
  samplesize_actual = ifelse(is.null(xml_data$recruitment$actualsamplesize), NA, xml_data$recruitment$actualsamplesize)

  # get email and name
  name = xml_data$contacts$contact$name
  email = xml_data$contacts$contact$email  

  ## store data 
  frame = data.frame(number = number, 
                     submitdate = submitdate,
                     name = name, email = email,
                     study_type = study_type,
                     assignment = assignment,
                     samplesize_target = samplesize_target, samplesize_actual = samplesize_actual,
                     stats_section = stats_section,
                     stringsAsFactors = FALSE)
  studies = bind_rows(studies, frame)
  frame =  NULL # tidy up
} # end of loop

### Data edits ###
# small fixes
studies = mutate(studies, 
                 samplesize_target = as.numeric(samplesize_target),
                 samplesize_actual = as.numeric(samplesize_actual)
) 

# check for duplicates, should be none
table(duplicated(studies$number))

# save
save(studies, excluded, censor.date, file='data/Stats_Sections.RData')


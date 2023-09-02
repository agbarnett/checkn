# 1_which_data.R
# creates filenames depending on data source
# October 2022

if(data_source == 'original'){
  in_data = 'data/0_Stats_Sections.RData'
  rds_file = 'data/1_stats_section_anzctr_cleaned.rds'
  file_processed = 'data/2_Stats_Sections_Processed.RData'
  file_params = 'data/3_Stats_Sections_Processed_Params.RData'
}

if(data_source == 'extra'){
  in_data = 'data/0_Stats_Sections_extra.RData' # from 0_read_stats_sections_extra.R
  rds_file = 'data/1_stats_section_anzctr_cleaned_extra.rds' # from 1_process_anzctr_stats_section.R
  file_processed = 'data/2_Stats_Sections_Processed_extra.RData' # from 2_find_sample_size.R
  file_params = 'data/3_Stats_Sections_Processed_extra_Params.RData'
}

# 1_random_select_checking.R
# randomly select 100 sections for hand checking
# data downloaded from https://www.anzctr.org.au/TrialSearch.aspx using empty search, then "download all to XML" button 
# March 2022
library(dplyr)
library(stringr)
library(openxlsx)

# get the data from 0_read_stats_sections.R
load('data/0_Stats_Sections.RData')

## randomly select stats sections
TeachingDemos::char2seed('mansfield')
must_contain = c('sample size','power','alpha','type i','type 1','drop.?out','attrition')
must_contain = paste(c('\\b', paste(must_contain, collapse='\\b|\\b')), collapse='') # with word breaks
selected = filter(studies, 
                  str_detect(stats_section, pattern=must_contain),
                  !str_detect(stats_section, 'pilot'), # exclude pilot studies
                  nchar(stats_section) > 100) %>% # must have a decent length
  sample_n(100) %>%
  select(number, stats_section) %>%
  mutate(type = NA, total_sample_size= NA, power= NA, alpha= NA, one_or_two_sided=NA, expect_diff= NA, sd_diff= NA, prop1=NA, prop2=NA, dropout= NA) %>%
  relocate(number, .after = dropout) # move number to last column

## export to excel
header_style <- createStyle(fontColour = "#ffffff", fgFill = "#4F80BD", halign = "left",
                  valign = "center", wrapText = TRUE, textDecoration = "Bold", border = "TopBottomLeftRight") # header style
cell_style <- createStyle(wrapText = TRUE, borderColour = "#4F81BD")
wb <- createWorkbook(title = 'stats sections for manual checking', subject = 'created by 1_random_select_checking.R')
options(openxlsx.borderColour = "#4F80BD")
options(openxlsx.borderStyle = "thin")
addWorksheet(wb, 'For checking', gridLines = TRUE)
freezePane(wb, sheet='For checking', firstRow = TRUE)
writeData(wb, sheet = 'For checking', x=selected, headerStyle = header_style, row.names=FALSE)
addStyle(wb, sheet='For checking', style = cell_style, cols=1, rows=1:101)
setColWidths(wb, sheet = 'For checking', cols=c(1,2,3,4,5,6,7,8,9,10,11,12), widths=c(180,10,10,10,10,10,11,10,10,10,10,10))
saveWorkbook(wb, "validation/checking_sample_size.xlsx", overwrite = TRUE)

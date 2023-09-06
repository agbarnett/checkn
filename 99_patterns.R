# 99_patterns.R
# patterns used in string detection
# September 2023

## some commonly used key-words:
type_1 = 'type.?(i\\b|1|one)' # needs break for `i` otherwise gets confused with type 2
type_1_error = paste(type_1, '.error', sep='')
type_2 = 'type.?(ii|2|two)'
type_2_error = paste(type_2, '.error', sep='')
significance = 'significan(t|ce)'

# years used in citations:
years = '(19[0-9][0-9]|20[0-2][0-9])' 

## key-words for sample size calculations
# beta must be whole word to avoid running into other words (also power and alpha)
# removed '0\\.05' as it just flagged p-value statements
keywords = c('\\bpower\\b','sample.size','\\balpha\\b','\\bbeta\\b', type_1, type_2)
keywords = paste(keywords, collapse='|')
# key words for more complex study designs for the sample size calculations
complex = c('longitudinal', 'repeated.measures?', '\\bauc\\b', '\\bspatial\\b', 'simulat(ed|ions?)', 'equivalence', 'non.?inferiority', '\\bcluster(ed|ing)?\\b', 'stepped.wedge', 'intra.?class.correlation')
complex = paste(complex, collapse='|')
# key words/phrases for pilot study
pilot1 = c('\\bpilot\\b', 'feasibility','progress to a larger') # exploratory? # phrase with study/rct
pilot1 = paste(pilot1, ' (rct|study)', sep='')
pilot2 = c('inform the sample size required','inform the required sample size')
pilot = c(pilot1, pilot2)
pilot = paste(pilot, collapse='|')

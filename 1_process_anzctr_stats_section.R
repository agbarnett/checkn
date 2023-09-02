# 1_process_anzctr_stats_section.R
# process the text in the stats section
# adapted from Nicole's code https://github.com/agbarnett/stats_section/blob/master/code/anzctr/2_process_anzctr_stats_section.R
# September 2023
library(tidyverse)
library(textclean)
library(tm)
library(spelling)
library(readxl)
library(stringi)
library(Unicode)
source('99_functions.R')


# Excel file from https://github.com/agbarnett/stats_section/blob/master/data/methods_dictionary.xlsx
stat_terms_hyphen = read_xlsx('data/methods_dictionary.xlsx', sheet = 'hyphen_terms')
stat_terms_single = read_xlsx('data/methods_dictionary.xlsx', sheet = 'single_terms')
stat_terms_model = read_xlsx('data/methods_dictionary.xlsx', sheet = 'models')
other_terms = read_xlsx('data/methods_dictionary.xlsx', sheet = 'other')

# which data to use
data_sources = c('original','extra')
data_source = data_sources[2]
source('1_which_data.R')

### section 1 ###
# get the data and process
load(in_data) # from 0_read_data_stats_section[_extra].R

# count sections as null if they are virtually empty
null_stats = c('NA', 'N/A', '\n  ', 'Nil', 'NIl ', 'None', 'None.', 'Pending') 

studies = filter(studies, 
         !str_detect(number, '^NCT')) %>% # none of these have stats section
 mutate( # binary outcome:
  stats_section = ifelse(stats_section %in% null_stats, NA, stats_section), 
  missing = as.numeric(is.na(stats_section)), 
  # length outcome (number of characters):
  length = nchar(stats_section), 
  length = ifelse(missing==TRUE, 0, length), 
  l_length = log(length+1), # log-transformed
  # number of words
  stats_section = str_replace_all(string=stats_section, pattern=' ', replacement = ' '), 
  stats_section = str_replace_all(string=stats_section, pattern='\n', replacement = ' '), 
  words = str_count(string=stats_section, pattern=' ') + 1, 
  # time
  year = as.numeric(format(submitdate, '%Y')), 
  date = as.numeric(submitdate - as.Date('2015-01-01')) / (1*365.25)) # standardised to one year

# it is clear that stats section was only available from 2013 onwards - confirmed on wayback machine that it was included then -- no earlier documentation
studies = filter(studies, 
         year >= 2013)
#only includes studies with a non-missing stats section
studies = filter(studies, missing==0)

stats_section = studies

#change column headings
stats_section = studies %>% rename('text_data' = stats_section) 
#change to native encoding
stats_section = stats_section %>% mutate(text_data = enc2native(text_data))
stats_section = stats_section %>% mutate(text_data_clean = tolower(text_data))

# remove full-stop from common abbreviations that end with .
abbreviations = c('approx','inc')
for (a in abbreviations){
  pattern = paste(' ', a, '\\.', sep='') # word with full-stop
  replace = paste(' ', a, sep='') # keep leading space
  stats_section = stats_section %>% 
    mutate(text_data_clean = str_replace_all(text_data_clean, pattern = pattern, replacement = replace))
}

#0. remove commas from large numbers
stats_section = stats_section %>% mutate(text_data_clean = gsub(",(?=\\d{3,})", "", text_data_clean, perl = TRUE)) # strip commas in numbers
# assume a comma between two digits without a space should be a decimal point, e.g, (1,1), but not (11,11); using look-ahead and look-behind
stats_section = stats_section %>% mutate(text_data_clean = gsub("(?<=\\b\\d{1}),(?=\\d{1}\\b)", ".", text_data_clean, perl = TRUE))
stats_section = stats_section %>% mutate(text_data_clean = gsub("(?<=\\b\\d{2}),(?=\\d{1}\\b)", ".", text_data_clean, perl = TRUE)) # version for things like `12,3`, but not `12,34`

#1. remove formatting/special characters
#numbered references eg [23]
#stats_section = stats_section %>% mutate(text_data_clean = str_replace_all(text_data_clean, "\\s*\\[\\d+\\]\\s*", ""))
stats_section = stats_section %>% mutate(text_data_clean = str_replace_all(text_data_clean, pattern="\\[\\S{1,3}\\]", replacement = ""))
stats_section = stats_section %>% mutate(text_data_clean = str_replace_all(string = text_data_clean, pattern = 'ß', replacement = 'beta')) # German eszett character 

## remove references - had to loop
### (could do more here, e.g, with DOI) # could remove any year in brackets
# `et al` and a year within brackets
for (k in 1:nrow(stats_section)){
  # pattern is all text in round/square brackets
  brackets = str_extract_all(stats_section$text_data_clean[k], pattern='\\(([^()]*)\\)|\\[([^()]*)\\]')[[1]]
  if(length(brackets) == 0){next} # go to next if no brackets
  for (j in 1:length(brackets)){
    is_ref = str_detect(brackets[j], pattern = years) + # has year
      str_detect(brackets[j], pattern = 'et.al') + # has et al
      str_detect(brackets[j], pattern = 'doi') # has doi 
    is_spss = str_detect(brackets[j], pattern = '\\bspss\\b') & 
      str_detect(brackets[j], pattern = 'chicago')
    if(is_ref >= 2 | is_spss == TRUE){ # remove reference or software; 2 out of 3 for reference
      stats_section$text_data_clean[k] = str_remove(stats_section$text_data_clean[k], pattern = brackets[j])
      print(brackets[j]) # display deleted reference
    }
  }
}


## turned this off - March 2022
#remove () including text within brackets "\\s*\\([^\\)]+\\)"
#option to keep text inside brackets is "[()]"
#stats_section = stats_section %>% mutate(text_data_clean = str_replace_all(text_data_clean, "[()]", " ")) # AGB, changed replace to space

# remove carriage tabs and tidy text due to line breaks
stats_section = stats_section %>% mutate(text_data_clean = str_replace_all(text_data_clean, pattern="\t", replacement = " "))
stats_section = stats_section %>% mutate(text_data_clean = str_replace_all(string = text_data_clean, pattern = "(?<= [:lower:]{1,45})- (?=[:lower:]+)", replacement = ""))

# 2. replace/standardise common symbols
#standardise dashes 
stats_section = stats_section %>% mutate(text_data_clean = str_replace_all(text_data_clean, pattern="\\s*(–+)\\s*", replacement = "-"))
 
## turn these ones off - may turn on again later ##
off = function(){
  #standardise text and spacing for >, <, = 
  stats_section = stats_section %>% mutate(text_data_clean = str_replace_all(text_data_clean, "\\s*[<]\\s*", " less-than "))
stats_section = stats_section %>% mutate(text_data_clean = str_replace_all(text_data_clean, "\\s*[>]\\s*", " greater-than "))
stats_section = stats_section %>% mutate(text_data_clean = str_replace_all(text_data_clean, "\\s*[=]\\s*", " equal-to "))
stats_section = stats_section %>% mutate(text_data_clean = str_replace_all(text_data_clean, "\\s*[<=]\\s*", " less-than-or-equal-to "))
stats_section = stats_section %>% mutate(text_data_clean = str_replace_all(text_data_clean, "\\s*[>=]\\s*", " greater-than-or-equal-to "))

stats_section = stats_section %>% mutate(text_data_clean = str_replace_all(text_data_clean, "\\bless than\\b", "less-than"))
stats_section = stats_section %>% mutate(text_data_clean = str_replace_all(text_data_clean, "\\bequal to\\b", "equal-to"))
stats_section = stats_section %>% mutate(text_data_clean = str_replace_all(text_data_clean, "\\bgreater than\\b", "greater-than"))

#randomisation ratios, convert to text (e.g. 1:1 -> 1-to-1)
stats_section = stats_section %>% mutate(text_data_clean = str_replace_all(text_data_clean, "\\s*(\\d+):(\\d+)\\s*", " \\1-to-\\2 "))
}

## remove version numbers with software packages - so they don't get confused with actual numbers
# e.g., "stata 17.0", 'r 4.1.3', 'g-power 3.1.9.7', add optional 'version'
software = c('stata','sas','g.?power','r','python','spss','graphpad.?prism','matlab','excel','jamovi','rstudio')
numbers = '[0-9][0-9]?\\.?[0-9]?\\.?[0-9]?\\.?[0-9]?' # potential version numbers
software = paste('(?<=\\b', software, '( version)?', ') ', rep(numbers, length(software)), sep='') # use look-ahead to only replace numbers
software = paste(software, collapse = '|')
stats_section = stats_section %>% mutate(text_data_clean = str_replace_all(text_data_clean, pattern = software, replacement = ' x'))
# test
#test_text = 'software r 2 not software br 1 stata version 17.0 between two independent groups. (g-power 3.1.9.7) usability'
#str_extract_all(test_text, pattern = software)
#str_replace_all(test_text, pattern = software, replacement = ' x')

# plus or minus 
stats_section = stats_section %>% mutate(text_data_clean = str_replace_all(text_data_clean, "\\s*(±)\\s*|\\s*(\\+/-+)\\s*", " plus-or-minus "))

# common symbols covered by text clean ($, %, #, @, &, w/)
stats_section = stats_section %>% mutate(text_data_clean = replace_symbol(text_data_clean))

# unicodes
stats_section = stats_section %>% mutate(text_data_clean = stri_escape_unicode(text_data_clean))

all_words = stats_section %>% unnest(text_data_clean) %>% 
 mutate(y=strsplit(text_data_clean, ' ')) %>% pull(y) %>% unlist()

# strip brackets and parentheses
stats_section = stats_section %>% 
  mutate(text_data_clean = str_replace_all(string = text_data_clean, pattern = '\\(|\\)|\\[|\\]', replacement = ' '))

#find all unicode characters
unicode_lookup = str_extract_all(all_words, pattern=regex("\\\\u\\w{4}")) %>% 
 unlist() %>%
 as_tibble() %>% count(value) %>%
 rename('unicode'=value) %>%
 arrange(-n)
#mutate: add label, symbol
unicode_lookup = unicode_lookup %>% mutate(label = u_char_name(gsub('\\\\u', '', unicode)))
unicode_lookup = unicode_lookup %>% mutate(label_clean = gsub(' ', '-', label) %>% tolower())
unicode_lookup = unicode_lookup %>% mutate(symbol = stri_unescape_unicode(unicode))

#update label_clean (custom)
## latin small/capital letters - take letter only
unicode_lookup = unicode_lookup %>% mutate(label_clean = gsub('latin-small-letter-(.*?)-.*$', '\\1', label_clean))
unicode_lookup = unicode_lookup %>% mutate(label_clean = gsub('latin-capital-letter-(.*?)-.*$', '\\1', label_clean))

#vulgar fraction
unicode_lookup = unicode_lookup %>% mutate(label_clean = gsub('vulgar-fraction-(.*?)', '\\1', label_clean))

## - sign. remove
unicode_lookup = unicode_lookup %>% mutate(label_clean = gsub('(.*?)-sign', '\\1', label_clean))

#greek letters (mu, beta)
unicode_lookup = unicode_lookup %>% mutate(label_clean = ifelse(unicode=='\\u00b5', 'mu', label_clean))
unicode_lookup = unicode_lookup %>% mutate(label_clean = ifelse(unicode=='\\u00df', 'beta', label_clean))
#basic operations
unicode_lookup = unicode_lookup %>% mutate(label_clean = ifelse(unicode=='\\u00f7', 'divided-by', label_clean))
unicode_lookup = unicode_lookup %>% mutate(label_clean = ifelse(unicode=='\\u00d7', 'multiplied-by', label_clean))
unicode_lookup = unicode_lookup %>% mutate(label_clean = ifelse(unicode=='\\u00b2', 'squared', label_clean))
unicode_lookup = unicode_lookup %>% mutate(label_clean = ifelse(unicode=='\\u02dc', 'approximately', label_clean))
#miscellaneous
unicode_lookup = unicode_lookup %>% mutate(label_clean = ifelse(unicode=='\\u00b7', '.', label_clean))
unicode_lookup = unicode_lookup %>% mutate(label_clean = ifelse(unicode=='\\u00af', 'bar', label_clean)) #e.g xbar
##end formatting of unicode labels

#format unicode characters (e.g. hair space <U+200A>)
#remove general punctuation unicodes u2x (no dashes identified)
unicode_spaces = unicode_lookup %>% filter(grepl("u2(\\w+)", unicode)) %>% pull(unicode) 
unicode_spaces_c = paste0('\\', unicode_spaces) %>% str_c(., collapse='|')
stats_section = stats_section %>% mutate(text_data_clean = str_replace_all(text_data_clean, pattern=unicode_spaces_c, replacement=" "))

#remove \u00a, \u00b4
unicode_remove = unicode_lookup %>% filter(grepl("u00a(\\w+)|u00b4", unicode)) %>% pull(unicode) 
unicode_remove_c = paste0('\\', unicode_remove) %>% str_c(., collapse='|')
stats_section = stats_section %>% mutate(text_data_clean = str_replace_all(text_data_clean, pattern=unicode_remove_c, replacement=" "))

#update text labels for remaining codes
unicode_set = unicode_lookup %>% filter(!unicode %in% c(unicode_spaces, unicode_remove)) %>% pull(unicode)
unicode_set_c = paste0('\\', unicode_set) %>% str_c(., collapse='|')

#add white space around unicode labels
unicode_to_text = function(input){
 out = unicode_lookup %>% filter(unicode==input) %>% pull(label_clean)
 paste0(' ', out, ' ')
}

stats_section = stats_section %>% mutate(text_data_clean = str_replace_all(text_data_clean, 
                                      unicode_set_c, 
                                      unicode_to_text))


#3. remove any remaining non-ascii characters, curly quotes
stats_section = stats_section %>% mutate(text_data_clean = replace_non_ascii(text_data_clean, replacement = " ")) 
stats_section = stats_section %>% mutate(text_data_clean = replace_curly_quote(text_data_clean, replacement = " "))

#remove punctuation except for a few key symbols
punctuation_to_keep = c("~~", ".", "-", '=', '<', '>', ',', ';', '(', ')', '[', ']')
stats_section$text_data_clean = strip(stats_section$text_data_clean, char.keep = punctuation_to_keep, apostrophe.remove=T, digit.remove=F)


# 4. make common statistical terms and methods consistent
#for each hyphenated term, create combined and unique plural terms
#plurals includes entries from stat_terms_model eg regressions to regression
stat_terms_hyphen = stat_terms_hyphen %>% mutate(combined_term = str_remove_all(term, ' '))
plural_terms = unique(c(paste0(c(stat_terms_hyphen[['term']], 
                 stat_terms_hyphen[['combined_term']], 
                 stat_terms_hyphen[['update']]), 's'), 
             stat_terms_model[['term']], 
             stat_terms_single[['term']]))

#str_c: define search strings
plural_terms_all = str_c("\\b", plural_terms, "\\b", collapse="|") #to turn plural to singular
stats_terms_all = str_c("\\b", stat_terms_hyphen[['term']], "\\b", collapse="|") #to join method words by hyphen
stats_combined_all = str_c("\\b", stat_terms_hyphen[['combined_term']], "\\b", collapse="|") #to split method words by hyphen
stats_terms_single = str_c("\\b", stat_terms_single[['term']], "\\b", collapse="|")


#update common US to GB spelling
other_terms_all = str_c(other_terms[['term']], collapse='|') #incorrect spellings identified
change_other = function(input){
 other_terms %>% filter(term==input) %>% pull(update)
}

stats_section = stats_section %>% mutate(text_data_clean = str_replace_all(text_data_clean, 
                                      other_terms_all, 
                                      change_other))


#change plural terms to singular
stats_section = stats_section %>% mutate(text_data_clean = str_replace_all(text_data_clean, 
                                      plural_terms_all, 
                                      function(x) gsub('s$', '', x)))

#standardise common stats terms with hyphen
change_stats_terms = function(input){
 stat_terms_hyphen %>% filter(term==input) %>% pull(update)
}
stats_section = stats_section %>% mutate(text_data_clean = str_replace_all(text_data_clean, 
                                      stats_terms_all, 
                                      change_stats_terms))

#combined stats terms; split by hyphen
change_stats_combined = function(input){
 stat_terms_hyphen %>% filter(combined_term==input) %>% pull(update)
}

stats_section = stats_section %>% mutate(text_data_clean = str_replace_all(text_data_clean, 
                                      stats_combined_all, 
                                      change_stats_combined))

#check for remaining instances
all_words = stats_section %>% unnest(text_data_clean) %>% 
 mutate(y=strsplit(text_data_clean, ' ')) %>% pull(y) %>% unlist() 

#word frequencies for checking
word_freq = tibble::enframe(all_words) %>% count(value) %>% arrange(-n)
word_freq %>% filter(value %in% stat_terms_hyphen[['combined_term']])
word_freq %>% filter(value %in% stat_terms_hyphen[['term']])
word_freq %>% filter(value %in% plural_terms)
word_freq %>% filter(value %in% stat_terms_hyphen[['update']])
word_freq %>% filter(value %in% other_terms[['term']])
word_freq %>% filter(value %in% other_terms[['update']])

# remove excess white space
stats_section = stats_section %>% 
  mutate(text_data_clean = replace_white(text_data_clean),
         text_data_clean = str_squish(text_data_clean))

# replace `et al` (with any space but avoiding `ethal`) with `et al`
et_al = 'et[^a-zA-Z0-9]al\\.?' # string to replace
stats_section = stats_section %>% 
  mutate(text_data_clean = str_replace_all(text_data_clean, et_al, 'et al'))

#choose 100 records to check quality for data cleaning
sample_studies = stats_section %>% distinct(number) %>% sample_n(., 100)
sample_data = stats_section %>% filter(number %in% sample_studies[['number']])

# drop data no longer needed
stats_section = select(stats_section, -missing, -year, -date, -l_length, -length)

# save
write_rds(stats_section, file = rds_file, compress = "xz", compression = 9L)


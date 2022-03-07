# 3_extract_parameters.R
# extract parameters required for calculating sample size
# February 2022
library(dplyr)
library(stringr)
library(textclean)

load('data/Stats_Sections_Processed.RData') # AGB, from 2_find_sample_size.R
#load('Stats_Sections_Processed.RData')

#generic function to find a number associated with a keyword in a sentence
#  will search for a number directly next to specific phrases first
#  if none are found, will search for numbers before or after keywords that are not broken by other specified keywords (breaking_words)
#  (e.g., "80% power and significance level of 0.05" will return 0.8 but not 0.05 for power)
#***negative numbers not currently implemented***
find_num_assoc_with_word <- function(sentences, phrases, keywords, phrases_position, breaking_words = NA,
                                     number_integer_general = FALSE, number_integer_percent = TRUE, 
                                     number_decimal_general = TRUE, number_decimal_percent = TRUE, 
                                     number_type = NA, number_likely_limits = c(0, Inf)) {
  
  #number_type can be any of c("proportion","probability","ratio","positive") 
  #number_likely_limits vector of lower and upper limit, set to NA to ignore
  
  #initialise vectors
  numbers_found = vector(mode = 'numeric', length = 0)
  
  #phrase patterns
  phrases_before = phrases[which(phrases_position %in% c('before', 'either'))]
  phrases_after = phrases[which(phrases_position %in% c('after', 'either'))]
  
  #fix number_likely_limits if only one limit is NA
  if (!all(is.na(number_likely_limits))) {
    if (is.na(number_likely_limits[1])) number_likely_limits[1] = 0
    if (is.na(number_likely_limits[2])) number_likely_limits[2] = Inf
  }
  
  #extract percentages
  patterns = vector(mode = 'character', length = 0)
  sentences_wo_percentages = sentences
  if ((number_integer_percent == TRUE)|(number_decimal_percent == TRUE)) {

    if (number_integer_percent == TRUE) {
      patterns = c(patterns, c(paste0('\\d+(?!\\.)(?=\\% ', phrases_after, ')'), #number % sign, keyword after
                               paste0('\\d+(?!\\.)(?= percent ', phrases_after, ')'), #number % word, keyword after
                               paste0('(?<=', phrases_before, ' )\\d+(?!\\.)(?=\\%)'), #number % sign, keyword before
                               paste0('(?<=', phrases_before, ' )\\d+(?!\\.)(?= percent)'))) #number with % word, keyword before
    }
    if (number_decimal_percent == TRUE) {
      patterns = c(patterns, c(paste0('(\\d+| )\\.\\d+(?=\\% ', phrases_after, ')'), #number % sign, keyword after
                               paste0('(\\d+| )\\.\\d+(?= percent ', phrases_after, ')'), #number % word, keyword after
                               paste0('(?<=', phrases_before, ' )(\\d+| )\\.\\d+(?=\\%)'), #number % sign, keyword before
                               paste0('(?<=', phrases_before, ' )(\\d+| )\\.\\d+(?= percent)'))) #number with % word, keyword before
    }
    for (j in 1:length(sentences)) numbers_found = c(numbers_found, as.numeric(unlist(str_extract_all(string = sentences[j], pattern = patterns))))
    numbers_found = numbers_found/100
    
    #remove percentages from sentence_parts before looking further
    sentences_wo_percentages = as.list(str_replace_all(string = sentences_wo_percentages, pattern = '\\d+\\.\\d+.?percent', replacement = ''))
    sentences_wo_percentages = as.list(str_replace_all(string = sentences_wo_percentages, pattern = '\\d+.?percent', replacement = ''))

  } 
  
  #extract other numbers, append to list of percentages
  patterns = vector(mode = 'character', length = 0)
  if ((number_integer_general == TRUE)|(number_decimal_general == TRUE)) {

    if (number_integer_general == TRUE) {
      patterns = c(patterns, c(paste0('\\d+(?!(\\.))(?= ', phrases_after, ')'), #decimal, keyword after
                               paste0('(?<=', phrases_before, ' )(?!\\d+\\.\\d+)\\d+'))) #decimal, keyword before
    }
    if ((number_decimal_general == TRUE)&(number_type %in% c('proportion', 'probability'))) {
      patterns = c(patterns, c(paste0('(0| )\\.\\d+(?= ', phrases_after, ')'), #decimal, keyword after
                               paste0('(?<=', phrases_before, ' )(0| )\\.\\d+'))) #decimal, keyword before
    }
    if ((number_decimal_general == TRUE)&(number_type %in% c('ratio', 'positive'))) {
      patterns = c(patterns, c(paste0('(\\d+| )\\.\\d+(?= ', phrases_after, ')'), #decimal, keyword after
                               paste0('(?<=', phrases_before, ' )(\\d+| )\\.\\d+'))) #decimal, keyword before
    }
    for (j in 1:length(sentences_wo_percentages)) numbers_found = c(numbers_found, as.numeric(unlist(str_extract_all(string = sentences_wo_percentages[j], pattern = patterns))))

  }
  
  #apply likely limits
  if (!all(is.na(number_likely_limits))&(length(numbers_found) > 0)) numbers_found = numbers_found[which((numbers_found >= number_likely_limits[1])&(numbers_found <= number_likely_limits[2]))]
  numbers_found = unique(numbers_found)
  
  #if numbers found using specific phrases, return those numbers, otherwise broaden search
  if (length(numbers_found) != 0) return(numbers_found)
  
  #break sentences into parts that start/end with keyword and nearest number
  #ignore sentence parts in which the keyword is after the number but the number is 
  #preceded by of|set at|set to|equal to|.?=.?|greater than|.?>.?
  #this will avoid picking up e.g., 0.05 for power in "significance level of 0.05, power..."
  #also ignore sentence parts where the keyword is followed by "and" 
  sentence_parts = vector(mode = 'character', length = 0)
  for (i in 1:length(keywords)) {
    for (j in 1:length(sentences)) {
      sentence_parts = c(sentence_parts, unlist(str_extract_all(string = sentences[j], pattern = c(paste0('(?<!(of|set at|set to|equal to|.?=.?|greater than|.?>.?) ?)(\\d+| )\\.\\d+\\D+', keywords[i]), #decimal, keyword after
                                                                                                   paste0(keywords[i], '(?! and)\\D+(\\d+| )\\.\\d+( percent|.?|%)'), #decimal, keyword before
                                                                                                   paste0('(?<!(of|set at|set to|equal to|.?=.?|greater than|.?>.?)) \\d+\\D+', keywords[i]), #integer, keyword after
                                                                                                   paste0(keywords[i], '(?! and)\\D+ \\d+(?!\\.)( percent|.?|%)'))))) #integer, keyword before
    }
  }
  
  if (length(sentence_parts) > 0) {
    
    #remove sentence parts that contain breaking words
    if ((breaking_words[1] != '')|(!is.na(breaking_words[1]))) {
      for (i in 1:length(breaking_words)) {
        sentence_parts = sentence_parts[is.na(str_match(string = sentence_parts, pattern = breaking_words[i])[,1])]
      }  
    }
    
    #extract number within remaining sentence parts 
    #extract percentages
    patterns = vector(mode = 'character', length = 0)
    if ((number_integer_percent == TRUE)|(number_decimal_percent == TRUE)) {
      
      if (number_integer_percent == TRUE) {
        patterns = c(patterns, c(paste0('\\d+(?!\\.)(?=\\%\\D+', keywords, ')'), #number % sign, keyword after
                                 paste0('\\d+(?!\\.)(?= percent\\D+', keywords, ')'), #number % word, keyword after
                                 paste0('(?<=', keywords, '\\D{0,100})\\d+(?!\\.)(?=\\%)'), #number % sign, keyword before (up to 100 characters, limit required by look-behind)
                                 paste0('(?<=', keywords, '\\D{0,100})\\d+(?!\\.)(?= percent)'))) #number with % word, keyword before (up to 100 characters, limit required by look-behind)
      }
      if (number_decimal_percent == TRUE) {
        patterns = c(patterns, c(paste0('(\\d+| )\\.\\d+(?=\\%\\D+', keywords, ')'), #number % sign, keyword after
                                 paste0('(\\d+| )\\.\\d+(?= percent\\D+', keywords, ')'), #number % word, keyword after
                                 paste0('(?<=', keywords, '\\D{0,100})(\\d+| )\\.\\d+(?=\\%)'), #number % sign, keyword before (up to 100 characters, limit required by look-behind)
                                 paste0('(?<=', keywords, '\\D{0,100})(\\d+| )\\.\\d+(?= percent)'))) #number with % word, keyword before (up to 100 characters, limit required by look-behind)
      }
      for (j in 1:length(sentence_parts)) numbers_found = c(numbers_found, as.numeric(unlist(str_extract_all(string = sentence_parts[j], pattern = patterns))))
      numbers_found = numbers_found/100
      
      #remove percentages from sentence_parts before looking further
      sentence_parts = as.list(str_replace_all(string = sentence_parts, pattern = '\\d+\\.\\d+.?percent', replacement = ''))
      sentence_parts = as.list(str_replace_all(string = sentence_parts, pattern = '\\d+.?percent', replacement = ''))
      
    }
    
    #extract other numbers, append to list of percentages
    patterns = vector(mode = 'character', length = 0)
    if ((number_integer_general == TRUE)|(number_decimal_general == TRUE)) {
      
      if (number_integer_general == TRUE) {
        patterns = c(patterns, c(paste0('\\d+(?!\\.)(?=\\D+', keywords, ')'), #integer, keyword after
                                 paste0('(?<=', keywords, '\\D{0,100})(?!\\d+\\.\\d+)\\d+'))) #integer, keyword before (up to 100 characters, limit required by look-behind)
      }
      if ((number_decimal_general == TRUE)&(number_type %in% c('proportion', 'probability'))) {
        patterns = c(patterns, c(paste0('(0| )\\.\\d+(?=\\D+', keywords, ')'), #decimal, keyword after
                                 paste0('(?<=', keywords, '\\D{0,100})(0| )\\.\\d+'))) #decimal, keyword before (up to 100 characters, limit required by look-behind)
      }
      if ((number_decimal_general == TRUE)&(number_type %in% c('ratio', 'positive'))) {
        patterns = c(patterns, c(paste0('(\\d+| )\\.\\d+(?=\\D+', keywords, ')'), #decimal, keyword after
                                 paste0('(?<=', keywords, '\\D{0,100})(\\d+| )\\.\\d+'))) #decimal, keyword before (up to 100 characters, limit required by look-behind)
      }
      for (j in 1:length(sentence_parts)) numbers_found = c(numbers_found, as.numeric(unlist(str_extract_all(string = sentence_parts[j], pattern = patterns))))
      
    }
    
    #apply likely limits
    if (!all(is.na(number_likely_limits))&(length(numbers_found) > 0)) numbers_found = numbers_found[which((numbers_found >= number_likely_limits[1])&(numbers_found <= number_likely_limits[2]))]
    numbers_found = unique(numbers_found)
    
  }
  
  if (length(numbers_found) == 0) numbers_found = NA
  return(numbers_found)
  
}



# big loop
N = nrow(studies)
update = NULL
for (k in 1:N){
  
  if (k %in% seq(0,N,100)) print(k)
  
  # get the study details
  this_study = studies[k,]
  stats_section = this_study$stats_section
  
  #one or two sided hypothesis test
  numsides = NA
  numsides.both = FALSE
  if (str_count(string = stats_section, pattern = c('(1|one|single)..?(side|tail)')) > 0) numsides = 1
  if (str_count(string = stats_section, pattern = c('(2|two|double)..?(side|tail)')) > 0) numsides = 2
  if ((str_count(string = stats_section, pattern = c('(1|one|single)..?(side|tail)')) > 0)&(str_count(string = stats_section, pattern = c('(2|two|double)..?(side|tail)')) > 0)) numsides.both = TRUE
  
  #replace 1/2 sided with one/two sided/tailed (so it is not picked up as a significance level)
  stats_section = str_replace_all(string = stats_section, pattern = '1(?=..?side)', replacement = 'one')
  stats_section = str_replace_all(string = stats_section, pattern = '1(?=..?tail)', replacement = 'one')
  stats_section = str_replace_all(string = stats_section, pattern = '2(?=..?side)', replacement = 'two')
  stats_section = str_replace_all(string = stats_section, pattern = '2(?=..?tail)', replacement = 'two')
  
  #replace digits in unit measures with words ***add more as find them***
  stats_section = str_replace_all(string = stats_section, pattern = '/cm2', replacement = ' per cm squared')
  stats_section = str_replace_all(string = stats_section, pattern = '/m2', replacement = ' per meter squared')
  stats_section = str_replace_all(string = stats_section, pattern = '/cm3', replacement = ' per cm cubed')
  stats_section = str_replace_all(string = stats_section, pattern = '/m3', replacement = ' per meter cubed')
  
  #replace decimals starting with a dot (missing zero) with 0. ***test this***
  stats_section = str_replace_all(string = stats_section, pattern = ' (?=\\.\\d+)', replacement = ' 0')
  
  #split stats_section into sentences
  sentences = unlist(str_split(string = stats_section, pattern = c('(?<=\\.|\\?|\\!)\\s(?=[a-z])')))
  
  #***to do: fix common typos in keywords before using find_num_assoc_with_word()***
  #***to do: replace years in citations with words so they are not picked up as parameters***
 
  #extract sample size
  keywords = 'sample size'
  phrases = c('of', 'set at', 'set to', 'equal to', '.?=.?', 'greater than' , 'greater than or equal to', '.?>.?', '.?>=.?')
  phrases = c('sample size', 'sample', paste0('sample size ', phrases), paste0('sample ', phrases), 'participants', 'patients', 'individuals', 'people', 'children', 'mothers', 'neonates', 'men', 'women', 'subjects', 'adults')
  phrases_position = c('either', 'either', rep('before', 9), rep('before', 9), rep('after', 11))
  sample.size = find_num_assoc_with_word(
    sentences,
    phrases = phrases,
    keywords = keywords,
    phrases_position = phrases_position,
    breaking_words = c('significan', 'alpha', 'type.?(1|one|i )','power', 'beta', 'type.?(ii|2|two)', 'confidence', ' and ', ' with '),
    number_integer_general = TRUE, number_integer_percent = FALSE, 
    number_decimal_general = FALSE, number_decimal_percent = FALSE, 
    number_type = "positive", number_likely_limits = c(NA, NA))
  sample.size = list(sample.size)
  
  #extract power
  keywords = 'power'
  phrases = c('of', 'set at', 'set to', 'equal to', '.?=.?', 'greater than' , 'greater than or equal to', '.?>.?', '.?>=.?')
  phrases = c(keywords, paste0('power ', phrases))
  phrases_position = c('either', rep('before', 9))
  power.vec = find_num_assoc_with_word(
    sentences,
    phrases = phrases,
    keywords = keywords,
    phrases_position = phrases_position,
    breaking_words = c('significan', 'alpha', 'type.?(1|one|i )', 'confidence', ' and ', ' with '),
    number_integer_general = FALSE, number_integer_percent = TRUE, 
    number_decimal_general = TRUE, number_decimal_percent = TRUE, 
    number_type = "probability", number_likely_limits = c(0.5, 1))
  
  #extract type 2 error
  keywords = c('beta', 'type.?(ii|2|two)')
  phrases = c('of', 'set at', 'set to', 'equal to', '.?=.?', 'less than' , 'less than or equal to', '.?<.?', '.?<=.?')
  phrases = c(keywords, 
              paste0('beta ', phrases),
              paste0('type.?(ii|2|two).error ', phrases))
  phrases_position = c('either', 'either', rep('before', 9), rep('before', 9))
  type2err.vec = find_num_assoc_with_word(
    sentences, 
    phrases = phrases, 
    keywords = keywords,
    phrases_position = phrases_position, 
    breaking_words = c('significan', 'alpha', 'type.?(1|one|i )', 'confidence', ' and ', ' with '), 
    number_integer_general = FALSE, number_integer_percent = TRUE, 
    number_decimal_general = TRUE, number_decimal_percent = TRUE, 
    number_type = "probability", number_likely_limits = c(0, 0.5))
  
  #reported power
  power.used = list(unique(na.omit(c(power.vec, 1-type2err.vec))))
  power.used = ifelse(length(unlist(power.used)) > 0, power.used, list(NA))
  
  # extract significance level
  keywords =c('significan', 'alpha', 'type.?(i |1|one)')
  phrases = c('of', 'set at', 'set to', 'equal to', '.?=.?', 'less than' , 'less than or equal to', '.?<.?', '.?<=.?')
  phrases = c('p ?= ?', 'significance', 'significance level', 'alpha', 'type.?(i |1|one) error', 
              paste('significance ', phrases), 
              paste('significance level ', phrases), 
              paste('alpha ', phrases), 
              paste('type.?(i |1|one) error ', phrases))
  phrases_position = c('before', 'either', 'either', 'either', 'either', 
                       rep(rep('before', 9), 4))
  siglev.vec = find_num_assoc_with_word(
    sentences, 
    phrases = phrases, 
    keywords = keywords, 
    phrases_position = phrases_position, 
    breaking_words = c('power', 'beta', 'type.?(ii|2|two)', ' and ', ' with '), 
    number_integer_general = FALSE, number_integer_percent = TRUE, 
    number_decimal_general = TRUE, number_decimal_percent = TRUE, 
    number_type = "probability", number_likely_limits = c(0, 0.5))
  siglev.vec = list(siglev.vec)

  #correction
  keywords = c('correction')
  phrases = c('of', 'set at', 'set to', 'equal to', '.?=.?')
  phrases = c('correction', 
              paste0('correction ', phrases))
  phrases_position = c('either', rep('before', 5))
  correct.vec = find_num_assoc_with_word(
    sentences, 
    phrases = phrases, 
    keywords = keywords, 
    phrases_position = phrases_position, 
    breaking_words = c('drop', 'loss', 'attrition', 'withdrawal', 'follow.?up'), 
    number_integer_general = FALSE, number_integer_percent = TRUE, 
    number_decimal_general = FALSE, number_decimal_percent = FALSE, 
    number_type = "positive", number_likely_limits = c(NA, NA))
  if (length(correct.vec) != 0) correct.vec = correct.vec*100 #return as %
  correct.vec = list(correct.vec)  
    
  #loss to follow up/attrition
  phrases = c('of', 'set at', 'set to', 'equal to', '.?=.?')
  phrases = c('loss?t? to follow.?up', 'follow.?up loss', 'withdrawal', 'attrition', 
              paste0('loss? to follow.?up ', phrases), 
              paste0('follow.?up loss ', phrases), 
              paste0('withdrawal ', phrases),
              paste0('attrition ', phrases))
  phrases_position = c('either', 'either', 'either', 
                       rep(c('before', 'before', 'before', 'before', 'before'), 3))
  loss.vec = find_num_assoc_with_word(
    sentences, 
    phrases = phrases, 
    keywords = c('loss', 'withdrawal', 'attrition'), 
    phrases_position = phrases_position, 
    breaking_words = c('drop', ' and ', ' with '), 
    number_integer_general = FALSE, number_integer_percent = TRUE, 
    number_decimal_general = TRUE, number_decimal_percent = FALSE, 
    number_type = "positive", number_likely_limits = c(NA, NA))
  if (length(loss.vec) != 0) loss.vec = loss.vec*100 #return as %
  loss.vec = list(loss.vec)
  
  #compliance/adherence: drop-ins
  phrases = c('of', 'set at', 'set to', 'equal to', '.?=.?')
  phrases = c('drop ?-?ins?', 'drop ?-?in rate', 'dropping.?in', 
              paste0('drop ?-?ins? ', phrases), 
              paste0('drop ?-?in rate ', phrases), 
              paste0('dropping.?in ', phrases))
  phrases_position = c('either', 'either', 'either', 
                       rep(c('before', 'before', 'before', 'before', 'before'), 3))
  dropin.vec = find_num_assoc_with_word(
    sentences, 
    phrases = phrases, 
    keywords = c('drop ?-?ins?', 'drop ?-?in rate', 'dropping.?in'), 
    phrases_position = phrases_position, 
    breaking_words = c('drop.?outs?', 'drop.?out rate', 'dropping.?out', ' and ', ' with '), 
    number_integer_general = FALSE, number_integer_percent = TRUE, 
    number_decimal_general = TRUE, number_decimal_percent = FALSE, #assuming will only be reported as integer% or 0<decimal<1
    number_type = "positive", number_likely_limits = c(NA, NA))
  if (length(dropin.vec) != 0) dropin.vec = dropin.vec*100 #return as %
  dropin.vec = list(dropin.vec)
  
  #compliance/adherence: drop-outs
  phrases = c('of', 'set at', 'set to', 'equal to', '.?=.?')
  phrases = c('drop.?outs?', 'drop.?out rate', 'dropping.?out', 
              paste0('drop.?outs? ', phrases), 
              paste0('drop.?out rate ', phrases), 
              paste0('dropping.?out ', phrases))
  phrases_position = c('either', 'either', 'either', 
                       rep(c('before', 'before', 'before', 'before', 'before'), 3))
  dropout.vec = find_num_assoc_with_word(
    sentences, 
    phrases = phrases, 
    keywords = c('drop.?outs?', 'drop.?out rate', 'dropping.?out'), 
    phrases_position = phrases_position, 
    breaking_words = c('drop ?-?ins?', 'drop ?-?in rate', 'dropping.?in', ' and ', ' with '), 
    number_integer_general = FALSE, number_integer_percent = TRUE, 
    number_decimal_general = TRUE, number_decimal_percent = FALSE, #assuming will only be reported as integer% or 0<decimal<1
    number_type = "positive", number_likely_limits = c(NA, NA))  
  if (length(dropout.vec) != 0) dropout.vec = dropout.vec*100 #return as %
  dropout.vec = list(dropout.vec)
  
  #unequal groups ***to do***  
  #need params unequal_groups = FALSE, unequal_q1 = 0.5, unequal_q2 = 0.5
  
  #difference in means
  phrases = c('of', 'equal to', '.?=.?')
  phrases = c('difference in means?', 'mean difference', 'difference in averages?', 'average difference', 'effect size', #how to differentiate this from diff in proportions?
              paste0('difference in means? ', phrases), 
              paste0('mean difference ', phrases), 
              paste0('difference in averages? ', phrases), 
              paste0('average difference ', phrases), 
              paste0('effect size ', phrases))
  phrases_position = c('either', 'either', 'either', 'either', 'either', 
                       rep(c('before', 'before', 'before'), 5))
  expect_diff = find_num_assoc_with_word(
    sentences, 
    phrases = phrases, 
    keywords = c('difference in means?', 'mean difference', 'difference in averages?', 'average difference', 'effect size'), 
    phrases_position = phrases_position, 
    breaking_words = c('proportion', 'power', 'type.?(ii|2|two)', 'significan', 'alpha', 'type.?(1|one|i )', ' and ', ' with ', 'confidence'), 
    number_integer_general = TRUE, number_integer_percent = TRUE, 
    number_decimal_general = TRUE, number_decimal_percent = TRUE, 
    number_type = "positive", number_likely_limits = c(NA, NA))  
  expect_diff = list(expect_diff)
  
  #standard deviation of difference
  phrases = c('of', 'equal to', '.?=.?')
  phrases = c('standard deviation', 'sd', 
              paste0('standard deviation ', phrases), 
              paste0(' sd ', phrases))
  phrases_position = c('either', 'either', 
                       rep(c('before', 'before', 'before'), 2))
  sd_diff = find_num_assoc_with_word(
    sentences, 
    phrases = phrases, 
    keywords = c('standard deviation', ' sd '), 
    phrases_position = phrases_position, 
    breaking_words = c('mean', 'average', 'power', 'type.?(ii|2|two)', 'significan', 'alpha', 'type.?(1|one|i )', ' and ', ' with ', 'confidence'), 
    number_integer_general = TRUE, number_integer_percent = TRUE, 
    number_decimal_general = TRUE, number_decimal_percent = TRUE, 
    number_type = "positive", number_likely_limits = c(NA, NA))   
  sd_diff = list(sd_diff)
  
  #relative risk reduction
  keywords = c('relative risk reduction', 'rrr', 'relative reduction') #need relative risk increase as well
  phrases = c('of', 'equal to', '.?=.?')
  phrases = c('relative risk reduction', 'rrr', 'relative reduction', 
              paste0('relative risk reduction ', phrases),
              paste0('relative reduction ', phrases),
              paste0('rrr ', phrases))
  phrases_position = c('either', 'either', 'either', rep(rep('before', 3), 3))
  rrr = find_num_assoc_with_word(
    sentences, 
    phrases = phrases, 
    keywords = keywords, 
    phrases_position = phrases_position, 
    breaking_words = c('difference', 'absolute', 'means?', 'averages?', 'power', 'type.?(ii|2|two)', 'significan', 'alpha', 'type.?(1|one|i )', ' and ', ' with '), 
    number_integer_general = FALSE, number_integer_percent = TRUE, 
    number_decimal_general = TRUE, number_decimal_percent = TRUE, 
    number_type = "positive", number_likely_limits = c(NA, NA))  
  rrr = list(rrr)
  
  #absolute risk reduction
  keywords = c('absolute risk reduction', 'arr', 'absolute reduction') #need absolute risk increase as well
  phrases = c('of', 'equal to', '.?=.?')
  phrases = c('absolute risk reduction', 'arr', 'absolute reduction',
              paste0('absolute risk reduction ', phrases),
              paste0('absolute reduction ', phrases),
              paste0('arr ', phrases))
  phrases_position = c('either', 'either', 'either', rep(rep('before', 3), 3))
  arr = find_num_assoc_with_word(
    sentences, 
    phrases = phrases, 
    keywords = keywords, 
    phrases_position = phrases_position, 
    breaking_words = c('difference', 'relative', 'means?', 'averages?', 'power', 'type.?(ii|2|two)', 'significan', 'alpha', 'type.?(1|one|i )', ' and ', ' with '), 
    number_integer_general = FALSE, number_integer_percent = TRUE, 
    number_decimal_general = TRUE, number_decimal_percent = TRUE, 
    number_type = "positive", number_likely_limits = c(NA, NA))  
  arr = list(arr)
  
  
  
  
  # #***continue to add all params required by sample size functions:***
  #
  # expect_diff = NA, sd_diff = NA, mu0 = NA, mu1 = NA,  sd0 = NA, sd1 = NA,  n0 = NA, n1 = NA, 
  # m = NA, icc = NA
  # 
  # #look for props, rates, arr, rrr
  # p_ctrl = NA, p_int = NA, arr = NA, rrr = NA, f = NA, delta = NA, 
  # m = NA, kappa = NA, pstar = NA, 
  # 
  # #survival
  # lambda_int = NA, lambda_ctrl = NA, 
  # t = 0, t0 = 0, 
  
  #***get study type from text: crossover, factorial, clusterrand, equivalence
  
  #, -alpha, -power, -loss_followup_pct, -dropin_pct, -dropout_pct, -expect_diff, -sd_diff
  
  ## store data 
  frame = select(this_study, -stats_section) %>%
    mutate(stats_section = stats_section,
           n_extracted = sample.size,
           alpha = siglev.vec, 
           power = power.used, 
           numsides = numsides,
           numsides.both = numsides.both,
           correction_pct = correct.vec,
           loss_followup_pct = loss.vec, 
           dropin_pct = dropin.vec, 
           dropout_pct = dropout.vec, 
           expect_diff = expect_diff, 
           sd_diff = sd_diff,
           rrr = rrr,
           arr = arr
    )
  update = bind_rows(update, frame)
  frame =  NULL # tidy up
  
}

# data edits
studies = mutate(update) 

#clean up
rm(stats_section, this_study, keywords, phrases, phrases_position, sentences, sample.size, arr, rrr,
   siglev.vec, power.vec, power.used, type2err.vec, numsides, numsides.both, 
   correct.vec, loss.vec, dropin.vec, dropout.vec, expect_diff, sd_diff)

# save
save(studies, excluded, file = 'data/Stats_Sections_Processed_wParams.RData')
#save(studies, excluded, file = 'data/Stats_Sections_Processed_Params.RData')

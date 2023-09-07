# 3_extract_parameters.R
# extract parameters from stats methods sections text that are required for calculating sample size
# September 2023
library(dplyr)
library(stringr)
library(textclean)
source('99_patterns.R')

# which data to use
data_sources = c('original','extra')
data_source = data_sources[2]
source('1_which_data.R')
load(file_processed) # AGB, from 2_find_sample_size.R

testing = function(){
#******TESTING******
#20220710
#subset of studies_full that includes only test set trials
testset <- read.csv(file = "checking_sample_size.csv")
for (i in c(1:6,31:90,92:94,96:97,99:100)) tmp[i,1] <- which(testset$number[i] == studies_full$number)
#some are missing, copy in previous one to preserve ordering 
tmp[c(7,30,91,95,98),1] <- tmp[c(7,30,91,95,98)-1,1]
studies <- studies_full[tmp,]
rm(tmp)
#******************
}


#generic function to find a number associated with a keyword in a sentence
# will search for a number directly next to specific phrases first
# if none are found, will search for numbers before or after keywords that are not broken by other specified keywords (breaking_words)
# (e.g., "80% power and significance level of 0.05" will return 0.8 but not 0.05 for power)
#***negative numbers not currently implemented***
find_num_assoc_with_word <- function(sentences, 
          phrases, 
          keywords, 
          phrases_position, # before ['phrase number'], after ['number phrase'], either
          breaking_words = NA,
          number_integer_general = FALSE, 
          number_integer_percent = TRUE, 
          number_decimal_general = TRUE, 
          number_decimal_percent = TRUE, 
          number_type = NA, 
          number_likely_limits = c(0, Inf)) {
 
 #number_type can be any of c("proportion","probability","ratio","positive") 
 #number_likely_limits vector of lower and upper limit, set to NA to ignore
 
 #initialise vectors
 numbers_found = vector(mode = 'numeric', length = 0)
 
 #phrase patterns
 phrases_before = phrases[which(phrases_position %in% c('before', 'either'))]
 phrases_after = phrases[which(phrases_position %in% c('after', 'either'))]
 if(length(phrases_before) ==0){stop('No before phrases')}
 if(length(phrases_after) ==0){stop('No after phrases')}
 
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
  patterns = c(patterns, c(
   paste0(' \\d+(?!\\.)(?= (\\%|per.?cent) ', phrases_after, ')'), #number % sign, keyword after
   paste0('(?<=', phrases_before, ' (were|was|is|are)? approx(imately)? ?)\\d+(?!\\.)(?= (\\%|per.?cent))'), #number % sign, keyword before plus `approximately`
   paste0('(?<=', phrases_before, ' ?)\\d+(?!\\.)(?= (\\%|per.?cent))') #number % sign, keyword before
  ))
 }
 if (number_decimal_percent == TRUE) {
  patterns = c(patterns, c(
   paste0('(\\d+| )\\.\\d+(?= (\\%|per.?cent) ', phrases_after, ')'), #number % word, keyword after
   paste0('(?<=', phrases_before, ' (were|was|is|are)? approx(imately)? ?)(\\d+| )\\.\\d+(?= (\\%|per.?cent))'), #number % sign, keyword before, with approximately
   paste0('(?<=', phrases_before, ' ?)(\\d+| )\\.\\d+(?= (\\%|per.?cent))') #number % sign, keyword before
  ))
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
  patterns = c(patterns, c(
   paste0(' \\d+(?!(\\.))(?= ', phrases_after, ')'), #decimal, keyword after
   paste0('(?<=', phrases_before, ' (were|was|is|are)? approx(imately)? ?)(?!\\d+\\.\\d+)\\d+'), #decimal, keyword before, with approximately
   paste0('(?<=', phrases_before, ' ?)(?!\\d+\\.\\d+)\\d+') #decimal, keyword before
   )) 
 }
 if ((number_decimal_general == TRUE)&(number_type %in% c('proportion', 'probability'))) {
  patterns = c(patterns, c(
   paste0('(0| )\\.\\d+(?= ', phrases_after, ')'), #decimal, keyword after
   paste0('(?<=', phrases_before, ' (were|was|is|are)? approx(imately)? ?)(0| )\\.\\d+'), #decimal, keyword before with approximately
   paste0('(?<=', phrases_before, ' ?)(0| )\\.\\d+') #decimal, keyword before
   )) 
 }
 if ((number_decimal_general == TRUE)&(number_type %in% c('ratio', 'positive'))) {
  patterns = c(patterns, c(
   paste0('(\\d+| )\\.\\d+(?= ', phrases_after, ')'), #decimal, keyword after
   paste0('(?<= ', phrases_before, ' (were|was|is|are)? approx(imately)? ?)(\\d+| )\\.\\d+'), #decimal, keyword before, with approximately
   paste0('(?<= ', phrases_before, ' ?)(\\d+| )\\.\\d+') #decimal, keyword before
   )) 
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
 #preceded by of|set at|set to|equal to|.?= .?|greater than|.?>.?
 #this will avoid picking up e.g., 0.05 for power in "significance level of 0.05, power..."
 #also ignore sentence parts where the keyword is followed by "and" 
 sentence_parts = vector(mode = 'character', length = 0)
 for (i in 1:length(keywords)) {
 for (j in 1:length(sentences)) {
  sentence_parts = c(sentence_parts, unlist(str_extract_all(string = sentences[j], pattern = c(
   paste0('(?<!(is|of|set at|set to|equal to|.?= .?|greater than|.?>.?) ?)(\\d+| )\\.\\d+\\D+', keywords[i]), #decimal, keyword after
   paste0(keywords[i], '(?! and)\\D+(\\d+| )\\.\\d+( percent|.?|%)'), #decimal, keyword before
   paste0('(?<!(is|of|set at|set to|equal to|.?= .?|greater than|.?>.?)) \\d+\\D+', keywords[i]), #integer, keyword after
   paste0(keywords[i], '(?! and)\\D+\\d+(?!\\.)( percent|.?|%)'))))) #integer, keyword before
 }
 }
 
 if (length(sentence_parts) > 0) {
 
 #remove sentence parts that contain breaking words
 if ((breaking_words[1] != '')|(!is.na(breaking_words[1]))) {
  for (i in 1:length(breaking_words)) {
  sentence_parts = sentence_parts[is.na(str_match(string = sentence_parts, pattern = breaking_words[i])[,1])]
  } 
 }
 
 # AGB: to avoid error if there are no parts after code above
 if(length(sentence_parts) == 0){sentence_parts = ' '} 
 
 #extract number within remaining sentence parts 
 #extract percentages
 patterns = vector(mode = 'character', length = 0)
 if ((number_integer_percent == TRUE)|(number_decimal_percent == TRUE)) {
  
  if (number_integer_percent == TRUE) {
  patterns = c(patterns, c(
   paste0(' \\d+(?!\\.)(?= (\\%|per.?cent)\\D+', keywords, ')'), #number % word, keyword after
   paste0('(?<=', keywords, '\\D{0,100})\\d+(?!\\.)(?= (\\%|per.?cent))') #number % sign, keyword before (up to 100 characters, limit required by look-behind)
  ))
  }
  if (number_decimal_percent == TRUE) {
  patterns = c(patterns, c(
   paste0('(\\d+| )\\.\\d+(?= (\\%|per.?cent)\\D+', keywords, ')'), #number % word, keyword after
   paste0('(?<=', keywords, '\\D{0,100})(\\d+| )\\.\\d+(?= (per.?cent|\\%))') #number % sign, keyword before (up to 100 characters, limit required by look-behind)
   )) 
  }
  for (j in 1:length(sentence_parts)) numbers_found = c(numbers_found, as.numeric(unlist(str_extract_all(string = sentence_parts[j], pattern = patterns))))
  numbers_found = numbers_found/100
  
  #remove percentages from sentence_parts before looking further
  sentence_parts = as.list(str_replace_all(string = sentence_parts, pattern = '\\d+\\.\\d+.?per.?cent', replacement = ''))
  sentence_parts = as.list(str_replace_all(string = sentence_parts, pattern = '\\d+.?per.?cent', replacement = ''))
  
 }
 
 #extract other numbers, append to list of percentages
 patterns = vector(mode = 'character', length = 0)
 if ((number_integer_general == TRUE)|(number_decimal_general == TRUE)) {
  
  if (number_integer_general == TRUE) {
  patterns = c(patterns, c(paste0(' \\d+(?!\\.)(?= \\D+', keywords, ')'), #integer, keyword after
         paste0('(?<=', keywords, '\\D{0,100})(?!\\d+\\.\\d+)\\d+'))) #integer, keyword before (up to 100 characters, limit required by look-behind)
  }
  if ((number_decimal_general == TRUE)&(number_type %in% c('proportion', 'probability'))) {
  patterns = c(patterns, c(paste0('(0| )\\.\\d+(?= \\D+', keywords, ')'), #decimal, keyword after
         paste0('(?<=', keywords, '\\D{0,100})(0| )\\.\\d+'))) #decimal, keyword before (up to 100 characters, limit required by look-behind)
  }
  if ((number_decimal_general == TRUE)&(number_type %in% c('ratio', 'positive'))) {
  patterns = c(patterns, c(paste0('(\\d+| )\\.\\d+(?= \\D+', keywords, ')'), #decimal, keyword after
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

## big loop ##
N = nrow(studies)
update = NULL
for (k in 1:N){
 
 if (k %in% seq(0,N,50)) print(k) # update
 
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
 stats_section = str_replace_all(string = stats_section, pattern = '1(?= ..?side)', replacement = 'one')
 stats_section = str_replace_all(string = stats_section, pattern = '1(?= ..?tail)', replacement = 'one')
 stats_section = str_replace_all(string = stats_section, pattern = '2(?= ..?side)', replacement = 'two')
 stats_section = str_replace_all(string = stats_section, pattern = '2(?= ..?tail)', replacement = 'two')
 
 #replace digits in unit measures with words ***add more as find them***
 stats_section = str_replace_all(string = stats_section, pattern = '/cm2', replacement = ' per cm squared')
 stats_section = str_replace_all(string = stats_section, pattern = '/m2', replacement = ' per meter squared')
 stats_section = str_replace_all(string = stats_section, pattern = '/cm3', replacement = ' per cm cubed')
 stats_section = str_replace_all(string = stats_section, pattern = '/m3', replacement = ' per meter cubed')
 
 #replace decimals starting with a dot (missing zero) with 0. ***test this***
 stats_section = str_replace_all(string = stats_section, pattern = ' (?= \\.\\d+)', replacement = ' 0')

 #flag for cohen's d, hedges g, etc. to try setting sd = 1 if empty
 flag_set_sd1 = FALSE
 flag_set_sd1 = ifelse(sum(str_count(string = stats_section, pattern = c('standardi(s|z)ed.mean.differences?', 'standardi(s|z)ed.differences?', 'cohen.?s.?f', 'cohen.?s.?d', 'hedges.?g', ' f.?=', ' f2.?=', ' d.?=', ' g.?=')) > 0), TRUE, FALSE) 

 #strip et al plus year [20xx/19xx]
 stats_section = str_replace_all(string = stats_section, pattern = ' et al,? \\d+', replacement = ' ')
 
 #split stats_section into sentences
 sentences = unlist(str_split(string = stats_section, pattern = c('(?<=\\.|\\?|\\!)\\s(?=[a-z])')))
 
 #***to do: fix common typos in keywords before using find_num_assoc_with_word()*** - e.g., statically significant
 
 # AGB: get the number of groups
 # still working on this
 keywords = c('equal (sized )?groups','(randomi(s|z)ed )?groups','(randomi(s|z)ed )?arms','fiddlesticks')
 phrases_position = c('after','after', 'after', 'before') # need at least one before
 n_groups = find_num_assoc_with_word(
  sentences,
  phrases = keywords,
  keywords = keywords,
  phrases_position = phrases_position,
  breaking_words = c(significance, '\\balpha\\b', type_1, '\\bpower\\b', '\\bbeta\\b', type_2, 'confidence' , '\\band\\b', '\\bwith\\b', '\\bthat\\b', '\\bwhich\\b'),
  number_integer_general = TRUE, number_integer_percent = FALSE, 
  number_decimal_general = FALSE, number_decimal_percent = FALSE, 
  number_type = "positive", number_likely_limits = c(1, 10))
 # use stated ratios to estimate number of groups
 if(is.na(n_groups[1]) == TRUE){
  if(str_detect(stats_section, 'randomi(s|z)ed (ratio )?[1-3]:[1-3]\\b') == TRUE){n_groups = 2}
  if(str_detect(stats_section, 'randomi(s|z)ed (ratio )?[1-3]:[1-3]:[1-3]\\b') == TRUE){n_groups = 3}
  if(str_detect(stats_section, 'randomi(s|z)ed (ratio )?[1-3]:[1-3]:[1-3]:[1-3]\\b') == TRUE){n_groups = 4}
 }
 # default of 2 groups if not stated
 if(is.na(n_groups[1]) == TRUE){n_groups = 2} 
 
 # extract sample size, AGB: include `n = `
 keywords = 'sample size'
 link_phrases = c('\\bof\\b', 'set at', 'set to', 'equal to', '.?= .?', 'greater than', 'greater than or equal to', '.?>.?', '.?>= .?')
 particpant_phrases = c('participants', 'patients', 'individuals', 'people', 'children', 'mothers', 'neonates', 'men', 'women', 'subjects', 'adults', 'rats', 'mice', 'sheep', 'cows', 'dogs')
 particpant_phrases = paste('(\\w+-\\w+ |\\w+ )?', particpant_phrases, sep = '') # add optional any word before phrases, e.g., "10 hospitalised patients", first option has hyphens
 phrases = c('sample size', 'sample', 
       paste('sample size', link_phrases), 
       paste('sample', link_phrases), 
       paste('\\bn\\b', link_phrases), particpant_phrases)
 phrases_position = c('either', 'either', 
           rep('before', length(link_phrases)), 
           rep('before', length(link_phrases)), 
           rep('before', length(link_phrases)), 
           rep('after', length(particpant_phrases)))
 sample.size = find_num_assoc_with_word(
 sentences,
 phrases = phrases,
 keywords = keywords,
 phrases_position = phrases_position,
 breaking_words = c(significance, '\\balpha\\b', type_1, '\\bpower\\b', '\\bbeta\\b', type_2, 'confidence'),
 number_integer_general = TRUE, number_integer_percent = FALSE, 
 number_decimal_general = FALSE, number_decimal_percent = FALSE, 
 number_type = "positive", number_likely_limits = c(NA, NA))
 
 # AGB: is the sample size per group?
 if(length(sample.size)>0){
  pattern = paste(sample.size, ' per\\b', sep='')
  per_group = str_detect(stats_section, pattern = pattern)
  # now increase sample size
  if(any(per_group)){
   mult = rep(1, length(sample.size)) # start with 1 as the multiplier
   if(any(per_group)){mult[which(per_group)] = n_groups} # only apply multiplier where 'per' was found
   sample.size = sample.size * mult # now apply multiplier
  }
 }
 sample.size = list(sample.size)
 n_groups = list(n_groups)
 
 #extract power
 keywords = c('power', '1 ?- ?beta')
 link_phrases = c('\\bof\\b', 'set at', '\\bat\\b', 'set to', 'equal to', '.?= .?', 'greater than' , 'greater than or equal to', '.?>.?', '.?>= .?')
 phrases = c(keywords, 
       paste(keywords[1], link_phrases),
       paste(keywords[2], link_phrases))
 phrases_position = c('either', 'either', 
           rep('before', length(link_phrases)),
           rep('before', length(link_phrases)))
 power.vec = find_num_assoc_with_word(
 sentences,
 phrases = phrases,
 keywords = keywords,
 phrases_position = phrases_position,
 breaking_words = c(significance, '\\balpha\\b', type_1, 'confidence', '\\band\\b', '\\bwith\\b', '\\bthat\\b', '\\bwhich\\b'),
 number_integer_general = FALSE, number_integer_percent = TRUE, 
 number_decimal_general = TRUE, number_decimal_percent = TRUE, 
 number_type = "probability", number_likely_limits = c(0.5, 1))
 
 #extract type 2 error
 keywords = c('\\bbeta\\b', type_2)
 link_phrases = c('\\bof\\b', 'set at', '\\bat\\b', 'set to', 'equal to', '.?= .?', 'less than' , 'less than or equal to', '.?<.?', '.?<=.?')
 phrases = c(keywords, 
    paste('\\bbeta\\b', link_phrases),
    paste(type_2_error, link_phrases))
 phrases_position = c('either', 'either', rep('before', length(link_phrases)), rep('before', length(link_phrases)))
 type2err.vec = find_num_assoc_with_word(
 sentences, 
 phrases = phrases, 
 keywords = keywords,
 phrases_position = phrases_position, 
 breaking_words = c(significance, '\\balpha\\b', type_1, 'confidence', '\\band\\b', '\\bwith\\b', '\\bthat\\b', '\\bwhich\\b'), 
 number_integer_general = FALSE, number_integer_percent = TRUE, 
 number_decimal_general = TRUE, number_decimal_percent = TRUE, 
 number_type = "probability", number_likely_limits = c(0, 0.5))
 
 #reported power
 power.used = list(unique(na.omit(c(power.vec, 1-type2err.vec))))
 power.used = ifelse(length(unlist(power.used)) > 0, power.used, list(NA))
 
 # extract significance level
 keywords = c(significance, '\\balpha\\b', type_1)
 link_phrases = c('\\bof\\b', 'set at', '\\bat\\b', 'set to', 'equal to', '.?= .?', 'less than' , 'less than or equal to', '.?<.?', '.?<=.?')
 phrases = c('p.?= .?', 'p.?<.?', ' a.?= .?', significance, 'significan(t|ce).level', '\\balpha\\b', type_1_error, 
    paste(significance, link_phrases), 
    paste('significan(t|ce).level', link_phrases), 
    paste('\\balpha\\b', link_phrases), 
    paste(type_1_error, link_phrases))
 phrases_position = c('before', 'before', 'before', 'either', 'either', 'either', 'either', 
      rep(rep('before', length(link_phrases)), 4))
 siglev.vec = find_num_assoc_with_word(
 sentences, 
 phrases = phrases, 
 keywords = keywords, 
 phrases_position = phrases_position, 
 breaking_words = c('\\bpower\\b', '\\bbeta\\b', type_2 , '\\band\\b', '\\bwith\\b', '\\bthat\\b', '\\bwhich\\b',
      '\\bmean\\b', 'standard deviation', '\\bsd\\b', 'coefficient', 'regression', '\\bdetect'), 
 number_integer_general = FALSE, number_integer_percent = TRUE, 
 number_decimal_general = TRUE, number_decimal_percent = TRUE, 
 number_type = "probability", number_likely_limits = c(0, 0.5))
 siglev.vec = list(siglev.vec)

 #correction
 keywords = c('correction')
 link_phrases = c('\\bof\\b', 'set at', 'set to', 'equal to', '.?= .?')
 phrases = c('correction', 
    paste('correction', link_phrases))
 phrases_position = c('either', rep('before', length(link_phrases)))
 correct.vec = find_num_assoc_with_word(
 sentences, 
 phrases = phrases, 
 keywords = keywords, 
 phrases_position = phrases_position, 
 breaking_words = c('drop(.out)?', 'loss', 'attrition', 'withdrawal', 'follow.?up'), 
 number_integer_general = FALSE, number_integer_percent = TRUE, 
 number_decimal_general = FALSE, number_decimal_percent = FALSE, 
 number_type = "positive", number_likely_limits = c(NA, NA))
 if (length(correct.vec) != 0) correct.vec = correct.vec*100 #return as %
 correct.vec = list(correct.vec) 
 
 #loss to follow up/attrition
 link_phrases = c('\\bof\\b', 'set at', 'set to', 'equal to', '.?= .?')
 phrases = c('los(s|t) to follow.?up', 'follow.?up loss', 'withdrawal', 'attrition', 'drop.?out',
    paste('loss?t? to follow.?up', link_phrases), 
    paste('follow.?up loss', link_phrases), 
    paste('withdrawal', link_phrases),
    paste('attrition', link_phrases),
    paste('drop.?out', link_phrases))
 phrases_position = c('either', 'either', 'either', 'either', 'either', 
      rep(c('before', 'before', 'before', 'before', 'before'), length(link_phrases)))
 loss.vec = find_num_assoc_with_word(
 sentences, 
 phrases = phrases, 
 keywords = c('loss', 'lost', 'withdrawal', 'attrition'), 
 phrases_position = phrases_position, 
 breaking_words = c('drop', ' and ', ' with '), 
 number_integer_general = FALSE, number_integer_percent = TRUE, 
 number_decimal_general = TRUE, number_decimal_percent = FALSE, 
 number_type = "positive", number_likely_limits = c(NA, NA))
 if (length(loss.vec) != 0) loss.vec = loss.vec*100 #return as %
 loss.vec = list(loss.vec)
 
 #compliance/adherence: drop-ins
 link_phrases = c('\\bof\\b', 'set at', 'set to', 'equal to', '.?= .?')
 phrases = c('drop.?ins?', 'drop.?in.rate', 'dropping.?in', 
    paste('drop.?ins?', link_phrases), 
    paste('drop.?in.rate', link_phrases), 
    paste('dropping.?in', link_phrases))
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
 link_phrases = c('\\bof\\b', 'set at', 'set to', 'equal to', '.?= .?')
 phrases = c('drop.?outs?', 'drop.?out.rate', 'dropping.?out','non.?compliance','non.?adherence',
    paste('drop.?outs?', link_phrases), 
    paste('drop.?out rate', link_phrases), 
    paste('dropping.?out', link_phrases),
    paste('non.?compliance', link_phrases),
    paste('non.?adherence', link_phrases))
 phrases_position = c('either', 'either', 'either', 'either', 'either', 
      rep(c('before', 'before', 'before', 'before', 'before'), 5))
 dropout.vec = find_num_assoc_with_word(
 sentences, 
 phrases = phrases, 
 keywords = c('drop.?outs?', 'drop.?out.rate', 'dropping.?out','non.?compliance','non.?adherence'), 
 phrases_position = phrases_position, 
 breaking_words = c('drop.?ins?', 'drop.?in.rate', 'dropping.?in', ' and ', ' with '), 
 number_integer_general = FALSE, number_integer_percent = TRUE, 
 number_decimal_general = TRUE, number_decimal_percent = FALSE, #assuming will only be reported as integer% or 0<decimal<1
 number_type = "positive", number_likely_limits = c(NA, NA)) 
 if (length(dropout.vec) != 0) dropout.vec = dropout.vec*100 #return as %
 dropout.vec = list(dropout.vec)
 
 #unequal groups ***to do*** 
 #need params unequal_groups = FALSE, unequal_q1 = 0.5, unequal_q2 = 0.5
 
 #difference in means, AGB: added clinically meaningful difference and acronym
 link_phrases = c('\\bof\\b', 'equal to', '.?= .?', 'in')
 keywords = c('differences?', 'difference in means?', 'mean differences?', 'difference in averages?', 'average differences?', 'effect (estimates?|sizes?)', 'effects? ','cohen.?s.?f', 'cohen.?s.?d', 'hedges.?g', 'expected.improvements?', 'clinically (important|meaningful) difference', 'mcid') #how to differentiate this from diff in proportions?
 phrases = c(keywords,
    paste(keywords[1], link_phrases), 
    paste(keywords[2], link_phrases), 
    paste(keywords[3], link_phrases), 
    paste(keywords[4], link_phrases), 
    paste(keywords[5], link_phrases), 
    paste(keywords[6], link_phrases), 
    paste(keywords[7], link_phrases), 
    paste(keywords[8], link_phrases), 
    paste(keywords[9], link_phrases), 
    paste(keywords[10], link_phrases), 
    paste(keywords[11], link_phrases), 
    paste(keywords[12], link_phrases), 
    paste(keywords[13], link_phrases),
    '\\bf.?= ','\\bd.?= ','\\bg.?= ')
 phrases_position = c('either', 'either', 'either', 'either', 'either', 'either', 'either', 'either', 'either', 'either', 'either', 'either', 'either', 
      rep(rep('before', length(link_phrases)), length(keywords)),
      'before', 'before', 'before')
 expect_diff = find_num_assoc_with_word(
 sentences, 
 phrases = phrases, 
 keywords = keywords, 
 phrases_position = phrases_position, 
 breaking_words = c('proportion', 'power', type_2, significance, 'alpha', type_1, ' and ', ' with ', 'confidence', 'sample', 'sample size', 'standard', 'deviation', ' s.?d.? ', 'et al', paste(c('\\bp.?'), c('=', '<', '<=', '>', '>=')), paste0(c('\\ba.?'), c('=', '<', '<=', '>', '>='))), 
 number_integer_general = TRUE, number_integer_percent = TRUE, 
 number_decimal_general = TRUE, number_decimal_percent = TRUE, 
 number_type = "positive", number_likely_limits = c(NA, NA)) 
 expect_diff = list(expect_diff)
 
 #standard deviation of difference
 sd_phrases = c('standard deviation of the (measurement|difference)', 'standard deviation', '\\bs\\.d.?', '\\bsd.?', 'std.? ?dev.?')
 link_phrases = c('\\bwas\\b', '\\bis\\b', '\\bof\\b', 'equal to', '.?= .?')
 phrases = c(sd_phrases,
    paste(sd_phrases[1], link_phrases), 
    paste(sd_phrases[2], link_phrases),
    paste(sd_phrases[3], link_phrases),
    paste(sd_phrases[4], link_phrases),
    paste(sd_phrases[5], link_phrases))
 phrases_position = c(rep('either', length(sd_phrases)),
      rep(c('before', 'before', 'before','before'), length(sd_phrases)))
 sd_diff = find_num_assoc_with_word(
 sentences, 
 phrases = phrases, 
 keywords = sd_phrases,
 phrases_position = phrases_position, 
 breaking_words = c('mean', 'average', 'power', type_2, significance, 'alpha', type_1, ' and ', ' with ', 'confidence', 'sample', 'size', 'differences?', 'effect', 'et al'), 
 number_integer_general = TRUE, number_integer_percent = TRUE, 
 number_decimal_general = TRUE, number_decimal_percent = TRUE, 
 number_type = "positive", number_likely_limits = c(NA, NA)) 
 if ((flag_set_sd1 == TRUE)&(all(is.na(sd_diff)))) sd_diff = 1 
 sd_diff = list(sd_diff)
 
 #relative risk reduction
 keywords = c('relative risk reductions?', 'rrrs?', 'relative reductions?') #need relative risk increase as well
 link_phrases = c('\\bis\\b', '\\bwas\\b', '\\bof\\b', 'equal to', '.?= .?')
 phrases = c('relative risk reductions?', 'rrrs?', 'relative reductions?', 
    paste('relative risk reductions?', link_phrases),
    paste('relative reductions?', link_phrases),
    paste('rrrs?', link_phrases))
 phrases_position = c('either', 'either', 'either', rep(rep('before', length(link_phrases)), 3))
 rrr = find_num_assoc_with_word(
 sentences, 
 phrases = phrases, 
 keywords = keywords, 
 phrases_position = phrases_position, 
 breaking_words = c('differences?', 'absolute', 'means?', 'averages?', 'power', type_2, significance, 'alpha', type_1, ' and ', ' with '), 
 number_integer_general = FALSE, number_integer_percent = TRUE, 
 number_decimal_general = TRUE, number_decimal_percent = TRUE, 
 number_type = "positive", number_likely_limits = c(NA, NA)) 
 rrr = list(rrr)
 
 #absolute risk reduction/increase
 keywords = c('absolute risk (reductions?|increases?)', 'arrs?', 'absolute (reductions?|increases?)') #
 link_phrases = c('\\bwas\\b', '\\bis\\b', '\\bof\\b', 'equal to', '.?= .?')
 phrases = c(keywords,
    paste(keywords[1], link_phrases),
    paste(keywords[2], link_phrases),
    paste(keywords[3], link_phrases))
 phrases_position = c('either', 'either', 'either', rep(rep('before', length(link_phrases)), 3))
 arr = find_num_assoc_with_word(
 sentences, 
 phrases = phrases, 
 keywords = keywords, 
 phrases_position = phrases_position, 
 breaking_words = c('differences?', 'relative', 'means?', 'averages?', 'power', type_2, significance, 'alpha', type_1, ' and ', ' with '), 
 number_integer_general = FALSE, number_integer_percent = TRUE, 
 number_decimal_general = TRUE, number_decimal_percent = TRUE, 
 number_type = "positive", number_likely_limits = c(NA, NA)) 
 arr = list(arr)
 
 # #***continue to add all params required by sample size functions:***
 #
 # expect_diff = NA, sd_diff = NA, mu0 = NA, mu1 = NA, sd0 = NA, sd1 = NA, n0 = NA, n1 = NA, 
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
   n_groups = n_groups,
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
 frame = NULL # tidy up
 
}

# data edits
studies = mutate(update) 



testing = function(){
#******TESTING******
#extract params for excel
studies_7 <- studies
check_params_all7 <- data.frame(number = studies[,"number"],
        stats_section = studies[,"stats_section"],
        TESTnumsides = testset[,"one_or_two_sided"],MYnumsides = studies["numsides"],
        TESTalpha = testset[,"alpha"],MYalpha = studies["alpha"],
        TESTpower = testset[,"power"],MYpower = studies["power"],
        Ssize_target = studies["samplesize_target"],SSize_actual = studies["samplesize_actual"],
        TESTssize = testset[,"total_sample_size"],MYssize = studies["n_extracted"],
        TESTexpect_diff = testset[,"expect_diff"],MYexpect_diff = studies["expect_diff"],
        TESTsd_diff = testset[,"sd_diff"],MYsd_diff = studies["sd_diff"],
        TESTdropout = testset[,"dropout"],MYdropout = studies["dropout_pct"],MYlossfollowup = studies["loss_followup_pct"])
#******************
}


#clean up
rm(stats_section, this_study, keywords, phrases, phrases_position, sentences, sample.size, arr, rrr,
 siglev.vec, power.vec, power.used, type2err.vec, numsides, numsides.both, 
 correct.vec, loss.vec, dropin.vec, dropout.vec, expect_diff, sd_diff)

# save
save(studies, excluded, file = file_params)

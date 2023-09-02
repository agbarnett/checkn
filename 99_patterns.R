# 99_patterns.R
# patterns used in string detection
# September 2023

type_1 = 'type.?(i |1|one)' # needs space otherwise gets confused with type 2
type_1_error = paste(type_1, '.error', sep='')
type_2 = 'type.?(ii|2|two)'
type_2_error = paste(type_2, '.error', sep='')
significance = 'significan(t|ce)'
years = '19[0-9][0-9]|20[0-2][0-9]' # years used in citations
# 3_flow_diagram.R
# CONSORT flow diagram of exclusions
# September 2023
library(diagram)
library(dplyr)

# which data to use
data_sources = c('original','extra')
data_source = data_sources[2]
source('1_which_data.R')

# original number of studies
load(in_data) # from 0_read_stats_sections_extra.R
N_start = nrow(studies) 
# excluded at pre-processing stage
load(rds_file) # from 1_process_anzctr_stats_section.R
stats_pre = group_by(excluded, reason) %>% 
  tally()
# excluded 
load(file_processed) # from 2_find_sample_size.R
stats_sample = group_by(excluded, reason) %>% 
  tally()
# check
if(nrow(studies) + sum(stats_pre$n) + sum(stats_sample$n) != N_start){cat('Error in numbers\n')}
n_remain = nrow(studies)

# labels, big N for institutions, little n for RIAs
l1 = paste('Number of entries\nscreened (N=',  format(N_start, big.mark=','), ')', sep='') # 
l2 = paste('Excluded\nempty methods (N=', format(stats_pre$n, big.mark=','), ')', sep='') # 
l3 = paste('Excluded\nno sample size (N=', format(stats_sample$n, big.mark=','), ')', sep='') # 
l4 = paste('Analysed\n(N=', format(n_remain, big.mark=','), ')', sep='') # 
null = ''
labels = c(l1, l2, l3, l4, null, null)
n.labels = length(labels)

#
### make data frame of box chars
# box.prop = length/width ratio, so > 1 = tall and thin
frame = read.table(sep='\t', stringsAsFactors=F, skip=0, header=TRUE, text='
i	x	y	box.col	box.type	box.prop	box.size
1	0.5	0.94	white	square	0.34	0.135
2	0.75	0.69	white	square	0.33	0.15
3	0.75	0.40	white	square	0.33	0.15
4	0.5	0.15	white	square	0.34	0.13
5	0.5	0.69	transparent	square	0.0001	0.0001
6	0.5	0.40	transparent	square	0.0001	0.0001
')
# positions:
pos = as.matrix(subset(frame, select=c(x, y)))
# joins between boxes
M = matrix(nrow = nrow(frame), ncol = nrow(frame), byrow = TRUE, data = 0)
M[4, 1] = "' '"
M[2, 5] = "' '"
M[3, 6] = "' '"
# colours
tcol = rep('black', nrow(frame))

## make figure 
jpeg('figures/consort.flow.jpg', width=7.5, height=7, units='in', res=400, quality = 100)
par(mai=c(0,0.04,0.04,0.04))
plotmat(M, pos = pos, name = labels, lwd = 1, shadow.size=0, curve=0, arr.pos = 0.25,
        box.lwd = 2, cex.txt = 1, box.size = frame$box.size, box.col=frame$box.col,
        box.type = frame$box.type, box.prop = frame$box.prop, txt.col = tcol)
dev.off()
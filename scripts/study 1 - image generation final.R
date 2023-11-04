##############################
# study 1 - image generation
# 
# ci creation
##############################

library(xlsx)
library(rcicr)
library(tidyverse)

base_img = 'base' # averageBWMF.jpeg
rDat = 'stimuli/rcic_seed_1_time_Mar_25_2022_14_36.Rdata'
load(rDat)

df <- read_csv('data files/image_gen_data.csv') %>%
  mutate_at(.vars = c('participant_id', 'condition'),
            .funs = as.factor)

sapply(df, class)


# separate out the different conditions to generate CIs/anti-CIs
bullshitter <- df %>%
  filter(condition == 'bullshitter')

bullshittee <- df %>%
  filter(condition == 'bullshittee')


# create CIs
# bullshitter
batchGenerateCI2IFC(as.data.frame(bullshitter),
                    by = 'participant_id',
                    stimuli = 'stim_num',
                    responses = 'response',
                    baseimage = base_img,
                    rdata = rDat,
                    targetpath = 'cis/bullshitter/true_cis')

# anti-cis
batchGenerateCI2IFC(as.data.frame(bullshitter),
                    by = 'participant_id',
                    stimuli = 'stim_num',
                    responses = 'response',
                    baseimage = base_img,
                    rdata = rDat,
                    antiCI = T,
                    targetpath = 'cis/bullshitter/anti_cis')


# bullshittee
batchGenerateCI2IFC(as.data.frame(bullshittee),
                    by = 'participant_id',
                    stimuli = 'stim_num',
                    responses = 'response',
                    baseimage = base_img,
                    rdata = rDat,
                    targetpath = 'cis/bullshittee/true_cis')

# anti-cis
batchGenerateCI2IFC(as.data.frame(bullshittee),
                    by = 'participant_id',
                    stimuli = 'stim_num',
                    responses = 'response',
                    baseimage = base_img,
                    rdata = rDat,
                    antiCI = T,
                    targetpath = 'cis/bullshittee/anti_cis')


# group level
batchGenerateCI2IFC(as.data.frame(df),
                    by = 'condition',
                    stimuli = 'stim_num',
                    responses = 'response',
                    baseimage = base_img,
                    rdata = rDat,
                    targetpath = 'cis/group_level/true_cis')

# anti-cis
batchGenerateCI2IFC(as.data.frame(df),
                    by = 'condition',
                    stimuli = 'stim_num',
                    responses = 'response',
                    baseimage = base_img,
                    rdata = rDat,
                    antiCI = T,
                    targetpath = 'cis/group_level/anti_cis')


# make a table of all IDs for later use
bullshitter_ids <- substr(
  list.files(path = 'cis/bullshitter/true_cis'),
  21, 28
)

bullshittee_ids <- substr(
  list.files(path = 'cis/bullshittee/true_cis'),
  21, 28
)

# bser_true_ids <- substr(
#   list.files(path = 'cis/bullshitter/true_cis'),
#   21, 28
# )
# 
# bser_anti_ids <- substr(
#   list.files(path = 'cis/bullshitter/anti_cis'),
#   21, 28
# )
# 
# bsee_true_ids <- substr(
#   list.files(path = 'cis/bullshittee/true_cis'),
#   21, 28
# )
# 
# bsee_anti_ids <- substr(
#   list.files(path = 'cis/bullshittee/anti_cis'),
#   21, 28
# )



# grab participant ID values to tie to stims
ids_df <- data.frame(
  p_id = c(rep(bullshitter_ids, 2), rep(bullshittee_ids, 2)),
  ci_id = paste0(
    'stim', str_pad(1:196, width = 3, side = 'left', pad = '0')
  ),
  ci_group = rep(c('bullshitter', 'bullshittee'), each = 98),
  image_type = rep(c('true_ci', 'anti_ci'), each = 49)
)

# write.xlsx(ids_df, 'participant ids.xlsx', row.names = F)

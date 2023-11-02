##############################
# study 1 - image generation
# 
# ci creation
##############################

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

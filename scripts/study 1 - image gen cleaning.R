##################################
# study 1 - image generation task
# data cleaning script
##################################

library(rcicr)
library(tidyverse)

# load data files
df_raw <- list.files(path = 'data files/reverse_correlation_image_gen_data',
                 pattern = '*.csv',
                 full.names = T) %>%
  map_df(~read_csv(., col_types = cols(.default = 'c')))

colnames(df_raw)


df <- df_raw %>%
  select(
    participant_id,
    condition,
    inverseImg,
    originalImg,
    invLocX,
    oriLocX,
    image_choice.keys
  ) %>%
  rename('response' = 'image_choice.keys') %>%
  na.omit() %>%
  mutate(
    response = case_when(
      oriLocX == -300 & response == 'left' ~ 1,
      oriLocX == 300 & response == 'right' ~ 1,
      oriLocX == -300 & response == 'right' ~ -1,
      oriLocX == 300 & response == 'left' ~ -1
    ),
    condition = if_else(condition == 1, 'bullshitter', 'bullshittee'),
    stim_num = as.numeric(substr(originalImg, 23, 25)),
    participant_id = str_pad(participant_id,
                             width = 7,
                             side = 'left',
                             pad = 0),
    participant_id = str_pad(participant_id,
                             width = 8,
                             side = 'left',
                             pad = 'P')
  ) %>%
  select(
    participant_id,
    condition,
    stim_num,
    response
  )

bad_ps <- df %>% 
  group_by(participant_id) %>% 
  count(condition) %>% 
  filter(n < 300) %>%
  select(participant_id)

df_final <- df %>%
  filter(!(participant_id %in% bad_ps$participant_id))

df_final %>%
  group_by(condition) %>%
  count() %>%
  mutate(n = n/300)

# write.csv(df_final, 'data files/image_gen_data.csv', row.names = F)

total_n <- 98

df_raw %>%
  select(age_resp.keys) %>%
  na.omit() %>%
  # rename('age' = 'age_resp.keys')
  group_by(age_resp.keys) %>%
  count() %>%
  mutate(age_resp.keys = case_when(age_resp.keys == 'a' ~ '18-27',
                                   age_resp.keys == 'b' ~ '28-37'),
         prop = n / total_n)

df_raw %>%
  select(sex_resp.keys) %>%
  na.omit() %>%
  # rename('age' = 'age_resp.keys')
  group_by(sex_resp.keys) %>%
  count() %>%
  mutate(prop = n / total_n)

df_raw %>%
  select(race_resp.keys) %>%
  na.omit() %>%
  # rename('age' = 'age_resp.keys')
  group_by(race_resp.keys) %>%
  count() %>%
  mutate(prop = n / total_n)

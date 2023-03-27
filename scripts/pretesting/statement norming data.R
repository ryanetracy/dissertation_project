#######################################
# norming data for bullshit statements
# corporate and social bullshit
#######################################

source('load_and_install.R')

pckgs <- c('psych', 'rstatix', 'tidyverse', 'ggcorrplot', 'correlation')

load_and_install(package_list = pckgs)


df <- read.csv('data/bullshit statements norming.csv')

colnames(df)

df <- df %>%
  filter(Progress >= 89) %>%
  select(!(contains(c('Date',
                     'Status',
                     'Address',
                     'Duration',
                     'Progress',
                     'Finished',
                     'Recipient',
                     'Location',
                     'Reference',
                     'Channel',
                     'Language',
                     'consent',
                     '_DO',
                     '_TEXT')
                   ))
         ) %>%
  rename('subj' = 'ResponseId')

rm(nums1to10, stim_nums)

# new vector for column names
bs_names <- paste0(
  rep('stim_'),
  rep(1:50, each = 13),
  rep(c('corp', 'socl'), each = 325),
  rep(c('prof',
        'deep',
        'mean',
        'belv',
        'insp',
        'conv',
        'motv',
        'undr',
        'wise',
        'pers',
        'prog',
        'infl',
        'spir'))
)

names(df)[2:651] <- bs_names

# long form
df_l <- df %>%
  select(-c('bot_check', 'age', 'gender', 'race')) %>%
  pivot_longer(cols = stim_1corpprof:stim_50soclspir,
               names_to = 'stimID',
               values_to = 'rating') %>%
  separate(col = 'stimID', into = c('stimID', 'trait'), sep = -4) %>%
  separate(col = 'stimID', into = c('stimID', 'bs_type'), sep = -4) %>%
  pivot_wider(names_from = 'trait',
              values_from = 'rating') %>%
  rename('profound' = 'prof',
         'meaningful' = 'mean',
         'believable' = 'belv',
         'inspirational' = 'insp',
         'convincing' = 'conv',
         'motivational' = 'motv',
         'understandable' = 'undr',
         'persuasive' = 'pers',
         'progressive' = 'prog',
         'influential' = 'infl',
         'spiritual' = 'spir') %>%
  mutate(bs_type = ifelse(bs_type == 'corp', 'corporate', 'social'))


bs_rates <- df_l %>%
  group_by(stimID, bs_type) %>%
  summarize(
    profound = mean(profound, na.rm = T),
    deep = mean(deep, na.rm = T),
    meaningful = mean(meaningful , na.rm = T),
    believable = mean(believable, na.rm = T),
    inspirational = mean(inspirational, na.rm = T),
    convincing = mean(convincing, na.rm = T),
    motivational = mean(motivational, na.rm = T),
    understandable = mean(understandable, na.rm = T),
    wise = mean(wise, na.rm = T),
    persuasive = mean(persuasive, na.rm = T),
    progressive = mean(progressive, na.rm = T),
    influential = mean(influential, na.rm = T),
    spiritual = mean(spiritual, na.rm = T)
  )

fact_anal <- fa(bs_rates[3:15], nfactors = 2)
fact_anal

# explore item correlations
item_corrs <- corr.test(bs_rates[3:15])
item_corrs

ggcorrplot(item_corrs$r,
           type = 'lower',
           p.mat = item_corrs$p,
           lab = T)
# profound, inspirational, persuasive might be the best to pick from









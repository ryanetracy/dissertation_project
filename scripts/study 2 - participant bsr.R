##################################
# study 2 - bullshit receptivity 

# participants' bsr
# bullshitter true-ci vs. anti-ci
# corporate bullshit vs. quotes
##################################


url = 'https://github.com/ryanetracy/misc_functions/blob/main/misc_functions.R?raw=TRUE'
devtools::source_url(url)

pckgs <- c(
  'psych',
  'lme4',
  'lmerTest',
  'effectsize',
  'parameters',
  'interactions',
  'rstatix',
  'FactoMineR',
  'factoextra',
  'ggcorrplot',
  'tidyverse',
  'ggrepel',
  'naniar',
  'simputation'
)

package_loader(pckgs)


# data
df1 <- read.csv('data files/study 2 data set 1.csv')
df2 <- read.csv('data files/study 2 data set 2.csv')

df_raw <- rbind(df1, df2)

colnames(df_raw)

df_raw %>%
  group_by(Progress) %>%
  count()

df_raw <- df_raw %>% 
  rename(block = FL_10_DO) %>%
  mutate(
    dup_response_id = duplicated(df_raw$ResponseId),
    dup_id = duplicated(df_raw$id)
  ) %>%
  filter(Progress == 100) %>% 
  filter(consent == 1) %>%
  filter(dup_response_id == F) %>%
  filter(dup_id == F) %>%
  select(-contains(c('dup_response_id', 'dup_id')))

demos <- df_raw %>%
  select(age, gender, race)

total_n <- nrow(demos)

demos %>% get_summary_stats(age, type = 'mean_sd')

demos %>%
  group_by(gender) %>% 
  count() %>%
  mutate(
    gender = case_when(
      gender == 1 ~ 'male',
      gender == 2 ~ 'female',
      gender == 3 ~ 'nonbinary',
      gender == 4 ~ 'other'
    ),
    prop = n / total_n
  )

demos %>%
  group_by(race) %>%
  count() %>%
  mutate(
    race = case_when(
      race == 1 ~ 'asian',
      race == 2 ~ 'black',
      race == 3 ~ 'latino',
      race == 4 ~ 'native american',
      race == 5 ~ 'middle eastern',
      race == 6 ~ 'pacific islander',
      race == 7 ~ 'white',
      race == 8 ~ 'biracial',
      race == 9 ~ 'not listed',
      TRUE ~ 'no response'
    ),
    prop = n / total_n
  )


# grab necessary columns

df_clean <- df_raw %>%
  select(-contains(
    c(
      'Progress',
      'ResponseId',
      'consent',
      '_DO',
      '_TEXT',
      'SC0',
      'age',
      'gender',
      'race'
    )
  )) %>%
  select(id, block, X1_traits_1:X40_traits_3.1)

# rename columns
# (block 1: cols 3-122)
# (block 2: cols 123-242)

ci_nums <- readxl::read_xlsx('face stimuli/ci urls.xlsx', sheet = 2) %>%
  select(stim_id)

ci_nums <- ci_nums$stim_id[1:40]

new_names1 <- paste0(
  rep(ci_nums, each = 3),
  '_',
  rep(c('trueCI', 'antiCI'), each = 60),
  '_',
  rep(c('bullshit', 'quote'), each = 30),
  '_',
  rep(c('profound', 'inspirational', 'persuasive'))
)

new_names2 <- paste0(
  rep(ci_nums, each = 3),
  '_',
  rep(c('trueCI', 'antiCI'), each = 60),
  '_',
  rep(c('quote', 'bullshit'), each = 30),
  '_',
  rep(c('profound', 'inspirational', 'persuasive'))
)

names(df_clean)[3:122] <- new_names1
names(df_clean)[123:242] <- new_names2

b1_long <- df_clean %>%
  filter(block == 'faces_1') %>%
  select(id:stim094_antiCI_quote_persuasive) %>%
  pivot_longer(
    cols = stim002_trueCI_bullshit_profound:stim094_antiCI_quote_persuasive,
    names_to = c('stimID', 'ci_type', 'statement_type', 'rating'),
    names_sep = '_',
    values_to = 'resp'
  ) %>%
  pivot_wider(
    names_from = 'rating',
    values_from = 'resp'
  )

b2_long <- df_clean %>%
  filter(block == 'faces_2') %>%
  select(id, 
         block,
         stim002_trueCI_quote_profound:stim094_antiCI_bullshit_persuasive) %>%
  pivot_longer(
    cols = stim002_trueCI_quote_profound:stim094_antiCI_bullshit_persuasive,
    names_to = c('stimID', 'ci_type', 'statement_type', 'rating'),
    names_sep = '_',
    values_to = 'resp'
  ) %>%
  pivot_wider(
    names_from = 'rating',
    values_from = 'resp'
  )

df_long <- rbind(b1_long, b2_long)

# assess fit between variables
find_nas(df_long)

psych::corr.test(df_long[, c('profound', 'inspirational', 'persuasive')])
psych::alpha(df_long[, c('profound', 'inspirational', 'persuasive')])

# impute missing values and compute bsr index
df_long <- df_long %>%
  mutate(
    profound = impute_mean(profound),
    inspirational = impute_mean(inspirational),
    persuasive = impute_mean(persuasive),
    bsr = rowMeans(df_long[, c('profound', 'inspirational', 'persuasive')]),
    ci_c = if_else(ci_type == 'trueCI', 1, -1),
    st_c = if_else(statement_type == 'bullshit', 1, -1)
  )

# initial model
mod1 <- lmer(bsr ~ ci_c * st_c
             + (1|id)
             + (1|stimID),
             data = df_long)
model_summary_lmer(mod1)
# more receptive to statements from true (vs. anti) cis
# more receptive to quotes (vs. bullshit)

ss <- sim_slopes(mod1,
                 pred = ci_c,
                 modx = st_c)$slopes |> as.data.frame()

d_val <- t_to_d(t = ss$`t val.`, df = 12478.501, paired = T)

print(
  cbind(
    round(ss, 3),
    round(d_val, 3)
  )
)

# visualize
plot_colors <- c('#003366', '#006666', '#ffd600')

df_sum <- df_long %>%
  group_by(ci_type, statement_type) %>%
  get_summary_stats(bsr, type = 'mean_ci')

df_part <- df_long %>%
  group_by(id, ci_type, statement_type) %>%
  get_summary_stats(bsr, type = 'mean_ci')

df_part %>%
  ggplot(aes(ci_type, mean, fill = statement_type, color = statement_type)) +
  geom_violin(color = 'black',
              position = position_dodge(.9),
              alpha = .75) +
  geom_point(color = 'black',
             alpha = .5,
             position = position_jitterdodge(.15, .05, .9),
             shape = 4) +
  geom_point(data = df_sum,
             aes(ci_type, mean),
             position = position_dodge(.9),
             color = plot_colors[3],
             shape = 9,
             size = 3) +
  geom_errorbar(data = df_sum,
                aes(ci_type, mean, ymin = mean - ci, ymax = mean + ci),
                color = plot_colors[3],
                width = .15,
                position = position_dodge(.9)) +
  theme_light(base_size = 25) +
  scale_color_manual(values = c(plot_colors[1], plot_colors[2]),
                     labels = c('Bullshit Statements', 'Inspirational Quotes')) +
  scale_fill_manual(values = c(plot_colors[1], plot_colors[2]),
                    labels = c('Bullshit Statements', 'Inspirational Quotes')) +
  labs(x = '',
       y = 'Statement Receptivity',
       fill = '',
       color = '') +
  scale_x_discrete(labels = c('Anti-CIs', 'True-CIs')) +
  scale_y_continuous(breaks = seq(1, 11, 1)) +
  theme(legend.position = 'top')

# ggsave('study 2 means.jpg',
#        device = 'jpeg',
#        units = 'cm',
#        path = 'plots')


# add in ci traits to test additional predictions
ci_traits <- read.csv('participant ids with ci ratings.csv')
head(ci_traits)

ci_traits <- ci_traits %>% select(stimID, trustworthy:percage)


df_long_traits <- df_long %>%
  left_join(ci_traits, by = c('stimID' = 'stimID'))


mod2 <- lmer(bsr ~ st_c 
             + trustworthy 
             + attractive 
             + wise 
             + gullible 
             + dominant 
             + sociable 
             + ambitious 
             + deceptive 
             + narcissistic 
             + competent 
             + warm 
             + eurocentric 
             + masculine 
             + percage
             + (1|id)
             + (1|stimID),
             data = df_long_traits)
model_summary_lmer(mod2)

mod3 <- lmer(bsr ~ st_c 
             + trustworthy 
             + attractive 
             + wise 
             + gullible 
             + dominant 
             + sociable 
             + ambitious 
             + deceptive 
             + narcissistic 
             + competent 
             + warm 
             + eurocentric 
             + masculine 
             + percage
             + trustworthy:st_c
             + attractive:st_c
             + wise:st_c
             + gullible:st_c
             + dominant:st_c
             + sociable:st_c
             + ambitious:st_c
             + deceptive:st_c
             + narcissistic:st_c
             + competent:st_c
             + warm:st_c
             + eurocentric:st_c
             + masculine:st_c
             + percage:st_c
             + (1|id)
             + (1|stimID),
             data = df_long_traits)
model_summary_lmer(mod3)

# significant interactions:
# attractive
# gullible
# dominant
# sociable
# ambitious
# narcissistic
# warm
# percage

# sim_slopes(mod3, pred = st_c, modx = attractive)
# sim_slopes(mod3, pred = st_c, modx = gullible)
# sim_slopes(mod3, pred = st_c, modx = dominant)
# sim_slopes(mod3, pred = st_c, modx = sociable)
# sim_slopes(mod3, pred = st_c, modx = ambitious)
# sim_slopes(mod3, pred = st_c, modx = narcissistic)
# sim_slopes(mod3, pred = st_c, modx = warm)
# sim_slopes(mod3, pred = st_c, modx = percage)
# 
# 
# attr_plot <- interact_plot(mod3,
#                            pred = 'st_c',
#                            modx = 'attractive',
#                            pred.labels = c('Inspirational\nQuote',
#                                            'Bullshit\nStatement'),
#                            legend.main = str_to_title('attractive')) +
#   theme_minimal(base_size = 15) +
#   labs(x = '',
#        y = '') +
#   theme(legend.position = 'bottom')
# 
# gull_plot <- interact_plot(mod3,
#                            pred = 'st_c',
#                            modx = 'gullible',
#                            pred.labels = c('Inspirational\nQuote',
#                                            'Bullshit\nStatement'),
#                            legend.main = str_to_title('gullible')) +
#   theme_minimal(base_size = 15) +
#   labs(x = '',
#        y = '') +
#   theme(legend.position = 'bottom')
# 
# dom_plot <- interact_plot(mod3,
#                           pred = 'st_c',
#                           modx = 'dominant',
#                           pred.labels = c('Inspirational\nQuote',
#                                           'Bullshit\nStatement'),
#                           legend.main = str_to_title('dominant')) +
#   theme_minimal(base_size = 15) +
#   labs(x = '',
#        y = '') +
#   theme(legend.position = 'bottom')
# 
# soc_plot <- interact_plot(mod3,
#                       pred = 'st_c',
#                       modx = 'sociable',
#                       pred.labels = c('Inspirational\nQuote',
#                                       'Bullshit\nStatement'),
#                       legend.main = str_to_title('sociable')) +
#   theme_minimal(base_size = 15) +
#   labs(x = '',
#        y = '') +
#   theme(legend.position = 'bottom')
# 
# amb_plot <- interact_plot(mod3,
#                           pred = 'st_c',
#                           modx = 'ambitious',
#                           pred.labels = c('Inspirational\nQuote',
#                                           'Bullshit\nStatement'),
#                           legend.main = str_to_title('ambitious')) +
#   theme_minimal(base_size = 15) +
#   labs(x = '',
#        y = '') +
#   theme(legend.position = 'bottom')
# 
# narc_plot <- interact_plot(mod3,
#                            pred = 'st_c',
#                            modx = 'narcissistic',
#                            pred.labels = c('Inspirational\nQuote',
#                                            'Bullshit\nStatement'),
#                            legend.main = str_to_title('narcissistic')) +
#   theme_minimal(base_size = 15) +
#   labs(x = '',
#        y = '') +
#   theme(legend.position = 'bottom')
# 
# warm_plot <- interact_plot(mod3,
#                            pred = 'st_c',
#                            modx = 'warm',
#                            pred.labels = c('Inspirational\nQuote',
#                                            'Bullshit\nStatement'),
#                            legend.main = str_to_title('warm')) +
#   theme_minimal(base_size = 15) +
#   labs(x = '',
#        y = '') +
#   theme(legend.position = 'bottom')
# 
# age_plot <- interact_plot(mod3,
#                           pred = 'st_c',
#                           modx = 'percage',
#                           pred.labels = c('Inspirational\nQuote',
#                                           'Bullshit\nStatement'),
#                           legend.main = str_to_title('Perceived\nAge')) +
#   theme_minimal(base_size = 15) +
#   labs(x = '',
#        y = '') +
#   theme(legend.position = 'bottom')
# 
# 
# ggpubr::ggarrange(attr_plot,
#                   gull_plot,
#                   dom_plot,
#                   soc_plot,
#                   amb_plot,
#                   narc_plot,
#                   warm_plot,
#                   age_plot,
#                   nrow = 3,
#                   ncol = 3)
  

### flip these (just to see)
sim_slopes(mod3, pred = attractive, modx = st_c)
sim_slopes(mod3, pred = gullible, modx = st_c)
sim_slopes(mod3, pred = dominant, modx = st_c)
sim_slopes(mod3, pred = sociable, modx = st_c)
sim_slopes(mod3, pred = ambitious, modx = st_c)
sim_slopes(mod3, pred = narcissistic, modx = st_c)
sim_slopes(mod3, pred = warm, modx = st_c)
sim_slopes(mod3, pred = percage, modx = st_c)


attr_plot <- interact_plot(mod3,
                           pred = 'attractive',
                           modx = 'st_c',
                           legend.main = str_to_title('Statement\nType'),
                           modx.labels = c('Inspirational\nQuote',
                                            'Bullshit\nStatement')) +
  theme_bw(base_size = 25) +
  labs(x = 'Attractive',
       y = '') +
  theme(legend.position = 'bottom')

gull_plot <- interact_plot(mod3,
                           pred = 'gullible',
                           modx = 'st_c',
                           legend.main = str_to_title('Statement\nType'),
                           modx.labels = c('Inspirational\nQuote',
                                           'Bullshit\nStatement')) +
  theme_bw(base_size = 25) +
  labs(x = 'Gullible',
       y = '') +
  theme(legend.position = 'bottom')

dom_plot <- interact_plot(mod3,
                          pred = 'dominant',
                          modx = 'st_c',
                          legend.main = str_to_title('Statement\nType'),
                          modx.labels = c('Inspirational\nQuote',
                                          'Bullshit\nStatement')) +
  theme_bw(base_size = 25) +
  labs(x = 'Dominant',
       y = '') +
  theme(legend.position = 'bottom')

soc_plot <- interact_plot(mod3,
                          pred = 'sociable',
                          modx = 'st_c',
                          legend.main = str_to_title('Statement\nType'),
                          modx.labels = c('Inspirational\nQuote',
                                          'Bullshit\nStatement')) +
  theme_bw(base_size = 25) +
  labs(x = 'Sociable',
       y = '') +
  theme(legend.position = 'bottom')

amb_plot <- interact_plot(mod3,
                          pred = 'ambitious',
                          modx = 'st_c',
                          legend.main = str_to_title('Statement\nType'),
                          modx.labels = c('Inspirational\nQuote',
                                          'Bullshit\nStatement')) +
  theme_bw(base_size = 25) +
  labs(x = 'Ambitious',
       y = '') +
  theme(legend.position = 'bottom')

narc_plot <- interact_plot(mod3,
                           pred = 'narcissistic',
                           modx = 'st_c',
                           legend.main = str_to_title('Statement\nType'),
                           modx.labels = c('Inspirational\nQuote',
                                           'Bullshit\nStatement')) +
  theme_bw(base_size = 25) +
  labs(x = 'Narcissistic',
       y = '') +
  theme(legend.position = 'bottom')

warm_plot <- interact_plot(mod3,
                           pred = 'warm',
                           modx = 'st_c',
                           legend.main = str_to_title('Statement\nType'),
                           modx.labels = c('Inspirational\nQuote',
                                           'Bullshit\nStatement')) +
  theme_bw(base_size = 25) +
  labs(x = 'Warm',
       y = '') +
  theme(legend.position = 'bottom')

age_plot <- interact_plot(mod3,
                          pred = 'percage',
                          modx = 'st_c',
                          legend.main = str_to_title('Statement\nType'),
                          modx.labels = c('Inspirational\nQuote',
                                         'Bullshit\nStatement')) +
  theme_bw(base_size = 25) +
  labs(x = 'Perceived Age',
       y = '') +
  theme(legend.position = 'bottom')


ggpubr::ggarrange(attr_plot,
                  gull_plot,
                  dom_plot,
                  soc_plot,
                  amb_plot,
                  narc_plot,
                  warm_plot,
                  age_plot,
                  nrow = 3,
                  ncol = 3,
                  # legend = 'right',
                  common.legend = T)

# ggsave('study 2 - interactions plot grid.jpg',
#        device = 'jpeg',
#        units = 'cm',
#        path = 'plots')
 


### additional analyses to explore different types of bullshit
corp_bs_stims1 <- c('stim002',
                    'stim013',
                    'stim016',
                    'stim017',
                    'stim018',
                    'stim051',
                    'stim062',
                    'stim065',
                    'stim066',
                    'stim067')

tech_bs_stims1 <- c('stim022',
                    'stim024',
                    'stim025',
                    'stim028',
                    'stim031',
                    'stim071',
                    'stim073',
                    'stim074',
                    'stim077',
                    'stim080')

corp_bs_stims2 <- c('stim032',
                    'stim033',
                    'stim034',
                    'stim036',
                    'stim037',
                    'stim081',
                    'stim082',
                    'stim083',
                    'stim085',
                    'stim086')

tech_bs_stims2 <- c('stim039',
                    'stim041',
                    'stim043',
                    'stim044',
                    'stim045',
                    'stim088',
                    'stim090',
                    'stim092',
                    'stim093',
                    'stim094')

df_bs <- df_long_traits %>%
  filter(statement_type == 'bullshit') %>%
  mutate(
    bullshit_type = case_when(
      block == 'faces_1' & stimID %in% corp_bs_stims1 ~ 'corporate',
      block == 'faces_1' & stimID %in% tech_bs_stims1 ~ 'tech',
      block == 'faces_2' & stimID %in% corp_bs_stims2 ~ 'corporate',
      block == 'faces_2' & stimID %in% tech_bs_stims2 ~ 'tech'
    ),
    bs_c = if_else(bullshit_type == 'corporate', -1, 1)
  )

mod4 <- lmer(bsr ~ ci_c * bs_c
             + (1|id)
             + (1|stimID),
             data = df_bs)
model_summary_lmer(mod4)


mod5 <- lmer(bsr ~ bs_c 
             + trustworthy 
             + attractive 
             + wise 
             + gullible 
             + dominant 
             + sociable 
             + ambitious 
             + deceptive 
             + narcissistic 
             + competent 
             + warm 
             + eurocentric 
             + masculine 
             + percage
             + trustworthy:bs_c
             + attractive:bs_c
             + wise:bs_c
             + gullible:bs_c
             + dominant:bs_c
             + sociable:bs_c
             + ambitious:bs_c
             + deceptive:bs_c
             + narcissistic:bs_c
             + competent:bs_c
             + warm:bs_c
             + eurocentric:bs_c
             + masculine:bs_c
             + percage:bs_c
             + (1|id)
             + (1|stimID),
             data = df_bs)
model_summary_lmer(mod5)

sim_slopes(mod5, pred = 'attractive', modx = 'bs_c')

interact_plot(mod5,
              pred = 'attractive',
              modx = 'bs_c',
              legend.main = str_to_title('Bullshit\nType'),
              modx.labels = c('Corporate', 'Tech')) +
  theme_bw(base_size = 15) +
  labs(x = 'Attractive',
       y = '') +
  theme(legend.position = 'top')


### finally, model the results of the efa from the ratings task
efa_df <- read.csv('efa_ci_scores.csv')

df_long_efa <- df_long_traits %>%
  select(id,
         stimID,
         ci_type,
         statement_type,
         bsr,
         ci_c,
         st_c) %>%
  left_join(efa_df %>% select(stim_id, social_competence, social_aggression),
            by = c('stimID' = 'stim_id'))

mod6 <- lmer(bsr ~ st_c
             + social_competence 
             + social_aggression
             + social_competence:st_c
             + social_aggression:st_c
             + (1|id)
             + (1|stimID),
             data = df_long_efa)
model_summary_lmer(mod6)

ss6.1 <- sim_slopes(mod6,
                 pred = 'social_competence',
                 modx = 'st_c')$slopes |> as.data.frame()

d_val6.1 <- t_to_d(t = ss6.1$`t val.`, df = 12481.328, paired = T)

print(
  cbind(
    round(ss6.1, 3),
    round(d_val6.1, 3)
  )
)

soc_comp_plt <- interact_plot(mod6,
                              pred = 'social_competence',
                              modx = 'st_c',
                              interval = T,
                              int.type = 'confidence',
                              legend.main = str_to_title('Statement\nType'),
                              modx.labels = c('Inspirational\nQuote',
                                              'Bullshit\nStatement')) +
  theme_bw(base_size = 25) +
  scale_x_continuous(breaks = seq(-2, 5, 1)) +
  ylim(4, 8) +
  labs(x = 'Social Competence',
       y = 'Statement Receptivity') +
  theme(legend.position = 'bottom')
soc_comp_plt

ss6.2 <- sim_slopes(mod6,
                    pred = 'social_aggression',
                    modx = 'st_c')$slopes |> as.data.frame()

d_val6.2 <- t_to_d(t = ss6.2$`t val.`, df = 12474.968, paired = T)

print(
  cbind(
    round(ss6.2, 3),
    round(d_val6.2, 3)
  )
)


soc_agg_plt <- interact_plot(mod6,
                             pred = 'social_aggression',
                             modx = 'st_c',
                             interval = T,
                             int.type = 'confidence',
                             legend.main = str_to_title('Statement\nType'),
                             modx.labels = c('Inspirational\nQuote',
                                             'Bullshit\nStatement')) +
  theme_bw(base_size = 25) +
  scale_x_continuous(breaks = seq(-2, 5, 1)) +
  ylim(4, 8) +
  labs(x = 'Social Aggression',
       y = '') +
  theme(legend.position = 'bottom')
soc_agg_plt

ggpubr::ggarrange(soc_comp_plt, soc_agg_plt,
                  ncol = 2,
                  common.legend = T)

# ggsave('study 2 - latent factors.jpg',
#        device = 'jpeg',
#        units = 'cm',
#        path = 'plots')

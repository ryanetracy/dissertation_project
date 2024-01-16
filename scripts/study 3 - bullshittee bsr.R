##################################
# study 3 - bullshit receptivity 

# bullshittees' bsr
# bullshitter true-ci vs. anti-ci
# corporate bullshit vs. quotes
##################################


url = 'https://github.com/ryanetracy/misc_functions/blob/main/misc_functions.R?raw=TRUE'
devtools::source_url(url)

pckgs <- c(
  'psych',
  'lme4',
  'lmerTest',
  'afex',
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
df1 <- read.csv('data files/study 3 data set 1.csv')
df2 <- read.csv('data files/study 3 data set 2.csv')

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

demos %>% get_summary_stats(age, type = 'mean_sd')

total_n <- nrow(demos)

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
ci_nums <- readxl::read_xlsx('face stimuli/ci urls.xlsx', sheet = 3) %>%
  select(bullshitter_id, bullshittee_id)

ci_nums_bser <- ci_nums$bullshitter_id[1:40]
ci_nums_bsee <- ci_nums$bullshittee_id[1:40]

new_names1 <- paste0(
  rep(ci_nums_bser, each = 3),
  '_',
  rep('bullshitter'),
  '_',
  rep(c('trueCI', 'antiCI'), each = 60),
  '_',
  rep(ci_nums_bsee, each = 3),
  '_',
  rep('bullshittee'),
  '_',
  rep(c('trueCI', 'antiCI'), each = 15),
  '_',
  rep(c('bullshit', 'quote'), each = 30),
  '_',
  rep(c('profound', 'inspirational', 'persuasive'))
)

new_names2 <- paste0(
  rep(ci_nums_bser, each = 3),
  '_',
  rep('bullshitter'),
  '_',
  rep(c('trueCI', 'antiCI'), each = 60),
  '_',
  rep(ci_nums_bsee, each = 3),
  '_',
  rep('bullshittee'),
  '_',
  rep(c('trueCI', 'antiCI'), each = 15),
  '_',
  rep(c('quote', 'bullshit'), each = 30),
  '_',
  rep(c('profound', 'inspirational', 'persuasive'))
)

names(df_clean)[3:122] <- new_names1
names(df_clean)[123:242] <- new_names2

# reshape to long
b1_long <- df_clean %>%
  filter(block == 'faces_1') %>%
  select(
    id:stim094_bullshitter_antiCI_stim194_bullshittee_antiCI_quote_persuasive
  ) %>%
  pivot_longer(
    cols = stim002_bullshitter_trueCI_stim099_bullshittee_trueCI_bullshit_profound:stim094_bullshitter_antiCI_stim194_bullshittee_antiCI_quote_persuasive,
    names_to = c('stimID_bser',
                 'ci_bser',
                 'ci_group_bser',
                 'stimID_bsee',
                 'ci_bsee',
                 'ci_group_bsee',
                 'statement_type',
                 'rating'),
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
         stim002_bullshitter_trueCI_stim099_bullshittee_trueCI_quote_profound:stim094_bullshitter_antiCI_stim194_bullshittee_antiCI_bullshit_persuasive) %>%
  pivot_longer(
    cols = stim002_bullshitter_trueCI_stim099_bullshittee_trueCI_quote_profound:stim094_bullshitter_antiCI_stim194_bullshittee_antiCI_bullshit_persuasive,
    names_to = c('stimID_bser',
                 'ci_bser',
                 'ci_group_bser',
                 'stimID_bsee',
                 'ci_bsee',
                 'ci_group_bsee',
                 'statement_type',
                 'rating'),
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
    bser_c = if_else(ci_group_bser == 'trueCI', 1, -1),
    bsee_c = if_else(ci_group_bsee == 'trueCI', 1, -1),
    st_c = if_else(statement_type == 'bullshit', 1, -1)
  )

# initial model
mod1 <- lmer(bsr ~ bser_c * bsee_c * st_c
             + (1|id)
             + (1|stimID_bser:stimID_bsee),
             control = lmerControl(
               boundary.tol = 1e-20,
               optCtrl = list(maxfun = 2e10)),
             data = df_long)
model_summary_lmer(mod1)
# bullshitter effect: higher receptivity for true (vs. anti) cis
# bullshittee appearance didn't matter (ci vs anti)
# lower receptivity for bullshit than quotes


# explore 3-way interaction

ss <- sim_slopes(mod1,
                 pred = st_c,
                 modx = bser_c,
                 mod2 = bsee_c)$slopes |> as.data.frame()
ss_bsee_anti <- ss[, 1:7]
ss_bsee_true <- ss[, 8:14]

d_val_bsee_anti <- t_to_d(t = ss_bsee_anti$t.val.,
                          df_error = 13675.919,
                          paired = T)
d_val_bsee_true <- t_to_d(t = ss_bsee_true$t.val..1,
                          df_error = 13675.919,
                          paired = T)

# bullshittee anti cis
print(
  cbind(
    round(ss_bsee_anti, 3),
    round(d_val_bsee_anti, 3)
  )
)

# bullshittee true cis
print(
  cbind(
    round(ss_bsee_true, 3),
    round(d_val_bsee_true, 3)
  )
)


# visualize
plot_colors <- c('#003366', '#006666', '#ffd600')

df_sum <- df_long %>%
  group_by(ci_group_bser, ci_group_bsee, statement_type) %>%
  get_summary_stats(bsr, type = 'mean_ci')

df_part <- df_long %>%
  group_by(id, ci_group_bser, ci_group_bsee, statement_type) %>%
  get_summary_stats(bsr, type = 'mean_ci')

facet_labs <- c('Bullshittee Anti-CIs', 'Bullshittee True-CIs')
names(facet_labs) <- c('antiCI', 'trueCI')

df_part %>%
  ggplot(aes(ci_group_bser, mean, fill = statement_type, color = statement_type)) +
  geom_violin(color = 'black',
              position = position_dodge(.9),
              alpha = .75) +
  geom_point(color = 'black',
             alpha = .5,
             position = position_jitterdodge(.15, .05, .9),
             shape = 4) +
  geom_point(data = df_sum,
             aes(ci_group_bser, mean),
             position = position_dodge(.9),
             color = plot_colors[3],
             shape = 9,
             size = 3) +
  geom_errorbar(data = df_sum,
                aes(ci_group_bser, mean, ymin = mean - ci, ymax = mean + ci),
                color = plot_colors[3],
                width = .15,
                position = position_dodge(.9)) +
  facet_wrap(~ ci_group_bsee, labeller = labeller(ci_group_bsee = facet_labs)) +
  theme_light(base_size = 25) +
  scale_color_manual(values = c(plot_colors[1], plot_colors[2]),
                     labels = c('Bullshit Statements', 'Inspirational Quotes')) +
  scale_fill_manual(values = c(plot_colors[1], plot_colors[2]),
                    labels = c('Bullshit Statements', 'Inspirational Quotes')) +
  labs(x = '',
       y = 'Statement Receptivity',
       fill = '',
       color = '') +
  scale_x_discrete(labels = c('Bullshitter\nAnti-CIs', 'Bullshitter\nTrue-CIs')) +
  scale_y_continuous(breaks = seq(1, 11, 1)) +
  theme(legend.position = 'top')

# ggsave('study 3 means.jpg',
#        device = 'jpeg',
#        units = 'cm',
#        path = 'plots')


### additional analyses with traits
ci_traits <- read.csv('participant ids with ci ratings.csv')
head(ci_traits)

ci_traits <- ci_traits %>% select(stimID, trustworthy:percage)

df_long_traits1 <- df_long %>%
  left_join(ci_traits, by = c('stimID_bser' = 'stimID'))

# model with bullshitter traits
mod2 <- lmer(bsr ~ st_c * bsee_c
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
             + (1|stimID_bser:stimID_bsee),
             control = lmerControl(
               boundary.tol = 1e-20,
               optCtrl = list(maxfun = 2e10)),
             data = df_long_traits1)
model_summary_lmer(mod2)


mod3 <- lmer(bsr ~ st_c * bsee_c
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
             + trustworthy:bsee_c
             + attractive:bsee_c
             + wise:bsee_c
             + gullible:bsee_c
             + dominant:bsee_c
             + sociable:bsee_c
             + ambitious:bsee_c
             + deceptive:bsee_c
             + narcissistic:bsee_c
             + competent:bsee_c
             + warm:bsee_c
             + eurocentric:bsee_c
             + masculine:bsee_c
             + percage:bsee_c
             + (1|id)
             + (1|stimID_bser:stimID_bsee),
             control = lmerControl(
               boundary.tol = 1e-20,
               optCtrl = list(maxfun = 2e10)),
             data = df_long_traits1)
model_summary_lmer(mod3)


mod4 <- lmer(bsr ~ st_c * bsee_c
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
             + trustworthy:bsee_c
             + attractive:bsee_c
             + wise:bsee_c
             + gullible:bsee_c
             + dominant:bsee_c
             + sociable:bsee_c
             + ambitious:bsee_c
             + deceptive:bsee_c
             + narcissistic:bsee_c
             + competent:bsee_c
             + warm:bsee_c
             + eurocentric:bsee_c
             + masculine:bsee_c
             + percage:bsee_c
             + (1|id)
             + (1|stimID_bser:stimID_bsee),
             control = lmerControl(
               boundary.tol = 1e-20,
               optCtrl = list(maxfun = 2e10)),
             data = df_long_traits1)
model_summary_lmer(mod4)


mod5 <- lmer(bsr ~ st_c * bsee_c
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
             + trustworthy:bsee_c
             + attractive:bsee_c
             + wise:bsee_c
             + gullible:bsee_c
             + dominant:bsee_c
             + sociable:bsee_c
             + ambitious:bsee_c
             + deceptive:bsee_c
             + narcissistic:bsee_c
             + competent:bsee_c
             + warm:bsee_c
             + eurocentric:bsee_c
             + masculine:bsee_c
             + percage:bsee_c
             + trustworthy:st_c:bsee_c
             + attractive:st_c:bsee_c
             + wise:st_c:bsee_c
             + gullible:st_c:bsee_c
             + dominant:st_c:bsee_c
             + sociable:st_c:bsee_c
             + ambitious:st_c:bsee_c
             + deceptive:st_c:bsee_c
             + narcissistic:st_c:bsee_c
             + competent:st_c:bsee_c
             + warm:st_c:bsee_c
             + eurocentric:st_c:bsee_c
             + masculine:st_c:bsee_c
             + percage:st_c:bsee_c
             + (1|id)
             + (1|stimID_bser:stimID_bsee),
             control = lmerControl(
               boundary.tol = 1e-20,
               optCtrl = list(maxfun = 2e10)),
             data = df_long_traits1)
model_summary_lmer(mod5)

# attractive
# wise
# sociable
# competent
# warm
# eurocentric

sim_slopes(mod5,
           pred = 'attractive',
           modx = 'st_c',
           mod2 = 'bsee_c')

sim_slopes(mod5,
           pred = 'wise',
           modx = 'st_c',
           mod2 = 'bsee_c')

sim_slopes(mod5,
           pred = 'sociable',
           modx = 'st_c',
           mod2 = 'bsee_c')

sim_slopes(mod5,
           pred = 'competent',
           modx = 'st_c',
           mod2 = 'bsee_c')

sim_slopes(mod5,
           pred = 'warm',
           modx = 'st_c',
           mod2 = 'bsee_c')

sim_slopes(mod5,
           pred = 'eurocentric',
           modx = 'st_c',
           mod2 = 'bsee_c')


# visualize
attr_plot1 <- interact_plot(mod5,
                            pred = 'attractive',
                            modx = 'st_c',
                            mod2 = 'bsee_c',
                            legend.main = '',
                            mod2.labels = c('Bullshittee\nAnti-CIs',
                                            'Bullshittee\nTrue-CIs'),
                            modx.labels = c('Inspirational Quotes',
                                            'Bullshit Statements')) +
  theme_bw(base_size = 15) +
  labs(x = 'Attractive',
       y = '') +
  theme(axis.text.x = element_text(angle = 45))

wise_plot1 <- interact_plot(mod5,
                            pred = 'wise',
                            modx = 'st_c',
                            mod2 = 'bsee_c',
                            legend.main = '',
                            mod2.labels = c('Bullshittee\nAnti-CIs',
                                            'Bullshittee\nTrue-CIs'),
                            modx.labels = c('Inspirational Quotes',
                                            'Bullshit Statements')) +
  theme_bw(base_size = 15) +
  labs(x = 'Wise',
       y = '') +
  theme(axis.text.x = element_text(angle = 45))

soc_plot1 <- interact_plot(mod5,
                           pred = 'sociable',
                           modx = 'st_c',
                           mod2 = 'bsee_c',
                           legend.main = '',
                           mod2.labels = c('Bullshittee\nAnti-CIs',
                                           'Bullshittee\nTrue-CIs'),
                           modx.labels = c('Inspirational Quotes',
                                           'Bullshit Statements')) +
  theme_bw(base_size = 15) +
  labs(x = 'Sociable',
       y = '') +
  theme(axis.text.x = element_text(angle = 45))

comp_plot1 <- interact_plot(mod5,
                            pred = 'competent',
                            modx = 'st_c',
                            mod2 = 'bsee_c',
                            legend.main = '',
                            mod2.labels = c('Bullshittee\nAnti-CIs',
                                            'Bullshittee\nTrue-CIs'),
                            modx.labels = c('Inspirational Quotes',
                                            'Bullshit Statements')) +
  theme_bw(base_size = 15) +
  labs(x = 'Competent',
       y = '') +
  theme(axis.text.x = element_text(angle = 45))

warm_plot1 <- interact_plot(mod5,
                            pred = 'warm',
                            modx = 'st_c',
                            mod2 = 'bsee_c',
                            legend.main = '',
                            mod2.labels = c('Bullshittee\nAnti-CIs',
                                            'Bullshittee\nTrue-CIs'),
                            modx.labels = c('Inspirational Quotes',
                                            'Bullshit Statements')) +
  theme_bw(base_size = 15) +
  labs(x = 'Warm',
       y = '') +
  theme(axis.text.x = element_text(angle = 45))

euro_plot1 <- interact_plot(mod5,
                            pred = 'eurocentric',
                            modx = 'st_c',
                            mod2 = 'bsee_c',
                            legend.main = '',
                            mod2.labels = c('Bullshittee\nAnti-CIs',
                                            'Bullshittee\nTrue-CIs'),
                            modx.labels = c('Inspirational Quotes',
                                            'Bullshit Statements')) +
  theme_bw(base_size = 15) +
  labs(x = 'Eurocentric',
       y = '') +
  theme(axis.text.x = element_text(angle = 45))


bser_traits_int <- ggpubr::ggarrange(attr_plot1,
                                     wise_plot1,
                                     soc_plot1,
                                     comp_plot1,
                                     warm_plot1,
                                     euro_plot1,
                                     nrow = 2,
                                     ncol = 3,
                                     common.legend = T)

bser_traits_int


old_names1 <- colnames(df_long_traits1)[17:30]
new_names1 <- paste0(old_names1, '_', 'bser')

names(df_long_traits1)[17:30] <- new_names1

df_long_traits2 <- df_long_traits1 %>%
  left_join(ci_traits, by = c('stimID_bsee' = 'stimID'))

old_names2 <- colnames(df_long_traits2)[31:44]
new_names2 <- paste0(old_names2, '_', 'bsee')

names(df_long_traits2)[31:44] <- new_names2

colnames(df_long_traits2)


## model with bullshittee traits
# mod6 <- lmer(bsr ~ st_c * bser_c
#              + trustworthy_bsee
#              + attractive_bsee
#              + wise_bsee
#              + gullible_bsee
#              + dominant_bsee
#              + sociable_bsee
#              + ambitious_bsee
#              + deceptive_bsee
#              + narcissistic_bsee
#              + competent_bsee
#              + warm_bsee
#              + eurocentric_bsee
#              + masculine_bsee
#              + percage_bsee
#              + (1|id)
#              + (1|stimID_bser:stimID_bsee),
#              control = lmerControl(
#                boundary.tol = 1e-20,
#                optCtrl = list(maxfun = 2e10)),
#              data = df_long_traits2)
# model_summary_lmer(mod6)
# 
# 
# mod7 <- lmer(bsr ~ st_c * bser_c
#              + trustworthy_bsee
#              + attractive_bsee
#              + wise_bsee
#              + gullible_bsee
#              + dominant_bsee
#              + sociable_bsee
#              + ambitious_bsee
#              + deceptive_bsee
#              + narcissistic_bsee
#              + competent_bsee
#              + warm_bsee
#              + eurocentric_bsee
#              + masculine_bsee
#              + percage_bsee
#              + trustworthy_bsee:bser_c
#              + attractive_bsee:bser_c
#              + wise_bsee:bser_c
#              + gullible_bsee:bser_c
#              + dominant_bsee:bser_c
#              + sociable_bsee:bser_c
#              + ambitious_bsee:bser_c
#              + deceptive_bsee:bser_c
#              + narcissistic_bsee:bser_c
#              + competent_bsee:bser_c
#              + warm_bsee:bser_c
#              + eurocentric_bsee:bser_c
#              + masculine_bsee:bser_c
#              + percage_bsee:bser_c
#              + (1|id)
#              + (1|stimID_bser:stimID_bsee),
#              control = lmerControl(
#                boundary.tol = 1e-20,
#                optCtrl = list(maxfun = 2e10)),
#              data = df_long_traits2)
# model_summary_lmer(mod7)
# 
# 
# mod8 <- lmer(bsr ~ st_c * bser_c
#              + trustworthy_bsee
#              + attractive_bsee
#              + wise_bsee
#              + gullible_bsee
#              + dominant_bsee
#              + sociable_bsee
#              + ambitious_bsee
#              + deceptive_bsee
#              + narcissistic_bsee
#              + competent_bsee
#              + warm_bsee
#              + eurocentric_bsee
#              + masculine_bsee
#              + percage_bsee
#              + trustworthy_bsee:bser_c
#              + attractive_bsee:bser_c
#              + wise_bsee:bser_c
#              + gullible_bsee:bser_c
#              + dominant_bsee:bser_c
#              + sociable_bsee:bser_c
#              + ambitious_bsee:bser_c
#              + deceptive_bsee:bser_c
#              + narcissistic_bsee:bser_c
#              + competent_bsee:bser_c
#              + warm_bsee:bser_c
#              + eurocentric_bsee:bser_c
#              + masculine_bsee:bser_c
#              + percage_bsee:bser_c
#              + trustworthy_bsee:st_c
#              + attractive_bsee:st_c
#              + wise_bsee:st_c
#              + gullible_bsee:st_c
#              + dominant_bsee:st_c
#              + sociable_bsee:st_c
#              + ambitious_bsee:st_c
#              + deceptive_bsee:st_c
#              + narcissistic_bsee:st_c
#              + competent_bsee:st_c
#              + warm_bsee:st_c
#              + eurocentric_bsee:st_c
#              + masculine_bsee:st_c
#              + percage_bsee:st_c
#              + (1|id)
#              + (1|stimID_bser:stimID_bsee),
#              control = lmerControl(
#                boundary.tol = 1e-20,
#                optCtrl = list(maxfun = 2e10)),
#              data = df_long_traits2)
# model_summary_lmer(mod8)


mod9 <- lmer(bsr ~ st_c * bser_c
             + trustworthy_bsee
             + attractive_bsee
             + wise_bsee
             + gullible_bsee
             + dominant_bsee
             + sociable_bsee
             + ambitious_bsee
             + deceptive_bsee
             + narcissistic_bsee
             + competent_bsee
             + warm_bsee
             + eurocentric_bsee
             + masculine_bsee
             + percage_bsee
             + trustworthy_bsee:bser_c
             + attractive_bsee:bser_c
             + wise_bsee:bser_c
             + gullible_bsee:bser_c
             + dominant_bsee:bser_c
             + sociable_bsee:bser_c
             + ambitious_bsee:bser_c
             + deceptive_bsee:bser_c
             + narcissistic_bsee:bser_c
             + competent_bsee:bser_c
             + warm_bsee:bser_c
             + eurocentric_bsee:bser_c
             + masculine_bsee:bser_c
             + percage_bsee:bser_c
             + trustworthy_bsee:st_c
             + attractive_bsee:st_c
             + wise_bsee:st_c
             + gullible_bsee:st_c
             + dominant_bsee:st_c
             + sociable_bsee:st_c
             + ambitious_bsee:st_c
             + deceptive_bsee:st_c
             + narcissistic_bsee:st_c
             + competent_bsee:st_c
             + warm_bsee:st_c
             + eurocentric_bsee:st_c
             + masculine_bsee:st_c
             + percage_bsee:st_c
             + trustworthy_bsee:st_c:bser_c
             + attractive_bsee:st_c:bser_c
             + wise_bsee:st_c:bser_c
             + gullible_bsee:st_c:bser_c
             + dominant_bsee:st_c:bser_c
             + sociable_bsee:st_c:bser_c
             + ambitious_bsee:st_c:bser_c
             + deceptive_bsee:st_c:bser_c
             + narcissistic_bsee:st_c:bser_c
             + competent_bsee:st_c:bser_c
             + warm_bsee:st_c:bser_c
             + eurocentric_bsee:st_c:bser_c
             + masculine_bsee:st_c:bser_c
             + percage_bsee:st_c:bser_c             
             + (1|id)
             + (1|stimID_bser:stimID_bsee),
             control = lmerControl(
               boundary.tol = 1e-20,
               optCtrl = list(maxfun = 2e10)),
             data = df_long_traits2)
model_summary_lmer(mod9)

sim_slopes(mod9,
           pred = 'dominant_bsee',
           modx = 'st_c',
           mod2 = 'bser_c')

sim_slopes(mod9,
           pred = 'eurocentric_bsee',
           modx = 'st_c',
           mod2 = 'bser_c')

dom_plot2 <- interact_plot(mod9,
                           pred = 'dominant_bsee',
                           modx = 'st_c',
                           mod2 = 'bser_c',
                           legend.main = '',
                           mod2.labels = c('Bullshitter\nAnti-CIs',
                                           'Bullshitter\nTrue-CIs'),
                           modx.labels = c('Inspirational Quotes',
                                           'Bullshit Statements')) +
  theme_bw(base_size = 15) +
  labs(x = 'Dominant',
       y = '') +
  theme(axis.text.x = element_text(angle = 45))

euro_plot2 <- interact_plot(mod9,
                            pred = 'eurocentric_bsee',
                            modx = 'st_c',
                            mod2 = 'bser_c',
                            legend.main = '',
                            mod2.labels = c('Bullshitter\nAnti-CIs',
                                            'Bullshitter\nTrue-CIs'),
                            modx.labels = c('Inspirational Quotes',
                                            'Bullshit Statements')) +
  theme_bw(base_size = 15) +
  labs(x = 'Eurocentric',
       y = '') +
  theme(axis.text.x = element_text(angle = 45))


bsee_traits_int <- ggpubr::ggarrange(dom_plot2, euro_plot2,
                                     nrow = 1, ncol = 2,
                                     common.legend = T)
bsee_traits_int

ggpubr::ggarrange(bser_traits_int,
                  bsee_traits_int,
                  ncol = 2,
                  common.legend = T,
                  labels = c('A', 'B'))

# ggsave('study 3 - trait model results 1.jpg',
#        device = 'jpeg',
#        units = 'cm',
#        path = 'plots')



### just because
# mod10 <- lmer(bsr ~ st_c
#               + trustworthy_bser
#               + attractive_bser
#               + wise_bser
#               + gullible_bser
#               + dominant_bser
#               + sociable_bser
#               + ambitious_bser
#               + deceptive_bser
#               + narcissistic_bser
#               + competent_bser
#               + warm_bser
#               + eurocentric_bser
#               + masculine_bser
#               + percage_bser
#               + trustworthy_bsee
#               + attractive_bsee
#               + wise_bsee
#               + gullible_bsee
#               + dominant_bsee
#               + sociable_bsee
#               + ambitious_bsee
#               + deceptive_bsee
#               + narcissistic_bsee
#               + competent_bsee
#               + warm_bsee
#               + eurocentric_bsee
#               + masculine_bsee
#               + percage_bsee
#               + (1|id)
#               + (1|stimID_bser:stimID_bsee),
#               control = lmerControl(
#                 boundary.tol = 1e-20,
#                 optCtrl = list(maxfun = 2e10)),
#               data = df_long_traits2)
# model_summary_lmer(mod10)


mod11 <- lmer(bsr ~ st_c 
              + trustworthy_bser
              + attractive_bser
              + wise_bser
              + gullible_bser
              + dominant_bser
              + sociable_bser
              + ambitious_bser
              + deceptive_bser
              + narcissistic_bser
              + competent_bser
              + warm_bser
              + eurocentric_bser
              + masculine_bser
              + percage_bser
              + trustworthy_bsee
              + attractive_bsee
              + wise_bsee
              + gullible_bsee
              + dominant_bsee
              + sociable_bsee
              + ambitious_bsee
              + deceptive_bsee
              + narcissistic_bsee
              + competent_bsee
              + warm_bsee
              + eurocentric_bsee
              + masculine_bsee
              + percage_bsee
              + trustworthy_bser:st_c
              + attractive_bser:st_c
              + wise_bser:st_c
              + gullible_bser:st_c
              + dominant_bser:st_c
              + sociable_bser:st_c
              + ambitious_bser:st_c
              + deceptive_bser:st_c
              + narcissistic_bser:st_c
              + competent_bser:st_c
              + warm_bser:st_c
              + eurocentric_bser:st_c
              + masculine_bser:st_c
              + percage_bser:st_c
              + trustworthy_bsee:st_c
              + attractive_bsee:st_c
              + wise_bsee:st_c
              + gullible_bsee:st_c
              + dominant_bsee:st_c
              + sociable_bsee:st_c
              + ambitious_bsee:st_c
              + deceptive_bsee:st_c
              + narcissistic_bsee:st_c
              + competent_bsee:st_c
              + warm_bsee:st_c
              + eurocentric_bsee:st_c
              + masculine_bsee:st_c
              + percage_bsee:st_c
              + (1|id)
              + (1|stimID_bser:stimID_bsee),
              control = lmerControl(
                boundary.tol = 1e-20,
                optCtrl = list(maxfun = 2e10)),
              data = df_long_traits2)
model_summary_lmer(mod11)
model_summary_lmer(mod11) %>% filter(significance %in% c('+', '*', '**', '***'))

# simple effects tests
# bullshitter
sim_slopes(mod11, pred = 'wise_bser', modx = 'st_c')
sim_slopes(mod11, pred = 'gullible_bser', modx = 'st_c')
sim_slopes(mod11, pred = 'sociable_bser', modx = 'st_c')
sim_slopes(mod11, pred = 'ambitious_bser', modx = 'st_c')
sim_slopes(mod11, pred = 'warm_bser', modx = 'st_c')

wise_bser <- interact_plot(mod11,
                           pred = 'wise_bser',
                           modx = 'st_c',
                           legend.main = '',
                           modx.labels = c('Inspirational Quotes',
                                           'Bullshit Statements')) +
  theme_bw(base_size = 15) +
  labs(x = 'Wise',
       y = '') +
  theme(axis.text.x = element_text(angle = 45))

gullible_bser <- interact_plot(mod11,
                               pred = 'gullible_bser',
                               modx = 'st_c',
                               legend.main = '',
                               modx.labels = c('Inspirational Quotes',
                                               'Bullshit Statements')) +
  theme_bw(base_size = 15) +
  labs(x = 'Gullible',
       y = '') +
  theme(axis.text.x = element_text(angle = 45))

sociable_bser <- interact_plot(mod11,
                               pred = 'sociable_bser',
                               modx = 'st_c',
                               legend.main = '',
                               modx.labels = c('Inspirational Quotes',
                                               'Bullshit Statements')) +
  theme_bw(base_size = 15) +
  labs(x = 'Sociable',
       y = '') +
  theme(axis.text.x = element_text(angle = 45))

ambitious_bser <- interact_plot(mod11,
                                pred = 'ambitious_bser',
                                modx = 'st_c',
                                legend.main = '',
                                modx.labels = c('Inspirational Quotes',
                                                'Bullshit Statements')) +
  theme_bw(base_size = 15) +
  labs(x = 'Ambitious',
       y = '') +
  theme(axis.text.x = element_text(angle = 45))

warm_bser <- interact_plot(mod11,
                           pred = 'warm_bser',
                           modx = 'st_c',
                           legend.main = '',
                           modx.labels = c('Inspirational Quotes',
                                           'Bullshit Statements')) +
  theme_bw(base_size = 15) +
  labs(x = 'Warm',
       y = '') +
  theme(axis.text.x = element_text(angle = 45))


bser_traits <- ggpubr::ggarrange(wise_bser,
                                 gullible_bser,
                                 sociable_bser,
                                 ambitious_bser,
                                 warm_bser,
                                 common.legend = T,
                                 legend = 'bottom')


# bullshittee
sim_slopes(mod11, pred = 'trustworthy_bsee', modx = 'st_c')
sim_slopes(mod11, pred = 'gullible_bsee', modx = 'st_c')
sim_slopes(mod11, pred = 'ambitious_bsee', modx = 'st_c')
sim_slopes(mod11, pred = 'warm_bsee', modx = 'st_c')
sim_slopes(mod11, pred = 'eurocentric_bsee', modx = 'st_c')
sim_slopes(mod11, pred = 'masculine_bsee', modx = 'st_c')

trustworthy_bsee <- interact_plot(mod11,
                                  pred = 'trustworthy_bsee',
                                  modx = 'st_c',
                                  legend.main = '',
                                  modx.labels = c('Inspirational Quotes',
                                                  'Bullshit Statements')) +
  theme_bw(base_size = 15) +
  labs(x = 'Trustworthy',
       y = '') +
  theme(axis.text.x = element_text(angle = 45))

gullible_bsee <- interact_plot(mod11,
                               pred = 'gullible_bsee',
                               modx = 'st_c',
                               legend.main = '',
                               modx.labels = c('Inspirational Quotes',
                                               'Bullshit Statements')) +
  theme_bw(base_size = 15) +
  labs(x = 'Gullible',
       y = '') +
  theme(axis.text.x = element_text(angle = 45))

ambitious_bsee <- interact_plot(mod11,
                                pred = 'ambitious_bsee',
                                modx = 'st_c',
                                legend.main = '',
                                modx.labels = c('Inspirational Quotes',
                                                'Bullshit Statements')) +
  theme_bw(base_size = 15) +
  labs(x = 'Ambitious',
       y = '') +
  theme(axis.text.x = element_text(angle = 45))

warm_bsee <- interact_plot(mod11,
                           pred = 'warm_bsee',
                           modx = 'st_c',
                           legend.main = '',
                           modx.labels = c('Inspirational Quotes',
                                           'Bullshit Statements')) +
  theme_bw(base_size = 15) +
  labs(x = 'Warm',
       y = '') +
  theme(axis.text.x = element_text(angle = 45))

euro_bsee <- interact_plot(mod11,
                           pred = 'eurocentric_bsee',
                           modx = 'st_c',
                           legend.main = '',
                           modx.labels = c('Inspirational Quotes',
                                           'Bullshit Statements')) +
  theme_bw(base_size = 15) +
  labs(x = 'Eurocentric',
       y = '') +
  theme(axis.text.x = element_text(angle = 45))

masculine_bsee <- interact_plot(mod11,
                                pred = 'masculine_bsee',
                                modx = 'st_c',
                                legend.main = '',
                                modx.labels = c('Inspirational Quotes',
                                                'Bullshit Statements')) +
  theme_bw(base_size = 15) +
  labs(x = 'Masculine',
       y = '') +
  theme(axis.text.x = element_text(angle = 45))

bsee_traits <- ggpubr::ggarrange(trustworthy_bsee,
                                 gullible_bsee,
                                 ambitious_bsee,
                                 warm_bsee,
                                 euro_bsee,
                                 masculine_bsee,
                                 common.legend = T,
                                 legend = 'bottom')


ggpubr::ggarrange(bser_traits,
                  bsee_traits,
                  ncol = 2,
                  labels = c('A.', 'B.'))


# ggsave('study 3 - interactions plot grid.jpg',
#        device = 'jpeg',
#        units = 'cm',
#        path = 'plots')


## explore the latent factors from study 1 efa
efa_df <- read.csv('efa_ci_scores.csv')


efa_df_full <- df_long %>% select(id,
                                  stimID_bser,
                                  ci_group_bser,
                                  stimID_bsee,
                                  ci_group_bsee,
                                  statement_type,
                                  bsr) %>%
  left_join(
    efa_df %>% 
      filter(is_bullshitter == 1) %>% 
      select(stim_id,
             social_competence,
             social_aggression) %>%
      rename('social_comp_bser' = 'social_competence',
             'social_agg_bser' = 'social_aggression'),
    by = c('stimID_bser' = 'stim_id')
  ) %>%
  left_join(
    efa_df %>%
      filter(is_bullshitter == 0) %>%
      select(stim_id,
             social_competence,
             social_aggression) %>%
      rename('social_comp_bsee' = 'social_competence',
             'social_agg_bsee' = 'social_aggression'),
    by = c('stimID_bsee' = 'stim_id')
  ) %>%
  mutate(st_c = if_else(statement_type == 'bullshit', 1, -1),
         ci_c_bsee = if_else(ci_group_bsee == 'trueCI', 1, -1),
         ci_c_bser = if_else(ci_group_bser == 'trueCI', 1, -1))


mod12 <- lmer(bsr ~ st_c
              + ci_c_bser
              + ci_c_bsee
              + social_comp_bser
              + social_agg_bser
              + social_comp_bsee
              + social_agg_bsee
              + social_comp_bser:st_c
              + social_agg_bser:st_c
              + social_comp_bsee:st_c
              + social_agg_bsee:st_c
              + social_comp_bser:ci_c_bser
              + social_agg_bser:ci_c_bser
              + social_comp_bsee:ci_c_bsee
              + social_agg_bsee:ci_c_bsee
              + social_comp_bser:ci_c_bser:st_c
              + social_agg_bser:ci_c_bser:st_c
              + social_comp_bsee:ci_c_bsee:st_c
              + social_agg_bsee:ci_c_bsee:st_c
              + (1|id)
              + (1|stimID_bser:stimID_bsee),
              data = efa_df_full)
model_summary_lmer(mod12)


latent_int1 <- interact_plot(mod12,
                             pred = 'social_comp_bser',
                             modx = 'ci_c_bser',
                             mod2 = 'st_c',
                             interval = T,
                             int.type = 'confidence',
                             modx.labels = c('Anti-CIs', 'True-CIs'),
                             mod2.labels = c('Inspirational\nQuotes',
                                             'Bullshit\nStatements'),
                             legend.main = '') +
  theme_bw(base_size = 15) +
  labs(x = 'Social Competence: Bullshitters',
       y = 'Statement Receptivity')

latent_int2 <- interact_plot(mod12,
                             pred = 'social_agg_bser',
                             modx = 'ci_c_bser',
                             mod2 = 'st_c',
                             interval = T,
                             int.type = 'confidence',
                             modx.labels = c('Anti-CIs', 'True-CIs'),
                             mod2.labels = c('Inspirational\nQuotes',
                                             'Bullshit\nStatements'),
                             legend.main = '') +
  theme_bw(base_size = 15) +
  labs(x = 'Social Aggression: Bullshitters',
       y = 'Statement Receptivity')

latent_int3 <- interact_plot(mod12,
                             pred = 'social_comp_bsee',
                             modx = 'ci_c_bsee',
                             mod2 = 'st_c',
                             interval = T,
                             int.type = 'confidence',
                             modx.labels = c('Anti-CIs', 'True-CIs'),
                             mod2.labels = c('Inspirational\nQuotes',
                                             'Bullshit\nStatements'),
                             legend.main = '') +
  theme_bw(base_size = 15) +
  labs(x = 'Social Competence: Bullshittees',
       y = 'Statement Receptivity')

latent_int4 <- interact_plot(mod12,
                             pred = 'social_agg_bsee',
                             modx = 'ci_c_bsee',
                             mod2 = 'st_c',
                             interval = T,
                             int.type = 'confidence',
                             modx.labels = c('Anti-CIs', 'True-CIs'),
                             mod2.labels = c('Inspirational\nQuotes',
                                             'Bullshit\nStatements'),
                             legend.main = '') +
  theme_bw(base_size = 15) +
  labs(x = 'Social Aggression: Bullshittees',
       y = 'Statement Receptivity')


ggpubr::ggarrange(latent_int1,
                  latent_int3,
                  latent_int2,
                  latent_int4,
                  common.legend = T,
                  legend = 'bottom',
                  labels = c('A.', 'B.'))


# ggsave('study 3 - latent factors.jpg',
#        device = 'jpeg',
#        units = 'cm',
#        path = 'plots')

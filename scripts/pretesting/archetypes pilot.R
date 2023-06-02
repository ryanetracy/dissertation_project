#####################################
# bullshitter/bullshitee pilot study
#####################################

source('load_and_install.R')

# packages
pckgs <- c(
  'psych',
  'rstatix',
  'tidyverse',
  'ggcorrplot',
  'correlation',
  'FactoMineR',
  'factoextra',
  'ggrepel',
  'ggpubr'
  )

load_and_install(package_list = pckgs)


# data
df <- read.csv('data files/archetypes pilot data.csv')

colnames(df)

# demographics
df %>%
  get_summary_stats(age, type = 'mean_sd')

df %>%
  count(gender) %>%
  mutate(prop = round(
    100 * (n / sum(n)), 2
  )) %>%
  mutate(gender = case_when(
    gender == 1 ~ 'male',
    gender == 2 ~ 'female', 
    gender == 3 ~ 'nonbinary',
    gender == 4 ~ 'other'
  ))

df %>%
  count(race) %>%
  mutate(prop = round(
    100 * (n / sum(n)), 2
  )) %>%
  mutate(race = case_when(
    race == 1 ~ 'asian',
    race == 2 ~ 'black',
    race == 3 ~ 'latino/a',
    race == 4 ~ 'middle eastern',
    race == 5 ~ 'native american',
    race == 6 ~ 'white',
    race == 7 ~ 'bi/multiracial',
    race == 8 ~ 'other'
  ))

bots <- df %>% select(bot_check)


# grab columns and sort by condition
df_full <- df %>%
  select(ResponseId, 
         corp_bser_rate_1:corp_bser_rate_26, 
         corp_bsee_rate_1:corp_bsee_rate_26,
         socl_bser_rate_1:socl_bser_rate_26,
         socl_bsee_rate_1:socl_bsee_rate_26,
         FL_8_DO) %>%
  rename('subj' = 'ResponseId',
         'condition' = 'FL_8_DO')

traits <- c(
  'trustworthiness',
  'honesty',
  'maliciousness',
  'narcissism',
  'machiavellianism',
  'psychopathy',
  'deceitfulness',
  'ambition',
  'drive',
  'dominance',
  'competence',
  'warmth',
  'gullibility',
  'intelligence',
  'wisdom',
  'sociability',
  'friendliness',
  'attractiveness',
  'motivating',
  'inspirational',
  'profound',
  'persuasive',
  'convincing',
  'appealing',
  'naivete',
  'spirituality'
  )

corporate_bullshitter <- df_full %>%
  filter(condition == 'corporate_bullshitter') %>%
  select(subj, corp_bser_rate_1:corp_bser_rate_26, condition)
names(corporate_bullshitter)[2:27] <- traits

corporate_bullshittee <- df_full %>%
  filter(condition == 'corporate_bullshittee') %>%
  select(subj, corp_bsee_rate_1:corp_bsee_rate_26, condition)
names(corporate_bullshittee)[2:27] <- traits

social_bullshitter <- df_full %>%
  filter(condition == 'social_bullshitter') %>%
  select(subj, socl_bser_rate_1:socl_bser_rate_26, condition)
names(social_bullshitter)[2:27] <- traits

social_bullshittee <- df_full %>%
  filter(condition == 'social_bullshittee') %>%
  select(subj, socl_bsee_rate_1:socl_bsee_rate_26, condition)
names(social_bullshittee)[2:27] <- traits

# check correlations
corp_bser_corrs <- psych::corr.test(corporate_bullshitter[2:27])
ggcorrplot(corp_bser_corrs$r,
           type = 'lower',
           p.mat = corp_bser_corrs$p)

corp_bsee_corrs <- psych::corr.test(corporate_bullshittee[2:27])
ggcorrplot(corp_bsee_corrs$r,
           type = 'lower',
           p.mat = corp_bsee_corrs$p)

socl_bser_corrs <- psych::corr.test(social_bullshitter[2:27])
ggcorrplot(socl_bser_corrs$r,
           type = 'lower',
           p.mat = socl_bser_corrs$p)

socl_bsee_corrs <- psych::corr.test(social_bullshittee[2:27])
ggcorrplot(socl_bsee_corrs$r,
           type = 'lower',
           p.mat = socl_bsee_corrs$p)

# measuring factorability
KMO(r = cor(corporate_bullshitter[2:27]))
KMO(r = cor(corporate_bullshittee[2:27]))
KMO(r = cor(social_bullshitter[2:27]))
KMO(r = cor(social_bullshittee[2:27]))

cortest.bartlett(corporate_bullshitter[2:27])
cortest.bartlett(corporate_bullshittee[2:27])
cortest.bartlett(social_bullshitter[2:27])
cortest.bartlett(social_bullshittee[2:27])

det(cor(corporate_bullshitter[2:27]))
det(cor(corporate_bullshittee[2:27]))
det(cor(social_bullshitter[2:27]))
det(cor(social_bullshittee[2:27]))


# number of factors to extract
get_n_factors <- function(df, sub_val = 0, rot = 'none') {
  fa_fit <-  fa(df, nfactors = ncol(df) - sub_val, rotate = rot)
  n_factors <-  length(fa_fit$e.values)
  
  scree_plt <- data.frame(
    factor_n <- as.factor(1:n_factors),
    eigenval <- fa_fit$e.values)
  
  scree_plt %>%
    ggplot(aes(x = factor_n, y = eigenval, group = 1)) +
    geom_point() +
    geom_line() +
    theme_bw() +
    labs(x = 'Number of factors',
         y = 'Initial eigenvalues',
         title = 'Scree plot',
         subtitle = 'Based on unreduced correlation matrix') +
    theme(plot.title = element_text(face = 'bold', hjust = .5),
          plot.subtitle = element_text(face = 'italic', hjust = .5))
  
  fa.parallel(df)
}

get_n_factors(df = corporate_bullshitter[2:27],
              sub_val = 2,
              rot = 'none')

get_n_factors(df = corporate_bullshittee[2:27],
              sub_val = 2,
              rot = 'none')

get_n_factors(df = social_bullshitter[2:27],
              sub_val = 2,
              rot = 'none')

get_n_factors(df = social_bullshittee[2:27],
              sub_val = 2,
              rot = 'none')


# plot the traits for each one in a scatterplot
fact_anal_scatter <- function(
    factor_analysis_df
    ) {

  # plot 1
  df_1 <- as.data.frame(factor_analysis_df$loadings[,c(1,2)])
  names(df_1) <- c('factor_1', 'factor_2')
  
  df_1_plt <- df_1 %>%
    ggplot(aes(factor_1, factor_2, label = rownames(df_1))) +
    geom_point(size = .1, alpha = .1) +
    theme_classic() +
    geom_text_repel() +
    # scale_x_continuous(breaks = c(-1, 1, .1)) +
    # scale_y_continuous(breaks = c(-1, 1, .1)) +
    labs(x = 'factor 1 loadings',
         y = 'factor 2 loadings')

  # plot 2
  df_2 <- as.data.frame(factor_analysis_df$loadings[,c(1,3)])
  names(df_2) <- c('factor_1', 'factor_3')
  
  df_2_plt <- df_2 %>%
    ggplot(aes(factor_1, factor_3, label = rownames(df_2))) +
    geom_point(size = .1, alpha = .1) +
    theme_classic() +
    geom_text_repel() +
    # scale_x_continuous(breaks = c(-1, 1, .1)) +
    # scale_y_continuous(breaks = c(-1, 1, .1)) +
    labs(x = 'factor 1 loadings',
         y = 'factor 3 loadings')

  # plot 3
  df_3 <- as.data.frame(factor_analysis_df$loadings[,c(2,3)])
  names(df_3) <- c('factor_2', 'factor_3')
  
  df_3_plt <- df_3 %>%
    ggplot(aes(factor_2, factor_3, label = rownames(df_3))) +
    geom_point(size = .1, alpha = .1) +
    theme_classic() +
    geom_text_repel() +
    # scale_x_continuous(breaks = c(-1, 1, .1)) +
    # scale_y_continuous(breaks = c(-1, 1, .1)) +
    labs(x = 'factor 2 loadings',
         y = 'factor 3 loadings')
  
  # loadings_plot <- fa.diagram(factor_analysis_df)
  
  # ggarrange(df_1_plt, df_2_plt, 
  #           df_3_plt, loadings_plot,
  #           nrow = 2)
  
  ggarrange(df_1_plt, df_2_plt, 
            df_3_plt,
            nrow = 3)
  
}

# check factor analyses
(c1_efa <- fa(corporate_bullshitter[2:27],
             nfactors = 3,
             fm = 'pa',
             max.iter = 100,
             rotate = 'varimax'))
# rmsr: .08
# tli: .651
fa.diagram(c1_efa)
fact_anal_scatter(c1_efa)

# primary loadings (f1):
# motivating = .9
# appealing = .8
# warm = .8
# friendly = .8
# sociable = .7
# attractive = .7
# inspirational = .7
# wise = .7
# persuasive = .7
# dominant = .7
# profound = .7
# trustworthy = .7
# convincing = .6
# spiritual = .5

(c2_efa <- fa(corporate_bullshittee[2:27],
             nfactors = 3,
             fm = 'pa',
             max.iter = 100,
             rotate = 'varimax'))
# rmsr: .1
# tli: .496
fa.diagram(c2_efa)
fact_anal_scatter(c2_efa
  # c2_efa$loadings[,c(1:2)],
  # c2_efa$loadings[,c(1,3)],
  # c2_efa$loadings[,c(2:3)]
)

# primary loadings (f1):
# wise = .8
# inspirational = .8
# intelligent = .8
# appealing = .8
# persuasive = .7
# convincing = .7
# attractive = .7
# competent = .6
# motivating = .6
# profound = .5
# friendly = .5
# sociable = .4
# naive (?)

(s1_efa <- fa(social_bullshitter[2:27],
             nfactors = 3,
             fm = 'pa',
             max.iter = 100,
             rotate = 'varimax'))
# rmsr: .07
# tli: .727
fa.diagram(s1_efa)
fact_anal_scatter(s1_efa)

# primary loadings (f1):
# trustworthy = .9
# wise = .9
# profound = .9
# spiritual = .8
# appealing = .8
# honest = .7
# attractive = .7
# competent = .7
# intelligent = .7
# inspirational = .7
# warm = .7
# motivating = .6
# friendly = .6
# convincing = .6
# persuasive = .6

(s2_efa <- fa(social_bullshittee[2:27],
             nfactors = 3,
             fm = 'pa',
             max.iter = 100,
             rotate = 'varimax'))
# rmsr: .06
# tli: .717
fa.diagram(s2_efa)
fact_anal_scatter(s2_efa)

# primary loadings (f1):
# profound = .8
# wise = .8
# competent = .8
# intelligent = .7
# convincing = .7
# gullible = -.7
# appealing = .7
# naive = -.7
# persuasive = .6
# inspirational = .6



# merge the datasets and run manova on all traits
df_merged <- bind_rows(corporate_bullshitter,
                       corporate_bullshittee,
                       social_bullshitter,
                       social_bullshittee)
df_merged <- df_merged %>%
  mutate(person_type = case_when(
    condition == 'corporate_bullshitter' | condition == 'social_bullshitter' ~ 'bullshitter',
    condition == 'corporate_bullshittee' | condition == 'social_bullshittee' ~ 'bullshittee'
  ),
  bullshit_type = case_when(
    condition == 'corporate_bullshitter' | condition == 'corporate_bullshittee' ~ 'corporate',
    condition == 'social_bullshitter' | condition == 'social_bullshittee' ~ 'social'
  ))

fit_1 <- manova(
  cbind(
    trustworthiness,
    honesty,
    maliciousness,
    narcissism,
    machiavellianism,
    psychopathy,
    deceitfulness,
    ambition,
    drive,
    dominance,
    competence,
    warmth,
    gullibility,
    intelligence,
    wisdom,
    sociability,
    friendliensss,
    attractiveness,
    motivating,
    inspirational,
    profound,
    persuasive,
    convincing,
    appealing,
    naivete,
    spirituality
    ) ~ bullshit_type * person_type, data = df_merged)
summary(fit_1, test = 'Wilks')
summary.aov(fit_1)
# some interesting differences for different traits
# no real interactions though

# visualize the means
df_plot <- df_merged %>%
  select(-condition) %>%
  pivot_longer(cols = trustworthiness:spirituality,
               names_to = 'trait',
               values_to = 'rating') %>%
  group_by(person_type, bullshit_type, trait) %>%
  get_summary_stats(rating, type = 'mean_ci')


df_plot %>%
  ggplot(aes(person_type, mean, fill = bullshit_type)) +
  geom_bar(stat = 'identity',
           color = 'black',
           alpha = .8,
           position = position_dodge(.9)) +
  geom_errorbar(aes(ymin = mean - ci, ymax = mean + ci),
                width = .25, alpha = .7, position = position_dodge(.9)) +
  theme_classic() +
  facet_wrap(~ trait) +
  scale_y_continuous(expand = c(0, 0),
                     breaks = seq(0, 10, 2.5)) +
  expand_limits(y = 10) +
  scale_fill_manual(values = c('#0b162a', '#c83803'),
                    labels = c('corporate\nbullshit', 'social\nbullshit')) +
  labs(x = '',
       y = 'rating',
       fill = '') +
  theme(legend.position = 'bottom')


# explore ranked traits for each category
corp_bser <- df_merged %>%
  filter(person_type == 'bullshitter' & bullshit_type == 'corporate') %>%
  pivot_longer(cols = trustworthiness:spirituality,
               names_to = 'trait',
               values_to = 'rating') %>%
  group_by(person_type, bullshit_type, trait) %>%
  get_summary_stats(rating, type = 'mean') %>%
  arrange(desc(mean))

corp_bsee <- df_merged %>%
  filter(person_type == 'bullshittee' & bullshit_type == 'corporate') %>%
  pivot_longer(cols = trustworthiness:spirituality,
               names_to = 'trait',
               values_to = 'rating') %>%
  group_by(person_type, bullshit_type, trait) %>%
  get_summary_stats(rating, type = 'mean') %>%
  arrange(desc(mean))

socl_bser <- df_merged %>%
  filter(person_type == 'bullshitter' & bullshit_type == 'social') %>%
  pivot_longer(cols = trustworthiness:spirituality,
               names_to = 'trait',
               values_to = 'rating') %>%
  group_by(person_type, bullshit_type, trait) %>%
  get_summary_stats(rating, type = 'mean') %>%
  arrange(desc(mean))

socl_bsee <- df_merged %>%
  filter(person_type == 'bullshittee' & bullshit_type == 'social') %>%
  pivot_longer(cols = trustworthiness:spirituality,
               names_to = 'trait',
               values_to = 'rating') %>%
  group_by(person_type, bullshit_type, trait) %>%
  get_summary_stats(rating, type = 'mean') %>%
  arrange(desc(mean))


corp_bser[1:10,c('person_type', 'bullshit_type', 'trait', 'mean')]
corp_bsee[1:10,c('person_type', 'bullshit_type', 'trait', 'mean')]
socl_bser[1:10,c('person_type', 'bullshit_type', 'trait', 'mean')]
socl_bsee[1:10,c('person_type', 'bullshit_type', 'trait', 'mean')]

corp_bser[16:26,c('person_type', 'bullshit_type', 'trait', 'mean')]
corp_bsee[16:26,c('person_type', 'bullshit_type', 'trait', 'mean')]
socl_bser[16:26,c('person_type', 'bullshit_type', 'trait', 'mean')]
socl_bsee[16:26,c('person_type', 'bullshit_type', 'trait', 'mean')]

# top bullshitter traits
# narcissism
# deceit
# machiavellian
# dominance
# ambition
# maliciousness

# bottom bullshitter traits
# trustworthy
# honest
# wise
# inspirational
# attractive
# warm

# top bullshittee traits
# gullibility
# naivete
# narcissism
# ambition
# sociability
# drive

# bottom bullshittee traits
# wise
# trustworthy
# honest
# competence
# attractive
# inspirational


# explore proposed reverse correlation ratings items
rc_ratings_traits <- c(
  'trustworthiness',
  'attractiveness',
  'wisdom',
  'gullibility',
  'dominance',
  'sociability',
  'ambition',
  'deceitfulness',
  'narcissism',
  'competence'
)


rc_traits_df <- df_merged %>%
  pivot_longer(cols = trustworthiness:spirituality,
               names_to = 'trait',
               values_to = 'rating') %>%
  filter(trait %in% rc_ratings_traits) %>%
  group_by(person_type, bullshit_type, trait) %>%
  get_summary_stats(rating, type = 'mean') %>%
  arrange(desc(mean), .by_group = T)

rc_traits_df %>%
  filter(person_type == 'bullshitter' & bullshit_type == 'corporate')

rc_traits_df %>%
  filter(person_type == 'bullshittee' & bullshit_type == 'corporate')

rc_traits_df %>%
  filter(person_type == 'bullshitter' & bullshit_type == 'social')

rc_traits_df %>%
  filter(person_type == 'bullshittee' & bullshit_type == 'social')

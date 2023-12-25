#####################################
# bullshitter/bullshitee pilot study
#####################################

url = 'https://github.com/ryanetracy/misc_functions/blob/main/misc_functions.R?raw=TRUE'
devtools::source_url(url)

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
  'ggpubr',
  'caret',
  'nnet'
)

package_loader(pckgs)



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


# measure factorability of the full dataset
df_merged <- bind_rows(corporate_bullshitter,
                       corporate_bullshittee,
                       social_bullshitter,
                       social_bullshittee) %>%
  mutate(person_type = case_when(
    condition == 'corporate_bullshitter' | condition == 'social_bullshitter' ~ 'bullshitter',
    condition == 'corporate_bullshittee' | condition == 'social_bullshittee' ~ 'bullshittee'
  ),
  bullshit_type = case_when(
    condition == 'corporate_bullshitter' | condition == 'corporate_bullshittee' ~ 'corporate',
    condition == 'social_bullshitter' | condition == 'social_bullshittee' ~ 'social'
  ))

KMO(r = cor(df_merged[2:27]))
cortest.bartlett(df_merged[2:27])
det(cor(df_merged[2:27]))


# number of factors to extract
get_n_factors <- function(df, sub_val = 0, rot = none) {
  fa_fit <- fa(df, nfactors = ncol(df) - sub_val, rotate = rot)
  n_factors <- length(fa_fit$e.values)
  
  scree_plt <- data.frame(factor_n = as.factor(1:n_factors),
                          eigenval = fa_fit$e.values)
  
  scree_plt %>%
    ggplot(aes(x = factor_n, y = eigenval, group = 1)) +
    geom_point() +
    geom_line() +
    theme_bw() +
    labs(
      x = 'Number of factors',
      y = 'Initial eigenvalues',
      title = 'Scree plot',
      subtitle = 'Based on unreduced correlation matrix'
    ) +
    theme(
      plot.title = element_text(face = 'bold', hjust = .5),
      plot.subtitle = element_text(face = 'italic', hjust = .5)
    )
  
  # fa.parallel(df)
}


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

fa.parallel(df_merged[2:27])
get_n_factors(df_merged[2:27], sub_val = 1, rot = 'none')

full_efa <- fa(df_merged[2:27],
               nfactors = 4,
               fm = 'ml',
               max.iter = 500,
               rotate = 'varimax')
full_efa
fa.diagram(full_efa)

fact_anal_scatter(full_efa)

# convert labels to binary outcomes
df_merged <- df_merged %>%
  mutate(
    is_bullshitter = if_else(person_type == 'bullshitter', 1, 0),
    is_corporate = if_else(bullshit_type == 'corporate', 1, 0)
  )

efa_scores_df <- data.frame(
  subj = df_merged$subj,
  var1 = full_efa$scores[, 1], # social competence
  var2 = full_efa$scores[, 2], # social aggression
  var3 = full_efa$scores[, 3], # gullibility
  var4 = full_efa$scores[, 4], # social drive
  is_bullshitter = df_merged$is_bullshitter,
  is_corporate = df_merged$is_corporate
)

glm1 <- glm(is_bullshitter ~ var1 + var2 + var3 + var4,
            family = binomial,
            data = efa_scores_df)
summary(glm1)
odds_ratio <- round(exp(glm1$coefficients), 3)
mod_se <- sqrt(diag(vcov(glm1)))
cbind(
  odds_ratio,
  OR_ci_ll = round(glm1$coefficients - (1.96 * mod_se), 3),
  OR_ci_ul = round(glm1$coefficients + (1.96 * mod_se), 3)
)

glm2 <- glm(is_corporate ~ var1 + var2 + var3 + var4,
            family = binomial,
            data = efa_scores_df)
summary(glm2)
odds_ratio <- round(exp(glm2$coefficients), 3)
mod_se <- sqrt(diag(vcov(glm2)))
cbind(
  odds_ratio,
  OR_ci_ll = round(glm2$coefficients - (1.96 * mod_se), 3),
  OR_ci_ul = round(glm2$coefficients + (1.96 * mod_se), 3)
)

# get overall means per bullshitter vs. bullshittee
type_means <- efa_scores_df %>%
  group_by(is_bullshitter) %>%
  get_summary_stats(var1, var2, var3, var4, type = 'mean_ci')
type_means

stmt_means <- efa_scores_df %>%
  group_by(is_corporate) %>%
  get_summary_stats(var1, var2, var3, var4, type = 'mean_ci')
stmt_means

# visualize the factor comparisons
type_means %>%
  ggplot(aes(variable, mean, fill = as.factor(is_bullshitter))) +
  geom_bar(stat = 'identity',
           color = 'black',
           alpha = .75,
           position = position_dodge(.9)) +
  geom_errorbar(aes(ymin = mean - ci, ymax = mean + ci),
                color = 'black',
                alpha = .75,
                position = position_dodge(.9),
                width = .25) +
  theme_minimal() +
  scale_x_discrete(labels = c('social competence',
                              'social aggression',
                              'gullibility',
                              'social drive')) +
  scale_fill_manual(values = c('#006666', '#ffd600'),
                    labels = c('bullshittee', 'bullshitter')) +
  labs(x = 'latent factor',
       y = 'mean loading score',
       fill = '') +
  theme(legend.position = 'bottom')
  

# run a multiclass classification model
efa_scores_df <- efa_scores_df %>%
  mutate(
    label = case_when(
      is_bullshitter == 1 & is_corporate == 1 ~ 'corp_bullshitter',
      is_bullshitter == 0 & is_corporate == 1 ~ 'corp_bullshittee',
      is_bullshitter == 1 & is_corporate == 0 ~ 'soc_bullshitter',
      is_bullshitter == 0 & is_corporate == 0 ~ 'soc_bullshittee'
    )
  )

# split into train/test sets
training_samples <- efa_scores_df$label %>%
  createDataPartition(p = .75, list = F)

train_df <- efa_scores_df[training_samples, ]
test_df <- efa_scores_df[-training_samples, ]

# build and evaluate model
mmod1 <- multinom(label ~ var1 + var2 + var3 + var4, data = train_df)
summary(mmod1)
pred_classes <- mmod1 %>% predict(test_df)
mean(pred_classes == test_df$label) # not accurate at all


# run on each cell indiviudally (exploration)
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



# run manova on all traits in the merge df
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
    friendliness,
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

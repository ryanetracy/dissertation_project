#################################
# study 1 - image rating task

# bullshitter vs. bullshittee
# true-ci vs. anti-ci
#################################


url = https://github.com/ryanetracy/misc_functions/blob/main/misc_functions.R?raw=TRUE
devtools::source_url(url)

pckgs <- c(
  psych,
  afex,
  lme4,
  lmerTest,
  effectsize,
  parameters,
  rstatix,
  FactoMineR,
  factoextra,
  ggcorrplot,
  tidyverse,
  ggrepel
)

package_loader(pckgs)


# read data
df <- read.csv(data files/bullshitter_bullshittee_ratings.csv)

colnames(df)

df %>% count(Progress)
df %>% count(consent)

df <- df %>% 
  filter(Progress >= 90) %>%
  filter(consent == 1) %>%
  select(-contains(c(Date,
                     Progress,
                     Finished,
                     consent,
                     _DO,
                     _TEXT,
                     SC0)))

total_n = nrow(df)

# get demographics
demos <- df %>%
  select(age, race, gender)

# someone wrote their name in the age response...
demos %>%
  select(age) %>%
  filter(age != Mairely Tineo) %>%
  mutate_at(.vars = age, .funs = as.numeric) %>%
  get_summary_stats(age, type = mean_sd)

demos %>%
  group_by(race) %>%
  count() %>%
  mutate(
    race = case_when(
      race == 1 ~ asian,
      race == 2 ~ black,
      race == 3 ~ latino,
      race == 4 ~ native american,
      race == 5 ~ middle eastern,
      race == 6 ~ pacific islander,
      race == 7 ~ white,
      race == 8 ~ biracial,
      race == 9 ~ not listed,
      TRUE ~ no response
    ),
    prop = n / total_n
  )

demos %>%
  group_by(gender) %>%
  count() %>%
  mutate(
    gender = case_when(
      gender == 1 ~ male,
      gender == 2 ~ female,
      gender == 3 ~ non binary,
      gender == 4 ~ other,
      TRUE ~ no response
    ),
    prop = n / total_n
  )



# rename columns
stim_nums <- str_pad(1:196, width = 3, side = left, pad = 0)

new_names <- paste0(
  rep(stim),
  rep(stim_nums, each = 14),
  rep(_),
  rep(c(bullshitter, bullshittee), each = 1372),
  rep(_),
  rep(c(trueCI, antiCI), each = 686),
  rep(c(_)),
  rep(c(trustworthy,
        attractive,
        wise,
        gullible,
        dominant,
        sociable,
        ambitious,
        deceptive,
        narcissistic,
        competent,
        warm,
        eurocentric,
        masculine,
        percage))
)

names(df)[2:2745] <- new_names


# reshape to long form
df_long <- df %>%
  select(ResponseId:stim196_bullshittee_antiCI_percage) %>%
  pivot_longer(
    cols = stim001_bullshitter_trueCI_trustworthy:stim196_bullshittee_antiCI_percage,
    names_to = c(stimID, ci_type, img_cat, trait),
    names_sep = _,
    values_to = rating
  ) %>%
  na.omit() %>%
  pivot_wider(names_from = trait, values_from = rating) %>%
  mutate(
    type_c = if_else(ci_type == bullshittee, -1, 1),
    cat_c = if_else(img_cat == antiCI, -1, 1)
  )


# check for missingness
find_nas(df_long)


# check for response invariance
bad_ps <- df_long %>%
  group_by(ResponseId) %>%
  summarize(
    trustworthy = sd(trustworthy, na.rm = T),
    attractive = sd(attractive, na.rm = T),
    wise = sd(wise, na.rm = T),
    gullible = sd(gullible, na.rm = T),
    dominant = sd(dominant, na.rm = T),
    sociable = sd(sociable, na.rm = T),
    ambitious = sd(ambitious, na.rm = T),
    deceptive = sd(deceptive, na.rm = T),
    narcissistic = sd(narcissistic, na.rm = T),
    competent = sd(competent, na.rm = T),
    warm = sd(warm, na.rm = T),
    eurocentric = sd(eurocentric, na.rm = T),
    masculine = sd(masculine, na.rm = T),
    percage = sd(percage, na.rm = T)
  )

bad_ps <- bad_ps %>%
  mutate(
    trait_invar = rowSums(bad_ps[, 2:12]),
    demo_invar = rowSums(bad_ps[, 13:15])
  ) %>%
  filter(trait_invar == 0 | demo_invar == 0)

# remove from final analyses
df_long <- df_long %>% filter(!(ResponseId %in% bad_ps$ResponseId))


# construct models for each trait individually
trust <- lmer(
  trustworthy ~ type_c * cat_c 
  + (1|ResponseId) 
  + (1|stimID),
  data = df_long
)
model_summary_lmer(trust)


attr <- lmer(
  attractive ~ type_c * cat_c 
  + (1|ResponseId) 
  + (1|stimID),
  data = df_long
)
model_summary_lmer(attr)


wise <- lmer(
  wise ~ type_c * cat_c 
  + (1|ResponseId) 
  + (1|stimID),
  data = df_long
)
model_summary_lmer(wise)


gull <- lmer(
  gullible ~ type_c * cat_c 
  + (1|ResponseId) 
  + (1|stimID),
  data = df_long
)
model_summary_lmer(gull)


dom <- lmer(
  dominant ~ type_c * cat_c 
  + (1|ResponseId) 
  + (1|stimID),
  data = df_long
)
model_summary_lmer(dom)


sociable <- lmer(
  sociable ~ type_c * cat_c 
  + (1|ResponseId) 
  + (1|stimID),
  data = df_long
)
model_summary_lmer(sociable)


abt <- lmer(
  ambitious ~ type_c * cat_c 
  + (1|ResponseId) 
  + (1|stimID),
  data = df_long
)
model_summary_lmer(abt)


decep <- lmer(
  deceptive ~ type_c * cat_c 
  + (1|ResponseId) 
  + (1|stimID),
  data = df_long
)
model_summary_lmer(decep)


narc <- lmer(
  narcissistic ~ type_c * cat_c 
  + (1|ResponseId) 
  + (1|stimID),
  data = df_long
)
model_summary_lmer(narc)


comp <- lmer(
  competent ~ type_c * cat_c 
  + (1|ResponseId) 
  + (1|stimID),
  data = df_long
)
model_summary_lmer(comp)


warm <- lmer(
  warm ~ type_c * cat_c 
  + (1|ResponseId) 
  + (1|stimID),
  data = df_long
)
model_summary_lmer(warm)


euro <- lmer(
  eurocentric ~ type_c * cat_c 
  + (1|ResponseId) 
  + (1|stimID),
  data = df_long
)
model_summary_lmer(euro)


masc <- lmer(
  masculine ~ type_c * cat_c 
  + (1|ResponseId) 
  + (1|stimID),
  data = df_long
)
model_summary_lmer(masc)


page <- lmer(
  percage ~ type_c * cat_c 
  + (1|ResponseId) 
  + (1|stimID),
  data = df_long
)
model_summary_lmer(page)



# conduct analyses at stimulus level
df_stims <- df_long %>%
  group_by(stimID, ci_type, img_cat) %>%
  summarize(
    trustworthy = mean(trustworthy, na.rm = T),
    attractive = mean(attractive, na.rm = T),
    wise = mean(wise, na.rm = T),
    gullible = mean(gullible, na.rm = T),
    dominant = mean(dominant, na.rm = T),
    sociable = mean(sociable, na.rm = T),
    ambitious = mean(ambitious, na.rm = T),
    deceptive = mean(deceptive, na.rm = T),
    narcissistic = mean(narcissistic, na.rm = T),
    competent = mean(competent, na.rm = T),
    warm = mean(warm, na.rm = T),
    eurocentric = mean(eurocentric, na.rm = T),
    masculine = mean(masculine, na.rm = T),
    percage = mean(percage, na.rm = T)
  )


mod1 <- manova(cbind(trustworthy,
                     attractive,
                     wise,
                     gullible,
                     dominant,
                     sociable,
                     ambitious,
                     deceptive,
                     narcissistic,
                     competent,
                     warm,
                     eurocentric,
                     masculine) ~ ci_type * img_cat, data = df_stims)
summary(mod1, Wilks)
summary.aov(mod1)

# interactions with:
# trustworthy
mod1.1 <- lm(trustworthy ~ ci_type * img_cat, data = df_stims)
Anova(mod1.1, type = 3)
df_stims %>%
  group_by(ci_type) %>%
  emmeans_test(trustworthy ~ img_cat, detailed = T)

# sociable
mod1.2 <- lm(sociable ~ ci_type * img_cat, data = df_stims)
Anova(mod1.2, type = 3)
df_stims %>%
  group_by(ci_type) %>%
  emmeans_test(sociable ~ img_cat, detailed = T)

# warm
mod1.3 <- lm(warm ~ ci_type * img_cat, data = df_stims)
Anova(mod1.3, type = 3)
df_stims %>%
  group_by(ci_type) %>%
  emmeans_test(warm ~ img_cat, detailed = T)


# explore perceived age
mod2 <- lm(percage ~ ci_type * img_cat, data = df_stims)
Anova(mod2, type = 3)


# make a table of group means and cohen d values
group_means <- df_stims %>%
  group_by(ci_type, img_cat) %>%
  get_summary_stats(trustworthy,
                    attractive,
                    wise,
                    gullible,
                    dominant,
                    sociable,
                    ambitious,
                    deceptive,
                    narcissistic,
                    competent,
                    warm,
                    eurocentric,
                    masculine,
                    percage, type = mean_sd)

# group_means %>% print(n = 60)

cond_labs <- data.frame(trait = unique(group_means$variable))

# get trait values per ci group and image type
bser_true <- group_means %>% 
  filter(ci_type == bullshitter & img_cat == trueCI)
bser_anti <- group_means %>% 
  filter(ci_type == bullshitter & img_cat == antiCI)

bsee_true <- group_means %>% 
  filter(ci_type == bullshittee & img_cat == trueCI)
bsee_anti <- group_means %>% 
  filter(ci_type == bullshittee & img_cat == antiCI)

# bullshitter true vs. anti cis
bullshitter_d_vals <- cbind(
  cond_labs,
  d_val = round(
    get_cohens_d(
      m1 = bser_true$mean,
      m2 = bser_anti$mean,
      sd1 = bser_true$sd,
      sd2 = bser_anti$sd
    ), 2
  )
)

# bullshittee true vs. anti cis
bullshittee_d_vals <- cbind(
  cond_labs,
  d_val = round(
    get_cohens_d(
      m1 = bsee_true$mean,
      m2 = bsee_anti$mean,
      sd1 = bsee_true$sd,
      sd2 = bsee_anti$sd
    ), 2
  )
)

# build the full table
group_means_table_bser <- bser_true %>%
  left_join(
    bser_anti,
    by = c('variable' = 'variable'),
    suffix = c('_true', '_anti')
  ) %>%
  select(
    variable,
    img_cat_true,
    mean_true,
    sd_true,
    img_cat_anti,
    mean_anti,
    sd_anti
  ) %>%
  mutate(
    ci_type_true = case_when(
      img_cat_true == 'trueCI' ~ 'Bullshitter True-CI'
    ),
    ci_type_anti = case_when(
      img_cat_anti == 'antiCI' ~ 'Bullshitter Anti-CI'
    )
  ) %>%
  select(
    variable,
    ci_type_true,
    mean_true,
    sd_true,
    ci_type_anti,
    mean_anti,
    sd_anti
  ) %>%
  left_join(bullshitter_d_vals, by = c('variable' = 'trait')) %>%
  mutate(variable = str_to_title(variable))


group_means_table_bsee <- bsee_true %>%
  left_join(
    bsee_anti,
    by = c('variable' = 'variable'),
    suffix = c('_true', '_anti')
  ) %>%
  select(
    variable,
    img_cat_true,
    mean_true,
    sd_true,
    img_cat_anti,
    mean_anti,
    sd_anti
  ) %>%
  mutate(
    ci_type_true = case_when(
      img_cat_true == 'trueCI' ~ 'Bullshittee True-CI'
    ),
    ci_type_anti = case_when(
      img_cat_anti == 'antiCI' ~ 'Bullshittee Anti-CI'
    )
  ) %>%
  select(
    variable,
    ci_type_true,
    mean_true,
    sd_true,
    ci_type_anti,
    mean_anti,
    sd_anti
  ) %>%
  left_join(bullshittee_d_vals, by = c('variable' = 'trait')) %>%
  mutate(variable = str_to_title(variable))

table_names <- c(
  'Trait',
  'CI Group 1',
  'Mean',
  'SD',
  'CI Group 2',
  'Mean',
  'SD',
  "Cohen's d"
)

full_table <- rbind(group_means_table_bser, group_means_table_bsee) %>%
  mutate(variable = case_when(
    variable == 'Percage' ~ 'Perceived Age',
    TRUE ~ variable
  )) # add in star values to indicate significance
names(full_table) <- table_names
full_table %>% print(n = 28)


# make a plot
plot_means <- df_stims %>%
  group_by(ci_type, img_cat) %>%
  get_summary_stats(trustworthy,
                    attractive,
                    wise,
                    gullible,
                    dominant,
                    sociable,
                    ambitious,
                    deceptive,
                    narcissistic,
                    competent,
                    warm,
                    eurocentric,
                    masculine,
                    type = mean_ci)

facet_labs <- c(Bullshittees, Bullshitters)
names(facet_labs) <- c(bullshittee, bullshitter)

plot_means %>%
  ggplot(aes(variable, mean, color = img_cat)) +
  geom_point(shape = 7,
             size = 5,
             alpha = .75) +
  geom_errorbar(aes(ymin = mean - ci, ymax = mean + ci)) +
  scale_color_manual(values = c(#034694, #dba111),
                     labels = c(Anti-CIs, True-CIs)) +
                     # labels = c(Bullshittee CIs, Bullshitter CIs)) +
  scale_x_discrete(labels = str_to_title(
    c(
      trustworthy,
      attractive,
      wise,
      gullible,
      dominant,
      sociable,
      ambitious,
      deceptive,
      narcissistic,
      competent,
      warm,
      eurocentric,
      masculine
    )
  )) +
  theme_light() +
  facet_wrap(~ ci_type, labeller = labeller(ci_type = facet_labs)) +
  coord_flip() +
  labs(x = ,
       y = Mean Rating,
       color = ) +
  theme(legend.position = bottom)

# ggsave(ci traits plot.jpeg,
#        device = jpeg,
#        units = cm,
#        path = plots)


plot_age <- df_stims %>%
  group_by(ci_type, img_cat) %>%
  get_summary_stats(percage, type = mean_ci)

df_stims %>%
  ggplot(aes(ci_type, percage, fill = img_cat, color = img_cat)) +
  geom_violin(color = black,
              alpha = .15) +
  geom_point(shape = 4,
             position = position_jitterdodge(.15, .05, .9)) +
  geom_point(data = plot_age, aes(ci_type, mean, color = img_cat),
             shape = 7,
             color = black,
             size = 5,
             position = position_dodge(.9)) +
  geom_errorbar(data = plot_age,
                aes(x = ci_type, y = mean, min = mean - ci, ymax = mean + ci),
                color = black,
                width = .25,
                position = position_dodge(.9)) +
  scale_fill_manual(values = c(#034694, #dba111),
                    labels = c(Anti-CIs, True-CIs)) +
  scale_color_manual(values = c(#034694, #dba111),
                     labels = c(Anti-CIs, True-CIs)) +
  scale_y_continuous(breaks = seq(30, 44, 2)) +
  expand_limits(y = 44) +
  scale_x_discrete(labels = c(Bullshittee CIs, Bullshitter CIs)) +
  labs(x = ,
       y = Perceived Age,
       fill = ,
       color = ) +
  theme_light() +
  theme(legend.position = bottom)

# ggsave(ci percived age plot.jpeg,
#        device = jpeg,
#        units = cm,
#        path = plots)


# join stim-level ratings data with participant id table
p_ids <- readxl::read_excel(participant ids.xlsx) %>%
  select(p_id, ci_id)

full_df <- df_stims %>%
  left_join(p_ids, by = c(stimID = ci_id)) %>%
  select(p_id, stimID:percage)

# write.csv(full_df, participant ids with ci ratings.csv, row.names = F)


# get a correlation of all traits
trait_cors <- corr.test(full_df[,5:18])

ggcorrplot(trait_cors$r,
           type = lower,
           lab = T,
           p.mat = trait_cors$p)

# ggsave(trait correlations plot.jpeg,
#        device = jpeg,
#        units = cm,
#        path = plots)



# get some rankings based on traits
bser_true_cis_sorted <- full_df %>%
  filter(ci_type == bullshitter & img_cat == trueCI) %>%
  arrange(trustworthy,
          attractive,
          wise,
          gullible,
          dominant,
          sociable,
          ambitious,
          deceptive,
          narcissistic,
          competent,
          warm,
          eurocentric,
          masculine)

bsee_true_cis_sorted <- full_df %>%
  filter(ci_type == bullshittee & img_cat == trueCI) %>%
  arrange(trustworthy,
          attractive,
          wise,
          gullible,
          dominant,
          sociable,
          ambitious,
          deceptive,
          narcissistic,
          competent,
          warm,
          eurocentric,
          masculine)


# measuring factorability
KMO(r = cor(bser_true_cis_sorted[,5:18]))
KMO(r = cor(bsee_true_cis_sorted[,5:18]))

cortest.bartlett(bser_true_cis_sorted[,5:18])
cortest.bartlett(bsee_true_cis_sorted[,5:18])

det(cor(bser_true_cis_sorted[,5:18]))
det(cor(bsee_true_cis_sorted[,5:18]))


# number of factors to extract
get_n_factors <- function(df, sub_val = 0, rot = none) {
  fa_fit <-  fa(df, nfactors = ncol(df) - sub_val, rotate = rot)
  n_factors <-  length(fa_fit$e.values)
  
  scree_plt <- data.frame(
    factor_n = as.factor(1:n_factors),
    eigenval = fa_fit$e.values)
  
  scree_plt %>%
    ggplot(aes(x = factor_n, y = eigenval, group = 1)) +
    geom_point() +
    geom_line() +
    theme_bw() +
    labs(x = Number of factors,
         y = Initial eigenvalues,
         title = Scree plot,
         subtitle = Based on unreduced correlation matrix) +
    theme(plot.title = element_text(face = bold, hjust = .5),
          plot.subtitle = element_text(face = italic, hjust = .5))
  
  fa.parallel(df)
}

get_n_factors(df = bser_true_cis_sorted[,5:18],
              sub_val = 2,
              rot = none)

get_n_factors(df = bsee_true_cis_sorted[,5:18],
              sub_val = 2,
              rot = none)


# conduct efas to explore trait contributions
bser_efa <- fa(bser_true_cis_sorted[, 5:18],
               nfactors = 2,
               fm = ml,
               max.iter = 500,
               rotate = varimax)
bser_efa
fa.diagram(bser_efa)


bsee_efa <- fa(bsee_true_cis_sorted[, 5:18],
               nfactors = 2,
               fm = ml,
               max.iter = 500,
               rotate = varimax)
bsee_efa
fa.diagram(bsee_efa)


bser_true_cis_sorted <- full_df %>%
  filter(ci_type == bullshitter & img_cat == trueCI) %>%
  arrange(sociable,
          ambitious,
          warm,
          dominant,
          narcissistic,
          masculine)

bsee_true_cis_sorted <- full_df %>%
  filter(ci_type == bullshittee & img_cat == trueCI) %>%
  arrange(warm,
          trustworthy,
          sociable,
          dominant,
          masculine,
          narcissistic)


top_bser <- bser_true_cis_sorted %>% tail(20) %>% arrange(stimID)
top_bser_anti <- full_df %>%
  filter(ci_type == bullshitter & img_cat == antiCI) %>%
  filter(p_id %in% top_bser$p_id)

s2_stims <- rbind(top_bser, top_bser_anti)

mod5 <- manova(cbind(sociable,
                     ambitious,
                     warm,
                     dominant,
                     narcissistic,
                     masculine) ~ img_cat,
               data = s2_stims)
summary(mod5, Wilks)



top_bsee <- bsee_true_cis_sorted %>% tail(20) %>% arrange(stimID)
top_bsee_anti <- full_df %>%
  filter(ci_type == bullshittee & img_cat == antiCI) %>%
  filter(p_id %in% top_bsee$p_id)

s3_stims <- rbind(top_bsee, top_bsee_anti)

mod6 <- manova(cbind(warm,
                     trustworthy,
                     sociable,
                     dominant,
                     masculine,
                     narcissistic) ~ img_cat,
               data = s3_stims)
summary(mod6, Wilks)

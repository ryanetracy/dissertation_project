####################
# deepfake ratings 
####################

source('load_and_install.R')

pckgs <- c(
  'car',
  'psych',
  'emmeans',
  'afex',
  'lme4',
  'lmerTest',
  'effectsize',
  'interactions',
  'parameters',
  'ggcorrplot',
  'rstatix',
  'tidyverse'
  )

load_and_install(package_list = pckgs)


# deepfake ratings data
df <- read.csv('face databases/deepfakes/deepfake ratings.csv')

colnames(df)

# don't worry about participant-level data, analyses will be done at stimulus-level
# simply need to build out a dataset of the stimuli to be used in the main study

# in the survey, participants saw one of 3 blocks of stimuli, 20 faces per block
# in all blocks the order was as follows:
# female - old, male - old, female - young, male - young
# trait ratings were in the following order:
# trustworthy, attractive, dominant, warm, intelligent, age

# rename the 'fl_8_do' column (block)
names(df)[404] <- 'block'

# grab the columns that will be needed for the analyses
df <- df %>%
  select(-contains(
    c('Date',
      'Status',
      'IPAddress',
      'Progress',
      'Duration',
      'Finished',
      'Name',
      'Recipient',
      'External',
      'Location',
      'Distribution',
      'Language',
      'consent',
      '_DO')
    )) %>%
  select(ResponseId:X20_age_1.1, block)


# now split by block
b1 <- df %>% 
  select(ResponseId:X20_age._1, block) %>%
  filter(block == 'faces1')
b2 <- df %>%
  select(ResponseId, X1_traits_1.1:X20_age_1, block) %>%
  filter(block == 'faces2')
b3 <- df %>%
  select(ResponseId, X1_traits_1.2:X20_age_1.1, block) %>%
  filter(block == 'faces3')


# create a vector to hold new column names
b1N <- paste0(
  rep('stim'),
  rep(c(1:5, 16:20, 31:35, 46:50), each = 6),
  rep(c('Fem', 'Mal'), each = 30),
  rep(c('Old', 'Yng'), each = 60),
  rep(c('Tr', 'At', 'Dm', 'Wm', 'In', 'Ag'))
)

b2N <- paste0(
  rep('stim'),
  rep(c(6:10, 21:25, 36:40, 51:55), each = 6),
  rep(c('Fem', 'Mal'), each = 30),
  rep(c('Old', 'Yng'), each = 60),
  rep(c('Tr', 'At', 'Dm', 'Wm', 'In', 'Ag'))
)

b3N <- paste0(
  rep('stim'),
  rep(c(11:15, 26:30, 41:45, 56:60), each = 6),
  rep(c('Fem', 'Mal'), each = 30),
  rep(c('Old', 'Yng'), each = 60),
  rep(c('Tr', 'At', 'Dm', 'Wm', 'In', 'Ag'))
)


# apply name vectors to block column names
names(b1)[2:121] <- b1N
names(b2)[2:121] <- b2N
names(b3)[2:121] <- b3N



# reshape each to long, then merge
b1L <- b1 %>%
  pivot_longer(cols = stim1FemOldTr:stim50MalYngAg,
               names_to = 'stimID', values_to = 'rating') %>%
  separate(col = 'stimID', into = c('stimID', 'trait'), sep = -2) %>%
  separate(col = 'stimID', into = c('stimID', 'age'), sep = -3) %>%
  separate(col = 'stimID', into = c('stimID', 'gender'), sep = -3) %>%
  mutate(age = ifelse(age == 'Yng', 'Young', 'Old')) %>%
  mutate(gender = ifelse(gender == 'Fem', 'Female', 'Male')) %>%
  pivot_wider(names_from = 'trait', values_from = 'rating')

b2L <- b2 %>%
  pivot_longer(cols = stim6FemOldTr:stim55MalYngAg,
               names_to = 'stimID', values_to = 'rating') %>%
  separate(col = 'stimID', into = c('stimID', 'trait'), sep = -2) %>%
  separate(col = 'stimID', into = c('stimID', 'age'), sep = -3) %>%
  separate(col = 'stimID', into = c('stimID', 'gender'), sep = -3) %>%
  mutate(age = ifelse(age == 'Yng', 'Young', 'Old')) %>%
  mutate(gender = ifelse(gender == 'Fem', 'Female', 'Male')) %>%
  pivot_wider(names_from = 'trait', values_from = 'rating')

b3L <- b3 %>%
  pivot_longer(cols = stim11FemOldTr:stim60MalYngAg,
               names_to = 'stimID', values_to = 'rating') %>%
  separate(col = 'stimID', into = c('stimID', 'trait'), sep = -2) %>%
  separate(col = 'stimID', into = c('stimID', 'age'), sep = -3) %>%
  separate(col = 'stimID', into = c('stimID', 'gender'), sep = -3) %>%
  mutate(age = ifelse(age == 'Yng', 'Young', 'Old')) %>%
  mutate(gender = ifelse(gender == 'Fem', 'Female', 'Male')) %>%
  pivot_wider(names_from = 'trait', values_from = 'rating')


# merge the data frames
dfL <- rbind(b1L, b2L, b3L)

head(dfL)

names(dfL)[6:11] <- c(
  'trustworthy',
  'attractive',
  'dominant',
  'warm',
  'intelligent',
  'percAge'
  )

# r does that annoying thing with numbers, so fix that for the stimID column
stimLevels <- paste0(rep('stim'), 1:60)
dfL$stimID <- factor(dfL$stimID, levels = stimLevels)


# now get stimulus-level ratings
dfStim <- dfL %>%
  group_by(stimID, gender, age) %>%
  summarize(
    trustworthy = mean(trustworthy, na.rm = T),
    attractive = mean(attractive, na.rm = T),
    dominant = mean(dominant, na.rm = T),
    warm = mean(warm, na.rm = T),
    intelligent = mean(intelligent, na.rm = T),
    percAge = mean(percAge, na.rm = T)
  )



# compare ratings
# trustworthiness 
tr <- aov_4(trustworthy ~ gender * age + (1|stimID), data = dfStim)
tr
pairs(emmeans(tr, ~ gender))
# female > male (F(1, 56) = 43.59, p < .001, ges = .44)


# attractiveness
attr <- aov_4(attractive ~ gender * age + (1|stimID), data = dfStim)
attr

attrY <- aov_4(attractive ~ gender + (1|stimID), data = filter(dfStim, age == 'Young'))
attrY
pairs(emmeans(attrY, ~ gender))

attrO <- aov_4(attractive ~ gender + (1|stimID), data = filter(dfStim, age == 'Old'))
attrO
pairs(emmeans(attrO, ~ gender))


# dominance
dom <- aov_4(dominant ~ gender * age + (1|stimID), data = dfStim)
dom

domY <- aov_4(dominant ~ gender + (1|stimID), data = filter(dfStim, age == 'Young'))
domY

domO <- aov_4(dominant ~ gender + (1|stimID), data = filter(dfStim, age == 'Old'))
domO
pairs(emmeans(domO, ~ gender))


# warmth
warm <- aov_4(warm ~ gender * age + (1|stimID), data = dfStim)
warm
pairs(emmeans(warm, ~ gender))


# intelligence
intel <- aov_4(intelligent ~ gender * age + (1|stimID), data = dfStim)
intel
pairs(emmeans(intel, ~ age))

intelY <- aov_4(intelligent ~ gender + (1|stimID), data = filter(dfStim, age == 'Young'))
intelY
pairs(emmeans(intelY, ~ gender))

intelO <- aov_4(intelligent ~ gender + (1|stimID), data = filter(dfStim, age == 'Old'))
intelO


# age 
age <- aov_4(percAge ~ gender * age + (1|stimID), data = dfStim)
age
pairs(emmeans(age, ~ gender))
pairs(emmeans(age, ~ age))

# write.csv(dfStim, 'face databases/deepfakes/deepfake stim ratings.csv', row.names = F)





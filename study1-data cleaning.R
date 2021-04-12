library(tidyverse)

##### CLEANING THE RAW DATASET IMPORTED FROM QUALTRICS #####

#setwd("C:/Users/Artur/OneDrive - University of Toronto/Desktop/thesis -docs/Study 1")

# Creating two datasets.
# 1) Numeric dataset
df <- read.csv("data-numeric.csv",na.strings = c("", "NA"))

# 2) Text-based dataset (to double-check numeric answers)
perguntas <- read.csv("data.csv")

df1 <- df # Will leave the raw dataset intact in case we need it later

# Cleaning unnecessary rows and columns that were imported from qualtrics
df1 = df1[-1:-2,]
df1 = df1[,-1:-17]

# R imported all data types as string. So we have to turn the numbers into numeric types.
# To that end, I created the function below to be used in data cleaning for each variable.
# parameters: x is the dataframe to be turned into numbers. 
# the first column of x SHOULD be id.

numerize <- function(x){
  for (i in 2:ncol(x))
  {
    x[i] <- as.numeric(unlist(x[i]))
  } 
  return (x)
}

############### Selecting only those who passed attentions checks ###############
# Creating clean df to store clean values
# Only the ids of subjects who passed attention checks
# Attention checks: Q1_45, Q32_3

# Adding the ids of only those who passed attention checks to the clean dataset
df1_clean <- df1 %>%
  filter(Q1_45==1 & Q32_3 == 2) %>%
  select(Q76) %>%
  rename('id'='Q76')

# Filtering in the original dataset too
df1 <- df1 %>%
  filter(Q1_45==1 & Q32_3 == 2)

# Filtering the text data set too
perguntas <- perguntas %>%
  filter(Q1_45 == "Strongly Disagree" & Q32_3=="A little")

############### CLEANING UP THE DATA ###############
# For each variable, we have to clean it according to how we score the items.
# So for each scale, we have to revert some items, sum scores for the same factor,
# rename them so it makes sense later.
# We will clean the data and the append the clean data to df1_clean

############### Cleaning up personality ###############

x <- df1%>%
  select(Q76,Q1_1:Q1_44) %>%
  numerize() %>% 
  na.omit() %>%
  mutate(
    extraversion = (Q1_1 + (6-Q1_6) + Q1_16 + Q1_11 + (6-Q1_21) + Q1_26 + (6-Q1_31) + Q1_36)/8,
    agreeableness = ((6- Q1_2) + Q1_7 + (6 - Q1_12) + Q1_17 + Q1_22 + (6-Q1_27) + Q1_32 + (6-Q1_37))/8,
    conscientiousness = (Q1_3 + (6-Q1_8) + Q1_13 + (6-Q1_18) + (6-Q1_23) + Q1_28 + Q1_33 + Q1_38 + (6-Q1_43))/9,
    neuroticism = (Q1_4 +(6- Q1_9) + Q1_14 + Q1_19 + (6-Q1_24)+ Q1_29 + (6-Q1_34)+ Q1_39)/8,
    openness = (Q1_5+Q1_10+Q1_15+Q1_20+Q1_25+Q1_30+(6-Q1_35)+Q1_40+(6-Q1_41)+Q1_44)/10,
    id = Q76
    )

# Merging personality into clean dataset
df1_clean <- left_join(df1_clean, select(x, c(id, extraversion, agreeableness, conscientiousness, neuroticism, openness)), by = NULL)

############### Cleaning up Loneliness scale (actually referred to as sense of belonging) ###############

# Loneliness items: Q2_1 a Q2_11

# Qualtrics had saved the values in a weird way (out of order). recoding the values and creating 
# new variables for the dimensions of the scale (emotional loneliness and social loneliness).
# Originally, the authors want us to count 1 for every positive response and 0 for every negative response.
# Because we want more variability, we will count every score. 
# The higher the score, the more lonely the subject feels. 

x <- df1 %>%
  select(Q76, Q2_1:Q2_11) %>%
  drop_na() %>%
  mutate(Q2_1=recode(Q2_1, '1'=1, '10'=2, '11'=3, '12'=4, '13'=5),
         Q2_2=recode(Q2_2, '1'=1, '10'=2, '11'=3, '12'=4, '13'=5),
         Q2_3=recode(Q2_3, '1'=1, '10'=2, '11'=3, '12'=4, '13'=5),
         Q2_4=recode(Q2_4, '1'=1, '10'=2, '11'=3, '12'=4, '13'=5),
         Q2_5=recode(Q2_5, '1'=1, '10'=2, '11'=3, '12'=4, '13'=5),
         Q2_6=recode(Q2_6, '1'=1, '10'=2, '11'=3, '12'=4, '13'=5),
         Q2_7=recode(Q2_7, '1'=1, '10'=2, '11'=3, '12'=4, '13'=5),
         Q2_8=recode(Q2_8, '1'=1, '10'=2, '11'=3, '12'=4, '13'=5),
         Q2_9=recode(Q2_9, '1'=1, '10'=2, '11'=3, '12'=4, '13'=5),
         Q2_10=recode(Q2_10, '1'=1, '10'=2, '11'=3, '12'=4, '13'=5),
         Q2_11=recode(Q2_11, '1'=1, '10'=2, '11'=3, '12'=4, '13'=5),
         emotional_loneliness = Q2_2 + Q2_3 + Q2_5 + Q2_6 + Q2_9 + Q2_10,
         social_loneliness = (6-Q2_1) + (6-Q2_4) + (6-Q2_7) + (6-Q2_8) + (6-Q2_11),
         loneliness = emotional_loneliness + social_loneliness
         )

# Merging loneliness scores into clean dataset
df1_clean <- left_join(df1_clean, select(x, c(Q76, emotional_loneliness, social_loneliness, loneliness)), by = c("id"='Q76'))

############### Cleaning up relationship with self (self-esteem scale) ###############

# "Low self-esteem responses are "disagree" or "strongly disagree" on items 1, 3, 4, 7, 10, 
# and "strongly agree" or "agree" on items 2, 5, 6, 8, 9."
# We are coding so that higher scores are related to higher self-esteem.

x <- df1 %>%
  select(Q76, Q21_1:Q21_10) %>%
  numerize() %>%
  drop_na() %>%
  mutate(
    self_esteem = Q21_1 + Q21_3 + Q21_4+ Q21_7 + Q21_10 + (5-Q21_2) + (5-Q21_5) + (5-Q21_6) + (5-Q21_8) + (5-Q21_9)
  )
  
df1_clean <- left_join(x = df1_clean, y = select(x, c(Q76, self_esteem)), by = c(id='Q76'))

############### Feelings about Life (actually satisfaction with life) ################
# Q19_1 to Q19_5

x <- df1 %>%
  select(Q76,Q19_1:Q19_5)%>%
  numerize() %>%
  mutate(
    sats_w_life = Q19_1 + Q19_2 + Q19_3 + Q19_4 + Q19_5
  )

df1_clean <- left_join(df1_clean, select(x, c(Q76, sats_w_life)), by = c(id='Q76'))


############### Boredom ###############
# Q61_1 to Q61_8

x <- df1 %>%
  select(Q76,Q61_1:Q61_8)%>%
  numerize() %>%
  mutate(
    boredom = Q61_1 + Q61_2 + Q61_3 + Q61_4 + Q61_5 + Q61_6 + Q61_7 + Q61_8)

df1_clean <- left_join(df1_clean, select(x, c(Q76, boredom)), by = c(id='Q76'))

#################### Ulriks Questions #################
# These questions pertain to another researcher. He wanted to collect data on a
# scale he was developing, so I collected data for him.
# I'm just cleaning the data and preparing to send it to him. 

# Q62 to Q72_59

#ulrik_numeric <- df1 %>%
#  select(Q37, Q76, Q62:Q72_59)
#ulrik_text <- perguntas %>%
#  select(Q37, Q76, Q62:Q72_59)
#write.csv(ulrik_numeric,"C:/Users/Artur/OneDrive - University of Toronto/Desktop/thesis -docs/social-media-study-1/scales_numeric.csv", row.names = TRUE)
#write.csv(ulrik_text,"C:/Users/Artur/OneDrive - University of Toronto/Desktop/thesis -docs/social-media-study-1/scales_text.csv", row.names = TRUE)

############### UPPS (digital impulsivity scale)###############
# Factors and items
# Positive Urgency: 30, 15, 5, 35
# Negative Urgency: 44, 12, 34, 17 (note these two factors are actually tagged as one factor, but for theoretical reasons kept separate)
# Perseverance: 32, 24, 37, 42
# Premeditation: 33, 48, 55, 6
# Sensation-seeking: 31, 3, 18, 8

# FIRST let's reverse the items. Making it so that higher scores are higher impulsivity. 
# Items to reverse: 1  6 11 14 16 19 21 28 33 38 43 48 53 55

# Creating list of items to be reversed
items <- c(1, 6, 11, 14, 16, 19, 21, 28, 33, 38, 43, 48, 53, 55)
to_reverse <- c(paste("Q72", items, sep = "_"))

df1[,to_reverse] = lapply(to_reverse,  function(x) 6 - as.numeric(df1[, x]))

x <- df1 %>%
  select(Q76, Q72_1:Q72_59) %>%
  numerize() %>%
  mutate(
    pos_urg = Q72_30 + Q72_15 + Q72_5 + Q72_35,
    neg_urg = Q72_44 + Q72_12 + Q72_34 + Q72_17,
    persev = Q72_32 + Q72_24 + Q72_37 + Q72_42,
    premed = Q72_33 + Q72_48 + Q72_55 + Q72_6,
    sens_seek = Q72_31 + Q72_3 + Q72_18 + Q72_8,
    impulsivity = pos_urg + neg_urg + persev + premed + sens_seek
  )

df1_clean <- left_join(df1_clean, select(x, c(Q76, pos_urg, neg_urg, persev, premed, sens_seek, impulsivity)), by = c(id='Q76'))


############### Social media use ###############
# What social media sites participants used
# This was a multiple answer question. So we had to take every number in the answer,
# map into the correspondent social media site, create a new variable for that site,
# and then (after the join) substitute the NAs (participants who didn't check it)
# by 0, since it is a binary variable. 

x <- df1 %>%
  select(Q76, Q22) %>%
  filter(grepl('1', Q22)) %>%
  mutate(facebook = 1)

replace(is.na(.), 0)
df1_clean <- left_join(df1_clean, select(x, c(Q76, facebook)), by = c('id'='Q76'))
df1_clean$facebook[is.na(df1_clean$facebook)] <- 0

x <- df1 %>%
  select(Q76, Q22) %>%
  filter(grepl('2', Q22)) %>%
  mutate(instagram = 1)

df1_clean <- left_join(df1_clean, select(x, c(Q76, instagram)), by = c('id'='Q76'))
df1_clean$instagram[is.na(df1_clean$instagram)] <- 0

x <- df1 %>%
  select(Q76, Q22) %>%
  filter(grepl('3', Q22)) %>%
  mutate(youtube = 1)

df1_clean <- left_join(df1_clean, select(x, c(Q76, youtube)), by = c('id'='Q76'))
df1_clean$youtube[is.na(df1_clean$youtube)] <- 0

x <- df1 %>%
  select(Q76, Q22) %>%
  filter(grepl('4', Q22)) %>%
  mutate(whatsapp = 1)

df1_clean <- left_join(df1_clean, select(x, c(Q76, whatsapp)), by = c('id'='Q76'))
df1_clean$whatsapp[is.na(df1_clean$whatsapp)] <- 0

x <- df1 %>%
  select(Q76, Q22) %>%
  filter(grepl('5', Q22)) %>%
  mutate(twitter = 1)

df1_clean <- left_join(df1_clean, select(x, c(Q76, twitter)), by = c('id'='Q76'))
df1_clean$twitter[is.na(df1_clean$twitter)] <- 0

x <- df1 %>%
  select(Q76, Q22) %>%
  filter(grepl('7', Q22)) %>%
  mutate(tiktok = 1)

df1_clean <- left_join(df1_clean, select(x, c(Q76, tiktok)), by = c('id'='Q76'))
df1_clean$tiktok[is.na(df1_clean$tiktok)] <- 0

x <- df1 %>%
  select(Q76, Q22, Q22_8_TEXT) %>%
  filter(grepl('8', Q22)) %>%
  mutate(other = Q22_8_TEXT)

df1_clean <- left_join(df1_clean, select(x, c(Q76, other)), by = c('id'='Q76'))

############### Time spent on social media ################
# Time spent on social media through smartphones

x <- df1 %>%
  select(Q76, Q23) %>%
  filter(grepl(':', Q23)) %>%
  separate(Q23, into = c('smartphone_hours', 'smartphone_minutes'), sep = ':') %>%
  drop_na() %>%
  numerize() %>%
  mutate(smartphone_SM_daily = (smartphone_hours*60) + smartphone_minutes)

df1_clean <- left_join(df1_clean, select(x, c(Q76, smartphone_SM_daily)), by = c(id = 'Q76'))

# Time spent on social media through computers

x <- df1 %>%
  select(Q76, Q59) %>%
  filter(grepl(':', Q59)) %>%
  separate(Q59, into = c('desktop_hours', 'desktop_minutes'), sep = ':') %>%
  drop_na() %>%
  numerize() %>%
  mutate(desktop_SM_daily = (desktop_hours*60) + desktop_minutes)

df1_clean <- left_join(df1_clean, select(x, c(Q76, desktop_SM_daily)), by = c(id = 'Q76'))

################ Online Community #################
# cleaning for "are you part of an online community?" 1 - yes, 0 - no

x <- df1 %>%
  select(Q76, Q5) %>%
  mutate(online_community = recode(Q5, '2'=0, '1'=1))
df1_clean <- left_join(df1_clean, select(x, c(Q76, online_community)), by = c(id="Q76"))

# cleaning the names of the communities

x <- df1 %>%
  select(Q76, Q6) %>%
  mutate(community_name = Q6)
df1_clean <- left_join(df1_clean, select(x, c(Q76, community_name)),  by = c(id='Q76'))


################ Sense of community online ################
# Part of the scale is an adaptation by me, and another part is a real scale
# This is why I'll do them separately

# items 1-8: my scale, items 9 - 17: Psychological Sense of Community Scale
# Q4_1 to Q4_17

x <- df1 %>%
  select(Q76, Q4_1:Q4_17)%>%
  numerize()%>%
  mutate(sense_community_online = Q4_1 + Q4_2 + Q4_3 + Q4_5 + Q4_6 + Q4_7 + Q4_8,
         psych_sense_community = Q4_9 + Q4_10 + Q4_11 + Q4_12 + Q4_13 + Q4_14 + Q4_15 + Q4_16 + Q4_17
         )

df1_clean <- left_join(df1_clean, select(x, c(Q76, sense_community_online, psych_sense_community)), by = c(id='Q76'))

############### Functions of social media use ###############
# Getting the responses of the new scale so Mickey can perform a factor analysis
# Q8_1 to Q8_28
#x <- df1 %>%
#  select(Q8_1:Q8_28)
#write.csv(x, file = 'SM_functions_of_use.csv', row.names = TRUE)
#a <- perguntas %>%
#  select(Q8_1:Q8_28)
#write.csv(a, file = "functions_of_use_NAMES.csv", row.names = FALSE )
#a %>% slice(1:1)
#y <- rbind(b, x)
#write.csv(y, file = "Functions_of_SM_use.csv")

# Cleaned data and added labels by hand on excel. Imported clean data

x <- df1 %>%
  select(Q76, Q8_1:Q8_28) %>%
  numerize() %>%
  mutate(
    relaxation = Q8_25 + Q8_26 + Q8_14,
    entertainment = Q8_7+Q8_5+Q8_4,
    commercial = Q8_16+Q8_8+Q8_27,
    info_seeking = Q8_18+Q8_17+Q8_11,
    interacting = Q8_21+Q8_10+Q8_6
  )

df1_clean <- left_join(df1_clean, select(x, c(Q76, relaxation, entertainment, commercial, info_seeking, interacting)), by=c(id='Q76'))


##################### Accounts following ####################
# What types of account participants follow? Q9_1 to Q9_13

x <- df1 %>%
  select(Q76, Q9_1:Q9_13_TEXT) %>%
  mutate(
    follows_friends = Q9_1,
    follows_family = Q9_2, 
    follows_acquaintances = Q9_3,
    follows_humor = Q9_4,
    follows_news = Q9_5,
    follows_influencers = Q9_6,
    follows_business = Q9_7,
    follows_famous = Q9_8,
    follows_blogs = Q9_9,
    follows_commoninterest = Q9_10,
    follows_interestingthings = Q9_12,
    follows_other = Q9_13,
    others = Q9_13_TEXT
    )

df1_clean <- left_join(df1_clean, select(x, c(Q76, follows_friends, follows_family, follows_acquaintances, follows_humor, follows_news, follows_influencers,
                                              follows_business, follows_famous, follows_blogs, follows_commoninterest, follows_interestingthings, follows_other, others)), by = c(id='Q76'))

############### Relationship with social media (in reality SM addiction) ################
# Q10_1 to Q10_7

x <- df1 %>%
  select(Q76, Q10_1:Q10_7) %>%
  numerize() %>%
  mutate(
    SM_addiction = Q10_1 + Q10_2 + Q10_3 + Q10_4 + Q10_5 + Q10_6 + Q10_7
  )

df1_clean <- left_join(df1_clean, select(x,c(Q76, SM_addiction)), by = c(id="Q76"))

############### Online stalking ################
# Q11, Q13, Q12, Q31, Q15, Q38, Q38_5_TEXT

# q11: have you ever stalked someone? 1 - yes; 0 - no
x <- df1 %>%
  select(Q76, Q11, Q13, Q12, Q31, Q15, Q38, Q38_5_TEXT) %>%
  mutate(stalked = recode(Q11, '1'=1, '2'=0),
         stalking_freq = Q13,
         howmany_stalked = Q12,
         gender_stalked = Q31,
         characteristic_stalked = Q15,
         relationship_stalked = Q38,
         relationship_stalked_text = Q38_5_TEXT
         )

df1_clean <- left_join(df1_clean, select(x, c(Q76, stalked, stalking_freq, howmany_stalked, gender_stalked, characteristic_stalked,
                                              relationship_stalked, relationship_stalked_text)), by=c(id='Q76'))
############### Political Orientation ###############
# Q39, Q39_9_TEXT

# 1 = very liberal, 7 = very conservative
x <- df1 %>%
  select(Q76, Q39, Q39_9_TEXT) %>%
  numerize() %>%
  filter(Q39!=9 | Q39 != 8) %>%
  mutate(
    political_party = Q39
  )

df1_clean <- left_join(df1_clean, select(x, c(Q76, political_party)), by = c(id='Q76'))

############### political engagement ###############
# Q16_1 Q16_5

x <- df1 %>%
  select(Q76, Q16_1:Q16_5) %>%
  mutate(can_vote = (recode(Q16_1, '1'=1, '2'=0)),
         have_voted = recode(Q16_2, '1'=1, '2'=0),
         contributed_campaign = recode(Q16_3, '1'=1, '2'=0),
         protested = recode(Q16_4, '1'=1, '2'=0),
         support_candidate=recode(Q16_5, '1'=1, '2'=0),
         pol_engagement = can_vote + have_voted + contributed_campaign + protested + support_candidate
           )
df1_clean <- left_join(df1_clean, select(x, c(Q76, pol_engagement)), by=c(id='Q76'))

############### Party Feelings ###############
# Q17_1(feelings towards dems) and Q17_2 (feelings towards reps)

x <- df1 %>%
  select(Q76, Q17_1, Q17_2) %>%
  numerize() %>%
  mutate(
    feelings_dems = Q17_1,
    feelings_reps = Q17_2
  )
df1_clean <- left_join(df1_clean, select(x, c(Q76, feelings_dems, feelings_reps)), by = c(id='Q76'))

############### Political Opinion (political consistency scale) ###############
# Q18_1 to Q18_20
# Data was imported weird, had to recode the values. 

x <- df1 %>%
  select(Q76, Q18_1:Q18_20) %>%
  mutate(con1 = recode(Q18_1, '11'=1, '12'=2, '13'=3, '14'=4, '15'=5),
         con2 = recode(Q18_2, '11'=1, '12'=2, '13'=3, '14'=4, '15'=5),
         con3 = recode(Q18_3, '11'=1, '12'=2, '13'=3, '14'=4, '15'=5),
         con4 = recode(Q18_4, '11'=1, '12'=2, '13'=3, '14'=4, '15'=5),
         con5 = recode(Q18_5, '11'=1, '12'=2, '13'=3, '14'=4, '15'=5),
         con6 = recode(Q18_6, '11'=1, '12'=2, '13'=3, '14'=4, '15'=5),
         con7 = recode(Q18_7, '11'=1, '12'=2, '13'=3, '14'=4, '15'=5),
         con8 = recode(Q18_8, '11'=1, '12'=2, '13'=3, '14'=4, '15'=5),
         con9 = recode(Q18_9, '11'=1, '12'=2, '13'=3, '14'=4, '15'=5),
         con10 = recode(Q18_10, '11'=1, '12'=2, '13'=3, '14'=4, '15'=5),
         lib1 = recode(Q18_11, '11'=1, '12'=2, '13'=3, '14'=4, '15'=5),
         lib2 = recode(Q18_12, '11'=1, '12'=2, '13'=3, '14'=4, '15'=5),
         lib3 = recode(Q18_13, '11'=1, '12'=2, '13'=3, '14'=4, '15'=5),
         lib4 = recode(Q18_14, '11'=1, '12'=2, '13'=3, '14'=4, '15'=5),
         lib5 = recode(Q18_15, '11'=1, '12'=2, '13'=3, '14'=4, '15'=5),
         lib6 = recode(Q18_16, '11'=1, '12'=2, '13'=3, '14'=4, '15'=5),
         lib7 = recode(Q18_17, '11'=1, '12'=2, '13'=3, '14'=4, '15'=5),
         lib8 = recode(Q18_18, '11'=1, '12'=2, '13'=3, '14'=4, '15'=5),
         lib9 = recode(Q18_19, '11'=1, '12'=2, '13'=3, '14'=4, '15'=5),
         lib10 = recode(Q18_20, '11'=1, '12'=2, '13'=3, '14'=4, '15'=5),
         con_ideas = con1 + con2 + con3 + con4 + con5 + con6 + con7 + con8 + con9 + con10,
         lib_ideas = lib1 + lib2 + lib3 + lib4 + lib5 + lib6 + lib7 + lib8 + lib9 + lib10 
         )

df1_clean <- left_join(df1_clean, select(x, c(Q76, con_ideas, lib_ideas)), by = c(id='Q76'))

############### Trust in Government ##################

x <- df1 %>%
  select(Q76, Q25) %>%
  numerize() %>%
  mutate(trust_govt = Q25)

df1_clean <- left_join(df1_clean, select(x, c(Q76, trust_govt)), by = c(id='Q76'))

################ Marriage between parties ################

x<- df1 %>%
  select(Q76, Q63_1, Q63_2) %>%
  numerize() %>%
  mutate(married_dem = Q63_1,
         married_rep = Q63_2)

df1_clean <- left_join(df1_clean, select(x, c(Q76, married_dem, married_rep)), by = c(id='Q76'))

################# News consumption habits ################
# Q27, Q28_1 to Q28_6 and Q28_6_TEXT

x <- df1 %>%
  select(Q76, Q27:Q28_6) %>%
  numerize() %>%
  mutate(
    news_cons_freq = Q27,
    news_socialmedia = Q28_1,
    news_newspaper = Q28_2,
    news_searchengine = Q28_3,
    news_aggregator = Q28_4,
    news_cabletv = Q28_5,
    news_other = Q28_6
  )

df1_clean <- left_join(df1_clean, select(x, c(Q76, news_cons_freq, news_socialmedia, news_newspaper, news_searchengine, news_aggregator, news_cabletv, news_other)), by = c(id='Q76'))

################# News media suspicion ##################
# Q32_1 to Q32_5, BUT Q32_3 is an attention check

x <- df1 %>%
  select(Q76, Q32_1, Q32_2, Q32_4, Q32_5) %>%
  numerize() %>%
  mutate(
    news_suspicion = Q32_1 + Q32_2 + Q32_4 + Q32_5
  )

df1_clean <- left_join(df1_clean, select(x, c(Q76, news_suspicion)), by = c(id='Q76'))

################# Shared ad regretted ##################
# Q50
# The higher the score, the more they have posted and regretted later

x <- df1 %>%
  select(Q76, Q50) %>%
  numerize() %>%
  mutate(shared_regretted = 6- Q50)

df1_clean <- left_join(df1_clean, select(x, c(Q76, shared_regretted)), by = c(id='Q76'))

################# Would share news? ################
# Q49_1 to Q49_9
# "Coronavirus North Korea's First Confirmed Patient Shot Dead"
# "Taiwan Experts Provide a Simple Self-Check That We Can do Every Morning"
# "Here's Proof That it's Actually Vaccinated People Who are Spreading Communicable Disease"
# "Adults with Covid-19 Twice as Likely to Have Eaten at Restaurants"
# "Coronavirus Spread may Last into 2021, but Impacts can be Blunted"
# "Teen Who Dies of Covid-19 was Denied Treatment Because he Didn't Have Health Insurance"
# "Vitamin C Protects against Coronavirus"
# "U.S. Sent Millions of Face Masks to China early This Year Ignoring Pandemic Warning Signs"
# "Vladimir Putins's daughter dies after COVID vaccine"

# 1- would read it, 2- would like it, 3- would share it, 4- would not engage with it

# Not sure how to examine this after cleaning it 

x <- df1 %>%
  select(Q76, Q49_1:Q49_9) %>%
  mutate(
    news_1 = Q49_1,
    news_2 = Q49_2,
    news_3 = Q49_3,
    news_4 = Q49_4,
    news_5 = Q49_5,
    news_6 = Q49_6,
    news_7 = Q49_7,
    news_8 = Q49_8,
    news_9 = Q49_9
  )
  
df1_clean <- left_join(df1_clean, select(x, c(Q76, news_1, news_2, news_3, news_4, news_5,news_6,news_7,news_8,news_9)), by = c(id = 'Q76'))

############### Creating some variables I need ###############

# Total Social Media usage time
# To create that variable, I need so sum the time on desktop and smartphone devices
a <- df1_clean%>%
  select(desktop_SM_daily, smartphone_SM_daily)

a <- as.data.frame(a)
a[is.na(a)] <- 0
df1_clean$total_use <- a$smartphone_SM_daily + a$desktop_SM_daily
df1_clean$total_use[df1_clean$total_use == 0] <- NA

# Total polarization score
df1_clean$polarization <- abs(df1_clean$feelings_dems - df1_clean$feelings_reps)

############### Joining with data from prolific ###############
# data is in differenct csv docs because I ran two batches (1 pilot)
info <- read.csv("prolific_info_participants.csv")
info2 <- read.csv("prolific_info_participants2.csv")

df1_clean <- left_join(df1_clean, select(info, c(participant_id, age, Sex)), by = c('id'="participant_id"))

info2 <- rename(info2, 'id' = 'participant_id')

info2 <- info2 %>% 
  select(id, age, Sex)

df1_clean <- merge(df1_clean, info2, by='id', all.x = T)
df1_clean <- df1_clean[,-89:-90]
df1_clean <- rename(df1_clean, age = age.x, Sex = Sex.x)



############### Writing new CSV file with clean dataset ###############
write.csv(df1_clean, file = "clean_dataset.csv")



#### PRIMO SGUARDO a df_5 ####

str(df_5_camp_cat)
summary(df_5_camp_cat)

#### PULIZIA df_5 ####

df_5_camp_cat_clean <- df_5_camp_cat

#### PULIZIA VARIABILE con varianza bassa in df_5: cancello la colonna Channel_camp ####

df_5_camp_cat_clean <- df_5_camp_cat_clean %>%
  select(-CHANNEL_CAMP)

#### REVIEW FINALE df_5_clean ####

str(df_5_camp_cat_clean)
summary(df_5_camp_cat_clean)


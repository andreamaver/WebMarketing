#### PRIMO SGUARDO a df_4 ####

str(df_4_cli_privacy)
summary(df_4_cli_privacy)

#### PULIZIA df_4 ####

df_4_cli_privacy_clean <- df_4_cli_privacy

#### PULIZIA DUPLICATI in df_4 ####

## controllo duplicati
df_4_cli_privacy_clean %>%
  summarize(TOT_ID_CLIs = n_distinct(ID_CLI)
            , TOT_ROWs = n())

#!!! nessun duplicato !!!#

#### PULIZIA DATA TYPES in df_4 ####

## da booleani a factor ##
df_4_cli_privacy_clean <- df_4_cli_privacy_clean %>%
  mutate(FLAG_PRIVACY_1 = as.factor(FLAG_PRIVACY_1)) %>%
  mutate(FLAG_PRIVACY_2 = as.factor(FLAG_PRIVACY_2)) %>%
  mutate(FLAG_DIRECT_MKT = as.factor(FLAG_DIRECT_MKT))

#### CONTROLLO CONSISTENZA ID_CLI in df_1/df_4 ####

cons_idcli_df1_df4 <- df_1_cli_fid_clean %>%
  select(ID_CLI) %>%
  mutate(is_in_df_1 = 1) %>%
  distinct() %>%
  full_join(df_4_cli_privacy_clean %>%
              select(ID_CLI) %>%
              mutate(is_in_df_4 = 1) %>%
              distinct()
            , by = "ID_CLI"
  ) %>%
  group_by(is_in_df_1, is_in_df_4) %>%
  summarize(NUM_ID_CLIs = n_distinct(ID_CLI)) %>%
  as.data.frame()

cons_idcli_df1_df4

#!!! tutti gli ID_CLI in df_1 sono anche in df_4 e viceversa !!!#

#### ESPLORIAMO LE COLONNE di df_4 ####

#### ???? TO DO df_4 ???? ####
# ESPLORIAMO LE variabili rilevanti in df_4_cli_privacy_clean
df_4_privacy <- df_4_cli_privacy_clean %>%
  group_by(FLAG_PRIVACY_1,FLAG_PRIVACY_2,FLAG_DIRECT_MKT) %>%
  summarize(TOT_ID_CLI = n_distinct(ID_CLI)) %>%
  mutate(PERCENT = TOT_ID_CLI/n_distinct(df_4_cli_privacy_clean)) %>%
  arrange(desc(PERCENT)) %>%
  as.data.frame()

df_4_privacy
# la maggior parte dei clienti dà il consenso a tutti e tre

#### REVIEW FINALE df_4_clean ####

str(df_4_cli_privacy_clean)
summary(df_4_cli_privacy_clean)


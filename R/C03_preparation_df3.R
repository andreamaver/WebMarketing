#### Primo sguardo a df_3 ####

str(df_3_cli_address)
summary(df_3_cli_address)

#### PULIZIA df_3 ####

df_3_cli_address_clean <- df_3_cli_address

#### PULIZIA DUPLICATI df_3 ####

## controllo duplicati
df_3_cli_address_clean %>%
  summarize(TOT_ID_ADDRESSes = n_distinct(ID_ADDRESS)
            , TOT_ROWs = n())

#!!! abbiamo molti duplicati !!!#

df_3_cli_address_clean <- df_3_cli_address_clean %>%
  distinct()

#### PULIRE I DATA TYPE in df_3 ####

## da stringa a factor ##
df_3_cli_address_clean <- df_3_cli_address_clean %>%
  mutate(CAP = as.character(CAP))

#### PULIRE I VALORI MANCANTI in df_3 ####

df_3_cli_address_clean %>%
  group_by(w_CAP = !is.na(CAP)
           , w_PRV = !is.na(PRV)
           , w_REGION = !is.na(REGION)) %>%
  summarize(TOT_ADDs = n_distinct(ID_ADDRESS))

## esaminiamo alcuni valori mancanti per la colonna REGION
df_3_cli_address_clean %>% filter(!is.na(PRV) & is.na(REGION))

## rimuoviamo le righe relative ai valori mancanti ##
df_3_cli_address_clean <- df_3_cli_address_clean %>%  
  filter(!is.na(CAP) & !is.na(PRV) & !is.na(REGION))

#### CONTROLLO CONSISTENZA ID_ADDRESS in df_2/df_3 ####

cons_idaddress_df2_df3 <- df_2_cli_account_clean %>%
  select(ID_ADDRESS) %>%
  mutate(is_in_df_2 = 1) %>%
  distinct() %>%
  full_join(df_3_cli_address_clean %>%
              select(ID_ADDRESS) %>%
              mutate(is_in_df_3 = 1) %>%
              distinct()
            , by = "ID_ADDRESS"
  ) %>%
  group_by(is_in_df_2, is_in_df_3) %>%
  summarize(NUM_ID_ADDRESSes = n_distinct(ID_ADDRESS)) %>%
  as.data.frame()

cons_idaddress_df2_df3

#!!!  ci sono ID_ADDRESSes non mappati in df_3 !!!#
#!!!  da ricordare quando si uniscono df2 e df3 !!!#

#### ESPLORIAMO LE COLONNE DI df_3 ####

#### ???? TO DO df_3 ???? ####
# ESPLORIAMO le variabili rilevanti in df_3_cli_address_clean

#Distribuzione nelle province
df_3_prv <- df_3_cli_address_clean %>%
  group_by(PRV) %>%
  summarize(TOT_ID_ADDRESS = n_distinct(ID_ADDRESS)) %>%
  mutate(PERCENT = TOT_ID_ADDRESS/sum(TOT_ID_ADDRESS)) %>%
  arrange(desc(PERCENT)) %>%
  as.data.frame()

df_3_prv

## selezioniamo le province con piu' clienti
df_best_prv <- df_3_prv %>%
  arrange(desc(PERCENT)) %>%
  mutate(PERCENT_COVERED = cumsum(TOT_ID_ADDRESS)/sum(TOT_ID_ADDRESS)) %>%
  mutate(PRV = as.character(PRV)) %>%
  mutate(AUX = if_else(PERCENT_COVERED < 0.50 | (PERCENT_COVERED > 0.50 & lag(PERCENT_COVERED) < 0.50), TRUE,FALSE)) %>%
  mutate(BEST_PRV = if_else(AUX, PRV, "others"))

head(df_best_prv, 20)

# grafico
plot_df_3_prv <- (
  ggplot(data=df_best_prv
         , aes(x=BEST_PRV, y=TOT_ID_ADDRESS)) +
    geom_bar(stat="identity"
             , fill="steelblue") +
    theme_minimal()
)

plot_df_3_prv

# Distribuzione nelle regioni
df_3_reg <- df_3_cli_address_clean %>%
  group_by(REGION) %>%
  summarize(TOT_ID_ADDRESS = n_distinct(ID_ADDRESS)) %>%
  mutate(PERCENT = TOT_ID_ADDRESS/sum(TOT_ID_ADDRESS)) %>%
  arrange(desc(PERCENT)) %>%
  as.data.frame()

df_3_reg

#### REVIEW FINALE df_3_clean ####

str(df_3_cli_address_clean)
summary(df_3_cli_address_clean)

write.csv(df_3_reg,file.path(data_dir,'df_3_reg.csv'))


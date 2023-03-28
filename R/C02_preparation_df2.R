#### PRIMO SGUARDO a df_2 ####

str(df_2_cli_account)
summary(df_2_cli_account)

#### PULIZIA df_2 ####

df_2_cli_account_clean <- df_2_cli_account

#### PULIZIA DUPLICATI in df_2 ####

## cerchiamo i duplicati
df_2_cli_account_clean %>%
  summarize(TOT_ID_CLIs = n_distinct(ID_CLI)
            , TOT_ROWs = n())

#!!! nessun duplicato !!!#

#### PULIRE I data type in df_2 ####

## formato booleano come factor ##
df_2_cli_account_clean <- df_2_cli_account_clean %>%
  mutate(W_PHONE = as.factor(W_PHONE))

## formato di categorie numeriche come factor ##
df_2_cli_account_clean <- df_2_cli_account_clean %>%
  mutate(TYP_CLI_ACCOUNT = as.factor(TYP_CLI_ACCOUNT))

#### PULIZIA MISSING VALUE in df_2 ####

## MISSING VALUE (NA) trasformati in 0 ##
df_2_cli_account_clean <- df_2_cli_account_clean %>%
  mutate(W_PHONE = fct_explicit_na(W_PHONE, "0"))

## MISSING VALUE trasformati in nuovi livelli in colonne categoriche ##
df_2_cli_account_clean <- df_2_cli_account_clean %>%  
  mutate(EMAIL_PROVIDER = fct_explicit_na(EMAIL_PROVIDER, "(missing)")) %>%
  mutate(TYP_JOB = fct_explicit_na(TYP_JOB, "(missing)"))

#### CONTROLLO SULLA CONSISTENZA DI ID_CLI in df_1/df_2 ####
## ID_CLI in df1 sono anche in df_2 e viceversa?

cons_idcli_df1_df2 <- df_1_cli_fid_clean %>%
  select(ID_CLI) %>%
  mutate(is_in_df_1 = 1) %>%
  distinct() %>%
  full_join(df_2_cli_account_clean %>%
              select(ID_CLI) %>%
              mutate(is_in_df_2 = 1) %>%
              distinct()
            , by = "ID_CLI"
  ) %>%
  group_by(is_in_df_1, is_in_df_2) %>%
  summarize(NUM_ID_CLIs = n_distinct(ID_CLI)) %>%
  as.data.frame()

cons_idcli_df1_df2

#!!! tutti gli ID_CLI in df_1 sono anche in df_2 e vice-versa !!!#

#### ESPLORA LE COLONNE di df_2 ####

### Variabile EMAIL_PROVIDER ###

## calcola la distribuzione
df_2_dist_emailprovider <- df_2_cli_account_clean %>%
  group_by(EMAIL_PROVIDER) %>%
  summarize(TOT_CLIs = n_distinct(ID_CLI)) %>%
  mutate(PERCENT = TOT_CLIs/sum(TOT_CLIs)) %>%
  arrange(desc(PERCENT)) %>%
  as.data.frame()

df_2_dist_emailprovider

tot_emailproviders <- n_distinct(df_2_dist_emailprovider$EMAIL_PROVIDER)

tot_emailproviders

#!!! troppi valori differenti per EMAIL_PROVIDER per essere una variabile categorica !!!#

#### ???? TO DO df_2 ???? ####
# CALCOLA LA DISTRIBUZIONE per le rimanenti variabili di df_2_cli_account_clean

#TYP_JOB=lavoro del cliente
df_2_typ_job <- df_2_cli_account_clean %>%
  group_by(TYP_JOB) %>%
  summarize(TOT_CLIs = n_distinct(ID_CLI)) %>%
  mutate(PERCENT = TOT_CLIs/sum(TOT_CLIs)) %>%
  arrange(desc(PERCENT)) %>%
  as.data.frame()

df_2_typ_job #perlopiuπ (missing), quindi non troppo rilevante

#TYP_CLI_ACCOUNT
df_2_typ_cli_acc <- df_2_cli_account_clean %>%
  group_by(TYP_CLI_ACCOUNT) %>%
  summarize(TOT_CLIs = n_distinct(ID_CLI)) %>%
  mutate(PERCENT = TOT_CLIs/sum(TOT_CLIs)) %>%
  arrange(desc(PERCENT)) %>%
  as.data.frame()

df_2_typ_cli_acc #netta maggioranza del 4

#W_PHONE
df_2_w_phone <- df_2_cli_account_clean %>%
  group_by(W_PHONE) %>%
  summarize(TOT_CLIs = n_distinct(ID_CLI)) %>%
  mutate(PERCENT = TOT_CLIs/sum(TOT_CLIs)) %>%
  arrange(desc(PERCENT)) %>%
  as.data.frame()

df_2_w_phone #ho il numero di telefono della netta maggioranza dei clienti

#### RIMODELLARE df_2 ####

## teniamo i piuπ frequenti valori di EMAIL_PROVIDER e aggiungiamo il livello "OTHER" per i rimanenti ##
df_2_dist_emailprovider %>%
  arrange(desc(PERCENT)) %>%
  mutate(PERCENT_COVERED = cumsum(TOT_CLIs)/sum(TOT_CLIs)) %>%
  as.data.frame() %>%
  head(20)

## teniamo il livello (missing) per motivi tecnici
## selezioniamo i livelli che coprono l'85% dei casi
clean_email_providers <- df_2_dist_emailprovider %>%
  arrange(desc(PERCENT)) %>%
  mutate(PERCENT_COVERED = cumsum(TOT_CLIs)/sum(TOT_CLIs)) %>%
  mutate(EMAIL_PROVIDER = as.character(EMAIL_PROVIDER)) %>%
  mutate(AUX = if_else(PERCENT_COVERED < 0.85 | (PERCENT_COVERED > 0.85 & lag(PERCENT_COVERED) < 0.85), 1,0)) %>%
  mutate(EMAIL_PROVIDER_CLEAN = if_else(AUX | EMAIL_PROVIDER == "(missing)", EMAIL_PROVIDER, "others"))

head(clean_email_providers, 20)

#Sostiutuiamo EMAIL_PROVIDER con la colonna EMAIL_PROVIDER_CLEAN
df_2_cli_account_clean <- df_2_cli_account_clean %>%
  mutate(EMAIL_PROVIDER = as.character(EMAIL_PROVIDER)) %>%
  left_join(clean_email_providers %>%
              select(EMAIL_PROVIDER, EMAIL_PROVIDER_CLEAN)
            , by = "EMAIL_PROVIDER") %>%
  select(-EMAIL_PROVIDER) %>%
  mutate(EMAIL_PROVIDER_CLEAN = as.factor(EMAIL_PROVIDER_CLEAN))


#### ESPLORIAMO LA NUOVA COLONNA EMAIL_PROVIDER_CLEAN in df_2 ####

## calcoliamo la distribuzione
df2_dist_emailproviderclean <- df_2_cli_account_clean %>%
  group_by(EMAIL_PROVIDER_CLEAN) %>%
  summarize(TOT_CLIs = n_distinct(ID_CLI)) %>%
  mutate(PERCENT = TOT_CLIs/sum(TOT_CLIs)) %>%
  arrange(desc(PERCENT))

df2_dist_emailproviderclean

## grafico della distribuzione
plot_df2_dist_emailproviderclean <- (
  ggplot(data=df2_dist_emailproviderclean
         , aes(x=EMAIL_PROVIDER_CLEAN, y=TOT_CLIs)) +
    geom_bar(stat="identity"
             , fill="#6c0f5e") +
    theme_minimal()
)

plot_df2_dist_emailproviderclean

#### ???? TO DO df_2 ???? ####
# ESPLORIAMO le rimanenti variabili importanti df_2_cli_account_clean

## variabile TYP_JOB
df_2_typ_job<-df_2_typ_job %>% filter(TYP_JOB != "(missing)") #tolgo i (missing perch√® sono la stragrande maggioranza)

plot_df2_typ_job <- (
  ggplot(data=df_2_typ_job
         , aes(x=TYP_JOB, y=TOT_CLIs)) +
    geom_bar(stat="identity"
             , fill="#6c0f5e") +
    theme_minimal()
)

plot_df2_typ_job

#variabile W_PHONE
lb = paste0(round(df_2_w_phone$PERCENT,2),"%")
pie(df_2_w_phone$PERCENT,labels = lb, col = c('#e2543b','#f09708'))
legend(-2.0,0.4,legend=df_2_w_phone$W_PHONE,cex=0.7,yjust=0.2, xjust = -0.1,
       fill = c('#e2543b','#f09708'), bty = "n")

#variabile TYP_CLI_ACCOUNT
lb = paste0(round(df_2_typ_cli_acc$PERCENT,3),"%")
pie(df_2_typ_cli_acc$PERCENT,labels = lb, col = c('#e2543b','#f09708'))
legend(-2.0,0.4,legend=df_2_typ_cli_acc$TYP_CLI_ACCOUNT,cex=0.7,yjust=0.2, xjust = -0.1,
       fill = c('#e2543b','#f09708'), bty = "n")

#### REVIEW FINALE df_2_clean ####

str(df_2_cli_account_clean)
summary(df_2_cli_account_clean)


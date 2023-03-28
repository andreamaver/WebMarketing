#### PRIMO SGUARDO a df_1 ####

str(df_1_cli_fid)
summary(df_1_cli_fid)

#### PULIZIA df_1 ####

df_1_cli_fid_clean <- df_1_cli_fid

#### PULIRE I DUPLICATI in df_1 ####

## cerco i duplicati
df_1_cli_fid_clean %>%
  summarize(TOT_ID_CLIs = n_distinct(ID_CLI) #numero di valori distinti di ID_CLI
            , TOT_ID_FIDs = n_distinct(ID_FID) #numero di valori distinti di ID_FID
            , TOT_ID_CLIFIDs = n_distinct(paste0(as.character(ID_CLI),"-",as.character(ID_FID)))
            #numero di valori distinti di ID_CLI-ID_FID
            , TOT_ROWs = n())
# TOT_ID_CLIs>TOT_ID_FIDs --> a qualche ID_FID corrispondono piu¹ ID_CLI
# TOT_ROWS>TOT_ID_CLIs --> Ho duplicati sia di ID_CLI sia di ID_FID

#!!! nessun duplicato per combinazioni CLI-FID !!!#

#### PULIRE I DATA TYPE in df_1 ####

## formattare le date ##
df_1_cli_fid_clean <- df_1_cli_fid_clean %>%
  mutate(DT_ACTIVE = as.Date(DT_ACTIVE))

## formattare i booleani come factor ##
df_1_cli_fid_clean <- df_1_cli_fid_clean %>%
  mutate(TYP_CLI_FID = as.factor(TYP_CLI_FID)) %>%
  mutate(STATUS_FID = as.factor(STATUS_FID))

#### CONTROLLO SULLA CONSISTENZA di df1: numero di sottoiscrizioni per cliente ####

## conta le sottoiscrizioni per ogni cliente
num_fid_x_cli <- df_1_cli_fid_clean %>%
  group_by(ID_CLI) %>%
  summarize(NUM_FIDs =  n_distinct(ID_FID)
            , NUM_DATEs = n_distinct(DT_ACTIVE)
            )

tot_id_cli <- n_distinct(num_fid_x_cli$ID_CLI)

## calcola la distribuzione del numero di sottoiscrizioni
dist_num_fid_x_cli <- num_fid_x_cli %>%
  group_by(NUM_FIDs, NUM_DATEs) %>%
  summarize(TOT_CLIs = n_distinct(ID_CLI)) %>%
  mutate(PERCENT_CLIs = TOT_CLIs/tot_id_cli)

dist_num_fid_x_cli
#Abbiamo alcuni clienti con piu¹ sottoiscrizioni fedelta',
#alcune fatte nello stesso giorno, altre fatte in giorni diversi

## esaminiamo nel dettaglio i clienti con piu¹ sottoiscrizioni

#clienti con 3 sottoiscrizioni
num_fid_x_cli %>% filter(NUM_FIDs == 3)

# le sottoiscrizioni che hanno date differenti
df_1_cli_fid %>% filter(ID_CLI == 621814)
# le sottoiscrizioni che hanno la stessa data [possibilmente per ragioni tecniche]
df_1_cli_fid %>% filter(ID_CLI == 320880)

#### RIMODELLAMENTO df_1 ####

## combiniamo le informazioni

# dalla prima sottoiscrizione  --> data di registrazione, negozio di registrazione
# dall'ultima sottoiscrizione  --> tipo di fedelta', status
# dalla conta delle sottoiscrizioni --> numero di sottoiscrizioni fatte

#prima sottoiscrizione
df_1_cli_fid_first <- df_1_cli_fid_clean %>%
  group_by(ID_CLI) %>%
  filter(DT_ACTIVE == min(DT_ACTIVE)) %>%
  arrange(ID_FID) %>%
  filter(row_number() == 1) %>%
  ungroup() %>%
  as.data.frame()

#ultima sottoiscrizione
df_1_cli_fid_last <- df_1_cli_fid_clean %>%
  group_by(ID_CLI) %>%
  filter(DT_ACTIVE == max(DT_ACTIVE)) %>%
  arrange(desc(ID_FID)) %>%
  filter(row_number() == 1) %>%
  ungroup() %>%
  as.data.frame()

#inserisco dati ultima sottoiscrizione in df_1_cli_fid_clean
df_1_cli_fid_clean <- df_1_cli_fid_last %>%
  select(ID_CLI
         , ID_FID
         , LAST_COD_FID = COD_FID
         , LAST_TYP_CLI_FID = TYP_CLI_FID
         , LAST_STATUS_FID = STATUS_FID
         , LAST_DT_ACTIVE = DT_ACTIVE) %>%
  left_join(df_1_cli_fid_first %>%
              select(ID_CLI
                     , FIRST_ID_NEG = ID_NEG
                     , FIRST_DT_ACTIVE = DT_ACTIVE)
            , by = 'ID_CLI') %>%
  left_join(num_fid_x_cli %>%
              select(ID_CLI
                     , NUM_FIDs) %>%
              mutate(NUM_FIDs = as.factor(NUM_FIDs))
            , by = 'ID_CLI')
# ora contiene anche info su numero sottoiscrizioni e quando sono state fatte

#### ESPLORIAMO LE COLONNE di df_1 ####

### variabile LAST_COD_FID ###

## calcoliamo la distribuzione
df1_dist_codfid <- df_1_cli_fid_clean %>%
  group_by(LAST_COD_FID) %>%
  summarize(TOT_CLIs = n_distinct(ID_CLI)) %>%
  mutate(PERCENT = TOT_CLIs/sum(TOT_CLIs)) %>%
  arrange(desc(PERCENT))

#tipo di sottoiscrizione
df1_dist_codfid

## grafico distribuzione
plot_df1_dist_codfid <- (
  ggplot(data=df1_dist_codfid
         , aes(x=LAST_COD_FID, y=TOT_CLIs)
         ) +
    geom_bar(stat="identity"
             , fill="#6c0f5e") +
    theme_minimal()
)

plot_df1_dist_codfid

#### ???? TO DO df_1 ???? ####
# ESPLORIAMO le rimanenti variabili importanti di df_1_cli_fid_clean

### variabile ID_NEG ###

## calcoliamo la distribuzione
df1_dist_id_neg <- df_1_cli_fid_clean %>%
  group_by(FIRST_ID_NEG) %>%
  summarize(TOT_CLIs = n_distinct(ID_CLI)) %>%
  mutate(PERCENT = TOT_CLIs/sum(TOT_CLIs)) %>%
  arrange(desc(PERCENT))

#negozio di sottoiscrizione: circa equamente distribuiti, esclusi i primi 3
df1_dist_id_neg
tot_sub <- n_distinct(df_1_cli_fid_clean$FIRST_ID_NEG)
tot_sub
#!!! troppi valori differenti per ID_NEG per essere una variabile categorica !!!#

#distribuzione sottoiscrizioni durante l'anno considerando solo ultime attivazioni
df_1_last_dt_active <- df_1_cli_fid_clean %>%
  group_by(LAST_DT_ACTIVE) %>%
  summarize(TOT_CLIs = n_distinct(ID_CLI)) %>%
  ungroup()  %>%
  as.data.frame()

plot(df_1_last_dt_active$LAST_DT_ACTIVE,df_1_last_dt_active$TOT_CLIs)
par(mfrow=c(1,1),cex.axis=1, cex.lab=1, cex.main=1, fg="black",col.axis="black", col.lab="black", col.main="black", bg="white")
plot(df_1_last_dt_active$LAST_DT_ACTIVE,df_1_last_dt_active$TOT_CLIs,
     xlab="", ylab="n° sottoiscrizioni", col="light green")

#raggruppo per mese
df_1_last_dt_active$LAST_MONTH_ACTIVE=format(df_1_last_dt_active$LAST_DT_ACTIVE, "%b-%Y")
df_1_last_mt_active <- df_1_last_dt_active %>%
  group_by(LAST_MONTH_ACTIVE) %>%
  summarize(TOT_CLIs = sum(TOT_CLIs)) %>%
  ungroup()  %>%
  as.data.frame()

months=c()
j=1
for(i in 1:length(df_1_last_dt_active$LAST_DT_ACTIVE)){
  if(!format(df_1_last_dt_active$LAST_DT_ACTIVE, "%b-%Y")[i] %in% months){
    months[j]=format(df_1_last_dt_active$LAST_DT_ACTIVE, "%b-%Y")[i]
    j=j+1
  }
}
months=months[1:length(months)-1]

plot_last_mt_active <- df_1_last_mt_active[!df_1_last_mt_active$LAST_MONTH_ACTIVE=="mag-2019",] %>%
  ggplot( aes(x=LAST_MONTH_ACTIVE, y=TOT_CLIs)) +
  geom_segment( aes(x=LAST_MONTH_ACTIVE, xend=LAST_MONTH_ACTIVE, y=0, yend=TOT_CLIs)) +
  geom_point( size=5, color='#6c0f5e', fill=alpha('#6c0f5e', 0.3), alpha=0.7, shape=21, stroke=2) +
  theme(axis.text.x = element_text(angle=45), plot.title = element_text(size=15,hjust = 0.5))+
  ggtitle("Ultima attivazione alla sottoiscrizione") +
  xlab("") +
  ylab("") +
  scale_x_discrete(limits = months)

plot_last_mt_active
#### REVIEW FINALE df_1_clean ####

str(df_1_cli_fid_clean)
summary(df_1_cli_fid_clean)

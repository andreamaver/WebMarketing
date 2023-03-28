#### PRIMO SGUARDO a df_6 ####

str(df_7_tic)
summary(df_7_tic)

#### PULIZIA df_7 ####

df_7_tic_clean <- df_7_tic

#### PULIZIA DATA TYPES in df_7 ####

## formattare le date ##
df_7_tic_clean <- df_7_tic_clean %>%
  mutate(TIC_DATETIME = as.POSIXct(DATETIME, format="%Y-%m-%dT%H%M%S")) %>%
  mutate(TIC_HOUR = hour(TIC_DATETIME)) %>%
  mutate(TIC_DATE = as.Date(TIC_DATETIME)) %>%
  select(-DATETIME)

## formato dei booleani come factor ##
df_7_tic_clean <- df_7_tic_clean %>%
  mutate(DIREZIONE = as.factor(DIREZIONE))

## formato delle categorie numeriche come factor ##
df_7_tic_clean <- df_7_tic_clean %>%
  mutate(COD_REPARTO = as.factor(COD_REPARTO))

#### CONTROLLO CONSISTENZA ID_CLI in df_1/df_7 ####

cons_idcli_df1_df7 <- df_1_cli_fid_clean %>%
  select(ID_CLI) %>%
  distinct() %>%
  mutate(is_in_df_1 = 1) %>%
  distinct() %>%
  full_join(df_7_tic_clean %>%
              select(ID_CLI) %>%
              distinct() %>%
              mutate(is_in_df_7 = 1) %>%
              distinct()
            , by = "ID_CLI"
  ) %>%
  group_by(is_in_df_1, is_in_df_7) %>%
  summarize(NUM_ID_CLIs = n_distinct(ID_CLI)) %>%
  as.data.frame()

cons_idcli_df1_df7

#!!! tutti gli ID_CLI in df_7 sono in df_1, MA non tutti gli ID_CLI in df_1 sono in df_7 !!!#  

#### RIMODELLAMENTO df_7 ####

df_7_tic_clean_final <- df_7_tic_clean %>%
  ## aggiungo caratterizzazione del giorno ##
  mutate(TIC_DATE_WEEKDAY = wday(TIC_DATE)) %>%
  mutate(TIC_DATE_HOLIDAY = isHoliday("Italy", TIC_DATE)) %>%
  mutate(TIC_DATE_TYP = case_when(
    (TIC_DATE_WEEKDAY %in% c(6,7)) ~ "weekend"
    , (TIC_DATE_HOLIDAY == TRUE) ~ "holiday"
    , (TIC_DATE_WEEKDAY < 7) ~ "weekday"
    , TRUE ~ "other"
    )
  )

#### EsPLORAZIONE VARIABILI in df_7 ####

### OVERVIEW GENERALE ###

## calcolo funzioni di aggregazione
df7_overview <- df_7_tic_clean_final %>% 
  summarize(MIN_DATE = min(TIC_DATE)
            , MAX_DATE = max(TIC_DATE)
            , TOT_TICs = n_distinct(ID_SCONTRINO)
            , TOT_CLIs = n_distinct(ID_CLI))

df7_overview

### Variabile DIREZIONE ###

## calcolo funzioni di aggregazione
df7_dist_direction <- df_7_tic_clean_final %>%
  group_by(DIREZIONE) %>%
  summarize(TOT_TICs = n_distinct(ID_SCONTRINO)
            , TOT_CLIs = n_distinct(ID_CLI)) %>%
  mutate(PERCENT_TICs = TOT_TICs/df7_overview$TOT_TICs
         , PERCENT_CLIs = TOT_CLIs/df7_overview$TOT_CLIs)

df7_dist_direction

### Variabile TIC_HOURS ###

## calcolo funzioni di aggregazione
df7_dist_hour <- df_7_tic_clean_final %>%
  group_by(TIC_HOUR, DIREZIONE) %>%
  summarize(TOT_TICs = n_distinct(ID_SCONTRINO)
            , TOT_CLIs = n_distinct(ID_CLI)) %>%
  left_join(df7_dist_direction %>%
              select(DIREZIONE
                     , ALL_TOT_TICs = TOT_TICs
                     , ALL_TOT_CLIs = TOT_CLIs)
            , by = 'DIREZIONE'
  ) %>%
  mutate(PERCENT_TICs = TOT_TICs/ALL_TOT_TICs
         , PERCENT_CLIs = TOT_CLIs/ALL_TOT_CLIs) %>%
  select(-ALL_TOT_TICs, -ALL_TOT_CLIs)

df7_dist_hour

## plot
plot_df7_dist_hour <- (
  ggplot(data=df7_dist_hour
         , aes(fill=DIREZIONE, x=TIC_HOUR, y=TOT_TICs)) +
    geom_bar(stat="identity") +
    theme_minimal()
)

plot_df7_dist_hour

## plot percentuale
plot_df7_dist_hour_percent <- (
  ggplot(data=df7_dist_hour
         , aes(fill=DIREZIONE, x=TIC_HOUR, y=TOT_TICs)) +
    geom_bar(stat="identity", position="fill" ) +
    theme_minimal()
)

plot_df7_dist_hour_percent


### Variabile COD_REPARTO ###

## calcolo funzioni di aggregazione
df7_dist_dep <- df_7_tic_clean_final %>%
  group_by(COD_REPARTO, DIREZIONE) %>%
  summarize(TOT_TICs = n_distinct(ID_SCONTRINO)
            , TOT_CLIs = n_distinct(ID_CLI)) %>%
  left_join(df7_dist_direction %>%
              select(DIREZIONE
                     , ALL_TOT_TICs = TOT_TICs
                     , ALL_TOT_CLIs = TOT_CLIs)
            , by = 'DIREZIONE'
            ) %>%
  mutate(PERCENT_TICs = TOT_TICs/ALL_TOT_TICs
         , PERCENT_CLIs = TOT_CLIs/ALL_TOT_CLIs) %>%
    select(-ALL_TOT_TICs, -ALL_TOT_CLIs)
    
df7_dist_dep

## plot
plot_df7_dist_dep <- (
  ggplot(data=df7_dist_dep
         , aes(fill=DIREZIONE, x=COD_REPARTO, y=TOT_TICs)) +
    geom_bar(stat="identity") +
    theme_minimal()
)

plot_df7_dist_dep

## plot percentuale
plot_df7_dist_dep_percent <- (
  ggplot(data=df7_dist_dep
         , aes(fill=DIREZIONE, x=COD_REPARTO, y=TOT_TICs)) +
    geom_bar(stat="identity", position="fill" ) +
    theme_minimal()
)

plot_df7_dist_dep_percent

### Variabile TIC_DATE_TYP ###

## calcolo funzioni di aggregazione
df7_dist_datetyp <- df_7_tic_clean_final %>%
  group_by(TIC_DATE_TYP, DIREZIONE) %>%
  summarize(TOT_TICs = n_distinct(ID_SCONTRINO)
            , TOT_CLIs = n_distinct(ID_CLI)) %>%
  left_join(df7_dist_direction %>%
              select(DIREZIONE
                     , ALL_TOT_TICs = TOT_TICs
                     , ALL_TOT_CLIs = TOT_CLIs)
            , by = 'DIREZIONE'
  ) %>%
  mutate(PERCENT_TICs = TOT_TICs/ALL_TOT_TICs
         , PERCENT_CLIs = TOT_CLIs/ALL_TOT_CLIs) %>%
  select(-ALL_TOT_TICs, -ALL_TOT_CLIs)

df7_dist_datetyp

## plot
plot_df7_dist_datetyp <- (
  ggplot(data=df7_dist_datetyp
         , aes(fill=DIREZIONE, x=TIC_DATE_TYP, y=TOT_TICs)) +
    geom_bar(stat="identity") +
    theme_minimal()
)

plot_df7_dist_datetyp

## plot percentuale
plot_df7_dist_datetyp_percent <- (
  ggplot(data=df7_dist_datetyp
         , aes(fill=DIREZIONE, x=TIC_DATE_TYP, y=TOT_TICs)) +
    geom_bar(stat="identity", position="fill" ) +
    theme_minimal()
)

plot_df7_dist_datetyp_percent

### Variabile IMPORTO_LORDO medio e SCONTO medio per TICKET ###

## calcolo funzioni di aggregazione
df7_dist_importosconto <- df_7_tic_clean_final %>%
  group_by(ID_SCONTRINO, DIREZIONE) %>%
  summarize(IMPORTO_LORDO = sum(IMPORTO_LORDO)
            , SCONTO = sum(SCONTO)) %>%
  ungroup() %>%
  as.data.frame()

df7_dist_avgimportosconto <- df7_dist_importosconto %>%
  group_by(DIREZIONE) %>%
  summarize(AVG_IMPORTO_LORDO = mean(IMPORTO_LORDO)
            , AVG_SCONTO = mean(SCONTO))

df7_dist_avgimportosconto

## plot importo
plot_df7_dist_importo <- (
  ggplot(data=df7_dist_importosconto %>%
           filter((IMPORTO_LORDO > -1000) & (IMPORTO_LORDO < 1000))
         , aes(color=DIREZIONE, x=IMPORTO_LORDO)) +
    geom_histogram(binwidth=10, fill="white", alpha=0.5) +
    theme_minimal()
)

plot_df7_dist_importo

## plot sconto
plot_df7_dist_sconto <- (
  ggplot(data=df7_dist_importosconto %>%
           filter((SCONTO > -250) & (IMPORTO_LORDO < 250))
         , aes(color=DIREZIONE, x=SCONTO)) +
    geom_histogram(binwidth=10, fill="white", alpha=0.5) +
    theme_minimal()
)

plot_df7_dist_sconto

#### ???? TO DO df_7 ???? ####
# - ESPLORAZIONE IMPORTO_LORDO medio e SCONTO medio raggruppando per COD_REPARTO
# - ESPLORAZIONE DISTRIBUZIONI ID_ARTICOLO (es. numero TICs raggruppando per ID_ARTICOLO)
# - ESPLORAZIONE IMPORTO_LORDO medio e SCONTO medio raggruppando per ID_CLI
# - calcolo la distribuzione dei clienti dal numero di acquisti (come descritto nelle slide):
# raggruppo per ID_CLI, per ogni cliente trovo sum(TICs), raggruppo per sum(TICs);
# - calcolo i giorni per la curva del prossimo acquisto (come descritto nelle slide)

### Variabile IMPORTO_LORDO medio e SCONTO medio per COD_REPARTO ###

## calcolo funzioni di aggregazione
df7_dist_importosconto_reparto <- df_7_tic_clean_final %>%
  group_by(COD_REPARTO, DIREZIONE) %>%
  summarize(IMPORTO_LORDO = sum(IMPORTO_LORDO)
            , SCONTO = sum(SCONTO)) %>%
  ungroup() %>%
  as.data.frame()

df7_dist_avg_importosconto_reparto <- df7_dist_importosconto_reparto %>%
  group_by(DIREZIONE) %>%
  summarize(AVG_IMPORTO_LORDO = mean(IMPORTO_LORDO)
            , AVG_SCONTO = mean(SCONTO))

df7_dist_avg_importosconto_reparto

# plot importo
plot_df7_dist_importo_reparto <- (
  ggplot(data=df7_dist_importosconto_reparto,
           #filter((IMPORTO_LORDO > -1000000) & (IMPORTO_LORDO < 1000000))
           aes(color=DIREZIONE, x=IMPORTO_LORDO)) +
    geom_histogram(binwidth=1000, fill="white", alpha=0.5) +
    theme_minimal()

)
plot_df7_dist_importo_reparto

# plot sconto
plot_df7_dist_sconto_reparto <- (
  ggplot(data=df7_dist_importosconto_reparto %>%
           filter((SCONTO > -250) & (IMPORTO_LORDO < 250))
         , aes(color=DIREZIONE, x=SCONTO)) +
    geom_histogram(binwidth=10, fill="white", alpha=0.5) +
    theme_minimal()
)

plot_df7_dist_sconto_reparto

### ESPLORAZIONE DISTRIBUZIONI ID_ARTICOLO (es. numero TICs raggruppando per ID_ARTICOLO) ###

df7_dist_articolo <- df_7_tic_clean_final %>%
  group_by(ID_ARTICOLO , DIREZIONE) %>%
  summarize(TOT_TICs = n_distinct(ID_SCONTRINO)
            , TOT_CLIs = n_distinct(ID_CLI)) %>%
  left_join(df7_dist_direction %>%
              select(DIREZIONE
                     , ALL_TOT_TICs = TOT_TICs
                     , ALL_TOT_CLIs = TOT_CLIs)
            , by = 'DIREZIONE'
  ) %>%
  mutate(PERCENT_TICs = TOT_TICs/ALL_TOT_TICs
         , PERCENT_CLIs = TOT_CLIs/ALL_TOT_CLIs) %>%
  select(-ALL_TOT_TICs, -ALL_TOT_CLIs)
   
df7_dist_articolo[order(df7_dist_articolo$TOT_TICs, decreasing = TRUE),]
# ordino  per TOT_TICs per vedere gli articoli più venduti

df7_dist_articolo

### Variabile IMPORTO_LORDO medio e SCONTO medio per ID_CLI ###

## calcolo funzioni di aggregazione
df7_dist_importosconto_idcli <- df_7_tic_clean_final %>%
  group_by(ID_CLI, DIREZIONE) %>%
  summarize(IMPORTO_LORDO = sum(IMPORTO_LORDO)
            , SCONTO = sum(SCONTO)) %>%
  ungroup() %>%
  as.data.frame()

df7_dist_importosconto_idcli[order(df7_dist_importosconto_idcli$IMPORTO_LORDO, decreasing = TRUE),]


df7_dist_avg_importosconto_idcli <- df7_dist_importosconto_idcli %>%
  group_by(DIREZIONE) %>%
  summarize(AVG_IMPORTO_LORDO = mean(IMPORTO_LORDO)
            , AVG_SCONTO = mean(SCONTO))

df7_dist_avg_importosconto_idcli

### CALCOLO la distribuzione dei clienti dal numero di acquisti (come descritto nelle slide) ###

# considero solo gli ascquisti e non i rimborsi
df_7_purchase<-df_7_tic_clean_final[df_7_tic_clean_final$DIREZIONE==1,]

#raggruppo per ID_CLI e calcolo TOT_TICs
df7_dist_numtics_idcli <- df_7_purchase %>%
  group_by(ID_CLI) %>%
  summarize(TOT_TICs = n_distinct(ID_SCONTRINO)) %>%
  ungroup() %>%
  as.data.frame()

#raggruppo per TOT_TICs, calcolo TOT_CLIs e la percentuale
df7_dist_numtics_idcli_gr <- df7_dist_numtics_idcli  %>%
  group_by(TOT_TICs) %>%
  summarize(TOT_CLIs = n_distinct(ID_CLI),
            PERCENT = TOT_CLIs/n_distinct(df_7_purchase$ID_CLI)) %>%
  ungroup() %>%
  as.data.frame()

CLIs=c()
for(i in 1:nrow(df7_dist_numtics_idcli_gr)){
  CLIs[i]=sum(df7_dist_numtics_idcli_gr$TOT_CLIs[i:nrow(df7_dist_numtics_idcli_gr)])
}
df7_dist_numtics_idcli_gr$CLIs<-CLIs

tab_graf<-df7_dist_numtics_idcli_gr[1:6,]
tab_graf$LABELS<-c(">=1 acquisti",">=2 acquisti",">=3 acquisti",
                   ">=4 acquisti", ">=5 acquisti", ">=6 acquisti")
plot_numtics <- tab_graf %>%
  ggplot( aes(x=LABELS, y=CLIs)) +
  geom_bar(stat = "identity", fill="#6c0f5e") +
  # geom_text(size=3,aes(label=round(SPESA_NETTA,0)), position=position_dodge(width=0.9), vjust=-0.25) +
  theme(axis.text.x = element_text(size=8.5),
    plot.title = element_text(size=15,hjust = 0.5))+
  ggtitle("Clienti per il numero di acquisti") +
  xlab("") +
  ylab("Numero di clienti")

plot_numtics

### CALCOLO i giorni per la curva del prossimo acquisto (come descritto nelle slide) ###

#considero solo gli acquisti e non i rimborsi
#prendo ogni cliente e le date dei suoi acquisti senza ripeterle
clienti_date <- df_7_purchase %>%
  group_by(ID_CLI, TIC_DATE) %>%
  select(ID_CLI, TIC_DATE)

clienti_date <- clienti_date %>% distinct(TIC_DATE) 

#elimino clienti con un solo acquisto, non servono per fare differenze
tab <- table(clienti_date$ID_CLI)
clienti_date <- clienti_date[clienti_date$ID_CLI %in% names(tab)[tab>1],]

#calcolo differenze tra date raggruppando per cliente
clienti_date$diff <- ave(as.numeric(clienti_date$TIC_DATE), factor(clienti_date$ID_CLI), 
                         FUN=function(x) c(NA,diff(x)))

#elimino NA, righe senza differenza
clienti_date <- clienti_date[complete.cases(clienti_date),]

#calcolo media delle differenze per ogni cliente
clienti_date_diff <- clienti_date %>%
  group_by(ID_CLI) %>%
  select(ID_CLI, diff) %>%
  summarise(Mean=mean(diff))

##PLOT

plot(ecdf(clienti_date_diff$Mean), main='Customers by days to next purchase',
     xlab='Average days to next purchase', ylab='% Customers')

##############################################################################
# ALTRA ESPLORAZIONE DATI e GRAFICI

### PURCHASES: percentuale di acquisti tra settimana/weekend/vacanze ###

#solo acquisti
purchase<-df7_dist_datetyp[df7_dist_datetyp$DIREZIONE==1,]
#totale scontrini
all_tot_tics=sum(purchase$TOT_TICs)
#totale clienti
all_tot_clis=sum(purchase$TOT_CLIs)
#percentuali
purchase$PERCENT_CLIs<-purchase$TOT_CLIs/all_tot_clis*100
purchase$PERCENT_TICs<-purchase$TOT_TICs/all_tot_tics*100

## plot: grafo a torta
lb = paste0(round(purchase$PERCENT_TICs,0),"%")
pie(purchase$PERCENT_TICs,labels = lb, col = c('#ea6631','#6c0f5e','#bc2353'))
legend(-2,0.4,legend=purchase$TIC_DATE_TYP,cex=0.7,yjust=0.2, xjust = -0.1,
       fill = c('#ea6631','#6c0f5e','#bc2353'), bty = "n")
title('% scontrini durante l\'anno', line=-0.7)

## plot: 100% Stacked Bar Chart
options(repr.plot.width=8, repr.plot.height=3)
ggplot(data=purchase, aes(fill=TIC_DATE_TYP, x = DIREZIONE, y = TOT_TICs, main="Percentuale acquisti")) +
  geom_bar(stat = "identity", position="fill") +
  coord_flip() +
  theme(axis.text.y = element_blank()) +
  ggtitle("") +
  xlab("") +
  ylab("Percentual of TICs")

### REFUND: distribuzione rimborsi tra settimana/weekend/vacanze ###

#considero solo i rimborsi
refund<-df7_dist_datetyp[df7_dist_datetyp$DIREZIONE==-1,]
#tutti gli scontrini
all_tot_tics=sum(refund$TOT_TICs)
#tutti i clienti
all_tot_clis=sum(refund$TOT_CLIs)
#Percentuali
refund$PERCENT_CLIs<-refund$TOT_CLIs/all_tot_clis*100
refund$PERCENT_TICs<-refund$TOT_TICs/all_tot_tics*100

##Plot: grafo a torta
lb = paste0(round(refund$PERCENT_TICs,0),"%")
pie(refund$PERCENT_TICs,labels = lb, col = c('#ea6631','#6c0f5e','#bc2353'))
legend(-2.0,0.4,legend=refund$TIC_DATE_TYP,cex=0.7,yjust=0.2, xjust = -0.1,
       fill = c('#ea6631','#6c0f5e','#bc2353'), bty = "n")
title('% rimborsi durante l\'anno', line=-0.7)

### PURCHASES VS REFUND: confronto acquisti e rimborsi ###

#raggruppo per la direzione (distinzione acquisti/rimborsi)
pur_vs_refund <- df7_dist_datetyp %>%
  group_by(DIREZIONE) %>%
  summarize(TOT_TICs = sum(TOT_TICs)
            , TOT_CLIs = sum(TOT_CLIs)) %>%
  ungroup() %>%
  as.data.frame()

#percentuali scontrini e clienti
pur_vs_refund$PERCENT_TICs<-pur_vs_refund$TOT_TICs/sum(pur_vs_refund$TOT_TICs)*100
pur_vs_refund$PERCENT_CLIs<-pur_vs_refund$TOT_CLIs/sum(pur_vs_refund$TOT_CLIs)*100

##Plot: percentuale rispetto agli scontrini
lb = paste0(round(pur_vs_refund$PERCENT_TICs,0),"%")
pie(pur_vs_refund$PERCENT_TICs,labels = lb, col = c('#bc2353','#6c0f5e'))
legend(-2.0,0.4,legend=c('rimborsi','acquisti'),cex=0.7,yjust=0.2, xjust = -0.1,
       fill = c('#bc2353','#6c0f5e'), bty = "n")
title('acquisti vs rimborsi (%)', line=-0.7)

##Plot: percentuale rispetto ai clienti
lb = paste0(round(pur_vs_refund$PERCENT_CLIs,0),"%")
pie(pur_vs_refund$PERCENT_CLIs,labels = lb, col = c('#bc2353','#6c0f5e'))
legend(-2.0,0.4,legend=c('clienti rimborsati','clienti soddisfatti'),cex=0.7,yjust=0.2, xjust = -0.1,
       fill = c('#bc2353','#6c0f5e'), bty = "n")
title('clienti richiedenti rimborso (%)', line=-0.7)

### variabile TIC_HOUR: solo per acquisti ###

df7_dist_hour

#solo acquisti
purchase_hour<-df7_dist_hour[df7_dist_hour$DIREZIONE==1,]
#totale scontrini
all_tot_tics=sum(purchase_hour$TOT_TICs)
#totale clienti
all_tot_clis=sum(purchase_hour$TOT_CLIs)
#percentuali
purchase_hour$PERCENT_CLIs<-purchase_hour$TOT_CLIs/all_tot_clis*100
purchase_hour$PERCENT_TICs<-purchase_hour$TOT_TICs/all_tot_tics*100

# converto TIC_HOUR in stringa
hours=c()
for(i in 1:length(purchase_hour$TIC_HOUR)){
  hours[i]=toString(purchase_hour$TIC_HOUR[i])
}
purchase_hour$TIC_HOUR<-hours

##Plot: grafico a barre con totale scontrini emessi lungo la giornata (rispetto all'orario)
plot_purchase_hour <- purchase_hour %>%
  ggplot( aes(x=TIC_HOUR, y=TOT_TICs)) +
  geom_bar(stat = "identity", fill="#6c0f5e") +
  geom_text(size=2.5,aes(label=TOT_TICs, vjust=-0.25)) +
  theme(axis.text.y = element_blank(), axis.ticks = element_blank(), plot.title = element_text(size=20,hjust = 0.5))+
  ggtitle("Scontrini lungo la giornata") +
  xlab("") +
  ylab("") +
  scale_x_discrete(limits = hours)

plot_purchase_hour

### Variabile ID_NEG ###

# calcolo spesa netta per negozio
df7_dist_spesa <- df_7_tic_clean_final %>%
  group_by(ID_NEG) %>%
  summarize(SPESA_NETTA = sum(IMPORTO_LORDO)-sum(SCONTO)) %>%
  ungroup() %>%
  as.data.frame()

# estrapolo i 10 negozi con più guadagni
df7_dist_spesa<-df7_dist_spesa[order(df7_dist_spesa$SPESA_NETTA, decreasing = TRUE),]
top_10_neg<-df7_dist_spesa[1:10,]

#converto ID_NEG in stringa
neg=c()
for(i in 1:length(top_10_neg$ID_NEG)){
  neg[i]=toString(top_10_neg$ID_NEG[i])
}
top_10_neg$ID_NEG<-neg

##Plot: grafico a barre con i 10 negozi con i guadagni netti maggiori
plot_top_10_neg_spesa <- top_10_neg %>%
  ggplot( aes(x=ID_NEG, y=SPESA_NETTA)) +
  geom_bar(stat = "identity", fill="#6c0f5e") +
  geom_text(size=3,aes(label=round(SPESA_NETTA,0)), position=position_dodge(width=0.9), vjust=-0.25) +
  theme(axis.text.y = element_blank(), axis.ticks = element_blank(), plot.title = element_text(size=20,hjust = 0.5))+
  ggtitle("Guadagni netti per negozio") +
  xlab("") +
  ylab("") +
  scale_x_discrete(limits = neg)
# 10 negozi con i guadagni netti maggiori
plot_top_10_neg_spesa

#### REVIEW FINALE df_7_clean ####

str(df_7_tic_clean_final)
summary(df_7_tic_clean_final)


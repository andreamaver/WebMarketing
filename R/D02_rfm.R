### MODELLO RFM ###

# considero solo alcune colonne di interesse del dataset con tutti gli scontrini
rfm<-df_7_tic_clean_final[,c('ID_CLI','ID_SCONTRINO','TIC_DATE','IMPORTO_LORDO','SCONTO')]

# calcolo spesa raggruppando per ID_CLI,ID_SCONTRINO,TIC_DATE
rfm_gr <- rfm %>%
  group_by(ID_CLI,ID_SCONTRINO,TIC_DATE) %>%
  summarise(SPESA=sum(IMPORTO_LORDO)-sum(SCONTO)) %>%
  as.data.frame()

# rimuovo refund (rimborsi)
rfm_gr=rfm_gr[rfm_gr$SPESA>=0,]

# rimuovo gli inattivi (clienti che non riacquistano da più di 160 gg)
data_max <- df_7_tic_clean_final %>%
  select(ID_CLI, TIC_DATE) %>%
  group_by(ID_CLI) %>%
  arrange(desc(TIC_DATE)) %>%
  slice(1L) %>%
  as.data.frame()

colnames(data_max)[2] <- "ULTIMA_DATA"

ids<-data_max['2018-11-20'<=data_max$ULTIMA_DATA,]

rfm_gr<-merge(rfm_gr,ids)

#rfm: risultati
rfm_gr$TIC_DATE<-as.Date(rfm_gr$TIC_DATE)

rfm_result <- rfm_table_order(
  data = rfm_gr,
  customer_id = ID_CLI,
  revenue = SPESA,
  order_date = TIC_DATE, 
  analysis_date = as.Date("2019-04-30"),
  recency_bins = 3,
  frequency_bins = 3,
  monetary_bins = 3
)

#suddivisione in cluster dei clienti
classes=c('One-Timer','Leaving','Engaged','Leaving Top','Top')
recency_lower=c(2,1,2,1,2)
recency_upper=c(3,1,3,1,3)
frequency_lower=c(1,1,2,3,3)
frequency_upper=c(1,2,2,3,3)
monetary_lower=rep(1,5)
monetary_upper=rep(3,5)

rfm_classes=rfm_segment(rfm_result,classes,recency_lower,recency_upper,frequency_lower,frequency_upper,monetary_lower,monetary_upper)

#assegnazione delle etichette Diamond, Gold, Silver, Bronze, Copper, Tin
label=c()
for(i in 1:nrow(rfm_classes)){
  if(rfm_classes[i,]$segment=='One-Timer'){
    if(rfm_classes[i,]$monetary_score==1){
      label[i]='Cheap'
    }else if(rfm_classes[i,]$monetary_score==2){
      label[i]='Tin'
    }else{
      label[i]='Copper'
    }
  }else if(rfm_classes[i,]$segment=='Leaving'){
    if(rfm_classes[i,]$monetary_score==1){
      label[i]='Tin'
    }else if(rfm_classes[i,]$monetary_score==2){
      label[i]='Copper'
    }else{
      label[i]='Bronze'
    }
  }else if(rfm_classes[i,]$segment=='Engaged'){
    if(rfm_classes[i,]$monetary_score==1){
      label[i]='Copper'
    }else if(rfm_classes[i,]$monetary_score==2){
      label[i]='Bronze'
    }else{
      label[i]='Silver'
    }
  }else if(rfm_classes[i,]$segment=='Leaving Top'){
    if(rfm_classes[i,]$monetary_score==1){
      label[i]='Bronze'
    }else if(rfm_classes[i,]$monetary_score==2){
      label[i]='Silver'
    }else{
      label[i]='Gold'
    }
  }else if(rfm_classes[i,]$segment=='Top'){
    if(rfm_classes[i,]$monetary_score==1){
      label[i]='Silver'
    }else if(rfm_classes[i,]$monetary_score==2){
      label[i]='Gold'
    }else{
      label[i]='Diamond'
    }
  }
}

rfm_classes$result<-label

##Plot: confronto recency, frequency, monetary
rfm_bar_chart(rfm_result, bar_color="#971584")

##Plot: barchart con totale di ogni categoria
classes_tot<-rfm_classes %>%
  group_by(result) %>%
  summarise(n=n_distinct(customer_id)) %>%
  as.data.frame()

metals <- c("Diamond", "Gold", 
            "Silver", "Bronze", "Copper", 
            "Tin", "Cheap")

plot_rfm_new <- classes_tot %>%
  ggplot( aes(x=result, y=n, fill=as.factor(result))) +
  geom_bar(stat="identity", width=0.8) +
  geom_text(aes(label=n), position=position_dodge(width=0.9), vjust=-0.25) +
  scale_fill_manual(values = c("#b08d57", "black", "#b87333", "#70d1f4", "#d4af37", "#A8A9AD", "#8f8b96") ) +
  ggtitle("Number of customers for each class")+
  xlab("") +
  ylab("") +
  theme(legend.position = "none", axis.text.y = element_blank(), axis.ticks = element_blank())+
  scale_x_discrete(limits = metals)
plot_rfm_new


##Plot: barchart con mediana del monetary value

mon_tot<-rfm_classes %>%
  group_by(result) %>%
  summarise(mon_median=median(amount)) %>%
  as.data.frame()

plot_mon_new <- mon_tot %>%
  ggplot( aes(x=result, y=mon_median, fill=as.factor(result))) +
  geom_bar(stat="identity", width=0.8) +
  geom_text(aes(label=mon_median), position=position_dodge(width=0.9), vjust=-0.25) +
  scale_fill_manual(values = c("#b08d57", "black", "#b87333", "#70d1f4", "#d4af37", "#A8A9AD", "#8f8b96") ) +
  ggtitle("Median Monetary Value")+
  xlab("") +
  ylab("Total of dollars spent") +
  theme(legend.position = "none", axis.text.y = element_blank(),axis.ticks = element_blank())+
  scale_x_discrete(limits = metals)
plot_mon_new


##Plot: barchart con mediana di recency

rec_tot<-rfm_classes %>%
  group_by(result) %>%
  summarise(rec_median=median(recency_days)) %>%
  as.data.frame()

plot_rec_new <- rec_tot %>%
  ggplot( aes(x=result, y=rec_median, fill=as.factor(result))) +
  geom_bar(stat="identity", width=0.8) +
  geom_text(aes(label=rec_median), position=position_dodge(width=0.9), vjust=-0.25) +
  scale_fill_manual(values = c("#b08d57", "black", "#b87333", "#70d1f4", "#d4af37", "#A8A9AD", "#8f8b96") ) +
  ggtitle("Median Recency Value")+
  xlab("") +
  ylab("Number of days") +
  theme(legend.position = "none", axis.text.y = element_blank(), axis.ticks = element_blank())+
  scale_x_discrete(limits = metals)
plot_rec_new


##Plot: barchart con mediana di frequency

freq_tot<-rfm_classes %>%
  group_by(result) %>%
  summarise(freq_median=median(transaction_count)) %>%
  as.data.frame()

plot_freq_new <- freq_tot %>%
  ggplot( aes(x=result, y=freq_median, fill=as.factor(result))) +
  geom_bar(stat="identity", width=0.8) +
  geom_text(aes(label=freq_median), position=position_dodge(width=0.9), vjust=-0.25) +
  scale_fill_manual(values = c("#b08d57", "black", "#b87333", "#70d1f4", "#d4af37", "#A8A9AD", "#8f8b96") ) +
  ggtitle("Median Frequency Value") +
  xlab("") +
  ylab("Number of days") +
  theme(legend.position = "none", axis.text.y = element_blank(), axis.ticks = element_blank())+
  scale_x_discrete(limits = metals)
plot_freq_new


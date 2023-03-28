#####################################
####TRASFORMAZIONI PER TABELLA CHURN###

###CAPIRE CHI è CHURNER###
#cerco data del primo e dell'ultimo acquisto di ogni cliente
#se la differenza tra date è =0, elimino cliente dal dataset
#poi confronto l'ultima data con 2018-11-20, cioè 160 giorni prima dell'ultimo giorno presente nel dataset
#se ultima data > 2018-11-20, il cliente non è churner e gli assegno valore 0, altrimenti 1

# creiamo un dataset con tutte informazioni sui clienti intersecando df_1,df_2,df_3,df_4
total_info_clients<-merge(df_1_cli_fid_clean,df_2_cli_account_clean,all='T')
total_info_clients<-merge(total_info_clients,df_3_cli_address_clean,all='T')
total_info_clients<-merge(total_info_clients,df_4_cli_privacy_clean,all='T')

# SOLO purchase, tolti i refund
df_7_tic_clean_final_pur <- df_7_tic_clean_final[df_7_tic_clean_final$DIREZIONE==1,]

#data del primo acquisto
data_min <- df_7_tic_clean_final_pur %>%
  select(ID_CLI, TIC_DATE) %>%
  group_by(ID_CLI) %>%
  arrange(TIC_DATE) %>%
  slice(1L) %>%
  as.data.frame()

colnames(data_min)[2] <- "PRIMA_DATA"

#data dell'ultimo acquisto
data_max <- df_7_tic_clean_final_pur %>%
  select(ID_CLI, TIC_DATE) %>%
  group_by(ID_CLI) %>%
  arrange(desc(TIC_DATE)) %>%
  slice(1L) %>%
  as.data.frame()

colnames(data_max)[2] <- "ULTIMA_DATA"

#unisco le tabelle e trovo differenza di giorni tra il primo e l'ultimo acquisto
diff_date_acquisto <- merge(data_min, data_max)
diff_date_acquisto$DIFF_IN_DAYS <- round(difftime(diff_date_acquisto$ULTIMA_DATA, diff_date_acquisto$PRIMA_DATA , units = c("days")))

#elimino righe con differenza 0: clienti che hanno effettuato acquisti un unico giorno
diff_date_acquisto<-diff_date_acquisto[!(diff_date_acquisto$DIFF_IN_DAYS=="0"),]

#cerco i clienti che hanno acquistato negli ultimi 160 giorni
#gli assegno il valore 0, NON CHURNERS
last_160<-diff_date_acquisto['2018-11-20'<=diff_date_acquisto$ULTIMA_DATA,] 
last_160['CHURN'] = '0'

#unisco last_160 e diff_date_acquisto poi assegno 1 ai churners
diff_date_acquisto <- merge(diff_date_acquisto, last_160, all=T)
diff_date_acquisto[is.na(diff_date_acquisto)] <- 1

diff_date_acquisto <- diff_date_acquisto %>%
  select(ID_CLI, CHURN)

#####TRASFORMAZIONE COLONNE PER MODELLI#####

# trovo importo netto per ogni scontrino
total_acquired_product <- transform(df_7_tic_clean_final, IMPORTO_LORDO = as.numeric(IMPORTO_LORDO), 
                                    SCONTO = as.numeric(SCONTO))

total_acquired_product$IMPORTO_NETTO <- (total_acquired_product$IMPORTO_LORDO - total_acquired_product$SCONTO)

#converto DIREZIONE da factor a numerico
total_acquired_product$DIREZIONE<-as.numeric(as.character(total_acquired_product$DIREZIONE))

# sommo il totale speso per ogni cliente e
# conto quanti articoli ha comprato un cliente
spesa_tot_cliente <- total_acquired_product %>% 
  group_by(ID_CLI) %>% 
  summarise(SPESA_TOTALE = sum(IMPORTO_NETTO),
            NUM_ARTICOLI=sum(DIREZIONE))

#AGGIUNGO COLONNE TROVATE A total_info_clients

total_info_clients = merge(total_info_clients, spesa_tot_cliente, all=TRUE)
total_info_clients = merge(total_info_clients, diff_date_acquisto, all=TRUE)


#droppo clienti che hanno spesa totale NA
total_info_clients <- total_info_clients[!is.na(total_info_clients$SPESA_TOTALE), ]
#n clienti passa da 360k a 212k

#DATASET CAMP_EVENT:
#aggiungo colonna CAMP che indica se cliente ha ricevuto mail per campagne marketing
event<-df_6_camp_event_clean_final

event$NUM_OPENs[is.na(event$NUM_OPENs)]<-0
event$NUM_CLICKs[is.na(event$NUM_CLICKs)]<-0

event_gr <- event %>%
  group_by(ID_CLI) %>%
  summarise(NUM_OPENs=sum(NUM_OPENs),
            NUM_CLICKs=sum(NUM_CLICKs)) %>%
  as.data.frame()

# il cliente ha ricevuto mail per campagne marketing (CAMP=1)
event_gr$CAMP<-1

total_info_clients<-merge(total_info_clients,event_gr,all=TRUE)
total_info_clients$CAMP[is.na(total_info_clients$CAMP)]<-0

########churn DI MODELLI CHURN############

# elimino clienti che hanno valori NA di churn,
# cioè quelli che hanno acquistato un solo giorno
churn <- total_info_clients[!is.na(total_info_clients$CHURN), ]

# considero solo le variabili di maggior interesse
churn <- churn %>%
  select(LAST_COD_FID, LAST_TYP_CLI_FID, LAST_STATUS_FID, FIRST_ID_NEG,
         REGION, FLAG_PRIVACY_1, FLAG_PRIVACY_2, FLAG_DIRECT_MKT, SPESA_TOTALE,
         NUM_ARTICOLI, CHURN)

# conversione delle variabili: binarizzate variabili categoriche
churn$LAST_COD_FID <- as.factor(churn$LAST_COD_FID)
churn$LAST_TYP_CLI_FID <- as.factor(churn$LAST_TYP_CLI_FID)
churn$LAST_STATUS_FID <- as.factor(churn$LAST_STATUS_FID)
churn$FIRST_ID_NEG <- as.factor(churn$FIRST_ID_NEG)
churn$REGION <- as.factor(churn$REGION)
churn$FLAG_PRIVACY_1 <- as.factor(churn$FLAG_PRIVACY_1)
churn$FLAG_PRIVACY_2 <- as.factor(churn$FLAG_PRIVACY_2)
churn$FLAG_DIRECT_MKT <- as.factor(churn$FLAG_DIRECT_MKT)
churn$CHURN <- as.factor(churn$CHURN)

# controllo se ci sono missing value
churn[!complete.cases(churn),]

#sono presenti delle osservazioni senza regione, le elimino
churn_tot <- na.omit(churn)

#faccio girare una random forest con tutte le variabili per capire quali sono le più significative
#undersampling del dataset per risolvere class imbalance
churn_tot <- RandUnderClassif(CHURN~., churn_tot, "balance")

prop.table(table(churn_tot$CHURN))

set.seed(123)
split <- sample.split(churn_tot$CHURN, SplitRatio = 0.80)
train_churn_tot <- churn_tot[split,]
test_churn_tot <- churn_tot[!(split),]

rfModel_tot <- randomForest(
  CHURN ~ .,
  data=train_churn_tot,
  ntree=250
)

varImpPlot(rfModel_tot, sort=T, n.var = 10, main = 'Top 10 Feature Importance',
           bg='#6c0f5e')

#le variabili più importanti sono SPESA_TOTALE, NUM_ARTICOLI e FIRST_ID_NEG
#creo nuovo dataset solo con quelle 3
churn1 <- churn %>%
            select(FIRST_ID_NEG,SPESA_TOTALE,NUM_ARTICOLI, CHURN)

# controllo se ci sono missing value
churn1[!complete.cases(churn1),] #non ce ne sono

#undersampling del dataset per risolvere class imbalance
churn1 <- RandUnderClassif(CHURN~., churn1, "balance")

prop.table(table(churn1$CHURN))

set.seed(123)
split <- sample.split(churn1$CHURN, SplitRatio = 0.80)
train_churn <- churn1[split,]
test_churn <- churn1[!(split),]

prop.table(table(train_churn$CHURN))

########MODELLI DI CHURN#########

#Logistic regression
glm <- glm(CHURN ~ ., data = train_churn, family = "binomial"(link='logit'))
summary(glm)

anova(glm, test="Chisq")

fitted.results_prob <- predict(glm, newdata=test_churn, type='response')
fitted.results <- ifelse(fitted.results_prob > 0.5, 1, 0)

table_lg <- table(Predicted = fitted.results, Actual = test_churn$CHURN)
table_lg

print(paste('Logistic regression Accuracy', sum(diag(table_lg)) / sum(table_lg)))
precision_lg=table_lg[2,2] / (table_lg[2,2]+table_lg[2,1])
print(paste('Logistic regression Precision', precision_lg))
recall_lg=table_lg[2,2] / (table_lg[2,2]+table_lg[1,2])
print(paste('Logistic regression Recall', recall_lg))
print(paste('Logistic regression F1-score', 2*precision_lg*recall_lg/(precision_lg+recall_lg)))


#Decision Tree
tree <- rpart(CHURN ~ ., method = "class", data = train_churn,
              control = rpart.control(minsplit = 5, cp = 0.000001))

pred_tree_prob <- predict(tree, test_churn, type = 'prob')
pred_tree_class <- ifelse(pred_tree_prob[,2] > 0.5, 1, 0)
table_dt <- table(Predicted = pred_tree_class, Actual = test_churn$CHURN)
table_dt

print(paste('Decision Tree Accuracy', sum(diag(table_dt)) / sum(table_dt)))
precision_dt=table_dt[2,2] / (table_dt[2,2]+table_dt[2,1])
print(paste('Decision Tree Precision', precision_dt))
recall_dt=table_dt[2,2] / (table_dt[2,2]+table_dt[1,2])
print(paste('Decision Tree Recall', recall_dt))
print(paste('Decision Tree F1-score', 2*precision_dt*recall_dt/(precision_dt+recall_dt)))


#Random forest
rfModel <- randomForest(
  CHURN ~ .,
  data=train_churn,
  ntree=250
)

print(rfModel)

pred_rfModel_prob = predict(rfModel, newdata=test_churn, type="prob")
pred_rfModel <- ifelse(pred_rfModel_prob[,2] > 0.5, 1, 0)
cm <- table(Predicted=pred_rfModel, Actual=test_churn$CHURN)
cm

print(paste('Random forest Accuracy', sum(diag(cm)) / sum(cm)))
precision_rf=cm[2,2] / (cm[2,2]+cm[2,1])
print(paste('Random Forest Precision', precision_rf))
recall_rf=cm[2,2] / (cm[2,2]+cm[1,2])
print(paste('Random Forest Recall', recall_rf))
print(paste('Random Forest F1-score', 2*precision_rf*recall_rf/(precision_rf+recall_rf)))

#plot error
plot(rfModel)

#Multilater perceptron
set.seed(123)
nnetGrid <-  expand.grid(size = 5, decay = 1e-5)
set.seed(555)
nn.1 <- train(CHURN ~ ., data=train_churn, method='nnet', 
              tuneGrid = nnetGrid,
              maxit = 100,
              trControl=trainControl(method='cv', number=5))

summary(nn.1)

predicted_nnet_prob <- predict(nn.1, test_churn, type = "prob")
predicted_nnet <- ifelse(predicted_nnet_prob[,2] > 0.5, 1, 0)
comparison_1 <- data.frame(Predicted = predicted_nnet, Actual = test_churn$CHURN)

table_nnet_1 <- table(comparison_1)
table_nnet_1

print(paste('Neural Network Accuracy', sum(diag(table_nnet_1)) / sum(table_nnet_1)))
precision_nnet=table_nnet_1[2,2] / (table_nnet_1[2,2]+table_nnet_1[2,1])
print(paste('Neural Network Precision', precision_nnet))
recall_nnet=table_nnet_1[2,2] / (table_nnet_1[2,2]+table_nnet_1[1,2])
print(paste('Neural Network Recall', recall_nnet))
print(paste('Neural Newtork F1-score', 2*precision_nnet*recall_nnet/(precision_nnet+recall_nnet)))

###ROC PER I TRE MODELLI###
par(pty='s')

roc(test_churn$CHURN, fitted.results_prob, plot=T, percent = T, legacy.axes=T,
    col='#6c0f5e', lwd=2, print.auc=T, print.auc.x=25, print.auc.y=65, main='ROC curve e AUC',
    xlab='Percentuale falsi positivi', ylab='Percentuale veri positivi') #glm

roc(test_churn$CHURN, pred_tree_prob[,2], plot=T, percent = T, col='#b31d56', legacy.axes=T,
    lwd=2, print.auc=T, print.auc.x=25, print.auc.y=55,  add=T) #decision tree

plot.roc(test_churn$CHURN, pred_rfModel_prob[,2], percent = T, col='#e2543b',
         lwd=2, print.auc=T, print.auc.x=25, print.auc.y=45,  add=T) #random forest

plot.roc(test_churn$CHURN, predicted_nnet_prob[,2], percent = T, col='#f09708',
         lwd=2, print.auc=T, print.auc.x=25, print.auc.y=35,  add=T) #Multilayer Perceptron

legend('bottomright', legend = c('Logistic regression', 'Decision Tree', 'Random forest', 'Multilayer Perceptron'),
       col = c('#6c0f5e', '#b31d56', '#e2543b', '#f09708'), lwd=2, cex = 0.75, bty='n')

par(pty='m')

# Modello migliore: nnet
test_churn$predicted<-predicted_nnet

test_churn$ID_CLI<-row.names(test_churn)
# Unione con risultati RFM
test_churn<-merge(test_churn,rfm_classes[,c('customer_id','result')],by.x='ID_CLI',by.y='customer_id')
churner<-test_churn[test_churn$predicted==1,]

churner_tot<-churner %>%
  group_by(result) %>%
  summarise(n=n_distinct(ID_CLI)) %>%
  as.data.frame()

metals <- c("Diamond", "Gold", 
            "Silver", "Bronze", "Copper", 
            "Tin", "Cheap")

plot_most_valuable <- churner_tot %>%
  ggplot( aes(x=result, y=n, fill=as.factor(result))) +
  geom_bar(stat="identity", width=0.8) +
  geom_text(aes(label=n), position=position_dodge(width=0.9), vjust=-0.25) +
  scale_fill_manual(values = c("#b08d57", "black", "#b87333", "#70d1f4", "#d4af37", "#A8A9AD", "#8f8b96") ) +
  ggtitle("Churners by group")+
  xlab("") +
  ylab("") +
  theme(legend.position = "none", axis.text.y = element_blank(),axis.ticks = element_blank())+
  scale_x_discrete(limits = metals)
# Distribuzione churner rispetto al valore dei clienti
plot_most_valuable

churner<-merge(churner,total_info_clients[,c('ID_CLI','CAMP')])
most_val<-churner[churner$result=='Diamond' | churner$result=='Gold' | churner$result=='Silver',]
# Quanti, fra i churner con più valore, rientrano in campagne marketing
most_val_gr <- most_val %>%
  group_by(predicted) %>%
  summarise(CAMP_PERC=sum(CAMP)/n_distinct(ID_CLI)*100) %>%
  as.data.frame()

lb = c(paste0(round(most_val_gr$CAMP_PERC,0),"%"),paste0(100-round(most_val_gr$CAMP_PERC,0),"%"))
pie(c(most_val_gr$CAMP_PERC,100-most_val_gr$CAMP_PERC),labels = lb, col = c('#6c0f5e','#bc2353'))
legend(-2.0,0.4,legend=c('partecipa','non partecipa'),cex=0.7,yjust=0.2, xjust = -0.1,
       fill = c('#6c0f5e','#bc2353'), bty = "n")
title('Campagna marketing', line=-0.3)


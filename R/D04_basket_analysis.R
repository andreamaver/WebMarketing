###MARKET BASKET ANALYSIS###

transactionData <- df_7_tic_clean_final[,c('ID_SCONTRINO','TIC_DATE',"ID_ARTICOLO")]
#raggruppo transazioni in base a ID_SCONTRINO e TIC_DATE
transactionData <- transactionData %>% 
            group_by(ID_SCONTRINO,TIC_DATE) %>%
            summarise(items = paste0(ID_ARTICOLO, collapse = ","))

View(transactionData)

# Voglio dataset con uno scontrino per ogni riga:

# Tolgo le colonne ID_SCONTRINO e TIC_DATE
transactionData$ID_SCONTRINO <- NULL
transactionData$TIC_DATE <- NULL

#nella colonna item sono presenti tutti gli articoli presenti nello scontrino
#separati da virgola
write.csv(transactionData, file.path(data_dir,"transactionData.csv"), quote = FALSE, row.names = FALSE)

tr <- read.transactions(
  file.path(data_dir,"transactionData.csv"),
  format = 'basket', sep = ','
)

tr
summary(tr)

#plot degli articoli più acquistati
#assoluto
itemFrequencyPlot(tr,topN=8,type="absolute",
                  col=c('#6c0f5e', '#97105c', '#bc2353', '#d84244', '#ea6631', '#f18d14', '#ebb500', '#dadc00'), 
                  main="Absolute Item Frequency Plot")

#relativo
itemFrequencyPlot(tr,topN=8,type="relative",
                  col=c('#6c0f5e', '#97105c', '#bc2353', '#d84244', '#ea6631', '#f18d14', '#ebb500', '#dadc00'), 
                  main="Absolute Item Frequency Plot")


# Min supporto 0.001, confidenza 0.8.
association.rules <- apriori(tr, parameter = list(supp=0.001, conf=0.8,maxlen=10))

inspect(association.rules[1:10])

#elimino regole ridondanti
subset.rules <- which(colSums(is.subset(association.rules, association.rules)) > 1) # get subset rules in vector
length(subset.rules)

subset.association.rules. <- association.rules[-subset.rules] # remove subset rules.

inspect(subset.association.rules.[1:10])

# Filtro regole con confidenza maggiore di 0.4 o 40%
subRules<-association.rules[quality(association.rules)$confidence>0.4]
#Plot SubRules
plot(subRules)
plot(subRules,method="two-key plot")

top10subRules <- head(subRules, n = 10, by = "confidence")
plot(top10subRules, method = "graph",  engine = "htmlwidget")

# Network delle regole di associazioni per le 10 migliori regole
top10subRules <- head(subRules, n = 10, by = "confidence")
plot(top10subRules, method = "graph", colors   =  c("#6c0f5e", "#ea6631"))


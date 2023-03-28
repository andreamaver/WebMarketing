# CALCOLO il numero di giorni dopo cui il 90% dei clienti riacquista
# utilizzo dataset clienti_date_max (riga 378 C07_preparation_df7):
# per ogni cliente, differenze di giorni in cui riacquista (tolti i Na e gli 0)

# calcolo max delle differenze per ogni cliente
clienti_date_max <- clienti_date %>%
  group_by(ID_CLI) %>%
  select(ID_CLI, diff) %>%
  summarise(Max=max(diff))

##PLOT

plot(ecdf(clienti_date_max$Max), main='Customers by days to next purchase',
     xlab='Maximum days to next purchase', ylab='% Customers')
abline(h=0.9)

# giorni: arrotondo a 160
quantile(clienti_date_max$Max, probs = c(0.9))


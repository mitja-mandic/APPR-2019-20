procent_014 <- procenti_poLetih %>% filter(Age_group == "0-14")
napoved_014 <- tibble(year = seq(1980,2025,5))

model014_lm <- lm(data = procent_014, percentage~year)

napoved_014$percentageLm <- predict(model014_lm, napoved_014)


model014_loess <- loess(percentage~year, procent_014, control=loess.control(surface = "direct"))

napoved_014$percentageLoess <- predict(model014_loess, napoved_014)

graf_014_napoved2 <- ggplot(procent_014, aes(x = year, y = percentage)) + geom_line() + 
  geom_line(data = napoved_014, aes(y = percentageLm, color = 'steelblue')) + 
  geom_line(data = napoved_014, aes(y = percentageLoess, color = 'pink')) + theme(legend.position = "none") +
  xlab("Procent") + ylab("0-14")

graf_014_napoved <- ggplotly(graf_014_napoved2)
#print(graf_014_napoved)

procent_1564 <- procenti_poLetih %>% filter(Age_group == "15-64")
napoved_1564 <- tibble(year = seq(1980,2025,5))

model1564_lm <- lm(data = procent_1564, percentage~year)
model1564_loess <- loess(percentage~year, data = procent_1564, control=loess.control(surface = "direct"))
napoved_1564$percentageLm <- predict(model1564_lm, napoved_1564)
napoved_1564$percentageLoess <- predict(model1564_loess, napoved_1564)

graf_1564_napoved1 <- ggplot(procent_1564, aes(x = year, y = percentage)) + geom_line() + 
  geom_line(data = napoved_1564, aes(y = percentageLm, color = "pink")) + 
  geom_line(data = napoved_1564, aes(y = percentageLoess, color = "steelblue")) + 
  theme(legend.position = "none") + xlab("Procent") + ylab("15-64")
graf_1564_napoved <- ggplotly(graf_1564_napoved1)
#print(graf_1564_napoved)




procent_65 <- procenti_poLetih %>% filter(Age_group == "65+")
napoved_65 <- tibble(year  = seq(1980,2025,5))


model65_lm <- lm(data = procent_65, percentage~year)
model65_loess <- loess(percentage~year, data = procent_65, control=loess.control(surface = "direct"))
napoved_65$percentageLm <- predict(model65_lm, napoved_65)
napoved_65$percentageLoess <- predict(model65_loess, napoved_65)

graf_65_napoved1 <- ggplot(procent_65, aes(x = year, y = percentage))  + geom_line() + 
  geom_line(data = napoved_65, aes(y = percentageLm, color = "pink")) + 
  geom_line(data = napoved_65, aes(y = percentageLoess, color = "steelblue")) + theme(legend.position = "none") + 
  xlab("Procent") + ylab("65+")
graf_65_napoved <- ggplotly(graf_65_napoved1)



napovedi <- subplot(graf_014_napoved, graf_1564_napoved, graf_65_napoved, nrows = 3, titleY = TRUE)
print(napovedi)

##CLUSTERING

#procenti starostnih skupin (ni bed)
#0 - 14
podatki_014_1980 <- StarostneStruktureProcent %>% filter(Age_group == "0-14", year == 1980)

cluster_014_1980 <- kmeans(podatki_014_1980$percentage, 5)

centri_014_1980 <- sort(cluster_014_1980$centers)

tabela_014_1980 <- data.frame(country = podatki_014_1980$country, 
                         group = factor(cluster_014_1980$cluster), percentage = podatki_014_1980$percentage)
tabela_014_1980$year <- 1980

podatki_014_2015 <- StarostneStruktureProcent %>% filter(Age_group == "0-14",year == 2015)

cluster_014_2015 <- kmeans(podatki_014_2015$percentage, 5)
centri_014_2015 <- sort(cluster_014_2015$centers)

tabela_014_2015 <- tibble(country = podatki_014_2015$country, 
                          group = factor(cluster_014_2015$cluster), percentage = podatki_014_2015$percentage)
tabela_014_2015$year <- 2015
tabela_014 <- rbind(tabela_014_1980, tabela_014_2015)


zemljevid_cluster_014_1980 <- tm_shape(merge(svet, tabela_014 %>% filter(year == 1980), 
                                             by.x = "NAME", by.y = "country")) + 
  tm_fill(col = "group",palette = "YlOrRd")

#print(zemljevid_cluster_014_1980)

zemljevid_cluster_014_2015 <- tm_shape(merge(svet, tabela_014 %>% filter(year == 2015),
                                             by.x = "NAME", by.y = "country")) + tm_fill(col = "group", palette = "YlOrRd")
#print(zemljevid_cluster_014_2015)


podatki_65_2015 <- StarostneStruktureProcent %>% filter(Age_group == "65+", year == 2015)
cluster_65_2015 <- kmeans(podatki_65_2015$percentage, 5)
tabela_65_2015 <- tibble(country = podatki_65_2015$country, group = factor(cluster_65_2015$cluster, ordered = TRUE),
                         percentage = podatki_65_2015$percentage)

zemljevid_cluster_65_2015 <- tm_shape(merge(svet, tabela_65_2015, by.x = "NAME", by.y = "country")) + 
  tm_fill(col = "group", palette = "YlOrRd")

#print(zemljevid_cluster_65_2015)

#PO BDP

podatkiBdp_2015 <- bdpji %>% filter(year == 2015)

cluster_bdpPc_2015 <- kmeans(podatkiBdp_2015$bdp_pc, 5)

tabela_cluster_gdpPc <- tibble(country = podatkiBdp_2015$country,
                               group = factor(cluster_bdpPc_2015$cluster, ordered = TRUE),
                               gdp_pc = podatkiBdp_2015$bdp_pc)
zemljevid_cluster_bdpPc_2015 <- tm_shape(merge(svet, tabela_cluster_gdpPc, by.x = "NAME", by.y = "country")) +
  tm_fill(col = "group", palette = "YlOrRd")


#print(zemljevid_cluster_bdpPc_2015)


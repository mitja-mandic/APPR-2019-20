procent_014 <- procenti_poLetih %>% filter(Age_group == "0-14")
napoved_014 <- data.frame(year = seq(1980,2025,5))

model014_lm <- lm(data = procent_014, percentage~year)

napoved_014$percentageLm <- predict(model014_lm, napoved_014)


model014_loess <- loess(percentage~year, procent_014, control=loess.control(surface = "direct"))

napoved_014$percentageLoess <- predict(model014_loess, napoved_014)

graf_014_napoved <- ggplot(procent_014, aes(x = year, y = percentage)) + geom_line() + 
  geom_line(data = napoved_014, aes(y = percentageLm, color = "red")) + geom_line(data = napoved_014, aes(y = percentageLoess, color = "blue"))
napoved_014 <- ggplotly(graf_014_napoved)


procent_1564 <- procenti_poLetih %>% filter(Age_group == "15-64")
napoved_1564 <- tibble(year = seq(1980,2025,5))

model1564_lm <- lm(data = procent_1564, percentage~year)
model1564_loess <- loess(percentage~year, data = procent_1564, control=loess.control(surface = "direct"))
napoved_1564$percentageLm <- predict(model1564_lm, napoved_1564)
napoved_1564$percentageLoess <- predict(model1564_loess, napoved_1564)

graf_1564_napoved <- ggplot(procent_1564, aes(x = year, y = percentage)) + geom_line() + 
  geom_line(data = napoved_1564, aes(y = percentageLm, color = "red")) + geom_line(data = napoved_1564, aes(y = percentageLoess, color = "blue"))



#CLUSTERING
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
                                             by.x = "NAME", by.y = "country")) + #tm_polygons(col = "group", midpoint = 2.5)
  tm_fill(col = "group", contrast = 1, palette = "YlOrRd")
print(zemljevid_cluster_014_1980)
zemljevid_cluster_014_2015 <- tm_shape(merge(svet, tabela_014 %>% filter(year == 2015),
                                             by.x = "NAME", by.y = "country")) + #tm_polygons(col = "group", midpoint = 1)
  tm_fill(col = "group", contrast = 1, palette = "YlOrRd")
print(zemljevid_cluster_014_2015)

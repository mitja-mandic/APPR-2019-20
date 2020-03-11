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

podatki_014 <- StarostneStruktureProcent %>% filter(Age_group == "0-14", year == 2000) %>% select(-Age_group)

cluster_014 <- kmeans(podatki_014$percentage, 5)
centri <- sort(cluster_014$centers)
tabela <- data.frame(country = podatki_014$country, group = factor(cluster_014$cluster), percentage = podatki_014$percentage)

graf_cluster_014 <- ggplot(tabela, aes(x = country, y = percentage, color = group)) + geom_point()



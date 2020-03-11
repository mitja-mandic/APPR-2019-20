procent_014 <- procenti_poLetih %>% filter(Age_group == "0-14")
napoved_014 <- data.frame(year = seq(1980,2025,5))

model014_lm <- lm(data = procent_014, percentage~year)

napoved_014$percentageLm <- predict(model014_lm, napoved_014)




model014_loess <- loess(percentage~year, procent_014, control=loess.control(surface = "direct"))

napoved_014$percentageLoess <- predict(model014_loess, napoved_014)

graf_014 <- ggplot(procent_014, aes(x = year, y = percentage)) + geom_line() + 
  geom_line(data = napoved_014, aes(y = percentageLm, color = "red")) + geom_line(data = napoved_014, aes(y = percentageLoess, color = "blue"))
print(graf_014)

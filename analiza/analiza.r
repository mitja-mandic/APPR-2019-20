procent_014 <- procenti_poLetih %>% filter(Age_group == "0-14")
napoved_014 <- data.frame(year = seq(1980,2025,5))

model014_lm <- lm(data = procent_014, percentage~year)

napoved_014$percentageLm <- predict(model014_lm, napoved_014)




model014_loess <- loess(data = procent_014, percentage~year)

napoved_014$percentageLoess <- predict(model014_loess, napoved_014)

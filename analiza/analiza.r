poskus <- bdp_starostneStrutkure %>% filter(Age_group == "0-14") %>% select(-gdp_ppp,-HDI, population) %>%
  na.omit()

model <- lm(bdp_pc ~ percentage, data = poskus)

bdp_ss_predict <- predict(model, interval = "confidence")
podatki <- cbind(poskus, bdp_ss_predict)

bdp_starostne_graf <- ggplot(podatki, aes(x = bdp_pc, y = percentage)) + facet_wrap(year~.) + geom_point() + 
  geom_smooth(method = loess,  fullrange = FALSE, col = "blue")  + scale_x_log10()


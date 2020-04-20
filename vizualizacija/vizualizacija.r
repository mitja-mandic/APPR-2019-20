#HISTOGRAMI STAROSTNIH STRUKTUR SKOZI LETA
procenti_poLetih <- vsote %>% mutate(procent_014 = 100 * prva/total, procent_1564 = 100 * druga/total,
                                     procent_65 = 100 * tretja/total) %>%
  select(-"total", -"prva", -"druga", -"tretja") %>% 
  gather(group, procent, "procent_014":"procent_65", -year) %>% arrange(year) %>%
  mutate(percentage = procent,Age_group = parse_number(group) %>% 
           factor(levels=c(14,1564,65), labels = c("0-14","15-64","65+"), ordered=TRUE)) %>% select(-procent, -group)

procenti_poLetih <- procenti_poLetih[c(1,3,2)]

procenti_poLetih_graf <- ggplot(procenti_poLetih, aes(x=year,y=percentage, fill=Age_group)) + 
  geom_col(position=position_dodge2(preserve = "total")) +
  labs(y = "Procent", color = "Skupina", x = "")
poLetih <- ggplotly(procenti_poLetih_graf, tooltip = 'percentage')


#BDP(ppp) IN STAROSTNE STRUKTURE GRAFI narejeno v shinyju 

bdp_starostneStrutkure <- inner_join(bdpji, StarostneStruktureProcent, by=c("year", "country")) %>%
  left_join(hdi, by = c("country", "year")) %>% left_join(populacija, by = c("year", "country")) %>% 
  mutate(gdp_ppp = gdp_ppp/1000000)

bdp_starostneStrutkure_graf <- ggplot(bdp_starostneStrutkure %>% filter(Age_group=="0-14"),
                                      aes(x=country, y = percentage, color = factor(year))) + geom_point() + 
  theme(axis.text.x = element_blank(), axis.text.y = element_blank())

bdp_starostne <- ggplotly(bdp_starostneStrutkure_graf)



#RELIGIJE IN STAROSTNE STRUKTURE
religije_procenti <- religije_procenti %>% rename('muslimani' = muslims ,'kristjani' = christians, 'tradicionalna verovanja' = folkReligions, 
                                                  'hindujci' = hindus, 'judje' = jews, 'neverujoči' = unaffiliated, 'ostalo' = other, 'budisti' = buddhists)

religion_tidy <- religije_procenti %>% 
  gather(religion, percentage, "muslimani":"kristjani",-country, na.rm = TRUE) %>%
  mutate(religion = religion %>% factor(ordered = TRUE)) %>% arrange(country, desc(percentage))

religion_unique <- religion_tidy %>% distinct(country, .keep_all = TRUE)

relig_starostne <- inner_join(religion_unique, StarostneStruktureProcent, by = "country") %>% 
  rename(percentageReligion = percentage.x, percentage = percentage.y)


povprecjaVere <-relig_starostne %>% group_by(Age_group, religion, year) %>% 
  summarise(povp = mean(percentage))


povpReligije_graf <- ggplot(povprecjaVere, aes(x = year, y = povp, color = religion)) + geom_line() + 
  facet_wrap(Age_group~.) + ggtitle("Graf starostnih skupin in religij skozi leta") + 
  labs(x = "Leto", y = "Povprečen delež populacije", color = "Religija") + 
  theme(axis.title.y = element_text(hjust = 1))
povpReligije <- ggplotly(povpReligije_graf)
print(povpReligije)

#ZEMLJEVIDI

zemljevid_median <- tm_shape(merge(svet, median_age2018, by.x = "NAME", by.y = "country")) + 
  tm_fill(col = "median", contrast = 1, palette = "YlOrRd", title = "Medianska starost", textNA = "Manjkajoči podatki") +
  tm_layout(legend.outside = TRUE)
#print(zemljevid_median)

#MEDIANSKA STAROST IN BDP

median_bdp <- bdpji_median %>% filter(year == 2018) %>% inner_join(median_age2018, by="country") %>% 
  inner_join(religion_unique, by="country") %>%
  inner_join(populacija2018, by="country") %>% mutate(country = country %>% factor(ordered = TRUE), gdp_pc = gdp/population) %>%
  select(-percentage)

median_bdp_graf1 <- ggplot(median_bdp, aes(x = gdp_pc, y=median, color = religion)) + geom_point() + scale_x_log10(labels = comma) + 
  labs(x="BDP per capita", y="Medianska starost", color = "Religija") + 
  ggtitle("Graf BDP p.c. in medianske starosti")

#print(median_bdp_graf1)

median_bdp_graf <- ggplotly(median_bdp_graf1, tooltip = c("country", "median", "religion"))

print(median_bdp_graf)


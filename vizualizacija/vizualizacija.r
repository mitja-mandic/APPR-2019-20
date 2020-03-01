#HISTOGRAMI STAROSTNIH STRUKTUR SKOZI LETA


procenti_poLetih <- vsote %>% mutate(procent_014 = 100 * prva/total, procent_1564 = 100 * druga/total,
                                     procent_65 = 100 * tretja/total) %>%
  select(-"total", -"prva", -"druga", -"tretja") %>% 
  gather(group, procent, "procent_014":"procent_65", -year) %>% arrange(year) %>%
  mutate(percentage = procent,Age_group = parse_number(group) %>% 
           factor(levels=c(14,1564,65), labels = c("0-14","15-64","65+"), ordered=TRUE)) %>% select(-procent, -group)
procenti_poLetih <- procenti_poLetih[c(1,3,2)]


procenti_poLetih_graf <- ggplot(procenti_poLetih, aes(x=year,y=percentage, color=Age_group)) + 
  geom_col(position=position_dodge2(preserve = "total"), fill = 'white')+
  labs(x = "leto", y = "procent", color = "Skupina")

poLetih <- ggplotly(procenti_poLetih_graf)
#print(poLetih)

#BDP(ppp) IN STAROSTNE STRUKTURE GRAFI

bdp_starostneStrutkure <- inner_join(bdpji_ppp, StarostneStruktureProcent, by=c("year", "country"))
bdp_starostneStrutkure <- left_join(bdp_starostneStrutkure, hdi, by = c("country", "year"))
  



bdp_starostneStrutkure_graf <- ggplot(bdp_starostneStrutkure, aes(x=country)) + geom_bar() +
  geom_bar(data = bdp_starostneStrutkure %>% arrange(gdp)) + facet_grid(year~Age_group) + 
  theme(axis.text.x = element_blank(), axis.text.y = element_blank())

print(bdp_starostneStrutkure_graf)


#print(bdp_starostneStrutkure_graf)


bdp_starostne <- ggplotly(bdp_starostneStrutkure_graf)

print(bdp_starostne)



#RELIGIJE IN STAROSTNE STRUKTURE

krscanske <- religion_tidy %>% filter(religion == "christians") %>% filter(percentage >= 70, percentage <= 100)

krscanske_starostneStrukture <- inner_join(StarostneStruktureProcent, krscanske, by = "country") %>% 
   mutate(percentage = percentage.x) %>% select(-"percentage.y", -"percentage.x")

muslimanske <- religion_tidy %>% filter(religion == "muslims") %>% filter(percentage >= 70, percentage <= 100) 
muslimanske_starostneStrukture <- inner_join(StarostneStruktureProcent, muslimanske, by = "country") %>% 
  mutate(percentage = percentage.x) %>% select(-"percentage.y", -"percentage.x")

starostne_poVeri <- rbind(krscanske_starostneStrukture, muslimanske_starostneStrukture) %>%
  arrange(by = country)

starostne_poVeri_mean <- starostne_poVeri %>% 
  group_by(Age_group, religion) %>% summarise(mean = mean(percentage))

starostne_poVeri_graf <- ggplot(starostne_poVeri, aes(x=country, y = percentage, color = religion)) + 
  geom_col() + facet_grid(year~Age_group) + 
  geom_hline(data = starostne_poVeri_mean, aes(yintercept = mean, color = religion), size=1.3)

budisticne <- religion_tidy %>% filter(religion == "buddhists") %>% filter(percentage >= 70) 

krscanske_starostneStrukture_graf <- ggplot(krscanske_starostneStrukture, aes(x=percentage)) +
  geom_histogram() + facet_grid(year~Age_group)




#print(krscanske_starostneStrukture_graf)


#ZEMLJEVIDI

zemljevid_median <- tm_shape(merge(svet, median_age2018, by.x = "NAME", by.y = "country")) + 
  tm_polygons(col = "median", midpoint = 1, legend.hist = TRUE) + tm_layout(legend.outside = TRUE)


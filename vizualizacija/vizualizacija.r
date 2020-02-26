#HISTOGRAMI STAROSTNIH STRUKTUR SKOZI LETA


procenti_poLetih <- vsote %>% mutate(procent_014 = 100 * prva/total, procent_1564 = 100 * druga/total,
                                     procent_65 = 100 * tretja/total) %>%
  select(-"total", -"prva", -"druga", -"tretja") %>% 
  gather(group, procent, "procent_014":"procent_65", -year) %>% arrange(year) %>%
  mutate(percentage = procent,Age_group = parse_number(group) %>% 
           factor(levels=c(14,1564,65), labels = c("0-14","15-64","65+"), ordered=TRUE)) %>% select(-procent, -group)
procenti_poLetih <- procenti_poLetih[c(1,3,2)]
View(procenti_poLetih)

procenti_poLetih_graf <- ggplot(procenti_poLetih, aes(x=year,y=percentage, color=Age_group)) + 
  geom_col(position=position_dodge2(preserve = "total"), fill = 'white') 




#BDP(ppp) IN STAROSTNE STRUKTURE GRAFI

bdp_starostneStrutkure <- inner_join(bdpji_ppp, StarostneStruktureProcent, by=c("year", "country"))
bdp_starostneStrutkure_graf <- ggplot(bdp_starostneStrutkure, aes(x=gdp,y=percentage)) + geom_point() +
  facet_grid(year~Age_group) + scale_x_log10()
print(bdp_starostneStrutkure_graf)



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
  geom_point() + facet_grid(year~Age_group) + 
  geom_hline(data = starostne_poVeri_mean, aes(yintercept = mean, color = religion), size=1.3)

print(starostne_poVeri_graf)

budisticne <- religion_tidy %>% filter(religion == "buddhists") %>% filter(percentage >= 70) 



krscanske_starostneStrukture_graf <- ggplot(krscanske_starostneStrukture, aes(x=percentage)) +
  geom_histogram() + facet_grid(year~Age_group)
#print(krscanske_starostneStrukture_graf)


#ZEMLJEVIDI


#zemljevid014 <- tm_shape(merge(svet, starostne_strukture014, by.x = "NAME", by.y = "country")) + 
#  tm_polygons("percentage", midpoint = 0.2)

#print(zemljevid64)



# zemljevid15_64 <- tm_shape(merge(svet, starost15_64, by.x = "NAME", by.y = "country")) + 
# tm_polygons("percentage", midpoint = 0.2)

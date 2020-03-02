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




#BDP(ppp) IN STAROSTNE STRUKTURE GRAFI narejeno v shinyju 

bdp_starostneStrutkure <- inner_join(bdpji_ppp, StarostneStruktureProcent, by=c("year", "country")) %>%
  left_join(hdi, by = c("country", "year")) %>% left_join(populacija, by = c("year", "country"))

bdp_starostneStrutkure_graf <- ggplot(bdp_starostneStrutkure %>% filter(Age_group=="0-14"),
                                      aes(x=country, y = percentage, color = factor(year))) + geom_point() + 
  theme(axis.text.x = element_blank(), axis.text.y = element_blank())

bdp_starostne <- ggplotly(bdp_starostneStrutkure_graf)



#RELIGIJE IN STAROSTNE STRUKTURE

prevladujocaVera_starostne <- inner_join(prevladujoceVere, StarostneStruktureProcent, by = "country")
povprecjaVere <- prevladujocaVera_starostne %>% group_by(Age_group, religion, year) %>% summarise(povp = mean(percentage))




prevladujocaVera_starostne_graf <- ggplot(prevladujocaVera_starostne %>% arrange(religion, desc(percentage)),
                                           aes(x = country, y = percentage, 
                                          color = religion, fill = religion)) + 
  geom_col(alpha = 0.5) +  facet_grid(year~Age_group)+ theme(axis.text.x = element_blank()) +
  geom_hline(yintercept = povprecjaVere)


povpReligije_graf <- ggplot(povprecjaVere, aes(x = year, y = povp, color = religion)) + geom_line() + 
  facet_wrap(Age_group~.)

povpReligije <- ggplotly(povpReligije_graf)



relig_poskus <- religion_tidy %>% arrange(religion, desc(percentage))
relig_starostne <- inner_join(relig_poskus, StarostneStruktureProcent, by = "country")

relig_starostne_graf <- ggplot(relig_starostne %>% filter(Age_group == "0-14", religion == "christians"), 
                               aes(x = country, y = percentage.y, color = religion, fill = religion)) +
  geom_col() + geom_col(data = relig_starostne %>%filter(Age_group == "0-14",religion == "muslims"),
                        position = position_dodge(preserve = "single")) + facet_wrap(year~.)
  

print(relig_starostne_graf)


#ZEMLJEVIDI

zemljevid_median <- tm_shape(merge(svet, median_age2018, by.x = "NAME", by.y = "country")) + 
  tm_polygons(col = "median", midpoint = 1, legend.hist = TRUE) + tm_layout(legend.outside = TRUE)


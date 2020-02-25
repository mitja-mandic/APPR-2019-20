#HISTOGRAMI STAROSTNIH STRUKTUR SKOZI LETA

# starostne_strukture014 <- StarostneStrukture_janos %>% filter(Age_group=="0-14") %>% filter(year>= "1980")
# starostneStruture014_graf <- ggplot(starostne_strukture014, aes(x=percentage)) +
#   geom_histogram(binwidth = 0.7) + facet_wrap(~year)
# 
# starostne_strukture1564 <- StarostneStrukture_janos %>% filter(Age_group=="15-64") %>% filter(year>= "1980")
# starostneStruture1564_graf <- ggplot(starostne_strukture1564, aes(x=percentage)) +
#   geom_histogram(binwidth = 0.7) + facet_wrap(~year)
# 
# starostne_strukture65 <- StarostneStrukture_janos %>% filter(Age_group=="65+") %>% filter(year>= "1980")
# starostneStruture65_graf <- ggplot(starostne_strukture65, aes(percentage)) + #stat_mean_line() +
#   geom_histogram(binwidth = 0.7) + facet_wrap(~year)
#print(starostneStruture65_graf)


procenti_poLetih <- vsote %>% mutate(procent_014 = 100 * prva/total, procent_1564 = 100 * druga/total,
                                     procent_65 = 100 * tretja/total) %>%
  select(-"total", -"prva", -"druga", -"tretja") %>% 
  gather(group, procent, procent_014, procent_1564, procent_65, -year) %>% arrange(year) %>%
  mutate(percentage = procent,Age_group = parse_number(group) %>% 
           factor(levels=c(14,1564,65), labels = c("0-14","15-64","65+"), ordered=TRUE))

procenti_poLetih_graf <- ggplot(procenti_poLetih, aes(x=year,y=percentage, color=Age_group)) + 
  geom_col(position=position_dodge2(preserve = "total"), fill = 'white') 

#print(procenti_poLetih_graf)


#BDP ppp IN STAROSTNE STRUKTURE GRAFI

bdp_starostneStrukture014 <- inner_join(bdpji, StarostneStrukture_janos, by = c("year", "country")) %>% 
  filter(Age_group == "0-14")
bdp_starostne014_graf <- ggplot(bdp_starostneStrukture014, aes(x=gdp, y=percentage)) +
  geom_point() + scale_x_log10() + facet_wrap(~year)
#print(bdp_starostne014_graf)

# + geom_hline(yintercept = group_by(bdp_starostneStrukture65, year) %>%
#               summarise(povp = mean(percentage)))


bdp_starostneStrukture65 <- inner_join(bdpji, StarostneStrukture_janos, by = c("year", "country")) %>% 
  filter(Age_group == "65+")
bdp_starostne65_graf <- ggplot(bdp_starostneStrukture65, aes(x=gdp, y=percentage)) +
  geom_point() + scale_x_log10() + facet_wrap(~year)


#print(bdp_starostne65_graf)


#RELIGIJE IN STAROSTNE STRUKTURE
#nimam pojma kaj zaenkrat




#ZEMLJEVIDI

#zemljevid014 <- tm_shape(merge(svet, starostne_strukture014, by.x = "NAME", by.y = "country")) + 
#  tm_polygons("percentage", midpoint = 0.2)

#print(zemljevid64)



zemljevid15_64 <- tm_shape(merge(svet, starost15_64, by.x = "NAME", by.y = "country")) + 
tm_polygons("percentage", midpoint = 0.2)

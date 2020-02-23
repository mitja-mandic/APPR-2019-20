#HISTOGRAMI STAROSTNIH STRUKTUR SKOZI LETA
starostne_strukture014 <- StarostneStrukture_janos %>% filter(Age_group=="0-14") %>% filter(year>= "1980")
starostneStruture014_graf <- ggplot(starostne_strukture014, aes(x=percentage)) + 
  geom_histogram(binwidth = 0.7) + facet_wrap(~year)

starostne_strukture1564 <- StarostneStrukture_janos %>% filter(Age_group=="15-64") %>% filter(year>= "1980")
starostneStruture1564_graf <- ggplot(starostne_strukture1564, aes(x=percentage)) + 
  geom_histogram(binwidth = 0.7) + facet_wrap(~year)





#print(starostneStruture1564_graf)

starostne_strukture65 <- StarostneStrukture_janos %>% filter(Age_group=="65+") %>% filter(year>= "1980")
starostneStruture65_graf <- ggplot(starostne_strukture65, aes(percentage)) + #stat_mean_line() + 
  geom_histogram(binwidth = 0.7) + facet_wrap(~year)

print(starostneStruture65_graf)

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

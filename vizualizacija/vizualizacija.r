
starost64 <- StarostneStrukture_janos %>% filter(Age_group == "65 in vec") %>% filter(year == "1960")


bdp_starostneStrukture65 <- inner_join(bdpji, StarostneStrukture_janos, by = c("year", "country")) %>% 
  filter(Age_group == "65 in vec")

bdp_starostne65_graf <- ggplot(bdp_starostneStrukture65, aes(x=gdp, y=percentage)) +
  geom_point() + scale_x_log10() + facet_wrap(~year)

# + geom_hline(yintercept = group_by(bdp_starostneStrukture65, year) %>%
#               summarise(povp = mean(percentage)))

print(bdp_starostne65_graf)




starost15_64 <- StarostneStrukture_janos %>% filter(Age_group == "15-64") %>% filter(year == "2015")
starost15_64_graf <- ggplot(starost15_64, aes(x = percentage)) +
  geom_histogram()
print(starost15_64_graf)



zemljevid64 <- tm_shape(merge(svet, starost64, by.x = "NAME", by.y = "country")) + 
  tm_polygons("percentage", midpoint = 0.2)

zemljevid15_64 <- tm_shape(merge(svet, starost15_64, by.x = "NAME", by.y = "country")) + 
tm_polygons("percentage", midpoint = 0.2)

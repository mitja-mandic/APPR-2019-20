starost15_64 <- StarostneStrukture_janos %>% filter(Age_group == "15-64") %>% filter(year == "2015")
starost15_64_graf <- ggplot(starost15_64, aes(x = percentage)) +
  geom_histogram()
print(starost15_64_graf)

starost64 <- StarostneStrukture_janos %>% filter(Age_group == "65 in vec") %>% filter(year == "1960")

zemljevid64 <- tm_shape(merge(svet, starost64, by.x = "NAME", by.y = "country")) + 
  tm_polygons("percentage", midpoint = 0.2)
print(zemljevid64)

zemljevid15_64 <- tm_shape(merge(svet, starost15_64, by.x = "NAME", by.y = "country")) + 
tm_polygons("percentage", midpoint = 0.2)

print(zemljevid15_64)

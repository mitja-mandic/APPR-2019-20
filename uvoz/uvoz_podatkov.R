library(readr)
library(dplyr)
StarostneStrukture_deležPrebivalstva <- read_csv("podatki/starostneStrukture_procentPopulacije.csv", na = ("..")) %>% select(-'Series Code', -'Country Code')

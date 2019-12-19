library(readr)
library(dplyr)
library(rvest)
library(XML)
library(gsubfn)
library(tidyr)


StarostneStrukture_deležPrebivalstva <- read_csv("podatki/starostneStrukture_procentPopulacije.csv", na = ("..")) %>% select(-'Series Code', -'Country Code')

url <- "https://en.wikipedia.org/wiki/List_of_countries_by_past_and_projected_GDP_(PPP)"
stran <- read_html(url)

library(readr)
library(dplyr)
library(rvest)
library(XML)
library(gsubfn)
library(tidyr)
library(stringr)
StarostneStrukture_delezPrebivalstva <- read_csv("podatki/podatki.csv", na = ("..")) %>% select(-'Series Code', -'Country Code')
colnames(StarostneStrukture_delezPrebivalstva) <- c("Age_group", "country", leta)
leta <- seq(1960, 2015, by=5)

starost0.14 <- StarostneStrukture_delezPrebivalstva %>% filter(Age_group == "Population ages 0-14 (% of total population)") %>%
  select(-Age_group) %>%
  gather(year, percentage, as.character(leta)) %>%
  arrange(country)


starost15.65 <- StarostneStrukture_delezPrebivalstva %>% filter(Age_group == "Population ages 15-64 (% of total population)") %>%
  select(-Age_group) %>%
  gather(year, percentage, as.character(leta)) %>%
  arrange(country)

starost65 <- StarostneStrukture_delezPrebivalstva %>% filter(Age_group == "Population ages 65 and above (% of total population)") %>%
  select(-Age_group) %>%
  gather(year, percentage, as.character(leta)) %>%
  arrange(country)


url <- "https://en.wikipedia.org/wiki/List_of_countries_by_past_and_projected_GDP_(PPP)"
stran <- read_html(url)

tabela_wiki <- stran %>% 
  html_nodes(xpath="//table[@class='sortable wikitable']//td") %>%
  html_text() %>% str_replace_all(',','')


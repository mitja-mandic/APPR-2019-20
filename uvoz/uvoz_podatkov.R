library(readr)
library(dplyr)
library(rvest)
library(XML)
library(gsubfn)
library(tidyr)
library(stringr)
leta <- seq(1960, 2015, by=5)

StarostneStrukture_delezPrebivalstva <- read_csv("podatki/podatki.csv", na = ("..")) %>% select(-'Series Code', -'Country Code')
colnames(StarostneStrukture_delezPrebivalstva) <- c("Age_group", "country", leta)

StarostneStrukture_janos <- StarostneStrukture_delezPrebivalstva %>%
  gather(year, percentage, -Age_group, -country, na.rm=TRUE) %>% # odstranimo manjkajoče vrednosti
  mutate(year=parse_number(year), # leta pretvorimo v števila
         Age_group=parse_number(Age_group) %>% # uvedemo urejen faktor za skupine
           factor(levels=c(0, 15, 65), labels=c("0-14", "15-64", "65 in več"),
                  ordered=TRUE))

url <- "https://en.wikipedia.org/wiki/List_of_countries_by_past_and_projected_GDP_(PPP)"
stran <- read_html(url)

tabela_wiki <- stran %>% 
  html_nodes(xpath="//table[@class='sortable wikitable']") %>% .[[1]] %>%
  html_table() %>% rename(country=`Country (or dependent territory)`) %>%
  gather(year, gdp, -country) %>%
  mutate(year=parse_number(year),
         gdp=parse_number(gdp, locale=locale(grouping_mark=",")))

religije <- read.csv("podatki/religije.csv", na=c("5000"))



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


gpd.ppp.8089 <- stran %>% html_node(xpath = "//*[@id = 'mw-content-text']/div/table") %>%
  html_table(fill=TRUE)

colnames(gpd.ppp.8089) <- c("country", leta = 1980:1989)


gpd.ppp.8089 <- gpd.ppp.8089 %>% gather(year, bdp, as.character(1980:1989)) %>% 
  arrange(country)

gdp.ppp.9099 <- stran %>% html_node(xpath = "//*[@id = 'mw-content-text']/div/table[2]") %>%
  html_table(fill=TRUE)
colnames(gdp.ppp.9099) <- c("country", 1990:1999)



gpd.ppp.9099 <- gpd.ppp.9099 %>% gather(year, bdp, as.character(1990:1999)) %>% 
  arrange(country)

gdp.ppp.0009 <- stran %>% html_node(xpath = "//*[@id = 'mw-content-text']/div/table[3]") %>%
  html_table(fill=TRUE)
colnames(gdp.ppp.0009) <- c("country", 2000:2009)
gpd.ppp.0009<- gpd.ppp.0009 %>% gather(year, bdp, as.character(2000:2009)) %>% 
  arrange(country)

gpd.ppp.1019 <- stran %>% html_node(xpath = "//*[@id = 'mw-content-text']/div/table[4]") %>%
  html_table(fill=TRUE)
colnames(gdp.ppp.1019) <- c("country", 2010:2019)
gpd.ppp.1019 <- gpd.ppp.1019 %>% gather(year, bdp, as.character(2010:2019)) %>% 
  arrange(country)




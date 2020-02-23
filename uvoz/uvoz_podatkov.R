source("lib/libraries.r", encoding="UTF-8")
leta <- seq(1960, 2015, by=5)
leta2 <- seq(1980, 2015, by=5)


StarostneStrukture_delezPrebivalstva_podatki <- read_csv("podatki/podatki.csv", na = ("..")) %>% select(-'Series Code', -'Country Code')
colnames(StarostneStrukture_delezPrebivalstva_podatki) <- c("Age_group", "country", leta)

StarostneStruktureCelota_podatki <- read_csv("podatki/starostneStrukture.csv", na = ("..")) %>% select(-'Series Code', -'Country Code')
colnames(StarostneStruktureCelota_podatki) <- c("Age_group", "country", leta2)


StarostneStruktureProcent_janos <- StarostneStrukture_delezPrebivalstva_podatki %>%
  gather(year, percentage, -Age_group, -country, na.rm=TRUE) %>% # odstranimo manjkajoče vrednosti
  mutate(year=parse_number(year), # leta pretvorimo v števila
         Age_group=parse_number(Age_group) %>% # uvedemo urejen faktor za skupine
           factor(levels=c(0, 15, 65), labels=c("0-14", "15-64", "65+"),
                  ordered=TRUE))

StarostneStruktureCelota <- StarostneStrukture_podatki %>%
  gather(year, number, "1980":"2015") %>% 
  mutate(year=parse_number(year), 
         Age_group=parse_number(Age_group) %>%
           factor(levels=c(0,15,65), labels=c("0-14", "15-64", "65+"), ordered=TRUE))




#BDPJI

url <- "https://en.wikipedia.org/wiki/List_of_countries_by_past_and_projected_GDP_(PPP)"
stran <- read_html(url)

bdp_osemdeseta <- stran %>% 
  html_nodes(xpath="//table[@class='sortable wikitable']") %>% .[[1]] %>%
  html_table() %>% rename(country=`Country (or dependent territory)`) %>%
  gather(year, gdp, -country) %>%
  mutate(year=parse_number(year),
         gdp=parse_number(gdp, locale=locale(grouping_mark=",")))

bdp_devetdeseta <- stran %>% 
  html_nodes(xpath="//table[@class='sortable wikitable']") %>% .[[2]] %>%
  html_table() %>% rename(country=`Country (or dependent territory)`) %>%
  gather(year, gdp, -country) %>%
  mutate(year=parse_number(year),
         gdp=parse_number(gdp, locale=locale(grouping_mark=",")))

bdp_deseta <- stran %>% 
  html_nodes(xpath="//table[@class='sortable wikitable']") %>% .[[3]] %>%
  html_table() %>% rename(country=`Country (or dependent territory)`) %>%
  gather(year, gdp, -country) %>%
  mutate(year=parse_number(year),
         gdp=parse_number(gdp, locale=locale(grouping_mark=",")))

bdp_dvajseta <- stran %>% 
  html_nodes(xpath="//table[@class='sortable wikitable']") %>% .[[4]] %>%
  html_table() %>% rename(country=`Country (or dependent territory)`) %>%
  gather(year, gdp, -country) %>%
  mutate(year=parse_number(year),
         gdp=parse_number(gdp, locale=locale(grouping_mark=",")))

bdpji_ppp <- rbind(bdp_osemdeseta,bdp_devetdeseta,bdp_deseta,bdp_dvajseta) 
bdpji$country <- standardize.countrynames(bdpji$country, suggest = "auto", print.changes = FALSE)


religije <- read.csv("podatki/religije.csv", na=c("5000")) %>%
  rename(country = name) %>%
  select(-"pop2019")
religije$country <- standardize.countrynames(religije$country, suggest = "auto", print.changes = FALSE)


svet <- uvozi.zemljevid(
  "http://www.naturalearthdata.com/http//www.naturalearthdata.com/download/50m/cultural/ne_50m_admin_0_countries.zip",
  "ne_50m_admin_0_countries", encoding="UTF-8")
svet$NAME <- standardize.countrynames(svet$NAME, suggest = "auto", print.changes = FALSE)

populacija <- read.csv("podatki/populacija.csv", na = ("..")) %>%
  select(-"ï..Series.Name",-"Series.Code",-"Country.Code") 
colnames(populacija) <- c("country", leta)

populacija <- populacija %>% gather(year, population, "1960":"2015") %>%
  mutate(year=parse_number(year))
populacija$country <- standardize.countrynames(populacija$country, suggest = "auto", print.changes = FALSE)


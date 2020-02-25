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


poLetih_014 <- inner_join(StarostneStruktureCelota, bdpji, by=c("country", "year")) %>%
  select(-"gdp") %>% filter(Age_group == "0-14") %>% group_by(year) %>%
  summarise(number = sum(number, na.rm = TRUE))

poLetih_1564 <- inner_join(StarostneStruktureCelota, bdpji, by=c("country", "year")) %>%
  select(-"gdp") %>% filter(Age_group == "15-64") %>% group_by(year) %>%
  summarise(number = sum(number, na.rm = TRUE))

poLetih_65 <- inner_join(StarostneStruktureCelota, bdpji, by=c("country", "year")) %>%
  select(-"gdp") %>% filter(Age_group == "65+") %>% group_by(year) %>%
  summarise(number = sum(number, na.rm = TRUE))





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

#RELIGIJE

religije <- read.csv("podatki/religije.csv", na=c("5000")) %>%
  rename(country = name)
religije$country <- standardize.countrynames(religije$country, suggest = "auto", print.changes = FALSE)

religije_procenti <- religije %>% mutate(pop2019 = 1000*pop2019, christians = 100 * chistians/pop2019,
                                    muslims = 100 * muslims/pop2019, unaffiliated = 100 * unaffiliated/pop2019,
                                    hindus = 100 * hindus/pop2019, buddhists = buddhists * 100 / pop2019,
                                    folkReligions = folkReligions * 100/pop2019,
                                    jews = 100 * jews/pop2019, other = 100 * other / pop2019) %>%
  select(-pop2019, -chistians)

religije_tidy <- religije_procenti %>% gather(religion, percentage, muslims, christians, buddhists, hindus, 
                                              jews, unaffiliated, folkReligions, other, -country) %>% 
  arrange(by = country)








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

celotnaPopulacija <- inner_join(bdpji, populacija, by=c("country", "year")) %>%
  select(-"gdp") %>% group_by(year) %>% summarise(population = sum(population, na.rm = TRUE))
 

vsote <- inner_join(celotnaPopulacija, poLetih_014, by="year") %>% inner_join(poLetih_1564, by="year") %>%
  inner_join(poLetih_65, by="year")
colnames(vsote) <- c("year","total", "prva", "druga", "tretja")

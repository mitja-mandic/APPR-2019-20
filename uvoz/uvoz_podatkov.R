source("lib/libraries.r", encoding="UTF-8")
leta <- seq(1960, 2015, by=5)
leta2 <- seq(1980, 2015, by=5)
# 
# #probs irelevantno
# StarostneStrukture_delezPrebivalstva_podatki <- read_csv("podatki/podatki.csv", na = ("..")) %>% select(-'Series Code', -'Country Code')
# colnames(StarostneStrukture_delezPrebivalstva_podatki) <- c("Age_group", "country", leta)
# 
# #ne uporabim, delam s surovimi podatki
# StarostneStruktureProcent_janos <- StarostneStrukture_delezPrebivalstva_podatki %>%
#   gather(year, percentage, -Age_group, -country, na.rm=TRUE) %>% # odstranimo manjkajoče vrednosti
#   mutate(year=parse_number(year), # leta pretvorimo v števila
#          Age_group=parse_number(Age_group) %>% # uvedemo urejen faktor za skupine
#            factor(levels=c(0, 15, 65), labels=c("0-14", "15-64", "65+"),
#                   ordered=TRUE))

StarostneStruktureCelota_podatki <- read_csv("podatki/starostneStrukture.csv", na = ("..")) %>% select(-'Series Code', -'Country Code')
colnames(StarostneStruktureCelota_podatki) <- c("Age_group", "country", leta2)

StarostneStruktureCelota <- StarostneStruktureCelota_podatki %>%
  gather(year, number, "1980":"2015", na.rm = TRUE) %>% 
   mutate(year=parse_number(year), 
          Age_group=parse_number(Age_group) %>%
            factor(levels=c(0,15,65), labels=c("0-14", "15-64", "65+"), ordered=TRUE))


#BDPJI

url <- "https://en.wikipedia.org/wiki/List_of_countries_by_past_and_projected_GDP_(PPP)"
stran <- read_html(url)

bdp_osemdeseta <- stran %>% 
  html_nodes(xpath="//table[@class='sortable wikitable']") %>% .[[1]] %>%
  html_table() %>% rename(country=`Country (or dependent territory)`) %>%
  gather(year, gdp, -country, na.rm = TRUE) %>%
  mutate(year=parse_number(year),
         gdp=parse_number(gdp, locale=locale(grouping_mark=",")))

bdp_devetdeseta <- stran %>% 
  html_nodes(xpath="//table[@class='sortable wikitable']") %>% .[[2]] %>%
  html_table() %>% rename(country=`Country (or dependent territory)`) %>%
  gather(year, gdp, -country, na.rm = TRUE) %>%
  mutate(year=parse_number(year),
         gdp=parse_number(gdp, locale=locale(grouping_mark=",")))

bdp_deseta <- stran %>% 
  html_nodes(xpath="//table[@class='sortable wikitable']") %>% .[[3]] %>%
  html_table() %>% rename(country=`Country (or dependent territory)`) %>%
  gather(year, gdp, -country, na.rm = TRUE) %>%
  mutate(year=parse_number(year),
         gdp=parse_number(gdp, locale=locale(grouping_mark=",")))

bdp_dvajseta <- stran %>% 
  html_nodes(xpath="//table[@class='sortable wikitable']") %>% .[[4]] %>%
  html_table() %>% rename(country=`Country (or dependent territory)`) %>%
  gather(year, gdp, -country, na.rm = TRUE) %>%
  mutate(year=parse_number(year),
         gdp=parse_number(gdp, locale=locale(grouping_mark=",")))

bdpji <- rbind(bdp_osemdeseta,bdp_devetdeseta,bdp_deseta,bdp_dvajseta)
bdpji$gdp <- bdpji$gdp * 1000000
bdpji$country <- standardize.countrynames(bdpji$country, suggest = "auto", print.changes = FALSE)




poLetih_014 <- inner_join(StarostneStruktureCelota, bdpji, by=c("country", "year")) %>%
  select(-"gdp") %>% filter(Age_group == "0-14") %>% group_by(year) %>%
  summarise(number = sum(number, na.rm = TRUE))

poLetih_1564 <- inner_join(StarostneStruktureCelota, bdpji, by=c("country", "year")) %>%
  select(-"gdp") %>% filter(Age_group == "15-64") %>% group_by(year) %>%
  summarise(number = sum(number, na.rm = TRUE))

poLetih_65 <- inner_join(StarostneStruktureCelota, bdpji, by=c("country", "year")) %>%
  select(-"gdp") %>% filter(Age_group == "65+") %>% group_by(year) %>%
  summarise(number = sum(number, na.rm = TRUE))

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

#CELOTE

populacija <- read_csv("podatki/populacija.csv", na="..", locale=locale(encoding="UTF-8")) %>%
  select(-"Series Name", -"Series Code", -"Country Code")
colnames(populacija) <- c("country", leta)

populacija <- populacija %>% gather(year, population, "1960":"2015") %>%
  mutate(year=parse_number(year))


populacija$country <- standardize.countrynames(populacija$country, suggest = "auto", print.changes = FALSE)

celotnaPopulacija <- inner_join(bdpji, populacija, by=c("country", "year")) %>%
  select(-"gdp") %>% group_by(year) %>% summarise(population = sum(population, na.rm = TRUE))
 
vsote <- inner_join(celotnaPopulacija, poLetih_014, by="year") %>% inner_join(poLetih_1564, by="year") %>%
  inner_join(poLetih_65, by="year")
colnames(vsote) <- c("year","total", "prva", "druga", "tretja")

StarostneStruktureProcent <- inner_join(StarostneStruktureCelota, populacija, by = c("country", "year")) %>%
  mutate(percentage = 100 * number/population) %>% select(-"population",-"number") 


bdpji <- inner_join(bdpji, populacija, by = c("country", "year")) %>% 
  mutate(bdp_pc = gdp / population) %>% select(-population) %>% rename(gdp_ppp = gdp)

bdpji <- na.omit(bdpji)



#ZA ZEMLJEVIDE
svet <- uvozi.zemljevid(
  "http://www.naturalearthdata.com/http//www.naturalearthdata.com/download/50m/cultural/ne_50m_admin_0_countries.zip",
  "ne_50m_admin_0_countries", encoding="UTF-8")
svet$NAME <- standardize.countrynames(svet$NAME, suggest = "auto", print.changes = FALSE)

#HDI 

hdi_podatki <- read_csv("podatki/hdi.csv", skip = 1, n_max = 189, na = c(".."))

hdi <- hdi_podatki %>% 
  select(-"HDI Rank (2018)") %>% gather(leto, HDI, "1990":"2018") %>%
  mutate(year = parse_number(leto)) %>% rename(country = Country) %>% select(-leto)
hdi <- hdi[c(1,3,2)]



#MEDIANSKA STAROST

link <- "https://en.wikipedia.org/wiki/List_of_countries_by_median_age"
page <- read_html(link)

median_age2018 <- page %>% 
  html_nodes(xpath="//table[@class='wikitable sortable']") %>% .[[1]] %>%
  html_table() %>% rename(country = "Country/Territory", median = "Median(Years)") %>%
  select(-'Rank', -'Male(Years)', -"Female (Years)")

median_age2018$country <- standardize.countrynames(median_age2018$country, suggest = "auto", print.changes = FALSE)




#IZOBRAZBA

enrollmentRatios_podatki <- read.csv("podatki/enrollmentRatiosGross.csv", na = (".."))  %>% select(-'Series.Code', -'Country.Code')
colnames(enrollmentRatios_podatki) <- c("series", "country", leta2)

enrollmentRatios <- enrollmentRatios_podatki %>% gather(year, percentage, "1980":"2015") %>% 
  mutate(series = factor(series, labels = c("","","","primary", "secondary","tertiary")), year = parse_number(year))
enrollmentRatios <- inner_join(bdpji, enrollmentRatios, by = c("country","year")) %>% select(-"gdp_ppp",-"bdp_pc")
enrollmentRatios <- enrollmentRatios[c(3,1,2,4)] %>% arrange(series)

izo_starostne <- inner_join(enrollmentRatios, StarostneStruktureProcent ,by = c("country", "year")) %>%
  rename(percentageIzo = percentage.x, percentage = percentage.y)




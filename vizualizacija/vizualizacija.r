# # 3. faza: Vizualizacija podatkov
# 
# # Uvozimo zemljevid.
# zemljevid <- uvozi.zemljevid("http://baza.fmf.uni-lj.si/OB.zip", "OB",
#                              pot.zemljevida="OB", encoding="Windows-1250")
# levels(zemljevid$OB_UIME) <- levels(zemljevid$OB_UIME) %>%
#   { gsub("Slovenskih", "Slov.", .) } %>% { gsub("-", " - ", .) }
# zemljevid$OB_UIME <- factor(zemljevid$OB_UIME, levels=levels(obcine$obcina))
# zemljevid <- fortify(zemljevid)
# 
# # Izračunamo povprečno velikost družine
# povprecja <- druzine %>% group_by(obcina) %>%
#   summarise(povprecje=sum(velikost.druzine * stevilo.druzin) / sum(stevilo.druzin))

# Za risanje bomo uporabili knjižnico ggplot2. Pri delu s podatki si bomo pomagali še s knjižnico dplyr.
# 
# library(ggplot2)
# library(dplyr)
# 
# Pri risanju s knjižnico ggplot2 uporabimo ukaz ggplot, ki mu podamo podatke, iz katerih bomo izpeljali graf. Funkcija vrne nedokončan graf, ki ga še ne moremo narisati. Zato mu bo treba "prišteti" še (vsaj en) tip grafa, ki ga želimo, in morda še kakšne dodatne nastavitve:
#   
#   funkcije geom_point(), geom_line(), geom_path() in geom_step() določajo grafe točk, črt, poti oziroma stopnic;
# funkciji geom_histogram() in geom_bar() določata histograme oziroma stolpčne diagrame;
# s funkcijami geom_hline(), geom_vline() in geom_abline() lahko rišemo vodoravne in navpične premice oziroma premice s poljubnim naklonom;
# s funkcijama geom_rect() in geom_polygon() lahko rišemo pravokotnike oziroma splošne večkotnike;
# s funkcijo geom_text() lahko na graf dodamo besedilo;
# s funkcijo coord_polar() lahko določimo, da bomo uporabljali polarne koordinate;
# s funkcijo stat_function() lahko dodamo krivuljo na graf;
# s funkcijo aes() določimo, iz katerih podatkov bomo risali graf (lahko jo podamo kar kot argument h ggplot).
# 
# Vsakič, ko narišemo osnovni graf, se stari graf pobriše.
# 
# # 500-krat si na različne načine izberimo naključno x in y koordinato
# ggplot(data.frame(x=runif(500), y=runif(500))) + aes(x=x, y=y) + geom_point()
# ggplot(data.frame(x=runif(500), y=rnorm(500))) + aes(x=x, y=y) + geom_point()
# ggplot(data.frame(x=rnorm(500), y=rnorm(500))) + aes(x=x, y=y) + geom_point()
# # Slika naključnega sprehoda v ravnini.
# ggplot(data.frame(x=cumsum(runif(30, -1, 1)), y=cumsum(runif(30, -1, 1)))) + aes(x=x, y=y) + geom_path()
# # Graf sinusne funkcije
# ggplot(data.frame(x=c(-2*pi, 2*pi))) + aes(x) + stat_function(fun=sin)
# 
# Seveda lahko dodamo še več elementov. Model "seštevanja" nam omogoča, da graf postopoma gradimo.
# 
# # Pripravimo graf s sinusno funkcijo, a brez informacije o koordinati x
# g <- ggplot(data.frame(x=c(-2*pi, 2*pi))) + stat_function(fun=sin)
# g + aes(x) # Za koordinato x vzamemo stolpec x v podatkih
# # Prikažimo še točke samo na izbranih koordinatah
# h <- g + geom_point(data=data.frame(x=seq(-2, 2, 0.25)*pi)) + aes(x, y=sin(x))
# # Če želimo iz programa narisati graf, uporabimo funkcijo print
# print(h)
# 
# S funkcijami xlab, ylab in ggtitle lahko grafu dodamo oznaki osi x in y oziroma naslov.
# 
# h + xlab("x") + ylab("y") + ggtitle("y = sin(x)")
# 
# S funkcijo theme lahko podamo več dodatnih parametrov glede izgleda grafa:
#   
#   Parameter panel.background nastavi izgled ozadja grafa - podamo mu funkcijo element_rect, tej pa dodatne nastavitve, npr fill za barvo ozadja. Barve lahko podamo z imeni ("white", "red", "green", ...), z RGB komponentami; (na primer "#00F16B"), ali s pomočjo funkcij rgb, hsv, hcl, ...
# Parametra panel.grid.major in panel.grid.minor nastavita izgled mrežnih črt - podamo jima funkcijo element_line, tej pa parametre color za barvo, size za širino oziroma linetype za tip črte ("solid", "dashed", "dotted", "dotdash" itd.). Če na konec imena parametra dodamo .x ali .y, nastavitev velja le za navpične oziroma vodoravne črte.
# Parameter plot.title nastavi izgled naslova grafa - podamo mu funkcijo element_text, tej pa parametre color za barvo, size za velikost itd.
# 
# Parametrov je še veliko več. Vse si lahko ogledate pri vgrajeni pomoči za ukaz theme.
# 
# g <- g + aes(x) + theme(panel.background=element_rect(fill="white"))
# g <- g + geom_point(data=data.frame(x=0, y=0), aes(x=x, y=y),
#                     color="gray", shape=1, size=5)
# g <- g + geom_point(data=data.frame(x=c(-3 * pi / 2, pi / 2), y=c(1, 1)),
#                     aes(x=x, y=y), color="red", shape=2, size=4)
# g <- g + geom_point(data=data.frame(x=c(-pi / 2, 3 * pi / 2), y=c(-1, -1)),
#                     aes(x=x, y=y), color="green", shape=6, size=4)
# 
# Narišimo še nekaj grafov iz obstoječih podatkov. Uporabili bomo razpredelnico MathAchieve iz knjižnice nlme s podatki o dosežkih učencev pri matematiki.
# 
# library(nlme)
# head(MathAchieve)
# 
# # vsi podatki
# ggplot(MathAchieve) + aes(x=SES, y=MathAch) + geom_point()
# # samo šola z oznako 1224
# p <- ggplot(MathAchieve %>% filter(School == 1224))
# p + aes(x=SES, y=MathAch) + geom_point()
# # pobarvajmo po spolu
# p + aes(x=SES, y=MathAch, color=Sex) + geom_point()
# # oznaka še po manjšini
# p + aes(x=SES, y=MathAch, color=Sex, shape=Minority) + geom_point()
# # razdelitev po spolu in manjšini
# p + aes(x=Sex, fill=Minority) + geom_bar()
# # tortni diagram po spolu
# p + aes(x=factor(1), fill=Sex) + geom_bar(width=1) +
#   coord_polar(theta="y") + xlab("") + ylab("")

starost15_64 <- StarostneStrukture_janos %>% filter(Age_group == "15-64") %>% filter(year == "1960")
starost15_64_graf <- ggplot(starost15_64, aes(x = percentage)) +
  geom_histogram()
#print(starost15_64_graf)

starost64 <- StarostneStrukture_janos %>% filter(Age_group == "65 in vec") %>% filter(year == "2015")
starost64_graf <- ggplot(starost64, aes(x = country, y = percentage)) +
  geom_point()
print(starost64_graf)


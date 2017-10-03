#' 
#' Piotr Sobczyk
#' szychtawdanych.pl
#' 
library(dplyr)
library(tidyr)
library(ggplot2)
library(ggthemes)

setwd("~/szychta_w_danych/erum2016/kody/")

load("../../wybory_parlamentarne_2015/mapa_gminy.Rdata")

#loading data
read.csv2("LUDN_3361_CTAB_20160818114416.csv") %>%
	select(Kod, Nazwa, ludnosc2015=w.wieku.poprodukcyjnym.ogółem.2015..osoba.) %>%
	rowwise %>% filter(! grepl("\\([4,5,8,9]\\)", Nazwa)) %>% ungroup %>% 
	arrange(desc(ludnosc2015)) %>% 
	mutate(cumulative=cumsum(ludnosc2015), topHalf=cumulative<sum(ludnosc2015, na.rm=T)/2,
				 Kod=round(Kod/10), topHalf=ifelse(is.na(topHalf), FALSE, topHalf)) -> postworking
postworking$topHalf[postworking$Nazwa=="Zielona Góra (2)"]=TRUE

read.csv2("LUDN_3361_CTAB_20160818114416.csv") %>% 
	select(Kod, Nazwa, ludnosc2015=w.wieku.przedprodukcyjnym.ogółem.2015..osoba.) %>%
	rowwise %>% filter(! grepl("\\([4,5,8,9]\\)", Nazwa)) %>% arrange(desc(ludnosc2015)) %>% 
	mutate(cumulative=cumsum(ludnosc2015), topHalf=cumulative<sum(ludnosc2015, na.rm=T)/2,
				 Kod=round(Kod/10), topHalf=ifelse(is.na(topHalf), FALSE, topHalf)) -> preworking
preworking$topHalf[preworking$Nazwa=="Zielona Góra (2)"]=TRUE

population=rbind(cbind(preworking, typ="Population < 18"),
							 cbind(postworking, typ="Population > 65")) %>%
	select(-Nazwa, -ludnosc2015, -cumulative)


population %>% inner_join(nazwy, by=c("Kod"="id")) -> population
population %>% inner_join(Gminy, by = c("kod"="id")) -> population

ggplot(population, aes(x = long, y = lat)) +
	geom_polygon(aes(group = group, fill=topHalf)) +
	theme_tufte() +theme_map() +
	facet_grid(.~typ)+
	ggtitle("Half of population lives in municipalities in blue") +
	theme(title = element_text(face="bold", size = 23, vjust = 0, hjust=0.2),
				strip.text=element_text(size=19), 
				strip.background=element_rect(fill = "gray80")) +
	guides(fill=FALSE)  -> p

ggsave("pre_vs_post_working_bad.jpg", p, width=13, height=7)

postworkingTopHalf=filter(postworking, !topHalf)$Kod
rbind(cbind(preworking, typ="Population < 18"), cbind(postworking, typ="Population > 65")) %>%
	mutate(colLevel=ifelse(topHalf, "a", "c"), 
				 colLevel=ifelse(typ=="Population < 18" & topHalf & Kod %in% postworkingTopHalf, "b", colLevel)) -> population
population$colLevel=factor(population$colLevel, levels = c("a", "b", "c"))
population %>% inner_join(nazwy, by=c("Kod"="id")) -> population
population %>% inner_join(Gminy, by = c("kod"="id")) -> population
population$typ=factor(population$typ, levels=rev(levels(population$typ)))

ggplot(population, aes(x = long, y = lat)) +
	geom_polygon(aes(group = group, fill=colLevel)) +
	theme_tufte() +theme_map() +
	facet_grid(.~typ)+
	ggtitle("Half of population lives in municipalities in blue") +
	theme(title = element_text(face="bold", size = 23, vjust = 0, hjust=0.2),
				strip.text=element_text(size=19), 
				strip.background=element_rect(fill = "gray80")) +
	scale_fill_manual(values = RColorBrewer::brewer.pal(5, "Paired")[c(2,1,5)]) + 
	guides(fill=FALSE)  -> p

ggsave("pre_vs_post_working.jpg", p, width=13, height=7)

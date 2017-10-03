#' 
#' Piotr Sobczyk
#' szychtawdanych.pl
#' 
#' Projekcje ludności w aglomeracjach
#' 

setwd("~/szychta_w_danych/erum2016/kody/")

library(xlsx)
library(dplyr)
library(ggplot2)
library(ggthemes)
library(RColorBrewer)
library(animation)

## powiaty ####
files=list.files("../../aglomeracje_projekcja_ludnosci_2050/Powiaty/", full.names = T)
all_powiaty = NULL

for(file in files){
	data = read.xlsx2(file, 1, startRow = 5)
	regName = read.xlsx2(file, 1, startRow = 3, endRow = 3, header = F)[1] %>% unlist
	names(data)[2:5] = c("type", "total", "male", "female")
	cbind(data.frame(year=rep(2013:2050, each=nrow(data)/38), 
									 region=regName, data)) %>%
		select(year, region, type, total, male, female) -> data
	
	all_powiaty = rbind(all_powiaty, data)
}

files=list.files("../../aglomeracje_projekcja_ludnosci_2050/",  pattern = "*.xls", full.names = T)

old_powiaty <- NULL
for(file in files){
	year1=as.numeric(gsub(".*([0-9]{4}).*", "\\1", file))
	temp = read.xlsx(file, sheetIndex = 1, startRow = 13, header = FALSE)
	temp[which(!temp$X1==toupper(temp$X1)),] -> temp
	names(temp)[1:4] = c("region", "total", "male", "female")
	cbind(year=year1, temp) %>%
		select(year, region, total, male, female) -> temp
	
	old_powiaty = rbind(old_powiaty, temp)
}

old_powiaty %>% mutate(total=as.numeric(as.character(total)),
											 region=trimws(as.character(region))) %>%
	filter(year<2013) -> old_powiaty
all_powiaty %>% mutate(total=as.numeric(as.character(total)),
											 region=trimws(as.character(region))) -> all_powiaty

all_powiaty %>% filter(type=="Ogółem") %>%
	select(-type) %>%
	rbind(old_powiaty) -> total_powiaty


total_powiaty %>% filter(year==2015) %>% arrange(desc(total))  %>% select(region) %>%
	cbind(kolor=rep(brewer.pal(12, name = "Set3"), 32)[1:380]) %>% inner_join(total_powiaty) %>%
	mutate(kolor=as.character(kolor)) %>% filter(grepl("m\\.", region)) %>%
	rowwise %>% mutate(region=tail(strsplit(region, " ")[[1]],1)) %>% ungroup -> total_powiaty

saveGIF(expr = {
	for (year0 in seq(2002, 2050, 1)){
		temp = total_powiaty %>% filter(year==year0) %>% top_n(10, total) %>%
			mutate(region=reorder(region, total)) %>% arrange(desc(total))
		colorVec = temp$kolor
		names(colorVec) = temp$region
		p <- ggplot(temp) +
			geom_bar(aes(x=region, y=total, fill=region), alpha=1, stat="identity") +
			geom_text(aes(x=region, y=1e6, label=region), size=5) +
			scale_y_continuous("Population", breaks=seq(5e5, 2e6, 5e5), 
												 labels=c("500K", "Million", "1.5 M", ""), limits = c(0, 1.8e6), expand = c(0,0)) +
			scale_x_discrete("") +
			scale_fill_manual(values=colorVec) +
			guides(fill=FALSE) +
			coord_flip() + ggtitle(year0) +theme_tufte() +
			theme(axis.text = element_text(size=20), title=element_text(size=22),
						axis.text.y = element_blank(), axis.ticks.y = element_blank(),
						panel.grid = element_line(colour = "black", size = 3),
						panel.grid.major.y = element_line(colour = "white", size = 4))
		plot(p)
	}
}, movie.name = "miasta.gif", interval=0.3, ani.width=800, ani.height=600)


total_powiaty %>% select(region, kolor, year, total) %>% tidyr::spread(year, total) -> plotData

ggplot(plotData %>% top_n(12, `2002`),aes(x=`2002`, y= `2050`)) +
	geom_point(aes(color=kolor), size=6.6) +
	ggrepel::geom_label_repel(aes(label=region), size=8) +
	scale_y_log10(breaks=1e5*2^(1:4)) + scale_x_log10(breaks=1e5*2^(1:4)) +
	guides(color=FALSE) +
	theme_tufte(base_size = 28)

ggsave("miasta_bad.jpg", width = 12, height = 9)
	

#' 
#' Piotr Sobczyk
#' 
#' ludność a bogactwo
#' 

setwd("~/szychta_w_danych/erum2016/kody/")
library(xlsx)
library(tidyr)
library(dplyr)
library(ggplot2)
library(ggthemes)
library(ggrepel)

ppp=read.xlsx2("indicator gdp_total_ppp.xlsx", 1, stringsAsFactors=FALSE)
pop=read.xlsx2("indicator gapminder population.xlsx", 1, stringsAsFactors=FALSE)
metadata <- read.csv("../../bieda_nierownosci/fb111153-76d5-40ce-aafa-b72ff64416f7_v2/Metadata_Country_fb111153-76d5-40ce-aafa-b72ff64416f7_v2.csv")

pop %>% gather(year, population, X1800:X2015) %>% 
	select(-c(`X.`: `X..9`), country=Total.population) -> pop

ppp %>% gather(year, ppp, X1800:X2013) %>% select(country=Total.GDP..PPP, year, ppp) -> ppp
ppp$country[ppp$country=="Czech Rep."] <- "Czech Republic"

inner_join(pop, ppp, by=c("country"="country", "year"="year")) %>%
	mutate(ppp=as.numeric(ppp), population=as.numeric(population), 
				 year=as.numeric(gsub("X", "", year)), pkbPerCapita=ppp/population) -> dane

dane %>% filter(year %in% c(2013))  %>%
	inner_join(metadata, by=c("country"="Country.Name"))  %>% 
	select(-SpecialNotes) -> dane2013

dane2013 %>% group_by(year) %>% arrange(pkbPerCapita) %>% 
	mutate(cumulativeppp=cumsum(ppp),
				 cumulativepopulation=cumsum(population),
				 cumulativeppp=cumulativeppp/max(cumulativeppp, na.rm = T),
				 cumulativepopulation=cumulativepopulation/max(cumulativepopulation,na.rm = T)) %>%
	filter(!is.na(cumulativeppp)) %>%
	mutate(aveppp=(cumulativeppp+c(0,head(cumulativeppp,-1)))/2,
				 avepop=(cumulativepopulation+c(0,head(cumulativepopulation,-1)))/2,
				 minppp=c(0,head(cumulativeppp,-1)),
				 minpop=c(0,head(cumulativepopulation,-1))) -> danePlot

countriesToPlot=c("Ethiopia", "Poland", "India", "United States", "China", "Mexico",
									"United Arab Emirates")
danePlot %>% filter(country %in% countriesToPlot) -> labelPlotData

ggplot(danePlot, aes(x=cumulativepopulation, y=cumulativeppp)) +
	geom_segment(aes(xend=cumulativepopulation, yend=cumulativeppp,
									 x=minpop, y=minppp,
									 color=Region, group=year), size=2, lineend="square") +
	geom_label_repel(data = labelPlotData, aes(x=avepop, y=aveppp, label=country)) +
	scale_x_continuous("Percent of world population", 
										 breaks = seq(0, 1, .25),
										 labels = paste0(seq(0, 100, 25), "%"), 
										 expand = c(0,0.01)) +
	scale_y_continuous("Percent of total GDP", 
										 breaks = seq(0, 1, .25),
										 labels = paste0(seq(0, 100, 25), "%"),
										 expand = c(0, 0.01)) +
	theme_tufte(base_size = 19) +
	ggtitle("Wealth inequalities in 2013") +
	theme(title=element_text(size=25)) -> p

p +	geom_segment(aes(x=0, xend=1, y=0, yend=1), linetype="dotted", color="gray80") +
	geom_segment(aes(x=0.3, xend=0.8, y=0.3, yend=0.8), color="gray20",
								 arrow=arrow(angle = 25, length = unit(0.4, "cm"))) +
	annotate("text", x=0.42, y=0.44, angle = 31, hjust=0, vjust=0, size=4, 
					 label="Countries arranged by GDP per capita") -> p2

p2 +	geom_segment(aes(x=0, xend=0.508, y=0.1834, yend=0.1834), linetype="dotted", color="gray20",
								 arrow=arrow(angle = 25, ends = "both", length = unit(0.4, "cm"))) +
	annotate("text", x=0.25, y=0.25, size=4.5, label="Half of the world population\nis poorer than Chinese") +
	geom_segment(aes(x=0.508, xend=0.508, y=0, yend=0.1834), linetype="dotted", color="gray20", 
							 arrow=arrow(angle = 25, ends = "both", length = unit(0.4, "cm"))) +
	annotate("text", x=0.66, y=0.1, size=4.5, label="Half of the world population\n produces 18% of global GDP") -> p3

ggsave("gdp_population_regions.jpg", plot =  p3, width=12, height=6)


ggplot(danePlot, aes(y=pkbPerCapita, x=population, group=year)) +
	geom_point(aes(size=ppp,color=Region)) +
	scale_x_log10("Population") + scale_y_log10("GDP per capita") +
	scale_size_continuous("GDP") +
	geom_label_repel(data = labelPlotData, aes(y=pkbPerCapita, x=population, label=country)) +
	theme_tufte(base_size = 19) +
	ggtitle("Wealth inequalities in 2013") +
	theme(title=element_text(size=25)) -> p4

ggsave("gdp_population_regions_bad.jpg", plot =  p4, width=12, height=6)

# by regions barplot
danePlot %>% group_by(Region) %>% summarise(population=sum(population), ppp=sum(ppp)) %>%
	mutate(pkbPerCapita=ppp/population, population=population/sum(population), ppp=ppp/sum(ppp)) -> danePlot2

danePlot2 %>% gather(type, value, -Region, -pkbPerCapita) -> danePlot2
danePlot2$Region=sapply(strsplit(as.character(danePlot2$Region), "&"), paste0, collapse="\n& ")

danePlot2 %>% arrange(pkbPerCapita) -> danePlot2

danePlot2$Region=factor(danePlot2$Region, levels=reorder(danePlot2$Region, X = danePlot2$pkbPerCapita)[seq(1,13,2)])

ggplot(danePlot2, aes(x=Region)) +
	geom_bar(aes(y=value, fill=type, group=type), position="dodge", stat="identity") +
	scale_y_continuous("%") +
	scale_x_discrete("") +
	scale_fill_discrete(name="", labels=c("Population", "GDP")) +
	theme_tufte(base_size = 19) +
	ggtitle("Wealth inequalities in 2013") +
	theme(title=element_text(size=25)) -> p5

ggsave("gdp_population_regions_only_bad.jpg", plot =  p5, width=12, height=6)

## small plot
dane %>% filter(year %in% c(1850, 1900, 2000, 2013))  %>%
	inner_join(metadata, by=c("country"="Country.Name"))  %>% 
	select(-SpecialNotes) -> daneSmallPlot

daneSmallPlot %>% group_by(year) %>% arrange(pkbPerCapita) %>% 
	mutate(cumulativeppp=cumsum(ppp),
				 cumulativepopulation=cumsum(population),
				 cumulativeppp=cumulativeppp/max(cumulativeppp, na.rm = T),
				 cumulativepopulation=cumulativepopulation/max(cumulativepopulation,na.rm = T)) %>%
	filter(!is.na(cumulativeppp)) %>%
	mutate(aveppp=(cumulativeppp+c(0,head(cumulativeppp,-1)))/2,
				 avepop=(cumulativepopulation+c(0,head(cumulativepopulation,-1)))/2,
				 minppp=c(0,head(cumulativeppp,-1)),
				 minpop=c(0,head(cumulativepopulation,-1))) %>% ungroup %>%
	mutate(year=as.factor(year)) -> daneSmallPlot

ggplot(daneSmallPlot, aes(x=cumulativepopulation, y=cumulativeppp)) +
	geom_segment(aes(xend=cumulativepopulation, yend=cumulativeppp,
									 x=minpop, y=minppp,
									 color=year, group=year), size=1, lineend="square") +
	geom_label(data=daneSmallPlot %>% filter(minpop<0.5, cumulativepopulation>0.5), 
									 aes(label=year, color=year, x=c(0.45, 0.25, 0.25, 0.45),
									 		y=c(0.65, 0.65, 0.45, 0.45)), size=5) +
	scale_color_wsj() +
	scale_x_continuous("Population", 
										 breaks = NULL,
										 expand = c(0,0.01)) +
	scale_y_continuous("GDP", 
										 breaks = NULL,
										 expand = c(0, 0.01)) +
	guides(color=FALSE) +
	theme_tufte(base_size = 19) -> subplot

library(grid)
vp <- viewport(width = 0.3, height = 0.3, x = 1,
							 y = 0.1, just = c("right", "bottom"))

full <- function() {
	print(p3)
	# theme_set(theme_bw(base_size = 8))
	print(subplot, vp = vp)
	# theme_set(theme_bw())
}

p6 <- full()

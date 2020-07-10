# Introduction ----
# Creating plots of LPI population frequency by biome
# http://www.livingplanetindex.org/home/index
# 7-9-20
# Author: Molly Johnson, Bradley University/Oregon State University

# Libraries ----
library(tidyr)
library(readr)
library(dplyr)
library(ggplot2)
library(RCurl)

# Functions ----

# provides consistent plot aesthetics for ggplot2
theme.LPI <- function(){
  theme_bw() +
  theme(axis.text.x=element_text(size=12, angle=45, vjust=1, hjust=1),
        axis.text.y=element_text(size=12),axis.title.x=element_text(size=14, face="plain"),
        axis.title.y=element_text(size=14, face="plain"),             
        panel.grid.major.x=element_blank(),
        panel.grid.minor.x=element_blank(),
        panel.grid.minor.y=element_blank(),
        panel.grid.major.y=element_blank(),
        plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), units = , "cm"),
        plot.title = element_text(size=20, vjust=1, hjust=0.5),
        legend.text = element_text(size=12, face="italic"),
        legend.title = element_blank(),
        legend.position=c(0.9, 0.9))
}

# Set Working Directory ----
setwd("C:/Users/mljoh/github/ecologyDataScienceCourses/CC-etiquette")

# Load Data ----
LPI <-read.csv("C:/Users/mljoh/github/ecologyDataScienceCourses/CC-etiquette/LPIdata_CC.csv")

# Clean Data ----

# long format, so abundance records in one col
LPI_long <-gather(Data,"year","abundance",9:53)

# force years to numeric
LPI_long$year <- parse_number(LPI_long$year)

# check names and make consistent case
names(LPI_long)
names(LPI_long) <- tolower(names(LPI_long))

# force abundance to numeric
LPI_long$abundance <- as.numeric(LPI_long$abundance)

# create summary data frame of population "n"
LPI_biome_summary<- LPI_long %>%
  group_by(biome) %>%
  summarise(population = n())

# check data frame has formed correctly
head(LPI_biome_summary)

# Create plots of population frequency per biome ----

# bar graph
type = "bar"
biome_barplot <- ggplot(LPI_long, aes(biome, color = biome)) + {
  if(type=="bar")
    geom_bar()
  else 
    geom_point(stat="count")
  } +
	theme.LPI() +
  ylab("Number of populations") +
  xlab("Biome") +
	theme(legend.position = "none")

# dot plot
type = "point"
biome_dotplot <- ggplot(LPI_long, aes(biome, color = biome)) + {
  if(type == "bar")
    geom_bar()
  else
    geom_point(stat = "count")
  } +
	theme.LPI() +
  ylab("Number of populations") +
  xlab("Biome") +
	theme(legend.position = "none")

# compare plots
biome_barplot
biome_dotplot

# bar graph looks better, save to pdf
type = "bar"
pdf(file="img/biome_freq_barplot.pdf",  width = 13.33, height = 26.66)
ggplot(LPI_long, aes(biome, color = biome)) + {
  if(type=="bar")
    geom_bar()
  else
    geom_point(stat="count")
  } +
	ThemeForLPI() +
  ylab("Number of populations") +
  xlab("Biome") +
	theme(legend.position = "none") 
dev.off()
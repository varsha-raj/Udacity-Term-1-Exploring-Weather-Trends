
##load packages
library(dplyr)
library(zoo)
library(tidyr)
library(data.table)
library(lubridate)

## set global environment
Sys.setenv(TZ='EST')
options(error=recover)
options(warn=-1)
options(stringsAsFactors=FALSE)
options(scipen=999)

##Read city data
## extract Philadelphia and Bangalore data from city data 
city_data <- read.csv('city_results.csv', header = TRUE)

local_data <-city_data %>%

filter(country=='United States' & city == 'Philadelphia' & year >=1750)

india_data <- city_data %>%

filter(country=='India' & city=='Bangalore')

## read global data and extract data prior to 2013

global_data <- read.csv('global_results.csv', header = TRUE)

global_data_new <- global_data %>%

filter(year <=2013)

## 5 year moving average calcs

local_data_5yr_MA <- local_data %>%

na.omit() %>% ## to remove the NA value in the year 1780

mutate(temp_5_MA = rollmean(avg_temp, 5, align='right', fill=NA)) %>%

na.omit()

write.csv(local_data_5yr_MA, 'local_data_5yr_MA.csv', row.names= FALSE, quote= FALSE)

india_data_5yr_MA <- india_data %>%

na.omit() %>% ## to remove the NA value in the year 1780

mutate(temp_5_MA = rollmean(avg_temp, 5, align='right', fill=NA)) %>%

na.omit()

write.csv(india_data_5yr_MA, 'india_data_5yr_MA.csv', row.names= FALSE, quote= FALSE)


global_data_5yr_MA <- global_data_new %>%

na.omit() %>%

mutate(temp_5_MA = rollmean(avg_temp, 5, align='right', fill=NA)) %>%

na.omit()

write.csv(global_data_5yr_MA, 'global_data_5yr_MA.csv', row.names= FALSE, quote= FALSE)

## load packages to plot graphs
library("ggplot2")
library("grid")
library("scales")
library(ggrepel)
library(directlabels)
library(ggalt)
options(warn=-1)

p1 <- ggplot()

p2 <- p1 + geom_line(data= local_data_5yr_MA, aes(x= year, y=temp_5_MA, color=''), 

	linetype='solid', size=0.5)

p3 <- p2 + geom_line(data=global_data_5yr_MA, aes(x=year, y=temp_5_MA, linetype=''),
	color='black', size=0.5)	

p4 <- p3 + scale_color_manual(values=c("#56B4E9"), label='Philadelphia, PA\nAverage Temperature\n(1750-2013)', name="")

p5 <- p4 + scale_linetype_manual(values=c('solid'), label='Global Average Temperature\n(1750-2013)', name="")

p6 <- p5 + geom_line(data=india_data_5yr_MA, aes(x=year, y=temp_5_MA, size=''), 

	linetype='solid', color="#009E73")

p7 <- p6 + scale_size_manual(values=c(0.5), label='Bangalore, India\nAverage Temperature\n(1796-2013)', name="")

p8 <- p7 + scale_y_continuous(breaks=seq(0,30,5), minor_breaks=waiver(), limits=c(0,30), expand=c(0,0))+

scale_x_continuous(breaks=seq(1750,2013,20), minor_breaks = waiver(), expand =c(0,0)) + coord_cartesian(xlim=c(1750,2015))

p9 <- p8 + labs(x='\nYear', y = "Average Temperature (degree Celsius)")

p10 <- p9 + annotate("text", x=1805, y=9.8, label='Missing Value:Year 1780', size=1.5, color='red') +

annotate("text", x=1850, y=23.5, label='Missing Values:Year 1808-1812; 1863-1864', size=1.5, color='red') #+

#annotate("text", x=1900, y=24.5, label='Missing Value:Year 1863-1864', size=1.5, color='red')


windowsFonts(F = windowsFont('Times New Roman'))

 p11 <- p10 + theme(axis.text.x=element_text(size=8, family="F"),
                                    axis.text.y=element_text(size=8, family="F"),
                                    axis.title.x=element_text(size=8, family="F"),
                                    axis.title.y=element_text(size=8, family="F"),
                                    legend.title=element_text(size=8, family="F"),
                                    legend.text=element_text(size=8, family="F"),
                                    plot.title=element_text(size=8, hjust = 0.5, face='bold',family="F"),
                                    #legend.text=element_blank(),
                                    panel.background = element_rect(fill = "white", colour = NA),
                                    panel.border = element_rect(fill = NA, color= 'grey20', size=0.4),
                                    legend.key.size = unit(0, "cm"),
                                    legend.key = element_rect(colour = NA, fill=NA),
                                    panel.grid.major = element_line(size=0.2, colour = 'gray80'),
                                    #panel.grid.major = element_blank(),
                                    panel.grid.minor = element_line(size=0.05, colour = 'gray80'),
                                    axis.ticks = element_line(size=0.2, colour='gray60'),
                                    #panel.grid.minor = element_blank(),
                                    legend.key.height=unit(0.5,"line"),
                                    legend.key.width=unit(1,"line"),
                                    #legend.spacing.y=unit(-0.5, 'cm'))
                                    legend.margin = unit(0.1,'cm'))


p12 <- p11 +

guides(size= guide_legend(order=1), color=guide_legend(order=2))

gt <- ggplot_gtable(ggplot_build(p12))
             #gt$layout$clip[gt$layout$name=='panel'] <- 'off'
             grid.draw(gt)
             tiff('weather_trends_5yr.tiff', units='in', width=6.5, height=4.5, res=250); plot(gt);dev.off()


## Regression Analysis 

## The results read from here and presneted in the pdf document

## global data regression

global_linear_regress <- lm(global_data_new$avg_temp ~ global_data_new$year)

## make sure only complete values are used 
local_data_new <- local_data %>%

na.omit()

##local data regression

local_linear_regress <- lm(local_data_new$avg_temp ~ local_data_new$year)

## make sure only complete values are used 
india_data_new <- india_data %>%

na.omit()

## Bangalore, India data regression
india_linear_regress <- lm(india_data_new$avg_temp ~ india_data_new$year)

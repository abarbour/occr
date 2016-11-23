#!/usr/bin/env Rscript --no-save

###
#	Dly1012d.R
#	/Users/abarbour/survey.processing/development/R/packages/occr/data-raw
#	Created by 
#		/Users/abarbour/bin/ropen ( v. 2.6.0 )
#	on 
#		2016:312 (07-November)
###

## local functions
#try(source('funcs.R'))

## libs

library(maps)
library(raster)
library(rgdal)
library(data.table)
library(dtplyr)
library(readxl)
# loads core tidy packages:  ggplot2, tibble, tidyr, readr, purrr, and dplyr
library(tidyverse)
#tidyverse_conflicts()
#tidyverse_update(TRUE)

## local/github libs
# devtools::install_github("abarbour/kook")
#library(kook)
Set1 <- c("#E41A1C","#377EB8","#4DAF4A","#984EA3","#FF7F00","#FFFF33","#A65628","#F781BF","#999999")
#Set1l <- brew.set1(TRUE)
Dark2 <- c("#1B9E77","#D95F02","#7570B3","#E7298A","#66A61E","#E6AB02","#A6761D","#666666")
#Dark2l <- brew.dark2(TRUE)
#Dark2ll <- brew.dark2(TRUE,TRUE)

#+++++++++++

shake <- FALSE
redo <- FALSE

redo.raw <- FALSE
if (!exists("dly.raw") | redo.raw){
	#Arbuckle AOI Wells - 1012D Reported Volumes	
	#This file contains the weekly reports of daily
	#volumes of all Arbuckle disposal wells in the Area
	#of Interest for Triggered Seismicity, submitted by
	#operators via the OCC efile system. Filing of the
	#daily data for these wells began on 3/27/2016	
	#Daily	Jim Marlatt 405-522-2758	ZIP
	read_excel("Dly1012d.xlsx") -> dly.raw
}

# Colorgorical
set.seed(12654)
cty.pal <- sample(c("#21f0b6", "#104b6d", "#a7d64e", "#4220f0", "#73f02e", "#b70d61", "#8ae1f9", "#315c31", "#fd6ca0", "#04a38f", "#1f3e9e", "#d5d0fa", "#7b038e", "#1c9820", "#c551dc", "#3d8bb7", "#6e334f", "#7f73ed", "#f1d438", "#c00018", "#e9bf98", "#6e3901", "#fe8f06", "#ab7673", "#a37e1a", "#fa1bfc"))
n.cty.pal <- length(cty.pal)

redo.cty <- FALSE
if (!exists("counties") | redo.cty){
	counties <- readOGR("tl_2010_40_county10",layer="tl_2010_40_county10")
	#OGR data source with driver: ESRI Shapefile
	#Source: "tl_2010_40_county10", layer: "tl_2010_40_county10"
	#with 77 features
	#It has 17 fields
	#
	# Object of class SpatialPolygonsDataFrame
	# Coordinates:
	#          min       max
	# x -103.00245 -94.43066
	# y   33.61579  37.00231
	# Is projected: FALSE
	# proj4string :
	# [+proj=longlat +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0]
	# Data attributes:
	
	#   STATEFP10          COUNTYFP10         COUNTYNS10          GEOID10
	#  Length:77          Length:77          Length:77          Length:77
	#  Class :character   Class :character   Class :character   Class :character
	#  Mode  :character   Mode  :character   Mode  :character   Mode  :character
	
	#     NAME10           NAMELSAD10           LSAD10           CLASSFP10
	#  Length:77          Length:77          Length:77          Length:77
	#  Class :character   Class :character   Class :character   Class :character
	#  Mode  :character   Mode  :character   Mode  :character   Mode  :character
	
	#    MTFCC10            CSAFP10            CBSAFP10          METDIVFP10
	#  Length:77          Length:77          Length:77          Length:77
	#  Class :character   Class :character   Class :character   Class :character
	#  Mode  :character   Mode  :character   Mode  :character   Mode  :character
	
	#   FUNCSTAT10           ALAND10             AWATER10          INTPTLAT10
	#  Length:77          Min.   :9.611e+08   Min.   :   952238   Length:77
	#  Class :character   1st Qu.:1.656e+09   1st Qu.: 13695217   Class :character
	#  Mode  :character   Median :2.079e+09   Median : 27560913   Mode  :character
	#                     Mean   :2.307e+09   Mean   : 43859903
	#                     3rd Qu.:2.592e+09   3rd Qu.: 50982330
	#                     Max.   :5.818e+09   Max.   :242915467
	
	#   INTPTLON10
	#  Length:77
	#  Class :character
	#  Mode  :character
	counties$County <- factor(counties$NAME10)
}

#+++++++++++

fix.names <- function(x){
	gsub("\r\n",".",x)
}
unique.warn <- function(.x.){
	x.u <- unique(.x.)
	if (length(x.u)>1) warning("Taking first of multiple-values: ", paste(x.u, collapse=" -- "))
	return(x.u[1])
}



if (!exists("All.dly") | redo | redo.raw){

	dplyr::select(dly.raw, -DirArea) -> dly.new
	names(dly.new) <- fix.names(names(dly.new))

	dly.new %>% 
	dplyr::mutate(., 
		Report.Date = as.Date(Report.Date),
		API = factor(trimws(gsub("^'3","3", API))),
		Operator.Number = factor(Operator.Number),
		Operator.Name = factor(Operator.Name),
		Well.Number = factor(trimws(gsub("^'","", Well.Number)))
	) %>% arrange(., API, Report.Date) %>% group_by(., API) -> All.dly
}

redo.quakes <- TRUE
date.filt1 <- "2015-04-01"
if (!exists("Eqs") | !exists("eqhull") | redo | redo.quakes){
	read_csv("anss/anss_catalog_M3.csv", col_names=TRUE) %>%
		dplyr::mutate(., Date = as.Date(DateTime)) %>%
		dplyr::filter(., Date >= as.Date(date.filt1)) -> Eqs
	coordinates(Eqs) <- ~ Longitude + Latitude
	proj4string(Eqs) <- "+proj=longlat +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0"

	Eqlocs <- coordinates(Eqs)
	eqhull.inds <- chull(Eqlocs)
	eqhull.inds <- c(eqhull.inds, eqhull.inds[1])
	eqhull <- Eqlocs[eqhull.inds, ]
}

if (!exists('anss') | !exists('anss.cty') | redo | redo.quakes){
	read_csv("anss/anss_catalog.csv", col_names=TRUE) %>%
		dplyr::mutate(., Date = as.Date(DateTime)) -> anss
		#%>% dplyr::filter(., Date >= as.Date(date.filt1)) 
	coordinates(anss) <- ~ Longitude + Latitude
	proj4string(anss) <- "+proj=longlat +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0"
	
	ovr <- over(anss, counties)
	cbind(as.data.frame(anss), ovr) -> anss.cty
	coordinates(anss.cty) <- ~ Longitude + Latitude
	proj4string(anss.cty) <- "+proj=longlat +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0"
	
}

redo.wells <- TRUE
if (!exists("All.wells") | redo | redo.raw | redo.wells){
	bad.coords <- c("3508123432","3508521170","3511921090")
	All.dly %>% as.data.frame %>% tbl_df %>%
	dplyr::filter(., !(API %in% bad.coords)) %>%
	group_by(., API) %>% 
	summarize(.,
		Name = unique.warn(Well.Name), 
		Number = unique.warn(Well.Number), 
		Operator = unique.warn(Operator.Name),
		Lon = unique.warn(Longitude),
		Lat = unique.warn(Latitude), 
		First.data = min(Report.Date), 
		Total.volume.bbl=sum(Volume.BPD, na.rm=TRUE),
		Max.volume.bbl=max(diff(Volume.BPD), na.rm=TRUE),
		N = n()) %>%
	dplyr::arrange(., Total.volume.bbl) -> All.wells
	coordinates(All.wells) <- ~ Lon + Lat
	proj4string(All.wells) <- "+proj=longlat +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0"
}

if (!exists("All.wells.cty") | redo | redo.raw | redo.cty | redo.wells){
	ovr <- over(All.wells, counties)
	cbind(as.data.frame(All.wells), ovr) %>% 
		dplyr::arrange(., -Total.volume.bbl) -> All.wells.cty
	coordinates(All.wells.cty) <- ~ Lon + Lat
	proj4string(All.wells.cty) <- "+proj=longlat +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0"
}

# Save for package ...

Dly1012d <- tbl_df(All.dly) # data.table -> tibble
save(Dly1012d, file='Dly1012d.rda')

Wells1012d <- All.wells.cty # SpatialPointsDataFrame
save(Wells1012d, file='Wells1012d.rda')

OKcounties <- counties #"SpatialPolygonsDataFrame"
save(OKcounties, file='OKcounties.rda')

OKquakes <- anss.cty #"SpatialPointsDataFrame"
save(OKquakes, file='OKquakes.rda')

#library(occr)
#pltdf <- Dly1012d
dcast(setDT(All.dly), Report.Date ~ API, value.var=c('Volume.BPD'), drop=FALSE, fun.aggregate=mean) %>% as.data.frame -> Dly.api

#matplot(Dly.api[,1], Dly.api[,-1], type='l')

events <- as.Date(c('2016-03-27',"2016-09-03",'2016-11-02','2016-11-07'))
evt.labels <- c("Daily filing regulation begins", expression(bold(M)*5.8), expression(bold(M)*4.5), expression(bold(M)*5.0))

Dly.api.filt <- filter(Dly.api, Report.Date >= as.Date('2014-01-01'))
xl <- range(c(Dly.api.filt$Report.Date, events)) + c(-7,7)

pltdf <- All.wells.cty
	
CTY <- function(alph=0.2){
	plot(counties, add=TRUE, border='grey', col=adjustcolor(cty.pal[((as.numeric(counties$County)-1) %% n.cty.pal)+1], alpha=alph))
	plot(extent(pltdf), add=TRUE, lty=2)
	#lines(eqhull, lty=2)
}

message("Plotting... See also is-oig/Mapping/Oklahoma/map_injection.R")

FIG <- function(){
	layout(matrix(c(1,1, 2,2, 3,3, 4,4, 5,6, 7,8, 5,6, 7,8), 4, byrow=FALSE), heights=c(1,1.5,1.5,1))

	par(las=1, cex=0.85)

	par(mar=10*c(0.01,0.01,0.01,0.01), lend='square')
	defc <- 0.15
	pltdf.bb <- bbox(pltdf)
	cexs <- scale(pltdf$Total.volume.bbl, center=FALSE)+defc
	cexs2 <- scale(pltdf$Max.volume.bbl, center=FALSE)+defc
	cexs3 <- scale((10**(Eqs$Magnitude))**(1/2.2), center=FALSE)*defc + defc*1.5
	cexs3 <- scale(Eqs$Magnitude**3, center=FALSE) - 0.5 + defc
	bgs <- adjustcolor(cty.pal[((as.numeric(pltdf$County)-1) %% n.cty.pal)+1], alpha=0.5)
	cols <- ifelse(bgs==defc, NA, "black")

	.plt_main <- function(lbl="", y = pltdf.bb['Lat','max'], ...){
		pu <- par('usr')
		x <- mean(pu[1:2])
		#axis(1, line=-3)
		#axis(2, line=-3)
		text(x, y, lbl, font=2, pos=3, ...)
		maps::map.scale(ratio=FALSE, relwidth=0.191, cex=0.75, tcl=-0.2)
	}

	plot(pltdf, col=NA)
	CTY(0.4)
	plot(pltdf, 
		bg=bgs, col=cols,
		pch=22,
		cex=6*defc, lwd=0.7, 
		add=TRUE)
	.plt_main("Active AOI Injectors filing Daily")

	plot(pltdf, col=NA)
	CTY()
	plot(pltdf, 
		bg=bgs, col=cols,
		pch=22, 
		cex=cexs, lwd=0.7,
		add=TRUE)
	.plt_main("Total volume since first data")

	plot(pltdf, col=NA)
	CTY()
	plot(Eqs, 
		#bg=bgs, col=cols, 
		pch=16,
		col=adjustcolor('black',alpha=0.5),
		cex=cexs3,
		add=TRUE)
	.plt_main(sprintf("ANSS  M>=2, Z<=10, t>=%s", date.filt1))

	plot(pltdf, col=NA)
	CTY()
	plot(pltdf, 
		bg=bgs, col=cols,
		pch=22, 
		cex=cexs2, lwd=0.7,
		add=TRUE)
	.plt_main("Peak daily rate since first data")

	par(mar=c(2,4.5,1,1), mgp=c(1.9,0.6,0),tcl=-0.3)

	x <- Dly.api.filt[,1]
	Y <- Dly.api.filt[,-1]
	y <- rowSums(Y, na.rm=TRUE)/1e6
	xi <- which(x >= as.Date(date.filt1) & x < as.Date('2016-09-03')) #events[1])
	m <- lm(y[xi] ~ x[xi])

	plot.new()

	plot(x, y, col=NA,
		ylim=c(0, max(y,na.rm=TRUE)*1.25),
		xlim=xl, xaxs='i',
		type='l',ylab=expression(10^6~~"bbl/dy"), main="Total daily-injection (AOIs only)")
	abline(v=events, col=Set1[c(2,3,3,3)]) #, lty=c(1,5,5,2))
	stages.st <- as.Date(paste0("2016-",c("03-07","03-29","04-18","05-08")))
	stages.en <- as.Date(paste0("2016-",c("03-28","04-17","05-07","05-28")))
	stg.st <- c(1.8, 1.8*0.75, (1.8*0.75)*0.75, ((1.8*0.75)*0.75)*0.75)
	stg.en <- c(1.8*0.75, (1.8*0.75)*0.75, ((1.8*0.75)*0.75)*0.75, (((1.8*0.75)*0.75)*0.75)*0.75)
	#segments(stages.st, stg.st, stages.en, stg.en, lty=2)
	text(events, 3.7 - 0.63*(seq_along(events)), evt.labels, col=Set1[c(2,3,3,3)], pos=2, offset=0.2, cex=0.85)
	abline(m, col=Set1[1], lty=4)
	lines(x, y, lwd=1.2)
	text(as.Date('2014-06-01'), 2.7, sprintf("%s bbl/dy",round(coef(m)[2]*1e6,-2)), srt=-5, col=Set1[1], cex=0.8)

	# plot(x, cumsum(rowSums(Y, na.rm=TRUE))/1e9,
	# 	xlim=xl, xaxs='i',
	# 	type='s',ylab=expression(10^9~~"bbl"))
	# #segments(as.Date('2015-01-01'), 0, as.Date('2016-07-01'), 1.3, lty=2)
	# abline(v=events, col=Set1[c(2,3,3,3)], lty=c(1,5,5,2))
	# mtext("Reported Cumulative Volume", line=-2, adj=0.03)

	par(mar=c(2,4.5,0.2,1))
	y2 <- apply(Y, 1, function(.x.) length(na.omit(.x.)))
	plot(x, y2, col=NA,
		xlim=xl, xaxs='i', ylab="")
	abline(v=events, col=Set1[c(2,3,3,3)]) #, lty=c(1,5,5,2))
	lines(x, y2, type='s', lwd=1.2)
	text(as.Date('2014-01-01'), 550, "Number of injectors\nin database", cex=0.8, pos=4)
	
	plot.new()

}

#+++++++++++

grDevices::png(filename="OCC_daily_volumes_AOI.png", width=11, height=5.0, units='in', res=300, bg='white')
try(FIG())
dev.off()

###

message("Update description sub-version to  ", strftime(Sys.time(),"%Y%m%d", tz='UTC'))

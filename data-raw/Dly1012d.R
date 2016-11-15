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
if (!exists("Eqs") | !exists("eqhull") | redo | redo.quakes){
	read_csv("anss/anss_catalog_M3.csv", col_names=TRUE) %>%
		dplyr::mutate(., Date = as.Date(DateTime)) %>%
		dplyr::filter(., Date >= as.Date("2015-05-01")) -> Eqs
	coordinates(Eqs) <- ~ Longitude + Latitude
	proj4string(Eqs) <- "+proj=longlat +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0"

	Eqlocs <- coordinates(Eqs)
	eqhull.inds <- chull(Eqlocs)
	eqhull.inds <- c(eqhull.inds, eqhull.inds[1])
	eqhull <- Eqlocs[eqhull.inds, ]
}

redo.wells <- FALSE
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
		Max.volume.bbl=max(Volume.BPD, na.rm=TRUE),
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

dcast(setDT(All.dly), Report.Date ~ API, value.var=c('Volume.BPD'), drop=FALSE, fun.aggregate=mean) %>% as.data.frame -> Dly.api

#matplot(Dly.api[,1], Dly.api[,-1], type='l')

events <- as.Date(c('2016-03-27',"2016-09-03",'2016-11-02','2016-11-07'))
evt.labels <- c("AOI filing requirement begins", expression(bold(M)*5.8), expression(bold(M)*4.5), expression(bold(M)*5.0))

Dly.api.filt <- filter(Dly.api, Report.Date >= as.Date('2014-01-01'))
xl <- range(c(Dly.api.filt$Report.Date, events)) + c(-7,7)

layout(matrix(c(1,2,3,4,5)), heights=c(1,1,1,1,1))

par(las=1, cex=0.9)

par(mar=20*c(0.01,0.3,0.01,0.3))
defc <- 0.15
pltdf <- All.wells.cty
cexs <- scale(pltdf$Total.volume.bbl, center=FALSE)+defc
cexs2 <- scale(pltdf$Max.volume.bbl, center=FALSE)+defc
cexs3 <- scale((10**round(Eqs$Magnitude))**(1/2), center=FALSE)*defc
bgs <- adjustcolor(as.numeric(pltdf$County), alpha=0.5)
cols <- ifelse(bgs==defc, NA, "black")

plot(pltdf, col=NA)
plot(counties, add=TRUE, border='grey', col=adjustcolor(as.numeric(counties$County), alpha=0.2))
plot(extent(pltdf), add=TRUE, lty=2)
#lines(eqhull, lty=2)
plot(pltdf, 
	bg=bgs, col=cols,
	pch=22, 
	cex=cexs,
	add=TRUE)
mtext("Total\nvolume", side=1, adj=0.15, line=-4, font=2)

plot(pltdf, col=NA)
plot(counties, add=TRUE, border='grey', col=adjustcolor(as.numeric(counties$County), alpha=0.2))
plot(extent(pltdf), add=TRUE, lty=2)
#lines(eqhull, lty=2)
plot(pltdf, 
	bg=bgs, col=cols,
	pch=22, 
	cex=cexs2,
	add=TRUE)
mtext("Peak\ndaily\nrate", side=1, adj=0.15, line=-4, font=2)

plot(Eqs, col=NA)
plot(counties, add=TRUE, border='grey', col=adjustcolor(as.numeric(counties$County), alpha=0.2))
plot(extent(pltdf), add=TRUE, lty=2)
#lines(eqhull, lty=2)
plot(Eqs, 
	#bg=bgs, col=cols, 
	pch=1, 
	cex=cexs3,
	add=TRUE)
mtext("ANSS\nM>=2\nZ<=10", side=1, adj=0.15, line=-4, font=2)


x <- Dly.api.filt[,1]
Y <- Dly.api.filt[,-1]
y <- rowSums(Y, na.rm=TRUE)/1e6
xi <- which(x >= as.Date('2015-05-01')) #events[1])
m <- lm(y[xi] ~ x[xi])

par(mar=c(2,4.5,1,1))
plot(x, y,
	ylim=c(0, max(y,na.rm=TRUE)),
	xlim=xl, xaxs='i',
	type='l',ylab=expression(10^6~~"bbl/dy"))
abline(v=events, col=Set1[c(2,3,3,3)], lty=c(1,5,5,2))
stages.st <- as.Date(paste0("2016-",c("03-07","03-29","04-18","05-08")))
stages.en <- as.Date(paste0("2016-",c("03-28","04-17","05-07","05-28")))
stg.st <- c(1.8, 1.8*0.75, (1.8*0.75)*0.75, ((1.8*0.75)*0.75)*0.75)
stg.en <- c(1.8*0.75, (1.8*0.75)*0.75, ((1.8*0.75)*0.75)*0.75, (((1.8*0.75)*0.75)*0.75)*0.75)
#segments(stages.st, stg.st, stages.en, stg.en, lty=2)
text(events, 2.7 - 0.1*seq_along(events), evt.labels, col=Set1[c(2,3,3,3)], pos=2, cex=0.9)
abline(m, col=Set1[1], lty=4)
text(x[xi][1], 2, sprintf("%s bbl/dy",round(coef(m)[2]*1e6,-2)), srt=-25, col=Set1[1])
mtext("Total daily-injection\n(AOIs only)", line=-3, adj=0.03, side=3)

# plot(x, cumsum(rowSums(Y, na.rm=TRUE))/1e9,
# 	xlim=xl, xaxs='i',
# 	type='s',ylab=expression(10^9~~"bbl"))
# #segments(as.Date('2015-01-01'), 0, as.Date('2016-07-01'), 1.3, lty=2)
# abline(v=events, col=Set1[c(2,3,3,3)], lty=c(1,5,5,2))
# mtext("Reported Cumulative Volume", line=-2, adj=0.03)

par(mar=c(3,4.5,1,1))
y2 <- apply(Y, 1, function(.x.) length(na.omit(.x.)))
plot(x, y2, col=NA,
	xlim=xl, xaxs='i')
abline(v=events, col=Set1[c(2,3,3,3)], lty=c(1,5,5,2))
lines(x, y2, type='s')
mtext("Active injectors", line=-2, adj=0.03)


#+++++++++++

#FIG <- function(x, ...){}
#if (shake){
#	FIG() 
#} else {
#	figfi <- "some_figure"
#	h <- 7
#	w <- 7
#	niceEPS(figfi, h=h, w=w, toPDF=TRUE)
#	try(FIG())
#	niceEPS()
#	nicePNG(figfi, h=h, w=w)
#	try(FIG())
#	nicePNG()
#}

###

#kook::warn_shake()
#if (require('kook') & packageVersion("kook") < '1.0.17') stop("update kook -- some defaults have changed!")


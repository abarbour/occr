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

library(sp)
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

if (!exists('anss') | !exists('anss.cty') | redo | redo.quakes){
	read_csv("anss/anss_catalog.csv", col_names=TRUE) %>%
		dplyr::mutate(., Date = as.Date(DateTime)) -> anss
		#%>% dplyr::filter(., Date >= as.Date(date.filt1))
	coordinates(anss) <- ~ Longitude + Latitude
	proj4string(anss) <- "+proj=longlat +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0"

	ovr <- over(anss, counties)
	cbind(as.data.frame(anss), ovr) %>% dplyr::filter(., !is.na(NAME10)) -> anss.cty
	coordinates(anss.cty) <- ~ Longitude + Latitude
	proj4string(anss.cty) <- "+proj=longlat +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0"

}

# flipped or zero coords
bad.coords <- c("3508123432","3508521170","3511921090","3511722524")

redo.wells <- TRUE
if (!exists("All.wells") | redo | redo.raw | redo.wells){
	All.dly %>% as.data.frame %>% tbl_df -> Orig.dly
	Orig.dly %>%
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

message("For plotting... see is-oig/Mapping/Oklahoma/map_injection.R")

###

message("")
message("!!!\t1) Update description sub-version to  ", strftime(Sys.time(),"%Y%m%d", tz='UTC'))
message("!!!\t2) Update ANSS and zzz.R.")
message("!!!\t3) Rebuild package.")
message("")

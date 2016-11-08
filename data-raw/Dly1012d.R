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

#if (!require("pacman")) install.packages("pacman", dependencies=TRUE)
#pacman::p_load(package1, package2, package_n)

# loads core tidy packages:  ggplot2, tibble, tidyr, readr, purrr, and dplyr
library(data.table)
library(dtplyr)
library(readxl)
library(tidyverse)
#tidyverse_conflicts()
#tidyverse_update(TRUE)

## local/github libs
# devtools::install_github("abarbour/kook")
#library(kook)
#Set1 <- brew.set1()
#Set1l <- brew.set1(TRUE)
#Dark2 <- brew.dark2()
#Dark2l <- brew.dark2(TRUE)
#Dark2ll <- brew.dark2(TRUE,TRUE)

#+++++++++++

shake <- FALSE
redo <- FALSE

if (!exists("dly.raw") | redo){
	#Arbuckle AOI Wells - 1012D Reported Volumes	
	#This file contains the weekly reports of daily
	#volumes of all Arbuckle disposal wells in the Area
	#of Interest for Triggered Seismicity, submitted by
	#operators via the OCC efile system. Filing of the
	#daily data for these wells began on 3/27/2016	
	#Daily	Jim Marlatt 405-522-2758	ZIP
	read_excel("Dly1012d.xlsx") -> dly.raw
}

#+++++++++++

fix.names <- function(x){
	gsub("\r\n",".",x)
}

if (!exists("All.dly") | redo){

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

All.dly

dcast(setDT(All.dly), Report.Date ~ API, value.var=c('Volume.BPD'), drop=FALSE, fun.aggregate=mean) %>% as.data.frame -> Dly.api

#matplot(Dly.api[,1], Dly.api[,-1], type='l')

events <- as.Date(c('2016-03-27',"2016-09-03",'2016-11-02','2016-11-07'))
evt.labels <- c("filing begins", expression(bold(M)*5.8), expression(bold(M)*4.5), expression(bold(M)*5.0))

Dly.api.filt <- filter(Dly.api, Report.Date >= as.Date('2015-01-01'))

layout(matrix(1:3), heights=c(3,1.3,1.3))
par(las=1, mar=c(3,4.5,1,1))
Y <- rowSums(Dly.api.filt[,-1], na.rm=TRUE)/1e6
plot(Dly.api.filt[,1], Y,
	ylim=c(0,max(Y,na.rm=TRUE)),
	type='l',ylab=expression(10^6~~"bbl"))
abline(v=events, col='red', lty=c(1,5,5,2))
stages.st <- as.Date(paste0("2016-",c("03-07","03-29","04-18","05-08")))
stages.en <- as.Date(paste0("2016-",c("03-28","04-17","05-07","05-28")))
stg.st <- c(1.8, 1.8*0.75, (1.8*0.75)*0.75, ((1.8*0.75)*0.75)*0.75)
stg.en <- c(1.8*0.75, (1.8*0.75)*0.75, ((1.8*0.75)*0.75)*0.75, (((1.8*0.75)*0.75)*0.75)*0.75)
segments(stages.st, stg.st, stages.en, stg.en, lty=2)
text(events, 2.5 - 0.1*seq_along(events), evt.labels, col='red', pos=2, cex=0.9)
mtext("Daily injection (all in AOI)", line=-2, adj=0.03, side=1)

plot(Dly.api.filt[,1], cumsum(rowSums(Dly.api.filt[,-1], na.rm=TRUE))/1e9,
	type='s',ylab=expression(10^9~~"bbl"))
#segments(as.Date('2015-01-01'), 0, as.Date('2016-07-01'), 1.3, lty=2)
mtext("Cumulative volume", line=-2, adj=0.03)
	
plot(Dly.api.filt[,1], apply(Dly.api.filt[,-1], 1, function(x) length(na.omit(x))), type='s', ylab="Active injectors")
abline(v=events, col='red', lty=c(1,5,5,2))
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


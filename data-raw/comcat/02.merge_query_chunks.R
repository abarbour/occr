#!/usr/bin/env Rscript --no-save

###
#	02.merge_query_chunks.R
#	/Users/abl/survey.processing/development/R/packages/occr/data-raw/comcat
#	Created by 
#		/Users/abl/bin/ropen ( v. 2.6.6 )
#	on 
#		2018:072 (13-March)
#
#	[ What this script does, broadly ]
#
###

## local functions
#try(source('funcs.R'))

## libs

library(tools)

#if (!require("pacman")) install.packages("pacman", dependencies=TRUE)
#pacman::p_load(package1, package2, package_n)
library(sp)
library(occr)

# loads core tidy packages:  ggplot2, tibble, tidyr, readr, purrr, and dplyr
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

chunks <- list(C1="query_chunk_01.csv", C2="query_chunk_02.csv")

#+++++++++++

shake <- FALSE
redo <- FALSE
inter <- interactive()

loader <- function(fi, ...){
	readr::read_csv(fi, col_names=TRUE) %>%
		dplyr::rename(., DateTime = `time`) %>%
		dplyr::mutate(., Date = as.Date(DateTime),
			horizontalError = as.numeric(horizontalError),
			depthError = as.numeric(depthError),
			magError = as.numeric(magError))
}

#+++++++++++

Dat <- lapply(chunks, loader)

All_quakes <- bind_rows(Dat)

#+++++++++++

# Perform some QC checks

All_quakes %>% 
	dplyr::group_by(id) %>% 
	dplyr::summarize(Nid=n()) -> tallies

tallies %>% dplyr::filter(Nid > 1) -> dupes

if (nrow(dupes)>0){
	print(as.data.frame(dupes))
	stop("some IDs have multiple entries!")
}

Prior <- loader('query_22122017.csv')

All_quakes %>% dplyr::filter(DateTime <= max(Prior$DateTime)) -> Current

id.current <- Current$id
id.previous <- Prior$id

plot(OKcounties)
points(latitude ~ longitude, dplyr::filter(Prior, id %in% setdiff(id.previous, id.current)), col='red', pch=".")
points(latitude ~ longitude, dplyr::filter(Current, id %in% setdiff(id.current, id.previous)), col='cyan', pch=3)

outfi <- 'comcat_catalog.csv'
write_csv(All_quakes, outfi)
message("\n\tSaved ", outfi, "\n")

#+++++++++++

#shake.fig <- TRUE
#if (shake.fig & inter){
#	FIG() 
#} else {
#	figfi <- "some_figure"
#	h <- w <- 7
#	niceEPS(figfi, h=h, w=w, toPDF=TRUE)
#	try(FIG())
#	niceEPS()
#	nicePNG(figfi, h=h, w=w)
#	try(FIG())
#	nicePNG()
#}

#kook::warn_shake()

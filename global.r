
 
install_packages <- c("shiny","shinyjs","DT", "V8","shinyWidgets","colourpicker",
            "reshape2", "dplyr",  "markdown",
			 "htmltools", "shinythemes",
			"shinycssloaders", "readxl", "tidyverse")
if (length(setdiff(install_packages, rownames(installed.packages()))) > 0) {
            install.packages(setdiff(install_packages, rownames(installed.packages())))
}


library(tidyverse)
library(readxl)
library(reshape2)
library(htmltools)
library(shinyWidgets)
library(colourpicker)
library(DT)
library(shinycssloaders)

CSVimport <- function(...) {
		Input <- list(...)[[1]]
		## check whether a .CSV file is uploaded
		validate(
			need(tools::file_ext(Input$name) %in% c(
			'csv'
			), "Wrong File Format Uploaded")
		)	
		## Import .CSV file
		read.csv(Input$datapath, header=TRUE, comment.char = "", check.names = FALSE)			
}				

TXTimport <- function(...) {
		Input <- list(...)[[1]]
		## check whether a .txt file is uploaded
		validate(
			need(tools::file_ext(Input$name) %in% c(
			'txt'
			), "Wrong File Format Uploaded")
		)	
		## Import .txt file
		read.table(Input$datapath, header=TRUE, comment.char = "", check.names = FALSE, sep="\t")
}				

EXCELimport <- function(...) {
		Input <- list(...)[[1]]
		Sheet <- list(...)[[2]]
		## check whether a .CSV file is uploaded
		validate(
			need(tools::file_ext(Input$name) %in% c(
			'xlsx', 'xls'
			), "Wrong File Format Uploaded")
		)		
		## Import .CSV file
		read_excel(Input$datapath, Sheet) %>% as.data.frame
	
}				

				
				
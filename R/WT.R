library(data.table)
library(dplyr)
library(ggplot2)
library(tidyr)

#' Reads data from clipboard and saves it as a data.table
#' 
#' @param *kwarg: same parameters as data.table::fread()
#' 
#' @return A data table / data frame
#' @export
#' 
#' @examples
#' var <- freadc()
freadc <- function(...) {
	tmpFile = paste0(Sys.getenv("temp"), "\\R_clipboard.txt")
	writeLines(readLines("clipboard"),tmpFile)
	fread(tmpFile, ...)
}


#' Convert column of strings to column of dates.  Keeps rest of dataframe intact
#' 
#' @param df dataframe
#' @param colName name of the column in string format
#' @param date_format Date format.  Defaults to %Y-%m-%d (ISO standard)
#'
#' @return dataframe with the updated column
#' @export
#' 
#' @examples
#' \dontrun{
#' df <- colToDate(df,"ValDate")
#' }
colToDate <- function(df, colName, date_format="%Y-%m-%d") {
	tmp = as.data.frame(df)
	tmp[,colName] = as.Date(tmp[,colName], format=date_format)
	return(as.data.table(tmp))
}


#' Convert column of strings to column of numbers.  Keeps rest of dataframe intact
#' 
#' @param df dataframe
#' @param colName name of the column in string format
#'
#' @return dataframe with the updated column
#' @export
#' 
#' @examples
#' \dontrun{
#' df <- colToNum(df,"Amount")
#' }
colToNum <- function(df, colName) {
	# coerces column of strings to a number (and takes out the commas)
	tmp = as.data.frame(df)
	tmp[,colName] = gsub(",","",tmp[,colName])
	tmp[,colName] = as.numeric(tmp[,colName])
	return(as.data.table(tmp))
}


#' Removes all the variables in global namespace (but keeps the functions)
#' 
#' @return Nothing.  Variables are removed from given namespace.  global by default
#' @export
#' 
#' @examples
#' \dontrun{
#' clearVar()
#' }
clearVar <- function(env) {
	if(missing(env)) {env = .GlobalEnv}
	rm(list= setdiff(ls(envir = env), lsf.str(envir = env)), envir = env)
}


#' copies the specified R dataframe to the clipboard
#' 
#' @param x: a dataframe to copy to the clipboard
#' 
#' @return Data saved in clipboard
#' @export
#' 
#' @examples
#' cb <- df()
cb <- function(df) {
	write.table(df, paste0("clipboard-", 2^16), sep="\t", row.names=FALSE)
}


#' save ggplot image in preferred format
#' 
#' @param width passes width to ggsave (different units accepted, inches by default
#' @param height passes height to ggsave (different units accepted, inches by default
#' @param fp the filepath to save the image to.  Include the extension for type
#' 
#' @return Saves a picture to the specified filepath
#' @export
#' 
#' @examples
#' \dontrun{
#' import(ggplot2)
#' data(iris)
#' p <- ggplot(iris,aes(Petal.length,Petal.Width)) + geom_point()
#' pic(p)
#' }
pic <- function(width=12,height=9,fp=paste0(Sys.getenv("temp"), "\\tmp.png")) {
	ggsave(fp, width=width, height=height)
	system(paste0('open"', fp, '"'))
}


#' Combines files in specified directory into one dataframe using rbindlist
#' 
#' @param strDir Location of directory
#' @param bAddCol (boolean) True will add a column with the source filename
#' 
#' @return A single dataframe with all data
#' @export
#' 
#' @examples
#' \dontrun{
#' combineFiles("dir1/subdir/")
#' }
combineFiles <- function(strDir, bAddCol) {
	filenames = list.files(path=strDir, full.names=FALSE)
	return(rbindlist(lapply(filenames, addColname, strDir=strDir, bAddCol=bAddCol)))
}


#' private function used in combineFiles().  Not meant to be stand-alone.
#' 
#' @param fn filename
#' @param strDir directory name (that doesn't end in "/")
#' @param bAddCol (boolean) True will add a column with the source filename
#'
#' @return data.table without the header
#' 
#' @examples
addColname <- function(fn, strDir, bAddCol) {
	tmp = fread(paste0(strDir, "/", fn))
	if (bAddCol) {tmp$fn = fn}
	return(tmp)
}


#' Goes through all the columns in the dataframe and replaces spaces and ampersand with
#' underscore.  Spaces and & do not play well when specifying the columns in other functions
#'
#' @param df dataframe
#'
#' @return same dataframe but with updated columns names if any had a space or ampersand
#' @export
#' 
#' @examples
#' \dontrun{
#' df <- updateColNames(df)
#' }
updateColNames <- function(df) {
	# goes through all the columns in the dataframe and replaces bad char with something safe
	names = colnames(df)
	colnames(df) = as.character(lapply(colnames(df), function(x) gsub(" ", "_", as.character(x))))
	colnames(df) = as.character(lapply(colnames(df), function(x) gsub("&", "_", as.character(x))))
	return(df)
}


# -------------------------------------------------
# Optional functions, used for specific use cases

#' Combines all csv files in specified directory into one dataframe
#'
#' @param strDir (string) Specified directory
#' @param bAddCol (boolean) column wiht source filename
#'
#' @return one dataframe with all data
#' @export
#' 
#' @examples
combineCSV <- function(strDir, bAddCol) {
	# reads all files in specified directory
	# bAddcol is a boolean that specifies if you want to add a column with the source filename
	filenames = list.files(path=strDir, full.names=FALSE)
	return(rbindlist(lapply(filenames, addColname2, strDir=strDir, bAddCol=bAddCol)))
}


#' Private function used in combineCSV().  not meant to be stand-alone
#' Reads the file into a data.table and also adds a column with the filename to label the data
#'
#' @param fn filename
#' @param strDir directory name without an ending "/"
#' @param bAddCol (boolean) add columns with the filename if true
#'
#' @return data.table without the header
#' 
#' @examples
addColname2 <- function(fn, strDir, bAddCol) {
	tmp = read.csv(paste0(strDir, "/", fn))
	if (bAddcol) {tmp$fn = fn}
	return(as.data.table(head(tmp,-1)))
}

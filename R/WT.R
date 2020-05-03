# My helper functions

library(data.table)

#' Reads df from clipboard
#'
#' Takes copied data from clipboard memory, saves to file where it is then loaded with fread
#'
#' @param ... standard fread arguments
#' @return dataframe
#' @export
freadc <- function(...){
  tmpFile = "C:/Users/m92p-320-8-3470t/AppData/Local/Temp/tmp_shared.txt"
  writeLines(readLines("clipboard-16384"), tmpFile)
  tmp = data.table::fread(tmpFile,...)
  return(tmp)
}

#' Converts all data in column to Date datatype
#'
#'
#' @param df original dataframe
#' @param colName column to convert
#' @param date_format the format the string is in. By default, ISO standard
#' @return dataframe
#' @export
colToDate <- function(df, colName, date_format="%Y-%m-%d"){
  tmp = as.data.frame(df)
  tmp[,colName] = as.Date(tmp[,colName], format=date_format)
  return(as.data.table(tmp))
}

#' Converts all data in column to numeric (float) datatype
#'
#'
#' @param df original dataframe
#' @param colName column to convert
#' @return dataframe
#' @export
colToNum <- function(df, colName){
  tmp = as.data.frame(df)
  tmp[,colName] = sub(",","",tmp[,colName])
  tmp[,colName] = as.numeric(tmp[,colName])
  return(as.data.table(tmp))
}

#' Clears all variables (and functions)
#'
#' @param env specific environment to apply to.  Default: .GlobalEnv
#' @export
clearVar <- function(env) {
  if(missing(env)) {env = .GlobalEnv}
  rm(list = setdiff(ls(envir=env), lsf.str(envir=env)),envir=env)
}

#' Copies the dataframe to clipboard
#'
#' @param df dataframe
#' @return NULL.  copies dataframe to memory
#' @export
cb <- function(df) {
  # copies the current dataframe to memory
  write.table(df, "clipboard", sep="\t", row.names=FALSE)
}

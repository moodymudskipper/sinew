# oxygenAddin roxy [sinew] ---- 
#' @importFrom rstudioapi getSourceEditorContext insertText
# oxygenAddin function [sinew] ----
oxygenAddin <- function() {
  context <- rstudioapi::getSourceEditorContext()
  obj_name <- gsub("[\"\']", "", context$selection[[1]]$text)
  assign(obj_name, eval(parse(text = obj_name)))
  eval(parse(text = sprintf("ret<-makeOxygen(%s,print=FALSE,cut=sinew_opts$get('cut'))", obj_name)))
  cl <- NULL
  eval(parse(text = sprintf("cl<-class(%s)", obj_name)))
  if (cl == "function") ret <- paste(ret, obj_name, sep = "\n")
  rstudioapi::insertText(text = ret)
}

# importAddin roxy [sinew] ---- 
#' @importFrom rstudioapi getSourceEditorContext insertText
# importAddin function [sinew] ----
importAddin <- function() {
  context <- rstudioapi::getSourceEditorContext()
  obj_name <- gsub("[\"\']", "", context$selection[[1]]$text)
  assign(obj_name, eval(parse(text = obj_name)))
  eval(parse(text = sprintf("ret<-makeImport(%s,print=FALSE,cut=sinew_opts$get('cut'))", obj_name)))
  cl <- NULL
  eval(parse(text = sprintf("cl<-class(%s)", obj_name)))
  if (cl == "function") ret <- paste(ret, obj_name, sep = "\n")
  rstudioapi::insertText(text = ret)
}

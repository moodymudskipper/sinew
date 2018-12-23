# strip_collapse roxy [sinew] ---- 
#' @title Remove sinew collapsing tags
#' @description Remove collapsing tags from a lines of code
#' @param path character, path to file(s) or directory to apply tags to
#' @return character
#' @author Jonathan Sidi
#' @export
#' @importFrom tools file_ext
# strip_collapse function [sinew] ----
strip_collapse <- function(path){
  
  FILES <- path
  
  if(length(path)==1){
    
    path <- normalizePath(path)
    
    if(!nzchar(tools::file_ext(basename(path)))){
      
      FILES <- list.files(path,pattern = '.(r|R)$',full.names = TRUE)
      if(length(FILES)==0){
        return(message('No R file extensions found in directory path'))
      } 
    }
    
  }
  
  FILES <- normalizePath(FILES)
  
  for(FILE in FILES){
    NEW_LINES <- paste0(strip_tags(readLines(FILE,warn = FALSE)),collapse='\n')
    cat(NEW_LINES,sep='\n',file=FILE)
    
  }
  
}

strip_tags <- function(lines){
  lines[!grepl('\\[sinew\\] ----',lines)]
}


# strip_collapse_addin roxy [sinew] ---- 
#' @importFrom rstudioapi getSourceEditorContext modifyRange setCursorPosition
# strip_collapse_addin function [sinew] ----
strip_collapse_addin <- function(){
  
  adc <- rstudioapi::getSourceEditorContext()
  
  replace_lines <- paste0(strip_tags(adc$contents),collapse = '\n')
  
  rstudioapi::modifyRange(doc_range(adc),replace_lines,id = adc$id)
  
  rstudioapi::setCursorPosition(c(1,1),id=adc$id)
  
}

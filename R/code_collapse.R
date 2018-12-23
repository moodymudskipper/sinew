# code_collapse roxy [sinew] ---- 
#' @title Collapsing tags
#' @description Append code collapsing tags for RStudio IDE to pairs
#'  of function and roxygen2 documentation
#' @param path character, path to file(s) or directory to apply tags to
#' @return character
#' @author Jonathan Sidi
#' @export
#' @importFrom tools file_ext
# code_collapse function [sinew] ----
code_collapse <- function(path){

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
    
    NEW_LINES <- paste0(append_collapse(readLines(FILE,warn = FALSE)),collapse='\n')
    cat(NEW_LINES,sep='\n',file=FILE)

  }
  
}

# append_collapse roxy [sinew] ---- 
#' @importFrom utils getParseData
# append_collapse function [sinew] ----
append_collapse <- function(lines){
  
  tf <- tempfile(fileext = '.R')
  
  on.exit(unlink(tf),add = TRUE)
  
  lines <- strip_tags(lines)
  
  cat(lines,file = tf,sep = '\n')
  
  tf_parse <- parse(file = tf)
  
  tf_data <- utils::getParseData(tf_parse,includeText = TRUE)
  
  tf_data$group <- tf_data$parent
  tf_data$group[tf_data$parent>0] <- NA
  tf_data$group[tf_data$parent<0] <- -tf_data$group[tf_data$parent<0]
  tf_data$group[tf_data$parent<0&!grepl("^#'",tf_data$text)] <- NA
  
  parent_idx <- which(tf_data$parent==0)
  tf_data$group[tf_data$parent==0] <- tf_data$id[tf_data$parent==0]
  rm_comment_idx <- tf_data$group[parent_idx[!grepl('function\\(',tf_data$text[parent_idx])]]
  tf_data$group[parent_idx[!grepl('function\\(',tf_data$text[parent_idx])]] <- NA
  tf_data$group[tf_data$parent%in%-c(rm_comment_idx)] <- NA
  
  tf_data_filter <- tf_data[!is.na(tf_data$group),]
  tf_data_split <- split(tf_data_filter,tf_data_filter$group)
  collapse_exits <- sapply(tf_data_split,function(x) grepl('\\[sinew\\] ----',x$text[1]))
  tf_data_split <- tf_data_split[!collapse_exits]
  
  if(length(tf_data_split)==0){
    return(lines)
  }
  
  tf_data_append <- lapply(tf_data_split,function(x){
    
    nr <- nrow(x)
    new_text <- gsub('\\s+','',gsub('<-(.*?)$','',x$text[nr]))
    x <- x[!(grepl("^#",x$text)&!grepl("^#'",x$text)),]
    if(nrow(x)>1){
      x <- x[-nr,]
      x$text[1] <- sprintf('# %s roxy [sinew] ---- \n%s',new_text,x$text[1])
      x$text[nr-1] <- sprintf('%s\n# %s function [sinew] ----',x$text[nr-1],new_text)
    }else{
      x$text[1] <- sprintf('# %s function [sinew] ---- \n%s',new_text,
                           gsub('\\n(.*?)$','',x$text[1]))
    }
    x[,c(1,9)]
  })
  
  tf_data_bind <- do.call('rbind',tf_data_append) 
  
  lines[tf_data_bind$line1] <- tf_data_bind$text
  
  lines
}

# code_collapse_addin roxy [sinew] ---- 
#' @importFrom rstudioapi getSourceEditorContext modifyRange setCursorPosition
# code_collapse_addin function [sinew] ----
code_collapse_addin <- function(){
  
  adc <- rstudioapi::getSourceEditorContext()
  
  replace_lines <- paste0(append_collapse(adc$contents),collapse = '\n')
  
  rstudioapi::modifyRange(doc_range(adc),replace_lines,id = adc$id)
  
  rstudioapi::setCursorPosition(c(1,1),id=adc$id)
  
}

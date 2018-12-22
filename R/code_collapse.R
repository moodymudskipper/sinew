#' @importFrom rstudioapi getSourceEditorContext modifyRange setCursorPosition
#' @importFrom utils getParseData
code_collapse <- function(){
  
  adc <- rstudioapi::getSourceEditorContext()
  
  tf <- tempfile(fileext = '.R')
  
  on.exit(unlink(tf),add = TRUE)
  
  cat(adc$contents,file = tf,sep = '\n')
  
  tf_parse <- parse(file = tf)
  
  tf_data <- utils::getParseData(tf_parse,includeText = TRUE)
  
  tf_data$group <- tf_data$parent
  tf_data$group[tf_data$parent>0] <- NA
  tf_data$group[tf_data$parent<0] <- -tf_data$group[tf_data$parent<0]
  tf_data$group[tf_data$parent==0] <- tf_data$id[tf_data$parent==0]
  
  tf_data_filter <- tf_data[!is.na(tf_data$group),]
  tf_data_split <- split(tf_data_filter,tf_data_filter$group)
  
  tf_data_append <- lapply(tf_data_split,function(x){
    nr <- nrow(x)
    new_text <- gsub('\\s+','',gsub('<-(.*?)$','',x$text[nr]))
    x <- x[!(grepl("^#",x$text)&!grepl("^#'",x$text)),]
    if(nrow(x)>1){
      x <- x[-nr,]
      x$text[1] <- sprintf('# %s roxy ---- \n%s',new_text,x$text[1])
      x$text[nr-1] <- sprintf('%s\n# %s function ---- \n',x$text[nr-1],new_text)
    }else{
      x$text[1] <- sprintf('# %s function ---- \n%s',
                           new_text,
                           gsub('\\n(.*?)$','',x$text[1]))
    }
    x[,c(1,9)]
  })
  
  tf_data_bind <- do.call('rbind',tf_data_append) 
  
  lines <- adc$contents
  lines[tf_data_bind$line1] <- tf_data_bind$text
  
  replace_lines <- paste0(lines,collapse = '\n')
  
  rstudioapi::modifyRange(doc_range(adc),replace_lines,id = adc$id)
  
  rstudioapi::setCursorPosition(c(1,1),id=adc$id)
  
}

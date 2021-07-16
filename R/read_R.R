read_R <- function(path,vector=TRUE){
    r <- list.files(path = path,pattern = '.R',full.names = TRUE)
    sapply(r,function(i) paste0(readLines(i),collapse = '\n'))
}

#' Extract file name
#'
#' @param ... one or more file path
#'
#' @return file names
#' @export
#'
#' @examples
#' file.name('f:/dir/1.txt')
#' file.name('f:/dir/1.txt', 'f:/dir/1.txt')
#' file.name('f:/dir/1.txt', 'f:/dir/1.txt', 'f:/dir/')
file.name <- function(...){
    fn <- c(...) |> 
        sapply(reverse) |> 
        Replace0('/.*','\\\\.*') |> 
        sapply(reverse)
    names(fn) <- NULL
    fn[!grepl('\\.',fn)] <- NA
    fn
}


#' Extract data of model
#'
#' @param fit fitted results
#' @importFrom stats as.formula update
#' @return dataframe in the model
#' @export
#'
#' @examples
#' fit <- lm(mpg~vs+am+poly(qsec,2),data=mtcars)
#' head(model.data(fit))
model.data <- function(fit){
    fit = update(fit,model=TRUE,x=TRUE,y=TRUE)
    # update model
    x=strsplit(deparse(fit$call$formula[[3]]),' \\+ ')[[1]]
    x2=do::Replace0(x,c('.*\\(',',.*','\\).*'))
    formu=as.formula(paste0(deparse(fit$call$formula[[2]]),'~',paste0(x2,collapse = '+')))
    fit2=update(fit,formu)
    fit2$model
}

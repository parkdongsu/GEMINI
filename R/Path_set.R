#' GEMINI path setting
#'
#' Work directory path setting
#' @keywords gemini
#' @export
#'

path_set <- function(){
    tryCatch(
    my_wd <- choose.dir(),
    error = function(e){
        my_wd <- readline("Set work directory path : ")
    })
    setwd(my_wd)
}

#' Make report
#'
#' This function render rmd file and execute html file
#' @keywords gemini
#' @export
#'

# Execute R markdown file
make_report<-function(){
    tryCatch({
        file.copy(from = paste0(.libPaths()[1],"/gemini/data/Gemini_md.Rmd"), to = getwd(),overwrite = T)
        rmarkdown::render(paste0(getwd(),"/Gemini_md.Rmd"),encoding = "UTF-8")
        browseURL(url=paste0(getwd(),"/Gemini_md.html"))
    },error = function(x){
        message("Need Rmd file.")
    })
    rm(list=ls())
}

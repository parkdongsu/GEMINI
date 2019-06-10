#' Death data
#'
#' This function for extract data from death table
#' @keywords gemini
#' @export
#'
death_data <- function(){
    death_sql<- splitSql(readSql(paste0(.libPaths()[1],"/gemini/extdata/Death_query.sql")))
    progressBar <- utils::txtProgressBar(max=length(death_sql),style=3)
    progress = 0
    prg_plus <- function(prgBar,prg){
        prg = prg + 1
        utils::setTxtProgressBar(prgBar, prg)
        return(prg)
    }
################################################################################
# Get data from death_date to check person who got 0 or many death_date
# If not value = 1, It should be wrong data
################################################################################
tryCatch({
  deathtbl_check <- queryRender(death_sql[1])
}, error = function(e) {
  deathtbl_check <- NULL
})
    progress <- prg_plus(progressBar,progress)
################################################################################
# Get data from death_type_concept_id to check death type.
################################################################################
tryCatch({
  deathtbl_type <- queryRender(death_sql[2])
}, error = function(e) {
  deathtbl_type <- NULL
})
    progress <- prg_plus(progressBar,progress)
    death_tbl <- list(deathtbl_check,deathtbl_type)
    return(death_tbl)
    close(progressBar)
}

#' Death data
#'
#' This function for extract data from death table
#' @keywords gemini
#' @export
#'
death_data <- function(){
    death_sql<- splitSql(readSql(paste0(.libPaths()[1],"/gemini/inst/extdata/Death_query.sql")))
################################################################################
# Get data from death_date to check person who got 0 or many death_date
# If not value = 1, It should be wrong data
################################################################################
tryCatch({
  deathtbl_check <- queryRender(death_sql[1])
}, error = function(e) {
  deathtbl_check <- NULL
})
################################################################################
# Get data from death_type_concept_id to check death type.
################################################################################
tryCatch({
  deathtbl_type <- queryRender(death_sql[2])
}, error = function(e) {
  deathtbl_type <- NULL
})
    death_tbl <- list(deathtbl_check,deathtbl_type)
    return(death_tbl)
}

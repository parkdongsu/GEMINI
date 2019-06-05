#' Drug era data
#'
#' This function extract data from drug era table
#' @keywords gemini
#' @export
#'
drug_era_data <- function(){
    drug_era_sql<- splitSql(readSql(paste0(.libPaths()[1],"/gemini/data/sql/Drug_era_query.sql")))
    if(connection@dbms == 'sql server'){
        drug_era_record_sql <- splitSql(readSql(paste0(.libPaths()[1],"/gemini/data/sql/System_query.sql")))[1]
    }else if(connection@dbms == 'postgresql'){
        drug_era_record_sql <- splitSql(readSql(paste0(.libPaths()[1],"/gemini/data/sql/System_query.sql")))[2]
    }
################################################################################
# Get data from drug_era_id
################################################################################
tryCatch(drug_eratbl_record <- queryRender(drug_era_record_sql,"drug_era")
  ,
  error = function(e) {
    drug_eratbl_record <- NULL
  }
)
################################################################################
# Get data from person_id
################################################################################
tryCatch(drug_eratbl_person_ratio <- queryRender(drug_era_sql[1],"drug_era")
  ,
  error = function(e) {
    drug_eratbl_person_ratio <- NULL
  }
)
################################################################################
# Extract drug_era_start_date
################################################################################
tryCatch(drug_eratbl_start <- queryRender(drug_era_sql[2],"drug_era", "drug_era_start_date")
  ,
  error = function(e) {
    drug_eratbl_start <- NULL
  }
)
################################################################################
# Extract drug_era_end_date
################################################################################
tryCatch(drug_eratbl_end <- queryRender(drug_era_sql[3],"drug_era", "drug_era_end_date")
  ,
  error = function(e) {
    drug_eratbl_end <- NULL
  }
)
################################################################################
# Get data from drug_era_diff_date
################################################################################
tryCatch({
  drug_eratbl_diff_date <- queryRender(drug_era_sql[4],"drug_era", startName = "drug_era_start_date", endName = "drug_era_end_date")
}, error = function(e) {
  drug_eratbl_diff_date <- NULL
})
################################################################################
# Get data from drug_exposure_count
#
################################################################################
tryCatch({
  drug_eratbl_exp_count <- queryRender(drug_era_sql[5],"drug_era", "drug_exposure_count")
}, error = function(e) {
  drug_eratbl_exp_count <- NULL
})
################################################################################
# Get data from gap_day
# hist
################################################################################
tryCatch({
drug_eratbl_gap_days <- queryRender(drug_era_sql[6],"drug_era")
}, error = function(e) {
  drug_eratbl_gap_days <- NULL
})
    drug_eratbl <- list(drug_eratbl_record, drug_eratbl_person_ratio, drug_eratbl_diff_date, drug_eratbl_start, drug_eratbl_end,
                        drug_eratbl_exp_count, drug_eratbl_gap_days)
    return(drug_eratbl)
}

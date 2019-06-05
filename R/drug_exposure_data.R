#' Drug exposure data
#'
#' This function extract data from drug exposure table
#' @keywords gemini
#' @export
#'
drug_exposure_data <- function(){
    drug_exp_sql<- splitSql(readSql(paste0(.libPaths()[1],"/gemini/data/sql/Drug_exposure_query.sql")))
    if(connection@dbms == 'sql server'){
        drug_exp_record_sql <- splitSql(readSql(paste0(.libPaths()[1],"/gemini/data/sql/System_query.sql")))[1]
    }else if(connection@dbms == 'postgresql'){
        drug_exp_record_sql <- splitSql(readSql(paste0(.libPaths()[1],"/gemini/data/sql/System_query.sql")))[2]
    }
################################################################################
# Get data from drug_exposure_id
################################################################################
tryCatch(drug_exptbl_record <- queryRender(drug_exp_record_sql,"drug_exposure")
  ,
  error = function(e) {
    drug_exptbl_record <- NULL
  }
)
################################################################################
# Get data from person_id
################################################################################
tryCatch(drug_exptbl_person_ratio <- queryRender(drug_exp_sql[1],"drug_exposure")
  ,
  error = function(e) {
    drug_exptbl_person_ratio <- NULL
  }
)
################################################################################
# Extract drug_exposure_start_date
################################################################################
tryCatch(drug_exptbl_start <- queryRender(drug_exp_sql[2],"drug_exposure", "drug_exposure_start_date")
  ,
  error = function(e) {
    drug_exptbl_start <- NULL
  }
)
################################################################################
# Extract drug_exposure_end_date
################################################################################
tryCatch(drug_exptbl_end <- queryRender(drug_exp_sql[3],"drug_exposure", "drug_exposure_end_date")
  ,
  error = function(e) {
    drug_exptbl_end <- NULL
  }
)
################################################################################
# Get data from drug_exposure_diff_date
################################################################################
tryCatch({
  drug_exptbl_diff_date <- queryRender(drug_exp_sql[4],"drug_exposure", startName = "drug_exposure_start_date", endName = "drug_exposure_end_date")
}, error = function(e) {
  drug_exptbl_diff_date <- NULL
})
################################################################################
# Get data from drug_type_concept_id
################################################################################
tryCatch({
  drug_exptbl_type_concept <- queryRender(drug_exp_sql[5],"drug_exposure", "drug_type_concept_id")
}, error = function(e) {
  drug_exptbl_type_concept <- NULL
})
################################################################################
# Get data from stop_reason
################################################################################
tryCatch({
  drug_exptbl_stop <- queryRender(drug_exp_sql[6],"drug_exposure")
}, error = function(e) {
  drug_exptbl_stop <- NULL
})
################################################################################
# Get data from route_concept_id
################################################################################
tryCatch({
  drug_exptbl_route <- queryRender(drug_exp_sql[7],"drug_exposure", "route_concept_id")
}, error = function(e) {
  drug_exptbl_route <- NULL
})
################################################################################
# Get data from visit_occurrence_id
################################################################################
tryCatch({
  drug_exptbl_visit_occurrence <- queryRender(drug_exp_sql[8],"drug_exposure", "visit_occurrence_id", comparedAttName = "drug_exposure_id")
}, error = function(e) {
  drug_exptbl_visit_occurrence <- NULL
})
################################################################################
# Get data from visit_detail_id
# No 'visit_detail_id' data in drug_exposure table
# drug_exposure_visit_detail <- get_compared_ratio("drug_exposure","visit_detail_id","drug_exposure_id")
################################################################################
tryCatch({
  drug_exptbl_visit_detail <- queryRender(drug_exp_sql[9],"drug_exposure", "visit_detail_id", comparedAttName = "drug_exposure_id")
}, error = function(e) {
  drug_exptbl_visit_detail <- NULL
})
    drug_exptbl <- list(drug_exptbl_record, drug_exptbl_person_ratio, drug_exptbl_diff_date, drug_exptbl_start, drug_exptbl_end, drug_exptbl_type_concept,
                        drug_exptbl_stop, drug_exptbl_route, drug_exptbl_visit_occurrence)
    return(drug_exptbl)
}
message("Drug exposure data extracting done.")

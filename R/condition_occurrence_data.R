#' Condition occurrence data
#'
#' This function extract data from condition occurrence table
#' @keywords gemini
#' @export
#'
condition_occurrence_data <- function(){
    condition_sql<- splitSql(readSql(paste0(.libPaths()[1],"/gemini/extdata/Condition_occurrence_query.sql")))
    if(connection@dbms == 'sql server'){
        condition_record_sql <- splitSql(readSql(paste0(.libPaths()[1],"/gemini/extdata/System_query.sql")))[1]
    }else if(connection@dbms == 'postgresql'){
        condition_record_sql <- splitSql(readSql(paste0(.libPaths()[1],"/gemini/extdata/System_query.sql")))[2]
    }
    progressBar <- utils::txtProgressBar(max=length(condition_sql)+1,style=3)
    progress = 0
    prg_plus <- function(prgBar,prg){
        prg = prg + 1
        utils::setTxtProgressBar(prgBar, prg)
        return(prg)
    }
################################################################################
# Get data from condition_occurrence_id
################################################################################
tryCatch(conditiontbl_record <- queryRender(condition_record_sql,"condition_occurrence")
  ,
  error = function(e) {
    conditiontbl_record <- NULL
  }
)
    progress <- prg_plus(progressBar,progress)
################################################################################
# Get data from person_id
################################################################################
tryCatch(conditiontbl_person_ratio <- queryRender(condition_sql[1],"condition_occurrence")
  ,
  error = function(e) {
    conditiontbl_person_ratio <- NULL
  }
)
    progress <- prg_plus(progressBar,progress)
################################################################################
# Extract condition_start_date
################################################################################
tryCatch(conditiontbl_start <- queryRender(condition_sql[2],"condition_occurrence", "condition_start_date")
  ,
  error = function(e) {
    conditiontbl_start <- NULL
  }
)
    progress <- prg_plus(progressBar,progress)
################################################################################
# Extract condition_end_date
################################################################################
tryCatch(conditiontbl_end <- queryRender(condition_sql[3],"condition_occurrence", "condition_end_date")
  ,
  error = function(e) {
    conditiontbl_end <- NULL
  }
)
    progress <- prg_plus(progressBar,progress)
################################################################################
# Extract condition_start_date, condition_end_date for histogram
################################################################################
tryCatch(conditiontbl_diff_date <- queryRender(condition_sql[4],"condition_occurrence", startName = "condition_start_date", endName = "condition_end_date")
  ,
  error = function(e) {
    conditiontbl_diff_date <- NULL
  }
)
    progress <- prg_plus(progressBar,progress)
################################################################################
# Get data from condition_type_concept_id
################################################################################
tryCatch(conditiontbl_type_concept <- queryRender(condition_sql[5],"condition_occurrence", "condition_type_concept_id")
  ,
  error = function(e) {
    conditiontbl_type_concept <- NULL
  }
)
    progress <- prg_plus(progressBar,progress)
################################################################################
# Get data from stop_reason
################################################################################
tryCatch({
  conditiontbl_stop <- queryRender(condition_sql[6],"condition_occurrence")
}, error = function(e) {
  conditiontbl_stop <- NULL
})
    progress <- prg_plus(progressBar,progress)
################################################################################
# Get data from visit_occurrence_id
################################################################################
tryCatch({
  conditiontbl_visit_occurrence <- queryRender(condition_sql[7],"condition_occurrence", "visit_occurrence_id", comparedAttName = "condition_occurrence_id")
}, error = function(e) {
  conditiontbl_visit_occurrence <- NULL
})
    progress <- prg_plus(progressBar,progress)
################################################################################
# Get data from visit_detail_id
# NO data in NHIS
################################################################################
tryCatch({
  conditiontbl_visit_detail <- queryRender(condition_sql[8],"condition_occurrence", "visit_detail_id", comparedAttName = "condition_occurrence_id")
}, error = function(e) {
  conditiontbl_visit_detail <- NULL
})
    progress <- prg_plus(progressBar,progress)
    condition_tbl <- list(conditiontbl_record, conditiontbl_person_ratio, conditiontbl_diff_date, conditiontbl_start, conditiontbl_end,
                          conditiontbl_type_concept, conditiontbl_stop, conditiontbl_visit_occurrence, conditiontbl_visit_detail)
    close(progressBar)
    return(condition_tbl)
}

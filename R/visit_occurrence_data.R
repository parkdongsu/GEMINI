#' visit occurrence data
#'
#' This function extract data from visit occurrence table
#' @keywords gemini
#' @export
#'
visit_occurrence_data <- function(){
    visit_sql<- splitSql(readSql(paste0(.libPaths()[1],"/gemini/extdata/Visit_occurrence_query.sql")))
    if(connection@dbms == 'sql server'){
        visit_record_sql <- splitSql(readSql(paste0(.libPaths()[1],"/gemini/extdata/System_query.sql")))[1]
    }else if(connection@dbms == 'postgresql'){
        visit_record_sql <- splitSql(readSql(paste0(.libPaths()[1],"/gemini/extdata/System_query.sql")))[2]
    }
    progressBar <- utils::txtProgressBar(max=length(visit_sql)+1,style=3)
    progress = 0
    prg_plus <- function(prgBar,prg){
        prg = prg + 1
        utils::setTxtProgressBar(prgBar, prg)
        return(prg)
    }
################################################################################
# Get data from visit_occurrence_id
# If no data, value have Null to check No Data
################################################################################
tryCatch(visittbl_record <- queryRender(visit_record_sql,"visit_occurrence")
  ,
  error = function(e) {
    visittbl_record <- NULL
  }
)
    progress <- prg_plus(progressBar,progress)
################################################################################
# Get data from person_id
################################################################################
tryCatch(visittbl_person_ratio <- queryRender(visit_sql[1],"visit_occurrence")
  ,
  error = function(e) {
    visittbl_person_ratio <- NULL
  }
)
    progress <- prg_plus(progressBar,progress)
################################################################################
# Get data from visit_concept_id
################################################################################
tryCatch(visittbl_visit_concept <- queryRender(visit_sql[2],"visit_occurrence", "visit_concept_id")
  ,
  error = function(e) {
    visittbl_visit_concept <- NULL
  }
)
    progress <- prg_plus(progressBar,progress)
################################################################################
# Get data from visit_start_date
################################################################################
tryCatch(visittbl_start <- queryRender(visit_sql[3],"visit_occurrence", "visit_start_date")
  ,
  error = function(e) {
    visittbl_start <- NULL
  }
)
    progress <- prg_plus(progressBar,progress)
################################################################################
# Get data from visit_end_date
################################################################################
tryCatch(visittbl_end <- queryRender(visit_sql[4],"visit_occurrence", "visit_end_date")
  ,
  error = function(e) {
    visittbl_end <- NULL
  }
)
    progress <- prg_plus(progressBar,progress)
################################################################################
# day diff
################################################################################
tryCatch(visittbl_diff_date <- queryRender(visit_sql[5],"visit_occurrence", startName = "visit_start_date", endName = "visit_end_date")
  ,
  error = function(e) {
    visittbl_diff_date <- NULL
  }
)
    progress <- prg_plus(progressBar,progress)
################################################################################
# sd Graph????
################################################################################
tryCatch({
  visittbl_count <- queryRender(visit_sql[6], endName = "visit_end_date")
}
,
error = function(e) {
  visittbl_count <- NULL
}
)
    progress <- prg_plus(progressBar,progress)
################################################################################
# Get data from visit_type_concept_id
################################################################################
tryCatch(visittbl_type_concept <- queryRender(visit_sql[7],"visit_occurrence", "visit_type_concept_id")
  ,
  error = function(e) {
    visittbl_type_concept <- NULL
  }
)
    progress <- prg_plus(progressBar,progress)
################################################################################
# Get data from care_site_id
################################################################################
tryCatch(visittbl_care_site <- queryRender(visit_sql[8],"visit_occurrence", "care_site_id")
  ,
  error = function(e) {
    visittbl_care_site <- NULL
  }
)
    progress <- prg_plus(progressBar,progress)
################################################################################
# Get data from visit_source_concept_id
################################################################################
tryCatch(visittbl_source_concept <- queryRender(visit_sql[9],"visit_occurrence", "visit_source_concept_id")
  ,
  error = function(e) {
    visittbl_source_concept <- NULL
  }
)
    progress <- prg_plus(progressBar,progress)
################################################################################
# Get data from admitting_source_concept_id
# No data in NHIS, So it will be NULL
################################################################################
tryCatch(visittbl_admitting_source <- queryRender(visit_sql[10],"visit_occurrence", "admitting_source_concept_id")
,
error = function(e) {
  visittbl_admitting_source <- NULL
}
)
    progress <- prg_plus(progressBar,progress)
################################################################################
# Get data from discharge_to_concept_id
# No data in NHIS
################################################################################
tryCatch(visittbl_discharge <- queryRender(visit_sql[11],"visit_occurrence", "discharge_to_concept_id")
  ,
  error = function(e) {
    visittbl_discharge <- NULL

})
    progress <- prg_plus(progressBar,progress)
################################################################################
# Get data from preceding_visit_occurrence_id
# No data in NHIS
################################################################################
tryCatch(visittbl_preceding <- queryRender(visit_sql[12],"visit_occurrence", "preceding_visit_occurrence_id", comparedAttName = "visit_occurrence_id")
  ,
  error = function(e) {
    visittbl_preceding <- NULL
})
    progress <- prg_plus(progressBar,progress)
    visit_tbl <- list(visittbl_record, visittbl_person_ratio, visittbl_visit_concept, visittbl_start, visittbl_end, visittbl_diff_date, visittbl_count,
                      visittbl_type_concept, visittbl_care_site, visittbl_source_concept, visittbl_admitting_source, visittbl_discharge, visittbl_preceding)
    close(progressBar)
    return(visit_tbl)
}

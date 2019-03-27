#' visit occurrence data
#'
#' This function extract data from visit occurrence table
#' @keywords gemini
#' @export
#'
visit_occurrence_data <- function(){
################################################################################
# Get data from visit_occurrence_id
# If no data, value have Null to check No Data
################################################################################
tryCatch(visittbl_record <<- get_total_records("visit_occurrence")
  ,
  error = function(e) {
    visittbl_record <<- NULL
  }
)
################################################################################
# Get data from person_id
################################################################################
tryCatch(visittbl_person_ratio <<- get_person_ratio("visit_occurrence")
  ,
  error = function(e) {
    visittbl_person_ratio <<- NULL
  }
)
################################################################################
# Get data from visit_concept_id
################################################################################
tryCatch(visittbl_visit_concept <<- get_ratio("visit_occurrence", "visit_concept_id")
  ,
  error = function(e) {
    visittbl_visit_concept <<- NULL
  }
)
################################################################################
# Get data from visit_start_date
################################################################################
tryCatch(visittbl_start <<- get_record_per_year("visit_occurrence", "visit_start_date")
  ,
  error = function(e) {
    visittbl_start <<- NULL
  }
)
################################################################################
# Get data from visit_end_date
################################################################################
tryCatch(visittbl_end <<- get_record_per_year("visit_occurrence", "visit_end_date")
  ,
  error = function(e) {
    visittbl_end <<- NULL
  }
)
################################################################################
# day diff
################################################################################
tryCatch(visittbl_diff_date <<- get_diff_year("visit_occurrence", "visit_start_date", "visit_end_date")
  ,
  error = function(e) {
    visittbl_diff_date <<- NULL
  }
)
################################################################################
# sd Graph????
################################################################################
tryCatch({
  sql <<- "select	YEAR(visit_end_date) as visit_year, count(person_id) as person_count FROM @cdm_database_schema.visit_occurrence
GROUP BY YEAR(visit_end_date)"
  visittbl_count <<- queryRender(sql)
  # sql <- SqlRender::renderSql(sql, cdm_database_schema = cdmDatabaseSchema)$sql
  # sql <- SqlRender::translateSql(sql, targetDialect = attr(connection, "dbms"))$sql
  # visittbl_count <- DatabaseConnector::querySql(connection, sql)
  # colnames(visittbl_count) <- SqlRender::snakeCaseToCamelCase(colnames(visittbl_count))
}
,
error = function(e) {
  visittbl_count <<- NULL
}
)
################################################################################
# Get data from visit_type_concept_id
################################################################################
tryCatch(visittbl_type_concept <<- get_ratio("visit_occurrence", "visit_type_concept_id")
  ,
  error = function(e) {
    visittbl_type_concept <<- NULL
  }
)
################################################################################
# Get data from care_site_id
################################################################################
tryCatch(visittbl_care_site <<- get_null_ratio("visit_occurrence", "care_site_id")
  ,
  error = function(e) {
    visittbl_care_site <<- NULL
  }
)
################################################################################
# Get data from visit_source_concept_id
################################################################################
tryCatch(visittbl_source_concept <<- get_ratio("visit_occurrence", "visit_source_concept_id")
  ,
  error = function(e) {
    visittbl_source_concept <<- NULL
  }
)
################################################################################
# Get data from admitting_source_concept_id
# No data in NHIS, So it will be NULL
################################################################################
tryCatch(visittbl_admitting_source <<- get_ratio("visit_occurrence", "admitting_source_concept_id")
,
error = function(e) {
  visittbl_admitting_source <<- NULL
}
)
################################################################################
# Get data from discharge_to_concept_id
# No data in NHIS
################################################################################
tryCatch(visittbl_discharge <<- get_ratio("visit_occurrence", "discharge_to_concept_id")
  ,
  error = function(e) {
    visittbl_discharge <<- NULL

})
################################################################################
# Get data from preceding_visit_occurrence_id
# No data in NHIS
################################################################################
tryCatch(visittbl_preceding <<- get_compared_ratio("visit_occurrence", "preceding_visit_occurrence_id", "visit_occurrence_id")
  ,
  error = function(e) {
    visittbl_preceding <<- NULL
})
}

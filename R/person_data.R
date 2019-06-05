#' Person data
#'
#' This function extract data from person table
#' @keywords gemini
#' @export
#'
person_data <- function(){
    person_sql<- splitSql(readSql(paste0(.libPaths()[1],"/gemini/data/extdata/Person_query.sql")))
    if(connection@dbms == 'sql server'){
        person_record_sql <- splitSql(readSql(paste0(.libPaths()[1],"/gemini/data/extdata/System_query.sql")))[1]
    }else if(connection@dbms == 'postgresql'){
        person_record_sql <- splitSql(readSql(paste0(.libPaths()[1],"/gemini/data/extdata/System_query.sql")))[2]
    }
################################################################################
# Calculate records ratio
# Query to find all table records count info, target table record and calculate with it
################################################################################

tryCatch(persontbl_record <- queryRender(person_record_sql,"PERSON")
  ,
  error = function(e) {
    persontbl_record <- NULL
  }
)
################################################################################
# Calculate person ratio
# In person Table, It will be 100%. Unless it should be error
################################################################################
tryCatch(persontbl_person_ratio <- queryRender(person_sql[1],"PERSON")
  ,
  error = function(e) {
    persontbl_person_ratio <- NULL
  }
)
################################################################################
# Using gender_concept_id, make plot
# In bar chart, focus on Male, Female to easy compare
################################################################################
tryCatch({
  persontbl_gender <- queryRender(person_sql[2],"PERSON","gender_concept_id")
},
error = function(e) {
  persontbl_gender <- NULL
}
)
################################################################################
# Extract data from person, observation_period to calculate ratio
# In query, Get person_ratio, age_range, gender_concept_id
# In R, Null append, 0.0 to 0.1 converted
# Query Year band with person
################################################################################
tryCatch({
persontbl_min_age <- queryRender(person_sql[3])
  # value 0.0 to 0.01 that is not no data. just too small to ceiling
  options(scipen = 9999)
  persontbl_min_age <- addNullGender(persontbl_min_age)
  persontbl_min_age <- zeroToDecimal(persontbl_min_age)

}, error = function(e) {
  persontbl_min_age <- NULL
})
################################################################################
# OBSERVATION_PERIOD end_date
# Same work
################################################################################
tryCatch({
persontbl_max_age <- queryRender(person_sql[4])
  # value 0.0 to 0.01 that is not no data. just too small to ceiling
  # options(scipen = 9999)
  persontbl_max_age <- addNullGender(persontbl_max_age)
  persontbl_max_age <- zeroToDecimal(persontbl_max_age)
  male_max_ratio <- persontbl_max_age$ratio[persontbl_max_age$genderConceptId == "8507"]
  female_max_ratio <- persontbl_max_age$ratio[persontbl_max_age$genderConceptId == "8532"]
  x_max_lbl <- persontbl_max_age$ageRange[persontbl_max_age$genderConceptId == "8507"]
}, error = function(e) {
  persontbl_max_age <- NULL
})
################################################################################
# Get data from race_concept_id
################################################################################
tryCatch(persontbl_race <- queryRender(person_sql[5],"person", "race_concept_id")
  ,
  error = function(e) {
    persontbl_race <- NULL
  }
)
################################################################################
# Get data from ethnicity_concept_id
################################################################################
tryCatch(persontbl_ethnicity <- queryRender(person_sql[6],"person", "ethnicity_concept_id")
  ,
  error = function(e) {
    persontbl_ethnicity <- NULL
  }
)
################################################################################
# Get data from location_id
# NULL ratio
################################################################################
tryCatch(persontbl_location <- queryRender(person_sql[7],"person", "location_id")
  ,
  error = function(e) {
    persontbl_location <- NULL
  }
)
################################################################################
# Get data from provider_id
################################################################################
tryCatch(persontbl_provider <- queryRender(person_sql[8],"person", "provider_id")
  ,
  error = function(e) {
    persontbl_provider <- NULL
  }
)
################################################################################
# Get data from care_site_id
################################################################################
tryCatch(
  persontbl_care_site <- queryRender(person_sql[9])
, error = function(e) {
  persontbl_care_site <- NULL
})
    person_tbl <- list(persontbl_record, persontbl_person_ratio, persontbl_gender, persontbl_min_age, persontbl_max_age, persontbl_race, persontbl_ethnicity,
                       persontbl_location, persontbl_location, persontbl_provider)
    return(person_tbl)
}

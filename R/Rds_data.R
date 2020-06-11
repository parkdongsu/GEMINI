queryRender <- function(sqlquery, tblName = "", AttName = "", comparedAttName = "", startName = "", endName = "", conn) {
    # Unset parameter call warning message
    options(warn = -1)
    sql <- SqlRender::renderSql(sqlquery,
                                cdm_database_schema = connectionDetails$schema,
                                tbl_name = tblName,
                                att_name = AttName,
                                compared_att_name = comparedAttName,
                                start_name = startName, end_name = endName
    )$sql
    options(warn = 1)
    sql <- SqlRender::translate(sql, targetDialect = attr(conn, "dbms"))
    sql <- gsub("\r|\t|\n"," ", sql)
    temp <- DatabaseConnector::querySql(conn, sql)
    # Colname change
    colnames(temp) <- SqlRender::snakeCaseToCamelCase(colnames(temp))

    return(temp)
}

#0 values get little numeric data for Pie3D...
zeroToDecimal <- function(attName){
    attName$ratio <- sapply(attName$ratio, function(x) if (x == 0.0 || x == 0){
        x <- 0.001
    }else{
        (x)
    }
    )
    return(attName)
}

#Append zero data frame
addNullGender <- function(attName){
    i <- 1
    while(i != nrow(attName) + 1){
        if (attName$genderConceptId[i] == "8507" && attName$genderConceptId[i + 1] != "8532") {
            temp <- c(attName$ageRange[i], "8532", 0)
            attName <- rbind(attName[c(1:i), ], temp, attName[c(i + 1:nrow(attName)), ])
        }
        else if (attName$genderConceptId[i] == "8532" && (attName$genderConceptId[i-1] != "8507" || identical(attName$genderConceptId[i-1],numeric(0)))) {
            temp <- c(attName$ageRange[i], "8507", 0)
            attName <- rbind(attName[c(1:i - 1), ], temp, attName[c(i:nrow(attName)), ])
        }
        i <- i + 1
    }
    return(attName)
}


#' Person data
#'
#' This function extract data from person table
#' @keywords gemini
#' @export
#'
person_data <- function(connection){
    person_sql<- splitSql(readSql(paste0(.libPaths()[1],"/gemini/extdata/Person_query.sql")))
    if(attr(connection, "dbms") == 'sql server'){
        person_record_sql <- splitSql(readSql(paste0(.libPaths()[1],"/gemini/extdata/System_query.sql")))[1]
    }else if(attr(connection, "dbms") == 'postgresql'){
        person_record_sql <- splitSql(readSql(paste0(.libPaths()[1],"/gemini/extdata/System_query.sql")))[2]
    }
    progressBar <- utils::txtProgressBar(max=length(person_sql)+1,style=3)
    progress = 0
    prg_plus <- function(prgBar,prg){
        prg = prg + 1
        utils::setTxtProgressBar(prgBar, prg)
        return(prg)
    }
    ################################################################################
    # Calculate records ratio
    # Query to find all table records count info, target table record and calculate with it
    ################################################################################

    tryCatch(persontbl_record <- queryRender(person_record_sql,"PERSON",conn = connection)
             ,
             error = function(e) {
                 persontbl_record <- NULL
             }
    )
    progress <- prg_plus(progressBar,progress)

    ################################################################################
    # Calculate person ratio
    # In person Table, It will be 100%. Unless it should be error
    ################################################################################
    tryCatch(persontbl_person_ratio <- queryRender(person_sql[1],"PERSON",conn = connection)
             ,
             error = function(e) {
                 persontbl_person_ratio <- NULL
             }
    )
    progress <- prg_plus(progressBar,progress)
    ################################################################################
    # Using gender_concept_id, make plot
    # In bar chart, focus on Male, Female to easy compare
    ################################################################################
    tryCatch({
        persontbl_gender <- queryRender(person_sql[2],"PERSON","gender_concept_id",conn = connection)
    },
    error = function(e) {
        persontbl_gender <- NULL
    }
    )
    progress <- prg_plus(progressBar,progress)
    ################################################################################
    # Extract data from person, observation_period to calculate ratio
    # In query, Get person_ratio, age_range, gender_concept_id
    # In R, Null append, 0.0 to 0.1 converted
    # Query Year band with person
    ################################################################################
    tryCatch({
        persontbl_min_age <- queryRender(person_sql[3],conn = connection)
        # value 0.0 to 0.01 that is not no data. just too small to ceiling
        options(scipen = 9999)
        persontbl_min_age <- addNullGender(persontbl_min_age)
        persontbl_min_age <- zeroToDecimal(persontbl_min_age)

    }, error = function(e) {
        persontbl_min_age <- NULL
    })
    progress <- prg_plus(progressBar,progress)
    ################################################################################
    # OBSERVATION_PERIOD end_date
    # Same work
    ################################################################################
    tryCatch({
        persontbl_max_age <- queryRender(person_sql[4],conn = connection)
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
    progress <- prg_plus(progressBar,progress)
    ################################################################################
    # Get data from race_concept_id
    ################################################################################
    tryCatch(persontbl_race <- queryRender(person_sql[5],"person", "race_concept_id",conn = connection)
             ,
             error = function(e) {
                 persontbl_race <- NULL
             }
    )
    progress <- prg_plus(progressBar,progress)
    ################################################################################
    # Get data from ethnicity_concept_id
    ################################################################################
    tryCatch(persontbl_ethnicity <- queryRender(person_sql[6],"person", "ethnicity_concept_id",conn = connection)
             ,
             error = function(e) {
                 persontbl_ethnicity <- NULL
             }
    )
    progress <- prg_plus(progressBar,progress)
    ################################################################################
    # Get data from location_id
    # NULL ratio
    ################################################################################
    tryCatch(persontbl_location <- queryRender(person_sql[7],"person", "location_id",conn = connection)
             ,
             error = function(e) {
                 persontbl_location <- NULL
             }
    )
    progress <- prg_plus(progressBar,progress)
    ################################################################################
    # Get data from provider_id
    ################################################################################
    tryCatch(persontbl_provider <- queryRender(person_sql[8],"person", "provider_id",conn = connection)
             ,
             error = function(e) {
                 persontbl_provider <- NULL
             }
    )
    progress <- prg_plus(progressBar,progress)
    ################################################################################
    # Get data from care_site_id
    ################################################################################
    tryCatch(
        persontbl_care_site <- queryRender(person_sql[9],conn = connection)
        , error = function(e) {
            persontbl_care_site <- NULL
        })

    progress <- prg_plus(progressBar,progress)

    person_tbl <- list(persontbl_record, persontbl_person_ratio, persontbl_gender, persontbl_min_age, persontbl_max_age, persontbl_race, persontbl_ethnicity,
                       persontbl_location, persontbl_provider, persontbl_care_site)
    #error part
    return(person_tbl)
    close(progress_bar)
}

#' Death data
#'
#' This function for extract data from death table
#' @keywords gemini
#' @export
#'
death_data <- function(connection){
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
        deathtbl_check <- queryRender(death_sql[1],conn = connection)
    }, error = function(e) {
        deathtbl_check <- NULL
    })
    progress <- prg_plus(progressBar,progress)
    ################################################################################
    # Get data from death_type_concept_id to check death type.
    ################################################################################
    tryCatch({
        deathtbl_type <- queryRender(death_sql[2],conn = connection)
    }, error = function(e) {
        deathtbl_type <- NULL
    })
    progress <- prg_plus(progressBar,progress)
    death_tbl <- list(deathtbl_check,deathtbl_type)
    return(death_tbl)
    close(progressBar)
}


#' visit occurrence data
#'
#' This function extract data from visit occurrence table
#' @keywords gemini
#' @export
#'
visit_occurrence_data <- function(connection){
    visit_sql<- splitSql(readSql(paste0(.libPaths()[1],"/gemini/extdata/Visit_occurrence_query.sql")))
    if(attr(connection, "dbms") == 'sql server'){
        visit_record_sql <- splitSql(readSql(paste0(.libPaths()[1],"/gemini/extdata/System_query.sql")))[1]
    }else if(attr(connection, "dbms") == 'postgresql'){
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
    tryCatch(visittbl_record <- queryRender(visit_record_sql,"visit_occurrence",conn = connection)
             ,
             error = function(e) {
                 visittbl_record <- NULL
             }
    )
    progress <- prg_plus(progressBar,progress)
    ################################################################################
    # Get data from person_id
    ################################################################################
    tryCatch(visittbl_person_ratio <- queryRender(visit_sql[1],"visit_occurrence",conn = connection)
             ,
             error = function(e) {
                 visittbl_person_ratio <- NULL
             }
    )
    progress <- prg_plus(progressBar,progress)
    ################################################################################
    # Get data from visit_concept_id
    ################################################################################
    tryCatch(visittbl_visit_concept <- queryRender(visit_sql[2],"visit_occurrence", "visit_concept_id",conn = connection)
             ,
             error = function(e) {
                 visittbl_visit_concept <- NULL
             }
    )
    progress <- prg_plus(progressBar,progress)
    ################################################################################
    # Get data from visit_start_date
    ################################################################################
    tryCatch(visittbl_start <- queryRender(visit_sql[3],"visit_occurrence", "visit_start_date",conn = connection)
             ,
             error = function(e) {
                 visittbl_start <- NULL
             }
    )
    progress <- prg_plus(progressBar,progress)
    ################################################################################
    # Get data from visit_end_date
    ################################################################################
    tryCatch(visittbl_end <- queryRender(visit_sql[4],"visit_occurrence", "visit_end_date",conn = connection)
             ,
             error = function(e) {
                 visittbl_end <- NULL
             }
    )
    progress <- prg_plus(progressBar,progress)
    ################################################################################
    # day diff
    ################################################################################
    tryCatch(visittbl_diff_date <- queryRender(visit_sql[5],"visit_occurrence",
                                               startName = "visit_start_date", endName = "visit_end_date",conn = connection)
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
        visittbl_count <- queryRender(visit_sql[6], endName = "visit_end_date",conn = connection)
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
    tryCatch(visittbl_type_concept <- queryRender(visit_sql[7],"visit_occurrence", "visit_type_concept_id",conn = connection)
             ,
             error = function(e) {
                 visittbl_type_concept <- NULL
             }
    )
    progress <- prg_plus(progressBar,progress)
    ################################################################################
    # Get data from care_site_id
    ################################################################################
    tryCatch(visittbl_care_site <- queryRender(visit_sql[8],"visit_occurrence", "care_site_id",conn = connection)
             ,
             error = function(e) {
                 visittbl_care_site <- NULL
             }
    )
    progress <- prg_plus(progressBar,progress)
    ################################################################################
    # Get data from visit_source_concept_id
    ################################################################################
    tryCatch(visittbl_source_concept <- queryRender(visit_sql[9],"visit_occurrence", "visit_source_concept_id",conn = connection)
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
    tryCatch(visittbl_admitting_source <- queryRender(visit_sql[10],"visit_occurrence", "admitting_source_concept_id",conn = connection)
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
    tryCatch(visittbl_discharge <- queryRender(visit_sql[11],"visit_occurrence", "discharge_to_concept_id",conn = connection)
             ,
             error = function(e) {
                 visittbl_discharge <- NULL

             })
    progress <- prg_plus(progressBar,progress)
    ################################################################################
    # Get data from preceding_visit_occurrence_id
    # No data in NHIS
    ################################################################################
    tryCatch(visittbl_preceding <- queryRender(visit_sql[12],"visit_occurrence", "preceding_visit_occurrence_id",
                                               comparedAttName = "visit_occurrence_id",conn = connection)
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


#' Condition occurrence data
#'
#' This function extract data from condition occurrence table
#' @keywords gemini
#' @export
#'
condition_occurrence_data <- function(connection){
    condition_sql<- splitSql(readSql(paste0(.libPaths()[1],"/gemini/extdata/Condition_occurrence_query.sql")))
    if(attr(connection, "dbms") == 'sql server'){
        condition_record_sql <- splitSql(readSql(paste0(.libPaths()[1],"/gemini/extdata/System_query.sql")))[1]
    }else if(attr(connection, "dbms") == 'postgresql'){
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
    tryCatch(conditiontbl_record <- queryRender(condition_record_sql,"condition_occurrence",conn = connection)
             ,
             error = function(e) {
                 conditiontbl_record <- NULL
             }
    )
    progress <- prg_plus(progressBar,progress)
    ################################################################################
    # Get data from person_id
    ################################################################################
    tryCatch(conditiontbl_person_ratio <- queryRender(condition_sql[1],"condition_occurrence",conn = connection)
             ,
             error = function(e) {
                 conditiontbl_person_ratio <- NULL
             }
    )
    progress <- prg_plus(progressBar,progress)
    ################################################################################
    # Extract condition_start_date
    ################################################################################
    tryCatch(conditiontbl_start <- queryRender(condition_sql[2],"condition_occurrence", "condition_start_date",conn = connection)
             ,
             error = function(e) {
                 conditiontbl_start <- NULL
             }
    )
    progress <- prg_plus(progressBar,progress)
    ################################################################################
    # Extract condition_end_date
    ################################################################################
    tryCatch(conditiontbl_end <- queryRender(condition_sql[3],"condition_occurrence", "condition_end_date",conn = connection)
             ,
             error = function(e) {
                 conditiontbl_end <- NULL
             }
    )
    progress <- prg_plus(progressBar,progress)
    ################################################################################
    # Extract condition_start_date, condition_end_date for histogram
    ################################################################################
    tryCatch(conditiontbl_diff_date <- queryRender(condition_sql[4],"condition_occurrence",
                                                   startName = "condition_start_date", endName = "condition_end_date",conn = connection)
             ,
             error = function(e) {
                 conditiontbl_diff_date <- NULL
             }
    )
    progress <- prg_plus(progressBar,progress)
    ################################################################################
    # Get data from condition_type_concept_id
    ################################################################################
    tryCatch(conditiontbl_type_concept <- queryRender(condition_sql[5],"condition_occurrence", "condition_type_concept_id",conn = connection)
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
        conditiontbl_stop <- queryRender(condition_sql[6],"condition_occurrence",conn = connection)
    }, error = function(e) {
        conditiontbl_stop <- NULL
    })
    progress <- prg_plus(progressBar,progress)
    ################################################################################
    # Get data from visit_occurrence_id
    ################################################################################
    tryCatch({
        conditiontbl_visit_occurrence <- queryRender(condition_sql[7],"condition_occurrence", "visit_occurrence_id",
                                                     comparedAttName = "condition_occurrence_id",conn = connection)
    }, error = function(e) {
        conditiontbl_visit_occurrence <- NULL
    })
    progress <- prg_plus(progressBar,progress)
    ################################################################################
    # Get data from visit_detail_id
    # NO data in NHIS
    ################################################################################
    tryCatch({
        conditiontbl_visit_detail <- queryRender(condition_sql[8],"condition_occurrence", "visit_detail_id",
                                                 comparedAttName = "condition_occurrence_id",conn = connection)
    }, error = function(e) {
        conditiontbl_visit_detail <- NULL
    })
    progress <- prg_plus(progressBar,progress)
    condition_tbl <- list(conditiontbl_record, conditiontbl_person_ratio, conditiontbl_diff_date, conditiontbl_start, conditiontbl_end,
                          conditiontbl_type_concept, conditiontbl_stop, conditiontbl_visit_occurrence, conditiontbl_visit_detail)
    close(progressBar)
    return(condition_tbl)
}



#' Drug exposure data
#'
#' This function extract data from drug exposure table
#' @keywords gemini
#' @export
#'
drug_exposure_data <- function(connection){
    drug_exp_sql<- splitSql(readSql(paste0(.libPaths()[1],"/gemini/extdata/Drug_exposure_query.sql")))
    if(attr(connection, "dbms") == 'sql server'){
        drug_exp_record_sql <- splitSql(readSql(paste0(.libPaths()[1],"/gemini/extdata/System_query.sql")))[1]
    }else if(attr(connection, "dbms") == 'postgresql'){
        drug_exp_record_sql <- splitSql(readSql(paste0(.libPaths()[1],"/gemini/extdata/System_query.sql")))[2]
    }
    progressBar <- utils::txtProgressBar(max=length(drug_exp_sql)+1,style=3)
    progress = 0
    prg_plus <- function(prgBar,prg){
        prg = prg + 1
        utils::setTxtProgressBar(prgBar, prg)
        return(prg)
    }
    ################################################################################
    # Get data from drug_exposure_id
    ################################################################################
    tryCatch(drug_exptbl_record <- queryRender(drug_exp_record_sql,"drug_exposure",conn = connection)
             ,
             error = function(e) {
                 drug_exptbl_record <- NULL
             }
    )
    progress <- prg_plus(progressBar,progress)
    ################################################################################
    # Get data from person_id
    ################################################################################
    tryCatch(drug_exptbl_person_ratio <- queryRender(drug_exp_sql[1],"drug_exposure",conn = connection)
             ,
             error = function(e) {
                 drug_exptbl_person_ratio <- NULL
             }
    )
    progress <- prg_plus(progressBar,progress)
    ################################################################################
    # Extract drug_exposure_start_date
    ################################################################################
    tryCatch(drug_exptbl_start <- queryRender(drug_exp_sql[2],"drug_exposure", "drug_exposure_start_date",conn = connection)
             ,
             error = function(e) {
                 drug_exptbl_start <- NULL
             }
    )
    progress <- prg_plus(progressBar,progress)
    ################################################################################
    # Extract drug_exposure_end_date
    ################################################################################
    tryCatch(drug_exptbl_end <- queryRender(drug_exp_sql[3],"drug_exposure", "drug_exposure_end_date",conn = connection)
             ,
             error = function(e) {
                 drug_exptbl_end <- NULL
             }
    )
    progress <- prg_plus(progressBar,progress)
    ################################################################################
    # Get data from drug_exposure_diff_date
    ################################################################################
    tryCatch({
        drug_exptbl_diff_date <- queryRender(drug_exp_sql[4],"drug_exposure",
                                             startName = "drug_exposure_start_date", endName = "drug_exposure_end_date",conn = connection)
    }, error = function(e) {
        drug_exptbl_diff_date <- NULL
    })
    progress <- prg_plus(progressBar,progress)
    ################################################################################
    # Get data from drug_type_concept_id
    ################################################################################
    tryCatch({
        drug_exptbl_type_concept <- queryRender(drug_exp_sql[5],"drug_exposure", "drug_type_concept_id",conn = connection)
    }, error = function(e) {
        drug_exptbl_type_concept <- NULL
    })
    progress <- prg_plus(progressBar,progress)
    ################################################################################
    # Get data from stop_reason
    ################################################################################
    tryCatch({
        drug_exptbl_stop <- queryRender(drug_exp_sql[6],"drug_exposure",conn = connection)
    }, error = function(e) {
        drug_exptbl_stop <- NULL
    })
    progress <- prg_plus(progressBar,progress)
    ################################################################################
    # Get data from route_concept_id
    ################################################################################
    tryCatch({
        drug_exptbl_route <- queryRender(drug_exp_sql[7],"drug_exposure", "route_concept_id",conn = connection)
    }, error = function(e) {
        drug_exptbl_route <- NULL
    })
    progress <- prg_plus(progressBar,progress)
    ################################################################################
    # Get data from visit_occurrence_id
    ################################################################################
    tryCatch({
        drug_exptbl_visit_occurrence <- queryRender(drug_exp_sql[8],"drug_exposure", "visit_occurrence_id",
                                                    comparedAttName = "drug_exposure_id",conn = connection)
    }, error = function(e) {
        drug_exptbl_visit_occurrence <- NULL
    })
    progress <- prg_plus(progressBar,progress)
    ################################################################################
    # Get data from visit_detail_id
    # No 'visit_detail_id' data in drug_exposure table
    # drug_exposure_visit_detail <- get_compared_ratio("drug_exposure","visit_detail_id","drug_exposure_id")
    ################################################################################
    tryCatch({
        drug_exptbl_visit_detail <- queryRender(drug_exp_sql[9],"drug_exposure", "visit_detail_id",
                                                comparedAttName = "drug_exposure_id",conn = connection)
    }, error = function(e) {
        drug_exptbl_visit_detail <- NULL
    })
    progress <- prg_plus(progressBar,progress)
    drug_exptbl <- list(drug_exptbl_record, drug_exptbl_person_ratio, drug_exptbl_diff_date, drug_exptbl_start, drug_exptbl_end, drug_exptbl_type_concept,
                        drug_exptbl_stop, drug_exptbl_route, drug_exptbl_visit_occurrence)
    close(progressBar)
    return(drug_exptbl)
}


#' Draw drug era
#'
#' This function for draw graph from drug era RDS data
#' @keywords gemini
#' @export
#'
################################################################################
# DRUG ERA TABLE VISUALLIZATION
################################################################################
#' Drug era data
#'
#' This function extract data from drug era table
#' @keywords gemini
#' @export
#'
drug_era_data <- function(connection){
    drug_era_sql<- splitSql(readSql(paste0(.libPaths()[1],"/gemini/extdata/Drug_era_query.sql")))
    if(attr(connection, "dbms") == 'sql server'){
        drug_era_record_sql <- splitSql(readSql(paste0(.libPaths()[1],"/gemini/extdata/System_query.sql")))[1]
    }else if(attr(connection, "dbms") == 'postgresql'){
        drug_era_record_sql <- splitSql(readSql(paste0(.libPaths()[1],"/gemini/extdata/System_query.sql")))[2]
    }
    progressBar <- utils::txtProgressBar(max=length(drug_era_sql)+1,style=3)
    progress = 0
    prg_plus <- function(prgBar,prg){
        prg = prg + 1
        utils::setTxtProgressBar(prgBar, prg)
        return(prg)
    }
    ################################################################################
    # Get data from drug_era_id
    ################################################################################
    tryCatch(drug_eratbl_record <- queryRender(drug_era_record_sql,"drug_era",conn = connection)
             ,
             error = function(e) {
                 drug_eratbl_record <- NULL
             }
    )
    progress <- prg_plus(progressBar,progress)
    ################################################################################
    # Get data from person_id
    ################################################################################
    tryCatch(drug_eratbl_person_ratio <- queryRender(drug_era_sql[1],"drug_era",conn = connection)
             ,
             error = function(e) {
                 drug_eratbl_person_ratio <- NULL
             }
    )
    progress <- prg_plus(progressBar,progress)
    ################################################################################
    # Extract drug_era_start_date
    ################################################################################
    tryCatch(drug_eratbl_start <- queryRender(drug_era_sql[2],"drug_era", "drug_era_start_date",conn = connection)
             ,
             error = function(e) {
                 drug_eratbl_start <- NULL
             }
    )
    progress <- prg_plus(progressBar,progress)
    ################################################################################
    # Extract drug_era_end_date
    ################################################################################
    tryCatch(drug_eratbl_end <- queryRender(drug_era_sql[3],"drug_era", "drug_era_end_date",conn = connection)
             ,
             error = function(e) {
                 drug_eratbl_end <- NULL
             }
    )
    progress <- prg_plus(progressBar,progress)
    ################################################################################
    # Get data from drug_era_diff_date
    ################################################################################
    tryCatch({
        drug_eratbl_diff_date <- queryRender(drug_era_sql[4],"drug_era",
                                             startName = "drug_era_start_date", endName = "drug_era_end_date",conn = connection)
    }, error = function(e) {
        drug_eratbl_diff_date <- NULL
    })
    progress <- prg_plus(progressBar,progress)
    ################################################################################
    # Get data from drug_exposure_count
    #
    ################################################################################
    tryCatch({
        drug_eratbl_exp_count <- queryRender(drug_era_sql[5],"drug_era", "drug_exposure_count",conn = connection)
    }, error = function(e) {
        drug_eratbl_exp_count <- NULL
    })
    progress <- prg_plus(progressBar,progress)
    ################################################################################
    # Get data from gap_day
    # hist
    ################################################################################
    tryCatch({
        drug_eratbl_gap_days <- queryRender(drug_era_sql[6],"drug_era",conn = connection)
    }, error = function(e) {
        drug_eratbl_gap_days <- NULL
    })
    progress <- prg_plus(progressBar,progress)
    drug_eratbl <- list(drug_eratbl_record, drug_eratbl_person_ratio, drug_eratbl_diff_date, drug_eratbl_start, drug_eratbl_end,
                        drug_eratbl_exp_count, drug_eratbl_gap_days)
    close(progressBar)
    return(drug_eratbl)
}

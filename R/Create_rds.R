#' Create rds file
#'
#' This function for creating server info files and trigger of create rds files
#' @keywords gemini
#' @export
#'
#' @import DatabaseConnector
#' @import SqlRender
create_rds<- function(connectionDetails, work_dir,schema_name){
    cat("Set directory to create rds files.\n")
    connection <- DatabaseConnector::connect(connectionDetails)
    tryCatch(save_data(connection, work_dir, schema_name),
             error = function(e){
                 print(e)
                 disconnect(connection)
             })
}

save_data <- function(connection, workDir, schemaName){
    dir.create(file.path(workDir, "Gemini RDS"), showWarnings = FALSE)
    dir.create(file.path(workDir, "Gemini RDS",schemaName), showWarnings = FALSE)
    workDir <- file.path(workDir, "Gemini RDS",schemaName,'/')
    table_name <- c('person','death','visit_occurrence','condition_occurrence','drug_exposure','drug_era')
    process_time <- sapply(table_name, function(x){extract_cdm(connection,workDir,x)})

    ################################################################################
    # File Saving
    ################################################################################
    
    # zip(zipfile = paste0(workDir,schemaName,".zip"),
    #     files = paste0(workDir,list.files(path = workDir,pattern = "\\w*.rds$")), flag= c("-j", "-r"))

    # if(length(list.files(path = file.path(workDir,schemaName), pattern = "\\w.zip$"))>0){
    #     file.remove(paste0(workDir,list.files(path = workDir, pattern = "\\w.rds$")))
    # }
    # time check
    cat(paste0("RDS files created.\nThis process takes ", sum(process_time), "s.\n"))
    ################################################################################
    # Disconnect DB
    ################################################################################
    DatabaseConnector::disconnect(connection)
}


extract_cdm <- function(connection, workDir, tableName){
    cat(paste("\n",tableName,"data extracting...\n"))
    switch(tableName,
           'person' = {
               tm <- as.numeric(round(system.time(persontbl <- person_data(connection))[3], digit = 1))
               rds_maker(workDir,persontbl,tableName)
           },
           'death' = {
               tm <- as.numeric(round(system.time(deathtbl <- death_data(connection))[3], digit = 1))
               rds_maker(workDir,deathtbl,tableName)
           },
           'visit_occurrence' = {
               tm <- as.numeric(round(system.time(visittbl <- visit_occurrence_data(connection))[3], digit = 1))
               rds_maker(workDir,visittbl,tableName)
           },
           'condition_occurrence' = {
               tm <- as.numeric(round(system.time(conditiontbl <- condition_occurrence_data(connection))[3], digit = 1))
               rds_maker(workDir,conditiontbl,tableName)
           },
           'drug_exposure' = {
               tm <- as.numeric(round(system.time(exptbl <- drug_exposure_data(connection))[3], digit = 1))
               rds_maker(workDir,exptbl,tableName)
           },
           'drug_era' = {
               tm <- as.numeric(round(system.time(eratbl <- drug_era_data(connection))[3], digit = 1))
               rds_maker(workDir,eratbl,tableName)
           }
    )
    return(tm)
}
rds_maker <- function(workDir,tbl,tableName){
    switch (tableName,
            "person" =  {
                persontbl_name_list <- c(
                    "persontbl_record", "persontbl_person_ratio", "persontbl_gender", "persontbl_min_age", "persontbl_max_age",
                    "persontbl_race", "persontbl_ethnicity", "persontbl_location", "persontbl_provider", "persontbl_care_site"
                )

                data_list <- tbl
                rds_path <- sapply(FUN = paste0, workDir,persontbl_name_list, ".rds")
            },
            "death" = {
                deathtbl_name_list <- c("deathtbl_check", "deathtbl_type")

                data_list <- tbl
                rds_path <- sapply(FUN = paste0, workDir, deathtbl_name_list, ".rds")
            },
            "visit_occurrence" = {
                visittbl_name_list <- list(
                    "visittbl_record", "visittbl_person_ratio", "visittbl_visit_concept", "visittbl_start", "visittbl_end",
                    "visittbl_diff_date", "visittbl_count", "visittbl_type_concept", "visittbl_care_site", "visittbl_source_concept",
                    "visittbl_admitting_source", "visittbl_discharge", "visittbl_preceding"
                )

                data_list <- tbl
                rds_path <- sapply(FUN = paste0, workDir, visittbl_name_list, ".rds")
            },
            "condition_occurrence" = {
                conditiontbl_name_list <- list(
                    "conditiontbl_record", "conditiontbl_person_ratio", "conditiontbl_diff_date", "conditiontbl_start",
                    "conditiontbl_end", "conditiontbl_type_concept", "conditiontbl_stop", "conditiontbl_visit_occurrence",
                    "conditiontbl_visit_detail"
                )

                data_list <- tbl
                rds_path <- sapply(FUN = paste0, workDir, conditiontbl_name_list, ".rds")
            },
            "drug_exposure" = {
                drug_exptbl_name_list <- list(
                    "drug_exptbl_record", "drug_exptbl_person_ratio", "drug_exptbl_diff_date", "drug_exptbl_start", "drug_exptbl_end",
                    "drug_exptbl_type_concept", "drug_exptbl_stop", "drug_exptbl_route", "drug_exptbl_visit_occurrence"
                )

                data_list <- tbl
                rds_path <- sapply(FUN = paste0, workDir, drug_exptbl_name_list, ".rds")
            },
            "drug_era" = {
                drug_eratbl_name_list <- list(
                    "drug_eratbl_record", "drug_eratbl_person_ratio", "drug_eratbl_diff_date", "drug_eratbl_start", "drug_eratbl_end",
                    "drug_eratbl_exp_count", "drug_eratbl_gap_days"
                )

                data_list <- tbl
                rds_path <- sapply(FUN = paste0, workDir, drug_eratbl_name_list, ".rds")
            }
    )
    mapply(saveRDS, object = data_list, file = rds_path)
}

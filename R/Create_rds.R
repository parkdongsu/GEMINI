#' Create rds file
#'
#' This function for creating server info files and trigger of create rds files
#' @keywords gemini
#' @export
#'
#' @import DatabaseConnector
#' @import SqlRender
create_rds<- function(connectionDetails){
    cat("Set directory to create rds files.\n")
    # Change working directory for confirming user to create where the files
    if(Sys.info()[[1]]=='Windows'){
        work_dir <- choose.dir()
    }
    else{
        work_dir <- readline("Set work directory path : ")
    }
    connection <- DatabaseConnector::connect(connectionDetails)
    tryCatch(save_data(work_dir,connection), error = function(e){
        e
        disconnect(connection)
    })

}

save_data <- function(work_dir,connection){
    dir.create(file.path(work_dir, "Gemini RDS"), showWarnings = FALSE)
    work_dir <- file.path(work_dir, "Gemini RDS","/")
    ################################################################################
    # PERSON DATA SAVING FUNCTION
    ################################################################################1
    cat("Person data extracting...\n")
    tm1 <- as.numeric(round(system.time(persontbl <- person_data(connection))[3], digit = 1))
    rds_maker(work_dir,persontbl,"person")
    ################################################################################
    # DEATH DATA SAVING FUNCTION
    ################################################################################
    cat("\nDeath data extracting...\n")
    tm2 <- as.numeric(round(system.time(deathtbl <- death_data(connection))[3], digit = 1))
    rds_maker(work_dir,deathtbl,"death")
    ################################################################################
    # VISIT_OCCURRENCE DATA SAVING FUNCTION
    ################################################################################
    cat("\nVisit occurrence data extracting...\n")
    tm3 <- as.numeric(round(system.time(visittbl <- visit_occurrence_data(connection))[3], digit = 1))
    rds_maker(work_dir,visittbl,"visit")
    ################################################################################
    # CONDITION_OCCURRENCE DATA SAVING FUNCTION
    ################################################################################
    cat("\nCondition occurrence data extracting...\n")
    tm4 <- as.numeric(round(system.time(conditiontbl <- condition_occurrence_data(connection))[3], digit = 1))
    rds_maker(work_dir,conditiontbl,"condition")
    ################################################################################
    # DRUG_EXPOSURE DATA SAVING FUNCTION
    ################################################################################
    cat("\nDrug exposure data extracting...\n")
    tm5 <- as.numeric(round(system.time(exptbl <- drug_exposure_data(connection))[3], digit = 1))
    rds_maker(work_dir,exptbl,"drug_exposure")
    ################################################################################
    # DRUG_ERA DATA SAVING FUNCTION
    ################################################################################
    cat("\nDrug era data extracting...\n")
    tm6 <- as.numeric(round(system.time(eratbl <- drug_era_data(connection))[3], digit = 1))
    rds_maker(work_dir,eratbl,"drug_era")

    ################################################################################
    # File Saving
    ################################################################################

    zip(zipfile = paste0(work_dir,cdmDatabaseSchema,".zip"),
        files = paste0(work_dir,list.files(path = work_dir,pattern = "\\w*.rds$")), flag= c("-j", "-r"))

    if(length(list.files(path = work_dir, pattern = "\\w.zip$"))>0){
        file.remove(paste0(work_dir,list.files(path = work_dir, pattern = "\\w.rds$")))
    }
    # time check
    cat(paste0("RDS files created.\nThis process takes ", sum(tm1, tm2, tm3, tm4, tm5, tm6), "s.\n"))
    ################################################################################
    # Disconnect DB
    ################################################################################
    DatabaseConnector::disconnect(connection)
}

rds_maker <- function(work_dir,tbl,table_name){
    switch (table_name,
            "person" =  {
                persontbl_name_list <- c(
                    "persontbl_record", "persontbl_person_ratio", "persontbl_gender", "persontbl_min_age", "persontbl_max_age",
                    "persontbl_race", "persontbl_ethnicity", "persontbl_location", "persontbl_provider", "persontbl_care_site"
                )

                data_list <- tbl
                rds_path <- sapply(FUN = paste0, work_dir,persontbl_name_list, ".rds")
            },
            "death" = {
                deathtbl_name_list <- c("deathtbl_check", "deathtbl_type")

                data_list <- tbl
                rds_path <- sapply(FUN = paste0, work_dir, deathtbl_name_list, ".rds")
            },
            "visit" = {
                visittbl_name_list <- list(
                    "visittbl_record", "visittbl_person_ratio", "visittbl_visit_concept", "visittbl_start", "visittbl_end",
                    "visittbl_diff_date", "visittbl_count", "visittbl_type_concept", "visittbl_care_site", "visittbl_source_concept",
                    "visittbl_admitting_source", "visittbl_discharge", "visittbl_preceding"
                )

                data_list <- tbl
                rds_path <- sapply(FUN = paste0, work_dir, visittbl_name_list, ".rds")
            },
            "condition" = {
                conditiontbl_name_list <- list(
                    "conditiontbl_record", "conditiontbl_person_ratio", "conditiontbl_diff_date", "conditiontbl_start",
                    "conditiontbl_end", "conditiontbl_type_concept", "conditiontbl_stop", "conditiontbl_visit_occurrence",
                    "conditiontbl_visit_detail"
                )

                data_list <- tbl
                rds_path <- sapply(FUN = paste0, work_dir, conditiontbl_name_list, ".rds")
            },
            "drug_exposure" = {
                drug_exptbl_name_list <- list(
                    "drug_exptbl_record", "drug_exptbl_person_ratio", "drug_exptbl_diff_date", "drug_exptbl_start", "drug_exptbl_end",
                    "drug_exptbl_type_concept", "drug_exptbl_stop", "drug_exptbl_route", "drug_exptbl_visit_occurrence"
                )

                data_list <- tbl
                rds_path <- sapply(FUN = paste0, work_dir, drug_exptbl_name_list, ".rds")
            },
            "drug_era" = {
                drug_eratbl_name_list <- list(
                    "drug_eratbl_record", "drug_eratbl_person_ratio", "drug_eratbl_diff_date", "drug_eratbl_start", "drug_eratbl_end",
                    "drug_eratbl_exp_count", "drug_eratbl_gap_days"
                )

                data_list <- tbl
                rds_path <- sapply(FUN = paste0, work_dir, drug_eratbl_name_list, ".rds")
            }
    )
    mapply(saveRDS, object = data_list, file = rds_path)
}

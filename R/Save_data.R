#' Save data
#'
#' This function for saving data by RDS file
#' @keywords gemini
#' @export
#'
################################################################################
# SAVE DATA RESOURCE
# Set location when project path is change
################################################################################

save_data <- function(){
    dir.create(file.path(getwd(), "Standard RDS"), showWarnings = FALSE)
    dir.create(file.path(getwd(), "Target RDS"), showWarnings = FALSE)
    gemini::Query_func()
################################################################################
# PERSON DATA SAVING FUNCTION
################################################################################
    message("Person data extracting...")
    tm1 <- as.numeric(round(system.time(gemini::person_data())[3], digit = 1))
    # list for save data
    # if append more data, append in list
    persontbl_list <- list(
      persontbl_record, persontbl_person_ratio, persontbl_gender, persontbl_min_age, persontbl_max_age, persontbl_race
      , persontbl_ethnicity, persontbl_location, persontbl_provider, persontbl_care_site
    )
    persontbl_name_list <- c(
      "persontbl_record", "persontbl_person_ratio", "persontbl_gender", "persontbl_min_age", "persontbl_max_age",
      "persontbl_race", "persontbl_ethnicity", "persontbl_location", "persontbl_provider", "persontbl_care_site"
    )
    # Making path, Save Data
    temp <- sapply(FUN = paste0, "Standard RDS/", persontbl_name_list, ".rds")
    mapply(saveRDS, object = persontbl_list, file = temp)

################################################################################
# DEATH DATA SAVING FUNCTION
################################################################################
    message("Death data extracting...")
    tm2 <- as.numeric(round(system.time(gemini::death_data())[3], digit = 1))
    deathtbl_list <- list(deathtbl_check, deathtbl_type)
    deathtbl_name_list <- c("deathtbl_check", "deathtbl_type")
    temp <- sapply(FUN = paste0, "Standard RDS/", deathtbl_name_list, ".rds")
    mapply(saveRDS, object = deathtbl_list, file = temp)
################################################################################
# VISIT_OCCURRENCE DATA SAVING FUNCTION
################################################################################
    message("Visit occurrence data extracting...")
    tm3 <- as.numeric(round(system.time(gemini::visit_occurrence_data())[3], digit = 1))
    visittbl_list <- list(
      visittbl_record, visittbl_person_ratio, visittbl_visit_concept, visittbl_start, visittbl_end, visittbl_diff_date
      , visittbl_count, visittbl_type_concept, visittbl_care_site, visittbl_source_concept, visittbl_admitting_source,
      visittbl_discharge, visittbl_preceding
    )
    visittbl_name_list <- list(
      "visittbl_record", "visittbl_person_ratio", "visittbl_visit_concept", "visittbl_start", "visittbl_end",
      "visittbl_diff_date", "visittbl_count", "visittbl_type_concept", "visittbl_care_site", "visittbl_source_concept",
      "visittbl_admitting_source", "visittbl_discharge", "visittbl_preceding"
    )
    temp <- sapply(FUN = paste0, "Standard RDS/", visittbl_name_list, ".rds")
    mapply(saveRDS, object = visittbl_list, file = temp)
################################################################################
# CONDITION_OCCURRENCE DATA SAVING FUNCTION
################################################################################
    message("Condition occurrence data extracting...")
    tm4 <- as.numeric(round(system.time(gemini::condition_occurrence_data())[3], digit = 1))
    conditiontbl_list <- list(
      conditiontbl_record, conditiontbl_person_ratio, conditiontbl_diff_date, conditiontbl_start, conditiontbl_end,
      conditiontbl_type_concept, conditiontbl_stop, conditiontbl_visit_occurrence, conditiontbl_visit_detail
    )
    conditiontbl_name_list <- list(
      "conditiontbl_record", "conditiontbl_person_ratio", "conditiontbl_diff_date", "conditiontbl_start",
      "conditiontbl_end", "conditiontbl_type_concept", "conditiontbl_stop", "conditiontbl_visit_occurrence",
      "conditiontbl_visit_detail"
    )
    temp <- sapply(FUN = paste0, "Standard RDS/", conditiontbl_name_list, ".rds")
    mapply(saveRDS, object = conditiontbl_list, file = temp)
################################################################################
# DRUG_EXPOSURE DATA SAVING FUNCTION
################################################################################
    message("Drug exposure data extracting...")
    tm5 <- as.numeric(round(system.time(gemini::drug_exposure_data())[3], digit = 1))
    drug_exptbl_list <- list(
      drug_exptbl_record, drug_exptbl_person_ratio, drug_exptbl_diff_date, drug_exptbl_start, drug_exptbl_end,
      drug_exptbl_type_concept, drug_exptbl_stop, drug_exptbl_route, drug_exptbl_visit_occurrence
    )
    drug_exptbl_name_list <- list(
      "drug_exptbl_record", "drug_exptbl_person_ratio", "drug_exptbl_diff_date", "drug_exptbl_start", "drug_exptbl_end",
      "drug_exptbl_type_concept", "drug_exptbl_stop", "drug_exptbl_route", "drug_exptbl_visit_occurrence"
    )
    temp <- sapply(FUN = paste0, "Standard RDS/", drug_exptbl_name_list, ".rds")
    mapply(saveRDS, object = drug_exptbl_list, file = temp)
################################################################################
# DRUG_ERA DATA SAVING FUNCTION
################################################################################
    message("Drug era data extracting...")
    tm6 <- as.numeric(round(system.time(gemini::drug_era_data())[3], digit = 1))
    drug_eratbl_list <- list(
      drug_eratbl_record, drug_eratbl_person_ratio, drug_eratbl_diff_date, drug_eratbl_start, drug_eratbl_end,
      drug_eratbl_exp_count, drug_eratbl_gap_days
    )
    drug_eratbl_name_list <- list(
      "drug_eratbl_record", "drug_eratbl_person_ratio", "drug_eratbl_diff_date", "drug_eratbl_start", "drug_eratbl_end",
      "drug_eratbl_exp_count", "drug_eratbl_gap_days"
    )
    temp <- sapply(FUN = paste0, "Standard RDS/", drug_eratbl_name_list, ".rds")
    mapply(saveRDS, object = drug_eratbl_list, file = temp)

    zip(files = "Standard RDS/", zipfile = paste0("Standard RDS/",cdmDatabaseSchema,"*.zip$"))
    file.remove(paste0("Standard RDS/",list.files(path = "Standard RDS/", pattern = "*.rds$")))

    # time check
    message(paste0("RDS files created.\nThis process takes ", sum(tm1, tm2, tm3, tm4, tm5, tm6), "s."))
################################################################################
# Disconnect DB
################################################################################
    DatabaseConnector::disconnect(connection)
}

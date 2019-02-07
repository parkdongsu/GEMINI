#' Get data
#'
#' Run GEMINI
#' @keywords gemini
#' @export
#'
# Read rds data
gemini <- function(){
    message("Choose where 'Standard RDS' folder exist.")
    gemini::path_set()
    gemini::create_folder()
    if(length(list.files(paste0(getwd(), "/Standard RDS/"), pattern = "*.zip$"))>0){
        unzip(zipfile = paste0("Standard RDS/",list.files(paste0(getwd(), "/Standard RDS/"), pattern = "*.zip$")), overwrite = T, exdir = "Standard RDS")
    }
    if(length(list.files(paste0(getwd(), "/Target RDS/"), pattern = "*.zip$"))>0){
        unzip(zipfile = paste0("Target RDS/",list.files(paste0(getwd(), "/Target RDS/"), pattern = "*.zip$")), , overwrite = T, exdir = "Target RDS")
    }
    ask_test <- function(){
        ask <- readline('No RDS file in Target RDS folder. Do you want to test gemini? (y / n) ')
        if(ask == "y" || ask == "Y"){
            lapply(no_files, function(x) assign(paste0("std_",gsub(x,pattern ="*.rds$",replacement = "")),value = readRDS(paste0("Standard RDS/", x)),envir=.GlobalEnv))
            lapply(no_files, function(x) assign(paste0("tar_",gsub(x,pattern ="*.rds$",replacement = "")),value = readRDS(paste0("Standard RDS/", x)),envir=.GlobalEnv))
            gemini::draw_func()
            gemini::draw_whole()
            gemini::draw_person()
            gemini::draw_death()
            gemini::draw_visit_occurrence()
            gemini::draw_condition_occurrence()
            gemini::draw_drug_exposure()
            gemini::draw_drug_era()
            gemini::make_report()
        }else if (ask == "n" || ask == "N"){
            message("Proceed is stopped.")
        }else{
            message("Wrong input.\n")
            ask_test()
        }
    }
    # Check rds data files

    str_name_list <- list.files(paste0(getwd(), "/Standard RDS/"), pattern = "*.rds$")
    tar_name_list <- list.files(paste0(getwd(), "/Target RDS/"), pattern = "*.rds$")
    name_list <- intersect(str_name_list,tar_name_list)
    no_files <- setdiff(str_name_list,tar_name_list)

    # assign value function
    set_val <<- function(name_list) {
        lapply(name_list, function(x) assign(gsub(lapply(x, function(y) paste0("std_",y)), pattern = "*.rds$", replacement = ""), readRDS(paste0("Standard RDS/", x)), envir = .GlobalEnv))
        lapply(name_list, function(x) assign(gsub(lapply(x, function(y) paste0("tar_",y)), pattern = "*.rds$", replacement = ""), readRDS(paste0("Target RDS/", x)), envir = .GlobalEnv))
    }
    if(length(no_files) != 0&&length(name_list)!=0){
        set_val(name_list)
        lapply(no_files, function(x) assign(paste0("std_",gsub(x,pattern ="*.rds$",replacement = "")),value = readRDS(paste0("Standard RDS/", x)),envir=.GlobalEnv))
        lapply(no_files, function(x) assign(paste0("tar_",gsub(x,pattern ="*.rds$",replacement = "")),value = NULL,envir=.GlobalEnv))
    }else if(length(no_files) != 0&&length(name_list)==0){
        ask_test()
    }else if(length(no_files) == 0 && length(name_list)==0){
        message("No data exist. proceed is stopped.")
    }else{
        set_val(name_list)
        message("All data exist.")
        gemini::draw_func()
        gemini::draw_whole()
        gemini::draw_person()
        gemini::draw_death()
        gemini::draw_visit_occurrence()
        gemini::draw_condition_occurrence()
        gemini::draw_drug_exposure()
        gemini::draw_drug_era()
        gemini::make_report()
    }
}

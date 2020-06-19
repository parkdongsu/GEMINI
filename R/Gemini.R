#' Get data
#'
#' Run GEMINI
#' @keywords gemini
#' @export
#'
# Read rds data
gemini <- function(name = name, analysisFilePath = analysisFilePath, work_dir = getwd() ){

    dir.create(file.path(work_dir, "Standard RDS"), showWarnings = FALSE)
    dir.create(file.path(work_dir, "Target RDS"), showWarnings = FALSE)
    dir.create(file.path(work_dir, "images"), showWarnings = FALSE)
    dir.create(file.path(work_dir, "images","whole"), showWarnings = FALSE)
    dir.create(file.path(work_dir, "images","PERSON"), showWarnings = FALSE)
    dir.create(file.path(work_dir, "images","DEATH"), showWarnings = FALSE)
    dir.create(file.path(work_dir, "images","Visit"), showWarnings = FALSE)
    dir.create(file.path(work_dir, "images","Condition"), showWarnings = FALSE)
    dir.create(file.path(work_dir, "images","Drug exposure"), showWarnings = FALSE)
    dir.create(file.path(work_dir, "images","Drug era"), showWarnings = FALSE)
    
    pick_std_schema <- analysisFilePath[1]
    std_schema <<- substr(name[1], start = "1", stop=tail(unlist(gregexpr("\\.[^ ]*\\.zip",name[1])),n = 1)-1)
    unzip(zipfile = pick_std_schema, overwrite = T, exdir = file.path(getwd(), "Standard RDS"))


    pick_tar_schema <- analysisFilePath[2]
    tar_schema <<- substr(name[2], start = "1", stop=tail(unlist(gregexpr("\\.[^ ]*\\.zip",name[2])),n = 1)-1)
    unzip(zipfile = pick_tar_schema, overwrite = T, exdir = file.path(getwd(),"Target RDS"))


    # Check rds data files

    str_name_list <- list.files(file.path(getwd(), "Standard RDS"), pattern = "*.rds$")
    tar_name_list <- list.files(file.path(getwd(), "Target RDS"), pattern = "*.rds$")
    name_list <- intersect(str_name_list,tar_name_list)
    no_files <- setdiff(str_name_list,tar_name_list)

    if(length(no_files) != 0&&length(name_list)!=0){
        set_val(name_list)
        lapply(no_files, function(x) assign(paste0("std_",gsub(x,pattern ="*.rds$",replacement = "")),value = readRDS(paste0("Standard RDS/", x)),envir=.GlobalEnv))
        lapply(no_files, function(x) assign(paste0("tar_",gsub(x,pattern ="*.rds$",replacement = "")),value = NULL,envir=.GlobalEnv))
    }else if(length(no_files) != 0&&length(name_list)==0){
        ask_test()
    }else if(length(no_files) == 0 && length(name_list)==0){
        cat("No data exist. proceed is stopped.\n")
    }else{
        set_val(name_list)
        cat("All data exist.\n")
        gemini::draw_func(std_schema,tar_schema)
        gemini::draw_whole(std_schema,tar_schema)
        gemini::draw_person(std_schema,tar_schema)
        gemini::draw_death()
        gemini::draw_visit_occurrence(std_schema,tar_schema)
        gemini::draw_condition_occurrence(std_schema,tar_schema)
        gemini::draw_drug_exposure(std_schema,tar_schema)
        gemini::draw_drug_era(std_schema,tar_schema)
        gemini::make_report()
    }
}

# assign value function
set_val <- function(name_list) {
    lapply(name_list, function(x) assign(gsub(lapply(x, function(y) paste0("std_",y)), pattern = "*.rds$", replacement = ""), readRDS(paste0("Standard RDS/", x)), envir = .GlobalEnv))
    lapply(name_list, function(x) assign(gsub(lapply(x, function(y) paste0("tar_",y)), pattern = "*.rds$", replacement = ""), readRDS(paste0("Target RDS/", x)), envir = .GlobalEnv))
}

ask_test <- function(){
    ask <- readline('No RDS file in Target RDS folder. Do you want to test gemini? (y / n) ')
    if(ask == "y" || ask == "Y"){
        lapply(no_files, function(x) assign(paste0("std_",gsub(x,pattern ="*.rds$",replacement = "")),value = readRDS(paste0("Standard RDS/", x)),envir=.GlobalEnv))
        lapply(no_files, function(x) assign(paste0("tar_",gsub(x,pattern ="*.rds$",replacement = "")),value = readRDS(paste0("Standard RDS/", x)),envir=.GlobalEnv))
        gemini::draw_func(pick_std_schema,pick_tar_schema)
        gemini::draw_whole(pick_std_schema,pick_tar_schema)
        gemini::draw_person(pick_std_schema,pick_tar_schema)
        gemini::draw_death()
        gemini::draw_visit_occurrence(pick_std_schema,pick_tar_schema)
        gemini::draw_condition_occurrence(pick_std_schema,pick_tar_schema)
        gemini::draw_drug_exposure(pick_std_schema,pick_tar_schema)
        gemini::draw_drug_era(pick_std_schema,pick_tar_schema)
        gemini::make_report()
    }else if (ask == "n" || ask == "N"){
        cat("Proceed is stopped.\n")
    }else{
        message("Wrong input.\n")
        ask_test()
    }
}

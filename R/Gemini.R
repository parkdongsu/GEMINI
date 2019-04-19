#' Get data
#'
#' Run GEMINI
#' @keywords gemini
#' @export
#'
# Read rds data
gemini <- function(){

    check.packages('plotrix')
    check.packages('ggplot2')
    check.packages("knitr")
    check.packages("rmarkdown")

    cat("Choose where 'Standard RDS' folder exist.\n")
    gemini::path_set()
    gemini::create_folder() #Create folder for avoid error "No directory"

    #cnt_rds <- readline("Set count : ") #This part needs to use multiple rds zips.

    #for(i in 1:cnt_rds)
    #{
        if(length(list.files(paste0(getwd(), "/Standard RDS/"), pattern = "*.zip$"))>0){
            pick_std_schema <- select.list(list.files(path = "Standard RDS", pattern = "\\w*.zip$"), title = "Select Standard schema zip flie.")
            std_schema <<- substr(pick_std_schema, start = "1", stop=tail(unlist(gregexpr("\\.dbo",pick_std_schema)),n = 1)-1)
            unzip(zipfile = paste0("Standard RDS/",pick_std_schema), overwrite = T, exdir = "Standard RDS")
        }
        else{
            std_schema <<- "A CDM"
        }
        if(length(list.files(paste0(getwd(), "/Target RDS/"), pattern = "*.zip$"))>0){
            pick_tar_schema <- select.list(list.files(path = "Target RDS", pattern = "\\w*.zip$"), title = "Select Target schema zip flie.")
            tar_schema <<- substr(pick_tar_schema, start = "1", stop=tail(unlist(gregexpr("\\.dbo",pick_tar_schema)),n = 1)-1)
            unzip(zipfile = paste0("Target RDS/",pick_tar_schema), overwrite = T, exdir = "Target RDS")
        }
        else{
            tar_schema <<- "B CDM"
        }
    #}
    # Check rds data files

    str_name_list <- list.files(paste0(getwd(), "/Standard RDS/"), pattern = "*.rds$")
    tar_name_list <- list.files(paste0(getwd(), "/Target RDS/"), pattern = "*.rds$")
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

#' Get data
#'
#' Run GEMINI
#' @keywords gemini
#' @export
#'
#' @import plotrix
#' @import lazyeval
#' @import withr
#' @import ggplot2
#' @import knitr
#' @import htmltools
#' @import rmarkdown
#' @import plot_ly
# Read rds data
gemini <- function(dbCount = 2,
                   testRunning = F){
    cat("Choose where 'Standard RDS' folder exist.\n")
    gemini::path_set()
    gemini::create_folder() #Create folder for avoid error "No directory"
    i<-1
    dbName <- array(data = NA, dim = dbCount)
    dbList <- array(data = NA, dim = dbCount)
    while(i-1!=dbCount){
        if(length(list.files(paste0(getwd(), "/Standard RDS/"), pattern = "*.zip$"))>0){
            pickDb <- select.list(list.files(path = "Standard RDS", pattern = "\\w*.zip$"), title = "Select DB zip flie.")
            dbName[i] <- substr(pickDb, start = "1", stop=tail(unlist(gregexpr("\\.",pickDb)),n = 1)-1)
            unzip(zipfile = paste0("Standard RDS/",pickDb), exdir = paste0(getwd(),"/Standard RDS/",dbName[i]), overwrite = T)
            dbList[i] <- as.data.frame(list.files(path = paste0(getwd(),"/Standard RDS/",dbName[i]), pattern = "*.rds$"))
        }
        else{
            dbName[i] <- paste(i,"CDM")
        }
        i <- i+1
    }


    # Check rds data files

#     if(length(no_files) != 0 && length(name_list)!=0){
#         set_val(name_list)
#         lapply(no_files, function(x) assign(paste0("std_",gsub(x,pattern ="*.rds$",replacement = "")),value = readRDS(paste0("Standard RDS/", x)),envir=.GlobalEnv))
#         lapply(no_files, function(x) assign(paste0("tar_",gsub(x,pattern ="*.rds$",replacement = "")),value = NULL,envir=.GlobalEnv))
#     }else if(length(no_files) != 0&&length(name_list)==0){
#         ask_test(no_files,pick_std_schema)
#     }else if(length(no_files) == 0 && length(name_list)==0){
#         cat("No data exist. proceed is stopped.\n")
#     }else{
#         set_val(name_list)
#         cat("All data exist.\n")
#
#     }
# }

# assign value function
}

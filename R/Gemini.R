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
gemini <- function(dbCount = 6){
    cat("Choose where 'Gemini RDS' folder exist.\n")
    if(Sys.info()[[1]]=='Windows'){
        work_dir <- choose.dir()
    }
    else{
        work_dir <- readline("Set work directory path : ")
    }
    connection <- DatabaseConnector::connect(connectionDetails)

    dir.create(file.path(getwd(), "Gemini RDS"), showWarnings = FALSE)
    dir.create(file.path(getwd(), "images"), showWarnings = FALSE)
    table <- c('Whole','Person','Death','Visit','Condition','Drug exposure','Drug era')
    sapply(X= file.path(getwd(),'images',table), FUN = function(x){dir.create(x,showWarnings = F)})

    dbName <- vector(length = dbCount)
    rds <- list()
    for(i in 1:dbCount){
        if(length(list.files(file.path(getwd(),"Gemini RDS"), pattern = "*.zip$"))>0){
            pickDb <- select.list(list.files(path = "Gemini RDS", pattern = "\\w*.zip$"), title = "Select DB zip flie.")
            dbName[i] <- substr(pickDb, start = "1", stop=tail(unlist(gregexpr("\\.",pickDb)))[1]-1)
            unzip(zipfile = file.path("Gemini RDS",pickDb),
                  exdir = file.path(getwd(),"Gemini RDS",dbName[i]),
                  overwrite = T)
            rdsPath <- list.files(path = file.path(getwd(),"Gemini RDS", dbName[i]),full.names = T)
            rdsList <- lapply(rdsPath, function(file){readRDS(file)})
            names(rdsList) <- gsub(x = list.files(path = file.path(getwd(),"Gemini RDS", dbName[i]),full.names = F),
                                   pattern = ".rds",
                                   replacement = "")
            rds[i] <- list(rdsList)
        }
        else{
            dbName[i] <- paste(i,"CDM")
        }
    }
}


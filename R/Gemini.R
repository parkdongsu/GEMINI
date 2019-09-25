#' Get data
#'
#' Run GEMINI
#' @keywords gemini
#' @export
#'
#' @import lazyeval
#' @import withr
#' @import ggplot2
#' @import knitr
#' @import htmltools
#' @import rmarkdown
#' @import plotly
#'
# Read rds data
gemini <- function(dbCount = 2){
    library(plotly)
    cat("Choose where 'Gemini RDS' folder exist.\n")
    if(Sys.info()[[1]]=='Windows'){
        work_dir <- choose.dir()
    }
    else{
        work_dir <- readline("Set work directory path : ")
    }
    dir.create(file.path(work_dir, "Gemini RDS"), showWarnings = FALSE)
    table <- c('Whole','Person','Death','Visit','Condition','Drug exposure','Drug era')
    sapply(X= file.path(work_dir,'images',table), FUN = function(x){dir.create(x,showWarnings = F)})

    dbName <<- vector(length = dbCount)
    rds <<- list()
    for(i in 1:dbCount){
        if(length(list.files(work_dir, pattern = "*.zip$"))>0){
            pickDb <- select.list(list.files(path = work_dir, pattern = "\\w*.zip$"), title = "Select DB zip flie.")
            dbName[i] <<- substr(pickDb, start = "1", stop=tail(unlist(gregexpr("\\.",pickDb)))[1]-1)
            unzip(zipfile = file.path(work_dir,pickDb),
                  exdir = file.path(work_dir,dbName[i]),
                  overwrite = T)
            rdsPath <- list.files(path = file.path(work_dir, dbName[i]),full.names = T)
            rdsList <- lapply(rdsPath, function(file){readRDS(file)})
            names(rdsList) <- gsub(x = list.files(path = file.path(work_dir, dbName[i]),full.names = F),
                                   pattern = ".rds",
                                   replacement = "")
            rds[i] <<- list(rdsList)
        }
        else{
            dbName[i] <<- paste(i,"CDM")
        }
    }
    make_report(work_dir)
}

make_report<-function(workDir){
    rmarkdown::render(paste0(.libPaths()[1],"/gemini/extdata/Gemini_md.Rmd"),encoding = "UTF-8",output_dir = workDir)
    browseURL(url=paste0(workDir,"/Gemini_md.html"))
    rm(rds,envir = .GlobalEnv)
    rm(dbName, envir = .GlobalEnv)
}


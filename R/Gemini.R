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
gemini <- function(dbCount = 2,name = name,analysisFilePath,work_dir=getwd()){
    library(plotly)
    
    dir.create(file.path(work_dir, "Gemini RDS"), showWarnings = FALSE)
    table <- c('Whole','Person','Death','Visit','Condition','Drug exposure','Drug era')
    sapply(X= file.path(getwd(),'images',table), FUN = function(x){dir.create(x,showWarnings = F)})
    dbName <<- vector(length = dbCount)
    rds <<- list()
    for(i in 1:length(analysisFilePath)){
        tmpPathIndex <- gregexpr('\\\\',analysisFilePath[i])[[1]]
        pickDb <- substr(analysisFilePath[i],tmpPathIndex[length(tmpPathIndex)]+1,nchar(analysisFilePath[i]))
        # dbName[i] <<- substr(pickDb, start = "1", stop=tail(unlist(gregexpr("\\.",pickDb)))[1]-1)
        dbName[i] <<- gsub(pattern = '.zip','',name[i])
        unzip(zipfile = analysisFilePath[i],
              exdir = file.path(getwd(),dbName[i]),
              overwrite = T)
        rdsPath <- list.files(path = file.path(getwd(), dbName[i]),full.names = T)
        rdsList <- lapply(rdsPath, function(file){readRDS(file)})
        names(rdsList) <- gsub(x = list.files(path = file.path(getwd(), dbName[i]),full.names = F),
                               pattern = ".rds",
                               replacement = "")
        rds[i] <<- list(rdsList)
    }
    
  
    
    make_report(work_dir)
}






make_report<-function(workDir){
    rmarkdown::render(paste0(.libPaths()[1],"/gemini/extdata/Gemini_md.Rmd"),encoding = "UTF-8",output_dir = workDir)
    browseURL(url=paste0(workDir,"/Gemini_md.html"))
    rm(rds,envir = .GlobalEnv)
    rm(dbName, envir = .GlobalEnv)
}


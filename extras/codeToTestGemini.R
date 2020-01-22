#Test build for Gemini
#usethis::edit_r_environ()
library(gemini)
library(plotly)
dbCount <- 2
work_dir <- Sys.getenv('gemini_rds_dir')
table <- c('Whole','Person','Death','Visit','Condition','Drug exposure','Drug era')
sapply(X= file.path(work_dir,'images',table), FUN = function(x){dir.create(x,showWarnings = F)})
dbName <- vector(length = dbCount)
rds <- list()
for(i in 1:dbCount){
    if(length(list.files(work_dir, pattern = "*.zip$"))>0){
        pickDb <- select.list(list.files(path = work_dir, pattern = "\\w*.zip$"), title = "Select DB zip flie.")
        dbName[i] <- substr(pickDb, start = "1", stop=tail(unlist(gregexpr("\\.",pickDb)))[1]-1)
        unzip(zipfile = file.path(work_dir,pickDb),
              exdir = file.path(work_dir,dbName[i]),
              overwrite = T)
        rdsPath <- list.files(path = file.path(work_dir, dbName[i]),full.names = T)
        rdsList <- lapply(rdsPath, function(file){readRDS(file)})
        names(rdsList) <- gsub(x = list.files(path = file.path(work_dir, dbName[i]),full.names = F),
                               pattern = ".rds",
                               replacement = "")
        rds[i] <- list(rdsList)
    }
}

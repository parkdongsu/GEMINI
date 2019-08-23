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
    work_dir <- path_set()
    connect_DB(connectionDetails)
    save_data()
}

connect_DB <- function(connectionDetails){
    #Set server information to connecting
    cdmDatabaseSchema <- connectionDetails$schema
    #Server connect
    tryCatch({
        connection <- DatabaseConnector::connect(connectionDetails)
    },error = function(e){
        print(e)
        stop("Connection failed.")
    })
}

#' Create rds file
#'
#' This function for creating server info files and trigger of create rds files
#' @keywords gemini
#' @export
#'
#' @import DatabaseConnector
#' @import SqlRender
create_rds<- function(dbms,ip=NULL,port=NULL,schema=NULL,id=NULL,pw=NULL){
    cat("Set directory to create rds files.\n")
    # Change working directory for confirming user to create where the files
    path_set()
    connectionDetails <- createConnectionDetails(dbms = dbms,server = ip,schema = schema,user = id,password = pw,port = port)
    connect_DB(connectionDetails)
    save_data()
}

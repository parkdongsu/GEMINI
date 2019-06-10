#' connect_db
#'
#' This function for DB server connecting
#' @keywords gemini
#' @export

# Set User Info to check authority
connect_DB <- function(connectionDetails){
    #Set server information to connecting
    cdmDatabaseSchema <<- connectionDetails$schema
    #Server connect
    tryCatch({
        connection <<- DatabaseConnector::connect(connectionDetails)
        DatabaseConnector::dbIsValid(connection)
        },error = function(e){
            print(e)
            stop("Connection failed.")
    })
}

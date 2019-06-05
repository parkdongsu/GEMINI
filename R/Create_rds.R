#' Create rds file
#'
#' This function for creating server info files and trigger of create rds files
#' @keywords gemini
#' @export
#'
create_rds<- function(){

    gemini::check.packages("DatabaseConnector")
    gemini::check.packages("SqlRender")

    cat("Set directory to create rds files.\n")
    # Change working directory for confirming user to create where the files
    gemini::path_set()

    # Main Code
    if(length(list.files(pattern = "server_info\\w*.cfg$"))>0)
        {
        recon <- select.list(c("y","n"), title = "Do you want to connect previous server?")
        if(recon == 'y'){
            pick_server <- select.list(list.files(pattern = "server_info\\w*.cfg$"), title = "Select previous server info flie.")
            cat("Try to connecting DB server...\n")
            gemini::connect_DB(pick_server)
            }
        else if(recon == 'n'){
            file.rename("server_info.cfg",infofile_rename())
            write(create_server_info(),"server_info.cfg")
            cat("Try to connecting DB server...\n")
            gemini::connect_DB()
            }
    }
    else{
        write(create_server_info(), "server_info.cfg")
        cat("Try to connecting DB server...\n")
        gemini::connect_DB()
    }
    gemini::save_data()
    rm(list = ls())
}


create_server_info <- function(){
    db_name<-paste0("dbName=",readline("Set db server name : "))
    server_ip<-paste0("server=",readline("Set server ip : "))
    port_number<-paste0("port=",readline("Set port : "))
    schema_name<-paste0("schemaName=",readline("Set schema name : "))
    user_id<-paste0("user=",readline("Set db user id : "))
    pwd<-paste0("password=",readline("Set password : "))

    if(!grepl(x = schema_name, pattern = "*.dbo$") && db_name=="sql server"){
        #schema_name <- paste0(schema_name,".dbo")
    }
    return(c(db_name,server_ip,port_number,schema_name,user_id,pwd))
}

infofile_rename <- function(){
    if( length(list.files(pattern = "server_info_old\\w*.cfg$"))>0){
    temp <- max(as.integer(gsub("*.cfg$","", gsub("server_info_old","",list.files(pattern="server_info_old\\d*.cfg$")))))
        temp <- paste0("server_info_old",as.character(temp+1),".cfg")
        return(temp)
    }
    else{
        return("server_info_old0.cfg")
    }
}

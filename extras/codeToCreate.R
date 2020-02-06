# CodeToRun
usethis::edit_r_environ() ## edit ID/PW
### work_dir <- readline(prompt="Enter work_dir: ")
##16ho AUSOM-----------------------------------------------------------------------------------------
connectionDetails <- DatabaseConnector::createConnectionDetails(dbms = 'sql server',
                                                                server = Sys.getenv("server_ip_16"),
                                                                user = Sys.getenv("chungsu_ID"),
                                                                password = Sys.getenv("chungsu_PW"),
                                                                schema = Sys.getenv("AUSOMv5_3"))
create_rds(connectionDetails)
##16ho AUSOM-----------------------------------------------------------------------------------------
connectionDetails <- DatabaseConnector::createConnectionDetails(dbms = 'sql server',
                                                                server = Sys.getenv("server_ip_16"),
                                                                user = Sys.getenv("chungsu_ID"),
                                                                password = Sys.getenv("chungsu_PW"),
                                                                schema = Sys.getenv("AUSOMv5_3_1"))
create_rds(connectionDetails)
##17ho CDMPv1----------------------------------------------------------------------------------------
connectionDetails <- DatabaseConnector::createConnectionDetails(dbms = 'sql server',
                                                                server = Sys.getenv("server_ip_17"),
                                                                user = Sys.getenv("USER_ID"),
                                                                password = Sys.getenv("PASSWORD"),
                                                                schema = Sys.getenv("CDMPv1"))
create_rds(connectionDetails)
##17ho Dolphin_CDM-----------------------------------------------------------------------------------
connectionDetails <- DatabaseConnector::createConnectionDetails(dbms = 'sql server',
                                                                server = Sys.getenv("server_ip_17"),
                                                                user = Sys.getenv("USER_ID"),
                                                                password = Sys.getenv("PASSWORD"),
                                                                schema = Sys.getenv("Dolphin_CDM"))
create_rds(connectionDetails)
##17ho synPUF_CDM------------------------------------------------------------------------------------
connectionDetails <- DatabaseConnector::createConnectionDetails(dbms = 'sql server',
                                                                server = Sys.getenv("server_ip_17"),
                                                                user = Sys.getenv("USER_ID"),
                                                                password = Sys.getenv("PASSWORD"),
                                                                schema = Sys.getenv("synPUF_CDM"))
create_rds(connectionDetails)
##17ho ICARUS----------------------------------------------------------------------------------------
connectionDetails <- DatabaseConnector::createConnectionDetails(dbms = 'sql server',
                                                                server = Sys.getenv("server_ip_17"),
                                                                user = Sys.getenv("USER_ID"),
                                                                password = Sys.getenv("PASSWORD"),
                                                                schema = Sys.getenv("ICARUS"))
create_rds(connectionDetails)

gemini::gemini(dbCount=10)


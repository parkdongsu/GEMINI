# CodeToRun
#usethis::edit_r_environ() ## edit ID/PW


connectionDetails <- DatabaseConnector::createConnectionDetails(dbms = 'sql server',
                                                                server = Sys.getenv("server_ip_17"),
                                                                user = Sys.getenv("USER_ID"),
                                                                password = Sys.getenv("PASSWORD_17"),
                                                                schema = Sys.getenv("ICARUS")
                                                                )
create_rds(connectionDetails)

gemini::gemini(dbCount=2)


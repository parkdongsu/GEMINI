# CodeToRun
#usethis::edit_r_environ() ## edit ID/PW
require(DatabaseConnector)
require(SqlRender)

connectionDetails <- DatabaseConnector::createConnectionDetails(dbms = 'sql server',
                                                                server = Sys.getenv("server_ip_17"),
                                                                user = Sys.getenv("user_id"),
                                                                password = Sys.getenv("user_pw"),
                                                                schema = Sys.getenv("ausom"))
create_rds(connectionDetails)

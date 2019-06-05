#' Query function
#'
#' This function for extracting data function
#' @keywords gemini
#' @export
#'

# Function about data extraction
# Sys.setlocale('LC_ALL','C')
Query_func<- function(){
    # sql query Render
        queryRender <<- function(sqlquery, tblName = "", AttName = "", comparedAttName = "", startName = "", endName = "") {
            # Unset parameter call warning message
            options(warn = -1)
            sql <- SqlRender::renderSql(sqlquery,
                                        cdm_database_schema = cdmDatabaseSchema, tbl_name = tblName, att_name = AttName,
                                        compared_att_name = comparedAttName, start_name = startName, end_name = endName
            )$sql
            options(warn = 1)
            sql <- SqlRender::translateSql(sql, targetDialect = attr(connection, "dbms"))$sql
            sql <- gsub("\r|\t|\n"," ", sql)
            temp <- DatabaseConnector::querySql(connection, sql)
            # Colname change
            colnames(temp) <- SqlRender::snakeCaseToCamelCase(colnames(temp))

            return(temp)
        }

        #0 values get little numeric data for Pie3D...
        zeroToDecimal <<- function(attName){
            attName$ratio <- sapply(attName$ratio, function(x) if (x == 0.0 || x == 0){
                x <- 0.001
            }else{
                (x)
            }
            )
            return(attName)
        }

        #Append zero data frame
        addNullGender <<- function(attName){
            i <- 1
            while(i != nrow(attName) + 1){
                if (attName$genderConceptId[i] == "8507" && attName$genderConceptId[i + 1] != "8532") {
                    temp <- c(attName$ageRange[i], "8532", 0)
                    attName <- rbind(attName[c(1:i), ], temp, attName[c(i + 1:nrow(attName)), ])
                }
                else if (attName$genderConceptId[i] == "8532" && (attName$genderConceptId[i-1] != "8507" || identical(attName$genderConceptId[i-1],numeric(0)))) {
                    temp <- c(attName$ageRange[i], "8507", 0)
                    attName <- rbind(attName[c(1:i - 1), ], temp, attName[c(i:nrow(attName)), ])
                }
                i <- i + 1
            }
            return(attName)
        }
}

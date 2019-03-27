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
            sql <- SqlRender::renderSql(sql,
                                        cdm_database_schema = cdmDatabaseSchema, tbl_name = tblName, att_name = AttName,
                                        compared_att_name = comparedAttName, start_name = startName, end_name = endName
            )$sql
            options(warn = 1)
            sql <- SqlRender::translateSql(sql, targetDialect = attr(connection, "dbms"))$sql

            temp <- DatabaseConnector::querySql(connection, sql)
            # Colname change
            colnames(temp) <- SqlRender::snakeCaseToCamelCase(colnames(temp))

            return(temp)
        }

        # Get total records
        # Query to find all table records count info, target table record and calculate with it
        get_total_records <<- function(tblName) {
            sql <<- "SELECT '@tbl_name' as attribute_name , count(*) as count,round(count(*)/CONVERT(float,(
            SELECT SUM(TEMP.rows) FROM(
            SELECT rows FROM(
            SELECT o.name, i.rows FROM sysindexes i
            INNER JOIN
            sysobjects o
            ON i.id = o.id
            WHERE i.indid < 2 AND  o.xtype = 'U' AND (o.name='person' OR o.name='death' OR o.name='visit_occurrence'
            OR o.name='condition_occurrence' OR o.name='drug_exposure' OR o.name='drug_era')
            )AS T
            )AS TEMP))*100,1
            )as ratio
            FROM @cdm_database_schema.@tbl_name"

            return(queryRender(sql, tblName))
        }

        # Get person ratio with Query
        get_person_ratio <<- function(tblName) {
            sql <<- "SELECT 'table person' as attribute_name,
            ROUND(count(distinct person_id)/CONVERT(float,(SELECT count(distinct person_id) FROM @cdm_database_schema.person))*100,1) as ratio
            FROM @cdm_database_schema.@tbl_name"

            return(queryRender(sql, tblName))
        }

        # Get Gender ratio
        get_gender_ratio <<- function(tblName, attName) {
            sql <<- "SELECT (SELECT CONCEPT_NAME FROM @cdm_database_schema.concept where concept_id = @att_name) as attribute_name,
            round(count(distinct person_id)/convert(float,(SELECT count(distinct person_id)
            FROM @cdm_database_schema.@tbl_name))*100,1) as ratio
            FROM @cdm_database_schema.@tbl_name
            GROUP BY @att_name"

            return(queryRender(sql, tblName, attName))
        }

        # Get regular type ratio
        get_ratio <<- function(tblName, attName) {
            sql <<- "SELECT @att_name as concept_id, (SELECT CONCEPT_NAME FROM @cdm_database_schema.concept where concept_id = @att_name) as attribute_name,
            round(count(@att_name)/convert(float,(SELECT count(*) FROM @cdm_database_schema.@tbl_name))*100,1) as ratio
            FROM @cdm_database_schema.@tbl_name
            GROUP BY @att_name"

            return(queryRender(sql, tblName, attName))
        }

        # Get regular type ratio with NULL ratio
        get_null_ratio <<- function(tblName, attName) {
            sql <<- "select '@att_name' as attribute_name ,count(distinct @att_name) as attribute_count,
            round(SUM(CASE WHEN @att_name IS NULL THEN 1 ELSE 0 END)/convert(float,SUM(CASE WHEN @att_name IS NULL THEN 1 ELSE 1 END))*100,1) as null_ratio
            from @cdm_database_schema.@tbl_name"

            return(queryRender(sql, tblName, attName))
        }

        # Get Person per Year data
        get_record_per_year <<- function(tblName, attName) {
            sql <<- "WITH T1 AS(
            select	LEFT(@att_name,4) as visit_year, count(person_id) as person_count  FROM @cdm_database_schema.@tbl_name
            GROUP BY LEFT(@att_name,4)
            )
            SELECT visit_year, ROUND(person_count/CONVERT(float,(SELECT SUM(person_count) FROM T1))*100,1) as person_ratio FROM T1
            ORDER BY visit_year ASC"

            return(queryRender(sql, tblName, attName))
        }

        # End date - Start date, to get Duration
        get_diff_year <<- function(tblName, startName, endName) {
            sql <<- "SELECT day_diff, SUM(person_temp) as person_count FROM(SELECT DATEDIFF(day, @start_name, @end_name) as day_diff,
            count(person_id) as person_temp FROM @cdm_database_schema.@tbl_name
            GROUP BY DATEDIFF(day, @start_name, @end_name)) AS T1
            GROUP BY day_diff"

            return(queryRender(sql, tblName, startName = startName, endName = endName))
        }

        # Extract data and Compare other column
        get_compared_ratio <<- function(tblName, attName, comparedAttName) {
            sql <<- "select 'Associate' as attribute_name, ROUND((count(@att_name)/CONVERT(float,count(@compared_att_name)))*100,1) as ratio
            from @cdm_database_schema.@tbl_name"

            return(queryRender(sql, tblName, attName, comparedAttName))
        }

        # Count stop reason
        get_reason_count <<- function(tblName) {
            sql <<- "select 'stop reason' as attribute_name ,count(distinct stop_reason) as attribute_count
            from @cdm_database_schema.@tbl_name"
            return(queryRender(sql, tblName))
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

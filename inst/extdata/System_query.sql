--MSSQL SYSTEM QUERY
SELECT '@tbl_name' as attribute_name , count(*) as count,round(count(*)/CONVERT(float,(
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
            FROM @cdm_database_schema.@tbl_name;

--POSTGRESQL SYSTEM QUERY
SELECT '@tbl_name' as attribute_name, count(*) AS count, round((count(*)/
(SELECT sum(n_live_tup)
 FROM pg_stat_all_tables WHERE schemaname='@cdm_database_schema' AND (relname='person' OR relname='death' OR relname='visit_occurrence'
													  OR relname='condition_occurrence' OR relname='drug_exposure'
													  OR relname='drug_era')))*100,2) as ratio
FROM @cdm_database_schema.@tbl_name;

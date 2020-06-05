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
SELECT '@tbl_name' as attribute_name, count(*) AS count, ROUND(CAST((count(*)/  (
	SELECT sum(reltuples)
	FROM pg_class
	WHERE (oid='@cdm_database_schema.person'::regclass
	or oid='@cdm_database_schema.visit_occurrence'::regclass
	or oid='@cdm_database_schema.condition_occurrence'::regclass
	or oid='@cdm_database_schema.drug_exposure'::regclass
	or oid='@cdm_database_schema.drug_era'::regclass)))*100 AS NUMERIC),2) as ratio
FROM @cdm_database_schema.@tbl_name;
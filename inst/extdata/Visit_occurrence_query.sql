--visittbl_person_ratio
SELECT 'table person' as attribute_name,
            ROUND(count(distinct person_id)/CAST((SELECT count(distinct person_id) FROM @cdm_database_schema.person)AS FLOAT)*100,1) as ratio
            FROM @cdm_database_schema.@tbl_name;

--visittbl_visit_concept
SELECT @att_name as concept_id, (SELECT CONCEPT_NAME FROM @cdm_database_schema.concept where concept_id = @att_name) as attribute_name,
            round(count(@att_name)/CAST((SELECT count(*) FROM @cdm_database_schema.@tbl_name)AS FLOAT)*100,1) as ratio
            FROM @cdm_database_schema.@tbl_name
            GROUP BY @att_name;
			
--visittbl_start
WITH T1 AS(
            select	YEAR(@att_name) as visit_year, count(person_id) as person_count  FROM @cdm_database_schema.@tbl_name
            GROUP BY YEAR(@att_name)
            )
            SELECT visit_year, ROUND(person_count/CAST((SELECT SUM(person_count) FROM T1)AS FLOAT)*100,1) as person_ratio FROM T1
            ORDER BY visit_year ASC;
			
--visittbl_end
WITH T1 AS(
            select	YEAR(@att_name) as visit_year, count(person_id) as person_count  FROM @cdm_database_schema.@tbl_name
            GROUP BY YEAR(@att_name)
            )
            SELECT visit_year, ROUND(person_count/CAST((SELECT SUM(person_count) FROM T1)AS FLOAT)*100,1) as person_ratio FROM T1
            ORDER BY visit_year ASC;

--visittbl_diff_date
SELECT DATEDIFF(day, @start_name, @end_name) as day_diff, count(person_id) as person_count FROM @cdm_database_schema.@tbl_name
            GROUP BY DATEDIFF(day, @start_name, @end_name)
			ORDER BY DATEDIFF(day, @start_name, @end_name) ASC;
			
-- visittbl_count
select	YEAR(visit_end_date) as visit_year, count(person_id) as person_count FROM @cdm_database_schema.visit_occurrence
GROUP BY YEAR(visit_end_date);			
			
--visittbl_type_concept
SELECT @att_name as concept_id, (SELECT CONCEPT_NAME FROM @cdm_database_schema.concept where concept_id = @att_name) as attribute_name,
            round(count(@att_name)/CAST((SELECT count(*) FROM @cdm_database_schema.@tbl_name)AS FLOAT)*100,1) as ratio
            FROM @cdm_database_schema.@tbl_name
            GROUP BY @att_name;
			
--visittbl_care_site
select '@att_name' as attribute_name ,count(distinct @att_name) as attribute_count,
            round(SUM(CASE WHEN @att_name IS NULL THEN 1 ELSE 0 END)/CAST(SUM(CASE WHEN @att_name IS NULL THEN 1 ELSE 1 END)AS FLOAT)*100,1) as null_ratio
            from @cdm_database_schema.@tbl_name;

--visittbl_source_concept
SELECT @att_name as concept_id, (SELECT CONCEPT_NAME FROM @cdm_database_schema.concept where concept_id = @att_name) as attribute_name,
            round(count(@att_name)/CAST((SELECT count(*) FROM @cdm_database_schema.@tbl_name)AS FLOAT)*100,1) as ratio
            FROM @cdm_database_schema.@tbl_name
            GROUP BY @att_name;
			
--visittbl_admitting_source
SELECT @att_name as concept_id, (SELECT CONCEPT_NAME FROM @cdm_database_schema.concept where concept_id = @att_name) as attribute_name,
            round(count(@att_name)/CAST((SELECT count(*) FROM @cdm_database_schema.@tbl_name)AS FLOAT)*100,1) as ratio
            FROM @cdm_database_schema.@tbl_name
            GROUP BY @att_name;
			
--visittbl_discharge
SELECT @att_name as concept_id, (SELECT CONCEPT_NAME FROM @cdm_database_schema.concept where concept_id = @att_name) as attribute_name,
            round(count(@att_name)/CAST((SELECT count(*) FROM @cdm_database_schema.@tbl_name)AS FLOAT)*100,1) as ratio
            FROM @cdm_database_schema.@tbl_name
            GROUP BY @att_name;
			
--visittbl_preceding
select 'Associate' as attribute_name, ROUND((count(@att_name)/CAST(count(@compared_att_name)AS FLOAT))*100,1) as ratio
            from @cdm_database_schema.@tbl_name;
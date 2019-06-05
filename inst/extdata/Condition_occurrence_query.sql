--conditiontbl_person_ratio
SELECT 'table person' as attribute_name,
            ROUND(count(distinct person_id)/CAST((SELECT count(distinct person_id) FROM @cdm_database_schema.person) AS float)*100,1) as ratio
            FROM @cdm_database_schema.@tbl_name;
			
--conditiontbl_start
WITH T1 AS(
            select	YEAR(@att_name) as visit_year, count(person_id) as person_count  FROM @cdm_database_schema.@tbl_name
            GROUP BY YEAR(@att_name)
            )
            SELECT visit_year, ROUND(person_count/CAST((SELECT SUM(person_count) FROM T1)AS float)*100,1) as person_ratio FROM T1
            ORDER BY visit_year ASC;

--conditiontbl_end
WITH T1 AS(
            select	YEAR(@att_name) as visit_year, count(person_id) as person_count  FROM @cdm_database_schema.@tbl_name
            GROUP BY YEAR(@att_name)
            )
            SELECT visit_year, ROUND(person_count/CAST((SELECT SUM(person_count) FROM T1)AS float)*100,1) as person_ratio FROM T1
            ORDER BY visit_year ASC;

--conditiontbl_diff_date
SELECT DATEDIFF(day, @start_name, @end_name) as day_diff, count(person_id) as person_count FROM @cdm_database_schema.@tbl_name
			GROUP BY DATEDIFF(day, @start_name, @end_name)
			ORDER BY DATEDIFF(day, @start_name, @end_name) ASC;

--conditiontbl_type_concept
SELECT @att_name as concept_id, (SELECT CONCEPT_NAME FROM @cdm_database_schema.concept where concept_id = @att_name) as attribute_name,
            round(count(@att_name)/cast((SELECT count(*) FROM @cdm_database_schema.@tbl_name)AS float)*100,1) as ratio
            FROM @cdm_database_schema.@tbl_name
            GROUP BY @att_name;

--conditiontbl_stop
select 'stop reason' as attribute_name ,count(distinct stop_reason) as attribute_count
            from @cdm_database_schema.@tbl_name;

--conditiontbl_visit_occurrence
select 'Associate' as attribute_name, ROUND((count(@att_name)/CAST(count(@compared_att_name)AS float))*100,1) as ratio
            from @cdm_database_schema.@tbl_name;

--conditiontbl_visit_detail
select 'Associate' as attribute_name, ROUND((count(@att_name)/CAST(count(@compared_att_name)AS float))*100,1) as ratio
            from @cdm_database_schema.@tbl_name;
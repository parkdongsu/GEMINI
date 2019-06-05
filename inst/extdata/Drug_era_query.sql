--drug_eratbl_person_ratio
SELECT 'table person' as attribute_name,
            ROUND(count(distinct person_id)/CAST((SELECT count(distinct person_id) FROM @cdm_database_schema.person)AS FLOAT)*100,1) as ratio
            FROM @cdm_database_schema.@tbl_name;			

--drug_eratbl_start
WITH T1 AS(
            select	YEAR(@att_name) as visit_year, count(person_id) as person_count  FROM @cdm_database_schema.@tbl_name
            GROUP BY YEAR(@att_name)
            )
            SELECT visit_year, ROUND(person_count/CAST((SELECT SUM(person_count) FROM T1)AS FLOAT)*100,1) as person_ratio FROM T1
            ORDER BY visit_year ASC;

--drug_eratbl_end
WITH T1 AS(
            select	YEAR(@att_name) as visit_year, count(person_id) as person_count  FROM @cdm_database_schema.@tbl_name
            GROUP BY YEAR(@att_name)
            )
            SELECT visit_year, ROUND(person_count/CAST((SELECT SUM(person_count) FROM T1)AS FLOAT)*100,1) as person_ratio FROM T1
            ORDER BY visit_year ASC;
			
--drug_eratbl_diff_date
SELECT DATEDIFF(day, @start_name, @end_name) as day_diff, count(person_id) as person_count FROM @cdm_database_schema.@tbl_name
			GROUP BY DATEDIFF(day, @start_name, @end_name)
			ORDER BY DATEDIFF(day, @start_name, @end_name) ASC;

--drug_eratbl_exp_count
SELECT @att_name as concept_id, (SELECT CONCEPT_NAME FROM @cdm_database_schema.concept where concept_id = @att_name) as attribute_name,
            round(count(@att_name)/CAST((SELECT count(*) FROM @cdm_database_schema.@tbl_name)AS FLOAT)*100,1) as ratio
            FROM @cdm_database_schema.@tbl_name
            GROUP BY @att_name;

--drug_eratbl_gap_days
WITH T1 AS(
  SELECT SUM(person) as person_count, gap_day_range FROM(
    SELECT count(person_id) as person ,CAST((gap_days/5)*5 as VARCHAR)+ '~' + CAST((gap_days/5)*5+4 as VARCHAR) as gap_day_range FROM @cdm_database_schema.drug_era
    GROUP BY gap_days)AS TEMP
  GROUP BY gap_day_range
)
SELECT gap_day_range, ROUND((person_count/CAST((SELECT SUM(person_count) FROM T1)AS FLOAT))*100,1) as person_ratio FROM T1
--persontbl_person_ratio
SELECT 'table person' as attribute_name,
            ROUND(count(distinct person_id)/CAST((SELECT count(distinct person_id) FROM @cdm_database_schema.person)AS float)*100,1) as ratio
            FROM @cdm_database_schema.@tbl_name;			

--persontbl_gender
SELECT (SELECT CONCEPT_NAME FROM @cdm_database_schema.concept where concept_id = @att_name) as attribute_name,
                round(count(distinct person_id)/CAST((SELECT count(distinct person_id)
                FROM @cdm_database_schema.@tbl_name)AS float)*100,1) as ratio
                FROM @cdm_database_schema.@tbl_name
                GROUP BY @att_name;

--persontbl_min_age
WITH T1 AS(
SELECT SUM(person_count) AS person_count, AGE_RANGE, gender_concept_id FROM(
SELECT	count(A.person_ID) as person_count
		, CAST(((MIN(YEAR(OBSERVATION_PERIOD_START_DATE)-year_of_birth))/5)*5 as VARCHAR)+ '~' + CAST(((MIN(YEAR(OBSERVATION_PERIOD_START_DATE)-year_of_birth))/5)*5+4 as VARCHAR) as AGE_RANGE
		, gender_concept_id FROM @cdm_database_schema.observation_period AS A
INNER JOIN @cdm_database_schema.person AS B ON A.PERSON_ID = B.person_id
GROUP BY A.person_id, YEAR(OBSERVATION_PERIOD_START_DATE)-year_of_birth, gender_concept_id
) AS TEMP
GROUP BY AGE_RANGE, gender_concept_id
)
SELECT AGE_RANGE, gender_concept_id, ROUND(person_count*100/CAST((SELECT SUM(person_count) FROM T1)AS float),1) as ratio FROM T1
ORDER BY CAST(LEFT(AGE_RANGE,CHARINDEX('~',AGE_RANGE)-1)AS INT) ASC ,gender_concept_id;

--persontbl_max_age
WITH T1 AS(
SELECT SUM(person_count) AS person_count, AGE_RANGE, gender_concept_id FROM(
SELECT	count(A.person_ID) as person_count
, CAST(((MAX(YEAR(OBSERVATION_PERIOD_START_DATE)-year_of_birth))/5)*5 as VARCHAR)+ '~' + CAST(((MAX(YEAR(OBSERVATION_PERIOD_START_DATE)-year_of_birth))/5)*5+4 as VARCHAR) as AGE_RANGE
, gender_concept_id FROM @cdm_database_schema.observation_period AS A
INNER JOIN @cdm_database_schema.person AS B ON A.PERSON_ID = B.person_id
GROUP BY A.person_id, YEAR(OBSERVATION_PERIOD_START_DATE)-year_of_birth, gender_concept_id
) AS TEMP
GROUP BY AGE_RANGE, gender_concept_id
)
SELECT AGE_RANGE, gender_concept_id, ROUND(person_count*100/CAST((SELECT SUM(person_count) FROM T1)AS float),1) as ratio FROM T1
ORDER BY CAST(LEFT(AGE_RANGE,CHARINDEX('~',AGE_RANGE)-1)AS INT) ASC ,gender_concept_id;

--persontbl_race
SELECT @att_name as concept_id, (SELECT CONCEPT_NAME FROM @cdm_database_schema.concept where concept_id = @att_name) as attribute_name,
            round(count(@att_name)/CAST((SELECT count(*) FROM @cdm_database_schema.@tbl_name) AS float)*100,1) as ratio
            FROM @cdm_database_schema.@tbl_name
            GROUP BY @att_name;

--persontbl_ethnicity
SELECT @att_name as concept_id, (SELECT CONCEPT_NAME FROM @cdm_database_schema.concept where concept_id = @att_name) as attribute_name,
            round(count(@att_name)/CAST((SELECT count(*) FROM @cdm_database_schema.@tbl_name)AS float)*100,1) as ratio
            FROM @cdm_database_schema.@tbl_name
            GROUP BY @att_name;

--persontbl_location
select '@att_name' as attribute_name ,count(distinct @att_name) as attribute_count,
            round(SUM(CASE WHEN @att_name IS NULL THEN 1 ELSE 0 END)/CAST(SUM(CASE WHEN @att_name IS NULL THEN 1 ELSE 1 END)AS float)*100,1) as null_ratio
            from @cdm_database_schema.@tbl_name;
			
--persontbl_provider
select '@att_name' as attribute_name ,count(distinct @att_name) as attribute_count,
            round(SUM(CASE WHEN @att_name IS NULL THEN 1 ELSE 0 END)/CAST(SUM(CASE WHEN @att_name IS NULL THEN 1 ELSE 1 END)AS float)*100,1) as null_ratio
            from @cdm_database_schema.@tbl_name;

--persontbl_care_site
select 'care_site_id' as attribute_name ,count(care_site_id) as attribute_count,
round(SUM(CASE WHEN care_site_id IS NULL THEN 1 ELSE 0 END)/CAST(SUM(CASE WHEN care_site_id IS NULL THEN 1 ELSE 1 END)AS float)*100,1) as null_ratio
              from @cdm_database_schema.person;
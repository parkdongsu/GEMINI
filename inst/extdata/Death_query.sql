-- deathtbl_check
SELECT attribute_name, ROUND(count(attribute_name)*100/CAST((select count(distinct person_id) from @cdm_database_schema.death)AS FLOAT),1) as ratio
        from (select count(death_date) as attribute_name from @cdm_database_schema.death group by person_id)
              AS death_date_temp group by attribute_name order by attribute_name;
			  
-- deattbl_type
SELECT (SELECT CONCEPT_NAME FROM @cdm_database_schema.concept where concept_id = death_type_concept_id) as attribute_name,
              round(100*count(distinct person_id)/CAST((SELECT count(distinct person_id) FROM @cdm_database_schema.death)AS FLOAT),1) as ratio
              FROM @cdm_database_schema.death
              GROUP BY death_type_concept_id;
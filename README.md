# GEMINI

Gemini means 'GEneral exaMINing and visualizing application for paired Institution'.
It uses a connection of DB for extract a part of CDM data and makes rds files where you want.

# TABLE required in CDM

PERSON

DEATH

VISIT OCCURRENCE

CONDITION OCCURRENCE

DRUG EXPOSURE

DRUG ERA

(OBSERVATION_PERIOD)

# HOW TO USE

0. Requirement

* Install Java SDK (developed in 8 Update 181)

1. Install GEMINI package using install_github().

```
install.packages("devtools")
library(devtools)
install_github("https://github.com/ABMI/GEMINI.git")
library(gemini)
```

2. After install, create_rds() to create RDS files.

You can set your own work directory path and create server_info.cfg file to connect DB server.
What you type down in Rstudio the server_info.cfg file in the following format.


```
dbName=<sql server name>
server=<server IP>
schemaName=<schema name>
user=<ID>
password=<password>
```

This process creates rds files zipped.

If you already have RDS files, put them in 'Standard RDS' folder and follow next process.

3. Request RDS files to other institution.

This RDS files(or zip file) must be stored in 'Target RDS' folder.

For testing, only standard rds files can run GEMINI but line graph will be shown single institution rds data.
  
4. Run gemini()

You should set path where 'Standard RDS' folder and 'Target RDS' folder exist.

Then GEMINI will create images folder(with image files), markdown file and html file and will appear browser to show you a report(html file) what GEMINI made.

If data is NULL (or image file shows 'No Data'), check the CDM data really it NULL.

> If R studio encoding is CP949 (Window default), don't open gemini_md.Rmd file until change R studio encoding to UTF-8. If not, it occurs encoding crashes.

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

```R
install.packages("devtools")
library(devtools)
install_github("https://github.com/parkdongsu/GEMINI.git", ref="geminiUI")
library(gemini)
```

2. After install, Run Gemini

````R
gemini::gemini_run()
````

3. description
- Create rds file : This process creates rds files zipped.
- gemini : Register the zip file of the institution you want to compare and press the Start button.


> If R studio encoding is CP949 (Window default), don't open gemini_md.Rmd file until change R studio encoding to UTF-8. If not, it occurs encoding crashes.

# Poster
![Poster](/OHDSI_GEMINI_poster.png)

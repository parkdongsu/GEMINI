#' Draw visit occurrence
#'
#' This function for draw graph from visit_occurrence RDS data
#' @keywords gemini
#' @export
#'
################################################################################
# VISIT OCCURRENCE TABLE VISUALLIZATION
################################################################################
draw_visit_occurrence <- function(std_schema_name,tar_schema_name){
    cat("Visit occurrence data visualizing...\n")
    visit_record_title <- "Comparison of records between institutions"
    visit_person_title <-"Comparison of person ratio between institutions"
    visit_concept_title <- "Comparison of visit concept between institutions"
    visit_diff_title <- "Comparison of duration between institutions"
    visit_start_title <- "Drug Exposure Start Date"
    visit_end_title <- "Drug Exposure End Date"
    visit_type_title <- "Comparison of visit type between institutions"
    visit_care_title <- "Comparison of stop reason between institutions"
    visit_source_title <- "Comparison of visit source between institutions"
    visit_admit_title <- "Comparison of visit admitting between institutions"
    visit_discharge_title <-"Comparison of visit discharge between institutions"
    visit_preceding_title <-"Comparison of visit precending between institutions"
################################################################################
# visit_occurrence_record
################################################################################
draw_table_pie(std_visittbl_record, tar_visittbl_record, "VISIT TABLE", "Visit/00.Visit_record.jpg")
mtext(visit_record_title, font = 2, side = 3, line = -5, outer = T, cex = 2.0)
mtext(paste0("count : ",std_visittbl_record$count), side = 1, line = -15, at=0.75, outer = T, cex = 1.5)
mtext(paste0("count : ",tar_visittbl_record$count), side = 1, line = -15, at=0.25, outer = T, cex = 1.5)
# Graph Save
dev.off() # It protect previous jpg file to not change current jpg image.
################################################################################
# visit_occurrence_person_id
################################################################################
draw_table_pie(std_visittbl_person_ratio, tar_visittbl_person_ratio, "VISIT PERSON", "Visit/01.Visit_person.jpg")
mtext(visit_person_title, font = 2, side = 3, line = -5, outer = T, cex = 2.0)
# Graph Save
dev.off() # It protect previous jpg file to not change current jpg image.
################################################################################
# visit_occurrence visit_concept_id
# too low value, each label cross
# uncomplete
################################################################################
draw_ratio_pie(std_visittbl_visit_concept, tar_visittbl_visit_concept, "Visit/02.Visit_concept.jpg")
mtext(visit_concept_title, font = 2, side = 3, line = -5, outer = T, cex = 2.0)
# par(mfrow=c(1,2))
# barplot(matrix(std_visittbl_visit_concept$ratio),legend=std_visittbl_visit_concept$attributeName, col = rainbow(nrow(std_visittbl_visit_concept)))
# barplot(matrix(tar_visittbl_visit_concept$ratio),legend=tar_visittbl_visit_concept$attributeName, col = rainbow(nrow(std_visittbl_visit_concept)))
# Graph Save
dev.off() # It protect previous jpg file to not change current jpg image.
################################################################################
# visit_occurrence_diff_date
################################################################################
# Image file open
jpeg(
    filename = "images/Visit/03.Visit_duration.jpg",
    width = 720, height = 720, quality = 75, bg = "white"
)

par(mfrow = c(1, 2), oma = c(0, 0, 2, 0))
tryCatch(
    hist(std_visittbl_diff_date$dayDiff, breaks = 25, xlab = "Visit Duration", main = std_schema_name, cex.main = 1.5, cex.axis = 1.5, cex.lab = 2.0)
    , # If data isn't exist...
    error = function(error_message) {
        print(error_message)
        afterError()
    }
)
tryCatch(
    hist(tar_visittbl_diff_date$dayDiff, breaks = 25, xlab = "Visit Duration", main = tar_schema_name, cex.main = 1.5, cex.axis = 1.5, cex.lab = 2.0)
    , # If data isn't exist...
    error = function(error_message) {
        print(error_message)
        afterError()
    }
)
title(main = visit_diff_title, line = -1, outer = TRUE, cex.main = 2.0)
# file close
dev.off() # It protect previous jpg file to not change current jpg image.
################################################################################
# visit_occurrence_start_date
# Using sd
# sd uncomplete
################################################################################
draw_line_start(std_visittbl_start, tar_visittbl_start, visit_start_title, "Visit/04.Visit_start.jpg")
# Graph Save
dev.off() # It protect previous jpg file to not change current jpg image.
################################################################################
# visit_occurrence_end_date
# std_visittbl_end error. [1,][2,] no difference
################################################################################
# 2999, NA Problem issue
if (length(std_visittbl_end[is.na(std_visittbl_end$visitYear)]) != 0) {
    std_visittbl_na_end <<- std_visittbl_end[is.na(std_visittbl_end$visitYear), 2]
    temp_std_s <<- 2
} else {
    std_visittbl_na_end <<- NA
    temp_std_s <<- 1
}
if (length(std_visittbl_end[std_visittbl_end$visitYear == 2999, ]$visitYear) != 0) {
    std_visittbl_over_end <<- std_visittbl_end[na.omit(std_visittbl_end$visitYear == 2999), 2]
    temp_std_e <<- nrow(std_visittbl_end) - 1
} else {
    std_visittbl_over_end <<- NA
    temp_std_e <<- nrow(std_visittbl_end)
}
if (length(tar_visittbl_end[is.na(tar_visittbl_end$visitYear)]) != 0) {
    tar_visittbl_na_end <<- tar_visittbl_end[is.na(tar_visittbl_end$visitYear), 2]
    temp_tar_s <<- 2
} else {
    tar_visittbl_na_end <<- NA
    temp_tar_s <<- 1
}
if (length(tar_visittbl_end[tar_visittbl_end$visitYear == 2999, ]$visitYear) != 0) {
    tar_visittbl_over_end <<- tar_visittbl_end[na.omit(tar_visittbl_end$visitYear == 2999), 2]
    temp_tar_e <<- nrow(tar_visittbl_end) - 1
} else {
    tar_visittbl_over_end <<- NA
    temp_tar_e <<- nrow(tar_visittbl_end)
}
visit_na_end <<- c(std_visittbl_na_end, tar_visittbl_na_end)
visit_over_end <<- c(std_visittbl_over_end, tar_visittbl_over_end)
# Grid line sketch
draw_line_end(std_visittbl_end[temp_std_s:temp_std_e, ], tar_visittbl_end[temp_tar_s:temp_tar_e, ], visit_na_end, visit_over_end,visit_end_title, "Visit/05.Visit_end.jpg")
# Graph Save
dev.off() # It protect previous jpg file to not change current jpg image.
################################################################################
# visit_occurrence_type_concept
################################################################################
draw_ratio_pie(std_visittbl_type_concept, tar_visittbl_type_concept, "Visit/06.Visit_type.jpg")
mtext(visit_concept_title, font = 2, side = 3, line = -5, outer = T, cex = 2.0)
# Graph Save
dev.off() # It protect previous jpg file to not change current jpg image.
################################################################################
# visit_occurrence care_site
################################################################################
draw_null_bar(std_visittbl_care_site, tar_visittbl_care_site, visit_care_title, "Visit/07.Visit_care_site.jpg")
# Graph Save
dev.off() # It protect previous jpg file to not change current jpg image.
################################################################################
# visit_occurrence visit_source_concept_id
################################################################################
draw_ratio_pie(std_visittbl_source_concept, tar_visittbl_source_concept, "Visit/08.Visit_source.jpg")
mtext(visit_source_title, font = 2, side = 3, line = -5, outer = T, cex = 2.0)
# Graph Save
dev.off() # It protect previous jpg file to not change current jpg image.
################################################################################
# visit_occurrence admitting_source_concept_id
# No data in NHIS
################################################################################
draw_ratio_pie(std_visittbl_admitting_source, tar_visittbl_admitting_source, "Visit/09.Visit_admitting.jpg")
mtext(visit_admit_title, font = 2, side = 3, line = -5, outer = T, cex = 2.0)
# Graph Save
dev.off() # It protect previous jpg file to not change current jpg image.
################################################################################
# visit_occurrence discharge_to_concept_id
# No data in NHIS
################################################################################
draw_ratio_pie(std_visittbl_discharge, tar_visittbl_discharge, "Visit/10.Visit_discharge.jpg")
mtext(visit_discharge_title, font = 2, side = 3, line = -5, outer = T, cex = 2.0)
# Graph Save
dev.off() # It protect previous jpg file to not change current jpg image.
################################################################################
# visit_occurrence preceding_visit_occurrence_id
# No data in NHIS
################################################################################
draw_compare_pie(std_visittbl_preceding, tar_visittbl_preceding, "Visit/11.Visit_preceding.jpg")
mtext(visit_preceding_title, font = 2, side = 3, line = -5, outer = T, cex = 2.0)
# Graph Save
dev.off() # It protect previous jpg file to not change current jpg image.
}

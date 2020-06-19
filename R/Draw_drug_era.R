#' Draw drug era
#'
#' This function for draw graph from drug era RDS data
#' @keywords gemini
#' @export
#'
################################################################################
# DRUG ERA TABLE VISUALLIZATION
################################################################################
draw_drug_era <- function(std_schema_name,tar_schema_name){
    cat("Drug era data visualizing...\n")
    drug_era_record_title <- "Comparison of records between institutions"
    drug_era_person_title <-"Comparison of person ratio between institutions"
    drug_era_start_title <- "Drug Era Start Date"
    drug_era_end_title <- "Drug Era End Date"
    drug_era_diff_title <- "Comparison of duration between institutions"
    drug_era_gap_title <- "Comparison of gap day between institutions"
################################################################################
# drug_era_record
################################################################################
draw_table_pie(std_drug_eratbl_record, tar_drug_eratbl_record, "DRUG ERA\nTABLE", "Drug era/00.Drug_era_record.jpg")
mtext(drug_era_record_title, font = 2, side = 3, line = -5, outer = T, cex = 2.0)
mtext(paste0("count : ",std_drug_eratbl_record$count), side = 1, line = -15, at=0.75, outer = T, cex = 1.5)
mtext(paste0("count : ",tar_drug_eratbl_record$count), side = 1, line = -15, at=0.25, outer = T, cex = 1.5)
# Graph Save
dev.off() # It protect previous jpg file to not change current jpg image.
################################################################################
# drug_era person_id
################################################################################
draw_table_pie(std_drug_eratbl_person_ratio, tar_drug_eratbl_person_ratio, "DRUG ERA\nPERSON", "Drug era/01.Drug_era_person.jpg")
mtext(drug_era_person_title, font = 2, side = 3, line = -5, outer = T, cex = 2.0)
# Graph Save
dev.off() # It protect previous jpg file to not change current jpg image.
################################################################################
# drug_era start_date
################################################################################
draw_line_start(std_drug_eratbl_start, tar_drug_eratbl_start, drug_era_start_title, "Drug era/02.Drug_era_start.jpg")
# Graph Save
dev.off() # It protect previous jpg file to not change current jpg image.
################################################################################
# drug_era end_date
################################################################################
# 2999, NA Problem issue
if (length(std_drug_eratbl_end[is.na(std_drug_eratbl_end$visitYear)]) != 0) {
    std_drug_eratbl_na_end <<- std_drug_eratbl_end[is.na(std_drug_eratbl_end$visitYear), 2]
    temp_std_s <<- 2
} else {
    std_drug_eratbl_na_end <<- NA
    temp_std_s <<- 1
}
if (length(std_drug_eratbl_end[std_drug_eratbl_end$visitYear == 2999, ]$visitYear) != 0) {
    std_drug_eratbl_over_end <<- std_drug_eratbl_end[na.omit(std_drug_eratbl_end$visitYear == 2999), 2]
    temp_std_e <<- nrow(std_drug_eratbl_end) - 1
} else {
    std_drug_eratbl_over_end <<- NA
    temp_std_e <<- nrow(std_drug_eratbl_end)
}
if (length(tar_drug_eratbl_end[is.na(tar_drug_eratbl_end$visitYear)]) != 0) {
    tar_drug_eratbl_na_end <<- tar_drug_eratbl_end[is.na(tar_drug_eratbl_end$visitYear), 2]
    temp_tar_s <<- 2
} else {
    tar_drug_eratbl_na_end <<- NA
    temp_tar_s <<- 1
}
if (length(tar_drug_eratbl_end[tar_drug_eratbl_end$visitYear == 2999, ]$visitYear) != 0) {
    tar_drug_eratbl_over_end <<- tar_drug_eratbl_end[tar_drug_eratbl_end$visitYear == 2999, 2]
    temp_tar_e <<- nrow(tar_drug_eratbl_end) - 1
} else {
    tar_drug_eratbl_over_end <<- NA
    temp_tar_e <<- nrow(tar_drug_eratbl_end)
} # Sometimes there sjlt

drug_era_na_end <<- c(std_drug_eratbl_na_end, tar_drug_eratbl_na_end)
drug_era_over_end <<- c(std_drug_eratbl_over_end, tar_drug_eratbl_over_end)

draw_line_end(
    std_drug_eratbl_end[temp_std_s:temp_std_e, ], tar_drug_eratbl_end[temp_tar_s:temp_tar_e, ], drug_era_na_end, drug_era_over_end,
    drug_era_end_title, "Drug era/03.Drug_era_end.jpg"
)

# Graph Save
dev.off() # It protect previous jpg file to not change current jpg image.
################################################################################
# drug_era diff_date
################################################################################
jpeg(
    filename = "images/Drug era/04.Drug_era_duration.jpg",
    width = 720, height = 720, quality = 75, bg = "white"
)
par(mfrow = c(1, 2), oma = c(0, 0, 2, 0))
tryCatch(
    hist(std_drug_eratbl_diff_date$dayDiff, breaks = 25, xlab = "Drug Era Duration", main = std_schema_name, cex.main = 2.0, cex.axis = 1.5, cex.lab = 1.5)
    , # If data isn't exist...
    error = function(error_message) {
        print(error_message)
        afterError()
    }
)
tryCatch(
    hist(tar_drug_eratbl_diff_date$dayDiff, breaks = 25, xlab = "Drug Era Duration", main = tar_schema_name, cex.main = 2.0, cex.axis = 1.5, cex.lab = 1.5)
    , # If data isn't exist...
    error = function(error_message) {
        print(error_message)
        afterError()
    }
)
title(drug_era_diff_title, outer = T, cex.main = 2.0)
# Graph Save
dev.off() # It protect previous jpg file to not change current jpg image.
################################################################################
# drug_era exp_count
################################################################################
# draw_ratio_pie(std_drug_eratbl_exp_count,tar_drug_eratbl_exp_count)
# mtext("Drug Exposure Count",font=2,side=3, line= -5,outer=T, cex = 1.5)
# #Graph Save
# dev.copy(device = jpeg ,filename=paste0("images/Drug era/"))
# dev.off() #It protect previous jpg file to not change current jpg image.
################################################################################
# drug_era gap_days
################################################################################
jpeg(
    filename = "images/Drug era/05.Drug_era_gap.jpg",
    width = 720, height = 720, quality = 75, bg = "white"
)
par(mfrow = c(1, 2), oma = c(0, 0, 2, 0))
tryCatch(
    hist(std_drug_eratbl_gap_days$personRatio, breaks = 25, xlab = "Gap Days Duration", main = std_schema_name, cex.main = 2.0, cex.axis = 1.5, cex.lab = 1.5)
    , # If data isn't exist...
    error = function(error_message) {
        print(error_message)
        afterError()
    }
)
tryCatch(
    hist(tar_drug_eratbl_gap_days$personRatio, breaks = 25, xlab = "Gap Days Duration", main = tar_schema_name, cex.main = 2.0, cex.axis = 1.5, cex.lab = 1.5)
    , # If data isn't exist...
    error = function(error_message) {
        print(error_message)
        afterError()
    }
)
title(drug_era_gap_title, outer = T, cex.main = 2.0)
dev.off() # It protect previous jpg file to not change current jpg image.
}

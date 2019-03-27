#' Draw whole
#'
#' This function for draw graph from summary RDS data
#' @keywords gemini
#' @export
#'
################################################################################
# WHOLE TABLE info VISUALLIZATION
################################################################################
draw_whole<-function(std_schema_name,tar_schema_name){
    cat("Summary data visualizing...\n")
    whole_record_title <- "Comparison of records between institutions"
    whole_person_title <- "Comparison of person between institutions"
################################################################################
# WHOLE TABLE Record info
################################################################################
par(mfrow = c(1, 1), xpd = T)
jpeg(
    filename = "images/Whole/00.Record.jpg",
    width = 720, height = 720, quality = 75, bg = "white"
)
tryCatch({
    record_bar <<- barplot(c(
        std_persontbl_record$ratio[1], tar_persontbl_record$ratio[1], std_visittbl_record$ratio[1], tar_visittbl_record$ratio[1],
        std_conditiontbl_record$ratio[1], tar_conditiontbl_record$ratio[1], std_drug_exptbl_record$ratio[1], tar_drug_exptbl_record$ratio[1],
        std_drug_eratbl_record$ratio[1], tar_drug_eratbl_record$ratio[1]
    ),
    beside = F, names = c(
        "Person", NA, "Visit", NA, "Condition", NA,
        "Drug exp", NA, "Drug era", NA
    ),
    ylim = c(0, 100), col = c("Green", "Yellow"), main = whole_record_title, xlab = "Table name", ylab = "Percentage (%)", cex.axis = 1.5, cex.names = 1.5,
    cex.main = 2.0, cex.lab = 1.5
    )
    text(
        x = record_bar, y = c(
            std_persontbl_record$ratio[1], tar_persontbl_record$ratio[1], std_visittbl_record$ratio[1], tar_visittbl_record$ratio[1],
            std_conditiontbl_record$ratio[1], tar_conditiontbl_record$ratio[1], std_drug_exptbl_record$ratio[1], tar_drug_exptbl_record$ratio[1],
            std_drug_eratbl_record$ratio[1], tar_drug_eratbl_record$ratio[1]
        ),
        labels = c(
            label_sort(std_persontbl_record$ratio[1], tar_persontbl_record$ratio[1]),
            label_sort(std_visittbl_record$ratio[1], tar_visittbl_record$ratio[1]),
            label_sort(std_conditiontbl_record$ratio[1], tar_conditiontbl_record$ratio[1]),
            label_sort(std_drug_exptbl_record$ratio[1], tar_drug_exptbl_record$ratio[1]),
            label_sort(std_drug_eratbl_record$ratio[1], tar_drug_eratbl_record$ratio[1])
        ), col = "black", cex = 2.0
    )
    legend("topleft", c(std_schema_name, tar_schema_name), pch = 15, cex = 1.5, col = c("green", "yellow"))
}, # If data isn't exist...
error = function(error_message) {
    print(error_message)
    afterError()
}
)
# dev.copy(device = jpeg , filename=paste0())
dev.off()

################################################################################
# WHOLE TABLE Person info
################################################################################
par(mfrow = c(1, 1))
jpeg(
    filename = "images/Whole/01.Person.jpg",
    width = 720, height = 720, quality = 75, bg = "white"
)
# Draw graph in one bar chart
tryCatch({
    person_bar <<- barplot(c(
        std_persontbl_person_ratio$ratio, tar_persontbl_person_ratio$ratio, std_visittbl_person_ratio$ratio, tar_visittbl_person_ratio$ratio,
        std_conditiontbl_person_ratio$ratio, tar_conditiontbl_person_ratio$ratio, std_drug_exptbl_person_ratio$ratio, tar_drug_exptbl_person_ratio$ratio,
        std_drug_eratbl_person_ratio$ratio, tar_drug_eratbl_person_ratio$ratio
    ),
    beside = F, names = c(
        "Person", NA, "Visit", NA, "Condition", NA,
        "Drug exp", NA, "Drug era", NA
    ),
    ylim = c(0, 100), col = c("Green", "Yellow"), main = whole_person_title, xlab = "Table name", ylab = "Percentage (%)", cex.axis = 1.5, cex.names = 1.5,
    cex.main = 2.0, cex.lab = 1.5
    )
    text(
        x = person_bar, y = c(
            std_persontbl_person_ratio$ratio, tar_persontbl_person_ratio$ratio, std_visittbl_person_ratio$ratio, tar_visittbl_person_ratio$ratio,
            std_conditiontbl_person_ratio$ratio, tar_conditiontbl_person_ratio$ratio, std_drug_exptbl_person_ratio$ratio, tar_drug_exptbl_person_ratio$ratio,
            std_drug_eratbl_person_ratio$ratio, tar_drug_eratbl_person_ratio$ratio
        ),
        labels = c(
            label_sort(std_persontbl_person_ratio$ratio, tar_persontbl_person_ratio$ratio),
            label_sort(std_visittbl_person_ratio$ratio, tar_visittbl_person_ratio$ratio),
            label_sort(std_conditiontbl_person_ratio$ratio, tar_conditiontbl_person_ratio$ratio),
            label_sort(std_drug_exptbl_person_ratio$ratio, tar_drug_exptbl_person_ratio$ratio),
            label_sort(std_drug_eratbl_person_ratio$ratio, tar_drug_eratbl_person_ratio$ratio)
        ), col = "black", cex = 2.0
    )
    legend("bottomleft", c(std_schema_name, tar_schema_name), pch = 15, cex = 1.5, col = c("green", "yellow"))
}, # If data isn't exist...
error = function(error_message) {
    print(error_message)
    afterError()
}
)
# dev.copy(device = jpeg ,filename=paste0("images/Whole/01.Person.jpg"))
dev.off()
}

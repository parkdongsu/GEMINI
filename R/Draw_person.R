#' Draw person
#'
#' This function for draw graph from person RDS data
#' @keywords gemini
#' @export
#'
################################################################################
# PERSON TABLE VISUALLIZATION
################################################################################
draw_person <- function(std_schema_name,tar_schema_name){
    cat("Person data visualizing...\n")
    person_record_title <- "Comparison of records between institutions"
    person_gender_title <- "Comparison of gender between institutions"
    person_min_title <- "First Visit Person per Year"
    person_max_title <- "Last visit Person per Year"
    person_race_title <- "Comparison of race between institutions"
    person_ethnicity_title <- "Comparison of ethnicity between institutions"
    person_location_title <- "Number of location type"
    person_provider_title <- "Number of location type"
    person_care_title <- "Number of location type"
    ################################################################################
    # person_record
    ################################################################################
    draw_table_pie(std_persontbl_record, tar_persontbl_record, "PERSON TABLE", "Person/00.Person_record.jpg")
    mtext(person_record_title, side = 3, line = -5, outer = T, cex = 2.0, font = 2)
    mtext(paste0("count : ",std_persontbl_record$count), side = 1, line = -15, at=0.75, outer = T, cex = 1.5)
    mtext(paste0("count : ",tar_persontbl_record$count), side = 1, line = -15, at=0.25, outer = T, cex = 1.5)
    # Graph Save
    dev.off() # It protect previous jpg file to not change current jpg image.
    ################################################################################
    # gender_concept_id
    ################################################################################
    jpeg(
        filename = "images/Person/01.Person_gender.jpg",
        width = 720, height = 720, quality = 75, bg = "white"
    )
    par(mfrow = c(1, 1), xpd = T)
    # Draw graph in one bar chart
    tryCatch({
        gender_bar <<- barplot(c(std_persontbl_gender$ratio[1], tar_persontbl_gender$ratio[1], std_persontbl_gender$ratio[2], tar_persontbl_gender$ratio[2]),
                               beside = F, ylim = c(0, 100), names = c(
                                   std_persontbl_gender$attributeName[1], tar_persontbl_gender$attributeName[1],
                                   std_persontbl_gender$attributeName[2], tar_persontbl_gender$attributeName[2]
                               ),
                               col = c("Green", "Yellow"), main = person_gender_title, xlab = "Gender", ylab = "Percentage (%)", cex.main = 2.0, cex.lab = 1.5,
                               cex.names = 1.5, cex.axis = 1.5
        )
        text(
            x = gender_bar, y = c(std_persontbl_gender$ratio[1], tar_persontbl_gender$ratio[1], std_persontbl_gender$ratio[2], tar_persontbl_gender$ratio[2]),
            labels = c(
                label_sort(std_persontbl_gender$ratio[1], tar_persontbl_gender$ratio[1]),
                label_sort(std_persontbl_gender$ratio[2], tar_persontbl_gender$ratio[2])
            ), col = "black", cex = 2.0
        )
        legend("topleft", c(std_schema_name, tar_schema_name), pch = 15, cex = 2.0, col = c("green", "yellow"))
    }, # If data isn't exist...
    error = function(error_message) {
        print(error_message)
        afterError()
    }
    )
    # Graph Save
    dev.off()
    ################################################################################
    # year_of_birth (min)
    ################################################################################
    jpeg(
        filename = "images/Person/02.Person_min_year.jpg",
        width = 720, height = 720, quality = 75, bg = "white"
    )
    par(mfrow = c(1, 1), xpd = F)
    # Divide ratio by Gender
    tryCatch({
        male_min_ratio <<- std_persontbl_min_age$ratio[std_persontbl_min_age$genderConceptId == "8507"]
        female_min_ratio <<- std_persontbl_min_age$ratio[std_persontbl_min_age$genderConceptId == "8532"]

        compared_male_min_ratio <<- tar_persontbl_min_age$ratio[tar_persontbl_min_age$genderConceptId == "8507"]
        compared_female_min_ratio <<- tar_persontbl_min_age$ratio[tar_persontbl_min_age$genderConceptId == "8532"]
        # Set label which got more long length
        min_lbl <<- union(std_persontbl_min_age$ageRange,std_persontbl_max_age$ageRange)
        # Draw line Graph
        plot(male_min_ratio,
             type = "p", pch = 19, col = 4, cex = 2, xlab = "AGE_RANGE", ylab = "GENDER_RATIO(%)", axes = F,
             ylim = c(0, as.numeric(max(c(male_min_ratio, female_min_ratio, compared_male_min_ratio, compared_female_min_ratio)))),
             xlim = c(0, length(min_lbl)), main = person_min_title, cex.main = 2.0, cex.lab = 1.5
        )
        axis(1, at = c(1:length(min_lbl)), labels = min_lbl, cex.axis = 1.5)
        # Set y_axis which got more big value
        if (max(std_persontbl_min_age$ratio) > max(tar_persontbl_min_age$ratio)) {
            y_axis <<- as.numeric(max(std_persontbl_min_age$ratio))
        } else {
            y_axis <<- as.numeric(max(tar_persontbl_min_age$ratio))
        }
        axis(2, at = c(0:ceiling(y_axis)), cex.axis = 1.5)

        # add another line graph
        lines(female_min_ratio, type = "p", pch = 19, col = 2, cex = 2) +
            lines(compared_male_min_ratio, type = "p", pch = 18, cex = 2.5,col = "cyan4") +
            lines(compared_female_min_ratio, type = "p", pch = 18, cex = 2.5, col = "brown")
        box()
        abline(
            h = c(0:ceiling(as.numeric(max(std_persontbl_min_age$ratio, tar_persontbl_min_age$ratio)))),
            v = c(1:(max(nrow(std_persontbl_min_age), nrow(tar_persontbl_min_age)) / 2)), lty = 3
        )
        legend("topright", c(
            "A.Male", "A.Female",
            "B.Male", "B.Female"
        ), pch = c(19, 19, 18, 18), cex = 2.0, col = c("blue", "red", "cyan4", "brown"))
    }, # If data isn't exist...
    error = function(error_message) {
        print(error_message)
        afterError()
    }
    )
    # Graph Save
    dev.off()
    ################################################################################
    # year_of_birth (max)
    ################################################################################
    jpeg(
        filename = "images/Person/03.Person_max_year.jpg",
        width = 720, height = 720, quality = 75, bg = "white"
    )
    par(mfrow = c(1, 1), xpd = F)
    # Divide ratio by Gender
    tryCatch({
        male_max_ratio <<- std_persontbl_max_age$ratio[std_persontbl_max_age$genderConceptId == "8507"]
        female_max_ratio <<- std_persontbl_max_age$ratio[std_persontbl_max_age$genderConceptId == "8532"]
        compared_male_max_ratio <<- tar_persontbl_max_age$ratio[tar_persontbl_max_age$genderConceptId == "8507"]
        compared_female_max_ratio <<- tar_persontbl_max_age$ratio[tar_persontbl_max_age$genderConceptId == "8532"]
        # Set label which got more long length
        if (length(std_persontbl_max_age$ageRange[std_persontbl_max_age$genderConceptId == "8507"])
            > length(tar_persontbl_max_age$ageRange[tar_persontbl_max_age$genderConceptId == "8507"])) {
            x_max_lbl <- std_persontbl_max_age$ageRange[std_persontbl_max_age$genderConceptId == "8507"]
        } else {
            x_max_lbl <- tar_persontbl_max_age$ageRange[tar_persontbl_max_age$genderConceptId == "8507"]
        }
        # Draw line Graph
        plot(male_max_ratio,
             type = "p", pch = 19, cex = 2, col = 4, xlab = "AGE_RANGE", ylab = "GENDER_RATIO(%)", axes = F,
             ylim = c(0, as.numeric(max(c(male_max_ratio, female_max_ratio, compared_male_max_ratio, compared_female_max_ratio)))),
             xlim = c(0, length(x_max_lbl)), main = person_max_title, cex.main = 2.0, cex.lab = 1.5
        )
        axis(1, at = c(1:length(x_max_lbl)), labels = x_max_lbl, cex.axis = 1.5)
        # Set y_axis which got more big value
        if (max(std_persontbl_max_age$ratio) > max(tar_persontbl_max_age$ratio)) {
            y_axis <<- as.numeric(max(std_persontbl_max_age$ratio))
        } else {
            y_axis <<- as.numeric(max(tar_persontbl_max_age$ratio))
        }
        axis(2, at = c(0:ceiling(y_axis)), cex.axis = 1.5)

        # add another line graph
        lines(female_max_ratio, type = "p", pch = 19, cex = 2, col = 2) +
            lines(compared_male_max_ratio, type = "p", pch = 18, cex = 2.5, col = "cyan4") +
            lines(compared_female_max_ratio, type = "p", pch = 18, cex = 2.5, col = "brown")
        box()
        abline(
            h = c(0:ceiling(as.numeric(max(std_persontbl_max_age$ratio, tar_persontbl_max_age$ratio)))),
            v = c(1:(max(nrow(std_persontbl_max_age), nrow(tar_persontbl_max_age)) / 2)), lty = 3
        )
        legend("topright", c(
            "A.Male", "A.Female",
            "B.Male", "B.Female"
        ), pch = c(19, 19, 18, 18), cex = 2.0, col = c("blue", "red", "cyan4", "brown"))
    }, # If data isn't exist...
    error = function(error_message) {
        print(error_message)
        afterError()
    }
    )
    # Graph Save
    dev.off()
    ################################################################################
    # race_concept_id
    ################################################################################
    draw_ratio_pie(std_persontbl_race, tar_persontbl_race, "Person/04.Person_race.jpg")
    mtext(person_race_title, font = 2, side = 3, line = -5, outer = T, cex = 2.5)
    # Graph Save
    dev.off() # It protect previous jpg file to not change current jpg image.
    ################################################################################
    # ethnicity_concept_id
    ################################################################################
    draw_ratio_pie(std_persontbl_ethnicity, tar_persontbl_ethnicity, "Person/05.Person_ethnicity.jpg")
    mtext(person_ethnicity_title, font = 2, side = 3, line = -5, outer = T, cex = 2.5)
    # Graph Save
    dev.off() # It protect previous jpg file to not change current jpg image.
    ################################################################################
    # location_concept_id
    ################################################################################
    draw_null_bar(std_persontbl_location, tar_persontbl_location, person_location_title, "Person/06.Person_location.jpg")
    # Graph Save
    dev.off() # It protect previous jpg file to not change current jpg image.
    ################################################################################
    # provider_concept_id
    ################################################################################
    draw_null_bar(std_persontbl_provider, tar_persontbl_provider, person_provider_title, "Person/07.Person_provider.jpg")
    # Graph Save
    dev.off() # It protect previous jpg file to not change current jpg image.
    ################################################################################
    # care_site_id
    ################################################################################
    draw_null_bar(std_persontbl_care_site, tar_persontbl_care_site, person_care_title, "Person/08.Person_care_site.jpg")
    # Graph Save
    dev.off() # It protect previous jpg file to not change current jpg image.
}

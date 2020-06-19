#' Draw death
#'
#' This function for draw graph from death RDS data
#' @keywords gemini
#' @export
#'
################################################################################
# DEATH TABLE VISUALLIZATION
################################################################################
draw_death <- function(){
    cat("Death data visualizing...\n")
    death_check_title <- "Check Multiple Death time between institutions"
    death_type_title <- "Comparison of death type between institutions"
################################################################################
# death_date
################################################################################
check.death <- function(temporary){
    if(nrow(temporary)!=1){
        main <- temporary[1,]
        main$attributeName[1] <- "Integrity data"
        main <- rbind(main, data.frame(attributeName = "Duplicate data", ratio = round(100 - temporary[1,]$ratio,1)))
    }else{
        main <- temporary
        main$attributeName <- "Integrity data"
    }

    return(main)
}
# For legend, Name should be change
draw_ratio_pie(check.death(std_deathtbl_check), check.death(tar_deathtbl_check), "Death/00.Death_deathcheck.jpg")
mtext(death_check_title, font = 2, side = 3, line = -5, outer = T, cex = 2.0)
# Graph Save
dev.off() # It protect previous jpg file to not change current jpg image.
################################################################################
# death_type_concept_id
################################################################################
draw_ratio_pie(std_deathtbl_type, tar_deathtbl_type, "Death/01.Death_type.jpg")
mtext(death_type_title, font = 2, side = 3, line = -5, outer = T, cex = 2.0)
# Graph Save
dev.off() # It protect previous jpg file to not change current jpg image.
}

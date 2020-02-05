#' Draw bar plot about CDM people count with plotly library
#'
#' @description
#' \code(draw_total_person) Draw bar plot about person count
#'
#' @details
#' \code(draw_total_person) Draw bar plot about person count
#'
#' @param dbName CDM database name
#' @param rds loaded CDM data
#'
#' @return
#' Return bar plot.
#'
#' @examples
#' \dontrun{
#' draw_total_person(dbName = 'MYCDM', rds = loaded_rds)
#' }
draw_total_person <- function(dbName, rds){
    Total <- c("Person","Visit Occurrence", "Condition Occurrence", "Drug Exposure", "Drug Era")
    plot <- plotly::plot_ly(x = ~Total,
                            y = c(rds[[1]]$persontbl_person_ratio$ratio,
                                  rds[[1]]$visittbl_person_ratio$ratio,
                                  rds[[1]]$conditiontbl_person_ratio$ratio,
                                  rds[[1]]$drug_exptbl_person_ratio$ratio,
                                  rds[[1]]$drug_eratbl_person_ratio$ratio),
                            hoverinfo = "y",
                            type = 'bar',
                            name = dbName[1], 
                            width = 1000)
    for(i in 2:length(dbName)){
        plot <- add_trace(p=plot, y=c(rds[[i]]$persontbl_person_ratio$ratio,
                                      rds[[i]]$visittbl_person_ratio$ratio,
                                      rds[[i]]$conditiontbl_person_ratio$ratio,
                                      rds[[i]]$drug_exptbl_person_ratio$ratio,
                                      rds[[i]]$drug_eratbl_person_ratio$ratio),
                          hoverinfo = "y",
                          name = dbName[i])
    }
    plot <- plotly::layout(p=plot,
                           title = "Total table Person", yaxis = list(title='Ratio(%)'), barmode='group')
    return(plot)
}
#draw_total_person(dbName, rds)

#' Draw bar plot about CDM record count with plotly library
#'
#' @description
#' \code(draw_total_record) Draw bar plot about record count
#'
#' @details
#' \code(draw_total_record) Draw bar plot about record count
#'
#' @param dbName CDM database name
#' @param rds loaded CDM data
#'
#' @return
#' Return bar plot.
#'
#' @examples
#' \dontrun{
#' draw_total_record(dbName = 'MYCDM', rds = loaded_rds)
#' }
draw_total_record <- function(dbName, rds){
    Total <- c("Person","Visit Occurrence", "Condition Occurrence", "Drug Exposure", "Drug Era")
    plot <- plotly::plot_ly(x = ~Total,
                            y = c(rds[[1]]$persontbl_record$ratio,
                                  rds[[1]]$visittbl_record$ratio,
                                  rds[[1]]$conditiontbl_record$ratio,
                                  rds[[1]]$drug_exptbl_record$ratio,
                                  rds[[1]]$drug_eratbl_record$ratio),
                            type = 'bar',
                            name = dbName[1], 
                            width = 1000)
    for(i in 2:length(dbName)){
        plot <- add_trace(p=plot, y=c(rds[[i]]$persontbl_record$ratio,
                                      rds[[i]]$visittbl_record$ratio,
                                      rds[[i]]$conditiontbl_record$ratio,
                                      rds[[i]]$drug_exptbl_record$ratio,
                                      rds[[i]]$drug_eratbl_record$ratio),
                          name = dbName[i])
    }
    plot <- plotly::layout(p=plot,
                           title = "Total Table Records", yaxis = list(title='Ratio(%)'), barmode='group')
    return(plot)
}
#draw_total_record(dbName, rds)

#' Draw pie plot about CDM person, record count with plotly library
#'
#' @description
#' \code(draw_meta_pie) Draw pie plot about table person or record
#'
#' @details
#' \code(draw_meta_pie) Draw pie plot about table person or record
#'
#' @param dbName CDM database name
#' @param rds loaded CDM data
#' @param tableName CDM database table name
#' @param type "person" or "record"
#'
#' @return
#' Return pie plot.
#'
#' @examples
#' \dontrun{
#' draw_meta_pie(dbName = 'MYCDM', rds = loaded_rds, tableName = 'person', type = 'person')
#' }
draw_meta_pie <- function(dbName, rds, tableName, type){
    k<-5
    len_dbName <- length(dbName)
    rows <- (len_dbName - 1) %/% k + 1
    columns <- (len_dbName - 1) %% k + 1
    
    plot <- plot_ly(width = 1000)
    switch(tolower(tableName),
           person = {
               if(tolower(type) == "person"){
                   for(i in 1:len_dbName){
                       val <- rds[[i]]$persontbl_person_ratio$ratio
                       plot <- add_pie(p=plot,
                                       values = c(val,100 -val),
                                       textposition='inside',
                                       textinfo='label+percent',
                                       opacity= 0.9,
                                       labels = c(paste0(dbName[i],"\nPerson"),"Others"),
                                       domain=list(row=floor((i-1)/5), column=floor((i-1)%%5)))
                   }
                   plot <- layout(p=plot, title = "Person Table person ratio", showlegend = T,
                                  grid=list(rows = rows, columns = columns),
                                  xaxis = list(showgrid = F, zeroline = FALSE, showticklabels = F),
                                  yaxis = list(showgrid = F, zeroline = FALSE, showticklabels = F))
               }
               else if(tolower(type) == "record"){
                   for(i in 1:len_dbName){
                       val <- rds[[i]]$persontbl_record$ratio
                       plot <- add_pie(p=plot,
                                       values = c(val, 100-val),
                                       textposition='inside',
                                       textinfo='label+percent',
                                       opacity= 0.9,
                                       labels = c(paste0(dbName[i],"\nPerson"),"Others"),
                                       domain=list(row=floor((i-1)/5), column=floor((i-1)%%5)))
                   }
                   plot <- layout(p=plot, title = "Person Table Record", showlegend = T,
                                  grid=list(rows = rows, columns = columns),
                                  xaxis = list(showgrid = F, zeroline = FALSE, showticklabels = F),
                                  yaxis = list(showgrid = F, zeroline = FALSE, showticklabels = F))
               }},
           visit = {
               if(tolower(type) == "person"){
                   for(i in 1:len_dbName){
                       val <- rds[[i]]$visittbl_person_ratio$ratio
                       plot <- add_pie(p=plot,
                                       values = c(val, 100-val),
                                       textposition='inside',
                                       textinfo='label+percent',
                                       opacity= 0.9,
                                       labels = c(paste0(dbName[i],"\nVisit Occurrence"),"Others"),
                                       domain=list(row=floor((i-1)/5), column=floor((i-1)%%5)))
                   }
                   plot <- layout(p=plot, title = "Visit Occurrence Table person ratio", showlegend = T,
                                  grid=list(rows = rows, columns = columns),
                                  xaxis = list(showgrid = F, zeroline = FALSE, showticklabels = F),
                                  yaxis = list(showgrid = F, zeroline = FALSE, showticklabels = F))
               }
               else if(tolower(type) == "record"){
                   for(i in 1:len_dbName){
                       val <- rds[[i]]$visittbl_record$ratio
                       plot <- add_pie(p=plot,
                                       values = c(val, 100-val),
                                       textposition='inside',
                                       textinfo='label+percent',
                                       opacity= 0.9,
                                       labels = c(paste0(dbName[i],"\nVisit Occurrence"),"Others"),
                                       domain=list(row=floor((i-1)/5), column=floor((i-1)%%5)))
                   }
                   plot <- layout(p=plot, title = "Visit Occurrence Table record", showlegend = T,
                                  grid=list(rows = rows, columns = columns),
                                  xaxis = list(showgrid = F, zeroline = FALSE, showticklabels = F),
                                  yaxis = list(showgrid = F, zeroline = FALSE, showticklabels = F))
               }},
           condition = {
               if(tolower(type) == "person"){
                   for(i in 1:len_dbName){
                       val <- rds[[i]]$conditiontbl_person_ratio$ratio
                       plot <- add_pie(p=plot,
                                       values = c(val, 100-val),
                                       textposition='inside',
                                       textinfo='label+percent',
                                       opacity= 0.9,
                                       labels = c(paste0(dbName[i],"\nCondition Occurrence"),"Others"),
                                       domain=list(row=floor((i-1)/5), column=floor((i-1)%%5)))
                   }
                   plot <- layout(p=plot, title = "Condition Occurrence Table person ratio", showlegend = T,
                                  grid=list(rows = rows, columns = columns),
                                  xaxis = list(showgrid = F, zeroline = FALSE, showticklabels = F),
                                  yaxis = list(showgrid = F, zeroline = FALSE, showticklabels = F))
               }
               else if(tolower(type) == "record"){
                   for(i in 1:len_dbName){
                       val <- rds[[i]]$conditiontbl_record$ratio
                       plot <- add_pie(p=plot,
                                       values = c(val, 100-val),
                                       textposition='inside',
                                       textinfo='label+percent',
                                       opacity= 0.9,
                                       labels = c(paste0(dbName[i],"\nCondition Occurrence"),"Others"),
                                       domain=list(row=floor((i-1)/5), column=floor((i-1)%%5)))
                   }
                   plot <- layout(p=plot, title = "Condition Occurrence Table record", showlegend = T,
                                  grid=list(rows = rows, columns = columns),
                                  xaxis = list(showgrid = F, zeroline = FALSE, showticklabels = F),
                                  yaxis = list(showgrid = F, zeroline = FALSE, showticklabels = F))
               }
           },
           drug_exp = {
               if(tolower(type) == "person"){
                   for(i in 1:len_dbName){
                       val <- rds[[i]]$drug_exptbl_person_ratio$ratio
                       plot <- add_pie(p=plot,
                                       values = c(val, 100-val),
                                       textposition='inside',
                                       textinfo='label+percent',
                                       opacity= 0.9,
                                       labels = c(paste0(dbName[i],"\nDrug Exposure"),"Others"),
                                       domain=list(row=floor((i-1)/5), column=floor((i-1)%%5)))
                   }
                   plot <- layout(p=plot, title = "Drug Exposure Table person ratio", showlegend = T,
                                  grid=list(rows = rows, columns = columns),
                                  xaxis = list(showgrid = F, zeroline = FALSE, showticklabels = F),
                                  yaxis = list(showgrid = F, zeroline = FALSE, showticklabels = F))
               }
               else if(tolower(type) == "record"){
                   for(i in 1:len_dbName){
                       val <- rds[[i]]$drug_exptbl_record$ratio
                       plot <- add_pie(p=plot,
                                       values = c(val, 100-val),
                                       textposition='inside',
                                       textinfo='label+percent',
                                       opacity= 0.9,
                                       labels = c(paste0(dbName[i],"\nDrug Exposure"),"Others"),
                                       domain=list(row=floor((i-1)/5), column=floor((i-1)%%5)))
                   }
                   plot <- layout(p=plot, title = "Drug Exposure Table record", showlegend = T,
                                  grid=list(rows = rows, columns = columns),
                                  xaxis = list(showgrid = F, zeroline = FALSE, showticklabels = F),
                                  yaxis = list(showgrid = F, zeroline = FALSE, showticklabels = F))
               }
           },
           drug_era = {
               if(tolower(type) == "person"){
                   for(i in 1:len_dbName){
                       val <- rds[[i]]$drug_eratbl_person_ratio$ratio
                       plot <- add_pie(p=plot,
                                       values = c(val, 100-val),
                                       textposition='inside',
                                       textinfo='label+percent',
                                       opacity= 0.9,
                                       labels = c(paste0(dbName[i],"\nDrug Era"),"Others"),
                                       domain=list(row=floor((i-1)/5), column=floor((i-1)%%5)))
                   }
                   plot <- layout(p=plot, title = "Drug Era Table person ratio", showlegend = T,
                                  grid=list(rows = rows, columns = columns),
                                  xaxis = list(showgrid = F, zeroline = FALSE, showticklabels = F),
                                  yaxis = list(showgrid = F, zeroline = FALSE, showticklabels = F))
               }
               else if(tolower(type) == "record"){
                   for(i in 1:len_dbName){
                       val <- rds[[i]]$drug_eratbl_record$ratio
                       plot <- add_pie(p=plot,
                                       values = c(val, 100-val),
                                       textposition='inside',
                                       textinfo='label+percent',
                                       opacity= 0.9,
                                       labels = c(paste0(dbName[i],"\nDrug Era"),"Others"),
                                       domain=list(row=floor((i-1)/5), column=floor((i-1)%%5)))
                   }
                   plot <- layout(p=plot, title = "Drug Era Table record", showlegend = T,
                                  grid=list(rows = rows, columns = columns),
                                  xaxis = list(showgrid = F, zeroline = FALSE, showticklabels = F),
                                  yaxis = list(showgrid = F, zeroline = FALSE, showticklabels = F))
               }
           }
    )
    return(plot)
}
#draw_meta_pie(dbName , rds, tableName = "drug_exp", type = "person")
#draw_meta_pie(dbName , rds, tableName = "drug_exp", type = "record")

#' Draw bar plot about CDM person gender with plotly
#'
#' @description
#' \code(draw_gender_bar) Draw bar plot about gender
#'
#' @details
#' \code(draw_gender_bar) Draw bar plot about gender
#'
#' @param dbName CDM database name
#' @param rds loaded CDM data
#'
#' @return
#' Return bar plot.
#'
#' @examples
#' \dontrun{
#' draw_gender_bar(dbName = 'MYCDM', rds = loaded_rds)
#' }
draw_gender_bar <- function(dbName, rds){
    domain <- c(rds[[1]]$persontbl_gender$attributeName[1],
                rds[[1]]$persontbl_gender$attributeName[2])
    val <- c(rds[[1]]$persontbl_gender$ratio[1],
             rds[[1]]$persontbl_gender$ratio[2])
    plot <- plot_ly(rds[[1]]$persontbl_gender,
                    x = domain, y = val, text = val, textposition = 'auto',type = 'bar', name = dbName[1], 
                    width = 1000)
    for(i in 2:length(dbName)){
        val <- c(rds[[i]]$persontbl_gender$ratio[1],
                 rds[[i]]$persontbl_gender$ratio[2])
        plot <- add_trace(p=plot, y = val, name = dbName[i], text = val, textposition = 'auto')
    }
    plot <- layout(p=plot, yaxis = list(title='Count'), barmode = 'group')
    return(plot)
}
#draw_gender_bar(dbName, rds)

#' Draw bar plot about CDM person gender with plotly
#'
#' @description
#' \code(draw_visit_freq_graph) Draw bar plot about gender
#'
#' @details
#' \code(draw_visit_freq_graph) Draw bar plot about gender
#'
#' @param dbName CDM database name
#' @param rds loaded CDM data
#' @param type 'min' or 'max'
#'
#' @return
#' Return bar plot.
#'
#' @examples
#' \dontrun{
#' draw_visit_freq_graph(dbName = 'MYCDM', rds = loaded_rds, type = 'min')
#' }

draw_visit_freq_graph <- function(dbName, rds, type){
    standard <- which(sapply(rds, function(x){length(x$persontbl_min_age$ageRange)}) ==
                          max(sapply(rds, function(x){length(x$persontbl_min_age$ageRange)})))[1]
    xax <- rds[[standard]]$persontbl_min_age[rds[[standard]]$persontbl_min_age$genderConceptId == '8507',]$ageRange
    # sapply(rds, function(x){
    #     person_minage <-x[[i]]$persontbl_min_age
    #     uniq_person_age <- unique(person_minage$ageRange)
    #     if(length(unique(uniq_person_age != xax))==2){
    #         diffValue <- setdiff(xax, uniq_person_age)
    #         addDataframe <- data.frame('ageRange' = diffValue, 'genderConceptId' = rep(8532,length(diffValue)) , 'ratio' = rep(0,length(diffValue)))
    #         addDataframe2 <- data.frame('ageRange' = diffValue, 'genderConceptId' = rep(8507,length(diffValue)) , 'ratio' = rep(0,length(diffValue)) )
    #         tempDataframe <- rbind(addDataframe2,addDataframe)
    #         x[[i]]$persontbl_min_age <- rbind(person_minage, tempDataframe)
    #     }
    # })
    # 
    
    rdsList <- list(rds[[1]]$persontbl_min_age,rds[[2]]$persontbl_min_age)
    
    rdsList <- list()
    for(i in 1:length(rds)){
        rdsList[[i]] <- rds[[i]]$persontbl_min_age
    }
    
    
    # names(rdsList) <- rep('persontbl_min_age',2)
    tempList <- lapply(rdsList, function(x){
        person_minage <- x
        uniq_person_age <- unique(person_minage$ageRange)
        if(length(unique(uniq_person_age != xax))==2){
            diffValue <- setdiff(xax, uniq_person_age)
            addDataframe <- data.frame('ageRange' = diffValue, 'genderConceptId' = rep(8532,length(diffValue)) , 'ratio' = rep(0,length(diffValue)))
            addDataframe2 <- data.frame('ageRange' = diffValue, 'genderConceptId' = rep(8507,length(diffValue)) , 'ratio' = rep(0,length(diffValue)) )
            tempDataframe <- rbind(addDataframe2,addDataframe)
            x <- rbind(person_minage, tempDataframe)
        }
        else{
            x <- x
        }
    })
    
    for(i in 1:length(rds)){
        rds[[i]]$persontbl_min_age <- tempList[[i]]
    }

    rds[[2]]$persontbl_min_age
    
    x_len <- length(xax)
    y_male <- rds[[1]]$persontbl_min_age[rds[[1]]$persontbl_min_age$genderConceptId == '8507',]$ratio
    y_female <- rds[[1]]$persontbl_min_age[rds[[1]]$persontbl_min_age$genderConceptId == '8532',]$ratio
    xform <- list(categoryorder = "array",
                  categoryarray = xax)
    
    plot1 <- plot_ly(data = data.frame(xax, y_male,y_female),
                     x=xax,
                     y=y_male,
                     type = 'bar',
                     name = paste(dbName[1], "Male"), 
                     width = 1000)
    plot2 <- plot_ly(data = data.frame(xax, y_male,y_female),
                     x=xax,
                     y=y_female,
                     type = 'bar',
                     name = paste(dbName[1], "Female"), 
                     width = 1000)
    for(i in 2:length(dbName)){
        y_male <- rds[[i]]$persontbl_min_age[rds[[i]]$persontbl_min_age$genderConceptId == '8507',]$ratio
        y_male <- append(y_male,vector(length = x_len-length(y_male)))
        y_female <- rds[[i]]$persontbl_min_age[rds[[i]]$persontbl_min_age$genderConceptId == '8532',]$ratio
        y_female <- append(y_female,vector(length = x_len-length(y_female)))
        plot1 <- add_trace(p=plot1, y=y_male, name = paste(dbName[i], "Male"))
        plot2 <- add_trace(p=plot2, y=y_female, name = paste(dbName[i], "Female"))
    }
    plot <- subplot(plot1,plot2,nrows = 2, shareX = T, shareY = F, titleX = T, titleY = T)
    plot <- layout(p=plot,
                   title = 'Min Graph',
                   xaxis = xform,
                   yaxis = list("Ratio(%)"))
    return(plot)
}
draw_visit_freq_graph_div_male <- function(dbName, rds, type){
    standard <- which(sapply(rds, function(x){length(x$persontbl_min_age$ageRange)}) ==
                          max(sapply(rds, function(x){length(x$persontbl_min_age$ageRange)})))[1]
    xax <- rds[[standard]]$persontbl_min_age[rds[[standard]]$persontbl_min_age$genderConceptId == '8507',]$ageRange
    x_len <- length(xax)
    y_male <- rds[[1]]$persontbl_min_age[rds[[1]]$persontbl_min_age$genderConceptId == '8507',]$ratio
    xform <- list(categoryorder = "array",
                  categoryarray = xax)
    plot <- plot_ly(data = data.frame(xax, y_male),
                    x=xax,
                    y=y_male,
                    type = 'bar',
                    name = paste(dbName[1], "Male"), 
                    width = 1000)
    plot <- layout(p=plot,
                   title = 'Male Min Graph',
                   xaxis = xform,
                   yaxis = list("Ratio(%)"))
    for(i in 2:length(dbName)){
        y_male <- rds[[i]]$persontbl_min_age[rds[[i]]$persontbl_min_age$genderConceptId == '8507',]$ratio
        y_male <- append(y_male,vector(length = x_len-length(y_male)))
        plot <- add_trace(p=plot, y=y_male, name = paste(dbName[i], "Male"))
    }
    return(plot)
}
draw_visit_freq_graph_div_female <- function(dbName, rds, type){
    standard <- which(sapply(rds, function(x){length(x$persontbl_min_age$ageRange)}) ==
                          max(sapply(rds, function(x){length(x$persontbl_min_age$ageRange)})))[1]
    xax <- rds[[standard]]$persontbl_min_age[rds[[standard]]$persontbl_min_age$genderConceptId == '8532',]$ageRange
    x_len <- length(xax)
    y_female <- rds[[1]]$persontbl_min_age[rds[[1]]$persontbl_min_age$genderConceptId == '8532',]$ratio
    xform <- list(categoryorder = "array",
                  categoryarray = xax)
    plot <- plot_ly(data = data.frame(xax,y_female),
                    x=xax,
                    y=y_female,
                    type = 'bar',
                    name = paste(dbName[1], "Female"), 
                    width = 1000)
    plot <- layout(p=plot,
                   title = 'Female Min Graph',
                   xaxis = xform,
                   yaxis = list("Ratio(%)"))
    for(i in 2:length(dbName)){
        y_female <- rds[[i]]$persontbl_min_age[rds[[i]]$persontbl_min_age$genderConceptId == '8532',]$ratio
        y_female <- append(y_female,vector(length = x_len-length(y_female)))
        plot <- add_trace(p=plot, y=y_female, name = paste(dbName[i], "Female"))
    }
    return(plot)
}
#draw_visit_freq_graph_div_male(dbName,rds,'min')
#draw_visit_freq_graph(dbName, rds, 'min')
#draw_visit_freq_graph_div_female(dbName, rds, 'min')


#' Draw pie plot about CDM data with plotly
#'
#' @description
#' \code(draw_pie) Draw bar plot about gender
#'
#' @details
#' \code(draw_pie) Draw bar plot about gender
#'
#' @param dbName CDM database name
#' @param rds loaded CDM data
#' @param rdsConcept rds
#'
#' @return
#' Return pie plot.
#'
#' @examples
#' \dontrun{
#' draw_pie(dbName = 'MYCDM', rds = loaded_rds, rdsConcept = 'visittbl_visit_concept')
#' }
draw_pie <- function(dbName, rds, rdsConcept, title){
    len_dbName <- length(dbName)
    k <-5
    plot <- plot_ly(width = 1000)
    temp_ratio <- vector(length=length(rds))
    temp_name <- vector(length=len_dbName)
    for(i in 1:len_dbName){
        if(is.null(rds[[i]][names(rds[[i]])==rdsConcept][[1]]$ratio)){
            temp_ratio[i] <- 100
            temp_name[i] <- 'No Data'
        }else if(is.na(rds[[i]][names(rds[[i]])==rdsConcept][[1]]$attributeName)){
            temp_ratio[i] <- 100
            temp_name[i] <- 'NULL'
        }else{
            temp_ratio[i] <- rds[[i]][names(rds[[i]])==rdsConcept][[1]]$ratio
            temp_name[i] <- rds[[i]][names(rds[[i]])==rdsConcept][[1]]$attributeName
        }
        plot <- add_pie(p = plot,
                        values = temp_ratio[i],
                        labels = dbName[i],
                        text = temp_name[i],
                        textposition='inside',
                        textinfo='text+percent',
                        hoverinfo = "label+text+percent",
                        opacity= 0.9,
                        domain=list(row=floor((i-1)/5), column=floor((i-1)%%5)))
    }
    rows <- (len_dbName - 1) %/% k + 1
    columns <- (len_dbName - 1) %% k + 1
    plot <- layout(p=plot, title = title, showlegend = T,
                   grid=list(rows = rows, columns = columns),
                   xaxis = list(showgrid = F, zeroline = FALSE, showticklabels = F),
                   yaxis = list(showgrid = F, zeroline = FALSE, showticklabels = F))
    
    return(plot)
}
#draw_pie(dbName, rds, "persontbl_race")
#draw_pie(dbName, rds, "persontbl_ethnicity")
#draw_pie(dbName, rds, "visittbl_visit_concept")


#' Draw bar plot with null ratio about CDM data
#'
#' @description
#' \code(draw_null_bar) Draw bar plot with null ratio
#'
#' @details
#' \code(draw_null_bar) Draw bar plot with null ratio
#'
#' @param dbName CDM database name
#' @param rds loaded CDM data
#' @param rdsConcept rds
#'
#' @return
#' Return two bar plot.
#'
#' @examples
#' \dontrun{
#' draw_null_bar(dbName = 'MYCDM', rds = loaded_rds, rdsConcept = 'persontbl_location')
#' }
draw_null_bar<- function(dbName, rds, rdsConcept){
    x <- dbName[1]
    y1 <- rds[[1]][names(rds[[1]])==rdsConcept][[1]]$attributeCount
    y2 <- rds[[1]][names(rds[[1]])==rdsConcept][[1]]$nullRatio
    plot1 <- plot_ly(data = data.frame(x, y1),
                     x=x,
                     y=y1,
                     type = 'bar',
                     text = y1,
                     textposition = 'auto',
                     name = x, 
                     width = 1000)
    plot2 <- plot_ly(data = data.frame(x, y2),
                     x=x,
                     y=y2,
                     type = 'bar',
                     text = y2,
                     textposition = 'auto',
                     name = x, 
                     width = 1000)
    for(i in 2:length(dbName)){
        x <- dbName[i]
        y1 <- rds[[i]][names(rds[[i]])==rdsConcept][[1]]$attributeCount
        y2 <- rds[[i]][names(rds[[i]])==rdsConcept][[1]]$nullRatio
        plot1 <- add_trace(p = plot1, x= x, y = y1, name = x, text = y1, textposition = 'auto')
        plot2 <- add_trace(p = plot2, x= x, y = y2, name = x, text = y2, textposition = 'auto')
    }
    plot1 <- layout(p = plot1,  yaxis = list(title = "Count"))
    plot2 <- layout(p = plot2,title = "Null and count",
                    yaxis = list(title = "Null Ratio"))
    plot <- subplot(plot1,plot2,nrows = 2, shareX = T, shareY = F, titleX = T, titleY = T)
    return(plot)
}
#draw_null_bar(dbName,rds, rdsConcept = 'persontbl_location')

draw_stop_bar <- function(dbName, rds, rdsConcept){
    x <- dbName[1]
    y <- rds[[1]][names(rds[[1]])==rdsConcept][[1]]$attributeCount
    plot <- plot_ly(data = data.frame(x, y),
                    x=x,
                    y=y,
                    type = 'bar',
                    text = y,
                    textposition = 'auto',
                    name = x, 
                    width = 1000)
    for(i in 2:length(dbName)){
        x <- dbName[i]
        y <- rds[[i]][names(rds[[i]])==rdsConcept][[1]]$attributeCount
        plot <- add_trace(p = plot, x= x, y = y, name = x, text = y, textposition = 'auto')
    }
    plot <- layout(p = plot,  yaxis = list(title = "Count"))
    return(plot)
}


#' Draw bar plot about CDM data
#'
#' @description
#' \code(draw_histogram) Draw bar plot
#'
#' @details
#' \code(draw_histogram) Draw bar plot
#'
#' @param dbName CDM database name
#' @param rds loaded CDM data
#' @param rdsConcept rds
#'
#' @return
#' Return bar plot.
#'
#' @examples
#' \dontrun{
#' draw_histogram(dbName = 'MYCDM', rds = loaded_rds, rdsConcept = 'persontbl_location')
#' }
draw_histogram <- function (dbName, rds, rdsConcept){
    plot <- plot_ly(alpha = 0.6, type='histogram',width = 1000)
    for(i in 1:length(dbName)){
        plot <- add_histogram(p = plot, x <- rds[[i]][names(rds[[i]])==rdsConcept][[1]]$dayDiff, name = dbName[i])
    }
    plot <- layout(p = plot, barmode = 'overlay')
    return(plot)
}
# draw_histogram(dbName, rds, rdsConcept = "visittbl_diff_date")
draw_gap_histogram <- function (dbName, rds, rdsConcept){
    standard <- which(sapply(rds, function(x){length(x[names(x)==rdsConcept][[1]]$gapDayRange)}) ==
                          max(sapply(rds, function(x){length(x[names(x)==rdsConcept][[1]]$gapDayRange)})))[1]
    xax <- rds[[standard]][names(rds[[standard]])==rdsConcept][[1]]$gapDayRange[order(as.numeric(gsub(x = rds[[standard]][names(rds[[standard]])==rdsConcept][[1]]$gapDayRange, pattern = '~\\w*', replacement = "")))]
    xform <- list(categoryorder = "array",
                  categoryarray = xax)
    # axe <- list(autotick = F,
    #             ticks = 'outside',
    #             tick0 = 0,
    #             dtick = 50,
    #             ticklen = 5,
    #             tickwidth = 1)
    y_val <- rds[[1]][names(rds[[1]])==rdsConcept][[1]]$personRatio
    temp <- seq(from = 0, to = 0, length = length(xax)-length(y_val))
    y_val <- append(y_val, temp, after = length(y_val))
    plot <- plot_ly(data = data.frame(xax, y_val),
                    x=xax,
                    y=y_val,
                    type = 'bar',
                    name = paste(dbName[1]), 
                    width = 1000)
    for(i in 2:length(dbName)){
        y_val <- rds[[i]][names(rds[[i]])==rdsConcept][[1]]$personRatio
        temp <- seq(from = 0, to = 0, length = length(xax)-length(y_val))
        y_val <- append(y_val, temp, after = length(y_val))
        plot <- add_trace(p = plot, x= xax, y = y_val,
                          name = paste(dbName[i]))
    }
    plot <- layout(p = plot, xaxis = xform)
    # plot <- layout(p = plot, xaxis = axe)
    return(plot)#그대로 사용하기 어려움.
}
# draw_gap_histogram(dbName, rds, rdsConcept = "drug_eratbl_gap_days")

#' Draw line graph about CDM data
#'
#' @description
#' \code(draw_line_graph) Draw line graph
#'
#' @details
#' \code(draw_line_graph) Draw line graph
#'
#' @param dbName CDM database name
#' @param rds loaded CDM data
#' @param rdsConcept rds
#'
#' @return
#' Return line graph
#'
#' @examples
#' \dontrun{
#' draw_line_graph(dbName = 'MYCDM', rds = loaded_rds, rdsConcept = 'conditiontbl_start')
#' }

draw_line_graph <- function(dbName,rds,rdsConcept){
    val_NA <- c()
    val_2999 <- c()
    temp <- rds[[1]][names(rds[[1]])==rdsConcept][[1]]
    if(nrow(subset(temp,is.na(visitYear)))>0){
        val_NA <- subset(temp,is.na(visitYear))$personRatio
        temp <- temp[-c(which(is.na(temp))),]
    }else{
        val_NA <- 0
    }
    if(nrow(subset(temp,visitYear =='2999'))>0){
        val_2999 <- subset(temp,visitYear =='2999')$personRatio
        temp <- temp[-c(which(temp$visitYear =='2999')),]
    }else{
        val_2999 <- 0
    }
    x <- temp$visitYear
    y <- temp$personRatio
    val_temp <- data.frame(c('NA','2999'),c(val_NA,val_2999))
    plot <- plot_ly(x= x, y=y, type = 'scatter', mode = 'lines', name = dbName[1], width = 1000)
    for(i in 2:length(dbName)){
        temp <- rds[[i]][names(rds[[i]])==rdsConcept][[1]]
        if(nrow(subset(temp,is.na(visitYear)))>0){
            val_NA <- subset(temp,is.na(visitYear))
            temp <- temp[-c(which(is.na(temp))),]
        }else{
            val_NA <- 0
        }
        if(nrow(subset(temp,visitYear =='2999'))>0){
            val_2999 <- subset(temp,visitYear =='2999')
            temp <- temp[-c(which(temp$visitYear =='2999')),]
        }else{
            val_2999 <- 0
        }
        x <- temp$visitYear
        y <- temp$personRatio
        val_temp <- cbind(val_temp,data.frame(c(val_NA,val_2999)))
        plot <- add_trace(p = plot, x= x, y=y, name = dbName[i])
    }
    names(val_temp) <- c('Error_time',dbName)
    plot_bar <- plot_ly(data = val_temp,
                        x = val_temp$Error_time,
                        y = unlist(as.list(val_temp[2])),
                        text = unlist(as.list(val_temp[2])),
                        textposition = 'auto',
                        name = names(val_temp[2]),
                        type = 'bar', 
                        width = 1000)
    for(i in 3:length(val_temp))
        plot_bar <- add_trace(p = plot_bar,
                              y = unlist(as.list(val_temp[i])),
                              text = unlist(as.list(val_temp[i])),
                              textposition = 'auto',
                              name = names(val_temp[i]))
    plot_bar <- layout(p = plot_bar, yaxis = list(title = 'Ratio'), barmode = 'group')
    plot <- subplot(plot,plot_bar, nrows=2,shareX = F, shareY = F)
    return(plot)
}
#draw_line_graph(dbName, rds, rdsConcept = 'conditiontbl_start')

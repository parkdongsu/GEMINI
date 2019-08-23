##All table people count
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
                         name = dbName[1])
    for(i in 2:length(dbName)){
        plot <- add_trace(p=plot, y=c(rds[[i]]$persontbl_person_ratio$ratio,
                                rds[[i]]$visittbl_person_ratio$ratio,
                                rds[[i]]$conditiontbl_person_ratio$ratio,
                                rds[[i]]$drug_exptbl_person_ratio$ratio,
                                rds[[i]]$drug_eratbl_person_ratio$ratio),
                          hoverinfo = "y",
                          name = dbName[i])
    }
    plot <- plotly::layout(p=plot,title = "Total table Person", yaxis = list(title='Ratio(%)'), barmode='group')
    return(plot)
}
draw_total_person(dbName, rds)

##All table records count
draw_total_record <- function(dbName, rds){
    Total <- c("Person","Visit Occurrence", "Condition Occurrence", "Drug Exposure", "Drug Era")
    plot <- plotly::plot_ly(x = ~Total,
                         y = c(rds[[1]]$persontbl_record$ratio,
                               rds[[1]]$visittbl_record$ratio,
                               rds[[1]]$conditiontbl_record$ratio,
                               rds[[1]]$drug_exptbl_record$ratio,
                               rds[[1]]$drug_eratbl_record$ratio),
                         type = 'bar',
                         name = dbName[1])
    for(i in 2:length(dbName)){
        plot <- add_trace(p=plot, y=c(rds[[i]]$persontbl_record$ratio,
                                rds[[i]]$visittbl_record$ratio,
                                rds[[i]]$conditiontbl_record$ratio,
                                rds[[i]]$drug_exptbl_record$ratio,
                                rds[[i]]$drug_eratbl_record$ratio),
                       name = dbName[i])
    }
    plot <- plotly::layout(p=plot,title = "Total Table Records", yaxis = list(title='Ratio(%)'), barmode='group')
    return(plot)
}
draw_total_record(dbName, rds)

##Each table people, records count by tableName and type
draw_meta_pie <- function(dbName, rds, tableName, type){
    plot <- plot_ly()
    switch(tolower(tableName),
           person = {
               if(tolower(type) == "person"){
               for(i in 1:length(dbName)){
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
                              grid=list(rows=floor(length(dbName)/6)+1,
                                        columns =floor(length(dbName)/5)*5
                                        -(floor(length(dbName)/5)*(length(dbName)%%5))
                                        +length(dbName)%%5),
                              xaxis = list(showgrid = F, zeroline = FALSE, showticklabels = F),
                              yaxis = list(showgrid = F, zeroline = FALSE, showticklabels = F))
               }
           else if(tolower(type) == "record"){
           for(i in 1:length(dbName)){
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
                          grid=list(rows=floor(length(dbName)/6)+1,
                                    columns =floor(length(dbName)/5)*5
                                    -(floor(length(dbName)/5)*(length(dbName)%%5))
                                    +length(dbName)%%5),
                          xaxis = list(showgrid = F, zeroline = FALSE, showticklabels = F),
                          yaxis = list(showgrid = F, zeroline = FALSE, showticklabels = F))
       }},
       visit = {
           if(tolower(type) == "person"){
               for(i in 1:length(dbName)){
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
                              grid=list(rows=floor(length(dbName)/6)+1,
                                        columns =floor(length(dbName)/5)*5
                                        -(floor(length(dbName)/5)*(length(dbName)%%5))
                                        +length(dbName)%%5),
                              xaxis = list(showgrid = F, zeroline = FALSE, showticklabels = F),
                              yaxis = list(showgrid = F, zeroline = FALSE, showticklabels = F))
           }
       else if(tolower(type) == "record"){
           for(i in 1:length(dbName)){
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
                          grid=list(rows=floor(length(dbName)/6)+1,
                                    columns =floor(length(dbName)/5)*5
                                    -(floor(length(dbName)/5)*(length(dbName)%%5))
                                    +length(dbName)%%5),
                          xaxis = list(showgrid = F, zeroline = FALSE, showticklabels = F),
                          yaxis = list(showgrid = F, zeroline = FALSE, showticklabels = F))
       }},
       condition = {
           if(tolower(type) == "person"){
               val <- rds[[i]]$conditiontbl_person_ratio$ratio
               for(i in 1:length(dbName)){
                   plot <- add_pie(p=plot,
                                   values = c(val, 100-val),
                                   textposition='inside',
                                   textinfo='label+percent',
                                   opacity= 0.9,
                                   labels = c(paste0(dbName[i],"\nCondition Occurrence"),"Others"),
                                   domain=list(row=floor((i-1)/5), column=floor((i-1)%%5)))
               }
               plot <- layout(p=plot, title = "Condition Occurrence Table person ratio", showlegend = T,
                              grid=list(rows=floor(length(dbName)/6)+1,
                                        columns =floor(length(dbName)/5)*5
                                        -(floor(length(dbName)/5)*(length(dbName)%%5))
                                        +length(dbName)%%5),
                              xaxis = list(showgrid = F, zeroline = FALSE, showticklabels = F),
                              yaxis = list(showgrid = F, zeroline = FALSE, showticklabels = F))
           }
           else if(tolower(type) == "record"){
               for(i in 1:length(dbName)){
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
                              grid=list(rows=floor(length(dbName)/6)+1,
                                        columns =floor(length(dbName)/5)*5
                                        -(floor(length(dbName)/5)*(length(dbName)%%5))
                                        +length(dbName)%%5),
                              xaxis = list(showgrid = F, zeroline = FALSE, showticklabels = F),
                              yaxis = list(showgrid = F, zeroline = FALSE, showticklabels = F))
           }
       },
       drug_exp = {
           if(tolower(type) == "person"){
               for(i in 1:length(dbName)){
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
                              grid=list(rows=floor(length(dbName)/6)+1,
                                        columns =floor(length(dbName)/5)*5
                                        -(floor(length(dbName)/5)*(length(dbName)%%5))
                                        +length(dbName)%%5),
                              xaxis = list(showgrid = F, zeroline = FALSE, showticklabels = F),
                              yaxis = list(showgrid = F, zeroline = FALSE, showticklabels = F))
           }
           else if(tolower(type) == "record"){
               for(i in 1:length(dbName)){
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
                              grid=list(rows=floor(length(dbName)/6)+1,
                                        columns =floor(length(dbName)/5)*5
                                        -(floor(length(dbName)/5)*(length(dbName)%%5))
                                        +length(dbName)%%5),
                              xaxis = list(showgrid = F, zeroline = FALSE, showticklabels = F),
                              yaxis = list(showgrid = F, zeroline = FALSE, showticklabels = F))
           }
       },
       drug_era = {
           if(tolower(type) == "person"){
               for(i in 1:length(dbName)){
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
                              grid=list(rows=floor(length(dbName)/6)+1,
                                        columns =floor(length(dbName)/5)*5
                                        -(floor(length(dbName)/5)*(length(dbName)%%5))
                                        +length(dbName)%%5),
                              xaxis = list(showgrid = F, zeroline = FALSE, showticklabels = F),
                              yaxis = list(showgrid = F, zeroline = FALSE, showticklabels = F))
           }
           else if(tolower(type) == "record"){
               for(i in 1:length(dbName)){
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
                              grid=list(rows=floor(length(dbName)/6)+1,
                                        columns =floor(length(dbName)/5)*5
                                        -(floor(length(dbName)/5)*(length(dbName)%%5))
                                        +length(dbName)%%5),
                              xaxis = list(showgrid = F, zeroline = FALSE, showticklabels = F),
                              yaxis = list(showgrid = F, zeroline = FALSE, showticklabels = F))
           }
       }
       )
    return(plot)
}
#draw_meta_pie(dbName , rds, tableName = "drug_exp", type = "person")
#draw_meta_pie(dbName , rds, tableName = "drug_exp", type = "record")

##Gender bar
draw_gender_bar <- function(dbName, rds){
    domain <- c(rds[[1]]$persontbl_gender$attributeName[1],
                rds[[1]]$persontbl_gender$attributeName[2])
    val <- c(rds[[1]]$persontbl_gender$ratio[1],
             rds[[1]]$persontbl_gender$ratio[2])
    plot <- plot_ly(rds[[1]]$persontbl_gender,
                    x = domain, y = val, text = val, textposition = 'auto',type = 'bar', name = dbName[1])
    for(i in 2:length(dbName)){
        val <- c(rds[[i]]$persontbl_gender$ratio[1],
                 rds[[i]]$persontbl_gender$ratio[2])
        plot <- add_trace(p=plot, y = val, name = dbName[i], text = val, textposition = 'auto')
    }
    plot <- layout(p=plot, yaxis = list(title='Count'), barmode = 'group')
    return(plot)
}
draw_gender_bar(dbName, rds)
#Person visit graph

##People visit
##Ask opinion
draw_visit_freq_graph <- function(dbName, rds, type){
    standard <- which(sapply(rds, function(x){length(x$persontbl_min_age$ageRange)}) ==
                          max(sapply(rds, function(x){length(x$persontbl_min_age$ageRange)})))[1]
    xax <- rds[[standard]]$persontbl_min_age[rds[[standard]]$persontbl_min_age$genderConceptId == '8507',]$ageRange
    x_len <- length(xax)
    y_male <- rds[[1]]$persontbl_min_age[rds[[1]]$persontbl_min_age$genderConceptId == '8507',]$ratio
    y_female <- rds[[1]]$persontbl_min_age[rds[[1]]$persontbl_min_age$genderConceptId == '8532',]$ratio
    xform <- list(categoryorder = "array",
                  categoryarray = xax)
    plot <- plot_ly(data = data.frame(xax, y_male,y_female),
                    x=xax,
                    y=y_male,
                    type = 'bar',
                    name = paste(dbName[1], "Male"))
    plot <- add_trace(p=plot, y=y_female, name = paste(dbName[1], "Female"))
    plot <- layout(p=plot,
                   title = 'Min Graph',
                   xaxis = xform,
                   yaxis = list("Ratio(%)"))
    for(i in 2:length(dbName)){
        y_male <- rds[[i]]$persontbl_min_age[rds[[i]]$persontbl_min_age$genderConceptId == '8507',]$ratio
        y_male <- append(y_male,vector(length = x_len-length(y_male)))
        y_female <- rds[[i]]$persontbl_min_age[rds[[i]]$persontbl_min_age$genderConceptId == '8532',]$ratio
        y_female <- append(y_female,vector(length = x_len-length(y_female)))
        plot <- add_trace(p=plot, y=y_male, name = paste(dbName[i], "Male"))
        plot <- add_trace(p=plot, y=y_female, name = paste(dbName[i], "Female"))
    }
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
                    name = paste(dbName[1], "Male"))
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
                    name = paste(dbName[1], "Female"))
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

##Draw pie general
draw_pie <- function(dbName, rds, rdsConcept){
    plot <- plot_ly()
    # if(length(dbName)<6){
    #     for(i in 1:length(dbName)){
    #         txt_w[i] = i/(length(dbName)+1)
    #         txt_h[i] = 0
    #     }
    # }else{
    #     for(i in 1:length(dbName)){
    #         txt_w[i] = (i-1)%%5 * 0.2 + 0.1
    #         txt_h[i] = (1-floor(i/6))*0.5
    #     }
    # }

    for(i in 1:length(dbName)){
        plot <- add_pie(p = plot,
                        values = rds[[i]][names(rds[[i]])==rdsConcept][[1]]$ratio,
                        labels = rds[[i]][names(rds[[i]])==rdsConcept][[1]]$attributeName,
                        text = dbName[i],
                        textposition='inside',
                        textinfo='text+percent',
                        opacity= 0.9,
                        domain=list(row=floor((i-1)/5), column=floor((i-1)%%5)))
        # plot <- add_annotations(p = plot, x=txt_w[i], y=txt_h[i], text = dbName[i], showarrow =F,inherit = F)
    }

    plot <- layout(p=plot, title = "Pie Plot", showlegend = T,
                   grid=list(rows=floor(length(dbName)/6)+1,
                             columns =floor(length(dbName)/5)*5
                             -(floor(length(dbName)/5)*(length(dbName)%%5))
                             +length(dbName)%%5),
                   xaxis = list(showgrid = F, zeroline = FALSE, showticklabels = F),
                   yaxis = list(showgrid = F, zeroline = FALSE, showticklabels = F))

    return(plot)
}
#draw_pie(dbName, rds, "persontbl_race")
#draw_pie(dbName, rds, "persontbl_ethnicity")
draw_pie(dbName, rds, "visittbl_visit_concept")


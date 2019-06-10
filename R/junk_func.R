conceptPieColor <- function(dbList, findValue, count){
    if()

    temp_cpt <- as.character(sort(union(concept1$attributeName,concept2$attributeName)))
    temp_col <- rainbow(length(temp_cpt), s=0.7)
    if(sum(is.na(concept1))>0||sum(is.na(concept2))>0){
        temp_cpt <- append(temp_cpt,"NA")
        temp_col <- append(temp_col,"#A0A0A0FF")
    }
    temp <- data.frame(temp_cpt,temp_col,stringsAsFactors = F)
    return(temp)
}

# Create Pie chart which use attribute_name
drawRatioPie <- function(dbList, findValue) {
    count <- length(dbList)
    jpeg(filename = paste0("images/", gsub(x=findValue,pattern = "tbl_.*$", replacement = "")),
         width = 1024, height = 1024, quality = 75, bg = "white")
    par(mfrow = c(1, count), xpd = T)
    conceptPieColor(dbList, findValue, count)
    i<- 1
    while(i-1 < count){
        tryCatch({
            if(!is.null(value$conceptId)){
                value <- value[order(value$conceptId),]
            }
            # If NA, value must get 0. So this NA value get error range 0.1
            cpt_col <- concept_piecolor(value,tar_value)
            value <- naTostring(value)
            std_cols <- unlist(sapply(value$attributeName, FUN=function(x) cpt_col[cpt_col$temp_cpt==x,])[2,],use.names = F)
            # Label Setting
            # Label name set , append ratio num, percentage mark
            std_lbl <- labeling(value)
            std_slices <- as.numeric(value$ratio)
            # pie3d doesn't work that some value is 0.0
            std_slices <- sapply(std_slices, function(x) if (x == 0.0) {
                x <- 0.01
            } else {
                (x)
            })
            if(is.null(value$conceptId)){
                std_legend <- value[order(value$attributeName),]$attributeName
            }
            else{
                std_legend <- paste0(value$conceptId,',',value[order(value$conceptId),]$attributeName)
            }

            # Draw Pie
            pie3D(std_slices,
                  labels = paste0(value$ratio, "%"), explode = 0.1, main = std_schema_name,
                  radius = 1.0, labelcex = 1.5, theta = 0.8, start = pi / 2, cex.main = 2.0, col = std_cols)
            legend(-1.5, -1.5,std_legend, cex = 1.3, fill = std_cols, xpd = T)
        },error = function(error_message) {
            print(error_message)
            afterError()
        })
        i <- i+1
    }
}

draw_compare_pie <<- function(std_value, tar_value, path) {
    jpeg(filename = paste0("images/", path), width = 720, height = 720, quality = 75, bg = "white")
    par(mfrow = c(1, 2))
    # standard CDM
    tryCatch({
        # If NA, value must get 0. And this NA value get error range 0.1
        std_value <- naTostring(std_value)
        # Label Setting
        # Label name set , append ratio num, percentage mark
        std_lbl <- labeling(std_value)
        std_slices <- c(as.numeric(std_value$ratio), as.numeric(100 - std_value$ratio))
        # pie3d doesn't work that some value is 0.0
        std_slices <- sapply(std_slices, function(x) if (x == 0.0 || x == 0) {
            x <- 0.01
        } else {
            (x)
        })
        # Draw pie
        pie3D(std_slices,
              labels = c("", std_lbl), explode = 0.03, main = std_schema_name,
              radius = 1.0, labelcex = 1.5, theta = 0.8, start = pi / 2, cex.main = 2.0, col = rainbow(nrow(std_value) + 1, s = 0.7)
        )
    }, # If data isn't exist...
    error = function(error_message) {
        print(error_message)
        afterError()
    }
    )
    # target CDM

    tryCatch({
        # If NA, value must get 0. And this NA value get error range 0.1
        tar_value <- naTostring(tar_value)
        # Label Setting
        tar_lbl <- labeling(tar_value)
        tar_slices <- c(as.numeric(tar_value$ratio), as.numeric(100 - tar_value$ratio))
        # pie3d doesn't work that some value is 0.0
        tar_slices <- sapply(tar_slices, function(x) if (x == 0.0) {
            x <- 0.01
        } else {
            (x)
        })
        # Draw pie
        pie3D(tar_slices,
              labels = c("", tar_lbl), explode = 0.03, main = tar_schema_name,
              radius = 1.0, labelcex = 1.5, theta = 0.8, start = pi / 2, cex.main = 2.0, col = rainbow(nrow(tar_value) + 1, s = 0.7)
        )
    }, # If data isn't exist...
    error = function(error_message) {
        print(error_message)
        afterError()
    }
    )
}

draw_table_pie <<- function(std_value, tar_value, tblname, path) {
    jpeg(filename = paste0("images/", path), width = 720, height = 720, quality = 75, bg = "white")
    par(mfrow = c(1, 2))
    # standard CDM
    tryCatch({
        # Label Setting
        # Label name set , append ratio num, percentage mark
        std_recordlbl <- paste(tblname, "\n", std_value$ratio)
        std_recordlbl <- paste(std_recordlbl, "%", seq = "")
        # Set pie slice
        # just 1 kind of attribute
        std_recordslices <- c(as.numeric(std_value$ratio), as.numeric(100 - std_value$ratio))
        # pie3d doesn't work which value is 0.0
        std_recordslices <- sapply(std_recordslices, function(x) if (x == 0.0) {
            x <- 0.01
        } else {
            (x)
        })
        pie3D(std_recordslices,
              labels = c(std_recordlbl, ""), explode = 0.03, main = std_schema_name,
              col = rainbow(nrow(std_value) + 1, s = 0.7), radius = 1.0, labelcex = 1.5, theta = 0.8, start = pi / 2, cex.main = 2.0
        )
    }, # If data isn't exist...
    error = function(error_message) {
        print(error_message)
        afterError()
    }
    )
    # target CDM
    tryCatch({
        # Label Setting
        tar_recordlbl <- paste(tblname, "\n", tar_value$ratio)
        tar_recordlbl <- paste(tar_recordlbl, "%", seq = "")
        # Set pie slice
        tar_recordslices <- c(as.numeric(tar_value$ratio), as.numeric(100 - tar_value$ratio))
        # pie3d doesn't work which value is 0.0
        tar_recordslices <- sapply(tar_recordslices, function(x) if (x == 0.0) {
            x <- 0.01
        } else {
            (x)
        })
        pie3D(tar_recordslices,
              labels = c(tar_recordlbl, ""), explode = 0.03, main = tar_schema_name,
              col = rainbow(nrow(tar_value) + 1, s = 0.7), radius = 1.0, labelcex = 1.5, theta = 0.8, start = pi / 2, cex.main = 2.0
        )
    }, # If data isn't exist...
    error = function(error_message) {
        print(error_message)
        afterError()
    }
    )
}

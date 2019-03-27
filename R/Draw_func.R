#' Draw func
#'
#' This function for draw graph
#' @keywords gemini
#' @export
#'

# Function for data visualization
draw_func<-function(std_schema_name,tar_schema_name){
    # Library for Pie 3D function
    options(scipen = 999999)
    # Draw graph in one bar chart

    # label set
    labeling <<- function(value) {
      lbl <- value$attributeName
      lbl <- paste0(lbl, "\n", value$ratio)
      lbl <- paste0(lbl, "%", seq = "")
      return(lbl)
    }
    # NA data
    naTostring <<- function(value) {
      napos <- sapply(value$attributeName, FUN = is.na)
      temp_sum <- sum(value$ratio)
      value[napos, 2] <- round(100 - temp_sum, 1)
      value$attributeName[napos] <- "NA"
      return(value)
    }
    # After no data error, print "No data" to show user
    afterError <<- function() {
        message("Failed visuallization.")
      plot(0, 0, axes = F, ann = F, type = "n")
      text(0, 0, "No data", cex = 2.0)
    }

    # Set x_axis by get min, max
    setXAxis <<- function(std_value, tar_value) {
      if (min(std_value$visitYear) < min(tar_value$visitYear)) {
        temp_min <- min(std_value$visitYear)
        temp_diff <- as.numeric(min(tar_value$visitYear)) - as.numeric(min(std_value$visitYear))
        temp_vec <- tar_value
      } else {
        temp_min <- min(tar_value$visitYear)
        temp_diff <- as.numeric(min(std_value$visitYear)) - as.numeric(min(tar_value$visitYear))
      }
      if (max(std_value$visitYear) > max(tar_value$visitYear)) {
        temp_max <- max(std_value$visitYear)
      } else {
        temp_max <- max(tar_value$visitYear)
      }
      lbl <- c(temp_min:temp_max)
      return(lbl)
    }

    setYAxis <<- function(std_value, tar_value) {
      # Set y_axis which got more big value
      if (max(std_value$personRatio) > max(tar_value$personRatio)) {
        axis <- as.numeric(ceiling(max(std_value$personRatio)))
      } else {
        axis <- as.numeric(ceiling(max(tar_value$personRatio)))
      }
      return(axis)
    }

    # low value label position setting
    label_sort <<- function(std_att, tar_att, unit = "%", dp = 70) {
      if (dp <= std_att) {
        std_lbl <- paste0("\n", std_att, unit)
      } else {
        std_lbl <- paste0(std_att, unit, "\n")
      }
      if (dp <= tar_att) {
        tar_lbl <- paste0("\n", tar_att, unit)
      } else {
        tar_lbl <- paste0(tar_att, unit, "\n")
      }
      return(c(std_lbl, tar_lbl))
    }

    gridline <<- function(std_value, tar_value) {
      if (nrow(std_value) > nrow(tar_value)) {
        abline(
          h = c(0:ceiling(as.numeric(max(std_value$personRatio, tar_value$personRatio)))),
          v = std_value$visitYear, lty = 3
        )
      } else {
        abline(
          h = c(0:ceiling(as.numeric(max(std_value$personRatio, tar_value$personRatio)))),
          v = tar_value$visitYear, lty = 3
        )
      }
    }

    concept_piecolor <- function(concept1,concept2){
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
    draw_ratio_pie <<- function(std_value, tar_value, path) {
      jpeg(filename = paste0("images/", path), width = 720, height = 720, quality = 75, bg = "white")

      par(mfrow = c(1, 2), xpd = T)
      # standard CDM
      tryCatch({
          if(!is.null(std_value$conceptId)){
              std_value <- std_value[order(std_value$conceptId),]
          }
        # If NA, value must get 0. So this NA value get error range 0.1
        cpt_col <- concept_piecolor(std_value,tar_value)
        std_value <- naTostring(std_value)
        std_cols <- unlist(sapply(std_value$attributeName, FUN=function(x) cpt_col[cpt_col$temp_cpt==x,])[2,],use.names = F)
        # Label Setting
        # Label name set , append ratio num, percentage mark
        std_lbl <- labeling(std_value)
        std_slices <- as.numeric(std_value$ratio)
        # pie3d doesn't work that some value is 0.0
        std_slices <- sapply(std_slices, function(x) if (x == 0.0) {
            x <- 0.01
          } else {
            (x)
          })
        if(is.null(std_value$conceptId)){
            std_legend <- std_value[order(std_value$attributeName),]$attributeName
        }
        else{
            std_legend <- paste0(std_value$conceptId,',',std_value[order(std_value$conceptId),]$attributeName)
        }

        # Draw Pie
        pie3D(std_slices,
          labels = paste0(std_value$ratio, "%"), explode = 0.1, main = std_schema_name,
          radius = 1.0, labelcex = 1.5, theta = 0.8, start = pi / 2, cex.main = 2.0, col = std_cols)
        legend(-1.5, -1.5,std_legend, cex = 1.3, fill = std_cols, xpd = T)
        },
        # If data isn't exist...
      error = function(error_message) {
        print(error_message)
        afterError()
      }
      )
      # target CDM

      tryCatch({
          if(!is.null(std_value$conceptId)){
              tar_value <- tar_value[order(tar_value$conceptId),]
          }
        # If NA, value must get 0. And this NA value get error range 0.1
        tar_value <- naTostring(tar_value)
        tar_cols <- unlist(sapply(tar_value$attributeName, FUN=function(x) cpt_col[cpt_col$temp_cpt==x,])[2,],use.names = F)
        # Label Setting
        tar_lbl <- labeling(tar_value)
        tar_slices <- as.numeric(tar_value$ratio)
        # pie3d doesn't work that some value is 0.0
        tar_slices <- sapply(tar_slices, function(x) if (x == 0.0) {
            x <- 0.01
          } else {
            (x)
          })

        if(is.null(tar_value$conceptId)){
            tar_legend <- tar_value[order(tar_value$attributeName),]$attributeName
        }
        else{
            tar_legend <- paste0(tar_value$conceptId,',',tar_value[order(tar_value$conceptId),]$attributeName)
        }
        # Draw Pie
        pie3D(tar_slices,
          labels = paste0(tar_value$ratio, "%"), explode = 0.03, main = tar_schema_name,
          radius = 1.0, labelcex = 1.5, theta = 0.8, start = pi / 2, cex.main = 2.0, col = tar_cols)
        legend(-1.5, -1.5, tar_legend, cex = 1.3, fill = tar_cols, xpd = T)
      }, # If data isn't exist...
      error = function(error_message) {
        print(error_message)
        afterError()
      }
      )
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
        sapply(std_recordslices, function(x) if (x == 0.0) {
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
        sapply(tar_recordslices, function(x) if (x == 0.0) {
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

    # Draw line graph for start date
    draw_line_start <<- function(std_value, tar_value, title = "", path) {
      jpeg(filename = paste0("images/", path), width = 720, height = 720, quality = 75, bg = "white")
      par(mfrow = c(1, 1))
      # Draw line Graph
      tryCatch({
        x_lbl <- setXAxis(std_value, tar_value)
        y_axis <- setYAxis(std_value, tar_value)
        # drawing line
        plot(std_value,
          type = "o", col = 4, lwd = 2, xlab = "YEAR", ylab = "Person ratio(%)", axes = F, xlim = c(min(x_lbl), max(x_lbl)), ylim = c(0, y_axis),
          main = paste0(title), cex.main = 2.0, cex.lab = 1.4
        )
        lines(tar_value, type = "o", col = 2, lwd = 2)
        axis(1, at = c(1:length(x_lbl)), labels = x_lbl)
        axis(2, at = c(0:y_axis), cex.axis = 2.0)
        box()
        gridline(std_value, tar_value)
        legend("topleft", c(std_schema_name, tar_schema_name), lwd = 2, lty = 1, cex = 1.5, col = c("blue", "red"))
      }, # If data isn't exist...
      error = function(error_message) {
        print(error_message)
        afterError()
      }
      )
    }

    # Draw line graph for end date
    draw_line_end <<- function(std_value, tar_value, na_value = "No data", over_value = "No data", title = "", path) {
      jpeg(filename = paste0("images/", path), width = 720, height = 720, quality = 75, bg = "white")
      par(mfrow = c(1, 1))
      # Draw line Graph
      tryCatch({
        # Set x_axis by get min, max
        x_lbl <- setXAxis(std_value, tar_value)
        y_axis <- setYAxis(std_value, tar_value)

        # drawing line
        plot(std_value,
          type = "o", col = 4, xlab = "YEAR", ylab = "Person_ratio(%)", axes = T, xlim = c(min(x_lbl), max(x_lbl)), ylim = c(0, y_axis),
          main = paste0(title), lwd = 2, cex.main = 2.0, cex.lab = 1.5
        )
        lines(tar_value, type = "o", col = 2, lwd = 2)
        axis(1, at = c(1:length(x_lbl)), labels = x_lbl)
        axis(2, at = c(0:y_axis), cex.axis = 2.0)
        box()
        gridline(std_value, tar_value)
        legend("topleft", c(std_schema_name, tar_schema_name), lwd = 2, lty = 1, cex = 1.5, col = c("blue", "red"))
        # Need to append NA, 2999 person data.
        if (is.null(na_value) && is.null(over_value)) {}
        else {
          # as.numeric(min(c(std_value$visitYear,tar_value$visitYear)))+10,y=as.numeric(max(c(std_value$personRatio,tar_value$personRatio)))
          mtext(
            side = 3, line = -5, adj = 1, cex = 1.2, font = 2, outer = T,
            text = paste0("std_schema_name NA : ", na_value[1], " / B CDM NA : ", na_value[2], "\nstd_schema_name 2999 : ", over_value[1], " / B CDM 2999 : ", over_value[2])
          )
        }
      }, # If data isn't exist...
      error = function(error_message) {
        print(error_message)
        afterError()
      }
      )
    }

    # Draw bar chart with count and NULL
    draw_null_bar <<- function(std_value, tar_value, title = "", path) {
      jpeg(filename = paste0("images/", path), width = 720, height = 720, quality = 75, bg = "white")
      par(mfrow = c(1, 2))
      tryCatch({
        # count drawing
        bar_data <- barplot(c(std_value$attributeCount, tar_value$attributeCount),
          ylim = c(0, max(std_value$attributeCount, tar_value$attributeCount)), cex.names = 2.0,
          beside = F, names = c(std_schema_name, tar_schema_name), col = c("Green", "Yellow"), main = paste(title), cex.main = 2.0, cex.axis = 2.0,
          ylab = "Counts (s)", cex.lab = 1.5
        )
        text(
          x = bar_data, y = c(std_value$attributeCount, tar_value$attributeCount),
          labels = label_sort(std_value$attributeCount, tar_value$attributeCount, unit = "s", dp = max(c(std_value$attributeCount, tar_value$attributeCount)) * 0.5)
          , col = "black", cex = 1.5
        )
        # NULL data drawing
        null_bar <- barplot(c(std_value$nullRatio, tar_value$nullRatio),
          ylim = c(0, max(std_value$nullRatio, tar_value$nullRatio)),
          beside = F, names = c(std_schema_name, tar_schema_name), col = c("Gray40", "Gray55"), main = "NULL Ratio", cex.main = 2.0, cex.axis = 2.0,
          ylab = "Percentage (%)", cex.names = 2.0, cex.lab = 1.5
        )
        text(
          x = null_bar, y = c(std_value$nullRatio, tar_value$nullRatio),
          labels = label_sort(std_value$nullRatio, tar_value$nullRatio, dp = max(c(std_value$nullRatio, tar_value$nullRatio)) * 0.5)
          , col = "black", cex = 1.5
        )
      }, # If data isn't exist...
      error = function(error_message) {
        print(error_message)
        afterError()
      }
      )
    }

    # Draw bar chart
    draw_ratio_bar <<- function(std_value, tar_value, path) {
      jpeg(filename = paste0("images/", path), width = 720, height = 720, quality = 75, bg = "white")
      par(mfrow = c(1, 1))
      tryCatch({
        # count drawing
        bar_data <- barplot(c(std_value$ratio, tar_value$ratio),
          beside = T, names = c(std_schema_name, tar_schema_name), col = c("Green", "Yellow"), main = "Count",
          xlab = "Institution", ylab = "Percentage (%)", cex.names = 2.5, cex.main = 2.0, cex.lab = 1.5, cex.axis = 2.0
        )
        text(
          x = bar_data, y = c(std_value$ratio, tar_value$ratio),
          labels = label_sort(std_value$ratio, tar_value$ratio, dp = max(c(std_value$ratio, tar_value$ratio)) * 0.5), col = "black", cex = 1.5
        )
      }, # If data isn't exist...
      error = function(error_message) {
        print(error_message)
        afterError()
      }
      )
    }
    # Using count how many kinds
    draw_count_bar <<- function(std_value, tar_value, text = "", path) {
      jpeg(filename = paste0("images/", path), width = 720, height = 720, quality = 75, bg = "white")
      par(mfrow = c(1, 1))
      tryCatch({
        # count drawing
        bar_data <- barplot(c(std_value$attributeCount, tar_value$attributeCount),
          beside = F, names = c(std_schema_name, tar_schema_name), col = c("Green", "Yellow"), main = text,
          xlab = "Institution", ylab = "Counts (s)", cex.names = 1.5, cex.main = 2.0, cex.lab = 1.5, cex.axis = 2.0
        )
        text(
          x = bar_data, y = c(std_value$attributeCount, tar_value$attributeCount),
          labels = label_sort(std_value$attributeCount, tar_value$attributeCount, unit = "s", dp = max(c(std_value$attributeCount, tar_value$attributeCount)) * 0.5)
          , col = "black", cex = 1.5
        )
      }, # If data isn't exist...
      error = function(error_message) {
        print(error_message)
        afterError()
      }
      )
    }
    # Compare two attributes
    draw_compare_bar <<- function(std_value, tar_value, text = "", path) {
      jpeg(filename = paste0("images/", path), width = 720, height = 720, quality = 75, bg = "white")
      par(mfrow = c(1, 1))
      tryCatch({
        # count drawing
        bar_data <- barplot(c(std_value$ratio, tar_value$ratio),
          beside = F, names = c(std_schema_name, tar_schema_name), col = c("Green", "Yellow"), main = text,
          xlab = "Institution", ylab = "Percentage (%)", cex.names = 2.5, cex.main = 2.0, cex.lab = 1.5, cex.axis = 2.0
        )
        text(
          x = bar_data, y = c(std_value$ratio, tar_value$ratio),
          labels = label_sort(std_value$ratio, tar_value$ratio, dp = max(c(std_value$ratio, tar_value$ratio)) * 0.5), col = "black", cex = 1.5
        )
      }, # If data isn't exist...
      error = function(error_message) {
        print(error_message)
        afterError()
      }
      )
    }

}

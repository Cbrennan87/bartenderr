globalVariables(c("Mean", "Mean2", "Error", "y", "values"))
#` Title: Creates a Stacked Bar Graph using Calculated Cumulative Means and Deviation
#'
#' Calculates the cumulative mean values from a data frame and creates a stacked bar plot based upon those values allowing for error bars to added.
#'
#' @param df A data frame.
#' @param columns A character vector. A list of column numbers or names containing the data.
#' @param x A number or string. The x axis variable name or column number.
#' @param standard_error The measurement of variation, either "sem" or "sd".
#' @param graph A string. Determines if the output is a graph or the data frame. Either "TRUE" or "FALSE".
#' @param y_axis_title A string. The name of the y axis. Defaults to "y".
#' @param fill_name A string. The name of the variable to colour by. Defaults to "fill".
#' @returns A stacked bar graph or data frame.
#' @import ggplot2
#' @import dplyr
#' @import randomcoloR
#' @import plotrix
#' @import tidyr
#' @examples
#' stackedbar3(iris, c(2,3), 5, standard_error = "sem", graph = "TRUE", y_axis_title = "Measurement", fill_name = "Attributes")
#'
#' stackedbar3(iris,c("Sepal.Width", "Sepal.Length"), "Species", standard_error = "sd", graph = "FALSE")

stackedbar3 <- function(df, columns, x, standard_error = "sem", graph = "TRUE", y_axis_title = "y", fill_name = "fill") {
  dataset <- df

  if(is.character(columns)){
    columns <- as.character(columns)
  }
  if(is.numeric(x)){
    dataset$x <- df[,x]
    x_axis_title <- names(df)[x]
  }
  if(is.character(x)){
    dataset$x <- df %>% pull(x)
    x_axis_title <- df %>% select(all_of(x)) %>% colnames()
  }


  graphing_df <- dataset %>%
    pivot_longer(cols = columns,
                 names_to = "y",
                 values_to = "values")



  if(standard_error == "sem"){
    Graphing_Data <- graphing_df %>% group_by(x, y)%>% filter(!is.na(values)) %>% summarise(Mean = mean(values), Error = std.error(values))
    Graphing_Data2 <- mutate(group_by(Graphing_Data,x), Mean2=cumsum(Mean))
  }

  if(standard_error == "sd"){
    Graphing_Data <- graphing_df %>% group_by(x, y)%>% filter(!is.na(values)) %>% summarise(Mean = mean(values), Error = sd(values))
    Graphing_Data2 <- mutate(group_by(Graphing_Data,x), Mean2=cumsum(Mean))  }

  if(graph == "TRUE"){
    n <- length(unique(Graphing_Data2$y))
    palette <- randomColor(count = n)

    Stacked_Graph <- ggplot(Graphing_Data2, aes(x = reorder(x, -Mean), Mean, fill = reorder(y, desc(y))))+geom_col(position = "stack", colour = 'black')+
      scale_y_continuous(name = y_axis_title, expand = c(0,0), limits = c(0,max(Graphing_Data2$Mean2+Graphing_Data2$Error)*1.2))+
      geom_errorbar(aes(ymin = Mean2-Error, ymax = Mean2+Error), width = 0.25)+
      scale_x_discrete(name = x_axis_title)+
      theme_classic()+
      scale_fill_manual(values = palette, name = fill_name)

    return(Stacked_Graph)
  }

  if(graph == "FALSE"){
    return(Graphing_Data2)

  }


}




globalVariables(c("Mean", "Mean2", "Error"))
#` Title: Creates a Stacked Bar Graph using Calculated Cumulative Means and Deviation
#'
#' Calculates the cumulative mean values from a already set data frame and creates a stacked bar plot based upon those values allowing for error bars to added.
#'
#' @param df A data frame.
#' @param x A character vector. The x axis variable name or column number.
#' @param y A numeric vector. The y axis variable name or column number.
#' @param fill_by A character vector. The name or column number of the stacking variable.
#' @param standard_error The measurement of variation, either "sem" or "sd".
#' @param graph Determines if the output is a graph or the data frame. Either "TRUE" or "FALSE".
#' @returns A stacked bar graph or data frame.
#' @import ggplot2
#' @import dplyr
#' @import randomcoloR
#' @import plotrix
#' @examples
#' stackedbar3(iris, 5, 2, 5, standard_error = "sem", graph = "TRUE")
#'
#' stackedbar3(iris,"Species", "Sepal.Width", "Species", standard_error = "sd", graph = "FALSE")


stackedbar3 <- function(df,x,y,fill_by,standard_error = "sem", graph = "TRUE") {
  dataset <- df

  if(is.numeric(x)){
    dataset$x <- df[,x]
    x_axis_title <- names(df)[x]
  }
  if(is.numeric(y)){
    dataset$y <- df[,y]
    y_axis_title <- names(df)[y]

  }

  if(is.numeric(fill_by)){
    dataset$fill_by <- df[,fill_by]
    fill_title <- names(df)[fill_by]

  }

  if(is.character(y)){
    dataset$y <- df %>% pull(y)
    y_axis_title <- df %>% select(all_of(y)) %>% colnames()
  }

  if(is.character(x)){
    dataset$x <- df %>% pull(x)
    x_axis_title <- df %>% select(all_of(x)) %>% colnames()
  }

  if(is.character(fill_by)){
    dataset$fill_by <- df %>% pull(fill_by)
    fill_title <- df %>% select(all_of(fill_by)) %>% colnames()
  }

  if(standard_error == "sem"){
    Graphing_Data <- dataset %>% group_by(x,fill_by)%>% filter(!is.na(y)) %>% summarise(Mean = mean(y), Error = std.error(y))
    Graphing_Data2 <- mutate(group_by(Graphing_Data,x), Mean2=cumsum(Mean))
  }

  if(standard_error == "sd"){
    Graphing_Data <- dataset %>% group_by(x,fill_by)%>% filter(!is.na(y)) %>% summarise(Mean = mean(y), Error = sd(y))
    Graphing_Data2 <- mutate(group_by(Graphing_Data,x), Mean2=cumsum(Mean))
  }

  if(graph == "TRUE"){
    n <- length(unique(Graphing_Data2$fill_by))
    palette <- randomColor(count = n)

    Stacked_Graph <- ggplot(Graphing_Data2, aes(x = reorder(x, -Mean), Mean, fill = reorder(fill_by, desc(fill_by))))+geom_col(position = "stack", colour = 'black')+
      scale_y_continuous(name = y_axis_title, expand = c(0,0), limits = c(0,max(Graphing_Data2$Mean2+Graphing_Data2$Error)*1.2))+
      geom_errorbar(aes(ymin = Mean2-Error, ymax = Mean2+Error), width = 0.25)+
      scale_x_discrete(name = x_axis_title)+
      theme_classic()+
      scale_fill_manual(values = palette, name = fill_title)
    return(Stacked_Graph)
  }

  if(graph == "FALSE"){

    return(Graphing_Data2)
  }


}



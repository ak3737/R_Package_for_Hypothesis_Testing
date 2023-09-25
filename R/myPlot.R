#' @title Method for plotting the class Rttest
#' @description
#' plot.Rttest is a function to streamlining the process of visualizing t-test
#' results by creating appropriate and well labelled box plots for both paired
#' and non- paired data. The function will generate appropriate box plots for
#' input data. If the data is non - paired, then, the plot.Rttest function will
#'  provide side by side box plots of x and y.
#'
#'  On the other hand. if the data is paired, then, the function will make a
#'  box plot of differences. Furthermore, the confidence interval  for the
#'  difference of means is plotted inside the box plot of differences. The plot
#'  function also ensures that all the box plots are well labelled.
#'
#'  If the input data is non- paired, then, function will return side by side
#'  box plots of x and y, with a title "Box plots for x and y".
#'
#' For paired input data, this function will return a box plot of differences,
#' titled "Difference between x and y" with the confidence interval for the
#' difference of means plotted inside the box plot of differences.

#' @param x An object of class Rttest which comes from the t test
#' @param ... Argument that will be passed to print.Rttest(unused currently)
#'
#' @return   A boxplot of the two samples using \code{ggplot()}
#'
#' @details
#' The function \code{myttest()} will produce a list of class Rttest, which
#' consists of three components. These are sourced from the object and in
#' \code{plot.Rttestt()}
#'
#' @export plot.Rttest
#' @importFrom ggplot2 aes geom_boxplot labs
#' @importFrom utils stack
#' @export
#' @rdname plot.Rttest
#' @examples
#' l <- myttest(x=rnorm(30), y=rnorm(40,0.5));plot(l)
#'
plot.Rttest<- function(x,...){
  # checking to see if the data is paired
  obj <- x
  data_paired <- obj[["TypeTest"]]=="Paired" # check if the data frame is paired
# For paired data, create data frame with ingle group titled differencesData
# and values (differences_data)
  if(data_paired){
    differences_data <- obj[["DataList"]]$x -obj[["DataList"]]$y
  # Now create a data frame
    df_plot <- data.frame(Group = "differencesData", Values =differences_data )
}
# if the data frame isn't paired then we create a data frame with two group x
# and y with the respective values
  else{
    col_name <- rep(c("x","y"), times = c(length(obj[["DataList"]]$x),
                                        length(obj[["DataList"]]$y)))
    col_values <- c(obj[["DataList"]]$x, obj[["DataList"]]$y)
    df_plot <- data.frame(Group =col_name, Values =  col_values)

}
# get the confidence interval for the difference of means and then create
# a label titled (ci_diff) that will be shown on the plot
  ci <- x[["SummaryStatistics"]]$conf.int
  #ci_diff <- paste0("[", round(ci[1],2), ",", round(ci[2],2),"]")



# Now, create the boxplots
  plt <- ggplot2::ggplot(df_plot, ggplot2::aes_string(x="Group", y = "Values",
                                                    fill = "Group"))+
    ggplot2::geom_boxplot()
  if(data_paired){
    plt <- plt +
      ggplot2::labs(title = "Difference between x and y")+
      ggplot2::xlab("Difference")+
      ggplot2::ylab("MyData")+
      ggplot2::geom_segment(aes(x = 1, xend = 1, y = ci[1],yend =ci[2]),
                            color = "blue", linetype = "dashed", size = 3 )

  }
  else{
    plt  <- plt +
      ggplot2::labs(title = "Boxplots for x and y")+
      ggplot2::xlab("Sample")+
      ggplot2::ylab("x and y")
  }
  plt <- plt +
    ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5,
                                                    size = 15))+
    ggplot2::theme(axis.title.x = ggplot2::element_text(size =10))+
    ggplot2::theme(axis.title.y = ggplot2::element_text(size =10))+
    ggplot2::scale_fill_manual(values = c("aquamarine", "pink"))

  print(plt)

}


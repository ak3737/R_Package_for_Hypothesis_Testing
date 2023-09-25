#' @title Method for printing the class Rttest
#' @param x An object of class Rttest which comes from the t-test
#' @param ... Argument that will be passed to print.Rttest, and extra options to
#' be sent to method
#'
#'
#' @description
#' The print.Rttest function will take an object of the class "Rttest" and,
#' present the  t-test results in a comprehensive and intuitive way.The print
#' function will print A(1-alpha)*100 confidence interval for μₓ -  μᵧ.
#'
#' @details
#' The function \code{myttest()} will produce a list of class Rttest, which
#' consists of three components. These are sourced from the object and in
#' \code{print.Rttestt()}
#'
#' @return The print.Rttest function  will print A(1-alpha)*100 confidence
#' interval for μₓ -  μᵧ.
#'
#'
#' @export
#' @export print.Rttest
#' @importFrom dplyr '%>%'
#' @importFrom knitr kable
#' @importFrom kableExtra kable_styling
#' @rdname print.Rttest
#'
#' @examples
#' l <- myttest(x=rnorm(30), y=rnorm(40,0.5))
#' print(l)
#'
#'
#'

print.Rttest <- function(x, ...) {
  obj <- x
  confidence_interval <- obj[["SummaryStatistics"]]$conf.int #confidence interval
  alpha_val <- as.numeric(obj["alpha"]) # alpha value
  confidence_level <- (1-alpha_val)* 100 # confidence level
  cat(sprintf("At %.0f%% confidence level, confidence interval for mu_x - mu_y  :[%.3f,%.3f]\n",
              confidence_level,
              confidence_interval[1],
              confidence_interval[2]))
  }

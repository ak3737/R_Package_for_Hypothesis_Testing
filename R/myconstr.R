#' @title A constructor function named myttest is used for conducting t-test
#' @param x vector of data
#' @param y vector of data
#' @param paired argument that is TRUE when data is paired and FALSE otherwise
#' @param alpha  alpha level between 0,1
#'
#' @importFrom stats 't.test'
#' @importFrom stats 'var.test'
#' @description This constructor function will take two vectors x and y. It will
#' check whether the data is paired or not.
#'
#'  If the data is paired, then , we run  the relevant t-test, where argument
#'  paired is set to TRUE. If the data is not paired ,an f-test is conducted to
#'  check for equality of variances. Subsequently, the appropriate t-test is
#'  conducted depending the variances are equal or not. After conducting the
#'  appropriate t-test, a list a returned that shows whether, "Welch", "T-Test"
#'  or "Paired" t-test was conducted, a Y/N conclusion as to whether we should
#'  reject the NULL of equality of means at the alpha level of significance,
#'  summary statistics made by the appropriate t.test, The data in a list.
#'
#'  Then, we we release the above list invisibly with the class "Rttest".
#'
#'
#' @return The constructor function will return a list containing the following
#' information in a list.
#'
#' 1. Which Type of t-test was conducted depending on the properties of the
#'   input data. The available options for different t-tests include "Welch",
#'   "T-Test"  or "Paired" t-test. When the input data x and y are paired, then
#'   we conduct the "Paired" t-test. For non- paired data, if the underlying
#'   variances are  the same(equal) , then "T-test" t-test is conducted. For
#'   non- paired data, if the underlying variances are he different(unequal),
#'   then "Welch" t-test is conducted.
#'
#' 2. A Y/N (Y= Yes, N = No) conclusion to whether we reject the null hypothesis
#'   or not at the alpha level of significance
#'
#' 3. The summary statistics obtained from the appropriate t.test.
#'
#' 4. The data in a list (data vectors x and y ).
#'
#' This list is then released invisibly with the "Rttest" class.
#'
#' @export
#'
#'@examples
#'myttest(x=rnorm(30,10,12), y=rnorm(40, 7, 10))
#'

# First define the function myttest,which takes two vectors and  alpha, and
# paired as arguments, the default value of paired argument will be false

myttest <- function(x, y, alpha = 0.05, paired = FALSE) {
  if(paired){                             # First , we will deal with the case
                                          # where paired = TRUE
    if(length(x)!= length(y)){            # check if x,y have different lengths,
                                          # for the paired test, x and y must be
                                          # of same length
      stop("X and Y must be of equal length to conduct the paired t-test")
    }
    result <- t.test(x,y,paired = TRUE)
    ttest_type <- "Paired"
  }
  else{
    var_test <- var.test(x,y)               # checking for equality of variances
    if(var_test$p.value < alpha){           # if p value of the f test is less
                                            # than alpha,variances are unequal,
                                            # we fail to reject  null hypothesis
      result <- t.test(x,y, var.equal = FALSE)
      ttest_type <- "Welch"
    }
    else{
      result <- t.test(x,y, var.equal = TRUE)
      ttest_type <- "T-test"
    }
  }
  # check if we should reject the null hypothesis or not, by checking whether the
  # p-value of result(ttest) is less than alpha,if p_value < alpha then, we can
  # reject the null hypothesis, returning Yes(Y), however, if p_value > alpha
  # then, that means, we cannot reject the null hypothesis returning No (N)
  reject_null <- ifelse(result$p.value < alpha , "Y", "N")

  # placing all the above information in a list
  output_list <- list(
    TypeTest = ttest_type, # the type of the t- test conducted
    RejectNull = reject_null, # reject null hypothesis (Y/N)
    SummaryStatistics =  result, # Summary statistics of the t-test
    DataList =  list(x = x, y= y), # data (x and y ) in a list
    alpha = alpha                  # alpha value (significance level)

  )
  # setting the appropriate class i.e class of output list to Rttest
  class(output_list) <- "Rttest"

  # Releasing  the list  invisibly
  return(invisible(output_list))
}

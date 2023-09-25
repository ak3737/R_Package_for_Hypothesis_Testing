#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
# loading the package
library(ProjectPackageInstallment3)

# First, define UI for application that implements t-test analysis using the
# package created
# In the  ui , section we will let the user put values for the following inputs.
# 1. alpha value
# 2. sample size of x and y vectors
# 3. mean of x vector
# 4. mean of y vector
# 5. standard deviation of x vector
# 6.standard deviation of y vector
#
# For the paired data set, we will add the following numeric inputs
# 1. x and y mean difference
# 2. x and y standard deviation
# 3. check box , that can be used for indicating whether data is paired or not
#
# Finally, we add a button for running the test
#
ui <- fluidPage(

  # Application title
  titlePanel("T-test analysis"),

  # Sidebar with a slider for alpha value (significance level), mean, standard
  # deviation, sample size
  sidebarLayout(
    sidebarPanel(
      numericInput("alpha", "significance level: ", 0.05, min = 0.01,
                   max = 0.5, step = 0.01),  # put in the alpha level
      numericInput("sample_size_x_y", "sample size: ", 30), # sample size
      numericInput("mean_x","mean of x: ", 10), # mean of vector x
      numericInput("mean_y","mean of y: ", 10), # mean of vector y
      numericInput("sd_x","standard deviation for x: ", 15),
      numericInput("sd_y","standard deviation for y: ", 15),


      # for the paired data set , need the following parameters

      numericInput("mean_difference","x and y mean difference (For paired data): "
                   , 5),
      numericInput("sd_difference","x and y standard deviation (For paired data):
                   ", 4),

      checkboxInput("paired", "Paired data", FALSE),
      actionButton("run_test", "Run T-test")

    ),

    # Show output of the t-test
    mainPanel(
      verbatimTextOutput("test_result"),
      plotOutput("test_plot")
    )
  )
)

#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)


# Define server logic required to draw a histogram and print information
# about the t-test
# In the server logic section, a reactive block named test_data was created.
# The test_data reactive block is used to create the input data for x and y ,
# using the inputs mentioned earlier , which are obtained from the users.

# The server function , sets a seed for both x and y vectors. It then generates
# data sets for paired and unpaired data. When the user indicates, that the data
# is non-paired, then, format of x and y vectors is as follows:
# x = rnorm(sample size of x , mean of x, standard deviation of x)
# y = rnorm(sample size of y , mean of y, standard deviation of y)

# On the other hand , if the data is paired, then the format of x vector
# stays the same, however, the y vector will look as follows
# y=x +(rnorm(sample size of y ,x and y mean difference,x and y standard deviation)


# another reactive block named, ttest_result is created, which is used to
# We used renderPrint() function to show the results obtained from the t-test
# by assigning renderPrint() function to the output object
# We used renderPlot() function to show the box plots by assigning renderPlot()
# function to the output object
# Define server logic required to draw a histogram
server <- function(input, output) {
  test_data <- reactive({
    set.seed(32) # set seed for x
    x=rnorm(input$sample_size_x_y,mean=input$mean_x, sd=input$sd_x)
    set.seed(35) # set seed for y
    if(input$paired){
      y = x+ rnorm(input$sample_size_x_y, input$mean_difference,sd= input$sd_difference)
    }
    else{
      y=rnorm(input$sample_size_x_y,mean=input$mean_y,sd=input$sd_y)
    }
    list(x=x, y = y )
  })
  ttest_result <- eventReactive(input$run_test,{
    myttest(test_data()$x, test_data()$y, alpha = input$alpha, paired = input$paired)
  })

  output$test_result <- renderPrint({
    print(ttest_result())
  })

  output$test_plot <- renderPlot({
    plot(ttest_result())
  })
}

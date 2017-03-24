library(shiny)
library(shinydashboard)
library(shinythemes)
# library(shinyFiles)

shinyUI(navbarPage(theme = shinytheme("cerulean"),
                   "Risk Scenario Simulation",
                   tabPanel("Curves simulated",
                            sidebarPanel(
                              helpText("Select from main portfolio curves and simulation parameters"),
                              sliderInput("numsimslider", label = h4("Number of Sims"),
                                          min = 200, max = 2000, step = 200, value = 500),
                              dateRangeInput("curvedaterange", label = h4("Range of Forward Curve Dates"),
                                             start = '2016-11-01', end = as.character(Sys.Date() - 1)),
                              dateRangeInput("simrangemonth", label = h4("Range of Forward Months to Simulate"),
                                             start = '2018-01-01', end = '2018-12-01'),
                              selectInput("mkt", "Market", c('ERCOT', 'PJM'),
                                          selected = 'ERCOT'),
                              checkboxGroupInput("curvelist", "Select components in that market:", choices = c("")),
                              checkboxInput("tblout", "include table with select percentiles of simulated prices", value = FALSE),
                              actionButton("goButton1", "Start"),
                              downloadButton('downloadData', 'Download')
                              ),
                            mainPanel(
                              tabsetPanel(
                                tabPanel("plot of simulated curves (50 paths)", plotOutput("pricePlot")),
                                tabPanel("table with select percentiles", dataTableOutput("pctileTbl"))
                              )
                            )
                    )
                  )

)
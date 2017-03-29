library(shiny)
library(shinydashboard)
library(shinythemes)
# library(shinyFiles)

shinyUI(navbarPage(theme = shinytheme("cerulean"),
                   "Risk Scenario Simulation",
                   tabPanel("Curves simulated",
                            sidebarPanel(
                              helpText("Select from main portfolio curves and simulation parameters;
                                       Note that number of simulations below 1000 has noticeable non-convergence to forwards."),
                              sliderInput("numsimslider", label = h4("Number of Sims"),
                                          min = 200, max = 2000, step = 200, value = 500),
                              dateRangeInput("curvedaterange", label = h4("Historical Range of Forward Curve Dates"),
                                             start = '2016-11-01', end = as.character(Sys.Date() - 3)),
                              helpText("Make sure the ending curve date selected is not a weekend or holiday. 
                                       This is used to calculate correlation matrix."),
                              dateRangeInput("simrangemonth", label = h4("Range of Forward Months to Simulate"),
                                             start = '2018-01-01', end = '2018-12-01'),
                              selectInput("mkt", "Market", c('ERCOT', 'PJM'),
                                          selected = 'ERCOT'),
                              checkboxGroupInput("curvelist", "Select components in that market:", choices = c("")),
                              dateRangeInput("aggrangemonth", label = h4("Select strip of aggregation (optional)"),
                                             start = '2017-07-01', end = '2017-08-01'),
                              helpText("Make sure the aggregation period is included in the simulation period
                                       selected above"),
                              checkboxInput("tblout", "include table with select percentiles of simulated prices", value = FALSE),
                              checkboxInput("aggreg", "include custom range aggregation", value = FALSE),
                              checkboxInput("spreadopt", "include valuation of dummy spread option", value = FALSE),
                              sliderInput("hr", label = h4("Heat rate for spread option"),
                                          min = 4, max = 15, step = 0.5, value = 10),
                              sliderInput("vom", label = h4("VOM for spread option"),
                                          min = 0, max = 10, step = 0.5, value = 2),
                              actionButton("goButton1", "Start"),
                              downloadButton('downloadPct', 'Download Percentiles'),
                              downloadButton('downloadSim', 'Download All Simulations'),
                              downloadButton('downloadAgg', 'Download Strip Simulations'),
                              downloadButton('downloadSprd', 'Download Spread Option Simulations')
                              ),
                            mainPanel(
                              tabsetPanel(
                                tabPanel("plots of simulated curves", 
                                         fluidRow(
                                           column(10, plotOutput("pricePlot")),
                                           column(10, plotOutput("distPlot")))),
                                tabPanel("table with select percentiles", dataTableOutput("pctileTbl")),
                                tabPanel("distribution of prices aggregated by strip",
                                         fluidRow(
                                           column(10, plotOutput("aggDistPlot")),
                                           column(10, dataTableOutput("aggregTbl"))
                                         )),
                                tabPanel("distibution of custom spread option", 
                                         fluidRow(
                                           column(10, dataTableOutput('spreadTbl')),
                                           column(10, plotOutput('spreadPeriodPlot'))))
                              )
                            )
                    )
                  )

)
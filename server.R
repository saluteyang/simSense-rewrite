library(shiny)
library(plotly)
library(aligne)
library(ggplot2)
library(scales)
library(data.table)
library(stringr)

shinyServer(function(input, output, session){

  simOutput <- reactive({
    
    if(input$goButton1 == 0)
      return()
    
    # validate user date inputs
    validate(
      need(wday(as.Date(input$curvedaterange[2])) %in% (2:6), 
           "Check that the ending curve date doesn't end on a weekend or holiday"),
      need(as.Date(input$simrangemonth[1]) >= 
             as.Date(timeDate::timeLastDayInMonth(as.Date(input$curvedaterange[2]))) + 1,
           "Check that the first simulated forward month is after the month of the ending curve date"),
      need(input$aggreg == 0 | 
             (as.Date(input$aggrangemonth[1]) >= as.Date(input$simrangemonth[1]) & 
              as.Date(input$aggrangemonth[2]) <= as.Date(input$simrangemonth[2])),
           "Check that the strip aggregation requested falls within the range of simulated months above")
    )
    
    ### user input variables #####
    nsims <- input$numsimslider
    # period of forward prices used
    curve.date.begin <- as.Date(input$curvedaterange[1]) # based on availability, may not all be available
    curve.date.end <- as.Date(input$curvedaterange[2]) # make sure this is not a weekend/holiday
    # start and end date of spot dates
    start_date <- as.Date(input$simrangemonth[1])
    end_date <- as.Date(input$simrangemonth[2])
    # curve market/component
    marketcomponentstr <- input$mktcomp
    ### end of input variables #####
    
    temp <- fwdSimWrapper(nsims, curve.date.begin, curve.date.end, start_date, end_date, marketcomponentstr)
    return(list(outputSim = temp$outputSim, outputFwd = temp$outputFwd))
    })

  fwd <- reactive({
    if(input$goButton1 == 0 | input$visfwd == 0)
      return()
    
    fwdData <- as.data.table(simOutput()$outputFwd)
    ifelse(input$pwrseg == 'On Peak',
           fwdData <- fwdData[Segment != 'opPrice', ],
           fwdData <- fwdData[Segment != 'pkPrice', ]
    ) 
    fwdData.cast <- dcast(fwdData, Date + Delmo ~ Segment, value.var = 'Price')
    fwdData.cast <- fwdData.cast[complete.cases(fwdData.cast),]
    fwdData.cast <- fwdData.cast[order(Delmo, Date),]
    fwdData.cast[, Delmo := as.character(format(Delmo, "%b%y"))]
    
    return(list(fwdData = fwdData, fwdData.cast = fwdData.cast))
  })
  
  output$p1 <- renderPlotly({
    if(input$goButton1 == 0 | input$visfwd == 0)
      return()
    
    d <- event_data("plotly_click")
    p <- plot_ly(fwd()$fwdData.cast, x = ~ Date, y = ~ rtcPrice) %>%
      add_lines(key = ~ Delmo, color = ~ Delmo)
    if (!is.null(d)) {
      m <- fwd()$fwdData.cast[Date == unique(d[["x"]]), ]
      p <- add_markers(p, data = m, color = I("red")) # text = ~paste("y"), clickinfo = "text")
    }
    p %>% layout(title = "NYMEX NG Prices",
                 yaxis = list(title = ""),
                 showlegend = FALSE) %>% config(displayModeBar = F)
  })
  
  output$p2 <- renderPlotly({
    if(input$goButton1 == 0 | input$visfwd == 0){
      return()
    }
    
    else {
      if(input$pwrseg == 'On Peak'){
        d <- event_data("plotly_click")
        p <- plot_ly(fwd()$fwdData.cast, x = ~ Date, y = ~ pkPrice) %>%
          add_lines(key = ~ Delmo, color = ~ Delmo)
        if (!is.null(d)) {
          m <- fwd()$fwdData.cast[Date == unique(d[["x"]]), ]
          p <- add_markers(p, data = m, color = I("red"))
        }
        p %>% layout(title = "On Peak Power Prices",
                     xaxis = list(title = ""),
                     yaxis = list(title = ""),
                     showlegend = FALSE) %>% config(displayModeBar = F)
      } else{
        d <- event_data("plotly_click")
        p <- plot_ly(fwd()$fwdData.cast, x = ~ Date, y = ~ opPrice) %>%
          add_lines(key = ~ Delmo, color = ~ Delmo)
        if (!is.null(d)) {
          m <- fwd()$fwdData.cast[Date == unique(d[["x"]]), ]
          p <- add_markers(p, data = m, color = I("red"))
        }
        p %>% layout(title = "Off Peak Power Prices",
                     xaxis = list(title = ""),
                     yaxis = list(title = ""),
                     showlegend = FALSE) %>% config(displayModeBar = F)
        
      }
    }
  })
  
  fwdVol <- reactive({
    if(input$goButton1 == 0 | input$visvol == 0)
      return()
    
    market <- c(unlist(strsplit(input$mktcomp, '-'))[c(TRUE, FALSE)], 'NYMEX')
    component <- c(unlist(strsplit(input$mktcomp, '-'))[c(FALSE, TRUE)], 'NG')
    fwdVolData <- as.data.table(monthlyPkVol.multi.rng(input$curvedaterange[1], input$curvedaterange[2],
                                                       input$simrangemonth[1], input$simrangemonth[2],
                                                       market = market, component = component))
    fwdVolData <- fwdVolData[order(DELMO, CURVEDATE),]
    fwdVolData[, ':=' (DELMO = as.character(format(DELMO, "%b%y")),
                       MARKET = trimws(MARKET),
                       COMPONENT = trimws(COMPONENT))]
    fwdVolPwr <- fwdVolData[COMPONENT != 'NG', ]
    fwdVolNG <- fwdVolData[COMPONENT == 'NG', ]
    return(list(fwdVolPwr = fwdVolPwr, fwdVolNG = fwdVolNG))
  })
  
  output$p3 <- renderPlotly({
    if(input$goButton1 == 0 | input$visvol == 0)
      return()
    
    d <- event_data("plotly_click")
    p <- plot_ly(fwdVol()$fwdVolNG, x = ~ CURVEDATE, y = ~ VOLATILITY) %>%
      add_lines(key = ~ DELMO, color = ~ DELMO)
    if (!is.null(d)) {
      m <- fwdVol()$fwdVolNG[CURVEDATE == unique(d[["x"]]), ]
      p <- add_markers(p, data = m, color = I("red")) # text = ~paste("y"), clickinfo = "text")
    }
    p %>% layout(title = "NYMEX NG Volatility",
                 xaxis = list(title = ""),
                 yaxis = list(title = ""),
                 showlegend = FALSE) %>% config(displayModeBar = F)
  })
  
  output$p4 <- renderPlotly({
    if(input$goButton1 == 0 | input$visvol == 0)
      return()
    
    d <- event_data("plotly_click")
    p <- plot_ly(fwdVol()$fwdVolPwr, x = ~ CURVEDATE, y = ~ VOLATILITY) %>%
      add_lines(key = ~ DELMO, color = ~ DELMO)
    if (!is.null(d)) {
      m <- fwdVol()$fwdVolPwr[CURVEDATE == unique(d[["x"]]), ]
      p <- add_markers(p, data = m, color = I("red")) # text = ~paste("y"), clickinfo = "text")
    }
    p %>% layout(title = "Peak Power Volatility",
                 xaxis = list(title = ""),
                 yaxis = list(title = ""),
                 showlegend = FALSE) %>% config(displayModeBar = F)
  })
  
  output$correlation <- renderPlotly({
    if(input$goButton1 == 0)
      return()
    
    if (!is.null(event_data("plotly_click"))) {
      m <- fwd()$fwdData[Date == unique(event_data("plotly_click")[["x"]]), ]
    } else {return()}
    
    # get click curve date
    clickDate <- unique(m$Date)
    windowStart <- as.numeric(clickDate) - as.numeric(input$window)
    if(min(fwd()$fwdData$Date) > windowStart)
      return() # add validate statement
    
    fwdData.pivot <- dcast(fwd()$fwdData[Date >= windowStart & Date <= clickDate,], Date~Component + Delmo + Segment, value.var = 'Price') # dcast sorts columns lexically
    fwdData.pivot <- cbind(dplyr::select(fwdData.pivot, Date),
                           dplyr::select(fwdData.pivot, -Date))
    fwdData.pivot <- fwdData.pivot[complete.cases(fwdData.pivot),]
    fwdData.ret <- log(fwdData.pivot[-1,-1]/fwdData.pivot[-dim(fwdData.pivot)[1],-1])
    rownames(fwdData.ret) <- fwdData.pivot$Date[-1]
    fwdData.cor <- round(cor(fwdData.ret), 2)
    fwdData.cor <- fwdData.cor[substr(rownames(fwdData.cor), 1, 2) == 'NG', 
                               substr(colnames(fwdData.cor), 1, 2) != 'NG']
    plot_ly(x = paste(str_pad(seq(1:length(unique(fwd()$fwdData$Delmo))), 2, pad = "0"), str_match(rownames(fwdData.cor), "^(.*)_(.*)_(.*)$")[,2],
                      format(as.Date(str_match(rownames(fwdData.cor), "^(.*)_(.*)_(.*)$")[,3]), '%b%y'),
                      sep = "."), 
            y = paste(str_pad(seq(1:length(unique(fwd()$fwdData$Delmo))), 2, pad = "0"), str_match(colnames(fwdData.cor), "^(.*)_(.*)_(.*)$")[,2],
                      format(as.Date(str_match(colnames(fwdData.cor), "^(.*)_(.*)_(.*)$")[,3]), '%b%y'),
                      sep = "."), 
            z = fwdData.cor, type = 'heatmap',
            colors = colorRamp(c('#e3dfc8', '#808c6c'))) %>% 
      layout(title = "Correlation matrix (hover over to show value)",
             xaxis = list(title = ""),
             yaxis = list(title = ""),
             margin = list(l = 120, r = 50, b = 120, t = 50)) %>% config(displayModeBar = F)
  })
  
  selectPaths <- reactive({
    if(input$goButton1 == 0)
      return()
    
    sim.prices.long.select <- as.data.table(simOutput()$outputSim)
    sim.prices.long.select <- sim.prices.long.select[, .SD[1:50], by = list(Component, Delmo, Segment)]
    return(sim.prices.long.select)
    
  })
  
  aggregation <- reactive({
    if (input$aggreg)
    aggregDist <- as.data.table(simOutput()$outputSim)
    aggregDist <- aggregDist[Delmo >= input$aggrangemonth[1] & Delmo <= input$aggrangemonth[2], ]
    aggregDist <- aggregDist[, {
      Strip_Price = mean(Price)
      list(Strip_Price = Strip_Price)},
      by = .(Component, Segment, SimNo)]
    aggregDist <- aggregDist[, MeanOfPeriod := mean(Strip_Price),
                             by = .(Component, Segment)]
    return(aggregDist)
  })  
  
  spreadSummary <- reactive({
    if(input$spreadopt){
      forspread <- as.data.table(simOutput()$outputSim)
      forspread$HR <- input$hr
      forspread$VOM <- input$vom
      forspread1 <- forspread[Component != 'NG',]
      forspread2 <- forspread[Component == 'NG',]
      forspread2 <- dplyr::select(forspread2, Delmo, SimNo, Price)
      setnames(forspread2, 'Price', 'gasPrice')
      forspread.fin <- join(forspread1, forspread2, by = c('Delmo', 'SimNo'), type = 'left')
      forspread.fin <- forspread.fin[, optPrice := ifelse(Price - gasPrice * HR - VOM > 0, 
                                                          Price - gasPrice * HR - VOM, 0)]
      return(forspread.fin)
    }
  })

  
  byperiodSpread <- reactive({
    if(input$spreadopt & input$aggreg)
    spreadOptPeriod <- as.data.table(spreadSummary())
    spreadOptPeriod <- spreadOptPeriod[Delmo >= input$aggrangemonth[1] & Delmo <= input$aggrangemonth[2], ]
    spreadDates <- expandDates(input$aggrangemonth[1], 
                               as.Date(timeDate::timeLastDayInMonth(input$aggrangemonth[2])))$time.segment
    spreadDates <- mutate(spreadDates, sumOffPk = sum7x8 + sum2x16)
    spreadDates <- spreadDates[, c('Delmo', 'sumPk', 'sumOffPk')]
    spreadDates <- rename(spreadDates, c('sumPk' = 'pkPrice', 'sumOffPk' = 'opPrice'))
    spreadDates <- melt(spreadDates, measure = c('pkPrice', 'opPrice'), variable.name = 'Segment', value.name = 'Hrs')
    spreadOptPeriod <- join(spreadOptPeriod, spreadDates, by = c('Delmo', 'Segment'), type = 'left')
    spreadOptPeriod <- spreadOptPeriod[, sumOptPrice := optPrice * Hrs]
    spreadOptPeriod <- spreadOptPeriod[, {
      monthPayoff = sum(sumOptPrice)
      list(monthPayoff = monthPayoff)
    },
    by = .(Delmo, Component, SimNo)]
    spreadOptPeriod <- spreadOptPeriod[, meanMonthPayoff := mean(monthPayoff), by = .(Delmo, Component)]
    return(spreadOptPeriod)
  })
  
  output$pricePlot <- renderPlotly({
    
    if(input$goButton1 == 0)
      return()
    
    sim.prices.long.select <- as.data.table(simOutput()$outputSim)
    
    isolate(
      ggplotly(
      ggplot() + geom_line(data = sim.prices.long.select, aes(x = Delmo, y = Price, group = SimNo), 
                           color = 'dodgerblue', size = 0.1) +
        geom_line(data = sim.prices.long.select, aes(x = Delmo, y = Forward_Price),
                  color = 'black', size = 0.5, position = 'identity') + 
        scale_x_date(expand = c(0,0), date_breaks = '1 month', labels = date_format('%y-%m')) +
        facet_grid(Component + Segment ~., scales = 'free_y') + 
        theme(legend.position = 'none', axis.text.x = element_text(angle = 90)) + 
        scale_x_discrete(name = "") + scale_y_continuous(name = "")
      ) %>% config(displayModeBar = F) %>% 
        layout(xaxis = list(title = 'Delivery Month'), yaxis = list(title = "Price"),
               margin = list(l = 50, r = 50, b = 50, t = 50))
    )
  })
  
  output$distPlot1 <- renderPlotly({
    if(input$goButton1 == 0)
      return()
    
    distData <- as.data.table(simOutput()$outputSim)
    uniqueDelmos <- unique(distData$Delmo)
    distData <- dcast(distData, Delmo + SimNo ~ Segment, value.var = 'Price')
    
    p1 <- plot_ly(distData[Delmo == uniqueDelmos[1], ], 
                 x = ~ pkPrice, type = 'histogram', 
                 name = as.character(format(uniqueDelmos[1], "%b%y")),
                 alpha = 0.5, autobinx = FALSE, xbins = list(start = 0, end = 200, size = 2.5)) %>% 
      config(displayModeBar = FALSE)
    for (i in 2:length(uniqueDelmos)){
      p1 <- add_histogram(p1, distData[Delmo == uniqueDelmos[i], pkPrice], 
                         type = 'histogram', 
                         name = as.character(format(uniqueDelmos[i], "%b%y"))) %>%
        layout(barmode = 'overlay')
    }
    
    
    isolate(
      p1 %>% layout(xaxis = list(title = "On Peak Power Price"),
                    yaxis = list(title = "Count"))
    )

    
  })
  
  output$distPlot2 <- renderPlotly({
    if(input$goButton1 == 0)
      return()
    
    distData <- as.data.table(simOutput()$outputSim)
    uniqueDelmos <- unique(distData$Delmo)
    distData <- dcast(distData, Delmo + SimNo ~ Segment, value.var = 'Price')
    
    p2 <- plot_ly(distData[Delmo == uniqueDelmos[1], ], 
                  x = ~ opPrice, type = 'histogram', 
                  name = as.character(format(uniqueDelmos[1], "%b%y")),
                  alpha = 0.5, autobinx = FALSE, xbins = list(start = 0, end = 100, size = 1.5)) %>% 
      config(displayModeBar = FALSE)
    for (i in 2:length(uniqueDelmos)){
      p2 <- add_histogram(p2, distData[Delmo == uniqueDelmos[i], opPrice], 
                          type = 'histogram', 
                          name = as.character(format(uniqueDelmos[i], "%b%y"))) %>%
        layout(barmode = 'overlay')
    }
    
    
    isolate(
      p2 %>% layout(xaxis = list(title = "Off Peak Power Price"),
                    yaxis = list(title = "Count"))
    )
    
    
  })
  
  output$distPlot3 <- renderPlotly({
    if(input$goButton1 == 0)
      return()
    
    distData <- as.data.table(simOutput()$outputSim)
    uniqueDelmos <- unique(distData$Delmo)
    distData <- dcast(distData, Delmo + SimNo ~ Segment, value.var = 'Price')
    
    p3 <- plot_ly(distData[Delmo == uniqueDelmos[1], ], 
                  x = ~ rtcPrice, type = 'histogram', 
                  name = as.character(format(uniqueDelmos[1], "%b%y")),
                  alpha = 0.5, autobinx = FALSE, xbins = list(start = 0, end = 10, size = 0.25)) %>% 
      config(displayModeBar = FALSE)
    for (i in 2:length(uniqueDelmos)){
      p3 <- add_histogram(p3, distData[Delmo == uniqueDelmos[i], rtcPrice], 
                          type = 'histogram', 
                          name = as.character(format(uniqueDelmos[i], "%b%y"))) %>%
        layout(barmode = 'overlay')
    }
    
    
    isolate(
      p3 %>% layout(xaxis = list(title = "Nat Gas Price"),
                    yaxis = list(title = "Count"))
    )
    
    
  })
  
  output$aggDistPlot <- renderPlot({
    if(input$goButton1 == 0 | input$aggreg == 0)
      return()
    isolate(
      ggplot(aggregation(), aes(x = Strip_Price)) + geom_histogram() + 
        geom_vline(aes(xintercept = MeanOfPeriod), color = 'red', linetype = 'dashed') +
        facet_grid(. ~ Component + Segment, scales = 'free')

    )
    
  })
  
  output$spreadPeriodPlot <- renderPlot({
    if(input$goButton1 == 0 | input$spreadopt == 0 | input$aggreg == 0)
      return()
    isolate(
      ggplot(byperiodSpread(), aes(x = monthPayoff)) + geom_histogram() +
        geom_vline(aes(xintercept = meanMonthPayoff), color = 'red', linetype = 'dashed') +
        facet_grid (.~ Component + Delmo, scales = 'free') + 
        labs(caption = 'When both strip and spread option are specified, the distribution gives
             by power curve and month, the profitability of a hypothetical 1 MW gas plant (all segment hours).')
    )
  })
  
  pctileSummary <- reactive({
    if(input$tblout){
      pricePercentile <- as.data.table(simOutput()$outputSim)
      pricePercentile <- pricePercentile[, list(`5th` = round(quantile(Price, .05), digits = 2),
                                                `10th` = round(quantile(Price, .1), digits = 2),
                                                # `50th` = round(quantile(Price, 0.5), digits = 2),
                                                `90th` = round(quantile(Price, 0.9), digits = 2),
                                                `95th`= round(quantile(Price, 0.95), digits = 2),
                                                mean = round(mean(Price), digits = 2)),
                                         by = c('Component', 'Delmo', 'Segment')]
      return(pricePercentile)
    }
  })
  
  pctileAggSummary <- reactive({
    if(input$aggreg){
      aggregationPercentile <- as.data.table(aggregation())
      aggregationPercentile <- aggregationPercentile[, list(`5th` = round(quantile(Strip_Price, .05), digits = 2),
                                                            `10th` = round(quantile(Strip_Price, .1), digits = 2),
                                                            `90th` = round(quantile(Strip_Price, 0.9), digits = 2),
                                                            `95th`= round(quantile(Strip_Price, 0.95), digits = 2),
                                                            mean = round(mean(Strip_Price), digits = 2)),
                                                     by = c('Component', 'Segment')]
      return(aggregationPercentile)
    }
  })

  bymonthSpread <- reactive({
  if(input$spreadopt){
    spreadOptMonth <- as.data.table(spreadSummary())
    spreadOptMonth <- spreadOptMonth[, list(`5th` = round(quantile(optPrice, .05), digits = 2),
                                            `10th` = round(quantile(optPrice, .1), digits = 2),
                                            `90th` = round(quantile(optPrice, 0.9), digits = 2),
                                            `95th`= round(quantile(optPrice, 0.95), digits = 2),
                                            mean = round(mean(optPrice), digits = 2)),
                                     by = c('Component', 'Delmo', 'Segment')]
  }  
  })

  
  output$pctileTbl <- renderDataTable({
    input$goButton1
    isolate(
      pctileSummary()
    )
  }, options = list(pageLength = 10))
  
  output$aggregTbl <- renderDataTable({
    input$goButton1
    isolate(
      pctileAggSummary()
      )
  }, option = list(pageLength = 5))
  
  output$spreadTbl <- renderDataTable({
    input$goButton1
    isolate(
      bymonthSpread()
    )
  }, option = list(pageLength = 5))
  
  output$downloadPct <- downloadHandler(
    filename = function(){
      paste("simulated_percentiles", Sys.Date(), ".csv", sep = "")
    },
    content = function(file1){
      write.csv(pctileSummary(), file1, row.names = FALSE)
    }
  )
  
  output$downloadSim <- downloadHandler(
    filename = function(){
      paste("simulated_prices", Sys.Date(), ".csv", sep = "")
      },
    content = function(file2){
      write.csv(simOutput()$outputSim, file2, row.names = FALSE)
    }
  )
  
  output$downloadAgg <- downloadHandler(
    filename = function(){
      paste("simulated_prices_aggregated", Sys.Date(), ".csv", sep = "")
    },
    content = function(file3){
      write.csv(aggregation(), file3, row.names = FALSE)
    }
  )
  
  output$downloadSprd <- downloadHandler(
    filename = function(){
      paste("simulated_spreadopt_value", Sys.Date(), ".csv", sep = "")
    },
    content = function(file4){
      write.csv(spreadSummary(), file4, row.names = FALSE)
    }
  )
    
  observe({
    # mkt.curvelist <- switch(input$mkt,
    #                         ERCOT = c('ZONE N'),
    #                         PJM = c ('WESTRT'))
    # updateCheckboxGroupInput(session, "curvelist", choices = mkt.curvelist)
    
    ascmkt.asccurvelist <- switch(input$ascmkt,
                                  ERCOT = c('Fwd\\Coal\\PRB\\PRB\\8400\\0.8',
                                            'Fwd\\Pwr\\OffPk\\ERCOT\\Houston',
                                             'Fwd\\Pwr\\OnPk\\ERCOT\\Houston'),
                                  PJM = c('Fwd\\Coal\\NAPP\\MGA\\13000\\3.4',
                                          'Fwd\\Gas\\Tetco M3',
                                          'Fwd\\Pwr\\OnPk\\PJM\\Western Hub',
                                          'Fwd\\Pwr\\OffPk\\PJM\\Western Hub'))
    updateCheckboxGroupInput(session, "asccurvelist", choices = ascmkt.asccurvelist)
  })
  
  ascSimOutput <- reactive({
    if(input$goButton2 == 0)
      return()
    studyID <- input$studynum
    rptdate <- input$studydate
    ascendCurvelist <- input$asccurvelist
    ascendSims <- ascFwdSimQuery(studyID = studyID, curvelist = ascendCurvelist, rptdate = rptdate)
    ascendSims <- as.data.table(ascendSims)
    ascendSims <- ascendSims[DELIVERYDATE <= '2019-12-01', 
                             c('JOBID', 'DELIVERYDATE', 'WXSIMREP', 'PRICE', 'DESCRIPTION')]
    ascendSims[, DELIVERYDATE := as.Date(DELIVERYDATE)]
    # ascendSims[, SHORTDESC := str_sub(DESCRIPTION, 
    #                                   start = max(unlist(gregexpr("\\\\", DESCRIPTION))) + 1), 
    #            by = .(DESCRIPTION)]
    return(ascendSims)
  })
  
  selectPathsAscend <- reactive({
    if(input$goButton2 == 0)
      return()
    
    ascendSimsSelect <- as.data.table(ascSimOutput())
    ascendSimsSelect <- ascendSimsSelect[, meanPRICE := mean(PRICE), by = .(DELIVERYDATE, DESCRIPTION)]
    ascendSimsSelect <- ascendSimsSelect[WXSIMREP <= 50, ]
    ascendSimsSelect <- ascendSimsSelect[, SHORTDESC := paste0(
      str_sub(DESCRIPTION, end = unlist(gregexpr("\\\\", DESCRIPTION))[2]), "\n",
      str_sub(DESCRIPTION, start = unlist(gregexpr("\\\\", DESCRIPTION))[2] + 1)), by = .(DESCRIPTION)]
    # ascendSimsSelect <- ascendSimsSelect[, .SD[1:50], by = list(DELIVERYDATE, DESCRIPTION)]
    return(ascendSimsSelect)
    
  })
  
  output$ascPricePlot <- renderPlot({
    if(input$goButton2 == 0)
      return()
    isolate(
      ggplot() + geom_line(data = selectPathsAscend(), 
                           aes(x = DELIVERYDATE, y = PRICE, group = WXSIMREP), color = 'dodgerblue') +
        geom_line(data = selectPathsAscend(), aes(x = DELIVERYDATE, y = meanPRICE),
                  color = 'black', size = 1, position = 'identity') + 
        scale_x_date(expand = c(0,0), date_breaks = '1 month', labels = date_format('%y-%m')) +
        facet_grid(SHORTDESC ~ ., scales = 'free') + 
        theme(legend.position = 'none', axis.text.x = element_text(angle = 90),
              strip.text = element_text(margin = margin (.1, 0, .1, 0, "cm"))) 
    )
  })
  
  ascPctileSummary <- reactive({
    if(input$asctblout){
      ascPricePercentile <- as.data.table(ascSimOutput())
      ascPricePercentile <- ascPricePercentile[, list(`5th` = round(quantile(PRICE, .05), digits = 2),
                                                `10th` = round(quantile(PRICE, .1), digits = 2),
                                                `90th` = round(quantile(PRICE, 0.9), digits = 2),
                                                `95th`= round(quantile(PRICE, 0.95), digits = 2),
                                                mean = round(mean(PRICE), digits = 2)),
                                         by = c('DESCRIPTION', 'DELIVERYDATE')]
      return(ascPricePercentile)
    }
  })
  
  output$ascPercentileTbl <- renderDataTable({
    input$goButton2
    isolate(
      ascPctileSummary()
    )
  }, option = list(pageLength = 10))
  
  output$downloadAscend <- downloadHandler(
    filename = function(){
      paste("ascend_simulated_prices", Sys.Date(), ".csv", sep = "")
    },
    content = function(file5){
      write.csv(ascSimOutput(), file5, row.names = FALSE)
    }
  )
  
})
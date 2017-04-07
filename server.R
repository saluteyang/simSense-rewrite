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
    market <- unlist(strsplit(input$mktcomp, '-'))[1]
    component <- unlist(strsplit(input$mktcomp, '-'))[2]
    
    ### end of input variables #####
    
    numofmonth <- (as.yearmon(end_date) - as.yearmon(start_date))*12 + 1
    # first forward month
    first.month <- start_date
    # forward delivery months included
    month.used <- seq(as.Date(first.month), by = "month", length.out = as.integer(numofmonth))
    
    # day counts
    end_date <- timeDate::timeLastDayInMonth(end_date)
    out.days <- expandDates(start_date, end_date)$time.days
    period.pre <- as.numeric(as.Date(first.month) - as.Date(curve.date.end))/365
    period.rtc.inter <- cumsum(out.days$numTotDays)
    # day count convention to be consistent with option quotes
    period.pk <- c(period.pre, period.rtc.inter/365 + period.pre)
    period.pk <- period.pk[-length(period.pk)]
    
    ## price generation process starts here #######
    set.seed(123)
    
    # pull forward power
    pwr.curves <- futures.pwr.multi.rng.w(curve.date.begin, curve.date.end,
                                          first.month, month.used[numofmonth],
                                          rep(market, length(component)),
                                          component)
    
    pwr.curves <- melt(pwr.curves, id.vars = c('Date', 'Market', 'Component', 'Delmo'), 
                       variable.name = 'Segment', value.name = 'Price')
    pwr.curves <- dplyr::filter(pwr.curves, Segment != 'rtcPrice')
    
    ng.curve <- futures.ng.rng(curve.date.begin, curve.date.end,
                               first.month, month.used[numofmonth])
    ng.curve <- mutate(rename(ng.curve, c('NG' = 'rtcPrice')),
                       Market = "NYMEX", Component = "NG")
    ng.curve <- melt(ng.curve, id.vars = c('Date', 'Market', 'Component', 'Delmo'), 
                     variable.name = 'Segment', value.name = 'Price')
    
    curves.comb <- rbind.data.frame(pwr.curves, ng.curve) # coal.curve
    curves.comb$Component <- trimws(curves.comb$Component)
    curves.comb$Market <- NULL
    
    # make delmo as subscript of distinct commodities
    curves.comb.pivot <- dcast(curves.comb, Date~Component + Delmo + Segment, value.var = 'Price')
    curves.comb.pivot <- cbind(dplyr::select(curves.comb.pivot[, order(colnames(curves.comb.pivot))], Date),
                               dplyr::select(curves.comb.pivot[, order(colnames(curves.comb.pivot))], -Date))
    curves.comb.pivot <- curves.comb.pivot[complete.cases(curves.comb.pivot),]
    curves.comb.ret <- log(curves.comb.pivot[-1,-1]/curves.comb.pivot[-dim(curves.comb.pivot)[1],-1])
    rownames(curves.comb.ret) <- curves.comb.pivot$Date[-1]
    curves.comb.cor <- cor(curves.comb.ret)
    
    price.fwd <- curves.comb.pivot[which(curves.comb.pivot$Date == curve.date.end), -1]
    vol.fwd <- monthlyPkVol.multi(curve.date.end, month.used[numofmonth],
                                  c(rep(market, length(component)), 'NYMEX'),
                                  c(component,'NG'))
    # trim not needed volatilities
    vol.fwd <- vol.fwd[which(vol.fwd$DELMO %in% month.used),]
    vol.fwd <- mutate(vol.fwd, SEGMENT = ifelse(MARKET %in% c('NYMEX '), 'rtc', 'pk'))
    vol.fwd.app <- mutate(dplyr::filter(vol.fwd, !MARKET %in% c('NYMEX ')), VOLATILITY = VOLATILITY * 0.8)
    vol.fwd.app <- mutate(vol.fwd.app, SEGMENT = 'op')
    vol.fwd <- rbind(vol.fwd, vol.fwd.app)
    vol.fwd <- mutate(vol.fwd, NewComponent = paste(trimws(COMPONENT), as.character(DELMO), SEGMENT, sep = "_"))
    vol.fwd <- vol.fwd[, c('NewComponent', 'VOLATILITY')][order(vol.fwd$NewComponent),]
    
    ## simulate the forwards ####
    mu <- rep(0,length(price.fwd))
    s0 <- as.vector(t(price.fwd))
    sigma <- vol.fwd[, 2]%*%t(vol.fwd[, 2])*curves.comb.cor
    
    # the dimensions of the array are commodity, nsteps, and nsims
    sim.prices <- asset.paths(s0, mu, sigma, nsims, periods = period.pk)
    # flatten the array
    sim.prices.long <- do.call('rbind.data.frame',
                               lapply(1:dim(sim.prices)[1], function(x) cbind.data.frame(sim.prices[x,,], idx = x)))
    sim.prices.long$Monthsout <- month.used # cycling
    sim.prices.long <- join(data.frame(idx = 1:length(price.fwd), NewComponent = rownames(t(price.fwd))),
                            sim.prices.long, by = "idx", type = "left")
    sim.prices.long$idx <- NULL # remove idx after joining as key
    sim.prices.long <- melt(sim.prices.long, id.vars = c("Monthsout", "NewComponent"), 
                            variable.name = "SimNo", value.name = "Price")
    
    sim.prices.long <- rename(cbind.data.frame(sim.prices.long,
                                               as.data.frame(
                                                 str_match(sim.prices.long$NewComponent, "^(.*)_(.*)_(.*)$")[, -1])
                                               ),
                              c('V1' = 'Component', 'V2' = 'Delmo', 'V3' = 'Segment'))
    sim.prices.long <- mutate(sim.prices.long,
                              Delmo = as.Date(Delmo),
                              Monthsout = as.Date(Monthsout),
                              NewComponent = NULL)
    sim.prices.final.long <- subset(sim.prices.long, Delmo == Monthsout) # filter out expired contracts
    price.fwd.bind <- cbind.data.frame(str_match(names(price.fwd), "^(.*)_(.*)_(.*)$")[, -1], unname(t(price.fwd)))
    colnames(price.fwd.bind) <- c('Component', 'Delmo', 'Segment', 'Forward_Price')
    price.fwd.bind <- mutate(price.fwd.bind, Delmo = as.Date(Delmo))
    sim.prices.final.long <- join(sim.prices.final.long, price.fwd.bind, by = c('Component', 'Delmo', 'Segment'),
                                   type = 'left')
    return(sim.prices.final.long)
    })


  selectPaths <- reactive({
    if(input$goButton1 == 0)
      return()
    
    sim.prices.long.select <- as.data.table(simOutput())
    sim.prices.long.select <- sim.prices.long.select[, .SD[1:50], by = list(Component, Delmo, Segment)]
    return(sim.prices.long.select)
    
  })
  
  aggregation <- reactive({
    if (input$aggreg)
    aggregDist <- as.data.table(simOutput())
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
      forspread <- as.data.table(simOutput())
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
    
    sim.prices.long.select <- as.data.table(simOutput())
    
    isolate(
      ggplotly(
      ggplot() + geom_line(data = sim.prices.long.select, aes(x = Delmo, y = Price, group = SimNo), 
                           color = 'dodgerblue', size = 0.1) +
        geom_line(data = sim.prices.long.select, aes(x = Delmo, y = Forward_Price),
                  color = 'black', size = 0.5, position = 'identity') + 
        scale_x_date(expand = c(0,0), date_breaks = '1 month', labels = date_format('%y-%m')) +
        facet_grid(Component + Segment ~., scales = 'free_y') + 
        theme(legend.position = 'none', axis.text.x = element_text(angle = 90))
      ) %>% config(displayModeBar = F) # %>% layout(height = 500)
    )
  })
  
  output$distPlot <- renderPlotly({
    if(input$goButton1 == 0)
      return()
    
    distData <- as.data.table(simOutput())
    uniqueDelmos <- unique(distData$Delmo)
    
    p1 <- plot_ly(distData[Component == 'NG' & Delmo == uniqueDelmos[1], ],
                  x = ~ Price, type = 'histogram', 
                  name = paste0('NG ', as.character(format(uniqueDelmos[1], "%b%y"))), alpha = 0.5) %>% 
      config(displayModeBar = FALSE)
      # config(displaylogo = F,
      #        modeBarButtonsToRemove = list(
      #          'sendDataToCloud',
      #          'toImage',
      #          'autoScale2d',
      #          'hoverClosestCartesian',
      #          'hoverCompareCartesian'))

    for (i in 2:length(uniqueDelmos)){
      p1 <- add_histogram(p1, distData[Component == 'NG' & Delmo == uniqueDelmos[i], Price], type = 'histogram', 
                          name = paste0('NG ', as.character(format(uniqueDelmos[i], "%b%y")))) %>% 
        layout(barmode = 'overlay', legend = list(orientation = 'h'))
    }
    
    p2 <- plot_ly(distData[Component != 'NG' & Delmo == uniqueDelmos[1] & Segment == 'pkPrice', ],
                  x = ~ Price, type = 'histogram', 
                  name = paste0('PK PWR ', as.character(format(uniqueDelmos[1], "%b%y"))), alpha = 0.5) %>%
      config(displayModeBar = FALSE)
    
    for (i in 2:length(uniqueDelmos)){
      p2 <- add_histogram(p2, distData[Component != 'NG' & Delmo == uniqueDelmos[i] & Segment == 'pkPrice', Price], 
                          type = 'histogram', 
                          name = paste0('PK PWR ', as.character(format(uniqueDelmos[i], "%b%y")))) %>% 
        layout(barmode = 'overlay', legend = list(orientation = 'h'))
    }
    
    p3 <- plot_ly(distData[Component != 'NG' & Delmo == uniqueDelmos[1] & Segment == 'opPrice', ],
                  x = ~ Price, type = 'histogram', 
                  name = paste0('OffPk PWR ', as.character(format(uniqueDelmos[1], "%b%y"))), alpha = 0.5) %>%
      config(displayModeBar = FALSE)
    
    for (i in 2:length(uniqueDelmos)){
      p3 <- add_histogram(p3, distData[Component != 'NG' & Delmo == uniqueDelmos[i] & Segment == 'opPrice', Price], 
                          type = 'histogram', 
                          name = paste0('OffPk PWR ', as.character(format(uniqueDelmos[i], "%b%y")))) %>% 
        layout(barmode = 'overlay', legend = list(orientation = 'h'))
    }
    
    isolate(

      subplot(p1, p2, p3, nrows = 3, margin = 0.05)
      
      # ggplot(terminalDist(), aes(x = Price)) + geom_histogram() +
      #   geom_vline(aes(xintercept = meanPrice), color = 'red', linetype = 'dashed') +
      #   facet_grid(. ~ Component + Segment, scales = 'free_x') +
      #   ggtitle('Distribution of terminal prices (last forward month) w/ mean')
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
      pricePercentile <- as.data.table(simOutput())
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
      write.csv(simOutput(), file2, row.names = FALSE)
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
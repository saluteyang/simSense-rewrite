library(shiny)
library(aligne)
library(ggplot2)
library(scales)
library(data.table)
library(stringr)

# simWrapper <- function(nsims, curve.date.begin, curve.date.end, start_date, 
#                        end_date, market, component){
#   
# 
# }

shinyServer(function(input, output, session){

  simOutput <- reactive({
    
    if(input$goButton1 == 0)
      return()
    
    nsims <- input$numsimslider
    # period of forward prices used
    curve.date.begin <- as.Date(input$curvedaterange[1]) # based on availability, may not all be available
    curve.date.end <- as.Date(input$curvedaterange[2]) # make sure this is not a weekend/holiday
    # start and end date of spot dates
    start_date <- as.Date(input$simrangemonth[1])
    end_date <- as.Date(input$simrangemonth[2])
    
    market <- input$mkt
    component <- input$curvelist
    
    ### end of input variables #####
    
    numofmonth <- (as.yearmon(end_date) - as.yearmon(start_date))*12 + 1
    # first forward month
    first.month <- start_date
    # forward delivery months included
    month.used <- seq(as.Date(first.month), by = "month", length.out = numofmonth - 1)
    
    # date matrix
    out <- expandDates(start_date, end_date)$time.long
    out.days <- expandDates(start_date, end_date)$time.days
    period.pre <- as.numeric(as.Date(first.month) - as.Date(curve.date.end))/365
    period.pk.inter <- cumsum(out.days$numPkDays)
    period.rtc.inter <- cumsum(out.days$numTotDays)
    # day count convention to be consistent with option quotes
    period.pk <- c(period.pre, period.rtc.inter/365 + period.pre)
    period.pk <- period.pk[-length(period.pk)] # time increments to forward steps
    
    ## price generation process starts here #######
    set.seed(123)
    
    # pull forward power
    pwr.curves <- futures.pwr.multi.rng.w(curve.date.begin, curve.date.end,
                                          first.month, month.used[numofmonth],
                                          list(market),
                                          list(component))
    
    pwr.curves <- melt(pwr.curves, id.vars = c('Date', 'Market', 'Component', 'Delmo'), 
                       variable.name = 'Segment', value.name = 'Price')
    pwr.curves <- dplyr::filter(pwr.curves, Segment != 'rtcPrice')
    
    ng.curve <- futures.ng.rng(curve.date.begin, curve.date.end,
                               first.month, month.used[numofmonth])
    ng.curve <- mutate(rename(ng.curve, c('NG' = 'rtcPrice')),
                       Market = "NYMEX", Component = "NG")
    ng.curve <- melt(ng.curve, id.vars = c('Date', 'Market', 'Component', 'Delmo'), variable.name = 'Segment', value.name = 'Price')
    
    # coal.curve <- futures.fuel.rng(curve.date.begin, curve.date.end, first.month, month.used[numofmonth], 'PRB', 'BU8408')
    # coal.curve <- mutate(coal.curve, Market = 'PRB', Component = 'BU8408', Segment = 'rtcPrice')
    
    curves.comb <- rbind.data.frame(pwr.curves, ng.curve) # coal.curve
    curves.comb$Component <- trimws(curves.comb$Component)
    curves.comb$Market <- NULL
    
    # make delmo as subscript of distinct commodities
    curves.comb.pivot <- dcast(curves.comb, Date~Component + Delmo + Segment, value.var = 'Price') # dcast sorts columns lexically
    curves.comb.pivot <- cbind(dplyr::select(curves.comb.pivot[, order(colnames(curves.comb.pivot))], Date),
                               dplyr::select(curves.comb.pivot[, order(colnames(curves.comb.pivot))], -Date))
    curves.comb.pivot <- curves.comb.pivot[complete.cases(curves.comb.pivot),]
    curves.comb.ret <- log(curves.comb.pivot[-1,-1]/curves.comb.pivot[-dim(curves.comb.pivot)[1],-1])
    rownames(curves.comb.ret) <- curves.comb.pivot$Date[-1]
    curves.comb.cor <- cor(curves.comb.ret)
    
    price.fwd <- curves.comb.pivot[which(curves.comb.pivot$Date == curve.date.end), -1]
    vol.fwd <- monthlyPkVol.multi(curve.date.end, month.used[numofmonth],
                                  list(market, 'NYMEX'),
                                  list(component,'NG'))
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
                                               as.data.frame(str_match(sim.prices.long$NewComponent, "^(.*)_(.*)_(.*)$")[, -1])), # regular expression
                              c('V1' = 'Component', 'V2' = 'Delmo', 'V3' = 'Segment'))
    sim.prices.long <- mutate(sim.prices.long,
                              Delmo = as.Date(Delmo),
                              Monthsout = as.Date(Monthsout),
                              NewComponent = NULL)
    sim.prices.final.long <- subset(sim.prices.long, Delmo == Monthsout) # filter out expired contracts
    return(sim.prices.final.long)
    })

  selectPaths <- reactive({
    if(input$goButton1 == 0)
      return()
    
    sim.prices.long.select <- as.data.table(simOutput())
    sim.prices.long.select <- sim.prices.long.select[, .SD[1:50], by = list(Component, Delmo, Segment)]
    return(sim.prices.long.select)
    
  })
  
  output$pricePlot <- renderPlot({
    
    if(input$goButton1 == 0)
      return()

    isolate(
      ggplot(selectPaths(), aes(x = Delmo, y = Price, group = SimNo, color = SimNo)) + 
      geom_line() + facet_grid(Component + Segment ~., scales = 'free_y') + theme(legend.position = 'none')
    )
  })
  
  pctileSummary <- reactive({
    if(input$tblout){
      pricePercentile <- as.data.table(simOutput())
      pricePercentile <- pricePercentile[, list(`5th` = quantile(Price, .05),
                                                `10th` = quantile(Price, .1),
                                                `50th` = quantile(Price, 0.5),
                                                `95th` = quantile(Price, 0.9),
                                                `99th`= quantile(Price, 0.99)),
                                         by = c('Component', 'Delmo', 'Segment')]
      return(pricePercentile)
    }
  })
  
  output$pctileTbl <- renderDataTable({
    input$goButton1
    isolate(
      pctileSummary()
    )
  }, options = list(searching = FALSE,
                    lengthChange = FALSE)) 
    
  observe({
    mkt.curvelist <- switch(input$mkt,
                            ERCOT = c('ZONE N', 'ZONE H'),
                            PJM = c ('WESTRT'))
    updateCheckboxGroupInput(session, "curvelist", choices = mkt.curvelist)
  })
  
})
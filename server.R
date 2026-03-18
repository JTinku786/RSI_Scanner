server <- function(input, output) {
  

  getRSIInfo <- function(stock,timeframe){
    
    if(timeframe == "1m"){
      response_data <- fromJSON(paste("https://query1.finance.yahoo.com/v8/finance/chart/",stock,"?region=IN&lang=en-IN&includePrePost=false&interval=1m&range=1d&corsDomain=in.finance.yahoo.com&.tsrc=financet", sep=""))
    }
    else if(timeframe == "2m"){
      response_data <- fromJSON(paste("https://query1.finance.yahoo.com/v8/finance/chart/",stock,"?region=IN&lang=en-IN&includePrePost=false&interval=2m&range=1d&corsDomain=in.finance.yahoo.com&.tsrc=financet", sep=""))
    }
    else if(timeframe == "5m"){
      response_data <- fromJSON(paste("https://query1.finance.yahoo.com/v8/finance/chart/",stock,"?region=IN&lang=en-IN&includePrePost=false&interval=5m&range=2d&corsDomain=in.finance.yahoo.com&.tsrc=financet", sep=""))
    }
    else if(timeframe == "15m"){
      # response_data <- fromJSON(paste("https://query1.finance.yahoo.com/v8/finance/chart/",stock,"?region=IN&lang=en-IN&includePrePost=false&interval=15m&range=60d&corsDomain=in.finance.yahoo.com&.tsrc=financet", sep=""))
      response_data <- fromJSON(paste("https://query1.finance.yahoo.com/v8/finance/chart/",stock,"?region=IN&lang=en-IN&includePrePost=false&interval=15m&range=5d&corsDomain=in.finance.yahoo.com&.tsrc=financet", sep=""))
    }
    else if(timeframe == "1h"){
      response_data <- fromJSON(paste("https://query1.finance.yahoo.com/v8/finance/chart/",stock,"?region=IN&lang=en-IN&includePrePost=false&interval=1h&range=60d&corsDomain=in.finance.yahoo.com&.tsrc=financet", sep=""))
    }
    else if(timeframe == "4h"){
      response_data <- fromJSON(paste("https://query1.finance.yahoo.com/v8/finance/chart/",stock,"?region=IN&lang=en-IN&includePrePost=false&interval=4h&range=15d&corsDomain=in.finance.yahoo.com&.tsrc=financet", sep=""))
    }
    else if(timeframe == "1d"){
      response_data <- fromJSON(paste("https://query1.finance.yahoo.com/v8/finance/chart/",stock,"?region=IN&lang=en-IN&includePrePost=false&interval=1d&range=4mo&corsDomain=in.finance.yahoo.com&.tsrc=financet", sep=""))
    }
    else if(timeframe == "1wk"){
      response_data <- fromJSON(paste("https://query1.finance.yahoo.com/v8/finance/chart/",stock,"?region=IN&lang=en-IN&includePrePost=false&interval=1wk&range=36mo&corsDomain=in.finance.yahoo.com&.tsrc=financet", sep=""))
    }
    else if(timeframe == "1mo"){
      response_data <- fromJSON(paste("https://query1.finance.yahoo.com/v8/finance/chart/",stock,"?region=IN&lang=en-IN&includePrePost=false&interval=1mo&range=36mo&corsDomain=in.finance.yahoo.com&.tsrc=financet", sep=""))
    }
    
    
    stock_timestamp <- response_data$chart$result[[1]]$timestamp
    Close <- response_data$chart$result[[1]]$indicators$quote[[1]]$close
    High <- response_data$chart$result[[1]]$indicators$quote[[1]]$high
    Low <- response_data$chart$result[[1]]$indicators$quote[[1]]$low
    Open <- response_data$chart$result[[1]]$indicators$quote[[1]]$open
    Volume <- response_data$chart$result[[1]]$indicators$quote[[1]]$volume
    final_data <- as.data.frame(cbind(as.POSIXct(stock_timestamp, origin="1970-01-01"),Close,High,Low,Open,Volume))
    
    # browser()
    
    if(nrow(final_data) == 0){
      stock_timestamp <- response_data$chart$result[[2]][[1]]
      Close <- response_data$chart$result[[3]]$quote[[1]]$close
      High <- response_data$chart$result[[3]]$quote[[1]]$high
      Low <- response_data$chart$result[[3]]$quote[[1]]$low
      Open <- response_data$chart$result[[3]]$quote[[1]]$open
      Volume <- response_data$chart$result[[3]]$quote[[1]]$volume
      
      
      final_data <- as.data.frame(cbind(as.POSIXct(stock_timestamp, origin="1970-01-01"),as.numeric(unlist(Close)),as.numeric(unlist(High)),as.numeric(unlist(Low)),as.numeric(unlist(Open)),as.numeric(unlist(Volume))))
    }
    
    
    colnames(final_data) <- c("V1","Close","High","Low","Open","Volume")
    
    if(typeof(final_data$V1) == "list"){
      final_data <- final_data[-c(which(final_data$Close == "NULL")),]
      new_stock_timestamp <- unlist(final_data$V1)
      Close <- unlist(final_data$Close)
      High <- unlist(final_data$High)
      Open <- unlist(final_data$Open)
      Low <- unlist(final_data$Low)
      Volume <- unlist(final_data$Volume)
      
      final_data <- data.frame(new_stock_timestamp,Close,High,Low,Open,Volume)
      
      final_data$Date <- as.POSIXct(final_data$new_stock_timestamp, origin="1970-01-01")
      
      final_data <- final_data %>% select(Date, Open, High, Low, Close,Volume)
    }else{
      final_data$Date <- as.POSIXct(final_data$V1, origin="1970-01-01")
      
      final_data <- final_data %>% select(Date, Open, High, Low, Close,Volume)
    }
    final_data <- na.omit(final_data)
    
    
    # browser()
    
    data <- as.data.frame(final_data)
    
    names(data) <- c("dates", "Open", "High", "Low", "Close", "Volume")
    
    # Clean data frame and calculate RSI indicators
    xbt <- data %>% 
      select(dates, Open, High, Low, Close, Volume) %>%
      mutate(
        rsi = TTR::RSI(Close, 14)
      )
    
    return(xbt)
    
    
  }
  
  observeEvent(input$bot_submit,{
    
    output$rsi_buyer <- DT::renderDataTable({
      
      stocks_df <- read.csv(paste0(getwd(),"/Fut_Opt_Stocks.csv", sep = ""))
      
      filtered_data = data.frame()
      
      current_row = 0
      
      withProgress(message = 'Running for Futures and Options : ', value = 0, {
       
        n_rows = nrow(stocks_df)
        # n_rows = 20
      
      for (i in 1:n_rows) {
        stock <- stocks_df[i,2]
        
        print(stock)
        
        incProgress(1/n_rows, detail = paste("Scanning for ", stock))
        
        xbt_1 <- getRSIInfo(stock,input$bot_timeframe_1)
        xbt_2 <- getRSIInfo(stock,input$bot_timeframe_2)
        xbt_3 <- getRSIInfo(stock,input$bot_timeframe_3)
        
        ### Check if the latest candle of the selected times crossing the limits
        if((tail(xbt_1,1)$rsi >= as.numeric(input$upper_limit)) &&  (tail(xbt_2,1)$rsi >= as.numeric(input$upper_limit)) && (tail(xbt_3,1)$rsi >= as.numeric(input$upper_limit))){
          
          ## Buy signal should trigger
          
          current_row = nrow(filtered_data) + 1
          
          filtered_data[current_row,"Stock"] = stock
          filtered_data[current_row,"Signal"] = "Buy"
          filtered_data[current_row,"TimeFrame_1_RSI"] = round(as.numeric(tail(xbt_1,1)$rsi),2)
          filtered_data[current_row,"TimeFrame_2_RSI"] = round(as.numeric(tail(xbt_2,1)$rsi),2)
          filtered_data[current_row,"TimeFrame_3_RSI"] = round(as.numeric(tail(xbt_3,1)$rsi),2)
          
          
        }else if((tail(xbt_1,1)$rsi <= as.numeric(input$lower_limit)) &&  (tail(xbt_2,1)$rsi <= as.numeric(input$lower_limit)) && (tail(xbt_3,1)$rsi <= as.numeric(input$lower_limit))){
          
          ## Sell trigger should trigger
          
          current_row = nrow(filtered_data) + 1
          
          filtered_data[current_row,"Stock"] = stock
          filtered_data[current_row,"Signal"] = "Sell"
          filtered_data[current_row,"TimeFrame_1_RSI"] = round(as.numeric(tail(xbt_1,1)$rsi),2)
          filtered_data[current_row,"TimeFrame_2_RSI"] = round(as.numeric(tail(xbt_2,1)$rsi),2)
          filtered_data[current_row,"TimeFrame_3_RSI"] = round(as.numeric(tail(xbt_3,1)$rsi),2)
          
        }
        # browser()
      }
      })
      
      write.csv(filtered_data,paste0(getwd(),"/rsi_breakouts.csv", sep = ""))
      
      DT::datatable(filtered_data)
      
      
    })
    
    output$rsi_breakout <- DT::renderDataTable({
      
      breakout_df = data.frame()
      
      stock_row <- input$rsi_buyer_rows_selected
      
      current_zones <- read.csv(paste0(getwd(),"/rsi_breakouts.csv", sep = ""))
      
      current_zones <- subset(current_zones, select = -c(X) )
      
      stock <- current_zones[stock_row,"Stock"]
      
      xbt_1 <- getRSIInfo(stock,input$bot_timeframe_1)
      xbt_2 <- getRSIInfo(stock,input$bot_timeframe_2)
      xbt_3 <- getRSIInfo(stock,input$bot_timeframe_3)
      
      signal <- current_zones[stock_row,"Signal"]
      
      if(signal == "Buy"){
        
        xbt_1 <- xbt_1[order(xbt_1$dates, decreasing = TRUE), ]
        
        rownames(xbt_1) <- NULL
        
        xbt_1$break_out <- ifelse(xbt_1$rsi >= 60 , 1, -1)
        xbt_1 <- mutate(xbt_1, next_rsi_breakout = lead(break_out))
        
        xbt_1$prev_rsi_check <- ifelse(xbt_1$break_out != xbt_1$next_rsi_breakout,0,1)
        
        latest_break_out_1 <- head(xbt_1[xbt_1$prev_rsi_check == 0,],1)
        
        
        xbt_2 <- xbt_2[order(xbt_2$dates, decreasing = TRUE), ]
        
        rownames(xbt_2) <- NULL
        
        xbt_2$break_out <- ifelse(xbt_2$rsi >= 60 , 1, -1)
        xbt_2 <- mutate(xbt_2, next_rsi_breakout = lead(break_out))
        
        xbt_2$prev_rsi_check <- ifelse(xbt_2$break_out != xbt_2$next_rsi_breakout,0,1)
        
        latest_break_out_2 <- head(xbt_2[xbt_2$prev_rsi_check == 0,],1)
        
        xbt_3 <- xbt_3[order(xbt_3$dates, decreasing = TRUE), ]
        
        rownames(xbt_3) <- NULL
        
        xbt_3$break_out <- ifelse(xbt_3$rsi >= 60 , 1, -1)
        xbt_3 <- mutate(xbt_3, next_rsi_breakout = lead(break_out))
        
        xbt_3$prev_rsi_check <- ifelse(xbt_3$break_out != xbt_3$next_rsi_breakout,0,1)
        
        latest_break_out_3 <- head(xbt_3[xbt_3$prev_rsi_check == 0,],1)
        
        breakout_df[1,"Stock"] <- stock
        breakout_df[1,"Datetime"] <- latest_break_out_1$dates
        breakout_df[1,"TimeFrame"] <- "Timeframe1"
        breakout_df[1,"Open"] <- round(latest_break_out_1$Open,2)
        breakout_df[1,"High"] <- round(latest_break_out_1$High,2)
        breakout_df[1,"Low"] <- round(latest_break_out_1$Low,2)
        breakout_df[1,"Close"] <- round(latest_break_out_1$Close,2)
        breakout_df[1,"RSI"] <- round(latest_break_out_1$rsi,2)
        
        breakout_df[2,"Stock"] <- stock
        breakout_df[2,"Datetime"] <- latest_break_out_2$dates
        breakout_df[2,"TimeFrame"] <- "Timeframe2"
        breakout_df[2,"Open"] <- round(latest_break_out_2$Open,2)
        breakout_df[2,"High"] <- round(latest_break_out_2$High,2)
        breakout_df[2,"Low"] <- round(latest_break_out_2$Low,2)
        breakout_df[2,"Close"] <- round(latest_break_out_2$Close,2)
        breakout_df[2,"RSI"] <- round(latest_break_out_2$rsi,2)
        
        breakout_df[3,"Stock"] <- stock
        breakout_df[3,"Datetime"] <- latest_break_out_3$dates
        breakout_df[3,"TimeFrame"] <- "Timeframe3"
        breakout_df[3,"Open"] <- round(latest_break_out_3$Open,2)
        breakout_df[3,"High"] <- round(latest_break_out_3$High,2)
        breakout_df[3,"Low"] <- round(latest_break_out_3$Low,2)
        breakout_df[3,"Close"] <- round(latest_break_out_3$Close,2)
        breakout_df[3,"RSI"] <- round(latest_break_out_3$rsi,2)

        
      }else{
        
        xbt_1 <- xbt_1[order(xbt_1$dates, decreasing = TRUE), ]
        
        rownames(xbt_1) <- NULL
        
        xbt_1$break_out <- ifelse(xbt_1$rsi <= 40 , 1, -1)
        xbt_1 <- mutate(xbt_1, next_rsi_breakout = lead(break_out))
        
        xbt_1$prev_rsi_check <- ifelse(xbt_1$break_out != xbt_1$next_rsi_breakout,0,1)
        
        latest_break_out_1 <- head(xbt_1[xbt_1$prev_rsi_check == 0,],1)
        
        
        xbt_2 <- xbt_2[order(xbt_2$dates, decreasing = TRUE), ]
        
        rownames(xbt_2) <- NULL
        
        xbt_2$break_out <- ifelse(xbt_2$rsi <= 40 , 1, -1)
        xbt_2 <- mutate(xbt_2, next_rsi_breakout = lead(break_out))
        
        xbt_2$prev_rsi_check <- ifelse(xbt_2$break_out != xbt_2$next_rsi_breakout,0,1)
        
        latest_break_out_2 <- head(xbt_2[xbt_2$prev_rsi_check == 0,],1)
        
        xbt_3 <- xbt_3[order(xbt_3$dates, decreasing = TRUE), ]
        
        rownames(xbt_3) <- NULL
        
        xbt_3$break_out <- ifelse(xbt_3$rsi <= 40 , 1, -1)
        xbt_3 <- mutate(xbt_3, next_rsi_breakout = lead(break_out))
        
        xbt_3$prev_rsi_check <- ifelse(xbt_3$break_out != xbt_3$next_rsi_breakout,0,1)
        
        latest_break_out_3 <- head(xbt_3[xbt_3$prev_rsi_check == 0,],1)
        
        breakout_df[1,"Stock"] <- stock
        breakout_df[1,"Datetime"] <- latest_break_out_1$dates
        breakout_df[1,"TimeFrame"] <- "Timeframe1"
        breakout_df[1,"Open"] <- round(latest_break_out_1$Open,2)
        breakout_df[1,"High"] <- round(latest_break_out_1$High,2)
        breakout_df[1,"Low"] <- round(latest_break_out_1$Low,2)
        breakout_df[1,"Close"] <- round(latest_break_out_1$Close,2)
        breakout_df[1,"RSI"] <- round(latest_break_out_1$rsi,2)
        
        breakout_df[2,"Stock"] <- stock
        breakout_df[3,"Datetime"] <- latest_break_out_2$dates
        breakout_df[2,"TimeFrame"] <- "Timeframe2"
        breakout_df[2,"Open"] <- round(latest_break_out_2$Open,2)
        breakout_df[2,"High"] <- round(latest_break_out_2$High,2)
        breakout_df[2,"Low"] <- round(latest_break_out_2$Low,2)
        breakout_df[2,"Close"] <- round(latest_break_out_2$Close,2)
        breakout_df[2,"RSI"] <- round(latest_break_out_2$rsi,2)
        
        breakout_df[3,"Stock"] <- stock
        breakout_df[3,"Datetime"] <- latest_break_out_3$dates
        breakout_df[3,"TimeFrame"] <- "Timeframe3"
        breakout_df[3,"Open"] <- round(latest_break_out_3$Open,2)
        breakout_df[3,"High"] <- round(latest_break_out_3$High,2)
        breakout_df[3,"Low"] <- round(latest_break_out_3$Low,2)
        breakout_df[3,"Close"] <- round(latest_break_out_3$Close,2)
        breakout_df[3,"RSI"] <- round(latest_break_out_3$rsi,2)
        
      }
      
      # browser()
      
      breakout_df$Datetime <- breakout_df$Datetime + hm("5:30") 
      
      
      DT::datatable(breakout_df)
          
      
      
    })
    
    
    output$potential_rsi <- DT::renderDataTable({
      
      stocks_df <- read.csv(paste0(getwd(),"/Fut_Opt_Stocks.csv", sep = ""))
      
      filtered_data = data.frame()
      
      current_row = 0
      
      withProgress(message = 'Running for Futures and Options : ', value = 0, {
        
        n_rows = nrow(stocks_df)
        # n_rows = 5
        
        for (i in 1:n_rows) {
          stock <- stocks_df[i,2]
          
          print(stock)
          
          incProgress(1/n_rows, detail = paste("Scanning for ", stock))
          
          xbt_1 <- getRSIInfo(stock,input$bot_timeframe_1)
          xbt_2 <- getRSIInfo(stock,input$bot_timeframe_2)
          xbt_3 <- getRSIInfo(stock,input$bot_timeframe_3)
          
          if((tail(xbt_1,1)$rsi >= as.numeric(input$upper_limit)) &&  (tail(xbt_2,1)$rsi >= as.numeric(input$upper_limit)) && (tail(xbt_3,1)$rsi >= as.numeric(input$upper_limit))){
           print("buy side") 
          }else if ((tail(xbt_1,1)$rsi <= as.numeric(input$lower_limit)) &&  (tail(xbt_2,1)$rsi <= as.numeric(input$lower_limit)) && (tail(xbt_3,1)$rsi <= as.numeric(input$lower_limit))){
            print("sell side")
          }else{
            if(((tail(xbt_1,1)$rsi >= as.numeric(input$upper_limit)) &&  (tail(xbt_2,1)$rsi >= as.numeric(input$upper_limit))) || ((tail(xbt_2,1)$rsi >= as.numeric(input$upper_limit)) &&  (tail(xbt_3,1)$rsi >= as.numeric(input$upper_limit))) ||  ((tail(xbt_3,1)$rsi >= as.numeric(input$upper_limit)) &&  (tail(xbt_1,1)$rsi >= as.numeric(input$upper_limit)))){
              
              current_row = nrow(filtered_data) + 1
              
              filtered_data[current_row,"Stock"] = stock
              filtered_data[current_row,"Signal"] = "Potential Buy"
              filtered_data[current_row,"TimeFrame_1_RSI"] = round(as.numeric(tail(xbt_1,1)$rsi),2)
              filtered_data[current_row,"TimeFrame_2_RSI"] = round(as.numeric(tail(xbt_2,1)$rsi),2)
              filtered_data[current_row,"TimeFrame_3_RSI"] = round(as.numeric(tail(xbt_3,1)$rsi),2)
              
            }else if(((tail(xbt_1,1)$rsi <= as.numeric(input$lower_limit)) &&  (tail(xbt_2,1)$rsi <= as.numeric(input$lower_limit))) || ((tail(xbt_2,1)$rsi <= as.numeric(input$lower_limit)) &&  (tail(xbt_3,1)$rsi <= as.numeric(input$lower_limit))) ||  ((tail(xbt_3,1)$rsi <= as.numeric(input$lower_limit)) &&  (tail(xbt_1,1)$rsi <= as.numeric(input$lower_limit)))){
              
              current_row = nrow(filtered_data) + 1
              
              filtered_data[current_row,"Stock"] = stock
              filtered_data[current_row,"Signal"] = "Potential Sell"
              filtered_data[current_row,"TimeFrame_1_RSI"] = round(as.numeric(tail(xbt_1,1)$rsi),2)
              filtered_data[current_row,"TimeFrame_2_RSI"] = round(as.numeric(tail(xbt_2,1)$rsi),2)
              filtered_data[current_row,"TimeFrame_3_RSI"] = round(as.numeric(tail(xbt_3,1)$rsi),2)
              
            }
          }
          
        }
      })
      DT::datatable(filtered_data)
      
      
    })
    
    
  })
  
  
  output$slickr <- renderSlickR({
    # browser()
    folder <- paste(getwd(),"/images" ,sep = "")
    
    imgs <- list.files(folder, pattern=".png", full.names = TRUE)
    slickR(imgs)
  })
  
  # contact notice
  output$follow<- renderText({ 
    # browser()
    HTML(paste0("<b> Feel free to reach out to me via below social platforms or write at saitejareddy123@gmail.com with any queries/comments on the dashboard!!</b>"))
  })
  
  output$Disclaimer<- renderText({ 
    HTML(paste0("<b> Disclaimer : This dashboard is purely built for education purpose only!!</b>"))
  })
}


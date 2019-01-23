source("format.R")
require(tools)
require(plotly)
require(lubridate)
require(readr) # guess encoding

options(shiny.maxRequestSize = 90*1024^2)


################################
###       FUNCTIONS         ####
################################

isValidPath<-function(stringPath){
  if(!is.null(stringPath))# & !is.na(stringPath) & stringPath!='' & file.exists(stringPath))
    return(T)
  return(F)
}



emptyTableValue <- as.tbl(as.data.frame("Please select a file to import"))


timestampToLocalDate<-function(timestamp){
  as_datetime(as.numeric(timestamp), tz="Europe/Paris")
  #as.character(timestamp)
}

renderTextFunc<-function(name, data){
  txt<-paste0(name,":")
  if(dim(data)[1]>1)
    txt<-paste(txt, dim(data)[1], "lines between (Paris hours):  ",timestampToLocalDate(data$timestamp[1]), "  and  ",timestampToLocalDate(last(data$timestamp[!is.na(data$timestamp)])))
  else
    txt<-paste(txt, "no data.")
  return(txt)
}


colDiplayChoices<-c("lng","lat","slope","curveRadius","speed","alt","accX","accY","accZ","accXSI","accYSI","accZSI")



#########################
###       UI         ####
#########################

ui <- fluidPage(mainPanel(
  tabsetPanel(
    tabPanel(title = "RT 1000",
             fileInput("file_rt1000", "RT 1000",multiple = FALSE,accept = c("text/csv","text/comma-separated-values,text/plain",".csv",".zip")),
             verbatimTextOutput("encoding"),
             dataTableOutput(outputId = "rt1000")
    ),
    tabPanel(title = "DWILEN 1",
             fileInput("file_dw1", "DWILEN 1",multiple = FALSE,accept = c("text/csv","text/commaseparated-values,text/plain",".csv",".zip")),
             dataTableOutput(outputId = "dw1")
    ),
    tabPanel(title = "DWILEN 2",
             fileInput("file_dw2", "DWILEN 2",multiple = FALSE,accept = c("text/csv","text/comma-separated-values,text/plain",".csv",".zip")),
             fileInput("file_dw2_raw", ". TRP",multiple = FALSE,accept = c(".trp")),
             dataTableOutput(outputId = "dw2")
    ),
    tabPanel(title = "DWILEN BRUT",
             fileInput("file_dwb", "DWILEN BRUT",multiple = FALSE,accept = c("text/csv","text/comma-separated-values,text/plain",".csv",".zip")),
             fileInput("file_dwb_trame", ".TRAME",multiple = FALSE,accept = c(".trame")),
             dataTableOutput(outputId = "dwb")
    ),
    tabPanel(title = "Parameter and visualize",
             h2("Infos"),
             h3("Time"),
             textOutput("rt1000_text"),
             textOutput("dw1_text"),
             textOutput("dw2_text"),
             textOutput("dwb_text"),
             h3("Shift"),
             htmlOutput("shift_all"),
             h2("Parameter"),
             checkboxInput("unshift_gps", "Add unshifted timestamp column for GPS", TRUE),
             htmlOutput("shift_gps_text"),
             checkboxInput("unshift_acc", "Add unshifted timestamp column for Accel", TRUE),
             htmlOutput("shift_acc_text"),
             h2("Visualize"),
             h3("RT accels filtered with a butterworth low-pass:"),
             numericInput("filter_freq", "Filter frequency:", 0.2),
             numericInput("filter_order", "Filter order:", 6),
             h3("Plot 1:"),
             selectInput("select1", "Select columns to display", colDiplayChoices, selected="speed", multiple = F),
             plotlyOutput("plot1"),
             h3("Plot 2:"),
             selectInput("select2", "Select columns to display", colDiplayChoices, selected="speed", multiple = F),
             plotlyOutput("plot2")
    ),
    tabPanel(title = "Export",
             h3("RT1000:"),
             downloadButton("download_rt_1000", "Download RT1000"),
             h3("DW 1:"),
             downloadButton("download_dw_1_cut", "Download DW1 correspondant à la RT"),
             downloadButton("download_dw_1", "Download DW1 complet"),
             h3("DW 2:"),
             downloadButton("download_dw_2_cut", "Download DW2 correspondant à la RT"),
             downloadButton("download_dw_2", "Download DW2 complet"),
             h3("DW BRUT:"),
             downloadButton("download_dwb_cut", "Download DW BRUT correspondant à la RT"),
             downloadButton("download_dwb", "Download DW BRUT complet")
    )
  )
))


#############################
###       SERVER         ####
#############################


server<-function(input, output, session) {
  
  readTestAndFormatCSV<-function(path, csvTypeExpected, dec=NA){
    # Handle zip file
    if(tolower(file_ext(path))=="zip")
    {
      path<-unzip(path)
    }
    message(paste("Reading",path))
    # Get separator
    if(is.na(dec))
      dec <- getSeparator(path)
    encod <- guess_encoding(path, n_max = 10000)[[1]][1]
    brut <- read.csv2(path, dec=dec, flush=T , fileEncoding = encod, encoding = encod)
    csvType <- getCSVType(brut)
    if(csvType!=csvTypeExpected)
      return(as.tbl(as.data.frame(paste("Invalid format (recognize:", csvType, "instead of ", csvTypeExpected,") (encoding:",guess_encoding(path, n_max = 1000)[[1]][1],")","colnames: ", toString(colnames(brut))))))
    # Update encoding display
    encoding$encod <- encod
    encoding$dec <- dec
    return(cleanCSV(brut))
  }
  
  encoding <- reactiveValues(encod="", dec="")
  
  data_rt1000 <- reactive({
    df_rt <- emptyTableValue
    if(isValidPath(input$file_rt1000$datapath)){
      df_rt<-readTestAndFormatCSV(input$file_rt1000$datapath, csvTypeExpected="RT")
      if(input$unshift_gps)
        df_rt$timestampShiftedGPS <- df_rt$timestamp
      if(input$unshift_acc)
        df_rt$timestampShiftedAcc <- df_rt$timestamp
    }
    df_rt
  })
  
  data_dw1 <- reactive({
    df_dw1 <- emptyTableValue
    if(isValidPath(input$file_dw1$datapath)){
      df_dw1<-readTestAndFormatCSV(input$file_dw1$datapath, csvTypeExpected="DW1")
    }
    df_dw1
  })
  
  data_dw2 <- reactive({
    df_dw2 <- emptyTableValue
    if(isValidPath(input$file_dw2$datapath)){
      df_dw2<-readTestAndFormatCSV(input$file_dw2$datapath, csvTypeExpected="DW2")
    }
    
    if(isValidPath(input$file_dw2_raw$datapath) & nrow(df_dw2)>1){
      pathname <- input$file_dw2_raw$datapath
      
      fileSize <- file.info(pathname)$size;
      raw <- readBin(pathname, what="raw", n=fileSize)
      
      time_to_add <- getDiffTime(raw, fileSize)
      
      df_dw2 %>% mutate(timestamp=timestamp+time_to_add)
    }
    df_dw2
    
  })
  
  data_dwb <- reactive({
    df_dwb <- emptyTableValue
    if(isValidPath(input$file_dwb$datapath)){
      df_dwb<-readTestAndFormatCSV(input$file_dwb$datapath, csvTypeExpected="DWB")
    }
    
    if(isValidPath(input$file_dwb_trame$datapath) & nrow(df_dwb)>1){
      info_dwb <- getInfoDWB(input$file_dwb_trame$datapath)
      
      df_dwb %>% mutate(
        timestamp = timestamp+info_dwb$shift,
        euler_angle_1 = info_dwb$euler_angle_1,
        euler_angle_2 = info_dwb$euler_angle_2,
        euler_angle_3 = info_dwb$euler_angle_3
      )
    }
    df_dwb
    
  })
  
  shift_dw1_gps <- reactive({
    shift_dw1_gps<-NA
    if(dim(data_dw1())[1]>1 & dim(data_rt1000())[1]>1)
      shift_dw1_gps<-getShift(RT=data_rt1000(), DW=data_dw1(), col_name = "speed")
    shift_dw1_gps
  })
  
  shift_dw2_gps <- reactive({
    shift_dw2_gps<-NA
    if(dim(data_dw2())[1]>1 & dim(data_rt1000())[1]>1)
      shift_dw2_gps<-getShift(RT=data_rt1000(), DW=data_dw2(), col_name = "speed")
    shift_dw2_gps
  })
  
  shift_dwb_gps <- reactive({
    shift_dwb_gps<-NA
    if(dim(data_dwb())[1]>1 & dim(data_rt1000())[1]>1)
      shift_dwb_gps<-getShift(RT=data_rt1000(), DW=data_dwb(), col_name = "speed")
    shift_dwb_gps
  })
  
  shift_dw1_acc <- reactive({
    shift_dw1_gps<-NA
    if(dim(data_dw1())[1]>1 & dim(data_rt1000())[1]>1)
      shift_dw1_gps<-getShift(RT=data_rt1000(), DW=data_dw1(), col_name = "accX")
    shift_dw1_gps
  })
  
  shift_dw2_acc <- reactive({
    shift_dw2_gps<-NA
    if(dim(data_dw2())[1]>1 & dim(data_rt1000())[1]>1)
      shift_dw2_gps<-getShift(RT=data_rt1000(), DW=data_dw2(), col_name = "accX")
    shift_dw2_gps
  })
  
  shift_dwb_acc <- reactive({
    shift_dwb_gps<-NA
    if(dim(data_dwb())[1]>1 & dim(data_rt1000())[1]>1)
      shift_dwb_gps<-getShift(RT=data_rt1000(), DW=data_dwb(), col_name = "accX")
    shift_dwb_gps
  })
  
  data_dw1_cut <- reactive({
    df_dw1_cut<-emptyTableValue
    if(dim(data_dw1())[1]>1 & dim(data_rt1000())[1]>1){
      df_dw1 <- data_dw1()
      col_to_cut <- c("timestamp")
      if(input$unshift_gps){
        df_dw1 <- addShiftedColumnDW(DW = df_dw1, shift = shift_dw1_gps(), shiftName = "timestampShiftedGPS")
        col_to_cut<-c(col_to_cut, "timestampShiftedGPS")
      }
      if(input$unshift_acc){
        df_dw1 <- addShiftedColumnDW(DW = df_dw1, shift = shift_dw1_acc(), shiftName = "timestampShiftedAcc")
        col_to_cut<-c(col_to_cut, "timestampShiftedAcc")
      }
      df_dw1_cut<-cutDWByRT(RT=data_rt1000(), DW=df_dw1, timestamp_cols = col_to_cut)
    }
    df_dw1_cut
  })
  
  data_dw2_cut <- reactive({
    df_dw2_cut<-emptyTableValue
    if(dim(data_dw2())[1]>1 & dim(data_rt1000())[1]>1){
      df_dw2 <- data_dw2()
      col_to_cut <- c("timestamp")
      if(input$unshift_gps){
        df_dw2 <- addShiftedColumnDW(DW = df_dw2, shift = shift_dw2_gps(), shiftName = "timestampShiftedGPS")
        col_to_cut<-c(col_to_cut, "timestampShiftedGPS")
      }
      if(input$unshift_acc){
        df_dw2 <- addShiftedColumnDW(DW = df_dw2, shift = shift_dw2_acc(), shiftName = "timestampShiftedAcc")
        col_to_cut<-c(col_to_cut, "timestampShiftedAcc")
      }
      df_dw2_cut<-cutDWByRT(RT=data_rt1000(), DW=df_dw2, timestamp_cols = col_to_cut)
    }
    df_dw2_cut
  })
  
  data_dwb_cut <- reactive({
    df_dwb_cut<-emptyTableValue
    if(dim(data_dwb())[1]>1 & dim(data_rt1000())[1]>1){
      df_dwb <- data_dwb()
      col_to_cut <- c("timestamp")
      if(input$unshift_gps){
        df_dwb <- addShiftedColumnDW(DW = df_dwb, shift = shift_dwb_gps(), shiftName = "timestampShiftedGPS")
        col_to_cut<-c(col_to_cut, "timestampShiftedGPS")
      }
      if(input$unshift_acc){
        df_dwb <- addShiftedColumnDW(DW = df_dwb, shift = shift_dwb_acc(), shiftName = "timestampShiftedAcc")
        col_to_cut<-c(col_to_cut, "timestampShiftedAcc")
      }
      df_dwb_cut<-cutDWByRT(RT=data_rt1000(), DW=df_dwb, timestamp_cols = col_to_cut)
    }
    df_dwb_cut
  })
  
  
  
  output$encoding <- renderText({
    return(paste("Dec:",encoding$dec, " / encod:",encoding$encod))
  })
  
  output$rt1000 <- renderDataTable({ head(data_rt1000()) })
  output$dw1 <- renderDataTable({ head(data_dw1()) })
  output$dw2 <- renderDataTable({ head(data_dw2()) })
  output$dwb <- renderDataTable({ head(data_dwb()) })
  output$dw1_cut <- renderDataTable({ head(data_dw1_cut()) })
  output$dw2_cut <- renderDataTable({ head(data_dw2_cut()) })
  output$dwb_cut <- renderDataTable({ head(data_dwb_cut()) })
  
  
  
  
  plotData<-reactive({
    data<-as.data.frame(NA)
    first<-T
    if(dim(data_rt1000())[1]>1)
    {
      data<-data_rt1000() %>% mutate(boitier="RT1000")
      first<-F
      if(dim(data_dw1_cut())[1]>1)
        data<-rbind(data, data_dw1_cut() %>% mutate(boitier="DW1 cut"))
      if(dim(data_dw2_cut())[1]>1)
        data<-rbind(data, data_dw2_cut() %>% mutate(boitier="DW2 cut"))
      if(dim(data_dwb_cut())[1]>1)
        data<-rbind(data, data_dwb_cut() %>% mutate(boitier="DWB cut"))
    }else{
      if(dim(data_dw1())[1]>1)
      {
        if(first)
        {
          data<-data_dw1() %>% mutate(boitier="DW1")
          first<-F
        }else{
          data<-rbind(data, data_dw1() %>% mutate(boitier="DW1"))
        }
      }
      if(dim(data_dw2())[1]>1)
      {
        if(first)
        {
          data<-data_dw2() %>% mutate(boitier="DW2")
          first<-F
        }else{
          data<-rbind(data, data_dw2() %>% mutate(boitier="DW2"))
        }
      }
      if(dim(data_dwb())[1]>1)
      {
        if(first)
        {
          data<-data_dwb() %>% mutate(boitier="DWB")
          first<-F
        }else{
          data<-rbind(data, data_dwb() %>% mutate(boitier="DWB"))
        }
      }
    }
    
    return(data)
  })
  
  output$plot1 <- renderPlotly({
    data<-plotData()
    if(dim(data)[1]>1){
      mintime<-min(data$timestamp, na.rm = T)
      timestamp_col <- "timestamp"
      if(input$select1 %in% c("accX", "accY", "accZ", "gyrX", "gyrY", "gyrZ", "accXSI", "accYSI", "accZSI"))
      {
        if(input$unshift_acc)
          timestamp_col <- "timestampShiftedAcc"
      }else{
        if(input$unshift_gps)
          timestamp_col <- "timestampShiftedGPS"
      }
      
      ggplotly(
        ggplot(data %>% mutate(nb_sec=(!!as.name(timestamp_col))-mintime), aes_string(x = "nb_sec", y = input$select1, group = "boitier", color="boitier"))+
          geom_line(),
        dynamicTicks=T
      )
    }
  })
  
  output$plot2 <- renderPlotly({
    data<-plotData()
    if(dim(data)[1]>1){
      mintime<-min(data$timestamp, na.rm = T)
      timestamp_col <- "timestamp"
      if(input$select2 %in% c("accX", "accY", "accZ", "gyrX", "gyrY", "gyrZ", "accXSI", "accYSI", "accZSI"))
      {
        if(input$unshift_acc)
          timestamp_col <- "timestampShiftedAcc"
      }else{
        if(input$unshift_gps)
          timestamp_col <- "timestampShiftedGPS"
      }
      ggplotly(
        ggplot(data %>% mutate(nb_sec=(!!as.name(timestamp_col))-mintime), aes_string(x = "nb_sec", y = input$select2, group = "boitier", color="boitier"))+
          geom_line(),
        dynamicTicks=T
      )
    }
  })
  
  
  
  
  
  
  output$rt1000_text <- renderText({ 
    renderTextFunc("RT1000",data_rt1000())
  })
  output$dw1_text <- renderText({ 
    renderTextFunc("DW1",data_dw1())
  })
  output$dw2_text <- renderText({ 
    renderTextFunc("DW2",data_dw2())
  })
  output$dwb_text <- renderText({ 
    renderTextFunc("DWB",data_dwb())
  })
  
  # Shifts
  renderTextShift <- function(name, shift, first_name="RT", second_name="DW"){
    txt<-paste0(name,":")
    if(!is.na(shift))
      txt<-paste(txt, shift/100, "s of shift between", first_name, "and", second_name, "(positive:", second_name, "is after, negative:", second_name, "is before)")
    else
      txt<-paste(txt, "no data.")
    return(txt)
  }
  output$shift_gps_text <- renderUI({ 
    HTML(paste(renderTextShift("DW1 GPS", shift_dw1_gps()), renderTextShift("DW2 GPS", shift_dw2_gps()), renderTextShift("DWB GPS", shift_dwb_gps()), sep = '<br/>'))
  })
  output$shift_acc_text <- renderUI({ 
    HTML(paste(renderTextShift("DW1 ACC", shift_dw1_acc()), renderTextShift("DW2 ACC", shift_dw2_acc()), renderTextShift("DWB ACC", shift_dwb_acc()), sep = '<br/>'))
  })
  
  output$shift_all <- renderUI({ 
    HTML(paste(h4("Internal"),
               renderTextShift("DW1 Internal:", shift_dw1_acc()-shift_dw1_gps(), first_name = "accel", second_name = "GPS"), 
               renderTextShift("DW2 Internal", shift_dw2_acc()-shift_dw2_gps(), first_name = "accel", second_name = "GPS"), 
               renderTextShift("DWB Internal", shift_dwb_acc()-shift_dwb_gps(), first_name = "accel", second_name = "GPS"), 
               h4("GPS"),
               renderTextShift("DW1 GPS", shift_dw1_gps()), 
               renderTextShift("DW2 GPS", shift_dw2_gps()), 
               renderTextShift("DWB GPS", shift_dwb_gps()),
               h4("Accels"),
               renderTextShift("DW1 ACC", shift_dw1_acc()), 
               renderTextShift("DW2 ACC", shift_dw2_acc()), 
               renderTextShift("DWB ACC", shift_dwb_acc()),
               sep = '<br/>'
    )
    )
  })
  
  
  
  
  
  
  
  output$download_rt_1000 <- downloadHandler(
    filename = function(){"rt1000.csv"},
    content = function(file) {
      write.table(data_rt1000(), file, row.names = FALSE, na = "", sep=";", dec = ".")
    }
  )
  output$download_dw_1 <- downloadHandler(
    filename = function(){"dw1.csv"},
    content = function(file) {
      write.table(data_dw1(), file, row.names = FALSE, na = "", sep=";", dec = ".")
    }
  )
  output$download_dw_2 <- downloadHandler(
    filename = function(){"dw2.csv"},
    content = function(file) {
      write.table(data_dw2(), file, row.names = FALSE, na = "", sep=";", dec = ".")
    }
  )
  output$download_dwb <- downloadHandler(
    filename = function(){"dwb.csv"},
    content = function(file) {
      write.table(data_dwb(), file, row.names = FALSE, na = "", sep=";", dec = ".")
    }
  )
  output$download_dw_1_cut <- downloadHandler(
    filename = function(){"dw1_cut.csv"},
    content = function(file) {
      write.table(data_dw1_cut(), file, row.names = FALSE, na = "", sep=";", dec = ".")
    }
  )
  output$download_dw_2_cut <- downloadHandler(
    filename = function(){"dw2_cut.csv"},
    content = function(file) {
      write.table(data_dw2_cut(), file, row.names = FALSE, na = "", sep=";", dec = ".")
    }
  )
  output$download_dwb_cut <- downloadHandler(
    filename = function(){"dwb_cut.csv"},
    content = function(file) {
      write.table(data_dwb_cut(), file, row.names = FALSE, na = "", sep=";", dec = ".")
    }
  )
  
}


shinyApp(ui = ui, server = server)

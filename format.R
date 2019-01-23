require(tidyverse)

#-------------------------------Utilities functions------------------------------------------------

mGToSI<-function(accG){
  return( (as.numeric(accG)*9.80665)/1000. )
}

SIToMG<-function(accSI){
  return( (as.numeric(accSI)*1000.)/9.80665)
}


# Function to transform a date string in Enovea format to a timestamp
getTimestampFromTime<-function(date){
  date<-as.numeric(date)
  date1 <- trunc(date)
  date2 <- date-date1
  as.numeric(as.POSIXct(strptime(as.character(date1), "%Y%m%d%H%M%S")))+
    date2
}

# Function to transform a date string in RT1000 format to a timestamp
getTimestampFromDateTime<-Vectorize(function(date, time){
  time<-strsplit(as.character(time), ",")
  if(length(time)!=1 || length(time[[1]])!=2)
    return(0)
  time<-time[[1]]
  as.numeric(as.POSIXct(strptime(paste0(as.character(date),time[1]), "%d/%m/%Y%H:%M:%S")))+
    as.numeric(paste0("0.",time[2]))
})

#-------------------------------DW/RT functions------------------------------------------------

# Function to decode a DW1 dataframe
decodeDW1<-function(debug){
  debug %>% 
    mutate(next_speed=lead(speed), 
           next_alt=lead(alt),
           next_slope=lead(slope),
           next_curveRadius=lead(curveRadius),
           next_lng=lead(lng),
           next_lat=lead(lat)) %>% 
    unite(col = "0", "accX.0.","accY.0.", "accZ.0.") %>% 
    unite(col = "1", "accX.1.","accY.1.", "accZ.1.") %>%
    unite(col = "2", "accX.2.","accY.2.", "accZ.2.") %>% 
    unite(col = "3", "accX.3.","accY.3.", "accZ.3.") %>% 
    gather(key = "ID", value = "value", c("0","1", "2", "3")) %>% 
    extract(value, c("accX", "accY", "accZ"), 
            "(\\-*\\d+\\.*\\d*)_(\\-*\\d+\\.*\\d*)_(\\-*\\d+\\.*\\d*)") %>% 
    mutate(timestamp=getTimestampFromTime(time)+0.2*as.numeric(ID),
           accX=as.numeric(accX), accY=as.numeric(accY), accZ=as.numeric(accZ)) %>% 
    mutate(speed=speed+(next_speed-speed)*as.numeric(ID)/5,
           alt=alt+(next_alt-alt)*as.numeric(ID)/5,
           slope=slope+(next_slope-slope)*as.numeric(ID)/5,
           curveRadius=curveRadius+(next_curveRadius-curveRadius)*as.numeric(ID)/5,
           lng=lng+(next_lng-lng)*as.numeric(ID)/5,
           lat=lat+(next_lat-lat)*as.numeric(ID)/5,
           isInterpolated=(ID=="0"),
           indexInRow=as.numeric(ID),
           gyrX=NA,
           gyrY=NA,
           gyrZ=NA
           ) %>% 
    select(timestamp, lng, lat, slope, curveRadius, speed, alt, isInterpolated, accX, accY, accZ, gyrX, gyrY, gyrZ) %>% 
    mutate(accXSI=mGToSI(accX), accYSI=mGToSI(accY), accZSI=mGToSI(accZ)) %>% 
    arrange(timestamp)
}

# Function to decode a DW2 dataframe
decodeDW2<-function(debug){
  
  message("Decode dw2")
  debug %>% 
    mutate(next_speed=lead(speed), 
           next_alt=lead(alt),
           next_slope=lead(slope),
           next_curveRadius=lead(curveRadius),
           next_lng=lead(lng),
           next_lat=lead(lat)) %>% 
    unite(col = "0", "accX.0.","accY.0.", "accZ.0.", "gyroscopeX0.", "gyroscopeY0.", "gyroscopeZ0.") %>% 
    unite(col = "1", "accX.1.","accY.1.", "accZ.1.", "gyroscopeX1.", "gyroscopeY1.", "gyroscopeZ1.") %>%
    unite(col = "2", "accX.2.","accY.2.", "accZ.2.", "gyroscopeX2.", "gyroscopeY2.", "gyroscopeZ2.") %>% 
    unite(col = "3", "accX.3.","accY.3.", "accZ.3.", "gyroscopeX3.", "gyroscopeY3.", "gyroscopeZ3.") %>% 
    unite(col = "4", "accX.4.","accY.4.", "accZ.4.", "gyroscopeX4.", "gyroscopeY4.", "gyroscopeZ4.") %>% 
    gather(key = "ID", value = "value", c("0","1", "2", "3", "4")) %>% 
    extract(value, c("accX", "accY", "accZ", "gyrX", "gyrY", "gyrZ"), 
            "(\\-*\\d+\\.*\\d*)_(\\-*\\d+\\.*\\d*)_(\\-*\\d+\\.*\\d*)_(\\-*\\d+\\.*\\d*)_(\\-*\\d+\\.*\\d*)_(\\-*\\d+\\.*\\d*)") %>% 
    mutate(timestamp=getTimestampFromTime(time)+0.2*as.numeric(ID),
           accX=as.numeric(accX), accY=as.numeric(accY), accZ=as.numeric(accZ), 
           gyrX=as.numeric(gyrX), gyrY=as.numeric(gyrY), gyrZ=as.numeric(gyrZ)) %>% 
    mutate(speed=speed+(next_speed-speed)*as.numeric(ID)/5,
           alt=alt+(next_alt-alt)*as.numeric(ID)/5,
           slope=slope+(next_slope-slope)*as.numeric(ID)/5,
           curveRadius=curveRadius+(next_curveRadius-curveRadius)*as.numeric(ID)/5,
           lng=lng+(next_lng-lng)*as.numeric(ID)/5,
           lat=lat+(next_lat-lat)*as.numeric(ID)/5,
           isInterpolated=(ID=="0"),
           indexInRow=as.numeric(ID)) %>% 
    select(timestamp, lng, lat, slope, curveRadius, speed, alt, isInterpolated, accX, accY, accZ, gyrX, gyrY, gyrZ) %>% 
    mutate(accXSI=mGToSI(accX), accYSI=mGToSI(accY), accZSI=mGToSI(accZ)) %>% 
    arrange(timestamp)
}

# Function to decode a DWB dataframe
decodeDWB<-function(debug){
  message("WTF")
  # debug %>% 
  #   mutate(next_speed=lead(speed), 
  #          next_alt=lead(alt),
  #          next_slope=lead(slope),
  #          next_curveRadius=lead(curveRadius),
  #          next_lng=lead(lng),
  #          next_lat=lead(lat)) %>% 
  #   unite(col = "0", "accX.0.","accY.0.", "accZ.0.", "gyroscopeX0.", "gyroscopeY0.", "gyroscopeZ0.") %>% 
  #   unite(col = "1", "accX.1.","accY.1.", "accZ.1.", "gyroscopeX1.", "gyroscopeY1.", "gyroscopeZ1.") %>%
  #   unite(col = "2", "accX.2.","accY.2.", "accZ.2.", "gyroscopeX2.", "gyroscopeY2.", "gyroscopeZ2.") %>% 
  #   unite(col = "3", "accX.3.","accY.3.", "accZ.3.", "gyroscopeX3.", "gyroscopeY3.", "gyroscopeZ3.") %>% 
  #   unite(col = "4", "accX.4.","accY.4.", "accZ.4.", "gyroscopeX4.", "gyroscopeY4.", "gyroscopeZ4.") %>% 
  #   gather(key = "ID", value = "value", c("0","1", "2", "3", "4")) %>% 
  #   extract(value, c("accX", "accY", "accZ", "gyrX", "gyrY", "gyrZ"), 
  #           "(\\-*\\d+\\.*\\d*)_(\\-*\\d+\\.*\\d*)_(\\-*\\d+\\.*\\d*)_(\\-*\\d+\\.*\\d*)_(\\-*\\d+\\.*\\d*)_(\\-*\\d+\\.*\\d*)") %>% 
  #   mutate(timestamp=getTimestampFromTime(time)+0.2*as.numeric(ID),
  #          accX=as.numeric(accX), accY=as.numeric(accY), accZ=as.numeric(accZ), 
  #          gyrX=as.numeric(gyrX), gyrY=as.numeric(gyrY), gyrZ=as.numeric(gyrZ)) %>% 
  #   mutate(speed=speed+(next_speed-speed)*as.numeric(ID)/5,
  #          alt=alt+(next_alt-alt)*as.numeric(ID)/5,
  #          slope=slope+(next_slope-slope)*as.numeric(ID)/5,
  #          curveRadius=curveRadius+(next_curveRadius-curveRadius)*as.numeric(ID)/5,
  #          lng=lng+(next_lng-lng)*as.numeric(ID)/5,
  #          lat=lat+(next_lat-lat)*as.numeric(ID)/5,
  #          isInterpolated=(ID=="0"),
  #          indexInRow=as.numeric(ID)) %>% 
  #   select(timestamp, lng, lat, slope, curveRadius, speed, alt, isInterpolated, accX, accY, accZ, gyrX, gyrY, gyrZ) %>% 
  #   mutate(accXSI=mGToSI(accX), accYSI=mGToSI(accY), accZSI=mGToSI(accZ)) %>% 
  #   arrange(timestamp)
}

# Function to decode a RT1000 dataframe
decodeRT1000<-function(debug){
  colnames<-colnames(debug)
  debug %>% 
    mutate(
      timestamp=getTimestampFromDateTime(as.character(Date..UTC.),as.character(Time..UTC.)),
      lng=as.numeric(PosLon..deg.),
      lat=as.numeric(PosLat..deg.),
      slope=as.numeric(NA),
      curveRadius=ifelse("Curvature..1.m" %in% colnames, as.numeric(Curvature..1.m), as.numeric(NA)),
      speed=as.numeric(Speed2D..km.h.),
      alt=as.numeric(PosAlt..m.),
      isInterpolated=F,
      accX=SIToMG(AccelX..m.s..),
      accY=-SIToMG(AccelY..m.s..),
      accZ=-SIToMG(AccelZ..m.s..),
      accXSI=as.numeric(AccelX..m.s..),
      accYSI=-as.numeric(AccelY..m.s..),
      accZSI=-as.numeric(AccelZ..m.s..),
      gyrX=as.numeric(AngleRateX..deg.s.),
      gyrY=as.numeric(AngleRateY..deg.s.),
      gyrZ=as.numeric(AngleRateZ..deg.s.)
    ) %>% 
    select(timestamp, lng, lat, slope, curveRadius, speed, alt, isInterpolated, accX, accY, accZ, gyrX, gyrY, gyrZ, accXSI, accYSI, accZSI)
}

getCSVType <- function(csvDF){
  case_when(
    "AccelX..m.s.." %in% colnames(csvDF) | "AccelX..m.s." %in% colnames(csvDF) ~ "RT",
    "accX.4." %in% colnames(csvDF) ~ "DW2",
    "accX.3." %in% colnames(csvDF) ~ "DW1",
    "acc.axis.X" %in% colnames(csvDF) ~ "DWB",
    T ~ "UNKNOWN"
  )
}

cleanCSV <- function(debug){
  csvType<-getCSVType(debug)
  stopifnot(csvType != "UNKNOWN")
  if(csvType=="RT")
    return(decodeRT1000(debug))
  if(csvType=="DW2")
    return(decodeDW2(debug))
  if(csvType=="DW1")
    return(decodeDW1(debug))
  if(csvType=="DWB")
    return(decodeDWB(debug))
}

getDiffTime<-function(raw, fileSize){
  readIntegerFromEnd<-function(raw, fileSize, startFromEnd, size){
    #readBin(con=raw[(fileSize-startFromEnd):(fileSize-(startFromEnd+size))], what="integer", signed=F, size=8, n=1, endian="little")
    posStart <- fileSize-startFromEnd
    rawvec <- readBin(raw[posStart:(posStart+size)], raw(), n=8, endian="little")
    # R can't read an uint64 directly...
    sum(
      2^.subset(
        0:63, 
        as.logical(
          rawToBits(
            rawvec
          )
        )
      )
    )
  }
  
  time_boitier <- readIntegerFromEnd(raw, fileSize, 26, 8)
  time_gps <- readIntegerFromEnd(raw, fileSize, 18, 8)
  
  time_to_add <- (time_gps-time_boitier)/1000
  return(time_to_add)
}


getInfoDWB<-function(file_path){
  data <- readLines(file_path)
  df <- read.csv2(text=data[1:2], stringsAsFactors = F)
  # Drop rtc_epoch (start of trip) because the second has the same name
  df <- df %>% select(-rtc_epoch) %>% cbind(read.csv2(text=data[3:4], stringsAsFactors = F))
  
  return(
    data.frame(
      shift = (df$gps_epoch - df$rtc_epoch)/1000,
      euler_angle_1 = as.numeric(df$euler_angle_1),
      euler_angle_2 = as.numeric(df$euler_angle_2),
      euler_angle_3 = as.numeric(df$euler_angle_3)
    )
  )
}

# Function that return the possible separator used in numerics values
getSeparator<-function(file_path){
  message("Get separator: ",file_path)
  lines <- readLines(file_path)
  nb_lines_to_look <- min(length(lines),100)
  if(any(grepl(pattern = '.',lines[2:nb_lines_to_look])))
    return('.')
  return(',')
}


cutDWByRT<-function(RT, DW, timestamp_cols=c("timestamp")){
  min_timestamp<-min(RT$timestamp)
  max_timestamp<-max(RT$timestamp)
  
  mins <- DW %>% select(one_of(timestamp_cols)) %>% summarise_all(min)
  last_col <- (mins %>% select_if(function(.) . == min(mins)) %>% names())[1]
  
  maxs <- DW %>% select(one_of(timestamp_cols)) %>% summarise_all(max)
  first_col <- (maxs %>% select_if(function(.) . == max(maxs)) %>% names())[1]
  
  if(min(DW[[first_col]])>min_timestamp | max(DW[[last_col]])<max_timestamp){
    print("Error: DW file does not overlap the RT file.")
    return(NA)
  }else{
    return(DW %>% filter((!!as.name(first_col))>=min_timestamp & (!!as.name(last_col))<=max_timestamp))
  }
}


# Positif: dw en retard
# Négatif: dw en avance
getShift <- function(RT, DW, col_name="speed", lag.max=NA){
  tmp_fus <- RT %>% select(timestamp, (!!as.name(col_name))) %>% mutate(timestamp=round(timestamp, 2), val=(!!as.name(col_name)))
  tmp_fus_dw <- DW %>% select(timestamp, (!!as.name(col_name))) %>% mutate(timestamp=round(timestamp, 2), val=(!!as.name(col_name)))
  tmp_fus <- tmp_fus %>% left_join(tmp_fus_dw, by=c("timestamp"), suffix = c(".rt", ".dw"))
  
  rt <- tmp_fus$val.rt
  dw <- tmp_fus$val.dw
  
  rt[is.na(rt)] <- 0
  dw[is.na(dw)] <- 0
  lag.max <- ifelse(is.na(lag.max), min(2000, length(rt)/2), lag.max)
  shift<- -lag.max:lag.max
  cor<-numeric()
  for(i in shift){
    as <- if(i<0) dw[1:(length(dw)+i)] else rt[1:(length(rt)-i)]
    bs <-  if(i<0) rt[(-i+1):length(rt)] else dw[(i+1):length(dw)]
    cor <- append(cor, sum(as*bs) / sqrt(sum(as^2)*sum(bs^2)))
  }
  shift[which.max(cor)]
}


# cleanShiftRT <- function(df_rt, shift, shiftName="timeGPS"){
#   if(shift>0){
#     df_rt <- df_rt[1:(nrow(df_rt)-shift),]
#   }
#   if(shift<0){
#     df_rt <- df_rt[(1-shift):nrow(df_rt),]
#   }
#   df_rt[[shiftName]] <- df_rt$timestamp-min(df_rt$timestamp)
#   df_rt
# }

addShiftedColumnDW <- function(DW, shift, shiftName="timeGPS"){
  DW[[shiftName]] <- DW$timestamp-shift/100
  DW
}


#------------------------------ Display functions -----------------------------------------------

# Filter the cleaned RT1000 with a butterworth low-pass 
filterRT1000 <- function(RT, freq=1/5, order=6){
  require(signal)
  filt<-butter(n=order, W=freq, type = "low")
  RT$accXSI <- as.numeric(filter(filt, x=RT$accXSI))
  RT$accYSI <- as.numeric(filter(filt, x=RT$accYSI))
  RT$accZSI <- as.numeric(filter(filt, x=RT$accZSI))
  RT$accX <- as.numeric(filter(filt, x=RT$accX))
  RT$accY <- as.numeric(filter(filt, x=RT$accY))
  RT$accZ <- as.numeric(filter(filt, x=RT$accZ))
  detach("package:signal", unload=TRUE) # Do that otherwise the "filter" function of signal interfere with dplyr's
  return(RT)
}

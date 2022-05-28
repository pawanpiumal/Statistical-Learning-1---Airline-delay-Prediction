#' @filter cors
cors <- function(req, res) {
  
  res$setHeader("Access-Control-Allow-Origin", "*")
  
  if (req$REQUEST_METHOD == "OPTIONS") {
    res$setHeader("Access-Control-Allow-Methods","*")
    res$setHeader("Access-Control-Allow-Headers", req$HTTP_ACCESS_CONTROL_REQUEST_HEADERS)
    res$status <- 200 
    return(list())
  } else {
    plumber::forward()
  }
  
}

#* Echo back the input
#* @get /api/carriers
function() {
  list(carriers = carriers)
}


#* Return the sum of two numbers
#* @param date Date
#* @param distance Distance Group
#* @param time Departure block
#* @param airport Departing Airport Group
#* @param carrier Carrier name
#* @param seats Number of seats
#* @post /api/predict
function(date,distance,time,airport,carrier,seats) {
  date = as.Date(date)
  week = c("MON","TUE","WED","THU","FRI","SAT","SUN")
  DAY_OF_WEEK = grep(format(date,"%a"),week,ignore.case = T)
  data = data.frame(format(date,"%d"), # DAY_OF_MONTH
           as.integer(seats),# NUMBER_OF_SEATS
           time,# DEP_BLOCK
           as.integer(distance), #DISTANCE_GROUP
           format(date,"%m"), # MONTH
           carrier, # CARRIER_NAME
           DAY_OF_WEEK ,# DAY_OF_WEEK
           airport # DEPARTING_AIRPORT
           )
  colnames(data) = c("DAY_OF_MONTH",
                     "NUMBER_OF_SEATS",
                     "DEP_BLOCK",
                     "DISTANCE_GROUP",
                     "MONTH",
                     "CARRIER_NAME",
                     "DAY_OF_WEEK",
                     "DEPARTING_AIRPORT")
  
  data$DISTANCE_GROUP = factor(data$DISTANCE_GROUP, levels= levels(train$DISTANCE_GROUP))
  data$CARRIER_NAME = factor(data$CARRIER_NAME, levels= levels(train$CARRIER_NAME))
  data$DEPARTING_AIRPORT = factor(data$DEPARTING_AIRPORT, levels= levels(train$DEPARTING_AIRPORT))
  data$DEP_BLOCK = factor(data$DEP_BLOCK, levels= levels(train$DEP_BLOCK))

  pred =predict(model,newdata=data, type = "prob")
  list(Probability=round(pred[2],4),
       Outcome=ifelse(pred[1]>0.5,"Low probability to get delayed!","High probability to get delayed!"),
       Delayed = ifelse(pred[1]>0.5,FALSE,TRUE))
}



#* Return the sum of two numbers
#* @param range Range Variable
#* @param date Date
#* @param distance Distance Group
#* @param time Departure block
#* @param airport Departing Airport Group
#* @param carrier Carrier name
#* @param seats Number of seats
#* @post /api/predictRange
function(range,date,distance,time,airport,carrier,seats) {
  date = as.Date(date)
  week = c("MON","TUE","WED","THU","FRI","SAT","SUN")
  
  data = data.frame(matrix(nrow=0,ncol=8))
  colnames(data) = c("DAY_OF_MONTH",
                     "NUMBER_OF_SEATS",
                     "DEP_BLOCK",
                     "DISTANCE_GROUP",
                     "MONTH",
                     "CARRIER_NAME",
                     "DAY_OF_WEEK",
                     "DEPARTING_AIRPORT")
  if(range == 'date'){
    dates = date+0:6
    for(i in 1:7){
      DAY_OF_WEEK = grep(format(dates[i],"%a"),week,ignore.case = T)
      data[i,] = c(format(dates[i],"%d"), # DAY_OF_MONTH
                        as.integer(seats),# NUMBER_OF_SEATS
                        time,# DEP_BLOCK
                        as.integer(distance), #DISTANCE_GROUP
                        format(dates[i],"%m"), # MONTH
                        carrier, # CARRIER_NAME
                        DAY_OF_WEEK ,# DAY_OF_WEEK
                        airport # DEPARTING_AIRPORT
                  )
    }
  }else if(range == 'distance'){
    distances = levels(train$DISTANCE_GROUP)
    DAY_OF_WEEK = grep(format(date,"%a"),week,ignore.case = T)
    for(i in 1:length(distances)){
      data[i,] = c(format(date,"%d"), # DAY_OF_MONTH
                   as.integer(seats),# NUMBER_OF_SEATS
                   time,# DEP_BLOCK
                   as.integer(distances[i]), #DISTANCE_GROUP
                   format(date,"%m"), # MONTH
                   carrier, # CARRIER_NAME
                   DAY_OF_WEEK ,# DAY_OF_WEEK
                   airport # DEPARTING_AIRPORT
      )
    }
  }else if(range == 'time'){
    times = levels(train$DEP_BLOCK)
    DAY_OF_WEEK = grep(format(date,"%a"),week,ignore.case = T)
    for(i in 1:length(times)){
      data[i,] = c(format(date,"%d"), # DAY_OF_MONTH
                   as.integer(seats),# NUMBER_OF_SEATS
                   times[i],# DEP_BLOCK
                   as.integer(distance), #DISTANCE_GROUP
                   format(date,"%m"), # MONTH
                   carrier, # CARRIER_NAME
                   DAY_OF_WEEK ,# DAY_OF_WEEK
                   airport # DEPARTING_AIRPORT
      )
    }
  }else if(range == 'airport'){
    airport = levels(train$DEPARTING_AIRPORT)
    DAY_OF_WEEK = grep(format(date,"%a"),week,ignore.case = T)
    for(i in 1:length(airport)){
      data[i,] = c(format(date,"%d"), # DAY_OF_MONTH
                   as.integer(seats),# NUMBER_OF_SEATS
                   time,# DEP_BLOCK
                   as.integer(distance), #DISTANCE_GROUP
                   format(date,"%m"), # MONTH
                   carrier, # CARRIER_NAME
                   DAY_OF_WEEK ,# DAY_OF_WEEK
                   airport[i] # DEPARTING_AIRPORT
      )
    }
  }else if(range == 'carrier'){
    carriers = levels(train$CARRIER_NAME)
    DAY_OF_WEEK = grep(format(date,"%a"),week,ignore.case = T)
    for(i in 1:length(carriers)){
      data[i,] = c(format(date,"%d"), # DAY_OF_MONTH
                   as.integer(seats),# NUMBER_OF_SEATS
                   time,# DEP_BLOCK
                   as.integer(distance), #DISTANCE_GROUP
                   format(date,"%m"), # MONTH
                   carriers[i], # CARRIER_NAME
                   DAY_OF_WEEK ,# DAY_OF_WEEK
                   airport # DEPARTING_AIRPORT
      )
    }
  }else{
    list(error="Error")
  }

  data$DISTANCE_GROUP = factor(data$DISTANCE_GROUP, levels= levels(train$DISTANCE_GROUP))
  data$CARRIER_NAME = factor(data$CARRIER_NAME, levels= levels(train$CARRIER_NAME))
  data$DEPARTING_AIRPORT = factor(data$DEPARTING_AIRPORT, levels= levels(train$DEPARTING_AIRPORT))
  data$DEP_BLOCK = factor(data$DEP_BLOCK, levels= levels(train$DEP_BLOCK))
  
  pred =predict(model,newdata=data, type = "prob")
  
  
  if(range == 'date'){
    pred = data.frame(round(pred[,2],4),dates)
  }else if(range == 'distance'){
    labels = c("0 - 249","250 - 499","500 - 749","750 - 999","1000 - 1249","1250 - 1499","1500 - 1749",
               "1750 - 1999","2000 - 2249","2250 - 2499","2500+")
    pred = data.frame(round(pred[,2],4),labels) 
  }else if(range == 'time'){
    labels = c("00:00 - 05:59","06:00 - 9:59","10:00 - 12:59","13:00 - 16:59","17:00 - 20:59","21:00 - 23:59")
    pred = data.frame(round(pred[,2],4),labels)
  }else if(range == 'airport'){
    pred = data.frame(round(pred[,2],4),airport) %>% arrange()
  }else if(range == 'carrier'){
    pred = data.frame(round(pred[,2],4),carriers) %>% arrange()
  }
  pred[,3]=ifelse(pred[,1]>0.5,TRUE,FALSE)
  
  print(data)
  print(pred)
  list(probability = pred[,1],
       variable = pred[,2],
       delay = pred[,3])
}



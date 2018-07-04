library("datetime")
library("anytime")
library("lubridate")

load('input_data.RData')

### MODEL DAY ###

get_daily_flow_model <-function(endd_string){
day<-NULL;
# days<-c(1,2,6,7,6,22)
# months<-c(7,7,7,7,9,6)
start = as.POSIXct("2015-06-02 0:00:00")
endd = as.POSIXct(endd_string)#"2018-02-27 0:00:00"

days <- seq(from=start,to=endd,by="1 DSTday")
for(i in 1:length(days)){
  start<-days[i]
  start_ts <- as.numeric(start)
  end <- seq(from=start,by="1 day", length=2)[2]
  end_ts <- as.numeric(end)
  
  tmp_day <- input_data[
    (input_data$timestamp>=start_ts & input_data$timestamp<end_ts)
      & (input_data$m==30 | input_data$m==0)
    , names(input_data) %in% c('flow','rainfall','timestamp','h','m')]
  
  ### korekta DST 
  if(dst(force_tz(start,"US/Eastern"))){
    tmp_day$flow <- c(tail(tmp_day$flow,-2),head(tmp_day$flow,2))
  }
  ###
  
  if(max(tmp_day$rainfall)==0)
    day <- c(day,list(tmp_day));
}

ideal_day<-day[[1]]
n <- 0
for(i in 2:length(day)){
  list <- day[[i]]
  
  if(length(list$flow)==length(ideal_day$flow)){
    ideal_day$flow <- ideal_day$flow + list$flow
    n <- n+1  
  }
}
ideal_day$flow<-ideal_day$flow / n;

###
  return(ideal_day);
}
# save(ideal_day,file='model_day.RData')

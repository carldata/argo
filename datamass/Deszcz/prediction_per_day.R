library("datetime")
library("SimDesign")
library("lubridate")
load('input_data.RData')
load('rain_params.RData')


### DATA ###
new_probe_period<-30
response_data <- input_data[(
  input_data$year==2017)
  & (input_data$m==30 | input_data$m==0),
  names(input_data) %in% c('flow','rainfall','timestamp','h','m')]
resp_length <- length(response_data[,1])

  ### RAINFALL RECALC FOR NEW PROBING PERIOD ###
  if(new_probe_period>5)
    for(i in 1:resp_length){
      v<-response_data[i,]
  
      filtered<-input_data[(input_data$timestamp > v$timestamp - new_probe_period)
                           & (input_data$timestamp <= v$timestamp),]
      response_data[i,]$rainfall <- sum(filtered$rainfall)
    }
  ###
###

### PREDICTION ###
output_vector <- rep.int(0,output_vector_length);
start = as.POSIXct("2017-04-06 0:00:00", tz='GMT')
endd = as.POSIXct("2017-04-10 23:59:59", tz='GMT')

days <- seq(from=start,to=endd,by="1 DSTday")
response <- NULL;
for(i in 1:length(days)){
  start <- days[i]
  start_ts <- as.numeric(start)
  end <- seq(from=start,by="1 day", length=2)[2]
  end_ts <- as.numeric(end)
  
  tmp_day <- response_data[
    (response_data$timestamp>=start_ts & response_data$timestamp<end_ts),
    , names(response_data) %in% c('flow','rainfall','timestamp','h','m')]
  
  if(i==1) ### startowy wektor wejsciowy
    input_vector <- rev(tmp_day$rainfall[1:input_vector_length])
  
  
  day_length <- length(tmp_day$flow)
  actual_response <- rep.int(0,day_length)
  predicted_response <- rep.int(0,day_length)
  prediction_error <- rep.int(0,day_length);
  
  ##DAILY_MODEL##
  ideal_day <- get_daily_flow_model(start) ###model dzienny
  
  
  for(j in 1:day_length){
    v<-tmp_day[j,]
     # v$rainfall<-0 #wylaczenie wplywu deszczu
    
    regression_vector <- c(output_vector,input_vector)
    
    y_hat <- drop(regression_vector %*% params) ###odpowiedz modelu deszczu
    output_vector <- c(-y_hat,head((output_vector),-1));
    input_vector <- c(v$rainfall, head((input_vector),-1));
    
    actual_response[j] <-v$flow
    
    ### korekta DST 
    if(dst(force_tz(start,"US/Eastern")))
      dst_correction <- 2
    else
      dst_correction <- 0
    ###
    
    predicted_response[j] <- y_hat + ideal_day$flow[(j+dst_correction)%%48+1] ###odpowiedz systemu
    prediction_error[j] <- abs((v$flow - predicted_response[j])/v$flow)
    # RMSE(v$flow,response_data$predicted_response[i])
    # drop(v$flow - response_data$predicted_response[i])
    
  }
  ### PLOT ###
  
    plot(tmp_day$timestamp,actual_response,type="l",xaxt='n',ylim=c(-50,250),xlab="", ylab="", col="blue")
    axis(1, at=tmp_day$timestamp,labels=format.datetime(tmp_day$timestamp,"%Y-%m-%d %H:%M"), las=0)
    
    lines(tmp_day$timestamp,predicted_response,type="l",xlab="", ylab="",col="orange")
    # lines(tmp_day$timestamp,r_Dm,type="l",xlab="", ylab="",col="red")
    # lines(tmp_day$timestamp,r_Dm_wDST,type="l",xlab="", ylab="",col="orange")
    # lines(tmp_day$timestamp,r_Dm_wRm_wDST,type="l",xlab="", ylab="",col="green")
    
    # lines(tmp_day$timestamp,prediction_error,type="l",xlab="", ylab="",col="red")

    axis(4,at=prediction_error, las=0)

    par(new=TRUE)

    plot(tmp_day$timestamp,tmp_day$rainfall,type="l",axes=FALSE,xlab="", ylab="",col="gray")
  ###
  response <- c(response,100*sum(prediction_error)/day_length)
  print(i)
}
###

rmse_score<-quantile(response,0.95,na.rm=TRUE)
print(rmse_score)

library("datetime")

load('~/input_data.RData')
load('~/model_day.RData')

### MODEL DAY ###
day<-NULL;
days<-c(23)
months<-c(6)
for(i in 1:length(days)){
  tmp_day<- input_data[
    input_data$day==days[i]
    & input_data$month==months[i]
    & input_data$year==2017
    & (input_data$m==30 | input_data$m==0),names(input_data) %in% c('flow','rainfall','timestamp','h','m','day')]
  day<-c(day,list(tmp_day));
}
start = as.POSIXct("2017-06-23 0:00:00", tz='GMT')
rain_day <- day[[1]]

ideal_day <- get_daily_flow_model(start) ###model dzienny
rain_day$flow <- rain_day$flow - ideal_day$flow;
###

plot(rain_day$timestamp,rain_day$flow,type="l",axes=FALSE,xlab="", ylab="", col="blue")
axis(2,at=rain_day$flow, las=0)
axis(1, at=rain_day$timestamp,labels=format.datetime(rain_day$timestamp,"%H:%M"), las=0)
# lines(rain_day$timestamp,ideal_day$flow,type="l",xlab="", ylab="",col="orange")
par(new=TRUE)
plot(rain_day$timestamp,rain_day$rainfall,type="l",axes=FALSE,xlab="", ylab="",col="gray")
axis(4,at=rain_day$rainfall, las=0)

save(rain_day,file='rain_day.RData')
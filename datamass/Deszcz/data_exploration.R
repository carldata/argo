library("datetime")
new_probe_period<-30
load('input_data.RData')

response_data <- input_data[(
  (
    ((input_data$day>=6 & input_data$day<=10) & input_data$month==4)
  )
  & input_data$year==2017)
  & (input_data$m==30| input_data$m==0),
  names(input_data) %in% c('flow','rainfall','timestamp','h','m')]

resp_length <- length(response_data[,1])
### RAINFALL RECALC FOR NEW PROBING PERIOD ###
for(i in 1:resp_length){
  v<-response_data[i,]
  
  filtered<-input_data[(input_data$timestamp > v$timestamp - new_probe_period) 
                       & (input_data$timestamp <= v$timestamp),]
  response_data[i,]$rainfall <- sum(filtered$rainfall)
}
###

plot_data <- response_data[,names(response_data) %in% c("timestamp","flow","rainfall")]

plot(plot_data$timestamp,plot_data$flow,type="l",axes=FALSE,xlab="", ylab="", col="blue")
axis(2,at=plot_data$flow, las=0)
axis(1, at=plot_data$timestamp,labels=format.datetime(plot_data$timestamp,"%Y-%m-%d %H:%M"), las=0)
par(new=TRUE)
plot(plot_data$timestamp,plot_data$rainfall,type="l",axes=FALSE,xlab="", ylab="",col="gray")
axis(4,at=plot_data$rainfall, las=0)
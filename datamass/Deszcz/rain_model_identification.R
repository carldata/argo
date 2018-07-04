library("datetime")
new_probe_period<-30
load('rain_day.RData')
load('input_data.RData')
lambda <- 0.9
output_vector_length <- 2
input_vector_length <- 2
data_length <- output_vector_length+input_vector_length

### Clean model ###
params <- rep.int(0.1,data_length)
P <- diag(data_length)

### DATA ###
response_data <- rain_day;
output_vector <-rep.int(0,output_vector_length);
# -rev(response_data$flow[1:output_vector_length])
input_vector <- rev(response_data$rainfall[1:input_vector_length])
###

resp_length <- length(response_data[,1])
response_data[7] <- rep.int(0,resp_length);
response_data[8] <- rep.int(0,resp_length);
colnames(response_data)[7] <-'predicted_response'
colnames(response_data)[8] <-'prediction_error'

### RAINFALL RECALC FOR NEW PROBING PERIOD ###
for(i in 1:resp_length){
  v<-response_data[i,]
  
  filtered<-input_data[(input_data$timestamp >= v$timestamp - new_probe_period/2) 
                       & (input_data$timestamp < v$timestamp + new_probe_period/2),]
  response_data[i,]$rainfall <- sum(filtered$rainfall)
}
###
params<-c(-0.757,-0.163, 2.81,12) #2+2 1.168291 ###  30 min big rain
### PREDICTION ###
for(i in 1:resp_length){
  v<-response_data[i,]
  
  input_vector <- c(v$rainfall, head((input_vector),-1));
  regression_vector <- c(output_vector,input_vector)
  
  predicted_response <- drop(regression_vector %*% params)
  prediction_error <- drop(v$flow - predicted_response)
  
  response_data$predicted_response[i] <- predicted_response
  response_data$prediction_error[i] <- prediction_error
  
  #P
  numerator <- (P %*% regression_vector %*% t(regression_vector) %*% P)
  denominator<- drop(1/(lambda + t(regression_vector) %*% P %*% regression_vector))
  P <- (1/lambda) * (P-numerator * denominator)
  
  #L
  L <- P %*% regression_vector
  
  #params
  # params <- params + L*prediction_error
  
  output_vector <- c(-predicted_response,head((output_vector),-1));
  
}
###


### PLOT ###
plot_data <- response_data[,names(response_data) %in% c("timestamp","flow","rainfall","predicted_response","prediction_error")]

plot(plot_data$timestamp,plot_data$flow,type="l",xaxt='n',ylim=c(-5,350),xlab="", ylab="", col="blue")
lines(plot_data$timestamp,plot_data$predicted_response,type="l",xlab="", ylab="",col="orange")
# axis(2,at=plot_data$flow, las=0)
axis(1, at=plot_data$timestamp,labels=format.datetime(plot_data$timestamp,"%Y-%m-%d %H:%M"), las=0)

lines(plot_data$timestamp,plot_data$prediction_error,type="l",xaxt='n',xlab="", ylab="",col="red")

axis(4,at=plot_data$prediction_error, las=0)
par(new=TRUE)
plot(plot_data$timestamp,plot_data$rainfall,type="l",axes=FALSE,xlab="", ylab="",col="gray")


###
a_score<-mean(abs(response_data$prediction_error))
# print(a_score)
rmse_score<-RMSE(response_data$predicted_response,response_data$flow)
print(rmse_score)

# save(list=c('params','output_vector_length','input_vector_length','lambda'),file='rain_params.RData')


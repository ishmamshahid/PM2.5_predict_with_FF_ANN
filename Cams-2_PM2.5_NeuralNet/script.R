data <- read.csv("CAMS2-FARMGATE all date.csv")
summary(data)
data<-data[366:2191,]
summary(data)
data<-data[-1]
summary(data)
date<- seq(as.Date('2014-01-01'),as.Date('2018-12-31'),by = 1)
date<-as.POSIXct(strptime(date, format ="%Y-%m-%d" ))
data<-cbind(date,data)
# calculate weekly means
Weekly_Means <- aggregate(data, format(data["date"],"%Y-%W"),
                               mean, na.rm = TRUE)
# derive the proper sequence of dates
Weekly_Means$date <- seq(min(data$date), max(data$date), length = nrow(Weekly_Means))
Weekly_Means<-Weekly_Means[-1]
plot(Weekly_Means$date, Weekly_Means[, "PM2.5"],
     type = "l",
     lwd = 1.5,
     pch = 16,
     col = "darkorange2",
     xlab = "year",
     ylab = "PM2.5",
     ylim = c(0, 400),
     main = "Weekly mean PM2.5 at CAMS-2.")

data<-data[-1]
dataPM2.5<- data[-6]
normalize <- function(x) {
  return((x - min(x)) / (max(x) - min(x)))
}

dataPM2.5_norm <- as.data.frame(lapply(dataPM2.5, normalize))
summary(dataPM2.5_norm$PM2.5)
summary(dataPM2.5$PM2.5)

dataPM2.5_train <- dataPM2.5_norm[1:1461, ]
dataPM2.5_test <- dataPM2.5_norm[1462:1826, ]

library(neuralnet)

#3
model_3 <- neuralnet(PM2.5 ~ Temp + ws +
                           Solar.Rad+ Rainfall+ Humidity ,
                         data = dataPM2.5_train,hidden = 3)
plot(model_3,show.weights = F,col.hidden = 'darkgreen',intercept = F,
     col.hidden.synapse = 'red', fill="lightblue")
#5
model_5 <- neuralnet(PM2.5 ~ Temp + ws +
                       Solar.Rad+ Rainfall+ Humidity ,
                     data = dataPM2.5_train,hidden = 5)
plot(model_5,show.weights = F,col.hidden = 'darkgreen',intercept = F,
     col.hidden.synapse = 'red', fill="lightblue")
#7
model_7 <- neuralnet(PM2.5 ~ Temp + ws +
                       Solar.Rad+ Rainfall+ Humidity ,
                     data = dataPM2.5_train,hidden = 7)
plot(model_7,show.weights = F,col.hidden = 'darkgreen',intercept = F,
     col.hidden.synapse = 'red', fill="lightblue")
#9
model_9 <- neuralnet(PM2.5 ~ Temp + ws +
                       Solar.Rad+ Rainfall+ Humidity ,
                     data = dataPM2.5_train,hidden = 9)
plot(model_9,show.weights = F,col.hidden = 'darkgreen',intercept = F,
     col.hidden.synapse = 'red', fill="lightblue")
#12
model_12 <- neuralnet(PM2.5 ~ Temp + ws +
                       Solar.Rad+ Rainfall+ Humidity ,
                     data = dataPM2.5_train,hidden = 12)
plot(model_12,show.weights = F,col.hidden = 'darkgreen',intercept = F,
     col.hidden.synapse = 'red', fill="lightblue")
#15
model_15 <- neuralnet(PM2.5 ~ Temp + ws +
                       Solar.Rad+ Rainfall+ Humidity ,
                     data = dataPM2.5_train,hidden = 15)
plot(model_15,show.weights = F,col.hidden = 'darkgreen',intercept = F,
     col.hidden.synapse = 'red', fill="lightblue")

#20
model_20 <- neuralnet(PM2.5 ~ Temp + ws +
                       Solar.Rad+ Rainfall+ Humidity ,
                     data = dataPM2.5_train,hidden = 20)
plot(model_20,show.weights = F,col.hidden = 'darkgreen',intercept = F,
     col.hidden.synapse = 'red', fill="lightblue")

model_results_3 <- compute(model_3, dataPM2.5_test[1:5])
model_results_5 <- compute(model_5, dataPM2.5_test[1:5])
model_results_7 <- compute(model_7, dataPM2.5_test[1:5])
model_results_9 <- compute(model_9, dataPM2.5_test[1:5])
model_results_12 <- compute(model_12, dataPM2.5_test[1:5])
model_results_15 <- compute(model_15, dataPM2.5_test[1:5])
model_results_20 <- compute(model_20, dataPM2.5_test[1:5])

predicted3 <- model_results_3$net.result
predicted5 <- model_results_5$net.result
predicted7 <- model_results_7$net.result
predicted9 <- model_results_9$net.result
predicted12 <- model_results_12$net.result
predicted15 <- model_results_15$net.result
predicted20 <- model_results_20$net.result

predicted3 <- model_results_3$net.result*(max(dataPM2.5$PM2.5)-min(dataPM2.5$PM2.5))+min(dataPM2.5$PM2.5)
predicted5 <- model_results_5$net.result*(max(dataPM2.5$PM2.5)-min(dataPM2.5$PM2.5))+min(dataPM2.5$PM2.5)
predicted7 <- model_results_7$net.result*(max(dataPM2.5$PM2.5)-min(dataPM2.5$PM2.5))+min(dataPM2.5$PM2.5)
predicted9 <- model_results_9$net.result*(max(dataPM2.5$PM2.5)-min(dataPM2.5$PM2.5))+min(dataPM2.5$PM2.5)
predicted12 <- model_results_12$net.result*(max(dataPM2.5$PM2.5)-min(dataPM2.5$PM2.5))+min(dataPM2.5$PM2.5)
predicted15 <- model_results_15$net.result*(max(dataPM2.5$PM2.5)-min(dataPM2.5$PM2.5))+min(dataPM2.5$PM2.5)
predicted20 <- model_results_20$net.result*(max(dataPM2.5$PM2.5)-min(dataPM2.5$PM2.5))+min(dataPM2.5$PM2.5)

actual_test <- (dataPM2.5_test$PM2.5)*(max(dataPM2.5$PM2.5)-min(dataPM2.5$PM2.5))+min(dataPM2.5$PM2.5)

predicted3<-  ifelse(predicted3 < 0, 0, predicted3)
predicted5<-  ifelse(predicted5 < 0, 0, predicted5)
predicted7<-  ifelse(predicted7 < 0, 0, predicted7)
predicted9<-  ifelse(predicted9 < 0, 0, predicted9)
predicted12<-  ifelse(predicted12 < 0, 0, predicted12)
predicted15<-  ifelse(predicted15 < 0, 0, predicted15)
predicted20<-  ifelse(predicted20 < 0, 0, predicted20)

library(caret)
metrics_df<-rbind(cbind(Cor.Coeff=cor(predicted3,actual_test),
                        data.frame(as.list(postResample(predicted3,actual_test)))),
                  cbind(Cor.Coeff=cor(predicted5,actual_test),
                        data.frame(as.list(postResample(predicted5,actual_test)))),
                  cbind(Cor.Coeff=cor(predicted7,actual_test),
                        data.frame(as.list(postResample(predicted7,actual_test)))),
                  cbind(Cor.Coeff=cor(predicted9,actual_test),
                        data.frame(as.list(postResample(predicted9,actual_test)))),
                  cbind(Cor.Coeff=cor(predicted12,actual_test),
                        data.frame(as.list(postResample(predicted12,actual_test)))),
                  cbind(Cor.Coeff=cor(predicted15,actual_test),
                        data.frame(as.list(postResample(predicted15,actual_test)))),
                  cbind(Cor.Coeff=cor(predicted20,actual_test),
                        data.frame(as.list(postResample(predicted20,actual_test)))))



row.names(metrics_df)<-c("3 Neurons","5 Neurons","7 Neurons","9 Neurons","12 Neurons",
                         "15 Neurons","20 Neurons")


library(ggplot2)
#9 Neurons best performs
test_date<- seq(as.Date('2018-01-01'),as.Date('2018-12-31'),by = 1)
test_date<-as.POSIXct(strptime(test_date, format ="%Y-%m-%d" ))
#rm(plot_df)
predicted9<-as.numeric(predicted9)
#rm(plot_df)
plot_df<-data.frame(test_date,actual_test,predicted9)
colnames(plot_df)<-c("date","actual","predicted")


p_2.5_2<- ggplot(data = plot_df, aes(x = date)) +
  geom_line(aes(y = actual, colour = "Actual")) +
  geom_line(aes(y = predicted, colour = "Predicted"),lwd=1) +
  
  scale_colour_manual("", 
                      breaks = c("Actual", "Predicted"),
                      values = c("darkblue", "red")) +
  ylab("PM2.5") 

p_2.5_2



R2 <- caret::postResample(pred = predicted9, obs = actual_test)
r2 <- ggplot(plot_df, aes(actual, predicted))
r2 <- r2 + geom_point(alpha = .5)
r2 <- r2 + annotate(geom = "text", x=350,y=100,  label = paste("R**2 = ", round(R2[2], 3)))
r2 <- r2 + labs(x = "Actual", y = "Predicted", title = "Correlation Plot (CAMS-2)")
r2 <- r2 + geom_smooth(se=F, method = "lm")
r2




save(model_3,model_5,model_7,model_9,
     model_12,model_15,model_20,file = "NeuralNet_models.RData")

saveRDS(r2, file = "r2_2.rds")










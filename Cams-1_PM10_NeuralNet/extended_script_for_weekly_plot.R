Weekly_Means<-Weekly_Means[-1]
rm(wk_dataPM10)
wk_dataPM10<- Weekly_Means[-7]

normalize <- function(x) {
  return((x - min(x)) / (max(x) - min(x)))
}

wk_dataPM10_norm <- as.data.frame(lapply(wk_dataPM10, normalize))
wk_dataPM10_test <- wk_dataPM10_norm[214:265, ]

wk_model_results_3 <- compute(model_3, wk_dataPM10_test[1:5])
wk_model_results_5 <- compute(model_5, wk_dataPM10_test[1:5])
wk_model_results_7 <- compute(model_7, wk_dataPM10_test[1:5])
wk_model_results_9 <- compute(model_9, wk_dataPM10_test[1:5])
wk_model_results_12 <- compute(model_12, wk_dataPM10_test[1:5])
wk_model_results_15 <- compute(model_15, wk_dataPM10_test[1:5])
wk_model_results_20 <- compute(model_20, wk_dataPM10_test[1:5])

wk_predicted3 <- wk_model_results_3$net.result*(max(dataPM10$PM10)-min(dataPM10$PM10))+min(dataPM10$PM10)
wk_predicted5 <- wk_model_results_5$net.result*(max(dataPM10$PM10)-min(dataPM10$PM10))+min(dataPM10$PM10)
wk_predicted7 <- wk_model_results_7$net.result*(max(dataPM10$PM10)-min(dataPM10$PM10))+min(dataPM10$PM10)
wk_predicted9 <- wk_model_results_9$net.result*(max(dataPM10$PM10)-min(dataPM10$PM10))+min(dataPM10$PM10)
wk_predicted12 <- wk_model_results_12$net.result*(max(dataPM10$PM10)-min(dataPM10$PM10))+min(dataPM10$PM10)
wk_predicted15 <- wk_model_results_15$net.result*(max(dataPM10$PM10)-min(dataPM10$PM10))+min(dataPM10$PM10)
wk_predicted20 <- wk_model_results_20$net.result*(max(dataPM10$PM10)-min(dataPM10$PM10))+min(dataPM10$PM10)


wk_actual_test <- (wk_dataPM10_test$PM10)*(max(wk_dataPM10$PM10)-min(wk_dataPM10$PM10))+min(wk_dataPM10$PM10)

wk_predicted3<-  ifelse(wk_predicted3 < 0, 0, wk_predicted3)
wk_predicted5<-  ifelse(wk_predicted5 < 0, 0, wk_predicted5)
wk_predicted7<-  ifelse(wk_predicted7 < 0, 0, wk_predicted7)
wk_predicted9<-  ifelse(wk_predicted9 < 0, 0, wk_predicted9)
wk_predicted12<-  ifelse(wk_predicted12 < 0, 0, wk_predicted12)
wk_predicted15<-  ifelse(wk_predicted15 < 0, 0, wk_predicted15)
wk_predicted20<-  ifelse(wk_predicted20 < 0, 0, wk_predicted20)







#rm(wk_metrics_df)

wk_metrics_df<-rbind(cbind(Cor.Coeff=cor(wk_predicted3,wk_actual_test),
                           data.frame(as.list(postResample(wk_predicted3,wk_actual_test)))),
                     cbind(Cor.Coeff=cor(wk_predicted5,wk_actual_test),
                           data.frame(as.list(postResample(wk_predicted5,wk_actual_test)))),
                     cbind(Cor.Coeff=cor(wk_predicted7,wk_actual_test),
                           data.frame(as.list(postResample(wk_predicted7,wk_actual_test)))),
                     cbind(Cor.Coeff=cor(wk_predicted9,wk_actual_test),
                           data.frame(as.list(postResample(wk_predicted9,wk_actual_test)))),
                     cbind(Cor.Coeff=cor(wk_predicted12,wk_actual_test),
                           data.frame(as.list(postResample(wk_predicted12,wk_actual_test)))),
                     cbind(Cor.Coeff=cor(wk_predicted15,wk_actual_test),
                           data.frame(as.list(postResample(wk_predicted15,wk_actual_test)))),
                     cbind(Cor.Coeff=cor(wk_predicted20,wk_actual_test),
                           data.frame(as.list(postResample(wk_predicted20,wk_actual_test)))))



row.names(wk_metrics_df)<-c("3 Neurons","5 Neurons","7 Neurons","9 Neurons","12 Neurons",
                         "15 Neurons","20 Neurons")

#3neurons performs best
wk_test_date<- seq(as.Date('2018-01-01'),as.Date('2018-12-30'),by = 7)
wk_test_date<-as.POSIXct(strptime(wk_test_date, format ="%Y-%m-%d" ))
wk_predicted3<-as.numeric(wk_predicted3)

wk_plot_df<-data.frame(wk_test_date,wk_actual_test,wk_predicted3)
colnames(wk_plot_df)<-c("date","actual","predicted")

w_10_1<- ggplot(data = wk_plot_df, aes(x = date)) +
  geom_line(aes(y = actual, colour = "Actual")) +
  geom_line(aes(y = predicted, colour = "Predicted"),lwd=1) +
  
  scale_colour_manual("", 
                      breaks = c("Actual", "Predicted"),
                      values = c("darkblue", "red")) +
  labs( y = "PM 10", title = "Predicted vs Observed values of PM 10 (weekly avg.) during 2018 in CAMS-1")

w_10_1

# Save an object to a file
saveRDS(w_10_1, file = "w_10_1.rds")
saveRDS(p_10_1, file = "p_10_1.rds")
saveRDS(wk_metrics_df, file = "wk_metrics_df.rds")







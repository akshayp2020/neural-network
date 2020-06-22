install.packages("neuralnet")
library(neuralnet)
concrete<-read.csv(choose.files())
View(concrete)
summary(concrete)

##Normalize function
normalize<- function(x) {
  return((x-min(x))/(max(x)-min(x)))
}
concrete_norm<-as.data.frame(lapply(concrete,normalize))

##split function in train and test data
concrete_train<-concrete_norm[1:750,]
concrete_test<-concrete_norm[751:1030,]

##train model on data
## train neuralnet model
#simple ann with only a single hidden neuron
concret_model<-neuralnet(formula = strength~.,concrete_train)

#visualize network topology
plot(concret_model)

##Evaluating the model
##obtain model result
model_result<-compute(concret_model,concrete_test[1:8])

##obtain predicted strength value
predicted_strength <-model_result$net.result

## Examine the correlation between predicted and actual vale
cor(predicted_strength,concrete_test$strength)

#improve model performance
concrete_model2<-neuralnet(formula = strength~.,concrete_train,hidden = c(4,2))
plot(concrete_model2)

#obtain model result
model_result2<-compute(concrete_model2,concrete_test[1:8])
predicted_strength2<-model_result2$net.result
cor(predicted_strength2,concrete_test$strength)

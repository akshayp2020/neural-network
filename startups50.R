install.packages("neuralnet")
library(neuralnet)

Startups<-read.csv(choose.files())
View(Startups)
startups<-Startups[,-4]
View(Startups)
#normalize function
normalize <- function(x){
  return((x-min(x))/(max(x)-min(x)))
}


startups_norm <-as.data.frame(lapply(startups,normalize))

##Divide date in training and testing
startups_train<-startups_norm[1:30,]
startups_test<-startups_norm[31:50,]


#simple ann with only a single hidden neuron
startups_model1<-neuralnet(formula= Profit~R.D.Spend+Administration+Marketing.Spend,data=startups_train)
View(startups_model1)

#Visualize network topology
plot(startups_model1)


#Evaluation the model
#obtain model results
model_results<-compute(startups_model1,startups_test[1:3])

#obtain predict strength values
predicted_profit<-model_results$net.result

#examine the corelation between predicted and actual values
cor(predicted_profit,startups_test$Profit)


#improve model performance
startups_model2<-neuralnet(formula = Profit~R.D.Spend+Administration+Marketing.Spend,data=startups_train,hidden = c(3,3,2))
plot(startups_model2)

##Evaluating the model
model_results2<-compute(startups_model2,startups_test[1:3])
predicted_profit2<-model_results2$net.result
cor(predicted_profit2,startups_test$Profit)


#imporove model performance
startups_model3<-neuralnet(formula = Profit~R.D.Spend+Administration+Marketing.Spend,data = startups_train,hidden = c(4,3,2))
plot(startups_model3)
model_result3<-compute(startups_model3,startups_test[1:3])
predicted_profit3<-model_result3$net.result
cor(predicted_profit3,startups_test$Profit)

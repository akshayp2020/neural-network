fireforest <-read.csv(choose.files())
View(fireforest)

install.packages("neuralnet")
library(neuralnet)

##Normalize function
normalize<-function(x){
  return((x)-min(x)/(max(x)-min(x)))
}

fireforest_norm<-as.data.frame(lapply(fireforest[3:30],normalize))

## split in train and test
train<-fireforest_norm[1:470,]
test<-fireforest_norm[471:517,]

library(neuralnet)

#simple ann with only a single hidden neuron
forestmodel<-neuralnet(formula = area~.,data=train)

#visualize network topology
plot(forestmodel)

##Evaluating the model
##Obtaining model results
model_results<-compute(forestmodel,test)

##Obtaining the presicted area
predicted_area<-model_results$net.result

#Examaine correlation
cor(predicted_area,test$area)

## improve model performance
forestmodel1<-neuralnet(formula = area~.,data = train,hidden = c(5,2))
plot(forestmodel1)

### evaluating model performance
model_results1<-compute(forestmodel1,test)
predicted_model1<-model_results1$net.result
cor(predicted_model1,test$area)

## improve model performance
forestmodel2<-neuralnet(formula = area~.,data = train,hidden = c(5,2,1))
plot(forestmodel2)

### evaluating model performance
model_results1<-compute(forestmodel2,test)
predicted_model1<-model_results1$net.result
cor(predicted_model1,test$area)


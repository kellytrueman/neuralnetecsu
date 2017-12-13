# data from https://datahub.io/core/global-temp

apply(monthly,2,function(x) sum(is.na(x))) #check for missing values
set.seed(3300)

#splitting data into training and testing
index = sample(1:nrow(monthly), round(0.75*nrow(monthly))) #index is essentially a random vector of indeces
train = monthly[index,]
test = monthly[-index,]

#  creating linear model
lm.fit = glm(Mean~., data = train)
summary(lm.fit)
pr.lm = predict(lm.fit, test)
#  Mean Squared Error to measure how much the prdictions are far away from the real data
MSE.lm = sum((pr.lm - test$Mean)^2)/nrow(test) #linear model for predicting

m2 = model.matrix(~Mean + Source + Date, data = train)

maxs = apply(m2, 2, max)
mins = apply(m2, 2, min)

scaled = as.data.frame(scale(m2, center = mins, scale = maxs - mins))
train_ = scaled[index,]
test_ = scaled[-index,]

library(neuralnet)
n = names(train_)

#hidden allows for a vector for the number of nodes in each layer of the network
#linear.output is if we want to do regression

# first configuration
nn1 = neuralnet(Mean ~ SourceGISTEMP + Date, data = train_, hidden = c(3,2), 
            linear.output = T)

# final configuration
nn2 = neuralnet(Mean ~ SourceGISTEMP + Date, data = train_, hidden = c(5,3), 
               linear.output = T)

plot(nn1) #shows neural network connections and weights
plot(nn2)

#predicting values
pr.nn1 = compute(nn1,test_[,3:4])
pr.nn2 = compute(nn2,test_[,3:4])

#scaling output because nerural network normalizes predictions
#need to scale for meaningful comparison
pr.nn1_ = pr.nn1$net.result*(max(monthly$Mean)-min(monthly$Mean)) + min(monthly$Mean)
pr.nn2_ = pr.nn2$net.result*(max(monthly$Mean)-min(monthly$Mean)) + min(monthly$Mean)
test.r = (test_$Mean)*(max(monthly$Mean)-min(monthly$Mean)) + min(monthly$Mean)                                                                

#mean squared error of neural networks
MSE.nn1 = sum((test.r - pr.nn1_)^2)/nrow(test_) 
MSE.nn2 = sum((test.r - pr.nn2_)^2)/nrow(test_) 

#comparing the linear model to the neural networks using mean squared error
print(paste(MSE.lm, MSE.nn1, MSE.nn2))

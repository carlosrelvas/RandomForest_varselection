setwd("C:/Users/Cacá Relvas/Desktop/blog/")

data = read.table("spambase.data", sep=",", header=F)
id = sample(1:nrow(data), 3000)
data.train = data[id,]
data.test = data[-id,]

formula = paste0("as.factor(V58) ~ ", paste0(paste0("V", 1:57), collapse="+"))

install.packages("ROCR")

diag = function(obs, prev){
  library(ROCR)
  pred = prediction(prev, obs)
  perf = performance(pred,"auc")
  GINI=2*attr(perf,'y.values')[[1]]-1
  return(GINI)
}

install.packages("randomForest")
library(randomForest)

ptm <- proc.time()
rf = randomForest(as.formula(formula), data=data.train, ntree=500, importance=TRUE)
proc.time() - ptm

#usuário   sistema decorrido 
#34.55      0.00     34.56 

vars.rank = names(sort(rf$importance[,"MeanDecreaseGini"], decreasing=T))
plot(sort(rf$importance[,"MeanDecreaseGini"], decreasing=T), xlab="Variable",
     ylab="MeanDecreaseGini", pch=16, main="Variable importance")

formula.rf = paste0("as.factor(V58) ~ ", paste0(paste0(vars.rank[1:16]),
                                                collapse="+"))
fit.rf = glm(as.formula(formula.rf), family=binomial, data=data.train)
diag(data.train[,"V58"], predict(fit.rf, newdata=data.train))
diag(data.test[,"V58"], predict(fit.rf, newdata=data.test))

#[1] 0.9339169
#[1] 0.922368

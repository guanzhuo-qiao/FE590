library(Quandl)
library(quantmod)
library(leaps)
library(splines)
library(gam)
library(MASS)
library(tree)
library(randomForest)
library(boot)
library(gbm)
library(class)
library(e1071)
library(glmnet)

setwd("D:/Grad 2/590/590Assignments/HW4/data")
rm(list = ls())
Quandl.api_key("xm1mMba7kjPFRVvxmzdM")
sugar <- Quandl("CHRIS/ICE_SB1",start_date = "2011-01-01")
ethanol <- Quandl("CHRIS/CME_CU11",start_date = "2011-01-01")
getSymbols("THB=X",from = "2011-01-01")
getSymbols("INR=X",from = "2011-01-01")
getSymbols("BRL=X",from = "2011-01-01")
stock_list = c("CZZ","MDLZ","TR","HSY","RMCF",
               "BRID",
               "CALM",
               "CVGW",
               "JVA",
               "FARM",
               "FLO",
               "FDP",
               "HAIN",
               "JJSF",
               "JBSS",
               "LANC",
               "MTEX",
               "MKC",
               "NAII",
               "NATR",
               "POST",
               "RELV",
               "RIBT",
               "STKL",
               'SJM')
for (i in stock_list){
  getSymbols(i,from = "2011-01-01")
  write.csv(as.data.frame(get(i)),paste(i,".csv",sep = ""),row.names = T)
}
write.csv(sugar,"sugar.csv")
write.csv(ethanol,"ethanol.csv")
write.csv(as.data.frame(`BRL=X`),"BRL_USD.csv",row.names = T)
write.csv(as.data.frame(`THB=X`),"THB_USD.csv",row.names = T)
write.csv(as.data.frame(`INR=X`),"INR_USD.csv",row.names = T)
rm(list = ls())

stock_list = c("CZZ","MDLZ","TR","HSY","RMCF",
               "BRID",
               "CALM",
               "CVGW",
               "JVA",
               "FARM",
               "FLO",
               "FDP",
               "HAIN",
               "JJSF",
               "JBSS",
               "LANC",
               "MTEX",
               "MKC",
               "NAII",
               "NATR",
               "POST",
               "RELV",
               "RIBT",
               "STKL",
               'SJM')
for (i in list.files()){
  variable_name <- strsplit(i,".csv")[[1]]
  assign(variable_name,read.csv(i))
}
Datetime <- sort(Reduce(intersect, list(sugar$Date,ethanol$Date,BRL_USD$X,INR_USD$X,THB_USD$X)))
for (i in stock_list){
  Datetime <- sort(intersect(Datetime,get(i)$X))
}
all_data <- as.data.frame(Datetime)

for (i in stock_list){
  all_data <- merge(all_data,get(i)[,c("X",paste(i,".Adjusted",sep = ""))],by.x = "Datetime",by.y = "X")
}
all_data <- merge(all_data,BRL_USD[,c("X","BRL.X.Adjusted")],by.x = "Datetime",by.y = "X")
all_data <- merge(all_data,INR_USD[,c("X","INR.X.Adjusted")],by.x = "Datetime",by.y = "X")
all_data <- merge(all_data,THB_USD[,c("X","THB.X.Adjusted")],by.x = "Datetime",by.y = "X")
all_data <- merge(all_data,ethanol[,c("Date","Settle")],by.x = "Datetime",by.y = "Date")
all_data <- merge(all_data,sugar[,c("Date","Settle")],by.x = "Datetime",by.y = "Date")




rownames(all_data) <- all_data$Datetime
all_data$Datetime <- NULL

colnames(all_data) <- c(stock_list,c("BRL","INR","THB"),c("ethanol","sugar"))
all_data = na.omit(all_data)

write.csv(all_data,"all_data.csv")
#############################################################################################################################
rm(list = ls())
all_data <- read.csv("all_data.csv",row.names = "X")
all_data <- log(all_data[c(2:nrow(all_data)),]/all_data[c(1:nrow(all_data)-1),])
all_data <- cbind(all_data[c(1:dim(all_data)[1]-1),c(1:dim(all_data)[2]-1)],all_data[c(2:dim(all_data)[1]),dim(all_data)[2]])
colnames(all_data)[ncol(all_data)] <- c("sugar")


set.seed(1)

train <- sample(nrow(all_data),floor(nrow(all_data)/2))
trainset <- all_data[train,]
testset <- all_data[-train,]
  

  
# Multiple Linear Regression (lasso and ridge)
# subset selection
subsets=regsubsets(sugar~.,data=trainset,method="exhaustive",nvmax=30)
summary(subsets)
# number of the variable
regfit.bwd.summary = summary(subsets)
par(mfrow=c(1,3))
plot(regfit.bwd.summary$adjr2,xlab="Number of Variables",ylab="Adjusted RSq",type="l")
points(which.max(regfit.bwd.summary$adjr2)
       ,regfit.bwd.summary$adjr2[which.max(regfit.bwd.summary$adjr2)]
       ,col="red",cex=2,pch=20)

plot(regfit.bwd.summary$cp,xlab="Number of Variables",ylab="Cp",type="l")
points(which.min(regfit.bwd.summary$cp)
       ,regfit.bwd.summary$cp[which.min(regfit.bwd.summary$cp)]
       ,col="red",pch=20,cex=2)

plot(regfit.bwd.summary$bic,xlab="Number of Variables",ylab="BIC",type='l')
points(which.min(regfit.bwd.summary$bic)
       ,regfit.bwd.summary$bic[which.min(regfit.bwd.summary$bic)]
       ,col="red",pch=20,cex=2)

fit1 = lm(sugar~FARM+CALM+NATR+INR+FDP+HAIN+RIBT+NAII,data = trainset)
summary((fit1))
prediction1 = predict(fit1,testset)
mse1 = mean((prediction1-testset$sugar)^2)

# lasso
x_train = as.matrix(trainset[,-1])
y_train = trainset$sugar
x_test = as.matrix(testset[,-1])
y_test = testset$sugar

lasso.mod = glmnet(x_train,y_train,alpha = 1)
set.seed(1)
cv.out = cv.glmnet(x_train,y_train,alpha=1)
bestlam = cv.out$lambda.min
lasso.pred = predict(lasso.mod,s = bestlam,newx = x_test)
mse11 = mean((lasso.pred-y_test)^2)

# ridge

ridge.mod = glmnet(x_train,y_train,alpha = 0)
set.seed(1)
cv.out2 = cv.glmnet(x_train,y_train,alpha=0)
bestlam2 = cv.out2$lambda.min
ridge.pred = predict(ridge.mod,s = bestlam2,newx = x_test)
mse12 = mean((ridge.pred-y_test)^2)





# Polynomial Regression (try intersection)
subsets2=regsubsets(sugar~poly(CZZ,3,raw = T)+poly(MDLZ,3,raw = T)+poly(TR,3,raw = T)+poly(HSY,3,raw = T)+poly(RMCF,3,raw = T)+poly(BRID,3,raw = T)
                    +poly(CALM,3,raw = T)+poly(CVGW,3,raw = T)+poly(JVA,3,raw = T)+poly(FARM,3,raw = T)+poly(FLO,3,raw = T)+poly(FDP,3,raw = T)
                    +poly(HAIN,3,raw = T)+ poly(JJSF,3,raw = T)+ poly(JBSS,3,raw = T)+ poly(LANC,3,raw = T)+ poly(MTEX,3,raw = T)+ poly(MKC,3,raw = T)
                    + poly(NAII,3,raw = T)+ poly(NATR,3,raw = T)+ poly(POST,3,raw = T)+ poly(RELV,3,raw = T)+ poly(RIBT,3,raw = T)+ poly(STKL,3,raw = T)
                    + poly(SJM,3,raw = T)+ poly(BRL,3,raw = T)+ poly(INR,3,raw = T)+ poly(THB,3,raw = T)+ poly(ethanol,3,raw = T),data=trainset,method="forward",nvmax=20)
summary(subsets2)
plot(summary(subsets2)$cp,xlab="Number of Variables",ylab="Cp",type="l")
points(which.min(summary(subsets2)$cp)
       ,summary(subsets2)$cp[which.min(summary(subsets2)$cp)]
       ,col="red",cex=2,pch=20)
plot(summary(subsets2)$adjr2,xlab="Number of Variables",ylab="Adjusted RSq",type="l")
points(which.max(summary(subsets2)$adjr2)
       ,summary(subsets2)$adjr2[which.max(summary(subsets2)$adjr2)]
       ,col="red",cex=2,pch=20)
fit2 = lm(sugar~FARM+poly(CALM,2,raw=T)+NATR+INR+poly(FDP,2,raw = T)+I(BRL^2),data = trainset)
summary(fit2)

prediction2 = predict(fit2,testset)
mse2 = mean((prediction2-testset$sugar)^2)

# GAM (try different combination and local regression)
fit31 = gam(sugar~s(FARM,3)+s(CALM,3)+s(NATR,3)+s(INR,3),data=trainset)
fit32 = gam(sugar~s(FARM,3)+s(CALM,3)+s(NATR,3)+s(INR,3)+s(FDP,3)+s(BRL,3),data=trainset)
fit33 = gam(sugar~s(FARM,3)+s(CALM,3)+s(CALM^2,3)+s(NATR,3)+s(INR,3),data=trainset)
fit34 = gam(sugar~s(FARM,3)+s(CALM,3)+s(CALM^2,3)+s(NATR,3)+s(INR,3)+s(FDP,3)+s(FDP^2,3),data=trainset)
fit35 = gam(sugar~s(FARM,3)+s(CALM,3)+s(CALM^2,3)+s(NATR,3)+s(INR,3)+s(FDP,3)+s(FDP^2,3)+s(BRL^2,3),data=trainset)
fit36 = gam(sugar~s(FARM,3)+s(CALM,3)+s(NATR,3)+s(INR,3)+s(FDP,3)+s(BRL,3)+lo(CALM,span = 0.5)+lo(FDP,span = 0.5),data=trainset)
anova(fit3,fit32,fit33,fit34,fit35)

prediction31 = predict(fit31,testset)
mse31 = mean((prediction31-testset$sugar)^2)
prediction32 = predict(fit32,testset)
mse32 = mean((prediction32-testset$sugar)^2)
prediction33 = predict(fit33,testset)
mse33 = mean((prediction33-testset$sugar)^2)
prediction34 = predict(fit34,testset)
mse34 = mean((prediction34-testset$sugar)^2)
prediction35 = predict(fit35,testset)
mse35 = mean((prediction35-testset$sugar)^2)

# Regression Tree
# normal tree
fit4 = tree(sugar~.,data = trainset)
summary(fit4)
prediction4 = predict(fit4,testset)
mse4 = mean((prediction4-testset$sugar)^2)
#pruned tree 
set.seed(1)
cv_41 = cv.tree(fit4)
plot(cv_41$size,cv_41$dev,type = "b")
best_ = cv_41$size[which.min(cv_41$dev)]
points(best_,cv_41$dev[which.min(cv_41$dev)],col="red")
fit42=prune.tree(fit4,best=1)
summary(fit42)
prediction42 = predict(fit42,testset)
mse42 = mean((prediction42-testset$sugar)^2)
#boosting
means_43=rep(0,99)
set.seed(1)
pows <- seq(-10, -0.2, by = 0.1)
lambdas <- 10^pows
for (i in 1:99){
  fit43=gbm(sugar~.,data = trainset,distribution = "gaussian"
               , n.trees = 1000, interaction.depth = 2,shrinkage = lambdas[i])
  pred43=predict(fit43,newdata=trainset,n.trees = 1000)
  means_43[i] = mean((trainset$sugar-pred43)^2)
}
plot(lambdas,means_43,type='b',xlab = "Shrinkage values", ylab = "Training MSE")

fit43 = gbm(sugar~.,data = trainset,distribution = "gaussian"
            , n.trees = 1000, interaction.depth = 2,shrinkage = 0.2)
prediction43 = predict(fit43,testset,n.trees = 1000)
mse43 = mean((prediction43-testset$sugar)^2)
#bagging
set.seed(1)
fit44 = randomForest(sugar~.,data = trainset,mtry = 29,importance = T)
prediction44 = predict(fit44,testset)
mse44 = mean((prediction44-testset$sugar)^2)


#Classification
# data convert
# logisitc regression
clas_data <- all_data
sugar_der <- rep(1,nrow(clas_data))
sugar_der[which(clas_data$sugar>=0)] = "up"
sugar_der[which(clas_data$sugar<0)] = "down"
sugar_der <- as.factor(sugar_der)
clas_data$sugar <- sugar_der
clas_train <- sample(nrow(clas_data),floor(nrow(clas_data)/2))
clas_trainset <- clas_data[clas_train,]
clas_testset <- clas_data[-clas_train,]


logfit=glm(sugar~.,data=clas_trainset,family=binomial)
summary(logfit)
result1 = predict(logfit,clas_testset,type = "response")
test_predict1 = rep(1,nrow(clas_testset))
test_predict1[which(result1>=0.5)] <- "up"
test_predict1[which(result1<0.5)] <- "down"
res_table1 = table(test_predict1,clas_testset$sugar)
(res_table1[1,1]+res_table1[2,2])/sum(res_table1)

#LDA/QDA
ldafit=lda(sugar~.,data=clas_trainset)
result2 = predict(ldafit,clas_testset,type = "response")$class
res_table2 = table(result2,clas_testset$sugar)
(res_table2[1,1]+res_table2[2,2])/sum(res_table2)


qdafit = qda(sugar~.,data=clas_trainset)
result3 = predict(qdafit,clas_testset,type = "response")$class
res_table3 = table(result3,clas_testset$sugar)
(res_table3[1,1]+res_table3[2,2])/sum(res_table3)

# KNN (different k)
set.seed(1)
knnfit1=knn(clas_trainset[,c(1:ncol(clas_trainset)-1)],clas_testset[,c(1:ncol(clas_testset)-1)],clas_testset$sugar,k=3)
res_table4 = table(knnfit1,clas_testset$sugar)
(res_table4[1,1]+res_table4[2,2])/sum(res_table4)

# SVM (different kernel)
set.seed(1)
tune.out = tune(svm,sugar~.,data = clas_trainset,kernel="linear",
                ranges = list(cost=c(0.001,0.01,0.1,1.5,10,100)))
summary(tune.out)#0.01
bestmod = tune.out$best.model
result4 = predict(bestmod,clas_testset)
res_table5 = table(result4,clas_testset$sugar)
(res_table5[1,1]+res_table5[2,2])/sum(res_table5)














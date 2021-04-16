data=read.csv("D:/大数据学院文件资料/2020秋课程/机器学习/assignment/homework6/simudata.csv")

boxplot(billnum~black,data=data,col=c("dodgerblue","coral2"),border=c("dimgray","dimgrey"),
        main='交易笔数~是否违约的箱线图',names=c("不违约","违约"),xlab="是否违约",ylab="交易笔数",plot=T)

boxplot(meanpay~black,data=data,col=c("dodgerblue","coral2"),border=c("dimgray","dimgrey"),
        main='所有行为均值~是否违约的箱线图',names=c("不违约","违约"),xlab="是否违约",ylab="所有行为均值",plot=T)



library("caTools")
set.seed(1234)
split = sample.split(data$black,SplitRatio = .7)
train = subset(data,split == TRUE)
test  = subset(data,split == FALSE)

set.seed(1234)
index <- sample(1:nrow(data), round(nrow(data)*7/10)) 
train <- data[index,] 
test <- data[-index,] 


set.seed(1234)
split=sample(2,nrow(data),replace=T,prob=c(0.7,0.3))
train=data[split==1,]
test=data[split==2,]


library("rpart")
model=rpart(black~.,data=train,method="class")

pred=predict(model,test,type="class")
pred
(sum(pred==test$black))/nrow(test)
  
library(rpart.plot)
rpart.plot(model)


library("pROC")

roc(test$black,as.numeric(pred)-1,plot=TRUE,main="测试集ROC曲线",xlab = "FPR", ylab = "TPR",
    print.thres=TRUE,print.auc=TRUE,legacy.axes=TRUE,grid=c(0.2,0.2),
    grid.col="dimgray",auc.polygon=TRUE,max.auc.polygon=TRUE,auc.polygon.col="darkslategray1",max.auc.polygon.col="deepskyblue")


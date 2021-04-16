# Problem 4 

# (1) read data
train=read.csv("D:/大数据学院文件资料/2020秋课程/机器学习/sampledata.csv")
test=read.csv("D:/大数据学院文件资料/2020秋课程/机器学习/preddata.csv")

# (2) box plot
boxplot(log(tenure)~churn,data=train,col=c("dodgerblue","coral2"),border=c("dimgray","dimgrey"),
        main='log(tenure)~churn箱线图',names=c("churn=0","churn=1"),xlab="churn or not",ylab="log(tenure)",plot=T)

boxplot(expense~churn,data=train,col=c("dodgerblue","coral2"),border=c("dimgray","dimgrey"),
        main='expense~churn箱线图',names=c("churn=0","churn=1"),xlab="churn or not",ylab="expense",plot=T)

boxplot(log(degree)~churn,data=train,col=c("dodgerblue","coral2"),border=c("dimgray","dimgrey"),
        main='log(degree)~churn箱线图',names=c("churn=0","churn=1"),xlab="churn or not",ylab="log(degree)",plot=T)

boxplot(log(tightness)~churn,data=train,col=c("dodgerblue","coral2"),border=c("dimgray","dimgrey"),
        main='log(tightness)~churn箱线图',names=c("churn=0","churn=1"),xlab="churn or not",ylab="log(tightness)",plot=T)

boxplot(entropy~churn,data=train,col=c("dodgerblue","coral2"),border=c("dimgray","dimgrey"),
        main='entropy~churn箱线图',names=c("churn=0","churn=1"),xlab="churn or not",ylab="entropy",plot=T)

boxplot(chgdegree~churn,data=train,col=c("dodgerblue","coral2"),border=c("dimgray","dimgrey"),
        main='chgdegree~churn箱线图',names=c("churn=0","churn=1"),xlab="churn or not",ylab="chgdegree",plot=T)

boxplot(chgexpense~churn,data=train,col=c("dodgerblue","coral2"),border=c("dimgray","dimgrey"),
        main='chgexpense~churn箱线图',names=c("churn=0","churn=1"),xlab="churn or not",ylab="chgexpense",plot=T)

train=read.csv("D:/大数据学院文件资料/2020秋课程/机器学习/sampledata.csv")
test=read.csv("D:/大数据学院文件资料/2020秋课程/机器学习/preddata.csv")

# (3)
train[2:8]=scale(train[2:8])
reg_log=glm(churn~tenure+expense+degree+tightness+entropy+chgexpense+chgdegree,data=train,family=binomial(link="logit")
)
summary(reg_log)

# (4)
train_pred=predict.glm(reg_log,newdata=train,type="response")
head(train_pred)
test=read.csv("D:/大数据学院文件资料/2020秋课程/机器学习/preddata.csv")
test[2:8]=scale(test[2:8])
test_pred=predict.glm(reg_log,newdata=test,type="response")
head(test_pred)

# (5)
library("pROC")

roc(train$churn,train_pred,plot=TRUE,main="训练集ROC曲线",xlab = "FPR", ylab = "TPR",
    print.thres=TRUE,print.auc=TRUE,legacy.axes=TRUE,grid=c(0.2,0.2),
    grid.col="dimgray",auc.polygon=TRUE,max.auc.polygon=TRUE,auc.polygon.col="darkslategray1",max.auc.polygon.col="deepskyblue")

roc(test$churn,test_pred,plot=TRUE,main="测试集ROC曲线",xlab = "FPR", ylab = "TPR",
    print.thres=TRUE,print.auc=TRUE,legacy.axes=TRUE,grid=c(0.2,0.2),
    grid.col="dimgray",auc.polygon=TRUE,max.auc.polygon=TRUE,auc.polygon.col="darkslategray1",max.auc.polygon.col="deepskyblue")

AUC_c = function(TP, FP){
        lrank <- 0
        for (i in 1:length(TP))
        {
                lrank = lrank+1*sum(FP>TP[i]) + 0.5*sum(FP == TP[i])
        }
        lrank = 1 - lrank/ (length(TP) * length(FP))
        return(lrank)
}

train_TP = train_pred[(train$churn==1)]
train_FP= train_pred[(train$churn==0)]
AUC_c(train_TP,train_FP)

test_TP = test_pred[(test$churn==1)]
test_FP= test_pred[(test$churn==0)]
AUC_c(test_TP,test_FP)


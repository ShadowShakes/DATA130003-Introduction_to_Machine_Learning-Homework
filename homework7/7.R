data=read.csv("D:/大数据学院文件资料/2020秋课程/机器学习/assignment/homework7/simudata.csv")


library("caTools")
library("pROC")
library("caret")
library("rpart")

set.seed(1234)
split=sample(2,nrow(data),replace=T,prob=c(0.7,0.3))
train=data[split==1,]
test=data[split==2,]


# logistic
# train
train_logis=train
train_logis[1:26]=scale(train_logis[1:26])
reg_log=glm(black~.,data=train_logis,family=binomial(link="logit")
)
summary(reg_log)

# predict
test_logis=test
test_logis[1:26]=scale(test_logis[1:26])
test_logis_pred=predict.glm(reg_log,newdata=test_logis,type="response")

# roc and auc
roc(test$black,as.numeric(test_logis_pred)-1,plot=TRUE,main="逻辑回归模型测试集ROC曲线",xlab = "FPR", ylab = "TPR",
    print.thres=TRUE,print.auc=TRUE,legacy.axes=TRUE,grid=c(0.2,0.2),
    grid.col="dimgray",auc.polygon=TRUE,max.auc.polygon=TRUE,auc.polygon.col="darkslategray1")

test_logis_pred=test_logis_pred>0.5
(sum(test_logis_pred==test_logis$black))/(length(test_logis_pred))
confusionMatrix(as.factor(as.numeric(test_logis_pred)), as.factor(test_logis$black))



# knn
# train
library("kknn")
train_knn=train
train_knn[1:26]=scale(train_knn[1:26])
test_knn=test
test_knn[1:26]=scale(test_knn[1:26])
model_knn=kknn(black~.,train_knn,test_knn,k=6)

# predict
test_knn_pred=fitted(model_knn)
test_knn_pred=test_knn_pred>0.5
(sum(as.numeric(test_knn_pred)==test_knn$black))/(nrow(test_knn))
confusionMatrix(as.factor(as.numeric(test_knn_pred)), as.factor(test_knn$black))

# roc and auc
roc(test_knn$black,as.numeric(test_knn_pred)-1,plot=TRUE,main="KNN模型测试集ROC曲线",xlab = "FPR", ylab = "TPR",
    print.thres=TRUE,print.auc=TRUE,legacy.axes=TRUE,grid=c(0.2,0.2),
    grid.col="dimgray",auc.polygon=TRUE,max.auc.polygon=TRUE,auc.polygon.col="darkslategray1")







# decision tree
# train
train_dt=train 
# train_dt[1:26]=scale(train_dt[1:26])  the result is better without scale
model_dt=rpart(black~.,data=train_dt,method="class")

# predict
test_dt=test
# test_dt[1:26]=scale(test_dt[1:26])   the result is better without scale
test_dt_pred=predict(model_dt,test_dt,type="class")
(sum(test_dt_pred==test_dt$black))/nrow(test_dt)
confusionMatrix(as.factor(test_dt_pred), as.factor(test_dt$black))

library(rpart.plot)
rpart.plot(model_dt)

test_pred_dt=predict(model_dt,test,type="class")
(sum(test_pred_dt==test_dt$black))/nrow(test_dt)

# roc and auc
library(rpart.plot)
rpart.plot(model)

library("pROC")
roc(test_dt$black,as.numeric(test_pred_dt)-1,plot=TRUE,main="决策树测试集ROC曲线",xlab = "FPR", ylab = "TPR",
    print.thres=TRUE,print.auc=TRUE,legacy.axes=TRUE,grid=c(0.2,0.2),
    grid.col="dimgray",auc.polygon=TRUE,max.auc.polygon=TRUE,auc.polygon.col="darkslategray1")



# boosting
library("adabag")
# train
train_bt=train 
train_bt[1:26]=scale(train_bt[1:26])
train_bt$black=as.factor(train_bt$black)
model_bt=boosting(black~.,data=train_bt)

# predict
test_bt=test
test_bt[1:26]=scale(test_bt[1:26])  
test_bt_pred=predict(model_bt,test_bt)
(sum(test_bt_pred$class==test_bt$black))/nrow(test_bt)
confusionMatrix(as.factor(as.numeric(test_bt_pred$class)), as.factor(test_bt$black))

# roc and auc
roc(test_bt$black,as.numeric(test_bt_pred$class),plot=TRUE,main="Boosting测试集ROC曲线",xlab = "FPR", ylab = "TPR",
    print.thres=TRUE,print.auc=TRUE,legacy.axes=TRUE,grid=c(0.2,0.2),
    grid.col="dimgray",auc.polygon=TRUE,max.auc.polygon=TRUE,auc.polygon.col="darkslategray1")







# random forest
# train
library("randomForest")
train_rf=train
train_rf[1:26]=scale(train_rf[1:26])
model_rf=randomForest(as.factor(train_rf$black)~.,data=train_rf,importance=T)
importance(model_rf,type=1)

# predict
test_rf=test
test_rf[1:26]=scale(test_rf[1:26])
test_pred_rf=predict(model_rf,test_rf)
(sum(test_pred_rf==test_rf$black))/nrow(test_rf)
confusionMatrix(as.factor(test_pred_rf), as.factor(test_rf$black))

# roc and auc
roc(test_rf$black,as.numeric(test_pred_rf)-1,plot=TRUE,main="随机森林模型测试集ROC曲线",xlab = "FPR", ylab = "TPR",
    print.thres=TRUE,print.auc=TRUE,legacy.axes=TRUE,grid=c(0.2,0.2),
    grid.col="dimgray",auc.polygon=TRUE,max.auc.polygon=TRUE,auc.polygon.col="darkslategray1")




# svm
# train
library("car")
library("e1071")
train_svm=train
train_svm[1:26]=scale(train_svm[1:26])
model_svm=svm(train_svm$black~.,data=train_svm,type="C-classification")

# predict
test_svm=test
test_svm[1:26]=scale(test_svm[1:26])
test_pred_svm=predict(model_svm,test_svm)
(sum(test_pred_svm==test_svm$black))/nrow(test_svm)
confusionMatrix(as.factor(test_pred_svm), as.factor(test_svm$black))

# roc and auc
roc(test_svm$black,as.numeric(test_pred_svm)-1,plot=TRUE,main="SVM模型测试集ROC曲线",xlab = "FPR", ylab = "TPR",
    print.thres=TRUE,print.auc=TRUE,legacy.axes=TRUE,grid=c(0.2,0.2),
    grid.col="dimgray",auc.polygon=TRUE,max.auc.polygon=TRUE,auc.polygon.col="darkslategray1")




roc1=roc(test$black,as.numeric(test_logis_pred)-1,plot=TRUE,main="测试集ROC曲线",xlab = "FPR", ylab = "TPR",col='red',
         legacy.axes=TRUE,grid=c(0.2,0.2),max.auc.polygon=TRUE)
text(-0.2,0.8,"logistic",col='red')
roc2=roc(test_knn$black,as.numeric(test_knn_pred)-1)
plot.roc(roc2, add=TRUE, col="black",)
text(-0.18,0.7,"knn",col='black')
roc3=roc(test_dt$black,as.numeric(test_pred_dt)-1)
plot.roc(roc3,add=TRUE,col='green')
text(-0.2,0.6,"decision tree",col='green')
roc4=roc(test_bt$black,as.numeric(test_bt_pred$class))
plot.roc(roc4,add=TRUE,col='purple')
text(-0.2,0.5,"Boosting",col='purple')
roc5=roc(test_rf$black,as.numeric(test_pred_rf)-1)
plot.roc(roc5,add=TRUE,col='blue')
text(-0.18,0.4,"random forest",col='blue')
roc6=roc(test_svm$black,as.numeric(test_pred_svm)-1)
plot.roc(roc6,add=TRUE,col='orange')
text(-0.2,0.3,"SVM",col='orange')

barplot(c(1/-log(0.7694),1/-log(0.6833),1/-log(0.6968),1/-log(0.7580),1/-log(0.7479),1/-log(0.7593)),names.arg=c('log','knn','decision tree','boosting','randomforest','svm'),xlab="不同模型",ylab="1/-log(accuracy)",col="coral",ylim=c(0,4),main="不同模型的准确度对比")


# Problem 2

train=read.csv("D:/大数据学院文件资料/2020秋课程/机器学习/assignment/homework5/市长电话分析/train_set.csv")
val=read.csv("D:/大数据学院文件资料/2020秋课程/机器学习/assignment/homework5/市长电话分析/test_set.csv")

library("sqldf")
names=train$单位名称
names=data.frame(names)
a=sqldf("select names as 政府单位 from names group by names")
a=as.vector(as.matrix(a))
b=numeric(length(a))
for (i in 1:length(a)){
  b[i]=sum(names==a[i])
}
count1=cbind(a,b)
o=rank(as.numeric(count1[,2]))
c=numeric(length(a))
d=numeric(length(a))
for (i in 1:length(a)){
  c[8-o[i]]=a[i]
  d[8-o[i]]=b[i]
}
单位名称=c;投诉数量=d
count1=cbind(单位名称,投诉数量);count1

num=table(train$单位名称)
num=sort(num,decreasing = TRUE)
num
colors=colorRampPalette(c("coral2", "gold"))(length(num))
bar=barplot(num,col=colors,main="政府单位投诉量降序柱状图",
            xlab="政府单位名称",ylab="投诉量",ylim=c(0,600))

words=apply(train[-1],1,sum)
hist=hist(words,col=c("yellow","coral","firebrick1",rainbow(30)),main="每条投诉的用词数量直方图",
            xlab="投诉用词数",ylab="分布数量",breaks=30,ylim=c(0,500))


boxplot(words~单位名称,data=train,col=rainbow(7),border=c("dimgray","dimgrey"),
        main='每条投诉信息词汇量~单位名称的箱线图',names=单位名称,xlab="单位名称",ylab="每条投诉信息词汇量",plot=T)
boxplot(log(words)~单位名称,data=train,col=rainbow(7),border=c("dimgray","dimgrey"),
        main='log(每条投诉信息词汇量)~单位名称的箱线图',names=单位名称,xlab="单位名称",ylab="log(每条投诉信息词汇量)",plot=T)

train[,-1]=(train[,-1]>=1) 
val[,-1]=(val[,-1]>=1)
library("e1071")
bayes=naiveBayes(train$单位名称~.,train)
bresult=predict(bayes,val)
sum(val$单位名称==bresult)

library("graphics")
pred=bresult 
real=val$单位名称
cm=as.matrix(as.data.frame.matrix(table(pred,real)))
cm
breaks.frequency=seq(from=min(cm), to=max(cm), length.out=10)
myColors=colorRampPalette(c("white", "#2874A6"))

draw=function(cm,axis=TRUE,label=TRUE){
  image(1:nrow(cm),1:ncol(cm),cm,breaks=breaks.frequency, 
        col=myColors(length(breaks.frequency)-1),axes=F,cex=2,xlab="预测类别",ylab="真实类别")
  if (axis){  
  for (x in 1:nrow(cm)) {
    for (y in 1:ncol(cm))  {
      text(x, y, cm[x,y],cex=2)
    }
  }
  }
  if(label){
  axis(2, 1:ncol(cm), colnames(cm), cex.axis=0.7)
  axis(1, 1:nrow(cm), rownames(cm), cex.axis=0.7)
  }
  }
draw(cm)


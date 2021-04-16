data = read.csv("D:/大数据学院文件资料/2020秋课程/机器学习/pj1/data.csv")
summary(data)

hist(data$rent,seq(from=500,to=7000,by=250),col="lightblue",main="月租金直方图",xlab = "月租金(元)",ylab="分布数量",ylim=c(0,800))

t=tapply(data$rent,data$region,mean)
t= sort(t, decreasing = TRUE)[1:8]                                           

bar=barplot(t,col="lightblue",main="降序平均租金柱状图",
            xlab="城区",ylab="月租金(元)",ylim=c(0,4000))

boxplot(data$rent~ data$region,col="lightblue",horizontal = F,
        xlab="城区",ylab="月租金(元)分布",plot = T)


data$room=factor(data$room,levels=c("次卧","主卧"))
data$floor_grp=factor(data$floor_grp,levels=c("低楼层","中楼层","高楼层"))
data$subway=factor(data$subway,levels=c("否","是"))
data$region=factor(data$region,levels=c("石景山","昌平","朝阳","大兴","东城","房山","丰台","海淀","顺义","通州","西城"))
data$heating=factor(data$heating,levels=c("自采暖","集中供暖"))
reg=lm(formula=rent~.,data=data)
summary(reg)

par(mfrow=c(2,2))
plot(reg,which=c(1:4))

library(car)
vif(reg)

reg=lm(formula=log(rent)~.,data=data)
summary(reg)

par(mfrow=c(2,2))
plot(reg,which=c(1:4))

data = read.csv("D:/大数据学院文件资料/2020秋课程/机器学习/pj1/data.csv")
data$room=factor(data$room,levels=c("次卧","主卧"))
data$floor_grp=factor(data$floor_grp,levels=c("低楼层","中楼层","高楼层"))
data$subway=factor(data$subway,levels=c("否","是"))
data$region=factor(data$region,levels=c("石景山","昌平","朝阳","大兴","东城","房山","丰台","海淀","顺义","通州","西城"))
data$heating=factor(data$heating,levels=c("自采暖","集中供暖"))


reg1=step(lm(rent~.,data=data),direction="both",trace=1,keep=NULL,steps=2000,k=log(nrow(data)))
summary(reg1)


reg2=step(lm(log(rent)~.,data=data),direction="both",trace=1,keep=NULL,steps=2000,k=log(nrow(data)))

pred.cv<-function(dat,k)
{
  ind=sample(1:k,nrow(dat),replace=T)
  pred_cv=rep(0,nrow(dat))
  sigma <- sum(reg2$residuals^2)/reg2$df.residual
  for (i in 1:k)
  {
    ii=which(ind==i)
    obj=lm(log(rent) ~ bedroom + bathroom + area + subway + region + 
             heating,data=dat[-ii,])
    pred_cv[ii]=predict(obj,data[ii,])
  }
  rmse=sqrt(mean((exp(sigma/2)*exp(pred_cv)-data$rent)^2))
  return(list(pred_cv=pred_cv,rmse=rmse))
}

set.seed(123)
rmses=rep(0,50)
for (i in 1:50)
{
  cat(i,"\r")
  pred_cv=pred.cv(dat=data,k=5)
  rmses[i]=pred_cv$rmse
}
mean(rmses)





library(DAAG)
cvlm=cv.lm(data=data,lm(rent ~ bedroom + bathroom + area + subway + region + 
                          heating,data=data),m=5,printit=F,seed=123)
sqrt(attr(cvlm,"ms"))







        
data$room=as.numeric(data$room)
data$floor_grp=as.numeric(data$floor_grp)
data$subway=as.numeric(data$subway)
data$region=as.numeric(data$region)
data$heating=as.numeric(data$heating)

cor_data=data
cor(cor_data)
res=cor(cor_data)
library('corrplot')
corrplot(res, type = "upper", tl.col = "black ", tl.srt = 45 )

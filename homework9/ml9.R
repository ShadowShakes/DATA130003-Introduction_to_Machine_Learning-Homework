
library("xlsx")
data=read.xlsx("D:/大数据学院文件资料/2020秋课程/机器学习/assignment/homework9/NBA数据分析降维/NBA.xlsx",1,encoding = "UTF-8")
summary(data)




# 做修正
data$投篮率 <- as.numeric(gsub("\\%", "", data$投篮率))/100 
data$三分投球率 <- as.numeric(gsub("\\%", "", data$三分投球率))/100 
data$罚球率 <- as.numeric(gsub("\\%", "", data$罚球率))/100
train_data=data
train_data[2:18]=scale(data[2:18])
pca=princomp(train_data[,2:(ncol(train_data)-1)],cor=FALSE)
pca
# 崖底碎石图
screeplot(pca,type="lines",cex.lab=1,main="崖底碎石图")

pca_temp=principal(train_data[2:18])
pca_temp



pca_value=numeric(0)
data1=data
data1[2:18]=scale(data1[2:18])
# 计算所有球员的主成分得分
for(i in 1:nrow(data1)){
  pca_value=c(pca_value,as.matrix(data1[i,2:(ncol(data1)-1)])%*%as.matrix(pca$loadings[,1]
))
}
head(pca_value)
# 解读某几位球员的主成分得分
value1=as.matrix(data1[data1$球员=='勒布朗-詹姆斯',2:(ncol(data1)-1)])%*%as.matrix(pca$loadings[,1])
value2=as.matrix(data1[data1$球员=='科比-布莱恩特',2:(ncol(data1)-1)])%*%as.matrix(pca$loadings[,1])
value3=as.matrix(data1[data1$球员=='卡尔-马龙',2:(ncol(data1)-1)])%*%as.matrix(pca$loadings[,1])
value4=as.matrix(data1[data1$球员=='斯蒂芬-库里',2:(ncol(data1)-1)])%*%as.matrix(pca$loadings[,1])
value5=as.matrix(data1[data1$球员=='保罗-乔治',2:(ncol(data1)-1)])%*%as.matrix(pca$loadings[,1])
value6=as.matrix(data1[data1$球员=='凯文-乐福',2:(ncol(data1)-1)])%*%as.matrix(pca$loadings[,1])
value7=as.matrix(data1[data1$球员=='艾德-戴维斯',2:(ncol(data1)-1)])%*%as.matrix(pca$loadings[,1])
value8=as.matrix(data1[data1$球员=='汤姆-里克尔',2:(ncol(data1)-1)])%*%as.matrix(pca$loadings[,1])
show=cbind(c('勒布朗-詹姆斯','科比-布莱恩特','卡尔-马龙','斯蒂芬-库里','保罗-乔治','凯文-乐福','艾德-戴维斯','汤姆-里克尔')
           ,rbind(value1,value2,value3,value4,value5,value6,value7,value8))
show_value=as.data.frame(row.names=c('d','d'),col.names=c('a','d'),show)
colnames(show_value) = c("球员名字","主成分得分")
show_value


# 评估类别数
class_num=numeric(15)
for(i in 1:15){
  clusters=kmeans(data1[,2:(ncol(data1)-1)],centers=i)
  class_num[i]=max(clusters$withinss)
}
plot(class_num,xlab="聚类的类别数",ylab="clusters$withinss类别内距离度量",main="类别内距离~聚类类别数",type='o')

# 进行聚类
clusters=kmeans(data1[,2:(ncol(data1)-1)],centers=4)
cluster_res=cbind(data1[2:18],clusters$cluster)
c_res=aggregate(cluster_res,by=list(clusters$cluster),mean)
colnames(c_res)[1]="聚类分组编号"
head(c_res)



PCA<-function(Dat,max.k){
  X=as.matrix(Dat) 
  R=1/(nrow(X)-1)*t(X)%*%X 
  lambda=eigen(R)$values[1:max.k] 
  para=eigen(R)$vectors[,1:max.k] 
  return(list(sdev=sqrt(lambda),paramatrix=para))
}
PCA_res=PCA(Dat=data1[,2:(ncol(data1)-1)],max.k=10)
PCA_res
screeplot(PCA_res,type="lines",cex.lab=1,main="PCA函数崖底碎石图")


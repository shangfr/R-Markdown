#清洗后行为特征提取
x=c("E:qx点击.csv","E:qx购物车.csv","E:qx收藏.csv","E:qx购买.csv")
y=c("1","2","3","4")
for(i in 1:4){
user=subset(qingxi,behavior_type==y[i])
write.csv(user, file = x[i], row.names =F, quote = F)
}

#取类
zj_cate=read.csv(file.choose())
zj_cate=zj_cate[,1] #转化成向量
buyer2=buyer[,1:2] #购买者u-i数组
m=0

#类内顾客相关性
for(i in 1:length(zj_cate))
{
category=subset(buyer,item_category==zj_cate[i]) #购买者按子集类划分
if(dim(category)[1]>1 & dim(category)[2]>1)    #类内用户大于1
{
glb=table(category$item_id,category$user_id) #user-item关联表
cglb=cor(glb) #user相关系数表
n=length(cglb[abs(cglb)>0.5])/2  #相关系数大于0.5的人数
id=rownames(which(abs(cglb)>0.5, arr.ind=TRUE)) #相关系数大于0.5的用户id
tuijian=data.frame(0,0)
for(i in 1:n)
{
item=subset(buyer2,user_id==id[i])
colnames(tuijian)=colnames(item)
tuijian=rbind(tuijian,item)
}
write.csv(tuijian, file =paste("E:",zj_cate[i],".csv"), row.names =F, quote = F)
m=m+1 #文本计数
}}

#清洗后行为特征提取
x=c("E:qx点击.csv","E:qx购物车.csv","E:qx收藏.csv","E:qx购买.csv")
y=c("1","2","3","4")
for(i in 1:4){
user=subset(qingxi,behavior_type==y[i])
write.csv(user, file = x[i], row.names =F, quote = F)
}

#取类
zj_cate=read.csv(file.choose())
zj_cate=zj_cate[,1] #转化成向量
buyer2=buyer[,1:2] #购买者u-i数组
m=0
tuijian=data.frame(0,0)
points=data.frame(0,0)  #存相关用户id
colnames(points)=c("user_id1","user_id2")
colnames(tuijian)=c("user_id","item_id")
#类内顾客相关性
for(i in 1:length(zj_cate))
{
category=subset(buyer,item_category==zj_cate[i]) #购买者按子集类划分
if(dim(category)[1]>1 & dim(category)[2]>1)    #类内用户大于1
{
glb=table(category$item_id,category$user_id) #user-item关联表
cglb=cor(glb) #user相关系数表
#相关系数大于0.5的人数 n=length(cglb[abs(cglb)>0.5])/2 
#相关系数大于0.5的用户id id=rownames(which(abs(cglb)>0.5, arr.ind=TRUE))
#relation=data.frame(0,0)
rn=rownames(cglb)
cn=colnames(cglb)
cglb[cglb==1]=0
cglb[is.na(cglb)]=0
p=0.5  #相关系数
for(i in 1:length(rn)){
   for(j in 1:length(cn))
   {
       if(cglb[i,j] >= p)
       {
       points[i,] <- c(rn[i],cn[j])
   }
   }    
}
}
}

write.csv(points, file =paste("E:相关用户表.csv"), row.names =F, quote = F)









for(i in 1:n) #相关用户购买集汇总
{
item=subset(buyer2,user_id==id[i])
colnames(relation)=colnames(item)
relation=rbind(relation,item)
}
for(i in 1:n) #推荐用户购买集
{
item=subset(buyer2,user_id==id[i])
aa=setdiff(relation$item_id,item$item_id) # tc1-tc2
tuijian[i,]=c(id[i],aa)
}
m=m+1 #文本计数
}
}
write.csv(tuijian, file =paste("E:",zj_cate[i],".csv"), row.names =F, quote = F)





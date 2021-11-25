#物品类内各用户相关性
for(i in 1:length(zj_cate))
{
{
glb=table(category$item_id,category$user_id) #user-item关联表
cglb=cor(glb) #user相关系数表
n=length(cglb[abs(cglb)>0.5])/2  #相关系数大于0.5的人数
id=rownames(which(abs(cglb)>0.5, arr.ind=TRUE)) #相关系数大于0.5的用户id
relation=data.frame(0,0)
for(i in 1:n) #推荐用户购买集
{
item=subset(buyer2,user_id==id[i])
aa=setdiff(relation$item_id,item$item_id) # tc1-tc2
tuijian[i,]=c(id[i],aa)
}
write.csv(tuijian, file =paste("E:",zj_cate[i],".csv"), row.names =F, quote = F)
m=m+1 #文本计数
}}
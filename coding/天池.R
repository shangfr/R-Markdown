tc=read.csv(file.choose()) #数据清洗
tc=tc[,-4] #去除地理信息
tc=tc[order(tc[,5]),]  #按时间排序
#时序序列
library(xts)
sample=xts(tc[,-5], as.Date(tc[,5]))
d1518=sample('2014-12-15/2014-12-1')
# 行为分类
x=c("E:点击.csv","E:购物车.csv","E:收藏.csv","E:购买.csv")
y=c("1","2","3","4")
for(i in 1:4){
user=subset(ntc,behavior_type==y[i])
action_freq=data.frame(table(user$user_id))
colnames(action_freq)=c("user_id","freq")
action_freq=action_freq[order(action_freq[,2]),]
write.csv(action_freq, file = x[i], row.names =F, quote = F)
}


#提取购买用户user3=subset(tc,behavior_type=="3")
#提取不购买用户user5=subset(tc,behavior_type!="4") 
#行为频率action_freq3=data.frame(table(user3$user_id))
#colnames(action_freq3)=c("user_id","freq")
#max(action_freq$freq)
#action_freq3=action_freq3[order(action_freq3[,2]),]

#1
super_ac=action_freq1[action_freq1$freq>7500,] 
pachong=setdiff(super_ac$user_id,action_freq4$user_id) #高频点击，无购买者集
subset(action_freq,user_id%in%pachong) #查看爬虫行为
real_user=tc[tc$user_id!=pachong] #过滤爬虫数据
write.csv(sample, file = "E:时序序列.csv", row.names = T, quote = F)
#2 新方法，计算点买比<100.
dd= merge(tc4,tc5, by = "user_id") #数据合并
dm=dd$freq/dd$freq4
dm=cbind(dd,dm)
plot(density(dm[,4]),xlim=c(0,1000)) #画点买比密度图，可知点买比符合正态分布。
rdm=rdm[rdm$dm<100,] 
real_tc= merge(tc,rdm, by = "user_id") #清洗结束


buyer=user123 #购买者数组
recom=read.csv(file.choose()) #推荐集

jihe=cbind(buyer,recom$item_id)  #构造新的数据框 

njihe=subset(jihe,item_id%in%item_id.1)
buyer_item=njihe[,1:2]

all(buyer_item$item_id%in%recom$item_id) #检验推荐集是否都在子集内
recom=unique(buyer_item) #去重
write.csv(action_freq, file = "E:不购买行为.csv", row.names = T, quote = F)

#NA处赋0值 tc[is.na(tc)]=0
# 画图分析
library(reshape2)
library(ggplot2)
mm=melt(tc,id.vars="n")
#windowsFonts(myFont1=windowsFont("Times New Roman"),myFont2=windowsFont("华文行楷"))
p=ggplot(mm, aes(x=n,y=value))+geom_line(aes(color=variable))+labs(list(title = "购物行为", x = "用户", y = "次数"))+ theme(text = element_text(face="plain",size=12,angle=0,color="pink"))

p+annotate("text", x = 9000, y = 5000, label = "爬虫干扰")

p=p+scale_y_continuous(limits=c(0,5000))
p+annotate("rect", xmin = 5000, xmax = 7500, ymin =750, ymax = 1500,alpha = .2)+annotate("text", x = 5000, y = 1000, label = "交叉区间")

# 画图分析结论：异常行为--购买>250 购物车>1000 收藏>750 点击>7500 
# 定位 match(x,y)



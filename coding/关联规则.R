library(arules)
library(RColorBrewer)#可视化
library(grid)
library(arulesViz)

data=read.csv(file.choose(),head=F)
trans <- as(data,"transactions") #数据格式转换

inspect(trans[1:2])  #数据查看
image(trans [1:50])  
itemFrequencyPlot(trans, support=0.5)  
itemFrequencyPlot(trans, topN=10, horiz=T)  



basketSize<-size(trans)
summary(basketSize)
itemFreq <- itemFrequency(trans)  
itemCount <- (itemFreq/sum(itemFreq))*sum(basketSize)  
summary(itemCount)  
orderedItem <- sort(itemCount, decreasing = T)  
orderedItem[1:10] 


#求关联规则
rules <- apriori(trans,parameter=list(support=0.3,confidence=1))
summary(rules)
inspect(rules[1:10])


edible <- subset(rules, rhs %in% c("V1=e"))  
inspect(edible[1:10]) 

 plot(rules[1:50], method="matrix", measure=c("lift", "confidence"))
 plot(rules[1:50], method="matrix", measure=c("lift", "confidence"),control=list(reorder=TRUE))

#规则保存
write(rules, file="rules.csv", sep=",", quote=TRUE, row.names=FALSE)  
rules_df <- as(rules, "data.frame")  


mushroom.rules <- apriori(trans,parameter = list(support = 0.8, confidence = 1))  


plot(mushroom.rules,   
control=list(jitter=2, col = rev(brewer.pal(9, "Greens")[4:9])),  
shading = "lift")    

plot(mushroom.rules, method="grouped",     
control=list(k=100,col = rev(brewer.pal(9, "Greens")[4:9])))  

plot(edible[1:20], measure="confidence", method="graph",   
control=list(type="items"), shading = "lift")
 plot(edible, method="paracoord", control=list(reorder=TRUE))
 plot(edible, method="paracoord")



#2 arules包中有一个eclat算法的实现，用于发现频繁项集(FP-Growth)
frequentsets <- eclat(trans, parameter = list(supp = 0.5, maxlen = 15))#求频繁项集
inspect(frequentsets[1:10])#查看频繁项集
#这里如果支持度选的比较大，也许没有10这么多，这里就不能写[1:10].
inspect(sort(frequentsets,by="support")[1:10])#根据支持度对求得的频繁项集排序并察看
#多选几次支持度阈值，更好的发现频繁模式。

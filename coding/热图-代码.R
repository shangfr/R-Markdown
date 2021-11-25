test=read.csv(file.choose())
ta=table(test$userId,test$movieId)
n=0
ratings=test[,3]
par(mfrow=c(1,2))
boxplot(ratings,boxwex = 0.5,main="评分统计", col = "springgreen2")
hist(ratings,main="评分统计", col = rainbow(150:100))

for(i in 1:nrow(ta)){
for(j in 1:ncol(ta)){
if(ta[i,j]==1){
n=n+1
ta[i,j]=ratings[n]}
}}
require("gplots")
require("RColorBrewer")

my_palette <- colorRampPalette(c("blue","green","yellow","tomato" ))(n = 399)
col_breaks = c(seq(0,0,length=100), seq(1,3,length=100),  seq(3,4,length=100),seq(4,5,length=100))

mat_data=ta[,1:500]

win.graph(width=1600, height=900, pointsize=7)
heatmap.2(mat_data,# cellnote = mat_data same data set for cell labels
main = "Correlation", # heat map title 
notecol="black"  , #change font color of cell labels to black 
density.info="none", # turns off density plot inside color legend 
trace="none", # turns off trace lines inside the heat map
margins =c(5,5), # widens margins around plot 
col=my_palette, # use on color palette defined earlier 
breaks=col_breaks, # enable color transition at specified limits 
dendrogram="none",
Rowv=TRUE,
Colv=TRUE
) # turn off column clustering

dev.off()



user=split(test,test$userId)
u1=user$"1"
plot(u1[,3])

par(mfrow=c(2,2))
plot(density(u1[,3]))
plot(density(u2[,3]))
plot(density(u3[,3]))
plot(density(u4[,3]))

hist(u1[,3])
hist(u2[,3])
hist(u3[,3])
hist(u4[,3])

movie=split(test,test$movieId)
m1=movie$"104841"
plot(density(m1[,3]))
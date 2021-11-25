#设置当前工作目录
setwd("E:/")

#加载packages
library(sp)
library(maptools)

#读取省会城市经纬度数据
data=read.csv(file.choose())
#读取各省的边界数据等
border <- readShapePoly(file.choose())

#设置输出文件
#jpeg("chinamap.jpeg")

#画地图
plot(border,col=rainbow(925),ylim = c(18, 54), panel.first = grid());
#增加省会城市坐标点
points(data[,1], data[,2], pch = 19,col=da[,4])
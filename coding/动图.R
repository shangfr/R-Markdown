
#数据模拟
fu=function(p,t)#初始价格p，时间周期t
{
 y=vector()
 y=p
   for(i in 1:t)
   {
       y[i + 1] = y[i] + rnorm(1)
   }
 fu_data <- data.frame(Date = 1:t,Price = y[-t])
 return(fu_data)
}

fu=fu(30,300)

#动态可视化gif
require(animation)
saveGIF({
    ani.options(nmax = 300)

 for(i in 1:300)
 {
  plot(fu[i,],xlim=c(0,300),ylim=c(0,max(fu[,2])),
       cex.axis=0.5,xlab="Time",ylab="Price",
       main="GDP Growth Rate", pch = 20, 
       cex = rnorm(1,5,0.5),col = "#97FFFF")
  lines(fu[1:i,])
 }
       }, interval = 0.05, movie.name = "tp.gif", 
          ani.width = 1600, ani.height = 900)


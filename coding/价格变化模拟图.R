
#数据模拟
fupr=function(p,t)#初始价格p，时间周期t
{
 y=vector()
 y1=y2=y3=p
   for(i in 1:t)
   {
       y1[i + 1] = y1[i] + rnorm(1)
y2[i + 1] = y2[i] + rnorm(1)
y3[i + 1] = y3[i] + rnorm(1)

   }
 fu_data <- data.frame(Date = 1:t,Price1 = y1[-t],Price2 = y2[-t],Price3 = y3[-t])
 return(fu_data)
}


t=500
fu=fupr(30,t)


#动态可视化gif
require(animation)
saveGIF({
    ani.options(nmax = t)

 for(i in 1:t)
 {
# i=100
  plot(rep(fu[i,1],3),fu[i,-1],xlim=c(0,t),ylim=c(min(fu[,-1])-2,max(fu[,-1])+2),
       cex.axis=1.5,xlab="Time",ylab="Price",
       main="Random Walk of Stock Price", pch = 20, 
       cex = rnorm(1,5,0.5),col = c("#00EEEE","#1874CD","#EE2C2C"))
   lines(fu[1:i,1],fu[1:i,2],col = "#00EEEE",lwd=2)
   lines(fu[1:i,1],fu[1:i,3],col = "#1874CD",lwd=2)
   lines(fu[1:i,1],fu[1:i,4],col = "#EE2C2C",lwd=2)

 }
       }, interval = 0.05, movie.name = "tp.gif", 
          ani.width = 800, ani.height = 450)




saveHTML({
    par(mar = c(4, 4, 0.5, 0.5))


 for(i in 1:t)
 {
# i=100
  plot(rep(fu[i,1],3),fu[i,-1],xlim=c(0,t),ylim=c(min(fu[,-1])-2,max(fu[,-1])+2),
       cex.axis=1.5,xlab="Time",ylab="Price",
       main="Random Walk of Stock Price", pch = 20, 
       cex = rnorm(1,5,0.5),col = c("#00EEEE","#1874CD","#EE2C2C"))
   lines(fu[1:i,1],fu[1:i,2],col = "#00EEEE",lwd=2)
   lines(fu[1:i,1],fu[1:i,3],col = "#1874CD",lwd=2)
   lines(fu[1:i,1],fu[1:i,4],col = "#EE2C2C",lwd=2)

 }



},  img.name = "norm_plot", single.opts = "utf8: false", autoplay = FALSE, 
    interval = 0.5, imgdir = "norm_dir", htmlfile = "raEndom.html", 
    ani.height = 450, ani.width = 800, title = "Demo of 50 Normal random numbers", 
    description = des)















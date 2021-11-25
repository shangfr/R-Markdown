library(geosphere)
# matrix
muer.lonlat = cbind(data[,1],data[,2] )
# æ´»∑º∆À„£¨Õ÷‘≤
muer.dists = distm(muer.lonlat, fun=distVincentyEllipsoid)
rownames(muer.dists) = data[,3]
colnames(muer.dists) = data[,3]

ans=round(muer.dists/1000,2)


x=6400*cos(x/360*pi)*cos(y/360*pi)
y=6400*cos(x/360*pi)*sin(y/360*pi)
z=6400*sin(x/360*pi)

scatterplot3d(x, y,z, main="Scatterplot3d", pch=16, highlight.3d=TRUE, type="p")
points(x,y,pch=19,col=cols)
            text(x,y,z,pos=1,cex=0.7)

library(REmap)
city_vec = c("北京","Shanghai","广州")
get_city_coord("Shanghai")
get_geo_position (city_vec)

# remap(mapdata, title = "", subtitle = "", 
      theme =get_theme("Dark"))

set.seed(125)
origin = rep("北京",10)
destination = c('上海','广州','大连','南宁','南昌',
                '拉萨','长春','包头','重庆','常州')
dat = data.frame(origin,destination)
out = remap(dat,title = "REmap实例数据",subtitle = "theme:Dark")
plot(out)


set.seed(125)
origin = rep("北京",length(outer))
destination = c(as.character(outer[-1,3]))

origin = as.character(outer[,3])
destination = c(as.character(outer[-1,3]),as.character(outer[1,3]))

dat = data.frame(origin,destination)
out = remap(dat,title = "GA算法-路径优化",subtitle = "Copyright by Shang")
plot(out)

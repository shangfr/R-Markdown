library(animation)

#1
ani.options(interval = 1)
saveGIF({
for (i in 1:5) {

plot(runif(10), ylim = c(0, 1))
ani.pause()}
})


#2
saveGIF({
for (i in 1:5) {

plot(runif(10), ylim = c(0, 1))}
}, interval = 0.1
)


#3
saveGIF({
for (i in 1:50) {

hist(rnorm(1000),col=11)

}
} , interval = runif(50, 0.1, 1)
)


#4
saveGIF({
for (i in 1:length(AirPassengers)) {

plot(AirPassengers[1:i],col=2,type="l") #Simple Line Plot

}
} , interval = 0.1
)



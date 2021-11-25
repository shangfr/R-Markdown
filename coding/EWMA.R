
require(rugarch)
require(xts)
data(sp500ret)
ewma.spec.fixed = ugarchspec(mean.model=list(armaOrder=c(0,0), include.mean=FALSE),
                             variance.model=list(model="iGARCH"), 
                             fixed.pars=list(alpha1=1-0.94, omega=0))

ewma.spec.est = ugarchspec(mean.model=list(armaOrder=c(0,0), include.mean=FALSE),
                           variance.model=list(model="iGARCH"), fixed.pars=list(omega=0))

igarch.spec = ugarchspec(mean.model=list(armaOrder=c(0,0), include.mean=FALSE),
                         variance.model=list(model="iGARCH"))

mod1 = ugarchfit(ewma.spec.fixed, as.xts(sp500ret))
mod2 = ugarchfit(ewma.spec.est, as.xts(sp500ret))
mod3 = ugarchfit(igarch.spec, as.xts(sp500ret))

plot(sigma(mod3), main="", auto.grid = FALSE,
     major.ticks = "auto",minor.ticks = FALSE)

lines(sigma(mod2), col=2, lty=2)
lines(sigma(mod1), col=3, lty=3)

cf1=round(coef(mod2)[3],3)
l1 = as.expression("iGARCH")
l2 = as.expression(substitute(paste("EWMA[est.",lambda,"=",x,"]"), list(x=cf1)))
l3 = as.expression(substitute(paste("EWMA[fix.",lambda,"=",x,"]"), list(x=0.94)))

legend("topright", c(l1, l2, l3), col=1:3, lty=1:3, bty="n", cex=0.9)

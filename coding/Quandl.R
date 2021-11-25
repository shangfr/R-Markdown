library(dygraphs)
library(Quandl)

stock = read.csv(file.choose(),header = TRUE) #读入股票代码表
tickers = paste("XSHG", stock$Code, sep="/")    #整理股票代码表



Quandl.api_key("zqfbZ3opnUBgPYKzrgYx")

mydata = Quandl(tickers[1], 
                type="xts",
                start_date="2014-1-1", end_date="2015-12-31")

stockdata = read.csv(file.choose(),header = TRUE) #读入股票数据
mydata = xts(stockdata[ ,-1], order.by = as.Date(stockdata[ ,1]))
dygraph(mydata, main = "股价走势") %>% 
  dyRangeSelector(dateWindow = c("2014-1-1", "2015-12-31"))

library(d3heatmap)
cordata = cor(mydata)
colnames(cordata) = c("上证指数",as.character(stock$Name))
rownames(cordata) = colnames(cordata)
d3heatmap(cordata, scale="column", colors="Blues")

library(ppcor)
pcordata = pcor(mydata)$estimate 
colnames(pcordata) = c("上证指数",as.character(stock$Name))
rownames(pcordata) = colnames(pcordata)
d3heatmap(pcordata, scale="column", colors="Blues")

library(igraph)
library(networkD3)
g1=graph.adjacency(cordata>0.3, mode="lower", weighted=NULL, diag=FALSE,
                   add.colnames=NULL, add.rownames=NA)
wc <- cluster_walktrap(g1)
members <- membership(wc)
g1_d3 <- igraph_to_networkD3(g1, group = members)
forceNetwork(Links = g1_d3$links, Nodes = g1_d3$nodes, 
             Source = 'source', Target = 'target', 
             NodeID = 'name', Group = 'group',
             fontSize = 15,opacity = 1,zoom = TRUE)


library(spaceExt)
res=glasso.miss(cordata,rho=0.01,emIter=25,penalize.diagonal=FALSE)
p=-res$wi
d=1/sqrt(diag(res$wi))
patialcor=-diag(d)%*%p%*%diag(d)
colnames(patialcor) = c("上证指数",as.character(stock$Name))
rownames(patialcor) = colnames(patialcor)

gp=graph_from_adjacency_matrix(patialcor>0, mode="lower", weighted=NULL, diag=FALSE,
                   add.colnames=NULL, add.rownames=NA)
plot(gp)
wc <- cluster_walktrap(gp)
members <- membership(wc)
gp_d3 <- igraph_to_networkD3(gp, group = members)
forceNetwork(Links = gp_d3$links, Nodes = gp_d3$nodes, 
             Source = 'source', Target = 'target', 
             NodeID = 'name', Group = 'group',
             fontSize = 15,opacity = 1,zoom = TRUE)

MyClickScript <- 'alert("点击的 " + d.name + " 在第 " +
       (d.index + 1) +  " 行");'
forceNetwork(Links = gp_d3$links, Nodes = gp_d3$nodes, 
             Source = 'source', Target = 'target', 
             NodeID = 'name', Group = 'group',
             fontSize = 15,opacity = 1,zoom = TRUE,
             clickAction = MyClickScript)



require(visNetwork, quietly = TRUE)
nodes = data.frame(id = V(gp)$name, title = V(gp)$name)
df=as_edgelist(gp, names = TRUE)
edges = data.frame(from = df[,1], to = df[,2])

visNetwork(nodes, edges) %>%
  visConfigure(enabled = TRUE)
  visIgraphLayout(layout = "layout_in_circle") %>%
  visOptions(highlightNearest = TRUE, nodesIdSelection = TRUE)






Quandl.api_key("zqfbZ3opnUBgPYKzrgYx")

data <- Quandl("NSE/OIL")
plot(stl(Quandl("WIKI/GOOG",type="ts",collapse="monthly")[,11],s.window="per"))

library(Quandl)

# Let's start by taking a look at the price of gold since 2010
# http://www.quandl.com/BUNDESBANK/BBK01_WT5511
pGold <- Quandl("BUNDESBANK/BBK01_WT5511", start_date="2010-01-01")

# Plot Data
library(ggplot2)

p1 <- ggplot(pGold, aes(x=Date,y=Value))+
  geom_line()+
  ggtitle("Price of Gold")

# Find Bitstamp Permalink Data at http://www.quandl.com/BITCOIN/BITSTAMPUSD
bitstamp <- Quandl("BITCOIN/BITSTAMPUSD")

names(bitstamp)
# bitstamp data has four different prices.
# high - highest price of day
# low - lowest price of day
# close - last price of day
# Weighted.Price- I believe this is calculated as:
#   sum(price*volume at price)/total volume
# I will be using the weighted price

# Drop infinitely large values (bitstamps worth > 10^6)
# And reduce the data to just 
bitstamp2 <- bitstamp[bitstamp$"Weighted Price"<10^6,c("Date", "Price")]

names(bitstamp2) <- c("Date", "Price")

p2 <- ggplot(bitstamp2, aes(x=Date,y=Price)) +
  geom_line()+
  ggtitle("Price of BitStamp")

# In order to compare our two time series let's combine their data
names(pGold) <- c("Date", "Price")

PriceData <- rbind(cbind(pGold, Good="Gold"), cbind(bitstamp2, Good="BitStamp"))

ggplot(PriceData, aes(x=Date,y=Price, colour=Good)) +
  geom_line()+
  ggtitle("Price Currency Options (USD)")

PriceData$P1d <- c(NA, PriceData$Price[-1]-PriceData$Price[-nrow(PriceData)])

PriceData$P1d[PriceData$Good[-1]!=PriceData$Good[-nrow(PriceData)]] <- NA

ggplot(subset(PriceData, Date>"2013-04-01"&Date<"2014-01-01"), 
       aes(x=Date,y=P1d, colour=Good)) +
  geom_line(size=1)+
  ggtitle("Price Currency Options (USD)")



library("reshape")

PriceData.wide <- 
  reshape(PriceData, direction="wide", idvar = "Date", timevar = "Good")

cor(PriceData.wide[,c(3,5)], use="pairwise.complete.obs")
summary(
  lm(P1d.BitStamp[-1]~P1d.Gold[-nrow(PriceData.wide)]-1, data=PriceData.wide))








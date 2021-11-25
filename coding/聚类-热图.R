require("gplots")
require("RColorBrewer")
data=read.csv(file.choose())

d=dist(t(data[,-1]))
hc=hclust(d)
plot( hc,  hang = -1)
re <- rect.hclust(hc, k = 3)
id <- cutree(hc, 3)


mat_data <- data.matrix(data[,2:ncol(data)])
mat_data = cor(mat_data)
mat_data=round(mat_data,1)

my_palette <- colorRampPalette(c("#00FF7F","yellow","tomato" ))(n = 299)
col_breaks = c(seq(-1,0,length=100), seq(0,0.7,length=100), seq(0.7,1,length=100))

heatmap.2(mat_data, cellnote = mat_data, # same data set for cell labels
main = "Correlation", # heat map title 
notecol="black", # change font color of cell labels to black 
density.info="none", # turns off density plot inside color legend 
trace="none", # turns off trace lines inside the heat map
 margins =c(12,9), # widens margins around plot 
col=my_palette, # use on color palette defined earlier 
breaks=col_breaks, # enable color transition at specified limits 
dendrogram="row", # only draw a row dendrogram 
Colv="NA") # turn off column clustering







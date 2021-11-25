library(jiebaRD)
library(jiebaR)

keys = worker("keywords", topn = 10)

tl=read.csv(file.choose())
n=dim(tl)[1]

for (i in 1:n){
  outkey=keys <= paste("C:/Users/Shang/Desktop/用来抓取关键词的文档/关键词文献TXT版/", as.character(tl[i,]), sep = "")
  
  #  outkey=data.frame(outkey)
  #  outkey[,2]=rownames(outkey)
  #  names(outkey)=c("keys","IDF")
  
  write.csv(outkey, file = paste("C:\\Users\\Shang\\Desktop\\关键词\\", as.character(tl[i,]), sep = ""), row.names = F, quote = T) 
}


# dir /w C:\Users\Shang\Desktop\用来抓取关键词的文档\关键词文献TXT 版 >> C:\Users\Shang\Desktop\标题.csv
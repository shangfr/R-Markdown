#多准则决策模型-TOPSIS评价方法

##R语言实现-代码

MCDM=function (decision = NULL, weights = NULL, impacts = NULL) #决策矩阵，权重向量，影响因子
{
    if (missing(weights)) 
        stop("缺少'权重向量-weights'")
    if (missing(impacts)) 
        stop("缺少'影响因子-impacts'")
    if (!is.matrix(decision) | is.data.frame(decision)) 
        stop("'决策矩阵-decision'必须是矩阵或数据框")
    if (length(weights) != ncol(decision)) 
        stop("权重向量长度错误")
    if (length(impacts) != ncol(decision)) 
        stop("影响因子长度错误")
    if (!all(weights > 0)) 
        stop("权重必须大于零")
    if (!is.character(impacts)) 
        stop("影响因子必须是字符型 '+'或'-' 符号")
    if (!all(impacts == "+" | impacts == "-")) 
        stop("影响因子只能是字符型 '+'或'-' 符号")
    weights <- weights/sum(weights)
    N <- matrix(nrow = nrow(decision), ncol = ncol(decision)) #建一个空矩阵
    for (i in 1:nrow(decision)) {
        for (j in 1:ncol(decision)) {
            N[i, j] <- decision[i, j]/sqrt(sum(decision[, j]^2))
        }
    }                 #决策矩阵标准化
    W = diag(weights) #建权重对角矩阵
    V = N %*% W       #构造加权规范化矩阵


#确定理想方案和负理想方案
    u <- as.integer(impacts == "+") * apply(V, 2, max) + as.integer(impacts == 
        "-") * apply(V, 2, min)
    l <- as.integer(impacts == "-") * apply(V, 2, max) + as.integer(impacts == 
        "+") * apply(V, 2, min)

#构建理想方案和负理想方案距离公式
    distance_u = function(x) {
        sqrt(sum((x - u)^2))
    }
    distance_l = function(x) {
        sqrt(sum((x - l)^2))
    }

#计算相对接近度并排序
    du <- apply(V, 1, distance_u)
    dl <- apply(V, 1, distance_l)
    score <- dl/(dl + du)
    outcome <- data.frame("方案"= 1:nrow(decision), 得分 = score, 
        排名 = rank(-score))

return(outcome)

}


##算法测算模拟

#决策矩阵d，权重向量w，影响因子i

dm=c(2.0,1.5,20,5.5,5,9,
     2.5,2.7,18,6.5,3,5,
     1.8,2.0,21,4.5,7,7,
     2.2,1.8,20,5.0,5,5)
d <- matrix(dm, ncol = 4,byrow=T)
w <- c(3, 1, 2,1, 1, 2)
i <- c("+", "+", "+", "-", "+", "+")

outcome=MCDM(d, w, i)

plot(1:nrow(outcome),outcome[,2],ylim=c(0,1),
     cex.axis=1.5,xaxt="n",xlab="方案",ylab="得分",
     main="多准则决策模型-TOPSIS评价方法", pch = 20, 
     cex =10*outcome[,2],col = rainbow(nrow(outcome)))
text(1:nrow(outcome),outcome[,2],as.character(outcome[,3]))
axis(1,labels=c("A","B","C","D"),at=1:4,las=1)


##参考Multiple Attribute Decision Making
##参考Introduction to Multiple Attribute Decision-making (MADM) Methods
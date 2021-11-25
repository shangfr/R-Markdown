require("XLConnect")

# 读取或创建一个XLSX文件，此步相当于建立一个连接

xls <- loadWorkbook('C:/Users/ShangFR/Desktop/test.xlsx',create=TRUE) 

# 创建工作表

createSheet(xls,name='namesheet')

# 写入数据

writeWorksheet(xls,iris,'namesheet',

               startRow=5,startCol=5, # 数据出现的左上角位置

               header=TRUE)

# 存入硬盘，直到此步方才有文档生成

saveWorkbook(xls)

# 上面四个步骤是新建文档、新建工作表、写入数据、最后存盘。如果要写入数据的同时创建好区域名称，则在第三步有所不同。

# 创建区域名

createName(xls,name='nameregion',

           formula='namesheet!$C$5', #区域的左上角单元格位置

           overwrite=TRUE)

# 写入数据

writeNamedRegion(xls,iris,name='nameregion')

读取文档则简单的多

xls <- loadWorkbook('test.xlsx',create=TRUE)

data <- readWorksheet(xls, 'Sheet3',

              startRow=1, startCol=1,

              endRow=0,endCol=0, #取0表示自动判断

              header=TRUE)


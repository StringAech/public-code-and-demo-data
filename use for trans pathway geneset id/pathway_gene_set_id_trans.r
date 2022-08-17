library(tidyverse)
library(dplyr)#%>% 需要调用这个包
library(clusterProfiler)#bitr()需要调用的包
library(org.Mm.eg.db)#小鼠的数据,我这里用的demodata是小鼠.其他物种
                    #需要安装对应的数据
data <- read.csv("TaxID_10090_pathway_No_Duplicates_Duplicate.csv",
                 sep = ",",
                 row.names = 1,
                 header = T)#读取文件,没什么好说的
data <- data %>%  #选定需要的列,transmute()这个函数很有意思,可以多注意
  transmute(
    name,
    category,
    source,
    taxname,
    taxid,
    geneids
  )
#定义一个注释函数ann,其作用是转化id
#=======================================================
ann <- function(a){
  c <- bitr(a$geneids%>%str_split("\\|")%>% unlist,
            fromType = "ENTREZID", 
            toType = "SYMBOL",
            OrgDb = "org.Mm.eg.db", 
            drop = TRUE)
  return(c)
  
}#注意,demo数据的geneid使用"|"分隔的.|是一个特殊符号,应该使用转义符"\\"
#======================================================
#准备一个data frame,不然再循环中合并哪里会报错,相当把第一次循环
#手动运行了,代码水平太差只能这样凑合了.
  a <- data[1,]
  c <- bitr(a$geneids%>%str_split("\\|")%>% unlist, 
            fromType = "ENTREZID", 
            toType = "SYMBOL", 
            OrgDb = "org.Mm.eg.db", 
            drop = TRUE)
  c <- paste0(c$SYMBOL,collapse = ",")
  a$GeneSymbol <- c
  a1 <- a
system.time(#查看for循环运行了多久,可以删除,但是注意括号删除干净
  for (i in 2: length(row.names(data))){#对每一行进行一次循环
    a <- data[i,]
    temp    <- try(c <- ann(a),
                   silent=FALSE)
    if('try-error' %in% class(temp))# 判断当前循环的try语句中的表达式是否运行正确
    {
      c <- paste0(row.names(a),"-NA") 
      a$GeneSymbol <- c
      a1 <- rbind(a1,a)# 如果循环报错,就运行这里
    }else{
      c <- paste0(c$SYMBOL,collapse = ",")
      a$GeneSymbol <- c
      a1 <- rbind(a1,a)#如果没有报错就运行这里
    }
    p <- i/length(row.names(data))#查看运行进度
    print(paste0(round(100*p, 2),"%"))#当通路特别多时候,运行时间很长
                                      #查看任务进度有助于平复心情
  }
)
write.csv(a1,"TaxID_10090_pathway_No_Duplicates_Duplicate_idtrans.csv",)


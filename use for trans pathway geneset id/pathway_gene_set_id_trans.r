library(tidyverse)
library(dplyr)
library(clusterProfiler)
library(org.Mm.eg.db)
datt <- read.csv("TaxID_10090_pathway_No_Duplicates_Duplicate.csv",
                 sep = ",",
                 row.names = 1,
                 header = T)
data <- datt %>%
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
  
}
#======================================================
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
  for (i in 2: length(row.names(data))){
    
    a <- data[i,]
    
    temp    <- try(c <- ann(a),
                   silent=FALSE)
    if('try-error' %in% class(temp))# 判断当前循环的try语句中的表达式是否运行正确
    {
      c <- paste0(row.names(a),"-NA") 
      a$GeneSymbol <- c
      a1 <- rbind(a1,a)# 此处可以对运行错误的情况进行处理应对
    }else{
      c <- paste0(c$SYMBOL,collapse = ",")
      a$GeneSymbol <- c
      a1 <- rbind(a1,a)
    }
    p <- i/length(row.names(data))
    print(paste0(round(100*p, 2),"%"))
  }
)


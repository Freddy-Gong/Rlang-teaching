##首先要对工作目录进行设置
fillNull<-function(tax){
  for(i in 1:nrow(tax)){
    for(j in 1:ncol(tax)){
      if(tax[i,j]==""){
        tax[i,j]<-"other"
        print(tax[i,j])
      }
    }
  }
  return(tax)
} 
otutable<-read.csv('otutable.csv', header=T)
tax<-read.csv('taxonomy.csv',header=F)
##对以上两个data.fram进行排序
otutable<-otutable[order(otutable[,1],decreasing = F),]
write.csv(otutable,'otutable.csv',row.names=FALSE)
tax<-tax[order(tax[,1],decreasing = F),]
##对tax行名进行重命名
colnames(tax)<-c("X.OTU.ID","Kingdom","phylum","class","order","family","genus")
##对缺失值进行处理
tax<-fillNull(tax)
write.csv(tax,'taxonomy.csv',row.names=FALSE)
otu_tax_table<-merge(otutable,tax,by="X.OTU.ID")
##将文件写出
write.csv(otu_tax_table,'otu_tax_table.csv',row.names=FALSE)





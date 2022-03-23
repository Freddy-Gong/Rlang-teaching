## 用来合并otu和taxnomy的脚本
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
table<-otutable[order(otutable[,1],decreasing = F),]
write.csv(otutable,'otutable.csv',row.names=FALSE)
tax<-tax[order(tax[,1],decreasing = F),]
colnames(tax)<-c("X.OTU.ID","Kingdom","phylum","class","order","family","genus")
tax<-fillNull(tax)
write.csv(tax,'taxonomy.csv',row.names=FALSE)
otu_tax_table<-merge(otutable,tax,by="X.OTU.ID")
write.csv(otu_tax_table,'otu_tax_table.csv',row.names=FALSE)





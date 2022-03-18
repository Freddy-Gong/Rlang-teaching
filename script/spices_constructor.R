
relativeAbundanceOtutable<-function(otutable){
  rowNumOtutable<-nrow(otutable)
  colNumOtutable<-ncol(otutable)
  result<-matrix(data=NA,nrow = rowNumOtutable,ncol = colNumOtutable)
  for (i in 1:rowNumOtutable) {
    for(j in 1:colNumOtutable){
      result[i,j]<-otutable[i,j]/otuSum[j]
    }
  }
  colnames(result)<-colnames(otutable)
  rownames(result)<-rownames(otutable)
  return(result)
}
generatorTab<-function(otu,tax,taxLevel){
  colNumOtutable<-ncol(otu)
  rowNumOtutable<-nrow(otu)
  reOtu<-relativeAbundanceOtutable(otu)
  taxCol<-which(colnames(tax)==taxLevel)
  Tax<-tax[,taxCol]
  uqTax<-unique(tax[,taxCol])
  result<-matrix(data=0,nrow = length(uqTax)+1,ncol = colNumOtutable+1)
  result[1,1]<-NA
  result[1:length(uqTax)+1,1]<-uqTax
  result[1,1:colNumOtutable+1]<-colnames(otu)
  for (i in 1:colNumOtutable) {
    for (j in 1:rowNumOtutable) {
      resultRow<-which(uqTax==Tax[j])
      result[resultRow+1,i+1]<-as.numeric(result[resultRow+1,i+1])+otu[j,i]
    }
  }
  return(result)
}
otu<-read.csv('otutable.csv', header=T,row.names = 1) 
tax<-read.csv('taxonomy.csv', header=T,row.names = 1) 
otuSum<- colSums(otu)
reOtu<-relativeAbundanceOtutable(otu)
re_class<-generatorTab(reOtu,tax,"class")
write.table(re_class, file = "re_class.csv",row.names=FALSE, na="",col.names=FALSE, sep=",")




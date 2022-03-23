data<-read.csv('re_class.csv', header=T,row.names = 1) 
group<-read.csv('design.csv',header = T)


annotation_col <- data.frame(Group=factor(group$group))
rownames(annotation_col) <- group$names


library(pheatmap)
install.packages("pheatmap")
pheatmap(head(data,n=10))
pheatmap(head(data,n=30),scale='column',cluster_rows = F)
pheatmap(head(data,n=30),scale='column',cluster_rows = F,annotation_col=annotation_col)
pheatmap(head(data,n=30),scale='column',cluster_rows = F,cluster_cols = F,annotation_col=annotation_col)

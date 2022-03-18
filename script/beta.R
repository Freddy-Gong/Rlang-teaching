#清除变量
rm(list=ls())
library(vegan)
library(ggplot2)
library(ggrepel)

otu<-read.csv('otutable.csv',row.names=1,header = T) 
#读取分组
group<-read.csv('design.csv',row.names=1,header = T) 
#--NMDS--------------------------------------------------------------
nmd<-t(otu)
nmds1 <- metaMDS(nmd, distance = 'bray', k = 4)
nmds1
#提取应力函数值（stress）
nmds1.stress <- nmds1$stress
#提取样本排序坐标
nmds1.point <- data.frame(nmds1$point)
#提取物种（OTU）排序坐标
nmds1.species <- data.frame(nmds1$species)
nmds_plot <- nmds1
nmds_plot$species <- {nmds_plot$species}[1:10, ]
ggplot(nmds1.point ,aes(x=MDS1,y=MDS2,shape=group$group,color=group$group))+
  geom_point(alpha=1,size=4)+stat_ellipse(level=0.95,size=3)+
  labs(x="NMDS1",y="NMDS2")+
  geom_vline(aes(xintercept=0),linetype="dotted")+
  geom_hline(aes(yintercept=0),linetype="dotted")+
  theme(panel.background = element_rect(fill='white',colour = 'black'),axis.title.x=element_text(colour = 'black',size=20),axis.title.y = element_text(colour = 'black',size=20),legend.text = element_text(size = 15))
#----------------------------------------------------------------
#数据转置
df1<-t(otu)
#计算距离
distance<-vegdist(df1,method='bray')
pcoa<- cmdscale(distance,k=(nrow(df1)-1),eig=TRUE)
#提取前两个分类解释
plot_data<-data.frame({pcoa$point})[1:2]
head(plot_data)
#前两个分类解释命名
names(plot_data)[1:2]<-c('PCoA1','PCoA2') 
eig=pcoa$eig
group1<-group['group']
data<-plot_data[match(rownames(group),rownames(plot_data)),]
data<-data.frame(group,plot_data)
head(data)
tail(data)
#作图
ggplot(data,aes(x=PCoA1,y=PCoA2,shape=group,color=group))+
  geom_point(alpha=1,size=4)+stat_ellipse(level=0.95,size=3)+
  labs(x=paste("PCoA1(",format(100*eig[1]/sum(eig),digits = 4),"%)",sep=""),y=paste("PCoA2(",format(100*eig[2]/sum(eig),digits = 4),"%)",sep=""))+
  geom_vline(aes(xintercept=0),linetype="dotted")+
  geom_hline(aes(yintercept=0),linetype="dotted")+
  theme(panel.background = element_rect(fill='white',colour = 'black'),axis.title.x=element_text(colour = 'black',size=20),axis.title.y = element_text(colour = 'black',size=20),legend.text = element_text(size = 15))

#-张老师喜欢的风格
ggplot(data, aes(PCoA1, PCoA2, color = group)) +
  theme(panel.grid = element_line(color = NA, linetype = 1, size = 0.1), panel.background = element_rect(color = 'black',size = 1, fill = 'transparent'), legend.key = element_rect(fill = 'transparent')) + #去掉背景桿
  geom_vline(xintercept = 0, color = 'black', size = 1) + 
  geom_hline(yintercept = 0, color = 'black', size = 1) +
  stat_ellipse(level = 0.95) +
  #geom_polygon(data = group_border, aes(fill = treatment)) + #绘制多边形区埿
  geom_point(aes( shape = group), size = 3, alpha = 1) + #可在这里修改点的透明度、大尿
  #scale_shape_manual(values = c(19,17,15)) + #可在这里修改点的形状
  scale_color_manual(values = c('black', 'black', 'black')) + #可在这里修改点的颜色
  scale_fill_manual(values = c('#C673FF2E', '#73D5FF2E', '#49C35A2E')) + #可在这里修改区块的颜艿
  #guides(fill = guide_legend(order = 1), shape = guide_legend(order = 2), color = guide_legend(order = 3)) + #设置图例展示顺序
  labs(x = paste('PCoA1: ', round(100 * eig[1], 2), '%'), y = paste('PCoA2: ', round(100 * eig[2], 2), '%')) 

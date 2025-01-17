alpha<-read.csv('alpha.csv',header = T)
#�����
library(ggplot2)
library(ggpubr) 
library(readxl)

#���÷����������
my_groups <- list(c("A", "B","C"))
#��ͼ�����ʽbox(ͼ1)
p=ggplot(alpha,aes(x=Sample,y=chao1,fill=Sample))+
  geom_boxplot(width = 0.3)+
  geom_jitter(width = 0.1)+
  theme(panel.background = element_rect(colour = 'black',size=2))+
  stat_compare_means(method ="t.test",comparisons=my_groups,label='p.signif',map_signif_level=T)
p

#��ͼ�������ɫbox��ͼ2��
p <- ggboxplot(alpha, x = "Sample", y = "chao1",color = "Sample",add="jitter",width =0.3,size=3)+
  xlab("Sample")+
  theme(panel.background=element_blank(),panel.border=element_rect(linetype="solid",fill=NA))+
  theme(axis.text=element_text(size=15,color="black"),axis.title=element_text(size=15,face="bold",color="black"))+
  theme(legend.position = "right",legend.text=element_text(face="bold",size=12))+
  stat_compare_means(method = "t.test",comparisons=my_groups,label='p.signif',map_signif_level=T)
p

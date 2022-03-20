otu<-read.csv('re_class.csv', header=T,row.names = 1) 
group<-read.csv('design.csv',header = T)
dis_bray<-vegan::vegdist(t(otu),method = 'bray')
tree<-hclust(dis_bray,method = 'average')
plot(tree)

grp <- group[1]
group_col <- c('red', 'blue')
names(group_col) <- c('1', '2')
group_name <- c('Control', 'Treat')
##??״ͼ ?޾???
#??????????ǩ
layout(t(c(1, 2, 2, 2, 3)))
par(mar = c(5, 2, 5, 0))
plot(0, type = 'n', xaxt = 'n', yaxt = 'n', frame.plot = FALSE, xlab = '', ylab = '',
     xlim = c(-max(tree$height), 0), ylim = c(0, length(tree$order)))
legend('topleft', legend = group_name, pch = 15, col = group_col, bty = 'n', cex = 1,text.font = 7)
#??????ɫ????
phylum_color <- c('#8DD3C7', '#FFFFB3', '#BEBADA', '#FB8072', '#80B1D3', '#FDB462', '#B3DE69', '#FCCDE5', '#BC80BD', '#CCEBC5')
names(phylum_color) <- rownames(otu)
#?ѵ?????ͼ
par(mar = c(5, 2, 5, 0))
bar <- barplot(as.matrix(otu), col = phylum_color, space = 0.4, width = 0.7, cex.axis = 1, horiz = T, cex.lab = 1.2,
               xlab = 'Relative Abundance', yaxt = 'n', las = 1, ylim = c(0, ncol(otu)), family = 'mono', )
mtext('Top 10 classes', side = 3, line = 1, cex = 1,font = 7)
color<-c("red","red","red","red","red","red","red","red","red","blue","blue","blue","blue","blue","blue","blue","blue","blue","blue")
text(x = -0.06, y = bar, labels = colnames(otu), col =color , xpd = TRUE,font = 7)
#????ͼͼ??
par(mar = c(5, 1, 5, 0))
plot(0, type = 'n', xaxt = 'n', yaxt = 'n', bty = 'n', xlab = '', ylab = '')
legend('left', pch = 15, col = phylum_color, legend = names(phylum_color), bty = 'n', cex = 1,text.font = 7)
##----------------------------------------------------------------------------------------
##?о???
#??????????ǩ
layout(t(c(1, 2, 2, 2, 3)))
par(mar = c(5, 2, 5, 0))
plot(0, type = 'n', xaxt = 'n', yaxt = 'n', frame.plot = FALSE, xlab = '', ylab = '',
     xlim = c(-max(tree$height), 0), ylim = c(0, length(tree$order)))
legend('topleft', legend = group_name, pch = 15, col = group_col, bty = 'n', cex = 1,text.font = 7)
#?????????ƣ???????????֧??ɫ
treeline <- function(pos1, pos2, height, col1, col2) {
  meanpos = (pos1[1] + pos2[1]) / 2
  segments(y0 = pos1[1] - 0.4, x0 = -pos1[2], y1 = pos1[1] - 0.4, x1 = -height,  col = col1,lwd = 2)
  segments(y0 = pos1[1] - 0.4, x0 = -height,  y1 = meanpos - 0.4, x1 = -height,  col = col1,lwd = 2)
  segments(y0 = meanpos - 0.4, x0 = -height,  y1 = pos2[1] - 0.4, x1 = -height,  col = col2,lwd = 2)
  segments(y0 = pos2[1] - 0.4, x0 = -height,  y1 = pos2[1] - 0.4, x1 = -pos2[2], col = col2,lwd = 2)
}
meanpos = matrix(rep(0, 2 * length(tree$order)), ncol = 2)
meancol = rep(0, length(tree$order))
for (step in 1:nrow(tree$merge)) {
  if(tree$merge[step, 1] < 0){
    pos1 <- c(which(tree$order == -tree$merge[step, 1]), 0)
    rowNum<-which(as.vector(grp)==tree$labels[-tree$merge[step, 1]])
    col1 <- group_col[group[rowNum,3]]
  } else {
    pos1 <- meanpos[tree$merge[step, 1], ]
    col1 <- meancol[tree$merge[step, 1]]
  }
  if (tree$merge[step, 2] < 0) {
    pos2 <- c(which(tree$order == -tree$merge[step, 2]), 0)
    rowNum<-which(as.vector(grp)==tree$labels[-tree$merge[step, 2]])
    col2 <- group_col[group[rowNum,3]]
  } else {
    pos2 <- meanpos[tree$merge[step, 2], ]
    col2 <- meancol[tree$merge[step, 2]]
  }
  height <- tree$height[step]
  treeline(pos1, pos2, height, col1, col2)
  meanpos[step, ] <- c((pos1[1] + pos2[1]) / 2, height)
  print(step)
  if (col1 == col2) meancol[step] <- col1 else meancol[step] <- 'grey'
}
##?ѵ?????ͼ
#????˳??????Ϊ?;??????е?˳??һ??
dat <- otu[ ,tree$order]
#??????ɫ????
phylum_color <- c('#8DD3C7', '#FFFFB3', '#BEBADA', '#FB8072', '#80B1D3', '#FDB462', '#B3DE69', '#FCCDE5', '#BC80BD', '#CCEBC5')
names(phylum_color) <- rownames(dat)
#?ѵ?????ͼ
par(mar = c(5, 4, 5, 0))
bar <- barplot(as.matrix(dat), col = phylum_color, space = 0.4, width = 0.7, cex.axis = 1, horiz = TRUE, cex.lab = 1.2,
               xlab = 'Relative Abundance', yaxt = 'n', las = 1, ylim = c(0, ncol(dat)), family = 'mono', )
mtext('Top 10 phylums', side = 3, line = 1, cex = 1,font = 7)
text(x = -0.07, y = bar, labels = colnames(dat), col = group_col[group[tree$order, 2]], xpd = TRUE,font = 7)
#????ͼͼ??
par(mar = c(5, 1, 5, 0))
plot(0, type = 'n', xaxt = 'n', yaxt = 'n', bty = 'n', xlab = '', ylab = '')
legend('left', pch = 15, col = phylum_color, legend = names(phylum_color), bty = 'n', cex = 1,text.font = 7)


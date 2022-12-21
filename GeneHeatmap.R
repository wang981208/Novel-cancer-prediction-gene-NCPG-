

#install.packages("pheatmap")


library(pheatmap)        
expFile="uniSigGeneExp.txt"        
geneCluFile="geneCluster.txt"      
m6aCluFile="m6aCluster.txt"        
cliFile="clinical.txt"            
setwd("D:\\biowolf\\m6aTME\\35.geneHeatmap")     

#读取输入文件
exp=read.table(expFile, header=T, sep="\t", check.names=F, row.names=1)
m6aClu=read.table(m6aCluFile, header=T, sep="\t", check.names=F, row.names=1)
geneClu=read.table(geneCluFile, header=T, sep="\t", check.names=F, row.names=1)

#合并数据
exp=as.data.frame(t(exp))
sameSample=intersect(row.names(exp), row.names(m6aClu))
exp=exp[sameSample,,drop=F]
expData=cbind(exp, geneCluster=geneClu[sameSample,], m6Acluster=m6aClu[sameSample,])
Project=gsub("(.*?)\\_.*", "\\1", rownames(expData))
rownames(expData)=gsub("(.*?)\\_(.*?)", "\\2", rownames(expData))
expData=cbind(expData, Project)

#合并临床数据
cli=read.table(cliFile, header=T, sep="\t", check.names=F, row.names=1)
sameSample=intersect(row.names(expData), row.names(cli))
expData=expData[sameSample,,drop=F]
cli=cli[sameSample,,drop=F]
data=cbind(expData, cli)

#提取热图数据
data=data[order(data$geneCluster),]
Type=data[,((ncol(data)-2-ncol(cli)):ncol(data))]
data=t(data[,1:(ncol(expData)-3)])

#聚类颜色
bioCol=c("#0066FF","#FF9900","#FF0000","#6E568C","#7CC767","#223D6C","#D20A13","#FFD121","#088247","#11AA4D")
ann_colors=list()
m6Acol=bioCol[1:length(levels(factor(Type$m6Acluster)))]
names(m6Acol)=levels(factor(Type$m6Acluster))
ann_colors[["m6Acluster"]]=m6Acol
GENEcol=bioCol[1:length(levels(factor(Type$geneCluster)))]
names(GENEcol)=levels(factor(Type$geneCluster))
ann_colors[["geneCluster"]]=GENEcol

#热图可视化
pdf("heatmap.pdf", height=6, width=8)
pheatmap(data,
         annotation=Type,
         annotation_colors = ann_colors,
         color = colorRampPalette(c(rep("blue",5), "white", rep("red",5)))(50),
         cluster_cols =F,
         cluster_rows =F,
         scale="row",
         show_colnames=F,
         show_rownames=F,
         fontsize=6,
         fontsize_row=2,
         fontsize_col=6)
dev.off()


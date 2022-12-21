

#if (!requireNamespace("BiocManager", quietly = TRUE))
#    install.packages("BiocManager")
#BiocManager::install("limma")

#install.packages("ggpubr")


#���ð�
library(limma)
library(ggpubr)
m6aCluFile="m6aCluster.txt"        #m6A�����ļ�
geneCluFile="geneCluster.txt"      #��������ļ�
scoreFile="m6Ascore.txt"           #m6A����ļ�
setwd("D:\\biowolf\\m6aTME\\41.clusterScore")     #���ù���Ŀ¼

#��ȡ�����ļ�
m6aClu=read.table(m6aCluFile, header=T, sep="\t", check.names=F, row.names=1)
geneClu=read.table(geneCluFile, header=T, sep="\t", check.names=F, row.names=1)
score=read.table(scoreFile, header=T, sep="\t", check.names=F, row.names=1)

#�ϲ�����
twoCluster=cbind(m6aClu, geneClu)
sameSample=intersect(row.names(twoCluster), row.names(score))
data=cbind(score[sameSample,,drop=F], twoCluster[sameSample,,drop=F])

#######m6A�������������########
#���ñȽ���
data$m6Acluster=factor(data$m6Acluster, levels=levels(factor(data$m6Acluster)))
group=levels(factor(data$m6Acluster))
comp=combn(group, 2)
my_comparisons=list()
for(i in 1:ncol(comp)){my_comparisons[[i]]<-comp[,i]}

#������ɫ
bioCol=c("#0066FF","#FF9900","#FF0000","#6E568C","#7CC767","#223D6C","#D20A13","#FFD121","#088247","#11AA4D")
bioCol=bioCol[1:length(levels(factor(data$m6Acluster)))]
	
#����boxplot
boxplot=ggboxplot(data, x="m6Acluster", y="m6Ascore", color="m6Acluster",
			      xlab="m6Acluster",
			      ylab="m6Ascore",
			      legend.title="m6Acluster",
			      palette=bioCol,
			      add = "jitter")+ 
	stat_compare_means(comparisons = my_comparisons)
	
#���ͼƬ
pdf(file="m6Acluster.pdf", width=5, height=4.5)
print(boxplot)
dev.off()
#######m6A�������������########


#######����������������########
#���ñȽ���
data$geneCluster=factor(data$geneCluster, levels=levels(factor(data$geneCluster)))
group=levels(factor(data$geneCluster))
comp=combn(group, 2)
my_comparisons=list()
for(i in 1:ncol(comp)){my_comparisons[[i]]<-comp[,i]}

#������ɫ
bioCol=c("#0066FF","#FF9900","#FF0000","#6E568C","#7CC767","#223D6C","#D20A13","#FFD121","#088247","#11AA4D")
bioCol=bioCol[1:length(levels(factor(data$geneCluster)))]
	
#����boxplot
boxplot=ggboxplot(data, x="geneCluster", y="m6Ascore", color="geneCluster",
			      xlab="geneCluster",
			      ylab="m6Ascore",
			      legend.title="geneCluster",
			      palette=bioCol,
			      add = "jitter")+ 
	stat_compare_means(comparisons = my_comparisons)
	
#���ͼƬ
pdf(file="geneCluster.pdf", width=5, height=4.5)
print(boxplot)
dev.off()
#######����������������########

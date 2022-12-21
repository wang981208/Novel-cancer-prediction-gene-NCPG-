

#install.packages("ggpubr")

#if (!requireNamespace("BiocManager", quietly = TRUE))
#    install.packages("BiocManager")
#BiocManager::install("limma")


#���ð�
library(limma)
library(ggpubr)
expFile="m6aGeneExp.txt"
mutFile="mutMatrix.txt"
mutGene="SYNE1"
setwd("D:\\biowolf\\m6aTME\\20.mutExp")

#��ȡ���������ļ�
exp=read.table(expFile, header=T, sep="\t", check.names=F, row.names=1)
colnames(exp)=gsub("(.*?)\\_(.*?)", "\\2", colnames(exp))
exp=t(exp)

#��ȡͻ�������ļ�
mut=read.table(mutFile, header=T, sep="\t", check.names=F, row.names=1)
mut=t(mut[mutGene,,drop=F])
colnames(mut)=c("Type")

#�ϲ�����
sameSample=intersect(row.names(mut), row.names(exp))
mut=mut[sameSample,,drop=F]
exp=exp[sameSample,,drop=F]
data=cbind(as.data.frame(exp), as.data.frame(mut))
data$Type=paste0(mutGene, " " , data$Type)

#���ñȽ���
data$Type=factor(data$Type, levels=c(paste0(mutGene, " Wild"), paste0(mutGene, " Mutation")) )
group=levels(factor(data$Type))
comp=combn(group,2)
my_comparisons=list()
for(i in 1:ncol(comp)){my_comparisons[[i]]<-comp[,i]}

for(gene in colnames(data)[1:(ncol(data)-1)]){
	data1=data[,c(gene, "Type")]
	colnames(data1)=c("expression", "Type")
	#��������ͼ
	boxplot=ggboxplot(data1, x="Type", y="expression", fill="Type",
				      xlab="",
				      ylab=paste0(gene, " expression"),
				      legend.title="",
				      palette=c("#0066FF", "#FF0000") )+ 
		stat_compare_means(comparisons = my_comparisons)
	#���ͼƬ
	pdf(file=paste0(mutGene, "(mut)_", gene,".pdf"), width=5, height=4.5)
	print(boxplot)
	dev.off()
}

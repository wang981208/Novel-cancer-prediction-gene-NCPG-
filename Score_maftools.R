

#if (!require("BiocManager"))
#    install.packages("BiocManager")
#BiocManager::install("maftools")


library(maftools)       #���ð�
setwd("D:\\biowolf\\m6aTME\\45.maftools")      #���ù���Ŀ¼

#��ȡm6A���ֵķ����ļ�
score=read.table("m6Ascore.group.txt", header=T, sep="\t", check.names=F)
outTab=score[,c(1, ncol(score))]
colnames(outTab)=c("Tumor_Sample_Barcode", "m6Ascore")
write.table(outTab, file="ann.txt", sep="\t", quote=F, row.names=F)

#��ȡ����ͻ���ļ�
geneNum=20
geneMut=read.table("geneMut.txt", header=T, sep="\t", check.names=F, row.names=1)
gene=row.names(geneMut)[1:geneNum]

#��ɫ
ann_colors=list()
col=c("#0066FF","#FF0000")
names(col)=c("Low", "High")
ann_colors[["m6Ascore"]]=col

#���������ٲ�ͼ
pdf(file="low.pdf", width=6, height=6)
maf=read.maf(maf="low.maf", clinicalData="ann.txt")
oncoplot(maf=maf, clinicalFeatures="m6Ascore", genes=gene, annotationColor=ann_colors, keepGeneOrder=T)
dev.off()

#���������ٲ�ͼ
pdf(file="high.pdf", width=6, height=6)
maf=read.maf(maf="high.maf", clinicalData="ann.txt")
oncoplot(maf=maf, clinicalFeatures="m6Ascore", genes=gene, annotationColor=ann_colors, keepGeneOrder=T)
dev.off()

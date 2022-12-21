

#if (!require("BiocManager"))
#    install.packages("BiocManager")
#BiocManager::install("maftools")


library(maftools)
setwd("D:\\biowolf\\m6aTME\\18.maftools")

#��ȡͻ������ļ�
geneRT=read.table("gene.txt", header=T, sep="\t", check.names=F, row.names=1)
gene=row.names(geneRT)

#�����ٲ�ͼ
pdf(file="oncoplot.pdf", width=6.5, height=6)
maf=read.maf(maf="input.maf")
oncoplot(maf=maf, genes=gene, draw_titv=T)
dev.off()

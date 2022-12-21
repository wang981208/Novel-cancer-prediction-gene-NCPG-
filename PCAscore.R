

expFile="uniSigGeneExp.txt"      #���������ļ�
setwd("D:\\COAD\\00COAD165\\37PCAscore")     #���ù���Ŀ¼

#��ȡ�����ļ�
data=read.table(expFile, header=T, sep="\t", check.names=F, row.names=1)
data=t(data)

#PCA����
pca=prcomp(data, scale=TRUE)
value=predict(pca)
m6Ascore=value[,1]+value[,2]
m6Ascore=as.data.frame(m6Ascore)
scoreOut=rbind(id=colnames(m6Ascore), m6Ascore)
write.table(scoreOut, file="m6Ascore.txt", sep="\t", quote=F, col.names=F)

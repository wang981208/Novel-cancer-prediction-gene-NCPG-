

#install.packages("survival")
#install.packages("survminer")


#���ð�
library(survival)
library(survminer)
scoreFile="m6Ascore.txt"     #m6A����ļ�
cliFile="time.txt"           #���������ļ�
setwd("D:\\COAD\\00COAD165\\38scoresur")      #���ù���Ŀ¼

#��ȡ�����ļ�
score=read.table(scoreFile, header=T, sep="\t", check.names=F, row.names=1)
sampleType=gsub("(.*?)\\_.*", "\\1", row.names(score))
score=cbind(score, sampleType)
rownames(score)=gsub("(.*?)\\_(.*?)", "\\2", rownames(score))
cli=read.table(cliFile, header=T, sep="\t", check.names=F, row.names=1)
colnames(cli)=c("futime", "fustat")
cli$futime=cli$futime/365

#���ݺϲ�
sameSample=intersect(row.names(score), row.names(cli))
data=cbind(cli[sameSample,], score[sameSample,])

#��ȡ����cutoff
res.cut=surv_cutpoint(data, time="futime", event="fustat", variables=c("m6Ascore"))
cutoff=as.numeric(res.cut$cutpoint[1])
print(cutoff)
Type=ifelse(data[,"m6Ascore"]<=cutoff, "Low", "High")
data$group=Type
outTab=rbind(id=colnames(data), data)
write.table(outTab, file="m6Ascore.group.txt", sep="\t", quote=F, col.names=F)

#����ߵͷ������������
data$group=factor(data$group, levels=c("Low", "High"))
diff=survdiff(Surv(futime, fustat) ~ group, data = data)
length=length(levels(factor(data[,"group"])))
pValue=1-pchisq(diff$chisq, df=length-1)
if(pValue<0.001){
	pValue="p<0.001"
}else{
	pValue=paste0("p=",sprintf("%.03f",pValue))
}
fit <- survfit(Surv(futime, fustat) ~ group, data = data)
#print(surv_median(fit))
	
#������������
bioCol=c("#0066FF","#FF0000","#6E568C","#7CC767","#223D6C","#D20A13","#FFD121","#088247","#11AA4D")
bioCol=bioCol[1:length]
surPlot=ggsurvplot(fit, 
			       data=data,
			       conf.int=F,
			       pval=pValue,
			       pval.size=6,
			       legend.title="m6Ascore",
			       legend.labs=levels(factor(data[,"group"])),
			       legend = c(0.8, 0.8),
			       font.legend=12,
			       xlab="Time(years)",
			       break.time.by = 3,
			       palette = bioCol,
			       surv.median.line = "hv",
			       risk.table=T,
			       cumevents=F,
			       risk.table.height=.25)

#����ͼƬ
pdf(file="survival.pdf", onefile = FALSE, width=6, height=5.5)
print(surPlot)
dev.off()
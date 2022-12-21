

#install.packages("survival")
#install.packages("survminer")


#���ð�
library(survival)
library(survminer)
tmbFile="TMB.txt"                  #����ͻ�为���ļ�
scoreFile="m6Ascore.group.txt"     #m6A��ֵķ����ļ�
setwd("D:\\biowolf\\m6aTME\\43.tmbSur")       #�޸Ĺ���Ŀ¼

#��ȡ�����ļ�
score=read.table(scoreFile, header=T, sep="\t", check.names=F, row.names=1)    #��ȡm6A��ֵķ����ļ�
tmb=read.table(tmbFile, header=T, sep="\t", check.names=F, row.names=1)        #��ȡTMB�����ļ�

#�ϲ�����
sameSample=intersect(row.names(tmb), row.names(score))
tmb=tmb[sameSample,,drop=F]
score=score[sameSample,,drop=F]
data=cbind(score, tmb)

#��ȡ����cutoff
res.cut=surv_cutpoint(data, time = "futime", event = "fustat", variables =c("TMB"))
cutoff=as.numeric(res.cut$cutpoint[1])
tmbType=ifelse(data[,"TMB"]<=cutoff, "L-TMB", "H-TMB")
scoreType=ifelse(data$group=="Low", "L-m6Ascore", "H-m6Ascore")
mergeType=paste0(tmbType, "+", scoreType)

#�������ߺ���
bioSurvival=function(surData=null, outFile=null){
	diff=survdiff(Surv(futime, fustat) ~ group, data=surData)
	length=length(levels(factor(surData[,"group"])))
	pValue=1-pchisq(diff$chisq, df=length-1)
	if(pValue<0.001){
		pValue="p<0.001"
	}else{
		pValue=paste0("p=",sprintf("%.03f",pValue))
	}
	fit <- survfit(Surv(futime, fustat) ~ group, data = surData)
	#print(surv_median(fit))
	
	#������������
	width=6.5
	height=5.5
	if(length(levels(factor(surData[,"group"])))>2){
		width=8
		height=6.5
	}
	bioCol=c("#FF0000","#0066FF","#6E568C","#7CC767","#223D6C","#D20A13","#FFD121","#088247","#11AA4D")
	bioCol=bioCol[1:length]
	surPlot=ggsurvplot(fit, 
			           data=surData,
			           conf.int=F,
			           pval=pValue,
			           pval.size=6,
			           legend.title="",
			           legend.labs=levels(factor(surData[,"group"])),
			           font.legend=10,
			           legend = c(0.8, 0.8),
			           xlab="Time(years)",
			           break.time.by = 1,
			           palette = bioCol,
			           surv.median.line = "hv",
			           risk.table=T,
			           cumevents=F,
			           risk.table.height=.25)
	#���ͼ��
	pdf(file=outFile, onefile = FALSE, width=width, height=height)
	print(surPlot)
	dev.off()
}

#����TMB����������
data$group=tmbType
bioSurvival(surData=data, outFile="TMB.survival.pdf")

#����TMB����m6A��ֵ���������
data$group=mergeType
bioSurvival(surData=data, outFile="TMB-score.survival.pdf")


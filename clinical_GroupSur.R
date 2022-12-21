
#install.packages("survival")
#install.packages("survminer")


#引用包
library(survival)
library(survminer)
scoreFile="m6Ascore.group.txt"     
cliFile="clinical.txt"             
trait="T"                
setwd("D:\\biowolf\\m6aTME\\47.cliGroupSur")         

#读取输入文件
score=read.table(scoreFile, header=T, sep="\t",check.names=F, row.names=1)
cli=read.table(cliFile, header=T, sep="\t", check.names=F, row.names=1)
sameSample=intersect(row.names(cli), row.names(score))
score=score[sameSample,]
cli=cli[sameSample,]
data=cbind(futime=score[,1], fustat=score[,2], cli, group=score[,"group"])

#提取临床数据
rt=data[,c("futime", "fustat", trait, "group")]
rt=rt[(rt[,trait]!="unknow"),]
colnames(rt)=c("futime", "fustat", "clinical", "group")
tab=table(rt[,"clinical"])
tab=tab[tab!=0]

#对每个临床信息里面的分类进行循环
for(j in names(tab)){
	rt1=rt[(rt[,"clinical"]==j),]
	tab1=table(rt1[,"group"])
	tab1=tab1[tab1!=0]
	labels=names(tab1)
	if(length(labels)==2){
		titleName=j
		if((trait=="age") | (trait=="Age") | (trait=="AGE")){
			titleName=paste0("age", j)
		}
		diff=survdiff(Surv(futime, fustat) ~group,data = rt1)
		pValue=1-pchisq(diff$chisq, df=1)
		if(pValue<0.001){
			pValue="p<0.001"
		}else{
			pValue=paste0("p=",sprintf("%.03f", pValue))
		}
		fit <- survfit(Surv(futime, fustat) ~ group, data = rt1)
		bioCol=c("#FF0000","#0066FF","#FF9900","#6E568C","#7CC767","#223D6C","#D20A13","#FFD121","#088247","#11AA4D")
		bioCol=bioCol[1:length(levels(factor(rt1[,"group"])))]
		#绘制生存曲线
		surPlot=ggsurvplot(fit, 
			           data=rt1,
			           conf.int=F,
			           pval=pValue,
			           pval.size=6,
			           title=paste0("Patients with ",titleName),
			           legend.title="m6Ascore",
			           legend.labs=labels,
			           font.legend=12,
			           xlab="Time(years)",
			           break.time.by = 1,
			           palette=bioCol,
			           risk.table=TRUE,
			       	   risk.table.title="",
			           risk.table.col = "strata",
			           risk.table.height=.25)
		#输出图片
		j=gsub(">=","ge",j);j=gsub("<=","le",j);j=gsub(">","gt",j);j=gsub("<","lt",j)
		pdf(file=paste0(trait,"_",j,".pdf"), onefile = FALSE, width = 5.5, height =5)
		print(surPlot)
		dev.off()
	}
}

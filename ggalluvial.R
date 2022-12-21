

#install.packages("ggplot2")
#install.packages("ggalluvial")


#���ð�
library(ggalluvial)
library(ggplot2)
library(dplyr)
m6aCluFile="m6aCluster.txt"         
geneCluFile="geneCluster.txt"       
scoreFile="m6Ascore.group.txt"     
cliFile="clinical.txt"           
trait="Fustat"                      
setwd("D:\\biowolf\\m6aTME\\39.ggalluvial")    

#��ȡ�����ļ�
m6aClu=read.table(m6aCluFile, header=T, sep="\t", check.names=F, row.names=1)
geneClu=read.table(geneCluFile, header=T, sep="\t", check.names=F, row.names=1)
score=read.table(scoreFile, header=T, sep="\t", check.names=F, row.names=1)
cli=read.table(cliFile, header=T, sep="\t", check.names=F, row.names=1)

#�ϲ�����
twoCluster=cbind(m6aClu, geneClu)
rownames(twoCluster)=gsub("(.*?)\\_(.*?)", "\\2", rownames(twoCluster))
sameSample=intersect(row.names(twoCluster), row.names(score))
scoreClu=cbind(score[sameSample,,drop=F], twoCluster[sameSample,,drop=F])
sameSample=intersect(row.names(scoreClu), row.names(cli))
rt=cbind(scoreClu[sameSample,], cli[sameSample,])

#׼��ɣ��ͼ�����ļ�
rt=rt[,c("m6Acluster", "geneCluster", "group", trait)]
colnames(rt)=c("m6Acluster", "geneCluster", "m6Ascore", trait)
corLodes=to_lodes_form(rt, axes = 1:ncol(rt), id = "Cohort")

#�õ�����ļ�
pdf(file="ggalluvial.pdf", width=6, height=6)
mycol=rep(c("#0066FF","#FF9900","#FF0000","#029149","#6E568C","#E0367A","#D8D155","#223D6C","#D20A13","#431A3D","#91612D","#FFD121","#088247","#11AA4D","#58CDD9","#7A142C","#5D90BA","#64495D","#7CC767"),15)
ggplot(corLodes, aes(x = x, stratum = stratum, alluvium = Cohort,fill = stratum, label = stratum)) +
  	 scale_x_discrete(expand = c(0, 0)) +  
  	 #��aes.flow����������ɫ��forward˵����ɫ��ǰ�����״ͼһ�£�backward˵���ͺ������״ͼһ�¡�
  	 geom_flow(width = 2/10,aes.flow = "forward") + 
	 geom_stratum(alpha = .9,width = 2/10) +
	 scale_fill_manual(values = mycol) +
	 #size=3���������С
	 geom_text(stat = "stratum", size = 3,color="black") +
	 xlab("") + ylab("") + theme_bw() + 
	 theme(axis.line = element_blank(),axis.ticks = element_blank(),axis.text.y = element_blank()) + #ȥ��������
	 theme(panel.grid =element_blank()) + 
	 theme(panel.border = element_blank()) + 
	 ggtitle("") + guides(fill = FALSE)                            
dev.off()

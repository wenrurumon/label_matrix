###################################################################
#ALL Loaded
###################################################################

# save(euc2,lab2,ec2,eu2,dmp,file='dmp.rda')
# save(al,lab2,euc2_demo,w,checklabel,dmp,file='dmp_demo.rda')

library(data.table)
library(dplyr)
setwd('C:\\Users\\WenluluSens\\Documents\\Project\\SohuVedio\\dmp\\')
#al 是所有视频的信息
#euc2_demo是从所有用户中提取了10000个样本的一个测试集
#lab2是标签体系
#函数dmp是输入了样本观看视频的信息以及标签体系矩阵以后，输出其标签得分
#函数checklabel是来看第i个label的极端剧用的
load('dmp_demo.rda')
#X.input是从测试集里随便抽取一个样本的所有记录
x.input <- as.data.table(select(filter(euc2_demo,uid==sample(euc2_demo$uid,1)),uid,cid,score=score2))
dmp(euc.input=x.input)
checklabel(2)

#Shiny sample
setwd('C:\\Users\\WenluluSens\\Documents\\Project\\SohuVedio\\dmp')
library(shiny)
runApp()

# 1、formula准备------------------------------------------
data=read.csv('data\\treat_two_hundred.csv',header = T)
data1=read.csv('data\\new.csv',header = T)
data2=read.csv('data\\new1.csv',header = T)
data3=read.csv('data\\new2.csv',header = T)
data1=data1[,-23]
all_data=cbind(data,data1,data2,data3)
# 分BP的路径：
{
  library(stringr)
  formula=read.csv2('data\\6810分BP.csv',header = F,stringsAsFactors = F)
  R6A=formula[1:8,];R6B=formula[10:17,]
  R8A=formula[19:30,];R8B=formula[32:43,]
  R10A=formula[45:60,];R10B=formula[62:77,]
  Formula=function(R){
    R_=str_split(R,pattern = ',',simplify = T)
    R_B=apply(R_[,1:3], 1, function(x)str_c(x,collapse = '-'))
    R_P=apply(R_[,4:6], 1, function(x)str_c(x,collapse = '-'))
    return(cbind(str_c(R_B,collapse = '|'), str_c(R_P,collapse = '|')))
  }
  R6A=Formula(R6A);R6B=Formula(R6B)
  R8A=Formula(R8A);R8B=Formula(R8B)
  R10A=Formula(R10A);R10B=Formula(R10B)
  
  path_BP=matrix(c(R6A,R8A,R10A, R6A,R8B,R10A, R6A,R8A,R10B, R6A,R8B,R10B, R6B,R8A,R10A, R6B,R8B,R10A, R6B,R8A,R10B, R6B,R8B,R10B),byrow = T,ncol = 6)
  colnames(path_BP)=c('B_R6','P_R6','B_R8','P_R8','B_R10','P_R10')
  
}
# 不分BP的路径：
{
  library(stringr)
  formula=read.table('data\\R6810.txt',header = T,stringsAsFactors = F)
  R6=formula[1:10,];R8=formula[12:25,];R10=formula[27:44,]
  R6=str_split(R6,pattern = ',',simplify = T)
  R6_1=apply(R6[,1:3], 1,function(x)str_c(x,collapse = '-'))
  R6_2=apply(R6[,4:6], 1,function(x)str_c(x,collapse = '-'))
  R61=str_c(R6_1,collapse = '|')
  R62=str_c(R6_2,collapse = '|')
  R8=str_split(R8,pattern = ',',simplify = T)
  R8_1=apply(R8[,1:3], 1,function(x)str_c(x,collapse = '-'))
  R8_2=apply(R8[,4:6], 1,function(x)str_c(x,collapse = '-'))
  R81=str_c(R8_1,collapse = '|')
  R82=str_c(R8_2,collapse = '|')
  R10=str_split(R10,pattern = ',',simplify = T)
  R10_1=apply(R10[,1:3], 1,function(x)str_c(x,collapse = '-'))
  R10_2=apply(R10[,4:6], 1,function(x)str_c(x,collapse = '-'))
  R101=str_c(R10_1,collapse = '|')
  R102=str_c(R10_2,collapse = '|')
  
  path=matrix(c(R61,R81,R101, R61,R82,R101, R61,R81,R102, R61,R82,R102, R62,R81,R101, R62,R82,R101, R62,R81,R102, R62,R82,R102),byrow = T,ncol = 3)
  colnames(path)=c('R6','R8','R10')
}
c=c('data','all_data','path_BP','path')
a=ls()
rm(list=(setdiff(a,c)))
gc()
library(stringr)
# 2、训练==================================================

# 2.1定义计算每10个真实数据下的真实路径final_comb:
true_path0=function(dataset){
  t0=table(dataset[1:10])
  result0=paste0(t0[1],t0[2])
  t1=table(dataset[1:9])
  result1=paste0(t1[1],t1[2])
  t2=table(dataset[1:8])
  result2=paste0(t2[1],t2[2])
  comb1=paste0(result2,'-',result1,'-',result0)
  
  t3=table(dataset[3:10])
  result3=paste0(t3[1],t3[2])
  t4=table(dataset[3:9])
  result4=paste0(t4[1],t4[2])
  t5=table(dataset[3:8])
  result5=paste0(t5[1],t5[2])
  comb2=paste0(result5,'-',result4,'-',result3)
  
  t6=table(dataset[5:10])
  result6=paste0(t6[1],t6[2])
  t7=table(dataset[5:9])
  result7=paste0(t7[1],t7[2])
  t8=table(dataset[5:8])
  result8=paste0(t8[1],t8[2])
  comb3=paste0(result8,'-',result7,'-',result6)
  return(matrix(c(comb1,comb2,comb3),ncol = 3))
  
}
true_path1=function(dataset){
  t0=table(dataset[c(1:9,10)])
  result0=paste0(t0[1],t0[2])
  t1=table(dataset[c(1:9)])
  result1=paste0(t1[1],t1[2])
  t2=table(dataset[c(1:8)])
  result2=paste0(t2[1],t2[2])
  comb1=paste0(result2,'-',result1,'-',result0)
  
  t3=table(dataset[c(1:7,10)])
  result3=paste0(t3[1],t3[2])
  t4=table(dataset[c(1:7)])
  result4=paste0(t4[1],t4[2])
  t5=table(dataset[c(1:6)])
  result5=paste0(t5[1],t5[2])
  comb2=paste0(result5,'-',result4,'-',result3)
  
  t6=table(dataset[c(1:5,10)])
  result6=paste0(t6[1],t6[2])
  t7=table(dataset[c(1:5)])
  result7=paste0(t7[1],t7[2])
  t8=table(dataset[c(1:4)])
  result8=paste0(t8[1],t8[2])
  comb3=paste0(result8,'-',result7,'-',result6)
  return(matrix(c(comb1,comb2,comb3),ncol = 3))
  
}
# 2.2定义10个真实数据下根据真实路径下的量化值-1或1:
# path=path_BP为分Bp的：

# Inverse有3种形式F，1，2，代表不同的取数据计算真实路径的方式，对应true_path0~2
new_quantize=function(path,set,Inverse=F){
  # 分BP的path有6列：
  if(Inverse==1)real_comb=true_path1(set)
  else if (Inverse==F)real_comb=true_path0(set)
  if(ncol(path)==6){
    r10=matrix(0,ncol = nrow(path),nrow = 1)
    for (j in 1:nrow(path)) {
      sparate_condition=ifelse(Inverse==1,set[1],set[1])
      r10[1,j]=ifelse(sparate_condition=='B'|sparate_condition==1,ifelse(str_detect(real_comb[1,1],path[j,'B_R10']),1,-1),ifelse(str_detect(real_comb[1,1],path[j,'P_R10']),1,-1))
    }
    r8=matrix(0,ncol = nrow(path),nrow = 1)
    for (j in 1:nrow(path)) {
      sparate_condition=ifelse(Inverse==1,set[1],set[3])
      r8[1,j]=ifelse(sparate_condition=='B'|sparate_condition==1,ifelse(str_detect(real_comb[1,2],path[j,'B_R8']),1,-1),ifelse(str_detect(real_comb[1,2],path[j,'P_R8']),1,-1))
    }
    r6=matrix(0,ncol = nrow(path),nrow = 1)
    for (j in 1:nrow(path)) {
      sparate_condition=ifelse(Inverse==1,set[1],set[5])
      r6[1,j]=ifelse(sparate_condition=='B'|sparate_condition==1,ifelse(str_detect(real_comb[1,3],path[j,'B_R6']),1,-1),ifelse(str_detect(real_comb[1,3],path[j,'P_R6']),1,-1))
    }
  }
  else if(ncol(path)==3){
    r10=matrix(0,ncol = nrow(path),nrow = 1)
    for (j in 1:nrow(path)) {
      r10[1,j]=ifelse(str_detect(real_comb[1,1],path[j,'R10']),1,-1)
    }
    r8=matrix(0,ncol = nrow(path),nrow = 1)
    for (j in 1:nrow(path)) {
      r8[1,j]=ifelse(str_detect(real_comb[1,2],path[j,'R8']),1,-1)
    }
    r6=matrix(0,ncol = nrow(path),nrow = 1)
    for (j in 1:nrow(path)) {
      r6[1,j]=ifelse(str_detect(real_comb[1,3],path[j,'R6']),1,-1)
    }
  }
  return(r10+r8+r6)
} 

# 2.3 定义诊断连续负的长度：
detect_continue_minus=function(sumtemp){
  continue_minus_num=c()
  for (i in 1:ncol(sumtemp)) {
    # 转化成字符串，查看多少个“0”在字符串内，例如”00000“这部分是否在字符串内
    strings=str_c(ifelse(sumtemp[,i]<0,0,1),collapse = '')
    for (k in 1:nrow(sumtemp)) {
      pattern=str_c(paste0(rep(0,k)),collapse = '')
      if(!str_detect(strings,pattern)){
        #print(paste0('最大连续为负个数',k-1))
        continue_minus_num[i]=k-1
        break
      }
    }
  }
  return(continue_minus_num)
}

# 2.4 定义取数据的方式：
{
  # 单轨从10个数据开始，连续取数据
  data_sort1=function(data=data,m,path){
    n=nrow(data)
    # 第一手
    {
      first_hand=matrix(ncol = nrow(path))
      for (i in seq(10,n,1)) {
        first_hand_quantity=new_quantize(path,set = data[(i-9):i,m])
        first_hand=rbind(first_hand,first_hand_quantity)
      }
      first_hand=first_hand[-1,]
    }
    return(list(first_hand))
  }
  # 平行双轨从21开始：
  data_sort2=function(data=data,m,path){
    n=nrow(data)
    # 第1手
    {
      first_hand=matrix(ncol = nrow(path))
      for (i in seq(21,n,2)) {
        first_hand_quantity=new_quantize(path,set = data[seq((i-18),i,2),m])
        first_hand=rbind(first_hand,first_hand_quantity)
      }
      first_hand=first_hand[-1,]
    }
    # 第2手
    {
      second_hand=matrix(ncol = nrow(path))
      for (i in seq(22,n,2)) {
        second_hand_quantity=new_quantize(path,set = data[seq((i-18),i,2),m])
        second_hand=rbind(second_hand,second_hand_quantity)
      }
      second_hand=second_hand[-1,]
    }
    return(list(first_hand,second_hand))
  }
  # plan31-1:从10开始取数据
  data_sort3=function(data=data,m,path){
    n=nrow(data)
    # 第一手
    {
      first_hand=matrix(ncol = nrow(path))
      for (i in seq(10,n,2)) {
        first_hand_quantity=new_quantize(path,set = data[(i-9):i,m])
        first_hand=rbind(first_hand,first_hand_quantity)
      }
      first_hand=first_hand[-1,]
    }
    # 第二手
    {
      second_hand=matrix(ncol = nrow(path))
      for (i in seq(11,n,2)) {
        second_hand_quantity=new_quantize(path,set=data[c((i-1):(i-9),i),m],Inverse = T)
        second_hand=rbind(second_hand,second_hand_quantity)
      }
      second_hand=second_hand[-1,]
    }
    return(list(first_hand,second_hand))
  }
  # 1、2均逆序+显现：
  data_sort31=function(data=data,m,path){
    n=nrow(data)
    # 第一手
    {
      first_hand=matrix(ncol = nrow(path))
      for (i in seq(10,n,2)) {
        first_hand_quantity=new_quantize(path,set = data[c((i-1):(i-9),i),m],Inverse = T)
        first_hand=rbind(first_hand,first_hand_quantity)
      }
      first_hand=first_hand[-1,]
    }
    # 第二手
    {
      second_hand=matrix(ncol = nrow(path))
      for (i in seq(11,n,2)) {
        second_hand_quantity=new_quantize(path,set=data[c((i-1):(i-9),i),m],Inverse = T)
        second_hand=rbind(second_hand,second_hand_quantity)
      }
      second_hand=second_hand[-1,]
    }
    return(list(first_hand,second_hand))
  }
  data_sort31_wrong=function(data=data,m,path){
    n=nrow(data)
    # 第一手
    {
      first_hand=matrix(ncol = nrow(path))
      for (i in seq(10,n,2)) {
        first_hand_quantity=new_quantize(path,set = data[c((i-1):(i-9),i),m])
        first_hand=rbind(first_hand,first_hand_quantity)
      }
      first_hand=first_hand[-1,]
    }
    # 第二手
    {
      second_hand=matrix(ncol = nrow(path))
      for (i in seq(11,n,2)) {
        second_hand_quantity=new_quantize(path,set=data[c((i-1):(i-9),i),m])
        second_hand=rbind(second_hand,second_hand_quantity)
      }
      second_hand=second_hand[-1,]
    }
    return(list(first_hand,second_hand))
  }
  # plan31-2:从10开始取数据_逆序+reflection
  data_sort41=function(data=data,m,path){
    n=nrow(data)
    # 第一手:顺+显
    {
      first_hand=matrix(ncol = nrow(path))
      for (i in seq(10,n,2)) {
        first_hand_quantity=new_quantize(path,set = data[c((i-9):(i-1),i),m])
        first_hand=rbind(first_hand,first_hand_quantity)
      }
      first_hand=first_hand[-1,]
    }
    # 第二手:逆+re
    {
      second_hand=matrix(ncol = nrow(path) )
      reflaction=ifelse(data[,m]=="B",'P','B')
      for (i in seq(11,n,2)) {
        rejection=factor(c(reflaction[(i-1):(i-9)],as.character(data[i,m])),levels = c('B',"P"))
        second_hand_quantity=new_quantize(path,set=rejection,Inverse=T)
        second_hand=rbind(second_hand,second_hand_quantity)
      }
      second_hand=second_hand[-1,]
    }
    return(list(first_hand,second_hand))
  }
  data_sort45=function(data=data,m,path){
    n=nrow(data)
    # 第一手:顺+re
    {
      reflaction=ifelse(data[,m]=="B",'P','B')
      first_hand=matrix(ncol = nrow(path))
      for (i in seq(10,n,2)) {
        rejection=factor(c(reflaction[(i-9):(i-1)],as.character(data[i,m])),levels = c('B',"P"))
        first_hand_quantity=new_quantize(path,set = rejection)
        first_hand=rbind(first_hand,first_hand_quantity)
      }
      first_hand=first_hand[-1,]
    }
    # 第二手:逆+显
    {
      second_hand=matrix(ncol = nrow(path))
      for (i in seq(11,n,2)) {
        second_hand_quantity=new_quantize(path,set=data[c((i-1):(i-9),i),m],Inverse = T)
        second_hand=rbind(second_hand,second_hand_quantity)
      }
      second_hand=second_hand[-1,]
    }
    return(list(first_hand,second_hand))
  }
  data_sort42=function(data=data,m,path){
    n=nrow(data)
    # 第一手:顺+显
    {
      first_hand=matrix(ncol = nrow(path))
      for (i in seq(10,n,2)) {
        first_hand_quantity=new_quantize(path,set = data[c((i-9):(i-1),i),m])
        first_hand=rbind(first_hand,first_hand_quantity)
      }
      first_hand=first_hand[-1,]
    }
    # 第二手:顺+re
    {
      second_hand=matrix(ncol = nrow(path) )
      reflaction=ifelse(data[,m]=="B",'P','B')
      for (i in seq(11,n,2)) {
        rejection=factor(c(reflaction[(i-9):(i-1)],as.character(data[i,m])),levels = c('B',"P"))
        second_hand_quantity=new_quantize(path,set=rejection)
        second_hand=rbind(second_hand,second_hand_quantity)
      }
      second_hand=second_hand[-1,]
    }
    return(list(first_hand,second_hand))
  }
  data_sort43=function(data=data,m,path){
    n=nrow(data)
    # 第一手:逆+显
    {
      first_hand=matrix(ncol = nrow(path))
      for (i in seq(10,n,2)) {
        first_hand_quantity=new_quantize(path,set = data[c((i-1):(i-9),i),m],Inverse = T)
        first_hand=rbind(first_hand,first_hand_quantity)
      }
      first_hand=first_hand[-1,]
    }
    # 第二手:顺+re
    {
      second_hand=matrix(ncol = nrow(path) )
      reflaction=ifelse(data[,m]=="B",'P','B')
      for (i in seq(11,n,2)) {
        rejection=factor(c(reflaction[(i-9):(i-1)],as.character(data[i,m])),levels = c('B',"P"))
        second_hand_quantity=new_quantize(path,set=rejection)
        second_hand=rbind(second_hand,second_hand_quantity)
      }
      second_hand=second_hand[-1,]
    }
    return(list(first_hand,second_hand))
  }
  data_sort44=function(data=data,m,path){
    n=nrow(data)
    # 第一手:逆+显
    {
      first_hand=matrix(ncol = nrow(path))
      for (i in seq(10,n,2)) {
        first_hand_quantity=new_quantize(path,set = data[c((i-1):(i-9),i),m],Inverse = T)
        first_hand=rbind(first_hand,first_hand_quantity)
      }
      first_hand=first_hand[-1,]
    }
    # 第二手:逆+re
    {
      second_hand=matrix(ncol = nrow(path) )
      reflaction=ifelse(data[,m]=="B",'P','B')
      for (i in seq(11,n,2)) {
        rejection=factor(c(reflaction[(i-1):(i-9)],as.character(data[i,m])),levels = c('B',"P"))
        second_hand_quantity=new_quantize(path,set=rejection,Inverse=T)
        second_hand=rbind(second_hand,second_hand_quantity)
      }
      second_hand=second_hand[-1,]
    }
    return(list(first_hand,second_hand))
  }
  data_sort41_wrong=function(data=data,m,path){
    n=nrow(data)
    # 第一手:顺+显
    {
      first_hand=matrix(ncol = nrow(path))
      for (i in seq(10,n,2)) {
        first_hand_quantity=new_quantize(path,set = data[c((i-9):(i-1),i),m])
        first_hand=rbind(first_hand,first_hand_quantity)
      }
      first_hand=first_hand[-1,]
    }
    # 第二手:逆+re
    {
      second_hand=matrix(ncol = nrow(path) )
      reflaction=ifelse(data[,m]=="B",'P','B')
      for (i in seq(11,n,2)) {
        rejection=factor(c(reflaction[(i-1):(i-9)],as.character(data[i,m])),levels = c('B',"P"))
        second_hand_quantity=new_quantize(path,set=rejection)
        second_hand=rbind(second_hand,second_hand_quantity)
      }
      second_hand=second_hand[-1,]
    }
    return(list(first_hand,second_hand))
  }
  data_sort42_wrong=function(data=data,m,path){
    n=nrow(data)
    # 第一手:顺+显
    {
      first_hand=matrix(ncol = nrow(path))
      for (i in seq(10,n,2)) {
        first_hand_quantity=new_quantize(path,set = data[c((i-9):(i-1),i),m])
        first_hand=rbind(first_hand,first_hand_quantity)
      }
      first_hand=first_hand[-1,]
    }
    # 第二手:顺+re
    {
      second_hand=matrix(ncol = nrow(path) )
      reflaction=ifelse(data[,m]=="B",'P','B')
      for (i in seq(11,n,2)) {
        rejection=factor(c(reflaction[(i-9):(i-1)],as.character(data[i,m])),levels = c('B',"P"))
        second_hand_quantity=new_quantize(path,set=rejection)
        second_hand=rbind(second_hand,second_hand_quantity)
      }
      second_hand=second_hand[-1,]
    }
    return(list(first_hand,second_hand))
  }
  data_sort43_wrong=function(data=data,m,path){
    n=nrow(data)
    # 第一手:逆+显
    {
      first_hand=matrix(ncol = nrow(path))
      for (i in seq(10,n,2)) {
        first_hand_quantity=new_quantize(path,set = data[c((i-1):(i-9),i),m])
        first_hand=rbind(first_hand,first_hand_quantity)
      }
      first_hand=first_hand[-1,]
    }
    # 第二手:顺+re
    {
      second_hand=matrix(ncol = nrow(path) )
      reflaction=ifelse(data[,m]=="B",'P','B')
      for (i in seq(11,n,2)) {
        rejection=factor(c(reflaction[(i-9):(i-1)],as.character(data[i,m])),levels = c('B',"P"))
        second_hand_quantity=new_quantize(path,set=rejection)
        second_hand=rbind(second_hand,second_hand_quantity)
      }
      second_hand=second_hand[-1,]
    }
    return(list(first_hand,second_hand))
  }
  data_sort44_wrong=function(data=data,m,path){
    n=nrow(data)
    # 第一手:错误逆+显
    {
      first_hand=matrix(ncol = nrow(path))
      for (i in seq(10,n,2)) {
        first_hand_quantity=new_quantize(path,set = data[c((i-1):(i-9),i),m])
        first_hand=rbind(first_hand,first_hand_quantity)
      }
      first_hand=first_hand[-1,]
    }
    # 第二手:错误逆+re
    {
      second_hand=matrix(ncol = nrow(path) )
      reflaction=ifelse(data[,m]=="B",'P','B')
      for (i in seq(11,n,2)) {
        rejection=factor(c(reflaction[(i-1):(i-9)],as.character(data[i,m])),levels = c('B',"P"))
        second_hand_quantity=new_quantize(path,set=rejection)
        second_hand=rbind(second_hand,second_hand_quantity)
      }
      second_hand=second_hand[-1,]
    }
    return(list(first_hand,second_hand))
  }
  data_sort48_wrong=function(data=data,m,path){
    n=nrow(data)
    # 第一手:错误逆+re
    {
      first_hand=matrix(ncol = nrow(path))
      reflaction=ifelse(data[,m]=="B",'P','B')
      for (i in seq(10,n,2)) {
        rejection=factor(c(reflaction[(i-1):(i-9)],as.character(data[i,m])),levels = c('B',"P"))
        first_hand_quantity=new_quantize(path,set = rejection)
        first_hand=rbind(first_hand,first_hand_quantity)
      }
      first_hand=first_hand[-1,]
    }
    # 第二手:错误逆+显
    {
      second_hand=matrix(ncol = nrow(path) )
      for (i in seq(11,n,2)) {
        second_hand_quantity=new_quantize(path,set=data[c((i-1):(i-9),i),m])
        second_hand=rbind(second_hand,second_hand_quantity)
      }
      second_hand=second_hand[-1,]
    }
    return(list(first_hand,second_hand))
  }
  # 双轨从21开始取数据
  #正确版本：逆序时Inverse = T
  data_sort50=function(data=data,m,path=path){
    n=nrow(data)
    # 第一手：显+逆
    {
      first_hand=matrix(ncol = nrow(path))
      for (i in seq(21,n,4)) {
        first_hand_quantity=new_quantize(path,set = data[seq(i,i-18,-2),m],Inverse = T)
        first_hand=rbind(first_hand,first_hand_quantity)
      }
      first_hand=first_hand[-1,]
    }
    # 第二手：显+逆
    { 
      second_hand=matrix(ncol = nrow(path) )
      for (i in seq(22,n,4)) {
        second_hand_quantity=new_quantize(path,set = data[seq(i,i-18,-2),m],Inverse = T)
        second_hand=rbind(second_hand,second_hand_quantity)
      }
      second_hand=second_hand[-1,]
    }
    # 第三手：显+逆
    {
      third_hand=matrix(ncol = nrow(path))
      for (i in seq(23,n,4)) {
        third_hand_quantity=new_quantize(path,set = data[seq(i,i-18,-2),m],Inverse = T)
        third_hand=rbind(third_hand,third_hand_quantity)
      }
      third_hand=third_hand[-1,]
    }
    # 第四手：显+逆
    { 
      forth_hand=matrix(ncol = nrow(path) )
      for (i in seq(24,n,4)) {
        forth_hand_quantity=new_quantize(path,set = data[seq(i,i-18,-2),m],Inverse = T)
        forth_hand=rbind(forth_hand,forth_hand_quantity)
      }
      forth_hand=forth_hand[-1,]
    }
    return(list(first_hand,second_hand,third_hand,forth_hand))
  }
  data_sort501=function(data=data,m,path=path){
    n=nrow(data)
    # 第一手：显+逆
    {
      first_hand=matrix(ncol = nrow(path))
      for (i in seq(21,n,4)) {
        first_hand_quantity=new_quantize(path,set = data[seq(i,i-18,-2),m])
        first_hand=rbind(first_hand,first_hand_quantity)
      }
      first_hand=first_hand[-1,]
    }
    # 第二手：显+逆
    { 
      second_hand=matrix(ncol = nrow(path) )
      for (i in seq(22,n,4)) {
        second_hand_quantity=new_quantize(path,set = data[seq(i,i-18,-2),m])
        second_hand=rbind(second_hand,second_hand_quantity)
      }
      second_hand=second_hand[-1,]
    }
    # 第三手：显+逆
    {
      third_hand=matrix(ncol = nrow(path))
      for (i in seq(23,n,4)) {
        third_hand_quantity=new_quantize(path,set = data[seq(i,i-18,-2),m])
        third_hand=rbind(third_hand,third_hand_quantity)
      }
      third_hand=third_hand[-1,]
    }
    # 第四手：显+逆
    { 
      forth_hand=matrix(ncol = nrow(path) )
      for (i in seq(24,n,4)) {
        forth_hand_quantity=new_quantize(path,set = data[seq(i,i-18,-2),m])
        forth_hand=rbind(forth_hand,forth_hand_quantity)
      }
      forth_hand=forth_hand[-1,]
    }
    return(list(first_hand,second_hand,third_hand,forth_hand))
  }
  data_sort51=function(data=data,m,path=path){
    n=nrow(data)
    # 第一手：显+顺
    {
      first_hand=matrix(ncol = nrow(path))
      for (i in seq(21,n,4)) {
        first_hand_quantity=new_quantize(path,set = data[seq(i-18,i,2),m])
        first_hand=rbind(first_hand,first_hand_quantity)
      }
      first_hand=first_hand[-1,]
    }
    # 第二手：显+顺
    { 
      second_hand=matrix(ncol = nrow(path) )
      for (i in seq(22,n,4)) {
        second_hand_quantity=new_quantize(path,set = data[seq(i-18,i,2),m])
        second_hand=rbind(second_hand,second_hand_quantity)
      }
      second_hand=second_hand[-1,]
    }
    # 第三手：reflection+逆
    {
      reflaction=ifelse(data[,m]=="B",'P','B')
      third_hand=matrix(ncol = nrow(path))
      for (i in seq(23,n,4)) {
        rejection=factor(c(reflaction[seq((i-2),(i-18),-2)],as.character(data[i,m])),levels = c('B',"P"))
        third_hand_quantity=new_quantize(path,set = rejection,Inverse = T)
        third_hand=rbind(third_hand,third_hand_quantity)
      }
      third_hand=third_hand[-1,]
    }
    # 第四手：reflection+逆
    { 
      reflaction=ifelse(data[,m]=="B",'P','B')
      forth_hand=matrix(ncol = nrow(path) )
      for (i in seq(24,n,4)) {
        rejection=factor(c(reflaction[seq((i-2),(i-18),-2)],as.character(data[i,m])),levels = c('B',"P"))
        forth_hand_quantity=new_quantize(path,set = rejection,Inverse = T)
        forth_hand=rbind(forth_hand,forth_hand_quantity)
      }
      forth_hand=forth_hand[-1,]
    }
    return(list(first_hand,second_hand,third_hand,forth_hand))
  }
  data_sort510=function(data=data,m,path=path){
    n=nrow(data)
    # 第一手：显+顺
    {
      first_hand=matrix(ncol = nrow(path))
      for (i in seq(21,n,4)) {
        first_hand_quantity=new_quantize(path,set = data[seq(i-18,i,2),m])
        first_hand=rbind(first_hand,first_hand_quantity)
      }
      first_hand=first_hand[-1,]
    }
    # 第二手：显+顺
    { 
      second_hand=matrix(ncol = nrow(path) )
      for (i in seq(22,n,4)) {
        second_hand_quantity=new_quantize(path,set = data[seq(i-18,i,2),m])
        second_hand=rbind(second_hand,second_hand_quantity)
      }
      second_hand=second_hand[-1,]
    }
    # 第三手：reflection+顺
    {
      reflaction=ifelse(data[,m]=="B",'P','B')
      third_hand=matrix(ncol = nrow(path))
      for (i in seq(23,n,4)) {
        rejection=factor(c(reflaction[seq((i-18),(i-2),2)],as.character(data[i,m])),levels = c('B',"P"))
        third_hand_quantity=new_quantize(path,set = rejection,Inverse = T)
        third_hand=rbind(third_hand,third_hand_quantity)
      }
      third_hand=third_hand[-1,]
    }
    # 第四手：reflection+顺
    { 
      reflaction=ifelse(data[,m]=="B",'P','B')
      forth_hand=matrix(ncol = nrow(path) )
      for (i in seq(24,n,4)) {
        rejection=factor(c(reflaction[seq((i-18),(i-2),2)],as.character(data[i,m])),levels = c('B',"P"))
        forth_hand_quantity=new_quantize(path,set = rejection,Inverse = T)
        forth_hand=rbind(forth_hand,forth_hand_quantity)
      }
      forth_hand=forth_hand[-1,]
    }
    return(list(first_hand,second_hand,third_hand,forth_hand))
  }
  # 错误版本：逆序时Inverse = F
  data_sort511=function(data=data,m,path=path){
    n=nrow(data)
    # 第一手：显+顺
    {
      first_hand=matrix(ncol = nrow(path))
      for (i in seq(21,n,4)) {
        first_hand_quantity=new_quantize(path,set = data[c(seq(i-18,i-2,2),i),m])
        first_hand=rbind(first_hand,first_hand_quantity)
      }
      first_hand=first_hand[-1,]
    }
    # 第二手：显+顺
    { 
      second_hand=matrix(ncol = nrow(path) )
      for (i in seq(22,n,4)) {
        second_hand_quantity=new_quantize(path,set = data[seq(i-18,i,2),m])
        second_hand=rbind(second_hand,second_hand_quantity)
      }
      second_hand=second_hand[-1,]
    }
    # 第三手：reflection+逆
    {
      reflaction=ifelse(data[,m]=="B",'P','B')
      third_hand=matrix(ncol = nrow(path))
      for (i in seq(23,n,4)) {
        rejection=factor(c(reflaction[c(seq(i-18,i-2,2),i)],as.character(data[i,m])),levels = c('B',"P"))
        third_hand_quantity=new_quantize(path,set = rejection)
        third_hand=rbind(third_hand,third_hand_quantity)
      }
      third_hand=third_hand[-1,]
    }
    # 第四手：reflection+逆
    { 
      reflaction=ifelse(data[,m]=="B",'P','B')
      forth_hand=matrix(ncol = nrow(path) )
      for (i in seq(24,n,4)) {
        rejection=factor(c(reflaction[seq((i-2),(i-18),-2)],as.character(data[i,m])),levels = c('B',"P"))
        forth_hand_quantity=new_quantize(path,set = rejection)
        forth_hand=rbind(forth_hand,forth_hand_quantity)
      }
      forth_hand=forth_hand[-1,]
    }
    return(list(first_hand,second_hand,third_hand,forth_hand))
  }
  # 1、２手显现+逆；３、４手reflection+逆
  data_sort54=function(data=data,m,path=path){
    n=nrow(data)
    # 第一手：显+逆
    {
      first_hand=matrix(ncol = nrow(path))
      for (i in seq(21,n,4)) {
        first_hand_quantity=new_quantize(path,set = data[c(seq(i-2,i-18,-2),i),m],Inverse = T)
        first_hand=rbind(first_hand,first_hand_quantity)
      }
      first_hand=first_hand[-1,]
    }
    # 第二手：显+逆
    { 
      second_hand=matrix(ncol = nrow(path) )
      for (i in seq(22,n,4)) {
        second_hand_quantity=new_quantize(path,set = data[c(seq(i-2,i-18,-2),i),m],Inverse = T)
        second_hand=rbind(second_hand,second_hand_quantity)
      }
      second_hand=second_hand[-1,]
    }
    # 第三手：reflection+逆
    {
      reflaction=ifelse(data[,m]=="B",'P','B')
      third_hand=matrix(ncol = nrow(path))
      for (i in seq(23,n,4)) {
        rejection=factor(c(reflaction[seq((i-2),(i-18),-2)],as.character(data[i,m])),levels = c('B',"P"))
        third_hand_quantity=new_quantize(path,set = rejection,Inverse = T)
        third_hand=rbind(third_hand,third_hand_quantity)
      }
      third_hand=third_hand[-1,]
    }
    # 第四手：reflection+逆
    { 
      reflaction=ifelse(data[,m]=="B",'P','B')
      forth_hand=matrix(ncol = nrow(path) )
      for (i in seq(24,n,4)) {
        rejection=factor(c(reflaction[seq((i-2),(i-18),-2)],as.character(data[i,m])),levels = c('B',"P"))
        forth_hand_quantity=new_quantize(path,set = rejection,Inverse = T)
        forth_hand=rbind(forth_hand,forth_hand_quantity)
      }
      forth_hand=forth_hand[-1,]
    }
    return(list(first_hand,second_hand,third_hand,forth_hand))
  }
  data_sort55=function(data=data,m,path=path){
    n=nrow(data)
    # 第一手：显+逆
    {
      first_hand=matrix(ncol = nrow(path))
      for (i in seq(21,n,4)) {
        first_hand_quantity=new_quantize(path,set = data[c(seq(i-2,i-18,-2),i),m])
        first_hand=rbind(first_hand,first_hand_quantity)
      }
      first_hand=first_hand[-1,]
    }
    # 第二手：re+逆
    { 
      reflaction=ifelse(data[,m]=="B",'P','B')
      second_hand=matrix(ncol = nrow(path) )
      for (i in seq(22,n,4)) {
        rejection=factor(c(reflaction[seq((i-2),(i-18),-2)],as.character(data[i,m])),levels = c('B',"P"))
        second_hand_quantity=new_quantize(path,set = rejection)
        second_hand=rbind(second_hand,second_hand_quantity)
      }
      second_hand=second_hand[-1,]
    }
    # 第三手：显+逆
    {
      third_hand=matrix(ncol = nrow(path))
      for (i in seq(23,n,4)) {
        third_hand_quantity=new_quantize(path,set = data[c(seq(i-2,i-18,-2),i),m])
        third_hand=rbind(third_hand,third_hand_quantity)
      }
      third_hand=third_hand[-1,]
    }
    # 第四手：reflection+逆
    { 
      reflaction=ifelse(data[,m]=="B",'P','B')
      forth_hand=matrix(ncol = nrow(path) )
      for (i in seq(24,n,4)) {
        rejection=factor(c(reflaction[seq((i-2),(i-18),-2)],as.character(data[i,m])),levels = c('B',"P"))
        forth_hand_quantity=new_quantize(path,set = rejection)
        forth_hand=rbind(forth_hand,forth_hand_quantity)
      }
      forth_hand=forth_hand[-1,]
    }
    return(list(first_hand,second_hand,third_hand,forth_hand))
  }
  data_sort555=function(data=data,m,path=path){
    n=nrow(data)
    # 第一手：显+逆
    {
      first_hand=matrix(ncol = nrow(path))
      for (i in seq(21,n,4)) {
        first_hand_quantity=new_quantize(path,set = data[c(seq(i-2,i-18,-2),i),m],Inverse = T)
        first_hand=rbind(first_hand,first_hand_quantity)
      }
      first_hand=first_hand[-1,]
    }
    # 第二手：re+逆
    { 
      reflaction=ifelse(data[,m]=="B",'P','B')
      second_hand=matrix(ncol = nrow(path) )
      for (i in seq(22,n,4)) {
        rejection=factor(c(reflaction[seq((i-2),(i-18),-2)],as.character(data[i,m])),levels = c('B',"P"))
        second_hand_quantity=new_quantize(path,set = rejection,Inverse = T)
        second_hand=rbind(second_hand,second_hand_quantity)
      }
      second_hand=second_hand[-1,]
    }
    # 第三手：显+逆
    {
      third_hand=matrix(ncol = nrow(path))
      for (i in seq(23,n,4)) {
        third_hand_quantity=new_quantize(path,set = data[c(seq(i-2,i-18,-2),i),m],Inverse = T)
        third_hand=rbind(third_hand,third_hand_quantity)
      }
      third_hand=third_hand[-1,]
    }
    # 第四手：reflection+逆
    { 
      reflaction=ifelse(data[,m]=="B",'P','B')
      forth_hand=matrix(ncol = nrow(path) )
      for (i in seq(24,n,4)) {
        rejection=factor(c(reflaction[seq((i-2),(i-18),-2)],as.character(data[i,m])),levels = c('B',"P"))
        forth_hand_quantity=new_quantize(path,set = rejection,Inverse = T)
        forth_hand=rbind(forth_hand,forth_hand_quantity)
      }
      forth_hand=forth_hand[-1,]
    }
    return(list(first_hand,second_hand,third_hand,forth_hand))
  }
  # “错误”的版本4
  data_sort541=function(data=data,m,path=path){
    n=nrow(data)
    # 第一手：显+逆
    {
      first_hand=matrix(ncol = nrow(path))
      for (i in seq(21,n,4)) {
        first_hand_quantity=new_quantize(path,set = data[c(seq(i-2,i-18,-2),i),m])
        first_hand=rbind(first_hand,first_hand_quantity)
      }
      first_hand=first_hand[-1,]
    }
    # 第二手：显+逆
    { 
      second_hand=matrix(ncol = nrow(path) )
      for (i in seq(22,n,4)) {
        second_hand_quantity=new_quantize(path,set = data[c(seq(i-2,i-18,-2),i),m])
        second_hand=rbind(second_hand,second_hand_quantity)
      }
      second_hand=second_hand[-1,]
    }
    # 第三手：reflection+逆
    {
      reflaction=ifelse(data[,m]=="B",'P','B')
      third_hand=matrix(ncol = nrow(path))
      for (i in seq(23,n,4)) {
        rejection=factor(c(reflaction[seq((i-2),(i-18),-2)],as.character(data[i,m])),levels = c('B',"P"))
        third_hand_quantity=new_quantize(path,set = rejection,Inverse = T)
        third_hand=rbind(third_hand,third_hand_quantity)
      }
      third_hand=third_hand[-1,]
    }
    # 第四手：reflection+逆
    { 
      reflaction=ifelse(data[,m]=="B",'P','B')
      forth_hand=matrix(ncol = nrow(path) )
      for (i in seq(24,n,4)) {
        rejection=factor(c(reflaction[seq((i-2),(i-18),-2)],as.character(data[i,m])),levels = c('B',"P"))
        forth_hand_quantity=new_quantize(path,set = rejection,Inverse = T)
        forth_hand=rbind(forth_hand,forth_hand_quantity)
      }
      forth_hand=forth_hand[-1,]
    }
    return(list(first_hand,second_hand,third_hand,forth_hand))
  }
  
}

run=function(data=data,path=path,data_sort,choice=0,benchmark=F){
  n_path=nrow(path)
  minus_result=matrix(nrow =length(data),ncol = n_path)
  new_index=matrix(nrow =length(data),ncol = n_path)
  for (m in 1:length(data)) {
    if(m%%50==0)print(m)
    if(data_sort==1){
      temp=data_sort1(data,m,path)
      first=temp[[1]]
      sumtemp=first
      pattern=matrix(1,nrow =nrow(sumtemp) ,ncol = nrow(path))
    }
    else if(data_sort==2){
      temp=data_sort2(data,m,path)
      first=temp[[1]]
      second=temp[[2]]
      n_sumtemp=nrow(first)+nrow(second)
      sumtemp=matrix(nrow =n_sumtemp ,ncol = nrow(path))
      sumtemp[seq(1,n_sumtemp,2),]=first
      sumtemp[seq(2,n_sumtemp,2),]=second
      # 计算新指标:
      pattern=matrix(nrow =n_sumtemp ,ncol = nrow(path))
      pattern[seq(1,n_sumtemp,2),]=1
      pattern[seq(2,n_sumtemp,2),]=1
      
    }
    else if(data_sort==3){
      temp=data_sort3(data,m,path)
      first=temp[[1]]
      second=temp[[2]]
      n_sumtemp=nrow(first)+nrow(second)
      second=ifelse(first[-nrow(first),]<0,second,-second)
      sumtemp=matrix(nrow =n_sumtemp ,ncol = nrow(path))
      sumtemp[seq(1,n_sumtemp,2),]=first
      sumtemp[seq(2,n_sumtemp,2),]=second
      # 计算新指标:
      pattern=matrix(nrow =n_sumtemp ,ncol = nrow(path))
      pattern[seq(1,n_sumtemp,2),]=1
      pattern[seq(2,n_sumtemp,2),]=ifelse(first[-nrow(first),]>0,1,2)
      
    }
    else if(data_sort==31){
      temp=data_sort31(data,m,path)
      first=temp[[1]]
      second=temp[[2]]
      n_sumtemp=nrow(first)+nrow(second)
      second=ifelse(first[-nrow(first),]<0,second,-second)
      sumtemp=matrix(nrow =n_sumtemp ,ncol = nrow(path))
      sumtemp[seq(1,n_sumtemp,2),]=first
      sumtemp[seq(2,n_sumtemp,2),]=second
      # 计算新指标:
      pattern=matrix(nrow =n_sumtemp ,ncol = nrow(path))
      pattern[seq(1,n_sumtemp,2),]=1
      pattern[seq(2,n_sumtemp,2),]=ifelse(first[-nrow(first),]>0,1,2)
      
    }
    else if(data_sort==311){
      temp=data_sort31_wrong(data,m,path)
      first=temp[[1]]
      second=temp[[2]]
      n_sumtemp=nrow(first)+nrow(second)
      second=ifelse(first[-nrow(first),]<0,second,-second)
      sumtemp=matrix(nrow =n_sumtemp ,ncol = nrow(path))
      sumtemp[seq(1,n_sumtemp,2),]=first
      sumtemp[seq(2,n_sumtemp,2),]=second
      # 计算新指标:
      pattern=matrix(nrow =n_sumtemp ,ncol = nrow(path))
      pattern[seq(1,n_sumtemp,2),]=1
      pattern[seq(2,n_sumtemp,2),]=ifelse(first[-nrow(first),]>0,1,2)
      
    }
    else if(data_sort==41){
      temp=data_sort41(data,m,path)
      first=temp[[1]]
      second=temp[[2]]
      n_sumtemp=nrow(first)+nrow(second)
      sumtemp=matrix(nrow =n_sumtemp ,ncol = nrow(path))
      if(benchmark==F){
        second=ifelse(first[-nrow(first),]<0,second,-second)
      }
      
      sumtemp[seq(1,n_sumtemp,2),]=first
      sumtemp[seq(2,n_sumtemp,2),]=second
      # 计算新指标:
      pattern=matrix(nrow =n_sumtemp ,ncol = nrow(path))
      pattern[seq(1,n_sumtemp,2),]=1
      pattern[seq(2,n_sumtemp,2),]=ifelse(first[-nrow(first),]>0,1,2)
      
    }
    else if(data_sort==42){
      temp=data_sort42(data,m,path)
      first=temp[[1]]
      second=temp[[2]]
      n_sumtemp=nrow(first)+nrow(second)
      sumtemp=matrix(nrow =n_sumtemp ,ncol = nrow(path))
      if(benchmark==F){
        second=ifelse(first[-nrow(first),]<0,second,-second)
      }
      
      sumtemp[seq(1,n_sumtemp,2),]=first
      sumtemp[seq(2,n_sumtemp,2),]=second
      # 计算新指标:
      pattern=matrix(nrow =n_sumtemp ,ncol = nrow(path))
      pattern[seq(1,n_sumtemp,2),]=1
      pattern[seq(2,n_sumtemp,2),]=ifelse(first[-nrow(first),]>0,1,2)
      
    }
    else if(data_sort==43){
      temp=data_sort43(data,m,path)
      first=temp[[1]]
      second=temp[[2]]
      n_sumtemp=nrow(first)+nrow(second)
      sumtemp=matrix(nrow =n_sumtemp ,ncol = nrow(path))
      if(benchmark==F){
        second=ifelse(first[-nrow(first),]<0,second,-second)
      }
      
      sumtemp[seq(1,n_sumtemp,2),]=first
      sumtemp[seq(2,n_sumtemp,2),]=second
      # 计算新指标:
      pattern=matrix(nrow =n_sumtemp ,ncol = nrow(path))
      pattern[seq(1,n_sumtemp,2),]=1
      pattern[seq(2,n_sumtemp,2),]=ifelse(first[-nrow(first),]>0,1,2)
      
    }
    else if(data_sort==44){
      temp=data_sort44(data,m,path)
      first=temp[[1]]
      second=temp[[2]]
      n_sumtemp=nrow(first)+nrow(second)
      sumtemp=matrix(nrow =n_sumtemp ,ncol = nrow(path))
      if(benchmark==F){
        second=ifelse(first[-nrow(first),]<0,second,-second)
      }
      
      sumtemp[seq(1,n_sumtemp,2),]=first
      sumtemp[seq(2,n_sumtemp,2),]=second
      # 计算新指标:
      pattern=matrix(nrow =n_sumtemp ,ncol = nrow(path))
      pattern[seq(1,n_sumtemp,2),]=1
      pattern[seq(2,n_sumtemp,2),]=ifelse(first[-nrow(first),]>0,1,2)
      
    }
    else if(data_sort==411){
      temp=data_sort41_wrong(data,m,path)
      first=temp[[1]]
      second=temp[[2]]
      n_sumtemp=nrow(first)+nrow(second)
      sumtemp=matrix(nrow =n_sumtemp ,ncol = nrow(path))
      if(benchmark==F){
        second=ifelse(first[-nrow(first),]<0,second,-second)
      }
      
      sumtemp[seq(1,n_sumtemp,2),]=first
      sumtemp[seq(2,n_sumtemp,2),]=second
      # 计算新指标:
      pattern=matrix(nrow =n_sumtemp ,ncol = nrow(path))
      pattern[seq(1,n_sumtemp,2),]=1
      pattern[seq(2,n_sumtemp,2),]=ifelse(first[-nrow(first),]>0,1,2)
      
    }
    else if(data_sort==421){
      temp=data_sort42_wrong(data,m,path)
      first=temp[[1]]
      second=temp[[2]]
      n_sumtemp=nrow(first)+nrow(second)
      sumtemp=matrix(nrow =n_sumtemp ,ncol = nrow(path))
      if(benchmark==F){
        second=ifelse(first[-nrow(first),]<0,second,-second)
      }
      
      sumtemp[seq(1,n_sumtemp,2),]=first
      sumtemp[seq(2,n_sumtemp,2),]=second
      # 计算新指标:
      pattern=matrix(nrow =n_sumtemp ,ncol = nrow(path))
      pattern[seq(1,n_sumtemp,2),]=1
      pattern[seq(2,n_sumtemp,2),]=ifelse(first[-nrow(first),]>0,1,2)
      
    }
    else if(data_sort==431){
      temp=data_sort43_wrong(data,m,path)
      first=temp[[1]]
      second=temp[[2]]
      n_sumtemp=nrow(first)+nrow(second)
      sumtemp=matrix(nrow =n_sumtemp ,ncol = nrow(path))
      if(benchmark==F){
        second=ifelse(first[-nrow(first),]<0,second,-second)
      }
      
      sumtemp[seq(1,n_sumtemp,2),]=first
      sumtemp[seq(2,n_sumtemp,2),]=second
      # 计算新指标:
      pattern=matrix(nrow =n_sumtemp ,ncol = nrow(path))
      pattern[seq(1,n_sumtemp,2),]=1
      pattern[seq(2,n_sumtemp,2),]=ifelse(first[-nrow(first),]>0,1,2)
      
    }
    else if(data_sort==441){
      temp=data_sort44_wrong(data,m,path)
      first=temp[[1]]
      second=temp[[2]]
      n_sumtemp=nrow(first)+nrow(second)
      sumtemp=matrix(nrow =n_sumtemp ,ncol = nrow(path))
      if(benchmark==F){
        second=ifelse(first[-nrow(first),]<0,second,-second)
      }
      
      sumtemp[seq(1,n_sumtemp,2),]=first
      sumtemp[seq(2,n_sumtemp,2),]=second
      # 计算新指标:
      pattern=matrix(nrow =n_sumtemp ,ncol = nrow(path))
      pattern[seq(1,n_sumtemp,2),]=1
      pattern[seq(2,n_sumtemp,2),]=ifelse(first[-nrow(first),]>0,1,2)
      
    }
    else if(data_sort==45){
      temp=data_sort45(data,m,path)
      first=temp[[1]]
      second=temp[[2]]
      n_sumtemp=nrow(first)+nrow(second)
      sumtemp=matrix(nrow =n_sumtemp ,ncol = nrow(path))
      if(benchmark==F){
        second=ifelse(first[-nrow(first),]<0,second,-second)
      }
      
      sumtemp[seq(1,n_sumtemp,2),]=first
      sumtemp[seq(2,n_sumtemp,2),]=second
      # 计算新指标:
      pattern=matrix(nrow =n_sumtemp ,ncol = nrow(path))
      pattern[seq(1,n_sumtemp,2),]=1
      pattern[seq(2,n_sumtemp,2),]=ifelse(first[-nrow(first),]>0,1,2)
      
    }
    else if(data_sort==481){
      temp=data_sort48_wrong(data,m,path)
      first=temp[[1]]
      second=temp[[2]]
      n_sumtemp=nrow(first)+nrow(second)
      sumtemp=matrix(nrow =n_sumtemp ,ncol = nrow(path))
      if(benchmark==F){
        second=ifelse(first[-nrow(first),]<0,second,-second)
      }
      
      sumtemp[seq(1,n_sumtemp,2),]=first
      sumtemp[seq(2,n_sumtemp,2),]=second
      # 计算新指标:
      pattern=matrix(nrow =n_sumtemp ,ncol = nrow(path))
      pattern[seq(1,n_sumtemp,2),]=1
      pattern[seq(2,n_sumtemp,2),]=ifelse(first[-nrow(first),]>0,1,2)
      
    }
    else if(data_sort==51){
      temp=data_sort51(data,m,path)
      first=temp[[1]]
      second=temp[[2]]
      third=temp[[3]]
      forth=temp[[4]]
      n_sumtemp=nrow(first)+nrow(second)+nrow(third)+nrow(forth)
      sumtemp=matrix(nrow =n_sumtemp ,ncol = nrow(path))
      # 计算新指标:
      pattern=matrix(nrow =n_sumtemp ,ncol = nrow(path))
      
      if(choice==0){
        k=1
        pattern[]=1
      }
      else if(choice==11){
        second=ifelse(first<0,second,-second)
        third=ifelse(first<0,third,-third)
        forth=ifelse(first*second>0,forth,-forth)
        pattern[seq(1,n_sumtemp,4),]=1
        pattern[seq(2,n_sumtemp,4),]=ifelse(first<0,1,2)
        pattern[seq(3,n_sumtemp,4),]=ifelse(first<0,1,2)
        pattern[seq(4,n_sumtemp,4),]=ifelse(first*second>0,1,2)
      }
      else if(choice==13){
        third=ifelse(first<0,third,-third)
        forth=ifelse(second<0,forth,-forth)
        pattern[seq(1,n_sumtemp,4),]=1
        pattern[seq(2,n_sumtemp,4),]=1
        pattern[seq(3,n_sumtemp,4),]=ifelse(first<0,1,2)
        pattern[seq(4,n_sumtemp,4),]=ifelse(second<0,1,2)
      }
      else if(choice==131){
        second=-second
        third=ifelse(first<0,third,-third)
        forth=ifelse(second>0,forth,-forth) # 注意这里的符号
        pattern[seq(1,n_sumtemp,4),]=1
        pattern[seq(2,n_sumtemp,4),]=2
        pattern[seq(3,n_sumtemp,4),]=ifelse(first<0,1,2)
        pattern[seq(4,n_sumtemp,4),]=ifelse(second>0,1,2)
      }
      
      sumtemp[seq(1,n_sumtemp,4),]=first
      sumtemp[seq(2,n_sumtemp,4),]=second
      sumtemp[seq(3,n_sumtemp,4),]=third
      sumtemp[seq(4,n_sumtemp,4),]=forth
      
    }
    else if(data_sort==510){
      temp=data_sort510(data,m,path)
      first=temp[[1]]
      second=temp[[2]]
      third=temp[[3]]
      forth=temp[[4]]
      n_sumtemp=nrow(first)+nrow(second)+nrow(third)+nrow(forth)
      sumtemp=matrix(nrow =n_sumtemp ,ncol = nrow(path))
      # 计算新指标:
      pattern=matrix(nrow =n_sumtemp ,ncol = nrow(path))
      
      if(choice==0){
        k=1
        pattern[]=1
      }
      else if(choice==11){
        second=ifelse(first<0,second,-second)
        third=ifelse(first<0,third,-third)
        forth=ifelse(first*second>0,forth,-forth)
        pattern[seq(1,n_sumtemp,4),]=1
        pattern[seq(2,n_sumtemp,4),]=ifelse(first<0,1,2)
        pattern[seq(3,n_sumtemp,4),]=ifelse(first<0,1,2)
        pattern[seq(4,n_sumtemp,4),]=ifelse(first*second>0,1,2)
      }
      else if(choice==13){
        third=ifelse(first<0,third,-third)
        forth=ifelse(second<0,forth,-forth)
        pattern[seq(1,n_sumtemp,4),]=1
        pattern[seq(2,n_sumtemp,4),]=1
        pattern[seq(3,n_sumtemp,4),]=ifelse(first<0,1,2)
        pattern[seq(4,n_sumtemp,4),]=ifelse(second<0,1,2)
      }
      else if(choice==131){
        second=-second
        third=ifelse(first<0,third,-third)
        forth=ifelse(second>0,forth,-forth) # 注意这里的符号
        pattern[seq(1,n_sumtemp,4),]=1
        pattern[seq(2,n_sumtemp,4),]=2
        pattern[seq(3,n_sumtemp,4),]=ifelse(first<0,1,2)
        pattern[seq(4,n_sumtemp,4),]=ifelse(second>0,1,2)
      }
      
      sumtemp[seq(1,n_sumtemp,4),]=first
      sumtemp[seq(2,n_sumtemp,4),]=second
      sumtemp[seq(3,n_sumtemp,4),]=third
      sumtemp[seq(4,n_sumtemp,4),]=forth
      
    }
    else if(data_sort==511){
      temp=data_sort511(data,m,path)
      first=temp[[1]]
      second=temp[[2]]
      third=temp[[3]]
      forth=temp[[4]]
      n_sumtemp=nrow(first)+nrow(second)+nrow(third)+nrow(forth)
      sumtemp=matrix(nrow =n_sumtemp ,ncol = nrow(path))
      # 计算新指标:
      pattern=matrix(nrow =n_sumtemp ,ncol = nrow(path))
      
      if(choice==0){
        k=1
        pattern[]=1
      }
      else if(choice==11){
        second=ifelse(first<0,second,-second)
        third=ifelse(first<0,third,-third)
        forth=ifelse(first*second>0,forth,-forth)
        pattern[seq(1,n_sumtemp,4),]=1
        pattern[seq(2,n_sumtemp,4),]=ifelse(first<0,1,2)
        pattern[seq(3,n_sumtemp,4),]=ifelse(first<0,1,2)
        pattern[seq(4,n_sumtemp,4),]=ifelse(first*second>0,1,2)
      }
      else if(choice==13){
        third=ifelse(first<0,third,-third)
        forth=ifelse(second<0,forth,-forth)
        pattern[seq(1,n_sumtemp,4),]=1
        pattern[seq(2,n_sumtemp,4),]=1
        pattern[seq(3,n_sumtemp,4),]=ifelse(first<0,1,2)
        pattern[seq(4,n_sumtemp,4),]=ifelse(second<0,1,2)
      }
      else if(choice==131){
        second=-second
        third=ifelse(first<0,third,-third)
        forth=ifelse(second>0,forth,-forth) # 注意这里的符号
        pattern[seq(1,n_sumtemp,4),]=1
        pattern[seq(2,n_sumtemp,4),]=2
        pattern[seq(3,n_sumtemp,4),]=ifelse(first<0,1,2)
        pattern[seq(4,n_sumtemp,4),]=ifelse(second>0,1,2)
      }
      
      sumtemp[seq(1,n_sumtemp,4),]=first
      sumtemp[seq(2,n_sumtemp,4),]=second
      sumtemp[seq(3,n_sumtemp,4),]=third
      sumtemp[seq(4,n_sumtemp,4),]=forth
      
    }
    else if(data_sort==54){
      temp=data_sort54(data,m,path)
      first=temp[[1]]
      second=temp[[2]]
      third=temp[[3]]
      forth=temp[[4]]
      n_sumtemp=nrow(first)+nrow(second)+nrow(third)+nrow(forth)
      sumtemp=matrix(nrow =n_sumtemp ,ncol = nrow(path))
      # 计算新指标:
      pattern=matrix(nrow =n_sumtemp ,ncol = nrow(path))
      
      if(choice==0){
        k=1
        pattern[]=1
      }
      else if(choice==11){
        second=ifelse(first<0,second,-second)
        third=ifelse(first<0,third,-third)
        forth=ifelse(first*second>0,forth,-forth)
        pattern[seq(1,n_sumtemp,4),]=1
        pattern[seq(2,n_sumtemp,4),]=ifelse(first<0,1,2)
        pattern[seq(3,n_sumtemp,4),]=ifelse(first<0,1,2)
        pattern[seq(4,n_sumtemp,4),]=ifelse(first*second>0,1,2)
      }
      else if(choice==13){
        third=ifelse(first<0,third,-third)
        forth=ifelse(second<0,forth,-forth)
        pattern[seq(1,n_sumtemp,4),]=1
        pattern[seq(2,n_sumtemp,4),]=1
        pattern[seq(3,n_sumtemp,4),]=ifelse(first<0,1,2)
        pattern[seq(4,n_sumtemp,4),]=ifelse(second<0,1,2)
      }
      else if(choice==131){
        second=-second
        third=ifelse(first<0,third,-third)
        forth=ifelse(second>0,forth,-forth) # 注意这里的符号
        pattern[seq(1,n_sumtemp,4),]=1
        pattern[seq(2,n_sumtemp,4),]=2
        pattern[seq(3,n_sumtemp,4),]=ifelse(first<0,1,2)
        pattern[seq(4,n_sumtemp,4),]=ifelse(second>0,1,2)
      }
      
      sumtemp[seq(1,n_sumtemp,4),]=first
      sumtemp[seq(2,n_sumtemp,4),]=second
      sumtemp[seq(3,n_sumtemp,4),]=third
      sumtemp[seq(4,n_sumtemp,4),]=forth
      
    }
    else if(data_sort==55){
      temp=data_sort55(data,m,path)
      first=temp[[1]]
      second=temp[[2]]
      third=temp[[3]]
      forth=temp[[4]]
      n_sumtemp=nrow(first)+nrow(second)+nrow(third)+nrow(forth)
      sumtemp=matrix(nrow =n_sumtemp ,ncol = nrow(path))
      # 计算新指标:
      pattern=matrix(nrow =n_sumtemp ,ncol = nrow(path))
      
      if(choice==0){
        k=1
        pattern[]=1
      }
      else if(choice==11){
        second=ifelse(first<0,second,-second)
        third=ifelse(first<0,third,-third)
        forth=ifelse(first*second>0,forth,-forth)
        pattern[seq(1,n_sumtemp,4),]=1
        pattern[seq(2,n_sumtemp,4),]=ifelse(first<0,1,2)
        pattern[seq(3,n_sumtemp,4),]=ifelse(first<0,1,2)
        pattern[seq(4,n_sumtemp,4),]=ifelse(first*second>0,1,2)
      }
      else if(choice==13){
        third=ifelse(first<0,third,-third)
        forth=ifelse(second<0,forth,-forth)
        pattern[seq(1,n_sumtemp,4),]=1
        pattern[seq(2,n_sumtemp,4),]=1
        pattern[seq(3,n_sumtemp,4),]=ifelse(first<0,1,2)
        pattern[seq(4,n_sumtemp,4),]=ifelse(second<0,1,2)
      }
      else if(choice==131){
        second=-second
        third=ifelse(first<0,third,-third)
        forth=ifelse(second>0,forth,-forth) # 注意这里的符号
        pattern[seq(1,n_sumtemp,4),]=1
        pattern[seq(2,n_sumtemp,4),]=2
        pattern[seq(3,n_sumtemp,4),]=ifelse(first<0,1,2)
        pattern[seq(4,n_sumtemp,4),]=ifelse(second>0,1,2)
      }
      
      sumtemp[seq(1,n_sumtemp,4),]=first
      sumtemp[seq(2,n_sumtemp,4),]=second
      sumtemp[seq(3,n_sumtemp,4),]=third
      sumtemp[seq(4,n_sumtemp,4),]=forth
      
    }
    else if(data_sort==555){
      temp=data_sort555(data,m,path)
      first=temp[[1]]
      second=temp[[2]]
      third=temp[[3]]
      forth=temp[[4]]
      n_sumtemp=nrow(first)+nrow(second)+nrow(third)+nrow(forth)
      sumtemp=matrix(nrow =n_sumtemp ,ncol = nrow(path))
      # 计算新指标:
      pattern=matrix(nrow =n_sumtemp ,ncol = nrow(path))
      
      if(choice==0){
        k=1
        pattern[]=1
      }
      else if(choice==11){
        second=ifelse(first<0,second,-second)
        third=ifelse(first<0,third,-third)
        forth=ifelse(first*second>0,forth,-forth)
        pattern[seq(1,n_sumtemp,4),]=1
        pattern[seq(2,n_sumtemp,4),]=ifelse(first<0,1,2)
        pattern[seq(3,n_sumtemp,4),]=ifelse(first<0,1,2)
        pattern[seq(4,n_sumtemp,4),]=ifelse(first*second>0,1,2)
      }
      else if(choice==13){
        third=ifelse(first<0,third,-third)
        forth=ifelse(second<0,forth,-forth)
        pattern[seq(1,n_sumtemp,4),]=1
        pattern[seq(2,n_sumtemp,4),]=1
        pattern[seq(3,n_sumtemp,4),]=ifelse(first<0,1,2)
        pattern[seq(4,n_sumtemp,4),]=ifelse(second<0,1,2)
      }
      else if(choice==131){
        second=-second
        third=ifelse(first<0,third,-third)
        forth=ifelse(second>0,forth,-forth) # 注意这里的符号
        pattern[seq(1,n_sumtemp,4),]=1
        pattern[seq(2,n_sumtemp,4),]=2
        pattern[seq(3,n_sumtemp,4),]=ifelse(first<0,1,2)
        pattern[seq(4,n_sumtemp,4),]=ifelse(second>0,1,2)
      }
      
      sumtemp[seq(1,n_sumtemp,4),]=first
      sumtemp[seq(2,n_sumtemp,4),]=second
      sumtemp[seq(3,n_sumtemp,4),]=third
      sumtemp[seq(4,n_sumtemp,4),]=forth
      
    }
    else if(data_sort==541){
      temp=data_sort541(data,m,path)
      first=temp[[1]]
      second=temp[[2]]
      third=temp[[3]]
      forth=temp[[4]]
      n_sumtemp=nrow(first)+nrow(second)+nrow(third)+nrow(forth)
      sumtemp=matrix(nrow =n_sumtemp ,ncol = nrow(path))
      # 计算新指标:
      pattern=matrix(nrow =n_sumtemp ,ncol = nrow(path))
      
      if(choice==0){
        k=1
        pattern[]=1
      }
      else if(choice==11){
        second=ifelse(first<0,second,-second)
        third=ifelse(first<0,third,-third)
        forth=ifelse(first*second>0,forth,-forth)
        pattern[seq(1,n_sumtemp,4),]=1
        pattern[seq(2,n_sumtemp,4),]=ifelse(first<0,1,2)
        pattern[seq(3,n_sumtemp,4),]=ifelse(first<0,1,2)
        pattern[seq(4,n_sumtemp,4),]=ifelse(first*second>0,1,2)
      }
      else if(choice==13){
        third=ifelse(first<0,third,-third)
        forth=ifelse(second<0,forth,-forth)
        pattern[seq(1,n_sumtemp,4),]=1
        pattern[seq(2,n_sumtemp,4),]=1
        pattern[seq(3,n_sumtemp,4),]=ifelse(first<0,1,2)
        pattern[seq(4,n_sumtemp,4),]=ifelse(second<0,1,2)
      }
      else if(choice==131){
        second=-second
        third=ifelse(first<0,third,-third)
        forth=ifelse(second>0,forth,-forth) # 注意这里的符号
        pattern[seq(1,n_sumtemp,4),]=1
        pattern[seq(2,n_sumtemp,4),]=2
        pattern[seq(3,n_sumtemp,4),]=ifelse(first<0,1,2)
        pattern[seq(4,n_sumtemp,4),]=ifelse(second>0,1,2)
      }
      
      sumtemp[seq(1,n_sumtemp,4),]=first
      sumtemp[seq(2,n_sumtemp,4),]=second
      sumtemp[seq(3,n_sumtemp,4),]=third
      sumtemp[seq(4,n_sumtemp,4),]=forth
      
    }
    else if(data_sort==501){
      temp=data_sort501(data,m,path)
      first=temp[[1]]
      second=temp[[2]]
      third=temp[[3]]
      forth=temp[[4]]
      n_sumtemp=nrow(first)+nrow(second)+nrow(third)+nrow(forth)
      sumtemp=matrix(nrow =n_sumtemp ,ncol = nrow(path))
      # 计算新指标:
      pattern=matrix(nrow =n_sumtemp ,ncol = nrow(path))
      
      if(choice==0){
        k=1
        pattern[]=1
      }
      else if(choice==11){
        second=ifelse(first<0,second,-second)
        third=ifelse(first<0,third,-third)
        forth=ifelse(first*second>0,forth,-forth)
        pattern[seq(1,n_sumtemp,4),]=1
        pattern[seq(2,n_sumtemp,4),]=ifelse(first<0,1,2)
        pattern[seq(3,n_sumtemp,4),]=ifelse(first<0,1,2)
        pattern[seq(4,n_sumtemp,4),]=ifelse(first*second>0,1,2)
      }
      else if(choice==13){
        third=ifelse(first<0,third,-third)
        forth=ifelse(second<0,forth,-forth)
        pattern[seq(1,n_sumtemp,4),]=1
        pattern[seq(2,n_sumtemp,4),]=1
        pattern[seq(3,n_sumtemp,4),]=ifelse(first<0,1,2)
        pattern[seq(4,n_sumtemp,4),]=ifelse(second<0,1,2)
      }
      else if(choice==131){
        second=-second
        third=ifelse(first<0,third,-third)
        forth=ifelse(second>0,forth,-forth) # 注意这里的符号
        pattern[seq(1,n_sumtemp,4),]=1
        pattern[seq(2,n_sumtemp,4),]=2
        pattern[seq(3,n_sumtemp,4),]=ifelse(first<0,1,2)
        pattern[seq(4,n_sumtemp,4),]=ifelse(second>0,1,2)
      }
      
      sumtemp[seq(1,n_sumtemp,4),]=first
      sumtemp[seq(2,n_sumtemp,4),]=second
      sumtemp[seq(3,n_sumtemp,4),]=third
      sumtemp[seq(4,n_sumtemp,4),]=forth
      
    }
    else if(data_sort==50){
      temp=data_sort50(data,m,path)
      first=temp[[1]]
      second=temp[[2]]
      third=temp[[3]]
      forth=temp[[4]]
      n_sumtemp=nrow(first)+nrow(second)+nrow(third)+nrow(forth)
      sumtemp=matrix(nrow =n_sumtemp ,ncol = nrow(path))
      # 计算新指标:
      pattern=matrix(nrow =n_sumtemp ,ncol = nrow(path))
      
      if(choice==0){
        k=1
        pattern[]=1
      }
      else if(choice==11){
        second=ifelse(first<0,second,-second)
        third=ifelse(first<0,third,-third)
        forth=ifelse(first*second>0,forth,-forth)
        pattern[seq(1,n_sumtemp,4),]=1
        pattern[seq(2,n_sumtemp,4),]=ifelse(first<0,1,2)
        pattern[seq(3,n_sumtemp,4),]=ifelse(first<0,1,2)
        pattern[seq(4,n_sumtemp,4),]=ifelse(first*second>0,1,2)
      }
      else if(choice==13){
        third=ifelse(first<0,third,-third)
        forth=ifelse(second<0,forth,-forth)
        pattern[seq(1,n_sumtemp,4),]=1
        pattern[seq(2,n_sumtemp,4),]=1
        pattern[seq(3,n_sumtemp,4),]=ifelse(first<0,1,2)
        pattern[seq(4,n_sumtemp,4),]=ifelse(second<0,1,2)
      }
      else if(choice==131){
        second=-second
        third=ifelse(first<0,third,-third)
        forth=ifelse(second>0,forth,-forth) # 注意这里的符号
        pattern[seq(1,n_sumtemp,4),]=1
        pattern[seq(2,n_sumtemp,4),]=2
        pattern[seq(3,n_sumtemp,4),]=ifelse(first<0,1,2)
        pattern[seq(4,n_sumtemp,4),]=ifelse(second>0,1,2)
      }
      
      sumtemp[seq(1,n_sumtemp,4),]=first
      sumtemp[seq(2,n_sumtemp,4),]=second
      sumtemp[seq(3,n_sumtemp,4),]=third
      sumtemp[seq(4,n_sumtemp,4),]=forth
      
    }
    
    minus_result[m,]=detect_continue_minus(sumtemp)
    # 计算新指标:
    symbol=ifelse(sumtemp>0,1,-1)
    new_index[m,]=colSums(pattern*symbol)
  }
  colnames(minus_result)=c(paste0('formula',1:n_path))
  
  # result存放满足连续负个数小于等于5的结果
  result=matrix(nrow =max(minus_result) ,ncol = n_path)
  for (i in 1:(max(minus_result))){
    result[i,]=apply(minus_result,2,function(x)sum(x==i))
  }
  colnames(result)=c(paste0('formula',1:n_path))
  
  # satisfy为某一个formula满足小于等于5的数据集个数：
  satisfy=apply(result[1:5,], 2, sum)
  return(list(minus_result,result,satisfy,new_index))
}

present=function(result){
  print(result[[2]])
  print(result[[3]])
  print(paste0('mean:',mean(result[[3]]),'  ','sd:',round(sd(result[[3]]),3)))
}
new_index_present=function(result){
  print(result[[3]])
  for (i in 1:length(result[[3]])) {
    print(paste0('formula ',i,'新指标:',round(sum(result[[4]][,i]>=0)/760,3),'；前、后5个结果：',str_c(sort(result[[4]][,i],decreasing = T)[1:5],collapse = ' '),'；',str_c(sort(result[[4]][,i])[1:5],collapse = ' ')))
  }
}

# 开始测试：单轨=========================================================
# 单轨不分BP和分BP结果：
plan1_=run(data=all_data,path=path,data_sort=1);present(plan1_)
plan1_BP=run(data=all_data,path=path_BP,data_sort=1);present(plan1_BP)

plan31_=run(data=all_data,path=path,data_sort=3);present(plan31_)
plan31_BP=run(data=all_data,path=path_BP,data_sort=3);present(plan31_BP)

plan31_1_=run(data=all_data,path=path,data_sort=31)
present(plan31_1_);new_index_present(plan31_1_)
plan31_1_BP=run(data=all_data,path=path_BP,data_sort=31)
present(plan31_1_BP);new_index_present(plan31_1_BP)
# 错误的逆序版本：纯显现+双逆序
plan31_wrong1_=run(data=all_data,path=path,data_sort=311)
present(plan31_wrong1_);new_index_present(plan31_wrong1_)
plan31_wrong1_BP=run(data=all_data,path=path_BP,data_sort=311)
present(plan31_wrong1_BP);new_index_present(plan31_wrong1_BP)

#第一种组合：1手显现+顺；2手reflection+逆：
plan31_2_=run(data=all_data,path=path,data_sort=41)
present(plan31_2_);new_index_present(plan31_2_)
plan31_2_BP=run(data=all_data,path=path_BP,data_sort=41)
present(plan31_2_BP);new_index_present(plan31_2_BP)
#第五种：1手reflection+顺；2手显现+逆
plan31_2_1_=run(data=all_data,path=path,data_sort=45)
present(plan31_2_1_);new_index_present(plan31_2_1_)
plan31_2_1_BP=run(data=all_data,path=path_BP,data_sort=45)
present(plan31_2_1_BP);new_index_present(plan31_2_1_BP)
# 第五种与第一种本质是一样的；
# 第二种：1手显现+顺；2手reflection+顺
plan31_2_2_=run(data=all_data,path=path,data_sort=42)
present(plan31_2_2_);new_index_present(plan31_2_2_)
plan31_2_2_BP=run(data=all_data,path=path_BP,data_sort=42)
present(plan31_2_2_BP);new_index_present(plan31_2_2_BP)
# 第三种：1手显现+逆；2手reflection+逆
plan31_2_3_=run(data=all_data,path=path,data_sort=43)
present(plan31_2_3_);new_index_present(plan31_2_3_)
plan31_2_3_BP=run(data=all_data,path=path_BP,data_sort=43)
present(plan31_2_3_BP);new_index_present(plan31_2_3_BP)
# 第四种：1手显现+逆；2手reflection+逆
plan31_2_4_=run(data=all_data,path=path,data_sort=44)
present(plan31_2_4_);new_index_present(plan31_2_4_)
plan31_2_4_BP=run(data=all_data,path=path_BP,data_sort=44)
present(plan31_2_4_BP);new_index_present(plan31_2_4_BP)
#第一种 错误版本
plan31_wrong_1_=run(data=all_data,path=path,data_sort=411)
present(plan31_wrong_1_);new_index_present(plan31_wrong_1_)
plan31_wrong_1_BP=run(data=all_data,path=path_BP,data_sort=411)
present(plan31_wrong_1_BP);new_index_present(plan31_wrong_1_BP)
#第二种 错误版本
plan31_wrong_2_=run(data=all_data,path=path,data_sort=421)
present(plan31_wrong_2_);new_index_present(plan31_wrong_2_)
plan31_wrong_2_BP=run(data=all_data,path=path_BP,data_sort=421)
present(plan31_wrong_2_BP);new_index_present(plan31_wrong_2_BP)
#第三种 错误版本
plan31_wrong_3_=run(data=all_data,path=path,data_sort=431)
present(plan31_wrong_3_);new_index_present(plan31_wrong_3_)
plan31_wrong_3_BP=run(data=all_data,path=path_BP,data_sort=431)
present(plan31_wrong_3_BP);new_index_present(plan31_wrong_3_BP)
#第四种：1手显现+逆；2手reflection+逆 错误版本
plan31_wrong_4_=run(data=all_data,path=path,data_sort=441)
present(plan31_wrong_4_);new_index_present(plan31_wrong_4_)
plan31_wrong_4_BP=run(data=all_data,path=path_BP,data_sort=441)
present(plan31_wrong_4_BP);new_index_present(plan31_wrong_4_BP)
#第八种：1手显现+逆；2手reflection+逆 错误版本
plan31_wrong_8_=run(data=all_data,path=path,data_sort=481)
present(plan31_wrong_8_);new_index_present(plan31_wrong_8_)
plan31_wrong_8_BP=run(data=all_data,path=path_BP,data_sort=481)
present(plan31_wrong_8_BP);new_index_present(plan31_wrong_8_BP)


# 双轨+顺序：======================================================
# 单纯双轨，不考虑reflection：
plan2_=run(data=all_data,path=path,data_sort=2);present(plan2_)
plan2_BP=run(data=all_data,path=path_BP,data_sort=2);present(plan2_BP)

# benchmark:没有反一反
plan34_2_0_=run(data=all_data,path=path,data_sort=51,choice = 0);present(plan34_2_0_)
plan34_2_0_BP=run(data=all_data,path=path_BP,data_sort=51,choice = 0);present(plan34_2_0_BP)
# benchmark+plan11形式的反一反
plan34_2_11_=run(data=all_data,path=path,data_sort=51,choice = 11);present(plan34_2_11_)
plan34_2_11_BP=run(data=all_data,path=path_BP,data_sort=5,choice = 11);present(plan34_2_11_BP)

plan34_2_13_=run(data=all_data,path=path,data_sort=51,choice = 13);present(plan34_2_13_)
plan34_2_13_BP=run(data=all_data,path=path_BP,data_sort=51,choice = 13);present(plan34_2_13_BP)

plan34_2_131_=run(data=all_data,path=path,data_sort=51,choice = 131);present(plan34_2_131_)
plan34_2_131_BP=run(data=all_data,path=path_BP,data_sort=51,choice = 131);present(plan34_2_131_BP)

present(plan34_2_11_);present(plan34_2_11_BP)
present(plan34_2_13_);present(plan34_2_13_BP)
present(plan34_2_131_);present(plan34_2_131_BP)


# 双轨+逆序：======================================================
# 单纯双轨，不考虑reflection：
plan2_=run(data=all_data,path=path,data_sort=2);present(plan2_)
plan2_BP=run(data=all_data,path=path_BP,data_sort=2);present(plan2_BP)

# benchmark:没有反一反
plan34_Inverse2_0_=run(data=all_data,path=path,data_sort=7,choice = 0);present(plan34_Inverse2_0_)
plan34_Inverse2_0_BP=run(data=all_data,path=path_BP,data_sort=7,choice = 0);present(plan34_Inverse2_0_BP)
# benchmark+plan11形式的反一反
plan34_Inverse2_11_=run(data=all_data,path=path,data_sort=7,choice = 11);present(plan34_Inverse2_11_)
plan34_Inverse2_11_BP=run(data=all_data,path=path_BP,data_sort=7,choice = 11);present(plan34_Inverse2_11_BP)

plan34_Inverse2_13_=run(data=all_data,path=path,data_sort=7,choice = 13);present(plan34_Inverse2_13_)
plan34_Inverse2_13_BP=run(data=all_data,path=path_BP,data_sort=7,choice = 13);present(plan34_Inverse2_13_BP)

plan34_Inverse2_131_=run(data=all_data,path=path,data_sort=7,choice = 131);present(plan34_Inverse2_131_)
plan34_Inverse2_131_BP=run(data=all_data,path=path_BP,data_sort=7,choice = 131);present(plan34_Inverse2_131_BP)

present(plan34_Inverse2_0_);present(plan34_Inverse2_0_BP)
present(plan34_Inverse2_11_);present(plan34_Inverse2_11_BP)
present(plan34_Inverse2_13_);present(plan34_Inverse2_13_BP)
present(plan34_Inverse2_131_);present(plan34_Inverse2_131_BP)








# 正确版本1：

plan1_0_=run(data=all_data,path=path,data_sort=51,choice = 0)
present(plan1_0_);new_index_present(plan1_0_)
plan1_0_BP=run(data=all_data,path=path_BP,data_sort=51,choice = 0)
present(plan1_0_BP);new_index_present(plan1_0_BP)

plan1_11_=run(data=all_data,path=path,data_sort=51,choice = 11)
present(plan1_11_);new_index_present(plan1_11_)
plan1_11_BP=run(data=all_data,path=path_BP,data_sort=51,choice = 11)
present(plan1_11_BP);new_index_present(plan1_11_BP)

plan1_13_=run(data=all_data,path=path,data_sort=51,choice = 13)
present(plan1_13_);new_index_present(plan1_13_)
plan1_13_BP=run(data=all_data,path=path_BP,data_sort=51,choice = 13)
present(plan1_13_BP);new_index_present(plan1_13_BP)

plan10_13_=run(data=all_data,path=path,data_sort=510,choice = 13)
present(plan10_13_);new_index_present(plan10_13_)
plan10_13_BP=run(data=all_data,path=path_BP,data_sort=510,choice = 13)
present(plan10_13_BP);new_index_present(plan10_13_BP)

plan1_131_=run(data=all_data,path=path,data_sort=51,choice = 131)
present(plan1_131_);new_index_present(plan1_131_)
plan1_131_BP=run(data=all_data,path=path_BP,data_sort=51,choice = 131)
present(plan1_131_BP);new_index_present(plan1_131_BP)
# 正确版本4：
plan4_0_=run(data=all_data,path=path,data_sort=54,choice = 0)
present(plan4_0_);new_index_present(plan4_0_)
plan4_0_BP=run(data=all_data,path=path_BP,data_sort=54,choice = 0)
present(plan4_0_BP);new_index_present(plan4_0_BP)

plan4_13_=run(data=all_data,path=path,data_sort=54,choice = 13)
present(plan4_13_);new_index_present(plan4_13_)
plan4_13_BP=run(data=all_data,path=path_BP,data_sort=54,choice = 13)
present(plan4_13_BP);new_index_present(plan4_13_BP)
plan4_131_BP=run(data=all_data,path=path_BP,data_sort=54,choice = 131)
present(plan4_131_BP);new_index_present(plan4_131_BP)

# 正确版本的13显现24reflection，均为逆序
plan5_13_=run(data=all_data,path=path,data_sort=555,choice = 13)
present(plan5_13_);new_index_present(plan5_13_)
plan5_13_BP=run(data=all_data,path=path_BP,data_sort=555,choice = 13)
present(plan5_13_BP);new_index_present(plan5_13_BP)
# 错误版本的13显现24reflection，均为逆序
plan55_13_=run(data=all_data,path=path,data_sort=55,choice = 13)
present(plan55_13_);new_index_present(plan55_13_)
plan55_13_BP=run(data=all_data,path=path_BP,data_sort=55,choice = 13)
present(plan55_13_BP);new_index_present(plan55_13_BP)

# 错误版本1：
plan11_13_=run(data=all_data,path=path,data_sort=511,choice = 13)
present(plan11_13_);new_index_present(plan11_13_)
plan11_13_BP=run(data=all_data,path=path_BP,data_sort=511,choice = 13)
present(plan11_13_BP);new_index_present(plan11_13_BP)
# 错误版本4：
plan41_13_=run(data=all_data,path=path,data_sort=541,choice = 13)
present(plan41_13_);new_index_present(plan41_13_)
plan41_13_BP=run(data=all_data,path=path_BP,data_sort=541,choice = 13)
present(plan41_13_BP);new_index_present(plan41_13_BP)

# 正确版本5：
plan5_0_=run(data=all_data,path=path,data_sort=50,choice = 13)
present(plan5_0_);new_index_present(plan5_0_)
plan5_0_BP=run(data=all_data,path=path_BP,data_sort=50,choice =13)
present(plan5_0_BP);new_index_present(plan5_0_BP)
# 错误版本5：
plan51_0_=run(data=all_data,path=path,data_sort=501,choice = 13)
present(plan51_0_);new_index_present(plan51_0_)
plan51_0_BP=run(data=all_data,path=path_BP,data_sort=501,choice =13)
present(plan51_0_BP);new_index_present(plan51_0_BP)

# 1、formula准备------------------------------------------
data=read.csv('data\\treat_two_hundred.csv',header = T)
data1=read.csv('data\\new.csv',header = T)
data2=read.csv('data\\new1.csv',header = T)
data3=read.csv('data\\new2.csv',header = T)
data1=data1[,-23]
all_data=cbind(data,data1,data2,data3)
# 不分BP的路径：
{
  library(stringr)
  formula=read.csv2('data\\246formula.csv',header = F,stringsAsFactors = F)
  R2A=formula[1:2,];R2B=formula[4:5,]
  R4A=formula[7:12,];R4B=formula[14:19,]
  R6A=formula[21:30,];R6B=formula[32:41,]
  Formula=function(R){
    R_=str_split(R,pattern = ',',simplify = T)
    k=ifelse(nchar(R[1])==8,3,2)
    R_BP=apply(R_[,1:k], 1, function(x)str_c(x,collapse = '-'))
    return(str_c(R_BP,collapse = '|'))
  }
  R2A=Formula(R2A);R2B=Formula(R2B)
  R4A=Formula(R4A);R4B=Formula(R4B)
  R6A=Formula(R6A);R6B=Formula(R6B)
  
  path=matrix(c(R2A,R4A,R6A, R2A,R4B,R6A, R2A,R4A,R6B, R2A,R4B,R6B, R2B,R4A,R6A, R2B,R4B,R6A, R2B,R4A,R6B, R2B,R4B,R6B),byrow = T,ncol = 3)
  colnames(path)=c('R2','R4','R6')
  
}
c=c('data','all_data','path_BP','path')
a=ls()
rm(list=(setdiff(a,c)))
gc()
library(stringr)
# 2、训练==================================================

# 2.1定义计算每6个真实数据下的真实路径final_comb:
true_path0=function(dataset){
  t0=table(dataset[1:6])
  result0=paste0(t0[1],t0[2])
  t1=table(dataset[1:5])
  result1=paste0(t1[1],t1[2])
  t2=table(dataset[1:4])
  result2=paste0(t2[1],t2[2])
  comb1=paste0(result2,'-',result1,'-',result0)
  
  t3=table(dataset[3:6])
  result3=paste0(t3[1],t3[2])
  t4=table(dataset[3:5])
  result4=paste0(t4[1],t4[2])
  t5=table(dataset[3:4])
  result5=paste0(t5[1],t5[2])
  comb2=paste0(result5,'-',result4,'-',result3)
  
  t6=table(dataset[5:6])
  result6=paste0(t6[1],t6[2])
  t7=table(dataset[5:5])
  result7=paste0(t7[1],t7[2])
  comb3=paste0(result7,'-',result6)
  return(matrix(c(comb1,comb2,comb3),ncol = 3))
  
}
true_path1=function(dataset){
  t0=table(dataset[c(1:5,6)])
  result0=paste0(t0[1],t0[2])
  t1=table(dataset[c(1:5)])
  result1=paste0(t1[1],t1[2])
  t2=table(dataset[c(1:4)])
  result2=paste0(t2[1],t2[2])
  comb1=paste0(result2,'-',result1,'-',result0)
  
  t3=table(dataset[c(1:3,6)])
  result3=paste0(t3[1],t3[2])
  t4=table(dataset[c(1:3)])
  result4=paste0(t4[1],t4[2])
  t5=table(dataset[c(1:2)])
  result5=paste0(t5[1],t5[2])
  comb2=paste0(result5,'-',result4,'-',result3)
  
  t6=table(dataset[c(1:1,6)])
  result6=paste0(t6[1],t6[2])
  t7=table(dataset[c(1:1)])
  result7=paste0(t7[1],t7[2])
  comb3=paste0(result7,'-',result6)
  return(matrix(c(comb1,comb2,comb3),ncol = 3))
  
}
# 2.2定义6个真实数据下根据真实路径下的量化值-1或1:
# path=path_BP为分Bp的：

# Inverse有3种形式F，代表不同的取数据计算真实路径的方式，对应true_path0~2
new_quantize=function(path,set,Inverse=F){
  # 分BP的path有6列：
  if(Inverse==T)real_comb=true_path1(set)
  else if (Inverse==F)real_comb=true_path0(set)
  if(ncol(path)==6){
    r6=matrix(0,ncol = nrow(path),nrow = 1)
    for (j in 1:nrow(path)) {
      sparate_condition=ifelse(Inverse==T,set[1],set[1])
      r6[1,j]=ifelse(sparate_condition=='B'|sparate_condition==1,ifelse(str_detect(real_comb[1,1],path[j,'B_R6']),1,-1),ifelse(str_detect(real_comb[1,1],path[j,'P_R6']),1,-1))
    }
    r4=matrix(0,ncol = nrow(path),nrow = 1)
    for (j in 1:nrow(path)) {
      sparate_condition=ifelse(Inverse==T,set[1],set[3])
      r4[1,j]=ifelse(sparate_condition=='B'|sparate_condition==1,ifelse(str_detect(real_comb[1,2],path[j,'B_R4']),1,-1),ifelse(str_detect(real_comb[1,2],path[j,'P_R4']),1,-1))
    }
    r2=matrix(0,ncol = nrow(path),nrow = 1)
    for (j in 1:nrow(path)) {
      sparate_condition=ifelse(Inverse==T,set[1],set[5])
      r2[1,j]=ifelse(sparate_condition=='B'|sparate_condition==1,ifelse(str_detect(real_comb[1,3],path[j,'B_R2']),1,-1),ifelse(str_detect(real_comb[1,3],path[j,'P_R2']),1,-1))
    }
  }
  else if(ncol(path)==3){
    r6=matrix(0,ncol = nrow(path),nrow = 1)
    for (j in 1:nrow(path)) {
      r6[1,j]=ifelse(str_detect(real_comb[1,1],path[j,'R6']),1,-1)
    }
    r4=matrix(0,ncol = nrow(path),nrow = 1)
    for (j in 1:nrow(path)) {
      r4[1,j]=ifelse(str_detect(real_comb[1,2],path[j,'R4']),1,-1)
    }
    r2=matrix(0,ncol = nrow(path),nrow = 1)
    for (j in 1:nrow(path)) {
      r2[1,j]=ifelse(str_detect(real_comb[1,3],path[j,'R2']),1,-1)
    }
  }
  return(r6+r4+r2)
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
# 双轨取数据：
{
  data_sort0=function(data=data,m,path=path){
    result=matrix(ncol = nrow(path),nrow = 68)
    for (i in 21:68) {
      result[i,]=new_quantize(path,set = data[i-9:i,m])
    }
    return(result[-c(1:20),])
  }
  # “错误”的版本4：断开取数据的方式-从21手
  data_sort1=function(data=data,m,path=path){
    n=nrow(data)
    reflaction=ifelse(data[,m]=="B",'P','B')
    # 第一手：显+逆
    {
      first_hand=matrix(ncol = nrow(path))
      for (i in seq(21,n,4)) {
        first_hand_quantity=new_quantize(path,set = data[c(seq(i-2,i-10,-2),i),m])
        first_hand=rbind(first_hand,first_hand_quantity)
      }
      first_hand=first_hand[-1,]
    }
    # 第二手：显+逆
    { 
      second_hand=matrix(ncol = nrow(path) )
      for (i in seq(22,n,4)) {
        second_hand_quantity=new_quantize(path,set = data[c(seq(i-2,i-10,-2),i),m])
        second_hand=rbind(second_hand,second_hand_quantity)
      }
      second_hand=second_hand[-1,]
    }
    # 第三手：reflection+逆
    {
      third_hand=matrix(ncol = nrow(path))
      for (i in seq(23,n,4)) {
        rejection=factor(c(reflaction[seq((i-2),(i-10),-2)],as.character(data[i,m])),levels = c('B',"P"))
        third_hand_quantity=new_quantize(path,set = rejection)
        third_hand=rbind(third_hand,third_hand_quantity)
      }
      third_hand=third_hand[-1,]
    }
    # 第四手：reflection+逆
    { 
      forth_hand=matrix(ncol = nrow(path) )
      for (i in seq(24,n,4)) {
        rejection=factor(c(reflaction[seq((i-2),(i-10),-2)],as.character(data[i,m])),levels = c('B',"P"))
        forth_hand_quantity=new_quantize(path,set = rejection)
        forth_hand=rbind(forth_hand,forth_hand_quantity)
      }
      forth_hand=forth_hand[-1,]
    }
    return(list(first_hand,second_hand,third_hand,forth_hand))
  }
  # “错误”的版本4：断开取数据的方式-从11手
  data_sort1_eleven=function(data=data,m,path=path){
    n=nrow(data)
    reflaction=ifelse(data[,m]=="B",'P','B')
    # 第一手：显+逆
    {
      first_hand=matrix(ncol = nrow(path))
      for (i in seq(11,n,4)) {
        first_hand_quantity=new_quantize(path,set = data[c(seq(i-2,i-10,-2),i),m])
        first_hand=rbind(first_hand,first_hand_quantity)
      }
      first_hand=first_hand[-1,]
    }
    # 第二手：显+逆
    { 
      second_hand=matrix(ncol = nrow(path) )
      for (i in seq(12,n,4)) {
        second_hand_quantity=new_quantize(path,set = data[c(seq(i-2,i-10,-2),i),m])
        second_hand=rbind(second_hand,second_hand_quantity)
      }
      second_hand=second_hand[-1,]
    }
    # 第三手：reflection+逆
    {
      third_hand=matrix(ncol = nrow(path))
      for (i in seq(13,n,4)) {
        rejection=factor(c(reflaction[seq((i-2),(i-10),-2)],as.character(data[i,m])),levels = c('B',"P"))
        third_hand_quantity=new_quantize(path,set = rejection)
        third_hand=rbind(third_hand,third_hand_quantity)
      }
      third_hand=third_hand[-1,]
    }
    # 第四手：reflection+逆
    { 
      forth_hand=matrix(ncol = nrow(path) )
      for (i in seq(14,n,4)) {
        rejection=factor(c(reflaction[seq((i-2),(i-10),-2)],as.character(data[i,m])),levels = c('B',"P"))
        forth_hand_quantity=new_quantize(path,set = rejection)
        forth_hand=rbind(forth_hand,forth_hand_quantity)
      }
      forth_hand=forth_hand[-1,]
    }
    return(list(first_hand,second_hand,third_hand,forth_hand))
  }
  
  # 正确的版本4：连续取数据的方式
  data_sort1_correct=function(data=data,m,path=path){
    n=nrow(data)
    reflaction=ifelse(data[,m]=="B",'P','B')
    # 第一手：显+逆
    {
      first_hand=matrix(ncol = nrow(path))
      for (i in seq(21,n,4)) {
        first_hand_quantity=new_quantize(path,set = data[c(seq(i-2,i-10,-2),i),m],Inverse = T)
        first_hand=rbind(first_hand,first_hand_quantity)
      }
      first_hand=first_hand[-1,]
    }
    # 第二手：显+逆
    { 
      second_hand=matrix(ncol = nrow(path) )
      for (i in seq(22,n,4)) {
        second_hand_quantity=new_quantize(path,set = data[c(seq(i-2,i-10,-2),i),m],Inverse = T)
        second_hand=rbind(second_hand,second_hand_quantity)
      }
      second_hand=second_hand[-1,]
    }
    # 第三手：reflection+逆
    {
      third_hand=matrix(ncol = nrow(path))
      for (i in seq(23,n,4)) {
        rejection=factor(c(reflaction[seq((i-2),(i-10),-2)],as.character(data[i,m])),levels = c('B',"P"))
        third_hand_quantity=new_quantize(path,set = rejection,Inverse = T)
        third_hand=rbind(third_hand,third_hand_quantity)
      }
      third_hand=third_hand[-1,]
    }
    # 第四手：reflection+逆
    { 
      forth_hand=matrix(ncol = nrow(path) )
      for (i in seq(24,n,4)) {
        rejection=factor(c(reflaction[seq((i-2),(i-10),-2)],as.character(data[i,m])),levels = c('B',"P"))
        forth_hand_quantity=new_quantize(path,set = rejection,Inverse = T)
        forth_hand=rbind(forth_hand,forth_hand_quantity)
      }
      forth_hand=forth_hand[-1,]
    }
    return(list(first_hand,second_hand,third_hand,forth_hand))
  }
  # plan2319 错误版本-21手
  data_sort2=function(data=data,m,path=path){
    n=nrow(data)
    reflaction=ifelse(data[,m]=="B",'P','B')
    # 第一手：显+逆
    {
      first_hand=matrix(ncol = nrow(path))
      for (i in seq(21,n,4)) {
        first_hand_quantity=new_quantize(path,set = data[c(seq(i-2,i-10,-2),i),m])
        first_hand=rbind(first_hand,first_hand_quantity)
      }
      first_hand=first_hand[-1,]
    }
    # 第二手：re+逆
    { 
      second_hand=matrix(ncol = nrow(path) )
      for (i in seq(22,n,4)) {
        rejection=factor(c(reflaction[seq((i-2),(i-10),-2)],as.character(data[i,m])),levels = c('B',"P"))
        second_hand_quantity=new_quantize(path,set =rejection )
        second_hand=rbind(second_hand,second_hand_quantity)
      }
      second_hand=second_hand[-1,]
    }
    # 第三手：显+逆
    {
      third_hand=matrix(ncol = nrow(path))
      for (i in seq(23,n,4)) {
        third_hand_quantity=new_quantize(path,set = data[c(seq(i-2,i-10,-2),i),m])
        third_hand=rbind(third_hand,third_hand_quantity)
      }
      third_hand=third_hand[-1,]
    }
    # 第四手：reflection+逆
    { 
      forth_hand=matrix(ncol = nrow(path) )
      for (i in seq(24,n,4)) {
        rejection=factor(c(reflaction[seq((i-2),(i-10),-2)],as.character(data[i,m])),levels = c('B',"P"))
        forth_hand_quantity=new_quantize(path,set = rejection)
        forth_hand=rbind(forth_hand,forth_hand_quantity)
      }
      forth_hand=forth_hand[-1,]
    }
    return(list(first_hand,second_hand,third_hand,forth_hand))
  }
  # plan2319 错误版本-11手
  data_sort2_eleven=function(data=data,m,path=path){
    n=nrow(data)
    reflaction=ifelse(data[,m]=="B",'P','B')
    # 第一手：显+逆
    {
      first_hand=matrix(ncol = nrow(path))
      for (i in seq(11,n,4)) {
        first_hand_quantity=new_quantize(path,set = data[c(seq(i-2,i-10,-2),i),m])
        first_hand=rbind(first_hand,first_hand_quantity)
      }
      first_hand=first_hand[-1,]
    }
    # 第二手：re+逆
    { 
      second_hand=matrix(ncol = nrow(path) )
      for (i in seq(12,n,4)) {
        rejection=factor(c(reflaction[seq((i-2),(i-10),-2)],as.character(data[i,m])),levels = c('B',"P"))
        second_hand_quantity=new_quantize(path,set =rejection )
        second_hand=rbind(second_hand,second_hand_quantity)
      }
      second_hand=second_hand[-1,]
    }
    # 第三手：显+逆
    {
      third_hand=matrix(ncol = nrow(path))
      for (i in seq(13,n,4)) {
        third_hand_quantity=new_quantize(path,set = data[c(seq(i-2,i-10,-2),i),m])
        third_hand=rbind(third_hand,third_hand_quantity)
      }
      third_hand=third_hand[-1,]
    }
    # 第四手：reflection+逆
    { 
      forth_hand=matrix(ncol = nrow(path) )
      for (i in seq(14,n,4)) {
        rejection=factor(c(reflaction[seq((i-2),(i-10),-2)],as.character(data[i,m])),levels = c('B',"P"))
        forth_hand_quantity=new_quantize(path,set = rejection)
        forth_hand=rbind(forth_hand,forth_hand_quantity)
      }
      forth_hand=forth_hand[-1,]
    }
    return(list(first_hand,second_hand,third_hand,forth_hand))
  }
  
  # plan2319 正确版本
  data_sort2_correct=function(data=data,m,path=path){
    n=nrow(data)
    reflaction=ifelse(data[,m]=="B",'P','B')
    # 第一手：显+逆
    {
      first_hand=matrix(ncol = nrow(path))
      for (i in seq(21,n,4)) {
        first_hand_quantity=new_quantize(path,set = data[c(seq(i-2,i-10,-2),i),m],Inverse = T)
        first_hand=rbind(first_hand,first_hand_quantity)
      }
      first_hand=first_hand[-1,]
    }
    # 第二手：re+逆
    { 
      second_hand=matrix(ncol = nrow(path) )
      for (i in seq(22,n,4)) {
        rejection=factor(c(reflaction[seq((i-2),(i-10),-2)],as.character(data[i,m])),levels = c('B',"P"))
        second_hand_quantity=new_quantize(path,set =rejection,Inverse = T )
        second_hand=rbind(second_hand,second_hand_quantity)
      }
      second_hand=second_hand[-1,]
    }
    # 第三手：显+逆
    {
      third_hand=matrix(ncol = nrow(path))
      for (i in seq(23,n,4)) {
        third_hand_quantity=new_quantize(path,set = data[c(seq(i-2,i-10,-2),i),m],Inverse = T)
        third_hand=rbind(third_hand,third_hand_quantity)
      }
      third_hand=third_hand[-1,]
    }
    # 第四手：reflection+逆
    { 
      forth_hand=matrix(ncol = nrow(path) )
      for (i in seq(24,n,4)) {
        rejection=factor(c(reflaction[seq((i-2),(i-10),-2)],as.character(data[i,m])),levels = c('B',"P"))
        forth_hand_quantity=new_quantize(path,set = rejection,Inverse = T)
        forth_hand=rbind(forth_hand,forth_hand_quantity)
      }
      forth_hand=forth_hand[-1,]
    }
    return(list(first_hand,second_hand,third_hand,forth_hand))
  }
  # plan2348
  data_sort3=function(data=data,m,path=path){
    n=nrow(data)
    reflaction=ifelse(data[,m]=="B",'P','B')
    # 第一手：显+逆
    {
      first_hand=matrix(ncol = nrow(path))
      for (i in seq(21,n,4)) {
        first_hand_quantity=new_quantize(path,set = data[c(seq(i-2,i-10,-2),i),m])
        first_hand=rbind(first_hand,first_hand_quantity)
      }
      first_hand=first_hand[-1,]
    }
    # 第二手：re+逆
    { 
      second_hand=matrix(ncol = nrow(path) )
      for (i in seq(22,n,4)) {
        rejection=factor(c(reflaction[seq((i-2),(i-10),-2)],as.character(data[i,m])),levels = c('B',"P"))
        second_hand_quantity=new_quantize(path,set =rejection )
        second_hand=rbind(second_hand,second_hand_quantity)
      }
      second_hand=second_hand[-1,]
    }
    # 第三手：re+逆
    {
      third_hand=matrix(ncol = nrow(path))
      for (i in seq(23,n,4)) {
        rejection=factor(c(reflaction[seq((i-2),(i-10),-2)],as.character(data[i,m])),levels = c('B',"P"))
        third_hand_quantity=new_quantize(path,set = rejection)
        third_hand=rbind(third_hand,third_hand_quantity)
      }
      third_hand=third_hand[-1,]
    }
    # 第四手：显+逆
    { 
      forth_hand=matrix(ncol = nrow(path) )
      for (i in seq(24,n,4)) {
        forth_hand_quantity=new_quantize(path,set = data[c(seq(i-2,i-18,-2),i),m])
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
    if(data_sort==0){
      temp=data_sort0(data,m,path)
      n_sumtemp=nrow(temp)
      sumtemp=matrix(nrow =n_sumtemp,ncol = nrow(path) )
      sumtemp[1:2,]=temp[1:2,]
      for (i in 3:n_sumtemp) {
        sumtemp[i,]=ifelse(temp[i-1,]==-3,-temp[i,],ifelse((temp[i-1,]==-1&temp[i-2,]>0)|(temp[i-1,]==1&temp[i-2,]>0),-temp[i,],temp[i,]))
      }
      # 计算新指标:
      pattern=matrix(nrow =n_sumtemp ,ncol = nrow(path));pattern[]=1
    }
    else if(data_sort==1){
      temp=data_sort1(data,m,path)
      first=temp[[1]]
      second=temp[[2]]
      third=temp[[3]]
      forth=temp[[4]]
      n_sumtemp=nrow(first)+nrow(second)+nrow(third)+nrow(forth)
      sumtemp=matrix(nrow =n_sumtemp ,ncol = nrow(path))
      # 计算新指标:
      pattern=matrix(nrow =n_sumtemp ,ncol = nrow(path))
      
      if(choice==0){
        forth=ifelse((third==-1&second>0)|(third==1&second>0),-forth,forth)
        third=ifelse((second==-1&first>0)|(second==1&first>0),-third,third)
        second=ifelse(first==-3,-second,second)
        pattern[]=1
      }
      else if(choice==11){
        second=ifelse(first<0,second,-second)
        third=ifelse(first<0,third,-third)
        forth=ifelse(first*second>0,forth,-forth)
        pattern[seq(1,n_sumtemp,4),]=1
        pattern[seq(2,n_sumtemp,4),]=1
        pattern[seq(3,n_sumtemp,4),]=ifelse(first<0,1,2)
        pattern[seq(4,n_sumtemp,4),]=ifelse(second<0,1,2)
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
        pattern[seq(2,n_sumtemp,4),]=1
        pattern[seq(3,n_sumtemp,4),]=ifelse(first<0,1,2)
        pattern[seq(4,n_sumtemp,4),]=ifelse(second<0,1,2)
      }
      
      
      sumtemp[seq(1,n_sumtemp,4),]=first
      sumtemp[seq(2,n_sumtemp,4),]=second
      sumtemp[seq(3,n_sumtemp,4),]=third
      sumtemp[seq(4,n_sumtemp,4),]=forth
      
    }
    else if(data_sort==-1){
      temp=data_sort1_eleven(data,m,path)
      first=temp[[1]]
      second=temp[[2]]
      third=temp[[3]]
      forth=temp[[4]]
      n_sumtemp=nrow(first)+nrow(second)+nrow(third)+nrow(forth)
      sumtemp=matrix(nrow =n_sumtemp ,ncol = nrow(path))
      # 计算新指标:
      pattern=matrix(nrow =n_sumtemp ,ncol = nrow(path))
      
      if(choice==0){
        forth=ifelse((third==-1&second[-nrow(second),]>0)|(third==1&second[-nrow(second),]>0),-forth,forth)
        third=ifelse((second[-nrow(second),]==-1&first>0)|(second[-nrow(second),]==1&first>0),-third,third)
        second=ifelse(first==-3,-second,second)
        pattern[]=1
      }
      else if(choice==11){
        second=ifelse(first<0,second,-second)
        third=ifelse(first[-nrow(first),]<0,third,-third)
        forth=ifelse(first[-nrow(first),]*second[-nrow(second),]>0,forth,-forth)
        pattern[seq(1,n_sumtemp,4),]=1
        pattern[seq(2,n_sumtemp,4),]=1
        pattern[seq(3,n_sumtemp,4),]=ifelse(first[-nrow(first),]<0,1,2)
        pattern[seq(4,n_sumtemp,4),]=ifelse(second[-nrow(second),]<0,1,2)
      }
      else if(choice==13){
        third=ifelse(first[-nrow(first),]<0,third,-third)
        forth=ifelse(second[-nrow(second),]<0,forth,-forth)
        pattern[seq(1,n_sumtemp,4),]=1
        pattern[seq(2,n_sumtemp,4),]=1
        pattern[seq(3,n_sumtemp,4),]=ifelse(first[-nrow(first),]<0,1,2)
        pattern[seq(4,n_sumtemp,4),]=ifelse(second[-nrow(second),]<0,1,2)
      }
      else if(choice==131){
        second=-second
        third=ifelse(first[-nrow(first),]<0,third,-third)
        forth=ifelse(second[-nrow(second),]>0,forth,-forth) # 注意这里的符号
        pattern[seq(1,n_sumtemp,4),]=1
        pattern[seq(2,n_sumtemp,4),]=1
        pattern[seq(3,n_sumtemp,4),]=ifelse(first[-nrow(first),]<0,1,2)
        pattern[seq(4,n_sumtemp,4),]=ifelse(second[-nrow(second),]<0,1,2)
      }
      
      
      sumtemp[seq(1,n_sumtemp,4),]=first
      sumtemp[seq(2,n_sumtemp,4),]=second
      sumtemp[seq(3,n_sumtemp,4),]=third
      sumtemp[seq(4,n_sumtemp,4),]=forth
      
    }
    else if(data_sort==10){
      temp=data_sort1_correct(data,m,path)
      first=temp[[1]]
      second=temp[[2]]
      third=temp[[3]]
      forth=temp[[4]]
      n_sumtemp=nrow(first)+nrow(second)+nrow(third)+nrow(forth)
      sumtemp=matrix(nrow =n_sumtemp ,ncol = nrow(path))
      # 计算新指标:
      pattern=matrix(nrow =n_sumtemp ,ncol = nrow(path))
      
      if(choice==0){
        forth=ifelse((third==-1&second>0)|(third==1&second>0),-forth,forth)
        third=ifelse((second==-1&first>0)|(second==1&first>0),-third,third)
        second=ifelse(first==-3,-second,second)
        pattern[]=1
      }
      else if(choice==11){
        second=ifelse(first<0,second,-second)
        third=ifelse(first<0,third,-third)
        forth=ifelse(first*second>0,forth,-forth)
        pattern[seq(1,n_sumtemp,4),]=1
        pattern[seq(2,n_sumtemp,4),]=1
        pattern[seq(3,n_sumtemp,4),]=ifelse(first<0,1,2)
        pattern[seq(4,n_sumtemp,4),]=ifelse(second<0,1,2)
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
        pattern[seq(2,n_sumtemp,4),]=1
        pattern[seq(3,n_sumtemp,4),]=ifelse(first<0,1,2)
        pattern[seq(4,n_sumtemp,4),]=ifelse(second<0,1,2)
      }
      
      sumtemp[seq(1,n_sumtemp,4),]=first
      sumtemp[seq(2,n_sumtemp,4),]=second
      sumtemp[seq(3,n_sumtemp,4),]=third
      sumtemp[seq(4,n_sumtemp,4),]=forth
      
    }
    else if(data_sort==2){
      temp=data_sort2(data,m,path)
      first=temp[[1]]
      second=temp[[2]]
      third=temp[[3]]
      forth=temp[[4]]
      n_sumtemp=nrow(first)+nrow(second)+nrow(third)+nrow(forth)
      # 计算新指标:
      pattern=matrix(nrow =n_sumtemp ,ncol = nrow(path))
      if(choice==0){
        second=ifelse(first<0,second,-second)
        forth=ifelse(third<0,forth,-forth)
        
        pattern[seq(1,n_sumtemp,4),]=1
        pattern[seq(2,n_sumtemp,4),]=ifelse(first<0,1,2)
        pattern[seq(3,n_sumtemp,4),]=1
        pattern[seq(4,n_sumtemp,4),]=ifelse(third<0,1,2)
        
      }
      if(choice==1){
        second=ifelse(first<0,second,-second)
        third=-third # 第三手固定反一反
        forth=ifelse(third<0,forth,-forth)
        
        pattern[seq(1,n_sumtemp,4),]=1
        pattern[seq(2,n_sumtemp,4),]=ifelse(first<0,1,2)
        pattern[seq(3,n_sumtemp,4),]=1
        pattern[seq(4,n_sumtemp,4),]=ifelse(third<0,1,2)
        
      }
      if(choice==2){
        second=ifelse(first<0,second,-second)
        third=ifelse(first<0,third,-third)
        forth=ifelse(first*third>0,forth,-forth)
        
        pattern[seq(1,n_sumtemp,4),]=1
        pattern[seq(2,n_sumtemp,4),]=ifelse(first<0,1,2)
        pattern[seq(3,n_sumtemp,4),]=1
        pattern[seq(4,n_sumtemp,4),]=ifelse(third<0,1,2)
        
      }
      sumtemp=matrix(nrow =n_sumtemp ,ncol = nrow(path))
      sumtemp[seq(1,n_sumtemp,4),]=first
      sumtemp[seq(2,n_sumtemp,4),]=second
      sumtemp[seq(3,n_sumtemp,4),]=third
      sumtemp[seq(4,n_sumtemp,4),]=forth
      
      
    }
    else if(data_sort==-2){
      temp=data_sort2_eleven(data,m,path)
      first=temp[[1]]
      second=temp[[2]]
      third=temp[[3]]
      forth=temp[[4]]
      n_sumtemp=nrow(first)+nrow(second)+nrow(third)+nrow(forth)
      # 计算新指标:
      pattern=matrix(nrow =n_sumtemp ,ncol = nrow(path))
      if(choice==0){
        second=ifelse(first<0,second,-second)
        forth=ifelse(third<0,forth,-forth)
        
        pattern[seq(1,n_sumtemp,4),]=1
        pattern[seq(2,n_sumtemp,4),]=ifelse(first<0,1,2)
        pattern[seq(3,n_sumtemp,4),]=1
        pattern[seq(4,n_sumtemp,4),]=ifelse(third<0,1,2)
        
      }
      if(choice==1){
        second=ifelse(first<0,second,-second)
        third=-third # 第三手固定反一反
        forth=ifelse(third<0,forth,-forth)
        
        pattern[seq(1,n_sumtemp,4),]=1
        pattern[seq(2,n_sumtemp,4),]=ifelse(first<0,1,2)
        pattern[seq(3,n_sumtemp,4),]=1
        pattern[seq(4,n_sumtemp,4),]=ifelse(third<0,1,2)
        
      }
      if(choice==2){
        second=ifelse(first<0,second,-second)
        third=ifelse(first[-nrow(first),]<0,third,-third)
        forth=ifelse(first[-nrow(first),]*third>0,forth,-forth)
        
        pattern[seq(1,n_sumtemp,4),]=1
        pattern[seq(2,n_sumtemp,4),]=ifelse(first<0,1,2)
        pattern[seq(3,n_sumtemp,4),]=1
        pattern[seq(4,n_sumtemp,4),]=ifelse(third<0,1,2)
        
      }
      sumtemp=matrix(nrow =n_sumtemp ,ncol = nrow(path))
      sumtemp[seq(1,n_sumtemp,4),]=first
      sumtemp[seq(2,n_sumtemp,4),]=second
      sumtemp[seq(3,n_sumtemp,4),]=third
      sumtemp[seq(4,n_sumtemp,4),]=forth
      
      
    }
    else if(data_sort==20){
      temp=data_sort2_correct(data,m,path)
      first=temp[[1]]
      second=temp[[2]]
      third=temp[[3]]
      forth=temp[[4]]
      n_sumtemp=nrow(first)+nrow(second)+nrow(third)+nrow(forth)
      # 计算新指标:
      pattern=matrix(nrow =n_sumtemp ,ncol = nrow(path))
      if(choice==0){
        second=ifelse(first<0,second,-second)
        forth=ifelse(third<0,forth,-forth)
        
        pattern[seq(1,n_sumtemp,4),]=1
        pattern[seq(2,n_sumtemp,4),]=ifelse(first<0,1,2)
        pattern[seq(3,n_sumtemp,4),]=1
        pattern[seq(4,n_sumtemp,4),]=ifelse(third<0,1,2)
        
      }
      if(choice==1){
        second=ifelse(first<0,second,-second)
        third=-third # 第三手固定反一反
        forth=ifelse(third<0,forth,-forth)
        
        pattern[seq(1,n_sumtemp,4),]=1
        pattern[seq(2,n_sumtemp,4),]=ifelse(first<0,1,2)
        pattern[seq(3,n_sumtemp,4),]=1
        pattern[seq(4,n_sumtemp,4),]=ifelse(third<0,1,2)
        
      }
      if(choice==2){
        second=ifelse(first<0,second,-second)
        third=ifelse(first<0,third,-third)
        forth=ifelse(first*third>0,forth,-forth)
        
        pattern[seq(1,n_sumtemp,4),]=1
        pattern[seq(2,n_sumtemp,4),]=ifelse(first<0,1,2)
        pattern[seq(3,n_sumtemp,4),]=1
        pattern[seq(4,n_sumtemp,4),]=ifelse(third<0,1,2)
        
      }
      sumtemp=matrix(nrow =n_sumtemp ,ncol = nrow(path))
      sumtemp[seq(1,n_sumtemp,4),]=first
      sumtemp[seq(2,n_sumtemp,4),]=second
      sumtemp[seq(3,n_sumtemp,4),]=third
      sumtemp[seq(4,n_sumtemp,4),]=forth
      
      
    }
    else if(data_sort==3){ 
      temp=data_sort3(data,m,path)
      first=temp[[1]]
      second=temp[[2]]
      third=temp[[3]]
      forth=temp[[4]]
      n_sumtemp=nrow(first)+nrow(second)+nrow(third)+nrow(forth)
      second=-second
      third=ifelse(first<0,third,-third)
      forth=ifelse(second>0,forth,-forth)
      # 计算新指标:
      pattern=matrix(nrow =n_sumtemp ,ncol = nrow(path))
      pattern[seq(1,n_sumtemp,4),]=1
      pattern[seq(2,n_sumtemp,4),]=1
      pattern[seq(3,n_sumtemp,4),]=ifelse(first<0,1,2)
      pattern[seq(4,n_sumtemp,4),]=ifelse(second<0,1,2)
      
      sumtemp=matrix(nrow =n_sumtemp ,ncol = nrow(path))
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
  cat('\n')
  final_result=rbind(result[[3]],round(apply(result[[4]], 2, function(x)sum(x>=0))/760,3))
  final_result=rbind(final_result,apply(result[[4]], 2, sum))
  row.names(final_result)=c('quantity','ratio','sum')
  first5=apply(result[[4]], 2, function(x)str_c(sort(x,decreasing = T)[1:5],collapse = ' '))
  last5=apply(result[[4]], 2, function(x)str_c(sort(x,decreasing = F)[1:5],collapse = ' '))
  #print(paste0('mean:',mean(result[[3]]),'  ','Standard deviation:',round(sd(result[[3]]),3)))
  for (i in 1:8) {
    print(paste0('formula',i,"' new index of First&last five:",first5[i],';',last5[i]))
  }
  final_result[1,]
  cat('\n')
  print(final_result)
}

##======================================================================

# 不分BP：错误版本4-从21
plan1_11_=run(data=all_data,path=path,data_sort=1,choice = 11)
present(plan1_11_)
plan1_13_=run(data=all_data,path=path,data_sort=1,choice = 13)
present(plan1_13_)
plan1_131_=run(data=all_data,path=path,data_sort=1,choice = 131)
present(plan1_131_)
# 不分BP：错误版本4-从11
plan1_11_eleven=run(data=all_data,path=path,data_sort=-1,choice = 11)
present(plan1_11_eleven)
plan1_13_eleven=run(data=all_data,path=path,data_sort=-1,choice = 13)
present(plan1_13_eleven)
plan1_131_eleven=run(data=all_data,path=path,data_sort=-1,choice = 131)
present(plan1_131_eleven)

# plan2319:不分BP-从21
plan2_2=run(data=all_data,path=path,data_sort=2,choice = 2)
present(plan2_2)#plan11
plan2_=run(data=all_data,path=path,data_sort=2)
present(plan2_)#plan13
plan2_1=run(data=all_data,path=path,data_sort=2,choice = 1)
present(plan2_1)#plan13-1
# plan2319:不分BP-从11
plan2_2_eleven=run(data=all_data,path=path,data_sort=-2,choice = 2)
present(plan2_2_eleven)#plan11
plan2_eleven=run(data=all_data,path=path,data_sort=-2)
present(plan2_eleven)#plan13
plan2_1_eleven=run(data=all_data,path=path,data_sort=-2,choice = 1)
present(plan2_1_eleven)#plan13-1


# 不分BP：正确版本4，只做不分BP的
plan1_110_=run(data=all_data,path=path,data_sort=10,choice = 11)
present(plan1_110_)
plan1_130_=run(data=all_data,path=path,data_sort=10,choice = 13)
present(plan1_130_)
plan1_1310_=run(data=all_data,path=path,data_sort=10,choice = 131)
present(plan1_1310_)
# plan2319:分BP
plan2_BP=run(data=all_data,path=path_BP,data_sort=2)
present(plan2_BP) #plan13
plan2_1_BP=run(data=all_data,path=path_BP,data_sort=2,choice = 1)
present(plan2_1_BP)#plan13-1
plan2_2_BP=run(data=all_data,path=path_BP,data_sort=2,choice = 2)
present(plan2_2_BP)#plan11

# 正确版本plan2319:不分BP
plan2c_=run(data=all_data,path=path,data_sort=20)
present(plan2c_)#plan13
plan2c_1=run(data=all_data,path=path,data_sort=20,choice = 1)
present(plan2c_1)#plan13-1
plan2c_2=run(data=all_data,path=path,data_sort=20,choice = 2)
present(plan2c_2)#plan11

# plan2348:13-1的结果
plan3_131_BP=run(data=all_data,path=path_BP,data_sort=3)
present(plan3_131_BP)


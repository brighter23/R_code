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
  if(Inverse==T)real_comb=true_path1(set)
  else if (Inverse==F)real_comb=true_path0(set)
  if(ncol(path)==6){
    r10=matrix(0,ncol = nrow(path),nrow = 1)
    for (j in 1:nrow(path)) {
      sparate_condition=ifelse(Inverse==T,set[1],set[1])
      r10[1,j]=ifelse(sparate_condition=='B'|sparate_condition==1,ifelse(str_detect(real_comb[1,1],path[j,'B_R10']),1,-1),ifelse(str_detect(real_comb[1,1],path[j,'P_R10']),1,-1))
    }
    r8=matrix(0,ncol = nrow(path),nrow = 1)
    for (j in 1:nrow(path)) {
      sparate_condition=ifelse(Inverse==T,set[1],set[3])
      r8[1,j]=ifelse(sparate_condition=='B'|sparate_condition==1,ifelse(str_detect(real_comb[1,2],path[j,'B_R8']),1,-1),ifelse(str_detect(real_comb[1,2],path[j,'P_R8']),1,-1))
    }
    r6=matrix(0,ncol = nrow(path),nrow = 1)
    for (j in 1:nrow(path)) {
      sparate_condition=ifelse(Inverse==T,set[1],set[5])
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
# 双轨取数据：
{
  data_sort0=function(data=data,m,path=path){
    result=matrix(ncol = nrow(path),nrow = 68)
    for (i in 21:68) {
      result[i,]=new_quantize(path,set = data[i-9:i,m])
    }
   return(result[-c(1:20),])
  }
  # “错误”的版本4：断开取数据的方式
  data_sort1=function(data=data,m,path=path){
    n=nrow(data)
    reflaction=ifelse(data[,m]=="B",'P','B')
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
      third_hand=matrix(ncol = nrow(path))
      for (i in seq(23,n,4)) {
        rejection=factor(c(reflaction[seq((i-2),(i-18),-2)],as.character(data[i,m])),levels = c('B',"P"))
        third_hand_quantity=new_quantize(path,set = rejection)
        third_hand=rbind(third_hand,third_hand_quantity)
      }
      third_hand=third_hand[-1,]
    }
    # 第四手：reflection+逆
    { 
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
  # 正确的版本4：连续取数据的方式
  data_sort1_correct=function(data=data,m,path=path){
    n=nrow(data)
    reflaction=ifelse(data[,m]=="B",'P','B')
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
  # plan2319 错误版本
  data_sort2=function(data=data,m,path=path){
    n=nrow(data)
    reflaction=ifelse(data[,m]=="B",'P','B')
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
      second_hand=matrix(ncol = nrow(path) )
      for (i in seq(22,n,4)) {
        rejection=factor(c(reflaction[seq((i-2),(i-18),-2)],as.character(data[i,m])),levels = c('B',"P"))
        second_hand_quantity=new_quantize(path,set =rejection )
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
  # plan2319 正确版本
  data_sort2_correct=function(data=data,m,path=path){
    n=nrow(data)
    reflaction=ifelse(data[,m]=="B",'P','B')
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
      second_hand=matrix(ncol = nrow(path) )
      for (i in seq(22,n,4)) {
        rejection=factor(c(reflaction[seq((i-2),(i-18),-2)],as.character(data[i,m])),levels = c('B',"P"))
        second_hand_quantity=new_quantize(path,set =rejection,Inverse = T )
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
  # plan2348
  data_sort3=function(data=data,m,path=path){
    n=nrow(data)
    reflaction=ifelse(data[,m]=="B",'P','B')
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
      second_hand=matrix(ncol = nrow(path) )
      for (i in seq(22,n,4)) {
        rejection=factor(c(reflaction[seq((i-2),(i-18),-2)],as.character(data[i,m])),levels = c('B',"P"))
        second_hand_quantity=new_quantize(path,set =rejection )
        second_hand=rbind(second_hand,second_hand_quantity)
      }
      second_hand=second_hand[-1,]
    }
    # 第三手：re+逆
    {
      third_hand=matrix(ncol = nrow(path))
      for (i in seq(23,n,4)) {
        rejection=factor(c(reflaction[seq((i-2),(i-18),-2)],as.character(data[i,m])),levels = c('B',"P"))
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
final_result_formula=matrix(c(-3,-1,-1,1,-1,1,1,3,-1,-3,1,-1,1,-1,3,1,-1,1,-3,-1,1,3,-1,1,-1,1,1,3,-3,-1,-1,1,1,-1,-1,-3,3,1,1,-1,1,-1,3,1,-1,-3,1,-1,
                              1,3,-1,1,-1,1,-3,-1,3,1,1,-1,1,-1,-1,-3),ncol=8,byrow=T)
final_result_formula=apply(final_result_formula,1,function(x)str_c(x,collapse = ','))
sumtemp=apply(sumtemp,1,function(x)which(final_result_formula==str_c(x,collapse = ',')))

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
plan000=run(data=all_data,path=path_BP,data_sort=0);present(plan000)
# 错误版本4：分BP
plan1_BP=run(data=all_data,path=path_BP,data_sort=10)
present(plan1_BP)
plan1_11_BP=run(data=all_data,path=path_BP,data_sort=1,choice = 11)
present(plan1_11_BP)
plan1_13_BP=run(data=all_data,path=path_BP,data_sort=1,choice = 13)
present(plan1_13_BP)
plan1_131_BP=run(data=all_data,path=path_BP,data_sort=1,choice = 131)
present(plan1_131_BP)
# 不分BP：错误版本4
plan1_11_=run(data=all_data,path=path,data_sort=1,choice = 11)
present(plan1_11_)
plan1_13_=run(data=all_data,path=path,data_sort=1,choice = 13)
present(plan1_13_)
plan1_131_=run(data=all_data,path=path,data_sort=1,choice = 131)
present(plan1_131_)
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
# plan2319:不分BP
plan2_=run(data=all_data,path=path,data_sort=2)
present(plan2_)#plan13
plan2_1=run(data=all_data,path=path,data_sort=2,choice = 1)
present(plan2_1)#plan13-1
plan2_2=run(data=all_data,path=path,data_sort=2,choice = 2)
present(plan2_2)#plan11
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


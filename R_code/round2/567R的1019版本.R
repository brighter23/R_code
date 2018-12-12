# 1、formula准备------------------------------------------
data=read.csv('data\\treat_two_hundred.csv',header = T)
data1=read.csv('data\\new.csv',header = T)
data2=read.csv('data\\new1.csv',header = T)
data3=read.csv('data\\new2.csv',header = T)
data1=data1[,-23]
all_data=cbind(data,data1,data2,data3)

{
  library(stringr)
  formula=read.csv2('data\\567formula.csv',header = F,stringsAsFactors = F)
  R5A=formula[1:6,];R5B=formula[8:13,]
  R6A=formula[15:22,];R6B=formula[24:31,]
  R7A=formula[33:42,];R7B=formula[44:53,]
  Formula=function(R){
    R_=str_split(R,pattern = ',',simplify = T)
    R_B=apply(R_[,1:3], 1, function(x)str_c(x,collapse = '-'))
    R_P=apply(R_[,4:6], 1, function(x)str_c(x,collapse = '-'))
    return(cbind(str_c(R_B,collapse = '|'), str_c(R_P,collapse = '|')))
  }
  R5A=Formula(R5A);R5B=Formula(R5B)
  R6A=Formula(R6A);R6B=Formula(R6B)
  R7A=Formula(R7A);R7B=Formula(R7B)
  # 6R分BP的路径：
  path_BP=matrix(c(R5A,R6A,R7A, R5A,R6B,R7A, R5A,R6A,R7B, R5A,R6B,R7B, R5B,R6A,R7A, R5B,R6B,R7A, R5B,R6A,R7B, R5B,R6B,R7B),byrow = T,ncol = 6)
  colnames(path_BP)=c('B_R5','P_R5','B_R6','P_R6','B_R7','P_R7')
  
  # 6R不分BP的路径：
  R6A=formula[56:60,];R6B=formula[62:66,]
  R6A=Formula(R6A);R6B=Formula(R6B)
  R6A=str_c(R6A,collapse = '|');R6B=str_c(R6B,collapse = '|')
  path=matrix(c(R5A,R6A,R7A, R5A,R6B,R7A, R5A,R6A,R7B, R5A,R6B,R7B, R5B,R6A,R7A, R5B,R6B,R7A, R5B,R6A,R7B, R5B,R6B,R7B),byrow = T,ncol = 5)
  colnames(path)=c('B_R5','P_R5','R6','B_R7','P_R7')
  
}

c=c('data','all_data','path_BP','path')
a=ls()
rm(list=(setdiff(a,c)))
gc()
library(stringr)
# 2、训练==================================================

# 2.1定义计算每7个真实数据下的真实路径final_comb:
# 注意这里的6R维新加的
true_path0=function(dataset){
  t0=table(dataset[1:7])
  result0=paste0(t0[1],t0[2])
  t1=table(dataset[1:6])
  result1=paste0(t1[1],t1[2])
  t2=table(dataset[1:5])
  result2=paste0(t2[1],t2[2])
  comb1=paste0(result2,'-',result1,'-',result0)
  # 计算5R
  t3=table(dataset[3:7])
  result3=paste0(t3[1],t3[2])
  t4=table(dataset[3:6])
  result4=paste0(t4[1],t4[2])
  t5=table(dataset[3:5])
  result5=paste0(t5[1],t5[2])
  comb2=paste0(result5,'-',result4,'-',result3)
  # 计算6R
  t6=table(dataset[2:7])
  result6=paste0(t6[1],t6[2])
  t7=table(dataset[2:6])
  result7=paste0(t7[1],t7[2])
  t8=table(dataset[2:5])
  result8=paste0(t8[1],t8[2])
  comb3=paste0(result8,'-',result7,'-',result6)
  # 注意comb2和comb3顺序
  return(matrix(c(comb1,comb3,comb2),ncol = 3))
  
} 
true_path1=function(dataset){
  t0=table(dataset[c(1:6,7)])
  result0=paste0(t0[1],t0[2])
  t1=table(dataset[c(1:6)])
  result1=paste0(t1[1],t1[2])
  t2=table(dataset[c(1:5)])
  result2=paste0(t2[1],t2[2])
  comb1=paste0(result2,'-',result1,'-',result0)
  
  t3=table(dataset[c(1:4,7)])
  result3=paste0(t3[1],t3[2])
  t4=table(dataset[c(1:4)])
  result4=paste0(t4[1],t4[2])
  t5=table(dataset[c(1:3)])
  result5=paste0(t5[1],t5[2])
  comb2=paste0(result5,'-',result4,'-',result3)
  # 6R
  t6=table(dataset[c(1:5,7)])
  result6=paste0(t6[1],t6[2])
  t7=table(dataset[c(1:5)])
  result7=paste0(t7[1],t7[2])
  t8=table(dataset[c(1:4)])
  result8=paste0(t8[1],t8[2])
  comb3=paste0(result8,'-',result7,'-',result6)
  return(matrix(c(comb1,comb3,comb2),ncol = 3))
  
}
# 在顺序下true_path0计算真实路径的方式是连续的，但在逆序时就是断开的
# 在顺序下true_path1计算真实路径的方式是断开的，但在逆序时就是连续的
# 因此可以考虑在使用 逆序 时使用true_path1，在 顺序 时使用true_path0

# 2.2定义7个真实数据下根据真实路径下的量化值-1或1:
# path=path_BP为分Bp的：

# Inverse有2种形式F,T，代表不同的取数据计算真实路径的方式， 对应true_path0~1
new_quantize=function(path,set,Inverse=F){
  # 分BP的path有6列：
  if(Inverse==T)real_comb=true_path1(set)# 连续的方式
  else if (Inverse==F)real_comb=true_path0(set) # 断开的方式进行量化
  if(ncol(path)==6){
    r7=matrix(0,ncol = nrow(path),nrow = 1)
    for (j in 1:nrow(path)) {
      sparate_condition=ifelse(Inverse==T,set[1],set[1])
      # 因为这里是567R，所以判断使用B或P的是输入序列的第1，2，3个
      # 当为逆序时，都是第1个，即567R一起使用B的或者一起使用P的，这点与357或579R一样
      r7[1,j]=ifelse(sparate_condition=='B'|sparate_condition==1,ifelse(str_detect(real_comb[1,1],path[j,'B_R7']),1,-1),ifelse(str_detect(real_comb[1,1],path[j,'P_R7']),1,-1))
    }
    r6=matrix(0,ncol = nrow(path),nrow = 1)
    for (j in 1:nrow(path)) {
      sparate_condition=ifelse(Inverse==T,set[1],set[2])
      r6[1,j]=ifelse(sparate_condition=='B'|sparate_condition==1,ifelse(str_detect(real_comb[1,2],path[j,'B_R6']),1,-1),ifelse(str_detect(real_comb[1,2],path[j,'P_R6']),1,-1))
    }
    r5=matrix(0,ncol = nrow(path),nrow = 1)
    for (j in 1:nrow(path)) {
      sparate_condition=ifelse(Inverse==T,set[1],set[3])
      r5[1,j]=ifelse(sparate_condition=='B'|sparate_condition==1,ifelse(str_detect(real_comb[1,3],path[j,'B_R5']),1,-1),ifelse(str_detect(real_comb[1,3],path[j,'P_R5']),1,-1))
    }
  }
  else if(ncol(path)==5){
    r7=matrix(0,ncol = nrow(path),nrow = 1)
    for (j in 1:nrow(path)) {
      sparate_condition=ifelse(Inverse==T,set[1],set[1])
      r7[1,j]=ifelse(sparate_condition=='B'|sparate_condition==1,ifelse(str_detect(real_comb[1,1],path[j,'B_R7']),1,-1),ifelse(str_detect(real_comb[1,1],path[j,'P_R7']),1,-1))
    }
    r6=matrix(0,ncol = nrow(path),nrow = 1)
    for (j in 1:nrow(path)) {
      r6[1,j]=ifelse(str_detect(real_comb[1,2],path[j,'R6']),1,-1)
    }
    r5=matrix(0,ncol = nrow(path),nrow = 1)
    for (j in 1:nrow(path)) {
      sparate_condition=ifelse(Inverse==T,set[1],set[3])
      r5[1,j]=ifelse(sparate_condition=='B'|sparate_condition==1,ifelse(str_detect(real_comb[1,3],path[j,'B_R5']),1,-1),ifelse(str_detect(real_comb[1,3],path[j,'P_R5']),1,-1))
    }
  }
  return(r7+r6+r5)
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
# 取数据：双轨+全部显现
{
  
  # “错误”的版本4：断开取数据的方式——从21手开始，都为逆序
  data_sort1=function(data=data,m,path=path){
    n=nrow(data)
    # 第一手：显+逆
    {
      first_hand=matrix(ncol = nrow(path))
      for (i in seq(21,n,4)) {
        first_hand_quantity=new_quantize(path,set = data[c(seq(i-2,i-12,-2),i),m])
        first_hand=rbind(first_hand,first_hand_quantity)
      }
      first_hand=first_hand[-1,]
    }
    # 第二手：显+逆
    { 
      second_hand=matrix(ncol = nrow(path) )
      for (i in seq(22,n,4)) {
        second_hand_quantity=new_quantize(path,set = data[c(seq(i-2,i-12,-2),i),m])
        second_hand=rbind(second_hand,second_hand_quantity)
      }
      second_hand=second_hand[-1,]
    }
    # 第三手：显+逆
    {
      third_hand=matrix(ncol = nrow(path))
      for (i in seq(23,n,4)) {
        third_hand_quantity=new_quantize(path,set = data[c(seq(i-2,i-12,-2),i),m])
        third_hand=rbind(third_hand,third_hand_quantity)
      }
      third_hand=third_hand[-1,]
    }
    # 第四手：显+逆
    { 
      forth_hand=matrix(ncol = nrow(path) )
      for (i in seq(24,n,4)) {
        forth_hand_quantity=new_quantize(path,set = data[c(seq(i-2,i-12,-2),i),m])
        forth_hand=rbind(forth_hand,forth_hand_quantity)
      }
      forth_hand=forth_hand[-1,]
    }
    return(list(first_hand,second_hand,third_hand,forth_hand))
  }
  # “错误”的版本4：断开取数据的方式——从13手开始，都为逆序
  data_sort1_thirteen=function(data=data,m,path=path){
    n=nrow(data)
    # 第一手：显+逆
    {
      first_hand=matrix(ncol = nrow(path))
      for (i in seq(13,n,4)) {
        first_hand_quantity=new_quantize(path,set = data[c(seq(i-2,i-12,-2),i),m])
        first_hand=rbind(first_hand,first_hand_quantity)
      }
      first_hand=first_hand[-1,]
    }
    # 第二手：显+逆
    { 
      second_hand=matrix(ncol = nrow(path) )
      for (i in seq(14,n,4)) {
        second_hand_quantity=new_quantize(path,set = data[c(seq(i-2,i-12,-2),i),m])
        second_hand=rbind(second_hand,second_hand_quantity)
      }
      second_hand=second_hand[-1,]
    }
    # 第三手：显+逆
    {
      third_hand=matrix(ncol = nrow(path))
      for (i in seq(15,n,4)) {
        third_hand_quantity=new_quantize(path,set = data[c(seq(i-2,i-12,-2),i),m])
        third_hand=rbind(third_hand,third_hand_quantity)
      }
      third_hand=third_hand[-1,]
    }
    # 第四手：显+逆
    { 
      forth_hand=matrix(ncol = nrow(path) )
      for (i in seq(16,n,4)) {
        forth_hand_quantity=new_quantize(path,set = data[c(seq(i-2,i-12,-2),i),m])
        forth_hand=rbind(forth_hand,forth_hand_quantity)
      }
      forth_hand=forth_hand[-1,]
    }
    return(list(first_hand,second_hand,third_hand,forth_hand))
  }
  
  # “错误”的版本4：断开取数据的方式——从21手开始，1&2手改为顺序
  data_sort1_sequence=function(data=data,m,path=path){
    n=nrow(data)
    # 第一手：显+顺
    {
      first_hand=matrix(ncol = nrow(path))
      for (i in seq(21,n,4)) {
        first_hand_quantity=new_quantize(path,set = data[c(seq(i-12,i-2,2),i),m])
        first_hand=rbind(first_hand,first_hand_quantity)
      }
      first_hand=first_hand[-1,]
    }
    # 第二手：显+顺
    { 
      second_hand=matrix(ncol = nrow(path) )
      for (i in seq(22,n,4)) {
        second_hand_quantity=new_quantize(path,set = data[c(seq(i-12,i-2,2),i),m])
        second_hand=rbind(second_hand,second_hand_quantity)
      }
      second_hand=second_hand[-1,]
    }
    # 第三手：显+逆
    {
      third_hand=matrix(ncol = nrow(path))
      for (i in seq(23,n,4)) {
        third_hand_quantity=new_quantize(path,set = data[c(seq(i-2,i-12,-2),i),m])
        third_hand=rbind(third_hand,third_hand_quantity)
      }
      third_hand=third_hand[-1,]
    }
    # 第四手：显+逆
    { 
      forth_hand=matrix(ncol = nrow(path) )
      for (i in seq(24,n,4)) {
        forth_hand_quantity=new_quantize(path,set = data[c(seq(i-2,i-12,-2),i),m])
        forth_hand=rbind(forth_hand,forth_hand_quantity)
      }
      forth_hand=forth_hand[-1,]
    }
    return(list(first_hand,second_hand,third_hand,forth_hand))
  }
  # “错误”的版本4：断开取数据的方式——从13手开始，1&2手改为顺序
  data_sort1_sequence_thirteen=function(data=data,m,path=path){
    n=nrow(data)
    # 第一手：显+顺
    {
      first_hand=matrix(ncol = nrow(path))
      for (i in seq(13,n,4)) {
        first_hand_quantity=new_quantize(path,set = data[c(seq(i-12,i-2,2),i),m])
        first_hand=rbind(first_hand,first_hand_quantity)
      }
      first_hand=first_hand[-1,]
    }
    # 第二手：显+顺
    { 
      second_hand=matrix(ncol = nrow(path) )
      for (i in seq(14,n,4)) {
        second_hand_quantity=new_quantize(path,set = data[c(seq(i-12,i-2,2),i),m])
        second_hand=rbind(second_hand,second_hand_quantity)
      }
      second_hand=second_hand[-1,]
    }
    # 第三手：显+逆
    {
      third_hand=matrix(ncol = nrow(path))
      for (i in seq(15,n,4)) {
        third_hand_quantity=new_quantize(path,set = data[c(seq(i-2,i-12,-2),i),m])
        third_hand=rbind(third_hand,third_hand_quantity)
      }
      third_hand=third_hand[-1,]
    }
    # 第四手：显+逆
    { 
      forth_hand=matrix(ncol = nrow(path) )
      for (i in seq(16,n,4)) {
        forth_hand_quantity=new_quantize(path,set = data[c(seq(i-2,i-12,-2),i),m])
        forth_hand=rbind(forth_hand,forth_hand_quantity)
      }
      forth_hand=forth_hand[-1,]
    }
    return(list(first_hand,second_hand,third_hand,forth_hand))
  }
  
  # “错误”的版本4：断开取数据的方式——从21手开始，3&4手改为顺序
  data_sort1_sequence1=function(data=data,m,path=path){
    n=nrow(data)
    # 第一手：显+逆
    {
      first_hand=matrix(ncol = nrow(path))
      for (i in seq(21,n,4)) {
        first_hand_quantity=new_quantize(path,set = data[c(seq(i-2,i-12,-2),i),m])
        first_hand=rbind(first_hand,first_hand_quantity)
      }
      first_hand=first_hand[-1,]
    }
    # 第二手：显+逆
    { 
      second_hand=matrix(ncol = nrow(path) )
      for (i in seq(22,n,4)) {
        second_hand_quantity=new_quantize(path,set = data[c(seq(i-2,i-12,-2),i),m])
        second_hand=rbind(second_hand,second_hand_quantity)
      }
      second_hand=second_hand[-1,]
    }
    # 第三手：显+顺
    {
      third_hand=matrix(ncol = nrow(path))
      for (i in seq(23,n,4)) {
        third_hand_quantity=new_quantize(path,set = data[c(seq(i-12,i-2,2),i),m])
        third_hand=rbind(third_hand,third_hand_quantity)
      }
      third_hand=third_hand[-1,]
    }
    # 第四手：显+顺
    { 
      forth_hand=matrix(ncol = nrow(path) )
      for (i in seq(24,n,4)) {
        forth_hand_quantity=new_quantize(path,set = data[c(seq(i-12,i-2,2),i),m])
        forth_hand=rbind(forth_hand,forth_hand_quantity)
      }
      forth_hand=forth_hand[-1,]
    }
    return(list(first_hand,second_hand,third_hand,forth_hand))
  }
  # “错误”的版本4：断开取数据的方式——从13手开始，3&4手改为顺序
  data_sort1_sequence1_thirteen=function(data=data,m,path=path){
    n=nrow(data)
    # 第一手：显+逆
    {
      first_hand=matrix(ncol = nrow(path))
      for (i in seq(13,n,4)) {
        first_hand_quantity=new_quantize(path,set = data[c(seq(i-2,i-12,-2),i),m])
        first_hand=rbind(first_hand,first_hand_quantity)
      }
      first_hand=first_hand[-1,]
    }
    # 第二手：显+逆
    { 
      second_hand=matrix(ncol = nrow(path) )
      for (i in seq(14,n,4)) {
        second_hand_quantity=new_quantize(path,set = data[c(seq(i-2,i-12,-2),i),m])
        second_hand=rbind(second_hand,second_hand_quantity)
      }
      second_hand=second_hand[-1,]
    }
    # 第三手：显+顺
    {
      third_hand=matrix(ncol = nrow(path))
      for (i in seq(15,n,4)) {
        third_hand_quantity=new_quantize(path,set = data[c(seq(i-12,i-2,2),i),m])
        third_hand=rbind(third_hand,third_hand_quantity)
      }
      third_hand=third_hand[-1,]
    }
    # 第四手：显+顺
    { 
      forth_hand=matrix(ncol = nrow(path) )
      for (i in seq(16,n,4)) {
        forth_hand_quantity=new_quantize(path,set = data[c(seq(i-12,i-2,2),i),m])
        forth_hand=rbind(forth_hand,forth_hand_quantity)
      }
      forth_hand=forth_hand[-1,]
    }
    return(list(first_hand,second_hand,third_hand,forth_hand))
  }
  
  # “错误”的版本4：断开取数据的方式——从21手开始，都为顺序
  data_sort1_allsequence=function(data=data,m,path=path){
    n=nrow(data)
    # 第一手：显+顺
    {
      first_hand=matrix(ncol = nrow(path))
      for (i in seq(21,n,4)) {
        first_hand_quantity=new_quantize(path,set = data[c(seq(i-12,i-2,2),i),m])
        first_hand=rbind(first_hand,first_hand_quantity)
      }
      first_hand=first_hand[-1,]
    }
    # 第二手：显+顺
    { 
      second_hand=matrix(ncol = nrow(path) )
      for (i in seq(22,n,4)) {
        second_hand_quantity=new_quantize(path,set = data[c(seq(i-12,i-2,2),i),m])
        second_hand=rbind(second_hand,second_hand_quantity)
      }
      second_hand=second_hand[-1,]
    }
    # 第三手：显+顺
    {
      third_hand=matrix(ncol = nrow(path))
      for (i in seq(23,n,4)) {
        third_hand_quantity=new_quantize(path,set = data[c(seq(i-12,i-2,2),i),m])
        third_hand=rbind(third_hand,third_hand_quantity)
      }
      third_hand=third_hand[-1,]
    }
    # 第四手：显+顺
    { 
      forth_hand=matrix(ncol = nrow(path) )
      for (i in seq(24,n,4)) {
        forth_hand_quantity=new_quantize(path,set = data[c(seq(i-12,i-2,2),i),m])
        forth_hand=rbind(forth_hand,forth_hand_quantity)
      }
      forth_hand=forth_hand[-1,]
    }
    return(list(first_hand,second_hand,third_hand,forth_hand))
  }
  # “错误”的版本4：断开取数据的方式——从13手开始，都为顺序
  data_sort1_allsequence_thirteen=function(data=data,m,path=path){
    n=nrow(data)
    # 第一手：显+顺
    {
      first_hand=matrix(ncol = nrow(path))
      for (i in seq(13,n,4)) {
        first_hand_quantity=new_quantize(path,set = data[c(seq(i-12,i-2,2),i),m])
        first_hand=rbind(first_hand,first_hand_quantity)
      }
      first_hand=first_hand[-1,]
    }
    # 第二手：显+顺
    { 
      second_hand=matrix(ncol = nrow(path) )
      for (i in seq(14,n,4)) {
        second_hand_quantity=new_quantize(path,set = data[c(seq(i-12,i-2,2),i),m])
        second_hand=rbind(second_hand,second_hand_quantity)
      }
      second_hand=second_hand[-1,]
    }
    # 第三手：显+顺
    {
      third_hand=matrix(ncol = nrow(path))
      for (i in seq(15,n,4)) {
        third_hand_quantity=new_quantize(path,set = data[c(seq(i-12,i-2,2),i),m])
        third_hand=rbind(third_hand,third_hand_quantity)
      }
      third_hand=third_hand[-1,]
    }
    # 第四手：显+顺
    { 
      forth_hand=matrix(ncol = nrow(path) )
      for (i in seq(16,n,4)) {
        forth_hand_quantity=new_quantize(path,set = data[c(seq(i-12,i-2,2),i),m])
        forth_hand=rbind(forth_hand,forth_hand_quantity)
      }
      forth_hand=forth_hand[-1,]
    }
    return(list(first_hand,second_hand,third_hand,forth_hand))
  }
  
}
library(stringr)
run=function(data=data,path=path,data_sort,choice=1019,rolling=F){
  n_path=nrow(path)
  minus_result=matrix(nrow =length(data),ncol = n_path)
  plus=matrix(nrow =length(data),ncol = n_path)
  minus=matrix(nrow =length(data),ncol = n_path)
  new_indexA=matrix(nrow =length(data),ncol = n_path)
  new_indexB=matrix(nrow =length(data),ncol = n_path)
  new_indexA1=matrix(nrow =length(data),ncol = n_path)
  new_indexB1=matrix(nrow =length(data),ncol = n_path)
  for (m in 1:length(data)) {
    if(m%%50==0)print(m)
    if(data_sort==1){
      temp=data_sort1(data,m,path)
      
    }
    else if(data_sort==-1){
      temp=data_sort1_thirteen(data,m,path)
      
    }
    else if(data_sort==12){
      temp=data_sort1_allsequence(data,m,path)
      
    }
    else if(data_sort==-12){
      temp=data_sort1_allsequence_thirteen(data,m,path)
      
    }
    
    {
      first=temp[[1]]
      second=temp[[2]]
      third=temp[[3]]
      forth=temp[[4]]
      n_sumtemp=nrow(first)+nrow(second)+nrow(third)+nrow(forth)
      sumtemp=matrix(nrow =n_sumtemp ,ncol = nrow(path))
      patternA=matrix(nrow =n_sumtemp ,ncol = nrow(path))
      patternB=matrix(nrow =n_sumtemp ,ncol = nrow(path))
      patternA1=matrix(nrow =n_sumtemp ,ncol = nrow(path))
      patternB1=matrix(nrow =n_sumtemp ,ncol = nrow(path))
      # 计算真实数据下的前一手和当前一手是否相同，相同为s,不同为d
      real_s_or_d=matrix(nrow =n_sumtemp ,ncol = nrow(path)) # 计算新指标A1&B1时会用到1为s,-1为d
      for (i in (nrow(data)-n_sumtemp+1):length(data[,m])) {
        # 注意这里双轨时是i-2,单轨时是i-1
        real_s_or_d[i-(nrow(data)-n_sumtemp),]=ifelse(data[i,m]==data[i-2,m],1,-1)
      }
    }
      # 不同的plan:1019,1019-1,1019-2
    if (choice==1019&rolling==F) {
        second=-second
        # 判断是否是负负的，是的3&4就使用（1，8），否则（8，1）
        # formula1和formula8表示两组against的formula，1为original，8为against
        third=ifelse(first<0&second<0,third,-third)
        forth=ifelse(first<0&second<0,-forth,forth)
        # 计算新指标AB:
        patternA[seq(1,n_sumtemp,4),]=1
        patternA[seq(2,n_sumtemp,4),]=2
        patternA[seq(3,n_sumtemp,4),]=ifelse(first<0&second<0,1,2)
        patternA[seq(4,n_sumtemp,4),]=ifelse(first<0&second<0,2,1)
        
        patternB[seq(1,n_sumtemp,4),]=2
        patternB[seq(2,n_sumtemp,4),]=1
        patternB[seq(3,n_sumtemp,4),]=ifelse(first<0&second<0,2,1)
        patternB[seq(4,n_sumtemp,4),]=ifelse(first<0&second<0,1,2)
        # 4个拼接到一个
        sumtemp[seq(1,n_sumtemp,4),]=first
        sumtemp[seq(2,n_sumtemp,4),]=second
        sumtemp[seq(3,n_sumtemp,4),]=third
        sumtemp[seq(4,n_sumtemp,4),]=forth
      }
    else if(choice==1019&rolling==T){
        # 4个拼接到一个
        sumtemp[seq(1,n_sumtemp,4),]=first
        sumtemp[seq(2,n_sumtemp,4),]=second
        sumtemp[seq(3,n_sumtemp,4),]=third
        sumtemp[seq(4,n_sumtemp,4),]=forth
        sumtemp[2,]=-sumtemp[2,]
        patternA[1,]=1;patternA[2,]=2
        patternB[1,]=2;patternB[2,]=1
        # 注意相比不滚动的，需要多1个变量用以记录前一个的路径，follow的不一定是（1，8）
        # patternA 刚好记录了路径
        # 2重判断，第一判定是否follow，第2判定follow哪一个
        for (i in seq(3,nrow(sumtemp)-1,2)) {
          
          sumtemp[i,]=ifelse(sumtemp[i-1,]<0&sumtemp[i-2,]<0,ifelse(patternA[i-2,]==1,sumtemp[i,],-sumtemp[i,]),ifelse(patternA[i-2,]==1,-sumtemp[i,],sumtemp[i,]))
          patternA[i,]=ifelse(sumtemp[i-1,]<0&sumtemp[i-2,]<0,ifelse(patternA[i-2,]==1,1,2),ifelse(patternA[i-2,]==1,2,1))
          patternB[i,]=ifelse(sumtemp[i-1,]<0&sumtemp[i-2,]<0,ifelse(patternA[i-2,]==1,2,1),ifelse(patternA[i-2,]==1,1,2))
          sumtemp[i+1,]=ifelse(sumtemp[i-1,]<0&sumtemp[i-2,]<0,ifelse(patternA[i-1,]==1,sumtemp[i+1,],-sumtemp[i+1,]),ifelse(patternA[i-1,]==1,-sumtemp[i+1,],sumtemp[i+1,]))
          patternA[i+1,]=ifelse(sumtemp[i-1,]<0&sumtemp[i-2,]<0,ifelse(patternA[i-1,]==1,1,2),ifelse(patternA[i-1,]==1,2,1))
          patternB[i+1,]=ifelse(sumtemp[i-1,]<0&sumtemp[i-2,]<0,ifelse(patternA[i-1,]==1,2,1),ifelse(patternA[i-1,]==1,1,2))
        }
      }
    else if(choice==10191&rolling==F){
        second=-second
        # 判断是否是负负或正正的，是的3&4就使用（1，8），否则（8，1）
        # formula1和formula8表示两组against的formula，1为original，8为against
        third=ifelse(first*second>0,third,-third)
        forth=ifelse(first*second>0,-forth,forth)
        # 计算新指标AB:
        patternA[seq(1,n_sumtemp,4),]=1
        patternA[seq(2,n_sumtemp,4),]=2
        patternA[seq(3,n_sumtemp,4),]=ifelse(first*second>0,1,2)
        patternA[seq(4,n_sumtemp,4),]=ifelse(first*second>0,2,1)
        
        patternB[seq(1,n_sumtemp,4),]=2
        patternB[seq(2,n_sumtemp,4),]=1
        patternB[seq(3,n_sumtemp,4),]=ifelse(first*second>0,2,1)
        patternB[seq(4,n_sumtemp,4),]=ifelse(first*second>0,1,2)
        # 4个拼接到一个
        sumtemp[seq(1,n_sumtemp,4),]=first
        sumtemp[seq(2,n_sumtemp,4),]=second
        sumtemp[seq(3,n_sumtemp,4),]=third
        sumtemp[seq(4,n_sumtemp,4),]=forth
      }
    else if(choice==10191&rolling==T){
        # 4个拼接到一个
        sumtemp[seq(1,n_sumtemp,4),]=first
        sumtemp[seq(2,n_sumtemp,4),]=second
        sumtemp[seq(3,n_sumtemp,4),]=third
        sumtemp[seq(4,n_sumtemp,4),]=forth
        sumtemp[2,]=-sumtemp[2,]
        patternA[1,]=1;patternA[2,]=2
        patternB[1,]=2;patternB[2,]=1
        # 注意相比不滚动的，需要多1个变量用以记录前一个的路径，follow的不一定是（1，8）
        # patternA 刚好记录了路径
        # 2重判断，第一判定是否follow，第2判定follow哪一个
        for (i in seq(3,nrow(sumtemp)-1,2)) {
          
          sumtemp[i,]=ifelse(sumtemp[i-1,]*sumtemp[i-2,]>0,ifelse(patternA[i-2,]==1,sumtemp[i,],-sumtemp[i,]),ifelse(patternA[i-2,]==1,-sumtemp[i,],sumtemp[i,]))
          patternA[i,]=ifelse(sumtemp[i-1,]*sumtemp[i-2,]>0,ifelse(patternA[i-2,]==1,1,2),ifelse(patternA[i-2,]==1,2,1))
          patternB[i,]=ifelse(sumtemp[i-1,]*sumtemp[i-2,]>0,ifelse(patternA[i-2,]==1,2,1),ifelse(patternA[i-2,]==1,1,2))
          sumtemp[i+1,]=ifelse(sumtemp[i-1,]*sumtemp[i-2,]>0,ifelse(patternA[i-1,]==1,sumtemp[i+1,],-sumtemp[i+1,]),ifelse(patternA[i-1,]==1,-sumtemp[i+1,],sumtemp[i+1,]))
          patternA[i+1,]=ifelse(sumtemp[i-1,]*sumtemp[i-2,]>0,ifelse(patternA[i-1,]==1,1,2),ifelse(patternA[i-1,]==1,2,1))
          patternB[i+1,]=ifelse(sumtemp[i-1,]*sumtemp[i-2,]>0,ifelse(patternA[i-1,]==1,2,1),ifelse(patternA[i-1,]==1,1,2))
        }
      }
    else if(choice==10192&rolling==F){
        second=-second
        # 判断是否是正负或正正的，是的3&4就使用（1，8），否则（8，1）
        # formula1和formula8表示两组against的formula，1为original，8为against
        third=ifelse(first>0|second>0,third,-third)
        forth=ifelse(first>0|second>0,-forth,forth)
        # 计算新指标AB:
        patternA[seq(1,n_sumtemp,4),]=1
        patternA[seq(2,n_sumtemp,4),]=2
        patternA[seq(3,n_sumtemp,4),]=ifelse(first>0|second>0,1,2)
        patternA[seq(4,n_sumtemp,4),]=ifelse(first>0|second>0,2,1)
        
        patternB[seq(1,n_sumtemp,4),]=2
        patternB[seq(2,n_sumtemp,4),]=1
        patternB[seq(3,n_sumtemp,4),]=ifelse(first>0|second>0,2,1)
        patternB[seq(4,n_sumtemp,4),]=ifelse(first>0|second>0,1,2)
        # 4个拼接到一个
        sumtemp[seq(1,n_sumtemp,4),]=first
        sumtemp[seq(2,n_sumtemp,4),]=second
        sumtemp[seq(3,n_sumtemp,4),]=third
        sumtemp[seq(4,n_sumtemp,4),]=forth
      }
    else if(choice==10192&rolling==T){
        # 4个拼接到一个
        sumtemp[seq(1,n_sumtemp,4),]=first
        sumtemp[seq(2,n_sumtemp,4),]=second
        sumtemp[seq(3,n_sumtemp,4),]=third
        sumtemp[seq(4,n_sumtemp,4),]=forth
        sumtemp[2,]=-sumtemp[2,]
        patternA[1,]=1;patternA[2,]=2
        patternB[1,]=2;patternB[2,]=1
        # 注意相比不滚动的，需要多1个变量用以记录前一个的路径，follow的不一定是（1，8）
        # patternA 刚好记录了路径
        # 2重判断，第一判定是否follow，第2判定follow哪一个
        for (i in seq(3,nrow(sumtemp)-1,2)) {
          
          sumtemp[i,]=ifelse(sumtemp[i-1,]>0|sumtemp[i-2,]>0,ifelse(patternA[i-2,]==1,sumtemp[i,],-sumtemp[i,]),ifelse(patternA[i-2,]==1,-sumtemp[i,],sumtemp[i,]))
          patternA[i,]=ifelse(sumtemp[i-1,]>0|sumtemp[i-2,]>0,ifelse(patternA[i-2,]==1,1,2),ifelse(patternA[i-2,]==1,2,1))
          patternB[i,]=ifelse(sumtemp[i-1,]>0|sumtemp[i-2,]>0,ifelse(patternA[i-2,]==1,2,1),ifelse(patternA[i-2,]==1,1,2))
          sumtemp[i+1,]=ifelse(sumtemp[i-1,]>0|sumtemp[i-2,]>0,ifelse(patternA[i-1,]==1,sumtemp[i+1,],-sumtemp[i+1,]),ifelse(patternA[i-1,]==1,-sumtemp[i+1,],sumtemp[i+1,]))
          patternA[i+1,]=ifelse(sumtemp[i-1,]>0|sumtemp[i-2,]>0,ifelse(patternA[i-1,]==1,1,2),ifelse(patternA[i-1,]==1,2,1))
          patternB[i+1,]=ifelse(sumtemp[i-1,]>0|sumtemp[i-2,]>0,ifelse(patternA[i-1,]==1,2,1),ifelse(patternA[i-1,]==1,1,2))
        }
      }
    
    # 计算连续负 最大 的手数
    minus_result[m,]=detect_continue_minus(sumtemp)
    # 计算新指标:
    # 1、正负比例
    plus[m,]=apply(sumtemp, 2, function(x)sum(x>0)) 
    minus[m,]=n_sumtemp-plus[m,]# 数据m下结果负的手数
    # 2、新指标A&B
    symbol=ifelse(sumtemp>0,1,-1)
    new_indexA[m,]=colSums(patternA*symbol)
    new_indexB[m,]=colSums(patternB*symbol)
    # 3、新指标A1&B1 直接由final result和真实路径是多少就可以计算出来
    # 所以当final result为正时，预测值与实际值相同，即直接使用实际路径的s或d就是预测值与前一手的s或d
    # 当为负，说明预测错误，预测值与实际值相反，预测值与前一手的结果s或d应该与实际值与前一手的s或d相反
    pred_s_or_d=ifelse(symbol>0,real_s_or_d,-real_s_or_d) #1为s
    patternA1=ifelse(pred_s_or_d==1,2,1)
    patternB1=ifelse(pred_s_or_d==1,1,2)
    new_indexA1[m,]=colSums(patternA1*symbol)
    new_indexB1[m,]=colSums(patternB1*symbol)
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
  return(list(minus_result,result,satisfy,plus,minus,new_indexA,new_indexB,new_indexA1,new_indexB1))
}


present=function(result){
  print(result[[2]])
  cat('\n')
  final_result=rbind(round(apply(result[[6]], 2, function(x)sum(x>=0))/760,3),round(apply(result[[8]], 2, function(x)sum(x>=0))/760,3))
  final_result=rbind(final_result,round(apply(result[[7]], 2, function(x)sum(x>=0))/760,3))
  final_result=rbind(final_result,round(apply(result[[9]], 2, function(x)sum(x>=0))/760,3))
  final_result=rbind(final_result,round(colSums(result[[4]])/colSums(result[[5]]),3))
  row.names(final_result)=c('A_ratio','A1_ratio','B_ratio','B1_ratio','plus/minus')
  colnames(final_result)=c(paste0('formula',1:8))
  final_result1=rbind(result[[3]],apply(result[[6]], 2, sum))
  final_result1=rbind(final_result1,apply(result[[8]], 2, sum))
  final_result1=rbind(final_result1,apply(result[[7]], 2, sum))
  final_result1=rbind(final_result1,apply(result[[9]], 2, sum))
  final_result1=rbind(final_result1,colSums(result[[4]]))
  final_result1=rbind(final_result1,colSums(result[[5]]))
  row.names(final_result1)=c('quantity  ','A_sum','A1_sum','B_sum','B1_sum','plus','minus')
  first5_A=apply(result[[6]], 2, function(x)str_c(sort(x,decreasing = T)[1:5],collapse = ' '))
  last5_A=apply(result[[6]], 2, function(x)str_c(sort(x,decreasing = F)[1:5],collapse = ' '))
  first5_B=apply(result[[7]], 2, function(x)str_c(sort(x,decreasing = T)[1:5],collapse = ' '))
  last5_B=apply(result[[7]], 2, function(x)str_c(sort(x,decreasing = F)[1:5],collapse = ' '))
  first5_A1=apply(result[[8]], 2, function(x)str_c(sort(x,decreasing = T)[1:5],collapse = ' '))
  last5_A1=apply(result[[8]], 2, function(x)str_c(sort(x,decreasing = F)[1:5],collapse = ' '))
  first5_B1=apply(result[[9]], 2, function(x)str_c(sort(x,decreasing = T)[1:5],collapse = ' '))
  last5_B1=apply(result[[9]], 2, function(x)str_c(sort(x,decreasing = F)[1:5],collapse = ' '))
  A=str_c(first5_A,last5_A,sep  = ';')
  B=str_c(first5_B,last5_B,sep  = ';')
  A1=str_c(first5_A1,last5_A1,sep  = ';')
  B1=str_c(first5_B1,last5_B1,sep  = ';')
  ss=data.frame(A=A,A1=A1,B=B,B1=B1)
  row.names(ss)=c(paste0('formula',1:8))
  colnames(ss)=c('first&last5 of index A','first&last5 of index A1','first&last5 of index B','first&last5 of index B1')
  print(ss)
  cat('\n')
  print(final_result)
  
  print(final_result1)
}
present=function(result){
  
  final_result=rbind(round(apply(result[[6]], 2, function(x)sum(x>=0))/760,3),round(apply(result[[8]], 2, function(x)sum(x>=0))/760,3))
  final_result=rbind(final_result,round(apply(result[[7]], 2, function(x)sum(x>=0))/760,3))
  final_result=rbind(final_result,round(apply(result[[9]], 2, function(x)sum(x>=0))/760,3))
  final_result=rbind(final_result,round(colSums(result[[4]])/colSums(result[[5]]),3))
  row.names(final_result)=c('A_ratio','A1_ratio','B_ratio','B1_ratio','plus/minus')
  colnames(final_result)=c(paste0('formula',1:8))
  final_result1=rbind(result[[3]],apply(result[[6]], 2, sum))
  final_result1=rbind(final_result1,apply(result[[8]], 2, sum))
  final_result1=rbind(final_result1,apply(result[[7]], 2, sum))
  final_result1=rbind(final_result1,apply(result[[9]], 2, sum))
  final_result1=rbind(final_result1,colSums(result[[4]]))
  final_result1=rbind(final_result1,colSums(result[[5]]))
  row.names(final_result1)=c('quantity  ','A_sum','A1_sum','B_sum','B1_sum','plus','minus')
  first5_A=apply(result[[6]], 2, function(x)str_c(sort(x,decreasing = T)[1:5],collapse = ' '))
  last5_A=apply(result[[6]], 2, function(x)str_c(sort(x,decreasing = F)[1:5],collapse = ' '))
  first5_B=apply(result[[7]], 2, function(x)str_c(sort(x,decreasing = T)[1:5],collapse = ' '))
  last5_B=apply(result[[7]], 2, function(x)str_c(sort(x,decreasing = F)[1:5],collapse = ' '))
  first5_A1=apply(result[[8]], 2, function(x)str_c(sort(x,decreasing = T)[1:5],collapse = ' '))
  last5_A1=apply(result[[8]], 2, function(x)str_c(sort(x,decreasing = F)[1:5],collapse = ' '))
  first5_B1=apply(result[[9]], 2, function(x)str_c(sort(x,decreasing = T)[1:5],collapse = ' '))
  last5_B1=apply(result[[9]], 2, function(x)str_c(sort(x,decreasing = F)[1:5],collapse = ' '))
  A=str_c(first5_A,last5_A,sep  = ';')
  B=str_c(first5_B,last5_B,sep  = ';')
  A1=str_c(first5_A1,last5_A1,sep  = ';')
  B1=str_c(first5_B1,last5_B1,sep  = ';')
  ss=data.frame(A=A,A1=A1,B=B,B1=B1)
  row.names(ss)=c(paste0('formula',1:8))
  colnames(ss)=c('first&last5 of index A','first&last5 of index A1','first&last5 of index B','first&last5 of index B1')
  print(ss)
  cat('\n')
  print(final_result)
  
  print(final_result1)
}
## =============================================================
# 567R 6R不分BP  ======================================================================
# 错误版本4：分BP，从13手开始,全部逆序
plan1019_noBP_thirteen_norolling=run(data=all_data,path=path,data_sort=-1,choice = 1019,rolling=F)
present(plan1019_noBP_thirteen_norolling)
plan1019_noBP_thirteen_rolling=run(data=all_data,path=path,data_sort=-1,choice = 1019,rolling=T)
present(plan1019_noBP_thirteen_rolling)
plan10191_noBP_thirteen_norolling=run(data=all_data,path=path,data_sort=-1,choice = 10191,rolling=F)
present(plan10191_noBP_thirteen_norolling)
plan10191_noBP_thirteen_rolling=run(data=all_data,path=path,data_sort=-1,choice = 10191,rolling=T)
present(plan10191_noBP_thirteen_rolling)
plan10192_noBP_thirteen_norolling=run(data=all_data,path=path,data_sort=-1,choice = 10192,rolling=F)
present(plan10192_noBP_thirteen_norolling)
plan10192_noBP_thirteen_rolling=run(data=all_data,path=path,data_sort=-1,choice = 10192,rolling=T)
present(plan10192_noBP_thirteen_rolling)
# 全部顺序
plan1019_noBP_thirteen_norolling_allsequence=run(data=all_data,path=path,data_sort=-12,choice = 1019,rolling=F)
present(plan1019_noBP_thirteen_norolling_allsequence)
plan1019_noBP_thirteen_rolling_allsequence=run(data=all_data,path=path,data_sort=-12,choice = 1019,rolling=T)
present(plan1019_noBP_thirteen_rolling_allsequence)
plan10191_noBP_thirteen_norolling_allsequence=run(data=all_data,path=path,data_sort=-12,choice = 10191,rolling=F)
present(plan10191_noBP_thirteen_norolling_allsequence)
plan10191_noBP_thirteen_rolling_allsequence=run(data=all_data,path=path,data_sort=-12,choice = 10191,rolling=T)
present(plan10191_noBP_thirteen_rolling_allsequence)
plan10192_noBP_thirteen_norolling_allsequence=run(data=all_data,path=path,data_sort=-12,choice = 10192,rolling=F)
present(plan10192_noBP_thirteen_norolling_allsequence)
plan10192_noBP_thirteen_rolling_allsequence=run(data=all_data,path=path,data_sort=-12,choice = 10192,rolling=T)
present(plan10192_noBP_thirteen_rolling_allsequence)
 


# 567R 6R分BP======================================================================
# 错误版本4：分BP，从13手开始,全部逆序
plan1019_BP_thirteen_norolling=run(data=all_data,path=path_BP,data_sort=-1,choice = 1019,rolling=F)
present(plan1019_BP_thirteen_norolling)
plan1019_BP_thirteen_rolling=run(data=all_data,path=path_BP,data_sort=-1,choice = 1019,rolling=T)
present(plan1019_BP_thirteen_rolling)
plan10191_BP_thirteen_norolling=run(data=all_data,path=path_BP,data_sort=-1,choice = 10191,rolling=F)
present(plan10191_BP_thirteen_norolling)
plan10191_BP_thirteen_rolling=run(data=all_data,path=path_BP,data_sort=-1,choice = 10191,rolling=T)
present(plan10191_BP_thirteen_rolling)
plan10192_BP_thirteen_norolling=run(data=all_data,path=path_BP,data_sort=-1,choice = 10192,rolling=F)
present(plan10192_BP_thirteen_norolling)
plan10192_BP_thirteen_rolling=run(data=all_data,path=path_BP,data_sort=-1,choice = 10192,rolling=T)
present(plan10192_BP_thirteen_rolling)
# 全部顺序
plan1019_BP_thirteen_norolling_allsequence=run(data=all_data,path=path_BP,data_sort=-12,choice = 1019,rolling=F)
present(plan1019_BP_thirteen_norolling_allsequence)
plan1019_BP_thirteen_rolling_allsequence=run(data=all_data,path=path_BP,data_sort=-12,choice = 1019,rolling=T)
present(plan1019_BP_thirteen_rolling_allsequence)
plan10191_BP_thirteen_norolling_allsequence=run(data=all_data,path=path_BP,data_sort=-12,choice = 10191,rolling=F)
present(plan10191_BP_thirteen_norolling_allsequence)
plan10191_BP_thirteen_rolling_allsequence=run(data=all_data,path=path_BP,data_sort=-12,choice = 10191,rolling=T)
present(plan10191_BP_thirteen_rolling_allsequence)
plan10192_BP_thirteen_norolling_allsequence=run(data=all_data,path=path_BP,data_sort=-12,choice = 10192,rolling=F)
present(plan10192_BP_thirteen_norolling_allsequence)
plan10192_BP_thirteen_rolling_allsequence=run(data=all_data,path=path_BP,data_sort=-12,choice = 10192,rolling=T)
present(plan10192_BP_thirteen_rolling_allsequence)
 


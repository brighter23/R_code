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
# 取数据：
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
  
  # plan2319 错误版本——从21手
  data_sort2=function(data=data,m,path=path){
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
  # plan2319 错误版本——从13手开始
  data_sort2_thirteen=function(data=data,m,path=path){
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
  
  # plan2319 错误版本——从21手,1&3手改为顺序
  data_sort2_sequence=function(data=data,m,path=path){
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
  # plan2319 错误版本——从13手开始,1&3手改为顺序
  data_sort2_sequence_thirteen=function(data=data,m,path=path){
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
  
  # plan2319 错误版本——从21手，2&4改为顺序
  data_sort2_sequence1=function(data=data,m,path=path){
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
  # plan2319 错误版本——从13手开始，2&4改为顺序
  data_sort2_sequence1_thirteen=function(data=data,m,path=path){
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
  
  # plan2319 错误版本——从21手，都为顺序
  data_sort2_allsequence=function(data=data,m,path=path){
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
  # plan2319 错误版本——从13手开始，都为顺序
  data_sort2_allsequence_thirteen=function(data=data,m,path=path){
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
run=function(data=data,path=path,data_sort,choice=0){
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
      temp=data_sort1_thirteen(data,m,path)
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
    else if(data_sort==11){
      temp=data_sort1_sequence1(data,m,path)
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
    else if(data_sort==-11){
      temp=data_sort1_sequence1_thirteen(data,m,path)
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
    else if(data_sort==12){
      temp=data_sort1_allsequence(data,m,path)
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
    else if(data_sort==-12){
      temp=data_sort1_allsequence_thirteen(data,m,path)
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
      temp=data_sort1_sequence(data,m,path)
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
    else if(data_sort==-10){
      temp=data_sort1_sequence_thirteen(data,m,path)
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
      temp=data_sort2_thirteen(data,m,path)
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
    else if(data_sort==21){
      temp=data_sort2_sequence1(data,m,path)
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
    else if(data_sort==-21){
      temp=data_sort2_sequence1_thirteen(data,m,path)
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
    else if(data_sort==22){
      temp=data_sort2_allsequence(data,m,path)
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
    else if(data_sort==-22){
      temp=data_sort2_allsequence_thirteen(data,m,path)
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
      temp=data_sort2_sequence(data,m,path)
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
    else if(data_sort==-20){
      temp=data_sort2_sequence_thirteen(data,m,path)
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
## 一、A 全部都是逆序=============================================================
# 1.2 不分BP======================================================================
# 错误版本4：分BP，从21手开始
plan1_11_noBP=run(data=all_data,path=path,data_sort=1,choice = 11)
present(plan1_11_noBP)
plan1_13_noBP=run(data=all_data,path=path,data_sort=1,choice = 13)
present(plan1_13_noBP)
plan1_131_noBP=run(data=all_data,path=path,data_sort=1,choice = 131)
present(plan1_131_noBP)
# 错误版本4：分BP，从13手开始
plan1_11_noBP_thirteen=run(data=all_data,path=path,data_sort=-1,choice = 11)
present(plan1_11_noBP_thirteen)
plan1_13_noBP_thirteen=run(data=all_data,path=path,data_sort=-1,choice = 13)
present(plan1_13_noBP_thirteen)
plan1_131_noBP_thirteen=run(data=all_data,path=path,data_sort=-1,choice = 131)
present(plan1_131_noBP_thirteen)

# 错误plan2319:分BP，从21手开始
plan2_2_noBP=run(data=all_data,path=path,data_sort=2,choice = 2)
present(plan2_2_noBP)#plan11
plan2_noBP=run(data=all_data,path=path,data_sort=2)
present(plan2_noBP) #plan13
plan2_1_noBP=run(data=all_data,path=path,data_sort=2,choice = 1)
present(plan2_1_noBP)#plan13-1

# 错误plan2319:分BP，从13手开始
plan2_2_noBP_thirteen=run(data=all_data,path=path,data_sort=-2,choice = 2)
present(plan2_2_noBP_thirteen)#plan11
plan2_noBP_thirteen=run(data=all_data,path=path,data_sort=-2)
present(plan2_noBP_thirteen) #plan13
plan2_1_noBP_thirteen=run(data=all_data,path=path,data_sort=-2,choice = 1)
present(plan2_1_noBP_thirteen)#plan13-1


## 二、B 1&2或1&3手顺序，3&4手或2&4手逆序==========================================
# 2.2、不分BP=================================================================
# 错误版本4：6R不分BP，从21手开始
plan1_11_sequence_noBP=run(data=all_data,path=path,data_sort=10,choice = 11)
present(plan1_11_sequence_noBP) 
plan1_13_sequence_noBP=run(data=all_data,path=path,data_sort=10,choice = 13)
present(plan1_13_sequence_noBP)
plan1_131_sequence_noBP=run(data=all_data,path=path,data_sort=10,choice = 131)
present(plan1_131_sequence_noBP)
# 错误版本4：6R不分BP，从13手开始
plan1_11_sequence_noBP_thirteen=run(data=all_data,path=path,data_sort=-10,choice = 11)
present(plan1_11_sequence_noBP_thirteen)
plan1_13_sequence_noBP_thirteen=run(data=all_data,path=path,data_sort=-10,choice = 13)
present(plan1_13_sequence_noBP_thirteen)
plan1_131_sequence_noBP_thirteen=run(data=all_data,path=path,data_sort=-10,choice = 131)
present(plan1_131_sequence_noBP_thirteen)

# 错误plan2319:6R不分BP，从21手开始
plan2_2_sequence_noBP=run(data=all_data,path=path,data_sort=20,choice = 2)
present(plan2_2_sequence_noBP)#plan11
plan2_sequence_noBP=run(data=all_data,path=path,data_sort=20)
present(plan2_sequence_noBP) #plan13
plan2_1_sequence_noBP=run(data=all_data,path=path,data_sort=20,choice = 1)
present(plan2_1_sequence_noBP)#plan13-1

# 错误plan2319:6R不分BP，从13手开始
plan2_2_sequence_noBP_thirteen=run(data=all_data,path=path,data_sort=-20,choice = 2)
present(plan2_2_sequence_noBP_thirteen)#plan11
plan2_sequence_noBP_thirteen=run(data=all_data,path=path,data_sort=-20)
present(plan2_sequence_noBP_thirteen) #plan13
plan2_1_sequence_noBP_thirteen=run(data=all_data,path=path,data_sort=-20,choice = 1)
present(plan2_1_sequence_noBP_thirteen)#plan13-1


## 三、C 1&2或1&3手逆序，3&4手或2&4手顺序==========================================
# 2.2、不分BP=================================================================
# 错误版本4：6R不分BP，从21手开始
plan1_11_sequence1_noBP=run(data=all_data,path=path,data_sort=11,choice = 11)
present(plan1_11_sequence1_noBP) 
plan1_13_sequence1_noBP=run(data=all_data,path=path,data_sort=11,choice = 13)
present(plan1_13_sequence1_noBP)
plan1_131_sequence1_noBP=run(data=all_data,path=path,data_sort=11,choice = 131)
present(plan1_131_sequence1_noBP)
# 错误版本4：6R不分BP，从13手开始
plan1_11_sequence1_noBP_thirteen=run(data=all_data,path=path,data_sort=-11,choice = 11)
present(plan1_11_sequence1_noBP_thirteen)
plan1_13_sequence1_noBP_thirteen=run(data=all_data,path=path,data_sort=-11,choice = 13)
present(plan1_13_sequence1_noBP_thirteen)
plan1_131_sequence1_noBP_thirteen=run(data=all_data,path=path,data_sort=-11,choice = 131)
present(plan1_131_sequence1_noBP_thirteen)

# 错误plan2319:6R不分BP，从21手开始
plan2_2_sequence1_noBP=run(data=all_data,path=path,data_sort=21,choice = 2)
present(plan2_2_sequence1_noBP)#plan11
plan2_sequence1_noBP=run(data=all_data,path=path,data_sort=21)
present(plan2_sequence1_noBP) #plan13
plan2_1_sequence1_noBP=run(data=all_data,path=path,data_sort=21,choice = 1)
present(plan2_1_sequence1_noBP)#plan13-1

# 错误plan2319:6R不分BP，从13手开始
plan2_2_sequence1_noBP_thirteen=run(data=all_data,path=path,data_sort=-21,choice = 2)
present(plan2_2_sequence1_noBP_thirteen)#plan11
plan2_sequence1_noBP_thirteen=run(data=all_data,path=path,data_sort=-21)
present(plan2_sequence1_noBP_thirteen) #plan13
plan2_1_sequence1_noBP_thirteen=run(data=all_data,path=path,data_sort=-21,choice = 1)
present(plan2_1_sequence1_noBP_thirteen)#plan13-1


## 四、D 全部都是顺序=============================================================

# 1.2 不分BP======================================================================
# 错误版本4：分BP，从21手开始
plan1_11_noBP_allsequence=run(data=all_data,path=path,data_sort=12,choice = 11)
present(plan1_11_noBP_allsequence)
plan1_13_noBP_allsequence=run(data=all_data,path=path,data_sort=12,choice = 13)
present(plan1_13_noBP_allsequence)
plan1_131_noBP_allsequence=run(data=all_data,path=path,data_sort=12,choice = 131)
present(plan1_131_noBP_allsequence)
# 错误版本4：分BP，从13手开始
plan1_11_noBP_allsequence_thirteen=run(data=all_data,path=path,data_sort=-12,choice = 11)
present(plan1_11_noBP_allsequence_thirteen)
plan1_13_noBP_allsequence_thirteen=run(data=all_data,path=path,data_sort=-12,choice = 13)
present(plan1_13_noBP_allsequence_thirteen)
plan1_131_noBP_allsequence_thirteen=run(data=all_data,path=path,data_sort=-12,choice = 131)
present(plan1_131_noBP_allsequence_thirteen)

# 错误plan2319:分BP，从21手开始
plan2_2_noBP_allsequence=run(data=all_data,path=path,data_sort=22,choice = 2)
present(plan2_2_noBP_allsequence)#plan11
plan2_noBP_allsequence=run(data=all_data,path=path,data_sort=22)
present(plan2_noBP_allsequence) #plan13
plan2_1_noBP_allsequence=run(data=all_data,path=path,data_sort=22,choice = 1)
present(plan2_1_noBP_allsequence)#plan13-1

# 错误plan2319:分BP，从13手开始
plan2_2_noBP_allsequence_thirteen=run(data=all_data,path=path,data_sort=-22,choice = 2)
present(plan2_2_noBP_allsequence_thirteen)#plan11
plan2_noBP_allsequence_thirteen=run(data=all_data,path=path,data_sort=-22)
present(plan2_noBP_allsequence_thirteen) #plan13
plan2_1_noBP_allsequence_thirteen=run(data=all_data,path=path,data_sort=-22,choice = 1)
present(plan2_1_noBP_allsequence_thirteen)#plan13-1









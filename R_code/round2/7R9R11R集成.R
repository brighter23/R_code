# 1、formula准备------------------------------------------
data=read.csv('data\\treat_two_hundred.csv',header = T)
data1=read.csv('data\\new.csv',header = T)
data2=read.csv('data\\new1.csv',header = T)
data3=read.csv('data\\new2.csv',header = T)
data1=data1[,-23]
all_data=cbind(data,data1,data2,data3)
# 分BP的路径：
# 相对之前使用的是不太一样的formula
{
  library(stringr)
  formula=read.csv2('data\\7911formula.csv',header = F,stringsAsFactors = F)
  R7A=formula[1:10,];R7B=formula[12:21,]
  R9A=formula[23:36,];R9B=formula[38:51,]
  R11A=formula[53:70,];R11B=formula[72:89,]
  special_R11A=R11A[c(1,2,3,18)];R11A=R11A[-c(1,2,3,18)]
  special_R11B=R11B[c(1,3,18)];R11B=R11B[-c(1,3,18)]
  Formula=function(R){
    R_=str_split(R,pattern = ',',simplify = T)
    tempt=c()
    for (i in 1:6) {
      tempt=c(tempt,apply(R_[,i,drop=F], 1, function(x) paste(str_split(x,pattern  = '',simplify = T),collapse  = ':')))
    }
    R_=matrix(tempt,ncol = 6)
    R_B=apply(R_[,1:3], 1, function(x)str_c(x,collapse = '-'))
    R_P=apply(R_[,4:6], 1, function(x)str_c(x,collapse = '-'))
    return(cbind(str_c(R_B,collapse = '|'), str_c(R_P,collapse = '|')))
  }
  R7A=Formula(R7A);R7B=Formula(R7B)
  R9A=Formula(R9A);R9B=Formula(R9B)
  R11A=Formula(R11A);R11B=Formula(R11B)
  add_R11A=c('9:0-10:0-11:0|9:0-9:1-10:1|8:1-9:1-10:1|1:8-1:9-2:9','0:9-0:10-0:11|0:9-1:9-1:10|8:1-9:1-9:2|1:8-1:9-1:10')
  add_R11B=c('9:0-10:0-10:1|8:1-9:1-9:2|1:8-1:9-1:10','0:9-0:10-1:10|8:1-9:1-10:1|1:8-1:9-2:9')
  R11A[,1]=paste( R11A[,1],add_R11A[1],sep = '|');R11A[,2]=paste( R11A[,2],add_R11A[2],sep = '|')
  R11B[,1]=paste( R11B[,1],add_R11B[1],sep = '|');R11B[,2]=paste( R11B[,2],add_R11B[2],sep = '|')
  path_BP=matrix(c(R7A,R9A,R11A, R7A,R9B,R11A, R7A,R9A,R11B, R7A,R9B,R11B, R7B,R9A,R11A, R7B,R9B,R11A, R7B,R9A,R11B, R7B,R9B,R11B),byrow = T,ncol = 6)
  colnames(path_BP)=c('B_R7','P_R7','B_R9','P_R9','B_R11','P_R11')
  
}

c=c('data','all_data','path_BP')
a=ls()
rm(list=(setdiff(a,c)))
gc()
library(stringr)
# 2、训练==================================================

# 2.1定义计算每10个真实数据下的真实路径final_comb:
# 顺序时的计算方式：
true_path=function(dataset){
  t0=table(dataset[1:11])
  result0=paste(t0[1],t0[2],sep = ':')
  t1=table(dataset[1:10])
  result1=paste(t1[1],t1[2],sep = ':')
  t2=table(dataset[1:9])
  result2=paste(t2[1],t2[2],sep = ':')
  comb1=paste0(result2,'-',result1,'-',result0)
  
  t3=table(dataset[3:11])
  result3=paste(t3[1],t3[2],sep = ':')
  t4=table(dataset[3:10])
  result4=paste(t4[1],t4[2],sep = ':')
  t5=table(dataset[3:9])
  result5=paste(t5[1],t5[2],sep = ':')
  comb2=paste0(result5,'-',result4,'-',result3)
  
  t6=table(dataset[5:11])
  result6=paste(t6[1],t6[2],sep = ':')
  t7=table(dataset[5:10])
  result7=paste(t7[1],t7[2],sep = ':')
  t8=table(dataset[5:9])
  result8=paste(t8[1],t8[2],sep = ':')
  comb3=paste0(result8,'-',result7,'-',result6)
  return(matrix(c(comb1,comb2,comb3),ncol = 3))
}
# 逆序时正确的计算方式：
true_path_reverse=function(dataset){
  t0=table(dataset[c(1:10,11)])
  result0=paste(t0[1],t0[2],sep = ':')
  t1=table(dataset[c(1:10)])
  result1=paste(t1[1],t1[2],sep = ':')
  t2=table(dataset[c(1:9)])
  result2=paste(t2[1],t2[2],sep = ':')
  comb1=paste0(result2,'-',result1,'-',result0)
  
  t3=table(dataset[c(1:8,11)])
  result3=paste(t3[1],t3[2],sep = ':')
  t4=table(dataset[c(1:8)])
  result4=paste(t4[1],t4[2],sep = ':')
  t5=table(dataset[c(1:7)])
  result5=paste(t5[1],t5[2],sep = ':')
  comb2=paste0(result5,'-',result4,'-',result3)
  
  t6=table(dataset[c(1:6,11)])
  result6=paste(t6[1],t6[2],sep = ':')
  t7=table(dataset[c(1:6)])
  result7=paste(t7[1],t7[2],sep = ':')
  t8=table(dataset[c(1:5)])
  result8=paste(t8[1],t8[2],sep = ':')
  comb3=paste0(result8,'-',result7,'-',result6)
  return(matrix(c(comb1,comb2,comb3),ncol = 3))
  
}
# 2.2定义10个真实数据下根据真实路径下的量化值-1或1:
# path=path_BP为分Bp的：

# Inverse有3种形式F，1，2，代表不同的取数据计算真实路径的方式，对应true_path0~2
new_quantize=function(path,set,Inverse=F){
  # 默认是顺序的TRUE=1二者等价
  if(Inverse==T)real_comb=true_path_reverse(set)
  else if (Inverse==F)real_comb=true_path(set)
  # 分BP的path有6列：
  if(ncol(path)==6){
    r11=matrix(0,ncol = nrow(path),nrow = 1)
    for (j in 1:nrow(path)) {
      sparate_condition=ifelse(Inverse==T,set[1],set[1])
      r11[1,j]=ifelse(sparate_condition=='B'|sparate_condition==1,ifelse(str_detect(real_comb[1,1],path[j,'B_R11']),1,-1),ifelse(str_detect(real_comb[1,1],path[j,'P_R11']),1,-1))
    }
    r9=matrix(0,ncol = nrow(path),nrow = 1)
    for (j in 1:nrow(path)) {
      sparate_condition=ifelse(Inverse==T,set[1],set[3])
      r9[1,j]=ifelse(sparate_condition=='B'|sparate_condition==1,ifelse(str_detect(real_comb[1,2],path[j,'B_R9']),1,-1),ifelse(str_detect(real_comb[1,2],path[j,'P_R9']),1,-1))
    }
    r7=matrix(0,ncol = nrow(path),nrow = 1)
    for (j in 1:nrow(path)) {
      sparate_condition=ifelse(Inverse==T,set[1],set[5])
      r7[1,j]=ifelse(sparate_condition=='B'|sparate_condition==1,ifelse(str_detect(real_comb[1,3],path[j,'B_R7']),1,-1),ifelse(str_detect(real_comb[1,3],path[j,'P_R7']),1,-1))
    }
  }
  else if(ncol(path)==3){
    r11=matrix(0,ncol = nrow(path),nrow = 1)
    for (j in 1:nrow(path)) {
      r11[1,j]=ifelse(str_detect(real_comb[1,1],path[j,'R11']),1,-1)
    }
    r9=matrix(0,ncol = nrow(path),nrow = 1)
    for (j in 1:nrow(path)) {
      r9[1,j]=ifelse(str_detect(real_comb[1,2],path[j,'R9']),1,-1)
    }
    r7=matrix(0,ncol = nrow(path),nrow = 1)
    for (j in 1:nrow(path)) {
      r7[1,j]=ifelse(str_detect(real_comb[1,3],path[j,'R7']),1,-1)
    }
  }
  return(r11+r9+r7)
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
  # “错误”的版本4：断开取数据的方式——计算真实路径和决定分BP的点使用顺序的方式
  data_sort1=function(data=data,m,path=path){
    n=nrow(data)
    reflaction=ifelse(data[,m]=="B",'P','B')
    # 第一手：显+逆
    {
      first_hand=matrix(ncol = nrow(path))
      for (i in seq(21,n,4)) {
        first_hand_quantity=new_quantize(path,set = data[c(seq(i-2,i-20,-2),i),m])
        first_hand=rbind(first_hand,first_hand_quantity)
      }
      first_hand=first_hand[-1,]
    }
    # 第二手：显+逆
    { 
      second_hand=matrix(ncol = nrow(path) )
      for (i in seq(22,n,4)) {
        second_hand_quantity=new_quantize(path,set = data[c(seq(i-2,i-20,-2),i),m])
        second_hand=rbind(second_hand,second_hand_quantity)
      }
      second_hand=second_hand[-1,]
    }
    # 第三手：reflection+逆
    {
      third_hand=matrix(ncol = nrow(path))
      for (i in seq(23,n,4)) {
        rejection=factor(c(reflaction[seq((i-2),(i-20),-2)],as.character(data[i,m])),levels = c('B',"P"))
        third_hand_quantity=new_quantize(path,set = rejection)
        third_hand=rbind(third_hand,third_hand_quantity)
      }
      third_hand=third_hand[-1,]
    }
    # 第四手：reflection+逆
    { 
      forth_hand=matrix(ncol = nrow(path) )
      for (i in seq(24,n,4)) {
        rejection=factor(c(reflaction[seq((i-2),(i-20),-2)],as.character(data[i,m])),levels = c('B',"P"))
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
        first_hand_quantity=new_quantize(path,set = data[c(seq(i-2,i-20,-2),i),m],Inverse = T)
        first_hand=rbind(first_hand,first_hand_quantity)
      }
      first_hand=first_hand[-1,]
    }
    # 第二手：显+逆
    { 
      second_hand=matrix(ncol = nrow(path) )
      for (i in seq(22,n,4)) {
        second_hand_quantity=new_quantize(path,set = data[c(seq(i-2,i-20,-2),i),m],Inverse = T)
        second_hand=rbind(second_hand,second_hand_quantity)
      }
      second_hand=second_hand[-1,]
    }
    # 第三手：reflection+逆
    {
      third_hand=matrix(ncol = nrow(path))
      for (i in seq(23,n,4)) {
        rejection=factor(c(reflaction[seq((i-2),(i-20),-2)],as.character(data[i,m])),levels = c('B',"P"))
        third_hand_quantity=new_quantize(path,set = rejection,Inverse = T)
        third_hand=rbind(third_hand,third_hand_quantity)
      }
      third_hand=third_hand[-1,]
    }
    # 第四手：reflection+逆
    { 
      forth_hand=matrix(ncol = nrow(path) )
      for (i in seq(24,n,4)) {
        rejection=factor(c(reflaction[seq((i-2),(i-20),-2)],as.character(data[i,m])),levels = c('B',"P"))
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
        first_hand_quantity=new_quantize(path,set = data[c(seq(i-2,i-20,-2),i),m])
        first_hand=rbind(first_hand,first_hand_quantity)
      }
      first_hand=first_hand[-1,]
    }
    # 第二手：re+逆
    { 
      second_hand=matrix(ncol = nrow(path) )
      for (i in seq(22,n,4)) {
        rejection=factor(c(reflaction[seq((i-2),(i-20),-2)],as.character(data[i,m])),levels = c('B',"P"))
        second_hand_quantity=new_quantize(path,set =rejection )
        second_hand=rbind(second_hand,second_hand_quantity)
      }
      second_hand=second_hand[-1,]
    }
    # 第三手：显+逆
    {
      third_hand=matrix(ncol = nrow(path))
      for (i in seq(23,n,4)) {
        third_hand_quantity=new_quantize(path,set = data[c(seq(i-2,i-20,-2),i),m])
        third_hand=rbind(third_hand,third_hand_quantity)
      }
      third_hand=third_hand[-1,]
    }
    # 第四手：reflection+逆
    { 
      forth_hand=matrix(ncol = nrow(path) )
      for (i in seq(24,n,4)) {
        rejection=factor(c(reflaction[seq((i-2),(i-20),-2)],as.character(data[i,m])),levels = c('B',"P"))
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
        first_hand_quantity=new_quantize(path,set = data[c(seq(i-2,i-20,-2),i),m],Inverse = T)
        first_hand=rbind(first_hand,first_hand_quantity)
      }
      first_hand=first_hand[-1,]
    }
    # 第二手：re+逆
    { 
      second_hand=matrix(ncol = nrow(path) )
      for (i in seq(22,n,4)) {
        rejection=factor(c(reflaction[seq((i-2),(i-20),-2)],as.character(data[i,m])),levels = c('B',"P"))
        second_hand_quantity=new_quantize(path,set =rejection,Inverse = T )
        second_hand=rbind(second_hand,second_hand_quantity)
      }
      second_hand=second_hand[-1,]
    }
    # 第三手：显+逆
    {
      third_hand=matrix(ncol = nrow(path))
      for (i in seq(23,n,4)) {
        third_hand_quantity=new_quantize(path,set = data[c(seq(i-2,i-20,-2),i),m],Inverse = T)
        third_hand=rbind(third_hand,third_hand_quantity)
      }
      third_hand=third_hand[-1,]
    }
    # 第四手：reflection+逆
    { 
      forth_hand=matrix(ncol = nrow(path) )
      for (i in seq(24,n,4)) {
        rejection=factor(c(reflaction[seq((i-2),(i-20),-2)],as.character(data[i,m])),levels = c('B',"P"))
        forth_hand_quantity=new_quantize(path,set = rejection,Inverse = T)
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
    if(data_sort==1){
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
        k=1
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
    if(data_sort==10){
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
        k=1
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
# 错误版本4： 不分BP
plan1_11_=run(data=all_data,path=path,data_sort=1,choice = 11)
present(plan1_11_)
plan1_13_=run(data=all_data,path=path,data_sort=1,choice = 13)
present(plan1_13_)
plan1_131_=run(data=all_data,path=path,data_sort=1,choice = 131)
present(plan1_131_)
# 错误版本4：分BP
plan1_11_BP=run(data=all_data,path=path_BP,data_sort=1,choice = 11)
present(plan1_11_BP)
plan1_13_BP=run(data=all_data,path=path_BP,data_sort=1,choice = 13)
present(plan1_13_BP)
plan1_131_BP=run(data=all_data,path=path_BP,data_sort=1,choice = 131)
present(plan1_131_BP)
# 正确版本4：分BP
plan10_11_BP=run(data=all_data,path=path_BP,data_sort=10,choice = 11)
present(plan10_11_BP)
plan10_13_BP=run(data=all_data,path=path_BP,data_sort=10,choice = 13)
present(plan10_13_BP)
plan10_131_BP=run(data=all_data,path=path_BP,data_sort=10,choice = 131)
present(plan10_131_BP)
# plan2319:分BP
plan2_BP=run(data=all_data,path=path_BP,data_sort=2)
present(plan2_BP) #plan13
plan2_1_BP=run(data=all_data,path=path_BP,data_sort=2,choice = 1)
present(plan2_1_BP)#plan13-1
plan2_2_BP=run(data=all_data,path=path_BP,data_sort=2,choice = 2)
present(plan2_2_BP)#plan11
# 正确plan2319:分BP
plan20_BP=run(data=all_data,path=path_BP,data_sort=20)
present(plan20_BP) #plan13
plan20_1_BP=run(data=all_data,path=path_BP,data_sort=20,choice = 1)
present(plan20_1_BP)#plan13-1
plan20_2_BP=run(data=all_data,path=path_BP,data_sort=20,choice = 2)
present(plan20_2_BP)#plan11
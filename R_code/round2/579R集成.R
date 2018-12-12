# 1、formula准备------------------------------------------
data=read.csv('data\\treat_two_hundred.csv',header = T)
data1=read.csv('data\\new.csv',header = T)
data2=read.csv('data\\new1.csv',header = T)
data3=read.csv('data\\new2.csv',header = T)
data1=data1[,-23]
all_data=cbind(data,data1,data2,data3)
# 分BP的路径：
{
  A91='70-80-81|70-71-72|61-71-72|61-62-63|52-53-54|52-62-63|43-44-45|43-53-54|34-44-45|34-35-36|25-35-36|25-26-27|16-17-18|16-26-27'
  A92='07-08-18|07-17-27|16-17-27|16-26-36|25-35-45|25-26-36|34-44-54|34-35-45|43-44-54|43-53-63|52-53-63|52-62-72|61-71-81|61-62-72'
  A9=cbind(A91,A92)
  B91='70-80-90|70-71-81|61-71-81|61-62-72|52-53-63|52-62-72|43-44-54|43-53-63|34-44-54|34-35-45|25-35-45|25-26-36|16-17-27|16-26-36'
  B92='07-08-09|07-17-18|16-17-18|16-26-27|25-35-36|25-26-27|34-44-45|34-35-36|43-44-45|43-53-54|52-53-54|52-62-63|61-71-72|61-62-63'
  B9=cbind(B91,B92)  
  A71='50-60-70|50-51-61|41-42-52|41-51-61|32-33-43|32-42-52|23-33-43|23-24-34|14-24-34|14-15-25'
  A72='05-06-07|05-15-16|14-24-25|14-15-16|23-33-34|23-24-25|32-33-34|32-42-43|41-42-43|41-51-52'
  A7=cbind(A71,A72)
  B71='50-60-61|50-51-52|41-42-43|41-51-52|32-33-34|32-42-43|23-33-34|23-24-25|14-24-25|14-15-16'
  B72='05-06-16|05-15-25|14-24-34|14-15-25|23-33-43|23-24-34|32-33-43|32-42-52|41-42-52|41-51-61'
  B7=cbind(B71,B72)
  A51='30-40-41|30-31-32|21-31-32|21-22-23|12-22-23|12-13-14'
  A52='03-04-14|03-13-23|12-13-23|12-22-32|21-22-32|21-31-41'
  A5=cbind(A51,A52)
  # A52是对A51的完全相反
  B51='30-40-50|30-31-41|21-31-41|21-22-32|12-22-32|12-13-23'
  # B51是对A51的第三项相反（反一反）
  B52='03-04-05|03-13-14|12-13-14|12-22-23|21-22-23|21-31-32'
  # B52是对B51的完全相反，也是对A52的第三项相反（反一反）
  B5=cbind(B51,B52)
  path_BP=matrix(c(A9,A7,A5, A9,B7,A5, A9,A7,B5, A9,B7,B5, B9,A7,A5, B9,B7,A5, B9,A7,B5, B9,B7,B5),byrow = T,ncol = 6)
  colnames(path_BP)=c('B_R9','P_R9','B_R7','P_R7','B_R5','P_R5')
  
}

c=c('data','all_data','path_BP','path')
a=ls()
rm(list=(setdiff(a,c)))
gc()
library(stringr)
# 2、训练==================================================

# 2.1定义计算每10个真实数据下的真实路径final_comb:
true_path0=function(dataset){
  t0=table(dataset[1:9])
  result0=paste0(t0[1],t0[2])
  t1=table(dataset[1:8])
  result1=paste0(t1[1],t1[2])
  t2=table(dataset[1:7])
  result2=paste0(t2[1],t2[2])
  comb1=paste0(result2,'-',result1,'-',result0)
  
  t3=table(dataset[3:9])
  result3=paste0(t3[1],t3[2])
  t4=table(dataset[3:8])
  result4=paste0(t4[1],t4[2])
  t5=table(dataset[3:7])
  result5=paste0(t5[1],t5[2])
  comb2=paste0(result5,'-',result4,'-',result3)
  
  t6=table(dataset[5:9])
  result6=paste0(t6[1],t6[2])
  t7=table(dataset[5:8])
  result7=paste0(t7[1],t7[2])
  t8=table(dataset[5:7])
  result8=paste0(t8[1],t8[2])
  comb3=paste0(result8,'-',result7,'-',result6)
  return(matrix(c(comb1,comb2,comb3),ncol = 3))
  
}
true_path1=function(dataset){
  t0=table(dataset[c(1:8,9)])
  result0=paste0(t0[1],t0[2])
  t1=table(dataset[c(1:8)])
  result1=paste0(t1[1],t1[2])
  t2=table(dataset[c(1:7)])
  result2=paste0(t2[1],t2[2])
  comb1=paste0(result2,'-',result1,'-',result0)
  
  t3=table(dataset[c(1:6,9)])
  result3=paste0(t3[1],t3[2])
  t4=table(dataset[c(1:6)])
  result4=paste0(t4[1],t4[2])
  t5=table(dataset[c(1:5)])
  result5=paste0(t5[1],t5[2])
  comb2=paste0(result5,'-',result4,'-',result3)
  
  t6=table(dataset[c(1:4,9)])
  result6=paste0(t6[1],t6[2])
  t7=table(dataset[c(1:4)])
  result7=paste0(t7[1],t7[2])
  t8=table(dataset[c(1:3)])
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
    r9=matrix(0,ncol = nrow(path),nrow = 1)
    for (j in 1:nrow(path)) {
      sparate_condition=ifelse(Inverse==T,set[1],set[1])
      r9[1,j]=ifelse(sparate_condition=='B'|sparate_condition==1,ifelse(str_detect(real_comb[1,1],path[j,'B_R9']),1,-1),ifelse(str_detect(real_comb[1,1],path[j,'P_R9']),1,-1))
    }
    r7=matrix(0,ncol = nrow(path),nrow = 1)
    for (j in 1:nrow(path)) {
      sparate_condition=ifelse(Inverse==T,set[1],set[3])
      r7[1,j]=ifelse(sparate_condition=='B'|sparate_condition==1,ifelse(str_detect(real_comb[1,2],path[j,'B_R7']),1,-1),ifelse(str_detect(real_comb[1,2],path[j,'P_R7']),1,-1))
    }
    r5=matrix(0,ncol = nrow(path),nrow = 1)
    for (j in 1:nrow(path)) {
      sparate_condition=ifelse(Inverse==T,set[1],set[5])
      r5[1,j]=ifelse(sparate_condition=='B'|sparate_condition==1,ifelse(str_detect(real_comb[1,3],path[j,'B_R5']),1,-1),ifelse(str_detect(real_comb[1,3],path[j,'P_R5']),1,-1))
    }
  }
  else if(ncol(path)==3){
    r9=matrix(0,ncol = nrow(path),nrow = 1)
    for (j in 1:nrow(path)) {
      r9[1,j]=ifelse(str_detect(real_comb[1,1],path[j,'R9']),1,-1)
    }
    r7=matrix(0,ncol = nrow(path),nrow = 1)
    for (j in 1:nrow(path)) {
      r7[1,j]=ifelse(str_detect(real_comb[1,2],path[j,'R7']),1,-1)
    }
    r5=matrix(0,ncol = nrow(path),nrow = 1)
    for (j in 1:nrow(path)) {
      r5[1,j]=ifelse(str_detect(real_comb[1,3],path[j,'R5']),1,-1)
    }
  }
  return(r9+r7+r5)
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
  
  # “错误”的版本4：断开取数据的方式
  data_sort1=function(data=data,m,path=path){
    n=nrow(data)
    reflaction=ifelse(data[,m]=="B",'P','B')
    # 第一手：显+逆
    {
      first_hand=matrix(ncol = nrow(path))
      for (i in seq(21,n,4)) {
        first_hand_quantity=new_quantize(path,set = data[c(seq(i-2,i-16,-2),i),m])
        first_hand=rbind(first_hand,first_hand_quantity)
      }
      first_hand=first_hand[-1,]
    }
    # 第二手：显+逆
    { 
      second_hand=matrix(ncol = nrow(path) )
      for (i in seq(22,n,4)) {
        second_hand_quantity=new_quantize(path,set = data[c(seq(i-2,i-16,-2),i),m])
        second_hand=rbind(second_hand,second_hand_quantity)
      }
      second_hand=second_hand[-1,]
    }
    # 第三手：reflection+逆
    {
      third_hand=matrix(ncol = nrow(path))
      for (i in seq(23,n,4)) {
        rejection=factor(c(reflaction[seq((i-2),(i-16),-2)],as.character(data[i,m])),levels = c('B',"P"))
        third_hand_quantity=new_quantize(path,set = rejection)
        third_hand=rbind(third_hand,third_hand_quantity)
      }
      third_hand=third_hand[-1,]
    }
    # 第四手：reflection+逆
    { 
      forth_hand=matrix(ncol = nrow(path) )
      for (i in seq(24,n,4)) {
        rejection=factor(c(reflaction[seq((i-2),(i-16),-2)],as.character(data[i,m])),levels = c('B',"P"))
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
        first_hand_quantity=new_quantize(path,set = data[c(seq(i-2,i-16,-2),i),m],Inverse = T)
        first_hand=rbind(first_hand,first_hand_quantity)
      }
      first_hand=first_hand[-1,]
    }
    # 第二手：显+逆
    { 
      second_hand=matrix(ncol = nrow(path) )
      for (i in seq(22,n,4)) {
        second_hand_quantity=new_quantize(path,set = data[c(seq(i-2,i-16,-2),i),m],Inverse = T)
        second_hand=rbind(second_hand,second_hand_quantity)
      }
      second_hand=second_hand[-1,]
    }
    # 第三手：reflection+逆
    {
      third_hand=matrix(ncol = nrow(path))
      for (i in seq(23,n,4)) {
        rejection=factor(c(reflaction[seq((i-2),(i-16),-2)],as.character(data[i,m])),levels = c('B',"P"))
        third_hand_quantity=new_quantize(path,set = rejection,Inverse = T)
        third_hand=rbind(third_hand,third_hand_quantity)
      }
      third_hand=third_hand[-1,]
    }
    # 第四手：reflection+逆
    { 
      forth_hand=matrix(ncol = nrow(path) )
      for (i in seq(24,n,4)) {
        rejection=factor(c(reflaction[seq((i-2),(i-16),-2)],as.character(data[i,m])),levels = c('B',"P"))
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
        first_hand_quantity=new_quantize(path,set = data[c(seq(i-2,i-16,-2),i),m])
        first_hand=rbind(first_hand,first_hand_quantity)
      }
      first_hand=first_hand[-1,]
    }
    # 第二手：re+逆
    { 
      second_hand=matrix(ncol = nrow(path) )
      for (i in seq(22,n,4)) {
        rejection=factor(c(reflaction[seq((i-2),(i-16),-2)],as.character(data[i,m])),levels = c('B',"P"))
        second_hand_quantity=new_quantize(path,set =rejection )
        second_hand=rbind(second_hand,second_hand_quantity)
      }
      second_hand=second_hand[-1,]
    }
    # 第三手：显+逆
    {
      third_hand=matrix(ncol = nrow(path))
      for (i in seq(23,n,4)) {
        third_hand_quantity=new_quantize(path,set = data[c(seq(i-2,i-16,-2),i),m])
        third_hand=rbind(third_hand,third_hand_quantity)
      }
      third_hand=third_hand[-1,]
    }
    # 第四手：reflection+逆
    { 
      forth_hand=matrix(ncol = nrow(path) )
      for (i in seq(24,n,4)) {
        rejection=factor(c(reflaction[seq((i-2),(i-16),-2)],as.character(data[i,m])),levels = c('B',"P"))
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
        first_hand_quantity=new_quantize(path,set = data[c(seq(i-2,i-16,-2),i),m],Inverse = T)
        first_hand=rbind(first_hand,first_hand_quantity)
      }
      first_hand=first_hand[-1,]
    }
    # 第二手：re+逆
    { 
      second_hand=matrix(ncol = nrow(path) )
      for (i in seq(22,n,4)) {
        rejection=factor(c(reflaction[seq((i-2),(i-16),-2)],as.character(data[i,m])),levels = c('B',"P"))
        second_hand_quantity=new_quantize(path,set =rejection,Inverse = T )
        second_hand=rbind(second_hand,second_hand_quantity)
      }
      second_hand=second_hand[-1,]
    }
    # 第三手：显+逆
    {
      third_hand=matrix(ncol = nrow(path))
      for (i in seq(23,n,4)) {
        third_hand_quantity=new_quantize(path,set = data[c(seq(i-2,i-16,-2),i),m],Inverse = T)
        third_hand=rbind(third_hand,third_hand_quantity)
      }
      third_hand=third_hand[-1,]
    }
    # 第四手：reflection+逆
    { 
      forth_hand=matrix(ncol = nrow(path) )
      for (i in seq(24,n,4)) {
        rejection=factor(c(reflaction[seq((i-2),(i-16),-2)],as.character(data[i,m])),levels = c('B',"P"))
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
        first_hand_quantity=new_quantize(path,set = data[c(seq(i-2,i-16,-2),i),m])
        first_hand=rbind(first_hand,first_hand_quantity)
      }
      first_hand=first_hand[-1,]
    }
    # 第二手：re+逆
    { 
      second_hand=matrix(ncol = nrow(path) )
      for (i in seq(22,n,4)) {
        rejection=factor(c(reflaction[seq((i-2),(i-16),-2)],as.character(data[i,m])),levels = c('B',"P"))
        second_hand_quantity=new_quantize(path,set =rejection )
        second_hand=rbind(second_hand,second_hand_quantity)
      }
      second_hand=second_hand[-1,]
    }
    # 第三手：re+逆
    {
      third_hand=matrix(ncol = nrow(path))
      for (i in seq(23,n,4)) {
        rejection=factor(c(reflaction[seq((i-2),(i-16),-2)],as.character(data[i,m])),levels = c('B',"P"))
        third_hand_quantity=new_quantize(path,set = rejection)
        third_hand=rbind(third_hand,third_hand_quantity)
      }
      third_hand=third_hand[-1,]
    }
    # 第四手：显+逆
    { 
      forth_hand=matrix(ncol = nrow(path) )
      for (i in seq(24,n,4)) {
        forth_hand_quantity=new_quantize(path,set = data[c(seq(i-2,i-16,-2),i),m])
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
###=============================================================
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
# 错误plan2319:分BP
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
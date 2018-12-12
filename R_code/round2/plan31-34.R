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
true_path=function(dataset){
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
# 2.2定义10个真实数据下根据真实路径下的量化值-1或1:
# 分BP：
new_quantize_BP=function(path,real_comb,set){
  r10=matrix(0,ncol = nrow(path),nrow = 1)
  for (j in 1:nrow(path)) {
    r10[1,j]=ifelse(set[1]=='B',ifelse(str_detect(real_comb[1,1],path[j,'B_R10']),1,-1),ifelse(str_detect(real_comb[1,1],path[j,'P_R10']),1,-1))
  }
  r8=matrix(0,ncol = nrow(path),nrow = 1)
  for (j in 1:nrow(path)) {
    r8[1,j]=ifelse(set[3]=='B',ifelse(str_detect(real_comb[1,2],path[j,'B_R8']),1,-1),ifelse(str_detect(real_comb[1,2],path[j,'P_R8']),1,-1))
  }
  r6=matrix(0,ncol = nrow(path),nrow = 1)
  for (j in 1:nrow(path)) {
    r6[1,j]=ifelse(set[5]=='B',ifelse(str_detect(real_comb[1,3],path[j,'B_R6']),1,-1),ifelse(str_detect(real_comb[1,3],path[j,'P_R6']),1,-1))
  }
  return(r10+r8+r6)
} 
# 不分BP的：
new_quantize_mix=function(path,real_comb,set){
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
  return(r10+r8+r6)
}
# 2.3 定义取数据的方式：
{
  # plan31:1手为正常，2为倒序
  data_sort1=function(data=data,m,path=path){
    n=nrow(data)
    # 第一手
    {
      first_hand=matrix(ncol = nrow(path))
      for (i in seq(10,n,2)) {
        real_comb=true_path(data[(i-9):i,m])
        first_hand_quantity=new_quantize(path,real_comb,set = data[(i-9):i,m])
        first_hand=rbind(first_hand,first_hand_quantity)
      }
      first_hand=first_hand[-1,]
    }
    # 第二手
    {
      second_hand=matrix(ncol = nrow(path) )
      for (i in seq(11,n,2)) {
        real_comb=true_path(data[c((i-1):(i-9),i),m])
        second_hand_quantity=new_quantize(path,real_comb,set=data[c((i-1):(i-9),i),m])
        second_hand=rbind(second_hand,second_hand_quantity)
      }
      second_hand=second_hand[-1,]
    }
    return(list(first_hand,second_hand))
  }
  # plan32:1、2正常，3、4倒序
  data_sort2=function(data=data,m,path=path){
    n=nrow(data)
    # 第一手
    {
      first_hand=matrix(ncol = nrow(path))
      for (i in seq(21,n,4)) {
        real_comb=true_path(data[seq(i-18,i,2),m])
        first_hand_quantity=new_quantize(path,real_comb,set = data[seq(i-18,i,2),m])
        first_hand=rbind(first_hand,first_hand_quantity)
      }
      first_hand=first_hand[-1,]
      
    }
    # 第二手
    { 
      second_hand=matrix(ncol = nrow(path) )
      for (i in seq(22,n,4)) {
        real_comb=true_path(data[seq(i-18,i,2),m])
        second_hand_quantity=new_quantize(path,real_comb,set = data[seq(i-18,i,2),m])
        second_hand=rbind(second_hand,second_hand_quantity)
      }
      second_hand=second_hand[-1,]
      
    }
    # 第三手
    {
      third_hand=matrix(ncol = nrow(path))
      for (i in seq(23,n,4)) {
        real_comb=true_path(data[c(seq(i-2,i-18,-2),i),m])
        third_hand_quantity=new_quantize(path,real_comb,set = data[c(seq(i-2,i-18,-2),i),m])
        third_hand=rbind(third_hand,third_hand_quantity)
      }
      third_hand=third_hand[-1,]
      
    }
    # 第四手
    { 
      forth_hand=matrix(ncol = nrow(path) )
      for (i in seq(24,n,4)) {
        real_comb=true_path(data[c(seq(i-2,i-18,-2),i),m])
        forth_hand_quantity=new_quantize(path,real_comb,set = data[c(seq(i-2,i-18,-2),i),m])
        forth_hand=rbind(forth_hand,forth_hand_quantity)
      }
      forth_hand=forth_hand[-1,]
    }
    return(list(first_hand,second_hand,third_hand,forth_hand))
  }
  # plan33：1、2均为倒序
  data_sort3=function(data=data,m,path=path){
    n=nrow(data)
    # 第一手
    {
      first_hand=matrix(ncol = nrow(path))
      for (i in seq(10,n,2)) {
        real_comb=true_path(data[c((i-1):(i-9),i),m])
        first_hand_quantity=new_quantize(path,real_comb,set = data[c((i-1):(i-9),i),m])
        first_hand=rbind(first_hand,first_hand_quantity)
      }
      first_hand=first_hand[-1,]
    }
    # 第二手
    {
      second_hand=matrix(ncol = nrow(path) )
      for (i in seq(11,n,2)) {
        real_comb=true_path(data[c((i-1):(i-9),i),m])
        second_hand_quantity=new_quantize(path,real_comb,set=data[c((i-1):(i-9),i),m])
        second_hand=rbind(second_hand,second_hand_quantity)
      }
      second_hand=second_hand[-1,]
    }
    return(list(first_hand,second_hand))
  }
  # plan34:
  data_sort4=function(data=data,m,path=path){
    n=nrow(data)
    # 第一手
    {
      first_hand=matrix(ncol = nrow(path))
      for (i in seq(21,n,4)) {
        real_comb=true_path(data[c(seq(i-2,i-18,-2),i),m])
        first_hand_quantity=new_quantize(path,real_comb,set = data[c(seq(i-2,i-18,-2),i),m])
        first_hand=rbind(first_hand,first_hand_quantity)
      }
      first_hand=first_hand[-1,]
      
    }
    # 第二手
    { 
      second_hand=matrix(ncol = nrow(path) )
      for (i in seq(22,n,4)) {
        real_comb=true_path(data[c(seq(i-2,i-18,-2),i),m])
        second_hand_quantity=new_quantize(path,real_comb,set = data[c(seq(i-2,i-18,-2),i),m])
        second_hand=rbind(second_hand,second_hand_quantity)
      }
      second_hand=second_hand[-1,]
    }
    # 第三手
    {
      third_hand=matrix(ncol = nrow(path))
      for (i in seq(23,n,4)) {
        real_comb=true_path(data[c(seq(i-2,i-18,-2),i),m])
        third_hand_quantity=new_quantize(path,real_comb,set = data[c(seq(i-2,i-18,-2),i),m])
        third_hand=rbind(third_hand,third_hand_quantity)
      }
      third_hand=third_hand[-1,]
    }
    # 第四手
    { 
      forth_hand=matrix(ncol = nrow(path) )
      for (i in seq(24,n,4)) {
        real_comb=true_path(data[c(seq(i-2,i-18,-2),i),m])
        forth_hand_quantity=new_quantize(path,real_comb,set = data[c(seq(i-2,i-18,-2),i),m])
        forth_hand=rbind(forth_hand,forth_hand_quantity)
      }
      forth_hand=forth_hand[-1,]
    }
    return(list(first_hand,second_hand,third_hand,forth_hand))
  }
  # plan35:1、3正常，2、4倒序
  data_sort5=function(data=data,m,path=path){
    n=nrow(data)
    # 第一手
    {
      first_hand=matrix(ncol = nrow(path))
      for (i in seq(21,n,4)) {
        real_comb=true_path(data[seq(i-18,i,2),m])
        first_hand_quantity=new_quantize(path,real_comb,set = data[seq(i-18,i,2),m])
        first_hand=rbind(first_hand,first_hand_quantity)
      }
      first_hand=first_hand[-1,]
      
    }
    # 第二手
    { 
      second_hand=matrix(ncol = nrow(path) )
      for (i in seq(22,n,4)) {
        real_comb=true_path(data[c(seq(i-2,i-18,-2),i),m])
        second_hand_quantity=new_quantize(path,real_comb,set = data[c(seq(i-2,i-18,-2),i),m])
        second_hand=rbind(second_hand,second_hand_quantity)
      }
      second_hand=second_hand[-1,]
      
    }
    # 第三手
    {
      third_hand=matrix(ncol = nrow(path))
      for (i in seq(23,n,4)) {
        real_comb=true_path(data[seq(i-18,i,2),m])
        third_hand_quantity=new_quantize(path,real_comb,set = data[seq(i-18,i,2),m])
        third_hand=rbind(third_hand,third_hand_quantity)
      }
      third_hand=third_hand[-1,]
      
    }
    # 第四手
    { 
      forth_hand=matrix(ncol = nrow(path) )
      for (i in seq(24,n,4)) {
        real_comb=true_path(data[c(seq(i-2,i-18,-2),i),m])
        forth_hand_quantity=new_quantize(path,real_comb,set = data[c(seq(i-2,i-18,-2),i),m])
        forth_hand=rbind(forth_hand,forth_hand_quantity)
      }
      forth_hand=forth_hand[-1,]
    }
    return(list(first_hand,second_hand,third_hand,forth_hand))
  }
  # plan36:
  data_sort6=function(data=data,m,path=path){
    n=nrow(data)
    # 第一手
    {
      first_hand=matrix(ncol = nrow(path))
      for (i in seq(21,n,4)) {
        real_comb=true_path(data[seq(i-18,i,2),m])
        first_hand_quantity=new_quantize(path,real_comb)
        first_hand=rbind(first_hand,first_hand_quantity)
      }
      first_hand=first_hand[-1,]
      
    }
    # 第二手
    { 
      sequance1_n=length(seq(6,n,4))*2
      sequance1=matrix(nrow =sequance1_n )
      sequance1[seq(1,sequance1_n,2),]=seq(5,n,4)
      sequance1[seq(2,sequance1_n,2),]=seq(6,n,4)
      
      second_hand=matrix(ncol = nrow(path) )
      for (i in seq(22,n,4)) {
        index=which(sequance1==i)
        real_comb=true_path(data[sequance1[(index-9):index],m])
        second_hand_quantity=new_quantize(path,real_comb)
        second_hand=rbind(second_hand,second_hand_quantity)
      }
      second_hand=second_hand[-1,]
      
    }
    # 第三手
    {
      third_hand=matrix(ncol = nrow(path) )
      for (i in seq(23,n,4)) {
        real_comb=true_path(data[(i-9):i,m])
        third_hand_quantity=new_quantize(path,real_comb)
        third_hand=rbind(third_hand,third_hand_quantity)
      }
      third_hand=third_hand[-1,]
      
    }
    # 第四手
    { 
      forth_hand=matrix(ncol = nrow(path) )
      for (i in seq(24,n,4)) {
        real_comb=true_path(data[c(seq(i-2,i-18,-2),i),m])
        forth_hand_quantity=new_quantize(path,real_comb)
        forth_hand=rbind(forth_hand,forth_hand_quantity)
      }
      forth_hand=forth_hand[-1,]
      
    }
    return(list(first_hand,second_hand,third_hand,forth_hand))
  }
  data_sort7=function(data=data,m,path=path){
    n=nrow(data)
    # 第一手
    {
      first_hand=matrix(ncol = nrow(path))
      for (i in seq(21,n,1)) {
        real_comb=true_path(data[(i-9):i,m])
        first_hand_quantity=new_quantize(path,real_comb,set = data[(i-9):i,m])
        first_hand=rbind(first_hand,first_hand_quantity)
      }
      first_hand=first_hand[-1,]
    }
    return(list(first_hand))
  }
  
}
data_sort_function=c(data_sort1,data_sort2,data_sort3,data_sort4,data_sort5,
    data_sort6,data_sort7)
# 2.4 定义诊断连续负的长度：
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
# 主函数：
run=function(data=data,path=path,data_sort,seq){
  
  n_path=nrow(path)
  # 1、minus_result行代表数据集索引，列代表formula，每格表示最大连负数目
  minus_result=matrix(nrow =length(data),ncol = n_path)
  new_index=matrix(nrow =length(data),ncol = n_path)
  for (m in 1:length(data)) {
    if(m%%50==0)print(m)
    # 取数据：
    {
      if(data_sort %in% c(1,3)){
        temp=data_sort_function[data_sort][[1]](data,m,path)
        first=temp[[1]]
        second=temp[[2]]
        n=nrow(first)+nrow(second)
        pattern=matrix(nrow = n,ncol =nrow(path))
        second=ifelse(first[-nrow(first),]<0,second,-second)
      }
      else if(data_sort %in% c(2,4,5,6)){
        temp=data_sort_function[data_sort][[1]](data,m,path)
        first=temp[[1]]  # 奇数轨
        second=temp[[2]] # 偶数轨
        third=temp[[3]]
        forth=temp[[4]]
        # 合并成final_result
        n=nrow(first)+nrow(second)+nrow(third)+nrow(forth)
        # 新指标的模式（1或2）和符号（正负）
        pattern=matrix(nrow = n,ncol =nrow(path))
      }
      else if(data_sort==7){
        temp=data_sort_function[data_sort][[1]](data,m,path)
        first=temp[[1]]
        n=nrow(first)
        pattern=matrix(nrow = n,ncol =nrow(path))
      }
    }
    
    # 反一反：
    {
      if(seq==0){
        # 基础版，不进行反一反：
        pattern[]=1
      }
      else if (seq==1){
        # plan20
        second=ifelse(first<0,second,-second)
        forth=ifelse(third<0,forth,-forth)
        
        pattern[seq(1,n,4),]=1 # 第1手固定  
        pattern[seq(2,n,4),]=ifelse(first<0,1,2) 
        pattern[seq(3,n,4),]=1 # 第3手固定
        pattern[seq(4,n,4),]=ifelse(third<0,1,2)
      }
      else if (seq==2){
        # plan13:
        
        third=ifelse(first<0,third,-third)
        forth=ifelse(second<0,forth,-forth)
        
        pattern[seq(1,n,4),]=1 # 第1手固定  
        pattern[seq(2,n,4),]=1 
        pattern[seq(3,n,4),]=ifelse(first<0,1,2)
        pattern[seq(4,n,4),]=ifelse(second<0,1,2)
      }
      else if (seq==3){
        # 13-1 新的反一反：
        second=-second
        third=ifelse(first<0,third,-third)
        forth=ifelse(second>0,forth,-forth)
        
        pattern[seq(1,n,4),]=1 # 第1手固定  
        pattern[seq(2,n,4),]=2 
        pattern[seq(3,n,4),]=ifelse(first<0,1,2)
        pattern[seq(4,n,4),]=ifelse(second>0,1,2)
      }
      else if (seq==4){
        # plan11：
        second=ifelse(first<0,second,-second)
        third=ifelse(first<0,third,-third)
        forth=ifelse(first*second>0,forth,-forth)
        
        pattern[seq(1,n,4),]=1 # 第1手固定  
        pattern[seq(2,n,4),]=2 
        pattern[seq(3,n,4),]=ifelse(first<0,1,2)
        pattern[seq(4,n,4),]=ifelse(first*second>0,1,2)
      }
    }
    
    if(data_sort %in% c(2,4,5,6)){
      sumtemp=matrix(nrow = n,ncol =nrow(path))
      sumtemp[seq(1,n,4),]=first
      sumtemp[seq(2,n,4),]=second
      sumtemp[seq(3,n,4),]=third
      sumtemp[seq(4,n,4),]=forth
    }
    else if(data_sort%in% c(1,3)){
      sumtemp=matrix(nrow = n,ncol =nrow(path))
      sumtemp[seq(1,n,2),]=first
      sumtemp[seq(2,n,2),]=second
    }
    else if(data_sort==7){
      sumtemp=matrix(nrow = n,ncol =nrow(path))
      sumtemp=first
    }
    
    # 正负符号直接根据是否匹配pattern的结果来
    symbol=ifelse(sumtemp>0,1,-1)
    
    minus_result[m,]=detect_continue_minus(sumtemp)
    new_index[m,]=colSums(pattern*symbol)
  }
  colnames(minus_result)=c(paste0('formula',1:n_path))
  
  # 2、result存放满足连续负个数小于等于5的结果
  result=matrix(nrow =max(minus_result) ,ncol = n_path)
  for (i in 1:(max(minus_result))){
    result[i,]=apply(minus_result,2,function(x)sum(x==i))
  }
  colnames(result)=c(paste0('formula',1:n_path))
  
  # 3、satisfy为某一个formula满足小于等于5的数据集个数：
  satisfy=apply(result[1:5,], 2, sum)
  return(list(minus_result,result,satisfy,new_index))
}


new_quantize=new_quantize_mix # 简单6810单轨不分BP：
plan0=run(all_data,path,7,0);plan0[[3]];mean(plan0[[3]]);sd(plan0[[3]])
new_quantize=new_quantize_BP # 简单6810单轨分BP：
plan01=run(all_data,path_BP,7,0);plan01[[3]];mean(plan01[[3]]);sd(plan01[[3]])
# plan31： 
plan31_1=run(all_data,path_BP,1,0);plan31_1[[3]];mean(plan31_1[[3]]);sd(plan31_1[[3]])
plan31_1[[2]]
# plan32:
plan32_0=run(all_data,path_BP,2,0);plan32_0[[3]]
plan32_13=run(all_data,path,2,2);plan32_13[[3]]
plan32_13_1=run(all_data,path,2,3);plan32_13_1[[3]]
plan32_11=run(all_data,path,2,4);plan32_11[[3]]
# plan33:
plan33_1=run(all_data,path,3,0);plan33_1[[3]]
# plan34:
plan34_20=run(all_data,path,4,1);plan34_20[[3]]
plan34_13=run(all_data,path,4,2);plan34_13[[3]]
plan34_13_1=run(all_data,path,4,3);plan34_13_1[[3]]
plan34_11=run(all_data,path,4,4);plan34_11[[3]]
# plan35:
plan35_0=run(all_data,path,5,0);plan35_0[[3]]
plan35_20=run(all_data,path,5,1);plan35_20[[3]]
plan35_13=run(all_data,path,5,2);plan35_13[[3]]
plan35_13_1=run(all_data,path,5,3);plan35_13_1[[3]]
plan35_11=run(all_data,path,5,4);plan35_11[[3]]
# plan 36:
plan36_0=run(all_data,path,6,0);plan36_0[[3]]
plan36_20=run(all_data,path,6,1);plan36_20[[3]]
plan36_13=run(all_data,path,6,2);plan36_13[[3]]
plan36_13_1=run(all_data,path,6,3);plan36_13_1[[3]]
plan36_11=run(all_data,path,6,4);plan36_11[[3]]

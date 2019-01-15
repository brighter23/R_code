# 1、formula准备------------------------------------------
data=read.csv('data\\treat_two_hundred.csv',header = T)
data1=read.csv('data\\new.csv',header = T)
data2=read.csv('data\\new1.csv',header = T)
data3=read.csv('data\\new2.csv',header = T)
data1=data1[,-23]
all_data=cbind(data,data1,data2,data3)
# YAY579
{
  library(stringr)
  formula=read.csv2('data\\YAY579.csv',header = F,stringsAsFactors = F)
  
  R5A=formula[1:6,];R5B=formula[8:13,]
  R7A=formula[15:46,];R7B=formula[48:79,]
  R9A=formula[81:208,];R9B=formula[210:337,]
  Formula=function(R,c1,c2){
    R_=str_split(R,pattern = ',',simplify = T)
    R_B=apply(R_[,c1], 1, function(x)str_c(x,collapse = '-'))
    R_P=apply(R_[,c2], 1, function(x)str_c(x,collapse = '-'))
    return(cbind(str_c(R_B,collapse = '|'), str_c(R_P,collapse = '|')))
  }
  
  R5A=Formula(R5A,c1 = 1:3,c2=4:6);R5B=Formula(R5B,c1 = 1:3,c2=4:6)
  R7A=Formula(R7A,c1 = 1:6,c2=7:12);R7B=Formula(R7B,c1 = 1:6,c2=7:12)
  R9A=Formula(R9A,c1 = 1:8,c2=9:16);R9B=Formula(R9B,c1 = 1:8,c2=9:16)
  
  path_BP_YAY579=matrix(c(R5A,R7A,R9A, R5A,R7B,R9A, R5A,R7A,R9B, R5A,R7B,R9B, R5B,R7A,R9A, R5B,R7B,R9A, R5B,R7A,R9B, R5B,R7B,R9B),byrow = T,ncol = 6)
  colnames(path_BP_YAY579)=c('B_R5','P_R5','B_R7','P_R7','B_R9','P_R9')
}

c=c('data','all_data','path_BP_YAY579')
a=ls()
rm(list=(setdiff(a,c)))
gc()
library(stringr)
# 当有新的formula时需要修改true_path、new_quantize、取数据的手数
# 2、训练==================================================
# 当有新的formula时需要修改true_path、new_quantize、取数据的手数(取7手计算final result的都一样，取9手的与9手的一样)

# 2.1定义计算每9个真实数据下的真实路径final_comb:
true_path0=function(dataset){
  #9R:8个
  t19=table(dataset[1:9])
  result19=paste0(t19[1],t19[2])
  t18=table(dataset[1:8])
  result18=paste0(t18[1],t18[2])
  t17=table(dataset[1:7])
  result17=paste0(t17[1],t17[2])
  t16=table(dataset[1:6])
  result16=paste0(t16[1],t16[2])
  t15=table(dataset[1:5])
  result15=paste0(t15[1],t15[2])
  t14=table(dataset[1:4])
  result14=paste0(t14[1],t14[2])
  t13=table(dataset[1:3])
  result13=paste0(t13[1],t13[2])
  t12=table(dataset[1:2])
  result12=paste0(t12[1],t12[2])
  comb1=paste0(result12,'-',result13,'-',result14,'-',result15,'-',result16,'-',result17,'-',result18,'-',result19)
  #7R：6个
  t39=table(dataset[3:9])
  result39=paste0(t39[1],t39[2])
  t38=table(dataset[3:8])
  result38=paste0(t38[1],t38[2])
  t37=table(dataset[3:7])
  result37=paste0(t37[1],t37[2])
  t36=table(dataset[3:6])
  result36=paste0(t36[1],t36[2])
  t35=table(dataset[3:5])
  result35=paste0(t35[1],t35[2])
  t34=table(dataset[3:4])
  result34=paste0(t34[1],t34[2])
  comb2=paste0(result34,'-',result35,'-',result36,'-',result37,'-',result38,'-',result39)
  #5R：3个，因为不是从02或20或11开始
  t59=table(dataset[5:9])
  result59=paste0(t59[1],t59[2])
  t58=table(dataset[5:8])
  result58=paste0(t58[1],t58[2])
  t57=table(dataset[5:7])
  result57=paste0(t57[1],t57[2])
  comb3=paste0(result57,'-',result58,'-',result59)
  return(matrix(c(comb1,comb2,comb3),ncol = 3))
  
}
true_path1=function(dataset){
  #9R:8个
  t19=table(dataset[c(1:8,9)])
  result19=paste0(t19[1],t19[2])
  t18=table(dataset[1:8])
  result18=paste0(t18[1],t18[2])
  t17=table(dataset[1:7])
  result17=paste0(t17[1],t17[2])
  t16=table(dataset[1:6])
  result16=paste0(t16[1],t16[2])
  t15=table(dataset[1:5])
  result15=paste0(t15[1],t15[2])
  t14=table(dataset[1:4])
  result14=paste0(t14[1],t14[2])
  t13=table(dataset[1:3])
  result13=paste0(t13[1],t13[2])
  t12=table(dataset[1:2])
  result12=paste0(t12[1],t12[2])
  comb1=paste0(result12,'-',result13,'-',result14,'-',result15,'-',result16,'-',result17,'-',result18,'-',result19)
  #7R：6个
  t39=table(dataset[c(1:6,9)])
  result39=paste0(t39[1],t39[2])
  t38=table(dataset[1:6])
  result38=paste0(t38[1],t38[2])
  t37=table(dataset[1:5])
  result37=paste0(t37[1],t37[2])
  t36=table(dataset[1:4])
  result36=paste0(t36[1],t36[2])
  t35=table(dataset[1:3])
  result35=paste0(t35[1],t35[2])
  t34=table(dataset[1:2])
  result34=paste0(t34[1],t34[2])
  comb2=paste0(result34,'-',result35,'-',result36,'-',result37,'-',result38,'-',result39)
  #5R：3个，因为不是从02或20或11开始
  t59=table(dataset[c(1:4,9)])
  result59=paste0(t59[1],t59[2])
  t58=table(dataset[1:4])
  result58=paste0(t58[1],t58[2])
  t57=table(dataset[1:3])
  result57=paste0(t57[1],t57[2])
  comb3=paste0(result57,'-',result58,'-',result59)
  return(matrix(c(comb1,comb2,comb3),ncol = 3))
  
} # 注：这里输入的时已经逆序处理的数据了
# 2.2定义7个真实数据下根据真实路径下的量化值-1或1:
# path=path_BP为分Bp的：
# Inverse有2种形式F,T，代表不同的取数据计算真实路径的方式，对应true_path0~1
new_quantize=function(path,set,Inverse){
  # 分BP的path有6列：
  if(Inverse)real_comb=true_path1(set)# 逆序的量化
  else real_comb=true_path0(set) # 顺序的方式进行量化
  if(ncol(path)==6){
    r9=matrix(0,ncol = nrow(path),nrow = 1)
    for (j in 1:nrow(path)) {
      sparate_condition=ifelse(Inverse,set[1],set[1])
      r9[1,j]=ifelse(sparate_condition=='B'|sparate_condition==1,ifelse(str_detect(real_comb[1,1],path[j,'B_R9']),1,-1),ifelse(str_detect(real_comb[1,1],path[j,'P_R9']),1,-1))
    }
    r7=matrix(0,ncol = nrow(path),nrow = 1)
    for (j in 1:nrow(path)) {
      sparate_condition=ifelse(Inverse,set[1],set[3])
      r7[1,j]=ifelse(sparate_condition=='B'|sparate_condition==1,ifelse(str_detect(real_comb[1,2],path[j,'B_R7']),1,-1),ifelse(str_detect(real_comb[1,2],path[j,'P_R7']),1,-1))
    }
    r5=matrix(0,ncol = nrow(path),nrow = 1)
    for (j in 1:nrow(path)) {
      sparate_condition=ifelse(Inverse,set[1],set[5])
      r5[1,j]=ifelse(sparate_condition=='B'|sparate_condition==1,ifelse(str_detect(real_comb[1,3],path[j,'B_R5']),1,-1),ifelse(str_detect(real_comb[1,3],path[j,'P_R5']),1,-1))
    }
  }
  else if(ncol(path)==3){
    r7=matrix(0,ncol = nrow(path),nrow = 1)
    for (j in 1:nrow(path)) {
      r7[1,j]=ifelse(str_detect(real_comb[1,1],path[j,'R9']),1,-1)
    }
    r6=matrix(0,ncol = nrow(path),nrow = 1)
    for (j in 1:nrow(path)) {
      r6[1,j]=ifelse(str_detect(real_comb[1,2],path[j,'R7']),1,-1)
    }
    r5=matrix(0,ncol = nrow(path),nrow = 1)
    for (j in 1:nrow(path)) {
      r5[1,j]=ifelse(str_detect(real_comb[1,3],path[j,'R5']),1,-1)
    }
  }
  return(r7+r5+r9)
} 
# new_quantize(path = path_BP_YAY579,set=data[1:9,1],Inverse=F) # 测试应该是正负相对
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
  #逆+不交叉+显现
  data_sort_thirteen_inverse=function(data=data,m,path=path){
    n=nrow(data)
    # 第一手：显+逆
    {
      first_hand=matrix(ncol = nrow(path))
      for (i in seq(17,n,4)) {
        first_hand_quantity=new_quantize(path,set = data[c(seq(i-2,i-16,-2),i),m],Inverse=T)
        first_hand=rbind(first_hand,first_hand_quantity)
      }
      first_hand=first_hand[-1,]
    }
    # 第二手：显+逆
    { 
      second_hand=matrix(ncol = nrow(path) )
      for (i in seq(18,n,4)) {
        second_hand_quantity=new_quantize(path,set = data[c(seq(i-2,i-16,-2),i),m],Inverse=T)
        second_hand=rbind(second_hand,second_hand_quantity)
      }
      second_hand=second_hand[-1,]
    }
    # 第三手：显+逆
    {
      third_hand=matrix(ncol = nrow(path))
      for (i in seq(19,n,4)) {
        third_hand_quantity=new_quantize(path,set = data[c(seq(i-2,i-16,-2),i),m],Inverse=T)
        third_hand=rbind(third_hand,third_hand_quantity)
      }
      third_hand=third_hand[-1,]
    }
    # 第四手：显+逆
    { 
      forth_hand=matrix(ncol = nrow(path) )
      for (i in seq(20,n,4)) {
        forth_hand_quantity=new_quantize(path,set = data[c(seq(i-2,i-16,-2),i),m],Inverse=T)
        forth_hand=rbind(forth_hand,forth_hand_quantity)
      }
      forth_hand=forth_hand[-1,]
    }
    return(list(first_hand,second_hand,third_hand,forth_hand))
  }
  #逆+交叉+显现
  data_sort_thirteen_inverse_cross=function(data=data,m,path=path){
    n=nrow(data)
    # 第一手：显+逆+奇偶交叉
    {
      first_hand=matrix(ncol = nrow(path))
      for (i in seq(17,n,4)) {
        first_hand_quantity=new_quantize(path,set = data[c(seq(i-1,i-15,-2),i),m],Inverse=T)
        first_hand=rbind(first_hand,first_hand_quantity)
      }
      first_hand=first_hand[-1,]
    }
    # 第二手：显+逆+奇偶交叉
    { 
      second_hand=matrix(ncol = nrow(path) )
      for (i in seq(18,n,4)) {
        second_hand_quantity=new_quantize(path,set = data[c(seq(i-3,i-17,-2),i),m],Inverse=T)
        second_hand=rbind(second_hand,second_hand_quantity)
      }
      second_hand=second_hand[-1,]
    }
    # 第三手：显+逆+奇偶交叉
    {
      third_hand=matrix(ncol = nrow(path))
      for (i in seq(19,n,4)) {
        third_hand_quantity=new_quantize(path,set = data[c(seq(i-1,i-15,-2),i),m],Inverse=T)
        third_hand=rbind(third_hand,third_hand_quantity)
      }
      third_hand=third_hand[-1,]
    }
    # 第四手：显+逆+奇偶交叉
    { 
      forth_hand=matrix(ncol = nrow(path) )
      for (i in seq(20,n,4)) {
        forth_hand_quantity=new_quantize(path,set = data[c(seq(i-3,i-17,-2),i),m],Inverse=T)
        forth_hand=rbind(forth_hand,forth_hand_quantity)
      }
      forth_hand=forth_hand[-1,]
    }
    return(list(first_hand,second_hand,third_hand,forth_hand))
  }
  #逆+不交叉+re
  data_sort_thirteen_inverse_refelection=function(data=data,m,path=path){
    n=nrow(data)
    reflection=as.factor(ifelse(data[,m]=='B','P','B'))
    # 第一手：re+逆
    {
      first_hand=matrix(ncol = nrow(path))
      for (i in seq(17,n,4)){
        first_hand_quantity=new_quantize(path,set = factor(c(reflection[seq(i-2,i-16,-2)],data[i,m]),levels=c(1,2)),Inverse=T)
        first_hand=rbind(first_hand,first_hand_quantity)
      }
      first_hand=first_hand[-1,]
    }
    # 第二手：re+逆
    {
      second_hand=matrix(ncol = nrow(path) )
      for (i in seq(18,n,4)) {
        second_hand_quantity=new_quantize(path,set = factor(c(reflection[seq(i-2,i-16,-2)],data[i,m]),levels=c(1,2)),Inverse=T)
        second_hand=rbind(second_hand,second_hand_quantity)
      }
      second_hand=second_hand[-1,]
    }
    # 第三手：re+逆
    {
      third_hand=matrix(ncol = nrow(path))
      for (i in seq(19,n,4)) {
        third_hand_quantity=new_quantize(path,set = factor(c(reflection[seq(i-2,i-16,-2)],data[i,m]),levels=c(1,2)),Inverse=T)
        third_hand=rbind(third_hand,third_hand_quantity)
      }
      third_hand=third_hand[-1,]
    }
    # 第四手：re+逆
    {
      forth_hand=matrix(ncol = nrow(path) )
      for (i in seq(20,n,4)) {
        forth_hand_quantity=new_quantize(path,set = factor(c(reflection[seq(i-2,i-16,-2)],data[i,m]),levels=c(1,2)),Inverse=T)
        forth_hand=rbind(forth_hand,forth_hand_quantity)
      }
      forth_hand=forth_hand[-1,]
    }
    return(list(first_hand,second_hand,third_hand,forth_hand))
  }
  #顺+不交叉+显现
  data_sort_sequence_allsequence=function(data=data,m,path=path){
    n=nrow(data)
    # 第一手：顺
    {
      first_hand=matrix(ncol = nrow(path))
      for (i in seq(17,n,4)) {
        first_hand_quantity=new_quantize(path,set = data[c(seq(i-16,i-2,2),i),m],Inverse = F)
        first_hand=rbind(first_hand,first_hand_quantity)
      }
      first_hand=first_hand[-1,]
    }
    # 第二手：顺
    { 
      second_hand=matrix(ncol = nrow(path) )
      for (i in seq(18,n,4)) {
        second_hand_quantity=new_quantize(path,set = data[c(seq(i-16,i-2,2),i),m],Inverse = F)
        second_hand=rbind(second_hand,second_hand_quantity)
      }
      second_hand=second_hand[-1,]
    }
    # 第三手：顺
    {
      third_hand=matrix(ncol = nrow(path))
      for (i in seq(19,n,4)) {
        third_hand_quantity=new_quantize(path,set = data[c(seq(i-16,i-2,2),i),m],Inverse = F)
        third_hand=rbind(third_hand,third_hand_quantity)
      }
      third_hand=third_hand[-1,]
    }
    # 第四手：顺
    { 
      forth_hand=matrix(ncol = nrow(path) )
      for (i in seq(20,n,4)) {
        forth_hand_quantity=new_quantize(path,set = data[c(seq(i-16,i-2,2),i),m],Inverse = F)
        forth_hand=rbind(forth_hand,forth_hand_quantity)
      }
      forth_hand=forth_hand[-1,]
    }
    return(list(first_hand,second_hand,third_hand,forth_hand))
  }
  #顺+交叉+显现
  data_sort_sequence_allsequence_cross=function(data=data,m,path=path){
    n=nrow(data)
    # 第一手：显+顺+奇偶交叉
    {
      first_hand=matrix(ncol = nrow(path))
      for (i in seq(17,n,4)) {
        first_hand_quantity=new_quantize(path,set = data[c(seq(i-15,i-1,2),i),m],Inverse = F)
        first_hand=rbind(first_hand,first_hand_quantity)
      }
      first_hand=first_hand[-1,]
    }
    # 第二手：显+顺+奇偶交叉
    { 
      second_hand=matrix(ncol = nrow(path) )
      for (i in seq(18,n,4)) {
        second_hand_quantity=new_quantize(path,set = data[c(seq(i-17,i-3,2),i),m],Inverse = F)
        second_hand=rbind(second_hand,second_hand_quantity)
      }
      second_hand=second_hand[-1,]
    }
    # 第三手：显+顺+奇偶交叉
    {
      third_hand=matrix(ncol = nrow(path))
      for (i in seq(19,n,4)) {
        third_hand_quantity=new_quantize(path,set = data[c(seq(i-15,i-1,2),i),m],Inverse = F)
        third_hand=rbind(third_hand,third_hand_quantity)
      }
      third_hand=third_hand[-1,]
    }
    # 第四手：显+顺+奇偶交叉
    { 
      forth_hand=matrix(ncol = nrow(path) )
      for (i in seq(20,n,4)) {
        forth_hand_quantity=new_quantize(path,set = data[c(seq(i-17,i-3,2),i),m],Inverse = F)
        forth_hand=rbind(forth_hand,forth_hand_quantity)
      }
      forth_hand=forth_hand[-1,]
    }
    return(list(first_hand,second_hand,third_hand,forth_hand))
  }
  # 1211版本，横向（同一轨内）逆序、顺序轮换
  data_sort_sequence_inverse_sequence=function(data=data,m,path=path){
    n=nrow(data)
    # 第一手：顺
    {
      first_hand=matrix(ncol = nrow(path))
      for (i in seq(17,n,4)) {
        first_hand_quantity=new_quantize(path,set = data[c(seq(i-16,i-2,2),i),m],Inverse = F)
        first_hand=rbind(first_hand,first_hand_quantity)
      }
      first_hand=first_hand[-1,]
    }
    # 第二手：顺
    { 
      second_hand=matrix(ncol = nrow(path) )
      for (i in seq(18,n,4)) {
        second_hand_quantity=new_quantize(path,set = data[c(seq(i-16,i-2,2),i),m],Inverse = F)
        second_hand=rbind(second_hand,second_hand_quantity)
      }
      second_hand=second_hand[-1,]
    }
    # 第三手：逆
    {
      third_hand=matrix(ncol = nrow(path))
      for (i in seq(19,n,4)) {
        third_hand_quantity=new_quantize(path,set = data[c(seq(i-2,i-16,-2),i),m],Inverse = T)
        third_hand=rbind(third_hand,third_hand_quantity)
      }
      third_hand=third_hand[-1,]
    }
    # 第四手：逆
    { 
      forth_hand=matrix(ncol = nrow(path) )
      for (i in seq(20,n,4)) {
        forth_hand_quantity=new_quantize(path,set = data[c(seq(i-2,i-16,-2),i),m],Inverse = T)
        forth_hand=rbind(forth_hand,forth_hand_quantity)
      }
      forth_hand=forth_hand[-1,]
    }
    return(list(first_hand,second_hand,third_hand,forth_hand))
  }
}
run=function(data=data,path=path,inverse=T,choice,rolling,reflection,cross){
  library(stringr)
  n_path=nrow(path)
  minus_result=matrix(nrow =length(data),ncol = n_path)
  plus=matrix(nrow =length(data),ncol = n_path)
  minus=matrix(nrow =length(data),ncol = n_path)
  new_indexA=matrix(nrow =length(data),ncol = n_path)
  new_indexB=matrix(nrow =length(data),ncol = n_path)
  new_indexA1=matrix(nrow =length(data),ncol = n_path)
  new_indexB1=matrix(nrow =length(data),ncol = n_path)
  new_indexc1=matrix(nrow =length(data),ncol = n_path)
  new_indexc2=matrix(nrow =length(data),ncol = n_path)
  new_indexD=matrix(nrow =length(data),ncol = n_path)
  new_indexE=matrix(nrow =length(data),ncol = n_path)
  for (m in 1:length(data)) {
    if(m%%50==0)print(m)
    if(choice %in% c(12111,12112,12113,12114)){
      temp=data_sort_sequence_inverse_sequence(data,m,path)
    }
    else if(inverse & cross & reflection){
      # 逆序+交叉+reflection
      print('未定义')
    }
    else if(inverse & cross & !reflection){
      # 逆序+交叉+显现
      temp=data_sort_thirteen_inverse_cross(data,m,path)
    }
    else if(inverse & !cross & reflection){
      # 逆序+不交叉+reflection
      temp=data_sort_thirteen_inverse_refelection(data,m,path)
    }
    else if(inverse & !cross & !reflection){
      # 逆序+不交叉+显现
      temp=data_sort_thirteen_inverse(data,m,path)
    }
    else if(!inverse & cross & reflection){
      # 顺序+交叉+reflection
      print('未定义')
    }
    else if(!inverse & cross & !reflection){
      # 顺序+交叉+显现
      temp=data_sort_sequence_allsequence_cross(data,m,path)
    }
    else if(!inverse & !cross & reflection){
      # 顺序+不交叉+reflection
      print('未定义')
    }
    else if(!inverse & !cross & !reflection){
      # 顺序+不交叉+显现
      temp=data_sort_sequence_allsequence(data,m,path)
    }
    
    # 1）初始化变量，以记录指标：
    {
      first=temp[[1]] # 奇数轨奇数行
      second=temp[[2]] # 偶数轨奇数行
      third=temp[[3]] # 奇数轨偶数行
      forth=temp[[4]] # 偶数轨偶数行
      n_sumtemp=nrow(first)+nrow(second)+nrow(third)+nrow(forth)
      sumtemp=matrix(nrow =n_sumtemp ,ncol = nrow(path))
      patternA=matrix(nrow =n_sumtemp ,ncol = nrow(path))
      patternB=matrix(nrow =n_sumtemp ,ncol = nrow(path))
      # 计算真实数据下的前一手和当前一手是否相同，相同为s,不同为d
      real_s_or_d=matrix(nrow =n_sumtemp ,ncol = nrow(path)) # 计算新指标A1&B1时会用到1为s,-1为d
      for (i in (nrow(data)-n_sumtemp+1):length(data[,m])) {
        # 注意这里双轨时是i-2,单轨时是i-1
        real_s_or_d[i-(nrow(data)-n_sumtemp),]=ifelse(data[i,m]==data[i-2,m],1,-1)
      }
    }
    # 2）不同的plan:1019,1019-1,1019-2……不同的最优化方式
    {
      # 因为1，2手与3，4手长度不一样，因因此截取到长度一样作为判断依据，拼接还是使用原始的
      min_=min(nrow(first),nrow(third))
      decide_first=first[1:min_,]
      decide_second=second[1:min_,]
      
      if (choice==12111&rolling==F) {
        
        third=ifelse(decide_first<0,third,-third)
        forth=ifelse(decide_second<0,forth,-forth)
        # 计算新指标AB:
        patternA[seq(1,n_sumtemp,4),]=1
        patternA[seq(2,n_sumtemp,4),]=2
        patternA[seq(3,n_sumtemp,4),]=ifelse(decide_first<0,1,2)
        patternA[seq(4,n_sumtemp,4),]=ifelse(decide_second<0,1,2)
        
        patternB[seq(1,n_sumtemp,4),]=2
        patternB[seq(2,n_sumtemp,4),]=1
        patternB[seq(3,n_sumtemp,4),]=ifelse(decide_first<0,2,1)
        patternB[seq(4,n_sumtemp,4),]=ifelse(decide_second<0,2,1)
        # 4个拼接到一个
        sumtemp[seq(1,n_sumtemp,4),]=first
        sumtemp[seq(2,n_sumtemp,4),]=second
        sumtemp[seq(3,n_sumtemp,4),]=third
        sumtemp[seq(4,n_sumtemp,4),]=forth
      }
      else if (choice==12112&rolling==F) {
        
        third=ifelse(decide_first>0,third,-third)
        forth=ifelse(decide_second>0,forth,-forth)
        # 计算新指标AB:
        patternA[seq(1,n_sumtemp,4),]=1
        patternA[seq(2,n_sumtemp,4),]=2
        patternA[seq(3,n_sumtemp,4),]=ifelse(decide_first>0,1,2)
        patternA[seq(4,n_sumtemp,4),]=ifelse(decide_second>0,1,2)
        
        patternB[seq(1,n_sumtemp,4),]=2
        patternB[seq(2,n_sumtemp,4),]=1
        patternB[seq(3,n_sumtemp,4),]=ifelse(decide_first>0,2,1)
        patternB[seq(4,n_sumtemp,4),]=ifelse(decide_second>0,2,1)
        # 4个拼接到一个
        sumtemp[seq(1,n_sumtemp,4),]=first
        sumtemp[seq(2,n_sumtemp,4),]=second
        sumtemp[seq(3,n_sumtemp,4),]=third
        sumtemp[seq(4,n_sumtemp,4),]=forth
      }
      else if (choice==12113&rolling==F) {
        second=-second # 第二手固定使用相反的
        third=ifelse(decide_first<0,third,-third)
        forth=ifelse(decide_second>0,forth,-forth) # 第4手小于0时反一反
        # 计算新指标AB:
        patternA[seq(1,n_sumtemp,4),]=1
        patternA[seq(2,n_sumtemp,4),]=2
        patternA[seq(3,n_sumtemp,4),]=ifelse(decide_first<0,1,2)
        patternA[seq(4,n_sumtemp,4),]=ifelse(decide_second>0,1,2)
        
        patternB[seq(1,n_sumtemp,4),]=2
        patternB[seq(2,n_sumtemp,4),]=1
        patternB[seq(3,n_sumtemp,4),]=ifelse(decide_first<0,2,1)
        patternB[seq(4,n_sumtemp,4),]=ifelse(decide_second>0,2,1)
        # 4个拼接到一个
        sumtemp[seq(1,n_sumtemp,4),]=first
        sumtemp[seq(2,n_sumtemp,4),]=second
        sumtemp[seq(3,n_sumtemp,4),]=third
        sumtemp[seq(4,n_sumtemp,4),]=forth
      }
      else if (choice==12114&rolling==F) {
        second=-second # 第二手固定使用相反的
        third=ifelse(decide_first>0,third,-third)
        forth=ifelse(decide_second<0,forth,-forth)
        # 计算新指标AB:
        patternA[seq(1,n_sumtemp,4),]=1
        patternA[seq(2,n_sumtemp,4),]=2
        patternA[seq(3,n_sumtemp,4),]=ifelse(decide_first>0,1,2)
        patternA[seq(4,n_sumtemp,4),]=ifelse(decide_second<0,1,2)
        
        patternB[seq(1,n_sumtemp,4),]=2
        patternB[seq(2,n_sumtemp,4),]=1
        patternB[seq(3,n_sumtemp,4),]=ifelse(decide_first>0,2,1)
        patternB[seq(4,n_sumtemp,4),]=ifelse(decide_second<0,2,1)
        # 4个拼接到一个
        sumtemp[seq(1,n_sumtemp,4),]=first
        sumtemp[seq(2,n_sumtemp,4),]=second
        sumtemp[seq(3,n_sumtemp,4),]=third
        sumtemp[seq(4,n_sumtemp,4),]=forth
      }
      else if (choice==1019&rolling==F) {
        second=-second
        # 判断是否是负负的，是的话3&4就使用（1，8），否则（8，1）
        # formula1和formula8表示两组against的formula，1为original，8为against
        opposite_condition=decide_first<0&decide_second<0
        third=ifelse(opposite_condition,third,-third)
        forth=ifelse(opposite_condition,-forth,forth)
        # 计算新指标AB:
        patternA[seq(1,n_sumtemp,4),]=1
        patternA[seq(2,n_sumtemp,4),]=2
        patternA[seq(3,n_sumtemp,4),]=ifelse(opposite_condition,1,2)
        patternA[seq(4,n_sumtemp,4),]=ifelse(opposite_condition,2,1)
        
        patternB[seq(1,n_sumtemp,4),]=2
        patternB[seq(2,n_sumtemp,4),]=1
        patternB[seq(3,n_sumtemp,4),]=ifelse(opposite_condition,2,1)
        patternB[seq(4,n_sumtemp,4),]=ifelse(opposite_condition,1,2)
        # 4个拼接到一个
        sumtemp[seq(1,n_sumtemp,4),]=first
        sumtemp[seq(2,n_sumtemp,4),]=second
        sumtemp[seq(3,n_sumtemp,4),]=third
        sumtemp[seq(4,n_sumtemp,4),]=forth
      }
      # 滚动时不受长度影响：
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
          opposite_condition=sumtemp[i-1,]<0&sumtemp[i-2,]<0
          sumtemp[i,]=ifelse(opposite_condition,ifelse(patternA[i-2,]==1,sumtemp[i,],-sumtemp[i,]),ifelse(patternA[i-2,]==1,-sumtemp[i,],sumtemp[i,]))
          patternA[i,]=ifelse(opposite_condition,ifelse(patternA[i-2,]==1,1,2),ifelse(patternA[i-2,]==1,2,1))
          patternB[i,]=ifelse(opposite_condition,ifelse(patternA[i-2,]==1,2,1),ifelse(patternA[i-2,]==1,1,2))
          sumtemp[i+1,]=ifelse(opposite_condition,ifelse(patternA[i-1,]==1,sumtemp[i+1,],-sumtemp[i+1,]),ifelse(patternA[i-1,]==1,-sumtemp[i+1,],sumtemp[i+1,]))
          patternA[i+1,]=ifelse(opposite_condition,ifelse(patternA[i-1,]==1,1,2),ifelse(patternA[i-1,]==1,2,1))
          patternB[i+1,]=ifelse(opposite_condition,ifelse(patternA[i-1,]==1,2,1),ifelse(patternA[i-1,]==1,1,2))
        }
        
      }
      else if(choice==10191&rolling==F){
        second=-second
        # 判断是否是负负或正正的，是的3&4就使用（1，8），否则（8，1）
        # formula1和formula8表示两组against的formula，1为original，8为against
        opposite_condition=decide_first*decide_second>0
        third=ifelse(opposite_condition,third,-third)
        forth=ifelse(opposite_condition,-forth,forth)
        # 计算新指标AB:
        patternA[seq(1,n_sumtemp,4),]=1
        patternA[seq(2,n_sumtemp,4),]=2
        patternA[seq(3,n_sumtemp,4),]=ifelse(opposite_condition,1,2)
        patternA[seq(4,n_sumtemp,4),]=ifelse(opposite_condition,2,1)
        
        patternB[seq(1,n_sumtemp,4),]=2
        patternB[seq(2,n_sumtemp,4),]=1
        patternB[seq(3,n_sumtemp,4),]=ifelse(opposite_condition,2,1)
        patternB[seq(4,n_sumtemp,4),]=ifelse(opposite_condition,1,2)
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
          opposite_condition=sumtemp[i-1,]*sumtemp[i-2,]>0
          sumtemp[i,]=ifelse(opposite_condition,ifelse(patternA[i-2,]==1,sumtemp[i,],-sumtemp[i,]),ifelse(patternA[i-2,]==1,-sumtemp[i,],sumtemp[i,]))
          patternA[i,]=ifelse(opposite_condition,ifelse(patternA[i-2,]==1,1,2),ifelse(patternA[i-2,]==1,2,1))
          patternB[i,]=ifelse(opposite_condition,ifelse(patternA[i-2,]==1,2,1),ifelse(patternA[i-2,]==1,1,2))
          sumtemp[i+1,]=ifelse(opposite_condition,ifelse(patternA[i-1,]==1,sumtemp[i+1,],-sumtemp[i+1,]),ifelse(patternA[i-1,]==1,-sumtemp[i+1,],sumtemp[i+1,]))
          patternA[i+1,]=ifelse(opposite_condition,ifelse(patternA[i-1,]==1,1,2),ifelse(patternA[i-1,]==1,2,1))
          patternB[i+1,]=ifelse(opposite_condition,ifelse(patternA[i-1,]==1,2,1),ifelse(patternA[i-1,]==1,1,2))
        }
      }
      else if(choice==10192&rolling==F){
        second=-second
        # 判断是否是正负或正正的，是的3&4就使用（1，8），否则（8，1）
        # formula1和formula8表示两组against的formula，1为original，8为against
        opposite_condition=decide_first>0|decide_second>0
        third=ifelse(opposite_condition,third,-third)
        forth=ifelse(opposite_condition,-forth,forth)
        # 计算新指标AB:
        patternA[seq(1,n_sumtemp,4),]=1
        patternA[seq(2,n_sumtemp,4),]=2
        patternA[seq(3,n_sumtemp,4),]=ifelse(opposite_condition,1,2)
        patternA[seq(4,n_sumtemp,4),]=ifelse(opposite_condition,2,1)
        
        patternB[seq(1,n_sumtemp,4),]=2
        patternB[seq(2,n_sumtemp,4),]=1
        patternB[seq(3,n_sumtemp,4),]=ifelse(opposite_condition,2,1)
        patternB[seq(4,n_sumtemp,4),]=ifelse(opposite_condition,1,2)
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
          opposite_condition=sumtemp[i-1,]>0|sumtemp[i-2,]>0
          sumtemp[i,]=ifelse(opposite_condition,ifelse(patternA[i-2,]==1,sumtemp[i,],-sumtemp[i,]),ifelse(patternA[i-2,]==1,-sumtemp[i,],sumtemp[i,]))
          patternA[i,]=ifelse(opposite_condition,ifelse(patternA[i-2,]==1,1,2),ifelse(patternA[i-2,]==1,2,1))
          patternB[i,]=ifelse(opposite_condition,ifelse(patternA[i-2,]==1,2,1),ifelse(patternA[i-2,]==1,1,2))
          
          sumtemp[i+1,]=ifelse(opposite_condition,ifelse(patternA[i-1,]==1,sumtemp[i+1,],-sumtemp[i+1,]),ifelse(patternA[i-1,]==1,-sumtemp[i+1,],sumtemp[i+1,]))
          patternA[i+1,]=ifelse(opposite_condition,ifelse(patternA[i-1,]==1,1,2),ifelse(patternA[i-1,]==1,2,1))
          patternB[i+1,]=ifelse(opposite_condition,ifelse(patternA[i-1,]==1,2,1),ifelse(patternA[i-1,]==1,1,2))
        }
      }
      # 往下默认都是滚动进行反一反：
      # plan121015:四手为一判定条件 
      else if(choice==121015){
        # 4个拼接到一个
        sumtemp[seq(1,n_sumtemp,4),]=first
        sumtemp[seq(2,n_sumtemp,4),]=second
        sumtemp[seq(3,n_sumtemp,4),]=third
        sumtemp[seq(4,n_sumtemp,4),]=forth
        
        sumtemp[2,]=-sumtemp[2,]
        sumtemp[3,]=-sumtemp[3,]
        
        patternA[1,]=1;patternA[2,]=2 # 1表示某一手数据使用original
        patternA[4,]=1;patternA[3,]=2
        
        patternB[1,]=2;patternB[2,]=1 # A与B相反
        patternB[4,]=2;patternB[3,]=1
        # 注意相比不滚动的，需要多1个变量用以记录前一个的路径，follow的不一定是（1，8）
        # patternA 刚好记录了路径
        # 2重判断，第一判定是否follow，第2判定follow哪一个
        for (i in seq(5,nrow(sumtemp)-1,4)) {
          # follow的条件:2个负时follow：
          follow_condition=apply(sumtemp[(i-4):(i-1),],2,function(x){ifelse(sum(x<0)==2,T,F)})
          
          sumtemp[i,]=ifelse(follow_condition,ifelse(patternA[i-4,]==1,sumtemp[i,],-sumtemp[i,]),ifelse(patternA[i-4,]==1,-sumtemp[i,],sumtemp[i,]))
          patternA[i,]=ifelse(follow_condition,ifelse(patternA[i-4,]==1,1,2),ifelse(patternA[i-4,]==1,2,1))
          patternB[i,]=ifelse(follow_condition,ifelse(patternA[i-4,]==1,2,1),ifelse(patternA[i-4,]==1,1,2))
          
          sumtemp[i+1,]=ifelse(follow_condition,ifelse(patternA[i-3,]==1,sumtemp[i+1,],-sumtemp[i+1,]),ifelse(patternA[i-3,]==1,-sumtemp[i+1,],sumtemp[i+1,]))
          patternA[i+1,]=ifelse(follow_condition,ifelse(patternA[i-3,]==1,1,2),ifelse(patternA[i-3,]==1,2,1))
          patternB[i+1,]=ifelse(follow_condition,ifelse(patternA[i-3,]==1,2,1),ifelse(patternA[i-3,]==1,1,2))
          
          sumtemp[i+2,]=ifelse(follow_condition,ifelse(patternA[i-2,]==1,sumtemp[i+2,],-sumtemp[i+2,]),ifelse(patternA[i-2,]==1,-sumtemp[i+2,],sumtemp[i+2,]))
          patternA[i+2,]=ifelse(follow_condition,ifelse(patternA[i-2,]==1,1,2),ifelse(patternA[i-2,]==1,2,1))
          patternB[i+2,]=ifelse(follow_condition,ifelse(patternA[i-2,]==1,2,1),ifelse(patternA[i-2,]==1,1,2))
          
          sumtemp[i+3,]=ifelse(follow_condition,ifelse(patternA[i-1,]==1,sumtemp[i+3,],-sumtemp[i+3,]),ifelse(patternA[i-1,]==1,-sumtemp[i+3,],sumtemp[i+3,]))
          patternA[i+3,]=ifelse(follow_condition,ifelse(patternA[i-1,]==1,1,2),ifelse(patternA[i-1,]==1,2,1))
          patternB[i+3,]=ifelse(follow_condition,ifelse(patternA[i-1,]==1,2,1),ifelse(patternA[i-1,]==1,1,2))
        }
      }
      # plan121016
      else if(choice==121016){
        # 4个拼接到一个
        sumtemp[seq(1,n_sumtemp,4),]=first
        sumtemp[seq(2,n_sumtemp,4),]=second
        sumtemp[seq(3,n_sumtemp,4),]=third
        sumtemp[seq(4,n_sumtemp,4),]=forth
        
        sumtemp[2,]=-sumtemp[2,]
        sumtemp[3,]=-sumtemp[3,]
        
        patternA[1,]=1;patternA[2,]=2 # 1表示某一手数据使用original
        patternA[4,]=1;patternA[3,]=2
        
        patternB[1,]=2;patternB[2,]=1 # A与B相反
        patternB[4,]=2;patternB[3,]=1
        # 注意相比不滚动的，需要多1个变量用以记录前一个的路径，follow的不一定是（1，8）
        # patternA 刚好记录了路径
        # 2重判断，第一判定是否follow，第2判定follow哪一个
        for (i in seq(5,nrow(sumtemp)-1,4)) {
          # follow的条件:2个负时follow：
          follow_condition=!apply(sumtemp[(i-4):(i-1),],2,function(x){ifelse(sum(x<0)==2,T,F)})
          
          sumtemp[i,]=ifelse(follow_condition,ifelse(patternA[i-4,]==1,sumtemp[i,],-sumtemp[i,]),ifelse(patternA[i-4,]==1,-sumtemp[i,],sumtemp[i,]))
          patternA[i,]=ifelse(follow_condition,ifelse(patternA[i-4,]==1,1,2),ifelse(patternA[i-4,]==1,2,1))
          patternB[i,]=ifelse(follow_condition,ifelse(patternA[i-4,]==1,2,1),ifelse(patternA[i-4,]==1,1,2))
          
          sumtemp[i+1,]=ifelse(follow_condition,ifelse(patternA[i-3,]==1,sumtemp[i+1,],-sumtemp[i+1,]),ifelse(patternA[i-3,]==1,-sumtemp[i+1,],sumtemp[i+1,]))
          patternA[i+1,]=ifelse(follow_condition,ifelse(patternA[i-3,]==1,1,2),ifelse(patternA[i-3,]==1,2,1))
          patternB[i+1,]=ifelse(follow_condition,ifelse(patternA[i-3,]==1,2,1),ifelse(patternA[i-3,]==1,1,2))
          
          sumtemp[i+2,]=ifelse(follow_condition,ifelse(patternA[i-2,]==1,sumtemp[i+2,],-sumtemp[i+2,]),ifelse(patternA[i-2,]==1,-sumtemp[i+2,],sumtemp[i+2,]))
          patternA[i+2,]=ifelse(follow_condition,ifelse(patternA[i-2,]==1,1,2),ifelse(patternA[i-2,]==1,2,1))
          patternB[i+2,]=ifelse(follow_condition,ifelse(patternA[i-2,]==1,2,1),ifelse(patternA[i-2,]==1,1,2))
          
          sumtemp[i+3,]=ifelse(follow_condition,ifelse(patternA[i-1,]==1,sumtemp[i+3,],-sumtemp[i+3,]),ifelse(patternA[i-1,]==1,-sumtemp[i+3,],sumtemp[i+3,]))
          patternA[i+3,]=ifelse(follow_condition,ifelse(patternA[i-1,]==1,1,2),ifelse(patternA[i-1,]==1,2,1))
          patternB[i+3,]=ifelse(follow_condition,ifelse(patternA[i-1,]==1,2,1),ifelse(patternA[i-1,]==1,1,2))
        }
      }
      # plan121017
      else if(choice==121017){
        # 4个拼接到一个
        sumtemp[seq(1,n_sumtemp,4),]=first
        sumtemp[seq(2,n_sumtemp,4),]=second
        sumtemp[seq(3,n_sumtemp,4),]=third
        sumtemp[seq(4,n_sumtemp,4),]=forth
        
        sumtemp[2,]=-sumtemp[2,]
        sumtemp[4,]=-sumtemp[4,]
        
        patternA[1,]=1;patternA[2,]=2 # 1表示某一手数据使用original
        patternA[3,]=1;patternA[4,]=2
        
        patternB[1,]=2;patternB[2,]=1 # A与B相反
        patternB[3,]=2;patternB[4,]=1
        # 注意相比不滚动的，需要多1个变量用以记录前一个的路径，follow的不一定是（1，8）
        # patternA 刚好记录了路径
        # 2重判断，第一判定是否follow，第2判定follow哪一个
        for (i in seq(5,nrow(sumtemp)-1,4)) {
          # follow的条件:2个负时follow：
          follow_condition=apply(sumtemp[(i-4):(i-1),],2,function(x){ifelse(sum(x<0)==2,T,F)})
          
          sumtemp[i,]=ifelse(follow_condition,ifelse(patternA[i-4,]==1,sumtemp[i,],-sumtemp[i,]),ifelse(patternA[i-4,]==1,-sumtemp[i,],sumtemp[i,]))
          patternA[i,]=ifelse(follow_condition,ifelse(patternA[i-4,]==1,1,2),ifelse(patternA[i-4,]==1,2,1))
          patternB[i,]=ifelse(follow_condition,ifelse(patternA[i-4,]==1,2,1),ifelse(patternA[i-4,]==1,1,2))
          
          sumtemp[i+1,]=ifelse(follow_condition,ifelse(patternA[i-3,]==1,sumtemp[i+1,],-sumtemp[i+1,]),ifelse(patternA[i-3,]==1,-sumtemp[i+1,],sumtemp[i+1,]))
          patternA[i+1,]=ifelse(follow_condition,ifelse(patternA[i-3,]==1,1,2),ifelse(patternA[i-3,]==1,2,1))
          patternB[i+1,]=ifelse(follow_condition,ifelse(patternA[i-3,]==1,2,1),ifelse(patternA[i-3,]==1,1,2))
          
          sumtemp[i+2,]=ifelse(follow_condition,ifelse(patternA[i-2,]==1,sumtemp[i+2,],-sumtemp[i+2,]),ifelse(patternA[i-2,]==1,-sumtemp[i+2,],sumtemp[i+2,]))
          patternA[i+2,]=ifelse(follow_condition,ifelse(patternA[i-2,]==1,1,2),ifelse(patternA[i-2,]==1,2,1))
          patternB[i+2,]=ifelse(follow_condition,ifelse(patternA[i-2,]==1,2,1),ifelse(patternA[i-2,]==1,1,2))
          
          sumtemp[i+3,]=ifelse(follow_condition,ifelse(patternA[i-1,]==1,sumtemp[i+3,],-sumtemp[i+3,]),ifelse(patternA[i-1,]==1,-sumtemp[i+3,],sumtemp[i+3,]))
          patternA[i+3,]=ifelse(follow_condition,ifelse(patternA[i-1,]==1,1,2),ifelse(patternA[i-1,]==1,2,1))
          patternB[i+3,]=ifelse(follow_condition,ifelse(patternA[i-1,]==1,2,1),ifelse(patternA[i-1,]==1,1,2))
        }
      }
      # plan121018 
      else if(choice==121018){
        # 4个拼接到一个
        sumtemp[seq(1,n_sumtemp,4),]=first
        sumtemp[seq(2,n_sumtemp,4),]=second
        sumtemp[seq(3,n_sumtemp,4),]=third
        sumtemp[seq(4,n_sumtemp,4),]=forth
        
        sumtemp[2,]=-sumtemp[2,]
        sumtemp[4,]=-sumtemp[4,]
        
        patternA[1,]=1;patternA[2,]=2 # 1表示某一手数据使用original
        patternA[3,]=1;patternA[4,]=2
        
        patternB[1,]=2;patternB[2,]=1 # A与B相反
        patternB[3,]=2;patternB[4,]=1
        # 注意相比不滚动的，需要多1个变量用以记录前一个的路径，follow的不一定是（1，8）
        # patternA 刚好记录了路径
        # 2重判断，第一判定是否follow，第2判定follow哪一个
        for (i in seq(5,nrow(sumtemp)-1,4)) {
          # follow的条件:2个负时follow：
          follow_condition=!apply(sumtemp[(i-4):(i-1),],2,function(x){ifelse(sum(x<0)==2,T,F)})
          
          sumtemp[i,]=ifelse(follow_condition,ifelse(patternA[i-4,]==1,sumtemp[i,],-sumtemp[i,]),ifelse(patternA[i-4,]==1,-sumtemp[i,],sumtemp[i,]))
          patternA[i,]=ifelse(follow_condition,ifelse(patternA[i-4,]==1,1,2),ifelse(patternA[i-4,]==1,2,1))
          patternB[i,]=ifelse(follow_condition,ifelse(patternA[i-4,]==1,2,1),ifelse(patternA[i-4,]==1,1,2))
          
          sumtemp[i+1,]=ifelse(follow_condition,ifelse(patternA[i-3,]==1,sumtemp[i+1,],-sumtemp[i+1,]),ifelse(patternA[i-3,]==1,-sumtemp[i+1,],sumtemp[i+1,]))
          patternA[i+1,]=ifelse(follow_condition,ifelse(patternA[i-3,]==1,1,2),ifelse(patternA[i-3,]==1,2,1))
          patternB[i+1,]=ifelse(follow_condition,ifelse(patternA[i-3,]==1,2,1),ifelse(patternA[i-3,]==1,1,2))
          
          sumtemp[i+2,]=ifelse(follow_condition,ifelse(patternA[i-2,]==1,sumtemp[i+2,],-sumtemp[i+2,]),ifelse(patternA[i-2,]==1,-sumtemp[i+2,],sumtemp[i+2,]))
          patternA[i+2,]=ifelse(follow_condition,ifelse(patternA[i-2,]==1,1,2),ifelse(patternA[i-2,]==1,2,1))
          patternB[i+2,]=ifelse(follow_condition,ifelse(patternA[i-2,]==1,2,1),ifelse(patternA[i-2,]==1,1,2))
          
          sumtemp[i+3,]=ifelse(follow_condition,ifelse(patternA[i-1,]==1,sumtemp[i+3,],-sumtemp[i+3,]),ifelse(patternA[i-1,]==1,-sumtemp[i+3,],sumtemp[i+3,]))
          patternA[i+3,]=ifelse(follow_condition,ifelse(patternA[i-1,]==1,1,2),ifelse(patternA[i-1,]==1,2,1))
          patternB[i+3,]=ifelse(follow_condition,ifelse(patternA[i-1,]==1,2,1),ifelse(patternA[i-1,]==1,1,2))
        }
      }
      # plan1019-20-1
      else if(choice==1019201){
        # 4个拼接到一个
        sumtemp[seq(1,n_sumtemp,4),]=first
        sumtemp[seq(2,n_sumtemp,4),]=second
        sumtemp[seq(3,n_sumtemp,4),]=third
        sumtemp[seq(4,n_sumtemp,4),]=forth
      }
      # plan1019-20-2
      else if(choice==1019202){
        # 4个拼接到一个
        second=-second
        forth=-forth
        # 计算新指标AB:
        patternA[seq(1,n_sumtemp,4),]=1
        patternA[seq(2,n_sumtemp,4),]=2
        patternA[seq(3,n_sumtemp,4),]=1
        patternA[seq(4,n_sumtemp,4),]=2
        
        patternB[seq(1,n_sumtemp,4),]=2
        patternB[seq(2,n_sumtemp,4),]=1
        patternB[seq(3,n_sumtemp,4),]=2
        patternB[seq(4,n_sumtemp,4),]=1
        
        sumtemp[seq(1,n_sumtemp,4),]=first
        sumtemp[seq(2,n_sumtemp,4),]=second
        sumtemp[seq(3,n_sumtemp,4),]=third
        sumtemp[seq(4,n_sumtemp,4),]=forth
      }
      # 1019-3~1019-13在负正或正负时follow,与1019-1相反
      else if(!(choice %in% c(1019,10191,10192,1019201,1019202))){
        # 4个拼接到一个
        sumtemp[seq(1,n_sumtemp,4),]=first
        sumtemp[seq(2,n_sumtemp,4),]=second
        sumtemp[seq(3,n_sumtemp,4),]=third
        sumtemp[seq(4,n_sumtemp,4),]=forth
        sumtemp[2,]=-sumtemp[2,]
        patternA[1,]=1;patternA[2,]=2
        patternB[1,]=2;patternB[2,]=1
        for (i in seq(3,nrow(sumtemp)-1,2)) {
          # condition为follow的条件：follow指的使用与上一个formula一样的
          if(choice==10193)condition=!(sumtemp[i-1,]>0&sumtemp[i-2,]<0) # 3与13相反
          else if(choice==10194)condition=!(sumtemp[i-1,]<0&sumtemp[i-2,]>0) #4与12相反
          else if(choice==10195)condition=sumtemp[i-2,]>0 # 5与10相反
          else if(choice==10196)condition=sumtemp[i-1,]>0 # 6与9相反
          else if(choice==10197)condition=sumtemp[i-1,]>0&sumtemp[i-2,]>0 # 7与8相反
          else if(choice==10198)condition=!(sumtemp[i-1,]>0&sumtemp[i-2,]>0)
          else if(choice==10199)condition=sumtemp[i-1,]<0
          else if(choice==101910)condition=sumtemp[i-2,]<0
          else if(choice==101911)condition=sumtemp[i-1,]*sumtemp[i-2,]<0
          else if(choice==101912)condition=sumtemp[i-1,]<0&sumtemp[i-2,]>0
          else if(choice==101913)condition=sumtemp[i-1,]>0&sumtemp[i-2,]<0
          else if(choice==101914){
            condition1=(sumtemp[i-2,]<0&patternA[i-2,]==1)
            condition2=(sumtemp[i-1,]<0&patternA[i-1,]==1)
          }
          else if(choice==101915){
            condition1=(sumtemp[i-2,]>0&patternA[i-2,]==1)
            condition2=(sumtemp[i-1,]>0&patternA[i-1,]==1)
          }
          if(choice!=101914&choice!=101915){condition1=condition;condition2=condition}
          sumtemp[i,]=ifelse(condition1,ifelse(patternA[i-2,]==1,sumtemp[i,],-sumtemp[i,]),ifelse(patternA[i-2,]==1,-sumtemp[i,],sumtemp[i,]))
          patternA[i,]=ifelse(condition1,ifelse(patternA[i-2,]==1,1,2),ifelse(patternA[i-2,]==1,2,1))
          patternB[i,]=ifelse(condition1,ifelse(patternA[i-2,]==1,2,1),ifelse(patternA[i-2,]==1,1,2))
          sumtemp[i+1,]=ifelse(condition2,ifelse(patternA[i-1,]==1,sumtemp[i+1,],-sumtemp[i+1,]),ifelse(patternA[i-1,]==1,-sumtemp[i+1,],sumtemp[i+1,]))
          patternA[i+1,]=ifelse(condition2,ifelse(patternA[i-1,]==1,1,2),ifelse(patternA[i-1,]==1,2,1))
          patternB[i+1,]=ifelse(condition2,ifelse(patternA[i-1,]==1,2,1),ifelse(patternA[i-1,]==1,1,2))
        }
      }
    }
    # 3）每个数据集m计算多个评价指标：
    {
      # 计算连续负 “最大” 的手数
      minus_result[m,]=detect_continue_minus(sumtemp)
      # 计算新指标:等价于AIC、BIC等评价指标
      # 1、正负比例
      plus[m,]=apply(sumtemp, 2, function(x)sum(x>0)) 
      minus[m,]=n_sumtemp-plus[m,]# 数据m下结果负的手数
      # 2、新指标A&B
      #新指标A，当某一手使用formula1时记为1，使用formula8时记为2，正负的话和这一手的final result的正负一致；
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
      # 4、新指标C-1：正负由final result的正负决定，绝对值 为1记为1，其他为2
      patternC1=ifelse(sumtemp==1|sumtemp==-1,1,2)
      new_indexc1[m,]=colSums(patternC1*symbol)
      # 5、新指标c-2: 
      #新指标C-2：相对C（现在为C-1），若final_result 是使用original formula得到的
      #则C-2与C-2相同，若使用的是opposite formula，则“相反”——1记为2，其他记为1：
      # c-2是在C-1基础上根据是否使用opposite formula进行变化,因为patternA记录是否使用opposite，1为origin，2为opposite
      # patternC2为1的条件：sumtemp绝对值为1，且使用original即patternA=1，
      # 或sumtemp绝对值为3，且使用opposite即patternA=2，刚好反过来
      patternC2=ifelse((patternA==1&abs(sumtemp)==1)|(patternA==2&abs(sumtemp)==3),1,2)
      new_indexc2[m,]=colSums(patternC2*symbol)
      # 6、指标D：
      # 第一、二手用1（初始化为1）。第三手根据第一首的final result ，当其为正则第三手为2，否则为1；
      # 第四手根据第二手，若其为正则第四手为2，否则为1;以此类推，56根据34，78根据56，滚动下去
      # 1还是2与横向前一手的正负相关，与“是否反一反”无关；正负还是与final result一样
      patternD=rbind(matrix(rep(1,8*2),ncol = 8),ifelse(symbol>0,2,1)[-c(nrow(symbol),nrow(symbol)-1),])
      new_indexD[m,]=colSums(patternD*symbol)
      # 7、指标E：1、2手固定1；3，4手固定2，正负根据final result
      patternE=matrix(rep(c(rep(c(1,1,2,2),n_sumtemp%/%4),c(1,1,2,2)[0:(n_sumtemp%%4)]),8),ncol = 8)
      new_indexE[m,]=colSums(patternE*symbol)
    } 
    
  }
  
  colnames(minus_result)=c(paste0('formula',1:n_path))
  # result存放最大连续负个数
  result=matrix(nrow =max(minus_result) ,ncol = n_path)
  for (i in 1:(max(minus_result))){
    result[i,]=apply(minus_result,2,function(x)sum(x==i))
  }
  colnames(result)=c(paste0('formula',1:n_path))
  
  # satisfy为某一个formula满足小于等于5的数据集个数：
  satisfy=apply(result[1:5,], 2, sum)
  return(list(minus_result,result,satisfy,plus,minus,
              new_indexA,new_indexB,new_indexA1,new_indexB1,
              new_indexc1,new_indexc2,new_indexD,new_indexE))
}
# 2.6 展示函数
present=function(result,index_c=T){
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
  final_result1=rbind(final_result1,colSums(result[[10]]))
  final_result1=rbind(final_result1,colSums(result[[11]]))
  final_result1=rbind(final_result1,colSums(result[[12]]))
  final_result1=rbind(final_result1,colSums(result[[13]]))
  row.names(final_result1)=c('quantity  ','A_sum','A1_sum','B_sum','B1_sum','plus','minus','C1_sum','C2_sum','D_sum','E_sum')
  first5_A=apply(result[[6]], 2, function(x)str_c(sort(x,decreasing = T)[1:5],collapse = ' '))
  last5_A=apply(result[[6]], 2, function(x)str_c(sort(x,decreasing = F)[1:5],collapse = ' '))
  first5_B=apply(result[[7]], 2, function(x)str_c(sort(x,decreasing = T)[1:5],collapse = ' '))
  last5_B=apply(result[[7]], 2, function(x)str_c(sort(x,decreasing = F)[1:5],collapse = ' '))
  first5_A1=apply(result[[8]], 2, function(x)str_c(sort(x,decreasing = T)[1:5],collapse = ' '))
  last5_A1=apply(result[[8]], 2, function(x)str_c(sort(x,decreasing = F)[1:5],collapse = ' '))
  first5_B1=apply(result[[9]], 2, function(x)str_c(sort(x,decreasing = T)[1:5],collapse = ' '))
  last5_B1=apply(result[[9]], 2, function(x)str_c(sort(x,decreasing = F)[1:5],collapse = ' '))
  first5_c1=apply(result[[10]], 2, function(x)str_c(sort(x,decreasing = T)[1:5],collapse = ' '))
  last5_c1=apply(result[[10]], 2, function(x)str_c(sort(x,decreasing = F)[1:5],collapse = ' '))
  first5_c2=apply(result[[11]], 2, function(x)str_c(sort(x,decreasing = T)[1:5],collapse = ' '))
  last5_c2=apply(result[[11]], 2, function(x)str_c(sort(x,decreasing = F)[1:5],collapse = ' '))
  first5_d=apply(result[[12]], 2, function(x)str_c(sort(x,decreasing = T)[1:5],collapse = ' '))
  last5_d=apply(result[[12]], 2, function(x)str_c(sort(x,decreasing = F)[1:5],collapse = ' '))
  first5_e=apply(result[[13]], 2, function(x)str_c(sort(x,decreasing = T)[1:5],collapse = ' '))
  last5_e=apply(result[[13]], 2, function(x)str_c(sort(x,decreasing = F)[1:5],collapse = ' '))
  
  A=str_c(first5_A,last5_A,sep  = ';')
  B=str_c(first5_B,last5_B,sep  = ';')
  A1=str_c(first5_A1,last5_A1,sep  = ';')
  B1=str_c(first5_B1,last5_B1,sep  = ';')
  c1=str_c(first5_c1,last5_c1,sep  = ';')
  c2=str_c(first5_c2,last5_c2,sep  = ';')
  d=str_c(first5_d,last5_d,sep  = ';')
  e=str_c(first5_e,last5_e,sep  = ';')
  ss=data.frame(A=A,A1=A1,B=B,B1=B1,C1=c1,C2=c2,D=d,E=e)
  row.names(ss)=c(paste0('formula',1:8))
  colnames(ss)=c('first&last5 of index A','first&last5 of index A1','first&last5 of index B','first&last5 of index B1',
                 'first&last5 of index C1','first&last5 of index C2','first&last5 of index D','first&last5 of index E')
  if(index_c){
    print(ss)
    cat('\n')
    print(final_result)
    print(final_result1)
  }
  else{
    print(ss[c('first&last5 of index A','first&last5 of index A1','first&last5 of index B','first&last5 of index B1',
               'first&last5 of index D','first&last5 of index E')])
    cat('\n')
    print(final_result)
    print(final_result1[c('quantity  ','A_sum','A1_sum','B_sum','B1_sum','plus','minus','D_sum','E_sum'),])
  }
}

# write.csv(as.data.frame(sumtemp[,2]),'C:\\Users\\Think\\Desktop\\1.csv',row.names = F)
####################################################
# 顺+不交叉：
plan1019201_YAY579=run(data = all_data,path = path_BP_YAY579,inverse = F,choice=1019201,rolling=T,reflection=F,cross = F)
present(plan1019201_YAY579)
# 顺序+交叉（twist）：
plan1019201_YAY579_cross=run(data = all_data,path = path_BP_YAY579,inverse = F,choice=1019201,rolling=T,reflection=F,cross = T)
present(plan1019201_YAY579_cross)
# 逆+不交叉：
plan1019201_YAY579_inverse=run(data = all_data,path = path_BP_YAY579,inverse = T,choice=1019201,rolling=T,reflection=F,cross = F)
present(plan1019201_YAY579_inverse)
# 逆+交叉（twist）：
plan1019201_YAY579_inverse_cross=run(data = all_data,path = path_BP_YAY579,inverse = T,choice=1019201,rolling=T,reflection=F,cross = T)
present(plan1019201_YAY579_inverse_cross)




# plan1211-1~4=======================================================
plan1211_1_YAY579=run(data = all_data,path = path_BP_YAY579,inverse = F,choice=12111,rolling=F,reflection=F,cross = F)
present(plan1211_1_YAY579,index_c = F)
plan1211_2_YAY579=run(data = all_data,path = path_BP_YAY579,inverse = F,choice=12112,rolling=F,reflection=F,cross = F)
present(plan1211_2_YAY579,index_c = F)
plan1211_3_YAY579=run(data = all_data,path = path_BP_YAY579,inverse = F,choice=12113,rolling=F,reflection=F,cross = F)
present(plan1211_3_YAY579,index_c = F)
plan1211_4_YAY579=run(data = all_data,path = path_BP_YAY579,inverse = F,choice=12114,rolling=F,reflection=F,cross = F)
present(plan1211_4_YAY579,index_c = F)


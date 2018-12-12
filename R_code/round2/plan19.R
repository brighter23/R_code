# 1、formula准备------------------------------------------
data=read.csv('data\\treat_two_hundred.csv',header = T)
data1=read.csv('data\\new.csv',header = T)
data2=read.csv('data\\new1.csv',header = T)
data3=read.csv('data\\new2.csv',header = T)
data1=data1[,-23]
all_data=cbind(data,data1,data2,data3)
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
  R41='20-30-31|20-21-31|11-12-13|11-21-31|02-03-13|02-12-13'
  R42='20-30-40|20-21-22|11-12-22|11-21-22|02-03-04|02-12-22'
  R21='20|02'
  R22='11'
  
  path1=matrix(c(R61,R81,R101, R61,R82,R101, R61,R81,R102, R61,R82,R102, R62,R81,R101, R62,R82,R101, R62,R81,R102, R62,R82,R102),byrow = T,ncol = 3)
  colnames(path1)=c('R6','R8','R10')
  path2=matrix(c(R21,R41,R61, R21,R42,R61, R21,R41,R62, R21,R42,R62, R22,R41,R61, R22,R42,R61, R22,R41,R62, R22,R42,R62),byrow = T,ncol = 3)
  colnames(path2)=c('R2','R4','R6')
  minus_path1=matrix(c(R62,R82,R102, R62,R81,R102, R62,R82,R101, R62,R81,R101, R61,R82,R102, R61,R81,R102, R61,R82,R101, R61,R81,R101),byrow = T,ncol = 3)
  colnames(minus_path1)=c('R6','R8','R10')
  minus_path2=matrix(c(R22,R42,R62, R22,R41,R62, R22,R42,R61, R22,R41,R61, R21,R42,R62, R21,R41,R62, R21,R42,R61, R21,R41,R61),byrow = T,ncol = 3)
  colnames(minus_path2)=c('R2','R4','R6')
}
c=c('data','all_data','path1','minus_path1','path2','minus_path2')
a=ls()
rm(list=(setdiff(a,c)))
gc()
if(!require(stringr))install.packages('stringr')
library(stringr)
# 2、训练----------------------------------------------

# 2.1定义计算每10个真实数据下的真实路径final_comb:
# 以及每6个真实数据下的真实路径final_comb:
true_path1=function(dataset){
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
true_path2=function(dataset){
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
  comb3=paste0(result6)
  return(matrix(c(comb1,comb2,comb3),ncol = 3))
  
}
# 2.2定义9个真实数据下根据真实路径下的量化值-1或1:
new_quantize1=function(path=path1,real_comb){
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
new_quantize2=function(path=path2,real_comb){
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
  return(r6+r4+r2)
}
# 2.3取数据集并量化：
data_sort=function(data=data,m,path1,path2){
  n=nrow(data)
  # 第一手
  {
    first_hand=matrix(ncol = nrow(path1) )
    for (i in seq(10,n,4)) {
      real_comb=true_path1(data[(i-9):i,m])
      first_hand_quantity=new_quantize1(path1,real_comb)
      first_hand=rbind(first_hand,first_hand_quantity)
    }
    first_hand=first_hand[-1,]
  }
  # 第二手
  {
    second_hand=matrix(ncol = nrow(path1) )
    for (i in seq(11,n,4)) {
      real_comb=true_path1(data[(i-9):i,m])
      second_hand_quantity=new_quantize1(path1,real_comb)
      second_hand=rbind(second_hand,second_hand_quantity)
    }
    second_hand=second_hand[-1,]
  }
  # 第三手
  {
    third_hand=matrix(ncol = nrow(path1) )
    for (i in seq(12,n,4)) {
      real_comb=true_path2(data[seq(i-10,i,2),m])
      third_hand_quantity=new_quantize2(path2,real_comb)
      third_hand=rbind(third_hand,third_hand_quantity)
    }
    third_hand=third_hand[-1,]
    
  }
  # 第四手
  {
    forth_hand=matrix(ncol = nrow(path1) )
    for (i in seq(13,n,4)) {
      real_comb=true_path2(data[seq(i-10,i,2),m])
      forth_hand_quantity=new_quantize2(path2,real_comb)
      forth_hand=rbind(forth_hand,forth_hand_quantity)
    }
    forth_hand=forth_hand[-1,]
    
  }
  return(list(first_hand,second_hand,third_hand,forth_hand))
}

# 2.3求每个formula下最大连负是多少
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

run=function(data=data,path1,path2,seq){
  n_path1=nrow(path1)
  n_path2=nrow(path2)
  n_path=n_path1*n_path2
  # 1、minus_result行代表数据集索引，列代表formula，每格表示最大连负数目
  minus_result=matrix(nrow =length(data),ncol = n_path)
  for (m in 1:length(data)) {
    if(m%%50==0)print(m)
    temp=data_sort(data,m,path1 = path1,path2 = path2)
    first=temp[[1]]
    second=temp[[2]]
    
    third=temp[[3]]
    forth=temp[[4]]
    # 合并成final_result
    n=nrow(first)+nrow(second)+nrow(third)+nrow(forth)
    if(seq==0){
     k=1
    }
    else if (seq==1){
      second=ifelse(first<0,second,-second)
      forth=ifelse(third[-nrow(third),]<0,forth,-forth)
    }
    else if (seq==2){
      second=ifelse(first>0,second,-second)
      # third=ifelse(second<0,third,-third)
      forth=ifelse(third[-nrow(third),]<0,forth,-forth)
    }
    else if (seq==3){
      second=ifelse(first>0,second,-second)
      third=ifelse(second>0,third,-third)
      forth=ifelse(third[-nrow(third),]<0,forth,-forth)
    }
    else{
      print('error')
      break
    }
    # 合并成sumtemp
    {
      final_first=matrix(rep(first,8),nrow = nrow(first))
      final_second=matrix(rep(second,8),nrow = nrow(second))
      final_third=matrix(nrow =nrow(third),ncol= n_path)
      for (i in 1:ncol(third)){
        final_third[,(i*8-7):(i*8)]= matrix(rep(third[,i],8),nrow = nrow(third))
      }
      final_forth=matrix(nrow =nrow(forth),ncol= n_path )
      for (i in 1:ncol(forth)){
        final_forth[,(i*8-7):(i*8)]= matrix(rep(forth[,i],8),nrow = nrow(forth))
      }
      
      sumtemp=matrix(nrow = n,ncol =n_path)
      sumtemp[seq(1,n,4),]= final_first
      sumtemp[seq(2,n,4),]= final_second
      sumtemp[seq(3,n,4),]= final_third
      sumtemp[seq(4,n,4),]= final_forth
    }
    # sumtemp[,1];table(detect_continue_minus(sumtemp))
    minus_result[m,]=detect_continue_minus(sumtemp)
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
  return(list(minus_result,result,satisfy))
}

#基础版：
plan0=run(all_data,path1 = path1,path2 = path2,0);max(plan0[[3]])
# plan19:
plan1=run(all_data,path1,path2,1);sort(plan1[[3]],decreasing = T)
# 2根据1使用我的版本
plan2=run(all_data,path1,path2,2);sort(plan2[[3]],decreasing = T)

plan3=run(all_data,path1,path2,3);max(plan3[[3]])

run(all_data,path,5)

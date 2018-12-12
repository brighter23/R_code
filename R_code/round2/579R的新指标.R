# 1、formula准备------------------------------------------
data=read.csv('data\\treat_two_hundred.csv',header = T)
data1=read.csv('data\\new.csv',header = T)
data2=read.csv('data\\new1.csv',header = T)
data3=read.csv('data\\new2.csv',header = T)
data1=data1[,-23]
all_data=cbind(data,data1,data2,data3)
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
  path=matrix(c(A9,A7,A5, A9,B7,A5, A9,A7,B5, A9,B7,B5, B9,A7,A5, B9,B7,A5, B9,A7,B5, B9,B7,B5),byrow = T,ncol = 6)
  colnames(path)=c('B_R9','P_R9','B_R7','P_R7','B_R5','P_R5')
  minus_path=matrix(c(B9,B7,B5, B9,A7,B5, B9,B7,A5, B9,A7,A5, A9,B7,B5, A9,A7,B5, A9,B7,A5, A9,A7,A5),byrow = T,ncol = 6)
  colnames(minus_path)=c('B_R9','P_R9','B_R7','P_R7','B_R5','P_R5')
}
library(stringr)
# 2、训练==================================================

# 2.1定义计算每9个真实数据下的真实路径final_comb:
true_path=function(dataset){
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
# 2.2定义9个真实数据下根据真实路径下的量化值-1或1:
new_quantize=function(path,real_comb,set){
  r9=matrix(0,ncol = nrow(path),nrow = 1)
  for (j in 1:nrow(path)) {
    r9[1,j]=ifelse(set[1]=='B',ifelse(str_detect(real_comb[1,1],path[j,'B_R9']),1,-1),ifelse(str_detect(real_comb[1,1],path[j,'P_R9']),1,-1))
  }
  r7=matrix(0,ncol = nrow(path),nrow = 1)
  for (j in 1:nrow(path)) {
    r7[1,j]=ifelse(set[3]=='B',ifelse(str_detect(real_comb[1,2],path[j,'B_R7']),1,-1),ifelse(str_detect(real_comb[1,2],path[j,'P_R7']),1,-1))
  }
  r5=matrix(0,ncol = nrow(path),nrow = 1)
  for (j in 1:nrow(path)) {
    r5[1,j]=ifelse(set[5]=='B',ifelse(str_detect(real_comb[1,3],path[j,'B_R5']),1,-1),ifelse(str_detect(real_comb[1,3],path[j,'P_R5']),1,-1))
  }
  return(r9+r7+r5)
}
# 2.3取数据集并量化：
data_sort=function(data=data,m,path=path){
  n=nrow(data)
  # 第一手
  {
    first_hand=matrix(ncol = nrow(path) )
    for (i in seq(17,n,2)) {
      real_comb=true_path(data[seq(i-16,i,2),m])
      first_hand_quantity=new_quantize(path,real_comb,set =data[seq(i-16,i,2),m] )
      first_hand=rbind(first_hand,first_hand_quantity)
    }
    first_hand=first_hand[-1,]
    
  }
  # 第二手
  {
    second_hand=matrix(ncol = nrow(path) )
    for (i in seq(18,n,2)) {
      real_comb=true_path(data[seq(i-16,i,2),m])
      second_hand_quantity=new_quantize(path,real_comb,set =data[seq(i-16,i,2),m] )
      second_hand=rbind(second_hand,second_hand_quantity)
    }
    second_hand=second_hand[-1,]
    
  }
  
  return(list(first_hand,second_hand))
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

run=function(data=data,path=path,seq){
  n_path=nrow(path)
  # 1、minus_result行代表数据集索引，列代表formula，每格表示最大连负数目
  minus_result=matrix(nrow =length(data),ncol = n_path)
  new_index=matrix(nrow =length(data),ncol = n_path)
  for (m in 1:length(data)) {
    if(m%%50==0)print(m)
    temp=data_sort(data,m,path)
    first=temp[[1]] # 奇数轨
    second=temp[[2]] # 偶数轨
    # 合并成final_result
    n=nrow(first)+nrow(second)
    # 新指标的模式（1或2）和符号（正负）
    pattern=matrix(nrow = n,ncol =nrow(path))
    
    if(seq==0){
      # 基础版，不进行反一反：
      sumtemp=matrix(nrow = n,ncol =nrow(path))
      sumtemp[seq(1,n,2),]=first
      sumtemp[seq(2,n,2),]=second
      
    }
    else if (seq==1){
      second=ifelse(first<0,second,-second)
    }
    else if (seq==2){
      first[seq(2,nrow(first),2),]=ifelse(first[seq(1,nrow(first),2),]<0,first[seq(2,nrow(first),2),],-first[seq(2,nrow(first),2),])
      second[seq(1,nrow(second),2),]=ifelse(first[seq(1,nrow(first),2),]<0,second[seq(1,nrow(second),2),],-second[seq(1,nrow(second),2),])
      second[seq(2,nrow(second),2),]=ifelse(first[seq(1,nrow(first),2),]* second[seq(1,nrow(second),2),]>0,second[seq(2,nrow(second),2),],-second[seq(2,nrow(second),2),])
      pattern[seq(1,n,4),]=1 # 奇数轨奇数行固定使用pattern A  
      pattern[seq(2,n,4),]=ifelse(first[seq(1,nrow(first),2),]<0,1,2) # 偶数轨奇数行根据奇数轨奇数行
      pattern[seq(3,n,4),]=ifelse(first[seq(1,nrow(first),2),]<0,1,2) # 奇数轨偶数行根据奇数轨奇数行
      pattern[seq(4,n,4),]=ifelse(first[seq(1,nrow(first),2),]* second[seq(1,nrow(second),2),]>0,1,2) # 偶数轨偶数行根据 奇数行奇数轨、偶数轨奇数行共同决定
    }
    else if(seq==3){
      first[seq(2,nrow(first),2),]=ifelse(first[seq(1,nrow(first),2),]<0,first[seq(2,nrow(first),2),],-first[seq(2,nrow(first),2),])
      second[seq(2,nrow(second),2),]=ifelse(second[seq(1,nrow(second),2),]<0,second[seq(2,nrow(second),2),],-second[seq(2,nrow(second),2),])
      
      pattern[seq(1,n,4),]=1 # 奇数轨奇数行固定使用pattern A  
      pattern[seq(2,n,4),]=1 # 偶数轨奇数行固定使用pattern A 
      pattern[seq(3,n,4),]=ifelse(first[seq(1,nrow(first),2),]<0,1,2) # 奇数轨偶数行根据奇数轨奇数行
      pattern[seq(4,n,4),]=ifelse(second[seq(1,nrow(second),2),]<0,1,2) # 偶数轨偶数行根据偶数轨奇数行
    }
    else if(seq==4){
      second[seq(1,nrow(second),2),]=-second[seq(1,nrow(second),2),]
      first[seq(2,nrow(first),2),]=ifelse(first[seq(1,nrow(first),2),]<0,first[seq(2,nrow(first),2),],-first[seq(2,nrow(first),2),])
      second[seq(2,nrow(second),2),]=ifelse(second[seq(1,nrow(second),2),]<0,second[seq(2,nrow(second),2),],-second[seq(2,nrow(second),2),])
      
      pattern[seq(1,n,4),]=1 # 奇数轨奇数行固定使用pattern A  
      pattern[seq(2,n,4),]=2 # 偶数轨奇数行固定使用pattern A 
      pattern[seq(3,n,4),]=ifelse(first[seq(1,nrow(first),2),]<0,1,2) # 奇数轨偶数行根据奇数轨奇数行
      pattern[seq(4,n,4),]=ifelse(second[seq(1,nrow(second),2),]<0,1,2) # 偶数轨偶数行根据偶数轨奇数行
    }
    sumtemp=matrix(nrow = n,ncol =nrow(path))
    sumtemp[seq(1,n,2),]=first
    sumtemp[seq(2,n,2),]=second
    # 正负符号直接根据是否匹配pattern的结果来
    symbol=matrix(nrow = n,ncol =nrow(path))
    symbol[seq(1,n,2),]=ifelse(first>0,1,-1)
    symbol[seq(2,n,2),]=ifelse(second>0,1,-1)
    
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

# 单纯双轨：没有反一反，540
R579R_new_index_0=run(all_data,path,0);R579R_new_index_0[[3]]
# plan3结果：536

R579R_new_index_1=run(all_data,path,1);R579R_new_index_1[[3]]
# plan11:558

R579R_new_index_2=run(all_data,path,2);R579R_new_index_2[[3]]
for(i in 1:8){
  print(sum(R579R_new_index_2[[4]][,i]>=0)/760)
}
# 极端情况：前后5个
sort(R579R_new_index_2[[4]][,7])[756:760];sort(R579R_new_index_2[[4]][,7])[1:5]

# plan13:556
R579R_new_index_3=run(all_data,path,3);R579R_new_index_3[[3]]
for(i in 1:8){
  print(sum(R579R_new_index_3[[4]][,i]>=0)/760)
}
# 极端情况：前后5个
sort(R579R_new_index_3[[4]][,7])[756:760];sort(R579R_new_index_3[[4]][,7])[1:5]

# plan13-1:
R579R_new_index_31=run(all_data,path,4);R579R_new_index_31[[3]]
sort(R579R_new_index_31[[4]][,8])[756:760];sort(R579R_new_index_31[[4]][,8])[1:5]

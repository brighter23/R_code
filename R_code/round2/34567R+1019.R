# 一、formula准备====================================================
# 1.1 读取“训练集”===================================================
data=read.csv('data\\treat_two_hundred.csv',header = T)
data1=read.csv('data\\new.csv',header = T)
data2=read.csv('data\\new1.csv',header = T)
data3=read.csv('data\\new2.csv',header = T)
data1=data1[,-23]
all_data=cbind(data,data1,data2,data3)
# 1.2构建“回归方程”——formula==========================================
{
  library(stringr)
  formula=read.csv2('data\\new34567R.csv',header = F,stringsAsFactors = F)
  # 拼接函数：
  joint=function(vec){
    # 先根据逗号切割，后再使用-为连接符
    a=sapply(vec, function(x)str_c(str_split(x,pattern = ',',simplify = T),collapse = '-'))
    # 最后使用|将所有可能的形式以“或”形式组织
    return(str_c(a,collapse = '|'))
  }
  R3A_B=joint(formula[2:3,]);R3A_P=joint(formula[5:6,])
  R3A=c(R3A_B,R3A_P)
  R3B_B=joint(formula[8:9,]);R3B_P=joint(formula[11:12,])
  R3B=c(R3B_B,R3B_P)
  R4A_B=joint(formula[14:17,]);R4A_P=joint(formula[19:22,])
  R4A=c(R4A_B,R4A_P)
  R4B_B=joint(formula[24:27,]);R4B_P=joint(formula[29:32,])
  R4B=c(R4B_B,R4B_P)
  R5A_B=joint(formula[34:39,]);R5A_P=joint(formula[41:46,])
  R5A=c(R5A_B,R5A_P)
  R5B_B=joint(formula[48:53,]);R5B_P=joint(formula[55:60,])
  R5B=c(R5B_B,R5B_P)
  R6A_B=joint(formula[62:69,]);R6A_P=joint(formula[71:78,])
  R6A=c(R6A_B,R6A_P)
  R6B_B=joint(formula[80:87,]);R6B_P=joint(formula[89:96,])
  R6B=c(R6B_B,R6B_P)
  R7A_B=joint(formula[98:107,]);R7A_P=joint(formula[109:118,])
  R7A=c(R7A_B,R7A_P)
  R7B_B=joint(formula[120:129,]);R7B_P=joint(formula[131:140,])
  R7B=c(R7B_B,R7B_P)

  # 全部分BP的路径：
  R3=matrix(rep(c(rep(R3A,16),rep(R3B,16)),1),byrow = T,ncol = 2)
  R4=matrix(rep(c(rep(R4A,8),rep(R4B,8)),2),byrow = T,ncol = 2)
  R5=matrix(rep(c(rep(R5A,4),rep(R5B,4)),4),byrow = T,ncol = 2)
  R6=matrix(rep(c(rep(R6A,2),rep(R6B,2)),8),byrow = T,ncol = 2)
  R7=matrix(rep(c(R7A,R7B),16),byrow = T,ncol = 2)
  path_BP=cbind(R3,R4);path_BP=cbind(path_BP,R5);path_BP=cbind(path_BP,R6);path_BP=cbind(path_BP,R7)
  colnames(path_BP)=c('B_R3','P_R3','B_R4','P_R4','B_R5','P_R5','B_R6','P_R6','B_R7','P_R7')
  
  # 4R和6R不分BP的路径：
  R4A=joint(formula[142:147,]);R4B=joint(formula[149:154,])
  R6A=joint(formula[156:165,]);R6B=joint(formula[167:176,])
  
  R3=matrix(rep(c(rep(R3A,16),rep(R3B,16)),1),byrow = T,ncol = 2)
  R4=matrix(rep(c(rep(R4A,8),rep(R4B,8)),2),byrow = T,ncol = 1)
  R5=matrix(rep(c(rep(R5A,4),rep(R5B,4)),4),byrow = T,ncol = 2)
  R6=matrix(rep(c(rep(R6A,2),rep(R6B,2)),8),byrow = T,ncol = 1)
  R7=matrix(rep(c(R7A,R7B),16),byrow = T,ncol = 2)
  path=cbind(R3,R4);path=cbind(path,R5);path=cbind(path,R6);path=cbind(path,R7)
  colnames(path)=c('B_R3','P_R3','R4','B_R5','P_R5','R6','B_R7','P_R7')
  
}
c=c('data','all_data','path_BP','path')
a=ls()
rm(list=(setdiff(a,c)))
gc()
library(stringr)
# 二、训练===========================================================
# 2.1定义计算每7个真实数据下的真实路径final_comb:======================
# 在顺序下true_path计算真实路径的方式是连续的，但在逆序时就是断开的

true_path=function(dataset){
  # 计算7R
  t0=table(dataset[1:7])
  result0=paste0(t0[1],t0[2])
  t1=table(dataset[1:6])
  result1=paste0(t1[1],t1[2])
  t2=table(dataset[1:5])
  result2=paste0(t2[1],t2[2])
  comb1=paste0(result2,'-',result1,'-',result0)
  # 计算6R
  t3=table(dataset[2:7])
  result3=paste0(t3[1],t3[2])
  t4=table(dataset[2:6])
  result4=paste0(t4[1],t4[2])
  t5=table(dataset[2:5])
  result5=paste0(t5[1],t5[2])
  comb2=paste0(result5,'-',result4,'-',result3)
  # 计算5R
  t6=table(dataset[3:7])
  result6=paste0(t6[1],t6[2])
  t7=table(dataset[3:6])
  result7=paste0(t7[1],t7[2])
  t8=table(dataset[3:5])
  result8=paste0(t8[1],t8[2])
  comb3=paste0(result8,'-',result7,'-',result6)
  # 计算4R
  t9=table(dataset[4:7])
  result9=paste0(t9[1],t9[2])
  t10=table(dataset[4:6])
  result10=paste0(t10[1],t10[2])
  t11=table(dataset[4:5])
  result11=paste0(t11[1],t11[2])
  comb4=paste0(result11,'-',result10,'-',result9)
  # 计算3R
  t12=table(dataset[5:7])
  result12=paste0(t12[1],t12[2])
  t13=table(dataset[5:6])
  result13=paste0(t13[1],t13[2])
  t14=table(dataset[5:5])
  result14=paste0(t14[1],t14[2])
  comb5=paste0(result14,'-',result13,'-',result12)
  return(matrix(c(comb5,comb4,comb3,comb2,comb1),ncol = 5))
} 
# true_path(data[1:7,1])
# 2.2 定义7个真实数据下的量化函数quantize：==============================
# 返回矩阵，计算32个回归方程下的满足程度（r7满足时为1，不满足为-1）的求和（r7+r6+r5+r4+r3）
quantize=function(path,set){
  real_comb=true_path(set) #计算实际数据路径
  n_row_path=ncol(path)
  # 分BP的path_BP有10列：
  if(ncol(path)==10){
    r7=matrix(0,ncol = n_row_path,nrow = 1)
    for (j in 1:n_row_path) {
      # 因为根据数据的开头得BP来判定
      r7[1,j]=ifelse(set[1]=='B'|set[1]==1,ifelse(str_detect(real_comb[1,5],path[j,'B_R7']),1,-1),ifelse(str_detect(real_comb[1,5],path[j,'P_R7']),1,-1))
    }
    r6=matrix(0,ncol = n_row_path,nrow = 1)
    for (j in 1:n_row_path) {
      r6[1,j]=ifelse(set[2]=='B'|set[2]==1,ifelse(str_detect(real_comb[1,4],path[j,'B_R6']),1,-1),ifelse(str_detect(real_comb[1,4],path[j,'P_R6']),1,-1))
    }
    r5=matrix(0,ncol = n_row_path,nrow = 1)
    for (j in 1:n_row_path) {
      r5[1,j]=ifelse(set[3]=='B'|set[3]==1,ifelse(str_detect(real_comb[1,3],path[j,'B_R5']),1,-1),ifelse(str_detect(real_comb[1,3],path[j,'P_R5']),1,-1))
    }
    r4=matrix(0,ncol = n_row_path,nrow = 1)
    for (j in 1:n_row_path) {
      r4[1,j]=ifelse(set[4]=='B'|set[4]==1,ifelse(str_detect(real_comb[1,2],path[j,'B_R4']),1,-1),ifelse(str_detect(real_comb[1,2],path[j,'P_R4']),1,-1))
    }
    r3=matrix(0,ncol = n_row_path,nrow = 1)
    for (j in 1:n_row_path) {
      r3[1,j]=ifelse(set[5]=='B'|set[5]==1,ifelse(str_detect(real_comb[1,1],path[j,'B_R3']),1,-1),ifelse(str_detect(real_comb[1,1],path[j,'P_R3']),1,-1))
    }
  }
  else{
    r7=matrix(0,ncol = nrow(path),nrow = 1)
    for (j in 1:nrow(path)) {
      r7[1,j]=ifelse(set[1]=='B'|set[1]==1,ifelse(str_detect(real_comb[1,5],path[j,'B_R7']),1,-1),ifelse(str_detect(real_comb[1,5],path[j,'P_R7']),1,-1))
    }
    r6=matrix(0,ncol = nrow(path),nrow = 1)
    for (j in 1:nrow(path)) {
      r6[1,j]=ifelse(str_detect(real_comb[1,4],path[j,'R6']),1,-1)
    }
    r5=matrix(0,ncol = nrow(path),nrow = 1)
    for (j in 1:nrow(path)) {
      r5[1,j]=ifelse(set[3]=='B'|set[3]==1,ifelse(str_detect(real_comb[1,3],path[j,'B_R5']),1,-1),ifelse(str_detect(real_comb[1,3],path[j,'P_R5']),1,-1))
    }
    r4=matrix(0,ncol = nrow(path),nrow = 1)
    for (j in 1:nrow(path)) {
      r4[1,j]=ifelse(str_detect(real_comb[1,2],path[j,'R4']),1,-1)
    }
    r3=matrix(0,ncol = nrow(path),nrow = 1)
    for (j in 1:nrow(path)) {
      r3[1,j]=ifelse(set[5]=='B'|set[5]==1,ifelse(str_detect(real_comb[1,1],path[j,'B_R3']),1,-1),ifelse(str_detect(real_comb[1,1],path[j,'P_R3']),1,-1))
    }
  }
  return(r7+r6+r5+r4+r3)
} 
# quantize(path_BP,data[1:7,1])
# 因此这里取值[-5,-3,-1,1,3,5],相比原来的3个的前后多了-5和正5
# 数值越大，说明该formula（回归方程）拟合的越好，捕捉到了数据的变化规律

# 2.3 定义诊断最多连续负的多少手：================================================
detect_continue_minus=function(sumtemp){
  continue_minus_num=c()
  for (i in 1:ncol(sumtemp)) {
    # 转化成字符串，查看多少个“0”在字符串内，例如”00000“这部分是否在字符串内
    strings=str_c(ifelse(sumtemp[,i]<0,0,1),collapse = '')
    for (k in 1:nrow(sumtemp)) {
      pattern=str_c(paste0(rep(0,k)),collapse = '')
      if(!str_detect(strings,pattern)){
        # print(paste0('最大连续为负个数',k-1))
        continue_minus_num[i]=k-1
        break
      }
    }
  }
  return(continue_minus_num)
}
# 2.4 取数据方式：每个样本(760个)为一串的“BP”的字符串，即为“特征”=====
# 从21手开始，都为顺序：
sort_allsequence=function(data=data,m,path=path){
  n=nrow(data)
  # 第一手：显+顺
  {
    first_hand=matrix(ncol = nrow(path))
    for (i in seq(21,n,4)) {
      first_hand_quantity=quantize(path,set = data[c(seq(i-12,i-2,2),i),m])
      first_hand=rbind(first_hand,first_hand_quantity)
    }
    first_hand=first_hand[-1,]
  }
  # 第二手：显+顺
  { 
    second_hand=matrix(ncol = nrow(path) )
    for (i in seq(22,n,4)) {
      second_hand_quantity=quantize(path,set = data[c(seq(i-12,i-2,2),i),m])
      second_hand=rbind(second_hand,second_hand_quantity)
    }
    second_hand=second_hand[-1,]
  }
  # 第三手：显+顺
  {
    third_hand=matrix(ncol = nrow(path))
    for (i in seq(23,n,4)) {
      third_hand_quantity=quantize(path,set = data[c(seq(i-12,i-2,2),i),m])
      third_hand=rbind(third_hand,third_hand_quantity)
    }
    third_hand=third_hand[-1,]
  }
  # 第四手：显+顺
  { 
    forth_hand=matrix(ncol = nrow(path) )
    for (i in seq(24,n,4)) {
      forth_hand_quantity=quantize(path,set = data[c(seq(i-12,i-2,2),i),m])
      forth_hand=rbind(forth_hand,forth_hand_quantity)
    }
    forth_hand=forth_hand[-1,]
  }
  return(list(first_hand,second_hand,third_hand,forth_hand))
}
# 从13手开始，都为顺序：
sort_allsequence_thirteen=function(data=data,m,path=path){
  n=nrow(data)
  # 第一手：显+顺
  {
    first_hand=matrix(ncol = nrow(path))
    for (i in seq(13,n,4)) {
      first_hand_quantity=quantize(path,set = data[c(seq(i-12,i-2,2),i),m])
      first_hand=rbind(first_hand,first_hand_quantity)
    }
    first_hand=first_hand[-1,]
  }
  # 第二手：显+顺
  { 
    second_hand=matrix(ncol = nrow(path) )
    for (i in seq(14,n,4)) {
      second_hand_quantity=quantize(path,set = data[c(seq(i-12,i-2,2),i),m])
      second_hand=rbind(second_hand,second_hand_quantity)
    }
    second_hand=second_hand[-1,]
  }
  # 第三手：显+顺
  {
    third_hand=matrix(ncol = nrow(path))
    for (i in seq(15,n,4)) {
      third_hand_quantity=quantize(path,set = data[c(seq(i-12,i-2,2),i),m])
      third_hand=rbind(third_hand,third_hand_quantity)
    }
    third_hand=third_hand[-1,]
  }
  # 第四手：显+顺
  { 
    forth_hand=matrix(ncol = nrow(path) )
    for (i in seq(16,n,4)) {
      forth_hand_quantity=quantize(path,set = data[c(seq(i-12,i-2,2),i),m])
      forth_hand=rbind(forth_hand,forth_hand_quantity)
    }
    forth_hand=forth_hand[-1,]
  }
  return(list(first_hand,second_hand,third_hand,forth_hand))
}
# 2.5 运行的主程序run===============================================
run=function(data=data,path=path,data_sort,choice=1019,rolling=F){
  library(stringr)
  n_path=nrow(path)
  minus_result=matrix(nrow =length(data),ncol = n_path)
  plus=matrix(nrow =length(data),ncol = n_path)
  minus=matrix(nrow =length(data),ncol = n_path)
  new_indexA=matrix(nrow =length(data),ncol = n_path)
  new_indexB=matrix(nrow =length(data),ncol = n_path)
  new_indexA1=matrix(nrow =length(data),ncol = n_path)
  new_indexB1=matrix(nrow =length(data),ncol = n_path)
  new_indexc=matrix(nrow =length(data),ncol = n_path)
  for (m in 1:length(data)) {
    if(m%%50==0)print(m)
    if(data_sort==21){
      temp=sort_allsequence(data,m,path)
    }
    else if(data_sort==13){
      temp=sort_allsequence_thirteen(data,m,path)
    }
    # 初始化变量，以记录指标：
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
    # 101911 在负正或正负时follow,与1019-1相反
    else if(choice==101911){
      # 4个拼接到一个
      sumtemp[seq(1,n_sumtemp,4),]=first
      sumtemp[seq(2,n_sumtemp,4),]=second
      sumtemp[seq(3,n_sumtemp,4),]=third
      sumtemp[seq(4,n_sumtemp,4),]=forth
      sumtemp[2,]=-sumtemp[2,]
      patternA[1,]=1;patternA[2,]=2
      patternB[1,]=2;patternB[2,]=1
      for (i in seq(3,nrow(sumtemp)-1,2)) {
        condition=sumtemp[i-1,]*sumtemp[i-2,]<0
        sumtemp[i,]=ifelse(condition,ifelse(patternA[i-2,]==1,sumtemp[i,],-sumtemp[i,]),ifelse(patternA[i-2,]==1,-sumtemp[i,],sumtemp[i,]))
        patternA[i,]=ifelse(condition,ifelse(patternA[i-2,]==1,1,2),ifelse(patternA[i-2,]==1,2,1))
        patternB[i,]=ifelse(condition,ifelse(patternA[i-2,]==1,2,1),ifelse(patternA[i-2,]==1,1,2))
        sumtemp[i+1,]=ifelse(condition,ifelse(patternA[i-1,]==1,sumtemp[i+1,],-sumtemp[i+1,]),ifelse(patternA[i-1,]==1,-sumtemp[i+1,],sumtemp[i+1,]))
        patternA[i+1,]=ifelse(condition,ifelse(patternA[i-1,]==1,1,2),ifelse(patternA[i-1,]==1,2,1))
        patternB[i+1,]=ifelse(condition,ifelse(patternA[i-1,]==1,2,1),ifelse(patternA[i-1,]==1,1,2))
      }
    }
    else if(choice==0){
      # 4个拼接到一个
      sumtemp[seq(1,n_sumtemp,4),]=first
      sumtemp[seq(2,n_sumtemp,4),]=second
      sumtemp[seq(3,n_sumtemp,4),]=third
      sumtemp[seq(4,n_sumtemp,4),]=forth
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
    # 4、新指标C：正负由final result的正负决定，绝对值 为1记为1，若为3或5记为2
    patternC=ifelse(sumtemp==1|sumtemp==-1,1,2)
    new_indexc[m,]=colSums(patternC*symbol)
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
  return(list(minus_result,result,satisfy,plus,minus,new_indexA,new_indexB,new_indexA1,new_indexB1,new_indexc))
}
# 2.6 展示函数:因为有32个，分成4*8个展示
present=function(result,index=c(1,8)){
  print(result[[2]][,index[1]:index[2]])
  cat('\n')
  final_result=rbind(round(apply(result[[6]][,index[1]:index[2]], 2, function(x)sum(x>=0))/760,3),round(apply(result[[8]][,index[1]:index[2]], 2, function(x)sum(x>=0))/760,3))
  final_result=rbind(final_result,round(apply(result[[7]][,index[1]:index[2]], 2, function(x)sum(x>=0))/760,3))
  final_result=rbind(final_result,round(apply(result[[9]][,index[1]:index[2]], 2, function(x)sum(x>=0))/760,3))
  final_result=rbind(final_result,round(colSums(result[[4]][,index[1]:index[2]])/colSums(result[[5]][,index[1]:index[2]]),3))
  row.names(final_result)=c('A_ratio','A1_ratio','B_ratio','B1_ratio','plus/minus')
  colnames(final_result)=c(paste0('formula',index[1]:index[2]))
  final_result1=rbind(result[[3]][index[1]:index[2]],apply(result[[6]][,index[1]:index[2]], 2, sum))
  final_result1=rbind(final_result1,apply(result[[8]][,index[1]:index[2]], 2, sum))
  final_result1=rbind(final_result1,apply(result[[7]][,index[1]:index[2]], 2, sum))
  final_result1=rbind(final_result1,apply(result[[9]][,index[1]:index[2]], 2, sum))
  final_result1=rbind(final_result1,colSums(result[[4]][,index[1]:index[2]]))
  final_result1=rbind(final_result1,colSums(result[[5]][,index[1]:index[2]]))
  final_result1=rbind(final_result1,colSums(result[[10]][,index[1]:index[2]]))
  row.names(final_result1)=c('quantity  ','A_sum','A1_sum','B_sum','B1_sum','plus','minus','C_sum')
  first5_A=apply(result[[6]][,index[1]:index[2]], 2, function(x)str_c(sort(x,decreasing = T)[1:5],collapse = ' '))
  last5_A=apply(result[[6]][,index[1]:index[2]], 2, function(x)str_c(sort(x,decreasing = F)[1:5],collapse = ' '))
  first5_B=apply(result[[7]][,index[1]:index[2]], 2, function(x)str_c(sort(x,decreasing = T)[1:5],collapse = ' '))
  last5_B=apply(result[[7]][,index[1]:index[2]], 2, function(x)str_c(sort(x,decreasing = F)[1:5],collapse = ' '))
  first5_A1=apply(result[[8]][,index[1]:index[2]], 2, function(x)str_c(sort(x,decreasing = T)[1:5],collapse = ' '))
  last5_A1=apply(result[[8]][,index[1]:index[2]], 2, function(x)str_c(sort(x,decreasing = F)[1:5],collapse = ' '))
  first5_B1=apply(result[[9]][,index[1]:index[2]], 2, function(x)str_c(sort(x,decreasing = T)[1:5],collapse = ' '))
  last5_B1=apply(result[[9]][,index[1]:index[2]], 2, function(x)str_c(sort(x,decreasing = F)[1:5],collapse = ' '))
  A=str_c(first5_A,last5_A,sep  = ';')
  B=str_c(first5_B,last5_B,sep  = ';')
  A1=str_c(first5_A1,last5_A1,sep  = ';')
  B1=str_c(first5_B1,last5_B1,sep  = ';')
  ss=data.frame(A=A,A1=A1,B=B,B1=B1)
  row.names(ss)=c(paste0('formula',index[1]:index[2]))
  colnames(ss)=c('first&last5 of index A','first&last5 of index A1','first&last5 of index B','first&last5 of index B1')
  print(ss)
  cat('\n')
  print(final_result)
  
  print(final_result1)
}
# 三、测试：=========================================================
plan1019_34567R=run(data=all_data,path=path_BP,data_sort=13,choice=1019,rolling=T)
present(plan1019_34567R,index=c(1,8))
present(plan1019_34567R,index=c(9,16))
present(plan1019_34567R,index=c(17,24))
present(plan1019_34567R,index=c(25,32))
plan10191_34567R=run(data=all_data,path=path_BP,data_sort=13,choice=10191,rolling=T)
present(plan10191_34567R,index=c(1,8))
present(plan10191_34567R,index=c(9,16))
present(plan10191_34567R,index=c(17,24))
present(plan10191_34567R,index=c(25,32))
plan10192_34567R=run(data=all_data,path=path_BP,data_sort=13,choice=10192,rolling=T)
present(plan10192_34567R,index=c(1,8))
present(plan10192_34567R,index=c(9,16))
present(plan10192_34567R,index=c(17,24))
present(plan10192_34567R,index=c(25,32))
plan101911_34567R=run(data=all_data,path=path_BP,data_sort=13,choice=101911)
present(plan101911_34567R,index=c(1,8))
present(plan101911_34567R,index=c(9,16))
present(plan101911_34567R,index=c(17,24))
present(plan101911_34567R,index=c(25,32))
plan0_34567R=run(data=all_data,path=path_BP,data_sort=13,choice=0)
present(plan0_34567R,index=c(1,8))
present(plan0_34567R,index=c(9,16))
present(plan0_34567R,index=c(17,24))
present(plan0_34567R,index=c(25,32))
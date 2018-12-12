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
  formula=read.csv2('data\\579R.csv',header = F,stringsAsFactors = F)
  R5A=formula[2:13,];R5B=formula[15:26,]
  R7A=formula[28:47,];R7B=formula[49:68,]
  R9A=formula[70:97,];R9B=formula[99:126,]
  Formula=function(R){
    R_=str_split(R,pattern = ',',simplify = T)
    R_=apply(R_[,1:ncol(R_)], 1, function(x)str_c(x,collapse = '-'))
    return(str_c(R_,collapse = '|'))
  }
  R5A=Formula(R5A);R5B=Formula(R5B)
  R7A=Formula(R7A);R7B=Formula(R7B)
  R9A=Formula(R9A);R9B=Formula(R9B)
  
  path=matrix(c(R5A,R7A,R9A, R5A,R7B,R9A, R5A,R7A,R9B, R5A,R7B,R9B, R5B,R7A,R9A,
                R5B,R7B,R9A, R5B,R7A,R9B, R5B,R7B,R9B),byrow = T,ncol = 3)
  colnames(path)=c('R5','R7','R9')
  
}

c=c('data','all_data','path')
a=ls()
rm(list=(setdiff(a,c)))
gc()
library(stringr)
# 二、训练===========================================================
# 2.1定义计算每9个真实数据下的真实路径final_comb:
# 定义正确的逆序的计算真实路径的方式，与原来不同了，
# 注意取数据应是连续的,输入已经逆序的BP片段，输出对应的真实路径：
true_path=function(dataset){
  # 计算9R:多出一个
  t0=table(dataset[c(1:8,9)])
  result0=paste0(t0[1],t0[2])
  t1=table(dataset[1:8])
  result1=paste0(t1[1],t1[2])
  t2=table(dataset[1:7])
  result2=paste0(t2[1],t2[2])
  t2_1=table(dataset[1:6])
  result2_1=paste0(t2_1[1],t2_1[2])
  comb1=paste0(result2_1,'-',result2,'-',result1,'-',result0)
  # 计算7R
  t3=table(dataset[c(1:6,9)])
  result3=paste0(t3[1],t3[2])
  t4=table(dataset[1:6])
  result4=paste0(t4[1],t4[2])
  t5=table(dataset[1:5])
  result5=paste0(t5[1],t5[2])
  t5_1=table(dataset[1:4])
  result5_1=paste0(t5_1[1],t5_1[2])
  comb2=paste0(result5_1,'-',result5,'-',result4,'-',result3)
  # 计算5R
  t9=table(dataset[c(1:4,9)])
  result9=paste0(t9[1],t9[2])
  t10=table(dataset[1:4])
  result10=paste0(t10[1],t10[2])
  t11=table(dataset[1:3])
  result11=paste0(t11[1],t11[2])
  t11_1=table(dataset[1:2])
  result11_1=paste0(t11_1[1],t11_1[2])
  comb4=paste0(result11_1,'-',result11,'-',result10,'-',result9)
  return(matrix(c(comb4,comb2,comb1),ncol = 3))
} 
# true_path(data[1:9,1])

# 2.2 定义9个真实数据下的量化函数quantize：==============================
# 返回矩阵，计算9个回归方程下的满足程度（r9满足时为1，不满足为-1）的求和（r9+r7+r5）
# 类似似然函数：该值越大，说明预测的越准
quantize=function(path,set){
  real_comb=true_path(set) #计算实际数据路径即真实的“Y”
  n_row_path=ncol(path)
  # 分BP的path_BP有6列：
  if(ncol(path)==6){
    r9=matrix(0,ncol = n_row_path,nrow = 1)
    for (j in 1:n_row_path) {
      # 因为根据数据的开头得BP来判定,逆序时都是根据第一个数据
      r9[1,j]=ifelse(set[1]=='B'|set[1]==1,ifelse(str_detect(real_comb[1,3],path[j,'B_R9']),1,-1),ifelse(str_detect(real_comb[1,3],path[j,'P_R9']),1,-1))
    }
    r7=matrix(0,ncol = n_row_path,nrow = 1)
    for (j in 1:n_row_path) {
      r7[1,j]=ifelse(set[1]=='B'|set[1]==1,ifelse(str_detect(real_comb[1,2],path[j,'B_R7']),1,-1),ifelse(str_detect(real_comb[1,2],path[j,'P_R7']),1,-1))
    }
    r5=matrix(0,ncol = n_row_path,nrow = 1)
    for (j in 1:n_row_path) {
      r5[1,j]=ifelse(set[1]=='B'|set[1]==1,ifelse(str_detect(real_comb[1,1],path[j,'B_R5']),1,-1),ifelse(str_detect(real_comb[1,1],path[j,'P_R5']),1,-1))
    }
  }
  else{
    r9=matrix(0,ncol = nrow(path),nrow = 1)
    for (j in 1:nrow(path)) {
      r9[1,j]=ifelse(str_detect(real_comb[1,3],path[j,'R9']),1,-1)
    }
    r7=matrix(0,ncol = nrow(path),nrow = 1)
    for (j in 1:nrow(path)) {
      r7[1,j]=ifelse(str_detect(real_comb[1,2],path[j,'R7']),1,-1)
    }
    r5=matrix(0,ncol = nrow(path),nrow = 1)
    for (j in 1:nrow(path)) {
      r5[1,j]=ifelse(str_detect(real_comb[1,1],path[j,'R5']),1,-1)
    }
  }
  return(r9+r7+r5)
} 
# quantize(path,set=data[1:9,1])
# 这里算出来一定是1与8的formula相反的，因为opposite formula对应final result就是相反的
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
# 从9手开始，都为逆序，计算一个data在8个formula下的“似然值”：
sort_allsequence_nine=function(data=data,m,path=path){
  n=nrow(data)
  # 第一手：逆序 9
  {
    first_hand=matrix(ncol = nrow(path))
    for (i in seq(9,n,4)) {
      first_hand_quantity=quantize(path,set = data[c(seq(i-1,i-8,-1),i),m])
      first_hand=rbind(first_hand,first_hand_quantity)
    }
    first_hand=first_hand[-1,]
  }
  # 第二手：逆序 10
  { 
    second_hand=matrix(ncol = nrow(path) )
    for (i in seq(10,n,4)) {
      second_hand_quantity=quantize(path,set = data[c(seq(i-1,i-8,-1),i),m])
      second_hand=rbind(second_hand,second_hand_quantity)
    }
    second_hand=second_hand[-1,]
  }
  # 第三手：逆序 奇数轨偶数手
  {
    third_hand=matrix(ncol = nrow(path))
    for (i in seq(11,n,4)) {
      third_hand_quantity=quantize(path,set = data[c(seq(i-1,i-8,-1),i),m])
      third_hand=rbind(third_hand,third_hand_quantity)
    }
    third_hand=third_hand[-1,]
  }
  # 第四手：逆序 偶数轨偶数手
  { 
    forth_hand=matrix(ncol = nrow(path) )
    for (i in seq(12,n,4)) {
      forth_hand_quantity=quantize(path,set = data[c(seq(i-1,i-8,-1),i),m])
      forth_hand=rbind(forth_hand,forth_hand_quantity)
    }
    forth_hand=forth_hand[-1,]
  }
  return(list(first_hand,second_hand,third_hand,forth_hand))
}
# 2.5 运行的主程序run===============================================
run=function(data=data,path=path,choice=1019,rolling=F){
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
  
  for (m in 1:length(data)) {
    if(m%%50==0)print(m)
    temp=sort_allsequence_nine(data,m,path)
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
 # 2）不同的plan:1019,1019-1,1019-2——不同的最优化方式
    {
      # 因为1，2手与3，4手长度不一样，因因此截取到长度一样作为判断依据，拼接还是使用原始的
      min_=min(nrow(first),nrow(third))
      decide_first=first[1:min_,]
      decide_second=second[1:min_,]
      
      if (choice==1019&rolling==F) {
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
    }
 # 3）每个数据集m计算多个个评价指标：
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
    }
    
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
  return(list(minus_result,result,satisfy,plus,minus,
              new_indexA,new_indexB,new_indexA1,new_indexB1,new_indexc1,new_indexc2))
}
# 2.6 展示函数:
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
  final_result1=rbind(final_result1,colSums(result[[10]]))
  final_result1=rbind(final_result1,colSums(result[[11]]))
  row.names(final_result1)=c('quantity  ','A_sum','A1_sum','B_sum','B1_sum','plus','minus','C1_sum','C2_sum')
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
  
  A=str_c(first5_A,last5_A,sep  = ';')
  B=str_c(first5_B,last5_B,sep  = ';')
  A1=str_c(first5_A1,last5_A1,sep  = ';')
  B1=str_c(first5_B1,last5_B1,sep  = ';')
  c1=str_c(first5_c1,last5_c1,sep  = ';')
  c2=str_c(first5_c2,last5_c2,sep  = ';')
  ss=data.frame(A=A,A1=A1,B=B,B1=B1,C1=c1,C2=c2)
  row.names(ss)=c(paste0('formula',1:8))
  colnames(ss)=c('first&last5 of index A','first&last5 of index A1','first&last5 of index B','first&last5 of index B1','first&last5 of index C1','first&last5 of index C2')
  print(ss)
  cat('\n')
  print(final_result)
  print(final_result1)
}
library(stringr)
#==============================================================================
plan1019=run(data = all_data,path = path,choice=1019,rolling=T)
present(plan1019)
plan1019_1=run(data = all_data,path = path,choice=10191,rolling=T)
present(plan1019_1)
plan1019_2=run(data = all_data,path = path,choice=10192,rolling=T)
present(plan1019_2)
plan1019_11=run(data = all_data,path = path,choice=101911,rolling=T)
present(plan1019_11)
plan1019_20_1=run(data = all_data,path = path,choice=1019201,rolling=T)
present(plan1019_20_1)
plan1019_20_2=run(data = all_data,path = path,choice=1019202,rolling=T)
present(plan1019_20_2)


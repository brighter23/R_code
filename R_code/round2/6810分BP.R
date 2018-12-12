# 1、formula准备------------------------------------------
data=read.csv('data\\treat_two_hundred.csv',header = T)
data1=read.csv('data\\new.csv',header = T)
data2=read.csv('data\\new1.csv',header = T)
data3=read.csv('data\\new2.csv',header = T)
data1=data1[,-23]
all_data=cbind(data,data1,data2,data3)
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
  
  path=matrix(c(R6A,R8A,R10A, R6A,R8B,R10A, R6A,R8A,R10B, R6A,R8B,R10B, R6B,R8A,R10A, R6B,R8B,R10A, R6B,R8A,R10B, R6B,R8B,R10B),byrow = T,ncol = 6)
  colnames(path)=c('B_R6','P_R6','B_R8','P_R8','B_R10','P_R10')
  
}
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
new_quantize=function(path,real_comb,set){
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
# 2.3 定义取数据的方式：
{
  # 平行双轨
  data_sort1=function(data=data,m,path=path){
    n=nrow(data)
    # 第一手
    {
      first_hand=matrix(ncol = nrow(path) )
      for (i in seq(21,n,2)) {
        real_comb=true_path(data[seq(i-18,i,2),m])
        first_hand_quantity=new_quantize(path,real_comb,set = data[seq(i-18,i,2),m])
        first_hand=rbind(first_hand,first_hand_quantity)
      }
      first_hand=first_hand[-1,]
      
    }
    # 第二手
    {
      second_hand=matrix(ncol = nrow(path) )
      for (i in seq(22,n,2)) {
        real_comb=true_path(data[seq(i-18,i,2),m])
        second_hand_quantity=new_quantize(path,real_comb,set=data[seq(i-18,i,2),m])
        second_hand=rbind(second_hand,second_hand_quantity)
      }
      second_hand=second_hand[-1,]
      
    }
    
    return(list(first_hand,second_hand))
  }
  data_sort2=function(data=data,m,path=path){
    n=nrow(data)
    # 第一手
    {
      sequance_n=length(seq(5,n,4))*2
      sequance=matrix(nrow =sequance_n )
      sequance[seq(1,sequance_n,2),]=seq(4,n-1,4)
      sequance[seq(2,sequance_n,2),]=seq(5,n,4)
      first_hand=matrix(ncol = nrow(path))
      for (i in seq(21,n,4)) {
        index=which(sequance==i)
        real_comb=true_path(data[sequance[(index-9):index],m])
        first_hand_quantity=new_quantize(path,real_comb,set = data[sequance[(index-9):index],m])
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
        second_hand_quantity=new_quantize(path,real_comb,set = data[sequance1[(index-9):index],m])
        second_hand=rbind(second_hand,second_hand_quantity)
      }
      second_hand=second_hand[-1,]
      
    }
    # 第三手
    {
      sequance_n=length(seq(7,n,4))*2
      sequance=matrix(nrow =sequance_n )
      sequance[seq(1,sequance_n,2),]=seq(6,n,4)
      sequance[seq(2,sequance_n,2),]=seq(7,n,4)
      third_hand=matrix(ncol = nrow(path))
      for (i in seq(23,n,4)) {
        index=which(sequance==i)
        real_comb=true_path(data[sequance[(index-9):index],m])
        third_hand_quantity=new_quantize(path,real_comb,set = data[sequance[(index-9):index],m])
        third_hand=rbind(third_hand,third_hand_quantity)
      }
      third_hand=third_hand[-1,]
      
    }
    # 第四手
    { 
      sequance2_n=length(seq(8,n,4))*2
      sequance2=matrix(nrow =sequance2_n )
      sequance2[seq(1,sequance2_n,2),]=seq(7,n,4)
      sequance2[seq(2,sequance2_n,2),]=seq(8,n,4)
      
      forth_hand=matrix(ncol = nrow(path) )
      for (i in seq(24,n,4)) {
        index=which(sequance2==i)
        real_comb=true_path(data[sequance2[(index-9):index],m])
        forth_hand_quantity=new_quantize(path,real_comb,set = data[sequance2[(index-9):index],m])
        forth_hand=rbind(forth_hand,forth_hand_quantity)
      }
      forth_hand=forth_hand[-1,]
      
    }
    return(list(first_hand,second_hand,third_hand,forth_hand))
  }
  # 螺旋双轨：
  data_sort3=function(data=data,m,path=path){
    n=nrow(data)
    # 第一轨
    {
      sequance1=rep(0,34)
      sequance1[seq(1,34,2)]=seq(1,68,4)
      sequance1[seq(2,34,2)]=seq(4,68,4)
      first_hand=matrix(ncol = nrow(path),nrow = 25)
      for (i in 1:25) {
        real_comb=true_path(data[sequance1[i:(i+9)],m])
        first_hand[i,]=new_quantize(path,real_comb,set = data[sequance1[i:(i+9)],m])
      }
      first_hand=first_hand[-1,] #第一行结果时21，第二行是24
    }
    # 第二轨
    {
      sequance2=rep(0,34)
      sequance2[seq(1,34,2)]=seq(2,68,4)
      sequance2[seq(2,34,2)]=seq(3,68,4)
      second_hand=matrix(ncol = nrow(path),nrow = 25)
      for (i in 1:25) {
        real_comb=true_path(data[sequance2[i:(i+9)],m])
        second_hand[i,]=new_quantize(path,real_comb,set=data[sequance2[i:(i+9)],m])
      }
      second_hand=second_hand[-1,] # 第一行22，第二行23
    }
    first=first_hand[seq(1,24,2),]
    forth=first_hand[seq(2,24,2),]
    second=second_hand[seq(1,24,2),]
    third=second_hand[seq(2,24,2),]
    return(list(first,second,third,forth))
  }
  # 1\2手平行双轨，3\4手连续
  data_sort4=function(data=data,m,path=path){
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
        real_comb=true_path(data[(i-9):i,m])
        third_hand_quantity=new_quantize(path,real_comb,set = data[(i-9):i,m])
        third_hand=rbind(third_hand,third_hand_quantity)
      }
      third_hand=third_hand[-1,]
      
    }
    # 第四手
    { 
      forth_hand=matrix(ncol = nrow(path) )
      for (i in seq(24,n,4)) {
        real_comb=true_path(data[(i-9):i,m])
        forth_hand_quantity=new_quantize(path,real_comb,set = data[(i-9):i,m])
        forth_hand=rbind(forth_hand,forth_hand_quantity)
      }
      forth_hand=forth_hand[-1,]
      
    }
    return(list(first_hand,second_hand,third_hand,forth_hand))
  }
  # 1\2手平行双轨，3\4手螺旋
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
        index=which(sequance1==i)
        real_comb=true_path(data[seq(i-18,i,2),m])
        second_hand_quantity=new_quantize(path,real_comb,set = data[seq(i-18,i,2),m])
        second_hand=rbind(second_hand,second_hand_quantity)
      }
      second_hand=second_hand[-1,]
      
    }
    # 第三手
    { 
      sequance1=rep(0,34)
      sequance1[seq(1,34,2)]=seq(2,68,4)
      sequance1[seq(2,34,2)]=seq(3,68,4)
      third_hand=matrix(ncol = nrow(path))
      for (i in seq(23,n,4)) {
        id=sequance1[(which(sequance1==i)-9):which(sequance1==i)]
        real_comb=true_path(data[id,m])
        third_hand_quantity=new_quantize(path,real_comb,set = data[id,m])
        third_hand=rbind(third_hand,third_hand_quantity)
      }
      third_hand=third_hand[-1,]
      
    }
    # 第四手
    {
      sequance2=rep(0,34)
      sequance2[seq(1,34,2)]=seq(1,68,4)
      sequance2[seq(2,34,2)]=seq(4,68,4)
      forth_hand=matrix(ncol = nrow(path) )
      for (i in seq(24,n,4)) {
        id=sequance2[(which(sequance2==i)-9):which(sequance2==i)]
        real_comb=true_path(data[id,m])
        forth_hand_quantity=new_quantize(path,real_comb,set = data[id,m])
        forth_hand=rbind(forth_hand,forth_hand_quantity)
      }
      forth_hand=forth_hand[-1,]
      
    }
    return(list(first_hand,second_hand,third_hand,forth_hand))
  }
}

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
run=function(data=data,path=path,data_sort=1,seq){
  n_path=nrow(path)
  
  # 1、minus_result行代表数据集索引，列代表formula，每格表示最大连负数目
  minus_result=matrix(nrow =length(data),ncol = n_path)
  new_index=matrix(nrow =length(data),ncol = n_path)
  for (m in 1:length(data)) {
    if(m%%50==0)print(m)
    {
      if(data_sort==1){
        temp=data_sort1(data,m,path)
        first=temp[[1]][seq(1,nrow(temp[[1]]),2),]  # 奇数轨奇数行
        second=temp[[2]][seq(1,nrow(temp[[2]]),2),] # 偶数轨奇数行
        third=temp[[1]][seq(2,nrow(temp[[1]]),2),]
        forth=temp[[2]][seq(2,nrow(temp[[2]]),2),]
      }
      if(data_sort==2){
        temp=data_sort2(data,m,path)
        first=temp[[1]]  # 奇数轨
        second=temp[[2]] # 偶数轨
        third=temp[[3]]
        forth=temp[[4]]
      }
      if(data_sort==3){
        temp=data_sort3(data,m,path)
        first=temp[[1]]  # 奇数轨
        second=temp[[2]] # 偶数轨
        third=temp[[3]]
        forth=temp[[4]]
      }
      if(data_sort==4){
        temp=data_sort4(data,m,path)
        first=temp[[1]]  # 奇数轨
        second=temp[[2]] # 偶数轨
        third=temp[[3]]
        forth=temp[[4]]
      }
      if(data_sort==5){
        temp=data_sort5(data,m,path)
        first=temp[[1]]  # 奇数轨
        second=temp[[2]] # 偶数轨
        third=temp[[3]]
        forth=temp[[4]]
      }
    }
    
    # 合并成final_result
    n=nrow(first)+nrow(second)+nrow(third)+nrow(forth)
    # 新指标的模式（1或2）和符号（正负）
    pattern=matrix(nrow = n,ncol =nrow(path))
    
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
      pattern[seq(4,n,4),]=ifelse(second<0,1,2)
    }
    
    sumtemp=matrix(nrow = n,ncol =nrow(path))
    sumtemp[seq(1,n,4),]=first
    sumtemp[seq(2,n,4),]=second
    sumtemp[seq(3,n,4),]=third
    sumtemp[seq(4,n,4),]=forth
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

new13_1=run(all_data,path,1,3);new13_1[[3]]

new11=run(all_data,path,1,4);new11[[3]]

new13=run(all_data,path,1,2);new13[[3]]

# plan20:
new20=run(all_data,path,4,1);new20[[3]]
# plan29:在plan13反一反的形式
new29=run(all_data,path,5,2);new29[[3]]

presient(new13)
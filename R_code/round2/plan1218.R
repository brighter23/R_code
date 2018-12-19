# 1、formula准备------------------------------------------
data=read.csv('data\\treat_two_hundred.csv',header = T)
data1=read.csv('data\\new.csv',header = T)
data2=read.csv('data\\new1.csv',header = T)
data3=read.csv('data\\new2.csv',header = T)
data1=data1[,-23]
all_data=cbind(data,data1,data2,data3)
row.names(data)=1:68
row.names(all_data)=1:68
{
  library(stringr)
  formula=read.csv2('data\\YAY88R_9R.csv',header = F,stringsAsFactors = F)

  R88A=formula[2:65,];R88B=formula[67:130,]
  R9A=formula[132:259,];R9B=formula[261:388,]
  Formula=function(R,c1,c2){
    R_=str_split(R,pattern = ',',simplify = T)
    R_B=apply(R_[,c1], 1, function(x)str_c(x,collapse = '-'))
    R_P=apply(R_[,c2], 1, function(x)str_c(x,collapse = '-'))
    return(cbind(str_c(R_B,collapse = '|'), str_c(R_P,collapse = '|')))
  }
  
  R88A=Formula(R88A,c1 = 1:7,c2=8:14);R88B=Formula(R88B,c1 = 1:7,c2=8:14)
  R9A=Formula(R9A,c1 = 1:8,c2=9:16);R9B=Formula(R9B,c1 = 1:8,c2=9:16)
  R88=rbind(R88A,R88B)
  colnames(R88)=c('R_B','R_P')
  rownames(R88)=c('systemA','systemB')
  R9=rbind(R9A,R9B)
  colnames(R9)=c('R_B','R_P')
  rownames(R9)=c('systemA','systemB')
}

c=c('data','all_data','R88','R9')
a=ls()
rm(list=(setdiff(a,c)))
gc()
library(stringr)
# 当有新的formula时需要修改true_path、new_quantize、取数据的手数
# 2、训练==================================================
# 当有新的formula时需要修改true_path、new_quantize、取数据的手数(取7手计算final result的都一样，取9手的与9手的一样)

# 2.1 定义计算每9个真实数据下的真实路径final_comb:
true_path9=function(dataset){
  #9R:8个
  t19=table(dataset[1:9])
  result19=paste(t19[1],t19[2],sep = ':')
  t18=table(dataset[1:8])
  result18=paste(t18[1],t18[2],sep = ':')
  t17=table(dataset[1:7])
  result17=paste(t17[1],t17[2],sep = ':')
  t16=table(dataset[1:6])
  result16=paste(t16[1],t16[2],sep = ':')
  t15=table(dataset[1:5])
  result15=paste(t15[1],t15[2],sep = ':')
  t14=table(dataset[1:4])
  result14=paste(t14[1],t14[2],sep = ':')
  t13=table(dataset[1:3])
  result13=paste(t13[1],t13[2],sep = ':')
  t12=table(dataset[1:2])
  result12=paste(t12[1],t12[2],sep = ':')
  comb1=paste0(result12,'-',result13,'-',result14,'-',result15,'-',result16,'-',result17,'-',result18,'-',result19)
  return(matrix(c(comb1),ncol = 1))
  
} # 仅9R
true_path8=function(dataset){
  t18=table(dataset[1:8])
  result18=paste(t18[1],t18[2],sep = ':')
  t17=table(dataset[1:7])
  result17=paste(t17[1],t17[2],sep = ':')
  t16=table(dataset[1:6])
  result16=paste(t16[1],t16[2],sep = ':')
  t15=table(dataset[1:5])
  result15=paste(t15[1],t15[2],sep = ':')
  t14=table(dataset[1:4])
  result14=paste(t14[1],t14[2],sep = ':')
  t13=table(dataset[1:3])
  result13=paste(t13[1],t13[2],sep = ':')
  t12=table(dataset[1:2])
  result12=paste(t12[1],t12[2],sep = ':')
  comb1=paste0(result12,'-',result13,'-',result14,'-',result15,'-',result16,'-',result17,'-',result18)
  return(matrix(c(comb1),ncol = 1))
  
} # 仅8R
# 2.2 定义8or9个真实数据下根据真实路径下的量化值-1或1:

# 因为这里仅使用8R或者9R的全部数据，在量化时不受逆序数据的影响，且都是根据第一手判定分BP
new_quantize_R88=function(R88,set){
  real_comb=true_path8(set)
  r=matrix(0,ncol = nrow(R88),nrow = 1)
  for (j in 1:ncol(R88)) {
    r[1,j]=ifelse(set[1]=='B'|set[1]==1,ifelse(str_detect(real_comb,R88[j,'R_B']),1,-1),ifelse(str_detect(real_comb,R88[j,'R_P']),1,-1))
  }
  colnames(r)=c('A','B')
  return(r)
} 
# new_quantize_R88(R88,data[1:8,1])
new_quantize_R9=function(R9,set){
  real_comb=true_path9(set)
  r=matrix(0,ncol = nrow(R9),nrow = 1)
  for (j in 1:ncol(R9)) {
    r[1,j]=ifelse(set[1]=='B'|set[1]==1,ifelse(str_detect(real_comb,R9[j,'R_B']),1,-1),ifelse(str_detect(real_comb,R9[j,'R_P']),1,-1))
  }
  colnames(r)=c('A','B')
  return(r)
} 
# new_quantize_R9(R9,data[1:9,1])
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
# 取数据：并生成预测值
{
  # 单轨，轨迹数据，2个一组
  data_sort_sequence_inverse_one=function(data,m,choice,R88,R9){
    n=nrow(data)
    final_result=matrix(nrow = n,ncol = 1) # final_result
    row.names(final_result)=1:n
    pre=data.frame(x1=matrix(nrow = n,ncol = 1),row.names = 1:n) # 奇数轨预测值
    for(i in 1:ncol(pre)){
      pre[,i]=factor(pre[,i],levels = c('B','P'))
    }    
    # 1、先计算使用原始数据的9-16手
    # 奇数手：使用R88的formula进行2次判别
    for (i in seq(9,15,2)) {
      # 计算 顺序前8手（到第i-1手）的结果：真实路径为结果是正的那个，以及确定使用的formula的索引index
      if(choice==12181){
        index=which(new_quantize_R88(R88,data[seq(i-8,i-1,1),m])<0)
      }
      else if(choice==12182){
        index=which(new_quantize_R88(R88,data[seq(i-8,i-1,1),m])>0)
      }
      # 计算 逆序前8手（到第i手）的final result,
      final_result[i]=new_quantize_R88(R88,data[c(seq(i-1,i-7,-1),i),m])[,index]
      # 计算预测值
      pre[i,]=ifelse(final_result[i]>0,ifelse(data[i,m]=='B','B','P'),ifelse(data[i,m]=='B','P','B'))
    }
    # 偶数手：使用R9的formula进行2次判别
    for (i in seq(10,16,2)) {
      # 计算 顺序前9手（到第i-1手）的结果：真实路径为结果是正的那个，以及确定使用的formula的索引index
      if(choice==12181){
        index=which(new_quantize_R9(R9,data[seq(i-9,i-1,1),m])<0)
      }
      else if(choice==12182){
        index=which(new_quantize_R9(R9,data[seq(i-9,i-1,1),m])>0)
      }
      # 计算 逆序前9手（到第i手）的final result,
      final_result[i]=new_quantize_R9(R9,data[c(seq(i-1,i-8,-1),i),m])[,index]
      # 计算预测值
      pre[i,]=ifelse(final_result[i]>0,ifelse(data[i,m]=='B','B','P'),ifelse(data[i,m]=='B','P','B'))
    }
    # 2、合再一起做，因为互相用到数据
    for(i in 17:n){
      # final result替换数据从重新计算
      # 奇数手使用R88的formula进行2次判别
      if (i %in% seq(17,n,2)){
        # 计算 顺序前8手（到第i-1手）的结果：真实路径为结果是正的那个，以及确定使用的formula的索引index
        if(choice==12181){
          index=which(new_quantize_R88(R88,pre[seq(i-8,i-1,1),])<0)
        }
        else if(choice==12182){
          index=which(new_quantize_R88(R88,pre[seq(i-8,i-1,1),])>0)
        }
        # 计算 逆序前8手（到第i手）的final result,
        pre[i,]=data[i,m] #使用真实数据暂时替代未知的预测数据，以便计算使用的formula是否符合
        final_result[i]=new_quantize_R88(R88,pre[c(seq(i-1,i-7,-1),i),])[,index]
        # 计算预测值
        pre[i,]=ifelse(final_result[i]>0,ifelse(data[i,m]=='B','B','P'),ifelse(data[i,m]=='B','P','B'))
      }
      # 偶数手使用R9的formula进行2次判别
      else{
        # 计算 顺序前9手（到第i-1手）的结果：真实路径为结果是正的那个，以及确定使用的formula的索引index
        if(choice==12181){
          index=which(new_quantize_R9(R9,pre[seq(i-9,i-1,1),])<0)
        }
        else if(choice==12182){
          index=which(new_quantize_R9(R9,pre[seq(i-9,i-1,1),])>0)
        }
        # 计算 逆序前9手（到第i手）的final result,
        pre[i,]=data[i,m] #使用真实数据暂时替代未知的预测数据，以便计算使用的formula是否符合
        final_result[i]=new_quantize_R9(R9,pre[c(seq(i-1,i-8,-1),i),])[,index]
        # 计算预测值
        pre[i,]=ifelse(final_result[i]>0,ifelse(data[i,m]=='B','B','P'),ifelse(data[i,m]=='B','P','B'))
      }
    }
    # 3、去除无法计算final result的：
    final_result=final_result[-c(1:8),]
    pre=pre[-c(1:8),]
    return(list(final_result,pre))
  }
  # data_sort_sequence_inverse_one(data,m,choice,R88,R9)
  # 单轨，原始数据，2个一组
  data_sort_sequence_inverse_origin_one=function(data,m,choice,R88,R9){
    n=nrow(data)
    final_result=matrix(nrow = n,ncol = 1) # final_result
    row.names(final_result)=1:n
    # 奇数手：使用R88的formula进行2次判别
    for (i in seq(9,n,2)) {
      # 计算 顺序前8手（到第i-1手）的结果：真实路径为结果是正的那个，以及确定使用的formula的索引index
      if(choice==12181){
        index=which(new_quantize_R88(R88,data[seq(i-8,i-1,1),m])<0)
      }
      else if(choice==12182){
        index=which(new_quantize_R88(R88,data[seq(i-8,i-1,1),m])>0)
      }
      # 计算 逆序前8手（到第i手）的final result,
      final_result[i]=new_quantize_R88(R88,data[c(seq(i-1,i-7,-1),i),m])[,index]
    }
    # 偶数手：使用R9的formula进行2次判别
    for (i in seq(10,n,2)) {
      # 计算 顺序前9手（到第i-1手）的结果：真实路径为结果是正的那个，以及确定使用的formula的索引index
      if(choice==12181){
        index=which(new_quantize_R9(R9,data[seq(i-9,i-1,1),m])<0)
      }
      else if(choice==12182){
        index=which(new_quantize_R9(R9,data[seq(i-9,i-1,1),m])>0)
      }
      # 计算 逆序前9手（到第i手）的final result,
      final_result[i]=new_quantize_R9(R9,data[c(seq(i-1,i-8,-1),i),m])[,index]
    }
    # 2、去除无法计算final result的：
    final_result=final_result[-c(1:8),]
    return(list(final_result))
  }
  # data_sort_sequence_inverse_origin_one(data,40,choice = 12181,R88,R9)
}
# data_test=read.csv('data//data_test.csv',header = F)
# data_sort_sequence_inverse_origin_one(data=data_test,m=1,choice = 121110,path = path_R11,R9=F,R11=T,R13=F)
run=function(data,choice,R88,R9,original){
  library(stringr)
  minus_result=matrix(nrow =length(data),ncol = 1)
  plus=matrix(nrow =length(data),ncol = 1)
  minus=matrix(nrow =length(data),ncol = 1)
  new_indexE1=matrix(nrow =length(data),ncol = 1)
  new_indexE2=matrix(nrow =length(data),ncol = 1)
  for (m in 1:length(data)) {
    if(m%%50==0)print(m)
    temp=ifelse(original,data_sort_sequence_inverse_origin_one(data,m,choice,R88,R9),data_sort_sequence_inverse_one(data,m,choice,R88,R9))  
    # 1）初始化变量，以记录指标：
    sumtemp=matrix(temp[[1]])
    n_sumtemp=nrow(sumtemp)
    # 2）每个数据集m计算多个评价指标：
    {
      # 计算连续负 “最大” 的手数
      minus_result[m,]=detect_continue_minus(sumtemp)
      # 计算新指标:评价指标
      # 1、正负比例
      plus[m,]=apply(sumtemp, 2, function(x)sum(x>0)) 
      minus[m,]=n_sumtemp-plus[m,]# 数据m下结果负的手数
      symbol=ifelse(sumtemp>0,1,-1)
      patternE1=matrix(rep(c(rep(c(1,2),(n_sumtemp)%/%2),c(1,2)[0:((n_sumtemp)%%2)]),1),ncol = 1)
      new_indexE1[m,]=colSums(patternE1*symbol)
      patternE2=matrix(rep(c(rep(c(2,1),(n_sumtemp)%/%2),c(2,1)[0:((n_sumtemp)%%2)]),1),ncol = 1)
      new_indexE2[m,]=colSums(patternE2*symbol)
    } 
    
  }
  
  colnames(minus_result)=c(paste0('formula',1:1))
  # result存放最大连续负个数
  result=matrix(nrow =max(minus_result) ,ncol = 1)
  for (i in 1:(max(minus_result))){
    result[i,]=apply(minus_result,2,function(x)sum(x==i))
  }
  colnames(result)=c(paste0('formula',1:1))
  
  # satisfy为某一个formula满足小于等于5的数据集个数：
  satisfy=sum(result[1:5,])
  return(list(minus_result,result,satisfy,plus,minus,
              new_indexE1,new_indexE2))
}
# 2.6 展示函数
present=function(result){
  library(stringr)
  print(result[[2]])
  cat('\n')
  final_result=matrix(round(colSums(result[[4]])/colSums(result[[5]]),3),nrow = 1)
  row.names(final_result)=c('plus/minus')
  colnames(final_result)=c(paste0('formula',1:1))
  final_result1=rbind(result[[3]],apply(result[[6]], 2, sum))
  final_result1=rbind(final_result1,apply(result[[7]], 2, sum))
  final_result1=rbind(final_result1,colSums(result[[4]]))
  final_result1=rbind(final_result1,colSums(result[[5]]))
  row.names(final_result1)=c('quantity','E1_sum','E2_sum','plus','minus')
  
  first5_e1=apply(result[[6]], 2, function(x)str_c(sort(x,decreasing = T)[1:5],collapse = ' '))
  last5_e1=apply(result[[6]], 2, function(x)str_c(sort(x,decreasing = F)[1:5],collapse = ' '))
  first5_e2=apply(result[[7]], 2, function(x)str_c(sort(x,decreasing = T)[1:5],collapse = ' '))
  last5_e2=apply(result[[7]], 2, function(x)str_c(sort(x,decreasing = F)[1:5],collapse = ' '))
  
  e1=str_c(first5_e1,last5_e1,sep  = ';')
  e2=str_c(first5_e2,last5_e2,sep  = ';')
  ss=data.frame(E1=e1,E2=e2)
  row.names(ss)=c(paste0('formula',1:1))
  colnames(ss)=c('first&last5 of index E1','first&last5 of index E2')
  print(ss)
  cat('\n')
  print(final_result)
  print(final_result1)
}

# write.csv(as.data.frame(sumtemp[,2]),'C:\\Users\\Think\\Desktop\\1.csv',row.names = F)
#######==================================================================================
# plan1218-1
plan1218_1_original=run(all_data,choice=12181,R88,R9,original=T)
present(plan1218_1_original)
plan1218_1=run(all_data,choice=12181,R88,R9,original=F)
present(plan1218_1)
# plan1218-2
plan1218_2_original=run(all_data,choice=12182,R88,R9,original=T)
present(plan1218_2_original)
plan1218_2=run(all_data,choice=12182,R88,R9,original=F)
present(plan1218_2)
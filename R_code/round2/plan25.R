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
  
  path=matrix(c(R61,R81,R101, R61,R82,R101, R61,R81,R102, R61,R82,R102, R62,R81,R101, R62,R82,R101, R62,R81,R102, R62,R82,R102),byrow = T,ncol = 3)
  colnames(path)=c('R6','R8','R10')
  
  minus_path=matrix(c(R62,R82,R102, R62,R81,R102, R62,R82,R101, R62,R81,R101, R61,R82,R102, R61,R81,R102, R61,R82,R101, R61,R81,R101),byrow = T,ncol = 3)
  colnames(minus_path)=c('R6','R8','R10')
  
}
c=c('data','all_data','path','minus_path')
a=ls()
rm(list=(setdiff(a,c)))
gc()
if(!require(stringr))install.packages('stringr')
library(stringr)
# 2、训练----------------------------------------------

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
new_quantize=function(path,real_comb){
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
# 2.3取数据集并量化：
data_sort=function(data=data,m,path=path){
  n=nrow(data)
  # 第一手
  {
    first_hand=matrix(ncol = nrow(path))
    for (i in seq(19,n,2)) {
      real_comb=true_path(data[seq(i-18,i,2),m])
      first_hand_quantity=new_quantize(path,real_comb)
      first_hand=rbind(first_hand,first_hand_quantity)
    }
    first_hand=first_hand[-1,]
    
  }
  # 第二手
  {
    second_hand=matrix(ncol = nrow(path) )
    for (i in seq(20,n,2)) {
      real_comb=true_path(data[seq(i-18,i,2),m])
      second_hand_quantity=new_quantize(path,real_comb)
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
  for (m in 1:length(data)) {
    if(m%%50==0)print(m)
    temp=data_sort(data,m,path)
    first=temp[[1]]
    second=temp[[2]]
    # 合并成final_result
    n=nrow(first)+nrow(second)
    if(seq==0){
      sumtemp=matrix(nrow = n,ncol =nrow(path))
      sumtemp[seq(1,n,2),]=first
      sumtemp[seq(2,n,2),]=second
    }
    else if (seq==1){
      # plan25:
      sumtemp=matrix(nrow = n,ncol =nrow(path))
      sumtemp[seq(1,n,2),]=first
      sumtemp[seq(2,n,2),]=second
      row.names(sumtemp)=19:68
      sumtemp[c('23','33','43','53','63'),]=ifelse(sumtemp[c('21','31','41','51','61'),]<0,sumtemp[c('23','33','43','53','63'),],-sumtemp[c('23','33','43','53','63'),])
      sumtemp[c('25','35','45','55','65'),]=ifelse(sumtemp[c('24','34','44','54','64'),]<0,sumtemp[c('25','35','45','55','65'),],-sumtemp[c('25','35','45','55','65'),])
      sumtemp[c('27','37','47','57','67'),]=ifelse(sumtemp[c('22','32','42','52','62'),]<0,sumtemp[c('27','37','47','57','67'),],-sumtemp[c('27','37','47','57','67'),])
      sumtemp[c('28','38','48','58','68'),]=ifelse(sumtemp[c('26','36','46','56','66'),]<0,sumtemp[c('28','38','48','58','68'),],-sumtemp[c('28','38','48','58','68'),])
      sumtemp[c('20','30','40','50','60'),]=ifelse(sumtemp[c('19','29','39','49','59'),]<0,sumtemp[c('20','30','40','50','60'),],-sumtemp[c('20','30','40','50','60'),])
    }
    else if (seq==2){
      # plan25-1:
      sumtemp=matrix(nrow = n,ncol =nrow(path))
      sumtemp[seq(1,n,2),]=first
      sumtemp[seq(2,n,2),]=second
      row.names(sumtemp)=19:68
      sumtemp[c('23','33','43','53','63'),]=ifelse(sumtemp[c('21','31','41','51','61'),]<0,sumtemp[c('23','33','43','53','63'),],-sumtemp[c('23','33','43','53','63'),])
      sumtemp[c('25','35','45','55','65'),]=ifelse(sumtemp[c('24','34','44','54','64'),]<0,sumtemp[c('25','35','45','55','65'),],-sumtemp[c('25','35','45','55','65'),])
      sumtemp[c('26','36','46','56','66'),]=ifelse(sumtemp[c('23','33','43','53','63'),]<0,sumtemp[c('26','36','46','56','66'),],-sumtemp[c('26','36','46','56','66'),])
      sumtemp[c('27','37','47','57','67'),]=ifelse(sumtemp[c('22','32','42','52','62'),]<0,sumtemp[c('27','37','47','57','67'),],-sumtemp[c('27','37','47','57','67'),])
      sumtemp[c('28','38','48','58','68'),]=ifelse(sumtemp[c('21','31','41','51','61'),]<0,sumtemp[c('28','38','48','58','68'),],-sumtemp[c('28','38','48','58','68'),])
      sumtemp[c('29','39','49','59'),]=ifelse(sumtemp[c('25','35','45','55'),]<0,sumtemp[c('29','39','49','59'),],-sumtemp[c('29','39','49','59'),])
      sumtemp[c('30','40','50','60'),]=ifelse(sumtemp[c('24','34','44','54'),]<0,sumtemp[c('30','40','50','60'),],-sumtemp[c('30','40','50','60'),])
    }
    else if (seq==3){
      # plan25-1:
      sumtemp=matrix(nrow = n,ncol =nrow(path))
      sumtemp[seq(1,n,2),]=first
      sumtemp[seq(2,n,2),]=second
      row.names(sumtemp)=19:68
      sumtemp[c('23','33','43','53','63'),]=ifelse(sumtemp[c('21','31','41','51','61'),]<0,sumtemp[c('23','33','43','53','63'),],-sumtemp[c('23','33','43','53','63'),])
      sumtemp[c('25','35','45','55','65'),]=ifelse(sumtemp[c('24','34','44','54','64'),]<0,sumtemp[c('25','35','45','55','65'),],-sumtemp[c('25','35','45','55','65'),])
      sumtemp[c('26','36','46','56','66'),]=ifelse(sumtemp[c('23','33','43','53','63'),]<0,sumtemp[c('26','36','46','56','66'),],-sumtemp[c('26','36','46','56','66'),])
      sumtemp[c('27','37','47','57','67'),]=ifelse(sumtemp[c('22','32','42','52','62'),]<0,sumtemp[c('27','37','47','57','67'),],-sumtemp[c('27','37','47','57','67'),])
      sumtemp[c('28','38','48','58','68'),]=ifelse(sumtemp[c('26','36','46','56','66'),]<0,sumtemp[c('28','38','48','58','68'),],-sumtemp[c('28','38','48','58','68'),])
      sumtemp[c('29','39','49','59'),]=ifelse(sumtemp[c('25','35','45','55'),]<0,sumtemp[c('29','39','49','59'),],-sumtemp[c('29','39','49','59'),])
      sumtemp[c('20','30','40','50','60'),]=ifelse(sumtemp[c('19','29','39','49','59'),]<0,sumtemp[c('20','30','40','50','60'),],-sumtemp[c('20','30','40','50','60'),])
    }
    else if (seq==4){
      # plan26:
      sumtemp=matrix(nrow = n,ncol =nrow(path))
      sumtemp[seq(1,n,2),]=first
      sumtemp[seq(2,n,2),]=second
      row.names(sumtemp)=19:68
      
      sumtemp[c('31','45','59'),]=ifelse(sumtemp[c('30','44','58'),]<0,sumtemp[c('31','45','59'),],-sumtemp[c('31','45','59'),])
      sumtemp[c('32','46','60'),]=ifelse(sumtemp[c('29','43','57'),]<0,sumtemp[c('32','46','60'),],-sumtemp[c('32','46','60'),])
      sumtemp[c('21','35','49','63'),]=ifelse(sumtemp[c('19','33','47','61'),]<0,sumtemp[c('21','35','49','63'),],-sumtemp[c('21','35','49','63'),])
      sumtemp[c('24','38','52','66'),]=ifelse(sumtemp[c('23','37','51','65'),]<0,sumtemp[c('24','38','52','66'),],-sumtemp[c('24','38','52','66'),])
      sumtemp[c('25','39','53','67'),]=ifelse(sumtemp[c('22','36','50','64'),]<0,sumtemp[c('25','39','53','67'),],-sumtemp[c('25','39','53','67'),])
      sumtemp[c('27','41','55'),]=ifelse(sumtemp[c('20','34','48'),]<0,sumtemp[c('27','41','55'),],-sumtemp[c('27','41','55'),])
      sumtemp[c('28','42','56'),]=ifelse(sumtemp[c('26','40','54'),]<0,sumtemp[c('28','42','56'),],-sumtemp[c('28','42','56'),])
    }
    else if (seq==5){

      sumtemp=matrix(nrow = n,ncol =nrow(path))
      sumtemp[seq(1,n,2),]=first
      sumtemp[seq(2,n,2),]=second
      row.names(sumtemp)=19:68
      
      sumtemp[c('22','30','38','46','54','62'),]=ifelse(sumtemp[c('21','29','37','45','53','61'),]<0,sumtemp[c('22','30','38','46','54','62'),],-sumtemp[c('22','30','38','46','54','62'),])
      sumtemp[c('24','32','40','48','56','64'),]=ifelse(sumtemp[c('23','31','39','47','55','63'),]<0,sumtemp[c('24','32','40','48','56','64'),],-sumtemp[c('24','32','40','48','56','64'),])
      sumtemp[c('25','33','41','49','57','65'),]=ifelse(sumtemp[c('22','30','38','46','54','62'),]<0,sumtemp[c('25','33','41','49','57','65'),],-sumtemp[c('25','33','41','49','57','65'),])
      sumtemp[c('26','34','42','50','58','66'),]=ifelse(sumtemp[c('21','29','37','45','53','61'),]<0,sumtemp[c('26','34','42','50','58','66'),],-sumtemp[c('26','34','42','50','58','66'),])
      sumtemp[c('27','35','43','51','59','67'),]=ifelse(sumtemp[c('24','32','40','48','56','64'),]<0,sumtemp[c('27','35','43','51','59','67'),],-sumtemp[c('27','35','43','51','59','67'),])
      sumtemp[c('28','36','44','52','60','68'),]=ifelse(sumtemp[c('23','31','39','47','55','63'),]<0,sumtemp[c('28','36','44','52','60','68'),],-sumtemp[c('28','36','44','52','60','68'),])
      
    }
    else if (seq==6){
      sumtemp=matrix(nrow = n,ncol =nrow(path))
      sumtemp[seq(1,n,2),]=first
      sumtemp[seq(2,n,2),]=second
      row.names(sumtemp)=19:68
      
      sumtemp[c('25','33','41','49','57','65'),]=ifelse(sumtemp[c('22','30','38','46','54','62'),]<0,sumtemp[c('25','33','41','49','57','65'),],-sumtemp[c('25','33','41','49','57','65'),])
      sumtemp[c('26','34','42','50','58','66'),]=ifelse(sumtemp[c('21','29','37','45','53','61'),]<0,sumtemp[c('26','34','42','50','58','66'),],-sumtemp[c('26','34','42','50','58','66'),])
      sumtemp[c('27','35','43','51','59','67'),]=ifelse(sumtemp[c('24','32','40','48','56','64'),]<0,sumtemp[c('27','35','43','51','59','67'),],-sumtemp[c('27','35','43','51','59','67'),])
      sumtemp[c('28','36','44','52','60','68'),]=ifelse(sumtemp[c('23','31','39','47','55','63'),]<0,sumtemp[c('28','36','44','52','60','68'),],-sumtemp[c('28','36','44','52','60','68'),])
      
    }
    
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

# 基础版：
plan0=run(all_data,path,0);plan0[[3]]
# plan25：
plan1=run(all_data,path,1);plan1[[3]]
#plan25-1:
plan1_1=run(all_data,path,2);plan1_1[[3]]
#plan25-2:
plan1_2=run(all_data,path,3);plan1_2[[3]]
# plan26:
plan2=run(all_data,path,4);plan2[[3]]
# plan27-1:
plan27_1=run(all_data,path,5);plan27_1[[3]]
# plan27-2:
plan27_2=run(all_data,path,6);plan27_2[[3]]



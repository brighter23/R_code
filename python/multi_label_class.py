# -*- coding: utf-8 -*-
# Filename:
import pandas as pd
import numpy as np
import os
import jieba
import datetime
import re
from sklearn.feature_extraction.text import CountVectorizer
from sklearn.feature_extraction.text import TfidfTransformer

# 多线程:
'''
from multiprocessing import Pool
res = []
    p = Pool(processors)
    for i in range(processors):
        res.append(p.apply_async(所需运算函数, args=(函数参数,i,)))
        print(str(i) + ' processor started !')
    p.close()
    p.join()
'''

'''
1、先对每篇文章分词形成一个list,以便计算tfidf值,tf与具体哪个文档相关，idf代表普遍重要性（不是由具体某一篇文档计算的，由所有文档中是否含有某个词计算；
2、选取tfidf值高的的前p个作为词典，进行tfidf编码{'word':tfidf_value}
3、不能直接用tfidf值，因为这个是在所有文档下计算的频率，但是可以作为这个词的重要性指标
4、三种编码：每个样本若出现这个词记为1，独热编码；该词出在文档出现的频率；出现频率用TFIDF加权；
5、
'''


# 根据股票代码和名称获取行业类别：
def industry_class_code(one_content, stock_frame):
    # 1、将目标文本中的4位港股和3位港股按照先4后3的顺序标准化成5位的
    one_content = re.sub(pattern="(\d{4}).HK", repl=r'0\1', string=one_content, flags=re.S)
    one_content = re.sub(pattern="(\d{3}).HK", repl=r'00\1', string=one_content, flags=re.S)
    # 2、提取目标文本股票代码：匹配5~6位数
    stock_code = re.findall(pattern=r'(\d{5,6})', string=one_content, flags=re.S)
    # 3、根据提取代码分行业：
    industry_class_bycode = []
    if len(stock_code) != 0:
        for code in stock_code:
            industry_class_bycode.extend(list(stock_frame['industry2'][stock_frame['st_code'] == int(code)]))
    # 4、根据名称分行业：
    industry_class_byname = []
    for name in stock_frame['name']:
        if name in one_content:
            industry_class_byname.extend(list(stock_frame['industry2'][stock_frame['name'] == name]))
    return list(set(industry_class_bycode + industry_class_byname))


# 读取数据：剔除短文本、得到行业类别推荐(时间较长……)
def data_pretreat(dtat_file, stock_file):
    # dtat_file= r"D:\py_project\text\data\new.xlsx"
    # stock_file=r"D:\py_project\text\data\stock_frame.xlsx"
    label_data = pd.read_excel(dtat_file)
    label_data = label_data[['title', 'content', 'class1_name']]
    # 1、将标题、内容整合：
    title_content = label_data.apply(lambda x: x['title'] + ' ' + x['content'], axis=1)
    # 2、去除短文本：小于400个字的不要，信息不足
    length = title_content.apply(len)
    title_content = title_content[length > 400]
    # 3、读取股票代码对应表：
    stock_frame = pd.read_excel(stock_file)
    # stock_frame.drop_duplicates(subset=['industry1', 'industry2', 'industry3'], keep='first')[
    #     ['industry1', 'industry2', 'industry3']].to_excel(r"D:\py_project\text\data\drop_duplicates_stock_frame.xlsx",
    #                                                       index=False)
    # 将港股代码标准成5位数,并将类型转化成int
    stock_frame['st_code'] = stock_frame['simple_code'].apply(
        lambda x: '{:0>5d}'.format(x) if len(str(x)) != 6 else x).astype('int')
    # 4、求行业推荐：
    label_data = label_data.loc[title_content.index]
    label_data['recommend'] = title_content.apply(lambda x: industry_class_code(x, stock_frame))
    label_data['title_content'] = title_content
    label_data.index = range(len(label_data))
    return label_data


#
def data_reading(file, num_test=10000, random_seed=2018):
    new_data = pd.read_csv(file)  # CSV的读取速度更快
    # 提取关注的变量：新闻标题和内容
    print('各个行业类别占比：\n', new_data.class_name.value_counts() / len(new_data) * 100, '%')
    # 划分训练集测试集和验证集:
    vail_test = new_data.sample(n=num_test * 2, random_state=random_seed)
    test = vail_test.sample(n=num_test, random_state=random_seed)
    vail = vail_test.drop(test.index, axis=0)
    train = new_data.drop(vail_test.index, axis=0)
    train.index = range(len(train))
    vail.index = range(len(vail))
    test.index = range(len(test))
    return train, vail, test


# 二、数据预处理，输入整个训练集，增加分好词和去除停用词的list:
def preprocsing(data, stop_words):
    title = data['title'].str.replace(pat=r"""[!#\$%&\'()\*+,-\./:;<=>\?@\[\\\\\]^_`{\|}~'【】《》：~；‘’。，？（）”“、—— \·\n]+""",
                                      repl='')
    content = data['content'].str.replace(
        pat=r"""[!#\$%&\'()\*+,-\./:;<=>\?@\[\\\\\]^_`{\|}~'【】《》：~；‘’。，？（）”“、—— \·\n]+""",
        repl='')
    title_spilt = title.apply(lambda x: ' '.join([word for word in jieba.lcut(x) if word not in stop_words]))
    content_spilt = content.apply(lambda x: ' '.join([word for word in jieba.lcut(x) if word not in stop_words]))
    data['title_spilt'] = title_spilt
    data['content_spilt'] = content_spilt
    return 0


# 三、计算tfidf值的字典：输入所有文章标题或者内容，输出ifidf值字典
def get_tfidf_dictionary():
    # vectorizer =
    # transformer =
    tfidf = TfidfTransformer(norm=None).fit_transform(CountVectorizer().fit_transform(train['title_spilt']))
    tfidf.toarray()


if __name__ == '__main__':
    start_time = datetime.datetime.now()
    print('一、筛选数据集并形成推荐行业')
    if not os.path.exists(r"D:\py_project\text\data\label_data.csv"):
        label_data = recommend = data_pretreat(dtat_file=r"D:\py_project\text\data\new.xlsx",
                                               stock_file=r"D:\py_project\text\data\stock_frame.xlsx")
        label_data['recommend_num'] = label_data['recommend'].apply(len)
        label_data.to_csv(r"D:\py_project\text\data\label_data.csv", index=False)
        label_data.to_excel(r"D:\py_project\text\data\label_data.xlsx", index=False)
    print('一、载入数据、词库和停止词……')
    train, vail, test = data_reading(r"D:\py_project\text\data\paper_data2.csv")
    jieba.load_userdict(r'D:\py_project\text\data\final\mycorpus.txt')
    stop_words = pd.read_csv(r'D:\py_project\text\data\final\stop.txt', header=None)
    stop_words = stop_words[0].tolist()

    print('二、数据预处理……')
    try:
        train['title_spilt']
    except:
        preprocsing(train, stop_words)
        preprocsing(vail, stop_words)
        preprocsing(test, stop_words)
    print('')
    end_time = datetime.datetime.now()
    print('时间：', end_time - start_time)

# -*- coding: utf-8 -*-
# !/D:/py_project/text/python3
# Filename: final.py

import pandas as pd
import numpy as np
import jieba
import jieba.analyse
import sys
import codecs
from textrank4zh import TextRank4Keyword, TextRank4Sentence
import re
import warnings
import os
from collections import Counter

warnings.filterwarnings(action='ignore', category=UserWarning, module='gensim')
from gensim.models import word2vec


# 一、读取数据==========================================================================
def data_pretreat(file):
    data = pd.read_excel(file)
    # 提取关注的变量：新闻标题和内容
    ourdata = data.iloc[:, [2, 3, 6, 8, 11]]
    ourdata.columns = ['title', 'content', 'reproduce', 'source', 'industry']
    print('各个行业类别：\n', ourdata.industry.value_counts())
    return ourdata


# 二、行业分类==========================================================================
# 将港股代码标准成5位数
def hk_stock_stander(code):
    st_code = []
    for i in code:
        if len(str(i)) == 6:
            st_code.append(str(i))
        else:
            st_code.append('{:0>5d}'.format(i))
    return st_code


def industry_class(data, stock_frame):
    st_code = hk_stock_stander(stock_frame.simple_code)  # 标准的港股代码
    # 提取标题和内容的代码：
    title_code = list(
        map(re.findall, np.repeat(r"(\b[0-9]{3}.HK\b|\b[0-9]{4}.HK\b|\b[0-9]{5}\b|\b[0-9]{6}\b)", len(data['title'])),
            data['title'].values.tolist()))
    content_code = list(
        map(re.findall, np.repeat(r"(\b[0-9]{3}.HK\b|\b[0-9]{4}.HK\b|\b[0-9]{5}\b|\b[0-9]{6}\b)", len(data['content'])),
            data['content'].values.tolist()))
    final_industry_class = []
    final_industry = []
    for i, j, k, l in zip(title_code, content_code, data['title'], data['content']):
        code = list(set(i + j))
        # 1、将代码转换成名称：
        cor_code_word = []
        for m in code:
            # 去除HK后转成5位数：
            m = '{:0>5d}'.format(int(re.findall(r"\d+", m)[0]))
            if m in st_code:
                cor_code_word = cor_code_word + [stock_frame['name'][st_code.index(m)]]
        # 2、提取文章和标题中含有的名称：
        cor_word_title = [word for word in stock_frame['name'] if word in k]
        cor_word_content = [word for word in stock_frame['name'] if word in l]
        # 3、合并所有股票名称：
        cor_word = list(set(cor_word_title + cor_word_content + cor_code_word))
        # 4、根据名称匹配：
        if len(cor_word) == 0:
            final_industry.append([0])
            final_industry_class.append(['随缘'])
        else:
            final_cor_word = list(set(cor_word).intersection(stock_frame['name']))
            industry = [[0]]
            industry_class1 = []
            for p in final_cor_word:
                industry.extend(stock_frame[stock_frame['name'] == p].values.tolist())
                industry_class1.extend(stock_frame[stock_frame['name'] == p]['industry3'].values.tolist())
            final_industry.append(industry)
            final_industry_class.append(Counter(industry_class1))

    return final_industry, final_industry_class


# 三、内容摘要=============================================================================
def textRank(doc):
    content_textRank = []
    tr4s = TextRank4Sentence()
    '''提取关键词一般，不建议使用
    tr4w = TextRank4Keyword()
     for i in doc:
        tr4w.analyze(text=i, lower=True, window=3)
        print('关键词：')
        for item in tr4w.get_keywords(10, word_min_len=1):
            print(item.word, item.weight)
        print('关键短语：')
        for phrase in tr4w.get_keyphrases(keywords_num=20, min_occur_num=2):
            print(phrase)
    '''
    for i in doc:
        textrank = []
        tr4s.analyze(text=i, lower=True, source='all_filters')
        for item in tr4s.get_key_sentences(num=5):
            textrank.append(item.sentence)  # index是语句在文本中位置，weight是权重
        content_textRank.append(textrank)
    return content_textRank


def reform(content_textRank):
    st_content_textRank = []
    for i in content_textRank:
        temp = str()
        for j in range(len(i)):
            temp = temp + str(' 第%d条摘要：%s。 ' % (j + 1, i[j]))
        st_content_textRank.append(temp)
    return st_content_textRank


# 四、tfidf提取关键词======================================================================
def jieba_tfidf(doc, working_path, data):
    # 去除符号（一般tfidf都会很大，因为在其他文章不怎么出现）：
    doc = doc.str.replace(pat=r"""[!#\$%&\'()\*+,-\./:;<=>\?@\[\\\\\]^_`{\|}~'【】《》：~；‘’。，？（）”“、—— \·\n]+""", repl='')
    # 载入自建语料，提高分词准确性
    jieba.load_userdict(working_path + r'\mycorpus.txt')
    # 去停用词:新闻报道常用词+新闻媒体名字
    stop = list(pd.read_table(working_path + r'\stop.txt').iloc[:, 0])
    stop.extend(
        ['快报', '每经', '读者', '记者', '稿件', 'ths', 'ths518', '以下简称', '专栏作家', '本报记者', '全文', '精彩内容', '字号', '早间', '早报', '晚间',
         '原作者', '责编', '字体', '个人观点', '今日', '上市公司', '日讯', '归属于', '该股', '万元',
         '新闻记者', '摘要', '凡本网', '作者', '责任编辑', '二维码', '笔者', '编辑', '月份'])
    stop.extend(list(set(list(data.reproduce) + list(data.source))))
    word = []
    for i in doc:
        # 分词并去除停用词：小技巧，分词时不去数字，但是去除停用词时去除数字，P2P、H7N9才会留下来
        temp = [j for j in jieba.lcut(i) if j not in stop and not j.isdigit()]
        word.append(jieba.analyse.tfidf(''.join(temp), topK=30))
    return word


# 五、训练词向量模型=======================================================================
# 生成模型训练语料：
def parseSent(doc, working_path, data):
    # 去除符号（一般tfidf都会很大，因为在其他文章不怎么出现）：
    doc = doc.str.replace(pat=r"""[\u3000!#\$%&\'()\*+,-\./:;<=>\?@\[\\\\\]^_`{\|}~'【】《》：~；‘’。，？（）”“、—— \·\n]+""",
                          repl='')
    # 载入自建语料，提高分词准确性
    jieba.load_userdict(working_path + r"\mycorpus.txt")
    # 去停用词:新闻报道常用词+新闻媒体名字
    stop = list(pd.read_table(working_path + r"\stop.txt").iloc[:, 0])
    stop.extend(
        ['快报', '每经', '读者', '记者', '稿件', 'ths', 'ths518', '以下简称', '专栏作家', '本报记者', '全文', '精彩内容', '字号', '早间', '早报', '晚间',
         '原作者', '责编', '字体', '个人观点', '今日', '上市公司', '日讯', '归属于', '该股', '万元',
         '新闻记者', '摘要', '凡本网', '作者', '责任编辑', '二维码', '笔者', '编辑', '月份'])
    stop.extend(list(set(list(data.reproduce) + list(data.source))))
    corpus = []
    key_word = []
    for i in doc:
        tempt = [j for j in jieba.cut(i) if j not in stop]
        key_word.append(tempt)
        corpus = corpus + tempt
    word2vec_corpus = " ".join(corpus)
    txtfile = codecs.open(working_path + r"\result\word2vec_corpus.txt", 'w', 'utf-8')
    txtfile.write("%s" % word2vec_corpus)
    txtfile.close()
    return 0


# 六、筛选出现在标题或者关键句中的tfidf关键词==============================================
# 筛选关键词：
def word_fliter(content_tiidf, data_title, content_textRank):
    final_fliter_word = []
    for tiidf, title, textRank in zip(content_tiidf, data_title, content_textRank):
        fliter_word = []
        for word in tiidf:
            # 将在标题或者关键句中的关键词筛选出来：
            if word in title:
                fliter_word.append(word)
            else:
                for topic in textRank:
                    if word in topic:
                        fliter_word.append(word)
        final_fliter_word.append(list(set(fliter_word)))
    return final_fliter_word


# 获取新闻推荐的词组：
def word_couple(recommend, num, model):
    import itertools
    all_recommend_word = []
    for words in recommend:
        word_similarity = []
        for i, j in list(itertools.combinations(range(len(words)), 2)):
            try:
                word_similarity.append([model.similarity(words[i], words[j]), words[i], words[j]])
            except KeyError:
                continue
        # 使用w2v计算的相似度排序，将最相近的词筛选出来
        word_similarity.sort(key=lambda x: x[0], reverse=True)
        recommend_word = []
        for k, i, j in word_similarity[0:num]:
            recommend_word.append((i, j))
        all_recommend_word.append(recommend_word)
    return all_recommend_word


# 将推荐词组重整，有共同关键词的放在一起：
def word_couple_reform(recommend_couple):
    final_sort_word = []
    for couple in recommend_couple:
        all_word = []
        for i, j in couple:
            all_word.extend([i] + [j])
        # 排序求高频的单词优先排在前面,有序的去重：
        sort_word = []
        for k, l in Counter(all_word).most_common():
            high_word = []
            for i, j in couple:
                if i == k:
                    if '(' + ','.join([i, j]) + ')' not in sort_word:  # 去重
                        high_word.append('(' + ','.join([i, j]) + ')')
                elif j == k:
                    if '(' + ','.join([i, j]) + ')' not in sort_word:  # 去重
                        high_word.insert(0, '(' + ','.join([i, j]) + ')')
            sort_word.extend(high_word)
        final_sort_word.append(sort_word)
    for i in range(len(final_sort_word)):
        final_sort_word[i] = '，'.join(final_sort_word[i])
    return final_sort_word


# 七、临时储存函数=========================================================================
# 将阶段性的结果存为 pkl 文件，方便下次调用
def dump_pkl(filename, dat):
    import pickle
    assert ".pkl" in filename  # 检查文件后缀是否正确
    with open(filename, "wb") as f:
        pickle.dump(dat, f)
    print("存储成功！")


# 下次调用时，可以使用如下代码从本地读取 cutted_docs.pkl 文件
def load_pkl(filePath):
    import pickle
    with open(filePath, "rb") as f:
        res = pickle.load(f)
    return res


def main():
    working_path = os.path.split(os.path.realpath(__file__))[0]  # 除非运行整个脚本，逐句运行时不要运行这句
    # 逐行运行时使用此句：working_path=os.getcwd()+r'\data\final'
    result_filename = working_path + r"\result"
    if not os.path.exists(result_filename):
        os.makedirs(result_filename)
    data = data_pretreat(working_path + '\data_of_10000.xlsx')
    print(' 1、读取股票代码及其行业类别的表格:', '\n')
    stock_frame = pd.read_excel(working_path + '\stock_frame.xlsx')
    print(' 2、获取股票分类推荐：', '\n')
    if os.path.exists(result_filename + r'\industry_class.pkl'):
        final_industry, final_industry_class = load_pkl(result_filename + r'\industry_class.pkl')
    else:
        final_industry, final_industry_class = industry_class(data, stock_frame)
        dump_pkl(result_filename + r'\industry_class.pkl', (final_industry, final_industry_class))
    print(' 3、获取内容摘要：', '\n')
    if os.path.exists(result_filename + r'\st_content_textRank.pkl'):
        st_content_textRank = load_pkl(result_filename + r'\st_content_textRank.pkl')
    else:
        content_textRank = textRank(data.content)
        st_content_textRank = reform(content_textRank)
        dump_pkl(result_filename + r'\st_content_textRank.pkl', st_content_textRank)
    print(' 4、tfidf提取内容关键词30个：', '\n')
    if os.path.exists(result_filename + r'\st_content_tiidf.pkl'):
        st_content_tiidf = load_pkl(result_filename + r'\st_content_tiidf.pkl')
    else:
        content_tiidf = jieba_tfidf(data.content, working_path, data)
        st_content_tiidf = list(map("，".join, content_tiidf))
        dump_pkl(result_filename + r'\st_content_tiidf.pkl', st_content_tiidf)
    print(' 5、词向量模型：', '\n')
    if os.path.exists(working_path + r"\result\corpus.model"):
        model = word2vec.Word2Vec.load(working_path + r"\result\corpus.model")
    else:
        parseSent(data.content, working_path, data)
        sentences = word2vec.Text8Corpus(working_path + r"\result\word2vec_corpus.txt")  # 加载语料
        model = word2vec.Word2Vec(sentences, sg=1, hs=1, window=10, min_count=5)  # 训练skip-gram模型
        model.save(working_path + r"\result\corpus.model")
        print('模型已保存')
    print(' 6、筛选关键词：', '\n')
    if os.path.exists(result_filename + r'\st_recommend_couple.pkl'):
        st_recommend_couple = load_pkl(result_filename + r'\st_recommend_couple.pkl')
    else:
        recommend_word = word_fliter(content_tiidf, data.title, content_textRank)
        # 获取20个的推荐词组：
        recommend_couple = word_couple(recommend=recommend_word, num=20, model=model)
        st_recommend_couple = word_couple_reform(recommend_couple)
        dump_pkl(result_filename + r'\st_recommend_couple.pkl', st_recommend_couple)
    mydata = data.iloc[:, [0, 1]]
    mydata['abstract'] = st_content_textRank
    mydata['tfidf_keyword'] = st_content_tiidf
    mydata['recommend_couple'] = st_recommend_couple
    mydata['industry'] = final_industry
    mydata['recommend_industry_class'] = final_industry_class
    mydata.to_excel(working_path + r'\result\mydata.xlsx', index=False)


if __name__ == '__main__':
    main()

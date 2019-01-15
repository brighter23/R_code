# -*- coding: utf-8 -*-
# Filename:

import datetime
import numpy as np
import pandas as pd
from matplotlib import pyplot as plt
import seaborn as sns
import re
from sklearn.feature_extraction.text import TfidfVectorizer
from sklearn.linear_model import LogisticRegression, LinearRegression, Lasso
from sklearn.metrics import f1_score, accuracy_score, confusion_matrix
from sklearn.naive_bayes import MultinomialNB
from sklearn.model_selection import KFold
from scipy import stats

'''
说明：

'''
pd.set_option('display.max_columns', 1000)
pd.set_option('display.width', 1000)
pd.set_option('display.max_colwidth', 1000)

pd.set_option('display.max_columns', 1000)
pd.set_option('display.width', 1000)
pd.set_option('display.max_colwidth', 1000)

'''
说明：
1、使用kaggle 毒评论数据，以分类为目的，多个标签，横向和纵向比较多个算法优劣
2、同时模拟数据，验证在哪种数据结构下，我们的算法优势最大
'''


# 标签的描述统计:不平衡性+相关性
def label_describe(data, cols_target):
    sns.set()
    plt.figure(1, figsize=(14, 14))

    print('标签分布个数', '*' * 100)
    print(data[cols_target].sum())  # 分布不平衡
    num_list = data[cols_target].sum()
    name_list = cols_target
    index = range(len(cols_target))
    plt.subplot(1, 2, 1)  # 子图1
    rects = plt.bar(index, num_list, color='rgby')
    # X轴标题
    plt.xticks(index, name_list)
    plt.ylabel("number of 1")  # X轴标签
    for rect in rects:
        height = rect.get_height()
        plt.text(rect.get_x() + rect.get_width() / 2, height, str(height), ha='center', va='bottom')
    plt.title('number of 1', y=1.05, size=14)

    plt.subplot(1, 2, 2)  # 子图2
    colormap = plt.cm.plasma
    plt.title('Correlation of labels', y=1.05, size=14)
    sns.heatmap(data[cols_target].astype(float).corr(), linewidths=0.1, vmax=1.0, square=True, cmap=colormap,
                linecolor='white', annot=True)


# 文本数据预处理:统一小写，替换缩写，去除首尾空格、换行符
def clean_text(text):
    text = text.lower()
    text = re.sub(r"what's", "what is ", text)
    text = re.sub(r"\'s", " ", text)
    text = re.sub(r"\'ve", " have ", text)
    text = re.sub(r"can't", "cannot ", text)
    text = re.sub(r"n't", " not ", text)
    text = re.sub(r"i'm", "i am ", text)
    text = re.sub(r"\'re", " are ", text)
    text = re.sub(r"\'d", " would ", text)
    text = re.sub(r"\'ll", " will ", text)
    text = re.sub(r"\'scuse", " excuse ", text)
    text = re.sub('\W', ' ', text)
    text = re.sub('\s+', ' ', text)
    text = text.strip(' ')
    return text


# 汉明损失：
def Hamming_Loss(pred_matrix, label_matrix):
    label_matrix.index = range(len(label_matrix))
    pred_matrix.index = range(len(pred_matrix))  # dateframe坑，索引
    tempt_df = pd.concat([label_matrix, pred_matrix], axis=1)
    num_label = label_matrix.shape[1]
    # 计算预测与实际不一样的标签个数
    loss = tempt_df.apply(lambda x: np.shape(np.nonzero(x[0:num_label].values - x[num_label:].values)[0])[0], axis=1)
    return loss.mean()


# 1、二元独立算法：逻辑回归&多项式贝叶斯,默认阈值0.5
def Binary_Relevance(cols_target, X_train, X_test, y_train, y_test, logist, unbalance_target=None):
    if logist:
        reg = LogisticRegression(penalty='l1')
    else:
        reg = MultinomialNB()
    if unbalance_target == None:
        print('不考虑数据不平衡')
    print(reg)
    print('-' * 100)
    acc = []
    f1 = []
    pred_train = pd.DataFrame()
    pred_test = pd.DataFrame()
    for label in range(len(cols_target)):
        print('... Processing {}'.format(cols_target[label]))
        # 二元独立训练每个标签
        reg.fit(X_train, y_train.values[:, label])
        # 训练集准确度
        pred_train[cols_target[label]] = reg.predict(X_train)
        print(
            'Training accuracy is {}'.format(accuracy_score(y_train.values[:, label], pred_train[cols_target[label]])))
        # 测试集
        pred_test_temp = reg.predict(X_test)
        print('test accuracy is {}'.format(accuracy_score(y_test.values[:, label], pred_test_temp)))
        acc.append(accuracy_score(y_test.values[:, label], pred_test_temp))
        print(confusion_matrix(y_test.values[:, label], pred_test_temp))  # 混淆矩阵
        print("test f1_score is {}".format(f1_score(y_test.values[:, label], pred_test_temp)))
        f1.append(f1_score(y_test.values[:, label], pred_test_temp))
        pred_test[cols_target[label]] = pred_test_temp
    return acc, f1, Hamming_Loss(pred_test, y_test)


# 标签特征化函数：
def add_feature(X, feature_to_add):
    '''
    Returns sparse feature matrix with added feature.
    feature_to_add can also be a list of features.
    '''
    from scipy.sparse import csr_matrix, hstack
    if csr_matrix(feature_to_add).shape[0] == 1:
        return hstack([X, csr_matrix(feature_to_add).T], 'csr')
    else:
        return hstack([X, csr_matrix(feature_to_add)], 'csr')


# 3、分类链算法：
def Classifier_Chains(cols_target, X_train, X_test, y_train, y_test, logist, unbalance_target=None):
    if logist:
        reg = LogisticRegression(penalty='l1')
    else:
        reg = MultinomialNB()
    if unbalance_target == None:
        print('不考虑数据不平衡')
    print(reg)
    print('-' * 100)
    acc = []
    f1 = []
    pred_test = pd.DataFrame()
    for label in range(len(cols_target)):
        print('... Processing {}'.format(cols_target[label]))
        reg.fit(X_train, y_train.values[:, label])
        # 训练集准确度
        pred_train = reg.predict(X_train)
        print('Training accuracy is {}'.format(accuracy_score(y_train.values[:, label], pred_train)))
        # 测试集
        pred_test_tempt = reg.predict(X_test)
        pred_test[cols_target[label]] = pred_test_tempt
        # test_pred_X = np.where(test_y_prob > y_train[label].mean(), 1, 0)
        print('test accuracy is {}'.format(accuracy_score(y_test.values[:, label], pred_test_tempt)))
        acc.append(accuracy_score(y_test.values[:, label], pred_test_tempt))
        print(confusion_matrix(y_test.values[:, label], pred_test_tempt))  # 混淆矩阵
        print("test f1_score is {}".format(f1_score(y_test.values[:, label], pred_test_tempt)))
        f1.append(f1_score(y_test.values[:, label], pred_test_tempt))
        # 将前面的标签加入后面的分类器中：
        X_train = add_feature(X_train, y_train.values[:, label])
        print('Shape of X_dtm is now {}'.format(X_train.shape))
        # 对于测试集使用预测值代入
        X_test = add_feature(X_test, pred_test_tempt)
        print('Shape of test_X_dtm is now {}'.format(X_test.shape))
    return acc, f1, Hamming_Loss(pred_test, y_test)


# 4、有的放矢的标签特征化分类：阈值不调整，两阶段，对于第一阶段分类效果好的不进行修正
def Classifier_net(cols_target, X_train, X_test, y_train, y_test, alpha, logist, unbalance_target=None,
                   acc_threshold=0.7):
    if logist:
        reg = LogisticRegression(penalty='l1')
    else:
        reg = MultinomialNB()
    if unbalance_target == None:
        print('不考虑数据不平衡')
    print(reg)
    print('-' * 100)
    # 第一阶段：确定最佳阈值和标签相关关系：
    stage1_prob_train = pd.DataFrame()
    stage1_test = pd.DataFrame()
    stage1_res = pd.DataFrame()
    train_acc = []
    for label in range(len(cols_target)):
        print('... Processing {}'.format(cols_target[label]))
        reg.fit(X_train, y_train.values[:, label])
        stage1_prob_train[cols_target[label]] = reg.predict_proba(X_train)[:, 1]
        stage1_test[cols_target[label]] = reg.predict_proba(X_test)[:, 1]
        stage1_res[cols_target[label]] = y_train.values[:, label] - stage1_prob_train[cols_target[label]]
        train_acc.append(accuracy_score(y_train.values[:, label], reg.predict(X_train)))
    # "好的"标签：
    good_labels = list(np.array(cols_target)[np.array(train_acc) > acc_threshold])
    # 2.1、变量选择,获取标签相关性：会去除一些标签，所以使用cols_target_temp
    correct_target = []
    for label in range(len(cols_target)):
        cols_target_temp = cols_target.copy()
        taget = cols_target_temp[label]
        cols_target_temp.remove(cols_target_temp[label])
        result = Lasso(alpha=alpha, fit_intercept=True).fit(stage1_res[cols_target_temp], stage1_res[taget])
        correct_cols_target = []
        for i in range(len(cols_target_temp)):
            if result.coef_[i] != 0:
                if cols_target_temp[i] in good_labels:
                    correct_cols_target.append(cols_target_temp[i])
        correct_target.append(correct_cols_target)
    # 将相关标签特征化
    acc = []
    f1 = []
    pred_test = pd.DataFrame()
    for label in range(len(cols_target)):
        print('... Processing {}'.format(cols_target[label]))
        # 将相关的标签加入后面的分类器中：当为空时加入0向量
        if y_train[correct_target[label]].shape[1] == 0:
            new_X_train = X_train
        else:
            new_X_train = add_feature(X_train, y_train[correct_target[label]].values)
        print('Shape of X_dtm is now {}'.format(new_X_train.shape))

        reg.fit(new_X_train, y_train.values[:, label])
        # 训练集准确度
        pred_train = reg.predict(new_X_train)
        print('Training accuracy is {}'.format(accuracy_score(y_train.values[:, label], pred_train)))

        # 测试集:使用新的特征：
        if y_train[correct_target[label]].shape[1] == 0:
            new_X_test = X_test
        else:
            new_X_test = add_feature(X_test, stage1_test[correct_target[label]].values)
        pred_test_tempt = reg.predict(new_X_test)
        pred_test[cols_target[label]] = pred_test_tempt
        print('test accuracy is {}'.format(accuracy_score(y_test.values[:, label], pred_test_tempt)))
        acc.append(accuracy_score(y_test.values[:, label], pred_test_tempt))
        print(confusion_matrix(y_test.values[:, label], pred_test_tempt))  # 混淆矩阵
        print("test f1_score is {}".format(f1_score(y_test.values[:, label], pred_test_tempt)))
        f1.append(f1_score(y_test.values[:, label], pred_test_tempt))
    return acc, f1, Hamming_Loss(pred_test, y_test)


def estimate_threshold(pre_prob, n=1000):
    var_ingropu = []
    for i in range(0, n):
        var_ingropu.append(
            pre_prob[pre_prob < (i / n)].var() + pre_prob[pre_prob > (i / n)].var())
    result = pd.DataFrame(var_ingropu)
    return result[result[0] == np.nanmin(var_ingropu)].index[-1] / n  # 忽视缺失值取最小


# 5、我的算法:两阶段训练法
def two_Classifier_net(cols_target, X_train, X_test, y_train, y_test, alpha, logist, unbalance_target=None,
                       acc_threshold=0.7, threshold_adjust=2):
    if logist:
        reg = LogisticRegression(penalty='l1')
    else:
        reg = MultinomialNB()
    print(reg)
    print('-' * 100)
    # 第一阶段：确定最佳阈值和标签相关关系：
    stage1_prob_train = pd.DataFrame()
    stage1_prob_test = pd.DataFrame()
    stage1_res = pd.DataFrame()
    train_acc = []
    for label in range(len(cols_target)):
        print('... Processing {}'.format(cols_target[label]))
        reg.fit(X_train, y_train.values[:, label])
        stage1_prob_train[cols_target[label]] = reg.predict_proba(X_train)[:, 1]
        stage1_prob_test[cols_target[label]] = reg.predict_proba(X_test)[:, 1]
        stage1_res[cols_target[label]] = y_train.values[:, label] - stage1_prob_train[cols_target[label]]
        train_acc.append(accuracy_score(y_train.values[:, label], reg.predict(X_train)))
    # 1.1\阈值移动：
    if threshold_adjust == 1:
        # 1、确定最佳阈值：使得错分率最小
        # 先根据实际标签分成1 的一类取最小，0的一类取最大的概率预测值，再2个中取最小的那个，最为最佳阈值
        threshold = []
        for i in range(stage1_prob_train.shape[0]):
            # 这里注意是np.nanmin，因为会产生缺失值
            threshold.append(
                np.nanmin(
                    [stage1_prob_train.iloc[i, y_train.values[i] == 1].min(),
                     stage1_prob_train.iloc[i, y_train.values[i] == 0].max()]))
        # 阈值与预测概率的OLS方程：
        stage1_prob_train['threshold'] = threshold
        linear_reg = LinearRegression(normalize=False)
        linear_reg.fit(stage1_prob_train.drop('threshold', axis=1), stage1_prob_train['threshold'])
        test_threshold = linear_reg.predict(stage1_prob_test)

    # 1.2、变量选择,获取标签相关性：会去除一些标签，所以使用cols_target_temp
    # "好的"标签：
    good_labels = list(np.array(cols_target)[np.array(train_acc) > acc_threshold])
    # 1.3、变量选择,获取标签相关性：会去除一些标签，所以使用cols_target_temp
    correct_target = []
    for label in range(len(cols_target)):
        cols_target_temp = cols_target.copy()
        taget = cols_target_temp[label]
        cols_target_temp.remove(cols_target_temp[label])
        result = Lasso(alpha=alpha, fit_intercept=True).fit(stage1_res[cols_target_temp], stage1_res[taget])
        correct_cols_target = []
        for i in range(len(cols_target_temp)):
            if result.coef_[i] != 0:
                if cols_target_temp[i] in good_labels:
                    correct_cols_target.append(cols_target_temp[i])
        correct_target.append(correct_cols_target)
    # 第二阶段：相关标签特化
    acc = []
    f1 = []
    pred_test = pd.DataFrame()
    for label in range(len(cols_target)):
        print('... Processing {}'.format(cols_target[label]))
        # 将相关的标签加入后面的分类器中：当为空时不加入
        if y_train[correct_target[label]].shape[1] == 0:
            new_X_train = X_train
        else:
            new_X_train = add_feature(X_train, y_train[correct_target[label]].values)
        print('Shape of X_dtm is now {}'.format(new_X_train.shape))
        # 对于测试集使用预测值代入
        if y_train[correct_target[label]].shape[1] == 0:
            new_X_test = X_test
        else:
            new_X_test = add_feature(X_test, stage1_prob_test[correct_target[label]].values)
        print('Shape of test_X_dtm is now {}'.format(new_X_test.shape))

        reg.fit(new_X_train, y_train.values[:, label])
        # 训练集准确度
        pred_train = reg.predict(new_X_train)
        print('Training accuracy is {}'.format(accuracy_score(y_train.values[:, label], pred_train)))
        # 测试集:使用新的特征：
        pred_prob_test = reg.predict_proba(new_X_test)[:, 1]
        if cols_target[label] in unbalance_target:
            if threshold_adjust == 1:
                # 使用联合阈值的方式预测每个样本的阈值
                pred_test_tempt = np.where(pred_prob_test > test_threshold, 1, 0)
            elif threshold_adjust == 2:
                # 使用“断点方式”预测每个标签的阈值
                pred_test_tempt = np.where(pred_prob_test > estimate_threshold(stage1_prob_test[cols_target[label]]), 1,
                                           0)
            else:
                # 不进行阈值移动
                print('不平衡标签不进行阈值移动')
                pred_test_tempt = np.where(pred_prob_test > 0.5, 1, 0)

        else:
            pred_test_tempt = np.where(pred_prob_test > 0.5, 1, 0)
        pred_test[cols_target[label]] = pred_test_tempt
        print('test accuracy is {}'.format(accuracy_score(y_test.values[:, label], pred_test_tempt)))
        acc.append(accuracy_score(y_test.values[:, label], pred_test_tempt))
        print(confusion_matrix(y_test.values[:, label], pred_test_tempt))  # 混淆矩阵
        print("test f1_score is {}".format(f1_score(y_test.values[:, label], pred_test_tempt)))
        f1.append(f1_score(y_test.values[:, label], pred_test_tempt))
    return acc, f1, Hamming_Loss(pred_test, y_test)


# 6、k-折交叉验证
def Cross_validation(X_dtm, Y, algorithm, unbalance_target=None, alpha=0.0, n_splits=5):
    kf = KFold(n_splits=n_splits)
    q = Y.shape[1]
    acc_logist = np.zeros(q)
    f1_logist = np.zeros(q)
    Hamming_Loss_logist = np.zeros(1)
    acc_Bayes = np.zeros(q)
    f1_Bayes = np.zeros(q)
    Hamming_Loss_Bayes = np.zeros(1)
    k = 0
    for train, test in kf.split(X_dtm):
        k += 1
        print('第', k, '折')
        X_train, X_test, y_train, y_test = X_dtm[train], X_dtm[test], Y.loc[train], Y.loc[test]
        if alpha != 0:
            acc_temp1, f1_temp1, h_l1 = algorithm(cols_target, X_train, X_test, y_train, y_test, alpha,
                                                  unbalance_target=unbalance_target, logist=True)
            acc_temp2, f1_temp2, h_l2 = algorithm(cols_target, X_train, X_test, y_train, y_test, alpha,
                                                  unbalance_target=unbalance_target, logist=False)
        else:
            acc_temp1, f1_temp1, h_l1 = algorithm(cols_target, X_train, X_test, y_train, y_test,
                                                  unbalance_target=unbalance_target, logist=True)  # 逻辑回归

            acc_temp2, f1_temp2, h_l2 = algorithm(cols_target, X_train, X_test, y_train, y_test,
                                                  unbalance_target=unbalance_target, logist=False)  # 多项式贝叶斯

        acc_logist += acc_temp1
        f1_logist += f1_temp1
        Hamming_Loss_logist += h_l1
        acc_Bayes += acc_temp2
        f1_Bayes += f1_temp2
        Hamming_Loss_Bayes += h_l2
        print('*' * 100)
    return acc_logist / n_splits, f1_logist / n_splits, Hamming_Loss_logist / n_splits, acc_Bayes / n_splits, f1_Bayes / n_splits, Hamming_Loss_Bayes / n_splits


# 7、数据模拟：产生数据
def generate_data(n, p, q, corr_num, balance, random_seed, corr_strength=.5):
    # 可观测特征X：
    X = pd.DataFrame()
    for i in range(p):
        X['feature' + str(i)] = stats.norm.rvs(0, 1, size=n, random_state=i)
    # β系数:信号强弱,不同标签不同，因此是一个矩阵:一行代表一个标签的特征的系数
    beta_X = np.array(np.zeros(p * q)).reshape(q, p)
    for j in range(q):
        beta_X[j] = stats.norm.rvs(0, 1, size=p, random_state=random_seed + j)
        beta_X[j] = np.where(np.abs(beta_X[j]) < 1, 0, beta_X[j])
        # 不可观测e
    e = pd.DataFrame()  # TODO e是造成标签相关的“潜在变量” 因子分析？
    for i in range(q):
        e['latent' + str(i)] = stats.norm.rvs(0, 1, size=n, random_state=i + 10000)
    beta_latent = stats.norm.rvs(0, 1, size=q, random_state=random_seed) * 5
    Y = pd.DataFrame()
    for j in range(q):
        # 随机产生相关标签:1表示相关
        corr_latent = stats.binom.rvs(1, corr_num, size=q, random_state=j + 20000)  # 控制标签之间的相关程度
        # 计算概率：优势比--》概率
        odd_rate = np.exp(np.dot((1 - corr_strength) * X.values, beta_X[j]) +
                          corr_strength * np.dot(e.values, np.where(corr_latent == 0, 0, beta_latent)))
        if balance:
            pi = odd_rate / (1 + odd_rate)  # 用这种方式产生的数据移一定是均衡数据
        else:
            if j % 2 == 0:
                pi = odd_rate / (1 + odd_rate)  # 用这种方式产生的数据移一定是均衡数据
            else:
                pi = 0.5 * odd_rate / (1 + odd_rate)  # 用这种方式产生的数据移一定是均衡数据
            # 二项分布，n=1为伯努利分布：
        Y['label' + str(j)] = stats.binom.rvs(1, pi, size=n, random_state=j + 30000)
    return pd.concat([X, Y], axis=1)


# 8、模拟数据的交叉验证
def simu_Cross_validation(X_dtm, Y, algorithm, unbalance_target=None, alpha=0.0, n_splits=5):
    kf = KFold(n_splits=n_splits)
    q = Y.shape[1]
    acc_logist = np.zeros(q)
    f1_logist = np.zeros(q)
    Hamming_Loss_logist = np.zeros(1)
    k = 0
    for train, test in kf.split(X_dtm):
        k += 1
        print('第', k, '折')
        X_train, X_test, y_train, y_test = X_dtm.values[train], X_dtm.values[test], Y.loc[train], Y.loc[test]
        if alpha != 0:
            acc_temp1, f1_temp1, h_l1 = algorithm(cols_target, X_train, X_test, y_train, y_test, alpha,
                                                  unbalance_target=unbalance_target, logist=True)
        else:
            acc_temp1, f1_temp1, h_l1 = algorithm(cols_target, X_train, X_test, y_train, y_test,
                                                  unbalance_target=unbalance_target, logist=True)  # 逻辑回归

        acc_logist += acc_temp1
        f1_logist += f1_temp1
        Hamming_Loss_logist += h_l1
        print('*' * 100)
    return acc_logist.mean() / n_splits, f1_logist.mean() / n_splits, Hamming_Loss_logist / n_splits


####################################################################################
start_time = datetime.datetime.now()
data = pd.read_csv('data/train.csv')
cols_target = ['obscene', 'insult', 'toxic', 'severe_toxic', 'identity_hate', 'threat']

print('1、描述统计……')
# 特征X的分布
data['char_length'] = data['comment_text'].apply(lambda x: len(str(x)))  # 长度
sns.set()
data['char_length'].hist()
plt.show()
# 便签的描述统计
label_describe(data, cols_target)

print('2、数据预处理……')
data['comment_text'] = data['comment_text'].apply(clean_text)
# 向量化：使用tiidf值进行向量化,使用5000个特征词
word_vector = TfidfVectorizer(max_features=5000, stop_words='english')  # 5000个的特征词
X_dtm = word_vector.fit_transform(data['comment_text'])
print(X_dtm)  # 显示非0位置，不将长度作为特征加入，因为非0的个数即为长度信息
Y = data[cols_target]
unbalance_target = ['severe_toxic', 'identity_hate', 'threat']  # 不平衡的标签

print('3、模型比较：', '*' * 100)
print('二元独立+5折交叉验证……')
result_Binary_Relevance = Cross_validation(X_dtm=X_dtm, Y=Y, algorithm=Binary_Relevance,
                                           unbalance_target=unbalance_target, alpha=0, n_splits=5)

print('分类链+5折交叉验证……')
result_Classifier_Chains = Cross_validation(X_dtm=X_dtm, Y=Y, algorithm=Classifier_Chains,
                                            unbalance_target=unbalance_target, alpha=0, n_splits=5)

print('分类网+5折交叉验证……')
result_Classifier_net = Cross_validation(X_dtm=X_dtm, Y=Y, algorithm=Classifier_net,
                                         unbalance_target=unbalance_target, alpha=0.002, n_splits=5)

print('阈值+分类网+5折交叉验证……')
result_two_Classifier_net = Cross_validation(X_dtm=X_dtm, Y=Y, algorithm=two_Classifier_net,
                                             unbalance_target=unbalance_target, alpha=0.002, n_splits=5)

print('4、数据模拟:', '*' * 100)
# 1、标签独立的数据，平衡数据
p = 200
q = 30
data = generate_data(n=10000, p=p, q=q, corr_num=0.7, corr_strength=0.5, balance=False, random_seed=2019)
cols_target = ['label' + str(i) for i in range(q)]
unbalance_target = None  # 不平衡的标签
X_dtm = data.drop(cols_target, axis=1)
Y = data[cols_target]
# # 特征X的分布
# X_dtm.describe()
# label_describe(data, cols_target)

print('二元独立+5折交叉验证……')
simu_Binary_Relevance = simu_Cross_validation(X_dtm, Y, Binary_Relevance, n_splits=5,
                                              unbalance_target=unbalance_target)
print('分类链+5折交叉验证……')
simu_Classifier_Chains = simu_Cross_validation(X_dtm, Y, Classifier_Chains, n_splits=5,
                                               unbalance_target=unbalance_target)

print('分类网+5折交叉验证……')
simu_Classifier_net = simu_Cross_validation(X_dtm, Y, Classifier_net, n_splits=5,
                                            unbalance_target=unbalance_target, alpha=0.05)

print('阈值+分类网+5折交叉验证……')
unbalance = ['label' + str(i) for i in range(q) if i % 2 != 0]
simu_two_Classifier_net = simu_Cross_validation(X_dtm, Y, two_Classifier_net, n_splits=5,
                                                unbalance_target=unbalance, alpha=0.05)

end_time = datetime.datetime.now()
print('时间：', end_time - start_time)

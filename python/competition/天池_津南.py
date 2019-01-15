# -*- coding: utf-8 -*-
import datetime
import numpy as np
import pandas as pd
import re
import seaborn as sns
import matplotlib as mpl
from sklearn.cluster import DBSCAN
from sklearn import metrics
from sklearn.preprocessing import StandardScaler
import matplotlib.pyplot as plt
from sklearn.model_selection import KFold, RepeatedKFold
import lightgbm as lgb
import xgboost as xgb
from sklearn.metrics import mean_squared_error
from sklearn.ensemble import RandomForestRegressor
from sklearn.linear_model import BayesianRidge
import warnings

sns.set_style("whitegrid")
mpl.rcParams['font.sans-serif'] = ['SimHei']  # 指定默认字体
mpl.rcParams['axes.unicode_minus'] = False  # 解决保存图像是负号'-'显示为方块的问题
warnings.simplefilter(action='ignore', category=FutureWarning)
warnings.filterwarnings("ignore")
pd.set_option('display.max_columns', 1000)
pd.set_option('display.width', 1000)
pd.set_option('display.max_colwidth', 1000)


# 查看某个变量在训练集和测试集的分布以及其对关注变量的影响
def variable_distribution(train, test, col_name, tag_name, dot=False):
    plt.figure(1, figsize=(15, 10))
    plt.subplot(2, 2, 1)
    train_tem = train.copy()
    test_tem = test.copy()
    train_tem['level'] = 'train'
    test_tem['level'] = 'test'
    tempt = pd.concat([train_tem[[col_name, 'level']], test_tem[[col_name, 'level']]], axis=0)
    sns.violinplot(x=col_name, y='level', data=tempt)
    plt.xlabel('特征%s的取值范围' % col_name)
    plt.title('%s在训练集和测试集的分布' % col_name)
    plt.subplot(2, 2, 3)
    sns.boxplot(x=col_name, y=tag_name, data=train)
    if dot:
        sns.stripplot(x=col_name, y=tag_name, data=train)
    plt.xlabel('特征%s的取值范围' % col_name)
    plt.ylabel('%s的取值范围' % tag_name)
    plt.title('不同%s下%s的箱线图' % (col_name, tag_name))
    plt.subplot(2, 2, 2)
    plt.plot(train[tag_name], c='red')
    plt.xlabel('样本索引')
    plt.ylabel('训练集%s的取值' % tag_name)
    plt.title('训练集%s的折线图' % tag_name)
    plt.subplot(2, 2, 4)
    plt.plot(train[col_name], c='green')
    plt.xlabel('样本索引')
    plt.ylabel('训练集%s的取值' % col_name)
    plt.title('训练集%s的折线图' % col_name)


# variable_distribution(train, test, 'DBSCAN_class', '收率',True)
def train_clean(train):
    train['B11'][87] = '22:00-12:00'  # cant judge 12 -1
    train.drop(train.index[87])
    train['B11'][476] = '10:30-12:00'  # '12:30-12:00'
    train['B10'][166] = '19:00-21:00'  # '9:00-21:00'
    train['B9'][687] = '2:00-7:00'  # '2:00-17:00' cant judge 2 -1
    train.drop(train.index[687])
    train['B9'][1012] = '23:00-7:30'  # cant judge 23 -1
    train.drop(train.index[1012])
    train['B9'][484] = '11:00-14:00'  # '11:00-4:00'
    train['A28'][1259] = '14:40-16:10'  # '17:40-16:10' cant judge 1140 1630
    train.drop(train.index[1259])
    train['A28'][44] = '17:00-18:30'  # 13:00-18:30
    train['A28'][683] = '11:00-18:00'  # 11:00-18:00
    train['A20'][582] = '18:30-19:00'  # 18:30-15:00
    train['A25'][1304] = 70
    train['A25'] = train['A25'].astype('int')
    train.loc[train['B14'] == 40, 'B14'] = 400
    train.loc[train['B1'] == 3.5, 'B1'] = 350
    train['B4'][44] = '22:00-0:00'
    train.loc[train['A20'] == '6:00-6:30分', 'A20'] = '6:00-6:30'
    # train.loc[[580, 1129], 'A20'] = None
    # train.loc[812, 'B4'] = None
    return train


def feature_drop(train, test):
    for df in [train, test]:
        df.drop(['B3', 'B13', 'A13', 'A18', 'A23'], axis=1, inplace=True)

        # 删除缺失率超过90%的列
    good_cols = list(train.columns)
    for col in train.columns:
        rate = train[col].value_counts(normalize=True, dropna=False).values[0]
        if rate > 0.9:
            good_cols.remove(col)
            print(col)
    test['收率'] = 0
    train = train[good_cols]
    test = test[good_cols]

    return train, test


def feature_rate(train, test):
    # B14
    f1 = 'B14'
    num_list = ['A1', 'A3', 'A4', 'A19', 'B1', 'B12']
    for f2 in num_list:
        for df in [train, test]:
            df[f2 + '/' + f1] = df[f2] / df[f1]
            df[f2 + '/' + f1] = df[f2 + '/' + f1].round(1)

    for df in [train, test]:
        df['b14/a1_a3_a4_a19_b1_b12'] = df['B14'] / (df['A4'] + df['A19'] + df['B1'] + df['B12'])
        df['b14/a1_a3_a4_a19_b1_b12'] = df['b14/a1_a3_a4_a19_b1_b12'].round(2)
    return train, test


# 根据分位数离散化编码
def discretization(df_feature):
    # 三个分位数临界值
    a = df_feature.quantile(0.25)
    b = df_feature.quantile(0.5)
    c = df_feature.quantile(0.75)
    new_feature = []
    for i in df_feature:
        if i > c:
            new_feature.append(4)
        elif (i > b) & (i < c):
            new_feature.append(3)
        elif (i > a) & (i < b):
            new_feature.append(2)
        else:
            new_feature.append(1)
    return new_feature


def base_feature(train, test):
    # 时长编码：
    def time_length(duration):
        try:
            duration = re.sub('24', '00', duration)
            duration = re.sub(':-', '-', duration)
            duration = re.sub(';', ':', duration)
            duration = re.sub('::', ':', duration)
            duration = re.sub('；', ':', duration)
            duration = re.sub('"', ':', duration)

            times = pd.to_datetime(re.split('-', duration), errors='coerce', format='%H:%M')
            if times[1] > times[0]:
                return (times[1] - times[0]).seconds / 3600
            else:
                return 24 - (times[0] - times[1]).seconds / 3600
        except:
            return None

    # 时刻编码：
    def time_class(hour):
        times = pd.to_datetime(hour, errors='coerce', format='%H:%M:%S')
        morning_time = pd.to_datetime('1900-01-01 6:00:00')
        afternoon_time = pd.to_datetime('1900-01-01 12:00:00')
        evening_time = pd.to_datetime('1900-01-01 20:30:00')
        if (times > morning_time) & (times < afternoon_time):
            return 0
        elif (times > afternoon_time) & (times < evening_time):
            return 1
        else:
            return 2

    # 计算两个输入时刻的时长
    def time_used(hour1, hour2):
        try:
            times1 = pd.to_datetime(hour1, errors='coerce', format='%H:%M:%S')
            times2 = pd.to_datetime(hour2, errors='coerce', format='%H:%M:%S')
            return (times2 - times1).seconds / 3600
        except:
            return None

    # 使用中位数填补
    def data_fill(data):
        fill_data = data.copy()
        null_list = dict(fill_data.isnull().sum())  # 缺失值填补
        for label, num in null_list.items():
            if num > 0:
                fill_data.loc[fill_data[label].isnull(), label] = fill_data[label].median()
        return fill_data

    # 密度聚类：
    def my_DBSCAN(X, eps=0.3, min_samples=10, present=True):
        X = StandardScaler().fit_transform(X)
        db = DBSCAN(eps=eps, min_samples=min_samples).fit(X)
        core_samples_mask = np.zeros_like(db.labels_, dtype=bool)
        core_samples_mask[db.core_sample_indices_] = True
        labels = db.labels_  # 负1表示异常值
        # Number of clusters in labels, ignoring noise if present.
        n_clusters_ = len(set(labels)) - (1 if -1 in labels else 0)
        n_noise_ = list(labels).count(-1)
        print('Estimated number of clusters: %d' % n_clusters_)
        print('Estimated number of noise points: %d' % n_noise_)
        print("Silhouette Coefficient: %0.3f" % metrics.silhouette_score(X, labels))
        if present:
            # Black removed and is used for noise instead.
            unique_labels = set(labels)
            colors = [plt.cm.Spectral(each)
                      for each in np.linspace(0, 1, len(unique_labels))]
            for k, col in zip(unique_labels, colors):
                if k == -1:
                    # Black used for noise.
                    col = [0, 0, 0, 1]

                class_member_mask = (labels == k)

                xy = X[class_member_mask & core_samples_mask]
                plt.plot(xy[:, 0], xy[:, 1], 'o', markerfacecolor=tuple(col),
                         markeredgecolor='k', markersize=14)

                xy = X[class_member_mask & ~core_samples_mask]
                plt.plot(xy[:, 0], xy[:, 1], 'o', markerfacecolor=tuple(col),
                         markeredgecolor='k', markersize=6)
            plt.title('Estimated number of clusters: %d' % n_clusters_)
            plt.show()
        return labels

    # 分箱特征
    def get_newfeature(data):
        cate_columns = ['A6', 'A10', 'A12', 'A15', 'A17', 'A19', 'A21', 'A22',
                        'A25', 'A27', 'B1', 'B6', 'B8', 'B12', 'B14']

        # 将少于3个类别归类
        for f in cate_columns:
            temp = data[f].value_counts()
            few_feature = temp[temp < 3].index
            data.loc[data[f].isin(few_feature), f] = -2

        train = data[data['level'] == 'train']
        test = data[data['level'] == 'test']
        target = data[data['level'] == 'train']['收率']
        train['target'] = target.reset_index(drop=True)
        train['intTarget'] = pd.cut(train['target'], 5, labels=False)
        train = pd.get_dummies(train, columns=['intTarget'])
        li = [i for i in train.columns if 'intTarget_' in i]
        for f1 in cate_columns:
            for f2 in li:
                col_name = f1 + "_" + f2 + '_mean'
                order_label = train.groupby([f1])[f2].mean()
                for df in [train, test]:
                    df[col_name] = df[f1].map(order_label)
        train.drop(li, axis=1, inplace=True)
        train.drop(['target'], axis=1, inplace=True)
        test = test[train.columns]
        return pd.concat([train, test], axis=0)

    print('basefeature...')
    train['level'] = 'train'
    test['level'] = 'test'
    test['收率'] = 0
    all_data = pd.concat([train, test], axis=0).reset_index(drop=True)

    # 将样本id转化为数字id:
    all_data['id'] = all_data['样本id'].apply(lambda x: int(re.split('_', x)[1]))
    all_data = all_data.drop('样本id', axis=1)
    all_data = all_data.drop(['A7', 'A8'], axis=1)
    all_data['B9_B10_B11'] = np.where(all_data['B10'].isna(), 0, np.where(all_data['B11'].isna(), 1, 2))
    all_data = all_data.drop(['B10', 'B11'], axis=1)

    # 添加温度变化特征
    all_data['hydrolyze_temperature'] = np.abs(all_data['A12'] - all_data['A10']) + np.abs(
        all_data['A15'] - all_data['A12']) + np.abs(all_data['A17'] - all_data['A15'])
    all_data['decolor_temperature'] = np.abs(all_data['A27'] - all_data['A25'])
    all_data['crystal_temperature'] = np.abs(all_data['B8'] - all_data['B6'])

    all_data['hydrolyze_time'] = all_data[['A5', 'A16']].apply(lambda x: time_used(x[0], x[1]), axis=1)
    all_data['crystal_time'] = all_data[['B5', 'B7']].apply(lambda x: time_used(x[0], x[1]), axis=1)
    # 将时间跨度转化成时差
    for label in ['A20', 'A28', 'B4', 'B9']:
        all_data[label] = all_data[label].apply(time_length)

    # 时刻数据转化成早中晚三班的：
    for label in ['A5', 'A9', 'A11', 'A14', 'A16', 'A24', 'A26', 'B5', 'B7']:
        all_data[label] = all_data[label].apply(time_class)

    # 密度聚类
    cluster_data = data_fill(all_data[['A19', 'B1', 'B12', 'B14']])
    all_data['DBSCAN_class'] = my_DBSCAN(cluster_data, 0.6, 10, False)

    # 填补缺失值
    all_data = data_fill(all_data)

    # 加入分箱特征
    final_data = get_newfeature(all_data)
    final_data = data_fill(final_data)

    train = final_data.loc[final_data['level'] == 'train'].drop(['level'], axis=1).reset_index(drop=True)
    test = final_data.loc[final_data['level'] == 'test'].drop(['收率', 'level'], axis=1).reset_index(drop=True)

    return train, test


# 模型交叉验证
def modeling_cross_validation(params, X, y, nr_folds=5):
    oof_preds = np.zeros(X.shape[0])
    # Split data with kfold
    folds = KFold(n_splits=nr_folds, shuffle=False, random_state=4096)

    for fold_, (trn_idx, val_idx) in enumerate(folds.split(X, y)):
        print("fold n°{}".format(fold_ + 1))
        trn_data = lgb.Dataset(X[trn_idx], y[trn_idx])
        val_data = lgb.Dataset(X[val_idx], y[val_idx])

        num_round = 20000
        clf = lgb.train(params, trn_data, num_round, valid_sets=[trn_data, val_data], verbose_eval=1000,
                        early_stopping_rounds=100)
        oof_preds[val_idx] = clf.predict(X[val_idx], num_iteration=clf.best_iteration)

    score = mean_squared_error(oof_preds, target)

    return score / 2


# 特征筛选
def featureSelect(init_cols):
    params = {'num_leaves': 10,
              'min_data_in_leaf': 15,
              'objective': 'regression',
              'max_depth': -1,
              'learning_rate': 0.03,
              "min_child_samples": 15,
              "boosting": "gbdt",
              "feature_fraction": 1,
              "bagging_freq": 1,
              "bagging_fraction": 1,
              "bagging_seed": 11,
              "metric": 'mse',
              "lambda_l1": 0,
              "verbosity": -1}
    best_cols = init_cols.copy()
    best_score = modeling_cross_validation(params, train[init_cols].values, target.values, nr_folds=5)
    print("初始CV score: {:<8.8f}".format(best_score))
    for f in init_cols:
        best_cols.remove(f)
        score = modeling_cross_validation(params, train[best_cols].values, target.values, nr_folds=5)
        diff = best_score - score
        print('-' * 10)
        if diff > 0.0000002:
            print("当前移除特征: {}, CV score: {:<8.8f}, 最佳cv score: {:<8.8f}, 有效果,删除！！".format(f, score, best_score))
            best_score = score
        else:
            print("当前移除特征: {}, CV score: {:<8.8f}, 最佳cv score: {:<8.8f}, 没效果,保留！！".format(f, score, best_score))
            best_cols.append(f)
    print('-' * 10)
    print("优化后CV score: {:<8.8f}".format(best_score))

    return best_cols


# 数据读取=================================================================================================
start_time = datetime.datetime.now()

print('loading...')
print('loading...')
train = pd.read_csv('./input/jinnan/jinnan_round1_train_20181227.csv', encoding='gb18030')
test = pd.read_csv('./input/jinnan/jinnan_round1_testA_20181227.csv', encoding='gb18030')
# 测试集输入错误=================================================================================================

test.loc[test['B14'] == 785, 'B14'] = 385

# 特征工程==============================================================================================

train = train_clean(train)  # 对train中的一部分特征进行修正

# 获取B14相关的比值特征
train, test = feature_rate(train, test)

# 对一些特征做映射和删除
train, test = feature_drop(train, test)

# 获取基础特征
train, test = base_feature(train, test)
# 对那些变化大的特征进行分箱处理:
discretization_label = ['hydrolyze_temperature', 'decolor_temperature', 'crystal_temperature', 'hydrolyze_time',
                        'crystal_time', 'A20', 'A28', 'B4', 'B9']
for label in discretization_label:
    train[label] = discretization(train[label])
    test[label] = discretization(test[label])

# 异常值筛选
train = train[train['收率'] > 0.86]

'''
#  一、根据id进行分段训练预测=======================================================================
# best_features = featureSelect(train.columns.tolist())  # 模型特征筛选
# 根据id 排序，然后训练时不打乱顺序
train1 = train.copy().sort_values(by="id", ascending=True)
train1.index = train1['id']
test1 = test.copy().sort_values(by="id", ascending=True)
test1.index = test1['id']
# 根据样本id进行交叉验证==========================================================================
num = 5  # 假设实验来自5个实验室，不一定每个实验室做个数一样
num_data = 2000 // num
num_val = 150 // num
# 将训练集平均分成 num 个区间，训练num个模型：当测试集id落在对应的区间内，就使用对应的分类器
oof_lgb = pd.DataFrame(np.zeros(len(train1)))
oof_lgb.index = train1.index
for k in range(num):
    print("fold n°{}".format(k + 1))
    idx = range(num_data * k, num_data * (k + 1))
    idx = [i for i in idx if i in train1.index]
    sub_data = train1.loc[idx]
    clf = RandomForestRegressor().fit(sub_data.drop('收率', axis=1).values, sub_data['收率'].values)
    oof_lgb.loc[idx] = clf.predict(sub_data.drop('收率', axis=1).values).reshape(-1, 1)  # 预测某个区间内所有样本
print("CV score: {:<8.8f}".format(round(mean_squared_error(oof_lgb, train_y) / 2, 9)))
'''

# 二、常规建模5折交叉验证==============================================================================

train_x = train.drop(['收率'], axis=1)
train_y = train['收率']
test_x = test

folds = KFold(n_splits=5, shuffle=True, random_state=2018)
oof_lgb = np.zeros(len(train_x))
predictions_lgb = np.zeros(len(test_x))
feature_importance_df = pd.DataFrame()

# lgb 模型
print('train size:', train_x.shape)

param = {'num_leaves': 10,
         'min_data_in_leaf': 15,
         'objective': 'regression',
         'max_depth': -1,
         'learning_rate': 0.03,
         "min_child_samples": 15,
         "boosting": "gbdt",
         "feature_fraction": 0.5,
         "bagging_freq": 1,
         "bagging_fraction": 1,
         "bagging_seed": 11,
         "metric": 'mse',
         "lambda_l1": 0,
         "verbosity": -1}
for fold_, (trn_idx, val_idx) in enumerate(folds.split(train_x, train_y)):
    print("fold n°{}".format(fold_ + 1))
    trn_data = lgb.Dataset(train_x.values[trn_idx], train_y.values[trn_idx])
    val_data = lgb.Dataset(train_x.values[val_idx], train_y.values[val_idx])

    num_round = 10000
    clf = lgb.train(param, trn_data, num_round, valid_sets=[trn_data, val_data], verbose_eval=200,
                    early_stopping_rounds=100)
    oof_lgb[val_idx] = clf.predict(train_x.values[val_idx], num_iteration=clf.best_iteration)

    fold_importance_df = pd.DataFrame()
    fold_importance_df["Feature"] = train_x.columns
    fold_importance_df["importance"] = clf.feature_importance()
    fold_importance_df["fold"] = fold_ + 1
    feature_importance_df = pd.concat([feature_importance_df, fold_importance_df], axis=0)
    predictions_lgb += clf.predict(test_x.values, num_iteration=clf.best_iteration) / folds.n_splits
    # 预测结果也是每折的预测结果相加
score = round(mean_squared_error(oof_lgb, train_y) / 2, 9)
print("CV score: {:<8.8f}".format(score))

feature_importance_df = feature_importance_df.pivot_table(index='Feature', columns='fold', values='importance')
feature_importance_df['mean'] = feature_importance_df.mean(axis=1)
feature_importance_df = feature_importance_df.sort_values('mean', ascending=False)

# xgb
xgb_params = {'eta': 0.005, 'max_depth': 10, 'subsample': 0.9, 'colsample_bytree': 0.5,
              'objective': 'reg:linear', 'eval_metric': 'rmse', 'silent': True, 'nthread': 4}

folds = KFold(n_splits=5, shuffle=True, random_state=2018)
oof_xgb = np.zeros(len(train_x))
predictions_xgb = np.zeros(len(test_x))

for fold_, (trn_idx, val_idx) in enumerate(folds.split(train_x, train_y)):
    print("fold n°{}".format(fold_ + 1))
    trn_data = xgb.DMatrix(train_x.values[trn_idx], train_y.values[trn_idx])
    val_data = xgb.DMatrix(train_x.values[val_idx], train_y.values[val_idx])

    watchlist = [(trn_data, 'train'), (val_data, 'valid_data')]
    clf = xgb.train(dtrain=trn_data, num_boost_round=20000, evals=watchlist, early_stopping_rounds=200,
                    verbose_eval=100, params=xgb_params)
    oof_xgb[val_idx] = clf.predict(xgb.DMatrix(train_x.values[val_idx]), ntree_limit=clf.best_ntree_limit)
    predictions_xgb += clf.predict(xgb.DMatrix(test_x.values), ntree_limit=clf.best_ntree_limit) / folds.n_splits
target = train_y
print("CV score: {:<8.8f}".format(mean_squared_error(oof_xgb, target) / 2))

# 将lgb和xgb的结果进行stacking
train_stack = np.vstack([oof_lgb, oof_xgb]).transpose()
test_stack = np.vstack([predictions_lgb, predictions_xgb]).transpose()

folds_stack = RepeatedKFold(n_splits=5, n_repeats=2, random_state=4590)
oof_stack = np.zeros(train_stack.shape[0])
predictions = np.zeros(test_stack.shape[0])

for fold_, (trn_idx, val_idx) in enumerate(folds_stack.split(train_stack, target)):
    print("fold {}".format(fold_))
    trn_data, trn_y = train_stack[trn_idx], target.iloc[trn_idx].values
    val_data, val_y = train_stack[val_idx], target.iloc[val_idx].values

    clf_3 = BayesianRidge()
    clf_3.fit(trn_data, trn_y)

    oof_stack[val_idx] = clf_3.predict(val_data)
    predictions += clf_3.predict(test_stack) / 10

score = round(mean_squared_error(target.values, oof_stack) / 2, 9)
print("CV score: {:<8.8f}".format(score))
score = '{:.8f}'.format(score)

# 提交结果
sub_df = pd.read_csv('./input/jinnan/jinnan_round1_submit_20181227.csv', header=None)
sub_df[1] = predictions
# sub_df[1] = np.round(predictions, 3)  # 保留三位小数
sub_df.to_csv('./output/jinnan/sub_CV' + score + '.csv', index=False, header=None)
end_time = datetime.datetime.now()
print('时间：', end_time - start_time)

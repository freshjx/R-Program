#多元线性回归
#1查看变量间的相关性
#1.1相关性矩阵
cor(states[,-1])
#1.2Scatter plot Matrix
library(car)
scatterplotMatrix(states[,-1],
                  spread = F,smoother.args = list(lty=2),main='Scatter plot Matrix')
#2拟回归
#2.1 无交互项的lm for multi-variables 精度较低
sts_lm <- lm(Murder~Population+Illiteracy+Income+Frost+HS.Grad,data=states)
summary(sts_lm)
#2.2有交互项的x1:x2 精度提高了
car_lm <- lm(mpg~hp+wt+hp:wt,data=mtcars)
summary(car_lm)
#another way for coding
lm(mpg~(hp+wt)^2,data=mtcars)
#plot by package(effects)
library('effects')
plot(effect('hp:wt',car_lm,,list(wt=c(2.2,3.2,4.2))),multiline = TRUE)
#states
st_lm <- lm(Murder~(Population+Illiteracy+Income+Frost+HS.Grad)^2,data=states)
summary(st_lm)
#plot by package(effects) 图形展示交互想项的结果
library('effects')
plot(effect('Population:Illiteracy',st_lm,,list(Illiteracy=c(1.1,1.5,2.1))),multiline = TRUE)

#3 回归诊断：检验模型是否合适
#查看多元回归中各变量的置信区间
confint(st_lm)#初阶
#3.1标准方法 图评价 4副:
par(mfrow=c(2,2))
plot(w_l)
 #1.Residual vs Fitted 若显示的是曲线，则表明需要为回归模型添加一个二次项,”线性“
 #2.Normal Q-Q 若点都落在45度角度直线上，则表明“正态性”的假设成立
 #3.Scale-Location 水平线周围的点随机分布，则表明样本符合“同方差性”，因变量方差不随自变量变化而变化。
 #4.Residual vs leverage 诊断样本中观测点(离群点 杠杆值 强影响点)
#3.2 改进方法 car包
#跟3.1一样，我们考量OLS的假设是否成立
 #3.2.1.正态性 qqPlot()
library(car)
qqPlot(st_lm,lables = row.names(states),id.method = 'identify',simulate=TRUE)
 #同时可以找出回归模型中的异常观测点："Florida"    "Nevada"     "California"
 #再对他们进行分析
 #观察数值
states[c( "Florida","Nevada","California"),]
 #查看模型计算结果
fitted(st_lm)[c( "Florida","Nevada","California")]
 #残差
residuals(st_lm)[c( "Florida","Nevada","California")]
 #模型诊断
rstudent(st_lm)[c( "Florida","Nevada","California")]
 #绘制残差图 自定义的
residplot(st_lm)
 #3.2.2 独立性
 #判断因变量或者残差是否相互独立：依据收集数据方式的先验知识(时间序列的自相关性) Durbin-Wstson检验
 #误差的序列相关性
durbinWatsonTest(st_lm)
 #95%假设他们独立
 #适用于时间独立的数据，不适于非聚集类型的数据
 #3.2.3线性 成分残差图(偏残差图) 
crPlots(sts_lm)
 #若图形存在非线性，则说明回归模型的建立不充分
 #不支持含有交互项(x:z)的回归模型
 #3.2.4 同方差性
ncvTest(sts_lm)
 #H0:假设误差方差不变
spreadLevelPlot(sts_lm)
 #Suggested power transformation : n 表示建议对模型中变量进行n次幂的转换后，
 #可以平稳非恒定的误差方差，就是削弱模型的异方差性
#3.3 线性模型假设的综合验证
library(gvlma)
gvmodel <- gvlma(sts_lm)
summary(gvmodel)
#3.4 多重共线性 可用于解释多元回归的结果
#解释变量之间存在较强的关联关系致使模型估计失准
library(car)
vif(sts_lm)
#sqrt(vif) > 2时，表明存在多重共线性的情况
#4.异常观测值
#4.1离群点 找|残差|最大的点的显著性来判断是否有离群点，预测效果不佳的
library(car)
outlierTest(sts_lm)
#4.2高杠杆点
#与其他自变量有关的离群点，一个异常的自变量值的组合，与因变量值没有关系
#hat statics帽子统计量和帽子均值p/n对比得出,p:样本参数数目(含截距)，n:样本量
#观测点点帽子值大于帽子均值的2或3倍，则表明其为高杠杆点
hat.plot(sts_lm)
#高杠杆点是离群点时就是强影响点！
#4.3强影响点
#对模型参数的估计产生影响过大的观测点
#cook距离 D统计量 >4/(n-k-1) ,n为样本量，k为自变量数目
cutoff <- 4/(nrow(states)-length(sts_lm$coefficients)-2)#sts_lm$coefficients中包含一个截距
plot(sts_lm,which = 4,cook.levels = cutoff)#which = 4 表示Cook's distance图
abline(h=cutoff,lty = 2,col = 'red')#只找点，不解释其影响
#变量添加图(added variable plot)
#对于每个自变量X_k,都绘制他在k-1个自变量上回归到残差之相对于因变量在其他k-1个自变量上回归到残差之的关系图
library(car)
avPlots(sts_lm,ask = F,id.method = 'identify')
influencePlot(sts_lm,id.method = 'identify',main = 'Influence Plot',
              sub = "Circle size is proportional to Cook's distance" )
#纵坐标超过+2或-2为离群点
#横坐标超过0.2或0.3为高杠杆
#圆圈越大影响越大

#5.改进措施
#5.1删除观测点
 #删除离群点，强影响点--> 重新拟合 -->删除离群点，强影响点-->重新拟合...
 #但是这些点也有其研究价值，可以发现一些问题！
#5.2变量变换
 #当模型不符合正态性，线性或者同方差性假设时， 变换一个或多个变量
 #5.2.1违反正态性假设时：变换响应变量（因变量）
summary(powerTransform(states$Murder)) #lamda即变量的变化程度（可提高正态性符合程度）
 #5.2.2违反线性假设时：变换预测变量（自变量）
boxTidwell(Murder~Population+Illiteracy,data=states)
 #5.2.3违反同方差性时：变换响应变量（因变量）
spreadLevelPlot(sts_lm) #spt值就是lamda
#5.3增删变量
 #删除冗余变量
 #5.3.1常用方法：删除某个存在多重共线性的变量（sqrt(vif) > 2)
 #5.3.2岭回归-多元回归的变体，专门处理多重共线性问题
#5.4其他方法
 #5.4.1存在离群点或强影响点：稳健回归替代OLS回归
 #5.4.2违背正态性：非参数回归
 #5.4.3显著的非线性：非线性回归
 #5.4.4违背同方差性：时间序列模型或多层次回归模型
 #5.4.5广义线性回归
#6.选择“最佳”的回归模型
#预测精度（尽可能地拟合数据）
#模型简洁度（简单且能复制）
#6.1模型比较
 #6.1.1 anova函数 比较两个嵌套模型的拟合优度。
 #嵌套模型：一个模型的一些项(变量)完全包含在另一个模型中。
anova(sts_lm,sts_lm1)
 #结果Pr(>F)不显著时，即可删除变量
 #(例：sts_lm较sts_lm1多出的Income，Frost,HS.Grad三个变量)
 #6.1.2 AIC函数 Akaike Information Criterion 赤池信息准则
 #同时考虑了模型的统计拟合度和用来拟合度参数数目
 #AIC越小的模型，越优先选择(说明模型用较少的参数获得了足够的拟合度)
AIC(sts_lm,st_lm)
 #不需要嵌套模型
#6.2变量选择
#6.1逐步回归 stepwise method
 #模型一次添加或删除一个变量，直到达到某个判停标准为止。其中：
 #向前逐步回归(forward stepwise regression) 每次添加一个预测变量，直到模型不会继续优化为止。
 #向后逐步回归(backward stepwise regression) 每次删除一个预测变量
 #向前向后逐步回归，简称逐步回归(stepwise stepwise regression) 每次进入一个就重新评价一次，并将没有贡献的预测变量删除，反复，直到最优
library(MASS)
stepAIC(sts_lm,direction = 'backward')
 #依据是精确AIC准则(模型的AIC最低),逐步删除<none>上方的预测变量
#争议：逐步回归方法可以找到一个好的模型，但是，不一定是最佳模型。(因为不是每一个可能的模型都被评价)
#6.2全子集回归
#可以克服逐步回归的问题！
#所有可能的模型都会被检验，且分析师可以选择只展示n个不同子集(预测变量)大小的最佳模型，或者所有可能的结果
library(leaps)
leaps<-regsubsets(Murder~Population+Illiteracy+Income+Frost+HS.Grad,data = states,nbest=4)
plot(leaps,scale = 'adjr2')#取某几个变量组成的R^2最大的，作为最优模型的变量
#或者
library(car)
subsets(leaps,statistic = 'cp',main='Cp for All Subsets Regression')
abline(1,1,lty=2,col='red')#选择1：1"最优"线附近的组合。
 #思考：全子集回归一般要优于逐步回归，但是，预测变量很多时效率低。
 #一般来说，变量自动选择作为辅助方法使用，关键还是对主题的理解！
#7.深层次分析
#7.1 交叉验证 评价回归模型的泛化能力
#将一定比例的数据挑选出来作为(k-1)个训练样本，另外的数据作为1个保留样本；先在训练样本上获得回归方程，再在保留样本上做测试。
#这样会得到k个预测方程，记录k个保留样本的预测表现结果，然后取其平均值。
skrinkage(sts_lm)
 #R^2减少的越少表明预测越精准，即change越小越好
#7.2 相对重要性
#7.2.1相对重要性
#先对变量进行标准化，再进行回归
zstates <- as.data.frame(scale(states[,-1]))#用scale()标准化
zfit <- lm(Murder ~ Population + Illiteracy + Income + Frost + HS.Grad,data=zstates)
coef(zfit)#根据各变量系数值（标准差）的大小来判定他的相对重要性
summary(zfit)
#7.2.2相对权重
#对所有有可能自模型添加一个预测变量引起的R平方平均增加量的一个近似值
relweights(st_lm,col = 'blue')# 从结果中查看各变量在模型解释了x%的R^2.
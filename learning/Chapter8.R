#��Ԫ���Իع�
#1�鿴������������
#1.1����Ծ���
cor(states[,-1])
#1.2Scatter plot Matrix
library(car)
scatterplotMatrix(states[,-1],
                  spread = F,smoother.args = list(lty=2),main='Scatter plot Matrix')
#2��ع�
#2.1 �޽������lm for multi-variables ���Ƚϵ�
sts_lm <- lm(Murder~Population+Illiteracy+Income+Frost+HS.Grad,data=states)
summary(sts_lm)
#2.2�н������x1:x2 ���������
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
#plot by package(effects) ͼ��չʾ��������Ľ��
library('effects')
plot(effect('Population:Illiteracy',st_lm,,list(Illiteracy=c(1.1,1.5,2.1))),multiline = TRUE)

#3 �ع���ϣ�����ģ���Ƿ����
#�鿴��Ԫ�ع��и���������������
confint(st_lm)#����
#3.1��׼���� ͼ���� 4��:
par(mfrow=c(2,2))
plot(w_l)
 #1.Residual vs Fitted ����ʾ�������ߣ��������ҪΪ�ع�ģ������һ��������,�����ԡ�
 #2.Normal Q-Q ���㶼����45�ȽǶ�ֱ���ϣ����������̬�ԡ��ļ������
 #3.Scale-Location ˮƽ����Χ�ĵ�����ֲ���������������ϡ�ͬ�����ԡ��������������Ա����仯���仯��
 #4.Residual vs leverage ��������й۲��(��Ⱥ�� �ܸ�ֵ ǿӰ���)
#3.2 �Ľ����� car��
#��3.1һ�������ǿ���OLS�ļ����Ƿ����
 #3.2.1.��̬�� qqPlot()
library(car)
qqPlot(st_lm,lables = row.names(states),id.method = 'identify',simulate=TRUE)
 #ͬʱ�����ҳ��ع�ģ���е��쳣�۲�㣺"Florida"    "Nevada"     "California"
 #�ٶ����ǽ��з���
 #�۲���ֵ
states[c( "Florida","Nevada","California"),]
 #�鿴ģ�ͼ�����
fitted(st_lm)[c( "Florida","Nevada","California")]
 #�в�
residuals(st_lm)[c( "Florida","Nevada","California")]
 #ģ�����
rstudent(st_lm)[c( "Florida","Nevada","California")]
 #���Ʋв�ͼ �Զ����
residplot(st_lm)
 #3.2.2 ������
 #�ж���������߲в��Ƿ��໥�����������ռ����ݷ�ʽ������֪ʶ(ʱ�����е��������) Durbin-Wstson����
 #�������������
durbinWatsonTest(st_lm)
 #95%�������Ƕ���
 #������ʱ����������ݣ������ڷǾۼ����͵�����
 #3.2.3���� �ɷֲв�ͼ(ƫ�в�ͼ) 
crPlots(sts_lm)
 #��ͼ�δ��ڷ����ԣ���˵���ع�ģ�͵Ľ��������
 #��֧�ֺ��н�����(x:z)�Ļع�ģ��
 #3.2.4 ͬ������
ncvTest(sts_lm)
 #H0:���������
spreadLevelPlot(sts_lm)
 #Suggested power transformation : n ��ʾ�����ģ���б�������n���ݵ�ת����
 #����ƽ�ȷǺ㶨�������������ģ�͵��췽����
#3.3 ����ģ�ͼ�����ۺ���֤
library(gvlma)
gvmodel <- gvlma(sts_lm)
summary(gvmodel)
#3.4 ���ع����� �����ڽ��Ͷ�Ԫ�ع�Ľ��
#���ͱ���֮����ڽ�ǿ�Ĺ�����ϵ��ʹģ�͹���ʧ׼
library(car)
vif(sts_lm)
#sqrt(vif) > 2ʱ���������ڶ��ع����Ե����
#4.�쳣�۲�ֵ
#4.1��Ⱥ�� ��|�в�|���ĵ�����������ж��Ƿ�����Ⱥ�㣬Ԥ��Ч�����ѵ�
library(car)
outlierTest(sts_lm)
#4.2�߸ܸ˵�
#�������Ա����йص���Ⱥ�㣬һ���쳣���Ա���ֵ����ϣ��������ֵû�й�ϵ
#hat staticsñ��ͳ������ñ�Ӿ�ֵp/n�Աȵó�,p:����������Ŀ(���ؾ�)��n:������
#�۲���ñ��ֵ����ñ�Ӿ�ֵ��2��3�����������Ϊ�߸ܸ˵�
hat.plot(sts_lm)
#�߸ܸ˵�����Ⱥ��ʱ����ǿӰ��㣡
#4.3ǿӰ���
#��ģ�Ͳ����Ĺ��Ʋ���Ӱ�����Ĺ۲��
#cook���� Dͳ���� >4/(n-k-1) ,nΪ��������kΪ�Ա�����Ŀ
cutoff <- 4/(nrow(states)-length(sts_lm$coefficients)-2)#sts_lm$coefficients�а���һ���ؾ�
plot(sts_lm,which = 4,cook.levels = cutoff)#which = 4 ��ʾCook's distanceͼ
abline(h=cutoff,lty = 2,col = 'red')#ֻ�ҵ㣬��������Ӱ��
#��������ͼ(added variable plot)
#����ÿ���Ա���X_k,����������k-1���Ա����ϻع鵽�в�֮����������������k-1���Ա����ϻع鵽�в�֮�Ĺ�ϵͼ
library(car)
avPlots(sts_lm,ask = F,id.method = 'identify')
influencePlot(sts_lm,id.method = 'identify',main = 'Influence Plot',
              sub = "Circle size is proportional to Cook's distance" )
#�����곬��+2��-2Ϊ��Ⱥ��
#�����곬��0.2��0.3Ϊ�߸ܸ�
#ԲȦԽ��Ӱ��Խ��

#5.�Ľ���ʩ
#5.1ɾ���۲��
 #ɾ����Ⱥ�㣬ǿӰ���--> ������� -->ɾ����Ⱥ�㣬ǿӰ���-->�������...
 #������Щ��Ҳ�����о���ֵ�����Է���һЩ���⣡
#5.2�����任
 #��ģ�Ͳ�������̬�ԣ����Ի���ͬ�����Լ���ʱ�� �任һ����������
 #5.2.1Υ����̬�Լ���ʱ���任��Ӧ�������������
summary(powerTransform(states$Murder)) #lamda�������ı仯�̶ȣ��������̬�Է��ϳ̶ȣ�
 #5.2.2Υ�����Լ���ʱ���任Ԥ��������Ա�����
boxTidwell(Murder~Population+Illiteracy,data=states)
 #5.2.3Υ��ͬ������ʱ���任��Ӧ�������������
spreadLevelPlot(sts_lm) #sptֵ����lamda
#5.3��ɾ����
 #ɾ���������
 #5.3.1���÷�����ɾ��ĳ�����ڶ��ع����Եı�����sqrt(vif) > 2)
 #5.3.2��ع�-��Ԫ�ع�ı��壬ר�Ŵ������ع���������
#5.4��������
 #5.4.1������Ⱥ���ǿӰ��㣺�Ƚ��ع����OLS�ع�
 #5.4.2Υ����̬�ԣ��ǲ����ع�
 #5.4.3�����ķ����ԣ������Իع�
 #5.4.4Υ��ͬ�����ԣ�ʱ������ģ�ͻ���λع�ģ��
 #5.4.5�������Իع�
#6.ѡ����ѡ��Ļع�ģ��
#Ԥ�⾫�ȣ������ܵ�������ݣ�
#ģ�ͼ��ȣ������ܸ��ƣ�
#6.1ģ�ͱȽ�
 #6.1.1 anova���� �Ƚ�����Ƕ��ģ�͵�����Ŷȡ�
 #Ƕ��ģ�ͣ�һ��ģ�͵�һЩ��(����)��ȫ��������һ��ģ���С�
anova(sts_lm,sts_lm1)
 #���Pr(>F)������ʱ������ɾ������
 #(����sts_lm��sts_lm1�����Income��Frost,HS.Grad��������)
 #6.1.2 AIC���� Akaike Information Criterion �����Ϣ׼��
 #ͬʱ������ģ�͵�ͳ����϶Ⱥ�������϶Ȳ�����Ŀ
 #AICԽС��ģ�ͣ�Խ����ѡ��(˵��ģ���ý��ٵĲ���������㹻����϶�)
AIC(sts_lm,st_lm)
 #����ҪǶ��ģ��
#6.2����ѡ��
#6.1�𲽻ع� stepwise method
 #ģ��һ�����ӻ�ɾ��һ��������ֱ���ﵽĳ����ͣ��׼Ϊֹ�����У�
 #��ǰ�𲽻ع�(forward stepwise regression) ÿ������һ��Ԥ�������ֱ��ģ�Ͳ�������Ż�Ϊֹ��
 #����𲽻ع�(backward stepwise regression) ÿ��ɾ��һ��Ԥ�����
 #��ǰ����𲽻ع飬����𲽻ع�(stepwise stepwise regression) ÿ�ν���һ������������һ�Σ�����û�й��׵�Ԥ�����ɾ����������ֱ������
library(MASS)
stepAIC(sts_lm,direction = 'backward')
 #�����Ǿ�ȷAIC׼��(ģ�͵�AIC���),��ɾ��<none>�Ϸ���Ԥ�����
#���飺�𲽻ع鷽�������ҵ�һ���õ�ģ�ͣ����ǣ���һ�������ģ�͡�(��Ϊ����ÿһ�����ܵ�ģ�Ͷ�������)
#6.2ȫ�Ӽ��ع�
#���Կ˷��𲽻ع�����⣡
#���п��ܵ�ģ�Ͷ��ᱻ���飬�ҷ���ʦ����ѡ��ֻչʾn����ͬ�Ӽ�(Ԥ�����)��С�����ģ�ͣ��������п��ܵĽ��
library(leaps)
leaps<-regsubsets(Murder~Population+Illiteracy+Income+Frost+HS.Grad,data = states,nbest=4)
plot(leaps,scale = 'adjr2')#ȡĳ����������ɵ�R^2���ģ���Ϊ����ģ�͵ı���
#����
library(car)
subsets(leaps,statistic = 'cp',main='Cp for All Subsets Regression')
abline(1,1,lty=2,col='red')#ѡ��1��1"����"�߸�������ϡ�
 #˼����ȫ�Ӽ��ع�һ��Ҫ�����𲽻ع飬���ǣ�Ԥ������ܶ�ʱЧ�ʵ͡�
 #һ����˵�������Զ�ѡ����Ϊ��������ʹ�ã��ؼ����Ƕ���������⣡
#7.���η���
#7.1 ������֤ ���ۻع�ģ�͵ķ�������
#��һ��������������ѡ������Ϊ(k-1)��ѵ�������������������Ϊ1����������������ѵ�������ϻ�ûع鷽�̣����ڱ��������������ԡ�
#������õ�k��Ԥ�ⷽ�̣���¼k������������Ԥ����ֽ����Ȼ��ȡ��ƽ��ֵ��
skrinkage(sts_lm)
 #R^2���ٵ�Խ�ٱ���Ԥ��Խ��׼����changeԽСԽ��
#7.2 �����Ҫ��
#7.2.1�����Ҫ��
#�ȶԱ������б�׼�����ٽ��лع�
zstates <- as.data.frame(scale(states[,-1]))#��scale()��׼��
zfit <- lm(Murder ~ Population + Illiteracy + Income + Frost + HS.Grad,data=zstates)
coef(zfit)#���ݸ�����ϵ��ֵ����׼��Ĵ�С���ж����������Ҫ��
summary(zfit)
#7.2.2���Ȩ��
#�������п�����ģ������һ��Ԥ����������Rƽ��ƽ����������һ������ֵ
relweights(st_lm,col = 'blue')# �ӽ���в鿴��������ģ�ͽ�����x%��R^2.
#======================================Set working Directory========================================
setwd("/Users/numerp/Documents/PGP-BABI/Module 9 Financial Risk Analytics/Project 8 Financial Risk Analytics")
getwd()
#======================================Calling Dataset==============================================
library(readxl)
my_train=read_excel("raw-data.xlsx",sheet = 1)
my_test=read_excel("validation_data.xlsx",sheet = 1)
#======================================Continuous to Discrete Variable==============================
dim(my_train)
colnames(my_train)=make.names(colnames(my_train))
names(my_train)
str(my_train)
my_train$Creditors.turnover=as.numeric(my_train$Creditors.turnover)
my_train$Debtors.turnover=as.numeric(my_train$Debtors.turnover)
my_train$Finished.goods.turnover=as.numeric(my_train$Finished.goods.turnover)
my_train$WIP.turnover=as.numeric(my_train$WIP.turnover)
my_train$Raw.material.turnover=as.numeric(my_train$Raw.material.turnover)
my_train$Shares.outstanding=as.numeric(my_train$Shares.outstanding)
my_train$Equity.face.value=as.numeric(my_train$Equity.face.value)
my_train$PE.on.BSE=as.numeric(my_train$PE.on.BSE)
str(my_train)
summary(my_train)
dim(my_test)
colnames(my_test)=make.names(colnames(my_test))
names(my_test)
str(my_test)
my_test$Creditors.turnover=as.numeric(my_test$Creditors.turnover)
my_test$Debtors.turnover=as.numeric(my_test$Debtors.turnover)
my_test$Finished.goods.turnover=as.numeric(my_test$Finished.goods.turnover)
my_test$WIP.turnover=as.numeric(my_test$WIP.turnover)
my_test$Raw.material.turnover=as.numeric(my_test$Raw.material.turnover)
my_test$Shares.outstanding=as.numeric(my_test$Shares.outstanding)
my_test$Equity.face.value=as.numeric(my_test$Equity.face.value)
my_test$PE.on.BSE=as.numeric(my_test$PE.on.BSE)
str(my_test)
summary(my_test)
#======================================Missing Value Treatments=====================================
colSums(is.na(my_train))
my_train=subset(my_train,select = -c(22))
class(my_train)
my_train=as.data.frame(my_train)
for(i in 1:ncol(my_train)){
  my_train[,i]=as.numeric(unlist(my_train[,i]))
  my_train[is.na(my_train[,i]),i]=median(my_train[,i],na.rm = TRUE)
}
any(is.na(my_train))
colSums(is.na(my_test))
my_test=subset(my_test,select = -c(22))
class(my_test)
my_test=as.data.frame(my_test)
for(i in 1:ncol(my_test)){
  my_test[,i]=as.numeric(unlist(my_test[,i]))
  my_test[is.na(my_test[,i]),i]=median(my_test[,i],na.rm = TRUE)
}
any(is.na(my_test))
#======================================Outlier Treatments===========================================
summary(my_train)
outlier=boxplot(my_train[,-c(1,2)],plot = FALSE)$out
print(outlier)
library(scales)
for(i in 3:ncol(my_train)){
q=quantile(my_train[,i],c(0.1,0.99))
my_train[,i]=squish(my_train[,i],q)
}
summary(my_train)
summary(my_test)
outlier1=boxplot(my_test[,-c(1,2)],plot = FALSE)$out
print(outlier1)
for(i in 3:ncol(my_test)){
  q=quantile(my_test[,i],c(0.1,0.99))
  my_test[,i]=squish(my_test[,i],q)
}
summary(my_test)
#======================================Default varaible Creation====================================
my_train$Default...1=ifelse(my_train$Networth.Next.Year>0,0,1)
summary(my_train$Default...1)
train_data=subset(my_train,select = -c(1,2))
test_data=subset(my_test,select = -c(1))
train_data=train_data[,c(50,1:49)]
colnames(train_data)
colnames(test_data)
train_data$Default...1=as.factor(train_data$Default...1)
test_data$Default...1=as.factor(test_data$Default...1)
#======================================EDA Visualization============================================
library(ggplot2)
library(DataExplorer)
attach(train_data)
plot_bar(data = train_data,ggtheme = theme_lares(),
         title = "Default Status in Train Data")
plot_histogram(data = train_data,ggtheme = theme_lares(),nrow = 5,ncol = 5,
               title = "Histogram for Train Dataset")
plot_boxplot(data = train_data,by = "Default...1",ggtheme = theme_lares(),nrow = 5,ncol = 5,
             title = "Boxplot for Train Dataset")
plot_density(data = train_data,ggtheme = theme_lares(),nrow = 5,ncol = 5,
             title = "Density Plots for Train Dataset")
plot_scatterplot(data = train_data,by = "Default...1",ggtheme = theme_lares(),nrow = 5,ncol = 5,
                 title = "Scatterplot for Train Dataset")
plot_qq(data = train_data,ggtheme = theme_lares(),nrow = 5,ncol = 5,
        title = "Quantile Chart for Train Dataset")
plot_bar(data = test_data,ggtheme = theme_lares(),
         title = "Default Status in Test Data")
plot_histogram(data = test_data,ggtheme = theme_lares(),nrow = 5,ncol = 5,
               title = "Histogram for Test Dataset")
plot_boxplot(data = test_data,by = "Default...1",ggtheme = theme_lares(),nrow = 5,ncol = 5,
             title = "Boxplot for Test Dataset")
plot_density(data = test_data,ggtheme = theme_lares(),nrow = 5,ncol = 5,
             title = "Density Plots for Test Dataset")
plot_scatterplot(data = test_data,by = "Default...1",ggtheme = theme_lares(),nrow = 5,ncol = 5,
                 title = "Scatterplot for Test Dataset")
plot_qq(data = test_data,ggtheme = theme_lares(),nrow = 5,ncol = 5,
        title = "Quantile Chart for Test Dataset")
#======================================Correlation Check============================================
summary(as.factor(Default...1))
243/(3298+243) #0.06862468
correlation=cor(train_data[,-1])
correlation
library(ggcorrplot)
ggcorrplot::ggcorrplot(correlation,method = "square",ggtheme = theme_classic(),
                       title = "Correlation Chart")
library(devtools)
#devtools::install_github("laresbernardo/lares")
library(lares)
corr_cross(correlation)
#======================================Multicollinearity Check======================================
attach(train_data)
model1=glm(Default...1~. -Default...1,data = train_data,family = binomial(link = logit))
summary(model1)
#Removing Insignificant variables
model2=glm(Default...1~Total.assets+PBDITA+PAT.as...of.net.worth+Other.income+Reserves.and.funds
           +Current.liabilities...provisions+Cumulative.retained.profits+Capital.employed+TOL.TNW
           +Total.term.liabilities...tangible.net.worth+Contingent.liabilities+Net.fixed.assets
           +Debt.to.equity.ratio..times.+Cash.to.average.cost.of.sales.per.day+Creditors.turnover
           +PE.on.BSE,data = train_data,family = binomial)
summary(model2)
#Removing Insignificant variables
model3=glm(Default...1~Total.assets+PBDITA+PAT.as...of.net.worth+Reserves.and.funds
           +Current.liabilities...provisions+Cumulative.retained.profits+Capital.employed+TOL.TNW
           +Total.term.liabilities...tangible.net.worth+Net.fixed.assets
           +Debt.to.equity.ratio..times.+Cash.to.average.cost.of.sales.per.day+Creditors.turnover
           +PE.on.BSE,data = train_data,family = binomial)
summary(model3)
#Removing Insignificant variables
model4=glm(Default...1~Total.assets+PAT.as...of.net.worth+Reserves.and.funds
           +Current.liabilities...provisions+Cumulative.retained.profits+Capital.employed+TOL.TNW
           +Total.term.liabilities...tangible.net.worth+Net.fixed.assets
           +Debt.to.equity.ratio..times.+Cash.to.average.cost.of.sales.per.day+Creditors.turnover
           +PE.on.BSE,data = train_data,family = binomial)
summary(model4)
library(car)
car::vif(model4)
#Removing Higher Variance Inflation Rate variables -Total Assets -Capital Employed
model5=glm(Default...1~PAT.as...of.net.worth+Reserves.and.funds
           +Cumulative.retained.profits+TOL.TNW
           +Total.term.liabilities...tangible.net.worth+Net.fixed.assets
           +Debt.to.equity.ratio..times.
           +Cash.to.average.cost.of.sales.per.day+Creditors.turnover
           +PE.on.BSE,data = train_data,family = binomial)
summary(model5)
#Removing Insignificant Variables
model6=glm(Default...1~PAT.as...of.net.worth
           +Cumulative.retained.profits+TOL.TNW
           +Total.term.liabilities...tangible.net.worth+Net.fixed.assets
           +Debt.to.equity.ratio..times.
           +Cash.to.average.cost.of.sales.per.day+Creditors.turnover
           +PE.on.BSE,data = train_data,family = binomial)
summary(model6)
car::vif(model6)
#Removing Higher Variation Inflation Rate Variables -Total Term Liabilities Tangible Net Worth -Debt Equity Ratio Times
model7=glm(Default...1~PAT.as...of.net.worth+Cumulative.retained.profits+TOL.TNW
           +Net.fixed.assets+Cash.to.average.cost.of.sales.per.day+Creditors.turnover
           +PE.on.BSE,data = train_data,family = binomial)
summary(model7)
car::vif(model7)
#======================================New Variables Creation=======================================
dim(train_data)
#Profitability Ratio - Gross Margin Ratio
train_data$Gross.Margin.Ratio=train_data$Total.income/train_data$Sales
#Profitability Ratio - Return on Assets Ratio
train_data$Return.on.assets.ratio=train_data$Total.income/train_data$Total.assets
#Profitability - Return on Equity Ratio
train_data$Return.on.equity.ratio=train_data$Total.income/train_data$Shareholders.funds
#Leverage Financial Ratio - Debt to Equity Ratio
train_data$Debt.to.equity.ratio=train_data$Total.liabilities/train_data$Shareholders.funds
#Liquidity Ratio - Current Ratio
train_data$Current.ratio=train_data$Current.assets/train_data$Current.liabilities...provisions
#Liquidity Ratio - Cash Ratio
train_data$Cash.ratio=train_data$Cash.profit/train_data$Current.liabilities...provisions
#Efficiency Ratio - Asset Turnover Ratio
train_data$Asset.turnover.ratio=train_data$Sales/train_data$Total.assets
#Efficiency Ratio - Day Sales in Inventory Ratio
train_data$Day.sales.in.inventory.ratio=365/train_data$Finished.goods.turnover
dim(train_data)
summary(train_data)
dim(test_data)
#Profitability Ratio - Gross Margin Ratio
test_data$Gross.Margin.Ratio=test_data$Total.income/test_data$Sales
#Profitability Ratio - Return on Assets Ratio
test_data$Return.on.assets.ratio=test_data$Total.income/test_data$Total.assets
#Profitability - Retun on Equity Ratio
test_data$Return.on.equity.ratio=test_data$Total.income/test_data$Shareholders.funds
#Leverage Financial Ratio - Debt to Equity Ratio
test_data$Debt.to.equity.ratio=test_data$Total.liabilities/test_data$Shareholders.funds
#Liquidity Ratio - Current Ratio
test_data$Current.ratio=test_data$Current.assets/test_data$Current.liabilities...provisions
#Liquidity Ratio - Cash Ratio
test_data$Cash.ratio=test_data$Cash.profit/test_data$Current.liabilities...provisions
#Efficiency Ratio - Asset Turnover Ratio
test_data$Asset.turnover.ratio=test_data$Sales/test_data$Total.assets
#Efficiency Ratio - Day Sales in Inventory Ratio
test_data$Day.sales.in.inventory.ratio=365/test_data$Finished.goods.turnover
dim(test_data)
summary(test_data)
#======================================Logisitics Regression Model==================================
log.reg1=glm(Default...1~.,data = train_data,family = binomial)
summary(log.reg1)
#Removing Insignificant VariablesA
log.reg2=glm(Default...1~Total.assets+Net.worth+Total.income+Change.in.stock+Total.expenses+
               Profit.after.tax+PBDITA+PBT+Cash.profit+PBDITA.as...of.total.income+
               PBT.as...of.total.income+PAT.as...of.total.income+Cash.profit.as...of.total.income+
               PAT.as...of.net.worth+Sales+Income.from.financial.services+Other.income+
               Total.capital+Reserves.and.funds+Borrowings+Current.liabilities...provisions+
               Deferred.tax.liability+Shareholders.funds+Cumulative.retained.profits+
               Capital.employed+TOL.TNW+Total.term.liabilities...tangible.net.worth+
               Contingent.liabilities...Net.worth....+Contingent.liabilities+Net.fixed.assets+
               Investments+Current.assets+Net.working.capital+Quick.ratio..times.+
               Current.ratio..times.+Debt.to.equity.ratio..times.+
               Cash.to.current.liabilities..times.+Cash.to.average.cost.of.sales.per.day+
               Creditors.turnover+Debtors.turnover+Finished.goods.turnover+WIP.turnover+
               Raw.material.turnover+Shares.outstanding+Equity.face.value+EPS+Adjusted.EPS+
               PE.on.BSE+Gross.Margin.Ratio+Return.on.assets.ratio+Return.on.equity.ratio+
               Debt.to.equity.ratio+Current.ratio+Cash.ratio+Asset.turnover.ratio+
               Day.sales.in.inventory.ratio,data = train_data,
             family = binomial)
summary(log.reg2)
car::vif(log.reg2)
#Removing Higher Variance Inflation Factor Variables
log.reg3=glm(Default...1~Change.in.stock+
               PBDITA+Cash.profit+PBDITA.as...of.total.income+
               PBT.as...of.total.income+PAT.as...of.total.income+Cash.profit.as...of.total.income+
               PAT.as...of.net.worth+Income.from.financial.services+Other.income+
               Total.capital+Reserves.and.funds+Borrowings+Current.liabilities...provisions+
               Deferred.tax.liability+Cumulative.retained.profits+
               TOL.TNW+Total.term.liabilities...tangible.net.worth+
               Contingent.liabilities...Net.worth....+Contingent.liabilities+Net.fixed.assets+
               Investments+Current.assets+Net.working.capital+Quick.ratio..times.+
               Current.ratio..times.+Debt.to.equity.ratio..times.+
               Cash.to.current.liabilities..times.+Cash.to.average.cost.of.sales.per.day+
               Creditors.turnover+Debtors.turnover+Finished.goods.turnover+WIP.turnover+
               Raw.material.turnover+Shares.outstanding+Equity.face.value+
               PE.on.BSE+Gross.Margin.Ratio+Return.on.assets.ratio+Return.on.equity.ratio+
               Debt.to.equity.ratio+Current.ratio+Cash.ratio+Asset.turnover.ratio+
               Day.sales.in.inventory.ratio,data = train_data,
             family = binomial)
summary(log.reg3)
#Removing Insignificant Variables
log.reg4=glm(Default...1~PAT.as...of.net.worth+Current.liabilities...provisions+
               Cumulative.retained.profits+TOL.TNW+Total.term.liabilities...tangible.net.worth+
               Contingent.liabilities...Net.worth....+Contingent.liabilities+Net.fixed.assets+
               Current.assets+Debt.to.equity.ratio..times.+PE.on.BSE+Gross.Margin.Ratio+
               Debt.to.equity.ratio+Day.sales.in.inventory.ratio,data = train_data,
             family = binomial)
summary(log.reg4)
#Removing Insignificant Variables
log.reg5=glm(Default...1~PAT.as...of.net.worth+Current.liabilities...provisions+
               Cumulative.retained.profits+TOL.TNW+Total.term.liabilities...tangible.net.worth+
               Contingent.liabilities+Net.fixed.assets+
               Current.assets+Debt.to.equity.ratio..times.+PE.on.BSE+Gross.Margin.Ratio+
               Debt.to.equity.ratio+Day.sales.in.inventory.ratio,data = train_data,
             family = binomial)
summary(log.reg5)
car::vif(log.reg5)
#Removing Higher Variance Inflation Factor
log.reg6=glm(Default...1~PAT.as...of.net.worth+
               Cumulative.retained.profits+TOL.TNW+
               Contingent.liabilities+Net.fixed.assets+
               PE.on.BSE+Gross.Margin.Ratio+
               Debt.to.equity.ratio+Day.sales.in.inventory.ratio,data = train_data,
             family = binomial)
summary(log.reg6)
#Removing Insignificance Variables
log.reg7=glm(Default...1~PAT.as...of.net.worth+
               Cumulative.retained.profits+TOL.TNW+
               Net.fixed.assets+
               PE.on.BSE+
               Day.sales.in.inventory.ratio,data = train_data,
             family = binomial)
summary(log.reg7)
car::vif(log.reg7)
exp(coef(log.reg7))
exp(coef(log.reg7))/(1+exp(coef(log.reg7)))
library(rms)
library(pscl)
#McFadden 0to0.10 - Bad,0.10to0.15 - Average,0.15to0.3 - Moderate,0.3to0.5 - Good,>0.5 Excellent
pscl::pR2(log.reg7)["McFadden"]
logLik(log.reg7)
#======================================Prediction on Train Dataset==================================
log.reg=predict.glm(log.reg7,train_data,type = "response")
log.reg
tab.train=table(train_data$Default...1,log.reg>0.3)
tab.train
sum(diag(tab.train))/sum(tab.train)
pred1=ifelse(log.reg>0.68,0,1)
pred1
actual1=train_data$Default...1
library(caret)
cm_log_train=confusionMatrix(as.factor(pred1),actual1,positive = "1")
cm_log_train
#======================================Prediction on Test Dataset===================================
log.reg.pred=predict.glm(log.reg7,test_data,type = "response")
log.reg.pred
table.log=table(test_data$Default...1,log.reg.pred>0.3)
table.log
sum(diag(table.log))/sum(table.log)
pred=ifelse(log.reg.pred>0.68,0,1)
pred
actual=test_data$Default...1
actual
cm_log=confusionMatrix(as.factor(pred),actual,positive = "1")
cm_log
#======================================Decile Based on Train Data===================================
train_data$pred=predict(log.reg7,train_data,type = "response")
decile = function(x){
  deciles = vector(length=10)
  for (i in seq(0.1,1,.1)){
    deciles[i*10] = quantile(x, i, na.rm=T)
  }
  return (
    ifelse(x<deciles[1], 1,
           ifelse(x<deciles[2], 2,
                  ifelse(x<deciles[3], 3,
                         ifelse(x<deciles[4], 4,
                                ifelse(x<deciles[5], 5,
                                       ifelse(x<deciles[6], 6,
                                              ifelse(x<deciles[7], 7,
                                                     ifelse(x<deciles[8], 8,
                                                            ifelse(x<deciles[9], 9, 10
                                                            ))))))))))
}
train_data$deciles=decile(train_data$pred)
m=data.table::data.table(train_data)
rank.default = m[, list(cnt=length(Default...1),
                     cnt_resp=sum(Default...1==1),
                     cnt_non_resp=sum(Default...1==0)
                     ), by=deciles][order(-deciles)]
rank.default$rrate=round(rank.default$cnt_resp/rank.default$cnt,4)
rank.default$cum_resp=cumsum(rank.default$cnt_resp)
rank.default$cum_non_resp=cumsum(rank.default$cnt_non_resp)
rank.default$cum_rel_resp=round(rank.default$cum_resp/sum(rank.default$cnt_non_resp),4)
rank.default$cum_rel_non_resp=round(rank.default$cum_non_resp/sum(rank.default$cnt_non_resp),4)
rank.default$ks=abs(rank.default$cum_rel_resp - rank.default$cum_rel_non_resp)*100
rank.default$rrate=scales::percent(rank.default$rrate)
rank.default$cum_rel_resp=scales::percent(rank.default$cum_rel_resp)
rank.default$cum_rel_non_resp=scales::percent(rank.default$cum_rel_non_resp)
train_data_rank=rank.default
View(train_data_rank)
print(train_data_rank)
plot(train_data_rank$ks)
lines(train_data_rank$ks)
#======================================Decile Based on Test Data====================================
test_data$pred=predict(log.reg7,test_data,type = "response")
decile = function(x){
  deciles = vector(length=10)
  for (i in seq(0.1,1,.1)){
    deciles[i*10] = quantile(x, i, na.rm=T)
  }
  return (
    ifelse(x<deciles[1], 1,
           ifelse(x<deciles[2], 2,
                  ifelse(x<deciles[3], 3,
                         ifelse(x<deciles[4], 4,
                                ifelse(x<deciles[5], 5,
                                       ifelse(x<deciles[6], 6,
                                              ifelse(x<deciles[7], 7,
                                                     ifelse(x<deciles[8], 8,
                                                            ifelse(x<deciles[9], 9, 10
                                                            ))))))))))
}
test_data$deciles=decile(test_data$pred)
n=data.table::data.table(test_data)
rank.test.default = n[, list(cnt=length(Default...1),
                        cnt_resp=sum(Default...1==1),
                        cnt_non_resp=sum(Default...1==0)
), by=deciles][order(-deciles)]
rank.test.default$rrate=round(rank.test.default$cnt_resp/rank.test.default$cnt,4)
rank.test.default$cum_resp=cumsum(rank.test.default$cnt_resp)
rank.test.default$cum_non_resp=cumsum(rank.test.default$cnt_non_resp)
rank.test.default$cum_rel_resp=round(rank.test.default$cum_resp/
                                       sum(rank.test.default$cnt_non_resp),4)
rank.test.default$cum_rel_non_resp=round(rank.test.default$cum_non_resp/
                                           sum(rank.test.default$cnt_non_resp),4)
rank.test.default$ks=abs(rank.test.default$cum_rel_resp - rank.test.default$cum_rel_non_resp)*100
rank.test.default$rrate=percent(rank.test.default$rrate)
rank.test.default$cum_rel_resp=percent(rank.test.default$cum_rel_resp)
rank.test.default$cum_rel_non_resp=percent(rank.test.default$cum_rel_non_resp)
test_data_rank=rank.test.default
View(test_data_rank)
print(test_data_rank)
plot(test_data_rank$ks)
lines(test_data_rank$ks)
#======================================Model Performance Measures===================================
library(ROCR)
library(pROC)
rocpred=prediction(log.reg.pred,test_data$Default...1)
#ROC Curve
ROC=pROC::roc(Default...1,log.reg7$fitted.values)
ROC
plot.roc(Default...1,log.reg7$fitted.values)
#Area Under Curve
AUC=as.numeric(performance(rocpred, "auc")@y.values)
AUC
#KS Chart
perf = performance(rocpred, "tpr","fpr")
KS=max(attr(perf, 'y.values')[[1]]-attr(perf, 'x.values')[[1]])
KS
plot(perf, colorize =T, print.cutoffs.at= seq(0, 1, .1), text.adj = c(-.2, 1.7))
#Gini Coefficient
library(ineq)
gini=ineq(log.reg.pred, type="Gini")
gini
#***************************************************************************************************
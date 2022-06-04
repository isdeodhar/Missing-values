library(mice)
library(missForest)
library(bnstruct)

data <- iris
summary(iris)
iris.org <- subset(iris, select = -c(Species))
#mean imputation, multiple,reression, missforest
nrmse_mean={}
nrmse_mul={}
nrmse_reg={}
nrmse_for={}
NRMSE_KNN={}

for(i in 1:10)
{
  set.seed(i*3)
  iris.mis <- prodNA(iris.org, noNA = 0.2)
  imputed_Data=complete(mice(iris.mis,method = "mean",m=1,maxit = 1,printFlag = F))
  nrmse_mean[i]=nrmse(imputed_Data,iris.mis,iris.org)
  imputed_Dmul=complete(mice(iris.mis,method = "pmm",m=5,maxit = 1,printFlag = F))
  nrmse_mul[i]=nrmse(imputed_Dmul,iris.mis,iris.org)
  imputed_reg=complete(mice(iris.mis,method = "norm.predict",m=1,maxit = 1,printFlag = F))
  nrmse_reg[i]=nrmse(imputed_reg,iris.mis,iris.org)
  imp_for=missForest(iris.mis)
  nrmse_for[i]=imp_for$OOBerror
  KNN_imp=knn.impute(as.matrix(iris.mis),k = 5,to.impute = 1:nrow(iris.mis),using = 1:nrow(iris.mis))
  NRMSE_KNN[i]=nrmse(KNN_imp,iris.mis,iris.org)
  
}
#nrmse_mean
avg_meanimp=mean(nrmse_mean)
avg_meanimp
#nrmse_mul
avg_mulimp=mean(nrmse_mul)
avg_mulimp

#nrmse_reg
avg_reg=mean(nrmse_reg)
avg_reg

#nrmse_for
avg_for=mean(nrmse_for)
avg_for

#nrmse_knn
avg_knn=mean(NRMSE_KNN)
avg_knn




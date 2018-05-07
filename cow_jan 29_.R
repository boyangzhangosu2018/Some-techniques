char_data_cow_1 <- read.csv("Jan 29th_good_Book8.csv", 
                            stringsAsFactors = F)

#char_data_cow_1[,54:77]<- log(char_data_cow_1[,54:77]+0.5)
char_data_cow <- char_data_cow_1[-93,-c(1,4)]
str(char_data_cow)

char_data_cow <- char_data_cow[,-(52:75)]
num_data_cow <- data.frame(data.matrix(char_data_cow))
numeric_columns_cow <- sapply(num_data_cow,function(x){mean(as.numeric(is.na(x)))<0.5})
numeric_columns_cow
final_data_cow <- data.frame(num_data_cow[,numeric_columns_cow], char_data_cow[,!numeric_columns_cow])
final_data_cow$CH4.DMI <- 10*final_data_cow$CH4/final_data_cow$DIM
final_data_cow$CH4.MBW <- final_data_cow$CH4/(final_data_cow$BW^0.75)
#final_data_cow[,c(2:48,50:51)]<-scale(final_data_cow[,c(2:75)],center=T,scale=T)
final_data_cow[,c(2:48,50:51)]<-scale(final_data_cow[,c(2:48,50:51)],center=T,scale=T)
final_data_cow$DIETCODE<-as.factor(final_data_cow$DIETCODE)
final_data_cow$Cyllamyces_Factor<- as.factor(final_data_cow$Cyllamyces_Factor)
final_data_cow$GE_Factor<- as.factor(final_data_cow$GE_Factor)
final_data_cow$MFA18cis_Factor<-as.factor(final_data_cow$MFA18cis_Factor)
final_data_cow$MFA18_Factor<- as.factor(final_data_cow$MFA18_Factor)
str(final_data_cow)
##PCA
for (i in 1:nrow(final_data_cow)){
  for (j in 1:ncol(final_data_cow)) {
    if (is.na(final_data_cow[i,j]) == TRUE){
      print(c(i,j))
    }
  }
}
##Dummy vaiable 
library(dummies)
final_data_cow_dum <-dummy.data.frame(final_data_cow,names = c("DIETCODE"))
which(apply(final_data_cow_dum, 2, var)==0)
prin_comp_cow <- prcomp(na.omit(final_data_cow_dum[,-1]),scale=TRUE)
names(prin_comp_cow)
#outputs the mean of variables
prin_comp_cow$center
#outputs the standard deviation of variables
prin_comp_cow$scale
prin_comp_cow$rotation
prin_comp_cow
biplot(prin_comp_cow, scale = 0)
#compute standard deviation of each principal component
std_dev <- prin_comp_cow$sdev
#compute variance
pr_var <- std_dev^2
pr_var[1:10]
pr_var
#proportion of variance explained
prop_varex <- pr_var/sum(pr_var)
#scree plot
plot(prop_varex, xlab = "Principal Component",
     ylab = "Proportion of Variance Explained",
     type = "b")
plot(cumsum(prop_varex), xlab = "Principal Component",
     ylab = "Cumulative Proportion of Variance Explained",
     type = "b")
prin_comp_cow
prin_comp_cow$rotation[1:5,1:4]
plot(prin_comp_cow, type = "l")
summary(prin_comp_cow)
##ggplot
require(devtools)

library(ggbiplot)
library(ggplot2)

g <- ggbiplot(prin_comp_cow, obs.scale = 1, var.scale = 1,
              groups = final_data_cow$Cyllamyces_Factor,
              ellipse = TRUE, 
              circle = TRUE)
g <- g + scale_color_discrete(name = '')
g <- g + theme(legend.direction = 'horizontal', 
               legend.position = 'top')
print(g)

g <- ggbiplot(prin_comp_cow, obs.scale = 1, var.scale = 1,
              groups = final_data_cow$MFA18cis_Factor,
              ellipse = TRUE, 
              circle = TRUE)
g <- g + scale_color_discrete(name = '')
g <- g + theme(legend.direction = 'horizontal', 
               legend.position = 'top')
print(g)


g <- ggbiplot(prin_comp_cow, obs.scale = 1, var.scale = 1,
              groups = final_data_cow$MFA18_Factor,
              ellipse = TRUE, 
              circle = TRUE)
g <- g + scale_color_discrete(name = '')
g <- g + theme(legend.direction = 'horizontal', 
               legend.position = 'top')
print(g)




##LASSO
final_data_cow$CH4.DMI <- 10*final_data_cow$CH4/final_data_cow$DIM
final_data_cow$CH4.MBW <- final_data_cow$CH4/(final_data_cow$BW^0.75)
final_data_cow$ANIMID <- char_data_cow_1$ï..ANIMID[-93]
final_data_cow<-final_data_cow[,-80]

final_data_cow <- final_data_cow[,-49]
str(final_data_cow)
Variables1 <- paste("X", c(2:74), sep="")
a <- data.frame(name=c("DIETCODE", Variables1,"CH4.DMI", "CH4.MBW","ANIMID"))
names(final_data_cow) <- a$name
length(names(final_data_cow[1,]))
final_data_cow <- final_data_cow[,-78]
name_fi <- names(final_data_cow[1,])[-c(1,49,54,53,52)]

##VIF check
Variables1 <- paste("X", c(2,5,7,9:10,15,18:20,22,23,26:29,31:35,37,40,42,44:52,54:65,67:74), sep="")
PredictionFormula <- formula(paste("CH4.DMI~as.factor(DIETCODE) + ",
                                   paste(name_fi, collapse=" + ")))

PredictionFormula <- formula(paste("CH4.DMI~ + ",
                                   paste(name_fi, collapse=" + ")))


vif_1 <- vif(lm(formula = PredictionFormula,data = final_data_cow))
alias( lm(formula = PredictionFormula,data = final_data_cow))
vif_bad <- which(vif_1>100)
names(vif_bad[1])
length(vif_bad)
b = 0
col_name_vif_bad <- c()
for (n in 1:length(vif_bad)) {
  b <- which(names(vif_bad[n]) == name_fi)
  print(b)
  col_name_vif_bad[n] <- b
}
name_final[-col_name_vif_bad[1]]

##VIF Reducer


for (m in 1:length(col_name_vif_bad)) {
  name_final_one_by_one <- name_final[-col_name_vif_bad[n]]
  PredictionFormula <- formula(paste("CH4..g.kg.DMI. ~",
                                     paste(name_final_one_by_one, collapse=" + ")))
  vif_one_by_one <- vif(lm(formula = PredictionFormula,data = final_data))
  vif_bad_one_by_one <- which(vif_one_by_one>100)
  print(length(vif_bad_one_by_one))
}

n = 0
m = 0
collection_vif <- c()
vif_sample_bad_2_50 <- c()
collection_vif_2 <- c()
vif_sample_bad_2_100 <-c()
for (i in 1:length(col_name_vif_bad)) {
  for (j in (i+1):length(col_name_vif_bad)) {
    for (j3 in (j+1):length(col_name_vif_bad)) {
      for (j4 in (j3+1):length(col_name_vif_bad)) {
        
        
        
        aij <- c(i,j,j3,j4)
        name_sample_bad_2 <- name_final[-col_name_vif_bad[aij]]
        PredictionFormula <- formula(paste("CH4..g.kg.DMI. ~",
                                           paste(name_sample_bad_2, collapse=" + ")))
        vif_sample_bad_2 <- vif(lm(formula = PredictionFormula,data = final_data))
        vif_sample_bad_2_100 <- which(vif_sample_bad_2>100)
        n= n + 1
        
        collection_vif[n] <- length(vif_sample_bad_2_100)
        if (collection_vif[n] == 0){
          m = m + 1
          
          vif_sample_bad_2_50 <- which(vif_sample_bad_2>70)
          collection_vif_2[m] <- length(vif_sample_bad_2_50)
          if(collection_vif_2[m] == 0){
            print("yes")
            print(aij)
            
          }
        }
      }}}}



b <- vif(lm(formula = PredictionFormula,data = final_data_cow))
which(b>40)

which.max(b)
b

final_data_cow$ANIMID <- as.factor(final_data_cow$ANIMID)
final_data_cow$EXP <- as.factor(char_data_cow_1$EXP[-93])
final_data_cow$EXP

glm1_final <- glmmLasso(fix=PredictionFormula, rnd = list(EXP= ~1),  
                        data = final_data_cow, lambda=20, final.re = TRUE, family=family)

summary(glm1_final)
##LASSO
set.seed(12345)
lambda <- seq(1000,0,by=-0.1)
##family
family =gaussian(link = "identity")
## Using BIC 

BIC_vec<-rep(Inf,length(lambda))
## first fit good starting model
PQL <- glmmPQL(CH4.DMI ~ 1, random = ~1 |EXP, family = family, data = final_data_cow)
PQL <- glmmPQL(CH4.DMI ~ 1, random = ~1 |ANIMID, family = family, data = final_data_cow)
PQL
PQL <- glmmPQL(CH4.DMI ~ 1, random = pdBlocked(list(pdIdent(~EXP - 1),pdIdent(~ANIMID - 1)))
                 , family = family, data = final_data_cow)
PQL$coef$fixed
Delta.start <- c(as.numeric(PQL$coef$fixed), rep(0, 100), as.numeric(t(PQL$coef$random$ANIMID)))
for(i in 1:300)
{
  Delta.start <- c(as.numeric(PQL$coef$fixed), rep(0, i), as.numeric(t(PQL$coef$random$ANIMID)))
  
  glm1_test <- try(glmmLasso(fix=PredictionFormula, rnd = list(ANIMID = ~1),  
                             data = final_data_cow, lambda=10, final.re = TRUE, family=family, 
                             control=list(start=Delta.start,q_start=Q.start)), silent=TRUE)  
  
  
  if(class(glm1_test)!="try-error")
  {  
    n = n + 1
    i_collection[n] <- i 
    i_default <<- i_collection[which.min(i_collection)]
  }
}
print(i_default)
Delta.start <- c(as.numeric(PQL$coef$fixed), rep(0, i_default), as.numeric(t(PQL$coef$random$ANIMID)))


Q.start <-as.numeric(VarCorr(PQL)[1,1])
##Test
glm1_test <- glmmLasso(fix=PredictionFormula, rnd = list(ANIMID = ~1),  
                       data = final_data_cow, lambda=10, final.re = TRUE, family=family,
                       control=list(start=Delta.start,q_start=Q.start))
class(glm1_test)
## iterations
for(j in 1:length(lambda))
{
  print(paste("Iteration ", j, sep=""))
  
  glm1 <- try(glmmLasso(fix=PredictionFormula, rnd = list(ANIMID = ~1),  
                        data = final_data_cow, lambda=lambda[j], final.re = TRUE, family=family, 
                        control=list(start=Delta.start,q_start=Q.start)), silent=TRUE)  
  
  
  if(class(glm1)!="try-error")
  {  
    BIC_vec[j]<-glm1$bic
  }
}
## plot iterations
plot(BIC_vec)
opt<-which.min(BIC_vec)
opt
lambda[opt]
glm1_final <- glmmLasso(fix=PredictionFormula, rnd = list(ANIMID = ~1),  
                        data = final_data_cow, lambda=lambda[opt], final.re = TRUE, family=family,
                        control=list(start=Delta.start,q_start=Q.start))

summary(glm1_final)
glm1_final$bic

const = factor(rep(1, length(final_data_cow$CH4.DMI)))
glmmPQL(CH4.DMI ~ 1, random = list(const = pdBlocked(pdIdent(~ ANIMID - 1),pdIdent(~ EXP - 1))),
                                                   family = family, data = final_data_cow)

glmmPQL(CH4.DMI ~ 1, random = list(ANIMID=~1,EXP~1),
        family = family, data = final_data_cow)
glmmPQL(CH4.DMI ~ 1, random = ~1 | ANIMID/EXP,
        family = family, data = final_data_cow)

glm1_final <- glmmLasso(fix=PredictionFormula, rnd = list(EXP= ~1),  
                        data = final_data_cow, lambda=20, final.re = TRUE, family=family)



glm1_final <- glmmLasso(fix=PredictionFormula, rnd = list(EXP= ~1, ANIMID=~1),  
                        data = final_data_cow, lambda=20, final.re = TRUE, family=family,
                        control=list(start=Delta.start,q_start=Q.start))



summary(glm1_final)

table(final_data_cow$EXP, final_data_cow$ANIMID)




##LASSO
set.seed(12345)
lambda <- seq(1000,0,by=-0.1)

## iterations
for(j in 1:length(lambda))
{
  print(paste("Iteration ", j, sep=""))
  
  glm1 <- try(glmmLasso(fix=PredictionFormula, rnd = list(ANIMID = ~1),  
                        data = final_data_cow, lambda=lambda[j], final.re = TRUE, family=family), silent=TRUE)  
  
  
  if(class(glm1)!="try-error")
  {  
    BIC_vec[j]<-glm1$bic
  }
}
plot(BIC_vec)
opt<-which.min(BIC_vec)
opt
lambda[opt]
glm1_final <- glmmLasso(fix=PredictionFormula, rnd = list(ANIMID = ~1),  
                        data = final_data_cow, lambda=lambda[opt], final.re = TRUE, family=family)

summary(glm1_final)
glm1_final$bic


final_data_cow$CH4.DMI <- 10*final_data_cow$CH4.DMI


set.seed(12345)
lambda <- seq(1000,0,by=-1)

## iterations
for(j in 1:length(lambda))
{
  print(paste("Iteration ", j, sep=""))
  
  glm1 <- try(glmmLasso(fix=PredictionFormula, rnd = list(ANIMID = ~1),  
                        data = final_data_cow, lambda=lambda[j], final.re = TRUE, family=family), silent=TRUE)  
  
  
  if(class(glm1)!="try-error")
  {  
    BIC_vec[j]<-glm1$bic
  }
}
plot(BIC_vec)
opt<-which.min(BIC_vec)
opt
lambda[opt]
glm1_final <- glmmLasso(fix=PredictionFormula, rnd = list(ANIMID = ~1),  
                        data = final_data_cow, lambda=lambda[opt], final.re = TRUE, family=family)

summary(glm1_final)
glm1_final$bic




final_data_cow$CH4.DMI <- 0.1*final_data_cow$CH4.DMI
final_data_cow$CH4.DMI <- 15*final_data_cow$CH4.DMI

set.seed(12345)
lambda <- seq(1000,0,by=-1)

## iterations
for(j in 1:length(lambda))
{
  print(paste("Iteration ", j, sep=""))
  
  glm1 <- try(glmmLasso(fix=PredictionFormula, rnd = list(ANIMID = ~1),  
                        data = final_data_cow, lambda=lambda[j], final.re = TRUE, family=family), silent=TRUE)  
  
  
  if(class(glm1)!="try-error")
  {  
    BIC_vec[j]<-glm1$bic
  }
}
plot(BIC_vec)
opt<-which.min(BIC_vec)
opt
lambda[opt]
glm1_final <- glmmLasso(fix=PredictionFormula, rnd = list(ANIMID = ~1),  
                        data = final_data_cow, lambda=lambda[opt], final.re = TRUE, family=family)

summary(glm1_final)
glm1_final$bic
##Fliter significant coefficients

summary_glm <- summary(glm1_final)
glm_coeff <- summary_glm$coefficients

i = 0
i_significant_coeff <- c()
n = 0
for (i in 1:nrow(glm_coeff)) {
  j = 4
  
  if (glm_coeff[i,j] < 0.1 && is.na(glm_coeff[i,j]) == FALSE){
    print(i)
    n = n + 1
    i_significant_coeff[n] <- i
  }
  
}
significan_coeff <- glm_coeff[i_significant_coeff,]  
significan_coeff_names <- names(significan_coeff[,1])[-1]
significan_coeff_names
lambada_opt <- lambda[opt]
write.csv(BIC_vec,"BIC_100000iterations_cow_without.csv")
write.csv(glm_coeff, "coeff_summary_glm__cow_without.csv")
write.csv(significan_coeff_names,"sign_coeff_summary_glm_cow_without.csv")
write.csv(lambada_opt,"opt_lambada_cow_without.csv")

library(lme4)
library(MASS)
library(nlme)
library(car)
##Recursion 
le <- 4
g_max_sum <<- Inf
s <<- c()
counter <<- 0
##Regressors
num1 <- c(le-3)
MicrobesVariables <- paste("X",num1,sep = "")
MicrobesVariables
regressors = c("DMI..kg.d.", "BW..Kg.", "A.P.ratio", MicrobesVariables)
regressors

p2 <- function (track) {
  if (length(track) == le) {
    res <- t(as.matrix(track))
    names(res) = c("DMI..kg.d.", "BW..Kg.", "A.P.ratio", MicrobesVariables)
    allModelsList = apply(res, 1, function(x) as.formula(
      paste(c("CH4..g.kg.DMI. ~ 1 + (1|ANIMID)", regressors[x]),
            collapse=" + ")) )
    print(allModelsList)
    model_lmer <- lmer(formula = allModelsList[[1]], data=final_train, REML = FALSE )
    sum <- summary(model_lmer)
   
    counter <<- counter+1
    print(counter)
      if(sum$AICtab[2] < g_max_sum){g_max_sum <<- sum$AICtab[2]
                                    
                                    s <<- sum <- summary(model_lmer)}
    
  } else {
    track <- c(track, FALSE)
    p2(track)
    length(track) <- length(track) - 1
    track <- c(track, TRUE)
    p2(track)
    length(track) <- length(track) - 1
  }
}
p2(c())
g_max_sum
s$AICtab
?lmer
final_train





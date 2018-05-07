library(rcompanion)
library(MASS)

##import data

char_data <- read.csv(file.choose(), stringsAsFactors = F)
num_data <- data.frame(data.matrix(char_data))
numeric_columns <- sapply(num_data,function(x){mean(as.numeric(is.na(x)))<0.5})
numeric_columns
final_data <- data.frame(num_data[,numeric_columns], char_data[,!numeric_columns])
final_data$ANIMID<-as.factor(final_data$ANIMID)

##factor or numeric then scale
## grid for the smoothing parameter
## center all metric variables so that also the starting values with glmmPQL are in the correct scaling

final_data$SEX<-as.factor(final_data$SEX)
final_data$SampleID<-as.factor(final_data$SampleID)
final_data$Sheep.Breed<-as.factor(final_data$Sheep.Breed)




sum_very_rare_ex <- c()
n <- 0
sum_col = 0 

for (j in 6:90) {
  sum_col = sum(final_data[,j]) 
  if (sum_col < 0.5){
    n = n + 1
    print(c(j))
    sum_very_rare_ex[n] = j
    
  }
}
final_data_not_0.5 <- final_data[,-sum_very_rare_ex]

final_data_not_0.5


##Produce a lot of distribution graphes like a lot!
library(jpeg)
names = c(6:58)
for (i in 6:58) {
                  
                  mypath <- file.path("C:","Users","Boyang Zhang","Documents",paste("myplot_", names[i-5], ".jpeg", sep = ""))
                  
                  jpeg(file=mypath)
                  Norm_gh_formula <- final_data_not_0.5[,i]
                  plotNormalHistogram(Norm_gh_formula)
                  dev.off()
                  }

##Produce a lot of distribution graphes like a lot! LOG
library(jpeg)
names = c(6:58)
for (i in 6:58) {
  
                  mypath <- file.path("C:","Users","Boyang Zhang","Documents",paste("myplot_", names[i-5], ".jpeg", sep = ""))
                  
                  jpeg(file=mypath)
                  Norm_gh_formula <- log(final_data_not_0.5[,i] + 1)
                  plotNormalHistogram(Norm_gh_formula)
                  dev.off()
                  }


Norm_gh_formula <-log(final_data_not_0.5[,7] + 1)
dat <- data.frame(
  
                  cond = factor(
                                  rep(c("original","LOG"), each=length(log(final_data_not_0.5[,6])))), 
                  R.A = c(final_data_not_0.5[,7],Norm_gh_formula))
                  
library(ggplot2)
ggplot(dat, aes(x=dat$R.A, fill=dat$cond)) + geom_density(alpha=.3) + 
  xlab("Methanobacteria") + 
  ylab("Density")+
  labs(fill='Condition')

p <- ggplot(dat, aes(x=R.A)) + geom_histogram(binwidth=.5)
p
plotNormalHistogram(Norm_gh_formula)

legend(1.8,48,legend =c("DMI Normality"),col="blue", lty=1)
plotNormalHistogram(final_data$CH4..g.d.)
legend(24,48,legend =c("Methane(g) Per day, Normality"),col="blue", lty=1)
plotNormalHistogram(final_data$BW..Kg.)
legend(24,60,legend =c("Body Weight Normality"),col="blue", lty=1)
plotNormalHistogram(final_data$A.P.ratio)
legend(4.55,75,legend =c("The acate:propinate ratio Normality"),col="blue", lty=1)
plotNormalHistogram(final_data$X1)
legend(0.003,75,legend =c("Archaea, Normality"),col="blue", lty=1)
        Box = boxcox(final_data$CH4..g.kg.DMI. ~final_data$X1 ,              # Transform Turbidity as a single vector
                     lambda = seq(0.1,6,0.1)      # Try values -6 to 6 by 0.1
        )
        Cox = data.frame(Box$x, Box$y)
        Cox2 = Cox[with(Cox, order(-Cox$Box.y)),]
        Cox2[1,]
        Cox2
        lambda = Cox2[1, "Box.x"]
        T_box = (final_data$X1 * lambda + 1)^(1/lambda)   # Transform the original data
        plotNormalHistogram(T_box)
        plotNormalHistogram(sqrt(final_data$X1))
        plotNormalHistogram((final_data$X1)^(1/3))
        qqnorm(final_data$X1,
               ylab="Sample Quantiles for Turbidity")
        
                
plotNormalHistogram(final_data$X2)
legend(0.45,35,legend =c("Archaea2, Normality"),col="blue", lty=1)
plotNormalHistogram(final_data$X3)
legend(0.35,40,legend =c("Archaea3, Normality"),col="blue", lty=1)
plotNormalHistogram(final_data$X1)
        x1_T <- logit(final_data$X1)
        plotNormalHistogram(x1_T)
plotNormalHistogram(final_data$X2)
plotNormalHistogram(final_data$X3)
plotNormalHistogram(final_data$X4)
plotNormalHistogram(final_data$X5)
        x5_T <- logit(final_data$X5)
        plotNormalHistogram(x5_T)
plotNormalHistogram(final_data$X6)
plotNormalHistogram(final_data$X7)
        x7_T <- logit(final_data$X7)
        plotNormalHistogram(x7_T)
plotNormalHistogram(final_data$X8)
plotNormalHistogram(final_data$X9)
plotNormalHistogram(final_data$X10)

##11-20
plotNormalHistogram(final_data$X11)
plotNormalHistogram(final_data$X12)
        x12_T <- logit(final_data$X12)
        plotNormalHistogram(x12_T)
plotNormalHistogram(final_data$X13)
plotNormalHistogram(final_data$X14)
plotNormalHistogram(final_data$X15)
plotNormalHistogram(final_data$X16)
plotNormalHistogram(final_data$X17)
plotNormalHistogram(final_data$X18)
plotNormalHistogram(final_data$X19)
plotNormalHistogram(final_data$X20)

#21-30
plotNormalHistogram(final_data$X21)
plotNormalHistogram(final_data$X22)
plotNormalHistogram(final_data$X23)

plotNormalHistogram(final_data$X24)
plotNormalHistogram(final_data$X25)
plotNormalHistogram(final_data$X26)
plotNormalHistogram(final_data$X27)
plotNormalHistogram(final_data$X28)
plotNormalHistogram(final_data$X29)

plotNormalHistogram(final_data$X30)

#31-40

plotNormalHistogram(final_data$X41)

plotNormalHistogram(final_data$X42)
plotNormalHistogram(final_data$X43)

plotNormalHistogram(final_data$X44)
plotNormalHistogram(final_data$X45)
plotNormalHistogram(final_data$X46)
plotNormalHistogram(final_data$X47)
plotNormalHistogram(final_data$X48)
plotNormalHistogram(final_data$X49)
plotNormalHistogram(final_data$X50)

#41-50
plotNormalHistogram(final_data$X31)
plotNormalHistogram(final_data$X32)
plotNormalHistogram(final_data$X33)
plotNormalHistogram(final_data$X34)
plotNormalHistogram(final_data$X35)
plotNormalHistogram(final_data$X36)
plotNormalHistogram(final_data$X37)
plotNormalHistogram(final_data$X38)
plotNormalHistogram(final_data$X39)
plotNormalHistogram(final_data$X40)

#51-60
plotNormalHistogram(final_data$X51)
plotNormalHistogram(final_data$X52)
plotNormalHistogram(final_data$X53)
plotNormalHistogram(final_data$X54)
plotNormalHistogram(final_data$X55)
plotNormalHistogram(final_data$X56)
plotNormalHistogram(final_data$X57)
plotNormalHistogram(final_data$X58)
plotNormalHistogram(final_data$X59)
plotNormalHistogram(final_data$X60)

#61-70
plotNormalHistogram(final_data$X61)
plotNormalHistogram(final_data$X62)
plotNormalHistogram(final_data$X63)
plotNormalHistogram(final_data$X64)
plotNormalHistogram(final_data$X65)
plotNormalHistogram(final_data$X66)
plotNormalHistogram(final_data$X67)
plotNormalHistogram(final_data$X68)
plotNormalHistogram(final_data$X69)
plotNormalHistogram(final_data$X70)

##71-80
plotNormalHistogram(final_data$X71)
plotNormalHistogram(final_data$X72)
plotNormalHistogram(final_data$X73)
plotNormalHistogram(final_data$X74)
plotNormalHistogram(final_data$X75)
plotNormalHistogram(final_data$X76)
plotNormalHistogram(final_data$X77)
plotNormalHistogram(final_data$X78)
plotNormalHistogram(final_data$X79)
plotNormalHistogram(final_data$X80)

#81-85
plotNormalHistogram(final_data$X81)
plotNormalHistogram(final_data$X82)
plotNormalHistogram(final_data$X83)
plotNormalHistogram(final_data$X84)
plotNormalHistogram(final_data$X85)

##PLOT 
MicrobesVariables_E_1 <- paste("X", 1:5, sep="")
MicrobesVariables_E_2 <- paste("X", 5:10, sep="")
MicrobesVariables_E_3 <- paste("X", 11:15, sep="")
MicrobesVariables_E_4 <- paste("X", 16:20, sep="")
MicrobesVariables_E_5 <- paste("X", 21:25, sep="")
MicrobesVariables_E_6 <- paste("X", 26:30, sep="")
MicrobesVariables_E_7 <- paste("X", 31:35, sep="")
MicrobesVariables_E_8 <- paste("X", 36:40, sep="")
MicrobesVariables_E_9 <- paste("X", 41:45, sep="")
MicrobesVariables_E_10 <- paste("X", 46:50, sep="")
MicrobesVariables_E_11<- paste("X", 51:55, sep="")
MicrobesVariables_E_12<- paste("X", 56:60, sep="")
MicrobesVariables_E_13<- paste("X", 61:65, sep="")
MicrobesVariables_E_14<- paste("X", 66:70, sep="")
MicrobesVariables_E_15 <- paste("X", 71:75, sep="")
MicrobesVariables_E_16 <- paste("X", 76:80, sep="")
MicrobesVariables_E_17 <- paste("X", 81:85, sep="")

PredictionFormula1 <- formula(paste("CH4..g.kg.DMI. ~ DMI..kg.d.+BW..Kg.+A.P.ratio+as.factor(SEX)" ))
PredictionFormula_1 <- formula(paste("CH4..g.kg.DMI. ~ ",
                                   paste(MicrobesVariables_E_1, collapse="+ ") ))
PredictionFormula_2 <- formula(paste("CH4..g.kg.DMI. ~ ",
                                   paste(MicrobesVariables_E_2, collapse=" + ")))
PredictionFormula_3 <- formula(paste("CH4..g.kg.DMI. ~",
                                   paste(MicrobesVariables_E_3, collapse=" + ")))
PredictionFormula_4 <- formula(paste("CH4..g.kg.DMI. ~",
                                     paste(MicrobesVariables_E_4, collapse=" + ")))
PredictionFormula_5 <- formula(paste("CH4..g.kg.DMI. ~",
                                     paste(MicrobesVariables_E_5, collapse=" + ")))
PredictionFormula_6 <- formula(paste("CH4..g.kg.DMI. ~",
                                     paste(MicrobesVariables_E_6, collapse=" + ")))
PredictionFormula_7 <- formula(paste("CH4..g.kg.DMI. ~",
                                     paste(MicrobesVariables_E_7, collapse=" + ")))
PredictionFormula_8 <- formula(paste("CH4..g.kg.DMI. ~",
                                     paste(MicrobesVariables_E_8, collapse=" + ")))
PredictionFormula_9 <- formula(paste("CH4..g.kg.DMI. ~ ",
                                     paste(MicrobesVariables_E_9, collapse="+ ") ))
PredictionFormula_10 <- formula(paste("CH4..g.kg.DMI. ~ ",
                                     paste(MicrobesVariables_E_10, collapse=" + ")))
PredictionFormula_11 <- formula(paste("CH4..g.kg.DMI. ~",
                                     paste(MicrobesVariables_E_11, collapse=" + ")))
PredictionFormula_12 <- formula(paste("CH4..g.kg.DMI. ~",
                                     paste(MicrobesVariables_E_12, collapse=" + ")))
PredictionFormula_13 <- formula(paste("CH4..g.kg.DMI. ~",
                                     paste(MicrobesVariables_E_13, collapse=" + ")))
PredictionFormula_14 <- formula(paste("CH4..g.kg.DMI. ~",
                                     paste(MicrobesVariables_E_14, collapse=" + ")))
PredictionFormula_15 <- formula(paste("CH4..g.kg.DMI. ~",
                                     paste(MicrobesVariables_E_15, collapse=" + ")))
PredictionFormula_16 <- formula(paste("CH4..g.kg.DMI. ~",
                                     paste(MicrobesVariables_E_16, collapse=" + ")))
PredictionFormula_17 <- formula(paste("CH4..g.kg.DMI. ~",
                                     paste(MicrobesVariables_E_17, collapse=" + ")))

pairs(PredictionFormula1,data = final_data)
pairs(PredictionFormula_1,data = final_data)
pairs(PredictionFormula_2,data = final_data)
pairs(PredictionFormula_3,data = final_data)
pairs(PredictionFormula_4,data = final_data)
pairs(PredictionFormula_5,data = final_data)
pairs(PredictionFormula_6,data = final_data)
pairs(PredictionFormula_7,data = final_data)
pairs(PredictionFormula_8,data = final_data)
pairs(PredictionFormula_9,data = final_data)
pairs(PredictionFormula_10,data = final_data)
pairs(PredictionFormula_11,data = final_data)
pairs(PredictionFormula_12,data = final_data)
pairs(PredictionFormula_13,data = final_data)
pairs(PredictionFormula_14,data = final_data)
pairs(PredictionFormula_15,data = final_data)
pairs(PredictionFormula_16,data = final_data)
pairs(PredictionFormula_17,data = final_data)

library(ggplot2)
str(final_data)
ggplot(final_data, aes(x = final_data$A.P.ratio, y = final_data$CH4..g.kg.DMI.)) +
  geom_point()


ggplot(final_data, aes(x = X4,
                       y = CH4..g.kg.DMI.)) +
                       xlab("Methanobacteria")+
                       ylab("CH4 per day per DMI")+
                       geom_point()
ggplot(final_data, aes(x = X37,
                       y = CH4..g.kg.DMI.)) +
                       xlab("Prevotella")+
                       ylab("CH4 per day per DMI")+
                       geom_point()

ggplot(final_data,aes(x= A.P.ratio,
                      y = CH4..g.kg.DMI., 
                      col = final_data$X66))+ 
                      ylab("CH4 per day per DMI")+
                      xlab("A.P ratio")+
                      labs(color='Ruminococcaceae')+
                      geom_point()+
  
                      facet_grid(.~final_data$Sheep.Breed)
ggplot(final_data,aes(x= A.P.ratio,
                      y = CH4..g.kg.DMI., 
                      col = final_data$X66))+ 
  ylab("CH4 per day per DMI")+
  xlab("A.P ratio")+
  labs(color='Ruminococcaceae')+
  geom_point()+
  
  facet_grid(.~final_data$ï..ANIMID)

length(final_data$SampleID)
## sO LARGE GRAPH, need smaller size

final_data_small <- final_data[c(1:5),]

                  ggplot(final_data_small,aes(x= A.P.ratio,
                                          y = CH4..g.kg.DMI., 
                                          col = X66))+ 
                    ylab("CH4 per day per DMI")+
                    xlab("A.P ratio")+
                    labs(color='Ruminococcaceae')+
                    geom_point()+
                    facet_grid(.~final_data_small$ï..ANIMID)
total_3 <- c()
n = 0 
for (i in 1:118) {
  find_total <- which(final_data_not_0.5$ï..ANIMID == i )
  total <- length(find_total)
  if (total > 2 ) {
    n = n + 1
    print("yeas")
    total_4[n] <- i
    append(list_3,find_total)
  }
  
}
## over 3 samples merage them

list_3 <- c(which(final_data_not_0.5$ï..ANIMID == 41 ),
            which(final_data_not_0.5$ï..ANIMID == 87 ),
            which(final_data_not_0.5$ï..ANIMID == 105 ),
            which(final_data_not_0.5$ï..ANIMID == 1 ),
            which(final_data_not_0.5$ï..ANIMID == 43 ),
            which(final_data_not_0.5$ï..ANIMID == 51 ),
            which(final_data_not_0.5$ï..ANIMID == 54 ),
            which(final_data_not_0.5$ï..ANIMID == 55 ),
            which(final_data_not_0.5$ï..ANIMID == 77 ),
            which(final_data_not_0.5$ï..ANIMID == 79 ),
            which(final_data_not_0.5$ï..ANIMID == 87 ),
            which(final_data_not_0.5$ï..ANIMID == 113 ))




ggplot(final_data_animal_difference,aes(x= A.P.ratio,
                            y = CH4..g.kg.DMI., 
                            col = X66))+ 
  ylab("CH4 per day per DMI")+
  xlab("A.P ratio")+
  labs(color='Ruminococcaceae')+
  geom_point()+
  facet_grid(.~final_data_animal_difference$ï..ANIMID)

## still large but try to seperate it to 4 graphs 

par(mfrow=c(2,2))
list_1 <- c(which(final_data_not_0.5$ï..ANIMID == 41 ),
            which(final_data_not_0.5$ï..ANIMID == 87 ),
            which(final_data_not_0.5$ï..ANIMID == 105 ))
final_data_animal_difference_1 <- final_data_not_0.5[list_1,]
plot_1 <- ggplot(final_data_animal_difference_1,aes(x= A.P.ratio,
                                        y = CH4..g.kg.DMI., 
                                        col = X66))+ 
  ylab("CH4 per day per DMI")+
  xlab("A.P ratio")+
  labs(color='Ruminococcaceae')+
  geom_point()+
  facet_grid(.~final_data_animal_difference_1$ï..ANIMID)


list_2 <- c(which(final_data_not_0.5$ï..ANIMID == 1 ),
            which(final_data_not_0.5$ï..ANIMID == 43 ),
            which(final_data_not_0.5$ï..ANIMID == 51 ))
final_data_animal_difference_2 <- final_data_not_0.5[list_2,]
plot_2 <- ggplot(final_data_animal_difference_2,aes(x= A.P.ratio,
                                          y = CH4..g.kg.DMI., 
                                          col = X66))+ 
  ylab("CH4 per day per DMI")+
  xlab("A.P ratio")+
  labs(color='Ruminococcaceae')+
  geom_point()+
  facet_grid(.~final_data_animal_difference_2$ï..ANIMID)

list_3 <- c(which(final_data_not_0.5$ï..ANIMID == 54 ),
            which(final_data_not_0.5$ï..ANIMID == 55 ),
            which(final_data_not_0.5$ï..ANIMID == 77 ))
final_data_animal_difference_3 <- final_data_not_0.5[list_3,]
plot_3 <- ggplot(final_data_animal_difference_3,aes(x= A.P.ratio,
                                                    y = CH4..g.kg.DMI., 
                                                    col = X66))+ 
  ylab("CH4 per day per DMI")+
  xlab("A.P ratio")+
  labs(color='Ruminococcaceae')+
  geom_point()+
  facet_grid(.~final_data_animal_difference_3$ï..ANIMID)

list_4 <- c(which(final_data_not_0.5$ï..ANIMID == 79 ),
            which(final_data_not_0.5$ï..ANIMID == 87 ),
            which(final_data_not_0.5$ï..ANIMID == 113 ))
final_data_animal_difference_4 <- final_data_not_0.5[list_4,]
plot_4 <- ggplot(final_data_animal_difference_4,aes(x= A.P.ratio,
                                                    y = CH4..g.kg.DMI., 
                                                    col = X66))+ 
  ylab("CH4 per day per DMI")+
  xlab("A.P ratio")+
  labs(color='Ruminococcaceae')+
  geom_point()+
  facet_grid(.~final_data_animal_difference_4$ï..ANIMID)
require(gridExtra)
grid.arrange(plot_1, plot_2, plot_3, plot_4, ncol=2)




##Acturally this one is good

final_data_animal_difference <- final_data_not_0.5[list_3,]


plot <- ggplot(final_data_animal_difference,aes(x= A.P.ratio,
                                                y = CH4..g.kg.DMI., 
                                                col = X66))+ 
  ylab("CH4 per day per DMI")+
  xlab("A.P ratio")+
  labs(color='Ruminococcaceae')+
  geom_point()+
  facet_wrap(~ final_data_animal_difference$ï..ANIMID, ncol=4)

plot





ggplot(final_data, aes(x = final_data$X14, y = final_data$CH4..g.kg.DMI.)) +
  geom_point()
ggplot(final_data, aes(x = final_data$X28, y = final_data$CH4..g.kg.DMI.)) +
  geom_point()
ggplot(final_data, aes(x = final_data$X33, y = final_data$CH4..g.kg.DMI.)) +
  geom_point()
ggplot(final_data, aes(x = final_data$X37, y = final_data$CH4..g.kg.DMI.)) +
  geom_point()
ggplot(final_data, aes(x = final_data$X42, y = final_data$CH4..g.kg.DMI.)) +
  geom_point()
ggplot(final_data, aes(x = final_data$X65, y = final_data$CH4..g.kg.DMI.)) +
  geom_point()
ggplot(final_data, aes(x = final_data$X66, y = final_data$CH4..g.kg.DMI.)) +
  geom_point()
ggplot(final_data, aes(x = final_data$X71, y = final_data$CH4..g.kg.DMI.)) + 
  geom_point()
ggplot(final_data, aes(x = final_data$X72, y = final_data$CH4..g.kg.DMI.)) +
  geom_point()
ggplot(final_data, aes(x = final_data$X74, y = final_data$CH4..g.kg.DMI.)) +
  geom_point()
    
    ##Two factors
    ggplot(final_data, aes(x = final_data$X80, y = final_data$CH4..g.kg.DMI.,color = final_data$X4)) +
      geom_point()+geom_smooth()
    ggplot(final_data, aes(x = final_data$A.P.ratio, y = final_data$CH4..g.kg.DMI.,color = final_data$X14)) +
      geom_point()+geom_smooth()
    ggplot(final_data, aes(x = final_data$A.P.ratio, y = final_data$CH4..g.kg.DMI.,color = final_data$X28)) +
      geom_point()+geom_smooth()
    ggplot(final_data, aes(x = final_data$A.P.ratio, y = final_data$CH4..g.kg.DMI.,color = final_data$X33)) +
      geom_point()+geom_smooth()
    ggplot(final_data, aes(x = final_data$A.P.ratio, y = final_data$CH4..g.kg.DMI.,color = final_data$X37)) +
      geom_point()+geom_smooth()
    ggplot(final_data, aes(x = final_data$A.P.ratio, y = final_data$CH4..g.kg.DMI.,color = final_data$X42)) +
      geom_point()+geom_smooth()
    ggplot(final_data, aes(x = final_data$A.P.ratio, y = final_data$CH4..g.kg.DMI.,color = final_data$X65)) +
      geom_point()+geom_smooth()
    ggplot(final_data, aes(x = final_data$A.P.ratio, y = final_data$CH4..g.kg.DMI.,color = final_data$X71)) +
      geom_point()+geom_smooth()
    ggplot(final_data, aes(x = final_data$A.P.ratio, y = final_data$CH4..g.kg.DMI.,color = final_data$X66)) +
      geom_point()+geom_smooth()
    ggplot(final_data, aes(x = final_data$A.P.ratio, y = final_data$CH4..g.kg.DMI.,color = final_data$X72)) +
      geom_point()+geom_smooth()
    ggplot(final_data, aes(x = final_data$A.P.ratio, y = final_data$CH4..g.kg.DMI.,color = final_data$X74)) +
      geom_point()+geom_smooth()
    
    ##Three factors
    ggplot(final_data, aes(x = final_data$X80, y = final_data$CH4..g.kg.DMI.,size = final_data$X4)) +
      geom_point()+geom_smooth()
    ggplot(final_data, aes(x = final_data$A.P.ratio, y = final_data$CH4..g.kg.DMI.,size = final_data$X14)) +
      geom_point()+geom_smooth()
    ggplot(final_data, aes(x = final_data$A.P.ratio, y = final_data$CH4..g.kg.DMI.,size = final_data$X28)) +
      geom_point()+geom_smooth()
    ggplot(final_data, aes(x = final_data$A.P.ratio, y = final_data$CH4..g.kg.DMI.,size = final_data$X33)) +
      geom_point()+geom_smooth()
    ggplot(final_data, aes(x = final_data$A.P.ratio, y = final_data$CH4..g.kg.DMI.,size = final_data$X37)) +
      geom_point()+geom_smooth()
    ggplot(final_data, aes(x = final_data$A.P.ratio, y = final_data$CH4..g.kg.DMI.,size = final_data$X42)) +
      geom_point()+geom_smooth()
    ggplot(final_data, aes(x = final_data$A.P.ratio, y = final_data$CH4..g.kg.DMI.,size = final_data$X65)) +
      geom_point()+geom_smooth()
    ggplot(final_data, aes(x = final_data$A.P.ratio, y = final_data$CH4..g.kg.DMI.,size = final_data$X71)) +
      geom_point()+geom_smooth()
    ggplot(final_data, aes(x = final_data$A.P.ratio, y = final_data$CH4..g.kg.DMI.,size = final_data$X66)) +
      geom_point()+geom_smooth()
    ggplot(final_data, aes(x = final_data$A.P.ratio, y = final_data$CH4..g.kg.DMI.,size = final_data$X72)) +
      geom_point()+geom_smooth()
    ggplot(final_data, aes(x = final_data$A.P.ratio, y = final_data$CH4..g.kg.DMI.,size = final_data$X74)) +
      geom_point()+geom_smooth()
                ##Four factor
                ggplot(final_data, aes(x = final_data$A.P.ratio, y = final_data$CH4..g.kg.DMI.,size = final_data$X4,color = final_data$X66)) +
                  geom_point()+geom_smooth()
                ggplot(final_data, aes(x = final_data$A.P.ratio, y = final_data$CH4..g.kg.DMI.,size = final_data$X28,color = final_data$X66)) +
                  geom_point()+geom_smooth()
                ggplot(final_data, aes(x = final_data$A.P.ratio, y = final_data$CH4..g.kg.DMI.,size = final_data$X33,color = final_data$X66)) +
                  geom_point()+geom_smooth()
                ggplot(final_data, aes(x = final_data$A.P.ratio, y = final_data$CH4..g.kg.DMI.,size = final_data$X37,color = final_data$X66)) +
                  geom_point()+geom_smooth()
                ggplot(final_data, aes(x = final_data$A.P.ratio, y = final_data$CH4..g.kg.DMI.,size = final_data$X42,color = final_data$X66)) +
                  geom_point()+geom_smooth()
                ggplot(final_data, aes(x = final_data$A.P.ratio, y = final_data$CH4..g.kg.DMI.,size = final_data$X65,color = final_data$X66)) +
                  geom_point()+geom_smooth()
                ggplot(final_data, aes(x = final_data$A.P.ratio, y = final_data$CH4..g.kg.DMI.,size = final_data$X72,color = final_data$X66)) +
                  geom_point()+geom_smooth()
                ggplot(final_data, aes(x = final_data$A.P.ratio, y = final_data$CH4..g.kg.DMI.,size = final_data$X74,color = final_data$X66)) +
                  geom_point()+geom_smooth()

ggplot(final_data, aes(x = final_data$A.P.ratio, y = final_data$CH4..g.kg.DMI.)) +
      geom_point()+geom_smooth()
ggplot(final_data, aes(x = final_data$X4, y = final_data$CH4..g.kg.DMI.)) +
      geom_point()+geom_smooth()
ggplot(final_data, aes(x = final_data$X14, y = final_data$CH4..g.kg.DMI.)) +
      geom_point()+geom_smooth()
ggplot(final_data, aes(x = final_data$X28, y = final_data$CH4..g.kg.DMI.)) +
      geom_point()+geom_smooth()
ggplot(final_data, aes(x = final_data$X33, y = final_data$CH4..g.kg.DMI.)) +
      geom_point()+geom_smooth()
ggplot(final_data, aes(x = final_data$X37, y = final_data$CH4..g.kg.DMI.)) +
      geom_point()+geom_smooth()
ggplot(final_data, aes(x = final_data$X42, y = final_data$CH4..g.kg.DMI.)) +
      geom_point()+geom_smooth()
ggplot(final_data, aes(x = final_data$X65, y = final_data$CH4..g.kg.DMI.)) +
      geom_point()+geom_smooth()
ggplot(final_data, aes(x = final_data$X66, y = final_data$CH4..g.kg.DMI.)) +
      geom_point()+geom_smooth()
ggplot(final_data, aes(x = final_data$X71, y = final_data$CH4..g.kg.DMI.)) +
      geom_point()+geom_smooth()
ggplot(final_data, aes(x = final_data$X72, y = final_data$CH4..g.kg.DMI.)) +
      geom_point()+geom_smooth()
ggplot(final_data, aes(x = final_data$X74, y = final_data$CH4..g.kg.DMI.)) +
      geom_point()+geom_smooth()
                ##Four factor Species. 
                ggplot(final_data,aes(x= final_data$A.P.ratio, y = final_data$CH4..g.kg.DMI., 
                                      col = final_data$X66))+ 
                                      geom_point()+
                                      facet_grid(.~final_data$Sheep.Breed)
                ##Four factor Species. 
                ggplot(final_data,aes(x= final_data$A.P.ratio, y = final_data$CH4..g.kg.DMI., 
                                      col = final_data$X66))+ 
                  geom_point()+
                  facet_grid(.~final_data$SEX)
                
                ##Four factor Species. 
                ggplot(final_data,aes(x= final_data$A.P.ratio, y = final_data$CH4..dg.kg.MBW., 
                                      col = final_data$X66))+ 
                  geom_point()+
                  facet_grid(.~final_data$SEX)+geom_smooth()
                
##After PCA
str(final_data)
ggplot(final_data, aes(x = final_data$X17, y = final_data$CH4..g.kg.DMI.)) +
                  geom_point()
final_data_exp <- final_data
final_data_exp
require(dummies)
final_data_exp <- dummy.data.frame(final_data_exp,names = c("SEX","Sheep.Breed"))
ggplot(final_data, aes(x = final_data$SEX, y = final_data$CH4..g.kg.DMI.)) +
  geom_point()
ggplot(final_data, aes(x = final_data$SEX, y = final_data$CH4..dg.kg.MBW.)) +
  geom_point()
ggplot(final_data, aes(x = final_data$X74, y = final_data$CH4..dg.kg.MBW.)) +
  geom_point()
ggplot(final_data, aes(x = final_data$A.P.ratio, y = final_data$CH4..dg.kg.MBW.)) +
  geom_point()
ggplot(final_data, aes(x = final_data$X32, y = final_data$CH4..dg.kg.MBW.)) +
  geom_point()
ggplot(final_data, aes(x = final_data$X47, y = final_data$CH4..dg.kg.MBW.)) +
  geom_point()
##Two factors

ggplot(final_data, aes(x = final_data$A.P.ratio, y = final_data$CH4..g.kg.DMI.,color = final_data$X32)) +
  geom_point()+geom_smooth(method = "lm")+facet_grid(.~final_data$SEX)




plot_CH4_gender <- ggplot(final_data,aes(x=final_data$SEX,y=final_data$CH4..g.d.))+geom_boxplot()+
                    scale_y_continuous(name = "Methane")+
                    scale_x_discrete(name= "Gender")+
                    ggtitle("Boxplot of Methane by Gender")+
                    geom_boxplot(fill = "#4271AE", colour = "#1F3552",alpha = 0.7)
                    
plot_CH4_gender

plot_AP_gender <- ggplot(final_data,aes(x=final_data$SEX,y=final_data$A.P.ratio))+geom_boxplot()+
  scale_y_continuous(name = "AP ratio")+
  scale_x_discrete(name= "Gender")+
  ggtitle("Boxplot of AP ratio by Gender")+
  geom_boxplot(fill = "#4271AE", colour = "#1F3552",alpha = 0.7)

plot_AP_gender

plot_BW_gender <- ggplot(final_data,aes(x=final_data$SEX,y=final_data$BW..Kg.))+geom_boxplot()+
  scale_y_continuous(name = "BW")+
  scale_x_discrete(name= "Gender")+
  ggtitle("Boxplot of BW ratio by Gender")+
  geom_boxplot(fill = "#4271AE", colour = "#1F3552",alpha = 0.7)

plot_BW_gender

plot_DMI_gender <- ggplot(final_data,aes(x=final_data$SEX,y=final_data$DMI..kg.d.))+geom_boxplot()+
  scale_y_continuous(name = "DMI")+
  scale_x_discrete(name= "Gender")+
  ggtitle("Boxplot of DMI by Gender")+
  geom_boxplot(fill = "#4271AE", colour = "#1F3552",alpha = 0.7)

plot_DMI_gender

library(car)

ab_model <- aov(final_data$CH4..g.d.~final_data$SEX*final_data$DMI..kg.d.
                *final_data$BW..Kg.*final_data$A.P.ratio
                *final_data$A6*final_data$F8*final_data$B80*final_data$P3)
summary(ab_model)  
ab_model <- aov(final_data$CH4..g.d.~final_data$SEX*final_data$DMI..kg.d.
                *final_data$BW..Kg.*final_data$A.P.ratio
                )
summary(ab_model)  
ab_model <- aov(final_data$CH4..g.d.~final_data$SEX
                *final_data$A6*final_data$F8*final_data$B80*final_data$P3)
summary(ab_model)  






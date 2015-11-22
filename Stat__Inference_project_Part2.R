#Statistical Inference Project Part 2
#
#Step1: Perform exploratory data analysis
library(datasets)
data("ToothGrowth")
#
str(ToothGrowth)
ToothGrowth$dose <- as.factor(ToothGrowth$dose)
#
library(ggplot2)
g1 <- ggplot(aes(x=dose,y=len), data=ToothGrowth) + geom_boxplot(aes(fill=dose))
print(g1)
g2 <- ggplot(aes(x=supp, y=len), data = ToothGrowth) + geom_boxplot(aes(fill=supp))
print(g2)
g3 <- ggplot(aes(x=supp, y=len), data = ToothGrowth) + geom_boxplot(aes(fill=supp))
g3 <- g3 + facet_wrap(~dose)
print(g3)
#
#overall summary:
summary(ToothGrowth)
#summary grouped by supplement and dose
by(ToothGrowth$len, INDICES = list(ToothGrowth$supp, ToothGrowth$dose), summary)
#Step2: Use hypothesis test to determine siginficant differences in tooth length
#from supp
#
t.test(len ~ supp, data = ToothGrowth)
#
#create three dose sub-level pairs and test for dose significance
t_tests <- list()
doses <- c("0.5","1","2")
for (dL in doses){
     for(dH in doses){
          if (dL < dH){
               ToothGrowth_sub <- subset(ToothGrowth, dose %in% c(dL,dH))
               t <- t.test(len ~ dose, data = ToothGrowth_sub)
               test_doses <- paste(dL,"-",dH)
               t_tests <- rbind(t_tests, list(test_doses=test_doses, p.value = t$p.value))
          }
     }
}
print(t_tests)
#
#create subsets of supp within dose and test for significance
t_tests <- NULL
supps <- c("OJ","VC")
for (dL in doses){
     for(dH in doses){
          if (dL < dH){
               ToothGrowth_sub <- subset(ToothGrowth, dose %in% c(dL,dH))
               for(s in supps){
                    ToothGrowth_sub_s <- subset(ToothGrowth_sub, supp %in% s)
                    t <- t.test(len ~ dose, data = ToothGrowth_sub_s)
                    test_doses <- paste(dL,"-",dH,s)
                    t_tests <- rbind(t_tests, list(test_doses=test_doses, p.value = t$p.value))
               }
          }
     }
}
print(t_tests)
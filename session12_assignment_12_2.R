#session12_assignment_12.2

#1. Use the given link Data Set. 
#Answer the below questions: 
yeast <- read.table('https://archive.ics.uci.edu/ml/machine-learning-databases/yeast/yeast.data', stringsAsFactors = FALSE) 
l <- readLines('https://archive.ics.uci.edu/ml/machine-learning-databases/yeast/yeast.names') 
l<-l[(grep('^7', l) + 1):(grep('^8', l) - 1)]
l <- l[grep('\\d\\..*:', l)] 

names(yeast) <- make.names(c(sub('.*\\d\\.\\s+(.*):.*', '\\1', l), 'class'))
View(yeast)


#a. What are the assumptions of ANOVA, test it out? 

#Assumptions of ANOVA:
  
#1. The data are Quantitative in nature and  are Normally Distributed.
plot(data,2) # the data is not normally distibuted, its a right skewed

# Extract the residuals
aov_residuals <- residuals(object = data)
# Run Shapiro-Wilk test
shapiro.test(x = aov_residuals ) # p valu is less than 0.5

 #output
#Shapiro-Wilk normality test

data:  aov_residuals
W = 0.7959, p-value < 2.2e-16

#2. samples are drawn from the population randomly and independently.

library(car)
Anova(data,type = "III") # two way anaova uing car package

#3.homogeneity of variance, variances of the population 
   # from which samples have been drawn are equal 
plot(data,1) # points 1288, 115 and 1080 are outliers



library(ggplot2)
ggplot(yeast, aes(x =yeast$class, y = yeast$nuc)) +
  geom_boxplot(fill = "grey80", colour = "blue") +
  scale_x_discrete() + xlab("class variable") +
  ylab("nuc variable")
 


data<-lm(nuc~class, data=yeast)
data<-lm(yeast$nuc~yeast$class, data=yeast)
summary(data)
t.test(yeast$nuc,yeast$yeastclass)
anova(data)
res.ano <- aov(nuc~class, data= yeast)
summary(res.ano)
 
#output

 #             Df Sum Sq Mean Sq F value Pr(>F)    
class          9  1.993 0.22141   22.01 <2e-16 ***
  Residuals   1474 14.825 0.01006                   
---
  Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
 
#b. Why ANOVA test? Is there any other way to answer the above question?
 
# to compare several population means at the same time.

kruskal.test(yeast$class, yeast$nuc) # when the data is not homoscadasticity , 
                                      # then we use non parametric test of Kruskal_Wallis test

# output
Kruskal-Wallis rank sum test

data:  yeast$class and yeast$nuc
Kruskal-Wallis chi-squared = 151.44, df = 67, p-value = 1.771e-08

# As an Alternative we can use Wilcox test 


pairwise.wilcox.test(yeast$nuc,yeast$class,p.adjust.method = "BH")

#output
Pairwise comparisons using Wilcoxon rank sum test 

data:  yeast$nuc and yeast$class 

CYT     ERL     EXC     ME1     ME2     ME3     MIT     NUC     POX    
ERL 0.96898 -       -       -       -       -       -       -       -      
  EXC 0.06093 0.20891 -       -       -       -       -       -       -      
  ME1 0.79331 0.96898 0.04065 -       -       -       -       -       -      
  ME2 0.63477 0.79331 0.15500 0.48277 -       -       -       -       -      
  ME3 0.16785 0.84965 0.00208 0.74838 0.15500 -       -       -       -      
  MIT 0.10389 0.74838 0.17667 0.21914 0.81416 0.00105 -       -       -      
  NUC < 2e-16 0.19820 1.6e-08 0.00081 4.2e-07 1.1e-08 < 2e-16 -       -      
  POX 0.25218 0.45796 0.81416 0.19513 0.44478 0.06152 0.48277 0.00013 -      
  VAC 0.89519 0.96898 0.03971 0.96898 0.52306 0.66483 0.31255 0.00091 0.19075

P value adjustment method: BH 





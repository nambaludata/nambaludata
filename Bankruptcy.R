#part 1-performing a two-sample t-test

#calling in the data
Bankruptcy <- read.csv("C:/Users/sign/Documents/Bankruptcy.csv")
head(Bankruptcy)

#load ggplot2 
library(ggplot2)

#we now formulate the null and alternative hypotheses
 ##null: there is no mean difference in employee growth rate
 #for healthy and distressed firms
 ##alternative: there is a mean difference in employee growth rate for
 ##healthy and distressed firms

Bankruptcy$yd <- as.factor(Bankruptcy$yd)
 ##we converted the yd variable to class factor since we need in factor form

#test statistic for variable gempl and yd
t.test(gempl ~ yd, data = Bankruptcy, var.equal = TRUE)
 ## our t = 4.2995 
 ##p value = 2.808e-05
 ##our conclusion is that since our p value is less than 0.05, we reject the null 
 ##hypothesis that there is no difference in means in employee growth rate
 ##for healthy and distressed firms
 ##based on the results we can see that the mean employee growth rate is 0.03
 ##way much higher than the mean employee growth rate for distressed firms which
 ## is -0.035


#test statistic for variable operating income to assets and yd

#null hypothesis: no difference in means of operating income to assets for healthy
 ##and distressed firms
#alternative hypothesis: there is a difference in means of operating income to 
 ##assets for healthy and distressed firms
t.test(opita ~ yd, data = Bankruptcy, var.equal = TRUE)

 #t = 6.2664
 #p value = 2.679e-09
 #based on the results, our p value is less than 0.05 and therefore we reject
 ##our null hypothesis that there is no difference in means of operating income 
 ##to assets for healthy and distressed firms
 #we can also conclude that healthy firms have a higher mean income to assets,
 ##0.158 as compared to distressed firms with a mean operating income 
 ###to assets of 0.056


#part 2- logit analysis
logistic <- glm(yd ~ tdta + gempl + opita + invsls + lsls, family = "binomial", data = Bankruptcy)
summary(logistic)

#by looking at the output, the intercept is negative with a value of -1.5087, this explains that
 ##that the log odds of bankruptcy are negative when the rest of the predictor variables are 
 ##held at zero

#the positive coefficient for debt to assets, 4.8133 indicates that the log odds of bankruptcy 
 ##when the debt ratio increases. A one unit increase in debt ratio is associated with 
 ###4.81 increase in the log odds of bankruptcy when other predictor variables are constant

#the coefficient for employee growth rate is negative, -6.1228 implying that the log odds
 ##of bankruptcy decrease when the employee growth rate increases. we can conclude that 
 ###a one unit increase in employee growth rate is associated with a -6.12 decrease in log odds
 ###of bankruptcy

#looking at the coefficient for operating income to assets, -5.1431, we can conclude that
 ##the log odds ratio of bankruptcy decrease with an increase in operating income to assets
 ###this means that one unit increase in "opita" is associated with a -5.14 decrease in log
 ###odds ratio of bankruptcy

#inventory to sales has a positive coefficient, 3.6199 indicating that there is an increase 
 ##in log odds ratio for bankruptcy as inventory to sales increases holding other predictor 
 ###variables constant. a one unit increase in inventory of sales is associated with 3.62 
 ###increase in log odds ratio of bankruptcy

#log of sales has a negative coefficient -0.2151, this explains that there is a decrease in 
 ##log odds of bankruptcy as log of sales increases. one unit of log of sales is associated
 ###with a -0.215 decrease in log odds of bankruptcy 



#Take Home Project 1
#Summary Statistics:
drugArows <- Drug.1[Drug.1[,3] == "A",]
drugArelief <- drugArows$Relief
drugBrows <- Drug.1[Drug.1[,3] == "B",]
drugBrelief <- drugBrows$Relief

drugAmean = mean(drugArelief)
drugBmean = mean(drugBrelief)
drugAmean
drugBmean

drugAsd = sd(drugArelief)
drugBsd = sd(drugBrelief)
drugAsd
drugBsd

fivenumA = fivenum(drugArelief)
fivenumB = fivenum(drugBrelief)
fivenumA
fivenumB

Asamplesize = nrow(drugArows)
Bsamplesize = nrow(drugBrows)
Asamplesize
Bsamplesize

hist(drugArelief, breaks=28, xlab="Number of hours of relief", ylab="Number of Subjects", main="Histogram of Random Sample of the Pain Relief (No.hours) from Drug A (n=12)")
hist(drugBrelief, breaks=28, xlab="Number of hours of relief", ylab="Number of Subjects", main="Histogram of Random Sample of the Pain Relief (No.hours) from Drug B (n=12)")

boxplot(drugArelief, ylab="Number of Hours of Relief", main="Boxplot of Random Sample of the Number of Hours of Pain Relief from Drug A (n=12)")
boxplot(drugBrelief, ylab="Number of Hours of Relief", main="Boxplot of Random Sample of the Number of Hours of Pain Relief from Drug B (n=12)")

#Code 2
numbers = Drug.1$Relief
groups = Drug.1$Drug
group = as.factor(groups)
dataframe = data.frame(numbers,groups)

library(coin)

all.perms = sapply(1:3000,function(i){
  the.numbers = Drug.1$Relief
  the.groups = as.factor(Drug.1$Drug)
  change.groups = sample(the.groups,length(the.groups),replace = FALSE) # shuffles groups
  group.1.med =  median(the.numbers[change.groups == levels(the.groups)[1]]) # finds median for group 1
  group.2.med = median(the.numbers[change.groups == levels(the.groups)[2]]) # finds median for group 2
  difference.in.meds= group.1.med-group.2.med #finds difference in means
  return(difference.in.meds)
})
difference = 1.70 #finds difference in median
p.value.two = mean(abs(all.perms) >= abs(difference)) #calculates two-sided p-value
p.value.two

#Code 3
p=0.009
R=3000
Z_0.95 = qnorm(0.95,mean=0,sd=1,lower.tail = TRUE)
Z_0.975 = qnorm(0.975,mean=0,sd=1,lower.tail = TRUE)
Z_0.995 = qnorm(0.995,mean=0,sd=1,lower.tail = TRUE)
Z_0.95
Z_0.975
Z_0.995
#90% CI for p-value
a = p - ((Z_0.95) * sqrt(p*(1-p)/R))
b = p + ((Z_0.95) * sqrt(p*(1-p)/R))
a
b
#95% CI for p-value
a = p - ((Z_0.975) * sqrt(p*(1-p)/R))
b = p + ((Z_0.975) * sqrt(p*(1-p)/R))
a
b
#99% CI for p-value
a = p - ((Z_0.995) * sqrt(p*(1-p)/R))
b = p + ((Z_0.995) * sqrt(p*(1-p)/R))
a
b


#Code 4
#Wilcoxon Sum Rank Test:
sortedDrug = sort(Drug.1$Relief)
sortedDrug
rankedDrug = rank(sortedDrug)
rankedDrug
W_obs = 16 + 18 + 9 + 1 + 19 + 14 + 23 + 21 + 15 + 22 + 24 + 12.5
W_obs

numbers= Drug.1$Relief
groups = Drug.1$Drug
trip = data.frame(numbers,groups)
group = as.factor(groups)
library(coin)
wilcox_test(numbers ~ group,data=trip,distribution = "exact",alternative = "two.sided")


#Kolmogorov Test:

split.up = split(Drug.1$Relief,Drug.1$Drug)
Group1 = split.up[[1]]
Group2 = split.up[[2]]

test1 = ks.test(Group1,Group2,alternative = "two.sided",exact = FALSE)
test1$statistic
test1$p.value

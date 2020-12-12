library(dplyr)
library(ggplot2)
pdata = read.csv('pdata.csv')
#Creating pharm indicator variable and removing NA's
pdata = filter(pdata, !is.na(pdata$gsubind))
pdata = mutate(pdata, pharm = case_when(pdata$gsubind == 35202010 ~ 1, pdata$gsubind != 35202010 ~ 0))
pdata = filter(pdata, !is.na(pdata$gp))
pdata = filter(pdata, !is.na(pdata$xrd))
pdata = filter(pdata, pdata$fyear != 2009)
#plotting profit by industry
boxplot(pdata$gp, pdata$gp[pdata$pharm==1], pdata$gp[pdata$gsector==10], pdata$gp[pdata$gsector==25], pdata$gp[pdata$gsector==40], pdata$gp[pdata$gsector==45],
        names = c('S&P 500', 'Pharma', 'Energy', 'Consumer Goods', 'Finance', 'IT'), main="Profit by Industry",
        xlab="Industry",
        ylab="Profit in Thousands of Dollars")
RD_Expense = c(mean(pdata$xrd), mean(pdata$revt), mean(pdata$xrd[pdata$pharm==1]),mean(pdata$revt[pdata$pharm==1]),mean(pdata$xrd[pdata$gsector==10]), 
         mean(pdata$revt[pdata$gsector==10]),mean(pdata$xrd[pdata$gsector==25]),mean(pdata$revt[pdata$gsector==25]),mean(pdata$xrd[pdata$gsector==40]), 
    mean(pdata$revt[pdata$gsector==40]),mean(pdata$xrd[pdata$gsector==45]), mean(pdata$revt[pdata$gsector==45]))
Industry = c('S&P 500','S&P 500','Pharmaceuticals','Pharmaceuticals',  'Energy', 'Energy','Consumer Goods','Consumer Goods','Finance','Finance','IT','IT')
j = list('R&D Expense', 'Revenue')
Legend = c('R&D Expense', 'Revenue','R&D Expense', 'Revenue','R&D Expense', 'Revenue','R&D Expense', 'Revenue','R&D Expense', 'Revenue','R&D Expense', 'Revenue')
pairb = as.pdataa.frame(cbind(RD_Expense, Legend,Industry))
pairb$RD_Expense = as.numeric(pairb$RD_Expense)
ggplot(pairb, aes(fill=Legend, y=RD_Expense, x=Industry)) + 
  geom_bar(position="dodge", stat="identity") + labs(title ="R&D Expense and Revenue by Industry", y = "Expense/Revenue (In Billions USD)")
jpharm = lm(gp~pharm,pdata)
full = lm(gp~pharm + xrd + mkvalt + factor(fyear), pdata)
noyear = lm(gp~pharm + xrd + mkvalt, pdata)
ints = pdataa.frame(Estimate = NA, Lower = NA, Upper = NA)
#creating matrix of coefficient results by year
for (i in 2010:2018){
  lm = lm(gp~pharm + xrd + mkvalt, pdataa = pdata[pdata$fyear==i,])
  confidence = confint(lm)
  ints[(i-2009),1] = lm$coefficients[2]
  ints[(i-2009),2] = confidence[2,1]
  ints[(i-2009),3] = confidence[2,2]
}
Years = 2010:2018
conyears = cbind(ints,Years)
require(plotrix)
plotCI(conyears$Years, conyears$Estimate, ui=conyears$Upper, li=conyears$Lower, main = 'Plot Differential by Year', xlab = 'Year', ylab='Average Pharmaceutical Profit - Average S&P 500 Profit')

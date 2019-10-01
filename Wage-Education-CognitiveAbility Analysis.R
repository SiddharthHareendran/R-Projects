library(DBI)
library(RSQLite)
library(data.table)
library(ggplot2)
wpull <- function(tablename){
  con <- DBI::dbConnect(RSQLite::SQLite(),'wooldridge2.db')
  dt <- DBI::dbReadTable(con,tablename)
  dt <- data.table(dt)
  print(DBI::dbReadTable(con,paste(tablename,'labels',sep='_')))
  DBI::dbDisconnect(con)
  rm(con)
  return(dt)
}
htv <- wpull('htv')
range <- max(htv$educ) - min(htv$educ)
range
count<- htv[which(educ==12),]
total<-1230
percentage<-(nrow(count)/total)*100
percentage
mean(htv$educ)
Average_parents <- (mean(htv$motheduc)+mean(htv$fatheduc))/2
Average_parents
summary(htv)
#range value: 14 
#Percentage of men completed 12th grade but no higher: 41.62%
#Average parents education: 12.31 Average education of men:13.03
#The men have higher education than their parents on an average. Also the men's average education is not low since the median education is 12. And the mean lies between second and third quartile. Hence neither is that low.
model1<- lm(educ~motheduc+fatheduc, data=htv)
summary(model1)
# Equation form: educ = 6.96 + 0.304(motheduc) + 0.1902(fatheduc)
# R squared: 0.2493. The education is dependent on parents education by 24.93%
# motheduc coefficient:0.304. With each unit increase in mother's education there is an increase of 0.304 in education of men. 
model2<- lm(educ~motheduc+fatheduc+abil, data=htv)
summary(model2)
#Equation form: educ= 8.448 + 0.18913(motheduc) + 0.11109(fatheduc) + 0.50248(abil)
# Coefficient of abil: 0.502 R sqaured: 0.4275
#Yes cognitive ability has a large effect on level of education. With each unit increase in cognitive ability there is an increase of 0.502 in education, all else is constant. 
#As well as When cognitive ability is added, model improves depicted by higher R squared.
model3<- lm(educ~motheduc+fatheduc+abil+I(abil^2), data=htv)
summary(model3)
library(Deriv)
value<-D(expression((0.190126*motheduc)+(0.108939*fatheduc)+(0.401462*abil)+(0.050599*abil^2)),"abil")
value
abilmin <- Re(polyroot(c(0.401462,0.050599 *2)))
abilmin
# minimum ability is:-3.967
#applying abil minimum in model3 when all else 0
mineduc<- 8.24+(0.190126*0)+(0.108939*0)+(0.401462*abilmin)+(0.050599*abilmin^2)
mineduc
D(value,"abil")
#second deravite value:0.05059*2. Hence we have minimum
htv[which(abil<(-3.967))]
#Yes only 15 out of 1230 men have cognitive ability less than the above obtained value
model<- lm(educ~motheduc+fatheduc+abil, data=htv)
#Equation under consideration: 8.448 + 0.18913(motheduc) + 0.11109(fatheduc) + 0.50248(abil)
##Average motheduc:12.18 Average fatheduc:12.45
model_value<- 8.448 + (0.18913*12.18) + (0.11109*12.45) + (0.50249*htv$abil)
plotting<- ggplot(htv,aes(x=abil,y=educ)) + geom_point() + scale_x_continuous(name="Ability") + scale_y_continuous(name="Education(Years)")
plotting
plotting_regressionline <- plotting + geom_line(aes(y=model_value),color="red",size=2)
plotting_regressionline


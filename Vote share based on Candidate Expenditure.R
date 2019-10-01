wpull <- function(tablename){
  con <- DBI::dbConnect(RSQLite::SQLite(),'wooldridge2.db')
  dt <- DBI::dbReadTable(con,tablename)
  dt <- data.table(dt)
  print(DBI::dbReadTable(con,paste(tablename,'labels',sep='_')))
  DBI::dbDisconnect(con)
  rm(con)
  return(dt)
}
vote <- wpull('vote1')
#Q1.1
model1 <- lm(voteA~log(expendA)+log(expendB)+prtystrA,data=vote)
model1
#B1 = 6.081. With 1 unit increase of expenditure by A there will be 6.081percent vote increase for A, provided all else constant
#Q1.2
#B1=-B2; considering theta1 = B1+B2
#Rearranging the terms in equation we get: voteA = B0+theta1[log(expendA)]+B2[-log(expendA)+log(expendB)]+B3prtystr
hypmodel<-lm(voteA~log(expendA)+I(-log(expendA)+log(expendB))+prtystrA,data=vote)
summary(hypmodel)
#Here we have log(expendA) coefficient as -0.53 with a tstat of -1.002 and a large p value of 0.31. Hence we fail to reject null hypothesis, true. With 1% increase in expenditure A it is offset by a 1% increase in expenditure B
#Q1.3
model1 <- lm(voteA~log(expendA)+log(expendB)+prtystrA,data=vote)
summary(model1)
#Equation: voteA = 45.08 + 6.08log(expendA) - 6.61log(expendB) +0.15(prtystrA)
#Yes expenditure by A does affect outcome. With every 1 unit increase of expenditure by A there is a 6.08 percent increase of votes for A
#Yes expenditure by B does have a negative affect on outcome. With every 1 unit increase of expenditure by B there is a 6.61 percent decrease in votes for A
#Yes we can since coefficient of A and B are having opposite effects on outcome. Hence offsetting
#Q1.4
hypmodel<-lm(voteA~log(expendA)+I(-log(expendA)+log(expendB))+prtystrA,data=vote)
summary(hypmodel)
#Q1.5
#We can conclude there with 1 unit increase in expenditute by a candidate will result in a 6%(approx) vote share increase for that candidate, all else constant. 
#We could also conclude that with 1% increase of expenditure by A is offset by 1% increase in expenditure by B
library(DBI)
library(RSQLite)
con <- dbConnect(RSQLite::SQLite(),'wooldridge2.db')
wpull <- function(tablename){
  con <- DBI::dbConnect(RSQLite::SQLite(),'wooldridge2.db')
  dt <- DBI::dbReadTable(con,tablename)
  dt <- data.table(dt)
  print(DBI::dbReadTable(con,paste(tablename,'labels',sep='_')))
  DBI::dbDisconnect(con)
  rm(con)
  return(dt)
}
ceosal2<-wpull('ceosal2')
mean(ceosal2$salary)
mean(ceosal2$ceoten)
#Average tenure:7.95 Average salary:865.86K dollars
table(ceosal2$ceoten)
max(ceosal2$ceoten)
#5 ceos are in their first year as CEO ceoten =0 
#Maximum years as ceo is 2
model <- lm(log(salary)~ceoten, data = ceosal2)
summary(model)
# The equation is of form: Log(salary) = 6.505 + (0.0097)ceoten
#B0=6.505 B1=0.009
#For the predicted model, for every unit increase or with 1 year increase in tenure, there is a percent increase of salary by 0.97%

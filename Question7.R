wpull <- function(tablename){
  con <- DBI::dbConnect(RSQLite::SQLite(),'wooldridge2.db')
  dt <- DBI::dbReadTable(con,tablename)
  dt <- data.table(dt)
  print(DBI::dbReadTable(con,paste(tablename,'labels',sep='_')))
  DBI::dbDisconnect(con)
  rm(con)
  return(dt)
}
wage1<-wpull('wage1')
#Q7.1
model<-lm(log(wage)~educ+exper+I(exper^2),data = wage1)
summary(model)
#Usual OLS equation: log(wage)=0.1263+0.0906(educ)+0.0409(exper)-0.000712(exper^2)
#Q7.2
#Yes at 1% exper^2 is significant as |tstat|=6.141
#Q7.3
library(Deriv)
value<-D(expression((0.0906*educ)+(0.0409*exper)-(0.000712*exper^2)),"exper")
value
#value = 0.0409 - 0.000712*2*exper
# Experience =5
value1<-0.0409 - (0.000712*2*10)
value1
#value1 = 0.02666 ; experience = 5
value2<- 0.0409 - (0.000712*2*20)
value2
#value2 - 0.0124 ; experience  =20
#Q7.4
#equation under consideration: 0=0.0409 - (0.000712*2*exper)
experval<-0.0409/(0.000712*2)
experval
#experience at which expected wage starts decreasing is 28.72 ~ 29 years
wage1[which(wage1$exper>=29)]
#There are 121 individuals with equal to or more than 29 years of experience.
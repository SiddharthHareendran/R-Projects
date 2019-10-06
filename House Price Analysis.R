wpull <- function(tablename){
  con <- DBI::dbConnect(RSQLite::SQLite(),'wooldridge2.db')
  dt <- DBI::dbReadTable(con,tablename)
  dt <- data.table(dt)
  print(DBI::dbReadTable(con,paste(tablename,'labels',sep='_')))
  DBI::dbDisconnect(con)
  rm(con)
  return(dt)
}
kiel<-wpull('kielmc')
kiel81<-kiel[kiel$year==1981]
#Q6.1
model<-lm(log(price)~log(dist),data = kiel81)
summary(model)
#Equation in consideration: log(price)= 8.04716 + 0.36(dist)
#b1 = 0.36. With every feet increase from incinerator, the selling price increases by 0.36 units.
#sign of b1 has to be positive as natually farther away from incinerator demands better price.
#Q6.2
model<-lm(log(price)~log(dist)+log(intst)+log(area)+log(land)+rooms+baths+age,data = kiel81)
summary(model)
#The presence of incinerator is not having as much as an effect on selling price as model1.
#other factors are coming into contention. The selling price is also highly determined by how far the house is from interstate, number of rooms as well as land area.
#Q6.3
model<-lm(log(price)~log(dist)+log(intst)+log(area)+log(land)+rooms+baths+age+I(log(intst)^2),data = kiel81)
summary(model)
#There is a dramatic affect on selling price depending on the distance between house and interstate. The interstate factor has the most significate impact on selling price
#Q6.4
model<-lm(log(price)~log(dist)+log(intst)+log(area)+log(land)+rooms+baths+age+I(log(intst)^2)+I(log(dist)^2),data = kiel81)
summary(model)
#Not significant. The distance of house from incinerator doesnt have much affect on selling price anymore. The coefficient = -0.036 and the t value is |0.331|

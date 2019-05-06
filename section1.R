#Reading and cleaning the data set

collisions <- read.csv('NYPD_Motor_Vehicle_Collisions.csv', stringsAsFactors = FALSE) #R
print(collisions[1,])
collisions$DATE<- as.Date(collisions$DATE, "%m/%d/%Y")
collisions <-collisions[collisions$DATE <= "2018-12-31",]

#Q1
persons_injured<-sum(collisions$NUMBER.OF.PERSONS.INJURED, na.rm = TRUE)

#Q2
subset_2016_BOROUGHS<-collisions[collisions$DATE >= "2016-01-01" & collisions$DATE <= "2016-12-31" & collisions$BOROUGH != "",]
rate_collisions_brooklyn_2016<-sum(subset_2016_BOROUGHS$BOROUGH == "BROOKLYN")/length(subset_2016_BOROUGHS$BOROUGH)

#Q3
subset_2016_ALL<-collisions[collisions$DATE >= "2016-01-01" & collisions$DATE <= "2016-12-31",]
cyclist_injury_death_rate<-sum(subset_2016_ALL$NUMBER.OF.CYCLIST.INJURED > 0 | subset_2016_ALL$NUMBER.OF.CYCLIST.KILLED >0)/length(subset_2016_ALL$NUMBER.OF.PERSONS.INJURED)

#Q4
subset_2017<-collisions[collisions$DATE >= "2017-01-01" & collisions$DATE <= "2017-12-31",]
alcohol_brooklyn<-sum(subset_2017$BOROUGH == "BROOKLYN" 
                      & (
                        subset_2017$CONTRIBUTING.FACTOR.VEHICLE.1 == "Alcohol Involvement"|
                        subset_2017$CONTRIBUTING.FACTOR.VEHICLE.2 == "Alcohol Involvement"|
                        subset_2017$CONTRIBUTING.FACTOR.VEHICLE.3 == "Alcohol Involvement"|
                        subset_2017$CONTRIBUTING.FACTOR.VEHICLE.4 == "Alcohol Involvement"|
                        subset_2017$CONTRIBUTING.FACTOR.VEHICLE.5 == "Alcohol Involvement"
                        ))
brooklyn_population<-2648771
rate_brooklyn_alcohol<-alcohol_brooklyn/brooklyn_population

alcohol_bronx<-sum(subset_2017$BOROUGH == "BRONX" 
                      & (
                        subset_2017$CONTRIBUTING.FACTOR.VEHICLE.1 == "Alcohol Involvement"|
                          subset_2017$CONTRIBUTING.FACTOR.VEHICLE.2 == "Alcohol Involvement"|
                          subset_2017$CONTRIBUTING.FACTOR.VEHICLE.3 == "Alcohol Involvement"|
                          subset_2017$CONTRIBUTING.FACTOR.VEHICLE.4 == "Alcohol Involvement"|
                          subset_2017$CONTRIBUTING.FACTOR.VEHICLE.5 == "Alcohol Involvement"
                      ))
bronx_population<-1471160
rate_bronx_alcohol<-alcohol_bronx/bronx_population

alcohol_manhattan<-sum(subset_2017$BOROUGH == "MANHATTAN" 
                   & (
                     subset_2017$CONTRIBUTING.FACTOR.VEHICLE.1 == "Alcohol Involvement"|
                       subset_2017$CONTRIBUTING.FACTOR.VEHICLE.2 == "Alcohol Involvement"|
                       subset_2017$CONTRIBUTING.FACTOR.VEHICLE.3 == "Alcohol Involvement"|
                       subset_2017$CONTRIBUTING.FACTOR.VEHICLE.4 == "Alcohol Involvement"|
                       subset_2017$CONTRIBUTING.FACTOR.VEHICLE.5 == "Alcohol Involvement"
                   ))
manhattan_population<-1664727
rate_manhattan_alcohol<-alcohol_manhattan/manhattan_population

alcohol_queens<-sum(subset_2017$BOROUGH == "QUEENS" 
                       & (
                         subset_2017$CONTRIBUTING.FACTOR.VEHICLE.1 == "Alcohol Involvement"|
                           subset_2017$CONTRIBUTING.FACTOR.VEHICLE.2 == "Alcohol Involvement"|
                           subset_2017$CONTRIBUTING.FACTOR.VEHICLE.3 == "Alcohol Involvement"|
                           subset_2017$CONTRIBUTING.FACTOR.VEHICLE.4 == "Alcohol Involvement"|
                           subset_2017$CONTRIBUTING.FACTOR.VEHICLE.5 == "Alcohol Involvement"
                       ))
queens_population<-2358582
rate_queens_alcohol<-alcohol_queens/queens_population

alcohol_sisland<-sum(subset_2017$BOROUGH == "STATEN ISLAND" 
                    & (
                      subset_2017$CONTRIBUTING.FACTOR.VEHICLE.1 == "Alcohol Involvement"|
                        subset_2017$CONTRIBUTING.FACTOR.VEHICLE.2 == "Alcohol Involvement"|
                        subset_2017$CONTRIBUTING.FACTOR.VEHICLE.3 == "Alcohol Involvement"|
                        subset_2017$CONTRIBUTING.FACTOR.VEHICLE.4 == "Alcohol Involvement"|
                        subset_2017$CONTRIBUTING.FACTOR.VEHICLE.5 == "Alcohol Involvement"
                    ))
sisland_population<-479458
rate_sisland_alcohol<-alcohol_sisland/sisland_population

#Q5
subset_2016_ALL$NUMBER.OF.VEHICLES.INVOLVED <- as.integer(subset_2016_ALL$CONTRIBUTING.FACTOR.VEHICLE.1 != "") + as.integer(subset_2016_ALL$CONTRIBUTING.FACTOR.VEHICLE.2 != "") +as.integer(subset_2016_ALL$CONTRIBUTING.FACTOR.VEHICLE.3 != "") +as.integer(subset_2016_ALL$CONTRIBUTING.FACTOR.VEHICLE.4 != "") +as.integer(subset_2016_ALL$CONTRIBUTING.FACTOR.VEHICLE.5 != "") 
groupzip<- aggregate(NUMBER.OF.VEHICLES.INVOLVED ~ ZIP.CODE, subset_2016_ALL, sum)
groupzip[rev(order(groupzip$NUMBER.OF.VEHICLES.INVOLVED)),]

#Q6
dateSplit<-split(collisions$DATE, format(as.Date(collisions$DATE), "%Y"))
x<- c(2013, 2014,2015,2016,2017, 2018)
y<- c(length(dateSplit$`2013`),length(dateSplit$`2014`),length(dateSplit$`2015`),length(dateSplit$`2016`),length(dateSplit$`2017`),length(dateSplit$`2018`))
lm(y ~ x)

#Q7
subset_2017$NUMBER.OF.VEHICLES.INVOLVED <- as.integer(subset_2017$CONTRIBUTING.FACTOR.VEHICLE.1 != "") + as.integer(subset_2017$CONTRIBUTING.FACTOR.VEHICLE.2 != "") +as.integer(subset_2017$CONTRIBUTING.FACTOR.VEHICLE.3 != "") +as.integer(subset_2017$CONTRIBUTING.FACTOR.VEHICLE.4 != "") +as.integer(subset_2017$CONTRIBUTING.FACTOR.VEHICLE.5 != "") 
subset_2017$MULTICOLISIONAL <- as.integer(subset_2017$NUMBER.OF.VEHICLES.INVOLVED >= 3)
monthSplit<-split(subset_2017, format(as.Date(subset_2017$DATE), "%m"))
monthSplit[1]
length(monthSplit)
monthColli<- rep(0,12)
mat<- matrix(0,nrow=2,ncol=12)
i<-1
for (month in monthSplit) {
  monthColli[i]<-sum(month$MULTICOLISIONAL)/length(month$DATE)
  mat[1,i]<- sum(month$MULTICOLISIONAL)
  mat[2,i] <- length(month$DATE)-sum(month$MULTICOLISIONAL)
  i<-i+1
}
x<- matrix(c(mat[1,1], mat[2,1], mat[1,5],mat[2,5]), nrow =2)
chisq <- chisq.test(mat)
chisq

#Q8



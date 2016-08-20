#ROSSMANN SALES DATASET : Data Cleansing 

#Installing required packages for cleaning
install.packages("lubridate")
install.packages("stringi")

#Train dataset as input file
rossmann_train<-read.csv(file.choose(), header=TRUE)

#Store dataset as input
store_train<-read.csv(file.choose(), header=TRUE)

#Merging Train dataset with the Store dataset so that it has all the columns combined
merged_data<-merge(rossmann_train,store_train,by='Store',all.x=T)

#write merged data to csv file
write.csv(merged_data,"merged_data_train_updated.csv",row.names=FALSE)

#-------Removing Outliers--------------------------------------------------
#Removing rows where sales=0 and Open=1
order_data<-merged_data[!(merged_data$Open==1 & merged_data$Sales==0),]

#Remove the rows where sales<0 when customers visit the store
order_data<-merged_data[(merged_data$Sales>0),]

#-------------------------------------------------------------------------

#Separated Date column into year, month and day for prediction variables
library(lubridate)
data_year<-year(order_data$Date)
data_month<-month(order_data$Date)
data_day<-day(order_data$Date)
data_week<-week(order_data$Date)

#Combining the 3 different columns - month, day and year with the order_data data frame
combined_data<-cbind(order_data[1:3], data_year, data_month, data_day, data_week, order_data[4:18])


#Changing number of days in a week with the corresponding character names - sales on Monday, Saturday, Sunday and rest of the weekdays are different
combined_data$DayOfWeek<-as.numeric(combined_data$DayOfWeek)
combined_data$DayOfWeek[combined_data$DayOfWeek==1]<-"Mon"
combined_data$DayOfWeek[combined_data$DayOfWeek==2 | combined_data$DayOfWeek==3 | combined_data$DayOfWeek==4 |combined_data$DayOfWeek==5 ]<-"Weekday"
combined_data$DayOfWeek[combined_data$DayOfWeek==6]<-"Sat"
combined_data$DayOfWeek[combined_data$DayOfWeek==7]<-"Sun"
combined_data$DayOfWeek<-as.factor(combined_data$DayOfWeek)

#Added a new column based on values in CompetitionOpenSinceMonth and CompetitionOpenSinceYear - FEATURE ENGINEERING
combined_data$HaveCompetitor<-ifelse((is.na(combined_data$CompetitionOpenSinceMonth) & is.na(combined_data$CompetitionOpenSinceYear)& is.na(combined_data$CompetitionDistance)), 0, 1)

#Boxplots
boxplot(combined_data$Sales~combined_data$StateHoliday,data=combined_data)
boxplot(combined_data$Sales~combined_data$DayOfWeek,data=combined_data)


#Merging b and c columns of StateHoliday because they have similar sales value as compared to others, a = public holiday, b = Easter holiday, c = Christmas, 0 = None
combined_data$StateHoliday<-as.character(combined_data$StateHoliday)
combined_data$StateHoliday[combined_data$StateHoliday=="b"|combined_data$StateHoliday=="c"]<-"d"
combined_data$StateHoliday<-as.factor(combined_data$StateHoliday)

#CompetitionDistance - replacing NAs with 100000 where HaveCompetitor = 0
combined_data$CompetitionDistance[(combined_data$HaveCompetitor==0)]<-100000

#Combine Promo2, Promo2SinceWeek, Promo2SinceYear and Promointerval to a promotion 2 indicator in historical sales data. The indicator indicates on a certain day whether a certain store is on promotion 2
combined_data$Promo2Indicator<-ifelse(((combined_data$Promo2==0) & (is.na(combined_data$Promo2SinceYear)) & (is.na(combined_data$Promo2SinceWeek)) 
                                          & (combined_data$PromoInterval=="")), 0, 1)

#FEATURE ENGINEERING - Adding a Promoer2Month column 
#PromoInterval records the first month of each email marketing, replaced it with Promo2Month, which has number of months when the store conducted the most recent email promotion 
library(stringi)

# Extracting each month name from PromoIntervalcolumn and treat them as promointerval quarters
#Fourth Quarter
w<-as.character(combined_data$PromoInterval)
promo_month<-function(w,n){substr(w,nchar(w)-n+1,nchar(w))}
combined_data$QuarterFour<-promo_month(w,3)

#Third Quarter
x<-as.character(combined_data$PromoInterval)
promo_month3<-function(x,n){substr(x,nchar(x)-n+1,nchar(x))}
combined_data$QuarterThree<-promo_month3(x,7)
combined_data$QuarterThree<-stri_sub(combined_data$QuarterThree,1,3)

#Second Quarter
y<-as.character(combined_data$PromoInterval)
promo_month2<-function(y,n){substr(y,nchar(y)-n+1,nchar(y))}
combined_data$QuarterTwo<-promo_month2(y,11)
combined_data$QuarterTwo<-stri_sub(combined_data$QuarterTwo,1,3)

#First Quarter
z<-as.character(combined_data$PromoInterval)
promo_month1<-function(z,n){substr(z,nchar(z)-n+1,nchar(z))}
combined_data$QuarterOne<-promo_month1(z,15)
combined_data$QuarterOne<-stri_sub(combined_data$QuarterOne,1,3)
 
#Changing the month names in corresponding columns to numeric values as per the months in a year
combined_data$QuarterFour[which(combined_data$QuarterFour=="Oct")]<-"10"
combined_data$QuarterFour[which(combined_data$QuarterFour=="Nov")]<-"11"
combined_data$QuarterFour[which(combined_data$QuarterFour=="Dec")]<-"12"
combined_data$QuarterThree[which(combined_data$QuarterThree=="Jul")]<-"7"
combined_data$QuarterThree[which(combined_data$QuarterThree=="Aug")]<-"8"
combined_data$QuarterThree[which(combined_data$QuarterThree=="Sep")]<-"9"
combined_data$QuarterTwo[which(combined_data$QuarterTwo=="Apr")]<-"4"
combined_data$QuarterTwo[which(combined_data$QuarterTwo=="May")]<-"5"
combined_data$QuarterTwo[which(combined_data$QuarterTwo=="Jun")]<-"6"
combined_data$QuarterOne[which(combined_data$QuarterOne=="Jan")]<-"1"
combined_data$QuarterOne[which(combined_data$QuarterOne=="Feb")]<-"2"
combined_data$QuarterOne[which(combined_data$QuarterOne=="Mar")]<-"3"

#Converting the four quarter columns to numeric value columns
combined_data$QuarterFour<-as.numeric(combined_data$QuarterFour)
combined_data$QuarterThree<-as.numeric(combined_data$QuarterThree)
combined_data$QuarterTwo<-as.numeric(combined_data$QuarterTwo)
combined_data$QuarterOne<-as.numeric(combined_data$QuarterOne)

#Calculating the number of months by applying the following formula to data_year, data_month, Promo2SinceYear and QuarterFour columns
combined_data$Promo2SinceYear<-as.numeric(combined_data$Promo2SinceYear)
b<-(12 * (combined_data$data_year - combined_data$Promo2SinceYear)) + (combined_data$data_month - combined_data$QuarterFour)
c<-(12 * (combined_data$data_year - combined_data$Promo2SinceYear)) + (combined_data$data_month - combined_data$QuarterThree)
d<-(12 * (combined_data$data_year - combined_data$Promo2SinceYear)) + (combined_data$data_month - combined_data$QuarterTwo)
e<-(12 * (combined_data$data_year - combined_data$Promo2SinceYear)) + (combined_data$data_month - combined_data$QuarterOne)

#Running the for-loop for taking out the positive number of months for the duration when the store performed most recent promotion
#Taking out negative values (starting promotion before shop opened)
combined_data$PromoMonth<-ifelse(combined_data$Promo2SinceYear>combined_data$data_year,0,
       ifelse(combined_data$QuarterFour<=combined_data$data_month, b,
              ifelse(combined_data$QuarterThree<=combined_data$data_month, c,
                     ifelse(combined_data$QuarterTwo<=combined_data$data_month, d,
                            ifelse(combined_data$QuarterOne<=combined_data$data_month, e,
                                   0)))))

#Replacing the NAs with zero in the PromoMonth Column
combined_data$PromoMonth[is.na(combined_data$PromoMonth)]<-0

#Taking the mean of Sales
mean<-mean(combined_data$Sales)

#Creating new column Expected_Sales
combined_data$Expected_Sales<-ifelse(combined_data$Sales<mean,0,1)
combined_data$Expected_Sales<-factor(combined_data$Expected_Sales, levels=c(0,1),labels=c("Below Average", "Above Average"))

#Building the final dataset by combining all the required columns
rossmann_cleanedfile<-cbind.data.frame(Store=combined_data$Store, DayOfWeek = combined_data$DayOfWeek, Day = combined_data$data_day, 
                      Month = combined_data$data_month, Year = combined_data$data_year, Week = combined_data$data_week, 
                      Customers = combined_data$Customers, Open = combined_data$Open, Promo = combined_data$Promo, 
                      PromoMonth = combined_data$PromoMonth, StateHoliday = combined_data$StateHoliday, 
                      SchoolHoliday = combined_data$SchoolHoliday, StoreType = combined_data$StoreType, 
                      Assortment = combined_data$Assortment, CompetitionDistance = combined_data$CompetitionDistance, 
                      Promo2Indicator = combined_data$Promo2Indicator, HaveCompetitor = combined_data$HaveCompetitor, 
                      Date=as.Date(combined_data$Date), Sales = combined_data$Sales, ExpectedSales = combined_data$Expected_Sales)



#Writing the Rossmann_CleanedFile.csv
write.csv(rossmann_cleanedfile, "cleanedData_RossmannSales.csv", row.names = FALSE)

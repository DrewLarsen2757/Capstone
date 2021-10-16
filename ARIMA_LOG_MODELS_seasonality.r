library(tidyverse)
library(tswge)


#Read in file
df <-read.csv(file="C:/users/bodie/Documents/CAPSTONE_DATA.csv")
#Change Date format
df$DAY <- as.Date(df$DAY,format="%m/%d/%Y")

#Subset dataframe
Load_df <- df %>% select('DAY','COAST_LOAD','EAST_LOAD','FAR_WEST_LOAD','NORTH_LOAD',
                         'NORTH_CENTRAL_LOAD','SOUTHERN_LOAD','SOUTH_CENTRAL_LOAD','WEST_LOAD')


##Creating List for compare later on

date_Range <- vector(mode = "list")
Load_Zoad <-vector(mode="list")
ASE_Score <- vector(mode = "list")


##Train set

Train_df <- subset(Load_df,DAY <= "2020-12-31" )

Actual_values <-subset(Load_df,DAY >"2020-12-31" & DAY <"2021-01-08")

###COAST

COAST_df <- Train_df %>% select('DAY','COAST_LOAD')

COAST_df$LOG_COAST_LOAD <- log(COAST_df$COAST_LOAD)

###Difference the data
###We are differencing here to fit model for P & Q. 

##Remove seasonality as well.
difference_data = artrans.wge(COAST_df$LOG_COAST_LOAD,phi.tr =1)
difference_data = artrans.wge(difference_data,phi.tr =c(rep(0,364),1))




aicselection <-aic5.wge(difference_data,p=0:5,q=0:5,type='bic')


###Note here we are loading the difference data into it in order to get the equation
COAST_Load_Model_First_Jan_Seven_Days = est.arma.wge(difference_data
                                                     ,p=aicselection[,1][1],q=aicselection[,2][1])



## Un-diff log data is loaded into forecast then it is d by d=1
f = fore.aruma.wge(COAST_df$LOG_COAST_LOAD,phi = COAST_Load_Model_First_Jan_Seven_Days$phi, 
                   theta = COAST_Load_Model_First_Jan_Seven_Days$theta, d=1,
                   n.ahead = 7,limits = T, lastn = T,s=c(rep(0,364),1))

##Unlog forecast
inverse_forecast <- expm1(f$f)

ase_unlogged <- mean((Actual_values$COAST_LOAD-inverse_forecast)^2)
ase_unlogged

#Append List
date_Range <- c(date_Range,"01/01/21-01/07/21")
Load_Zoad <-c(Load_Zoad,"COAST_LOAD")
ASE_Score <- c(ASE_Score,ase_unlogged)

####EAST_LOAD
EAST_df <- Train_df %>% select('DAY','EAST_LOAD')

EAST_df$LOG_EAST_LOAD <- log(EAST_df$EAST_LOAD)

###Difference the data
###We are differencing here to fit model for P & Q. 

difference_data = artrans.wge(EAST_df$LOG_EAST_LOAD,phi.tr =1)
difference_data = artrans.wge(difference_data,phi.tr =c(rep(0,364),1))

aicselection <-aic5.wge(difference_data,p=0:5,q=0:5,type='bic')

EAST_Load_Model_First_Jan_Seven_Days = est.arma.wge(difference_data
                                                     ,p=aicselection[,1][1],q=aicselection[,2][1])


f = fore.aruma.wge(EAST_df$LOG_EAST_LOAD,phi = EAST_Load_Model_First_Jan_Seven_Days$phi, 
                   theta = EAST_Load_Model_First_Jan_Seven_Days$theta, d=1,
                   n.ahead = 7,limits = T, lastn = T,s=c(rep(0,364),1))

##Unlog forecast
inverse_forecast <- expm1(f$f)

ase_unlogged <- mean((Actual_values$EAST_LOAD-inverse_forecast)^2)
ase_unlogged

#Append List
date_Range <- c(date_Range,"01/01/21-01/07/21")
Load_Zoad <-c(Load_Zoad,"EAST_LOAD")
ASE_Score <- c(ASE_Score,ase_unlogged)

####FAR_WEST_LOAD
FAR_WEST_df <- Train_df %>% select("DAY","FAR_WEST_LOAD")

FAR_WEST_df$LOG_FAR_WEST_LOAD <- log(FAR_WEST_df$FAR_WEST_LOAD)

###Difference the data
###We are differencing here to fit model for P & Q. 

difference_data = artrans.wge(FAR_WEST_df$LOG_FAR_WEST_LOAD,phi.tr =1)
difference_data = artrans.wge(difference_data,phi.tr =c(rep(0,364),1))


aicselection <-aic5.wge(difference_data,p=0:5,q=0:5,type='bic')

FAR_WEST_Load_Model_First_Jan_Seven_Days = est.arma.wge(difference_data
                                                    ,p=aicselection[,1][1],q=aicselection[,2][1])


f = fore.aruma.wge(FAR_WEST_df$LOG_FAR_WEST_LOAD,phi = FAR_WEST_Load_Model_First_Jan_Seven_Days$phi, 
                   theta = FAR_WEST_Load_Model_First_Jan_Seven_Days$theta, d=1,
                   n.ahead = 7,limits = T, lastn = T,s=c(rep(0,364),1))

##Unlog forecast
inverse_forecast <- expm1(f$f)

ase_unlogged <- mean((Actual_values$FAR_WEST_LOAD-inverse_forecast)^2)
ase_unlogged

#Append List
date_Range <- c(date_Range,"01/01/21-01/07/21")
Load_Zoad <-c(Load_Zoad,"FAR_WEST_LOAD")
ASE_Score <- c(ASE_Score,ase_unlogged)

####NORTH
NORTH_df <- Train_df %>% select('DAY','NORTH_LOAD')

NORTH_df$LOG_NORTH_LOAD <- log(NORTH_df$NORTH_LOAD)

###Difference the data
###We are differencing here to fit model for P & Q. 

difference_data = artrans.wge(NORTH_df$LOG_NORTH_LOAD,phi.tr =1)
difference_data = artrans.wge(difference_data,phi.tr =c(rep(0,364),1))


aicselection <-aic5.wge(difference_data,p=0:5,q=0:5,type='bic')

NORTH_Load_Model_First_Jan_Seven_Days = est.arma.wge(difference_data
                                                        ,p=aicselection[,1][1],q=aicselection[,2][1])


f = fore.aruma.wge(NORTH_df$LOG_NORTH_LOAD,phi = NORTH_Load_Model_First_Jan_Seven_Days$phi, 
                   theta = NORTH_Load_Model_First_Jan_Seven_Days$theta, d=1,
                   n.ahead = 7,limits = T, lastn = T,s=c(rep(0,364),1))

##Unlog forecast
inverse_forecast <- expm1(f$f)

ase_unlogged <- mean((Actual_values$NORTH_LOAD-inverse_forecast)^2)
ase_unlogged

#Append List
date_Range <- c(date_Range,"01/01/21-01/07/21")
Load_Zoad <-c(Load_Zoad,"NORTH_LOAD")
ASE_Score <- c(ASE_Score,ase_unlogged)

####NORTH_CENTRAL
NORTH_CENTRAL_df <- Train_df %>% select('DAY','NORTH_CENTRAL_LOAD')

NORTH_CENTRAL_df$LOG_NORTH_CENTRAL_LOAD <- log(NORTH_CENTRAL_df$NORTH_CENTRAL_LOAD)

###Difference the data
###We are differencing here to fit model for P & Q. 

difference_data = artrans.wge(NORTH_CENTRAL_df$LOG_NORTH_CENTRAL_LOAD,phi.tr =1)

difference_data = artrans.wge(difference_data,phi.tr =c(rep(0,364),1))

aicselection <-aic5.wge(difference_data,p=0:5,q=0:5,type='bic')

NORTH_CENTRAL_Load_Model_First_Jan_Seven_Days = est.arma.wge(difference_data
                                                     ,p=aicselection[,1][1],q=aicselection[,2][1])


f = fore.aruma.wge(NORTH_CENTRAL_df$LOG_NORTH_CENTRAL_LOAD,phi = NORTH_CENTRAL_Load_Model_First_Jan_Seven_Days$phi, 
                   theta = NORTH_CENTRAL_Load_Model_First_Jan_Seven_Days$theta, d=1,
                   n.ahead = 7,limits = T, lastn = T,
                   s=c(rep(0,364),1))


##Unlog forecast
inverse_forecast <- expm1(f$f)

ase_unlogged <- mean((Actual_values$NORTH_CENTRAL_LOAD-inverse_forecast)^2)
ase_unlogged

#Append List
date_Range <- c(date_Range,"01/01/21-01/07/21")
Load_Zoad <-c(Load_Zoad,"NORTH_CENTRAL_LOAD")
ASE_Score <- c(ASE_Score,ase_unlogged)


####SOUTHERN
SOUTHERN_df <- Train_df %>% select('DAY','SOUTHERN_LOAD')

SOUTHERN_df$LOG_SOUTHERN_LOAD <- log(SOUTHERN_df$SOUTHERN_LOAD)

###Difference the data
###We are differencing here to fit model for P & Q. 

difference_data = artrans.wge(SOUTHERN_df$LOG_SOUTHERN_LOAD,phi.tr =1)
difference_data = artrans.wge(difference_data,phi.tr =c(rep(0,364),1))


aicselection <-aic5.wge(difference_data,p=0:5,q=0:5,type='bic')

SOUTHERN_Load_Model_First_Jan_Seven_Days = est.arma.wge(difference_data
                                                             ,p=aicselection[,1][1],q=aicselection[,2][1])


f = fore.aruma.wge(SOUTHERN_df$LOG_SOUTHERN_LOAD,phi = SOUTHERN_Load_Model_First_Jan_Seven_Days$phi, 
                   theta = SOUTHERN_Load_Model_First_Jan_Seven_Days$theta, d=1,
                   n.ahead = 7,limits = T, lastn = T,s=c(rep(0,364),1))

##Unlog forecast
inverse_forecast <- expm1(f$f)

ase_unlogged <- mean((Actual_values$SOUTHERN_LOAD-inverse_forecast)^2)
ase_unlogged

#Append List
date_Range <- c(date_Range,"01/01/21-01/07/21")
Load_Zoad <-c(Load_Zoad,"SOUTHERN_LOAD")
ASE_Score <- c(ASE_Score,ase_unlogged)

####South Central
SOUTH_CENTRAL_df <- Train_df %>% select('DAY','SOUTH_CENTRAL_LOAD')

SOUTH_CENTRAL_df$LOG_SOUTH_CENTRAL_LOAD <- log(SOUTH_CENTRAL_df$SOUTH_CENTRAL_LOAD)

###Difference the data
###We are differencing here to fit model for P & Q. 

difference_data = artrans.wge(SOUTH_CENTRAL_df$LOG_SOUTH_CENTRAL_LOAD,phi.tr =1)
difference_data = artrans.wge(difference_data,phi.tr =c(rep(0,364),1))


aicselection <-aic5.wge(difference_data,p=0:5,q=0:5,type='bic')

SOUTH_CENTRAL_Load_Model_First_Jan_Seven_Days = est.arma.wge(difference_data
                                                        ,p=aicselection[,1][1],q=aicselection[,2][1])


f = fore.aruma.wge(SOUTH_CENTRAL_df$LOG_SOUTH_CENTRAL_LOAD,phi = SOUTH_CENTRAL_Load_Model_First_Jan_Seven_Days$phi, 
                   theta = SOUTH_CENTRAL_Load_Model_First_Jan_Seven_Days$theta, d=1,
                   n.ahead = 7,limits = T, lastn = T,s=c(rep(0,364),1))

##Unlog forecast
inverse_forecast <- expm1(f$f)

ase_unlogged <- mean((Actual_values$SOUTH_CENTRAL_LOAD-inverse_forecast)^2)
ase_unlogged

#Append List
date_Range <- c(date_Range,"01/01/21-01/07/21")
Load_Zoad <-c(Load_Zoad,"SOUTH_CENTRAL_LOAD")
ASE_Score <- c(ASE_Score,ase_unlogged)

#############WEST

WEST_df <- Train_df %>% select('DAY','WEST_LOAD')

WEST_df$LOG_WEST_LOAD <- log(WEST_df$WEST_LOAD)

###Difference the data
###We are differencing here to fit model for P & Q. 

difference_data = artrans.wge(WEST_df$LOG_WEST_LOAD,phi.tr =1)
difference_data = artrans.wge(difference_data,phi.tr =c(rep(0,364),1))


aicselection <-aic5.wge(difference_data,p=0:5,q=0:5,type='bic')

WEST_Load_Model_First_Jan_Seven_Days = est.arma.wge(difference_data
                                                             ,p=aicselection[,1][1],q=aicselection[,2][1])


f = fore.aruma.wge(WEST_df$LOG_WEST_LOAD,phi = WEST_Load_Model_First_Jan_Seven_Days$phi, 
                   theta = WEST_Load_Model_First_Jan_Seven_Days$theta, d=1,
                   n.ahead = 7,limits = T, lastn = T,s=c(rep(0,364),1))

##Unlog forecast
inverse_forecast <- expm1(f$f)

ase_unlogged <- mean((Actual_values$WEST_LOAD-inverse_forecast)^2)
ase_unlogged

#Append List
date_Range <- c(date_Range,"01/01/21-01/07/21")
Load_Zoad <-c(Load_Zoad,"WEST_LOAD")
ASE_Score <- c(ASE_Score,ase_unlogged)

######################################################02/13/2021-02/19/2021

Train_df <- subset(Load_df,DAY <= "2021-02-12" )

Actual_values <-subset(Load_df,DAY >"2021-02-12" & DAY <"2021-02-20")

###COAST

COAST_df <- Train_df %>% select('DAY','COAST_LOAD')

COAST_df$LOG_COAST_LOAD <- log(COAST_df$COAST_LOAD)

###Difference the data
###We are differencing here to fit model for P & Q. 

difference_data = artrans.wge(COAST_df$LOG_COAST_LOAD,phi.tr =1)
difference_data = artrans.wge(difference_data,phi.tr =c(rep(0,364),1))


aicselection <-aic5.wge(difference_data,p=0:5,q=0:5,type='bic')


###Note here we are loading the difference data into it in order to get the equation
COAST_Load_Model_First_Feb_storm = est.arma.wge(difference_data
                                                     ,p=aicselection[,1][1],q=aicselection[,2][1])



## Un-diff log data is loaded into forecast then it is d by d=1
f = fore.aruma.wge(COAST_df$LOG_COAST_LOAD,phi = COAST_Load_Model_First_Feb_storm$phi, 
                   theta = COAST_Load_Model_First_Feb_storm$theta, d=1,
                   n.ahead = 7,limits = T, lastn = T,
                   s=c(rep(0,364),1))


##Unlog forecast
inverse_forecast <- expm1(f$f)

ase_unlogged <- mean((Actual_values$COAST_LOAD-inverse_forecast)^2)
ase_unlogged

#Append List
date_Range <- c(date_Range,"02/13/2021-02/19/2021")
Load_Zoad <-c(Load_Zoad,"COAST_LOAD")
ASE_Score <- c(ASE_Score,ase_unlogged)


####EAST_LOAD
EAST_df <- Train_df %>% select('DAY','EAST_LOAD')

EAST_df$LOG_EAST_LOAD <- log(EAST_df$EAST_LOAD)

###Difference the data
###We are differencing here to fit model for P & Q. 

difference_data = artrans.wge(EAST_df$LOG_EAST_LOAD,phi.tr =1)
difference_data = artrans.wge(difference_data,phi.tr =c(rep(0,364),1))


aicselection <-aic5.wge(difference_data,p=0:5,q=0:5,type='bic')

EAST_Load_Model_First_Feb_storm = est.arma.wge(difference_data
                                                    ,p=aicselection[,1][1],q=aicselection[,2][1])


f = fore.aruma.wge(EAST_df$LOG_EAST_LOAD,phi = EAST_Load_Model_First_Feb_storm$phi, 
                   theta = EAST_Load_Model_First_Feb_storm$theta, d=1,
                   n.ahead = 7,limits = T, lastn = T,s=c(rep(0,364),1))

##Unlog forecast
inverse_forecast <- expm1(f$f)

ase_unlogged <- mean((Actual_values$EAST_LOAD-inverse_forecast)^2)
ase_unlogged

#Append List
date_Range <- c(date_Range,"02/13/2021-02/19/2021")
Load_Zoad <-c(Load_Zoad,"EAST_LOAD")
ASE_Score <- c(ASE_Score,ase_unlogged)

####FAR_WEST_LOAD
FAR_WEST_df <- Train_df %>% select('DAY','FAR_WEST_LOAD')

FAR_WEST_df$LOG_FAR_WEST_LOAD <- log(FAR_WEST_df$FAR_WEST_LOAD)

###Difference the data
###We are differencing here to fit model for P & Q. 

difference_data = artrans.wge(FAR_WEST_df$LOG_FAR_WEST_LOAD,phi.tr =1)
difference_data = artrans.wge(difference_data,phi.tr =c(rep(0,364),1))


aicselection <-aic5.wge(difference_data,p=0:5,q=0:5,type='bic')

FAR_WEST_Load_Model_First_Feb_storm = est.arma.wge(difference_data
                                                        ,p=aicselection[,1][1],q=aicselection[,2][1])


f = fore.aruma.wge(FAR_WEST_df$LOG_FAR_WEST_LOAD,phi = FAR_WEST_Load_Model_First_Feb_storm$phi, 
                   theta = FAR_WEST_Load_Model_First_Feb_storm$theta, d=1,
                   n.ahead = 7,limits = T, lastn = T,
                   s=c(rep(0,364),1))


##Unlog forecast
inverse_forecast <- expm1(f$f)

ase_unlogged <- mean((Actual_values$FAR_WEST_LOAD-inverse_forecast)^2)
ase_unlogged

#Append List
date_Range <- c(date_Range,"02/13/2021-02/19/2021")
Load_Zoad <-c(Load_Zoad,"FAR_WEST_LOAD")
ASE_Score <- c(ASE_Score,ase_unlogged)

####NORTH
NORTH_df <- Train_df %>% select('DAY','NORTH_LOAD')

NORTH_df$LOG_NORTH_LOAD <- log(NORTH_df$NORTH_LOAD)

###Difference the data
###We are differencing here to fit model for P & Q. 

difference_data = artrans.wge(NORTH_df$LOG_NORTH_LOAD,phi.tr =1)
difference_data = artrans.wge(difference_data,phi.tr =c(rep(0,364),1))


aicselection <-aic5.wge(difference_data,p=0:5,q=0:5,type='bic')

NORTH_Load_Model_First_Feb_storm = est.arma.wge(difference_data
                                                     ,p=aicselection[,1][1],q=aicselection[,2][1])


f = fore.aruma.wge(NORTH_df$LOG_NORTH_LOAD,phi = NORTH_Load_Model_First_Feb_storm$phi, 
                   theta = NORTH_Load_Model_First_Feb_storm$theta, d=1,
                   n.ahead = 7,limits = T, lastn = T,s=c(rep(0,364),1))

##Unlog forecast
inverse_forecast <- expm1(f$f)

ase_unlogged <- mean((Actual_values$NORTH_LOAD-inverse_forecast)^2)
ase_unlogged

#Append List
date_Range <- c(date_Range,"02/13/2021-02/19/2021")
Load_Zoad <-c(Load_Zoad,"NORTH_LOAD")
ASE_Score <- c(ASE_Score,ase_unlogged)

####NORTH_CENTRAL
NORTH_CENTRAL_df <- Train_df %>% select('DAY','NORTH_CENTRAL_LOAD')

NORTH_CENTRAL_df$LOG_NORTH_CENTRAL_LOAD <- log(NORTH_CENTRAL_df$NORTH_CENTRAL_LOAD)

###Difference the data
###We are differencing here to fit model for P & Q. 

difference_data = artrans.wge(NORTH_CENTRAL_df$LOG_NORTH_CENTRAL_LOAD,phi.tr =1)
difference_data = artrans.wge(difference_data,phi.tr =c(rep(0,364),1))


aicselection <-aic5.wge(difference_data,p=0:5,q=0:5,type='bic')

NORTH_CENTRAL_Load_Model_First_Feb_storm = est.arma.wge(difference_data
                                                             ,p=aicselection[,1][1],q=aicselection[,2][1])


f = fore.aruma.wge(NORTH_CENTRAL_df$LOG_NORTH_CENTRAL_LOAD,phi = NORTH_CENTRAL_Load_Model_First_Feb_storm$phi, 
                   theta = NORTH_CENTRAL_Load_Model_First_Feb_storm$theta, d=1,
                   n.ahead = 7,limits = T, lastn = T,s=c(rep(0,364),1))

##Unlog forecast
inverse_forecast <- expm1(f$f)

ase_unlogged <- mean((Actual_values$NORTH_CENTRAL_LOAD-inverse_forecast)^2)
ase_unlogged

#Append List
date_Range <- c(date_Range,"02/13/2021-02/19/2021")
Load_Zoad <-c(Load_Zoad,"NORTH_CENTRAL_LOAD")
ASE_Score <- c(ASE_Score,ase_unlogged)


####SOUTHERN
SOUTHERN_df <- Train_df %>% select('DAY','SOUTHERN_LOAD')

SOUTHERN_df$LOG_SOUTHERN_LOAD <- log(SOUTHERN_df$SOUTHERN_LOAD)

###Difference the data
###We are differencing here to fit model for P & Q. 

difference_data = artrans.wge(SOUTHERN_df$LOG_SOUTHERN_LOAD,phi.tr =1)
difference_data = artrans.wge(difference_data,phi.tr =c(rep(0,364),1))


aicselection <-aic5.wge(difference_data,p=0:5,q=0:5,type='bic')

SOUTHERN_Load_Model_First_Feb_storm = est.arma.wge(difference_data
                                                        ,p=aicselection[,1][1],q=aicselection[,2][1])


f = fore.aruma.wge(SOUTHERN_df$LOG_SOUTHERN_LOAD,phi = SOUTHERN_Load_Model_First_Feb_storm$phi, 
                   theta = SOUTHERN_Load_Model_First_Feb_storm$theta, d=1,
                   n.ahead = 7,limits = T, lastn = T,
                   s=c(rep(0,364),1))


##Unlog forecast
inverse_forecast <- expm1(f$f)

ase_unlogged <- mean((Actual_values$SOUTHERN_LOAD-inverse_forecast)^2)
ase_unlogged

#Append List
date_Range <- c(date_Range,"02/13/2021-02/19/2021")
Load_Zoad <-c(Load_Zoad,"SOUTHERN_LOAD")
ASE_Score <- c(ASE_Score,ase_unlogged)

####South Central
SOUTH_CENTRAL_df <- Train_df %>% select('DAY','SOUTH_CENTRAL_LOAD')

SOUTH_CENTRAL_df$LOG_SOUTH_CENTRAL_LOAD <- log(SOUTH_CENTRAL_df$SOUTH_CENTRAL_LOAD)

###Difference the data
###We are differencing here to fit model for P & Q. 

difference_data = artrans.wge(SOUTH_CENTRAL_df$LOG_SOUTH_CENTRAL_LOAD,phi.tr =1)
difference_data = artrans.wge(difference_data,phi.tr =c(rep(0,364),1))


aicselection <-aic5.wge(difference_data,p=0:5,q=0:5,type='bic')

SOUTH_CENTRAL_Load_Model_First_Feb_storm = est.arma.wge(difference_data
                                                             ,p=aicselection[,1][1],q=aicselection[,2][1])


f = fore.aruma.wge(SOUTH_CENTRAL_df$LOG_SOUTH_CENTRAL_LOAD,phi = SOUTH_CENTRAL_Load_Model_First_Feb_storm$phi, 
                   theta = SOUTH_CENTRAL_Load_Model_First_Feb_storm$theta, d=1,
                   n.ahead = 7,limits = T, lastn = T,s=c(rep(0,364),1))

##Unlog forecast
inverse_forecast <- expm1(f$f)

ase_unlogged <- mean((Actual_values$SOUTH_CENTRAL_LOAD-inverse_forecast)^2)
ase_unlogged

#Append List
date_Range <- c(date_Range,"02/13/2021-02/19/2021")
Load_Zoad <-c(Load_Zoad,"SOUTH_CENTRAL_LOAD")
ASE_Score <- c(ASE_Score,ase_unlogged)

#############WEST

WEST_df <- Train_df %>% select('DAY','WEST_LOAD')

WEST_df$LOG_WEST_LOAD <- log(WEST_df$WEST_LOAD)

###Difference the data
###We are differencing here to fit model for P & Q. 

difference_data = artrans.wge(WEST_df$LOG_WEST_LOAD,phi.tr =1)
difference_data = artrans.wge(difference_data,phi.tr =c(rep(0,364),1))


aicselection <-aic5.wge(difference_data,p=0:5,q=0:5,type='bic')

WEST_Load_Model_First_Feb_storm = est.arma.wge(difference_data
                                                    ,p=aicselection[,1][1],q=aicselection[,2][1])


f = fore.aruma.wge(WEST_df$LOG_WEST_LOAD,phi = WEST_Load_Model_First_Feb_storm$phi, 
                   theta = WEST_Load_Model_First_Feb_storm$theta, d=1,
                   n.ahead = 151,limits = T, lastn = T,s=c(rep(0,364),1))

##Unlog forecast
inverse_forecast <- expm1(f$f)

ase_unlogged <- mean((Actual_values$WEST_LOAD-inverse_forecast)^2)
ase_unlogged

#Append List
date_Range <- c(date_Range,"02/13/2021-02/19/2021")
Load_Zoad <-c(Load_Zoad,"WEST_LOAD")
ASE_Score <- c(ASE_Score,ase_unlogged)


##############01/01/2021-05/31/2021

Train_df <- subset(Load_df,DAY <= "2020-12-31" )

Actual_values <-subset(Load_df,DAY >"2020-12-31")

###COAST

COAST_df <- Train_df %>% select('DAY','COAST_LOAD')

COAST_df$LOG_COAST_LOAD <- log(COAST_df$COAST_LOAD)

###Difference the data
###We are differencing here to fit model for P & Q. 

difference_data = artrans.wge(COAST_df$LOG_COAST_LOAD,phi.tr =1)
difference_data = artrans.wge(difference_data,phi.tr =c(rep(0,364),1))


aicselection <-aic5.wge(difference_data,p=0:5,q=0:5,type='bic')


###Note here we are loading the difference data into it in order to get the equation
COAST_Load_Model_First_half_2021 = est.arma.wge(difference_data
                                                     ,p=aicselection[,1][1],q=aicselection[,2][1])



## Un-diff log data is loaded into forecast then it is d by d=1
f = fore.aruma.wge(COAST_df$LOG_COAST_LOAD,phi = COAST_Load_Model_First_half_2021$phi, 
                   theta = COAST_Load_Model_First_half_2021$theta, d=1,
                   n.ahead = 151,limits = T, lastn = T,s=c(rep(0,364),1))

##Unlog forecast
inverse_forecast <- expm1(f$f)

ase_unlogged <- mean((Actual_values$COAST_LOAD-inverse_forecast)^2)
ase_unlogged

#Append List
date_Range <- c(date_Range,"01/01/2021-05/31/2021")
Load_Zoad <-c(Load_Zoad,"COAST_LOAD")
ASE_Score <- c(ASE_Score,ase_unlogged)

####EAST_LOAD
EAST_df <- Train_df %>% select('DAY','EAST_LOAD')

EAST_df$LOG_EAST_LOAD <- log(EAST_df$EAST_LOAD)

###Difference the data
###We are differencing here to fit model for P & Q. 

difference_data = artrans.wge(EAST_df$LOG_EAST_LOAD,phi.tr =1)
difference_data = artrans.wge(difference_data,phi.tr =c(rep(0,364),1))


aicselection <-aic5.wge(difference_data,p=0:5,q=0:5,type='bic')

EAST_Load_Model_First_half_2021 = est.arma.wge(difference_data
                                                    ,p=aicselection[,1][1],q=aicselection[,2][1])


f = fore.aruma.wge(EAST_df$LOG_EAST_LOAD,phi = EAST_Load_Model_First_half_2021$phi, 
                   theta = EAST_Load_Model_First_half_2021$theta, d=1,
                   n.ahead = 151,limits = T, lastn = T,s=c(rep(0,364),1))

##Unlog forecast
inverse_forecast <- expm1(f$f)

ase_unlogged <- mean((Actual_values$EAST_LOAD-inverse_forecast)^2)
ase_unlogged

#Append List
date_Range <- c(date_Range,"01/01/2021-05/31/2021")
Load_Zoad <-c(Load_Zoad,"EAST_LOAD")
ASE_Score <- c(ASE_Score,ase_unlogged)

####FAR_WEST_LOAD
FAR_WEST_df <- Train_df %>% select("DAY","FAR_WEST_LOAD")

FAR_WEST_df$LOG_FAR_WEST_LOAD <- log(FAR_WEST_df$FAR_WEST_LOAD)

###Difference the data
###We are differencing here to fit model for P & Q. 

difference_data = artrans.wge(FAR_WEST_df$LOG_FAR_WEST_LOAD,phi.tr =1)
difference_data = artrans.wge(difference_data,phi.tr =c(rep(0,364),1))


aicselection <-aic5.wge(difference_data,p=0:5,q=0:5,type='bic')

FAR_WEST_Load_Model_First_half_2021 = est.arma.wge(difference_data
                                                        ,p=aicselection[,1][1],q=aicselection[,2][1])


f = fore.aruma.wge(FAR_WEST_df$LOG_FAR_WEST_LOAD,phi = FAR_WEST_Load_Model_First_half_2021$phi, 
                   theta = FAR_WEST_Load_Model_First_half_2021$theta, d=1,
                   n.ahead = 151,limits = T, lastn = T,s=c(rep(0,364),1))

##Unlog forecast
inverse_forecast <- expm1(f$f)

ase_unlogged <- mean((Actual_values$FAR_WEST_LOAD-inverse_forecast)^2)
ase_unlogged

#Append List
date_Range <- c(date_Range,"01/01/2021-05/31/2021")
Load_Zoad <-c(Load_Zoad,"FAR_WEST_LOAD")
ASE_Score <- c(ASE_Score,ase_unlogged)

####NORTH
NORTH_df <- Train_df %>% select('DAY','NORTH_LOAD')

NORTH_df$LOG_NORTH_LOAD <- log(NORTH_df$NORTH_LOAD)

###Difference the data
###We are differencing here to fit model for P & Q. 

difference_data = artrans.wge(NORTH_df$LOG_NORTH_LOAD,phi.tr =1)
difference_data = artrans.wge(difference_data,phi.tr =c(rep(0,364),1))


aicselection <-aic5.wge(difference_data,p=0:5,q=0:5,type='bic')

NORTH_Load_Model_First_half_2021 = est.arma.wge(difference_data
                                                     ,p=aicselection[,1][1],q=aicselection[,2][1])


f = fore.aruma.wge(NORTH_df$LOG_NORTH_LOAD,phi = NORTH_Load_Model_First_half_2021$phi, 
                   theta = NORTH_Load_Model_First_half_2021$theta, d=1,
                   n.ahead = 151,limits = T, lastn = T,s=c(rep(0,364),1))

##Unlog forecast
inverse_forecast <- expm1(f$f)

ase_unlogged <- mean((Actual_values$NORTH_LOAD-inverse_forecast)^2)
ase_unlogged

#Append List
date_Range <- c(date_Range,"01/01/2021-05/31/2021")
Load_Zoad <-c(Load_Zoad,"NORTH_LOAD")
ASE_Score <- c(ASE_Score,ase_unlogged)

####NORTH_CENTRAL
NORTH_CENTRAL_df <- Train_df %>% select('DAY','NORTH_CENTRAL_LOAD')

NORTH_CENTRAL_df$LOG_NORTH_CENTRAL_LOAD <- log(NORTH_CENTRAL_df$NORTH_CENTRAL_LOAD)

###Difference the data
###We are differencing here to fit model for P & Q. 

difference_data = artrans.wge(NORTH_CENTRAL_df$LOG_NORTH_CENTRAL_LOAD,phi.tr =1)
difference_data = artrans.wge(difference_data,phi.tr =c(rep(0,364),1))


aicselection <-aic5.wge(difference_data,p=0:5,q=0:5,type='bic')

NORTH_CENTRAL_Load_Model_First_half_2021 = est.arma.wge(difference_data
                                                             ,p=aicselection[,1][1],q=aicselection[,2][1])


f = fore.aruma.wge(NORTH_CENTRAL_df$LOG_NORTH_CENTRAL_LOAD,phi = NORTH_CENTRAL_Load_Model_First_half_2021$phi, 
                   theta = NORTH_CENTRAL_Load_Model_First_half_2021$theta, d=1,
                   n.ahead = 151,limits = T, lastn = T,s=c(rep(0,364),1))

##Unlog forecast
inverse_forecast <- expm1(f$f)

ase_unlogged <- mean((Actual_values$NORTH_CENTRAL_LOAD-inverse_forecast)^2)
ase_unlogged

#Append List
date_Range <- c(date_Range,"01/01/2021-05/31/2021")
Load_Zoad <-c(Load_Zoad,"NORTH_CENTRAL_LOAD")
ASE_Score <- c(ASE_Score,ase_unlogged)


####SOUTHERN
SOUTHERN_df <- Train_df %>% select('DAY','SOUTHERN_LOAD')

SOUTHERN_df$LOG_SOUTHERN_LOAD <- log(SOUTHERN_df$SOUTHERN_LOAD)

###Difference the data
###We are differencing here to fit model for P & Q. 

difference_data = artrans.wge(SOUTHERN_df$LOG_SOUTHERN_LOAD,phi.tr =1)
difference_data = artrans.wge(difference_data,phi.tr =c(rep(0,364),1))


aicselection <-aic5.wge(difference_data,p=0:5,q=0:5,type='bic')

SOUTHERN_Load_Model_First_half_2021 = est.arma.wge(difference_data
                                                        ,p=aicselection[,1][1],q=aicselection[,2][1])


f = fore.aruma.wge(SOUTHERN_df$LOG_SOUTHERN_LOAD,phi = SOUTHERN_Load_Model_First_half_2021$phi, 
                   theta = SOUTHERN_Load_Model_First_half_2021$theta, d=1,
                   n.ahead = 151,limits = T, lastn = T,s=c(rep(0,364),1))

##Unlog forecast
inverse_forecast <- expm1(f$f)

ase_unlogged <- mean((Actual_values$SOUTHERN_LOAD-inverse_forecast)^2)
ase_unlogged

#Append List
date_Range <- c(date_Range,"01/01/2021-05/31/2021")
Load_Zoad <-c(Load_Zoad,"SOUTHERN_LOAD")
ASE_Score <- c(ASE_Score,ase_unlogged)

####South Central
SOUTH_CENTRAL_df <- Train_df %>% select('DAY','SOUTH_CENTRAL_LOAD')

SOUTH_CENTRAL_df$LOG_SOUTH_CENTRAL_LOAD <- log(SOUTH_CENTRAL_df$SOUTH_CENTRAL_LOAD)

###Difference the data
###We are differencing here to fit model for P & Q. 

difference_data = artrans.wge(SOUTH_CENTRAL_df$LOG_SOUTH_CENTRAL_LOAD,phi.tr =1)
difference_data = artrans.wge(difference_data,phi.tr =c(rep(0,364),1))


aicselection <-aic5.wge(difference_data,p=0:5,q=0:5,type='bic')

SOUTH_CENTRAL_Load_Model_First_half_2021 = est.arma.wge(difference_data
                                                             ,p=aicselection[,1][1],q=aicselection[,2][1])


f = fore.aruma.wge(SOUTH_CENTRAL_df$LOG_SOUTH_CENTRAL_LOAD,phi = SOUTH_CENTRAL_Load_Model_First_half_2021$phi, 
                   theta = SOUTH_CENTRAL_Load_Model_First_half_2021$theta, d=1,
                   n.ahead = 151,limits = T, lastn = T,s=c(rep(0,364),1))

##Unlog forecast
inverse_forecast <- expm1(f$f)

ase_unlogged <- mean((Actual_values$SOUTH_CENTRAL_LOAD-inverse_forecast)^2)
ase_unlogged

#Append List
date_Range <- c(date_Range,"01/01/2021-05/31/2021")
Load_Zoad <-c(Load_Zoad,"SOUTH_CENTRAL_LOAD")
ASE_Score <- c(ASE_Score,ase_unlogged)

#############WEST

WEST_df <- Train_df %>% select('DAY','WEST_LOAD')

WEST_df$LOG_WEST_LOAD <- log(WEST_df$WEST_LOAD)

###Difference the data
###We are differencing here to fit model for P & Q. 

difference_data = artrans.wge(WEST_df$LOG_WEST_LOAD,phi.tr =1)
difference_data = artrans.wge(difference_data,phi.tr =c(rep(0,364),1))


aicselection <-aic5.wge(difference_data,p=0:5,q=0:5,type='bic')

WEST_Load_Model_First_half_2021 = est.arma.wge(difference_data
                                                    ,p=aicselection[,1][1],q=aicselection[,2][1])


f = fore.aruma.wge(WEST_df$LOG_WEST_LOAD,phi = WEST_Load_Model_First_half_2021$phi, 
                   theta = WEST_Load_Model_First_half_2021$theta, d=1,
                   n.ahead = 151,limits = T, lastn = T,s=c(rep(0,364),1))

##Unlog forecast
inverse_forecast <- expm1(f$f)

ase_unlogged <- mean((Actual_values$WEST_LOAD-inverse_forecast)^2)
ase_unlogged

#Append List
date_Range <- c(date_Range,"01/01/2021-05/31/2021")
Load_Zoad <-c(Load_Zoad,"WEST_LOAD")
ASE_Score <- c(ASE_Score,ase_unlogged)


#turn list into dataframe
Model_results <-cbind(date_Range,Load_Zoad,ASE_Score)
Model_results <- as.data.frame(Model_results)
Model_results

##Fix the formatting 
new_df <-  spread(Model_results,Load_Zoad,ASE_Score)
new_df <- apply(new_df,2,as.character)
new_df <- as.data.frame(new_df)
new_df <- new_df %>% select('date_Range','COAST_LOAD','EAST_LOAD','FAR_WEST_LOAD',
                            'NORTH_LOAD','NORTH_CENTRAL_LOAD','SOUTH_CENTRAL_LOAD','SOUTHERN_LOAD','WEST_LOAD')

write.csv(new_df,"ARIMA_SEASONALITY_MODEL_Results.csv")

#R codebook for Disconnected Youth in PUMS 2014
#12/1/15

#hello, this is the codebook for calculating disconnected youth in the USA.
#There were several steps in this process – omitting NAs, cleaning the data, and then finally #looking at the weighted results. 
# We are storing the data in MySQL.
# We us the replicate weights and Fay's method for calculating the standard error.

#install the MySQL packages and survey
 install.packages("RMySQL")
 library(RMySQL)
 library("survey", lib.loc="~/R/R-3.2.0/library")
 library("plyr", lib.loc="~/R/R-3.2.0/library")

# let's try and calculate a national number! Can r handle 44 million people? Of course it can. 

 library(RMySQL)
 library("survey", lib.loc="~/R/R-3.2.0/library")

# tell R where it can find the database
pums14 <- dbConnect(MySQL(), user='******', password='*****', dbname='moa_db', host='mysql.ssrc-dbhost-2914.org')
 dbListTables(pums14)


# Function to make it easier to query 
query <- function(...) dbGetQuery(pums14, ...)



# Select out the variables we need for DY and pull out just the data for LA county to test.
USAb<-query("SELECT SERIALNO, PUMA, PWGTP, ST, AGEP, RAC1P, SCH, ESR, HISP, SEX, pwgtp1 ,
pwgtp2 ,
pwgtp3 ,
pwgtp4 ,
pwgtp5 ,
pwgtp6 ,
pwgtp7 ,
pwgtp8 ,
pwgtp9 ,
pwgtp10 ,
pwgtp11 ,
pwgtp12 ,
pwgtp13 ,
pwgtp14 ,
pwgtp15 ,
pwgtp16 ,
pwgtp17 ,
pwgtp18 ,
pwgtp19 ,
pwgtp20 ,
pwgtp21 ,
pwgtp22 ,
pwgtp23 ,
pwgtp24 ,
pwgtp25 ,
pwgtp26 ,
pwgtp27 ,
pwgtp28 ,
pwgtp29 ,
pwgtp30 ,
pwgtp31 ,
pwgtp32 ,
pwgtp33 ,
pwgtp34 ,
pwgtp35 ,
pwgtp36 ,
pwgtp37 ,
pwgtp38 ,
pwgtp39 ,
pwgtp40 ,
pwgtp41 ,
pwgtp42 ,
pwgtp43 ,
pwgtp44 ,
pwgtp45 ,
pwgtp46 ,
pwgtp47 ,
pwgtp48 ,
pwgtp49 ,
pwgtp50 ,
pwgtp51 ,
pwgtp52 ,
pwgtp53 ,
pwgtp54 ,
pwgtp55 ,
pwgtp56 ,
pwgtp57 ,
pwgtp58 ,
pwgtp59 ,
pwgtp60 ,
pwgtp61 ,
pwgtp62 ,
pwgtp63 ,
pwgtp64 ,
pwgtp65 ,
pwgtp66 ,
pwgtp67 ,
pwgtp68 ,
pwgtp69 ,
pwgtp70 ,
pwgtp71 ,
pwgtp72 ,
pwgtp73 ,
pwgtp74 ,
pwgtp75 ,
pwgtp76 ,
pwgtp77 ,
pwgtp78 ,
pwgtp79 ,
pwgtp80 
 FROM s14pusb WHERE AGEP >='16'AND AGEP<= '24' AND ST < 70 ;")

# We also need table a from PUMS! 

USAa<-query("SELECT SERIALNO, PUMA, PWGTP, ST, AGEP, RAC1P, SCH, ESR, HISP, SEX, pwgtp1 ,
pwgtp2 ,
pwgtp3 ,
pwgtp4 ,
pwgtp5 ,
pwgtp6 ,
pwgtp7 ,
pwgtp8 ,
pwgtp9 ,
pwgtp10 ,
pwgtp11 ,
pwgtp12 ,
pwgtp13 ,
pwgtp14 ,
pwgtp15 ,
pwgtp16 ,
pwgtp17 ,
pwgtp18 ,
pwgtp19 ,
pwgtp20 ,
pwgtp21 ,
pwgtp22 ,
pwgtp23 ,
pwgtp24 ,
pwgtp25 ,
pwgtp26 ,
pwgtp27 ,
pwgtp28 ,
pwgtp29 ,
pwgtp30 ,
pwgtp31 ,
pwgtp32 ,
pwgtp33 ,
pwgtp34 ,
pwgtp35 ,
pwgtp36 ,
pwgtp37 ,
pwgtp38 ,
pwgtp39 ,
pwgtp40 ,
pwgtp41 ,
pwgtp42 ,
pwgtp43 ,
pwgtp44 ,
pwgtp45 ,
pwgtp46 ,
pwgtp47 ,
pwgtp48 ,
pwgtp49 ,
pwgtp50 ,
pwgtp51 ,
pwgtp52 ,
pwgtp53 ,
pwgtp54 ,
pwgtp55 ,
pwgtp56 ,
pwgtp57 ,
pwgtp58 ,
pwgtp59 ,
pwgtp60 ,
pwgtp61 ,
pwgtp62 ,
pwgtp63 ,
pwgtp64 ,
pwgtp65 ,
pwgtp66 ,
pwgtp67 ,
pwgtp68 ,
pwgtp69 ,
pwgtp70 ,
pwgtp71 ,
pwgtp72 ,
pwgtp73 ,
pwgtp74 ,
pwgtp75 ,
pwgtp76 ,
pwgtp77 ,
pwgtp78 ,
pwgtp79 ,
pwgtp80 
 FROM s14pusa WHERE AGEP >='16'AND AGEP<= '24' AND ST < 70 ;")

#now take both of these big USA tables and row combine them to make one table.
USA<- rbind(USAa, USAb)

# remove the two seperate tables so that they don't take up space
rm(USAa, USAb)

#creating a dummy variable for youth

USA$youth <- as.numeric(ifelse(USA$AGEP >= 16 & USA$AGEP <=24, 1, 0))


#creating dummy variable for DY 

USA$DYdummy <- as.numeric(ifelse(USA$SCH == 1 & (USA$ESR == 3 | USA$ESR == 6) & USA$youth ==1 , 1, 0))

#creating a variable for gender

USA$male <- as.numeric(ifelse(USA$SEX == 1 &  USA$youth ==1,  1, 0))
USA$female <- as.numeric(ifelse(USA$SEX == 2 &  USA$youth ==1 , 1, 0))
          

#creating dummy variables for race

####stopped here on Jan 8th
#creating dummy variables for race
#Here is the code directly from PUMS for how they categorize race.
## Recoded detailed race code
# 1 .White alone
# 2 .Black or African American alone
# 3 .American Indian alone
# 4 .Alaska Native alone
# 5 .American Indian and Alaska Native tribes specified; or American .Indian or Alaska Native, not specified and no other races
# 6 .Asian alone
# 7 .Native Hawaiian and Other Pacific Islander alone
# 8 .Some Other Race alone
# 9 .Two or More Races 
 
USA$white <- as.numeric(ifelse(USA$RAC1P == 1 & USA$HISP == "01" , 1, 0))
USA$black <- as.numeric(ifelse(USA$RAC1P == 2 & USA$HISP == "01" , 1, 0))
USA$asian <- as.numeric(ifelse((USA$RAC1P == 6 |  USA$RAC1P == 7 ) & USA$HISP == "01" , 1, 0)) # asian plus Native Hawiian and API
USA$native <-as.numeric(ifelse(( USA$RAC1P == 3  | USA$RAC1P == 4 | USA$RAC1P == 5 ) & (USA$HISP == "01"), 1 ,0))
USA$latino <- as.numeric(ifelse( USA$HISP != "01" , 1,0)) 
USA$other <- as.numeric(ifelse(USA$RAC1P == 8 & USA$HISP == 1  , 1, 0))
USA$two <- as.numeric(ifelse(USA$RAC1P == 9 & USA$HISP == 1  , 1, 0))


#here we create a dummy variable for gender race/ethnic for late use in the city and state data.


#USA$dy.male <-  as.numeric(ifelse(USA$DYdummy == 1 & USA$male ==1, 1, 0))
#USA$dy.female <-as.numeric(ifelse(USA$DYdummy == 1 & USA$female ==1, 1, 0))
#USA$dy.white<-as.numeric(ifelse(USA$DYdummy == 1 & USA$white ==1, 1, 0))
#USA$dy.black <-  as.numeric(ifelse(USA$DYdummy == 1 & USA$black ==1, 1, 0))
#USA$dy.asian <- as.numeric(ifelse(USA$DYdummy == 1 & USA$asian ==1, 1, 0))
#USA$dy.native <- as.numeric(ifelse(USA$DYdummy == 1 & USA$native ==1, 1, 0))
#USA$dy.latino <-  as.numeric(ifelse(USA$DYdummy == 1 & USA$latino ==1, 1, 0)) 

#create a unique ID for each row

USA$id <- rownames(USA)
 
 #converting all variables to numeric form
 
for(i in c(1,1:ncol(USA))) {
    USA[,i] <- as.numeric(USA[,i])
}




# putting each race/gender into it's own table

male <- 0
female <- 0
white <- 0
white.m <-0
white.f <-0 
black <- 0
black.m <- 0
black.f <- 0
asian <- 0
asian.m <- 0
asian.f <-0
native <- 0
native.m <- 0
native.f <- 0
latino <- 0
latino.m <- 0
latino.f <- 0

#now let's create a dummy for all dy by race ethnicity.
male <- USA[USA$male ==1, ]
female <- USA[USA$female ==1, ]
white <- USA[USA$white ==1, ]
white.m <- USA[c(USA$white ==1 & USA$male ==1) , ]
white.f <- USA[c(USA$white ==1 & USA$female ==1) , ]
black <- USA[USA$black==1, ]
black.m <- USA[c(USA$black==1 & USA$male ==1), ]
black.f<- USA[c(USA$black==1 & USA$female ==1), ]
asian <- USA[USA$asian==1, ]
asian.m <- USA[c(USA$asian==1 & USA$male ==1), ]
asian.f <- USA[c(USA$asian==1 & USA$female ==1), ]
native <- USA[USA$native==1, ]
native.m <- USA[c(USA$native  & USA$male ==1), ]
native.f <- USA[c(USA$native & USA$female ==1), ]
latino <- USA[USA$latino==1, ]
latino.m <- USA[c(USA$latino & USA$male ==1), ]
latino.f <- USA[c(USA$latino & USA$female ==1), ]


# put the above in a list 
genrac <-list( USA, male 
,female
,white 
,white.m
,white.f 
,black
,black.m 
,black.f
,asian 
,asian.m 
,asian.f 
,native 
,native.m 
,native.f 
,latino 
,latino.m 
,latino.f 
 )


# Code for for replicate wts, using the R package “Survey”

genrac.function <- function(x){
brr.dy.x <-svrepdesign(variables=x[ ,c(1:10,91:102)],
repweights= x[,11:90],combined.weights=TRUE,
weights= x$PWGTP,
type="Fay", rho=(0.5), scale=1,rscales=1)


# This calculates the.sub of DY
mean<-(100*(as.data.frame(svymean(~DYdummy, brr.dy.x))))


#This calculates the total number of DY

total<-as.data.frame(svytotal(~DYdummy, brr.dy.x))
dytable<- cbind(mean,total)
}

 USA.genrac <- sapply(genrac, genrac.function, USE.NAMES = TRUE)
 
 #transpose and format ( need to do sig figs)
 dy.USA.genrac <- t(USA.genrac)

 






#now let's create a function that can be applied to different geographic regions

#the large table is USA  We want to divide that up into states by number.
#Let's make 51 dataframes

AL1<- 0
AK2<- 0
AZ4 <- 0
AR5<-0
CA6<-0
CO8<-0
CT9<-0
DE10<-0
DC11<-0
FL12<-0
GA13<-0
HI15<-0
ID16<-0
IL17<-0
IN18<-0
IA19<-0
KS20<-0
KY21<-0
LA22<-0
ME23<-0
MD24<-0
MA25<-0
MI26<-0
MN27<-0
MS28<-0
MO29<-0
MT30<-0
NE31<-0
NV32<-0
NH33<-0
NJ34<-0
NM35<-0
NY36<-0
NC37<-0
ND38<-0
OH39<-0
OK40<-0
OR41<-0
PA42<-0
RI44<-0
SC45<-0
SD46<-0
TN47<-0
TX48<-0
UT49<-0
VT50<-0
VA51<-0
WA53<-0
WV54<-0
WI55<-0
WY56<-0

# now let's put them in a tidy list
state.list <- list(AL1,
AK2,
AZ4,
AR5,
CA6,
CO8,
CT9,
DE10,
DC11,
FL12,
GA13,
HI15,
ID16,
IL17,
IN18,
IA19,
KS20,
KY21,
LA22,
ME23,
MD24,
MA25,
MI26,
MN27,
MS28,
MO29,
MT30,
NE31,
NV32,
NH33,
NJ34,
NM35,
NY36,
NC37,
ND38,
OH39,
OK40,
OR41,
PA42,
RI44,
SC45,
SD46,
TN47,
TX48,
UT49,
VT50,
VA51,
WA53,
WV54,
WI55,
WY56
)

#now let us split up the USA.id data frame by states

state.list <- split(USA, USA$ST)

#now we need to assign each dataframe in the list of state.list to each of the state dataframes

lapply(seq_along(state.list), function(x) {assign(c( "AL1 ",
 "AK2 ",
 "AZ4 ",
 "AR5 ",
 "CA6 ",
 "CO8 ",
 "CT9 ",
 "DE10 ",
 "DC11 ",
 "FL12 ",
 "GA13 ",
 "HI15 ",
 "ID16 ",
 "IL17 ",
 "IN18 ",
 "IA19 ",
 "KS20 ",
 "KY21 ",
 "LA22 ",
 "ME23 ",
 "MD24 ",
 "MA25 ",
 "MI26 ",
 "MN27 ",
 "MS28 ",
 "MO29 ",
 "MT30 ",
 "NE31 ",
 "NV32 ",
 "NH33 ",
 "NJ34 ",
 "NM35 ",
 "NY36 ",
 "NC37 ",
 "ND38 ",
 "OH39 ",
 "OK40 ",
 "OR41 ",
 "PA42 ",
 "RI44 ",
 "SC45 ",
 "SD46 ",
 "TN47 ",
 "TX48 ",
 "UT49 ",
 "VT50 ",
 "VA51 ",
 "WA53 ",
 "WV54 ",
 "WI55 ",
 "WY56 "
)[x], state.list[[x]], envir=.GlobalEnv)
    }
)



state.function <- function(x){
brr.dy.x <-svrepdesign(variables=x[,c(1:10,91:106)],
repweights= x[,11:90],
combined.weights=TRUE,
weights= x$PWGTP,
type="Fay", rho=(0.5), scale=1,rscales=1)


# This calculates the.sub of DY
mean<-100*(as.data.frame(svymean(~DYdummy, brr.dy.x)))

#This calculates the total number of DY
total<-as.data.frame(svytotal(~DYdummy, brr.dy.x))

#now let's calculate gender and race/ethnicuty

dy.male.total<-as.data.frame(svytotal(~dy.male, brr.dy.x))
dy.male.mean<-100*(as.data.frame(svymean(~dy.male, brr.dy.x)))

male.total<-as.data.frame(svytotal(~male, brr.dy.x))
dy.male.sub<-((dy.male.total/male.total)*100)
se <- sqrt(var(dy.male.sub)/length(male.total))

dy.female.total<-as.data.frame(svytotal(~dy.female, brr.dy.x))
dy.female.mean<-100*(as.data.frame(svymean(~dy.female, brr.dy.x)))
female.total<-as.data.frame(svytotal(~female, brr.dy.x))
dy.female.sub <- ((dy.female.total/female.total)*100)

 dy.white.total<-as.data.frame(svytotal(~dy.white, brr.dy.x))
 dy.white.mean<-100*(as.data.frame(svymean(~dy.white, brr.dy.x)))
 white.total<-as.data.frame(svytotal(~white, brr.dy.x))
 dy.white.sub <- ((dy.white.total/white.total)*100)

 dy.latino.total<-as.data.frame(svytotal(~dy.latino, brr.dy.x))
 dy.latino.mean<-100*(as.data.frame(svymean(~dy.latino, brr.dy.x)))
 latino.total<-as.data.frame(svytotal(~latino, brr.dy.x))
 dy.latino.sub <- ((dy.latino.total/latino.total)*100)

 dy.black.total<-as.data.frame(svytotal(~dy.black, brr.dy.x))
 dy.black.mean<-100*(as.data.frame(svymean(~dy.black, brr.dy.x)))
 black.total<-as.data.frame(svytotal(~black, brr.dy.x))
 dy.black.sub <- ((dy.black.total/black.total)*100)

 dy.asian.total<-as.data.frame(svytotal(~dy.asian, brr.dy.x))
 dy.native.total<-as.data.frame(svytotal(~dy.native, brr.dy.x))


 dytable<- as.data.frame(cbind(mean,total, dy.male.mean, dy.male.sub,dy.female.mean, dy.female.sub, dy.white.mean, dy.white.sub, dy.latino.mean, dy.latino.sub, dy.black.mean, dy.black.sub ))




}

 state.dy.table <- as.data.frame(sapply(state.list, state.function, USE.NAMES = TRUE))
 
 #add column names and row names
colnames(state.dy.table)<- c( "AL1",
"AK2",
"AZ4",
"AR5",
"CA6",
"CO8",
"CT9",
"DE10",
"DC11",
"FL12",
"GA13",
"HI15",
"ID16",
"IL17",
"IN18",
"IA19",
"KS20",
"KY21",
"LA22",
"ME23",
"MD24",
"MA25",
"MI26",
"MN27",
"MS28",
"MO29",
"MT30",
"NE31",
"NV32",
"NH33",
"NJ34",
"NM35",
"NY36",
"NC37",
"ND38",
"OH39",
"OK40",
"OR41",
"PA42",
"RI44",
"SC45",
"SD46",
"TN47",
"TX48",
"UT49",
"VT50",
"VA51",
"WA53",
"WV54",
"WI55",
"WY56"
)

#This handy function gets rid of those pesky list and creates a matrix. Thanks andy!

dy.state<- ldply(state.dy.table, data.frame)

#add the column names
 colnames(dy.state) <- c("state","mean" ,"mean.se", "total", "total.se", "dy.male.mean", "dy.male.mean.se", "dy.male.sub", "dy.male.sub.se", "dy.female.mean", "dy.female.mean.se","dy.female.sub", "dy.female.sub.se", "dy.white.mean", "dy.white.mean.se","dy.white.sub", "dy.white.sub.se", "dy.latino.mean", "dy.latino.mean.se", "dy.latino.sub", "dy.latino.sub.se", "dy.black.mean", "dy.black.mean.se", "dy.black.sub", "dy.black.sub.se" )


#here we need to omit unreliable data
# Using this logic.
#IF (NEET_COUNTY_SE/NEET_COUNTY_FINAL > .2) NEET_COUNTY_UNRELIABLE=1
#We need to create a flag for the unreliable estimates

dy.state$flag.male <-  as.numeric(ifelse((dy.state$dy.male.mean.se/dy.state$dy.male.mean) > 0.2 , 1, 0)) 
dy.state$flag.female <-  as.numeric(ifelse((dy.state$dy.female.mean.se/dy.state$dy.female.mean) > 0.2 , 1, 0)) 
dy.state$flag.white <-  as.numeric(ifelse((dy.state$dy.white.mean.se/dy.state$dy.white.mean) > 0.2 , 1, 0)) 
dy.state$flag.latino <-  as.numeric(ifelse((dy.state$dy.latino.mean.se/dy.state$dy.latino.mean) > 0.2 , 1, 0)) 
dy.state$flag.black <-  as.numeric(ifelse((dy.state$dy.black.mean.se/dy.state$dy.black.mean) > 0.2 , 1, 0)) 

#now put a blank space if there is a flag
dy.state$dy.male.sub <- as.numeric(ifelse(dy.state$flag.male== 1 , NA, dy.state$dy.male.sub )) 
dy.state$dy.female.sub <- as.numeric(ifelse(dy.state$flag.female== 1 , NA, dy.state$dy.female.sub )) 
dy.state$dy.white.sub <- as.numeric(ifelse(dy.state$flag.white== 1 , NA, dy.state$dy.white.sub )) 
dy.state$dy.latino.sub <- as.numeric(ifelse(dy.state$flag.latino== 1 , NA, dy.state$dy.latino.sub )) 
dy.state$dy.black.sub <- as.numeric(ifelse(dy.state$flag.black== 1 , NA, dy.state$dy.black.sub )) 

write.csv(dy.state, file = "dy.state.csv" )


# Let's look at the top 100 cities in 2014. I have found the top 100 cities in 2014 and created a spreadsheet with all the PUMAS contained within the CBSA.
# We want to Subset USA based on the PUMAS found in the top 100 cities.

#Puma to CBSA allocation table  Gen.sub a correspondence file to allocate
#PUMAs (PUMA "2012") to the most-recent Core Based Statistical Areas (CBSA:
#Core Based (Metro/Micro-politan) Statistical Area - 2013) using the
#MABLE/Geocorr12 tool: http://mcdc.missouri.edu/websas/geocorr12.html. The
#correspondence will not be neat in many cases but our standard approach is to
#include all PUMAs that are all or partially within each metro area as being
#part of that metro. What this means is that the approximations of CBSAs will
#sometimes be larger in area and population than the actual CBSAs. If any
#PUMAs end up falling partially within two or more metro areas, include the
#PUMA in the metro that is shares the most population with and note any such
#cases for our records.

 PUMA.to.CBSA.crosswalk<- read.csv("N:/Disconnected Youth Work/2015 Report Work/Data Exploration/PUMA to CBSA crosswalk from Mablecorr 2012.csv")

P2C<-(PUMA.to.CBSA.crosswalk)
colnames(P2C)
# "state"    "puma12"   "cbsa"     "stab"     "cbsaname" "PUMAname" "pop12"    "afact"    "AFACT2"


#instead of removing pumas that are completely allocated to one cbsa and that don't belong to a CBSA, we are just going to remove pumas that don't belong to any CBSA.

cbsa_a_p <- subset(P2C, P2C$cbsa != "")

#cbsa_a_p = cbsa all or partial. 

#now to create the dummy variable for those PUMAs that are entirely encapsulated by a CBSA

cbsa_a_p$dummy <- ifelse(cbsa_a_p$afact == 1, c("1"), c("0"))

#now to deal with MSAs that are divided by three or more

cbsa_just_partial <- subset(cbsa_a_p, cbsa_a_p$dummy == 0)

#subset of whole pumas for later
cbsa_whole <- subset(cbsa_a_p, cbsa_a_p$dummy == 1)

#using the dplyr package will be very helpful

library(dplyr)
df_greatest_percentage_of_puma_in_cbsa <- cbsa_just_partial %>% group_by(stab,puma12,PUMAname) %>% summarise(afact = max(afact))

#now to merge back CBSA numbers to this new partial dataset

duplicates_removed_with_cbsa <-merge(x = df_greatest_percentage_of_puma_in_cbsa, y = cbsa_just_partial, all.x= TRUE)

#now to merge pumas back with the whole pumas

pumas_for_analysis <- merge(x = duplicates_removed_with_cbsa, y = cbsa_whole, all = TRUE)

#Here we want to import the rankings of cbsas so that we can select only the top 100 cities
 ACS_14_1YR_S0101_with_ann <- read.csv("N:/Disconnected Youth Work/2015 Report Work/ACS_14_1YR_S0101_with_ann.csv")

cityranks<-  (ACS_14_1YR_S0101_with_ann)
 
 #add a column indicating the ranking
 cityranks$rank <- NA
cityranks$rank <- 1:nrow(cityranks)

#Now merge with the pumas_for_analysis data set based on the cbsa number
rank<-merge(x = pumas_for_analysis, y = cityranks, by.x = "cbsa", by.y = "GEO.id2",  all = TRUE)

#keep just the columns we need
Puma.ranked.city <- rank[ ,c(1:11, 228:235)]

#this adds a zero to the PUMA so that it matches PUMS format
 
Puma.ranked.city$PUMA.1 <-as.numeric(sprintf("%05d", Puma.ranked.city$puma12))

#select just the top 100 CBSAs

top100 <- subset(Puma.ranked.city, rank.x < 102 , select=c(cbsa, cbsaname, state, puma12, AFACT2, rank.x))

#"cbsa"              "stab"              "puma12"            "PUMAname"          "afact"             "state"             "cbsaname"          "pop12"             #"AFACT2"           
#"dummy"             "GEO.id"            "GEO.display.label" "HC01_EST_VC01"     "rank"              "PUMA.1"     



# here we are subsetting the USA table based on matches to first the state then the puma
 
USA.top.100 <- USA[ which(USA$ST %in% top100$state  & USA$PUMA %in% top100$PUMA.1), ]

#now we want to merge in the CBSA numbers and names.

USA.cities<-merge(x = USA.top.100 , y = top100, by.x = c("ST", "PUMA"), by.y = c("state",  "PUMA.1" ))

#Let's go ahead and split up the USA.cities data by city

cbsalist <- split(USA.cities, USA.cities$cbsa)

#here we get just a list of the top cbsa names so that I can print them below. I'm lazy yo, I am sure there is a better way to do this.
cbsa.names <- as.character(unique(USA.cities$cbsaname))

#this creates an empty table of cities


 lapply(seq_along(cbsalist), function(x) {assign(c( "13820",
"37980",
"47900",
"45300",
"29460",
"27260",
"33100",
"35840",
"36740",
"19660",
"15980",
"37340",
"12060",
"16860",
"12260",
"46520",
"14260",
"41180",
"16980",
"26900",
"17140",
"31140",
"19780",
"36540",
"48620",
"28140",
"12940",
"35380",
"12580",
"14460",
"44140",
"49340",
"39300",
"24340",
"19820",
"33460",
"32820",
"27140",
"29820",
"35620",
"10900",
"10740",
"15380",
"40380",
"45060",
"10580",
"39580",
"20500",
"24660",
"49180",
"16740",
"17460",
"49660",
"10420",
"45780",
"18140",
"19380",
"38060",
"46060",
"36420",
"46140",
"38900",
"38300",
"25420",
"42540",
"24860",
"16700",
"17900",
"28940",
"34980",
"19100",
"21340",
"26420",
"12420",
"41700",
"32580",
"36260",
"41620",
"39340",
"30780",
"40060",
"47260",
"42660",
"31540",
"33340",
"41860",
"37100",
"40900",
"23420",
"12540",
"31080",
"40140",
"41740",
"44700",
"41940",
"17820",
"19740",
"14860",
"25540",
"35300")

[x], cbsalist[[x]], envir=.GlobalEnv)
    }
)



city.function <- function(x){
brr.dy.x <-svrepdesign(variables=x[,c(1:10,91:104)],
repweights= x[,11:90],
combined.weights=TRUE,
weights= x$PWGTP,
type="Fay", rho=(0.5), scale=1,rscales=1)


#This pulls out the msa number
msa.num <- as.numeric(x$cbsa[[2]])


#First let's make sure there are enough youth in this city to calculate DY
total.youth<-as.data.frame(svytotal(~youth, brr.dy.x))
mean.youth<-100*(as.data.frame(svymean(~youth, brr.dy.x)))


# This calculates the sub population of DY out of total youth
sub.youth<-subset(brr.dy.x, youth == 1)
total.dy<-(as.data.frame(svytotal(~DYdummy, sub.youth)))
mean.dy<- round(100*(as.data.frame(svymean(~DYdummy, sub.youth))), 2)


#now let's calculate gender and race/ethnicuty
#gents

#this counts all the you in the male sub.pop. 
sub.male<-subset(sub.youth, male == 1)
dy.male.mean<-100*(as.data.frame(svymean(~DYdummy, sub.male)))
dy.male.total<-as.data.frame(svytotal(~DYdummy, sub.male))
male.youth.total<-as.data.frame(svytotal(~youth, sub.male))


#ladies
sub.female<-subset(sub.youth, female == 1)
dy.female.mean<-100*(as.data.frame(svymean(~DYdummy, sub.female)))
dy.female.total<-as.data.frame(svytotal(~DYdummy, sub.female))
female.youth.total<-as.data.frame(svytotal(~youth, sub.female))


#white
sub.white<-subset(sub.youth, white == 1)
dy.white.mean<-100*(as.data.frame(svymean(~DYdummy, sub.white)))
dy.white.total<-as.data.frame(svytotal(~DYdummy, sub.white))
white.youth.total<-as.data.frame(svytotal(~youth, sub.white))

#latino
sub.latino<-subset(sub.youth, latino == 1)
dy.latino.mean<-100*(as.data.frame(svymean(~DYdummy, sub.latino)))
dy.latino.total<-as.data.frame(svytotal(~DYdummy, sub.latino))
latino.youth.total<-as.data.frame(svytotal(~youth, sub.latino))

#black
sub.black<-subset(sub.youth, black == 1)
dy.black.mean<-100*(as.data.frame(svymean(~DYdummy, sub.black)))
dy.black.total<-as.data.frame(svytotal(~DYdummy, sub.black))
black.youth.total<-as.data.frame(svytotal(~youth, sub.black))


 dytable<- as.data.frame(cbind(msa.num, total.youth, mean.dy, total.dy,  dy.male.mean,  dy.male.total, male.youth.total, dy.female.mean,  dy.female.total, female.youth.total, dy.white.mean,  dy.white.total, white.youth.total, dy.latino.mean,  dy.latino.total, latino.youth.total, dy.black.mean,  dy.black.total, black.youth.total))


}


 city.dy.table <- as.data.frame(sapply(cbsalist, city.function, USE.NAMES = TRUE))
#city.dy.table <- as.data.frame(ddply(USA.cities, cbsa, city.function, USE.NAMES = TRUE))

dy.city<- ldply(city.dy.table, data.frame)

#add the column names
 colnames(dy.city) <- c("city","citynum", "total.youth" ,"total.youth.se", "mean.dy.youth", "mean.dy.youth.se", "total.dy", "total.dy.se", "dy.male.mean", "dy.male.mean.se", "dy.male.total", "dy.male.total.se" ,"male.youth.total", "male.youth.total.se","dy.female.mean", "dy.female.mean.se",  "dy.female.total","dy.female.total.se","female.youth.total", "female.youth.total.se", "dy.white.mean", "dy.white.mean.se",  "dy.white.total","dy.white.total.se","white.youth.total", "white.youth.total.se", "dy.latino.mean", "dy.latino.mean.se",  "dy.latino.total","dy.latino.total.se","latino.youth.total", "latino.youth.total.se", "dy.black.mean", "dy.black.mean.se",  "dy.black.total","dy.black.total.se","black.youth.total", "black.youth.total.se")

#here we need to omit unreliable data
# Using this logic.
#IF (NEET_COUNTY_SE/NEET_COUNTY_FINAL > .2) NEET_COUNTY_UNRELIABLE=1
#We need to create a flag for the unreliable estimates

dy.city$flag.city <-  as.numeric(ifelse((dy.city$total.youth.se/dy.city$total.youth) > 0.2 | (dy.city$total.dy.se/dy.city$total.dy) > 0.2 , 1, 0)) 
dy.city$flag.male <-  as.numeric(ifelse((dy.city$dy.male.total.se/dy.city$dy.male.total) > 0.2 | (dy.city$male.youth.total.se/dy.city$male.youth.total) > 0.2 , 1, 0)) 
dy.city$flag.female <-  as.numeric(ifelse((dy.city$dy.female.total.se/dy.city$dy.female.total) > 0.2 |(dy.city$female.youth.total.se/dy.city$female.youth.total) > 0.2 , 1, 0)) 
dy.city$flag.white <-  as.numeric(ifelse((dy.city$dy.white.total.se/dy.city$dy.white.total) > 0.2 |(dy.city$white.youth.total.se/dy.city$white.youth.total) > 0.2 , 1, 0)) 
dy.city$flag.latino <-  as.numeric(ifelse((dy.city$dy.latino.total.se/dy.city$dy.latino.total) > 0.2 |(dy.city$latino.youth.total.se/dy.city$latino.youth.total) > 0.2 , 1, 0)) 
dy.city$flag.black <-  as.numeric(ifelse((dy.city$dy.black.total.se/dy.city$dy.black.total) > 0.2 |(dy.city$black.youth.total.se/dy.city$black.youth.total) > 0.2 , 1, 0))

#now put a blank space if there is a flag
dy.city$mean <- as.numeric(ifelse(dy.city$flag.city== 1 , NA, dy.city$mean.dy.youth )) 
dy.city$dy.male.mean <- as.numeric(ifelse(dy.city$flag.male== 1 , NA, dy.city$dy.male.mean )) 
dy.city$dy.female.mean <- as.numeric(ifelse(dy.city$flag.female== 1 , NA, dy.city$dy.female.mean )) 
dy.city$dy.white.mean <- as.numeric(ifelse(dy.city$flag.white== 1 , NA, dy.city$dy.white.mean )) 
dy.city$dy.latino.mean <- as.numeric(ifelse(dy.city$flag.latino== 1 , NA, dy.city$dy.latino.mean )) 
dy.city$dy.black.mean <- as.numeric(ifelse(dy.city$flag.black== 1 , NA, dy.city$dy.black.mean )) 





 #merge in the names and ranks of the cities

 city.dy.2014<-merge(x = dy.city , y = top100, by.x = "city", by.y = "cbsa" , all.y = FALSE)


 #pulling out only the rows that have a unique cbsa number
 dy.city.2014<-city.dy.2014[!duplicated(city.dy.2014$city), ]


  write.csv(dy.city.2014, file = "dy.city.2014.10.csv")
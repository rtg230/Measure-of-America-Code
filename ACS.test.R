# Here's what we need to convert D2G ACS data into R. 
install.packages("acs")
library("acs", lib.loc="~/R/R-3.2.0/library")
 api.key.install(key="5b498d1ae7076f392073d199334c5cc03aba91bc")
 just.bronx = acs.fetch(geo = geo.make (state = "NY" , county = "Bronx" ), table.number="B01003", endyear = "2013", span = 1, check=T)
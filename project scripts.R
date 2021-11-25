#loading packages------

install.packages("tidyverse")
library("tidyverse")
library(here)
install.packages("sydneybeaches")

#loading sydneybeaches.csv data

library(readr)    #to import data-- data has to be in the same directory
# sydneybeaches <-read_csv(here("data","sydneybeaches.csv")) - if data is in a sub folder in the same directory


sydneybeaches<-read_csv("sydneybeaches.csv")

View("sydneybeaches")
spec(sydneybeaches)
head(sydneybeaches)
glimpse(sydneybeaches)
View(sydneybeaches)

#making beaches variable (sydneybeaches is a long name anyway)

beaches<-sydneybeaches

#Viewing data

View(beaches)
dim(beaches)        #tells you the dimension that is how big the data file is
str(beaches)        #tells you about the structure and data types etc
glimpse(beaches)
head(beaches)    #top columns
tail(beaches)    #bottom columns
summary(beaches) #gives the summary about the data

install.packages("skimr")   #to provide summary statistics about variables
library(skimr)

skim(beaches)


#tidying columns-----------

glimpse(beaches)

# dplyr under tidyverse is a data wrangling or data cleaning package
#select,filter,rename,change etc

select_all(beaches,toupper)  #changes the column variables in upper case letters
select_all(beaches,tolower)  #changes the column variables in lower case letters

library(janitor)  #a data cleaning package

clean_names(beaches) #separates the columns using a underscore

names(beaches)   #gives you the column names

clean_names(beaches)  #these operations does not affect the data , to save the result assigning to a new object is important

cleanbeaches<- clean_names(beaches)  #results of clean_names() assigned to cleanbeaches

names(cleanbeaches)
rename(cleanbeaches,beachbugs=enterococci_cfu_100ml)  #rename(data,newname=oldname)
View(cleanbeaches)  #still old view
names(cleanbeaches)  #column name is still old one 

cleanbeaches<-  rename(cleanbeaches,beachbugs=enterococci_cfu_100ml) #overwriting on the same table as we already created cleanbeaches after cleaning
names(beaches)


#using select to select the subset of the data

select(cleanbeaches,beach_id,council,site,beachbugs )#returns the requested columns

select(cleanbeaches,beach_id,council,site,beachbugs,everything() )      #returns beach_id,council,site,beachbugs first and then rest of the columns)

#pipe %>%  comes with magrittr package
#no need to put data name in clean_names and rename because we are using pipe 
cleanbeaches <- beaches %>%
  clean_names() %>%
  rename(beachbugs=enterococci_cfu_100ml) %>%
  select(Council,site,beachbugs)

#cleaning objects using broom and loading beaches again
+
  #Another way of doing the above process
sydneybeaches<-read_csv(here("data","sydneybeaches.csv"))
View(sydneybeaches)
beaches<-sydneybeaches
View(beaches)

cleanbeaches<-rename(cleanbeaches,beachbugs=enterococci_cfu_100ml)
names(cleanbeaches)
write.csv(cleanbeaches,"cleanbeaches.csv")

#again using pipe to get every column in cleanbeaches

names (beaches)
cleanbeaches <- beaches %>%
  clean_names() %>%
  rename(beachbugs=enterococci_cfu_100ml)
View(cleanbeaches)


#analysis

#Which beach has the most number of bugs
#arrange() a dplyr fucntion: sorts the data

cleanbeaches %>% 
  arrange(desc (beachbugs))   #returns - little bay beach -4900 
#or 

worstbugs<-cleanbeaches %>% 
  arrange(-beachbugs) 
View(worstbugs)


#finding beachbugs at "Coogee beach" site using filter(a dplyr fucntion) function

cleanbeaches %>% 
  filter(site=="Coogee Beach") %>% 
  arrange(-beachbugs) 

#comparing bugs' value around different beaches

coogee_bondi<-cleanbeaches %>% 
  filter(site %in% c("Coogee Beach","Bondi Beach")) %>%  #%in% or operator, c() is a vector
arrange(-beachbugs) %>% View()

# summarising the data 
cleanbeaches %>% 
  filter(site %in% c("Coogee Beach","Bondi Beach")) %>%  
  arrange(-beachbugs) %>% 
  group_by(site) %>% 
  summarise(maxbugs=max(beachbugs)) %>% View()  #summarise doesn't work when there are NA values in the data

cleanbeaches %>% 
  filter(site %in% c("Coogee Beach","Bondi Beach")) %>%  
  arrange(-beachbugs) %>% 
  group_by(site) %>% 
  summarise(maxbugs=max(beachbugs,na.rm = TRUE),
            
            meanbugs=mean(beachbugs,na.rm=TRUE),sdbugs=sd(beachbugs,na.rm = TRUE)) %>% View()  # na.rm remove NA values from the data


# Summarising the above for every beaches
cleanbeaches %>% 
  arrange(-beachbugs) %>% 
  group_by(site) %>% 
  summarise(maxbugs=max(beachbugs,na.rm = TRUE),medianbeaches=median(beachbugs,na.rm=TRUE),meanbugs=mean(beachbugs,na.rm=TRUE),
            sdbugs=sd(beachbugs,na.rm = TRUE)) %>% 
  arrange(-meanbugs)

# comparing councils
cleanbeaches %>% distinct(council)
  
cleanbeaches %>% 
  group_by(council) %>% 
  summarise(meanbugs=mean(beachbugs,na.rm=TRUE),medianbugs=median(beachbugs,na.rm=TRUE))
   #grouping council and site 
cleanbeaches %>% distinct(council)

council_by_site<-cleanbeaches %>%                                                                        #assigned this table to council_by_site
  group_by(council,site) %>% 
  summarise(meanbugs=mean(beachbugs,na.rm=TRUE),medianbugs=median(beachbugs,na.rm=TRUE)) 
View(council_by_site)

# Compute new variables-----------
glimpse(cleanbeaches)     #note that date is in charachter form, need to change the format

cleanbeaches %>% separate(date,c("day","month","year")) %>%  View() #separates the data in to three columns and removes the original column

cleanbeaches %>% separate(date,c("day","month","year"),remove = FALSE) %>% glimpse() #separates the data in to three columns and keeps the original column

#unite opposite of separate

cleanbeaches %>% 
  unite(council_state,c("council","site"),remove = FALSE)

# Using mutate to compute numeric value

cleanbeaches %>% 
  mutate(beachbugsdiff=beachbugs-log(beachbugs))

#using mutate to compute logical values

cleanbeaches %>% 
  mutate(buggier=beachbugs>mean(beachbugs,na.rm=TRUE))  

# to have a value in your environment using $ dollar sign

meanbugs=mean(cleanbeaches$beachbugs,na.rm=TRUE)  #meanbugs=mean(datasourcse$column_to_be_computed)

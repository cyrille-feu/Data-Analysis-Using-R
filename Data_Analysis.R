library(haven)
library(abind)
library(dplyr)
library(forcats)
library(ggplot2)
library(purrr)
library(stringr)
library(tibble)
library(tidyr)
library(tidyselect)

#function to read the data
Read_all_data <-function(i){
  setwd("/media/mydata/AIMS-CAMEROON/Data Analysis with R/week 1/assignment1/m99R31rRB_data_analysis_R/data")
  data=list.files( pattern = "sav")
  read_sav(data[i])
}

# loop to read all the data
list_dat=list()
for (i in 1:9) {
  list_dat[[i]] <- Read_all_data(i)
}

# create an atomic object with all our choose variables in both dataset(individual and country level)
var_slc_i<-c("name","cntry","chldhm","domicil","pdwrk","dsbld","dsbldp","vote","happy","wkhtot")
var_slc_cl<-c("cntry","c_gnipc_2014","c_hditr_2014")

#check if all the variables are in all the data
map(list_dat,function(x){
  names(x) %in% var_slc_i %>%
    sum()
})

# create a smalldataset containing only our selected variables
small_list1<-map(list_dat[1:8],function(x){
  select(x,var_slc_i)
})

#merge all the individual data
dataf2 <-reduce(small_list1,rbind)

#replace differents names of the waves by year of record data and variable name by year
old_name=c("ESS1e06_6","ESS2e03_6","ESS3e03_7","ESS4e04_5","ESS5e03_4","ESS6e02_4","ESS7e02_2","ESS8e02_1")
new_name=c("2002","2004","2006","2008","2010","2012","2014","2016")
for (j in 1:8){
  dataf2$name[dataf2$name==old_name[j]]<-new_name[j] 
}

#rename variable name by year
colnames(dataf2)[1] <- "year"

# select country level variables
small_list2<-select(list_dat[[9]],var_slc_cl)

#Checking if keys are unique in table
small_list2 %>%
  count(cntry) %>%
  filter(n > 1)

#mergin individual and country level data
data_f<-full_join(dataf2, small_list2, by = "cntry")

#check missing key values
data_f %>% count(cntry) %>% filter(n > 1)

# check all the categorical variable
x<-attributes(data_f)$names
for (k in 1:length(x)){
  print(table(data_f[x[k]]))
}
#Coding categorical variable as factor

#All labels  
domocil=c("A big city","The suburbs or outskirts of a big city","A town or a small city","A country village","A farm or home in the countryside")
chldhm=c("Yes","No")
pdwrk=c("No","Yes")
dsbld=c("No","Yes")
dsbldp=c("No","Yes")
vote=c("Yes","No","Not eligible to")
happy=c("degre-0","degre-1","degre-2","degre-3","degre-4","degre-5","degre-6","degre-7","degre-8","degre-9","degre-10")

#list containing all labels
list_label=list(chldhm,domocil,pdwrk,dsbld,dsbldp,vote,happy)
for(h in 1:length(list_label)){
  data_f[[x[h+2]]] <- factor(data_f[[x[h+2]]],labels = list_label[[h]])
}

# Coding continuous variables
qplot(data_f$wkhtot)
qplot(data_f$c_gnipc_2014)
qplot(data_f$c_hditr_2014)

# summarize of our final data
summary(data_f)

#remove the missing data
data_f <- data_f %>% filter(complete.cases(.))

#Comparing happy using "fill"
ggplot(data_f, aes(year, fill = happy)) +
  geom_bar(position = "dodge")

ggplot(data_f, aes(year, fill = happy)) +
  geom_bar(position = "fill")

ggplot(data_f, aes(cntry, fill = happy)) +
  geom_bar(position = "fill")


#Comparing vote using “dodge” and "fill"
ggplot(data_f, aes(year, fill = vote)) +
  geom_bar(position = "dodge")

ggplot(data_f, aes(year, fill = vote)) +
  geom_bar(position = "fill")

ggplot(data_f, aes(cntry, fill = vote)) +
  geom_bar(position = "dodge")

ggplot(data_f, aes(cntry, fill = vote)) +
  geom_bar(position = "fill")


#check happynes take in account other variables
ggplot(data_f, aes(happy, fill = cntry)) +   
  geom_bar(position = "fill")

ggplot(data_f, aes(happy, fill = chldhm)) +   
  geom_bar(position = "dodge")

ggplot(data_f, aes(happy, fill = pdwrk)) +    
  geom_bar(position = "fill")

ggplot(data_f, aes(happy, fill = domicil)) +
  geom_bar(position = "dodge")

ggplot(data_f, aes(happy, fill = dsbldp)) +
  geom_bar(position = "fill")

#check vote take in account other variables
ggplot(data_f, aes(vote, fill = cntry)) +   
  geom_bar(position = "fill")

ggplot(data_f, aes(vote, fill = chldhm)) +   
  geom_bar(position = "fill")

ggplot(data_f, aes(vote, fill = pdwrk)) +    
  geom_bar(position = "fill")

ggplot(data_f, aes(vote, fill = domicil)) +
  geom_bar(position = "dodge")

ggplot(data_f, aes(vote, fill = domicil)) +
  geom_bar(position = "fill")

ggplot(data_f, aes(vote, fill = dsbldp)) +
  geom_bar(position = "dodge")

# barchat vote
ggplot(data_f, aes(vote)) +
  geom_bar()

#barchat domicil
ggplot(data_f, aes(domicil)) +
  geom_bar()

# pie chart happy
ggplot(data=data_f,mapping =aes(fill=happy,x="")) +
  geom_bar(width = 1)+theme_grey()+coord_polar(theta = "y")

#boxplot for numerical variable
ggplot(data_f, aes(reorder(happy, c_gnipc_2014, FUN = median), c_gnipc_2014)) +
  geom_boxplot()

ggplot(data_f, aes(reorder(happy, c_gnipc_2014, FUN = median), c_gnipc_2014)) +
  geom_boxplot()

ggplot(data_f, aes(reorder(happy, c_hditr_2014, FUN = median), c_hditr_2014)) +
  geom_boxplot()

ggplot(data_f, aes(reorder(vote, c_hditr_2014, FUN = median), c_hditr_2014)) +
  geom_boxplot()

ggplot(data_f, aes(reorder(vote, wkhtot, FUN = median), wkhtot)) +
  geom_boxplot()

ggplot(data_f, aes(reorder(happy, wkhtot, FUN = median), wkhtot)) +
  geom_boxplot()

ggplot(data_f, aes(reorder(happy, c_gnipc_2014 ,FUN = median), c_gnipc_2014)) +
  geom_boxplot()

View(data_f)






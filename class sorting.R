#set working directory
setwd("C:/Users/EMMANUEL/Documents/my projects")

#load readxl function
library(readxl)

#Import excel dataset
excel_sheets("listalphaorder2021.xlsx")
read_excel("listalphaorder2021.xlsx")
alphaorderlist <- read_excel("listalphaorder2021.xlsx", skip = 8)
alphaorderlist

#Load libraries
library(dplyr)
library(tidyr)
library(stringr)

#rename serial number column
alphaorderlist <- alphaorderlist %>% rename(s_no = `S/No`)

#separte the class teachers from the class
donn <- alphaorderlist %>%
  separate(CLASS, into = c("class", "teacher"), sep = "--")
donn

#Remove class alphabets from classes      
alphaorderclassonly <-donn %>%
  mutate(class = str_remove(class, "[A-Z]\\s$"))
alphaorderclassonly
alphaorderclassonly <- alphaorderclassonly %>%
  separate(NAME, into = c("Surname", "First_name", "Other_names"), sep = " ")
alphaorderclassonly

#check for distinct classes
alphaorderclassonly %>%
  count(class)

#gets the count of people whose surname appear more than once(they are most likely siblings)
same_surname <- alphaorderclassonly %>%
  count(Surname) %>%
  filter(n > 1) %>%
  arrange(desc(n))
same_surname
#summing helps to know the total number of students in this category
sum(same_surname$n)

#this shows(filters) the students with siblings
siblings <- alphaorderclassonly %>%
  filter(Surname %in% same_surname$Surname)
siblings

#gets the count of people who dont share surname
dif_surname <- alphaorderclassonly %>%
  count(Surname)%>%
  filter(n==1)
dif_surname

#shows(filters) the students without siblings
no_siblings <- alphaorderclassonly %>%
  filter(Surname %in% dif_surname$Surname)
no_siblings

#Initial Grouping Process
alphaorderclassonly$groups <- NA
surname_counter <- 1

for(name in alphaorderclassonly$Surname){
  name_holder <- alphaorderclassonly$Surname[1]
  if (name == name_holder){
    alphaorderclassonly$groups[[surname_counter]] <- "group a"
    surname_counter <- surname_counter + 1
    next
  }
  else{
    surname_counter <- surname_counter + 1
    name_holder <- alphaorderclassonly$Surname[surname_counter]
    if (name == name_holder){
      alphaorderclassonly$groups[[surname_counter]] <- "group b"
    }
    else{
      break
    }
  }
}
alphaorderclassonly

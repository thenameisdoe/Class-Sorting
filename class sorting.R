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

#separate the class teachers from the class
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

#this shows(filters) the students with siblings
siblings <- alphaorderclassonly %>%
  filter(Surname %in% same_surname$Surname)
siblings

#Creates a new column to initialize grouping
same_surname$groups <- NA

#Assigns numbers to the respective distinct surnames
same_surname$groups <- 1:nrow(same_surname)
same_surname

#summing helps to know the total number of students in this category
sum(same_surname$n)

#Left join to assign the groups to the table that contains the names of children with siblings 
sm_join_with_siblings <-left_join(same_surname, siblings, by = "Surname")
sm_join_with_siblings

#Gets the count of people who dont share surname
dif_surname <- alphaorderclassonly %>%
  count(Surname)%>%
  filter(n==1)
dif_surname

#shows(filters) the students without siblings
no_siblings <- alphaorderclassonly %>%
  filter(Surname %in% dif_surname$Surname)
no_siblings

#Initial Grouping Process for non siblings
no_siblings$groups <- NA

#Assign numbers to the groups
no_siblings$groups <- 1: nrow(no_siblings)
no_siblings

#Removing unwanted columns from the two tables
sm_join_with_siblings <- sm_join_with_siblings %>%
  select(-n, -teacher, -SCHOLARSHIP, -`ON REBATE?`, -`COMPUTER No`)
sm_join_with_siblings

no_siblings <- no_siblings%>%
  select(-teacher, -SCHOLARSHIP, -`ON REBATE?`, -`COMPUTER No`)
no_siblings

#Joining the two tables
full_grouped_table <- full_join(sm_join_with_siblings, no_siblings)
full_grouped_table <- full_grouped_table %>% arrange(Surname)

#Separating the groups into even number groups and odd number groups (even = a, odd = b)
full_grouped_table$groups_alpha <- NA

i <- 1
for(i in full_grouped_table$groups){
  if(full_grouped_table$groups[i] %% 2 == 0){
    full_grouped_table$groups_alpha[i] <- "group a"
  }else{
    full_grouped_table$groups_alpha[i] <- "group b"
    next
  }
  i <- i + 1
}
full_grouped_table

#Separating groups
group_a_pupils <- full_grouped_table %>%
  filter(groups_alpha == "group a")
group_a_pupils

group_b_pupils <- full_grouped_table %>%
  filter(groups_alpha == "group b")
group_b_pupils

#Filter for different class
kg_1_group_a <- group_a_pupils %>%
  filter(class == "KG 1")
kg_1_group_a

kg_1_group_b <- group_b_pupils %>%
  filter(class == "KG 1")
kg_1_group_b

kg_2_group_a <- group_a_pupils %>%
  filter(class == "KG 2")
kg_2_group_a
#and so on
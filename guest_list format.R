##This program was created on 8/14/24 by Neil Yetz.
## The purpose of this program is to format Karina and Neil's wedding guest list to fit the standards for being imported into theknot.com.
#All data and code is meant to be stored in the same file with a corresponding R-project

#Load Libraries
library(tidyverse)
library(readxl)
library(writexl)

#Clear environment
rm(list=ls())

#Create Functions
##match_paste() will match names based on a matching variable (Used to match by dyad_ID later)
match_paste <- function(v) {
  Reduce(f=paste, x = v)
}

#Read "must haves" for Karina and Neil
karina <- read_excel("wedding guestlist - with must list.xlsx", sheet = "Karina's_Must", skip = 2)
neil   <- read_excel("wedding guestlist - with must list.xlsx", sheet = "Neil's_Must"  , skip = 2)


karina <- karina %>% mutate_if(is.numeric, as.character)
neil <- neil %>% mutate_if(is.numeric, as.character)


#Bind rows for full list of both Neil and Karina's "Must haves"
all <- bind_rows(karina, neil)


#combine couples so that it reads as "First name1 & First name2 Last Name" for couples with the same first name 
#or "First Name1 Last Name1 & First Name2 Last Name 2" for couples with different last names 
couples <- all %>% 
  filter(dyad == TRUE) %>% 
  arrange(dyad_ID, ID) %>% 
  group_by(dyad_ID, last_name,  grp = with(rle(last_name), rep(seq_along(lengths), lengths))) %>% 
  mutate(duplicate = seq_along(grp)) %>% 
  ungroup() %>% 
  mutate(last_name = ifelse(duplicate == 2, "", last_name),
         order = ifelse(last_name == "", 1, 2)) %>% 
  arrange(dyad_ID, order) %>% 
  mutate(full_name = paste(first_name, last_name, "&")) #Will remove unnecessary final "&" in later step

couples <- couples %>% 
  group_by(dyad_ID) %>% 
  summarize(couple_name = match_paste(as.character(full_name))) %>%
  merge(., couples, by = 'dyad_ID') %>% 
  ungroup() %>% 
  group_by(dyad_ID, grp = with(rle(couple_name), rep(seq_along(lengths), lengths))) %>% 
  mutate(duplicate = seq_along(grp)) %>% 
  ungroup() %>% 
  filter(duplicate == 1) %>% 
  select(-grp ,-duplicate, -order, -full_name) %>% 
  rename(full_name = "couple_name")

#Removes a final "&" that I created earlier
couples$full_name = substr(couples$full_name, 1, nchar(couples$full_name)-1)

#preserve original order of names
names <- names(all)

#Remove dyads to add in the new dyad list above. Reorder to preserve original column order
no_dyad <- all %>% 
  filter(dyad == FALSE) %>% 
  select(all_of(names)) %>% 
  mutate(full_name = paste(first_name, last_name)) #Recreated full_name to be first name last name

#Reorder couples columns to match
couples <- couples %>% 
  select(all_of(names))

#bind couples and no_dyads dataset
all_cd <- bind_rows(couples, no_dyad)

#Set up for Knot list
all_cd <- all_cd %>% 
  mutate_if(is.numeric, as.character) %>% 
  mutate_all(~replace_na(., "")) %>% 
  mutate(`Street Address` = paste(number, street),
         full_name = ifelse(plus_one == 1,    paste(full_name, "& guest"), full_name), #adding "& plus one to guests with a plus one"
         full_name = ifelse(children == TRUE, paste(full_name, "+ children"), full_name)) %>%  #, #Adding "+ children" to guests with children
  mutate_all(~str_squish(.)) %>% 
  mutate(zip = ifelse(country == "USA" & nchar(zip) < 5, str_pad(zip, width = 5, side = "left", pad ="0"), zip),
         zip = ifelse(zip == "00000", "", zip))


#Final Knot list
knot_final <- all_cd %>% 
  select(`Guest Names (Required*)` = "full_name", 
         `Phone Number` = "phone",
         `Email Address` = "email",
         `Street Address`,
         `Apt/Suite`,
         City = "city",
         State = "state", 
         Zip = "zip")

#Write final knot list to excel file
write_xlsx(knot_final, "Final_Knot_List.xlsx", format_headers = FALSE)


##Special list for Neil's Mom to fill out. Only contains Neil's family members.
#mom <- all_cd %>% 
#  filter(family_side == "Neil") %>% 
#  select(`Guest Names (Required*)` = "full_name", 
#         `Phone Number` = "phone",
#         `Email Address` = "email",
#         `Street Address`,
#         `Apt/Suite`,
#         City = "city",
#         State = "state", 
#         Zip = "zip")

#export file for mom
#write_xlsx(mom, "Family_List_for_mom.xlsx", format_headers = TRUE)

#Program last updated on 8/15/24 at 10:49am by Neil Yetz
#End Program
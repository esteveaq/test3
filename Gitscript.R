library(tidyverse)

#LOAD  ------------------------------------------------------------------
list.files()
maisons <- read_csv("antibes_architectes.csv")

#EXPLORE ---------------------------------------------------------------

glimpse(maisons) #glimpse
names(maisons) #get all names of cols
str(maisons) #get all types of cols
summary(maisons) #get data summary (length, min, max, etc...)
colSums(is.na(maisons)) #count missing values
plot(maisons)

#GGally for numeric data exploration (not useful in our case)
install.packages("GGally")          
library("GGally")                   
maisons %>%
  select(Coll_Nom,`date campagne initiale de construction`) %>%
  ggpairs(maisons)

#Base R table and bar chart
table(maisons$`date campagne initiale de construction`)
barplot(table(maisons$`date campagne initiale de construction`))

table(maisons$`campagne initiale de construction`, maisons$`Nom architecte`)
barplot(table(maisons$`campagne initiale de construction`, maisons$`Nom architecte`))

#Hist & boxplot
hist(maisons$`date campagne initiale de construction`)
hist(maisons$`date campagne initiale de construction`, breaks = 15)
boxplot(maisons$`date campagne initiale de construction`)
boxplot(maisons$`date campagne initiale de construction`~ maisons$`Nom architecte`)

# Warning : 
# The ggplot default for histograms is to use right-closed intervals, 
# whereas the default for hist(...) is left closed intervals. 
# Also, ggplot uses a different algorithm for calculating the x-axis breaks 
# and limits.

#CLEAN ------------------------------------------------------------------

#clean cols
maisons2 <- 
  maisons %>%
  select(2,13) %>% #remove other col
  rename("siret" = 1 , "date_construction" = 2) #rename col by position

#rename col with pure R
colnames(maisons2) = c("siret", "date_construction")

# Remove NA 
maisons2 %>% 
  filter(!is.na(date_construction)) %>% #remove NA from col 
  print()
         
#Better way to remove NA
maisons2 %>%
  drop_na(date_construction)

#Saving csv -----------------------------------------------------------------
maisons2 %>%
  write_csv("maisons_data") 

#Total load & cleaning ------------------------------------------------------
maisons <- read_csv("antibes_architectes.csv")
maisons2 <- 
  maisons %>%
  select(2,13) %>% #remove other col
  rename("siret" = 1 , "date_construction" = 2) %>% #rename col by position
  drop_na(date_construction) %>%
  write_csv("maisons_data") %>%
  print()

#ANALYZE -------------------------------------------------------------------
a1 <- 
  maisons2 %>%
  count(date = date_construction) %>% #count by date
  mutate(période = cut(date, breaks = c(1930, 1950,1975,2000), labels = c("1930-1950","1950-1975","1975-2000"))) #bin in periods

g1 <- 
a1 %>%
ggplot(aes(x = période, y = n)) +
  geom_col()
print(g1)

g2 <-           
a1 %>%
  ggplot(aes(x = date, y = n)) +
  geom_col()
print(g2)

g3 <-           
  a1 %>%
  ggplot(aes(x = date)) +
  geom_histogram(bins = 10)
print(g3)


#Making a change by doing a comment (UI version)
#Comment (CLI)

# Branch 2 comment (CLI) edit 
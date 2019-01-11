################################################################
#**************************************************************#
#--------------------------------------------------------------#
######### Dissertation Artifact: Nishad Abdul Latheef ##########
##------ Market Basket Analysis using Apriori Algorithm ------##
#--------------------------------------------------------------#
#**************************************************************#
################################################################

#Command used to extract and add readxml
#install.packages("readxml")
library(readxl)

#Command used to extract and add tidyverse
#install.packages("tidyverse")
library(tidyverse)

#Command used to extract and add knitr
#install.packages("knitr")
library(knitr)

#Command used to add ggplot2
library(ggplot2)

#Command used to extract and add lubridate
#install.packages("lubridate")
library(lubridate)

#Command used to extract and add plyr
#install.packages("plyr")
library(plyr)

#Command used add dplyr 
library(dplyr)

#Command used to extract and add package arules
#install.packages("arules")
library(arules)

#Command used to extract and add arulesViz
#install.packages("arulesViz")
library(arulesViz)

################################################################
#--------------------------------------------------------------#
# Step 1: Data Preparation
#--------------------------------------------------------------#
################################################################

#Command used for reading the given dataset in excel format into
#R dataframe
retailData <- read_excel('Online_Shopping.xlsx')
#The data might have missing values

#The fuction complete.cases(data) will provide a logical vector 
#representing that rows which have no missing values. 
retailData <- retailData[complete.cases(retailData), ]

products <- unique(retailData[3]) 
write.csv(products,"products.csv", 
          quote = FALSE, row.names = FALSE)

#The in-built mutate function is a part of dplyr package. It is 
#utilised to change the stucture of columns in a dataframe.Here,
#Description column is transformed into factor column.
retailData %>% mutate(Description = as.factor(Description))
retailData %>% mutate(Country = as.factor(Country))

#Command used to transform character data to date format.
retailData$DateOfPurchase <- as.Date(retailData$InvoiceDate)
#Command used to obtain the time from the column InvoiceDate 
#and keep it in a fresh variable
TimeOfPurchase <- format(retailData$InvoiceDate,"%H:%M:%S")
#Command used to change the format of InvoiceNo into numeric
InvoiceNo <- as.numeric(as.character(retailData$InvoiceNo))

#Command used to attach newly formed columns TimeOfPurchase and
#InvoiceNo into the given dataframe retailData
cbind(retailData,TimeOfPurchase)
cbind(retailData,InvoiceNo)

#Command used to take a peek at the data
glimpse(retailData)
# 406,829 Observastions and 9 variables

#Command used to add plyr
library(plyr)
#Command used to build the transaction data
transaction_Details<- ddply(retailData,
                            c("InvoiceNo","DateOfPurchase"),
                            function(df1)paste(df1$Description,
                                               collapse = ","))
#Checking the result
transaction_Details

#The unwanted columns are removed
#The column InvoiceNo is removed from transaction_Details  
transaction_Details$InvoiceNo <- NULL
#The column DateOfPurchase is removed transaction_Details
transaction_Details$DateOfPurchase <- NULL

#The column is renamed to item_list
colnames(transaction_Details) <- c("item_list")

#The Dataframe transaction_Details is viewed
transaction_Details

#The output of the transaction is stored as in a csv format 
write.csv(transaction_Details,"transactions_output.csv", 
          quote = FALSE, row.names = FALSE)
#The data is prepared for modelling

################################################################
#--------------------------------------------------------------#
# Step 2: Rule Generation
#--------------------------------------------------------------#
################################################################

#The csv file is loaded into a variable Tran
Tran <- read.transactions('transactions_output.csv', 
                          format = 'basket', sep=',')

#The dataframe is viewed
Tran

#Thhe summary of Tran is viewed
summary(Tran)

#The association rules are generated using apriori function by
# setting Minimum Support as 0.001 and  confidence as 0.75.
Generated_Rules <- apriori(Tran, parameter = list(supp=0.001,
                                                    conf=0.75,
                                                    maxlen=10))


#The summary of generated rules is viewed
summary(Generated_Rules)

################################################################
#--------------------------------------------------------------#
# Step 3: Evaluation
#--------------------------------------------------------------#
################################################################

#The top 50 rules are inspected 
rules1 <- inspect(Generated_Rules[1:50])

#The rules are stored in a file
write.csv(rules1,"Rules_1.csv", 
          quote = FALSE, row.names = TRUE)


#Making the rules stronger by limiting the number of items and
# increasing the minimum support
Stronger.Generated_Rules <- apriori(Tran, 
                                   parameter = list(supp=0.001,
                                                    conf=0.8,
                                                    maxlen=3))
#Stronger rules are generated

#The top 50 rules are inspected 
rules_Strong <- inspect(Stronger.Generated_Rules[1:50])

#The rules are stored in a file
write.csv(rules_Strong,"Strong_Rules.csv", 
          quote = FALSE, row.names = TRUE)

#Finding the redundant rule
subset.rules <- which(colSums(is.subset(Generated_Rules, 
                                        Generated_Rules)) > 1)

#Removing unwanted rules
new_Generated_Rules. <- Generated_Rules[-subset.rules]

#Discovering Rules related to a particular item
Focused.Generated_Rules <- apriori(Tran, 
                                   parameter = list(supp=0.001,
                                                    conf=0.8),
                                   appearance=list(default="lhs"
                                                   ,rhs="COFFEE"))
#Inspecting the focused rules
inspect(Focused.Generated_Rules)

#Discovering Rules related to a particular item
Focused.Generated_Rules <- apriori(Tran, 
                                   parameter = list(supp=0.001,
                                                    conf=0.8),
                                   appearance=list(lhs="BLACK TEA"
                                                   ,default="rhs"))
#Inpecting the focused rules
inspect(Focused.Generated_Rules)


################################################################
#--------------------------------------------------------------#
# Step 4: Deployment
#--------------------------------------------------------------#
################################################################

# The rules that have confidence greater than 50% is filtered out
Fil.Rules<-Generated_Rules[quality(Generated_Rules)$confidence>0.5]

inspect(Fil.Rules[1:10])
#the filtered rules are plotted
plot(Fil.Rules)

#Plotting the two-key method
plot(Fil.Rules,method="two-key plot")

#Plotting grouped interative plot
plot(Fil.Rules, method = "grouped", interactive = TRUE)

#Filtering top 10 rules 
Fil.rules_2 <- head(Generated_Rules, n = 10, by = "lift")

#Plotting Graph based visualisation
plot(Fil.rules_2, method = "graph", engine = "htmlwidget")

#Saving the graph file
saveAsGraph(head(Fil.Rules, n = 1000, by = "lift"), 
            file = "Graph_of_rules.graphml")

#The top 30 rules that have the highest lift is selected
Fil.rules_3 <- head(Fil.Rules, n=30, by="lift")

#plotting Parallel Coordinates
plot(Fil.rules_3, method="paracoord")


################################################################
#----------------------------The End---------------------------#
################################################################

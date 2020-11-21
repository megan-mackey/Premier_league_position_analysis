# load neccesary libraries

library(tidyverse)
library(ggplot2)


# load the data file needed
load("Data/processed_data_finance_edit.rda")


# Each team has a graph under its name which shows how the payment they receive their total payment
# and it also shows how their payment was categorized. 

# Arsenal 
arsenal <- finance_final %>%
    filter(team == "Arsenal") %>%
    ggplot(aes(x = season, y = Finances, fill = type)) +
    geom_col(position = "stack") +
    geom_point(aes(x = season, y = position)) +
labs(title = "Amount of Money received by Arsenal",
     x = "Season",
     y = "Money Received ( Millions of Pounds)") +
    scale_fill_discrete(name = "Type")

# Aston Villa
aston_villa <- finance_final %>%
    filter(team == "Aston Villa") %>%
    ggplot(aes(x = season, y = Finances, fill = type)) +
    geom_col(position = "stack")+
    geom_point(aes(x = season, y = position)) +
    labs(title = "Amount of Money received by Aston Villa",
         x = "Season",
         y = "Money Received ( Millions of Pounds)") +
    scale_fill_discrete(name = "Type")

# Birmingham
birmingham <- finance_final %>%
    filter(team == "Birmingham") %>%
    ggplot(aes(x = season, y = Finances, fill = type)) +
    geom_col(position = "stack") +
    geom_point(aes(x = season, y = position)) +
    labs(title = "Amount of Money received by Birmingham",
         x = "Season",
         y = "Money Received ( Millions of Pounds)") +
    scale_fill_discrete(name = "Type")
    

# Blackburn
blackburn <-  finance_final %>%
    filter(team == "Blackburn") %>%
    ggplot(aes(x = season, y = Finances, fill = type)) +
    geom_col(position = "stack") +
    geom_point(aes(x = season, y = position)) +
    labs(title = "Amount of Money received by Blackburn",
         x = "Season",
         y = "Money Received ( Millions of Pounds)") +
    scale_fill_discrete(name = "Type")

# Blackpool
blackpool <-  finance_final %>%
    filter(team == "Blackpool") %>%
    ggplot(aes(x = season, y = Finances, fill = type)) +
    geom_col(position = "stack") +
    geom_point(aes(x = season, y = position)) +
    labs(title = "Amount of Money received by Blackpool",
         x = "Season",
         y = "Money Received ( Millions of Pounds)") +
    scale_fill_discrete(name = "Type")

# Bolton
bolton <- finance_final %>%
    filter(team == "Bolton") %>%
    ggplot(aes(x = season, y = Finances, fill = type)) +
    geom_col(position = "stack") +
    geom_point(aes(x = season, y = position)) +
    labs(title = "Amount of Money received by Bolton",
         x = "Season",
         y = "Money Received ( Millions of Pounds)") +
    scale_fill_discrete(name = "Type")

# Burnley
burnley<-  finance_final %>%
    filter(team == "Burnley") %>%
    ggplot(aes(x = season, y = Finances, fill = type)) +
    geom_col(position = "stack") +
    geom_point(aes(x = season, y = position)) +
    labs(title = "Amount of Money received by Burnley",
         x = "Season",
         y = "Money Received ( Millions of Pounds)") +
    scale_fill_discrete(name = "Type")

# Cardiff
cardiff <-  finance_final %>%
    filter(team == "Cardiff") %>%
    ggplot(aes(x = season, y = Finances, fill = type)) +
    geom_col(position = "stack") +
    geom_point(aes(x = season, y = position)) +
    labs(title = "Amount of Money received by Cardiff",
         x = "Season",
         y = "Money Received ( Millions of Pounds)") +
    scale_fill_discrete(name = "Type")

# Chelsea
chelsea <-  finance_final %>%
    filter(team == "Chelsea") %>%
    ggplot(aes(x = season, y = Finances, fill = type)) +
    geom_col(position = "stack") +
    geom_point(aes(x = season, y = position)) +
    labs(title = "Amount of Money received by Chelsea",
         x = "Season",
         y = "Money Received ( Millions of Pounds)") +
    scale_fill_discrete(name = "Type")

# Crystal Palace
crystal_palace <-  finance_final %>%
    filter(team == "Crystal Palace") %>%
    ggplot(aes(x = season, y = Finances, fill = type)) +
    geom_col(position = "stack") +
    geom_point(aes(x = season, y = position)) +
    labs(title = "Amount of Money received by Crystal Palace",
         x = "Season",
         y = "Money Received ( Millions of Pounds)") +
    scale_fill_discrete(name = "Type")

# Everton
everton <-  finance_final %>%
    filter(team == "Everton") %>%
    ggplot(aes(x = season, y = Finances, fill = type)) +
    geom_col(position = "stack") +
    geom_point(aes(x = season, y = position)) +
    labs(title = "Amount of Money received by Everton",
         x = "Season",
         y = "Money Received ( Millions of Pounds)") +
    scale_fill_discrete(name = "Type")

# Fulham 
fulham <-  finance_final %>%
    filter(team == "Fulham") %>%
    ggplot(aes(x = season, y = Finances, fill = type)) +
    geom_col(position = "stack") +
    geom_point(aes(x = season, y = position)) +
    labs(title = "Amount of Money received by Fulham",
         x = "Season",
         y = "Money Received ( Millions of Pounds)") +
    scale_fill_discrete(name = "Type")

# Hull
hull<-  finance_final %>%
    filter(team == "Hull") %>%
    ggplot(aes(x = season, y = Finances, fill = type)) +
    geom_col(position = "stack") +
    geom_point(aes(x = season, y = position)) +
    labs(title = "Amount of Money received by Hull",
         x = "Season",
         y = "Money Received ( Millions of Pounds)") +
    scale_fill_discrete(name = "Type")

# Leicester
leicester <-  finance_final %>%
    filter(team == "Leicester") %>%
    ggplot(aes(x = season, y = Finances, fill = type)) +
    geom_col(position = "stack") +
    geom_point(aes(x = season, y = position)) +
    labs(title = "Amount of Money received by Leicester",
         x = "Season",
         y = "Money Received ( Millions of Pounds)") +
    scale_fill_discrete(name = "Type")

#Liverpool
liverpool <-  finance_final %>%
    filter(team == "Liverpool") %>%
    ggplot(aes(x = season, y = Finances, fill = type)) +
    geom_col(position = "stack") +
    geom_point(aes(x = season, y = position)) +
    labs(title = "Amount of Money received by Liverpool",
         x = "Season",
         y = "Money Received ( Millions of Pounds)") +
    scale_fill_discrete(name = "Type")

#Man city
man_city <-  finance_final %>%
    filter(team == "Man City") %>%
    ggplot(aes(x = season, y = Finances, fill = type)) +
    geom_col(position = "stack") +
    geom_point(aes(x = season, y = position)) +
    labs(title = "Amount of Money received by Man City",
         x = "Season",
         y = "Money Received ( Millions of Pounds)") +
    scale_fill_discrete(name = "Type")

# Man United
man_united <-  finance_final %>%
    filter(team == "Man United") %>%
    ggplot(aes(x = season, y = Finances, fill = type)) +
    geom_col(position = "stack") +
    geom_point(aes(x = season, y = position)) +
    labs(title = "Amount of Money received by Man United",
         x = "Season",
         y = "Money Received ( Millions of Pounds)") +
    scale_fill_discrete(name = "Type")

# Newcastle 
newcastle <-  finance_final %>%
    filter(team == "Newcastle") %>%
    ggplot(aes(x = season, y = Finances, fill = type)) +
    geom_col(position = "stack") +
    geom_point(aes(x = season, y = position)) +
    labs(title = "Amount of Money received by Newcastle",
         x = "Season",
         y = "Money Received ( Millions of Pounds)") +
    scale_fill_discrete(name = "Type")

# Norwich 
norwich <-  finance_final %>%
    filter(team == "Norwich") %>%
    ggplot(aes(x = season, y = Finances, fill = type)) +
    geom_col(position = "stack") +
    geom_point(aes(x = season, y = position)) +
    labs(title = "Amount of Money received by Norwich",
         x = "Season",
         y = "Money Received ( Millions of Pounds)")+
    scale_fill_discrete(name = "Type")

# Portsmouth
portsmouth <-  finance_final %>%
    filter(team == "Portsmouth") %>%
    ggplot(aes(x = season, y = Finances, fill = type)) +
    geom_col(position = "stack") +
    geom_point(aes(x = season, y = position)) +
    labs(title = "Amount of Money received by Portsmouth",
         x = "Season",
         y = "Money Received ( Millions of Pounds)") +
    scale_fill_discrete(name = "Type")

#QPR 
qpr <-  finance_final %>%
    filter(team == "QPR") %>%
    ggplot(aes(x = season, y = Finances, fill = type)) +
    geom_col(position = "stack") +
    geom_point(aes(x = season, y = position)) +
    labs(title = "Amount of Money received by QPR",
         x = "Season",
         y = "Money Received ( Millions of Pounds)") +
    scale_fill_discrete(name = "Type")

# Reading
reading <-  finance_final %>%
    filter(team == "Reading") %>%
    ggplot(aes(x = season, y = Finances, fill = type)) +
    geom_col(position = "stack") +
    geom_point(aes(x = season, y = position)) +
    labs(title = "Amount of Money received by Reading",
         x = "Season",
         y = "Money Received ( Millions of Pounds)") +
    scale_fill_discrete(name = "Type")

# Southampton
southampton <-  finance_final %>%
    filter(team == "Southampton") %>%
    ggplot(aes(x = season, y = Finances, fill = type)) +
    geom_col(position = "stack") +
    geom_point(aes(x = season, y = position)) +
    labs(title = "Amount of Money received by Southampton",
         x = "Season",
         y = "Money Received ( Millions of Pounds)") +
    scale_fill_discrete(name = "Type")

# Stoke
stoke <-  finance_final %>%
    filter(team == "Stoke") %>%
    ggplot(aes(x = season, y = Finances, fill = type)) +
    geom_col(position = "stack") +
    geom_point(aes(x = season, y = position)) +
    labs(title = "Amount of Money received by Stoke",
         x = "Season",
         y = "Money Received ( Millions of Pounds)") +
    scale_fill_discrete(name = "Type")

# sunderland
sunderland <-  finance_final %>%
    filter(team == "Sunderland") %>%
    ggplot(aes(x = season, y = Finances, fill = type)) +
    geom_col(position = "stack") +
    geom_point(aes(x = season, y = position)) +
    labs(title = "Amount of Money received by Sunderland",
         x = "Season",
         y = "Money Received ( Millions of Pounds)") +
    scale_fill_discrete(name = "Type")

#Swansea
swansea <-  finance_final %>%
    filter(team == "Swansea") %>%
    ggplot(aes(x = season, y = Finances, fill = type)) +
    geom_col(position = "stack") + 
    geom_point(aes(x = season, y = position)) +
    labs(title = "Amount of Money received by Swansea",
         x = "Season",
         y = "Money Received ( Millions of Pounds)") +
    scale_fill_discrete(name = "Type")

# Tottenham
tottenham <-  finance_final %>%
    filter(team == "Tottenham") %>%
    ggplot(aes(x = season, y = Finances, fill = type)) +
    geom_col(position = "stack") +
    geom_point(aes(x = season, y = position)) +
    labs(title = "Amount of Money received by Tottenham",
         x = "Season",
         y = "Money Received ( Millions of Pounds)") +
    scale_fill_discrete(name = "Type")

# West brom
west_brom <-  finance_final %>%
    filter(team == "West Brom") %>%
    ggplot(aes(x = season, y = Finances, fill = type)) +
    geom_col(position = "stack") +
    geom_point(aes(x = season, y = position)) +
    labs(title = "Amount of Money received by West Brom",
         x = "Season",
         y = "Money Received ( Millions of Pounds)") +
    scale_fill_discrete(name = "Type")

# West Ham
west_ham <-  finance_final %>%
    filter(team == "West Ham") %>%
    ggplot(aes(x = season, y = Finances, fill = type)) +
    geom_col(position = "stack") +
    geom_point(aes(x = season, y = position)) +
    labs(title = "Amount of Money received by West Ham",
         x = "Season",
         y = "Money Received ( Millions of Pounds)") +
    scale_fill_discrete(name = "Type")

# Wigan 
wigan <-  finance_final %>%
    filter(team == "Wigan") %>%
    ggplot(aes(x = season, y = Finances, fill = type)) +
    geom_col(position = "stack") +
    geom_point(aes(x = season, y = position)) +
    labs(title = "Amount of Money received by Wigan",
         x = "Season",
         y = "Money Received ( Millions of Pounds)") +
    scale_fill_discrete(name = "Type")

# Wolves
wolves <-  finance_final %>%
    filter(team == "Wolves") %>%
    ggplot(aes(x = season, y = Finances, fill = type)) +
    geom_col(position = "stack") +
    geom_point(aes(x = season, y = position)) +
    labs(title = "Amount of Money received by Wolves",
         x = "Season",
         y = "Money Received ( Millions of Pounds)") + 
    scale_fill_discrete(name = "Type")



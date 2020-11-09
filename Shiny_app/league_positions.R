
library(tidyverse)
library(ggplot2)
library(gganimate)


load("Data/processed_data.rda")

# Plot League Positions over 2004-2005 season

 season_2004_2005 <- df_all %>%
  filter(season == "2004-2005") %>%
  
  ggplot(aes(x = num_match , y = position, color = team)) +
  geom_line()+
  labs(title = "Change in position over the 2004-2005 season", 
       x = " Number of match", y = "League Position")+
    transition_reveal(date)
 
 # Plot League Positions over 2005-2006 season
 
 season_2005_2006 <- df_all %>%
   filter(season == "2005-2006") %>%
   
   ggplot(aes(x = num_match , y = position, color = team)) +
   geom_line()+
   labs(title = "Change in position over the 2005-2006 season", 
        x = " Number of match", y = "League Position") +
    transition_reveal(date)
    
 
 # Plot League Positions over 2006-2007 season
 
 season_2006_2007 <- df_all %>%
    filter(season == "2006-2007") %>%
    
    ggplot(aes(x = num_match , y = position, color = team)) +
    geom_line()+
    labs(title = "Change in position over the 2006-2007 season", 
         x = " Number of match", y = "League Position") +
    transition_reveal(date)
 
 
 # Plot League Positions over 2007-2008 season
 
 season_2007_2008 <- df_all %>%
   filter(season == "2007-2008") %>%
   
   ggplot(aes(x = num_match , y = position, color = team)) +
   geom_line()+
   labs(title = "Change in position over the 2007-2008 season", 
        x = " Number of match", y = "League Position")+
    transition_reveal(date)
 
 # Plot League Positions over 2008-2009 season
 
 season_2008_2009 <- df_all %>%
   filter(season == "2008-2009") %>%
   
   ggplot(aes(x = num_match , y = position, color = team)) +
   geom_line()+
   labs(title = "Change in position over the 2008-2009 season", 
        x = " Number of match", y = "League Position") +
    transition_reveal(date)
 
 # Plot League Positions over 2009-2010 season

 season_2009_2010 <- df_all %>%
   filter(season == "2009-2010") %>%
   
   ggplot(aes(x = num_match , y = position, color = team)) +
   geom_line()+
   labs(title = "Change in position over the 2009-2010 season", 
        x = " Number of match", y = "League Position") +
    transition_reveal(date)
 
 # Plot League Positions over 2010-2011 season
 
 season_2010_2011 <- df_all %>%
   filter(season == "2010-2011") %>%
   
   ggplot(aes(x = num_match , y = position, color = team)) +
   geom_line()+
   labs(title = "Change in position over the 2010-2011 season", 
        x = " Number of match", y = "League Position") +
    transition_reveal(date)
 
 # Plot League Positions over 2011-2012 season 

 season_2011_2012 <- df_all %>%
   filter(season == "2011-2012") %>%
   
   ggplot(aes(x = num_match , y = position, color = team)) +
   geom_line()+
   labs(title = "Change in position over the 2011-2012 season", 
        x = " Number of match", y = "League Position") +
    transition_reveal(date)
 
 # Plot League Positions over 2012-2013 season
 
 season_2012_2013 <- df_all %>%
   filter(season == "2012-2013") %>%
   
   ggplot(aes(x = num_match , y = position, color = team)) +
   geom_line()+
   labs(title = "Change in position over the 2012-2013 season", 
        x = " Number of match", y = "League Position") +
    transition_reveal(date)
 
 # Plot League Positions over 2013-2014 season
 
 season_2013_2014 <- df_all %>%
   filter(season == "2013-2014") %>%
   
   ggplot(aes(x = num_match , y = position, color = team)) +
   geom_line()+
   labs(title = "Change in position over the 2013-2014 season", 
        x = " Number of match", y = "League Position") +
    transition_reveal(date)
 
 # Plot League Positions over 2014-2015 season 
 
 season_2014_2015 <- df_all %>%
   filter(season == "2014-2015") %>%
   
   ggplot(aes(x = num_match , y = position, color = team)) +
   geom_line()+
   labs(title = "Change in position over the 2014-2015 season", 
        x = " Number of match", y = "League Position") +
    transition_reveal(date)

  


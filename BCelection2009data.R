# VOTER DEMOGRAPHICS
#
# some background reading material
# http://www.r-bloggers.com/ggplot2-a-little-twist-on-back-to-back-bar-charts/
# http://learnr.wordpress.com/2009/09/24/ggplot2-back-to-back-bar-charts/
# http://svitsrv25.epfl.ch/R-doc/library/Hmisc/html/histbackback.html
# 
#
# install necessary packages
library("ggplot2")
library("plyr")
library("reshape2")
library("vcd")
#
library(wesanderson)
# for more on the Wes Anderson colour palette:
# https://github.com/karthik/wesanderson#wes-anderson-palettes
# http://blog.revolutionanalytics.com/2014/03/give-your-r-charts-that-wes-anderson-style.html
#
#
# set working directory
setwd("I:/10_External Clients/Elections BC/1612 Exploring non-voters/2. voter demographics/report/graphics_R")
#
# read the source data
BCdata <- read.csv("BCvoterdemog2009.csv", header=TRUE)
str(BCdata)
#
#
# PREPARE THE DATA
# reshape the data into "long" form from the existing "wide" layout
BCdata_long <- melt(BCdata, id=c("AGE"))
#
# add a new variable "vote2009" with 2009 voting behaviour ("NV..." are non-voters, "V..." are voters)
BCdata_long$vote09 <- "Vote"
BCdata_long$vote09[BCdata_long$variable == "NV.Consistent"] <- "NonVote"
BCdata_long$vote09[BCdata_long$variable == "NV.Inconsistent"] <- "NonVote"
BCdata_long$vote09[BCdata_long$variable == "NV.NewlyEligible"] <- "NonVote"
# add a new variable "pattern" with 2001-2005-2009 voting pattern
BCdata_long$pattern <- "Consistent"
BCdata_long$pattern[BCdata_long$variable == "V.Inconsistent"] <- "Inconsistent"
BCdata_long$pattern[BCdata_long$variable == "NV.Inconsistent"] <- "Inconsistent"
BCdata_long$pattern[BCdata_long$variable == "V.NewlyEligible"] <- "NewlyEligible"
BCdata_long$pattern[BCdata_long$variable == "NV.NewlyEligible"] <- "NewlyEligible"
#
#
#
# ##############################################################
# PLOTS
# ##############################################################
#
# FACET PLOT
#
# base plot == total voter count (y = value) by age group (x)
p1 <- ggplot(BCdata_long, aes(x = AGE, y = value)) + 
  geom_bar(stat="identity") 
# facet by pattern and vote 2009
p1 + facet_grid(vote09 ~ pattern)
#
#
# BACK TO BACK PLOT
# http://learnr.wordpress.com/2009/09/24/ggplot2-back-to-back-bar-charts/
#
p2 <- ggplot(BCdata_long, aes(AGE)) + 
  geom_bar(subset = .(vote09 == "Vote"), aes(y = value, fill = pattern), stat = "identity") +
  geom_bar(subset = .(vote09 == "NonVote"), aes(y = -value, fill = pattern), stat = "identity") +
  xlab("Age group") + scale_y_continuous("Vote - NonVote")
#
# add some Wes Anderson "Grand Budapest Hotel" colour
p2 + scale_fill_manual(values = wes.palette(4, "GrandBudapest")) 
# 

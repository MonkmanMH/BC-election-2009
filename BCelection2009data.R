# VOTER DEMOGRAPHICS
#
# replicating the charts in the report 
# "Who heads to the polls? Exploring the Demographics of Voters in British Columbia" (March 2010, BC Stats for Elections BC)
# https://www.google.ca/url?sa=t&rct=j&q=&esrc=s&source=web&cd=1&cad=rja&uact=8&ved=0CB4QFjAA&url=http%3A%2F%2Fwww.elections.bc.ca%2Fdocs%2Fstats%2FWho-heads-to-the-polls.pdf&ei=yT6rU9y8JtHdoASUyoHoDQ&usg=AFQjCNFk_xgmEFMjih5Clxl6_hX4GN_Tug&sig2=buQET2cR23LaTFcwrOZeJA&bvm=bv.69837884,d.cGU
#
# some background reading material regarding back-to-back bar charts
# http://www.r-bloggers.com/ggplot2-a-little-twist-on-back-to-back-bar-charts/
# http://learnr.wordpress.com/2009/09/24/ggplot2-back-to-back-bar-charts/
# http://svitsrv25.epfl.ch/R-doc/library/Hmisc/html/histbackback.html
# 
#
# install necessary packages
if (!require(ggplot2)) install.packages("ggplot2")
library("ggplot2")
if (!require(plyr)) install.packages("plyr")
library("plyr")
if (!require(reshape2)) install.packages("reshape2")
library("reshape2")
if (!require(vcd)) install.packages("vcd")
library("vcd")
#
# Use the function source_GitHubData, which requires the package devtools
if (!require(devtools)) install.packages("devtools")
library(devtools)
# The functions' gist ID is 4466237
source_gist("4466237")
#
# Download data, which is stored as a csv file at github
BCdata <- source_GitHubData("https://raw.githubusercontent.com/MonkmanMH/BC-election-2009/master/BCelection2009data.csv")
# a quick look at the data
head(BCdata)
str(BCdata)
#
#
if (!require(wesanderson)) install.packages("wesanderson")
library(wesanderson)
# for more on the Wes Anderson colour palette:
# https://github.com/karthik/wesanderson#wes-anderson-palettes
# http://blog.revolutionanalytics.com/2014/03/give-your-r-charts-that-wes-anderson-style.html
#
#
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

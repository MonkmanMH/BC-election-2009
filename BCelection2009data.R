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
#
# BACK TO BACK PLOT -- STACKED BAR CHART / HISTOGRAM
# http://docs.ggplot2.org/0.9.3.1/geom_bar.html
# http://learnr.wordpress.com/2009/09/24/ggplot2-back-to-back-bar-charts/
# http://www.r-bloggers.com/ggplot2-a-little-twist-on-back-to-back-bar-charts/
# http://www.r-bloggers.com/making-back-to-back-histograms/
# http://stackoverflow.com/questions/7579995/facet-grid-of-back-to-back-histogram-failing
#
# -----
# core plot as object "p2"
p2 <- ggplot(BCdata_long, aes(AGE)) + 
  geom_bar(subset = .(vote09 == "Vote"), aes(y = value, fill = variable), stat = "identity") +
  geom_bar(subset = .(vote09 == "NonVote"), aes(y = -value, fill = variable), stat = "identity")
p2
#
# a variant on object "p2" with the original colours
# original colours:
#   voters:
#      Newly eligible  255-192-0    == FFC000
#      Inconsistent    255-146-47   == FF922F
#      Consistent      192-0-0      == C00000
#   NonVoters:
#      Newly eligible  0-0-0        == 000000
#      Inconsistent    166-166-166  == A6A6A6
#      Consistent      75-172-198   == AFAC66
#
# NOTE: the following does not work!  It is missing some of the variables --
#       first thought was that they were overlapped, but using [position ="stacked"] didn't change anything
#       Further debugging required
p2a <- ggplot(BCdata_long, aes(AGE)) + 
  geom_bar(subset = .(variable == "V.NewlyEligible"), aes(y = value, fill = "#FFC000"), postition = "stack", stat = "identity") +
  geom_bar(subset = .(variable == "V.Inconsistent"), aes(y = value, fill = "#FF922F"), postition = "stack", stat = "identity") +
  geom_bar(subset = .(variable == "V.Consistent"), aes(y = value, fill = "#C00000"), postition = "stack", stat = "identity") +
  geom_bar(subset = .(variable == "NV.Consistent"), aes(y = -value, fill = "#000000"), postition = "stack", stat = "identity") +
  geom_bar(subset = .(variable == "NV.Inconsistent"), aes(y = -value, fill = "#A6A6A6"), postition = "stack", stat = "identity") +
  geom_bar(subset = .(variable == "NV.NewlyEligible"), aes(y = -value, fill = "#AFAC66"), postition = "stack", stat = "identity")
p2a
#
#
# add some tweaks to the orginal p2
p2 +
  xlab("Age group") + scale_y_continuous("Non-Voters - Voters") +
  geom_hline(yintercept = 0, colour = "white") +
  scale_fill_brewer(palette="Blues") 
#
#
# -----
# like "p2" but with colour based on variable "pattern"
p3 <- ggplot(BCdata_long, aes(AGE)) + 
  geom_bar(subset = .(vote09 == "Vote"), aes(y = value, fill = pattern), stat = "identity") +
  geom_bar(subset = .(vote09 == "NonVote"), aes(y = -value, fill = pattern), stat = "identity")
p3
# add some tweaks
p3 +
  xlab("Age group") + scale_y_continuous("Non-Voters - Voters") +
  geom_hline(yintercept = 0, colour = "black") +
  scale_fill_brewer(palette="Greys") 
#
# alternate colour scheme with ColorBrewer
p3 +
  geom_hline(yintercept = 0, colour = "grey90") +
  scale_fill_brewer(palette="Blues") 
#
# colours in ggplot2
# http://stackoverflow.com/questions/12910218/set-specific-fill-colors-in-ggplot2-by-sign

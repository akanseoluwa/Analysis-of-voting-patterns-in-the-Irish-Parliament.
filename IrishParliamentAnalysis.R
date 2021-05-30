## Name : Akanseoluwa Adegoke


# Loading the voting data into R
load("32ndDail_FifthSession_BinaryVotes.Rdata")

# Hierchical clustering has been chosen because it is well suited for binary data and 
# it allows us to selcet from many distance functions. The votes of the TDs are recorded 
# as 2 for yes and 1 for No. We transform the dataset to a binary form of 1 and 0 and 
# we subtract 1 from the dataset to get this. A dissimilarity matrix is constructed and 
# we apply different linkages to create a dendogram. 

# The average linkage is used to decide the cluster solution as it has a very clear
# dendogram out of all the other linkages and it shows a particular part of the dataset
# grouped together well. Cutting the height at 0.90 results in a 
# three(3) cluster solution.


vote = bin.votes

# Subtracting 1 from the dataset
vote = vote - 1

# Visualisng our data
plot(vote, main= "Voting Data ")

#Calculate the (Binary) distance matrix
dist.bin = dist(vote, method = "binary")

# Perform hierarchical clustering with single linkage
cl.single = hclust(dist.bin, method = "single")

# Perform hierarchical clustering with average linkage
cl.average = hclust(dist.bin, method = "average")

# Perform hierchical clustering with complete linkage
cl.complete = hclust(dist.bin, method = "complete")

par(mfrow = c(1,3))

# plot the dendrogram
plot(cl.single, hang = -1, cex =0.4,  main = "Dendogram(Single Linkage)")
plot(cl.average, hang = -1,cex = 0.4, main = "Dendogram(Average Linkage)")
abline(h = 0.90, lty =2 )
plot(cl.complete, hang = -1, cex = 0.4,  main = "Dendogram(Complete Linkage)")
abline(h = 0.9, lty = 2 )

# Cutting the dendrogram horizontally at a particular height partitions the
# data into dis-joint clusters, represented by the vertical lines
# which intersect the horizontal divisor

# A solution with 3 clusters is chosen
hcl = cutree(cl.average, k = 3)
table(hcl)


# The data is transformed back to its original state by adding 1 to every column. 
# This is required as the Polytomous latent class analysis (poLCA) function cannot 
# work with data in the form of 0 and 1, which is binary data. 
 
#  After transforming the data, we choose the number of latent classes. To accomplish
# this, we fit a model for different numbers of n-class and store the Bayesian Information 
# Criterion (BIC) value in an array. The local maxima problem is taken care of as the 
# the poLCA function has been called ten times for each class. 
#After the result is plotted, a minumum value of BIC 
# is gotten for 2 latent  classes. 

# Another criterion to check is the Akaike Information Criterion (AIC) . We fit a model
# for different numbers of n-class and store the AIC value in an array. After the result 
# is plotted, a minimum value of AIC is gotten for 4 latent classes. In order to 
# choose between 2,3 or 4 clusters, the sum of the corresponding AIC and BIC values are
# calculated. From our results, the sum is minimised for three (3) clusters.
 
# Therefore, we can conclude that there are three (3) clusters in the voting data. 
# The 3 different clusters are in the percentage of 36.04 percent, 26.95 percent
# and 37.02 percent.


library(poLCA)
vote2 = bin.votes

bic = rep(0, 10)
aic = rep(0, 10)

for (i in 1 : 10) {
  polmod = poLCA(cbind(Environment, RentFreeze, SocialWelfare, GamingAndLotteries, 
        HousingMinister, FirstTimeBuyers)~1, vote2 ,nclass = i, maxiter = 150000, nrep = 20)
  bic[i] = polmod$bic
  aic[i] = polmod$aic
}

par(mfrow = c(1,2))
plot(c(1: 10), bic, main = "BIC")
lines(bic)

plot( c(1 :10), aic, main ="AIC")
lines(aic)

aic[2] + bic[2]
aic[3] + bic[3]
aic[4] + bic[4]

polmod_best = poLCA(cbind(Environment, RentFreeze, SocialWelfare, GamingAndLotteries, 
                        HousingMinister, FirstTimeBuyers)~1, vote2 ,nclass = 3, 
                        maxiter = 20000, nrep = 20) 

# To compare the clustering from Polytomous latent class analysis with Hierarchical clustering, we 
# use the e1071 library in R. 
# The Rand Index tells us that the two clusterings have an agreement of 0.73, which is pretty high
# as 0.73 is close to 1.
# The Adjusted Rand Index shows a reduction in the agreement between the two clusterings.
# The Adjusted Rand Index shows an agreement of 0.47. 

# The optimal number of clusters will be taken as three(3).

comp_clus = table(hcl, polmod_best$predclass)
comp_clus

library(e1071)
classAgreement(comp_clus)


## Voting Patterns in the 32nd Dáil Éireann. 

## Background Information
# Ireland is a parliamentary democracy. The National Parliament (the Oireachtas) 
# consists of the President and two Houses: Dáil Éireann (House of Representatives) 
# and Seanad Éireann (the Senate). The members of Dail Éireann, called 
# Teachtaí Dála(TDs), are directly elected by the people. Different parties 
# participated in the 32nd Dáil Éireann elcetion. These parties include Fine Gael(FG),
# Labour Party(LP), Sinn Fein(SF), Anti-Austerity Alliance People Before Profit(AAA-PBP),  
# Fianna Fail(FF), Social Democrats(SD) , Green Party(Green) . There are 6 AAA-PBP 
# members, 43 Fianna Fail members, 49 Fine Gael Memebers, 3 Green party members, 
# 18 Indepenedent TDs, 7 labour party members, 3 Social Democrats, 24 Sinn Fein members.
# There are 156 TD's in total during the fifth session
# and the following topics were delibrated : Environment, Social Welfare, Gaming 
# and Lotteries, Rent Freeze, Housing Minister and First Time Buyers.
# 
# On the 28 November 2019, there was the Planning and Development (First Time Buyers) bill.
# On the 3 December 2019, there was a vote of confidence on the Minister for Housing. 
# On the 4 December 2019, there was the Gaming and Lotteries ammendment bill.
# On the 5 December 2019, the Social Welfare amendment bill started the second reading.
# On 12 December 2019, Rent Freeze Bill resumed the Second Reading
# On 18 December 2019, Environmental Policy: motion  resumed.
# After voting, the First Time Buyers bill, Rent Freeze Bill, Social Welfare , 
# Gaming and Lotteries bills were passed. The Environment and vote of no confidence
# on the Housing minister did not go through.


## Introduction

# A simple way to find patterns in voting is by classifying the voters according to
# their party showing those in favour of a bill and those against a bill.

# Also, Latent Class Analysis(LCA) is a statistical method for identifying measured class 
# membership among subjects using continuous or categorical variables.LCA groups 
# observations into latent classes that return observations behaviour with respect to each
# manifest variable. We can use LCA to identify clusters in the voting patterns of parties. 
# The Latent Class Analysis performed using the poLCA package gives a solution of three 
# clusters. The manifest vaiables are the voting topics. The poLCA function takes the 
# model formula, number of classes, number of repetitions as input. It outputs a data frame
# of manifest variables, the number of cases used in the model, number of fully observed cases
# in the model.

## Analysis

# From our analysis, we can see that Fine Gael was the party in support of the environment 
# bill while all other parties including the independent candidates were not in support of it.

# Concerning the Housing Minister , Fine Gael and Fianna Fail voted against the notion of no
# confidence in the housing minister including 10 independent TDs. Contrarily, almost all other 
# parties and Dail Eireann members voted in favour of the notion.

# For the Gaming and Lotteries Bill, there seems to an unconsistent pattern in voting. 
# Social Democrats, Sinn Finn, Green Party, Solidarity-PbP and 15 TDs voted against the bill. 

# For the Rent Freeze Bill, Fine Gael party did not support it as 48 out of  49 of them voted
# against it while most of the TD's in other parties voted in favour of it including the 
# independent candidates. Contrarily, Fine Gael and Fianna Fail voted in favour of the bill with
# some of their candidates doing otherwise.

# For the First Time Buyers Bill, Sinn Fein and Independent TDs show random voting patterns as t
# heir votes are not united. Most Fianna Fail members, Social Democrats, Solidarity-PbP, Indepedent
# for Change and the Labour party voted in favour of the First Time Buyers bill.
# On the other hand, Fine Gael and the Green Party voted against the bill.

# For the Social Welfare Bill, Most parties voted in favour of the bill except Fine Gael who 
# had 48 members out of 49 voting against the bill and 13 out of 18 independent TDs voting against 
# the bill.

## Conclusion
# Fine Gael and Fianna Fail TDs vote in a similar pattern some times. The left wing parties and the
# Green Party cooperate together on several occassions concerning the bills and they looked like an
# opposition to the Fine Gael party which was the ruling and majority party. The eighteen (18) independent 
# TDs voted randomly as it was hard to distinguish the voting patterns of the independent TDs.


# Furthermore, we conclude that Latent Class Analysis provides a better understanding of the 
# association between parties and voting patterns.
# We also compare the clustering done by Party membership and by Latent Component Analysis. From the 
# clustering table, there are three clusters. In the first cluster, it is made up of mainly Fine Gael
# party members. There are 48 Fine Gael members out of 49 members, just one Independent for Change member
# and 7 Independent TDs. In the second cluster, there are 24 Sinn Fein members, 7 independent TDs, 
# 6 AAA-PBP members, 4 Labour party members, 3 Green party members, 2 Social Democrats and 1 Fine Gael member
# who happens to be the only Fine Gael member not in the same cluster with the rest of his party members.
# In the third cluster, all Fianna Fail members are there. There are also 4 Independent TDs, 3 Labour party 
# members, 2 Independent for Change members and 1 Social Democrat.
#
# The Rand Index which is a measure of agreement between clusterings has a value 
# of 0.83 which indicates 83 percent agreement between the clusters which is high.
# The adjusted Rand Index is reduced as shows a value of 43.4 percent. 
# 
# 

party = read.csv("TDs_names_parties.csv")

party1 = cbind(vote, party$Party)

tim = table(party$Party)

percent = tim / nrow(party) * 100
t1 = rbind(tim, percent)
rownames(t1) = c("Freq", "percentage")
colnames(t1) = c("PBP", "Fianna Fail", "Fine Gael", "Green Party", 
                 "Independent-Change", "Independent TDs","Labour Party", "Social Democrats", "Sinn Fein")


pie(x = c(6,43,39,3,3,18,7,3,24),labels = c("PBP Party","Fianna Fail Party","Fine Gael Party","Green P
arty","Independents_Change ","Independent TDs","Labour Party","Social Democrats ","Sinn Fein Party"), 
    col=c(1,6,3,4,5,2,7,8,13))

t1

# Environment Analysis
(table(party[which(bin.votes$Environment ==2),2]))  # Those in Support
(table(party[which(bin.votes$Environment !=2),2]))  # Those against
sum(table(party[which(bin.votes$Environment==2),2]))/1.56   # overall percentage in support

# Rent Freeze Analysis
(table(party[which(bin.votes$RentFreeze ==2),2]))   # Those in Support
(table(party[which(bin.votes$RentFreeze !=2),2]))   # Those against
sum(table(party[which(bin.votes$RentFreeze==2),2]))/1.56   # overall percentage in support

# Social Welfare Analysis
(table(party[which(bin.votes$SocialWelfare ==2),2]))  #Those in support
(table(party[which(bin.votes$SocialWelfare !=2),2]))  # Those against 
sum(table(party[which(bin.votes$SocialWelfare==2),2]))/1.56 # overall percentage in support

# Gaming and Lottery Analysis
(table(party[which(bin.votes$GamingAndLotteries ==2),2]))   # Those in support
(table(party[which(bin.votes$GamingAndLotteries !=2),2]))   # Those against
sum(table(party[which(bin.votes$GamingAndLotteries==2),2]))/1.56   # overall percentage in support

#  Housing Minister Analysis
(table(party[which(bin.votes$HousingMinister ==2),2]))   #Those in support
(table(party[which(bin.votes$HousingMinister !=2),2]))   #Those against
sum(table(party[which(bin.votes$HousingMinister==2),2]))/1.56  # overall percentage in support

# Firstime Buyers Analysis 
(table(party[which(bin.votes$FirstTimeBuyers ==2),2]))  #Those in support
(table(party[which(bin.votes$FirstTimeBuyers !=2),2]))  #Those against
sum(table(party[which(bin.votes$FirstTimeBuyers==2),2]))/1.56  # overall percentage in support

# Crosstabulating the different clusterings.
tab1d = table(polmod_best$predclass,party$Party)# Crosstabulating the two clusterings.
tab1d
classAgreement(tab1d) # classAgreement of the table



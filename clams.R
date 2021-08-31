#Let's take a look at Betsy's clam data

#load my favorite data manipulation library
library(tidyverse)

#load a library to plot data
library(visreg)

#load  library to read excel files
library(readxl)

#import your data
clams <- read_excel("Suisun Marsh field and clam data 2018-20 for R.xlsx")

#mke sure it got read in correctly
str(clams)
#Looks good!

#I'm just going to do a few, quick, exploritory plots.

#first some historgrams
ggplot(clams) + geom_histogram(aes(x = C_AFDM))
ggplot(clams) + geom_histogram(aes(x = P_AFDM))
ggplot(clams) + geom_histogram(aes(x = T_filt))

#So, you're definitely not normally distributed!
#will log-transforming help?
ggplot(clams) + geom_histogram(aes(x = log(C_AFDM+1)))
ggplot(clams) + geom_histogram(aes(x = log(P_AFDM+1)))
ggplot(clams) + geom_histogram(aes(x = log(T_filt+1)))

#Gross. Even log-transformed those are bad. Highly zero-inflated. 
#A regular-old linear model probably won't work. We'll probably need a hurdle model or zero-inflated model.
#But I'll start with showing you how regular linear models work, just 'cause.

#The "lm" function wants to you specficy the model formula and give it the data set yo uwant it to model.
#for details on how to write formulas, see: https://cran.r-project.org/doc/manuals/r-release/R-intro.html#Formulae-for-statistical-models 
m1 = lm(log(C_AFDM+1)~ Month + Habitat, data = clams)

#The "summary" function gives you the output
summary(m1)

#using the 'plot" function gives us some diagnostics to see whether our model fit the assumptions
plot(m1)
#most of those look OK, but the normal QQ plot is super wacky. Probably due to all the excess zeros. 

#look at partial residuals plots. THese take the effect of each factor and graph it on its own.
visreg(m1)

#Generalized linear models are the next step. They are just like a linear model, but instead of
#assuming you have a normal distribution, you can change the distribution using the "family" function.

#For example, if we want to model our clam data as presence/absence instead of looking at AFDM, we can 
#use a binomial model. 

#first create a column for presence/absence
clams = mutate(clams, C_YN = as.logical(C_AFDM))

m2 = glm(C_YN~ Month + Habitat, data = clams, family = "binomial")
summary(m2)
visreg(m2)
#Well, that isn't great, but it gives you an idea of how it works.

#The model coefficients it gives you tells you whether the factors are significantly different from the "baseline"
#If you want to do all pairwise comparisons, you need a post-hoc
library(emmeans)
emmeans(m2, pairwise~ Habitat )

#Now, because of all the zeros and stuff, we are probably going to want to do a really
#obnoxious zero-inflated negative binomial model. Sigh. 

library(MASS)
library(pscl)

#The "zeroinfl" function in the pscl package can do zero inflated negative binomial or poisson models.
zip1 = zeroinfl(C_AFDM~ Month + Habitat, dist = "negbin", data = clams)
#Unfortunately, this expects count data. Since AFDM isn't an integer, it has trouble. I'm going to do some 
#more research on this, but in the meantime:

zip2 = zeroinfl(round(C_AFDM)~ Month + Habitat, dist = "negbin", data = clams)

summary(zip2)
#This is telling you taht you have a higher likelihood of catching zero clams in 
#the sloughs versus the Channels, and that you will catch fewer clasm in the sloughs when you catch them. 
#Woohoo! THat is exactly what we were expecting. 

#But that is just the factorial predictors. Let's add some continuous predictors too.

zip3 = zeroinfl(round(C_AFDM)~ Month + Habitat + Salinity + Chlor + Depth, dist = "negbin", data = clams)
summary(zip3)
#Hmmm... Why are we getting that warning?

summary(clams)
#I bet it is becasue "salinity" is on such a different scale from everything else that it is trhowing things off.
#let's change the scale so it's closer to everything else.

clams$Salinity2 = scale(clams$Salinity)

zip4 = zeroinfl(round(C_AFDM)~ Month + Habitat + Salinity2 + Chlor + Depth, dist = "negbin", data = clams)
summary(zip4)
#Better! Now this is telling us we are still more likely to catch zero clams in sloughs, and also less
#likely to catch zero clams in deep water. We are likely to catch fewer clams in Sloughs than channels, when we do 
#catch clams. 
emmeans(zip4, pairwise~Habitat)

#Channel is more than slough, but no difference between river and channel or slough.

#But how do you know which predictors to include? Here is where model comparison comes in.

#set up a model with ALL your predictors
global = zeroinfl(round(C_AFDM)~ Month + Habitat + Salinity2 + Chlor + Depth + DO + W_temp, 
                  dist = "negbin", data = clams, na.action = "na.fail")

#we can then use the "dredge" function to look at all possible combinations of the variables and 
#rank how good they are at explaining our results. 
allmodels = dredge(global)
View(allmodels)

#Models that are less than three AICc points apart are pretty much identical, so we want to set up our "best" 
#model including all the terms from the models that were within an ACIc of 3.

bestmod = zeroinfl(round(C_AFDM)~ Habitat +  Chlor + Depth + Month, 
                   dist = "negbin", data = clams, na.action = "na.fail")
summary(bestmod)
#So, depth and Habitat are the only ones that come out as significant, but Month and Chlorophyll improve the model.

#Now go ahead and do this again with Potamocorbula AFDM at total filtration!!

#####################################################################################

#And, since you asked about PERMANOVA

#In R we use the "vegan" package and the function "adonis"
library(vegan)

#first we need to make a matrix of our dependent variables.
Clammat = as.matrix(dplyr::select(clams, c(C_AFDM, P_AFDM)))

#now we can set up our PERMANOVA with much the same syntax as a linear model

p1 = adonis(Clammat~ Habitat + Depth + Month, data = clams)
#ooo, yeah, you can't really do a PERMANOVA on empty rows. IT doesn't know what to do with an empty community.

#let's remove the empty rows and try again
clams2 = filter(clams, T_AFDM != 0)
Clammat2 = as.matrix(dplyr::select(clams2, c(C_AFDM, P_AFDM)))

p2 = adonis(Clammat2~ Habitat + Depth + Month, data = clams2)
p2

#OK! Small difference in habitat and depth, none by month.


##############################################33

#filter out sites only sampled one year

clams2 = filter(clams, Both_years == "yes") %>%
  mutate(Year2 = as.factor(Year))
#Erg. 

#set up a model with ALL your predictors
global2 = zeroinfl(round(C_AFDM)~ Month + Habitat + Salinity2 + Chlor + Depth + DO + W_temp + Year2, 
                  dist = "negbin", data = clams2, na.action = "na.fail")

#we can then use the "dredge" function to look at all possible combinations of the variables and 
#rank how good they are at explaining our results. 
allmodels2 = dredge(global2)
View(allmodels2)

bestmod2 = zeroinfl(round(C_AFDM)~ Habitat +  Depth + Chlor, 
dist = "negbin", data = clams2, na.action = "na.fail")

summary(bestmod2)


library(lme4)
library(lmerTest)
ggplot(clams, aes(x= Habitat, y = Depth)) + geom_boxplot()
mx = lmer(Depth~Habitat + (1|Station), data = clams)
summary(mx)

bestmod2a = zeroinfl(round(C_AFDM)~ Habitat + Chlor, 
                    dist = "negbin", data = clams2, na.action = "na.fail")
summary(bestmod2a)

bestmod2b = zeroinfl(round(C_AFDM)~ Depth + Chlor, 
                     dist = "negbin", data = clams2, na.action = "na.fail")
summary(bestmod2b)

AICc(bestmod2a, bestmod2b)

test = c(rnorm(24, mean = 20, sd = 5), rnorm(24, mean = 10, sd = 5), rnorm(24, mean = 25, sd = 5))
test = c(rnorm(6, mean = 22, sd = 5), rnorm(6, mean = 24, sd = 5),rnorm(6, mean = 21, sd = 5),
         rnorm(6, mean = 12, sd = 5), rnorm(6, mean = 13, sd = 5),rnorm(6, mean = 9, sd = 5),
         rnorm(6, mean = 22, sd = 5), rnorm(6, mean = 26, sd = 5),rnorm(6, mean = 23, sd = 5))

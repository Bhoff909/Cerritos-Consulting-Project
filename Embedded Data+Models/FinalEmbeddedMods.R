 # Final Model:


# Math #####
library(MatchIt)
ET.math = read.csv("ET.MTH.19.23.copy.csv")
ET.math$Student_ID <- stringr::str_pad(ET.math$Student_ID, side = 'left', width = 7, pad = '0')
ET.math <-ET.math %>% dplyr::select(-X) 

### Cleaning data ####
library(mltools)
library(data.table)
# Make variables into factors
ET.math.copy <- ET.math
ET.math$session.count2 <- as.factor(ET.math $session.count2)
ET.math.copy$Enrolled_next_fall[ET.math.copy$Enrolled_next_fall=='FALSE'] <- 0
ET.math.copy$Enrolled_next_fall[ET.math.copy$Enrolled_next_fall=='TRUE'] <- 1
ET.math.copy$Enrolled_next_fall <- as.factor(ET.math.copy$Enrolled_next_fall)

ET.math.copy$Enrolled_next_spring[ET.math.copy$Enrolled_next_spring=='FALSE'] <- 0
ET.math.copy$Enrolled_next_spring[ET.math.copy$Enrolled_next_spring=='TRUE'] <- 1
ET.math.copy$Enrolled_next_spring <- as.factor(ET.math.copy$Enrolled_next_spring)

ET.math.copy$pass <- as.factor(ET.math.copy$pass)
ET.math.copy$tutor <- as.factor(ET.math.copy$tutor)
ET.math.copy$session.count2 <- as.factor(ET.math.copy$session.count2)
ET.math.copy$ethnicity<- as.factor(ET.math.copy$ethnicity)

ET.math.copy$gender<- as.factor(ET.math.copy$gender)
ET.math.copy$first_gen<- as.factor(ET.math.copy$first_gen)


ET.math.copy$term_1199<- as.factor(ET.math.copy$term_1199)
ET.math.copy$term_1209<- as.factor(ET.math.copy$term_1209)
ET.math.copy$term_1219<- as.factor(ET.math.copy$term_1219)
ET.math.copy$term_1229<- as.factor(ET.math.copy$term_1229)
ET.math.copy$term_1239<- as.factor(ET.math.copy$term_1239)
ET.math.copy$gpa<- as.factor(ET.math.copy$gpa)

# setting the reference 
ET.math.copy$session.count2 <- relevel(ET.math.copy$session.count2, ref = "none")

ET.math.copy$gender<- relevel(ET.math.copy$gender, ref = "Male")
ET.math.copy$first_gen <- as.factor(ET.math.copy$first_gen)
ET.math.copy$first_gen<- relevel(ET.math.copy$first_gen, ref = "Not First Generation")

ET.math.copy$ethnicity<- relevel(ET.math.copy$ethnicity, ref = "Hispanic/Latino")



## Propensity score matching ####
test1 = matchit(tutor~gpa+first_gen+ethnicity+gender+term_1199+term_1209+
                  term_1219+term_1229+term_1239,data=ET.math.copy) # more predictors for the matching

matched_data1 = match.data(test1)


### diagnostic plots
library(cobalt)
bal.plot(test1,var.name="gpa",which="both")
bal.plot(test1,var.name="first_gen",which="both")
bal.plot(test1,var.name="ethnicity",which="both")
bal.plot(test1,var.name="gender",which="both")
bal.plot(test1,var.name="term_1219",which="both")

love.plot(bal.tab(test1,m.threshold=0.1),stat="mean.diffs",abs=F, stars = "std")

# releveling matched data 
matched_data1$session.count2 <- relevel(matched_data1$session.count2, ref = "none")
matched_data1$gender <- as.factor(matched_data1$gender)
matched_data1$first_gen <- as.factor(matched_data1$first_gen)
matched_data1$gender<- relevel(matched_data1$gender, ref = "Male")
matched_data1$first_gen<- relevel(matched_data1$first_gen, ref = "Not First Generation")
matched_data1$ethnicity <- as.factor(matched_data1$ethnicity)
matched_data1$ethnicity<- relevel(matched_data1$ethnicity, ref = "Hispanic/Latino")


#### Success ######
summary(glm(pass~session.count2+ethnicity,data=matched_data1,weights = weights,family="binomial"))
matched_data1$ethnicity = as.factor(as.numeric(as.factor(matched_data1$ethnicity)))
pass.p1 = glm(pass~n+ethnicity,weights = weights,data = matched_data1, 
              family = "binomial")

newdata = with(matched_data1,
               data.frame(n = rep(seq(from = 0, to=36, 
                                      length.out = 116),4),
                          ethnicity = factor(rep(1:4, each = 116))))

newdata1 = cbind(newdata, predict(pass.p1, newdata, type='link', se=TRUE))

newdata1 = within(newdata1,{pass.pred = plogis(fit)})

library(plyr)
newdata1$ethnicity = as.factor(mapvalues(newdata1$ethnicity, 
                                         from=c(1,2,3,4), 
                                         to= c( "Hispanic/Latino",
                                                "Asian",
                                                "Underrepresented",
                                                "White/Two or more races")))
library(ggplot2)
pass.prob1 = ggplot(newdata1, aes(n,pass.pred,colour = ethnicity, group = ethnicity))+
  geom_line(size = 1.5)+ylab("Probability of Success")+
  xlab("Session Count")+ggtitle("Success, Embedded Math ")

pass.prob1+
  scale_color_manual(name = "Ethnicity",
                     values=c("#B2C3FF","#445B9A","#00B389",'#A23248'),
                     labels=c("Hispanic", "Asian", "Underespresented","White/Two or More Races"))


#### Persistence ######
summary(glm(Enrolled_next_spring~session.count2+ethnicity,data=matched_data1,weights = weights,family="binomial"))

persist.p1 = glm(Enrolled_next_spring~n+ethnicity,weights = weights,data = matched_data1, 
                 family = "binomial")

newdata2 = cbind(newdata, predict(persist.p1 , newdata, type='link', se=TRUE))
newdata2 = within(newdata2, {persist.pred = plogis(fit)})
newdata2$ethnicity = as.factor(mapvalues(newdata2$ethnicity, 
                                         from=c(1,2,3,4), 
                                         to= c(
                                           "Hispanic/Latino",
                                           "Asian",
                                           "Underrepresented",
                                           "White/Two or more races")))

persist.prob1 = ggplot(newdata2, aes(n,persist.pred,colour = as.factor(ethnicity),group = ethnicity))+
  geom_line(size = 1.5)+
  ylab("Probability of Success")+
  xlab("Session Count")+ggtitle("Persistence, Embedded Math ")


persist.prob1+  scale_color_manual(name = "Ethnicity",
                                   values=c("#B2C3FF","#445B9A","#00B389", '#A23248'),
                                   labels=c("Hispanic", "Asian", "Underespresented","White/Two or More Races"))

#### Retention ######
summary(glm(Enrolled_next_fall~session.count2+ethnicity,data=matched_data1,weights = weights,family="binomial"))

reten.p1 = glm(Enrolled_next_fall~n+ethnicity,weights = weights,data = matched_data1, 
               family = "binomial")

newdata.r = with(matched_data1,
                 data.frame(n = rep(seq(from = 0, to=20, 
                                        length.out = 116),4),
                            ethnicity = factor(rep(1:4, each = 116))))

newdata3 = cbind(newdata.r, predict(reten.p1 , newdata.r, type='link', se=TRUE))
newdata3 = within(newdata3, {reten.pred = plogis(fit)})
newdata3$ethnicity = as.factor(mapvalues(newdata3$ethnicity, 
                                         from=c(1,2,3,4), 
                                         to= c(
                                           "Hispanic/Latino",
                                           "Asian",
                                           "Underrepresented",
                                           "White/Two or more races")))

reten.prob1 = ggplot(newdata3, aes(n,reten.pred,colour = as.factor(ethnicity),group = ethnicity))+
  geom_line(size = 1.5)+
  ylab("Probability of Retention")+
  xlab("Session Count")+ggtitle("Retention, Embedded Math ")
reten.prob1+
  scale_color_manual(name = "Ethnicity",
                     values=c("#B2C3FF","#445B9A","#00B389",'#A23248'),
                     labels=c("Hispanic", "Asian", "Underespresented","White/Two or More Races"))


# Chemistry #####
Embed.chem <- read.csv("Embed.chem.copy2.csv")
Embed.chem$Student_ID <- stringr::str_pad(Embed.chem$Student_ID, side = 'left', width = 7, pad = '0')
Embed.chem <-Embed.chem %>% dplyr::select(-X) # need to add tutor column and clean before modeling

### Cleaning data ####
library(mltools)
library(data.table)
Embed.chem$term <- as.factor(Embed.chem$term)
Embed.chem2 <- one_hot(as.data.table(Embed.chem))
Embed.chem.copy <- Embed.chem2
library(dplyr)
# Remove the variables we dont need
#Math.ET.dat.copy <- Math.ET.dat %>% select(-c(X,X.1,Student_ID,courses_passed,courses_enrolled,subject))
#Embed.chem.copy <- Embed.chem2 %>% dplyr::select(-X) # dplyr clashes with MASS package
# Make variables into factors
Embed.chem$session.count2 <- as.factor(Embed.chem$session.count2)
Embed.chem.copy$Enrolled_next_fall[Embed.chem.copy$Enrolled_next_fall=='FALSE'] <- 0
Embed.chem.copy$Enrolled_next_fall[Embed.chem.copy$Enrolled_next_fall=='TRUE'] <- 1
Embed.chem.copy$Enrolled_next_fall <- as.factor(Embed.chem.copy$Enrolled_next_fall)

Embed.chem.copy$Enrolled_next_spring[Embed.chem.copy$Enrolled_next_spring=='FALSE'] <- 0
Embed.chem.copy$Enrolled_next_spring[Embed.chem.copy$Enrolled_next_spring=='TRUE'] <- 1
Embed.chem.copy$Enrolled_next_spring <- as.factor(Embed.chem.copy$Enrolled_next_spring)

Embed.chem.copy$pass <- as.factor(Embed.chem.copy$pass)
Embed.chem.copy$tutor <- as.factor(Embed.chem.copy$tutor)
Embed.chem.copy$session.count2 <- as.factor(Embed.chem.copy$session.count2)
Embed.chem.copy$ethnicity<- as.factor(Embed.chem.copy$ethnicity)

Embed.chem.copy$gender<- as.factor(Embed.chem.copy$gender)
Embed.chem.copy$first_gen<- as.factor(Embed.chem.copy$first_gen)


Embed.chem.copy$term_1199<- as.factor(Embed.chem.copy$term_1199)
Embed.chem.copy$term_1209<- as.factor(Embed.chem.copy$term_1209)
Embed.chem.copy$term_1219<- as.factor(Embed.chem.copy$term_1219)
Embed.chem.copy$term_1229<- as.factor(Embed.chem.copy$term_1229)
Embed.chem.copy$term_1239<- as.factor(Embed.chem.copy$term_1239)
Embed.chem.copy$gpa<- as.factor(Embed.chem.copy$gpa)

# setting the reference 
Embed.chem.copy$session.count2 <- relevel(Embed.chem.copy$session.count2, ref = "none")

Embed.chem.copy$gender<- relevel(Embed.chem.copy$gender, ref = "Male")
Embed.chem.copy$first_gen <- as.factor(Embed.chem.copy$first_gen)
Embed.chem.copy$first_gen<- relevel(Embed.chem.copy$first_gen, ref = "Not First Generation")

Embed.chem.copy$ethnicity<- relevel(Embed.chem.copy$ethnicity, ref = "Hispanic/Latino")

## Propensity score matching ####
# renames Embed.chem.copy
Embed.chem.copy2.1 <- Embed.chem.copy
names(Embed.chem.copy2.1)[names(Embed.chem.copy2.1) == "term_1199" ] <- "Fall 19"
names(Embed.chem.copy2.1)[names(Embed.chem.copy2.1) == "term_1209" ] <- "Fall 20"
names(Embed.chem.copy2.1)[names(Embed.chem.copy2.1) == "term_1219" ] <- "Fall 21"
names(Embed.chem.copy2.1)[names(Embed.chem.copy2.1) == "term_1229" ] <- "Fall 22"
names(Embed.chem.copy2.1)[names(Embed.chem.copy2.1) == "term_1239" ] <- "Fall 23"
names(Embed.chem.copy2.1)[names(Embed.chem.copy2.1) == "ethnicity"] <- "Ethnicity"
names(Embed.chem.copy2.1)[names(Embed.chem.copy2.1) == "first_gen" ] <- "First.Gen"
names(Embed.chem.copy2.1)[names(Embed.chem.copy2.1) == "gpa" ] <- "GPA"
names(Embed.chem.copy2.1)[names(Embed.chem.copy2.1) == "gender" ] <- "Gender"
## this is just for plotting 
test2.1 = matchit(tutor~GPA+First.Gen+Ethnicity+Gender+ `Fall 19`+`Fall 20`+
                    `Fall 21`+ `Fall 22`,
                  data=Embed.chem.copy2.1) 
love.plot(bal.tab(test2.1,m.threshold=0.1),stat="mean.diffs",abs=F, stars = "std")

test2 = matchit(tutor~gpa+first_gen+ethnicity+gender+term_1199+term_1209+
                  term_1219+term_1229+term_1239,data=Embed.chem.copy) # more predictors for the matching

matched_data2 = match.data(test2)


## Diagnostics Plot 
bal.plot(test2,var.name="gpa",which="both")
bal.plot(test2,var.name="first_gen",which="both")
bal.plot(test2,var.name="ethnicity",which="both")
bal.plot(test2,var.name="gender",which="both")
bal.plot(test2,var.name="term_1219",which="both")

love.plot(bal.tab(test2,m.threshold=0.1),stat="mean.diffs",abs=F, stars = "std")


#### Success ####
summary(glm(pass~session.count2+ethnicity,data=matched_data2,weights = weights,family="binomial"))
matched_data2$ethnicity = as.factor(as.numeric(as.factor(matched_data2$ethnicity)))
pass.p = glm(pass~n+ethnicity,weights = weights,data = matched_data2, 
             family = "binomial")
newdata = with(matched_data2,
               data.frame(n = rep(seq(from = 0, to=20, 
                                      length.out = 116),4),
                          ethnicity = factor(rep(1:4, each = 116))))

newdata1 = cbind(newdata, predict(pass.p, newdata, type='link', se=TRUE))

newdata1 = within(newdata1,{pass.pred = plogis(fit)})

library(plyr)
newdata1$ethnicity = as.factor(mapvalues(newdata1$ethnicity, 
                                         from=c(1,2,3,4), 
                                         to= c("Asian",
                                               "Hispanic/Latino",
                                               "Underrepresented",
                                               "White/Two or more races")))

pass.prob1 = ggplot(newdata1, aes(n,pass.pred,colour = ethnicity, group = ethnicity))+
  geom_line(size = 1.5)+ylab("Probability of Success")+
  xlab("Session Count")+ggtitle("Success, Embedded Chemistry ")


pass.prob1+
  scale_color_manual(name = "Ethnicity",
                     values=c("#B2C3FF","#445B9A","#00B389",'#A23248'),
                     labels=c("Hispanic", "Asian", "Underespresented","White/Two or More Races"))

#### Persistence ####
summary(glm(Enrolled_next_spring~session.count2+ethnicity,data=matched_data2,weights = weights,family="binomial"))

persist.p = glm(Enrolled_next_spring~n+ethnicity,weights = weights,data = matched_data2, 
                family = "binomial")


newdata2 = cbind(newdata, predict(persist.p, newdata, type='link', se=TRUE))
newdata2 = within(newdata2,{persist.pred = plogis(fit)})

library(plyr)
newdata2$ethnicity = as.factor(mapvalues(newdata2$ethnicity, 
                                         from=c(1,2,3,4), 
                                         to= c("Asian",
                                               "Hispanic/Latino",
                                               "Underrepresented",
                                               "White/Two or more races")))

persist.prob1 = ggplot(newdata2, aes(n,persist.pred,colour = ethnicity, group = ethnicity),show.legend = FALSE)+
  geom_line(size = 1.5,aes(linetype=ethnicity))+ylab("Probability of Persistence")+
  xlab("Session Count")+ggtitle("Persistence, Embedded Chemistry ")

persist.prob1+
  scale_color_manual(name = "Ethnicity",
                     values=c("#B2C3FF","#445B9A","#00B389",'#A23248'),
                     labels=c("Hispanic", "Asian", "Underespresented","White/Two or More Races"))+
  theme(legend.position ="none")

#### Retention ####
summary(glm(Enrolled_next_fall~session.count2+ethnicity,data=matched_data2,weights = weights,family="binomial"))

reten.p = glm(Enrolled_next_fall~n+ethnicity,weights = weights,data = matched_data2, 
              family = "binomial")


newdata3 = cbind(newdata, predict(reten.p, newdata, type='link', se=TRUE))

newdata3 = within(newdata3,{reten.pred = plogis(fit)})

newdata3$ethnicity = as.factor(mapvalues(newdata3$ethnicity, 
                                         from=c(1,2,3,4), 
                                         to= c("Asian",
                                               "Hispanic/Latino",
                                               "Underrepresented",
                                               "White/Two or more races")))

reten.prob1 = ggplot(newdata3, aes(n,reten.pred,colour = ethnicity, group = ethnicity))+
  geom_line(size = 1.5)+ylab("Probability of Retention")+
  xlab("Session Count")+ggtitle("Retention, Embedded Chemistry ")


reten.prob1+
  scale_color_manual(name = "Ethnicity",
                     values=c("#B2C3FF","#445B9A","#00B389",'#A23248'),
                     labels=c("Hispanic", "Asian", "Underespresented","White/Two or More Races"))










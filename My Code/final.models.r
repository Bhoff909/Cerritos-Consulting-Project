math.courses = read.csv("C:/Users/jbrin/OneDrive/Documents/Spring 2024/math.courses.final.csv")
chem.general = read.csv("C:/Users/jbrin/OneDrive/Documents/Spring 2024/chem.courses.final.csv")

#set reference groups
library(stats)
#math
math.courses$session.count = as.factor(math.courses$session.count)
contrasts(math.courses$session.count)
math.courses$session.count <- relevel(math.courses$session.count, ref = "none")
math.courses$first_gen = as.factor(math.courses$first_gen)
contrasts(math.courses$first_gen)
math.courses$first_gen <- relevel(math.courses$first_gen, ref = "Not First Generation")
math.courses$ethnicity = as.factor(math.courses$ethnicity)
contrasts(math.courses$ethnicity)
math.courses$ethnicity <- relevel(math.courses$ethnicity, ref = "Hispanic/Latino")
math.courses$prior.gpa.cat = as.factor(math.courses$prior.gpa.cat)
contrasts(math.courses$prior.gpa.cat)
math.courses$prior.gpa.cat <- relevel(math.courses$prior.gpa.cat, ref = "low")
#chem
chem.general$session.count = as.factor(chem.general$session.count)
contrasts(chem.general$session.count)
chem.general$session.count <- relevel(chem.general$session.count, ref = "none")
chem.general$first_gen = as.factor(chem.general$first_gen)
contrasts(chem.general$first_gen)
chem.general$first_gen <- relevel(chem.general$first_gen, ref = "Not First Generation")
chem.general$ethnicity = as.factor(chem.general$ethnicity)
contrasts(chem.general$ethnicity)
chem.general$ethnicity <- relevel(chem.general$ethnicity, ref = "Hispanic/Latino")
chem.general$prior.gpa.cat = as.factor(chem.general$prior.gpa.cat)
contrasts(chem.general$prior.gpa.cat)
chem.general$prior.gpa.cat <- relevel(chem.general$prior.gpa.cat, ref = "low")

#psm - math
library(MatchIt)
math.courses$tutor[math.courses$tutor == "yes"] = 1
math.courses$tutor[math.courses$tutor == "no"] = 0
math.courses$tutor = as.numeric(math.courses$tutor)
math.courses$STRM = as.factor(math.courses$STRM)

##################one hot encoding
library(dplyr)
library(tidyr)
library(stringr)
math.courses2 <- math.courses %>% mutate(value = 1)  %>% spread(STRM, value,  fill = 0 ) 
colnames(math.courses2)[colnames(math.courses2) == 1199] = "fall19"
colnames(math.courses2)[colnames(math.courses2) == 1209] = "fall20"
colnames(math.courses2)[colnames(math.courses2) == 1219] = "fall21"
colnames(math.courses2)[colnames(math.courses2) == 1229] = "fall22"
colnames(math.courses2)[colnames(math.courses2) == 1239] = "fall23"

test = matchit(tutor~prior.gpa.cat+first_gen+ethnicity+STRM+gender,data=math.courses)


#plots
library(cobalt)
bal.plot(test,var.name="prior.gpa.cat",which="both")
bal.plot(test,var.name="first_gen",which="both")
bal.plot(test,var.name="ethnicity",which="both")
bal.plot(test,var.name="gender",which="both")
bal.plot(test,var.name="STRM",which="both")

b1 <- bal.tab(tutor~prior.gpa.cat+first_gen+ethnicity+STRM+gender,data=math.courses)
v1 <- var.names(b1, type = "vec")
v1["prior.gpa.cat_low"] <- "Low GPA"
v1["prior.gpa.cat_high"] <- "High GPA"
v1["prior.gpa.cat_mid"] <- "Mid GPA"
v1["first_gen_Not First Generation"] <- "Not First-Gen"
v1["first_gen_First Generation"] <- "First-Gen"
v1["first_gen_Unknown"] <- "First-Gen: Unknown"
v1["ethnicity_Hispanic/Latino"] <- "Hispanic/Latino"
v1["ethnicity_Asian"] <- "Asian"
v1["ethnicity_underrepresented"] <- "Underrepresented"
v1["ethnicity_white/two or more races"] <- "White/Two or More Races"
v1["STRM_1199"] <- "Fall 19"
v1["STRM_1209"] <- "Fall 20"
v1["STRM_1219"] <- "Fall 21"
v1["STRM_1229"] <- "Fall 22"
v1["STRM_1239"] <- "Fall 23"
v1["gender_Male"] <- "Gender: Male"
love.plot(test, var.names = v1,m.threshold=0.1)

names = c("Low GPA","High GPA","Mid GPA","Not First-Gen","First-Gen","First-Gen Unknown","Hispanic/Latino","Asian","Underrepresented","White/Two or More Races","Fall 19","Fall 20","Fall 21","Fall 22","Fall 23","Gender: Male")
out =love.plot(bal.tab(test,m.threshold=0.1),stat="mean.diffs",abs=F,stars="std",var.names=names)

matched_data = match.data(test)

# library(dplyr)
# library(tidyr)
# library(stringr)
# newdata <- matched_data %>% mutate(value = 1)  %>% spread(STRM, value,  fill = 0 ) 
# colnames(newdata)[colnames(newdata) == 1199] = "fall19"
# colnames(newdata)[colnames(newdata) == 1209] = "fall20"
# colnames(newdata)[colnames(newdata) == 1219] = "fall21"
# colnames(newdata)[colnames(newdata) == 1229] = "fall22"
# colnames(newdata)[colnames(newdata) == 1239] = "fall23"


matched_data$DESCR = as.factor(matched_data$DESCR)
matched_data$STRM = as.factor(matched_data$STRM)
summary(glm(pass_rate~session.count+ethnicity,data=matched_data,weights = weights,family="binomial"))
summary(glm(pers.f2s~session.count+ethnicity,data=matched_data,weights = weights,family="binomial"))
summary(glm(ret.f2f~session.count+ethnicity,data=matched_data,weights = weights,family="binomial"))

#retention plot
matched_data$ethnicity = as.factor(as.numeric(as.factor(matched_data$ethnicity)))
ret_n = glm(ret.f2f ~ n + ethnicity,weights=weights, data = matched_data, 
            family = "binomial")

newdata = with(matched_data,
               data.frame(n = rep(seq(from = 0, to=37, 
                                      length.out = 100),4),
                          ethnicity = factor(rep(1:4, each = 100))))

newdata4 = cbind(newdata, predict(ret_n, newdata, type='link', se=T))
newdata4 = within(newdata4, {
  ret.pred = plogis(fit)
})
library(plyr)
newdata4$Ethnicity = as.factor(mapvalues(newdata4$ethnicity, 
                                         from=c(1,2,3,4), 
                                         to= c("Hispanic/Latino",
                                               "Asian",
                                               "Underrepresented",
                                               "White/Two or more races")))
library(ggplot2)
ret_plot = ggplot(newdata4, aes(n,ret.pred,colour = Ethnicity, group = Ethnicity))+geom_line(size=1.5)+ylab("Probability of Retention")+xlab("Session Count")+ggtitle("Retention, Math General")
ret_plot +scale_color_manual(name = "Ethnicity",
                             values=c("#B2C3FF","#445B9A","#00B389","#A23248"),
                             labels=c("Hispanic", "Asian", "Underespresented","White/Two or More Races"))
#persistence plot
#matched_data_chem$ethnicity = as.factor(as.numeric(as.factor(matched_data_chem$ethnicity)))
ret_n = glm(pers.f2s ~ n + ethnicity, weights=weights, data = matched_data, 
            family = "binomial")

# newdata = with(matched_data_chem,
#                data.frame(n = rep(seq(from = 0, to=50, 
#                                       length.out = 100),4),
#                           ethnicity = factor(rep(1:4, each = 100))))

newdata5 = cbind(newdata, predict(ret_n, newdata, type='link', se=T))
newdata5 = within(newdata5, {
  ret.pred = plogis(fit)
})
library(plyr)
newdata5$Ethnicity = as.factor(mapvalues(newdata5$ethnicity, 
                                         from=c(1,2,3,4), 
                                         to= c("Hispanic/Latino",
                                               "Asian",
                                               "Underrepresented",
                                               "White/Two or more races")))
library(ggplot2)
pers_plot = ggplot(newdata5, aes(n,ret.pred,colour = Ethnicity, group = Ethnicity))+geom_line(size=1.5)+ylab("Probability of Persistence")+xlab("Session Count")+ggtitle("Persistence, Math General")
pers_plot +scale_color_manual(name = "Ethnicity",
                             values=c("#B2C3FF","#445B9A","#00B389","#A23248"),
                             labels=c("Hispanic", "Asian", "Underespresented","White/Two or More Races"))
#success plot
#matched_data_chem$ethnicity = as.factor(as.numeric(as.factor(matched_data_chem$ethnicity)))
ret_n = glm(pass_rate ~ n + ethnicity, weights = weights, data = matched_data, 
            family = "binomial")

# newdata = with(matched_data_chem,
#                data.frame(n = rep(seq(from = 0, to=50, 
#                                       length.out = 100),4),
#                           ethnicity = factor(rep(1:4, each = 100))))

newdata6 = cbind(newdata, predict(ret_n, newdata, type='link', se=T))
newdata6 = within(newdata6, {
  ret.pred = plogis(fit)
})
library(plyr)
newdata6$Ethnicity = as.factor(mapvalues(newdata6$ethnicity, 
                                         from=c(1,2,3,4), 
                                         to= c("Hispanic/Latino",
                                               "Asian",
                                               "Underrepresented",
                                               "White/Two or more races")))
library(ggplot2)
suc_plot = ggplot(newdata6, aes(n,ret.pred,colour = Ethnicity, group = Ethnicity))+geom_line(size=1.5)+ylab("Probability of Success")+xlab("Session Count")+ggtitle("Success, Math General")
suc_plot +scale_color_manual(name = "Ethnicity",
                               values=c("#B2C3FF","#445B9A","#00B389","#A23248"),
                               labels=c("Hispanic", "Asian", "Underespresented","White/Two or More Races"))
#psm chem###############################################################################################################
chem.general$tutor[chem.general$tutor == "yes"] = 1
chem.general$tutor[chem.general$tutor == "no"] = 0
chem.general$tutor = as.numeric(chem.general$tutor)
chem.general$STRM = as.factor(chem.general$STRM)

test2 = matchit(tutor~prior.gpa.cat+first_gen+ethnicity+STRM+gender,data=chem.general)

#plots
library(cobalt)
bal.plot(test2,var.name="prior.gpa.cat",which="both")
bal.plot(test2,var.name="first_gen",which="both")
bal.plot(test2,var.name="ethnicity",which="both")
bal.plot(test2,var.name="gender",which="both")
bal.plot(test2,var.name="STRM",which="both")

love.plot(bal.tab(test2,m.threshold=0.1),stat="mean.diffs",abs=F,stars = "raw")

matched_data_chem = match.data(test2)

suc = glm(pass_rate~session.count+ethnicity,data=matched_data_chem,weights = weights,family="binomial")
pers = glm(pers.f2s~session.count+ethnicity,data=matched_data_chem,weights = weights,family="binomial")
ret = glm(ret.f2f~session.count+ethnicity,data=matched_data_chem,weights = weights,family="binomial")

#retention plot
matched_data_chem$ethnicity = as.factor(as.numeric(as.factor(matched_data_chem$ethnicity)))
ret_n = glm(ret.f2f ~ n + ethnicity,weights=weights, data = matched_data_chem, 
            family = "binomial")

newdata = with(matched_data_chem,
               data.frame(n = rep(seq(from = 0, to=20,
                                      length.out = 100),4),
                          ethnicity = factor(rep(1:4, each = 100))))

newdata2 = cbind(newdata, predict(ret_n, newdata, type='link', se=T))
newdata2 = within(newdata2, {
  ret.pred = plogis(fit)
})
library(plyr)
newdata2$Ethnicity = as.factor(mapvalues(newdata2$ethnicity, 
                                         from=c(1,2,3,4), 
                                         to= c("Hispanic/Latino",
                                               "Asian",
                                               "Underrepresented",
                                               "White/Two or more races")))
library(ggplot2)
ggplot(newdata2, aes(n,ret.pred,colour = Ethnicity, group = Ethnicity))+geom_line(size=1.5)+ylab("Probability of Retention")+xlab("Session Count")+ggtitle("Retention, Chem General")+scale_color_manual(name = "Ethnicity",
                                                                                                                                                                                                 values=c("#B2C3FF","#445B9A","#00B389","#A23248"),
                                                                                                                                                                                                 labels=c("Hispanic", "Asian", "Underespresented","White/Two or More Races"))

#persistence plot
#matched_data_chem$ethnicity = as.factor(as.numeric(as.factor(matched_data_chem$ethnicity)))
ret_n = glm(pers.f2s ~ n + ethnicity, weights=weights, data = matched_data_chem, 
            family = "binomial")

# newdata = with(matched_data_chem,
#                data.frame(n = rep(seq(from = 0, to=50, 
#                                       length.out = 100),4),
#                           ethnicity = factor(rep(1:4, each = 100))))

newdata3 = cbind(newdata, predict(ret_n, newdata, type='link', se=T))
newdata3 = within(newdata3, {
  ret.pred = plogis(fit)
})
library(plyr)
newdata3$Ethnicity = as.factor(mapvalues(newdata3$ethnicity, 
                                         from=c(1,2,3,4), 
                                         to= c("Hispanic/Latino",
                                               "Asian",
                                               "Underrepresented",
                                               "White/Two or more races")))
library(ggplot2)
ggplot(newdata3, aes(n,ret.pred,colour = Ethnicity, group = Ethnicity))+geom_line(size=1.5)+ylab("Probability of Persistence")+xlab("Session Count")+ggtitle("Persistence, Chem General")+scale_color_manual(name = "Ethnicity",
                                                                                                                                                                                                     values=c("#B2C3FF","#445B9A","#00B389","#A23248"),
                                                                                                                                                                                                     labels=c("Hispanic", "Asian", "Underespresented","White/Two or More Races"))

#success plot
#matched_data_chem$ethnicity = as.factor(as.numeric(as.factor(matched_data_chem$ethnicity)))
ret_n = glm(pass_rate ~ n + ethnicity, weights = weights, data = matched_data_chem, 
            family = "binomial")

# newdata = with(matched_data_chem,
#                data.frame(n = rep(seq(from = 0, to=50, 
#                                       length.out = 100),4),
#                           ethnicity = factor(rep(1:4, each = 100))))

newdata4 = cbind(newdata, predict(ret_n, newdata, type='link', se=T))
newdata4 = within(newdata4, {
  ret.pred = plogis(fit)
})
library(plyr)
newdata4$Ethnicity = as.factor(mapvalues(newdata4$ethnicity, 
                                         from=c(1,2,3,4), 
                                         to= c("Hispanic/Latino",
                                               "Asian",
                                               "Underrepresented",
                                               "White/Two or more races")))
library(ggplot2)
ggplot(newdata4, aes(n,ret.pred,colour = Ethnicity, group = Ethnicity))+geom_line(size=1.5)+ylab("Probability of Success")+xlab("Session Count")+ggtitle("Success, Chem General")+scale_color_manual(name = "Ethnicity",
                                                                                                                                                                                             values=c("#B2C3FF","#445B9A","#00B389","#A23248"),
                                                                                                                                                                                             labels=c("Hispanic", "Asian", "Underespresented","White/Two or More Races"))

######################################################Some eda plots

#success by session count for math general
library(ggplot2)
library(dplyr)
YET.success.rate.session <- math.courses %>% dplyr::select(session.count, pass_rate,STRM) %>% group_by(session.count,STRM) %>% 
  dplyr::count(pass_rate) %>%  mutate(prop= n / sum(n))

YET.success.rate.session$STRM<- as.character(YET.success.rate.session$STRM)
YET.success.rate.session$STRM[YET.success.rate.session$STRM == '1199'] <- "Fall 19"
YET.success.rate.session$STRM[YET.success.rate.session$STRM == '1209'] <- "Fall 20"
YET.success.rate.session$STRM[YET.success.rate.session$STRM == '1219'] <- "Fall 21"
YET.success.rate.session$STRM[YET.success.rate.session$STRM == '1229'] <- "Fall 22"
YET.success.rate.session$STRM[YET.success.rate.session$STRM == '1239'] <- "Fall 23"

YET.success.rate.session.plot <- ggplot(YET.success.rate.session ,aes(reorder(x= session.count, -n),n, fill = as.factor(pass_rate))) + 
  geom_col(position = position_dodge()) + xlab("Session Count Category")+ylab("Students")+
  geom_text(aes(label = prop%>% scales::percent(accuracy = 1)),  position = position_dodge(0.7),vjust= 0.75, hjust = 0, size = 3)+
  ggtitle("Math General Students Success by Session Count")+ facet_wrap(~STRM)+coord_flip()+
  theme(text = element_text(size = 15),axis.text.x=element_text(margin = margin(1, unit = "cm"),vjust =1))

YET.success.rate.session.plot+ scale_fill_manual(
  name = "Passed Their Course",
  values = c("tomato","skyblue2"),
  label = c("No","Yes"))

#success by session count for chem general
library(ggplot2)
library(dplyr)
chem.success.rate.session <- chem.general %>% dplyr::select(session.count, pass_rate,STRM) %>% group_by(session.count,STRM) %>% 
  dplyr::count(pass_rate) %>%  mutate(prop= n / sum(n))

chem.success.rate.session$STRM<- as.character(chem.success.rate.session$STRM)
chem.success.rate.session$STRM[chem.success.rate.session$STRM == '1199'] <- "Fall 19"
chem.success.rate.session$STRM[chem.success.rate.session$STRM == '1209'] <- "Fall 20"
chem.success.rate.session$STRM[chem.success.rate.session$STRM == '1219'] <- "Fall 21"
chem.success.rate.session$STRM[chem.success.rate.session$STRM == '1229'] <- "Fall 22"
chem.success.rate.session$STRM[chem.success.rate.session$STRM == '1239'] <- "Fall 23"

chem.success.rate.session.plot <- ggplot(chem.success.rate.session ,aes(reorder(x= session.count, -n),n, fill = as.factor(pass_rate))) + 
  geom_col(position = position_dodge()) + xlab("Session Count Category")+ylab("Students")+
  geom_text(aes(label = prop%>% scales::percent(accuracy = 1)),  position = position_dodge(0.7),vjust= 0.75, hjust = 0, size = 3)+
  ggtitle("Chem General Students Success by Session Count")+ facet_wrap(~STRM)+coord_flip()+
  theme(text = element_text(size = 15),axis.text.x=element_text(margin = margin(1, unit = "cm"),vjust =1))

chem.success.rate.session.plot+ scale_fill_manual(
  name = "Passed Their Course",
  values = c("tomato","skyblue2"),
  label = c("No","Yes"))

#persistence by session count for chem general
library(ggplot2)
library(dplyr)
chem.persistence.rate.session <- chem.general %>% dplyr::select(session.count, pers.f2s,STRM) %>% group_by(session.count,STRM) %>% 
  dplyr::count(pers.f2s) %>%  mutate(prop= n / sum(n))

chem.success.rate.session$STRM<- as.character(chem.success.rate.session$STRM)
chem.success.rate.session$STRM[chem.success.rate.session$STRM == '1199'] <- "Fall 19"
chem.success.rate.session$STRM[chem.success.rate.session$STRM == '1209'] <- "Fall 20"
chem.success.rate.session$STRM[chem.success.rate.session$STRM == '1219'] <- "Fall 21"
chem.success.rate.session$STRM[chem.success.rate.session$STRM == '1229'] <- "Fall 22"
chem.success.rate.session$STRM[chem.success.rate.session$STRM == '1239'] <- "Fall 23"

chem.success.rate.session.plot <- ggplot(chem.success.rate.session ,aes(reorder(x= session.count, -n),n, fill = as.factor(pass_rate))) + 
  geom_col(position = position_dodge()) + xlab("Session Count Category")+ylab("Students")+
  geom_text(aes(label = prop%>% scales::percent(accuracy = 1)),  position = position_dodge(0.7),vjust= 0.75, hjust = 0, size = 3)+
  ggtitle("Chem General Students Success by Session Count")+ facet_wrap(~STRM)+coord_flip()+
  theme(text = element_text(size = 15),axis.text.x=element_text(margin = margin(1, unit = "cm"),vjust =1))

chem.success.rate.session.plot+ scale_fill_manual(
  name = "Passed Their Course",
  values = c("tomato","skyblue2"),
  label = c("No","Yes"))

#success by session count for math general
library(ggplot2)
library(dplyr)
math.success.rate.session <- math.courses %>% dplyr::select(session.count, pass_rate,STRM) %>% group_by(session.count,STRM) %>% 
  dplyr::count(pass_rate) %>%  mutate(prop= n / sum(n))

math.success.rate.session$STRM<- as.character(math.success.rate.session$STRM)
math.success.rate.session$STRM[math.success.rate.session$STRM == '1199'] <- "Fall 19"
math.success.rate.session$STRM[math.success.rate.session$STRM == '1209'] <- "Fall 20"
math.success.rate.session$STRM[math.success.rate.session$STRM == '1219'] <- "Fall 21"
math.success.rate.session$STRM[math.success.rate.session$STRM == '1229'] <- "Fall 22"
math.success.rate.session$STRM[math.success.rate.session$STRM == '1239'] <- "Fall 23"

math.success.rate.session.plot <- ggplot(math.success.rate.session ,aes(reorder(x= session.count, -n),n, fill = as.factor(pass_rate))) + 
  geom_col(position = position_dodge()) + xlab("Session Count Category")+ylab("Students")+
  geom_text(aes(label = prop%>% scales::percent(accuracy = 1)),  position = position_dodge(0.7),vjust= 0.75, hjust = 0, size = 3)+
  ggtitle("Math General Students Success by Session Count")+ facet_wrap(~STRM)+coord_flip()+
  theme(text = element_text(size = 15),axis.text.x=element_text(margin = margin(1, unit = "cm"),vjust =1))

math.success.rate.session.plot+ scale_fill_manual(
  name = "Passed Their Course",
  values = c("tomato","skyblue2"),
  label = c("No","Yes"))

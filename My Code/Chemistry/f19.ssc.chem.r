##################################Remove embedded data from ssc_tutor
library(dplyr)
success_center = anti_join(ssc_tutor, embedded_tutor, by = c("Subtask","Taskdesc"))

unique(success_center$Subtask)
unique(success_center$Taskdesc)

#remove residual embedded information
success_center = success_center %>% filter(Taskdesc != 'FA23 EMB Tutoring PHIL' & 
                                             Taskdesc != 'FA23 EMB Tutoring AP' & 
                                             Taskdesc != 'FA23 EMB Tutoring PHIL' & 
                                             Taskdesc != 'FA23 EMB Tutoring CHEM' &
                                             Taskdesc != 'FA23 EMB Tutoring ACCT')

#students who visited the ssc in fall of 2019
f19.ssc <-success_center %>% filter(between(Logintime, as.Date('2019-08-19'), as.Date('2019-12-20'))) # 24165 students
unique(f19.ssc$Taskdesc)
unique(f19.ssc$Subtask)

#students who visited the ssc in fall of 2019 for chemistry tutoring
f19.ssc.chem = filter(f19.ssc, grepl('Chemistry', Subtask))
#f19.ssc.chem = f19.ssc %>% filter(str_detect(Subtask, 'Chemistry'))
unique(f19.ssc.chem$Taskdesc)
unique(f19.ssc.chem$Subtask)

#select students who enrolled in chemistry courses in fall 2019
library(tidyverse)
library(stringr)
fall.2019 <- enrollment %>% filter(str_detect(STRM, '199'))
length(unique(fall.2019$Student_ID)) # 24639 student enrolled in fall 2019
fall.2019.chem <- fall.2019 %>% filter(str_detect(SUBJECT, 'CHEM'))

#compare number of chemistry students who went to tutoring to those who didn't
length(unique(fall.2019.chem$Student_ID)) #1078 chem students total
length(unique(f19.ssc.chem$Student_ID)) #215 went to tutoring, should be 199

#remove students who went to tutoring from enrollment table
f19.chem.notutoring = anti_join(fall.2019.chem, f19.ssc.chem, by = "Student_ID")
#get enrollment info for students who went to tutoring
f19.chem.tutoring = enrollment %>% filter(str_detect(STRM, '199')) %>% filter(str_detect(SUBJECT, 'CHEM')) %>% filter(Student_ID %in% f19.ssc.chem$Student_ID)
length(unique(f19.chem.notutoring$Student_ID)) #879 students didn't go to tutoring
length(unique(f19.chem.tutoring$Student_ID)) #199 students went to tutoring

#Why don't the totals on lines 32, 33, 39, and 40, match?
#too many students in line 33

#remove students with zero enrollment
f19.chem.notutoring <- f19.chem.notutoring %>%filter(enrolled != 0) #1723 of 5915 dropped
f19.chem.tutoring <- f19.chem.tutoring %>%filter(enrolled != 0) #60 of 1634 dropped 

# removing students with no ethnicity data 
ethn.inx <-which(!(fall.2019.chem$Student_ID %in% demographics$Student_ID),arr.ind = TRUE )
eth.not.present <- fall.2019.chem[ethn.inx,] ## some students not found?????
no.ethnicity <- eth.not.present$Student_ID
f19.chem.notutoring <- f19.chem.notutoring %>%  filter(!(Student_ID %in% no.ethnicity))
f19.chem.tutoring <- f19.chem.tutoring %>%  filter(!(Student_ID %in% no.ethnicity))

#count pass rates for students who attended and didn't attend tutoring
f.19.chem.notutoring.enroll.pass <-f19.chem.notutoring %>%  group_by(Student_ID) %>% 
  summarise(courses_enrolled = sum(enrolled), courses_passed = sum(success), pass_rate = sum(success)/sum(enrolled))

f.19.chem.tutoring.enroll.pass <-f19.chem.tutoring %>%  group_by(Student_ID) %>% 
  summarise(courses_enrolled = sum(enrolled), courses_passed = sum(success), pass_rate = sum(success)/sum(enrolled))

#remove students not enrolled from ssc logs
#f19.ssc.math = f19.ssc.math %>% filter(Student_ID %in% f.19.tutoring.enroll.pass$Student_ID)
f19.ssc.chem = f19.ssc.chem %>% filter(Student_ID %in% f.19.chem.tutoring.enroll.pass$Student_ID)

#Count tutoring sessions and times for students who attended tutoring
chem_stu_id =f19.ssc.chem$Student_ID
chem_in_time = f19.ssc.chem$Logintime
chem_out_time = f19.ssc.chem$Logouttime

chem_time_diff = difftime(chem_out_time, chem_in_time, units = "mins")
chem_time_diff = as.double(chem_time_diff)
chem_time_df = data.frame(chem_stu_id, chem_in_time,
                          chem_out_time, chem_time_diff)

aggregate_chem = aggregate(chem_time_diff ~ chem_stu_id, data = chem_time_df, FUN = sum)

##### 4.3) counting the number of times the student went to tutoring  #####
session.count.ssc <- f19.ssc.chem %>% count(Student_ID) %>% select(n)
#Add ethnicity
ethnicity.chem.ssc <- demographics %>% filter(Student_ID %in% f.19.chem.tutoring.enroll.pass$Student_ID) %>% arrange(Student_ID) %>% select(ethnicity)
aggregate_chem$ethnicity = ethnicity.chem.ssc #add ethnicity to table
aggregate_chem$sessions = session.count.ssc #add session count to table
aggregate_chem$enrolled = f.19.chem.tutoring.enroll.pass$courses_enrolled
aggregate_chem$passed = f.19.chem.tutoring.enroll.pass$courses_passed
aggregate_chem$pass_rate = f.19.chem.tutoring.enroll.pass$pass_rate
aggregate_chem$STRM = rep(1199, dim(aggregate_chem)[1])
aggregate_chem$tutor = rep('yes',nrow(aggregate_chem))

#add information to students who did not attend tutoring
f.19.chem.notutoring.enroll.pass$ethnicity = demographics %>% filter(Student_ID %in% f.19.chem.notutoring.enroll.pass$Student_ID) %>% arrange(Student_ID) %>% select(ethnicity)
f.19.chem.notutoring.enroll.pass$math_time_diff = rep(0,dim(f.19.chem.notutoring.enroll.pass)[1]) 
f.19.chem.notutoring.enroll.pass$sessions = rep(0,dim(f.19.chem.notutoring.enroll.pass)[1]) 
f.19.chem.notutoring.enroll.pass$STRM = rep(1199,dim(f.19.chem.notutoring.enroll.pass)[1])
#reorganize columns
f.19.chem.notutoring.enroll.pass = f.19.chem.notutoring.enroll.pass[c("Student_ID", "math_time_diff", "ethnicity", "sessions","courses_enrolled","courses_passed","pass_rate","STRM")]
#make names match
f.19.chem.notutoring.enroll.pass <- setNames(f.19.chem.notutoring.enroll.pass, names(aggregate_chem)) 
f.19.chem.notutoring.enroll.pass$tutor = rep('no',nrow(f.19.chem.notutoring.enroll.pass))

#complete table
fall.19.SSC.chem = rbind(aggregate_chem,f.19.chem.notutoring.enroll.pass)
fall.19.SSC.chem = fall.19.SSC.chem %>% arrange(chem_stu_id)

#add cumulative gpa
gpa.chem = academic_career %>% filter(Student_ID %in% fall.19.SSC.chem$chem_stu_id) %>% filter(str_detect(STRM, '199')) %>% arrange(Student_ID) %>% select(CUM_GPA)
fall.19.SSC.chem$cum_gpa = gpa.chem

########################add persistence/retention
f19.reten.pers = retention_persistence %>% filter(str_detect(STRM, '199'))
index <- which(f19.reten.pers$Student_ID %in% fall.19.SSC.chem$chem_stu_id, arr.ind = TRUE)
f19.reten.pers.chem <- f19.reten.pers[index,]
f19.reten.pers.chem = f19.reten.pers.chem %>% arrange(Student_ID)
fall.19.SSC.chem$ret.f2f = f19.reten.pers.chem$f2f_num/f19.reten.pers.chem$f2f_denom
fall.19.SSC.chem$pers.f2s = f19.reten.pers.chem$f2s_num/f19.reten.pers.chem$f2s_denom

#categorize session count and time spent
session.count <- cut(fall.19.SSC.chem$sessions$n, breaks=c(0,3,4,14,37), labels=c('rare', 'occasionally', 'frequently', 'very.frequently')) 
fall.19.SSC.chem$session.count = session.count
time.spent <- cut(fall.19.SSC.chem$chem_time_diff, breaks=c(0, 60, 153, 386, 467,2800), labels=c('very.low', 'low', 'average', 'above.average','substantial'))
fall.19.SSC.chem$time.spent = time.spent

#remove students with NA of unknown ethnicity
fall.19.SSC.chem <-fall.19.SSC.chem %>% filter(!str_detect(ethnicity$ethnicity, "NA")) %>% filter(!str_detect(ethnicity$ethnicity, "Unknown"))
# combine alaskan native and native hawaiian
fall.19.SSC.chem$ethnicity[fall.19.SSC.chem$ethnicity == "American Indian or Alaska Native"] <- "Native American or Pacific Islander"
fall.19.SSC.chem$ethnicity[fall.19.SSC.chem$ethnicity == "Native Hawaiian or Other Pacific Islander"] <- "Native American or Pacific Islander"


###############################write table##########################################
#fall.19.embedded1 = read.csv("C:/Users/jbrin/Documents/chem/ET.F19.CHEM.csv",header=T)
# fall.19.embedded = fall.19.embedded[,-1]

embedded = read.csv("C:/Users/jbrin/Documents/chem/ET.chem.ids.csv",header=T)
fall.19.embedded = embedded %>% filter(term == 1199)

test2 = fall.19.SSC.chem
islst <- sapply(test2, is.list)
# ASSERT: number of rows and embedded lists/frames are the same length
all(nrow(test2) == sapply(test2[islst], lengths))
test2 <- do.call("cbind.data.frame", c(test2[!islst], test2[islst], stringsAsFactors = FALSE))
table(test2$tutor)
test2 = test2 %>% filter(!(chem_stu_id %in% fall.19.embedded$Student_ID))
table(test2$tutor)

write.csv(test2,"C:/Users/jbrin/Documents/fall.19.ssc.chem.csv",row.names = F)
#new=read.csv("C:/Users/jbrin/Documents/chem/fall.19.ssc.chem.csv",header=T)


#modeling##################################################################################

#logistic regression
tutor.yes = fall.19.SSC.chem %>% filter(sessions > 0)
success.tutored = glm(pass_rate ~ ethnicity$ethnicity + cum_gpa$CUM_GPA + time.spent + session.count, data = tutor.yes, family = "binomial")
#success.tutored2 = glm(pass_rate ~ ethnicity$ethnicity + cum_gpa$CUM_GPA + chem_time_diff + sessions$n, data = tutor.yes, family = "binomial")

tutor.no = fall.19.SSC.chem %>% filter(sessions == 0)
success.nontutored = glm(pass_rate ~ ethnicity$ethnicity + cum_gpa$CUM_GPA, data = tutor.no, family = "binomial")

chem.copy = fall.19.SSC.chem

chem.copy$tutor[chem.copy$tutor=='no'] <- 0
chem.copy$tutor[chem.copy$tutor=='yes'] <- 1
chem.copy$tutor <- as.numeric(chem.copy$tutor)
chem.copy$tutor <- as.factor(chem.copy$tutor)

chem.copy$ethnicity$ethnicity = as.factor(chem.copy$ethnicity$ethnicity)
chem.copy$ethnicity <- relevel(chem.copy$ethnicity, ref = 3)

success.overall = glm(pass_rate ~ ethnicity$ethnicity + cum_gpa$CUM_GPA + chem_time_diff + sessions$n +tutor , data = chem.copy, family = "binomial")

#kmeans clustering
chem.copy = chem.copy %>% select(-c(session.count,time.spent,STRM,ethnicity,chem_stu_id,enrolled,passed))
chem.copy = na.omit(chem.copy) 

out = kmeans(scale(chem.copy),5,5)
out
library(factoextra)
library(cluster)
fviz_cluster(out, data = chem.copy)

#need a correlation plot
library(psych)
corPlot(chem.copy)
cor(chem.copy)

#linear model
lin.success = lm(pass_rate ~ ethnicity$ethnicity + cum_gpa$CUM_GPA + time.spent + session.count, data=fall.19.SSC.chem)

######################40/60
balanced_df = chem.copy %>% filter(tutor == 1) %>% bind_rows(chem.copy %>% filter(tutor==0) %>% sample_n(293, replace = FALSE))
success1 = glm(pass_rate ~ ethnicity$ethnicity + cum_gpa$CUM_GPA + chem_time_diff + sessions$n +tutor , data = balanced_df, family = "binomial")
summary(success1)
###########################################################################################


#plot gpa densities for two groups
f19.gpa.notutoring = fall.19.SSC.chem %>% filter(sessions == 0)
f19.gpa.tutoring = fall.19.SSC.chem %>% filter(sessions > 0)

plot(density(fall.19.SSC.chem$cum_gpa$CUM_GPA[fall.19.SSC.chem$sessions > 0]),col="blue", main="GPA")
lines(density(fall.19.SSC.chem$cum_gpa$CUM_GPA[fall.19.SSC.chem$sessions == 0]),col="red")

gpa.curve <-ggplot(fall.19.SSC.chem,aes(x = cum_gpa$CUM_GPA, col=tutor))+geom_density(lwd = 1,alpha = 0.5, position = "identity")+
  ggtitle("F19 Cumulative GPA's of Chem Students")+xlab("Cumulative GPA")
gpa.curve+ scale_color_manual(values = c("red","blue"))

#plot gpa by ethnicity
gpa.cur.curve <-ggplot(f19.gpa.tutoring,aes(x = cum_gpa$CUM_GPA, col =ethnicity$ethnicity ))+geom_density(alpha = 0.5, position = "identity")+
  ggtitle("F19 Cumulative GPA of Chem Students, Tutoring")

gpa.cur.curve2 <-ggplot(f19.gpa.notutoring,aes(x = cum_gpa$CUM_GPA, col =ethnicity$ethnicity ))+geom_density(alpha = 0.5, position = "identity")+
  ggtitle("F19 Cumulative GPA of Chem Students, No Tutoring")

library(cowplot)
plot_grid(gpa.cur.curve,gpa.cur.curve2)

#gpa curve grouped by time spent in tutoring
gpa.cur.time <-ggplot(f19.gpa.tutoring,aes(x = cum_gpa$CUM_GPA, col = time.spent ))+geom_density(alpha = 0.5, position = "identity")+
  ggtitle("F19 Cumulative GPA of Chem Students")
gpa.cur.time + scale_color_manual(name = "Minutes Spent", labels=c("very low: 0-59", "low: 60-152", "average: 153-385","above average: 386-466","substantial: 467-2800"),values = c("pink2","orange","purple","navy", "forestgreen"))

#gpa curve grouped by session count
gpa.cur.sess <-ggplot(f19.gpa.tutoring,aes(x = cum_gpa$CUM_GPA, col = session.count ))+geom_density(alpha = 0.5, position = "identity")+
  ggtitle("F19 Cumulative GPA of Chem Students")

#plot success rates for two groups
chem_tutoring_success = sum(fall.19.SSC.chem$passed[fall.19.SSC.chem$sessions > 0])/sum(fall.19.SSC.chem$enrolled[fall.19.SSC.chem$sessions > 0])
chem_notutoring_success = sum(fall.19.SSC.chem$passed[fall.19.SSC.chem$sessions == 0])/sum(fall.19.SSC.chem$enrolled[fall.19.SSC.chem$sessions == 0])

barplot(c(chem_tutoring_success,chem_notutoring_success),names.arg = c("Tutoring","No Tutoring"),xlab = "Chemistry Success Rates",col = c("red","blue"),ylim=c(0,.8))

#persistence by ethnicity

success.rate.tutoring <- fall.19.SSC.chem %>% filter(passed<2) %>% select(enrolled, passed, tutor) %>% group_by(tutor,enrolled) %>% 
  count(passed) %>%  mutate(prop= n / sum(n))

#########################################################################################
#persistence by ethnicity
###################################################################################
tutor.yes <- fall.19.SSC.chem %>% select(pers.f2s, ethnicity, tutor) %>% group_by(tutor,ethnicity) %>% count(pers.f2s) %>%  mutate(prop= n / sum(n))
tutor.yes2 = tutor.yes %>% filter(pers.f2s>0)

  persist.chem.eth = ggplot(tutor.yes2, aes(reorder(tutor,-prop),prop, fill = ethnicity$ethnicity)) + geom_col(position = position_dodge())+labs()+coord_flip()+geom_text(aes(label = round(prop,2)),position = position_dodge(width=.9),hjust = -0.15, size = 5,color="#28A8E0")+
    ggtitle("Fall to Spring Persistence by Ethnicity")+xlab("Tutoring")+ylab("Persistence Rate")+scale_fill_manual(values = c("#445B9A", "#00B389","#B2C3FF","#D3A517","#A23248","#00B7C6"))

persist.chem.eth+labs(fill = "Ethnicity") 

#####################################################################################
#retention by ethnicity
#######################################################################################
tutor.ret <- fall.19.SSC.chem %>% select(ret.f2f, ethnicity, tutor) %>% group_by(tutor,ethnicity) %>% count(ret.f2f) %>%  mutate(prop= n / sum(n))
tutor.ret2 = tutor.ret %>% filter(ret.f2f>0)

ret.chem.eth = ggplot(tutor.ret2, aes(reorder(tutor,-prop),prop, fill = ethnicity$ethnicity)) + geom_col(position = position_dodge())+labs()+coord_flip()+geom_text(aes(label = round(prop,2)),position = position_dodge(width=.9),hjust = -0.15, size = 5,color="#28A8E0")+
  ggtitle("Fall to Fall Retention by Ethnicity")+xlab("Tutoring")+ylab("Retention Rate")+scale_fill_manual(values = c("#445B9A", "#00B389","#B2C3FF","#D3A517","#A23248","#00B7C6"))
                                                                                                           

ret.chem.eth+labs(fill = "Ethnicity") 

#test
tutor.yes=fall.19.SSC.chem%>%filter(sessions>0)
hisp.ret = tutor.yes%>%filter(ethnicity$ethnicity=="Hispanic/Latino")
hisp.retained = hisp.ret%>%filter(ret.f2f==1)
################################################################miriams code
tutored <- fall.19.SSC.chem %>% filter(sessions>0)
persist.prop <- tutored %>% group_by(ethnicity,tutor,pers.f2s) %>% summarise(n = n()) %>% mutate(Freq = n/sum(n))

persist.row.plot <- ggplot(persist.prop ,aes(x = reorder(ethnicity,-Freq), Freq)) + facet_wrap("pers.f2s")

persist.row.plot  + geom_bar(stat = 'identity',aes(fill = ethnicity$ethnicity)) +
  geom_text(aes(label=Freq%>% scales::percent())) +
  ylab('Percent of Cylinder Group, %') +
  coord_flip()
#############################################################################


persist.chem.eth.notut <- ggplot(tutor.no, aes(x =pers.f2s, fill = ethnicity$ethnicity)) + geom_bar()+labs()+
  stat_count(geom = "text", colour = "navy", size =3,
             aes(label = ..prop.. %>% scales::percent()),position=position_stack(vjust=0.5))+
  ggtitle("Fall to Spring Persistence, Not Tutored")+xlab("Persistence Rate")+ylab("Students")
persist.chem.eth.notut+labs(fill = "Ethnicity")

#Retention by ethnicity
# tutor.yes <- fall.19.SSC.chem %>% filter(sessions>0)
# tutor.no <- fall.19.SSC.chem %>% filter(sessions==0)
reten.chem.eth <- ggplot(tutor.yes, aes(x = ret.f2f, fill = ethnicity$ethnicity)) + geom_bar()+labs()+
  stat_count(geom = "text", colour = "navy", size =3,
             aes(label = ..prop.. %>% scales::percent()),position=position_stack(vjust=0.5))+
  ggtitle("Fall to Fall Retention, Tutored")+xlab("Retention Rate")+ylab("Students")
reten.chem.eth+labs(fill = "Ethnicity")
reten.chem.eth.notut <- ggplot(tutor.no, aes(x =ret.f2f, fill = ethnicity$ethnicity)) + geom_bar()+labs()+
  stat_count(geom = "text", colour = "navy", size =3,
             aes(label = ..prop.. %>% scales::percent()),position=position_stack(vjust=0.5))+
  ggtitle("Fall to Fall Retention, Not Tutored")+xlab("Retention Rate")+ylab("Students")
reten.chem.eth.notut+labs(fill = "Ethnicity")+scale_fill_manual(name = "Attended Tutoring",
                                                                values = c("tomato", "skyblue2"),
                                                                labels = c("No","Yes"))

#persistence and retention by tutored and not tutored
persist.chem.tutor <- ggplot(fall.19.SSC.chem, aes(x = pers.f2s, fill = tutor)) + geom_bar()+labs()+
  stat_count(geom = "text", colour = "navy", size =3,
             aes(label = ..prop.. %>% scales::percent()),position=position_stack(vjust=0.5))+
  ggtitle("Fall to Spring Persistence")+xlab("Persistence Rate")+ylab("Students")

reten.chem.tutor <- ggplot(fall.19.SSC.chem, aes(x =ret.f2f, fill = tutor)) + geom_bar()+labs()+
  stat_count(geom = "text", colour = "navy", size =3,
             aes(label = ..prop.. %>% scales::percent()),position=position_stack(vjust=0.5))+
  ggtitle("Fall to Fall Retention")+xlab("Retention Rate")+ylab("Students")+scale_fill_manual(name = "Attended Tutoring",
                                                                                              values = c("tomato", "skyblue2"),
                                                                                              labels = c("No","Yes"))

library(ggpubr)
ggarrange(persist.chem.tutor+
            scale_fill_manual(name = "Attended Tutoring",
                              values = c("tomato", "skyblue2"),
                              labels = c("No","Yes")),
          reten.chem.tutor+
            scale_fill_manual(name = "Attended Tutoring",
                              values = c("tomato", "skyblue2"),
                              labels = c("No","Yes")),  ncol=2,nrow=1         )
##########################success by tutored and non tutored (add persistence and retention)
success.rate.tutoring <- fall.19.SSC.chem %>% filter(passed<2) %>% select(enrolled, passed, tutor) %>% group_by(tutor,enrolled) %>% 
  count(passed) %>%  mutate(prop= n / sum(n))

plot66 <- ggplot(success.rate.tutoring ,aes(tutor, prop, fill =as.factor(passed))) + 
  geom_col(position = position_dodge()) + 
  geom_text(aes(label = round(prop,2)), position = position_dodge(width=1), size =4)+
  ggtitle("Success Rates by Groups")+scale_fill_manual(name = "Courses Passed",
                                                       values = c("orangered","cornflowerblue","navy"),
                                                       labels=c("0","1","2"))+xlab("Tutored")+ylab("Success Rate")+coord_flip()

plot_grid(persist.chem.tutor,reten.chem.tutor,plot66)

#######################table with persistence, retention, and success rates for tutored vs. non-tutored
table1 = data.frame(
  type = c("Success","Persistence","Retention"),
  notutor = c(0.58,0.87,0.652),
  tutor = c(0.73,0.94,0.722))

heatmap(table1)

table1_data_long <- table1 %>%
  pivot_longer(cols =-type, names_to = "Average_Value", values_to = "Value")

ggplot(table1_data_long, aes(x = type, y = Average_Value, fill = Value)) +
  geom_tile(color = "white") +
  scale_fill_gradient(low = "white", high = "deepskyblue3") +
  geom_text(aes(label = round(Value, 2)), color = "black") +  # Add text
  scale_fill_gradient(low = "white", high = "deepskyblue3", limits = c(0, NA), na.value = NA) +  # Scale fill color row-wise
  labs(title = "Fall 2019 Chemistry Student Outcomes",
       x = "Outcomes",
       y = "",
       fill = "Value") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# library(cowplot)
# plot_grid(plot1,plot2)

##########################success by ethnicity
success.rate.ethnicity.tutoring <- fall.19.SSC.chem %>% filter(sessions>0) %>% select(ethnicity, passed) %>% group_by(ethnicity) %>% 
  count(passed) %>%  mutate(prop= n / sum(n))

plot1 <- ggplot(success.rate.ethnicity.tutoring ,aes(ethnicity$ethnicity, prop, fill =as.factor(passed))) + 
  geom_col(position = position_dodge()) + 
  geom_text(aes(label = round(prop,2)), position = position_dodge(width=1), size =5,color="#28A8E0",hjust=.01)+
  ggtitle("Success Rates With Tutoring")+xlab("Ethnicity")+ylab("Success Rate")+scale_fill_manual(name = "Success",
                                                                                                      values = c("#002E66", "#A8AABC"),
                                                                                                      labels = c("No","Yes"))+theme(axis.text.x=element_text(angle=45,margin = margin(1, unit = "cm"),vjust =1))+coord_flip()+theme(legend.position="none")
# plot1+scale_fill_manual(name = "Success",
#                         values = c("tomato", "skyblue2"),
#                         labels = c("No","Yes"))        

success.rate.ethnicity.notutoring <- fall.19.SSC.chem %>% filter(sessions==0 &  enrolled< 2) %>% select(ethnicity, passed) %>% group_by(ethnicity) %>% 
  count(passed) %>%  mutate(prop= n / sum(n))

plot2 <- ggplot(success.rate.ethnicity.notutoring ,aes(ethnicity$ethnicity, prop, fill =as.factor(passed))) + 
  geom_col(position = position_dodge()) + 
  geom_text(aes(label = round(prop,2)), position = position_dodge(width=1), size =5,color="#28A8E0",hjust=.01)+
  ggtitle("Success Rates Without Tutoring")+ylab("Success Rate")+scale_fill_manual(name = "Success",
                                                                                                         values = c("#002E66", "#A8AABC"),
                                                                                                         labels = c("No","Yes"))+theme(axis.text.x=element_text(angle=45,margin = margin(1, unit = "cm"),vjust =1))+coord_flip()+theme(axis.text.y=element_blank(),axis.title.y=element_blank())
# plot2+scale_fill_manual(name = "Success",
#                         values = c("tomato", "skyblue2"),
#                         labels = c("No","Yes"))
library(cowplot)
plot_grid(plot1,plot2,rel_widths = c(2,1.7))

#success rate by time spent?
success.rate.timespent.tutoring <- fall.19.SSC.chem %>% filter(sessions>0) %>% select(time.spent, passed) %>% group_by(time.spent) %>% 
  count(passed) %>%  mutate(prop= n / sum(n))

plot3 <- ggplot(success.rate.timespent.tutoring ,aes(time.spent, n, fill =as.factor(passed))) + 
  geom_col(position = position_stack()) + 
  geom_text(aes(label = round(prop,2)), position = position_stack(vjust =0.5), size =3)+
  ggtitle("Success Rates by Time Spent in Tutoring")

#success rate by # of sessions
success.rate.sessions.tutoring <- fall.19.SSC.chem %>% filter(sessions>0) %>% select(session.count, passed) %>% group_by(session.count) %>% 
  count(passed) %>%  mutate(prop= n / sum(n))

plot4 <- ggplot(success.rate.sessions.tutoring ,aes(session.count, prop, fill =as.factor(passed))) + 
  geom_col(position = position_dodge()) + 
  geom_text(aes(label = round(prop,2)), position = position_dodge(width =1), size =7,color="#28A8E0",hjust = .01)+
  ggtitle("Success Rates by Number of Tutoring Sessions")+xlab("Session Count")+ylab("Success Rate")+coord_flip()
plot4 + scale_fill_manual(name = "Success",
                          values = c("#002E66", "#A8AABC"),
                          labels = c("No","Yes")) 

#Time spent 
tutor.yes <- fall.19.SSC.chem %>% filter(sessions>0) 
time_spent.plot = ggplot(tutor.yes, aes(time.spent,color=time.spent,sort.val = "desc")) +
  geom_bar(fill = "white", lwd = 1.5)+ 
  stat_count(geom = "text", colour = "navy", size =4,
             aes(label = ..count..),position=position_stack(vjust=0.5))+
  ggtitle("F19 Total Time Chem. Students Spent at SSC")+xlab("")+labs(color ="")

time_spent.plot+
  scale_color_manual(name = "Minutes Spent",
                     values = c("pink2","orange","purple","navy", "forestgreen"),
                     labels=c("very.low: 0-59", "low: 60-152", 
                              "average: 153-385","above.average: 386-466",
                              "substantial: 467-2800"))

# session count
session_count.plot = ggplot(tutor.yes, aes(session.count,color=session.count,sort.val = "desc")) +
  geom_bar(fill = "white", lwd = 1.5)+ 
  stat_count(geom = "text", colour = "navy", size =4,
             aes(label = ..count..),position=position_stack(vjust=0.5))+
  ggtitle("SSC Session Count for Chem Students")+xlab("")+labs(color ="")

session_count.plot+
  scale_color_manual(name = "Count",
                     values = c("black","limegreen","blue", "red"), 
                     labels = c("rare: 1-2", "occasionally: 3-4", "frequently: 5-14","'very.frequently: 15-26"),
                     guide = guide_legend(override.aes = list(fill = c("NA","NA","NA","NA"))))

library(cowplot)
plot_grid(time_spent.plot+
            scale_color_manual(name = "Minutes Spent",
                               values = c("pink2","orange","purple","navy", "forestgreen"),
                               labels=c("very.low: 0-59", "low: 60-152", 
                                        "average: 153-385","above.average: 386-466",
                                        "substantial: 467-2800")),session_count.plot+
            scale_color_manual(name = "Count",
                               values = c("black","limegreen","blue", "red"), 
                               labels = c("rare: 1-2", "occasionally: 3-4", "frequently: 5-14","'very.frequently: 15-26"),
                               guide = guide_legend(override.aes = list(fill = c("NA","NA","NA","NA")))))

#Time spent and count by ethnicity
#Time spent 
tutor.yes <- fall.19.SSC.chem %>% filter(sessions>0) 
time_spent.ethnicity = ggplot(tutor.yes, aes(time.spent,fill=ethnicity$ethnicity,sort.val = "desc")) +
  geom_bar(aes(col=time.spent), lwd = 1.5)+ 
  stat_count(geom = "text", colour = "navy", size =4,
             aes(label = ..count..),position=position_stack(vjust=0.5))+
  ggtitle("F19 Total Time Chem. Students Spent at SSC, Grouped by Ethnicity")+xlab("")+labs(color ="")

time_spent.ethnicity+
  scale_color_manual(name = "Minutes Spent",
                     values = c("pink2","orange","purple","navy", "forestgreen"),
                     labels=c("very.low: 0-59", "low: 60-152", 
                              "average: 153-385","above.average: 386-466",
                              "substantial: 467-2800"),
                               guide= guide_legend(override.aes = list(fill = c("NA","NA","NA","NA","NA"))))+
                    scale_fill_manual(name = "Ethnicity",
                    values = c("mistyrose","lightgoldenrod1","lightgreen","mediumturquoise", "cornflowerblue","plum2"))

count.ethnicity = ggplot(tutor.yes, aes(session.count,fill=ethnicity$ethnicity,sort.val = "desc")) +
  geom_bar(aes(col=session.count), lwd = 1.5)+ 
  stat_count(geom = "text", colour = "navy", size =4,
             aes(label = ..count..),position=position_stack(vjust=0.5))+
  ggtitle("F19 Chem. Student Tutoring Session Count, Grouped by Ethnicity")+xlab("")+labs(color ="")

count.ethnicity+
  scale_color_manual(name = "Session Count",
                     values = c("black","limegreen","blue", "red"), 
                     labels = c("rare: 1-2", "occasionally: 3-4", "frequently: 5-14","'very.frequently: 15-26"),
                     guide= guide_legend(override.aes = list(fill = c("NA","NA","NA","NA"))))+
  scale_fill_manual(name = "Ethnicity",
                    values = c("mistyrose","lightgoldenrod1","lightgreen","mediumturquoise", "cornflowerblue","plum2"))

plot_grid(time_spent.ethnicity+
            scale_color_manual(name = "Minutes Spent",
                               values = c("pink2","orange","purple","navy", "forestgreen"),
                               labels=c("very.low: 0-59", "low: 60-152", 
                                        "average: 153-385","above.average: 386-466",
                                        "substantial: 467-2800"),
                               guide= guide_legend(override.aes = list(fill = c("NA","NA","NA","NA","NA"))))+
            scale_fill_manual(name = "Ethnicity",
                              values = c("mistyrose","lightgoldenrod1","lightgreen","mediumturquoise", "cornflowerblue","plum2"))
,count.ethnicity+
  scale_color_manual(name = "Session Count",
                     values = c("black","limegreen","blue", "red"), 
                     labels = c("rare: 1-2", "occasionally: 3-4", "frequently: 5-14","'very.frequently: 15-26"),
                     guide= guide_legend(override.aes = list(fill = c("NA","NA","NA","NA"))))+
  scale_fill_manual(name = "Ethnicity",
                    values = c("mistyrose","lightgoldenrod1","lightgreen","mediumturquoise", "cornflowerblue","plum2")))



######################################################################################

ethnicity.row.plot <- ggplot(fall.19.SSC.chem ,aes(x = fct_infreq(ethnicity$ethnicity), y = tutor, fill = ethnicity$ethnicity, position = 'fill'))

# the plots are in descending order 
ethnicity.row.plot +geom_bar(aes( y=..count../tapply(..count.., ..x.. ,sum)[..x..]), position="dodge" ) +
  geom_text(aes( y=..count../tapply(..count.., ..x.. ,sum)[..x..], label=scales::percent(..count../tapply(..count.., ..x.. ,sum)[..x..]) ),stat="count", position=position_dodge(0.9), vjust=-0.5, hjust =1)+
  ylab('Percent of Cylinder Group, %') +
  scale_y_continuous(labels = scales::percent)

#ethnicity proportions
ethn.prop <- fall.19.SSC.chem %>% select(ethnicity) %>% group_by(ethnicity) %>% count(ethnicity) %>%  mutate(prop= n / sum(n))

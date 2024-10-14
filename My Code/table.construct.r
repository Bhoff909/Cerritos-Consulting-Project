setwd("C:/Users/jbrin/Documents/chem")
fall.19 = read.csv("C:/Users/jbrin/Documents/chem/fall.19.ssc.chem.1.csv")

################################remove embedded students from ssc logs
fall.19.embedded = read.csv("C:/Users/jbrin/Documents/chem/ET.F19.CHEM.csv",header=T)
fall.19.embedded = fall.19.embedded[,-1]

#colnames(fall.19.SSC.chem)[1] <- "Student_ID"
#test = anti_join(fall.19.SSC.chem, F19.chem.ET, by = "Student_ID")
test = fall.19.SSC.chem %>% filter(!(chem_stu_id %in% fall.19.embedded$Student_ID))
table(test$tutor)

fall.20.embedded = read.csv("C:/Users/jbrin/Documents/chem/ET.F20.CHEM.csv",header=T)
fall.20.embedded = fall.20.embedded[,-1]
test2 = fall.20.SSC.chem %>% filter(!(chem_stu_id %in% fall.20.embedded$Student_ID))
table(test2$tutor)

write.csv(test2,"fall.19.test.csv",row.names = F)
fall = read.csv("fall.19.test.csv")

class(fall.19.SSC.chem)

###############################fixed table issue#########################
islst <- sapply(test2, is.list)
# ASSERT: number of rows and embedded lists/frames are the same length
all(nrow(test2) == sapply(test2[islst], lengths))
test2 <- do.call("cbind.data.frame", c(test2[!islst], test2[islst], stringsAsFactors = FALSE))
write.csv(test2,"fall.19.test.csv",row.names = F)
new=read.csv("fall.19.test.csv",header=T)
#########################################################################

fall19 = read.csv("C:/Users/jbrin/Documents/math/fall.19.ssc.math.csv",header=T)
fall20 = read.csv("C:/Users/jbrin/Documents/math/fall.20.ssc.math.csv",header=T)
fall21 = read.csv("C:/Users/jbrin/Documents/math/fall.21.ssc.math.csv",header=T)
fall22 = read.csv("C:/Users/jbrin/Documents/math/fall.22.ssc.math.csv",header=T)
fall23 = read.csv("C:/Users/jbrin/Documents/math/fall.23.ssc.math.csv",header=T)

out = rbind(fall19,fall20,fall21,fall22,fall23)
write.csv(out,"C:/Users/jbrin/Documents/math/fall.19_23.ssc.math.csv",row.names = F)
test = read.csv("C:/Users/jbrin/Documents/math/fall.19_23.ssc.math.csv",header=T)


test$session.count[is.na(test$session.count)] <- "none"
test$time.spent[is.na(test$time.spent)] <- "none"
test$pass_rate[test$pass_rate > 0] <- 1
write.csv(test,"C:/Users/jbrin/Documents/math/fall.19_23.ssc.math.csv",row.names = F)

nbew = read.csv("C:/Users/jbrin/Documents/math/fall.19_23.ssc.math.csv",header=T)

library(dplyr)
nums = nbew %>% filter(nbew$enrolled> 1)
count = nums %>% filter(nums$tutor == "yes")

tut = nbew %>% filter(nbew$tutor == "yes")

###########################################chemistry
fall19 = read.csv("C:/Users/jbrin/Documents/fall.19.ssc.chem.csv",header=T)
fall20 = read.csv("C:/Users/jbrin/Documents/fall.20.ssc.chem.csv",header=T)
fall21 = read.csv("C:/Users/jbrin/Documents/fall.21.ssc.chem.csv",header=T)
fall22 = read.csv("C:/Users/jbrin/Documents/fall.22.ssc.chem.csv",header=T)
fall23 = read.csv("C:/Users/jbrin/Documents/fall.23.ssc.chem.csv",header=T)

out = rbind(fall19,fall20,fall21,fall22,fall23)

out$pass_rate <- out$passed/out$enrolled

out$ret.f2f[is.na(out$ret.f2f)] <- 0
out$pers.f2s[is.na(out$pers.f2s)] <- 0

out$session.count[is.na(out$session.count)] <- "none"
out$time.spent[is.na(out$time.spent)] <- "none"

gpa <- cut(out$CUM_GPA, breaks=c(0,2,3,4), labels=c('low', 'mid', 'high'))
out = cbind(out,gpa)
out$gpa[is.na(out$gpa)] <- "low"
out$gpa = as.factor(out$gpa)

colnames(out)[colnames(out) == "chem_stu_id"] = "Student_ID"
out$Student_ID = stringr::str_pad(out$Student_ID, side = 'left', width = 7, pad = '0')

gen.fg <- demographics %>% dplyr::select(gender,first_gen,Student_ID)
out2<-inner_join(out,gen.fg,"Student_ID") 

library(stats)
out2$session.count = as.factor(out2$session.count)
contrasts(out2$session.count)
out2$session.count <- relevel(out2$session.count, ref = "none")
out2$first_gen = as.factor(out2$first_gen)
contrasts(out2$first_gen)
out2$first_gen <- relevel(out2$first_gen, ref = "Not First Generation")

write.csv(out2,"C:/Users/jbrin/Documents/fall.19_23.ssc.chem.csv",row.names = F)

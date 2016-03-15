setwd("C:\\Users\\shikhagarg.CORP\\Documents\\R\\dyd\\test-date-your-data")
test = read.csv("test.csv")
internship = read.csv("Internship.csv")
student = read.csv("Student.csv")
test_int = left_join(test, internship)

test_int_f = test_int[which(colnames(test_int) %in% c("Internship_Profile","Start_Date","Skills_required","Stipend1","Internship_Location","Internship_deadline","Internship_ID","Student_ID","Earliest_Start_Date","Expected_Stipend","Minimum_Duration","Preferred_location","Is_Part_Time","Is_Shortlisted"))]
colnames(test_int_f) = paste("I",colnames(test_int_f),sep="_")

colnames(student) = paste("S",colnames(student),sep="_")

library(dplyr)
rollstudent = student %>% group_by(S_Student_ID,S_hometown,S_Institute_Category
                                   ,S_Institute_location,S_Stream,S_Year_of_graduation) %>%
    summarise(
        S_Current_year_mx = max(as.numeric(S_Current_year))
        ,S_Engg = ifelse(sum(ifelse(S_Degree %in% c("B.E","B.Tech","MCA"),1,0))>0,1,0)
        ,S_Exp = sum(ifelse(as.Date(S_End.Date,"%d-%m-%Y") < as.Date(S_Start.Date,"%d-%m-%Y")
                            ,0,round(difftime(as.Date(S_End.Date,"%d-%m-%Y"),as.Date(S_Start.Date,"%d-%m-%Y"), units= "days")/30/12,1)))
        ,S_Performance_PG = mean((S_Performance_PG/S_PG_scale)*100,na.rm=T)
        ,S_Performance_UG = mean((S_Performance_UG/S_UG_Scale)*100,na.rm=T)
        ,S_Performance_10th_12th = mean((S_Performance_12th + S_Performance_10th)/2 ,na.rm=T)
    )

newdata <- student[c("S_Student_ID","S_Profile")]
library(plyr)
library(dplyr)
newdata <- ddply(newdata, .(S_Student_ID), summarize, Profile = paste0(S_Profile, sep = "", collapse = ","))
detach("package:dplyr")

rollstudent <- left_join(rollstudent, newdata)

colnames(rollstudent)[which(colnames(rollstudent) == "Profile")] = "S_Profile"
colnames(rollstudent)[which(colnames(rollstudent) == "S_Student_ID")] = "I_Student_ID"

test_s1 = left_join(test_int_f, rollstudent)

apply(test_s1, 2, function(x)sum(is.na(x)))
test_s1$S_Exp[which(is.na(test_s1$S_Exp))] = 0
test_s1 = test_s1[which(!is.na(test_s1$S_Stream)),]

test_s2 = test_s1 

test_s2$joinig_gap = round(difftime(as.Date(test_s2$I_Start_Date,"%d-%m-%Y"),as.Date(test_s2$I_Earliest_Start_Date,"%d-%m-%Y"),units = "days")/30,1)
test_s2$intern_gap = round(difftime(as.Date(test_s2$I_Start_Date,"%d-%m-%Y"),as.Date(test_s2$I_Internship_deadline,"%d-%m-%Y"),units = "days")/30,1)

test_s2$I_Earliest_Start_Date = NULL
test_s2$I_Internship_deadline = NULL
test_s2$I_Start_Date = NULL
test_s2$I_Stipend1  = NULL

test_s2$Pref_Loc_dum = ifelse(test_s2$I_Preferred_location %in%
                                   c("IHFG",    
                                     "IHJB",     
                                     "IIBD",
                                     "IIDB",  
                                     "JBBG",
                                     "JCDD",
                                     "NULL"
                                   ),1,0)


test_s2$I_Preferred_location = NULL
test_s2$interstr = paste(test_s2$I_Internship_Profile,test_s2$I_Skills_required,sep = " ")
test_s2$I_Internship_Profile = NULL
test_s2$I_Skills_required = NULL
test_s2$studstr = paste(test_s2$S_Profile,test_s2$S_Stream,sep = " ")

test_s2$S_Profile = NULL
test_s2$S_Stream = NULL

test_s3 = test_s2
test_s3$studstr=NULL
test_s3$interstr=NULL
test_s3$S_hometown = NULL
test_s3$I_Internship_Location = NULL
test_s3$I_Internship_ID = NULL
test_s3$I_Student_ID = NULL
test_s3$S_Exp = NULL
test_s3$I_Minimum_Duration = NULL
test_s3$S_Year_of_graduation  =NULL
str(test_s3)


write.csv(test_s3,"testdata.csv")


library(caret)
ind = createDataPartition(train_s3$I_Is_Shortlisted, p=0.85, list=F)
trn = train_s3[ind,]
tst = train_s3[-ind,]

fit = glm(I_Is_Shortlisted~. ,data=trn, family=binomial)
summary(fit)
trn$predict = predict(fit,trn, type="response")
hist(trn$predict)
trn$predict2 = ifelse(trn$predict > 0.17,1,0)

confusionMatrix(table(trn$predict2,trn$I_Is_Shortlisted))
library(ROCR)
ROCRpred <- prediction(trn$predict , trn$I_Is_Shortlisted)
ROCRperf <- performance(ROCRpred, 'tpr','fpr')
plot(ROCRperf, colorize = TRUE, text.adj = c(-0.2,1.7))

predict = predict(fit,tst, type="response")

tst$predict = ifelse(predict > 0.2,1,0)
confusionMatrix(table(tst$predict,tst$I_Is_Shortlisted))


mydata$prob=prob
library(pROC)
g <- roc(I_Is_Shortlisted ~ predict, data = trn)
plot(g)

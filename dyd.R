setwd("C:\\Users\\shikhagarg.CORP\\Downloads\\dyd\\trainfiles")
library(dplyr)
library(caret)
library(plyr)
trn <- read.csv("train.csv")
intrn <- read.csv("Internship.csv")
intrn <- intrn[,1:13]

stud <- read.csv("Student.csv")

trn <- left_join(trn,intrn)
trn <- left_join(trn, stud)

head(stud)
stud$Start.Date <- as.Date(stud$Start.Date,"%d-%m-%Y")
stud$End.Date <- as.Date(stud$End.Date,"%d-%m-%Y")
stud$Student_ID <- as.factor(stud$Student_ID)
trn$Expected_Stipend <- NULL
trn$Stipend1<- NULL
trn$Stipend2<- NULL
trn$happy_abt_stipend <- NULL
trn$happy_abt_stipend <- 0

trn$happy_abt_stipend<-if(trn$Expected_Stipend=="10K+" & trn$Stipend1 >= 10000){
    1
}else if(trn$Expected_Stipend=="2-5K"){
    1
}else if(trn$Expected_Stipend=="5-10K" & (trn$Stipend1>=5000 | trn$Stipend2<=10000)){
    1
}else if(trn$Expected_Stipend=="No Expectations"){    
    1
}else 0


stud$experiance_in_months <- ifelse(stud$End.Date<stud$Start.Date,0,round(difftime(stud$End.Date,stud$Start.Date,units = "days")/30,1))
#remove start date and end date

head(stud[stud$experiance_in_months==0 & !is.na(stud$experiance_in_months),], n = 10)
head(stud[stud$End.Date<stud$Start.Date & !is.na(stud$Start.Date) & !is.na(stud$End.Date),],n=10)
dim(trn)

stud$Start.Date <- NULL
stud$End.Date <- NULL

trn$Preferred_location <- as.factor(trn$Preferred_location)

head((stud$Performance_12th+stud$Performance_10th)/2)

stud$avg_perf <- (stud$Performance_12th+stud$Performance_10th)/2

stud$Performance_12th <- NULL
stud$Performance_10th <- NULL

apply(trn, 2, function(x) sum(is.na(x)))
sum(is.na(trn))
apply(stud, 2, function(x) sum(is.na(x)))

#removed missing values from rows : profile, stream, degree
trn <- trn[which(!is.na(trn$Degree)),]
trn <- trn[which(!is.na(trn$Profile)),]
trn <- trn[which(!is.na(trn$Stream)),]

stud$experiance_in_months[is.na(stud$experiance_in_months)] <- 0


str(trn)

trn$Internship_ID <- as.factor(trn$Internship_ID)
trn$Student_ID <- as.(trn$Student_ID)
trn$Is_Shortlisted <- as.factor(trn$Is_Shortlisted)
trn$Is_Part_Time <- as.factor(trn$Is_Part_Time)


names(trn)
sum(trn)
new <- data.frame()

trn$Earliest_Start_Date <- as.Date(trn$Earliest_Start_Date)
trn$Start.Date <- as.Date(trn$Start.Date)
trn$End.Date <- as.Date(trn$End.Date)
trn$Start_Date <- as.Date(trn$Start_Date)
str(trn)
    
View(trn)





stud[stud$Institute_location %in% c("IHJB", "IIBD", "IIDB","IIGA", "IIJJ","IJAE","IJCE","IJGB"
                                    ,"IJJI","JABD", "JAGD","JBDB", "JBEI","JEEH","JEJJ"),]

table(stud$Student_ID, stud$Institute_Category)

trn[trn$Student_ID=="7668678",8]


stud$Engg <- if(stud$Degree == "B.Tech" | stud$Degree == "B.E" | stud$Degree == "MCA"){
    1
}else{
    0
}

stud$Degree <- NULL
attach(trn)

write.csv(trn,file= "final.csv")

trn <- read.csv("final.csv")

fit = glm(Is_Shortlisted~Earliest_Start_Date+Minimum_Duration+Is_Part_Time+
          +experiance_in_months+avg_perf+Current_year+Engg, data= trn, family=binomial)

memory.limit(size = 10000)
memory.limit()




ddply(stud, .(stud$Profile))


setwd("C:\\Users\\shikhagarg.CORP\\Documents\\R")
a <- read.csv("a.txt", sep = "")


b <- ddply(a,.(A,B),summarize, C= paste0(C, sep = "", collapse = ","))




a <- c("HEllo" ,"world","!!" ,"I", "am", "Shikha")
b <- c("Hello","Shikha","!!", "You", "are", "awesome","HEllo")
class(pmatch(a,b, nomatch = NA))
as.list(pmatch(a,b, nomatch = NA))


sum(!is.na(pmatch(a,b, nomatch = NA)),na.rm=T)
a
a <- split(a," ")
#combine intership profile, skills reqd
#combine profile, stream

trn <- read.csv("trn.csv")
head(trn)

names(trn)


t <- trn[,c("Skills_required","Internship_Profile","Stream","Profile")]

head(t)
attach(t)

class(t$Skills_required)
class(t$Internship_Profile)
t$Skills_required = as.character.factor(t$Skills_required)
t$Internship_Profile = as.character.factor(t$Internship_Profile)
t$Stream = as.character.factor(t$Stream)
t$Profile = as.character.factor(t$Profile)


t$internshipprofile2<- paste(t$Skills_required,t$Internship_Profile,sep=", ")

t$studentprofile2<- paste(t$Profile,t$Stream,sep=", ")
t<- t[1:1000,]
View(t)

t$pmatch <- sum(!is.na(pmatch(strsplit(t$internshipprofile2," "),strsplit(t$studentprofile2," "), nomatch = NA)))

f <- sum(!is.na(as.list(pmatch(strsplit(t$internshipprofile2," "),
                               strsplit(t$studentprofile2," "), nomatch = NA))))
head(f)
sum(!is.na(as.list(pmatch(strsplit(t$internshipprofile2,","),strsplit(t$studentprofile2," "), nomatch = NA))))



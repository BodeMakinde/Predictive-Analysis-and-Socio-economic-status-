---
  title: "First Year & First Year Transfer Enrollment Predictive Analytic Project: Part One"
author: "Meaghan Wetherell, Olabode Makinde"
date: "March 20, 2017"
output: 
  word_document:
  reference_docx: mystyle2.docx
---
  
  ```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```
```{r, results='hide', warning=F, message=F}
library(dplyr)
library(randomForest)
library(sjPlot)
library(e1071)
library(caret)
library(reshape2)
```

##Upload Data
Now, we will upload and rename our data files as follows. 

1. Data on student admission and enrollment (**data**) from *application_admission_rpt*
  2. Financial aid data (**faid**) pulled from *ps_stdnt_awrd_actv_v*
  3. A document detailing the grant and loan names (**rosetta**) pulled from *financialaidaward_rpt*
  4. Birthdate information (**dob**) from *person_details_v*
  5. Distance between the university and student hometown (**distance**) calculated using a seperate R Markdown file. Because we may recruit students from different areas in the future, it should be re-run to get all possible distance options before proceeding in the future.  

```{r}
data <- read.csv("N:/Institutional Effectiveness/Student Enrollment/data14.csv")
faid <- read.csv("N:/Institutional Effectiveness/Student Enrollment/financialaid.csv")
rosetta <- read.csv("N:/Institutional Effectiveness/Student Enrollment/financialaidRosetta.csv")
dob <- read.csv("N:/Institutional Effectiveness/Student Enrollment/dob.csv")
distances <- read.csv("N:/Institutional Effectiveness/Student Enrollment/distances.csv")

```

moneydate <- data %>%
  filter(CURRENT_ADMIT_TYPE_CODE %in% c("FYR", "FYT")) %>%
  mutate(AppDate = ifelse(grepl("Fall", CURRENT_ADMIT_TERM_ACADEMIC_YEAR_NAME), paste(CURRENT_ADMIT_TERM_YEAR_NAME, "0901", sep=""), ifelse(grepl("Winter", CURRENT_ADMIT_TERM_ACADEMIC_YEAR_NAME), paste(CURRENT_ADMIT_TERM_YEAR_NAME, "0101", sep=""), ifelse(grepl("Spring", CURRENT_ADMIT_TERM_ACADEMIC_YEAR_NAME), paste(CURRENT_ADMIT_TERM_YEAR_NAME, "0301", sep=""), paste(CURRENT_ADMIT_TERM_YEAR_NAME, "0601", sep=""))))) %>%
  mutate(AppDate = as.Date(AppDate, "%Y%m%d")) %>%
  dplyr::select(APPLICANT_ID, AppDate) 

faid$ActualYear <- faid$AID_YEAR - 1
data$ActualYear <- ifelse(grepl("Spring|Winter|Summer", data$CURRENT_ADMIT_TERM_ACADEMIC_YEAR_NAME), data$CURRENT_ADMIT_TERM_YEAR_NAME - 1, data$CURRENT_ADMIT_TERM_YEAR_NAME)

faid[faid=="."] <- "0"

faid1 <- merge(faid, rosetta, by.x="ITEM_TYPE", by.y="FINANCIAL_AID_ITEM_TYPE_CODE", all.x=TRUE, sort=FALSE)
faid1 <- merge(faid1, moneydate,  by.x="EMPLID", by.y="APPLICANT_ID", all.x=TRUE, sort=FALSE)

#This ensures you've correctly merged to duplicate, turn on if needed to check
#test <- faid1 %>% group_by(EMPLID) %>% mutate(howmany = length(unique(AppDate))) %>% filter(howmany>1)

faid1 <- faid1 %>%
  mutate(ACTION_DTTM = as.Date(ACTION_DTTM, "%Y/%m/%d")) %>%
  mutate(STUDENT_ID = EMPLID) %>%
  arrange(EMPLID, ACTION_DTTM) %>%
  dplyr::select(STUDENT_ID, ACTION_DTTM, OFFER_AMOUNT, AID_YEAR, ActualYear, FINANCIAL_AID_TYPE_NAME, FINANCIAL_AID_SOURCE_DESCRIPTION, AppDate)

faid2 <- faid1 %>%  
  group_by(STUDENT_ID) %>%
  filter(ACTION_DTTM < AppDate)

faid3 <- dcast(faid2, STUDENT_ID+ActualYear~FINANCIAL_AID_SOURCE_DESCRIPTION+FINANCIAL_AID_TYPE_NAME,  value.var="OFFER_AMOUNT", fun.aggregate=sum, na.rm=TRUE)

faid4 <- faid3 %>%
  mutate(LoansOffered = Federal_Loan+Private_Loan) %>%
  mutate(OtherMoneyOffered = Federal_Grant + Institutional_Grant + Institutional_Scholarship + Institutional_Waiver + Other_Grant  + Private_Grant + Private_Scholarship + State_Grant + State_Scholarship) %>%
  mutate(TotalMoneyOffered = Federal_Loan+Private_Loan + Federal_Grant + Institutional_Grant + Institutional_Scholarship + Institutional_Waiver + Other_Grant +  Private_Grant + Private_Scholarship + State_Grant + State_Scholarship) %>%
  dplyr::select(STUDENT_ID, ActualYear, LoansOffered, OtherMoneyOffered, TotalMoneyOffered)

data <- merge(data, faid4, by.x=c("APPLICANT_ID", "ActualYear"), by.y=c("STUDENT_ID", "ActualYear"), all.x=TRUE, all.y=FALSE)
data[is.na(data)] <- 0
```


```{r}
distances$address <- paste(distances$City, distances$State, sep=", ")
data$address <- paste(data$ORIGIN_CITY, data$ORIGIN_STATE, sep=", ")

data <- merge(data, dob, by.x=c("APPLICANT_ID"), by.y=c("PERSONID"), all.x=TRUE)
data <- merge(data, distances, by="address", all.x=TRUE) 
```{r}
data <- data %>%
  mutate(year = CURRENT_ADMIT_TERM_YEAR_NAME) %>%
  mutate(CGPA = as.numeric(ifelse(LAST_EXTERNAL_ORGANIZATION_GPA == ".", paste(HIGH_SCHOOL_GPA), paste(LAST_EXTERNAL_ORGANIZATION_GPA)))) %>%
  mutate(APPLIED_DATE = as.Date(APPLIED_DATE, "%Y/%m/%d")) %>% 
  mutate(DATE_OF_BIRTH = as.Date(DATE_OF_BIRTH, "%Y/%m/%d")) %>%
  mutate(ACADEMIC_INTEREST_DESCRIPTION_COMBO = paste(ACADEMIC_INTEREST_DESCRIPTION1, ACADEMIC_INTEREST_DESCRIPTION2, ACADEMIC_INTEREST_DESCRIPTION3, sep=", ")) %>%
  mutate(Specialty = as.factor(ifelse(grepl("Education|Teach|Geology|Wine|Craft|Music|Composition|Theatre|Performance|Paramedic|Info Tech|Information Technology|Business|Marketing|Administrator|Human Resource Management|Supply Chain|Accounting|Aviation|Pilot", ACADEMIC_INTEREST_DESCRIPTION_COMBO), "Yes","No"))) %>%
  mutate(SEASON = as.factor(ifelse(grepl("Fall", CURRENT_ADMIT_TERM_ACADEMIC_YEAR_NAME), "Fall", ifelse(grepl("Winter", CURRENT_ADMIT_TERM_ACADEMIC_YEAR_NAME), "Winter", ifelse(grepl("Summer", CURRENT_ADMIT_TERM_ACADEMIC_YEAR_NAME), "Summer", ifelse(grepl("Spring", CURRENT_ADMIT_TERM_ACADEMIC_YEAR_NAME), "Spring", "Unknown"))))) ) %>%
  mutate(address = paste(ORIGIN_CITY, ORIGIN_STATE, sep=", ")) %>%
  mutate(ENROLLED_COUNT = factor(ENROLLED_COUNT), CURRENT_ADMIT_TYPE_CODE = factor(CURRENT_ADMIT_TYPE_CODE), ETHNICITY_RACE = factor(ETHNICITY_RACE), EWO = factor(EWO)) %>%
  mutate(AGE2 = as.numeric((APPLIED_DATE - DATE_OF_BIRTH)/365))
```
Don't worry about that NA warning - you'll deal with it in a second.

```{r}
data2 <- data %>%
  filter(CURRENT_ADMIT_TYPE_CODE == "FYR") %>%
  filter(CURRENT_ADMIT_TERM_YEAR_NAME %in% c(2010, 2011, 2012, 2013, 2014, 2015, 2016))%>%
  filter(ADMITTED_COUNT == "1") %>%
  filter(WA_RESIDENCY_FLAG != "." | WA_RESIDENCY_FLAG != "") %>%
  filter(PERSON_SELF_REPORTED_DISABILITY_FLAG != "." | PERSON_SELF_REPORTED_DISABILITY_FLAG != ".") %>%
  filter(!is.na(CGPA)) %>%
  filter(!is.na(AGE2)) %>%
  filter(CGPA != ".") %>%
  filter(CGPA != "0") %>%
  filter(DENIED_COUNT != "1")%>%
  filter(Miles != "NA") %>%
  mutate(ROYALL_APPLICATION_COUNT = ifelse(ROYALL_APPLICATION_COUNT == "1", "Royall Application", "Not Royall Application")) %>%
  mutate(ROYALL_APPLICATION_COUNT = as.factor(ifelse(is.na(ROYALL_APPLICATION_COUNT), "Not Royall Application", paste(ROYALL_APPLICATION_COUNT)))) %>%  
  mutate(Specialty = as.factor(ifelse(grepl("Education|Teach|Geology|Wine|Craft|Music|Composition|Theatre|Performance|Paramedic|Info Tech|Information Technology|Business|Marketing|Administrator|Human Resource Management|Supply Chain|Accounting|Aviation|Pilot", ACADEMIC_INTEREST_DESCRIPTION_COMBO), "Yes","No"))) %>%
  mutate(duplicated = as.factor(ifelse(duplicated(APPLICANT_ID), "Duplicated", "Not Duplicated")))

```

##Filter For FYT
Same process, different dataset.
```{r}
data3 <- data %>%
  filter(CURRENT_ADMIT_TYPE_CODE == "FYT") %>%
  filter(CURRENT_ADMIT_TERM_YEAR_NAME %in% c(2010, 2011, 2012, 2013, 2014, 2015, 2016))%>%
  filter(ADMITTED_COUNT == "1") %>%
  filter(WA_RESIDENCY_FLAG != "." | WA_RESIDENCY_FLAG != "") %>%
  filter(PERSON_SELF_REPORTED_DISABILITY_FLAG != "." | PERSON_SELF_REPORTED_DISABILITY_FLAG != ".") %>%
  filter(!is.na(CGPA)) %>%
  filter(!is.na(AGE2)) %>%
  filter(CGPA != ".") %>%
  filter(CGPA != "0") %>%
  filter(DENIED_COUNT != "1")%>%
  filter(Miles != "NA") %>%
  mutate(ROYALL_APPLICATION_COUNT = ifelse(ROYALL_APPLICATION_COUNT == "1", "Royall Application", "Not Royall Application")) %>%
  mutate(ROYALL_APPLICATION_COUNT = as.factor(ifelse(is.na(ROYALL_APPLICATION_COUNT), "Not Royall Application", paste(ROYALL_APPLICATION_COUNT)))) %>%  
  mutate(Specialty = as.factor(ifelse(grepl("Education|Teach|Geology|Wine|Craft|Music|Composition|Theatre|Performance|Paramedic|Info Tech|Information Technology|Business|Marketing|Administrator|Human Resource Management|Supply Chain|Accounting|Aviation|Pilot", ACADEMIC_INTEREST_DESCRIPTION_COMBO), "Yes","No"))) %>%
  mutate(duplicated = as.factor(ifelse(duplicated(APPLICANT_ID), "Duplicated", "Not Duplicated")))
```
```{r}
PredictFYR <- data %>%
  filter(CURRENT_ADMIT_TYPE_CODE == "FYR") %>%
  filter(CURRENT_ADMIT_TERM_YEAR_NAME %in% c(2017,2018))%>%
  filter(ADMITTED_COUNT == "1") %>%
  filter(WA_RESIDENCY_FLAG != "." | WA_RESIDENCY_FLAG != "") %>%
  filter(PERSON_SELF_REPORTED_DISABILITY_FLAG != "." | PERSON_SELF_REPORTED_DISABILITY_FLAG != ".") %>%
  filter(!is.na(CGPA)) %>%
  filter(!is.na(AGE2)) %>%
  filter(CGPA != ".") %>%
  filter(CGPA != "0") %>%
  filter(DENIED_COUNT != "1")%>%
  filter(Miles != "NA") %>%
  mutate(ROYALL_APPLICATION_COUNT = ifelse(ROYALL_APPLICATION_COUNT == "1", "Royall Application", "Not Royall Application")) %>%
  mutate(ROYALL_APPLICATION_COUNT = as.factor(ifelse(is.na(ROYALL_APPLICATION_COUNT), "Not Royall Application", paste(ROYALL_APPLICATION_COUNT)))) %>%  
  mutate(Specialty = as.factor(ifelse(grepl("Education|Teach|Geology|Wine|Craft|Music|Composition|Theatre|Performance|Paramedic|Info Tech|Information Technology|Business|Marketing|Administrator|Human Resource Management|Supply Chain|Accounting|Aviation|Pilot", ACADEMIC_INTEREST_DESCRIPTION_COMBO), "Yes","No"))) %>%
  mutate(duplicated = as.factor(ifelse(duplicated(APPLICANT_ID), "Duplicated", "Not Duplicated")))

levels(PredictFYR$CAMPUS_CODE) <- levels(data2$CAMPUS_CODE)
levels(PredictFYR$ENROLLED_COUNT ) <- levels(data2$ENROLLED_COUNT )
levels(PredictFYR$PERSON_GENDER) <- levels(data2$PERSON_GENDER)
levels(PredictFYR$FIRST_GENERATION_FLAG) <- levels(data2$FIRST_GENERATION_FLAG)
levels(PredictFYR$WA_RESIDENCY_FLAG) <- levels(data2$WA_RESIDENCY_FLAG)
levels(PredictFYR$SEASON) <- levels(data2$SEASON)
levels(PredictFYR$PERSON_VETERAN_FLAG) <- levels(data2$PERSON_VETERAN_FLAG)
levels(PredictFYR$PERSON_SELF_REPORTED_DISABILITY_FLAG) <- levels(data2$PERSON_SELF_REPORTED_DISABILITY_FLAG)
levels(PredictFYR$ETHNICITY_RACE) <- levels(data2$ETHNICITY_RACE)
```

##Create Training and Test Datasets

```{r}
trainFYR <- data2%>% filter(ActualYear %in% c(2010, 2011, 2012, 2013, 2014))
testFYR <- data2 %>% filter(ActualYear %in% c(2015))
trainFYT <- data3 %>% filter(ActualYear %in% c(2010, 2011, 2012, 2013, 2014))
testFYT <- data3 %>% filter(ActualYear %in% c(2015))
**rf.modF = randomForest(ENROLLED_COUNT ~ CAMPUS_CODE+ PERSON_GENDER + FIRST_GENERATION_FLAG + WA_RESIDENCY_FLAG+ AGE2+ SEASON  + Miles+ CGPA + PERSON_VETERAN_FLAG  + PERSON_SELF_REPORTED_DISABILITY_FLAG + ETHNICITY_RACE + LoansOffered + OtherMoneyOffered, data=data2, importance=TRUE)**
  
  ```{r}
rf.modF = randomForest(ENROLLED_COUNT ~ AGE2 + SEASON  + Miles + CGPA + LoansOffered + OtherMoneyOffered, data=trainFYR, importance=TRUE)

rf.modT = randomForest(ENROLLED_COUNT ~ CAMPUS_CODE + AGE2 + SEASON  + Miles+ CGPA + LoansOffered + OtherMoneyOffered, data=trainFYT, importance=TRUE)
```

Now, you'll want to check these models for accuracy. To do so, create a confusion matrix. First, have the model predict enrollment on the original (or new) dataset.

```{r}
testFYR$Predicted <- predict(rf.modF, testFYR)
testFYT$Predicted <- predict(rf.modT, testFYT)
```

Then, create a confusion matrix to determine accuracy and whether the model is conservative or not in its estimates.

```{r}
confusionMatrix(data=testFYR$Predicted, reference=testFYR$ENROLLED_COUNT, positive='1') 
confusionMatrix(data=testFYT$Predicted, reference=testFYT$ENROLLED_COUNT, positive='1') 

varImpPlot(rf.modF, sort=F, type=1, main="First Year Freshmen")
varImpPlot(rf.modT, sort=F, type=1, main = "First Year Transfer")
```

##Predict New Enrollment
Use this model to predict on 2017-2018 data.You can get both response (enrolled or not), and liklihood.

```{r}
PredictFYR$Predicted <- predict(rf.modF, PredictFYR, type="response")
PredictFYR$predictionprob <- predict(rf.modF, PredictFYR, type="prob")
```
If you are confident enrollment is finished, you can also test the predictions using the confusion matrix.

```{r}
confusionMatrix(data=PredictFYR$Predicted, reference=PredictFYR$ENROLLED_COUNT, positive='1')

```{r, echo=TRUE, fig.width=12, fig.height=9}
#rownames(rf.modF$importance)<- c("Service Campus","Gender", "First Generation","Residency","Age","Season","Distance from the university","GPA","Veteran","Disability","Ethnicity/Race", "Loans Offered", "Scholarships & Grants & Waivers")
#rownames(rf.modT$importance)<- c("Service Campus","Gender", "First Generation","Residency","Age","Season","Distance from the university","GPA","Veteran","Disability","Ethnicity/Race", "Loans Offered", "Scholarships & Grants & Waivers")

par(mfrow=c(1,2))
varImpPlot(rf.modF, sort=F, type=1, main="First Year Freshmen")
varImpPlot(rf.modT, sort=F, type=1, main = "First Year Transfer")

```

But there's also a reduction in "Gini" to be concerned about. Essentially, how many more splits must a tree make if this character is removed? Higher numbers mean these characters greatly simplify trees, even if they don't increase accuracy necessarily.

```{r, echo=TRUE, fig.width=12, fig.height=9}
par(mfrow=c(1,2))
varImpPlot(rf.modF, sort=F, type=2, main="First Year Freshmen")
varImpPlot(rf.modT, sort=F, type=2, main = "First Year Transfer")

```

##Racial makeup Analysis
Before we go anywhere, I want to take a look at our application admission data and look at people who changed their races in different application years, to see who our "not reported" group really is. 72% of people who reported their race as Not Reported and changed it on another application called themselves white european on at least one of those other applications. 11% called themselves latino. 
```{r}
r.change <- data %>%
group_by(APPLICANT_ID, ETHNICITY_RACE) %>%
tally()
r.change2 <- dcast(r.change, APPLICANT_ID~ETHNICITY_RACE, value.var = "n")
r.change2[is.na(r.change2)] <- 0
r.change3 <- r.change2 %>%
mutate(differentgroups = `African American/Black`+`Alaskan/Native American`+Asian+`European/Middle Eastern/White`+`Hawaiian/Pacific Islander`+`Latino/Hispanic`+Multiracial+`NonResident Alien`+`Not Reported`) %>%
filter(differentgroups > 1) %>%
filter(`Not Reported` != differentgroups) %>%
filter(`Not Reported` != 0)

r.change4 <- data.frame(colSums(r.change3 !=0))
r.change4$labels <- rownames(r.change4) 
rownames(r.change4) <- NULL
r.change4 <- r.change4[c(3:11),]
colnames(r.change4) <- c("count", "Ethnicity/Race")
r.change5 <- r.change4 %>% mutate(percentage = count/count[9])
knitr::kable(r.change5, caption="Individuals whose Race/Ethnicity was Not Reported on one or more applications, and how they reported on a different applications")
```



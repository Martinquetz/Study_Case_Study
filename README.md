# Study_Case_Study

---
title: "Study Habit Case Study with R"
- author: Martin U
- date: 'August 19, 2022 at 11:59PM'
---


## **<span style="color: #8F2050;">Introduction</span>**

## Introduction

In QMM1001 and QMM1002 all students collected personalized data to analyze, using RMarkdown to analyze this data. The objective of this term's case study is to determine if there are differences in the average study times for students in the different analytics streams. Also, we need to determine if the distribution of days studied are more than 3.13 hours (the average for McGill's students). The study is also interested to know how my personal study time changed over time.  The above and other habits were analyzed and compared to plan, using recorded personal data documented over time. I started recording my data in the first semester, stopped at the end of the semester, and resumed at the beginning of the second semester. I have so far recorded these data for 116 days. Table 1.1 below shows the relevant variables for this Case Study 2.

*Table 1.1:  Recorded Variables Relevant to Case Study 2*

Variable | Type
------------- | -------------
Date | Identifier
Hours Studying  | Quantitative
Term (F19, W20, F21 or W22) | Categorical
Program (BAPG, CAGC, HAGC) | Categorical

You must also include calculations of summary statistics. This is how you run code in RMarkdown. Note that the `echo = FALSE` parameter was added to the code chunk to prevent printing of the R code that generated the summary statistics, only the output is shown. You can also set `include = FALSE` to hide all code and output and `messgae = FALSE` and `warning = FALSE` to hide all warnings and messages when installing new packages. 

**Summary Statistics**

These summary statistics can be used to compare the amount of time I spent studying to other students and to other students by program. 

```{r include=FALSE}
mart<-read.csv(file="Unukpo, Martin Personalized Data.csv", header=TRUE)
mart.studymean<-mean(mart$Study, na.rm=TRUE)
mart.studysd<-sd(mart$Study, na.rm=TRUE)

combined<-read.csv(file="Combined.csv", header=TRUE, fileEncoding="UTF-8-BOM")

bapg <- subset(combined, combined$Program=='BAPG')
mbapg <- mean(bapg$Study)
sbapg <- sd(bapg$Study)

cagc <- subset(combined, combined$Program=='CAGC')
mcagc <- mean(cagc$Study)
scagc <- sd(cagc$Study)

hagc <- subset(combined, combined$Program=='HAGC')
mhagc <- mean(hagc$Study)
shagc <- sd(hagc$Study)
```

Data Set | Mean | Standard Deviation
------------- | ------------- | ------------- 
Mart | `r mean(mart$Study, na.rm=TRUE)` | `r sd(mart$Study, na.rm=TRUE)`
All Students | `r mean(combined$Study, na.rm=TRUE)` | `r sd(combined$Study, na.rm=TRUE)`
BAPG | `r mean(bapg$Study, na.rm=TRUE)` | `r sd(bapg$Study, na.rm=TRUE)`
CAGC | `r mean(cagc$Study, na.rm=TRUE)` | `r sd(cagc$Study, na.rm=TRUE)`
HAGC | `r mean(hagc$Study, na.rm=TRUE)` | `r sd(hagc$Study, na.rm=TRUE)`


You will use your own personalized data set and a combined class data set to answer the questions: 

1. Are there differences in the average study times for students in the different analytics streams? There are differences in the average study times among students of the different analytics streams.
2. Is the distribution of days studied more than 3.13 hours (the average daily study time for students at McGill) the same for students in the different analytics streams (or in other words, independent of program stream)? Yes, it is more than 3.13 hours
3. How does your personal study time change over time? mine is lower than the group average.

You must also summarize the methods that will be used throughout Case Study 2 to answer these questions (ANOVA, chi-square tests, time series analysis.


## **<span style="color: #8F2050;">Data Analysis</span>**
Don't forget you must include more than 5 examples of your daily activities that help to explain the results of your analysis for full marks in this section!

### Part 1: ANOVA

This section will analyze the "combined" data of all three analytics programs collected over time. Since we're interested in comparing the mean of the study times between the programs, our hypotheses statement is, therefore, as follows:

Null Hypothesis: There is no difference between the mean study times of the programs
Alternative Hypothesis: At least one mean is different. 

This is can be expressed as:

$H_0: \mu_B = \mu_C = \mu_H$

$H_A:$ At least one mean is different


```{r include=FALSE}
library(dplyr)
#include code within chunks that have hidden output
bapg <- subset(combined, combined$Program=='BAPG')  #Sub-setting the BAPG data
#Determining the summary statistics of BAPG
mbapg <- mean(bapg$Study)   
sbapg <- sd(bapg$Study)

cagc <- subset(combined, combined$Program=='CAGC') #Sub-setting the CAGC data
#Determining the summary statistics of BAPG
mcagc <- mean(cagc$Study)
scagc <- sd(cagc$Study)

hagc <- subset(combined, combined$Program=='HAGC')  #Sub-setting the HAGC data
#Determining the summary statistics of BAPG
mhagc <- mean(hagc$Study)
shagc <- sd(hagc$Study)
ohagc <- hagc%>% filter(!Study=="NA")     #to filter out "NA"/null values

#for creating random subsets for the streams 
bapg.samp <- bapg[sample(1:nrow(bapg), size = 50), ]
cagc.samp <- cagc[sample(1:nrow(cagc), size = 50), ]
hagc.samp <- ohagc[sample(1:nrow(ohagc), size = 50), ]

#Combining 50 days samples of all three programs using rbind()
prosamps <- rbind(bapg.samp, cagc.samp, hagc.samp)
#use last digits of student number ex. 138572 to get SAME random 
#numbers each time you run your code so your output doesn't change
set.seed(240653)
```

We will now use the above data to perform an ANOVA test.

```{r}
result<-aov(prosamps$Study~prosamps$Program, data=prosamps)
summary(result)

qf(0.95, df1 = 2, df2 = 147)  #3.057621
```

The summary showed the p-value for the test is P-value=0.131 > 0.05 = $\alpha$. Also, the F-value=2.06 < 3.0576=F$\*$. Therefore, we fail to reject the null and conclude that there's no significant difference between the means Study times of the analytics programs. 

## Conduct Tukey's HSD Test
We can use Tukey's HSD to find which mean(s) are different at $\alpha=0.05$.

```{r}
TukeyHSD(result, conf.level = 0.95)
```

We fail to reject the null for the tests between BAPG, CAGC and HAGC. This means the mean study times of the three analytics program is not significantly different.

## Using a barplot to interprete the result
```{r}
#install.packages("ggplot2")
library(ggplot2)
ggplot(prosamps, aes(Program, Study, fill=Program))+
  stat_summary(fun="mean", geom="bar")+
  stat_summary(fun.data=mean_cl_normal, geom="errorbar", width=0.2)+
  labs(x="Programs", y="Study Time", title="Results of Study Time Averages")+
  scale_fill_brewer(palette="Set3")
```
From the plot above, we can see error bars of rhe programs overlap each other, therefore, we can conclude that there's no significant difference between the mean study time of the three analytics programs.

## Using the GranovaGG Chart to further express the above
```{r}
library(granovaGG)
granovagg.1w(prosamps$Study, group=prosamps$Program)
```
The blue squares (light and dark) at the bottom right corner of the chart further tells a similar story, i.e. there's no significant difference between the MST and MSE, suggesting the F-Value is closer to 1 than away from it.


## Checking the conditions for using ANOVA.
1. Independence Assumption: All students recorded their personal data with no external influence
2. randomization Condition: The Student were randomly selected from the larger data using the sample() and subset functions.
3. Independent Groups: The program groups are independent of each other and don't influence each other either.



### Part 2: Chi-Square Tests

## The test Hypothesis
$\Ho$: the distribution of groups is the same
$\Ha$: the distribution of groups is different


```{r echo=FALSE, message=FALSE, warning=FALSE}
#include code within chunks that have hidden output

#Follow instruction on Module 9 Applied Activity Case Study 2 Check Point
#for creating categorical variable to analyze counts

class<-read.csv("Combined.csv", header=TRUE, fileEncoding="UTF-8-BOM")
BAPG<-subset(class, Program=="BAPG")
CAGC<-subset(class, Program=="CAGC")
HAGC<-subset(class, Program=="HAGC")

library(dplyr)
BAPG.50<-sample_n(BAPG, 50)
CAGC.50<-sample_n(CAGC, 50)
HAGC.50<-sample_n(HAGC, 50)
full.50<-rbind(BAPG.50, CAGC.50, HAGC.50)

full.50$HoursCategory<-ifelse(full.50$Study>3.13, "Above", "Below")

observed.hours<-table(full.50$HoursCategory, full.50$Program)
observed.hours
```
## Using the in-built R chisquare function to determine the Chi-Square
```{r}
chisq.test(observed.hours)
#X-squared = 1.0828, df = 2, p-value = 0.5819

#Using critical values
qchisq(0.05, df=(dim(observed.hours)[1]-1)*(dim(observed.hours)[2]-1), lower.tail=FALSE)
#X^2 = 1.0828 < 5.991465 = X^2*
#and
#P-Value=0.5819 > 0.05=alpha
```
From the chi-square analysis above, 

$\X^2$ = 1.0828 < 5.991465 = $\X^2$*

and,

P-Value=0.5819 > $\alpha=0.05$

Therefore We fail to reject the null Hypothesis,There is significant evidence that the distribution of the above and below groups are the same.


##Assumptions and Conditions
Counted Data Condition: The data are counts for the level categories.
Independence Assumption: The counts in the cells are independent of each other
Randomization Condition: The counted individual times are a random sample of the population. 
Expected Cell Frequency/Sample Size Condition: there are more than 5 individuals data per cell.

##Chart of an association plot
```{r}
assocplot(observed.hours, xlab = "Level", ylab = "Program", main = "Association plot for program Study Distribution Between vs McGill's")
```

##Interpretation of the Chart

The tallest bar corresponds to the Below CAGC's group.This means that this is the most unusual residual. 
The tallest red bar is again the Below CAGC's group, which means it is the largest negative residual.
The tallest black bar is for  Below CAGC's group which means it is the largest positive residual.
The widest bar is for Above group. This means that is where we expected the highest count of study time. 
The most narrow bar is Below BAPG. This means that is where we expected the lowest count of study time.


### Part 3: Time Series Analysis

Place your analysis here. The code below is to set up your time series data. You will need to alter this code for your own data, create multiple moving average models, and the best exponential smoothing model as per the case study instructions.

```{r include=FALSE, warning = FALSE, message=FALSE}
#you must install these packages in the console first using 
#install.packages("zoo")
#install.packages("forecast")
library(zoo)
library(forecast)
mart<-read.csv(file="Unukpo, Martin Personalized Data.csv", header=TRUE, fileEncoding="UTF-8-BOM")
mart$Date <- as.Date(mart$Date,format="%m/%d/%Y") #Check your format! Depending on how you entered dates in Excel, you might have to change it
mart.zoo <- zoo(mart[,3],mart[,1]) #remove all columns except study time and set dates to index
#mart.zoo2 <-omit(mart.zoo)
mart.all <- merge(mart.zoo,zoo(,seq(start(mart.zoo),end(mart.zoo),by="day")), all=TRUE) #include all missing dates

#Make a time series with the longest stretch of dates
mart.ts<-ts(na.contiguous(mart.all), frequency = 7) #frequency = 7 lets us plot the decomposition
```

```{r}
plot(decompose(mart.ts))
```
##Interpretation
The hours studied showed an increasing trend.
There is a pattern of seasonality, weekly.
There are no significant irregular patterns seen
There is no cyclical pattern either, especially since this data is under a year.

```{r include=FALSE, warning = FALSE, message=FALSE}
#you can then do any of the time series steps with your new data set!
plot.ts(mart.ts, xlab="Days since January 10, 2022", ylab="Hours Studied", main="Time Series Plot")

```


```{r echo=FALSE, warning = FALSE, message=FALSE}
library(TTR)
#Assign any value to the length of the moving average below  
L<-12
mart.maL<-SMA(mart.ts, L)

#Make sure you change the xlab and main title for YOUR PERSONALIZED DATA
plot.ts(cbind(mart.ts, mart.maL), plot.type="single", col=c("black", "red"), 
        xlab="Days since January 10, 2022", ylab="Hours Studied", main="Martin's Personalized Study Data")
legend("bottomright", legend=c("Data", "MA"), col=c("black", "red"), lty=1, cex=0.5)
```
The best model is the 12 day moving average as this smoothens out the noise but not too much that it impacts the responsiveness of the model.

## **<span style="color: #8F2050;">Conclusion</span>**
Summarize your findings for all questions: 
 
In summary, from the analysis above, there are no significant differences between the average study times for students in the different analytics streams.
We also found that there is no significant evidence that the distribution of days studied are more or less than 3.13 hours (the average daily study time for students at McGill) the same for students in the different analytics streams (or in other words, independent of program stream).
Also, we found that my hours of study showed an increasing trend, although there are some seasonality, but no significant irregularity nor cyclical pattern shown.

In conclusion, I found that my study habits improved as the semester went on, although I studied less weekday and more on weekends, which was well reflected by the seasonality shown in the decompose chart. 


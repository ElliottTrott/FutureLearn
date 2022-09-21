# the data will be loaded from the 'data' folder, do we need to load the data within the 'munge' folder too?

# library calls
library(readr)
library(tidyverse)
library(dplyr)
library(ggplot2)
library(lubridate)
library(ggpubr)
library(reshape)


# on the assumption that we do need to load the data in
# loading the data for enrollments
cyber_security_1_enrolments <- read_csv("~/Desktop/DMET/data/cyber-security-1_enrolments.csv")
cyber_security_2_enrolments <- read_csv("~/Desktop/DMET/data/cyber-security-2_enrolments.csv")
cyber_security_3_enrolments <- read_csv("~/Desktop/DMET/data/cyber-security-3_enrolments.csv")
cyber_security_4_enrolments <- read_csv("~/Desktop/DMET/data/cyber-security-4_enrolments.csv")
cyber_security_5_enrolments <- read_csv("~/Desktop/DMET/data/cyber-security-5_enrolments.csv")
cyber_security_6_enrolments <- read_csv("~/Desktop/DMET/data/cyber-security-6_enrolments.csv")
cyber_security_7_enrolments <- read_csv("~/Desktop/DMET/data/cyber-security-7_enrolments.csv")
# need to think about how we are loading these in, if we could use the ~ sign it might be better practice


# removing duplicates
cyber_security_1_enrolments[!duplicated(cyber_security_1_enrolments),]
cyber_security_2_enrolments[!duplicated(cyber_security_2_enrolments),]
cyber_security_3_enrolments[!duplicated(cyber_security_3_enrolments),]
cyber_security_4_enrolments[!duplicated(cyber_security_4_enrolments),]
cyber_security_5_enrolments[!duplicated(cyber_security_5_enrolments),]
cyber_security_6_enrolments[!duplicated(cyber_security_6_enrolments),]
cyber_security_7_enrolments[!duplicated(cyber_security_7_enrolments),]



# Basic Analysis

# average number of participants over 7 years?
# take unique learner ids for each year and collect in a table, 1 column for year, 1 for number of participants
length(unique(cyber_security_1_enrolments$learner_id))
# making table with numbers of unique learner ids
TableForParticipantNumbers <- matrix(c(length(unique(cyber_security_1_enrolments$learner_id)), 1, length(unique(cyber_security_2_enrolments$learner_id)), 2, length(unique(cyber_security_3_enrolments$learner_id)), 3, length(unique(cyber_security_4_enrolments$learner_id)), 4, length(unique(cyber_security_5_enrolments$learner_id)), 5, length(unique(cyber_security_6_enrolments$learner_id)), 6, length(unique(cyber_security_7_enrolments$learner_id)), 7), ncol=2, byrow=TRUE)
# adding labels
colnames(TableForParticipantNumbers) <- c("Participant.Numbers", "Year")
# turning into dataframe to allow for analysis
TableForParticipantNumbers <- as.data.frame(TableForParticipantNumbers)
# for average over the 7 years 
# mean(TableForParticipantNumbers$Participant.Numbers)


# Make table for participants who purchased statements 
# need to find work-around to display the numbers of purchased statements
TableForPurchasedNumbers <- matrix(c(length(na.omit(cyber_security_1_enrolments$purchased_statement_at)), 1, length(na.omit(cyber_security_2_enrolments$purchased_statement_at)),2, length(na.omit(cyber_security_3_enrolments$purchased_statement_at)), 3, length(na.omit(cyber_security_4_enrolments$purchased_statement_at)), 4, length(na.omit(cyber_security_5_enrolments$purchased_statement_at)), 5, length(na.omit(cyber_security_6_enrolments$purchased_statement_at)), 6, length(na.omit(cyber_security_7_enrolments$purchased_statement_at)), 7), ncol=2, byrow=TRUE)
# adding labels
colnames(TableForPurchasedNumbers) <- c("Number.Purchased", "Year")
# into df
TableForPurchasedNumbers <- as.data.frame(TableForPurchasedNumbers)
# then take the average across the 7 years with
# mean(TableForPurchasedNumbers$)



# make table with both the purchased numbers and completed numbers
merged_percentage_table <- left_join(TableForParticipantNumbers, TableForPurchasedNumbers, id = "Year")
# compute percentages 
merged_percentage_table <- merged_percentage_table %>% mutate(percentage_purchased = (Number.Purchased/Participant.Numbers)*100)
# plot the data need to control y values
ggplot(data = merged_percentage_table, aes(x = Year, y = percentage_purchased, colour = Year)) + geom_bar(stat = "identity")
# further work needed to control y axis and colour scheme



##### demographic data
# joining 4-7 years together 
cyberenrolment4.5 <- bind_rows(cyber_security_4_enrolments, cyber_security_5_enrolments, id = NULL)
cyberenrolment4.5.6 <- bind_rows(cyberenrolment4.5, cyber_security_6_enrolments, id = NULL)
cyberenrolmentJOINED <- bind_rows(cyberenrolment4.5.6, cyber_security_7_enrolments, id = NULL)

# need to change certain categories to binary
cyberenrolmentJOINED$fully_participated_at <-  ifelse(!is.na(cyberenrolmentJOINED$fully_participated_at),T,F)

# screening by gender and plotting frequency on the y axis..
ggplot(data = cyberenrolmentJOINED, aes(fill = fully_participated_at, y = frequency(fully_participated_at), x = gender)) + 
  geom_bar(position="stack", stat="identity")
# need to remove 'unknown'...make a df with just male and female
gender_enrol <- cyberenrolmentJOINED %>% mutate(gend = case_when((.$gender == "male") ~ "male",
                                                                 (.$gender =="other") ~ "other",
                                                              (.$gender == "female") ~ "female",))

# remove NAs from new column
gender_enrol <-  gender_enrol[!is.na(gender_enrol$gend), ]
# plot the graph
ggplot(data = gender_enrol, aes(fill = fully_participated_at, y = frequency(fully_participated_at), x = gender)) + 
  geom_bar(position="stack", stat="identity")





#### More demographic work, what audiences are we appealing to?
# we can use another columns from the same csv: highest_education_level

# need to mutate and add a column that has all the education levels set
demo_data <- cyberenrolmentJOINED %>% mutate(educ_level = case_when((.$highest_education_level == "apprenticeship") ~ "apprenticeship",
                                                              (.$highest_education_level == "less_than_secondary") ~ "<secondary",
                                                              (.$highest_education_level == "professional") ~ "pro",
                                                              (.$highest_education_level == "secondary") ~ "secondary",
                                                              (.$highest_education_level == "tertiary") ~ "tertiary",
                                                              (.$highest_education_level == "university_degree") ~ "uni_deg",
                                                              (.$highest_education_level == "university_doctorate") ~ "uni_doc",
                                                              (.$highest_education_level == "university_masters") ~ "uni_masters",))
# remove NAs from this new column
demo_data <-  demo_data[!is.na(demo_data$educ_level), ]
# plot a barchart of x = highest_education_level and y = tally for true completed
ggplot(data = demo_data, aes(x = educ_level, y = frequency(fully_participated_at), fill = fully_participated_at)) +
  geom_bar(position = "stack", stat = "identity")




########### INVESTIGATION 1

# leaving responses 
# load in data
cyber_security_4_leaving_survey_responses <- read_csv("~/Desktop/DMET/data/cyber-security-4_leaving-survey-responses.csv")
# this is the start of the data for leaving responses (at year 4)
cyber_security_5_leaving_survey_responses <- read_csv("~/Desktop/DMET/data/cyber-security-5_leaving-survey-responses.csv")
cyber_security_6_leaving_survey_responses <- read_csv("~/Desktop/DMET/data/cyber-security-6_leaving-survey-responses.csv")
cyber_security_7_leaving_survey_responses <- read_csv("~/Desktop/DMET/data/cyber-security-7_leaving-survey-responses.csv")
# to create a plot/table with the leaving survey responses

# join tables together
cyber_leaving4.5 <- bind_rows(cyber_security_4_leaving_survey_responses, cyber_security_5_leaving_survey_responses, id = NULL)
cyber_leaving4.5.6 <- bind_rows(cyber_leaving4.5, cyber_security_6_leaving_survey_responses, id = NULL)
cyber_leavingJOINED <- bind_rows(cyber_leaving4.5.6, cyber_security_7_leaving_survey_responses, id = NULL)
# written in the script is the output of a table

# for plot of joined leaving responses
ggplot(cyber_leavingJOINED, aes(x= frequency(leaving_reason) , y = leaving_reason )) + 
  geom_bar(stat="identity", colour="black", fill="white") + 
  xlab("") + ylab("")


# plot individual graphs specific to the years to enable better visualizations
# ggplot(cyber_security_4_leaving_survey_responses, aes(x = leaving_reason))


# also need an overall view of the weeks in which individuals were leaving 
# for joined 
table(cyber_leavingJOINED$last_completed_week_number)
# can talk about this/plot in a barchart or use a line chart if we are going for specific step numbers

# individual bar charts for years
ggplot(cyber_security_4_leaving_survey_responses, aes(x= last_completed_step_number)) + 
  geom_line(aes(y = frequency(last_completed_step_number))) + 
  xlab("") + ylab("") # not working 
# need to think more about how to make the line plots
table(cyber_security_4_leaving_survey_responses$last_completed_step_number)



# some leaving response investigation

#add a column with values for cyberleaving df
cyber_leavingJOINED <- cyber_leavingJOINED %>%
  add_column(Value = 1)

ggplot(data = cyber_leavingJOINED, aes(x = last_completed_week_number, y = frequency(Value), color = last_completed_week_number)) + 
  geom_bar(stat = "identity")


# week 1 investigation only
# need to screen for just the week 1 responses
week_1_leaving_responses <- subset(cyber_leavingJOINED, last_completed_week_number == "1")

# plot
ggplot(week_1_leaving_responses, aes(x= frequency(leaving_reason) , y = leaving_reason )) + 
  geom_bar(stat="identity", colour="black", fill="white") + 
  xlab("") + ylab("")


###### INVESTIGATION 2

# what can the video stats tell us?

# import data
cyber_security_3_video_stats <- read_csv("~/Desktop/DMET/data/cyber-security-3_video-stats.csv")



cyber_security_4_video_stats <- read_csv("~/Desktop/DMET/data/cyber-security-4_video-stats.csv")
cyber_security_5_video_stats <- read_csv("~/Desktop/DMET/data/cyber-security-5_video-stats.csv")
cyber_security_6_video_stats <- read_csv("~/Desktop/DMET/data/cyber-security-6_video-stats.csv")
cyber_security_7_video_stats <- read_csv("~/Desktop/DMET/data/cyber-security-7_video-stats.csv")
a <- bind_rows(cyber_security_4_video_stats, cyber_security_5_video_stats, id = NULL)
b <- bind_rows(a, cyber_security_6_video_stats, id = NULL)
JOINED_video_stats <- bind_rows(b, cyber_security_7_video_stats, id = NULL)


# another statistic we need to consider is length of videos against percentage watching
ggplot(data = cyber_security_7_video_stats, aes(x = video_duration, y = viewed_onehundred_percent)) + 
  geom_point() + geom_smooth(method="auto", se=TRUE, fullrange=FALSE, level=0.95)

# for joined data set
ggplot(data = JOINED_video_stats, aes(x = video_duration, y = viewed_onehundred_percent)) + 
  geom_point() + geom_smooth(method="auto", se=TRUE, fullrange=FALSE, level=0.95)
ggplot(data = JOINED_video_stats, aes(x = video_duration, y = total_views)) + 
  geom_point() + geom_smooth(method="auto", se=TRUE, fullrange=FALSE, level=0.95)


# group by video length and get mean for each
total_mean_views <- aggregate(JOINED_video_stats[, 4], list(JOINED_video_stats$video_duration), mean)
# plot new data frame based on mean views
ggplot(data = total_mean_views, aes(x = Group.1, y = total_views)) +
  geom_point() + geom_smooth(method="auto", se=TRUE, fullrange=FALSE, level=0.95)

# for one hundred percent viewings
onehundred_mean_views <- aggregate(JOINED_video_stats[, 15], list(JOINED_video_stats$video_duration), mean)
# plotting data
ggplot(data = onehundred_mean_views, aes(x = Group.1, y = viewed_onehundred_percent)) +
  geom_point() + geom_smooth(method="auto", se=TRUE, fullrange=FALSE, level=0.95)


ggplot(data = JOINED_video_stats, aes(x = video_duration, y = viewed_onehundred_percent)) 





# what percentage of individuals are watching the videos all the way through?
table(cyber_security_5_video_stats$viewed_onehundred_percent)
# maybe just take the step number and the actual stat to make a more concise table


# plot the graph of step number against viewed 100%
ggplot(cyber_security_5_video_stats, aes(x = step_position, y = viewed_onehundred_percent)) + geom_line()
# need to think about order 




###### INVESTIGATION 3

# step activity research
# which steps are allowing individuals to complete them?

# should perform this over a merged dataset? years 4-7 for example?
cyber_security_4_step_activity <- read_csv("~/Desktop/DMET/data/cyber-security-4_step-activity.csv")
cyber_security_5_step_activity <- read_csv("~/Desktop/DMET/data/cyber-security-5_step-activity.csv")
cyber_security_6_step_activity <- read_csv("~/Desktop/DMET/data/cyber-security-6_step-activity.csv")
cyber_security_7_step_activity <- read_csv("~/Desktop/DMET/data/cyber-security-7_step-activity.csv")
# removing duplicates
cyber_security_4_step_activity[!duplicated(cyber_security_4_step_activity),] # removing any duplicates
cyber_security_5_step_activity[!duplicated(cyber_security_5_step_activity),] # removing any duplicates
cyber_security_6_step_activity[!duplicated(cyber_security_6_step_activity),] # removing any duplicates
cyber_security_7_step_activity[!duplicated(cyber_security_7_step_activity),] # removing any duplicates
# join the datasets
cyberstep_activity4.5 <- bind_rows(cyber_security_4_step_activity, cyber_security_5_step_activity, id = NULL)
cyberstep_activity4.5.6 <- bind_rows(cyberstep_activity4.5, cyber_security_6_step_activity, id = NULL)
cyberstep_activityJOINED <- bind_rows(cyberstep_activity4.5.6, cyber_security_7_step_activity, id = NULL)
# change the specific column to variable
binary_step_activityJOINED <- cyberstep_activityJOINED
binary_step_activityJOINED$last_completed_at <-  ifelse(!is.na(binary_step_activityJOINED$last_completed_at),T,F)
# need to order the data based on week number
# or we need to try and extract 3 different df that only relate to week 1,2 and 3
binary_step_week1.JOINED <- subset(binary_step_activityJOINED, week_number = "1") # this creates a df with only week 1 data inside
binary_step_week2.JOINED <- subset(binary_step_activityJOINED, week_number = "2") # week 2 only
binary_step_week3.JOINED <- subset(binary_step_activityJOINED, week_number = "3") # week 3 only

## plots
# plot for producing stacked bar chart of individuals finishing each step
ggplot(data = binary_step_week1.JOINED, aes(fill = last_completed_at, y = frequency(last_completed_at), x = step_number)) + 
  geom_bar(position="stack", stat="identity") + ggtitle("Week 1 frequency of students completing step numbers for years 4-7")

# week 2
ggplot(data = binary_step_week2.JOINED, aes(fill = last_completed_at, y = frequency(last_completed_at), x = step_number)) + 
  geom_bar(position="stack", stat="identity") + ggtitle("Week 2 frequency of students completing step numbers for years 4-7")

# week 3
ggplot(data = binary_step_week3.JOINED, aes(fill = last_completed_at, y = frequency(last_completed_at), x = step_number)) + 
  geom_bar(position="stack", stat="identity") + ggtitle("Week 3 frequency of students completing step numbers for years 4-7")










# this is the structure of how to change NAs to 0 ... # d[is.na(d)] <- 0
# can do this another way
# another df for binary stuff
binary_step_activity6 <- cyber_security_6_step_activity
# setting variables to either True or False for specific column
binary_step_activity6$last_completed_at <-  ifelse(!is.na(binary_step_activity6$last_completed_at),T,F)
# now we can see how many individuals complete each step.


# split the data into steps to afford the plots
table(binary_step_activity6$step)
# steps going from 1.1-3.9

# need to plot the graph of x = step number and y = number completed/ number participated
#ggplot(data = binary_step_activity6, aes(x = step, y = frequency(last_completed_at))) + geom_bar() # failed

# possibly need to change the figures for step number... group the steps into 5s? 
# binary_step_activity6 <- binary_step_activity6 %>% mutate(step_simple = case_when(.$step == "tertiary" ~ "3",
                                             #  .$step == "university_masters" ~ "7",
                                             #  .$highest_education_level == "apprenticeship" ~ "4"))

# could instead produce 3 plots: 1 for each week
full_binary_step_activity6 <- binary_step_activity6
full_binary_step_activity6$first_visited_at <-  ifelse(!is.na(full_binary_step_activity6$first_visited_at),T,F)
# make a different data frame for each of the 3 weeks
# data_1a <- data[1:5, ] basic structure 
# find out the first data point for each of the weeks
binary_step_week1.6 <- full_binary_step_activity6[1:16572, ] # this creates a df with only week 1 data inside
binary_step_week2.6 <- full_binary_step_activity6[16572:25567, ] # week 2 only
binary_step_week3.6 <- full_binary_step_activity6[25567:31472, ] # week 3 only
# now can plot for each of the weeks with step number on the x axis


# it is clear that T and F are bot being counted, we need a situation where only true is occurring
# basic formula:
# ggplot(data, aes(fill=condition, y=value, x=specie)) + geom_bar(position="stack", stat="identity")

# plot for producing stacked bar chart of individuals finishing each step
ggplot(data = binary_step_week1.6, aes(fill = last_completed_at, y = frequency(last_completed_at), x = step_number)) + 
  geom_bar(position="stack", stat="identity") + ggtitle("Week 1 frequency of students completing step numbers for year 6")

# week 2
ggplot(data = binary_step_week2.6, aes(fill = last_completed_at, y = frequency(last_completed_at), x = step_number)) + 
  geom_bar(position="stack", stat="identity") + ggtitle("Week 2 frequency of students completing step numbers for year 6")

# week 3
ggplot(data = binary_step_week3.6, aes(fill = last_completed_at, y = frequency(last_completed_at), x = step_number)) + 
  geom_bar(position="stack", stat="identity") + ggtitle("Week 3 frequency of students completing step numbers for year 6")


# need to perform this between years 6 and 7 as there was a significant dip in purchases between these years
binary_step_activity7 <- cyber_security_7_step_activity
# setting variables to either True or False for specific column
binary_step_activity7$last_completed_at <-  ifelse(!is.na(binary_step_activity7$last_completed_at),T,F)

full_binary_step_activity7 <- binary_step_activity7
full_binary_step_activity7$first_visited_at <-  ifelse(!is.na(full_binary_step_activity7$first_visited_at),T,F)
# make a different data frame for each of the 3 weeks
# find out the first data point for each of the weeks
binary_step_week1.7 <- full_binary_step_activity7[1:14230, ] # this creates a df with only week 1 data inside
binary_step_week2.7 <- full_binary_step_activity7[14230:22629, ] # week 2 only
binary_step_week3.7 <- full_binary_step_activity7[22629:28304, ] # week 3 only


# plot for producing stacked bar chart of individuals finishing each step
ggplot(data = binary_step_week1.7, aes(fill = last_completed_at, y = frequency(last_completed_at), x = step_number)) + 
  geom_bar(position="stack", stat="identity") + ggtitle("Week 1 frequency of students completing step numbers for year 7")

# week 2
ggplot(data = binary_step_week2.7, aes(fill = last_completed_at, y = frequency(last_completed_at), x = step_number)) + 
  geom_bar(position="stack", stat="identity") + ggtitle("Week 2 frequency of students completing step numbers for year 7")

# week 3
ggplot(data = binary_step_week3.7, aes(fill = last_completed_at, y = frequency(last_completed_at), x = step_number)) + 
  geom_bar(position="stack", stat="identity") + ggtitle("Week 3 frequency of students completing step numbers for year 7")






# useful operations
#figure <- ggarrange(sp, bp + font("x.text", size = 10),
                #    ncol = 1, nrow = 2)
#annotate_figure(figure,
 #               top = text_grob("Visualizing mpg", color = "red", face = "bold", size = 14),
  #              bottom = text_grob("Data source: \n mtcars data set", color = "blue",
   #                                hjust = 1, x = 1, face = "italic", size = 10),
    #            left = text_grob("Figure arranged using ggpubr", color = "green", rot = 90),
     #           right = "I'm done, thanks :-)!",
      #          fig.lab = "Figure 1", fig.lab.face = "bold")
















###### INVESTIGATION 4 

# how does submission time effect question responses?
# make one graph that plots frequency/ occurrences of true over time
# make a graph that plots the same for false over time

# first load in data
cyber_security_4_question_response <- read_csv("~/Desktop/DMET/data/cyber-security-4_question-response.csv")
cyber_security_5_question_response <- read_csv("~/Desktop/DMET/data/cyber-security-5_question-response.csv")
cyber_security_6_question_response <- read_csv("~/Desktop/DMET/data/cyber-security-6_question-response.csv")
cyber_security_7_question_response <- read_csv("~/Desktop/DMET/data/cyber-security-7_question-response.csv")

cyber_security_4_question_response[!duplicated(cyber_security_4_question_response),] # removing any duplicates
cyber_security_5_question_response[!duplicated(cyber_security_5_question_response),] # removing any duplicates
cyber_security_6_question_response[!duplicated(cyber_security_6_question_response),] # removing any duplicates
cyber_security_7_question_response[!duplicated(cyber_security_7_question_response),] # removing any duplicates
dim(cyber_security_4_question_response)

# plot true occurrences only
ggplot(data = cyber_security_4_question_response, aes(x = submitted_at, y = frequency(correct), fill = correct)) + geom_area()
# not computing, maybe need to order the data first?

# in order to do this fairly need to look at one question at a time
# for question 1 only: 1936
question_1_responses.4 <- cyber_security_4_question_response[1:1936, ]
ggplot(data = question_1_responses.4, aes(x = submitted_at, y = frequency(correct), fill = correct)) + 
  geom_area(alpha=0.6 , size=1, colour="black")
# need to work on this, possibly use lubridate
# converting to date.time?
question_1_responses.4$submitted_at <- as_datetime(question_1_responses.4$submitted_at)
# need to order the data
# before that need to remove nas from specific column
question_1_responses.4 <- question_1_responses.4[!is.na(question_1_responses.4$submitted_at),]
# ordering by submission time
ordered_question_1_resp.4 <- question_1_responses.4[order(question_1_responses.4$submitted_at),]
# plot time against submission results
ggplot(data = ordered_question_1_resp.4, aes(x = submitted_at, y = cumsum(correct), fill = correct)) + 
  geom_area()
# when computing with cumsum it only counts the TRUE occurrences as 1 and FAILs as 0, therefore not working
# possibly need to add a column that counts values of occurrances
# basic structure: dataset %>% group_by(out) %>% mutate(count = sequence(n()))
ordered_question_1_resp.4 <- ordered_question_1_resp.4 %>% group_by(correct) %>% mutate(count_correct = sequence(n()))
# plot
ggplot(data = ordered_question_1_resp.4, aes(x = submitted_at, y = count_correct, fill = correct)) + 
  geom_area()
# need to make one transparent or not use area
ggplot(data = ordered_question_1_resp.4, aes(x = submitted_at, y = count_correct, colour = correct)) + 
  geom_line()



# need to repeat this process for random questions from other years
# question 2 end: 3499
# q 3 end: 5527
# q 4 end: 6710
# question 5 end: 7814
# q 6 end: 10043

# repeating process
question_2_responses.4 <- cyber_security_4_question_response[1936:3499, ]
question_3_responses.4 <- cyber_security_4_question_response[3499:5527, ]
question_4_responses.4 <- cyber_security_4_question_response[5527:6710, ]
question_5_responses.4 <- cyber_security_4_question_response[6710:7814, ]
question_6_responses.4 <- cyber_security_4_question_response[7814:10043, ]

# converting to date.time
question_2_responses.4$submitted_at <- as_datetime(question_2_responses.4$submitted_at)
question_3_responses.4$submitted_at <- as_datetime(question_3_responses.4$submitted_at)
question_4_responses.4$submitted_at <- as_datetime(question_4_responses.4$submitted_at)
question_5_responses.4$submitted_at <- as_datetime(question_5_responses.4$submitted_at)
question_6_responses.4$submitted_at <- as_datetime(question_6_responses.4$submitted_at)


# removing NAs from specific columns in each df
question_2_responses.4 <- question_2_responses.4[!is.na(question_2_responses.4$submitted_at),]
question_3_responses.4 <- question_3_responses.4[!is.na(question_3_responses.4$submitted_at),]
question_4_responses.4 <- question_4_responses.4[!is.na(question_4_responses.4$submitted_at),]
question_5_responses.4 <- question_5_responses.4[!is.na(question_5_responses.4$submitted_at),]
question_6_responses.4 <- question_6_responses.4[!is.na(question_6_responses.4$submitted_at),]

# ordering the data by submission time
ordered_question_2_resp.4 <- question_2_responses.4[order(question_2_responses.4$submitted_at),]
ordered_question_3_resp.4 <- question_3_responses.4[order(question_3_responses.4$submitted_at),]
ordered_question_4_resp.4 <- question_4_responses.4[order(question_4_responses.4$submitted_at),]
ordered_question_5_resp.4 <- question_5_responses.4[order(question_5_responses.4$submitted_at),]
ordered_question_6_resp.4 <- question_6_responses.4[order(question_6_responses.4$submitted_at),]

# add column for true occurances
ordered_question_2_resp.4 <- ordered_question_2_resp.4 %>% group_by(correct) %>% mutate(count_correct = sequence(n()))
ordered_question_3_resp.4 <- ordered_question_3_resp.4 %>% group_by(correct) %>% mutate(count_correct = sequence(n()))
ordered_question_4_resp.4 <- ordered_question_4_resp.4 %>% group_by(correct) %>% mutate(count_correct = sequence(n()))
ordered_question_5_resp.4 <- ordered_question_5_resp.4 %>% group_by(correct) %>% mutate(count_correct = sequence(n()))
ordered_question_6_resp.4 <- ordered_question_6_resp.4 %>% group_by(correct) %>% mutate(count_correct = sequence(n()))

# plotting all graphics
plot_q2.4 <- ggplot(data = ordered_question_2_resp.4, aes(x = submitted_at, y = count_correct, colour = correct)) + 
  geom_line() + ggtitle("correct against time for q.2")
plot_q3.4 <- ggplot(data = ordered_question_3_resp.4, aes(x = submitted_at, y = count_correct, colour = correct)) + 
  geom_line() + ggtitle("correct against time for q.3")
plot_q4.4 <- ggplot(data = ordered_question_4_resp.4, aes(x = submitted_at, y = count_correct, colour = correct)) + 
  geom_line() + ggtitle("correct against time for q.4")
plot_q5.4 <- ggplot(data = ordered_question_5_resp.4, aes(x = submitted_at, y = count_correct, colour = correct)) + 
  geom_line() + ggtitle("correct against time for q.5")
plot_q6.4 <- ggplot(data = ordered_question_6_resp.4, aes(x = submitted_at, y = count_correct, colour = correct)) + 
  geom_line() + ggtitle("correct against time for q.6")

ggarrange(plot_q2.4, plot_q3.4, plot_q4.4, plot_q6.4,
          ncol = 2, nrow = 2)

# 5 is anomolous and had only correct answers


#### AM/PM tests

# check if am/pm effects the submission results
sum(am(question_1_responses.4$submitted_at))
sum(pm(question_1_responses.4$submitted_at))
# there are actually more submissions for pm does that hold true over the whole dataset?
sum(am(cyber_security_4_question_response$submitted_at))
sum(pm(cyber_security_4_question_response$submitted_at))
# yes it does

## Analysis of the joined data set
# join data
response4.5 <- bind_rows(cyber_security_4_question_response, cyber_security_5_question_response, id = NULL)
response4.5.6 <- bind_rows(response4.5, cyber_security_6_question_response, id = NULL)
responseJOINED <- bind_rows(response4.5.6, cyber_security_7_question_response, id = NULL)

# number of responses for each 
sum(am(responseJOINED$submitted_at))
sum(pm(responseJOINED$submitted_at))
# make pie chart for this?
# make a data frame with just these responses in?
Pie_df <- matrix(c(sum(am(responseJOINED$submitted_at)), "am", sum(pm(responseJOINED$submitted_at)), "pm"), ncol=2, byrow=TRUE)
colnames(Pie_df) <- c("submission_count", "time_of_day")
Pie_df <- as.data.frame(Pie_df)
# need to make a simple bar chart
bp <- ggplot(data = Pie_df, aes(x= "", y = submission_count, fill = time_of_day)) +
  geom_bar(width = 1, stat = "identity")
# plot the pie graph
bp + coord_polar("y", start=0)

# from this, does the time of day effect submission results?
# could make 2 different data frames one for am and one for pm, compare side by side?
am_only <- subset(responseJOINED, am(submitted_at))
pm_only <- subset(responseJOINED, pm(submitted_at))

# bar chart for true against false for am
ggplot(data = am_only, aes(x = correct, y = frequency(correct), fill = correct)) +
  geom_bar(stat = "identity")
# bar plot for pm
ggplot(data = pm_only, aes(x = correct, y = frequency(correct), fill = correct)) +
  geom_bar(stat = "identity")
# plot side by side 



# make a table of percentages for am and pm time frames 
# calculating percentages
#am true and false
((sum(am_only$correct=="TRUE")) / (sum(am_only$correct=="TRUE") + sum(am_only$correct=="FALSE")))*100
((sum(am_only$correct=="FALSE")) / (sum(am_only$correct=="TRUE") + sum(am_only$correct=="FALSE")))*100
# pm true and false
((sum(pm_only$correct=="TRUE")) / (sum(pm_only$correct=="TRUE") + sum(pm_only$correct=="FALSE")))*100
((sum(pm_only$correct=="FALSE")) / (sum(pm_only$correct=="TRUE") + sum(pm_only$correct=="FALSE")))*100

# creating a matrix with the percentages
percentage_table_am_pm <- matrix(c(((sum(am_only$correct=="TRUE")) / (sum(am_only$correct=="TRUE") + sum(am_only$correct=="FALSE")))*100,
                                   ((sum(pm_only$correct=="TRUE")) / (sum(pm_only$correct=="TRUE") + sum(pm_only$correct=="FALSE")))*100,
                                   ((sum(am_only$correct=="FALSE")) / (sum(am_only$correct=="TRUE") + sum(am_only$correct=="FALSE")))*100,
                                   ((sum(pm_only$correct=="FALSE")) / (sum(pm_only$correct=="TRUE") + sum(pm_only$correct=="FALSE")))*100), ncol = 2, byrow = TRUE)

# naming the columns and rows
colnames(percentage_table_am_pm) <- c("am", "pm")
rownames(percentage_table_am_pm) <- c("%_TRUE", "%_FALSE")







#### EXTRAS 
# selecting devices for the device analysis
# setting up means in each case
vid_df3 = cyber_security_3_video_stats[, c(17, 18, 20)]
year3vid = colMeans(vid_df3)

vid_df4 = cyber_security_4_video_stats[, c(17, 18, 20)]
year4vid = colMeans(vid_df4)

vid_df5 = cyber_security_5_video_stats[, c(17, 18, 20)]
year5vid = colMeans(vid_df5)

vid_df6 = cyber_security_6_video_stats[, c(17, 18, 20)]
year6vid = colMeans(vid_df6)

vid_df7 = cyber_security_7_video_stats[, c(17, 18, 20)]
year7vid = colMeans(vid_df7)

# binding the vid_dfs together 
extra_vid_df = rbind(year3vid, year4vid, year5vid, year6vid, year7vid)
colnames(extra_vid_df) = c("Desktop", "Mobile", "Tablet")

# using melt to convert from wide to molten
molten_mean_df = melt(extra_vid_df)
colnames(molten_mean_df) = c('Iteration', 'Device', 'Usage')

ggplot()+
  geom_bar(data = molten_mean_df, mapping = aes(x = Iteration, y = Usage, fill = Device), stat = 'identity', position = position_dodge())+
  geom_line(as.data.frame(extra_vid_df), mapping = aes(x = as.factor(rownames(extra_vid_df)), y = Mobile, group = 1, color = "Mobile Usage"), size = 1.5)+
  geom_point(as.data.frame(extra_vid_df), mapping = aes(x = as.factor(rownames(extra_vid_df)), y = Mobile, group = 1, color = "Mobile Usage"), size = 4)+
  ylab("Device Usage")+
  xlab("Year")+
  scale_color_manual(" ", values = c("Mobile Usage" = "Black"))+
  scale_fill_brewer(palette = "Set4")




###### Bits that were missed out 

# table displaying the most common leaving responses
table(cyber_leavingJOINED$leaving_reason)


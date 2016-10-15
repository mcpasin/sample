library(readr)
library(dplyr)

questions <- read_csv("questions.csv.gz")
question_tags <- read_csv("question_tags.csv.gz")

#Subset only 2016 questions for memory reasons
library(lubridate)
questions_2016<- filter(questions, year(CreationDate)=="2016")
head(questions_2016)
summary(questions_2016)

#Merge questions with question_tag (it duplicates questions df rows where there multiple tags )
merged_df<-left_join(questions_2016,question_tags,by="Id")
#looks fine: see e.g. first question http://stackoverflow.com/questions/34552550/scope-between-methods

### What are the most popular tags?
pop_tags<-question_tags %>%
  count(Tag, sort = TRUE)
pop_tags<-as.data.frame(pop_tags)

#popular tags in 2016
pop_tags_2016<-merged_df %>%
  select(Id,Tag) %>%
  count(Tag,sort=TRUE) %>%
  mutate(freq=paste0(round(100*n/sum(n),2),"%")) 
#R at 17th place, it was 27th in the whole history
#we can see that share is vert fragmented


library(ggplot2)

questions_2016 %>%
  count(Month = round_date(CreationDate, "month")) %>%
  ggplot(aes(Month, n)) +
  geom_line()

### Tags trend over time (number of questions made for each tag)

### Tags that tend to have higher or lower score. 
#first note that score can be positive or negative, lots of questions don´t get a score at all ("0")
summary(questions_2016$Score)

score_tags_2016<-merged_df %>%
  select(Score,Tag) %>%
  group_by(Tag) %>%
  summarize(score_avg=mean(Score),n_questions=n())%>%
  arrange(desc(n_questions))

score_tags_2016_ds<-merged_df_ds %>%
  select(Score,Tag) %>%
  group_by(Tag) %>%
  summarize(score_avg=mean(Score),n_questions=n())%>%
  arrange(desc(n_questions))

#Which was the R question with the highest grade in 2016 (so far)?
#Let find actually the top 5 scored questions
merged_df_ds %>%
  filter(Tag=="r") %>%
  arrange(desc(Score)) %>%
            head(5)


### Tags that tend to have higher number of answers. 
sum(is.na(questions_2016$AnswerCount)) #there are several NA
answer_tags_2016<-merged_df %>%
  select(AnswerCount,Tag) %>%
  group_by(Tag) %>%
  summarize(answer_avg=mean(AnswerCount,na.rm=T),n_questions=n())%>%
  arrange(desc(n_questions))
#among popular tags, CSS and HTML tags tend to have higher response rate

answer_tags_2016_ds<-merged_df_ds %>%
  select(AnswerCount,Tag) %>%
  group_by(Tag) %>%
  summarize(answer_avg=mean(AnswerCount,na.rm=T),n_questions=n())%>%
  arrange(desc(n_questions))


### What is the rate of question closure for each tag? and trend over time
sum(!is.na(questions_2016$ClosedDate))/nrow(questions_2016)
# About 10% of questions created in 2016 have been closed.

#FUNCIONA! very fast to calculate
closed_tags_2016<- merged_df %>%
  select(ClosedDate,Tag) %>%
  group_by(Tag) %>%
  summarise_each(funs(close_rate=sum(!is.na(.)) / length(.)*100, n_questions=n())) %>%
  arrange(desc(n_questions))
#very interesting results: R has 14% closure rate, vba just 4%,etc.

closed_tags_2016_ds<- merged_df_ds %>%
  select(ClosedDate,Tag) %>%
  group_by(Tag) %>%
  summarise_each(funs(close_rate=sum(!is.na(.)) / length(.)*100, n_questions=n())) %>%
  arrange(desc(n_questions))

### What´s the average time to close a question for each tag?
# time is in hours

time_tags_2016<-merged_df %>%
  filter (!is.na(ClosedDate)) %>%
  mutate(difference=(ClosedDate-CreationDate)/3600) %>%
  select(difference,Tag) %>%
  group_by(Tag) %>%
  summarize(avg_hours=round(mean(difference),2),n_questions_closed=n())%>%
  arrange(desc(n_questions_closed))


### How many questions have more than one tag? questions distribution by number of tags
distrib_tags_2016<-merged_df %>%
  select(Id,Tag) %>%
  group_by(Id) %>%
  summarize (n_tags=n()) %>%
  arrange(desc(n_tags))
#double checked on stackoverflow site and it look ok e.g. question 34552565 has 5 tags

library(ggplot2)
qplot(distrib_tags_2016$n_tags,
      geom="histogram",
      binwidth = 0.5,  
      main = "Tags per question", 
      xlab = "Number of tags", 
      ylab = "Number of questions",
      label=distrib_tags_2016$n_tags,
      fill=I("blue"))
#most of questions have 3 or 2 tags. I guess there is a max allowed of 5 tags.


sum(distrib_tags_2016$n_tags==1)/nrow(distrib_tags_2016)

### Focus on data science language. Above questions comparing R, Python, SQL,Excel,matlab, SAS, SPSS, Pandas 
#(4 main ds languages as reported in http://www.kdnuggets.com/2014/08/four-main-languages-analytics-data-mining-data-science.html)
#plus a few more

#create a vector containing data science related tags
ds_tags<-c("r","python","sas","sql","pandas","excel","matlab")
pop_ds_tags_2016<- pop_tags_2016 %>%
  filter(Tag %in% ds_tags)

#Subset the merged dataset (questions+tags) to include only data science tags
merged_df_ds<- merged_df %>%
  filter(Tag %in% ds_tags)
#check it contains only ds tags
unique(merged_df_ds_tags$Tag)

#Plot number of questions MoM by ds tags
test<-merged_df_ds %>%
  mutate(Week = round_date(CreationDate, "week")) %>%
           select(Week,Tag) %>%
  group_by (Week,Tag) %>%
  summarize(n_questions=n())  
     

## close rate for ds questions
closed_tags_ds<- merged_df_ds %>%
  select(ClosedDate,Tag) %>%
  group_by(Tag) %>%
  summarise_each(funs(close_rate=sum(!is.na(.)) / length(.)*100, n_questions=n())) %>%
  arrange(desc(n_questions)) %>%
  ggplot( aes(x = Tag, y = close_rate,fill=Tag))+ geom_bar(stat="identity") + ggtitle("Closure rate for data science questions")

closed_tags_ds
#curious that matlab has highest closure rate.

##time to close questions for ds ...use only graphics!
time_tags_ds<-merged_df_ds %>%
  filter (!is.na(ClosedDate)) %>%
  mutate(difference=(ClosedDate-CreationDate)/3600) %>%
  select(difference,Tag) %>%
  group_by(Tag) %>%
  summarize(avg_hours=round(mean(difference),2))%>%
  arrange(desc(avg_hours)) %>%
  ggplot( aes(x = Tag, y = avg_hours,label = avg_hours,fill=Tag))+ geom_bar(stat="identity")+ ggtitle("Speed (in hours) at which data science questions are closed")
                

### Tags association. What are the most popular tags combinations?
#http://varianceexplained.org/r/seven-fav-packages/

test<-head(question_tags,1000)
tags_counts <- test %>%
  count(Tag) %>%
  filter(n >= 2)

library(devtools)
install_github("dgrtwo/widyr")
library("widyr")
#semi_join keeps only observations in test that have match in tags_counts (that is tags with n>=2), and only columns of x
#result of semi_join is a table with Id question and tag mentioned,ordered by Id and tag
tags_correlations <- test %>%
  semi_join(tags_counts) %>%
  pairwise_cor(Tag, Id, sort = TRUE, upper = FALSE)

View(tags_correlations)

#Let plot tags co-ocurrencies
#install forst required packages
https://github.com/thomasp85/ggraph
devtools::install_github('thomasp85/ggforce')
devtools::install_github('thomasp85/ggraph')
library(ggraph)
library(igraph)

set.seed(2016)

# we set an arbitrary threshold of connectivity
tags_correlations %>%
  filter(correlation > .3) %>%
  graph_from_data_frame(vertices = tags_counts) %>%
  ggraph(layout = "fr") +
  geom_edge_link(aes(edge_alpha = correlation)) +
  geom_node_point(aes(size = n), color = "lightblue") +
  theme_void() +
  geom_node_text(aes(label = name), repel = TRUE) +
  theme(legend.position = "none")


### Explore available data for Digital Analytics (GA,Adobe, GTM,etc.)
subset(pop_tags,Tag=='google-analytics')
subset(pop_tags,Tag=='google-tag-manager')
subset(pop_tags,Tag=='adobe-analytics')


### Focus on GA. Above questions


### Focus on co-presence GA+R
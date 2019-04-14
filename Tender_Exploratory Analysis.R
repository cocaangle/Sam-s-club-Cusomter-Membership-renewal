#load packages
library(data.table)
library(caret) 
library(dplyr) 
library(ggplot2)
#import and read data
pos=fread("pos.txt")
tender=fread("tender_type.txt")
member=fread('members.txt')
dmm=fread("dmm_gmm.txt")

##Split training and testing dataset
set.seed(5) 
member_sample=member[sample(nrow(member), 20000), ] 
member_train=member_sample[1:10000,]
member_test=member_sample[10001:20000,]
pos_train=pos[which(pos$MEMBERSHIP_ID %in% member_train$MEMBERSHIP_ID ),] 
pos_test=pos[which(pos$MEMBERSHIP_ID %in% member_test$MEMBERSHIP_ID ),]
tender_train_related=tender[which(tender$VISIT_NBR %in% pos_train$VISIT_NBR),]
data=left_join(tender_train_related, pos_train) 
data2=data[,1:7]
uni=unique(data2) 
tender_train=na.omit(uni)
tender_test_related=tender[which(tender$VISIT_NBR %in% pos_test$VISIT_NBR),] 
data=left_join(tender_test_related, pos_test)
data2=data[,1:7]
uni=unique(data2)
tender_test=na.omit(uni)

#Write to csv
write.csv(member_train, 'member_train.csv', row.names = FALSE)
write.csv(member_test, 'member_test.csv', row.names = FALSE)
write.csv(pos_train, 'pos_train.csv', row.names = FALSE) 
write.csv(pos_test, 'pos_test.csv', row.names = FALSE) 
write.csv(tender_train, 'tender_train.csv', row.names = FALSE) 
write.csv(tender_test, 'tender_test.csv', row.names = FALSE)

#Explore for tender data

#Total AMT VS Tender_Type
tender_AMT= tender_train %>% group_by(TENDER_TYPE_DESC) %>%summarise(totalAMT = sum(TENDER_AMT))%>%arrange(desc(totalAMT))
top_tender=top_n(tender_AMT,8)
ggplot(top_tender, aes(x=TENDER_TYPE_DESC,y = totalAMT)) +geom_bar(stat = "identity",fill="#FF6666")

#We can see the TOP3 total transaction amount tender_type is debit card>visa>Sam's consumer credit

#Average AMT VS Tender_type
tender_avg_AMT= tender_train %>% group_by(TENDER_TYPE_DESC) %>%summarise(AVG_AMT = mean(TENDER_AMT))%>%arrange(desc(AVG_AMT))
top_tender_avg=top_n(tender_avg_AMT,8)
ggplot(top_tender_avg, aes(x=TENDER_TYPE_DESC,y = AVG_AMT)) +geom_bar(stat = "identity",fill="#FF6666")
table(tender_train$TENDER_TYPE_DESC)
# We can see that for the average of each transaction amount, Unknown>>Sam's business credit>Sam's direct credit>service income
#(shall "unknown" be removed or not?)--Customers using these tender types are more likely to renew their membership as they spend more than others in each
# of their transaction

#Count of Tender_Type
counts=data.frame(table(tender_train$TENDER_TYPE_DESC))
top_counts=top_n(counts,8)
ggplot(top_counts, aes(x=Var1,y = Freq)) +geom_bar(stat = "identity",fill="#FF6666")

#Similar pattern with total transaction amount, we can see the tender type that's used most is debit card>visa>Sam's consumer credit

#Total AMT VS each visit
each_visit= tender_train %>% group_by(VISIT_DATE,VISIT_NBR,CLUB_NBR) %>%summarise(total_AMT = sum(TENDER_AMT))%>%arrange(desc(total_AMT))
head(each_visit)

#Total AMT VS each member
each_member= tender_train %>% group_by(MEMBERSHIP_ID) %>%summarise(tot_mem = sum(TENDER_AMT))%>%arrange(desc(tot_mem))
top_member=top_n(each_member,8)
top_member
# So these members maybe more likely to renew their membership as they spend more on than others in Sam's club, who maybe the loyalty customers for Sam
# club, to confirm the assumption, we could join these top_membertable with member_train to see the account renew status

#Join table 
# Renewed VS top spended member
df=merge(x=each_member,y=member_train,by="MEMBERSHIP_ID",all.x=TRUE)
df=df%>%arrange(desc(tot_mem))
top_df=df[1:3879,]
table(top_df$RENEW_IND)
pct=262/3879
pct
write.csv(df,file="top_fifty_percent_users.csv")
# So we see on the top 50% people who spend most , only 7% of them not renewed, which indicate people who spend more are more likely to renew their memership
# For next step, we can check the pattern of these top people's tenure years, plus_status, market area and categories of the items they buy most to get more 
# insights.

#Renewed VS tender type
df1=merge(x=tender_train,y=member_train,by="MEMBERSHIP_ID",all.x=TRUE)
renew_df1=df1%>%filter(RENEW_IND!="UNRENEWED")
type_df=as.data.frame(table(renew_df1$TENDER_TYPE_DESC))
type_df=type_df%>%arrange(desc(Freq))%>%head(8)
ggplot(type_df, aes(x=Var1,y = Freq)) +geom_bar(stat = "identity",fill="#FF6666")
#Same as as the count of tender type in total sample: Debit card>Visa>Sam's consumer credit
# So there may be no difference in tender type for people who renew their membership or who not renew


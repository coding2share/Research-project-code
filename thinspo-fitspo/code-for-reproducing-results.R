#thinspo and fitspo project
#examines content of tweets with hashtags thinspo and fitspo
#author Jenine K. Harris contact harrisj@wustl.edu
#data clean-twit.csv saved at: 
#https://github.com/coding2share/Research-project-code/blob/master/thinspo-fitspo/clean-twit.csv
#variable dictionary codebook-twit.txt saved at:
#https://github.com/coding2share/Research-project-code/blob/master/thinspo-fitspo/codebook-twit.txt
#last edited 11/16/2017

#bring in clean data from CSV (need install data.table library)
library(data.table)
twit2 <- fread("https://raw.githubusercontent.com/coding2share/Research-project-code/master/thinspo-fitspo/clean-twit.csv",
               stringsAsFactors = TRUE)

#fix variable types
twit2$imgfocustext <- as.character(twit2$imgfocustext)
twit2$webtopictext <- as.character(twit2$webtopictext)
twit2$tweeturl <- as.character(twit2$tweeturl)
twit2$name1 <- as.character(twit2$name1)
twit2$Name <- as.character(twit2$Name)
twit2$hashtags <- as.character(twit2$hashtags)
twit2$mention <- as.factor(twit2$mention)

#determine the number of relevant and non-relevant tweets
rel <- table(twit2$relevant)
rel
prop.table(rel)

#how many of the unable to tell relevance tweets were
#not in English
table(twit2$relevant,twit2$english)

#determine how many twitter users have >1 tweet in data
library(descr)
userfreq <- data.frame(freq(twit2$name1, plot=F))
freq(userfreq$Frequency[userfreq$Frequency!=1035], plot=F)

#subset the data so analyses are performed on relevant tweets
twit2rel <- subset(twit2, twit2$relevant=="Yes")

#how many of the hashtag only tweets included an image
table(twit2rel$hashtagonly, twit2rel$imagevid)
#159/162 of hashtag only tweets had image or video
159/162
#98.1%

#frequency with image/video
table(twit2rel$imagevid)

#frequency with mentions
table(twit2rel$mention)

#recode replies to binary
twit2rel$repliesbi[twit2rel$replies>0]<-1
twit2rel$repliesbi[twit2rel$replies==0]<-0
twit2rel$repliesbi <- as.factor(twit2rel$repliesbi)

#Table 1 for categorical variables
#totals
sapply(twit2rel, function(x) if(class(x)=="factor") freq(x, plot=F))
median(twit2rel$RT, na.rm=T)
median(twit2rel$likes, na.rm=T)
median(twit2rel$replies, na.rm=T)
IQR(twit2rel$RT, na.rm=T)
IQR(twit2rel$likes, na.rm=T)
IQR(twit2rel$replies, na.rm=T)
#thinspo vs. fitspo
#Table 1 using subset
twitfit <- subset(twit2rel, twit2rel$thinfit=="fitspo")
twitthin <- subset(twit2rel, twit2rel$thinfit=="thinspo")
sapply(twitfit, function(x) if(class(x)=="factor") freq(x, plot=F))
median(twitfit$RT, na.rm=T)
median(twitfit$likes, na.rm=T)
median(twitfit$replies, na.rm=T)
IQR(twitfit$RT, na.rm=T)
IQR(twitfit$likes, na.rm=T)
IQR(twitfit$replies, na.rm=T)
sapply(twitthin, function(x) if(class(x)=="factor") freq(x, plot=F))
median(twitthin$RT, na.rm=T)
median(twitthin$likes, na.rm=T)
median(twitthin$replies, na.rm=T)
IQR(twitthin$RT, na.rm=T)
IQR(twitthin$likes, na.rm=T)
IQR(twitthin$replies, na.rm=T)

#are all the muscular fitspo images of males?
table(twitfit$binumimgmale,twitfit$binumimgmuscular)
table(twitfit$binumimagesboth,twitfit$binumimgmuscular)
table(twitfit$binumimgcanttell,twitfit$binumimgmuscular)
#of 45 muscular images, 13 were males only
#5 were both males, females; 1 was can't tell

#Table 2 unique users
#remove duplicates
twit2user <- twit2rel[!duplicated(twit2rel$name1),]
#identify user level variables and subset
twituser <- subset(twit2user, select=c(tweetertype, thinfit,Followed,Favorites,Tweets,Followers))
sapply(twituser, function(x) if(class(x)=="factor") freq(x, plot=F))
median(twituser$Followed, na.rm=T)
median(twituser$Followers, na.rm=T)
median(twituser$Tweets, na.rm=T)
median(twituser$Favorites, na.rm=T)
IQR(twituser$Followed, na.rm=T)
IQR(twituser$Followers, na.rm=T)
IQR(twituser$Tweets, na.rm=T)
IQR(twituser$Favorites, na.rm=T)

#thinspo and fitspo tweeter comparison for Table 1
twituserfit <- subset(twituser, twituser$thinfit=="fitspo")
twituserthin <- subset(twituser, twituser$thinfit=="thinspo")
sapply(twituserfit, function(x) if(class(x)=="factor") freq(x, plot=F))
median(twituserfit$Followed, na.rm=T)
median(twituserfit$Followers, na.rm=T)
median(twituserfit$Tweets, na.rm=T)
median(twituserfit$Favorites, na.rm=T)
IQR(twituserfit$Followed, na.rm=T)
IQR(twituserfit$Followers, na.rm=T)
IQR(twituserfit$Tweets, na.rm=T)
IQR(twituserfit$Favorites, na.rm=T)
sapply(twituserthin, function(x) if(class(x)=="factor") freq(x, plot=F))
median(twituserthin$Followed, na.rm=T)
median(twituserthin$Followers, na.rm=T)
median(twituserthin$Tweets, na.rm=T)
median(twituserthin$Favorites, na.rm=T)
IQR(twituserthin$Followed, na.rm=T)
IQR(twituserthin$Followers, na.rm=T)
IQR(twituserthin$Tweets, na.rm=T)
IQR(twituserthin$Favorites, na.rm=T)




#graphs of topics
#subset to tweet topics variables
topic <- subset(twit2rel, select=c(mentionmeds,mentioneatless,mentioneathealthy,mentionexercise,
                            mentionstrength,mentionloseweight,mentionbinge,mentionvomit,
                            mentionwantbody,mentionnone,mentiondisorder,thinfit))
topic[topic =="No"] <- NA

topicthin <- subset(topic, thinfit=="thinspo")
topicfit <-subset(topic, thinfit=="fitspo")

w2 <- apply(topicthin, 2, table)
w3 <- apply(topicfit, 2, table)
final <- data.frame(rbind(w2,w3))
final[final=="integer(0)"] <- 0
final <- data.frame(t(final))

colnames(final) <- c("thinspo","fitspo")
final <- final[which(final$thinspo!=458), ]
final$topic = rownames(final)

#458 thinspo tweets, 238 fitspo
final$pthinspo <- as.numeric(paste(final$thinspo))/458
final$pfitspo <- as.numeric(paste(final$fitspo))/238
fitvars <- c('fitspo','pfitspo','topic')
thinvars <- c('thinspo','pthinspo','topic')
fitspodata <- final[fitvars]
fitspodata$thinfit <- "fitspo"
colnames(fitspodata) <- c("tweets","ptweets","topic","thinfit")

thinspodata <- final[thinvars]
thinspodata$thinfit <- "thinspo"
colnames(thinspodata) <- c("tweets","ptweets","topic","thinfit")

finallong <- rbind(fitspodata,thinspodata)
finallong <- subset(finallong, finallong$topic!="thinfit")


#Figure 1
library(ggplot2)
library(grid)
library(gridExtra)
ggplot(data=finallong, aes(x=reorder(topic, ptweets), y=ptweets*100, fill=thinfit)) +
  geom_bar(position = "dodge", stat="identity") + 
  scale_x_discrete(breaks=c("mentionloseweight", "mentionexercise", "mentiondisorder",
                            'mentionwantbody','mentioneatless','mentionstrength',
                            'mentionvomit','mentioneathealthy','mentionmeds',
                            'mentionbinge','mentionnone'),
                   labels=c("Losing weight", "Exercise", "Eating disorder",
                            'Wanting a body type or body part characteristic',
                            'Eating less','Gaining strength',
                            'Purging','Eating healthy','Medication',
                            'Binging',"None of these"))+
  labs(x="Topic Mentioned", y="Percentage of Tweets Within\n#thinspo or #fitspo Category", fill='')+
  scale_fill_manual(values=c('gray60','gray30'), 
                    labels = c("#fitspo", "#thinspo"),
                    guide = guide_legend(reverse=TRUE))+ 
  coord_flip()+
  theme_classic() +
  theme(legend.position = c(0.72, 0.2), 
        panel.grid.minor.x = element_line(color="grey80"),
        axis.line = element_line(color="grey80"),
        axis.ticks.y = element_blank(),
        axis.ticks.x = element_line(color="grey80"),
        plot.margin = unit(c(.5,.5,.5,.5), "cm")) 
grid.rect(width = .98, height = 1, gp = gpar(lwd = 1, col = "gray80", fill = NA))

#test normality for retweets, likes, replies
shapiro.test(twit2rel$RT)        
shapiro.test(twit2rel$likes)     
shapiro.test(twit2rel$replies)
#all three fail

#examine distributions
hist(twit2rel$RT, xlim=c(0,200), breaks=500)        
hist(twit2rel$likes, xlim=c(0,200), breaks=500)     
hist(twit2rel$replies, xlim=c(0,10), breaks=50)
#extremely right skewed for RT and likes
#replies seems more like a binary variable
#with 0=no, 1=yes

#descriptives for outcomes
library(descr)
mean(twit2rel$RT, na.rm=T)
var(twit2rel$RT, na.rm=T)
mean(twit2rel$likes, na.rm=T)
var(twit2rel$likes, na.rm=T)
freq(twit2rel$replies)

#how many unique twitter users
unique(twit2rel$name1)

#check correlation between thinspo/fitspo and images in tweet
cor.test(as.numeric(twit2rel$thinfit), as.numeric(twit2rel$imagevid))

#multilevel negative binomial regression
library(glmmADMB) #must install from source see http://glmmadmb.r-forge.r-project.org/
#installation if needed:
#install.packages("R2admb")
#install.packages("glmmADMB", 
                 #repos=c("http://glmmadmb.r-forge.r-project.org/repos",
                         #getOption("repos")),
                 #type="source")

#has to have factor for groups
#recode groups variable to factor
twit2rel$name1<-as.factor(twit2rel$name1)
#has to have complete cases data or error
#subset data so complete cases for all variables
#included in model
twit3 <- subset(twit2rel, !is.na(twit2rel$RT) & !is.na(twit2rel$name1) & 
                  !is.na(twit2rel$thinfit) & !is.na(twit2rel$Tweets) &
                  !is.na(twit2rel$tweetertype)&
                  !is.na(twit2rel$Followers) )
twit3thin <-subset(twit3, twit3$thinfit=="thinspo")
twit3fit <-subset(twit3, twit3$thinfit=="fitspo")
#model retweets
#tweets and followers in hundreds
twit3$Tweets <- (twit3$Tweets)/100
twit3$Followers <- (twit3$Followers)/100
model1<-glmmadmb(RT ~  tweetertype + Followers + Tweets  + mention + (1|name1), data=twit3,
                 zeroInflation=F,family="nbinom")
summary(model1)


#add thinspo fitspo variable
model2<-glmmadmb(RT ~ tweetertype + Followers + Tweets + thinfit  + mention + (1|name1), data=twit3,
                 zeroInflation=F,family="nbinom")
summary(model2)
#add fitted values to data set
#plot fitted vs. actual for model fit
twit3$fit <- model1$fitted
plot(twit3$RT, twit3$fit)

#subset the model likes
twit4 <- subset(twit2rel, !is.na(twit2rel$likes) & !is.na(twit2rel$name1) & 
                  !is.na(twit2rel$thinfit) & !is.na(twit2rel$Tweets) &
                  !is.na(twit2rel$Followers) &
                  !is.na(twit2rel$tweetertype))
twit4$name1<-as.factor(twit4$name1)
twit4$Tweets <- (twit4$Tweets)/100
twit4$Followers <- (twit4$Followers)/100
model3<-glmmadmb(likes ~ tweetertype + Followers + Tweets +mention +(1|name1), data=twit4,zeroInflation=F,family="nbinom")
summary(model3)
model4<-glmmadmb(likes ~ thinfit + tweetertype + Followers + Tweets + mention + (1|name1), data=twit4,zeroInflation=F,family="nbinom")
summary(model4)
#plot fitted vs. actual for model fit
twit4$fit <- model2$fitted
plot(twit4$RT, twit4$fit)

#recode replies, subset, model
twit5 <- subset(twit2rel, !is.na(twit2rel$repliesbi) & !is.na(twit2rel$name1) & 
                  !is.na(twit2rel$thinfit) & !is.na(twit2rel$Tweets) &
                  !is.na(twit2rel$Followers) & 
                  !is.na(twit2rel$tweetertype))
twit5$Tweets <- (twit5$Tweets)/100
twit5$Followers <- (twit5$Followers)/100
model5<-glmmadmb(repliesbi ~ tweetertype + Tweets + Followers  + mention + (1|name1), data=twit5,zeroInflation=F,family="binom")
summary(model5)
model6<-glmmadmb(repliesbi ~ tweetertype + Tweets + Followers  +thinfit  + mention + (1|name1), data=twit5,zeroInflation=F,family="binom")
summary(model6)


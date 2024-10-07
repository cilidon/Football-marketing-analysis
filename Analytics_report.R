# american viewers watching european football analytics report

# Name: Mandar Mhatre, Shuai Shi

# Date: November 14, 2023

getwd()
setwd('C:/Users/manda/Downloads')
eufs <- read.csv("eufs.csv")

# setup the variables and create new columns

eufs[,1:7] <- lapply(eufs[,1:7],factor,levels=c("Yes","No"))
eufs[,8:16] <- lapply(eufs[,8:16], as.factor)

# Total European football club logos recognized per participant (ClubLogoRecog)

eufs$ClubLogoRecog <- apply(eufs[,17:52], MARGIN=1, FUN=sum)
eufs$ClubLogoRecog

#	Total percentage of European football club logos recognized per participant (PercentLogoRecog)

eufs$PercentLogoRecog <- round(100*eufs$ClubLogoRecog/36, digits=1)
eufs$PercentLogoRecog  

# Total European football club sponsor logos recognized per participant (SponLogoRecog)

eufs$SponLogoRecog <- apply(eufs[,53:70], MARGIN = 1, FUN=sum)
eufs$SponLogoRecog

# Total percentage of European football club sponsor logos recognized per participant (PercentSponLogoRecog)

eufs$PercentSponLogoRecog <- round(100*eufs$SponLogoRecog/18, digit =1)
eufs$PercentSponLogoRecog


# Q1. What is the correlation between hours of frequency of the game being played and the European football watched?

f1 <- function(x) c(n=round(length(x[x!=0]), digits=0),
                    mean=round(mean(x[x!=0]), digits=1),
                    sd=round(sd(x[x!=0]), digits=1))

q1 = eufs[eufs$fifa!="No Response",]
q1$fifa= factor(q1$fifa, levels=c("Never", "Rarely or Monthly", "Weekly or Daily"))
summary(q1$hours)
summary(q1$fifa)
aggregate(q1$ClubLogoRecog, by=list(q1$fifa, q1$hours), f1 )

a1 <- lm(ClubLogoRecog~fifa*hours, data = q1)
anova(a1)


#Visualizing the interactions of hours and fifa

#on total clubs recognized

install.packages("emmeans")
library(emmeans)
install.packages('ggplot2')
library(ggplot2)


emmip(a1, fifa~hours) +
  scale_color_discrete(name = "FIFA Video Game Usage")+
  xlab("Hours Spent Watching European Football Weekly")+
  ylab("Total Club Logos Recognized") +
  ggtitle("Club Logo Recognition by EA Sports FIFA Video Game Usage and Viewership")+
  theme(legend.position="top",
        plot.title=element_text(size=16),
        axis.title = element_text(size=12))

# Q2. What percentage of users who correctly recognized the club logo recognized the club sponsors logo.


q2 = eufs

# Remove the ending "S" from each column name
colnames(q2) <- sub("S$", "", colnames(q2))

club=sapply(q2[,17:52],sum)
sponsor=sapply(q2[,53:70],sum)

ClubRecog <- data.frame(
  Column_Name = names(club),
  club_Sum = club
)

SponRecog <- data.frame(
  Column_Name = names(sponsor),
  sponsor_Sum = sponsor
)

ClubRecog
SponRecog

# Merge the data frames based on the 'ID' column
merged_df <- merge(ClubRecog, SponRecog, by = "Column_Name", all = TRUE)

# Print the merged data frame
print(merged_df)
merged_df = na.omit(merged_df)

merged_df$perc = round(merged_df$sponsor_Sum/merged_df$club_Sum*100,digits=1)

merged_df <- merged_df[order(-merged_df$perc),]

#Q3 Is the successful identification of club logos and sponsor logos tied to age, gender, and hours of football watched?

# means of European football club logos recognized by gender

f1 <- function(x) c(n=length(x),
                    mean=round(mean(x), digits=1),
                    sd=round(sd(x), digits=1))
summary(eufs$gender)
binary = eufs[eufs$gender == "Male"|eufs$gender == "Female",]
aggregate(binary$ClubLogoRecog, by=list(binary$gender),f1)


# mean number of European football club logos recognized using the grouping variable of gender

binary$gender <- factor(binary$gender, levels=c("Female", "Male"))
t.test(binary$ClubLogoRecog~binary$gender)
boxplot(binary$ClubLogoRecog~binary$gender,
        main="Average Number of Club Logos Recognized by Gender",
        xlab="Gender",
        ylab="Average Number of Club Logos Recognized",
        col=3:4)   

# mean percentages of European football club sponsor logos recognized by age

f1 <- function(x) c(n=length(x),
                    mean=round(mean(x), digits=1),
                    sd=round(sd(x), digits=1))
summary(eufs$age)
ages = eufs[eufs$age != "No Response",]
aggregate(ages$SponLogoRecog, by=list(ages$age),f1)


# the mean percentages of European football club sponsor logos recognized using the grouping variable of age

ages$age <- factor(ages$age, levels=c("18-21","22-25","26-29","30-41","42+"))

a1 <- aov(SponLogoRecog~age, data = ages)
anova(a1)

emmeans(a1, pairwise~age, adjust="tukey")

boxplot(ages$SponLogoRecog~ages$age,
        main="Percentage of Club Sponsor Logos Recognized by Age",
        xlab="Age",
        ylab="Percentage of Club Sponsor Logos Recognized ",
        col=c(3:7))   



# Q4 how effective marketing translates to logo recognition

q8  = eufs
q8 = q8[q8$marketing != "No Response" ,]
aggregate(q8$ClubLogoRecog, by=list(q8$marketing), f1)

a8 <- aov(ClubLogoRecog~marketing, data = q8)
anova(a8)

emmeans(a8, pairwise~marketing, adjust="tukey")

p4 <- ggplot(data=q8, aes(x=marketing, y=ClubLogoRecog, fill=marketing))

p4 + geom_boxplot(color="black", outlier.shape=NA, size=0.7) +
  scale_fill_manual(values=c("lavender", "lemonchiffon", "azure", "lavenderblush"))+
  ylab("Number of Club logos recognised")+
  ggtitle("Number of Club logos recognised by marketing effectivenss") +
  theme(plot.title = element_text(hjust=0.5, size=15, color="gray49"),
        axis.title.y=element_blank(),
        axis.title.x=element_blank(),
        axis.text.x=element_text(size=11, color="darkblue"),
        axis.text.y=element_text(size=11, color="darkblue"),
        legend.title=element_blank(),
        panel.grid.major=element_line(color="azure3"),
        panel.grid.minor=element_blank())

# Q5. How the years of watching European football affect the sponsor logo recognition?
  

q8  = eufs
q8 = q8[q8$years != "No Response" ,]
summary(q8$years)
aggregate(q8$SponLogoRecog, by=list(q8$years), f1)

a9 <- aov(SponLogoRecog~years, data = q8)
anova(a9)

emmeans(a9, pairwise~years, adjust="tukey")

p4 <- ggplot(data=q8, aes(x=years, y=SponLogoRecog, fill=years))

p4 + geom_boxplot(color="black", outlier.shape=NA, size=0.7) +
  scale_fill_manual(values=c("lavender", "lemonchiffon", "azure", "lavenderblush","cyan"))+
  ylab("Number of sponsosr logos recognised")+
  ggtitle("Number of sponsor logos recognised by years of watching european football") +
  theme(plot.title = element_text(hjust=0.5, size=15, color="gray49"),
        axis.title.y=element_blank(),
        axis.title.x=element_blank(),
        axis.text.x=element_text(size=11, color="darkblue"),
        axis.text.y=element_text(size=11, color="darkblue"),
        legend.title=element_blank(),
        panel.grid.major=element_line(color="azure3"),
        panel.grid.minor=element_blank())

#Q6  Does the number of european leagues watched get affected by the frequency of the fifa game played?

q10 =eufs
# converting marketing variable into numeric variable.
q10 = q10[q10$fifa != "No Response",]
# Convert factor levels to numeric values (0 for "disagree" and 1 for "agree")
q10$ucl <- as.numeric(q10$ucl == "Yes")
q10$uel <- as.numeric(q10$uel == "Yes")
q10$epl <- as.numeric(q10$epl == "Yes")
q10$LaLiga <- as.numeric(q10$LaLiga == "Yes")
q10$Bundesliga <- as.numeric(q10$Bundesliga == "Yes")
q10$SerieA <- as.numeric(q10$SerieA == "Yes")
q10$Ligue1 <- as.numeric(q10$Ligue1 == "Yes")

q10$LeaugesWatched <- apply(q10[,0:6], MARGIN=1, FUN=sum)
q10$LeaugesWatched


q10 = q10[q10$fifa != "No Response" ,]
q10$fifa <- factor(q10$fifa, levels=c("Never","Rarely or Monthly", "Weekly or Daily"))
summary(q10$fifa)
aggregate(q10$LeaugesWatched, by=list(q10$fifa), f1)

a10 <- aov(LeaugesWatched~fifa, data = q10)
anova(a10)

emmeans(a10, pairwise~fifa, adjust="tukey")

p4 <- ggplot(data=q10, aes(x=fifa, y=LeaugesWatched, fill=fifa))

p4 + geom_boxplot(color="black", outlier.shape=NA, size=0.7) +
  scale_fill_manual(values=c("lavender", "lemonchiffon", "azure", "lavenderblush","cyan"))+
  ylab("Number of European leagues watched")+
  ggtitle("Number of European leagues watched by frequency of fifa played") +
  theme(plot.title = element_text(hjust=0.5, size=15, color="gray49"),
        axis.title.y=element_blank(),
        axis.title.x=element_blank(),
        axis.text.x=element_text(size=11, color="darkblue"),
        axis.text.y=element_text(size=11, color="darkblue"),
        legend.title=element_blank(),
        panel.grid.major=element_line(color="azure3"),
        panel.grid.minor=element_blank())


#Q7  Does the number of European leagues watched get affected by Age of users

q10 =eufs
# converting marketing varaible into numeric variable.
q10 = q10[q10$age != "No Response",]
# Convert factor levels to numeric values (0 for "disagree" and 1 for "agree")

# Convert factor levels to numeric values (0 for "disagree" and 1 for "agree")
q10$ucl <- as.numeric(q10$ucl == "Yes")
q10$uel <- as.numeric(q10$uel == "Yes")
q10$epl <- as.numeric(q10$epl == "Yes")
q10$LaLiga <- as.numeric(q10$LaLiga == "Yes")
q10$Bundesliga <- as.numeric(q10$Bundesliga == "Yes")
q10$SerieA <- as.numeric(q10$SerieA == "Yes")
q10$Ligue1 <- as.numeric(q10$Ligue1 == "Yes")
q10$LeaugesWatched <- apply(q10[,0:6], MARGIN=1, FUN=sum)
q10$LeaugesWatched


q10$age <- factor(q10$age, levels=c("18-21","22-25","26-29","30-41","42+"))

a11 <- aov(LeaugesWatched~age, data = q10)
anova(a11)

emmeans(a11, pairwise~age, adjust="tukey")

aggregate(q10$LeaugesWatched, by=list(q10$age), f1)

a10 <- aov(LeaugesWatched~fifa, data = q10)
anova(a10)

emmeans(a10, pairwise~fifa, adjust="tukey")

p4 <- ggplot(data=q10, aes(x=age, y=LeaugesWatched, fill=age))

p4 + geom_boxplot(color="black", outlier.shape=NA, size=0.7) +
  scale_fill_manual(values=c("lavender", "lemonchiffon", "azure", "lavenderblush","cyan"))+
  ylab("Number of European leagues watched")+
  ggtitle("Number of European leagues watched by age") +
  theme(plot.title = element_text(hjust=0.5, size=15, color="gray49"),
        axis.title.y=element_blank(),
        axis.title.x=element_blank(),
        axis.text.x=element_text(size=11, color="darkblue"),
        axis.text.y=element_text(size=11, color="darkblue"),
        legend.title=element_blank(),
        panel.grid.major=element_line(color="azure3"),
        panel.grid.minor=element_blank())


# Q8 Does the number of European leagues watched get affected by Gender of users?

q10 = q10[q10$gender != "No Response" & q10$gender != "Other Not Listed",]
q10$gender <- factor(q10$gender, levels=c("Female","Male"))
summary(q10$gender)

t.test(q10$LeaugesWatched~q10$gender)

result =aggregate(q10$LeaugesWatched, by=list(q10$gender), f1 )
result


p4 <- ggplot(data=q10, aes(x=gender, y=LeaugesWatched, fill=gender))

p4 + geom_boxplot(color="black", outlier.shape=NA, size=0.7) +
  scale_fill_manual(values=c("lavender", "lemonchiffon", "azure", "lavenderblush","cyan"))+
  ylab("Number of European leagues watched")+
  ggtitle("Number of European leagues watched by gender") +
  theme(plot.title = element_text(hjust=0.5, size=15, color="gray49"),
        axis.title.y=element_blank(),
        axis.title.x=element_blank(),
        axis.text.x=element_text(size=11, color="darkblue"),
        axis.text.y=element_text(size=11, color="darkblue"),
        legend.title=element_blank(),
        panel.grid.major=element_line(color="azure3"),
        panel.grid.minor=element_blank())

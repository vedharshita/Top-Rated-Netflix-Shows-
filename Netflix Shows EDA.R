#####################################################################################

library(rmarkdown)
library(ggplot2)
library(tidyverse)
library(dplyr)

#Reading the Dataset
NetflixData <- read_csv("C:\\Users\\harsh\\Downloads\\NetflixData_mod.csv")

#Filtering data with NA entries and nulls in ratingDescription, release year and user rating score
NetflixData <-  filter(NetflixData, `user rating score` != "")

#Tidying data by gather
NetflixData <- NetflixData%>%gather(genre,j,-title,-rating,-ratingDescription,-ratingLevel,-`release year`,-`user rating score`,-`user rating size`)%>%
  filter(j==1)%>%select(-j)
NetflixData$genre <- ifelse(NetflixData$genre %in% c("parents","parental","mature"), "mature content", NetflixData$genre)

#Tidying data and removing `user rating size` column as it does'nt has much useful information
drops = c("ratingDescription")
NetflixData = NetflixData[,!(names(NetflixData) %in% drops)] 

#Tidying data by dropping duplicate entries in df
NetflixData <- distinct(NetflixData)

#representing as a Tibble to print 10 rows using head
head(NetflixData, n =10)


NetflixData$`release year` <-as.factor(NetflixData$`release year`)
levels(NetflixData$`release year`)

#Graph saying how many series came out on netflix in year
CountGraph <- ggplot(data=NetflixData) + 
  geom_bar(aes(x=`release year`), color='blue', width = 0.5, fill="green") +
  theme(axis.text.x = element_text(angle=65, vjust=0.6)) + 
  xlab("Year") + ylab("Count") + ggtitle("Shows released per year on Netflix")

CountGraph
##########################

NetflixData$`release year` <- as.numeric(NetflixData$`release year`)

ggplot(data=NetflixData,aes(x=`release year`))+
  geom_histogram(color='black',aes(fill=genre))

###############################
#Pie chart
pie_df <- as.data.frame(table(NetflixData$genre))
colnames(pie_df) <- c("Genre", "Freq")
pie_df$ratio <- pie_df$Freq/sum(pie_df$Freq)

bp<- ggplot(pie_df, aes(x="", y=Genre, fill = Genre))+
  geom_bar(stat = "identity") +labs(x = NULL, y = NULL)
pie <- bp + coord_polar("y", start=0) + labs(x = NULL, y = NULL) + ggtitle("Shows count based on Genre")
pie = pie + theme_classic() + theme(axis.line = element_blank(),
                                    axis.text = element_blank(),
                                    axis.ticks = element_blank(),
                                    plot.title = element_text(hjust = 0.5, color = "#666666"))
pie

##########################
top10_df <- top_n(NetflixData, n=10, `user rating score`)
unique(top10_df$title)
Top10 <- ggplot(data=top10_df,aes(x=`title`, y = `user rating score`)) + 
  geom_bar(color='pink', width = 0.5, fill="red", stat = "identity") +
  theme(axis.text.x = element_text(angle=65, vjust=0.6))
Top10



#From Raw to graph 
rm(list=ls())
data_14 = readRDS("~/Google Drive/CRPE/Cleveland/clean_choice_14.Rda")
data_15 = readRDS("~/Google Drive/CRPE/Cleveland/clean_choice_15.Rda")
data_16 = readRDS("~/Google Drive/CRPE/Cleveland/clean_choice_16.Rda")


#By quality Rating 
quality_14 = data_14 %>%
  group_by(quality_ratings) %>%
  summarise(total.count=n()) %>%
  mutate(perc = paste0(round(100 * total.count/sum(total.count), 0), "%"),
         percent = total.count/sum(total.count))

quality_15 = data_15 %>%
  group_by(quality_ratings) %>%
  summarise(total.count=n()) %>%
  mutate(perc = paste0(round(100 * total.count/sum(total.count), 0), "%"),
         percent = total.count/sum(total.count))

#2016 did not have Performance rating, it was added.
quality_16 = data_16 %>%
  group_by(Rating) %>%
  summarise(total.count=n()) %>%
  mutate(perc = paste0(round(100 * total.count/sum(total.count), 0), "%"),
         percent = total.count/sum(total.count))

names(quality_16) <- c("quality_ratings","total.count","perc","percent")

quality_14$year = 2014
quality_15$year = 2015
quality_16$year = 2016

quality = data.frame(rbind(quality_14, quality_15, quality_16))

###################################################################
#From Aggregated data
#quality <- readRDS("~/Google Drive/CRPE/Cleveland/quality_final.Rda")

quality = subset(quality, quality_ratings != "error" & quality_ratings != "Closed" & quality_ratings != "#N/A")

quality$quality_ratings1 <- factor(quality$quality_ratings, 
                                   c("Failing", "Low Performing", "Mid Performing", 
                                     "High Performing", "New School Design"))
names(quality)
p <- ggplot(quality, aes(quality_ratings1, percent, fill = factor(year))) +   
  geom_bar(position = "dodge", stat="identity") + 
  geom_text(aes(label=perc),position = position_dodge(.9),vjust=-.5, size=3)
p <- p + xlab("Quality Index") + ylab("Proportion") + 
  ggtitle("School Choice Participation by Rating") +
  theme_bw() + scale_fill_manual(values=c('#e0f3db','#a8ddb5','#43a2ca'),labels = c("March 2014","March 2015","March 2016"))
p <- p + guides(fill=guide_legend(title=NULL))
p
ggsave("quality_16.png",width = 8, height =5)
####
#Line 
#get rid of "new School Design" 
quality <- filter(quality, quality_ratings1 != "New School Design")
quality$quality_ratings1 <- factor(quality$quality_ratings, 
                                   c("High Performing","Low Performing","Failing","Mid Performing"))
p2 <- ggplot(data = quality, aes(x = factor(year), y = percent, group=quality_ratings1)) +       
  geom_line(aes(color=quality_ratings1)) + geom_point(aes(color=quality_ratings1),size=2) + 
  scale_y_continuous(labels = scales::percent) +
  scale_color_manual(values=c('#0075B2','#F6A01A','#a50f15','#9ABB50')) + theme_bw() +
  theme(legend.position="none")
p2 <- p2 + annotate("text", x = 1.9, y = .363,label = "High Performing", color='#0075B2') +
  annotate("text", x = 1.9, y = .237,label = "Failing", color='#a50f15') +
  annotate("text", x = 1.9, y = .167,label = "Low Performing", color='#F6A01A') +
  annotate("text", x = 1.9, y = .06,label = "Mid Performing", color='#9ABB50')
p2 <- p2 + scale_x_discrete(expand = c(0.05,0.05)) + theme(panel.border = element_blank(),
                                                           panel.grid.minor.y=element_blank(),
                                                           panel.grid.major.y=element_line(color='grey',linetype = 'dashed'),
                                                           panel.grid.major.x = element_blank())

p2 <- p2 + xlab("Enrollment Period") 
p2 <- p2 + ylab("Total Percent") 
p2

ggsave("quality_16ALT.png",width = 10, height =5)
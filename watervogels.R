library(ggplot2)
waarnemingen <- read.csv('/home/gehau/git/watervogels/data/occurrence.csv', sep = ';')
summary(waarnemingen)

#eerste 1000 lijnen, dat gaat sneller
waarnemingen <- head(waarnemingen,10000) 
#View(waarnemingen)
ggplot(waarnemingen, aes(x=eventDate, y=individualCount)) +
  geom_point(shape=1) +    # Use hollow circles
  geom_smooth(method=lm) 

ggplot(data=waarnemingen, aes(x=eventDate, y=individualCount, group=vernacularName)) +
  geom_line()+
  geom_point()

ggplot(data=waarnemingen, aes(x=eventDate, y=individualCount, group=vernacularName, fill=vernacularName, color=vernacularName)) + stat_summary(fun.y = sum, na.rm=TRUE, geom='line')

ggplot(subset(waarnemingen, individualCount %in% c("MA", "TX")),
       aes(x=Date,
           y=Home.Value,
           color=State))+
  geom_point()
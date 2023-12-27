
library(ggplot2)
library(gganimate)
library(ggforce)
library(gapminder)
library(ggpubr)
library(lubridate)

setwd("C:/Users/SADIK/Downloads")
############################################################################################

literacy <- read.csv("literacy-rate-adults.csv")

head(literacy)
colnames(literacy) = c("Entity", "Code", "Year", "Literacy_Rate")


lit_filt <- literacy[literacy$Entity %in% c("Turkey", "World", "Europe and Central Asia (WB)"), ]


p <- ggplot(data = lit_filt, aes(x = Year, y = Literacy_Rate)) +
  geom_line(aes(color = Entity), linewidth = 1.4) + 
  geom_point(aes(color = Entity, shape = Entity), size = 4) +
  theme_bw() +
  theme(text = element_text(family = "serif", face = "bold",size=16), legend.position = "top")

p

anim <- p + transition_reveal(Year) + enter_fade()

anim_save("animated_plot.gif", anim, renderer = gifski_renderer())

##########################################################################################



gap <- gapminder
summary(gapminder)


gap_tr <- gapminder[gapminder$country == "Turkey", ]


p2 <- ggplot(data = gapminder, aes(x = gdpPercap, y = lifeExp))+ 
  geom_point(aes(colour = continent, size = pop),alpha = 0.4)


p2 <- p2 + scale_x_continuous(trans='log10')+ 
  theme_bw() + xlab("GDP Per Capita ") + ylab("Life Expectancy (years)") + 
  theme(text = element_text(family = "serif", face = "bold",size=16))

p2 <- p2 + geom_point(data = gap_tr,  color = "black")

p2 + facet_zoom(x = country == "Turkey")

p2 + facet_wrap(~continent) + geom_smooth()

p2

#######################################################################################

movies <- read.csv("movies_3.csv")  
summary(movies)

movies$Release_Date <- as.Date(movies$Release_Date)
movies$Year <- year(movies$Release_Date)

movies_filt <- movies[movies$Genre %in% c("Action", "Comedy", "Family", "Thriller", "Horror", "Drama"), ]

p3 <- ggplot(data = movies_filt, aes(x = Genre, y = Rating)) + 
  geom_boxplot(size = 1.4) + geom_jitter(aes(color = Budget, size = Gross), alpha = 0.6) +
  theme_bw() +
  theme(text = element_text(family = "serif", face = "bold",size=16))
p3

##########################################################################################


pisa <- read.csv("OECD_PISA_data.csv") 
pisa_tr <- pisa[pisa$LOCATION =="TUR", ]

summary(pisa)

p4 <- ggplot(data = pisa[pisa$LOCATION %in% c("TUR"), ], aes(x = INDICATOR, y = Value, fill= SUBJECT)) + 
  geom_col(position=position_dodge(), color = "white",  width=0.9, linewidth = 1) + facet_wrap(~TIME) +
  theme_bw() +
  theme(text = element_text(family = "serif", face = "bold",size=16)) + scale_fill_grey()

p4  

###########################################################################################

men <- read.csv("mens-life-expectancy-at-birth.csv") 
women <- read.csv("womens-life-expectancy-at-birth.csv") 


men$Gender <- "M"
women$Gender <- "F"

colnames(men) <- c("Entity", "Code", "Year", "LifeExp", "Gender")
colnames(women) <- c("Entity", "Code", "Year", "LifeExp", "Gender")
total <-  rbind(men, women)

p5_0 <- ggplot(data = total[total$Entity == "Turkey",], aes(x = Year, y = LifeExp)) + 
  geom_line(aes(color = Gender), linewidth = 1.4)

p5_0

p5 <- ggplot(data = total[total$Entity == "Turkey",]) + 
  geom_histogram(aes(LifeExp, fill = Gender), binwidth = 1, color = "white", linewidth = 1)

p5


p5_1 <- ggplot(data = total[total$Year > 2015,], aes(x = LifeExp) ) + 
  geom_histogram(aes( fill = Gender), binwidth = 1, color = "white", linewidth = 0.5) + facet_wrap(~Year) 

p5_1


p5_2 <- ggplot(data = total[total$Year > 2015,], aes(x = LifeExp) ) + 
  geom_density(aes(fill = Gender), alpha = 0.4) + facet_wrap(~Year) +
  theme_dark() +
  theme(text = element_text(family = "serif", face = "bold",size=16))

p5_2

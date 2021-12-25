library(tidytuesdayR)
library(tidyverse)
library(tidytext)
library(png)
library(cowplot)
library(ggtext)
tuesdata <- tidytuesdayR::tt_load('2021-12-21')
tuesdata$starbucks->starbucks
glimpse(starbucks)


starbucks%>%
  select(product_name,size,caffeine_mg)%>%
  filter(str_detect(product_name,"Iced|Cold"))%>%
  distinct()%>%
  group_by(product_name)%>%
  arrange(desc(caffeine_mg))->sbb
  
sbb1%>%
  data.frame()%>%
  drop_na()%>%
  filter(caffeine_mg!=0)->sbb1
  
 sbb%>%
  group_by(product_name,size)%>%
  arrange(product_name,desc(caffeine_mg))%>%
  data.frame()->sbb1

sbb1[-c(14,21,22,24,37,39,41),]->sbb1


sbb1%>%
  mutate(product_name = as.factor(product_name),
         size = reorder_within(size, caffeine_mg,product_name))->sbb2


ggplot(sbb2, aes(size,caffeine_mg, label=paste(caffeine_mg,"mg")))+
  geom_col(fill="#0b421a", colour="#eac784", width = 1, show.legend = FALSE)+
  geom_text(size=3,hjust=1.5, colour="#eac784",fontface="bold")+
  facet_wrap(~product_name, scales = "free",labeller = label_wrap_gen()) +
  coord_flip() +
  scale_x_reordered()+
  theme(axis.title = element_blank(),
        axis.ticks = element_blank(),
        axis.text.y = element_text(colour="white",face = "bold"),
        axis.text.x = element_blank(),
        axis.line=element_blank(),
        panel.background = element_rect(fill="black"),
        plot.background=element_rect(fill="black"),
        panel.grid = element_blank(),
        plot.margin = unit(c(1,1.5,1,1.5),"cm"),
        strip.background = element_rect(fill="gray10"),
        strip.text = element_text(colour="white",size=10,face="bold",margin = margin(t=5,b=5)),
        legend.position = "none",
        plot.title.position = "plot",
        plot.caption.position = "plot",
        plot.title = element_text(size=18,face="bold",color="#eac784",margin=margin(b=15)),
        plot.subtitle = element_text(size=14, colour="#eac784",margin=margin(b=21)),
        plot.caption = element_text(size=12, colour="#eac784",hjust=0,margin=margin(t=30)))+  
  theme(panel.spacing = unit(3, "lines"))+
  labs(title="HOW MUCH CAFFEINE CAN YOUR STARTBUCKS COLD DRINK CONTAIN?",
       subtitle = str_wrap("The below visualization shows the maximum caffeine content that the various cold/iced drinks contain, based on the drink sizes",140),
       caption="Data: PythonCoderUnicorn via Tidy Tuesday| Design and Analysis: @annapurani93")->plot

readPNG("C:/Users/Annapurani/Desktop/starbucks logo.png")->logo

ggdraw(plot) +
  draw_image(logo, x = 0.4, y =-0.4, scale = .12)->plot1

library(cowplot)
ggsave("Starbucks.png",plot1,width=16,height=11)







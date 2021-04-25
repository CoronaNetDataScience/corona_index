# author contribution heat map

require(googlesheets4)
require(ggplot2)
require(ggthemes)
require(tidyverse)

contrib <- read_sheet("https://docs.google.com/spreadsheets/d/1VO7E9MMalwqe9acS3NXtJAlxDfaVrzE7eTg13PpGU3Y/edit#gid=0") %>% 
  mutate(author=factor(author,levels=rev(author)))

contrib %>% 
  gather(key="type",value="mark",-author) %>% 
  ggplot(aes(y=author,
             x=type)) +
  geom_tile(aes(fill=factor(mark)),alpha=.7) +
  scale_fill_manual(values=c("white",'black')) +
  scale_x_discrete(position="top") +
  labs(x="",y="",caption="Roles taken from the CRediT Contributor Roles Taxonomy (https://casrai.org/credit/).") +
  theme_tufte() +
  guides(fill="none") +
  theme(axis.text.x = element_text(angle=270),
        axis.ticks = element_blank())

ggsave("plots/contrib_plot.png")


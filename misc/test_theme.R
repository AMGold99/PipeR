

theme_piper <- function() {
  
  #ggplot2::scale_color_brewer(palette = palette)+
  
  ggplot2::theme(
    panel.background = element_blank(),
    panel.border = element_rect(fill = NA, colour = "black"),
    plot.margin = margin(t = 12, r = 12, b = 12, l = 12, unit = "pt"),
    legend.position = "right",
    strip.background = element_rect(fill = "#E4A01B", color = "#E4A01B")
  )
  
}



time_series <- piperr::collate_csv(directory = "~/piper analysis/survey/time series/research", 
                                   group = "Ethnicity")


ggplot(data = time_series, aes(year, Amount, colour = Ethnicity))+
  
  geom_hline(yintercept = c(0, .25, .5, .75, 1), 
             colour = "grey70")+
  
  geom_line()+
  
  geom_point(size = 2)+
  
  scale_x_continuous(
    breaks = unique(time_series$year), 
    expand = c(0,0.25))+
  
  scale_y_continuous(
    limits = c(0,1),
    expand = c(0.05,0)) + 
  
  facet_wrap( ~ Value, nrow = 1) +
  
  labs(title = "Title of Plot",
       subtitle = "subtitle of plot (n = 999)") +
  
  ylab("Participation Rate") +
  
  xlab("Year") +
  
  scale_color_brewer(palette = "Dark2")+
  
  theme_piper()

ggsave("ex_plot.png", width = 2400, height = 1500, units = "px")


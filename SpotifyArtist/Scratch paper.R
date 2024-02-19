# Top 15 popular songs
output$popplot<-renderPlot({
  likedSongs %>% 
    group_by(track_name) %>% 
    filter(track.popularity > 80) %>% 
    ggplot(aes(x = track_name, y= track.popularity, color= track_name)) +
    geom_segment(aes(x= track_name, xend= track_name, y= 0, yend= track.popularity )) +
    geom_point(size= 2, color= "cyan3") +
    scale_y_continuous(breaks= seq(0, 100, 10)) +
    scale_color_viridis(discrete = TRUE, guide = FALSE, option="C") +
    theme_light(base_size=12, base_family="HiraKakuProN-W3") +
    theme (
      panel.grid.major.x = element_blank(),
      panel.border = element_blank(),
      axis.ticks.x = element_blank()
    ) +
    labs(title = "Top 15 popular songs") +
    xlab("") +
    ylab("Popularity") +
    coord_flip()
})


# 
# output$energy_vs_positivity_plot_output <- renderPlot({
#   # PLOT EMOTIONAL QUADRANT TOP FOUR ARTISTS
#   ggplot(artist, aes(x = positivity, y = energy, color = artist_name)) +
#     geom_jitter() +
#     geom_vline(xintercept = 0.5) +
#     geom_hline(yintercept = 0.5) +
#     scale_x_continuous(limits = c(0, 1)) +
#     scale_y_continuous(limits = c(0, 1)) +
#     annotate('text', 0.25 / 2, 1, label = "Aggressive") +
#     annotate('text', 1.75 / 2, 1, label = "Joyful") +
#     annotate('text', 1.75 / 2, 0, label = "Chill") +
#     annotate('text', 0.25 / 2, 0, label = "Sad") +
#     labs(x= "Positivity", y= "Energy") +
#     ggtitle("Emotional quadrant for Artists")
# })
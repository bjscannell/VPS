
df_ave <- leo %>% mutate(day_hour = format(timestamp, "%Y-%m-%d %H")) %>% 
  group_by(day_hour) %>% 
  summarise(lat = mean(location.lat),
            lon = mean(location.long))
  
  
datetime_sequence <- data.frame(x = format(seq(from = min(leo$timestamp), to = max(leo$timestamp), by = "hour"), "%Y-%m-%d %H"))
  
  
df_all = left_join(datetime_sequence,df_ave, by = c("x" = "day_hour")) %>% 
  mutate(day_hour =  as.POSIXct(x, format = "%Y-%m-%d %H"),
         lat_lon = paste(round(lat, 4), round(lon,4), sep = "-")) %>% 
  distinct(lat_lon, .keep_all = T) %>% ungroup()
  
  


p <- ggplot() +
  geom_point(data = df_all, 
             aes(x=lon,y=lat),
             alpha = 0.7, shape=21, size = 2) +
  transition_reveal(day_hour) +
  ease_aes('linear')  +
  labs(title = 'Values at {(frame_along)}')
  
  
  
  
# Animate the plot
anim <- animate(p, nframes = nrow(df_all), fps = 10)

# Display the animation
anim_save("animation.gif", anim)





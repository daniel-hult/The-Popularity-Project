
rm(list = ls())
cat('\14')

library(huupta)
huupta.libraries()

library(gtExtras)
library(scales)
library(rtweet)
# library(RSelenium)

#================== LOADER PHASE! ==============================================
#------------------ CONNECTING TO SPOTIFY & TWITTER API -----------------------------------

spotify.auth()
twitter.auth()

#------------------ GLOBAL VARIABLES -----------------------------------

# ALL GENRES .CSV
all_genres <- read.csv2("C:/Users/dthul/OneDrive/Documents/R Projects/The Popularity Project/Other Files/all_spotify_genres.csv", sep = ',')

# DAILY GENRE DISCARD PILE
daily_genre <- read.csv2("C:/Users/dthul/OneDrive/Documents/R Projects/The Popularity Project/Other Files/daily_genre.csv", sep = ',')

# MOST POPULAR ARTIST .CSV - UPDATES DAILY
most_pop_artist <- read.csv2("C:/Users/dthul/OneDrive/Documents/R Projects/The Popularity Project/Other Files/most_popular_artists.csv", sep = ',')

# FONT
font <- 'Montserrat'

#================== RETRIEVE DATA ==============================================
#------------------ FIND MPA (MOST POPULAR ARTIST - ARTIST W/ 100 POPULARITY) -----------------------------------

x = 0
while (x == 0) {
  
  print("Searching for the artist in these genres...")
  
  for (i in 1:length(all_genres$Genre)) {
    print(all_genres$Genre[i])
    search_genre <- get_genre_artists(genre = all_genres$Genre[i], limit = 10)
    search_genre <- search_genre[,c('id','name','popularity')] %>% 
      arrange(-popularity)
    
    if (search_genre$popularity[1] == 100) {
      top_artist <- search_genre[1,]
      x = 1
      break
    }
  }
}

paste0("Found them! - It's ",top_artist$name)

#------------------ SELECT GENRE FOR TOP 5 ARTISTS -----------------------------------

genre_pool <- all_genres$Genre[!(all_genres$Genre %in% daily_genre$Genre)]
genre_pool <- genre_pool[nchar(genre_pool) < 23]

y = 0
while(y == 0) {
  
  print("Selecting a genre...")
  select_genre <- sample(genre_pool,1,replace = F)

  tryCatch(
    {
      top5_artists <- get_genre_artists(select_genre) %>% 
        select(id, name, popularity, images) %>% 
        arrange(-popularity) %>% 
        slice(1:5) %>%
        mutate(bar_chart = popularity,
               image = NA) %>% 
        select(images,image,name,popularity,bar_chart)
      
      y = 1
      break
    }, error = function(cond) {
      "Genre Not Found"
    }
  )
  
}
paste0("Genre Found: ",select_genre)

for (i in 1:length(top5_artists$name)) {
  top5_artists$image[i] <- top5_artists$images[[i]]$url[1]
}

top5_artists <- top5_artists %>% 
  select(image, name, popularity, bar_chart)

genre_for_caption <- gsub('-',' ',select_genre)

#================== GRAPHICS! TABLES & DONUTS! =================================
#------------------ MAKE POP-100 DONUT IMAGE -----------------------------------

# DONUT DATA PREP
donut_data <- make.pop.donut(top_artist$popularity)

# MAKE POP-100 DONUT GRAPH
pop_donut <- ggplot(donut_data, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=category)) +
  geom_rect(fill = c('#E12120','whitesmoke')) +
  coord_polar(theta="y") +
  xlim(c(1, 4)) +
  ggtitle('MOST POPULAR ARTIST') +
  theme_void() +
  geom_text(x = 1.4,aes(y = 0, label = top_artist$name), size = (11+(-0.44*str_length(top_artist$name))), family = font, show.legend = F, fontface = 'bold') +
  geom_text(x = 0.6,aes(y = 0, label = top_artist$popularity), size = 10, family = font, show.legend = F, fontface = 'bold') +
  geom_text(x = -2.4,aes(y = 0, label = format(Sys.Date(),format = '%B %d, %Y')), size = 6, family = font, show.legend = F, fontface = 'bold') +
  theme(plot.title = element_text(size = 20,family = font,face = 'bold',hjust = 0.5,vjust = -3))

#------------------ MAKE TABLE: TOP 5 TRACKS FOR MOST POPULAR ARTIST -----------------------------------

# GET TABLE DATAFRAME
top5_tracks <- get_artist_top_tracks(top_artist$id) %>% 
  select(id, name, popularity) %>% 
  arrange(-popularity) %>% 
  top_n(5) %>% 
  select(name, popularity) %>%
  mutate(bar_chart = popularity)

# MAKE TOP 5 TRACKS TABLE
top5_tracks_table <- top5_tracks %>% 
  gt() %>% 
  gtExtras::gt_theme_pff() %>% 
  tab_header(title = md(paste0('**','TOP 5 ',toupper(top_artist$name),' TRACKS','**')),
             subtitle = md(paste0('*','Track Popularity for ',format(Sys.Date(),format = '%B %d, %Y'),'*'))) %>% 
  tab_source_note(md('*Data Source: Spotify API*')) %>% 
  opt_table_font(font = font) %>%
  gt_plt_bar_pct(column = bar_chart, fill = '#E12120',scaled = T) %>% 
  cols_align('right', popularity) %>% 
  cols_label(name = 'TRACK NAME',
             popularity = 'POPULARITY',
             bar_chart = 'POPULARITY INDEX') %>% 
  cols_width(1 ~ px(200), 2 ~ px(80), 3 ~ px(200)) %>% 
  tab_options(heading.align = 'center', table.font.size = 16)

#------------------ MAKE TABLE: TOP 5 ARTISTS PER GENRE -----------------------------------

# MAKE TOP 5 TRACKS TABLE
top5_artists_table <- top5_artists %>% 
  gt() %>% 
  gtExtras::gt_theme_pff() %>% 
  gtExtras::gt_img_rows(image) %>% 
  tab_header(title = md(paste0('**','TOP 5: ','*',toupper(genre_for_caption),'*',' GENRE','**'))) %>% 
  tab_source_note(md(paste0('*','Data Source: Spotify API - ',format(Sys.Date(),format = '%B %d, %Y'),'*'))) %>% 
  opt_table_font(font = font) %>%
  gt_plt_bar_pct(column = bar_chart, height = 20, fill = '#E12120',scaled = T) %>% 
  cols_align('right', popularity) %>% 
  cols_label(image = '',
             name = 'ARTIST NAME',
             popularity = 'POPULARITY',
             bar_chart = 'POPULARITY INDEX') %>% 
  cols_width(1 ~ px(30), 3 ~ px(80), 4 ~ px(200)) %>% 
  tab_options(heading.align = 'center', table.font.size = 20)

#================== VIEW ALL GRAPHICS =============================

# SAVE ALL GRAPHICS
ggsave(filename = paste0('pop100_',Sys.Date(),'.png'), plot = pop_donut, device = 'png', path = "C:/Users/dthul/OneDrive/Documents/R Projects/The Popularity Project/images/MPA/",width = 1080,height = 1080,units = 'px',bg = 'white')
gtsave(top5_tracks_table, paste0("top5_tracks_",Sys.Date(),'.png'), path = 'C:/Users/dthul/OneDrive/Documents/R Projects/The Popularity Project/images/Top 5 Tracks/')
gtsave(top5_artists_table, paste0("top5_artists_",Sys.Date(),'.png'), path = 'C:/Users/dthul/OneDrive/Documents/R Projects/The Popularity Project/images/Top 5 Artists/')

# OPEN ALL GRAPHICS
shell.exec(paste0("C:/Users/dthul/OneDrive/Documents/R Projects/The Popularity Project/images/MPA/pop100_",Sys.Date(),".png"))
shell.exec(paste0('C:/Users/dthul/OneDrive/Documents/R Projects/The Popularity Project/images/Top 5 Tracks/',"top5_tracks_",Sys.Date(),'.png'))
shell.exec(paste0('C:/Users/dthul/OneDrive/Documents/R Projects/The Popularity Project/images/Top 5 Artists/',"top5_artists_",Sys.Date(),'.png'))

#================== UPDATE DATA =============================

# GET DATE & ARTIST NAME
update_data <- data.frame(date = Sys.Date(),artist = top_artist$name)

# UPDATE ARTIST STREAK
update_data <- update_data %>% 
  mutate(streak = case_when(most_pop_artist$artist[length(most_pop_artist$artist)] == top_artist$name ~ most_pop_artist$streak[length(most_pop_artist$streak)] + 1,
                            TRUE ~ 1))

# APPEND GENRE TO THE LIST
daily_genre[nrow(daily_genre) + 1,] = select_genre

# WRITE UPDATE DATA TO .CSV
write.table(update_data, file = "C:/Users/dthul/OneDrive/Documents/R Projects/The Popularity Project/Other Files/most_popular_artists.csv",sep = ',',col.names = F,row.names = F,append = T)
write.table(daily_genre, file = "C:/Users/dthul/OneDrive/Documents/R Projects/The Popularity Project/Other Files/daily_genre.csv",sep = ',',col.names = T,row.names = F,append = F)

#================== POSTING TO TWITTER & INSTAGRAM =============================
#------------------ WRITE CAPTIONS FOR POSTS ----------------------------------

# WRITE MPA CAPTION
if (update_data$streak == 1) {
  caption <- paste(update_data$artist,'is now the most popular artist on Spotify.','Previously,',most_pop_artist$artist[length(most_pop_artist$artist)],'was the most popular artist on Spotify for',most_pop_artist$streak[length(most_pop_artist$streak)],'day(s)')
} else {
  caption <- paste(update_data$artist,'is the most popular artist on Spotify today.',update_data$artist,'has been the most popular artist on Spotify for the past',update_data$streak,'days')
}

# WRITE TOP 5 ARTISTS/GENRE CAPTION
artist_caption <- paste0(top5_artists$name[1],' is the most popular artist in the ',genre_for_caption,' genre with a Spotify Popularity Index score of ',top5_artists$popularity[1],'. What genres should we do next?')

# PRINT CAPTIONS
print(caption)
print(artist_caption)

#------------------ POST TO TWITTER ----------------------------------

# POST MPA TWEET
post_tweet(status = caption,
           media = paste0("C:/Users/dthul/OneDrive/Documents/R Projects/The Popularity Project/images/MPA/pop100_",Sys.Date(),".png"),
           media_alt_text = 'Source: Spotify API')

# GET MY TWEETS
my_tweets <- rtweet::get_timeline(user = 'popularity_100') %>% 
  arrange(rev(created_at))

Sys.sleep(2)

# POST TOP 5 TRACKS TWEET
post_tweet(status = '',
           media = paste0("C:/Users/dthul/OneDrive/Documents/R Projects/The Popularity Project/images/Top 5 Tracks/top5_tracks_",Sys.Date(),".png"),
           media_alt_text = '',
           in_reply_to_status_id = my_tweets$id_str[1])

# POST TOP 5 ARTISTS TWEET
post_tweet(status = artist_caption,
           media = paste0("C:/Users/dthul/OneDrive/Documents/R Projects/The Popularity Project/images/Top 5 Artists/top5_artists_",Sys.Date(),".png"),
           media_alt_text = '')

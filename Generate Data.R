library(spotifyr)
df = rbind(
  get_genre_artists('pop',limit=10)[,c(3,5,6,11,12)],
  get_genre_artists('wonky',limit=10)[,c(3,5,6,11,12)],
  get_genre_artists('rock',limit=10)[,c(3,5,6,11,12)],
  get_genre_artists('blues',limit=10)[,c(3,5,6,11,12)])

pop = get_artist_audio_features(as.character(df[1,2]))[,c(1,2,3,6,7,9,10,11,12,13,14,15,16,17,18,19,20,22,26,30,36,37,38,39)]
for(i in 2:5){
  pop = rbind(pop,get_artist_audio_features(as.character(df[i,2]))[,c(1,2,3,6,7,9,10,11,12,13,14,15,16,17,18,19,20,22,26,30,36,37,38,39)])
}
for(i in 6:10){
  pop = rbind(pop,get_artist_audio_features(as.character(df[i,2]))[,c(1,2,3,6,7,9,10,11,12,13,14,15,16,17,18,19,20,22,26,30,36,37,38,39)])
}

wonky = get_artist_audio_features(as.character(df[11,2]))[,c(1,2,3,6,7,9,10,11,12,13,14,15,16,17,18,19,20,22,26,30,36,37,38,39)]
for(i in 12:15){
  wonky = rbind(wonky,get_artist_audio_features(as.character(df[i,2]))[,c(1,2,3,6,7,9,10,11,12,13,14,15,16,17,18,19,20,22,26,30,36,37,38,39)])
}
for(i in 16:20){
  wonky = rbind(wonky,get_artist_audio_features(as.character(df[i,2]))[,c(1,2,3,6,7,9,10,11,12,13,14,15,16,17,18,19,20,22,26,30,36,37,38,39)])
}

rock = get_artist_audio_features(as.character(df[21,2]))[,c(1,2,3,6,7,9,10,11,12,13,14,15,16,17,18,19,20,22,26,30,36,37,38,39)]
for(i in 22:25){
  rock = rbind(rock,get_artist_audio_features(as.character(df[i,2]))[,c(1,2,3,6,7,9,10,11,12,13,14,15,16,17,18,19,20,22,26,30,36,37,38,39)])
}
for(i in 26:30){
  rock = rbind(rock,get_artist_audio_features(as.character(df[i,2]))[,c(1,2,3,6,7,9,10,11,12,13,14,15,16,17,18,19,20,22,26,30,36,37,38,39)])
}

blues = get_artist_audio_features(as.character(df[31,2]))[,c(1,2,3,6,7,9,10,11,12,13,14,15,16,17,18,19,20,22,26,30,36,37,38,39)]
for(i in 32:35){
  blues = rbind(blues,get_artist_audio_features(as.character(df[i,2]))[,c(1,2,3,6,7,9,10,11,12,13,14,15,16,17,18,19,20,22,26,30,36,37,38,39)])
}
for(i in 36:40){
  blues = rbind(blues,get_artist_audio_features(as.character(df[i,2]))[,c(1,2,3,6,7,9,10,11,12,13,14,15,16,17,18,19,20,22,26,30,36,37,38,39)])
}

df = rbind(pop,wonky,rock,blues)
write.csv(df, "data.csv")
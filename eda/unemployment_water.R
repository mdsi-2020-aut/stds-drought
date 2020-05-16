load("data/unemployment_water.RData")

load("data/HPT/water_sa4.RData")

head(water_unemployment)
unique(water_sa4$territory_sa4)

split(water_unemployment, water_unemployment$territory_sa4)[2]

Adld_cnh <- split(water_unemployment, water_unemployment$territory_sa4)[2]
Adld_cnh <- data.frame()
for (i in 1:5) {
  df <- rbind(df, as.data.frame(Adld_cnh[[i]]))
}
df1_5 <- as.data.frame(df)

names(Adld_cnh)[names(Adld_cnh) == 'Adelaide...Central.and.Hills.unemployment_rate'] <- "unemployment.rate"
names(Adld_cnh)[names(Adld_cnh) == 'Adelaide...Central.and.Hills.date'] <- "date"
names(Adld_cnh)[names(Adld_cnh) == 'Adelaide...Central.and.Hills.waterlevel_mean'] <- "waterlevel.mean"

ggplot(data=Adld_cnh, aes(x=waterlevel.mean,y=unemployment.rate)) + geom_point()

cor(Adld_cnh$unemployment.rate, Adld_cnh$date)

slm <- lm(unemployment.rate~waterlevel.mean, data = Adld_cnh)

which.max(Adld_cnh$waterlevel.mean)
Adld_cnh <- Adld_cnh[-c(144),]

ggplot(data = df1_5) + 
  geom_point(mapping = aes(x = waterlevel_mean, y = unemployment_rate)) + 
  facet_wrap(~ territory_sa4, nrow=2)
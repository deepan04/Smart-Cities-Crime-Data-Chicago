glm.fit=glm(Arrest???Day, data=crimedata ,family=binomial )
colnames(crimedata)
summary (glm.fit)
glm.probs=predict(glm.fit ,type="response")
glm.pred[glm.probs >.27]="True"
table(glm.pred,crimedata$Arrest)
glm.pred=rep("TRUE",20419)
glm.pred[glm.probs <.31]="FALSE"
table(glm.pred,crimedata$Arrest)
mean(glm.pred==crimedata$Arrest)

glm.fit=glm(Arrest???Day+Hourd, data=crimedata ,family=binomial )
glm.probs=predict(glm.fit ,type="response")
max(glm.probs)
glm.pred=rep("TRUE",20419)
glm.pred[glm.probs <.31]="FALSE"
table(glm.pred,crimedata$Arrest)
mean(glm.pred==crimedata$Arrest)

train=(crimedata$Year<=2008)
crimedata.2005= crimedata[!train,]
Arrest.2005= Arrest[!train]
glm.fit=glm(Arrest???crimedata$Primary.Type+crimedata$IUCR, data=crimedata ,family=binomial,subset=train)
glm.probs=predict(glm.fit ,crimedata.2005,type="response")
max(glm.probs)
glm.pred=rep("TRUE",20419)
glm.pred[glm.probs <.5]="FALSE"
table(glm.pred,crimedata$Arrest)
mean(glm.pred==crimedata$Arrest)

crimedata=read.csv(file="D:/Masters Studies/Fall 2016/DS - 502/Project 1/Temp_R.csv", header = TRUE, sep = ",")
train=(crimedata$Year<=2008)
crimedata.2005= crimedata[!train,]
hour.2005= crimedata$Hourd[!train]

lda.fit=lda(crimedata$Hourd~Primary.Type, data=crimedata)
lda.pred=predict (lda.fit,type="response")
lda.class=lda.pred$class
table(lda.class ,crimedata$Hourd)
lda.fit
mean(lda.class==crimedata$Hourd)

crimedata=read.csv(file="D:/Masters Studies/Fall 2016/DS - 502/Project 1/Model_R.csv", header = TRUE, sep = ",")
train=(crimedata$Year<2016)
crimedata.2005= crimedata[!train,]
Arrest.2005= crimedata$Arrest[!train]
glm.fit=glm(Arrest???crimedata$Primary.Type, data=crimedata ,family=binomial,subset=train)
glm.probs=predict(glm.fit ,crimedata.2005,type="response")
glm.pred=rep("TRUE",length(glm.probs))
max(glm.probs)
glm.pred[glm.probs <.5]="FALSE"
mean(glm.pred==crimedata$Arrest)

summary (lm.fit)
lm.fit=lm(crimedata1$CrimeCount???crimedata1$IUCR)
summary (lm.fit)
lm.fit=lm(crimedata1$CrimeCount???crimedata1$Location.Description + crimedata1$District)
summary (lm.fit)
lm.fit=lm(crimedata1$CrimeCount???crimedata1$District)
summary (lm.fit)
lm.fit=lm(crimedata1$CrimeCount???crimedata1$District+crimedata1$Ward)
summary (lm.fit)
lm.fit=lm(crimedata1$CrimeCount???crimedata1$District+crimedata1$Ward+crimedata1$Community.Area)
summary (lm.fit)
lm.fit=lm(crimedata1$CrimeCount???crimedata1$District+crimedata1$Ward+crimedata1$Year)
summary (lm.fit)
lm.fit=lm(crimedata1$CrimeCount???crimedata1$District+crimedata1$Ward+crimedata1$Year+crimedata1$DayofWeekd)
summary (lm.fit)
lm.fit=lm(crimedata1$CrimeCount???crimedata1$District+crimedata1$Ward+crimedata1$Year+crimedata1$DayofWeekd+crimedata1#Hourd)
          lm.fit=lm(crimedata1$CrimeCount???crimedata1$District+crimedata1$Ward+crimedata1$Year+crimedata1$DayofWeekd+crimedata1$Hourd)
          summary (lm.fit)
          lm.fit=lm(crimedata1$CrimeCount???crimedata1$District+crimedata1$Ward+crimedata1$Year+crimedata1$DayofWeekd+crimedata1$Hourd+crimedata1$Month)
          summary (lm.fit)
          lm.fit=lm(crimedata1$CrimeCount???crimedata1$District+crimedata1$Ward+crimedata1$Year+crimedata1$DayofWeekd+crimedata1$Hourd+crimedata1$Month+crimedata1$Community.Area)
          summary (lm.fit)
          lm.fit=lm(crimedata1$CrimeCount???crimedata1$District+crimedata1$Ward+crimedata1$Year+crimedata1$DayofWeekd+crimedata1$Hourd+crimedata1$Month+crimedata1$Location.Description)
          summary (lm.fit)
          lm.fit=lm(crimedata1$CrimeCount???crimedata1$District+crimedata1$Ward+crimedata1$Year+crimedata1$DayofWeekd+crimedata1$Hourd+crimedata1$Month+crimedata1$Location.Description+crimedata1$IUCR)
          summary (lm.fit)
          crimedata1=read.csv(file="D:/Masters Studies/Fall 2016/DS - 502/Project 1/Final_R.csv", header = TRUE, sep = ",")
          
          
          
          
          
          
          
          
          
          
          
          
          crimedataset=read.csv('D:/Masters Studies/Fall 2016/DS - 502/Project 1/Rohit1.csv')
          highcrimes=ddply(crimedataset, "Hourd", function(x) head(x[order(x$CrimeCount, decreasing = TRUE) , ], 20)) noon_crimes=subset(highcrimes,Hourd=="Afternoon")
          morn_crimes=subset(highcrimes,Hourd=="Morning") 
          night_crimes=subset(highcrimes,Hourd=="Night")
          police= iconList( zone = makeIcon("D:/Masters Studies/Fall 2016/DS - 502/Project 1/police.jpg", "D:/Masters Studies/Fall 2016/DS - 502/Project 1/police@4x.jpg", 40, 40))
          police_setup=read.csv('D:/Masters Studies/Fall 2016/DS - 502/Project 1/Police_Stations.csv')
          map=leaflet(highcrimes) %>%
            + addTiles(group = "OSM (default)") %>% addProviderTiles("Stamen.Toner", group = "Toner") %>% addProviderTiles("Stamen.TonerLite", group = "Toner Lite") %>%
            + addCircles(data= morn_crimes ,~ ILongitude, ~ ILatitude,popup= ~ CrimeCount,stroke=F,group="Morning", weight=1, radius= ~ sqrt(morn_crimes$CrimeCount) * 20 ) %>%
            + addCircles(data= noon_crimes ,~ ILongitude, ~ ILatitude,popup= ~ CrimeCount,stroke=F,group="Afternoon", weight=1, color="black",radius= ~ sqrt(noon_crimes$CrimeCount) * 20 ) %>%
            + addCircles(data= night_crimes ,~ ILongitude, ~ ILatitude,popup= ~ CrimeCount,group="Night",color="red", weight=1, stroke=F,radius= ~ sqrt(night_crimes$CrimeCount) * 20 ) %>%
            + addMarkers(data= police_setup, ~ police_setup$LONGITUDE, ~police_setup$LATITUDE, popup= ~ police_setup$DISTRICT.NAME, icon= ~ police["zone"] ) %>%
            + addLayersControl(  baseGroups = c("OSM (default)", "Toner", "Toner Lite"),    overlayGroups = c("Morning","Afternoon", "Night"),  options = layersControlOptions(collapsed = FALSE))         
          
          
          
          
          
          
          

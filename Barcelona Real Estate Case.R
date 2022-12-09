library(readxl)
data <- read_excel("/Users/Lin/Downloads/BarcelonaRE_Data.xlsx",sheet = "413 properties for analysis")
colnames(data)[4]='space'
colnames(data)[8]='Atico'
colnames(data)[3]='City.Zone'

#add 9 dummies for city zones.
sa=ifelse(data$City.Zone=='Sant Andreu',1,0)
cv=ifelse(data$City.Zone=='Ciutat Vella',1,0)
ei=ifelse(data$City.Zone=='Eixample',1,0)
smo=ifelse(data$City.Zone=='Sants - Montjuïc',1,0)
gr=ifelse(data$City.Zone=='Gràcia',1,0)
ssg=ifelse(data$City.Zone=='Sarria - Sant Gervasi',1,0)
hg=ifelse(data$City.Zone=='Horta - Guinardó',1,0)
nb=ifelse(data$City.Zone=='Nou Barris',1,0)
lc=ifelse(data$City.Zone=='Les Corts',1,0)
sm=ifelse(data$City.Zone=='Sant Marti',1,0)
data=data.frame(data,sa,cv,ei,smo,gr,ssg,hg,nb,lc,sm)

#add all interaction terms
saspace=data$sa*data$space
cvspace=data$cv*data$space
eispace=data$ei*data$space
smospace=data$smo*data$space
grspace=data$gr*data$space
ssgspace=data$ssg*data$space
hgspace=data$hg*data$space
nbspace=data$nb*data$space
lcspace=data$lc*data$space
smspace=data$sm*data$space
data=data.frame(data,saspace,cvspace,eispace,smospace,grspace,ssgspace,hgspace,nbspace,lcspace,smspace)
elspace=data$space*data$Elevator
data=data.frame(data,elspace)
elbath=data$Bathrooms*data$Elevator
data=data.frame(data,elbath)
elroom=data$Rooms*data$Elevator
atroom=data$Rooms*data$Atico
atspace=data$space*data$Atico
atbath=data$Atico*data$Bathrooms
tespace=data$space*data$Terrasse
teroom=data$Rooms*data$Terrasse
tebath=data$Bathrooms*data$Terrasse
paspace=data$Parking*data$space
paroom=data$Rooms*data$Parking
parbath=data$Bathrooms*data$Parking
kispace=data$space*data$Kitchen
kiroom=data$Rooms*data$Kitchen
kibath=data$Bathrooms*data$Kitchen
tyspace=data$Type*data$space
tyroom=data$Rooms*data$Type
tybath=data$Bathrooms*data$Type
yaspace=data$Yard*data$space
yaroom=data$Rooms*data$Yard
yabath=data$Bathrooms*data$Yard
saspace=data$space*data$sa
saroom=data$Rooms*data$sa
cvroom=data$Rooms*data$cv
eiroom=data$Rooms*data$ei
smoroom=data$Rooms*data$smo
grroom=data$Rooms*data$gr
ssgroom=data$Rooms*data$ssg
hgroom=data$Rooms*data$hg
nbroom=data$Rooms*data$nb
lcroom=data$Rooms*data$lc
smroom=data$Rooms*data$sm
saba=data$Bathrooms*data$sa
cvba=data$Bathrooms*data$cv
eiba=data$Bathrooms*data$ei
smoba=data$Bathrooms*data$smo
grba=data$Bathrooms*data$gr
ssgba=data$Bathrooms*data$ssg
hgba=data$Bathrooms*data$hg
nbba=data$Bathrooms*data$nb
lcba=data$Bathrooms*data$lc
smba=data$Bathrooms*data$sm
data=data.frame(data,elroom,atroom,atspace,atbath,tespace,teroom,tebath,paspace,paroom,parbath,kispace,kiroom,kibath,tyspace,tyroom,tybath,yaspace,yaroom,yabath,saspace,saroom,cvroom,eiroom,smoroom,grroom,ssgroom,hgroom,nbroom,lcroom,smroom,saba,cvba,eiba,smoba,grba,ssgba,hgba,nbba,lcba,smba)

#regression with all variables
summary(lm(Price~space+Rooms+Bathrooms+saspace+cvspace+eispace+smospace+grspace+ssgspace+hgspace+nbspace+lcspace+smspace+elspace+elbath+elroom+atroom+atspace+atbath+tespace+teroom+tebath+paspace+paroom+parbath+kispace+kiroom+tyspace+tyroom+tybath+yaspace+yaroom+yabath+saroom+cvroom+smoroom+grroom+ssgroom+nbroom+lcroom+saba+cvba+eiba+smoba+grba+ssgba+hgba+nbba+lcba,data=data))



#Final regression model
model=lm(formula = Price ~ space + Bathrooms + saspace + cvspace + 
     eispace + smospace + ssgspace + hgspace + elspace + elbath + 
     atbath + tespace + teroom + paspace + parbath + tyroom + 
     tybath + yaspace + yaroom + yabath + cvroom + smoroom + grroom + 
     nbroom + lcroom + saba + cvba + eiba + grba + hgba + nbba, 
   data = data)
summary(model)
acf(residuals(model))


a <- read_excel("/Users/Lin/Downloads/BarcelonaRE_Data.xlsx",sheet = "200 properties to be priced")
colnames(a)[4]='space'
colnames(a)[8]='Atico'
colnames(a)[3]='City.Zone'
sa=ifelse(a$`City.Zone`=='Sant Andreu',1,0)
cv=ifelse(a$City.Zone=='Ciutat Vella',1,0)
ei=ifelse(a$City.Zone=='Eixample',1,0)
smo=ifelse(a$City.Zone=='Sants - Montjuïc',1,0)
ssg=ifelse(a$City.Zone=='Sarria - Sant Gervasi',1,0)
hg=ifelse(a$City.Zone=='Horta - Guinardó',1,0)
gr=ifelse(a$City.Zone=='Gràcia',1,0)
nb=ifelse(a$City.Zone=='Nou Barris',1,0)
lc=ifelse(a$City.Zone=='Les Corts',1,0)
a=data.frame(a,sa,cv,ei,smo,ssg,hg,gr,nb,lc)
elspace=a$space*a$Elevator
saspace=a$space*a$sa
cvspace=a$cv*a$space
eispace=a$ei*a$space
smospace=a$smo*a$space
ssgspace=a$ssg*a$space
hgspace=a$hg*a$space
elbath=a$Bathrooms*a$Elevator
atbath=a$Atico*a$Bathrooms
tespace=a$space*a$Terrasse
teroom=a$Rooms*a$Terrasse
paspace=a$Parking*a$space
parbath=a$Bathrooms*a$Parking
tyroom=a$Rooms*a$Type
tybath=a$Bathrooms*a$Type
yaspace=a$Yard*a$space
yaroom=a$Rooms*a$Yard
yabath=a$Bathrooms*a$Yard
cvroom=a$Rooms*a$cv
smoroom=a$Rooms*a$smo
grroom=a$Rooms*a$gr
nbroom=a$Rooms*a$nb
lcroom=a$Rooms*a$lc
saba=a$Bathrooms*a$sa
cvba=a$Bathrooms*a$cv
eiba=a$Bathrooms*a$ei
grba=a$Bathrooms*a$gr
hgba=a$Bathrooms*a$hg
nbba=a$Bathrooms*a$nb
grroom=a$Rooms*a$gr
nbroom=a$Rooms*a$nb
lcroom=a$Rooms*a$lc
grba=a$Bathrooms*a$gr
hgba=a$Bathrooms*a$hg
nbba=a$Bathrooms*a$nb
a=data.frame(a,saspace,cvspace,eispace,smospace,ssgspace,hgspace,elspace,elbath,atbath,tespace,teroom,paspace,parbath,tyroom,tybath,yaspace,yaroom,yabath,cvroom,smoroom,grroom,nbroom,lcroom,saba,cvba,eiba,grba,hgba,nbba)
predict(model,newdata=a)


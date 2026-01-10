ew<-0.1                  #Energy per unit of time of waiting
ep<-1                    #Energy per unit of time of pursuing the prey
v<-0.5                   #Velocity of the predator in chase
a<-0.005                 #Abundance of prey
e<-10                    #Energy value of the prey

rc<-seq(0.001,4,0.001)   #Radius of the hunting ground 

tw <- 1/(a*pi*((rc^2)/2))   #Waiting time 
tp<-4/3*rc/v                #Pursuing time
ti<-tw+tp                   #Time per item 
ei<-e-ew*tw-ep*tp           #Energy per item
et<-ei/ti                   #Energy per unit of time

et <-function()
{
  (e-ew*(1/(a*pi*((rc^2)/2)))-ep*(4/3*rc/v))/(1/(a*pi*((rc^2)/2))+4/3*rc/v)
}

plot(rc,et(),type="l",
     xlab="Radius of the territory",
     ylab="Energy per unit of time (cal/sec) ")
abline(0,0)
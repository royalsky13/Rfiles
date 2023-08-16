classic_monty <- function() {
  # Assign the prize
  prize <- sample(1:3,1)
  # Pick a door
  choice <- sample(1:3,1)
  # Monty picks a door
  monty <- sample(c(1:3)[-c(choice,prize)], 1)
  return(ifelse(prize!=choice, "Switch", "Stick"))
}

montyhall <- function ()
{
door <- sample(1:3 ,3) # random sequence of doors
prize <- sample(1:3,1) # any door can have the car
choice <- sample(1:3,1)
monty <- sample(door[-c(choice,prize)],1) #revealed door can't be the door having the prize or door chosen by me
#since I know I have 2/3 probability of winning the game if I switch, so I will always switch.
door_switch <- sample (door[c(-monty)],1) # will switch to any of the two doors except the revealed door
if (prize==door_switch)
{
  return (1)
}else
{
  return(0)
}
}
store <- numeric (length=1e3)
for (i in 1:length(store))
{
  store[i]<-montyhall()
}
print(mean(store))


chips <- function () {
cpacket <- 0 # initially zero packets
packet <- c() #empty vector of packet
while (TRUE)
{
  #pick a toy after opening packet
  toy_sample <- sample(1:7,1,prob=c(0.25,0.2,0.2,0.15,0.1,0.05,0.05),replace=TRUE)
  #concatenate two vectors
  packet <- c(packet,toy_sample)
  #unique toys in our opened packets
  upacket<- unique(packet)
  #packet counter
  cpacket <- cpacket + 1
  #criterion
  if (sum(upacket>=1 & upacket<=7)==7)
  {
    #loop exit
    break
  }
}
#display total packets required to obtain all toys
cpacket
}
# average packets required
store <- numeric (length=1e3)
for (i in 1:length(store))
{
  store[i]<-chips()
}
print(mean(store))

  encounter <- function() 
{
  days <- 0
flag <- 1
bottle <- numeric (length = 100) #100 whole tablets in bottle represented as 0's
while (TRUE) #will run for infinte times but gets terminated when a break is encountered
{
  tablet_draw <- sample(0:1, 1, prob=c((100-sum(bottle))/100, sum(bottle)/100))
  # 0 denotes whole tablet
  # 1 denotes half tablet
  if (tablet_draw==1) { days <-days+1 
  break #loop terminates
  }else
  {
    days<- days+1
    bottle[flag]=1
    flag<-flag+1
    
  }
}
return (days)
}
mean_encounter <- function ()
{
  store <- numeric (length=1e3)
  for (i in 1:length(store))
  {
    store[i]<-encounter()
  }
  return(mean(store))
}
mean_encounter()



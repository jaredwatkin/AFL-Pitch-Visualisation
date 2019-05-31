# AFL-Pitch-Visualisation
Visualising an AFL pitch in R using ggplot2

library(ggplot2)

#First want to define the colour scheme using Adobe Colour Tool
grass_colour <- "#60BC48"
line_colour <- "#FFFFFC"
background_colour <- "#60BC48"
#Will also have coloured lines to indicate goalposts*****

#Create a theme for the plot background
theme_blankPitch = function(size=12) { 
  theme(
    #axis.line=element_blank(), 
    axis.text.x=element_blank(), 
    axis.text.y=element_blank(), 
    #axis.ticks.y=element_text(size=size),
    #   axis.ticks=element_blank(),
    axis.ticks.length=unit(0, "lines"), 
    #axis.ticks.margin=unit(0, "lines"), 
    axis.title.x=element_blank(), 
    axis.title.y=element_blank(), 
    legend.background=element_rect(fill=background_colour, colour=NA), 
    legend.key=element_rect(colour=background_colour,fill=background_colour), 
    legend.key.size=unit(1.2, "lines"), 
    legend.text=element_text(size=size), 
    legend.title=element_text(size=size, face="bold",hjust=0),
    strip.background = element_rect(colour = background_colour, fill = background_colour, size = .5),
    panel.background=element_rect(fill=background_colour,colour=background_colour), 
    #       panel.border=element_blank(), 
    panel.grid.major=element_blank(), 
    panel.grid.minor=element_blank(), 
    panel.spacing=element_blank(), 
    plot.background=element_blank(), 
    plot.margin=unit(c(0, 0, 0, 0), "lines"), 
    plot.title=element_text(size=size*1.2), 
    strip.text.y=element_text(colour=background_colour,size=size,angle=270),
    strip.text.x=element_text(size=size*1))}
    
#Set out field dimensions (based on Gabba dimension)
ymin <- 0
ymax <- 15000
xmin <- 0
xmax <- 17000

#Define dimensions of major line markings on the field
goalpostwidth <- 640
centresquareW <- 5000
centresquareL <- 5000
goalsquareW <- 640
goalsquareL <- 900
centrecirclemax_d <- 1000
centrecirclemin_d <- 300
fiftyarc_d <- 10000

#Create a "circle" function for centre circles, 50m arcs
circleFun <- function(centre=c(0,0),diameter=1,npoints=100){
  r = diameter/2
  tt <- seq(0,2*pi,length.out = npoints)
  xx <- centre[1] + r * cos(tt)
  yy <- centre[2] + r * sin(tt)
  return(data.frame(x = xx, y = yy))
}

#Calculations for field landmarks
CentreSquareWidth <- c(((ymax/2)+(centresquareW/2)),((ymax/2)-(centresquareW/2)))
CentreSquareLength <- c(((xmax/2)+(centresquareL/2)),((xmax/2)-(centresquareL/2)))

LeftGoalSquareWidth <- c(((ymax/2)+(goalsquareW/2)),((ymax/2)-(goalsquareW/2)))
LeftGoalSquareLength <- c(xmin,xmin+goalsquareL)

RightGoalSquareWidth <- c(((ymax/2)+(goalsquareW/2)),((ymax/2)-(goalsquareW/2)))
RightGoalSquareLength <- c(xmax,xmax-goalsquareL)

centrecirclemax <- circleFun(c((xmax/2),(ymax/2)),centrecirclemax_d,npoints = 100)
centrecirclemin <- circleFun(c((xmax/2),(ymax/2)),centrecirclemin_d,npoints = 100)

fiftyarcleft <- circleFun(c(xmin,(ymax/2)),fiftyarc_d,npoints=100)
fiftyarcright <- circleFun(c(xmax,(ymax/2)),fiftyarc_d,npoints=100)

#Create a blank canvas that the field will be plotted onto
field <- ggplot() + xlim(c(-10,xmax+10)) + ylim(c(-10,ymax+10))

#Start adding layers to the blank canvas
field + 
  #add theme_blankPitch onto canvas
  theme_blankPitch() +
  #overlay boundary line (this will be oval in time, but will be rectangle for now as practice)
  geom_rect(aes(xmin=0, xmax=xmax, ymin=0, ymax=ymax),fill=grass_colour,colour=line_colour) +
  #add centre square
 geom_rect(aes(xmin=CentreSquareLength[2],xmax=CentreSquareLength[1],ymin=CentreSquareWidth[2],ymax=CentreSquareWidth[1]),fill=grass_colour,colour=line_colour) +
  #add centre circles
  geom_path(data=centrecirclemax, aes(x=x,y=y),colour=line_colour) +
  geom_path(data=centrecirclemin, aes(x=x,y=y),colour=line_colour) +
  #add 50m arcs
  geom_path(data=fiftyarcleft,aes(x=x,y=y),colour=line_colour) +
  geom_path(data=fiftyarcright,aes(x=x,y=y),colour=line_colour) +
  #add goal squares
 geom_rect(aes(xmin=xmin,xmax=LeftGoalSquareLength[2],ymin=LeftGoalSquareWidth[2],ymax=LeftGoalSquareWidth[1]),fill=grass_colour,colour=line_colour) +
 geom_rect(aes(xmin=RightGoalSquareLength[2],xmax=xmax,ymin=RightGoalSquareWidth[2],ymax=RightGoalSquareWidth[1]),fill=grass_colour,colour=line_colour)
  

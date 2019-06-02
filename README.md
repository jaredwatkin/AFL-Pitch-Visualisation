# AFL-Pitch-Visualisation
Visualising an AFL pitch in R using ggplot2

# Setting a Theme
First up I want to set a colour theme for the plot. I am taking inspiration from [FC RStats'](https://github.com/FCrSTATS) [soccer pitch visualisation](https://github.com/FCrSTATS/Visualisations/blob/master/3.CreateAPitch.md) and using the [Adobe Colour Tool](https://color.adobe.com/create) to create a basic green background with white field markings. 
```
library(ggplot2)
```
```
grass_colour <- "#60BC48"
line_colour <- "#FFFFFC"
background_colour <- "#60BC48"
```
Next up is creating a theme to help control how the field will be displayed in ggplot. Many of the theme features that we don't require can be turned off using element_blank().
```
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
    strip.text.x=element_text(size=size*1))
    }
```
# Creating our field
To create our field, I will need to define the field size to the nearest centimetre for precision. To begin I will use the rough dimensions of Brisbane's [GABBA stadium](https://en.wikipedia.org/wiki/The_Gabba).
```
xmin <- 0
xmax <- 17000
ymin <- 0
ymax <- 15000
```
The next step is to define and calculate the landmarks and line markings we want to plot.

```
# Landmark dimensions
goalpostwidth <- 640
centresquareW <- 5000
centresquareL <- 5000
goalsquareW <- 640
goalsquareL <- 900

## Landmark calculations
# Centre Square
CentreSquareWidth <- c(((ymax/2)+(centresquareW/2)),((ymax/2)-(centresquareW/2)))
CentreSquareLength <- c(((xmax/2)+(centresquareL/2)),((xmax/2)-(centresquareL/2)))

# Goal squares at each end
LeftGoalSquareWidth <- c(((ymax/2)+(goalsquareW/2)),((ymax/2)-(goalsquareW/2)))
LeftGoalSquareLength <- c(xmin,xmin+goalsquareL)
RightGoalSquareWidth <- c(((ymax/2)+(goalsquareW/2)),((ymax/2)-(goalsquareW/2)))
RightGoalSquareLength <- c(xmax,xmax-goalsquareL)
```

I will again borrow from FC R Stats' to create a circle function that will allow me to plot the centre circles and 50m arcs.
```
# Large centre circle diameter
centrecirclemax_d <- 1000
# Small centre circle diameter
centrecirclemin_d <- 300
# 50m arc diameter
fiftyarc_d <- 10000

## define the circle function
circleFun <- function(centre=c(0,0),diameter=1,npoints=100){
  r = diameter/2
  tt <- seq(0,2*pi,length.out = npoints)
  xx <- centre[1] + r * cos(tt)
  yy <- centre[2] + r * sin(tt)
  return(data.frame(x = xx, y = yy))
}

# Create the large, small centre circles
centrecirclemax <- circleFun(c((xmax/2),(ymax/2)),centrecirclemax_d,npoints = 100)
centrecirclemin <- circleFun(c((xmax/2),(ymax/2)),centrecirclemin_d,npoints = 100)

## Left 50m arc
fiftyarcleft <- circleFun(c(xmin,(ymax/2)),fiftyarc_d,npoints=100)
## Right 50m arc
fiftyarcright <- circleFun(c(xmax,(ymax/2)),fiftyarc_d,npoints=100)
```
# Plotting everything

We have all the main landmarks we will need to begin creating our AFL field. Using ggplot2, we will add each element as a new layer on top off a background plot
```
## Initiate ggplot and set boundaries for our canvas that will allow some excess space around the plot. I will set this as the variable "field" to save typing
field <- ggplot() + xlim(c(-10,xmax+10)) + ylim(c(-10,ymax+10))
```
Now let's add the preset theme to the background
```
field + 
# add theme_blankPitch onto canvas
theme_blankPitch()
```
Now I can start adding the field landmarks.

The boundary:
```
field + 
# add theme_blankPitch onto canvas
theme_blankPitch() +
### This will be an oval in time, but I am still figuring out how to plot that in ggplot
geom_rect(aes(xmin=0, xmax=xmax, ymin=0, ymax=ymax),fill=grass_colour,colour=line_colour)
```
The centre square:
```
field + 
# add theme_blankPitch onto canvas
theme_blankPitch() +
### This will be an oval in time, but I am still figuring out how to plot that in ggplot
geom_rect(aes(xmin=0, xmax=xmax, ymin=0, ymax=ymax),fill=grass_colour,colour=line_colour) +
geom_rect(aes(xmin=CentreSquareLength[2],xmax=CentreSquareLength[1],ymin=CentreSquareWidth[2],ymax=CentreSquareWidth[1]),fill=grass_colour,colour=line_colour)
```
The centre circles:
```
field + 
# add theme_blankPitch onto canvas
theme_blankPitch() +
### This will be an oval in time, but I am still figuring out how to plot that in ggplot
geom_rect(aes(xmin=0, xmax=xmax, ymin=0, ymax=ymax),fill=grass_colour,colour=line_colour) +
geom_rect(aes(xmin=CentreSquareLength[2],xmax=CentreSquareLength[1],ymin=CentreSquareWidth[2],ymax=CentreSquareWidth[1]),fill=grass_colour,colour=line_colour) + 
geom_path(data=centrecirclemax, aes(x=x,y=y),colour=line_colour) +
geom_path(data=centrecirclemin, aes(x=x,y=y),colour=line_colour)
```
50m arcs:
```
field + 
# add theme_blankPitch onto canvas
theme_blankPitch() +
### This will be an oval in time, but I am still figuring out how to plot that in ggplot
geom_rect(aes(xmin=0, xmax=xmax, ymin=0, ymax=ymax),fill=grass_colour,colour=line_colour) +
geom_rect(aes(xmin=CentreSquareLength[2],xmax=CentreSquareLength[1],ymin=CentreSquareWidth[2],ymax=CentreSquareWidth[1]),fill=grass_colour,colour=line_colour) + 
geom_path(data=centrecirclemax, aes(x=x,y=y),colour=line_colour) +
geom_path(data=centrecirclemin, aes(x=x,y=y),colour=line_colour) +
geom_path(data=fiftyarcleft,aes(x=x,y=y),colour=line_colour) +
geom_path(data=fiftyarcright,aes(x=x,y=y),colour=line_colour)
```
Goal squares:
```
field + 
# add theme_blankPitch onto canvas
theme_blankPitch() +
### This will be an oval in time, but I am still figuring out how to plot that in ggplot
geom_rect(aes(xmin=0, xmax=xmax, ymin=0, ymax=ymax),fill=grass_colour,colour=line_colour) +
geom_rect(aes(xmin=CentreSquareLength[2],xmax=CentreSquareLength[1],ymin=CentreSquareWidth[2],ymax=CentreSquareWidth[1]),fill=grass_colour,colour=line_colour) + 
geom_path(data=centrecirclemax, aes(x=x,y=y),colour=line_colour) +
geom_path(data=centrecirclemin, aes(x=x,y=y),colour=line_colour) +
geom_path(data=fiftyarcleft,aes(x=x,y=y),colour=line_colour) +
geom_path(data=fiftyarcright,aes(x=x,y=y),colour=line_colour) +
geom_rect(aes(xmin=xmin,xmax=LeftGoalSquareLength[2],ymin=LeftGoalSquareWidth[2],ymax=LeftGoalSquareWidth[1]),fill=grass_colour,colour=line_colour) +
geom_rect(aes(xmin=RightGoalSquareLength[2],xmax=xmax,ymin=RightGoalSquareWidth[2],ymax=RightGoalSquareWidth[1]),fill=grass_colour,colour=line_colour)
```

# AFL-Pitch-Visualisation
This post is going to step through visualising an AFL pitch in R using ggplot2. The idea behind this is to ideally use it as a starting point for different AFL-related data viz that can be plotted onto the field.

# Setting a Theme
First up I want to set a colour theme for the plot. I am taking inspiration from [FC RStats'](https://github.com/FCrSTATS) [soccer pitch visualisation](https://github.com/FCrSTATS/Visualisations/blob/master/3.CreateAPitch.md) and using the [Adobe Colour Tool](https://color.adobe.com/create) to create a basic green background with white field markings. 
```R
library(ggplot2)
library(ggforce)
```
```R
grass_colour <- "#60BC48"
line_colour <- "#FFFFFC"
background_colour <- "#60BC48"
```
Next up is creating a theme to help control how the field will be displayed in ggplot. Many of the theme features that we don't require can be turned off using element_blank().
```R
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
```R
xmin <- 0
xmax <- 17000
ymin <- 0
ymax <- 15000
```
The next step is to define and calculate the landmarks and line markings we want to plot.

```R
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
```R
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

# Create our boundary line
oval <- list(ggforce::geom_ellipse(aes(x0=xmax/2,y0=ymax/2,a=xmax/2,b=ymax/2,angle=0),colour=line_colour))

# Create the large, small centre circles
centrecirclemax <- circleFun(c((xmax/2),(ymax/2)),centrecirclemax_d,npoints = 100)
centrecirclemin <- circleFun(c((xmax/2),(ymax/2)),centrecirclemin_d,npoints = 100)

## Left 50m arc
fiftyarcleft <- circleFun(c(xmin,(ymax/2)),fiftyarc_d,npoints=100)
## Right 50m arc
fiftyarcright <- circleFun(c(xmax,(ymax/2)),fiftyarc_d,npoints=100)
```
# Plotting everything

We have all the main landmarks we will need to begin creating our AFL field. Using ggplot2, we will add each element as a new layer on top of a background plot
```R
## Initiate ggplot and set boundaries for our canvas that will allow some excess space around the plot. I will set this as the variable "field" to save typing
field <- ggplot() + xlim(c(-10,xmax+10)) + ylim(c(-10,ymax+10))
```
![Blank plot](https://github.com/jaredwatkin/AFL-Pitch-Visualisation/blob/master/Plot1.png)

Now let's add the preset theme to the background
```R
field + 
# add theme_blankPitch onto canvas
theme_blankPitch()
```
![Blank + field](https://github.com/jaredwatkin/AFL-Pitch-Visualisation/blob/master/Plot2.png)

Now I can start adding the field landmarks.

The boundary:
```R
field + 
# add theme_blankPitch onto canvas
theme_blankPitch() +
oval
```
![Field](https://github.com/jaredwatkin/AFL-Pitch-Visualisation/blob/master/Plot8.png)

The centre square:
```R
field + 
# add theme_blankPitch onto canvas
theme_blankPitch() +
oval +
geom_rect(aes(xmin=CentreSquareLength[2],xmax=CentreSquareLength[1],ymin=CentreSquareWidth[2],ymax=CentreSquareWidth[1]),fill=grass_colour,colour=line_colour)
```
![Field](https://github.com/jaredwatkin/AFL-Pitch-Visualisation/blob/master/StepByStepPlots/Plot9.png)

The centre circles:
```R
field + 
# add theme_blankPitch onto canvas
theme_blankPitch() +
oval +
geom_rect(aes(xmin=CentreSquareLength[2],xmax=CentreSquareLength[1],ymin=CentreSquareWidth[2],ymax=CentreSquareWidth[1]),fill=grass_colour,colour=line_colour) + 
geom_path(data=centrecirclemax, aes(x=x,y=y),colour=line_colour) +
geom_path(data=centrecirclemin, aes(x=x,y=y),colour=line_colour)
```
![Field](https://github.com/jaredwatkin/AFL-Pitch-Visualisation/blob/master/StepByStepPlots/Plot10.png)

50m arcs:
```R
field + 
# add theme_blankPitch onto canvas
theme_blankPitch() +
oval +
geom_rect(aes(xmin=CentreSquareLength[2],xmax=CentreSquareLength[1],ymin=CentreSquareWidth[2],ymax=CentreSquareWidth[1]),fill=grass_colour,colour=line_colour) + 
geom_path(data=centrecirclemax, aes(x=x,y=y),colour=line_colour) +
geom_path(data=centrecirclemin, aes(x=x,y=y),colour=line_colour) +
geom_path(data=fiftyarcleft,aes(x=x,y=y),colour=line_colour) +
geom_path(data=fiftyarcright,aes(x=x,y=y),colour=line_colour)
```
![Field](https://github.com/jaredwatkin/AFL-Pitch-Visualisation/blob/master/StepByStepPlots/Plot11.png)

Goal squares:
```R
field + 
# add theme_blankPitch onto canvas
theme_blankPitch() +
oval +
geom_rect(aes(xmin=CentreSquareLength[2],xmax=CentreSquareLength[1],ymin=CentreSquareWidth[2],ymax=CentreSquareWidth[1]),fill=grass_colour,colour=line_colour) + 
geom_path(data=centrecirclemax, aes(x=x,y=y),colour=line_colour) +
geom_path(data=centrecirclemin, aes(x=x,y=y),colour=line_colour) +
geom_path(data=fiftyarcleft,aes(x=x,y=y),colour=line_colour) +
geom_path(data=fiftyarcright,aes(x=x,y=y),colour=line_colour) +
geom_rect(aes(xmin=xmin,xmax=LeftGoalSquareLength[2],ymin=LeftGoalSquareWidth[2],ymax=LeftGoalSquareWidth[1]),fill=grass_colour,colour=line_colour) +
geom_rect(aes(xmin=RightGoalSquareLength[2],xmax=xmax,ymin=RightGoalSquareWidth[2],ymax=RightGoalSquareWidth[1]),fill=grass_colour,colour=line_colour)
```
![Field](https://github.com/jaredwatkin/AFL-Pitch-Visualisation/blob/master/StepByStepPlots/Plot12.png)

# Putting it into a Function

In time I will have the kinks ironed out and turn the plot into a function that can be modified for different field dimensions by changing (xmax, ymax) as well as our plot colours.

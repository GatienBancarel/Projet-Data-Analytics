# Shiny application

#### This application was developed as part of a data analysis course at ECE Paris. 
##### It was realized by Baptiste Grobon, Augustin Guermond, Gatien Bancarel and Timothé Presles, and supervised by [Maggie Mhanna](https://www.linkedin.com/in/mmhanna) 

The application has been designed to study data sets from airbnb in Barcelona, Bordeaux and Amsterdam.

#### Cross Cities Dashborad:

In this part, we can plot 3 different graphs. They show respectively the number of airbnb per room type, per property type or per number of days available.

We can compare these graphs for any 2 of the three cities.

All this data contains every date available for each of the city.

#### City Dashboard: 

In this part we can choose between 3 cities :
- Amsterdam
- Barcelona
- Bordeaux

For each of them we can see a map with the residences existing on Airbnb. It is possible to zoom in and out and drag the map.
It is also possible to choose betbeen different parameters and each of them refresh the map when updated :

- Date 

We can select one date among three. 

- Room type

We can choose if we want a : Entire room/apt, Private room, Shared room, Hotel room.

- Number of beds

Whith a slide bar it is possible to select between a minimum of 1 and a maximum of 20.

There is another pannel displaying in a graph the number of bedrooms for each type of residences. This can be modulated by two slide bars :
The first one to pick a range of prices, the second one to select a minimum of nights available.

##### More:
[Pitch Presentation on RPubs](https://rpubs.com/gatien/695968)
<br>Shiny App Deployed: [shinyapps.io](https://bancarel-presles-grobon-guermond.shinyapps.io/ProjectApp/)

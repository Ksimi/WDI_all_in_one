### World Development Indicators – all info on one screen
![alt-text](https://github.com/Ksimi/WDI_all_in_one/blob/master/data-raw/WDI_all_in_one_screenshot.png)

The [World Development Indicators](http://datatopics.worldbank.org/world-development-indicators/) is a compilation of relevant, high-quality, and internationally comparable statistics compiled by the World Bank from officially recognized international sources. The database contains 1,400+ time series indicators for 217 economies with data for many indicators going back more than 50 years.

#### The WDI helps data users find information related to all aspects of development, both historical and current, and to follow trends and monitor progress towards a myriad of goals and targets. The database is compiled from officially-recognized sources and includes national, regional, and global estimates.

The WDI_all_in_one app utilizes the power of the [shinydashboard](http://rstudio.github.io/shinydashboard/index.html) package and provides a convenient interface to represent a selected WDI indicator on one screen:
*	On a big choropleth map driven by [Leaflet](https://rstudio.github.io/leaflet/)
*	As top 10/ bottom 10 countries view: a) on a smaller choropleth map; b) on a bar chart
*	As a progression of the world level values through the years
*	Including the full collateral info, such as indicator definition, development relevance, periodicity etc.

#### The app features:
*	One universal tool for all indicators
* Interdependent filtering by topic, up to three subtopics and a year
*	Automatic filtering out the years without any data
*	Extensive palette color selection between sequential and diverging colors, including colorblind-safe sets
*	Palette type selection between continuous, improved bins (with a proprietary algorithm) and quantile ones
*	Map type selection between regular and scaled (normalized cartogram-like)
*	[Leaflet](https://rstudio.github.io/leaflet/) map [provider selection](http://leaflet-extras.github.io/leaflet-providers/preview/index.html) of 9 different versions of Esri, Stamen and Carto types

#### Here's the direct access to the WDI_all_in_one shinyapp:
 https://ksimi.shinyapps.io/WDI_all_in_one/

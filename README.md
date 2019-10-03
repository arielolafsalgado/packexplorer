packexplorer

[![Project Status: Active – The project has reached a stable, usable state and is being actively developed.](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)

Discover new r-packages with a easy going visualization

Facilitate the searching process of new interesting packages from the CRAN library. It provides an interfase build on leaflet, allowing to move around the packages' network, reading their descriptions, and finding groups of highly related packages. It also provides a system of recommendation, indicating relevant packages that directly relate to the ones you already installed.

You can install it directly from github by using devtools:

```
devtools::install_github("arielolafsalgado/packexplorer")
```


Start trying out
```
plot_neighbors()
who.are.you('leaflet')
expre2graph('info')
```
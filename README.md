# curveFitter

https://tercen.shinyapps.io/curveFitter/
 
To run it locally : 

```
git clone https://github.com/tercen/curveFitter.git
cd curveFitter
```

```R
install.packages('nplr')
install.packages('XLConnect')
install.packages('shinyjs')
devtools::install_github("tercen/rtercen", ref = "2.10")
shiny::runApp(port=3042, launch.browser = FALSE)
```


### [Open in browser](https://fredcommo.shinyapps.io/curveFitter/)
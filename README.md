# PSgraphing

##Installation

```R
install.packages('devtools') 
library(devtools)
install_github('AlexPiche/PSgraphing')
```

If you run into trouble installing “devtools” look at: https://github.com/hadley/devtools

##Minimal Example

```R
library(PSgraphing)
data(PSgraphing)
PSgraphing(count=Count_data3, estimate=Estimate_RR, text='(total n range: 3237 to 3245 per decile)', name='myPSgraph.pdf')
```


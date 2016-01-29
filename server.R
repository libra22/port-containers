library(shiny)
require(rCharts)
library(markdown)
library(rmarkdown)

kargo <- read.csv("kargo2.csv")
kargo$PORT <- as.character(kargo$PORT)
kargo$TONNAGETYPE <- as.character((kargo$TONNAGETYPE))
kargo$TONNE <- as.numeric(kargo$TONNE)

function(input, output) {

  #output$t1plot <- renderChart({
  #  p1 <- nPlot(TONNE ~ YEAR, group = 'TONNAGETYPE', type = 'lineChart', data = subset(kargo,PORT="Johor"))
  ##  p2 <- rPlot(TONNE~PORT,data = subset(kargo,PORT=="JOHOR" & TONNAGETYPE=="IMPORT"), color = "TONNAGETYPE", facet = "TONNAGETYPE", type = 'bar')
  #  p1 <- rPlot(TONNE~TONNAGETYPE|YEAR,data = subset(kargo,PORT=="JOHOR"), color = "TONNAGETYPE", facet = "TONNAGETYPE", type = 'bar')
  ##  p1 <- rPlot(TONNE~TONNAGETYPE,data = subset(kargo,PORT=="JOHOR"), color = "TONNAGETYPE", facet = "TONNAGETYPE", type = 'bar',position="dodge")

  ##      p1$addParams(dom = 't1plot')
  ##  return(p1)
  ##})

  output$ttplot <- renderChart({
    tt <- rPlot(TONNE~TONNAGETYPE|YEAR,data = subset(kargo,PORT==as.character(input$ttport)), color = "TONNAGETYPE", type = 'bar')
    tt$guides(x=list(title="TONNAGE TYPE"))
    tt$guides(y=list(title="TONNE",numticks=5,min=0,max=max(kargo$TONNE[kargo$PORT==input$ttport])+5000))
    tt$addParams(dom = 'ttplot')
    return(tt)
  })

  output$tt2plot <- renderChart({
    tt2 <- rPlot(TONNE~YEAR,data = subset(kargo,PORT==as.character(input$ttport)), color = "TONNAGETYPE", facet = "TONNAGETYPE",type = 'line')
    tt2$layer(x = "YEAR", y = "TONNE", color = 'TONNAGETYPE', data = subset(kargo,PORT==as.character(input$ttport)), type = 'point', size = "TONNE")
    tt2$guides(x=list(title="YEAR",numticks=14,min="2002",max="2015"))
    tt2$guides(y = list(title = "TONNE", numticks=10, min = min(kargo$TONNE[kargo$PORT==as.character(input$ttport)]),max=max(kargo$TONNE[kargo$PORT==as.character(input$ttport)])+5000))
        tt2$addParams(dom = 'tt2plot')
    return(tt2)
  })

  output$cpplot <- renderChart({
    cp <- rPlot(TONNE~YEAR,data = subset(kargo,(PORT==input$cpport1 | PORT==input$cpport2) & TONNAGETYPE==input$cptype), color = "PORT", type = 'line')
    cp$layer(x = "YEAR", y = "TONNE", color = 'PORT', data = subset(kargo,(PORT==input$cpport1 | PORT==input$cpport2) & TONNAGETYPE==input$cptype), type = 'point', size = "TONNE")
    cp$guides(x=list(title="YEAR",numticks=14,min="2002",max="2015"))
    cp$guides(y = list(title = paste(input$cptype," (TONNE)"), numticks=10,min=0,max=max(kargo$TONNE[(kargo$PORT==input$cpport1 | kargo$PORT==input$cpport2) & kargo$TONNAGETYPE ==input$cptype])+5000))
    cp$addParams(dom = 'cpplot')
    return(cp)
  })

  output$ctyplot <- renderChart({
    if(input$chkalltype == FALSE)
    {
       cty <- rPlot(PORT ~ TONNE,data = subset(kargo,YEAR==input$ctyyear & TONNAGETYPE==input$ctytype), type = 'point',size="TONNE")
       cty$guides(y = list(title = "PORT", ticks = unique(kargo$PORT),sorted=TRUE))
       cty$guides(x = list(title = paste(input$ctytype," (TONNE)"),numticks=10, min=0,max=max(kargo$TONNE[kargo$YEAR==input$ctyyear & kargo$TONNAGETYPE ==input$ctytype])+5000))
       cty$addParams(dom = 'ctyplot')
       return(cty)
    }
    else if (input$chkalltype == TRUE & input$chkfacet == FALSE)
    {
      cty <- rPlot(PORT ~ TONNE,data = subset(kargo,YEAR==input$ctyyear), color = "TONNAGETYPE", type = "point",size="TONNE")
      cty$guides(y = list(title = "PORT", ticks = unique(kargo$PORT),sorted=TRUE))
      cty$guides(x = list(title = "TONNE",numticks=10, min=0,max=max(kargo$TONNE[kargo$YEAR==input$ctyyear])+5000))
      cty$addParams(dom = 'ctyplot')
      return(cty)
    }
    else if (input$chkalltype == TRUE & input$chkfacet == TRUE)
    {
      cty <- rPlot(PORT ~ TONNE|TONNAGETYPE,data = subset(kargo,YEAR==input$ctyyear), color = "TONNAGETYPE", type = "point",size="TONNE")
      cty$guides(y = list(title = "PORT", ticks = unique(kargo$PORT),sorted=TRUE))
      cty$guides(x = list(title = "TONNE",numticks=5, min=0,max=max(kargo$TONNE[kargo$YEAR==input$ctyyear])+5000))
      cty$addParams(dom = 'ctyplot')
      return(cty)
    }
  })

  output$aggplot <- renderChart({
    if (input$rbag == "Total")
    {
      aggp <- rPlot(PORT ~ sum(TONNE),data = subset(kargo, TONNAGETYPE==input$agtype),type = 'point')
      aggp$guides(y = list(title = "PORT", ticks = unique(kargo$PORT),sorted=TRUE))
      aggp$guides(x = list(title = paste("TOTAL",input$agtype," (TONNE)"), numticks=10, min=0,max=max(kargo$TONNE[kargo$TONNAGETYPE ==input$agtype])+10000))
      aggp$addParams(dom = 'aggplot', title="Total over years (2003-2014)")
      return(aggp)
    }
    else if (input$rbag == "Average")
    {
      aggp <- rPlot(PORT ~ mean(TONNE),data = subset(kargo, TONNAGETYPE==input$agtype), type = 'point')
      aggp$guides(y = list(title = "PORT", ticks = unique(kargo$PORT),sorted=TRUE))
      aggp$guides(x = list(title = paste("AVERAGE ",input$agtype," (TONNE)"), numticks=10, min=0,max=max(kargo$TONNE[kargo$TONNAGETYPE ==input$agtype])+10000))
      aggp$addParams(dom = 'aggplot',title="Average over years (2003-2014)")
      return(aggp)
    }
  })
}

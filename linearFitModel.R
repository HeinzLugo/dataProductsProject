## This function fits a linear model on the mtcars data based on the variables and values chosen
## thorugh the user interface. The function generates a graph showing the data points used
## and the linear model fit.

library(datasets)
library(dplyr)
library(ggplot2)
linearFitModel <- function(summarySelection, predictorSelection, groupDataBy, conditionalQueryIndex, am, vs)
{
  ## Step 1. Select the columns of interest.
  if(as.numeric(summarySelection) == 1)
  {
    pairs(~., data = mtcars, panel = panel.smooth, main = "Motor Trend Cars data")
  }
  else
  {
    ## Step 2. Select the rows of interest.
    rawDataSet <- tbl_df(mtcars)
    rawDataSet$cyl <- factor(rawDataSet$cyl)
    rawDataSet$gear <- factor(rawDataSet$gear)
    rawDataSet$carb <- factor(rawDataSet$carb)
    if(as.numeric(conditionalQueryIndex) == 1)
    {
      ## Filter cylinder number.
      if(as.numeric(am) == 1)
      {
        processedDataSet <- filter(rawDataSet, am == 0)
      }
      else
      {
        processedDataSet <- filter(rawDataSet, am == 1)
      }
    }
    else
    {
      ## Filter transmission type.
      if(as.numeric(vs) == 1)
      {
        processedDataSet <- filter(rawDataSet, vs == 0)
      }
      else
      {
        processedDataSet <- filter(rawDataSet, vs == 1)
      }
    }
    ## Step 3. Create the graph.
    if(as.numeric(predictorSelection) == 1)
    {
      predictorPlot <- ggplot(data = processedDataSet, aes(x = disp, y = mpg))
    }
    else if(as.numeric(predictorSelection) == 2)
    {
      predictorPlot <- ggplot(data = processedDataSet, aes(x = hp, y = mpg))
    }
    else if(as.numeric(predictorSelection) == 3)
    {
      predictorPlot <- ggplot(data = processedDataSet, aes(x = drat, y = mpg))
    }
    else if(as.numeric(predictorSelection) == 4)
    {
      predictorPlot <- ggplot(data = processedDataSet, aes(x = wt, y = mpg))
    }
    else
    {
      predictorPlot <- ggplot(data = processedDataSet, aes(x = qsec, y = mpg))
    }
    if(as.numeric(groupDataBy) == 1)
    {
      resultPlot <- predictorPlot + geom_point(aes(col = cyl)) + geom_smooth(method = "lm") 
    }
    else if(as.numeric(groupDataBy) == 2)
    {
      resultPlot <- predictorPlot + geom_point(aes(col = gear)) + geom_smooth(method = "lm")
    }
    else
    {
      resultPlot <- predictorPlot + geom_point(aes(col = carb)) + geom_smooth(method = "lm")
    }
    print(resultPlot) 
  }
}
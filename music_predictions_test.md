About the Data
--------------

The data set in use is the **Million Song Dataset** available at <https://labrosa.ee.columbia.edu/millionsong/>. This dataset includes various pieces of information including lyrics, musical data, genre labels, tags, user data, etc. for one million contemporary songs.

This dataset ranges from the 1920's to 2011, with release years very heavily skewed to the left, due to the increase in music recordings over time.) This data weighs in at 280GB, more than double the capacity of my hard drive. For this reason, I opted for a subset of the data, providing 500,000 records with 90 variables specifying timbre (pitch and intensity) characteristics. This dataset is about 200MB - much more reasonable. However my computer was still unhappy. I took a further subset of this data (3,000 records, 90 variables) which left me with a very manageable 2.5MB.

Additionally, I added column names to each variable, creatively named "Variable1" through "Variable90"

Once I selected my database, I dove into some exploratory analysis.

Wait, not yet...

What is timbre?
---------------

Timbre (pronounced "tamber") is a way to describe sound based on its pitch, volume, and quality. In a lot of ways timbre is similar to tone.

Timbre is a fairly abstract concept so this is difficult to measure mathematically. The timbre data from this dataset was calculated using a 12-dimensional vector. This means that even the data is incredibly abstract and difficult for a human being to interpret.

Exploring the Data
------------------

Okay, exploratory analysis time. The first two things I did were view a summary of the data, and list all column names to ensure data was imported correctly. For visibility's sake, I am displaying jus the first 6 variables in this writeup. Considering the data is so abstract to begin with, it is difficult to discern any patterns based on summary statistics alone.

The next thing I did was to create correlation matrix of all available data and export it to a CSV file for easy viewing. I chose the 6 variables that appeared to have the strongest relationship to Year (Variable1, Variable6, Variable73, Variable38, Variable36, Variable67).

``` r
# load in the tidyverse package
library(tidyverse)

# read data
library(readxl)
music_data <- read_excel("music_data.xlsx")

# examining data
summary(music_data[,c(1:6)])
```

    ##       Year        Variable1       Variable2          Variable3       
    ##  Min.   :1930   Min.   :18.98   Min.   :-265.266   Min.   :-161.802  
    ##  1st Qu.:1996   1st Qu.:40.35   1st Qu.: -23.398   1st Qu.: -11.132  
    ##  Median :2003   Median :44.60   Median :  10.019   Median :  10.799  
    ##  Mean   :1999   Mean   :43.63   Mean   :   1.401   Mean   :   8.456  
    ##  3rd Qu.:2006   3rd Qu.:48.07   3rd Qu.:  35.427   3rd Qu.:  29.783  
    ##  Max.   :2010   Max.   :54.01   Max.   : 171.163   Max.   : 233.168  
    ##    Variable4         Variable5      
    ##  Min.   :-70.041   Min.   :-91.857  
    ##  1st Qu.: -6.693   1st Qu.:-21.601  
    ##  Median :  1.358   Median : -6.401  
    ##  Mean   :  3.630   Mean   : -7.217  
    ##  3rd Qu.: 11.271   3rd Qu.:  7.035  
    ##  Max.   :112.688   Max.   :262.069

``` r
# loading in rpart package
library(rpart)

# listing column names
names(music_data[,c(1:6)])
```

    ## [1] "Year"      "Variable1" "Variable2" "Variable3" "Variable4" "Variable5"

``` r
cor_data <- cor(music_data)
```

Model Making - Decision Tree
----------------------------

I chose to give decision trees a shot at first, to see how they would fair. Decision tree was the first model I learned about and I still think the most intuitive to visualize.

``` r
#creating new model
fit <- rpart(Year ~ Variable1 + Variable6 + Variable73 + Variable38 + Variable36 + Variable67, data = music_data)
```

Evaluating the Decision Tree
----------------------------

Here, I printed the first 6 records, their predictions, and the actual year each song was released.

This seemed pretty good, almost suspiciously good, considering how simple this model is.

I then calculated the Mean Absolute Error. The MAE essentially tells you "On average, your guesses are \_\_\_\_ years off from the real value."

In this case, it was about 7 years off.

    ## [1] "Making predictions for:"

    ## # A tibble: 6 x 91
    ##    Year Variable1 Variable2 Variable3 Variable4 Variable5 Variable6
    ##   <dbl>     <dbl>     <dbl>     <dbl>     <dbl>     <dbl>     <dbl>
    ## 1  2001      49.9    21.5        73.1      8.75   -17.4       -13.1
    ## 2  2001      48.7    18.4        70.3     12.9    -10.3       -24.8
    ## 3  2001      51.0    31.9        55.8     13.4     -6.58      -18.5
    ## 4  2001      48.2    -1.90       36.3      2.59     0.972     -26.2
    ## 5  2001      51.0    42.2        67.1      8.47   -15.9       -16.8
    ## 6  2001      50.5     0.316      92.4     22.4    -25.5       -19.0
    ## # ... with 84 more variables: Variable7 <dbl>, Variable8 <dbl>,
    ## #   Variable9 <dbl>, Variable10 <dbl>, Variable11 <dbl>, Variable12 <dbl>,
    ## #   Variable13 <dbl>, Variable14 <dbl>, Variable15 <dbl>,
    ## #   Variable16 <dbl>, Variable17 <dbl>, Variable18 <dbl>,
    ## #   Variable19 <dbl>, Variable20 <dbl>, Variable21 <dbl>,
    ## #   Variable22 <dbl>, Variable23 <dbl>, Variable24 <dbl>,
    ## #   Variable25 <dbl>, Variable26 <dbl>, Variable27 <dbl>,
    ## #   Variable28 <dbl>, Variable29 <dbl>, Variable30 <dbl>,
    ## #   Variable31 <dbl>, Variable32 <dbl>, Variable33 <dbl>,
    ## #   Variable34 <dbl>, Variable35 <dbl>, Variable36 <dbl>,
    ## #   Variable37 <dbl>, Variable38 <dbl>, Variable39 <dbl>,
    ## #   Variable40 <dbl>, Variable41 <dbl>, Variable42 <dbl>,
    ## #   Variable43 <dbl>, Variable44 <dbl>, Variable45 <dbl>,
    ## #   Variable46 <dbl>, Variable47 <dbl>, Variable48 <dbl>,
    ## #   Variable49 <dbl>, Variable50 <dbl>, Variable51 <dbl>,
    ## #   Variable52 <dbl>, Variable53 <dbl>, Variable54 <dbl>,
    ## #   Variable55 <dbl>, Variable56 <dbl>, Variable57 <dbl>,
    ## #   Variable58 <dbl>, Variable59 <dbl>, Variable60 <dbl>,
    ## #   Variable61 <dbl>, Variable62 <dbl>, Variable63 <dbl>,
    ## #   Variable64 <dbl>, Variable65 <dbl>, Variable66 <dbl>,
    ## #   Variable67 <dbl>, Variable68 <dbl>, Variable69 <dbl>,
    ## #   Variable70 <dbl>, Variable71 <dbl>, Variable72 <dbl>,
    ## #   Variable73 <dbl>, Variable74 <dbl>, Variable75 <dbl>,
    ## #   Variable76 <dbl>, Variable77 <dbl>, Variable78 <dbl>,
    ## #   Variable79 <dbl>, Variable80 <dbl>, Variable81 <dbl>,
    ## #   Variable82 <dbl>, Variable83 <dbl>, Variable84 <dbl>,
    ## #   Variable85 <dbl>, Variable86 <dbl>, Variable87 <dbl>,
    ## #   Variable88 <dbl>, Variable89 <dbl>, Variable90 <dbl>

    ## [1] "Predictions are"

    ##        1        2        3        4        5        6 
    ## 2000.496 2004.881 2000.496 2004.881 2000.496 2004.881

    ## [1] "Actual year is"

    ## [1] 2001 2001 2001 2001 2001 2001

    ## [1] "The MAE is"

    ## Warning: package 'modelr' was built under R version 3.5.1

    ## [1] 7.089455

Evaluating Mean Absolute Error
------------------------------

I was also curious about how including more branches in the decision tree would effect the results. This function displays how the MAE is affected by different numbers of branches. As displayed in the results, a max depth of 3 appears to be ideal.

``` r
# function created to get the maximum average error for a given max depth.
get_mae <- function(maxdepth, target, predictors, training_data, testing_data){
  
  predictors <- paste(predictors, collapse="+")
  formula <- as.formula(paste(target,"~",predictors,sep = ""))
  
  model <- rpart(formula, data = training_data,
                 control = rpart.control(maxdepth = maxdepth))
  
  mae <- mae(model, testing_data)
  return(mae)
}

# finding maxdepth that leads to the lowest mean average error for this dataset
splitData <- resample_partition(music_data, c(test = 0.3, train = 0.7))

target <- "Year"
predictors <-  c("Variable1", "Variable6", "Variable73", "Variable38", "Variable36", "Variable67")

for(i in 1:10){
  mae <- get_mae(maxdepth = i, target = target, predictors = predictors,
                 training_data = splitData$train, testing_data = splitData$test)
  print(glue::glue("Maxdepth: ",i,"\t MAE: ",mae))
}
```

    ## Maxdepth: 1   MAE: 7.34135597952469
    ## Maxdepth: 2   MAE: 7.20018235431243
    ## Maxdepth: 3   MAE: 7.18604439667443
    ## Maxdepth: 4   MAE: 7.18604439667443
    ## Maxdepth: 5   MAE: 7.18604439667443
    ## Maxdepth: 6   MAE: 7.18604439667443
    ## Maxdepth: 7   MAE: 7.18604439667443
    ## Maxdepth: 8   MAE: 7.18604439667443
    ## Maxdepth: 9   MAE: 7.18604439667443
    ## Maxdepth: 10  MAE: 7.18604439667443

Random Forest Experimentation
-----------------------------

Out of curiousity, I also wanted to try using a random forest model. I assumed this would provide a slightly more accurate prediction, and this ended up being the case.

``` r
# read in the library we'll use for random forests
library(randomForest)
```

    ## Warning: package 'randomForest' was built under R version 3.5.1

    ## randomForest 4.6-14

    ## Type rfNews() to see new features/changes/bug fixes.

    ## 
    ## Attaching package: 'randomForest'

    ## The following object is masked from 'package:dplyr':
    ## 
    ##     combine

    ## The following object is masked from 'package:ggplot2':
    ## 
    ##     margin

``` r
# training random forest model to compare
fitRandomForest <- randomForest(Year ~ Variable1 + Variable6 + Variable73 + Variable38 + Variable36 + Variable67, data = splitData$train)

# compute MAE
print("MAE is")
```

    ## [1] "MAE is"

``` r
mae(model = fitRandomForest, data = splitData$test)
```

    ## [1] 7.03959

Final Thoughts
--------------

While the predictions seem to be close to the actual value, I'm skeptical that this may be due to most of the records being released in the early 2000's. I'm curious to see how well it may predict a song from the 20's or 30's. While there is less data to go on for these older decades, they will also have a more distinct sound, which may make it easier.

Another question brought to my attention during this process is how the model might handle a song which relies on sampling (perhaps of old records). Will these songs throw it off?

I'm leaving these points open for future experimentation.

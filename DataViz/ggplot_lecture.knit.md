---
title: "Plot Superstar"
author: "AMS"
date: '2022-07-01'
output: html_document
---



### Where we were?

-   Installed R and R studio

-   Learned to install packages/load packages

-   Learned about R environments (super helpful, not regularly taught. Be bold!)

-   Learned how to load data sets

-   Name data sets

### Where are we going?

-   What is an R object?

-   Understand the different types of variables

-   What is a dataframe

-   What's inside a dataframe

-   How can we tell the story of the data

### Data visualization:

-   Use knowledge of perception and cognition to plan, create, and evaluate data visualizations.

-   Conduct exploratory data analysis using tabulated and graphical data visualization techniques

-   Choose color pallets for visualization based on principles of perception.

### Goals:

-   Lots of coding

### Let's get it started.

### What is an object?

-   Simplest description, an object within the R programming language is a noun describing different types of data structures.

-   Six types:

    -   Vector:

        -   The most simple type of data structure in R.

-   "A sequence of data elements"

    
    ```r
    #numberical vector
    #create an r object named vec1 that consists of the the numbers 1,2,3,4
    vec1 = c(1, 2, 3, 4) 
    #print vec1
    vec1
    ```
    
    ```
    ## [1] 1 2 3 4
    ```

Create a vector named vec2, containing four numerical elements.


```r
#answer here:
```

Example solution:


```r
vec2 = c(-1, -2, -3, -4)
```

Statistical and mathematical operations can be conducted between vectors. Create a new vector, vec3, by adding vec1 and vec2 together. Add vec1 and vec2 together. Print vec3.


```r
#answer here:
```

Example solution:


```vec3
vec3
```

-   Vectors can also include data structures such as characters such as unique letter, word, or letter/word/number combinations.

Example


```vec4

```

-   Lists

    -   Objects of R that contain various types of elements including characters, numbers, vectors, and a nested list inside it. It can also consist of matrices or functions as elements. It can be created with the help of the list() function.

    -   We will not focus on lists today, provided below are useful websites that outline methods for working with lists

        -   <https://data-flair.training/blogs/r-list-tutorial/>

        -   <https://www.r-bloggers.com/2015/08/how-to-use-lists-in-r/>

        -   <https://bioconnector.github.io/workshops/r-lists.html>

-   matrix

    -   arrange elements in one to two dimensional space

    -   Columns and rows within matrix **environments do NOT have names**

    -   Example:

    -   $$
        \begin{bmatrix}1 & 2 & 3\\a & b & c\end{bmatrix}
        $$

-   array:

    -   Stores data in more than 2 dimensions. Will not be addressed today, but can be created with `array()`

    -   Useful links:

        -   <https://www.tutorialspoint.com/r/r_arrays.htm>

        -   <https://www.educative.io/answers/how-to-work-with-arrays-in-r>

        -   <https://www.geeksforgeeks.org/r-array/>

-   Dataframe:

    -   a 2-dimensional object where each column consists of the value of one variable and each row and each row is the value of each column.


```r
starwars = dplyr::starwars
df = starwars[,1:6]
head(df)
```

```
## # A tibble: 6 × 6
##   name           height  mass hair_color  skin_color  eye_color
##   <chr>           <int> <dbl> <chr>       <chr>       <chr>    
## 1 Luke Skywalker    172    77 blond       fair        blue     
## 2 C-3PO             167    75 <NA>        gold        yellow   
## 3 R2-D2              96    32 <NA>        white, blue red      
## 4 Darth Vader       202   136 none        white       yellow   
## 5 Leia Organa       150    49 brown       light       brown    
## 6 Owen Lars         178   120 brown, grey light       blue
```

### Variable types:

-   As noted above, elements within a vector, matrix, list, array, and dataframes can come in multiple forms. The following variable types are utilized by R and we will utilize a couple of these today.

    -   Character

        -   "a", "name", "a1", "!a"

    -   Numeric

        -   Real numbers: integers (1, 2, 10, etc) and irrational numbers (0.12, -0.1, 12.45)

    -   Integers:

        -   Forces whole numers

    -   Two additional variable structures are included in R, but will not be covered today.

        -   Logical

        -   Complex

-   Define the variable types in the dataset below.


```r
df = df %>%
  mutate(mass = mass + rnorm(n = 6, mean= 0, sd = 1))
```

```
## Warning in mass + rnorm(n = 6, mean = 0, sd = 1): longer object length is not a
## multiple of shorter object length
```

```r
head(df)
```

```
## # A tibble: 6 × 6
##   name           height  mass hair_color  skin_color  eye_color
##   <chr>           <int> <dbl> <chr>       <chr>       <chr>    
## 1 Luke Skywalker    172  75.3 blond       fair        blue     
## 2 C-3PO             167  77.2 <NA>        gold        yellow   
## 3 R2-D2              96  34.6 <NA>        white, blue red      
## 4 Darth Vader       202 136.  none        white       yellow   
## 5 Leia Organa       150  49.8 brown       light       brown    
## 6 Owen Lars         178 121.  brown, grey light       blue
```

#### Hold up and recap:

#### Where were we?

-   What is an R object?

-   Understand the different types of variables

-   What is a dataframe

-   What's inside a dataframe

#### Data Visualization:

-   The process of translating information into a visual context to help the human brain understand and make insightful inferences about data.

-   Goal:

    -   Make it easy to identify patters, trends, and outliers in large data sets.

-   Your visualization should allow the individual examining the figure to extract information that is not easy to present in text, or to compliment text to help a reader unpack knowledge.

-   Visualiations that require large amounts of text to explain

The required libraries for this tutorial include `ggplot2` . As learned in previous lectures, please install and load the library below.

Good practice when working in rmarkdowns is to install packages from the console or `#comment` out any installation lines of code, because some installs can be lengthy. In the below code, install the following libraries `dplyr` and `ggplot2`. Load both of the packages along with `readr`.


```r
#install ggplot2 below.

#install dplyr below.

#load the library ggplot2, dplyr, and readr
```

## Solution


```r
#install ggplot2 below.
#install.packages('ggplot2')
#install dplyr below.
#install.packages(`dplyr`)
#load the library ggplot2, dplyr, and readr
library(ggplot2)
library(dplyr)
library(readr)
```

### Let's load some data. Briefly these data describe variables from


```r
pnt = read.csv("pnt.csv")
```

-   About the data

    -   \- These data are from the 175 items from a commonly utilized picture naming test in aphasiology.

    -   Data set includes six variables

        -   word: Target word name

        -   frequency: The frequency the word it utilized in publically available data

        -   aoa: The estimated the age of acquisition of the word

        -   length: The number of phonemes of the word

        -   difficulty: The estimated naming difficulty of the item.

            -   Lower numbers are easier than higher numbers

            -   0 is the mean difficulty of the item

### After loading your data there are a couple ways to take a quick look at the type of variables included in the dataset and some of their descriptive statistics.

![](variable%20names.png)

### Take a peak at the data.

There are multiple functions that can help you quickly learn about your data. Each command requires you to input the dataframe name into the parentheses following the function name.

-   `summary()` provides descriptive statistics such as the minimum, maximum, median and mean variables. It will also provide the number of character values in a given variable along with the number of missing values of a given variable.

-   `str()` prints the name of each column, followed by the type of data within each variable.

-   `head()` prints the first 6 rows of your data. \### Excercise 1-8: Previewing data

#### Previewing data, exercise 1: Print a summary of the six variables from the dataset `pnt`

##### Response 1:


```r
#PROVIDE ANSWER HERE
```

##### Solution 1:


```r
summary(pnt)
```

```
##        X             word                freq            aoa           
##  Min.   :  1.0   Length:175         Min.   :0.0000   Length:175        
##  1st Qu.: 44.5   Class :character   1st Qu.:0.9031   Class :character  
##  Median : 88.0   Mode  :character   Median :1.2788   Mode  :character  
##  Mean   : 88.0                      Mean   :1.3376                     
##  3rd Qu.:131.5                      3rd Qu.:1.8293                     
##  Max.   :175.0                      Max.   :3.2119                     
##                                                                        
##      length           cat              difficulty     
##  Min.   : 1.000   Length:175         Min.   :-2.0080  
##  1st Qu.: 3.000   Class :character   1st Qu.:-0.9650  
##  Median : 4.000   Mode  :character   Median :-0.4855  
##  Mean   : 4.497                      Mean   :-0.4551  
##  3rd Qu.: 5.000                      3rd Qu.: 0.0740  
##  Max.   :11.000                      Max.   : 1.8800  
##                                      NA's   :1
```

#### Previewing data, exercise 2: Print a summary of the `length` variable only from the `pnt` dataset

##### Response 2:


```r
#Hint: Use the $ operator
#TYPE SOLUTION HERE
```

##### Solution 2:


```r
#use the $ operator to tell R which variable you would like it to process
summary(pnt$length)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##   1.000   3.000   4.000   4.497   5.000  11.000
```

#### Previewing data, exercise 3: Print the structure of every variable of the `pnt` dataset with a single command.

##### Response 3:


```r
#INSERT ANSWER HERE
```

##### Solution 3:


```r
str(pnt)
```

```
## 'data.frame':	175 obs. of  7 variables:
##  $ X         : int  1 2 3 4 5 6 7 8 9 10 ...
##  $ word      : chr  "fan" "cannon" "beard" "helicopter" ...
##  $ freq      : num  1.23 0.778 1.398 1.204 1.763 ...
##  $ aoa       : chr  "3" "3" "3" "2" ...
##  $ length    : int  3 5 4 10 3 5 9 8 7 4 ...
##  $ cat       : chr  "Misc." "Misc." "Body part" "Vehicle" ...
##  $ difficulty: num  -1.23 0.157 0.137 0.522 0.579 0.157 0.772 0.772 -0.283 0.579 ...
```

#### Previewing data, exercise 4: Print the structure of the variable `difficulty` within the `pnt` dataframe.

##### Response 4:


```r
#INSERT ANSWER HERE
```

##### Solution 4:


```r
str(pnt$difficulty)
```

```
##  num [1:175] -1.23 0.157 0.137 0.522 0.579 0.157 0.772 0.772 -0.283 0.579 ...
```

#### Previewing data, exercise 5: Print the first six rows of `pnt` for all of the variables

##### Response 5:


```r
#INSERT ANSWER HERE
```

##### Solution 5:


```r
head(pnt)
```

```
##   X       word   freq  aoa length       cat difficulty
## 1 1        fan 1.2304    3      3     Misc.     -1.230
## 2 2     cannon 0.7782    3      5     Misc.      0.157
## 3 3      beard 1.3979    3      4 Body part      0.137
## 4 4 helicopter 1.2041    2     10   Vehicle      0.522
## 5 5        van 1.7634 NULL      3   Vehicle      0.579
## 6 6     sailor 1.0792    3      5    Person      0.157
```

#### Previewing data, exercise 6: Print the first six rows of the variable `word` from the `pnt` dataset.

##### Response 5:


```r
#INSERT RESPONSE HERE
```

##### Solution 6:


```r
head(pnt$word)
```

```
## [1] "fan"        "cannon"     "beard"      "helicopter" "van"       
## [6] "sailor"
```

#### Previewing data, exercise 7: The `tail` function is the opposite of head. Print the last 6 items of all variables with pnt below.

##### Response 7:


```r
#INSERT SOLUTION HERE
```

##### Solution 7:


```r
tail(pnt)
```

```
##       X   word   freq aoa length                cat difficulty
## 170 170   fish 2.2122   1      3      Animal, other     -1.555
## 171 171   shoe 1.8976   1      2           Clothing     -1.494
## 172 172 spider 0.8451   3      6      Animal, other     -0.242
## 173 173   belt 1.4314   2      4           Clothing     -0.841
## 174 174 toilet 1.4472   3      5 Object, structural     -0.119
## 175 175 saddle 1.0000   3      5              Misc.      0.311
```

#### Previewing data, exercise 8: Within the `tail` and `head` functions, you may also specifiy how many rows you want to output by adding a `n = #` where \# is the desired number of output rows. Make sure to add a comma after the dataframe name.

#### Print the first 10 rows of PNT for all variables.

##### Response 8a


```r
#INSERT SOLUTION HERE
```

##### Solution 8a


```r
head(pnt, n = 10)
```

```
##     X         word   freq  aoa length         cat difficulty
## 1   1          fan 1.2304    3      3       Misc.     -1.230
## 2   2       cannon 0.7782    3      5       Misc.      0.157
## 3   3        beard 1.3979    3      4   Body part      0.137
## 4   4   helicopter 1.2041    2     10     Vehicle      0.522
## 5   5          van 1.7634 NULL      3     Vehicle      0.579
## 6   6       sailor 1.0792    3      5      Person      0.157
## 7   7    ambulance 0.9542 NULL      9     Vehicle      0.772
## 8   8 cheerleaders 0.0000 NULL      8      Person      0.772
## 9   9     sandwich 1.0000    2      7 Food, other     -0.283
## 10 10        skull 1.3222 NULL      4   Body part      0.579
```

#### Print the last 4 rows of PNT for all variables.

##### Response 8b:


```r
#INSERT SOLUTION HERE
```

##### Solution 8b:


```r
tail(pnt, n = 4)
```

```
##       X   word   freq aoa length                cat difficulty
## 172 172 spider 0.8451   3      6      Animal, other     -0.242
## 173 173   belt 1.4314   2      4           Clothing     -0.841
## 174 174 toilet 1.4472   3      5 Object, structural     -0.119
## 175 175 saddle 1.0000   3      5              Misc.      0.311
```

#### Run the summary function by inserting the dataframe name into the the summary function

### Variables:

-   Categorical: A characteristic of someone of something that can be called by name (e.g., handedness)

    -   Types of plots for categorical variables:

        Pie plot:


```r
df2 = read.csv("osf_meir and Bangash.csv")
df2 = df2 %>%
  mutate(Handedness == "Right", 1, 0)
df2 = df2 %>% #create df2 based on a function to follow the pipe symbol 
  count(Handedness) #count the number of each unique observation found in the "Handedness" variable

#Use pie to generate pie chart
pie(df2$n,  #provide dataframe and variable name
    labels = c("Left", "Right"), #generate labels for variables. Variable names are defined in alphabetic and numerical order.
    main = "Pie chart of handedness")  #Main function is used to create a title. title must be in quotes and can included spaces.
```

<img src="ggplot_lecture_files/figure-html/pie_chart-1.png" width="672" />

-   Bar plot


```r
#create education variable
library(ggplot2)
df = read.csv("osf_meir and Bangash.csv")
df$ed = ifelse(df$Education < 12.1, "High School",
               ifelse(df$Education > 12 & df$Education < 16, "Some College",
                      ifelse(df$Education == 16, "Bachelor's", "Graduate")))
df$ed = factor(df$ed, levels = c("High School", "Some College", "Bachelor's", "Graduate"))

barplot = ggplot(df) + # create ggplot object from data = df+
  geom_bar(aes(x = ed, fill = ed)) +
  labs(
    title = "Barplot of education level",
    x = "Level of education",
    y = "Count",
    fill = "Education Level"
  )
barplot
```

<img src="ggplot_lecture_files/figure-html/unnamed-chunk-28-1.png" width="672" />

#### Here is a screen grab from [osf](https://osf.io/3eg4x/wiki/home/)

#### Now that we have a loaded the dataset let's jump into plotting. Whenever a plot is generated, it should convey a message that is not easily done so by words.

### 

#### The first question you should think about when starting the data visualization processing is "why." Why do you want to look at your data.

### Consider the third variable in the data set, `handedness`.

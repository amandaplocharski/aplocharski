---
  title: "Virginia Election Project"
author: "Amanda Plocharski"
execute:
  echo: true
format:
  html:
  self-contained: true
code-tools: true
---
  
  ## For this dataset, we will be looking at data from the VA governor race as well as results from the 2020 presidential election. First, we will load our packages and the dataset.
  
  ```{r setup, include=FALSE, warning=FALSE, message=FALSE}


library(tidyverse)
library(janitor)
library(httr)
library(jsonlite)
library(kableExtra)
library(here)
library(DT)
library(ggplot2)
options(scipen = 999)
options(stringsAsFactors = FALSE)


joined_vacomparison <- readRDS(here("processed_data", "joined_vacomparison.rds"))



```

```{r}
## In order to allow people to interact with the entire dataset, we are creating an interactive table. To do this, we use the package "DT." In the parentheses is the dataset name (joined_vacomparison), rownames = FALSE which removes the row names with numbers that are automatically assigned to data, and filter = "top" puts the filter option on the top of the table.

DT::datatable(joined_vacomparison, 
              rownames = FALSE, 
              filter = "top")
```

## Comparing Virginia Governor vs. President

```{r}
## Getting into the data, we are first going to take a look at how Trump performed in VA in the 2020 election versus how Youngkin performed in the governor race. 

## To compare this, we will use the "mutate" function to look at the percents and to create a new column in our dataset. You'll see that using this mutate function will create a new dataset called "joined_vacomparison_republican," and in that dataset there will be a column titled "dem_pct" where we will see the margin of victory of Youngkin over Trump, or vice versa.

joined_vacomparison_republican <- joined_vacomparison %>% 
  mutate(
    rep_pct = pct_youngkin - trump_pct
  )
```

```{r}

# Now, we are going to build off of the code above to take a look at the top 5 counties where Youngkin performed better than Trump. To do this, we will use the "arrange," "head" and "ggplot" functions.

# The first part is the same exact code as above. Now included, however, is "arrange" and "head." "Arrange(desc(rep_pct))" is arranging that new "rep_pct" column in our dataset by descending order. So, counties where Youngkin had the highest margin of victory will appear at the top, and lowest margin of victory will appear at the bottom. "Head(5)" is going to pull out the top 5 counties from that entire dataset.

joined_vacomparison_republican <- joined_vacomparison %>% 
  mutate(rep_pct = pct_youngkin - trump_pct)%>%
  arrange(desc(rep_pct))%>%
  head(5)

# The "ggplot" function is going to create a bar chart to show the results  of these top five counties from the section of code above. The x axis is going to be "locality" (the county name) and the y axis is going to be the top five values from that "rep_pct" column. The angle of the text is also goinng to be rotated by 45 degrees.  

ggplot(joined_vacomparison_republican, aes(x = locality, y = rep_pct)) +
  geom_col() +  theme(axis.text.x = element_text(angle = 45))


```

```{r}

# Next, we are going to do the same as above, but now we are going to take a look at where Youngkin got the highest percentage of the vote.To do this, we are going to make a new dataset called "youngkin_top_5," and arrange the "pct_youngkin" column in descending order, and only show the top five results. This is the same exact as above, except without using mutate or creating a new column since this data is already in the original "joined_vacomparison" dataset. 

youngkin_top_5 <- joined_vacomparison%>%
  arrange(desc(pct_youngkin))%>%
  head(5)

# This ggplot function is going to be the exact same thing as above, only this time, we are pulling the data from the new "youngkin_top_five" datatset, and making the y axis be from the "pct_youngkin" column. 

ggplot(youngkin_top_5, aes(x = locality, y = pct_youngkin)) +
  geom_col() +  theme(axis.text.x = element_text(angle = 45))
```

```{r}
# Now, we are going to take a look at the top five counties where McCauliffe got the highest percentage of the vote. Instead of making a chart, however, we are going to create a table.

# We are going to start by taking our dataset "joined_vacomparison" and using the "select" function to pull out only the specific columns that we need from the dataset. Here, we only need "locality" and "pct_mcauliffe." 

# Then, we are using "arrange" and "head" to arrange the "pct_mcauliffe" column from highest to lowest value and pulling only the top five results.  

#Finally, using similar code to above when we made our original interactive datatable, we are going to use the "DT" package to create a simpler table using the data we just pulled. Since this is a simple table with all of the results able to be seen, we are removing the option to search the option to create different pages to view the data (which is useful when there is a lot of columns to be seen that do not all fit on the screen). 

joined_vacomparison %>% 
  select(locality, pct_mcauliffe)%>%
  arrange(desc(pct_mcauliffe))%>%
  head(5) %>% 
  DT::datatable(rownames = FALSE, 
                options = list(searching = FALSE, paging = FALSE, dom = "tip"))
```

```{r}

#Now, we are going to take a look at the top five counties where Biden performed better than McCauliffe, and then pull that data into a chart. 

# To do this, we are going to start by creating a new column like we did before with Trump v. Youngkin. Using the "mutate" function, we are creating a new column titled "dem_pct" and findinf the difference in percentage of votes between Biden ("biden_pct") and McCauliffe ("pct_mcauliffe"). We are using "arrange" and "head" to arrange the new "dem_pct" column from highest to lowest and pulling the top five results. This column is going to be saved into a new dataset titled "joined_vacomparison_democrat."


joined_vacomparison_democrat <- joined_vacomparison %>% 
  mutate(dem_pct = biden_pct - pct_mcauliffe)%>%
  arrange(desc(dem_pct))%>%
  head(5)

# Using this new "joined_va_comparison_democrat" dataset, we are going to use the "ggplot" function to create a bar chart. The x axis is going to be the "locality" column, and the y axis is going to be the "dem_pct" column. We are also going to rotate the text on the y axis by 45 degrees to make sure that the county names are legible. 

ggplot(joined_vacomparison_democrat, aes(x = locality, y = dem_pct)) +
  geom_col() +  theme(axis.text.x = element_text(angle = 45))

```

```{r}
# Now, we are going to take a look at the total number of votes for democrats measured against the total number of votes for republicans in these two elections to see in which county the democrats accumulated more votes than the republicans.  

# To do this, we are going to first create a new column that adds up all of Biden's votes and McAuliffe's votes, and another column that adds up all of Trump's votes and Youngkin's votes. Using the "mutate" function, we are going to add up Biden and McAuliffe and create a new column titled "dem_vote_total," and then add up Trump and Youngkin and create a new column titled "rep_vote_total." Those new columns will be saved in a new dataset titled "joined_comparison_total."

# # I also want to see which five counties had the highest difference in collective votes between democrats and republicans. To do this, I am  using that same "mutate" function to find the difference in votes between the "dem_vote_total" and "rep_vote_total" columns we are making. This new column that details the difference in total votes will be called "dem_v_rep." Then, I am arranging this column from highest to lowest value and pulling the top five results.

joined_vacomparison_total <- joined_vacomparison %>% 
  mutate(dem_vote_total = biden + mcauliffe, rep_vote_total = trump + youngkin, dem_v_rep = dem_vote_total - rep_vote_total)%>%
  arrange(desc(dem_v_rep))%>%
  head(5)


# Finally, to visualize this data, I am going to use "ggplot" to create a new bar chart. Using the "joined_comparison_total" dataset, I am putting "locality" on the x axis, and the new "dem_v_rep" column on the y axis. And again, rotating the text on the x axis by 45 degrees to fit the county names better. 

ggplot(joined_vacomparison_total, aes(x = locality, y = dem_v_rep)) +
  geom_col() +  theme(axis.text.x = element_text(angle = 45))
```

```{r}
# Now, we are going to do the same as above, only this time, we are going to take a look at the top five counties where republicans accumulated the most votes..  

# To do this, we are going to first create a new column that adds up all of Biden's votes and McAuliffe's votes, and another column that adds up all of Trump's votes and Youngkin's votes. Using the "mutate" function, we are going to add up Biden and McAuliffe and create a new column titled "dem_vote_total," and then add up Trump and Youngkin and create a new column titled "rep_vote_total." Those new columns will be saved in a new dataset titled "joined_comparison_total." 

# I also want to see which five counties had the highest difference in collective votes between democrats and republicans. To do this, I am  using that same "mutate" function to find the difference in votes between the "dem_vote_total" and "rep_vote_total" columns we are making. This new column that details the difference in total votes will be called "dem_v_rep." Then, I am arranging this column from highest to lowest value and pulling the top five results.

joined_vacomparison_total_2 <- joined_vacomparison %>% 
  mutate(dem_vote_total = biden + mcauliffe, rep_vote_total = trump + youngkin, dem_v_rep = rep_vote_total - dem_vote_total)%>%
  arrange(desc(dem_v_rep))%>%
  head(5)


# Finally, to visualize this data, I am going to use the "DT" package to create a new, simple table using the "joined_comparison_total_2" dataset. Using the "select" function, I am going to only pull the "locality" and "dem_v_rep" columns. Since this is a simple table with all of the results able to be seen, we are removing the option to search the option to create different pages to view the data (which is useful when there is a lot of columns to be seen that do not all fit on the screen).  

joined_vacomparison_total_2 %>% 
  select(locality, dem_v_rep)%>%
  arrange(desc(dem_v_rep))%>%
  head(5) %>% 
  DT::datatable(rownames = FALSE, 
                options = list(searching = FALSE, paging = FALSE, dom = "tip"))
```

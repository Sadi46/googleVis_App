---
title: "googleVis_App"
subtitle: "a peer graded assignment"
output:
  html_document: default
  html_notebook: default
---

This Shiny application is an (unfinished!) tool to keep learned R skills in one place 
and to work with different datasets in a generalized way to

- load and read data
- manipulate data
- visualize data

 
Quick start 
========================================================

- Pick an included dataset to visualize or download your own dataset.
- Manipulate the data any way you want.   
- Subset the final dataset and go to the googleVis tab to visualize graphics.   
- For now, click on `enable` in the googleVis table to see options, as the app is not yet finished.

<small>
I usually first want to know the size of the dataset, the variable names, make sure the names are correctly written for R, the variable classes and if the dataset contains missing data. The custom function `peak()` does that automatically (in case of `unknown/url` data, it reads only 30 rows to help decide if and how to download the complete dataset). </small> 
 



Using and Changing
========================================================

<br></br>
This is a beginners project under development. 
Most of the code was built by many others 
(as R is developed on the shoulders of thousands) and adapted to my needs.

I would like to appreciate the expertise of some giants though:  
https://github.com/hadley   
https://github.com/jcheng5

This application may not contain the best possible code and is unfinished,
so feel free to use and change it to your needs.
I have tried to include resources within the code files where ever I used custom functions from other people. 
Most resources can be used according to terms under the [Apache License, Version 2.](https://opensource.org/licenses/Apache-2.0),
or according to terms described in the [creative commons Attribution 3.0 License](http://creativecommons.org/licenses/by/3.0/).
GoogleVis portions are created by "Google" and shared under terms described [here](https://developers.google.com/terms/)
    
<small>See https://www.coursera.org/learn/data-products and the links in the application for more info on developing data products</small> 


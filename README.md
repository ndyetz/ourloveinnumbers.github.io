# Welcome to the repository for Neil & Karina's website!

This repository stores all code used to create a website containing fun charts that help to illustrate our relationship. We utilized R and R-markdown to create the website. The main goal of this project was to create a website that showed off trends in my fiancee (to be my wife on August 2nd!) and my relationship. We had a lot of fun making it and I will detail some of the components below.

## Link to the website
The live website is available here: https://ndyetz.github.io/ourloveinnumbers.github.io/

# Information on each of the website's tabs

We divided the segments of our relationship based on data we were able to obtain I will describe some general methods for each tab of the website below:

- **Home Page**: I copied the text font from Theknot.com's wedding website for us. I was focused on making the front page feel like it was part of theknot.com's website interface. In the image of us together, I overlayed a few charts from the other tabs and put a high amount of opacity on them. The charts overlayed are the texting word cloud, quantity of texts over time and the S&P 500 market chart. I really just thought they looked neat when overlayed over our engagement photo.
  
- **Texting Data**: Data was obtained from Karina's T-mobile service provider and Neil's Verizon service provider. I was able to merge the two datasets to get the most information we could. Karina's T-mobile data had a great summary of the number of texts and Neil's Verizon data provided the actual words in each text. Pretty much every chart was created with ggplot2 besides the word cloud that was created using the website, worditout.com. I was able to extrapolate words using the stringr package.
  
- **Travel Data**: I created a collage of all of the places we visited and, with the help of Karina, formed all of the longitude and lattitude points of the places we have visited since we started dating (SO their may even be some places missing!). The map was created using ggmap::google_way().
  
- **Snowboarding Data**: Luckily we get chairlift data automatically tracked from our Mt. Hood Meadows season pass! I created the lollipop charts via ggplot2 and the treemaps were created using plotly::plot_ly(). I would like to note that I would have used ggplot2 for the treemaps - however - I could not get interactivity with the treemap extension package of ggplot2. The map showing the marker where we go engaged was created using ggmap:googleway().
  
- **Discretionary Spending**: We obtained data for these treemaps by downloading all of our credit card and bank statements. We both categorized our spending habits by hand (admittedly, Karina did *most* of this work) and then converted it all to a percentage of spending. The charts we created via plotly::plot_ly(). As noted in the tab on the website, we purposely did nto include "boring" spending (i.e. mortgage, student loans, etc.). This tab probably took the most leg work to create and is likely the least reproducible report. But it is still a fun one!
  
- **Market Comparisons**: Data was taken directly the Wall Street Journal. Simply put, Karina grabbed me the data, and I graphed it via ggplot2 and added some of our relationship milestones (All dates I created are found in [source_functions.R](https://github.com/ndyetz/ourloveinnumbers.github.io/blob/main/source_functions.R)).
  
- **Deeper Dive**: Admittedly, I think we both had the most fun with the charts in this tab. This section can be split into three sepaarte analyses:  
  ---1.) ***Market Prediction and Texting*** - Neil ran an exploratory regression analysis on our text quantity, time, and the S&P500 and found a really silly and relationship between the predictor and outcome variables. The interaction charts were created using ggeffect::ggpredict() to visually show the interaction effect in the models.  
  ---2.) ***CAPM Betas*** - Karina knew the most about CAPM betas and served as the content expert. If you're interested in learning more about what a Beta is in finance, [Click Here](https://corporatefinanceinstitute.com/resources/valuation/what-is-beta-guide/#:~:text=The%20beta%20(%CE%B2)%20of%20an,Asset%20Pricing%20Model%20(CAPM).). I found some great resources to calculate a portfolio beta compared to the S&P500 and created a program that is available on this github page (see [beta_calc.R](https://github.com/ndyetz/ourloveinnumbers.github.io/blob/main/beta_calc.R)). Karina and I had to use some teamwork on this analysis - which was pretty great! I had the expertise to create a program that automatically creates beta values based on the S&P 500 during a specific date range and utilzied Karina's expertise to help make sure the program was runnign as expected (A real team-work driven analysis!).  
  ---3.) ***Macro Spending Trends*** - Lastly, we pulled customer spending trends from the  U.S. Bureau of Economic Analysis to simply provide the honest answer that customer spending rising over time is a more likely explanation for these trends (i.e. the confounder we are purposely ignoring for fun). I think it is a great ending to the website.

# Conclusion
And that's pretty much it! Thanks for taking a look at the Github! Highly recommend making an awesome website for your relationship that analyzes trends! I'll probably continue updating the Snowboarding and Texting Data analyses for fun. I am curious how much our snowboarding data will differ from season to season! 

Feel free to reach out if you have any questions about this repository! If you are one of my family/friends reading this then I love you!

Cheers,  
Neil




# Load packages
library(zoo)
library(kableExtra)
library(tidyverse)
library(readxl)
library(lubridate)
library(hms)
library(plotly)
library(ggmap)
library(googleway)
library(scales)
library(manipulateWidget)
library(writexl)
library(pals)
library(ggiraph)
library(ggiraphExtra)
library(ggeffects)


# Special Dates - Keep at top - add any more special ones!

`Tinder Match`     <- as_date("2022-09-30") # Tinder Match
`First Kiss`       <- as_date("2022-10-12") #Our first kiss
`New York`         <- as_date("2022-10-24") #First trip to New York
`Snowboarding`     <- as_date("2022-12-09") #First time snowboarding together
`Meet Parents`     <- as_date("2022-12-31") #Karina Meets Neil's parents
Relationship       <- as_date("2023-01-03") #We form a relationship
`Neil Meets Ben`   <- as_date("2023-01-16") #First time Neil meets Ben
`Build Furniture`  <- as_date("2023-03-10") #First time building IKEA Furniture together
`Duck Fence`       <- as_date("2023-04-23") #Duck Fence built together
Chicago            <- as_date("2023-04-26") #Chicago Trip
`Duck Pond`        <- as_date("2023-05-07") #Built Duck pond together
`Dirt Bikes`       <- as_date("2023-06-02") #Buy dirt bikes
`Neil moves in`    <- as_date("2023-08-17") #Neil Moves in with Karina
`Scuba Diving`     <- as_date("2023-09-23") #First time scuba diving together
`We Get Engaged!`  <- as_date("2024-02-17") #Engagement date
`Tahiti Trip`      <- as_date("2024-04-05") #Tahiti Trip
`We Buy Property`  <- as_date("2024-08-22") #Close on La Pine Property


#Custom Functions

#Just found it annoying the way I had to suppress unnecessary warnings in read_csv and read_excel, so creating function to get rid of multiple functions
suppress <- function(x){
  suppressWarnings(suppressMessages(x))
}

# Excel sheets have different specs, creating function to help handle the differences and make the code easier to read.
my_import <- function(start = 1, end = Inf, skip_lines = 0, path = paste0(getwd(), "/random data for fun charts.xlsx")){ # <- utilizes specific file name. CHANGE IF FILENAME CHANGES!
  lapply(path,function(x) {
    sheets <- excel_sheets(x)
    dfs <- lapply(sheets, function(y) {
      suppress(read_excel(x, sheet = y,skip=skip_lines, col_names = TRUE))  
    })
    names(dfs) <- sheets
    dfs[start:end]
  })[[1]]
}

##match_paste() will match names based on a matching variable
match_paste <- function(v) {
  Reduce(f=paste, x = v)
}


#ggplot formatting for Line charts quant - MAKE SURE #SPECIAL DATES ARE CORRECT AT TOP!

line_plots_quant <- function(data, x, y) {
  ggplot(data, aes({{x}}, {{y}}, color = `Sent by`)) + #<- only works if variable is labeled `Sent by`!
    geom_line(linewidth = .6) + 
    theme_classic() +
    scale_y_continuous(expand = c(0, 0, .14, 0)) +
    labs(title="Number of texts sent by us since the start of our relationship", x="") +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))  +
    scale_x_date(date_labels="%b %Y",date_breaks  ="1 month") +
    
    geom_point(aes(x = (`First Kiss`)     , y = max({{y}}) , color = "First Kiss"      ), shape=18, size = 4) +
    geom_point(aes(x = (`New York`)       , y = max({{y}}) , color = "New York"        ), shape=18, size = 4) +
    geom_point(aes(x = (`Snowboarding`)   , y = max({{y}}) , color = "Snowboarding"    ), shape=18, size = 4) +
    geom_point(aes(x = (`Meet Parents`)   , y = max({{y}}) , color = "Meet Parents"    ), shape=18, size = 4) +
    geom_point(aes(x = (Relationship)     , y = max({{y}}) , color = "Relationship"    ), shape=18, size = 4) +
    geom_point(aes(x = (`Neil Meets Ben`) , y = max({{y}}) , color = "Neil Meets Ben"  ), shape=18, size = 4) +
    geom_point(aes(x = (`Build Furniture`), y = max({{y}}) , color = "Build Furniture" ), shape=18, size = 4) +
    geom_point(aes(x = (`Duck Fence`)     , y = max({{y}}) , color = "Duck Fence"      ), shape=18, size = 4) +
    geom_point(aes(x = (`Chicago`)        , y = max({{y}}) , color = "Chicago"         ), shape=18, size = 4) +
    geom_point(aes(x = (`Duck Pond`)      , y = max({{y}}) , color = "Duck Pond"       ), shape=18, size = 4) +
    geom_point(aes(x = (`Dirt Bikes`)     , y = max({{y}}) , color = "Dirt Bikes"      ), shape=18, size = 4) +
    geom_point(aes(x = (`Neil moves in` ) , y = max({{y}}) , color = "Neil moves in"   ), shape=18, size = 4) +
    geom_point(aes(x = (`Scuba Diving` )  , y = max({{y}}) , color = "Scuba Diving"    ), shape=18, size = 4) +
    geom_point(aes(x = (`We Get Engaged!`), y = max({{y}}) , color = "We Get Engaged!" ), shape=18, size = 4) +   
    geom_point(aes(x = (`Tahiti Trip`)    , y = max({{y}}) , color = "Tahiti Trip"     ), shape=18, size = 4) +
    geom_point(aes(x = (`We Buy Property`), y = max({{y}}) , color = "We Buy Property" ), shape=18, size = 4) +
    
    
    geom_segment(aes(x=       (`First Kiss`)            ,         xend=   (`First Kiss`)                          , y=0, yend= {{y}}, color =                            "First Kiss"                        ), linewidth = .05) +
    geom_segment(aes(x=       (`New York`)              ,         xend=   (`New York`)                            , y=0, yend= {{y}}, color =                            "New York"                          ), linewidth = .05) +
    geom_segment(aes(x=       (`Snowboarding`)          ,         xend=   (`Snowboarding`)                        , y=0, yend= {{y}}, color =                            "Snowboarding"                      ), linewidth = .05) +
    geom_segment(aes(x=       (`Meet Parents`)          ,         xend=   (`Meet Parents`)                        , y=0, yend= {{y}}, color =                            "Meet Parents"                      ), linewidth = .05) +
    geom_segment(aes(x=       (Relationship)            ,         xend=   (Relationship)                          , y=0, yend= {{y}}, color =                            "Relationship"                      ), linewidth = .05) +
    geom_segment(aes(x=       (`Neil Meets Ben`)        ,         xend=   (`Neil Meets Ben`)                      , y=0, yend= {{y}}, color =                            "Neil Meets Ben"                    ), linewidth = .05) +
    geom_segment(aes(x=       (`Build Furniture`)       ,         xend=   (`Build Furniture`)                     , y=0, yend= {{y}}, color =                            "Build Furniture"                   ), linewidth = .05) +
    geom_segment(aes(x=       (`Duck Fence`)            ,         xend=   (`Duck Fence`)                          , y=0, yend= {{y}}, color =                            "Duck Fence"                        ), linewidth = .05) +
    geom_segment(aes(x=       (`Chicago`)               ,         xend=   (`Chicago`)                             , y=0, yend= {{y}}, color =                            "Chicago"                           ), linewidth = .05) +
    geom_segment(aes(x=       (`Duck Pond`)             ,         xend=   (`Duck Pond`)                           , y=0, yend= {{y}}, color =                            "Duck Pond"                         ), linewidth = .05) +
    geom_segment(aes(x=       (`Dirt Bikes`)            ,         xend=   (`Dirt Bikes`)                          , y=0, yend= {{y}}, color =                            "Dirt Bikes"                        ), linewidth = .05) +
    geom_segment(aes(x=       (`Neil moves in` )        ,         xend=   (`Neil moves in` )                      , y=0, yend= {{y}}, color =                            "Neil moves in"                     ), linewidth = .05) +
    geom_segment(aes(x=       (`Scuba Diving` )         ,         xend=   (`Scuba Diving` )                       , y=0, yend= {{y}}, color =                            "Scuba Diving"                      ), linewidth = .05) +
    geom_segment(aes(x=       (`We Get Engaged!`)       ,         xend=   (`We Get Engaged!`)                     , y=0, yend= {{y}}, color =                            "We Get Engaged!"                   ), linewidth = .05) +
    geom_segment(aes(x=       (`Tahiti Trip`)           ,         xend=   (`Tahiti Trip`)                         , y=0, yend= {{y}}, color =                            "Tahiti Trip"                       ), linewidth = .05) +
    geom_segment(aes(x=       (`We Buy Property`)       ,         xend=   (`We Buy Property`)                     , y=0, yend= {{y}}, color =                            "We Buy Property"                   ), linewidth = .05) 
  
}


#ggplot formatting for Line charts words - MAKE SURE #SPECIAL DATES ARE CORRECT AT TOP!

line_plots_words <- function(data, x, y) {
  ggplot(data, aes({{x}}, {{y}}, color = `Word(s)`)) +
    geom_line(linewidth = .6) + 
    theme_classic() +
    scale_y_continuous(expand = c(0, 0, .14, 0)) +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))  +
    scale_x_date(date_labels="%b %Y",date_breaks  ="1 month") +
    labs(title = "How our usage of words has changed over time",
         x = "") +
    geom_point(aes(x = (`First Kiss`)     , y = max(`Count of particular word(s) used in our texts`) , color = "First Kiss"      ), shape=18, size = 4) +
    geom_point(aes(x = (`New York`)       , y = max(`Count of particular word(s) used in our texts`) , color = "New York"        ), shape=18, size = 4) +
    geom_point(aes(x = (`Snowboarding`)   , y = max(`Count of particular word(s) used in our texts`) , color = "Snowboarding"    ), shape=18, size = 4) +
    geom_point(aes(x = (`Meet Parents`)   , y = max(`Count of particular word(s) used in our texts`) , color = "Meet Parents"    ), shape=18, size = 4) +
    geom_point(aes(x = (Relationship)     , y = max(`Count of particular word(s) used in our texts`) , color = "Relationship"    ), shape=18, size = 4) +
    geom_point(aes(x = (`Neil Meets Ben`) , y = max(`Count of particular word(s) used in our texts`) , color = "Neil Meets Ben"  ), shape=18, size = 4) +
    geom_point(aes(x = (`Build Furniture`), y = max(`Count of particular word(s) used in our texts`) , color = "Build Furniture" ), shape=18, size = 4) +
    geom_point(aes(x = (`Duck Fence`)     , y = max(`Count of particular word(s) used in our texts`) , color = "Duck Fence"      ), shape=18, size = 4) +
    geom_point(aes(x = (`Chicago`)        , y = max(`Count of particular word(s) used in our texts`) , color = "Chicago"         ), shape=18, size = 4) +
    geom_point(aes(x = (`Duck Pond`)      , y = max(`Count of particular word(s) used in our texts`) , color = "Duck Pond"       ), shape=18, size = 4) +
    geom_point(aes(x = (`Dirt Bikes`)     , y = max(`Count of particular word(s) used in our texts`) , color = "Dirt Bikes"      ), shape=18, size = 4) +
    geom_point(aes(x = (`Neil moves in` ) , y = max(`Count of particular word(s) used in our texts`) , color = "Neil moves in"   ), shape=18, size = 4) +
    geom_point(aes(x = (`Scuba Diving` )  , y = max(`Count of particular word(s) used in our texts`) , color = "Scuba Diving"    ), shape=18, size = 4) +
    geom_point(aes(x = (`We Get Engaged!`), y = max(`Count of particular word(s) used in our texts`) , color = "We Get Engaged!" ), shape=18, size = 4) +   
    geom_point(aes(x = (`Tahiti Trip`)    , y = max(`Count of particular word(s) used in our texts`) , color = "Tahiti Trip"     ), shape=18, size = 4) +
    geom_point(aes(x = (`We Buy Property`), y = max(`Count of particular word(s) used in our texts`) , color = "We Buy Property" ), shape=18, size = 4) +
    
    
    geom_segment(aes(x=       (`First Kiss`)            ,    xend=   (`First Kiss`)        , y=0, yend= `Count of particular word(s) used in our texts` , color =     "First Kiss"          ), linewidth = .05) +
    geom_segment(aes(x=       (`New York`)              ,    xend=   (`New York`)          , y=0, yend= `Count of particular word(s) used in our texts` , color =     "New York"          ), linewidth = .05) +
    geom_segment(aes(x=       (`Snowboarding`)          ,    xend=   (`Snowboarding`)      , y=0, yend= `Count of particular word(s) used in our texts` , color =     "Snowboarding"        ), linewidth = .05) +
    geom_segment(aes(x=       (`Meet Parents`)          ,    xend=   (`Meet Parents`)      , y=0, yend= `Count of particular word(s) used in our texts` , color =     "Meet Parents"        ), linewidth = .05) +
    geom_segment(aes(x=       (Relationship)            ,    xend=   (Relationship)        , y=0, yend= `Count of particular word(s) used in our texts` , color =     "Relationship"        ), linewidth = .05) +
    geom_segment(aes(x=       (`Neil Meets Ben`)        ,    xend=   (`Neil Meets Ben`)    , y=0, yend= `Count of particular word(s) used in our texts` , color =     "Neil Meets Ben"      ), linewidth = .05) +
    geom_segment(aes(x=       (`Build Furniture`)       ,    xend=   (`Build Furniture`)   , y=0, yend= `Count of particular word(s) used in our texts` , color =     "Build Furniture"     ), linewidth = .05) +
    geom_segment(aes(x=       (`Duck Fence`)            ,    xend=   (`Duck Fence`)        , y=0, yend= `Count of particular word(s) used in our texts` , color =     "Duck Fence"          ), linewidth = .05) +
    geom_segment(aes(x=       (`Chicago`)               ,    xend=   (`Chicago`)           , y=0, yend= `Count of particular word(s) used in our texts` , color =     "Chicago"             ), linewidth = .05) +
    geom_segment(aes(x=       (`Duck Pond`)             ,    xend=   (`Duck Pond`)         , y=0, yend= `Count of particular word(s) used in our texts` , color =     "Duck Pond"           ), linewidth = .05) +
    geom_segment(aes(x=       (`Dirt Bikes`)            ,    xend=   (`Dirt Bikes`)        , y=0, yend= `Count of particular word(s) used in our texts` , color =     "Dirt Bikes"          ), linewidth = .05) +
    geom_segment(aes(x=       (`Neil moves in` )        ,    xend=   (`Neil moves in` )    , y=0, yend= `Count of particular word(s) used in our texts` , color =     "Neil moves in"       ), linewidth = .05) +
    geom_segment(aes(x=       (`Scuba Diving` )         ,    xend=   (`Scuba Diving` )     , y=0, yend= `Count of particular word(s) used in our texts` , color =     "Scuba Diving"        ), linewidth = .05) +
    geom_segment(aes(x=       (`We Get Engaged!`)       ,    xend=   (`We Get Engaged!`)   , y=0, yend= `Count of particular word(s) used in our texts` , color =     "We Get Engaged!"     ), linewidth = .05) +
    geom_segment(aes(x=       (`Tahiti Trip`)           ,    xend=   (`Tahiti Trip`)       , y=0, yend= `Count of particular word(s) used in our texts` , color =     "Tahiti Trip"         ), linewidth = .05) +
    geom_segment(aes(x=       (`We Buy Property`)       ,    xend=   (`We Buy Property`)   , y=0, yend= `Count of particular word(s) used in our texts` , color =     "We Buy Property"     ), linewidth = .05) 
}




# Sets text in hover and range of slider for text quantity charts - MAKE SURE #SPECIAL DATES ARE CORRECT AT TOP!
hover_range_quant <- function(data) {
  ggplotly(data)%>% 
    # layout(hovermode = 'x') %>%
    style(hovertext = paste0("Our first kiss was on "                                             , format(as_date(`First Kiss`), "%b %d, %Y"), "!")                                                                                      , traces = 3  ) %>% 
    style(hovertext = paste0("We take our first trip to New York together on "                    , format(as_date(`New York`), "%b %d, %Y"), ". \nThis is also the first time Neil Met Karina's parents.")                               , traces = 4  ) %>% 
    style(hovertext = paste0("Our first time ever snowboarding together was on "                  , format(as_date(`Snowboarding`), "%b %d, %Y"), ". \nNeil proved he can shred to Karina!")                                              , traces = 5  ) %>% 
    style(hovertext = paste0("Karina flies to Alpine, CA to meet Neil's parents on "              , format(as_date(`Meet Parents`), "%b %d, %Y"), ". \nWe played card games all New Years Eve and kissed at midnight!")                   , traces = 6  ) %>% 
    style(hovertext = paste0("We make it official, we decide we want to be in a relationship on " , format(as_date(Relationship), "%b %d, %Y"), '. \nWe also said "I love you" in person for the first time on this day.')                , traces = 7  ) %>% 
    style(hovertext = paste0("We go snow boarding with Karina's son, Ben, on "                    , format(as_date(`Neil Meets Ben`), "%b %d, %Y"), ". It's the first time Neil met Ben.")                                                , traces = 8  ) %>% 
    style(hovertext = paste0("We build our first piece of IKEA furniture together on "            , format(as_date(`Build Furniture`), "%b %d, %Y"), ". Safe to say we fight while building it.")                                         , traces = 9  ) %>% 
    style(hovertext = paste0("We built a duck fence on "                                          , format(as_date(`Duck Fence`), "%b %d, %Y"), ". Our first outside project!")                                                           , traces = 10 ) %>% 
    style(hovertext = paste0("We take a trip to Chicago on "                                      , format(as_date(`Chicago`), "%b %d, %Y"), ". Neil got to meet some of Karina's grad school peers.")                                    , traces = 11 ) %>% 
    style(hovertext = paste0("We built a duck pond on "                                           , format(as_date(`Duck Pond`), "%b %d, %Y"), ". A continuation of the duck fence project.")                                             , traces = 12 ) %>% 
    style(hovertext = paste0("We bought our dirt bikes on "                                       , format(as_date(`Dirt Bikes`), "%b %d, %Y"), ". We each have a Yamaha TTR230 and we love them!")                                       , traces = 13 ) %>% 
    style(hovertext = paste0("Neil moved in with Karina on "                                      , format(as_date(`Neil moves in`), "%b %d, %Y"), ". Notice how our average number of texts sent each week goes down.")                  , traces = 14 ) %>% 
    style(hovertext = paste0("We go scuba diving for the first time in Key Largo, FL on "         , format(as_date(`Scuba Diving`), "%b %d, %Y"), "!")                                                                                    , traces = 15 ) %>%
    style(hovertext = paste0("We got engaged on "                                                 , format(as_date(`We Get Engaged!`), "%b %d, %Y"), "! Neil hired a photographer to capture the moment at Mt. Hood Meadows ski resort.") , traces = 16 ) %>%
    style(hovertext = paste0("We take our first trip as an engaged coupel to Tahiti on "          , format(as_date(`Tahiti Trip`), "%b %d, %Y"), ". We sailed on a Yach to different islabds for 10 days!")                               , traces = 17 ) %>% 
    style(hovertext = paste0("We bought a 1 acre piece of land in La Pine, OR on "                , format(as_date(`We Buy Property`), "%b %d, %Y"), ". Our plan is to build a getaway cabin on the land!")                               , traces = 18 ) %>% 
    style(hoverinfo = "none", traces = 19:34) %>% 
    rangeslider(start = min(as.numeric(texts_quant_graph$week_of)) - 30, end = max(as.numeric(texts_quant_graph$week_of)) + 50, thickness = ".05")  
}


# Sets text in hover and range of slider for word charts - MAKE SURE #SPECIAL DATES ARE CORRECT AT TOP!
hover_range_words <- function(data) {
  ggplotly(data)%>% 
    #  layout(hovermode = 'x') %>%
    style(hovertext = paste0("Our first kiss was on "                                            , format(as_date(`First Kiss`), "%b %d, %Y"), " !")                                                                                     , traces = 8  ) %>% 
    style(hovertext = paste0("We take our first trip to New York together on "                   , format(as_date(`New York`), "%b %d, %Y"), ". \nThis is also the first time Neil Met Karina's parents.")                               , traces = 9  ) %>% 
    style(hovertext = paste0("Our first time ever snowboarding together was on "                 , format(as_date(`Snowboarding`), "%b %d, %Y"), ". \nNeil proved he can shred to Karina!")                                              , traces = 10  ) %>% 
    style(hovertext = paste0("Karina flies to Alpine, CA to meet Neil's parents on "             , format(as_date(`Meet Parents`), "%b %d, %Y"), ". \nWe played card games all New Years Eve and kissed at midnight!")                   , traces = 11  ) %>% 
    style(hovertext = paste0("We make it official, we decide we want to be in a relationship on" , format(as_date(Relationship), "%b %d, %Y"), '. \nWe also said "I love you" in person for the first time on this day.')                , traces = 12  ) %>% 
    style(hovertext = paste0("We go snow boarding with Karina's son, Ben, on "                   , format(as_date(`Neil Meets Ben`), "%b %d, %Y"), ". It's the first time Neil met Ben.")                                                , traces = 13  ) %>% 
    style(hovertext = paste0("We build our first piece of IKEA furniture together on "           , format(as_date(`Build Furniture`), "%b %d, %Y"), ". Safe to say we fight while building it.")                                         , traces = 14  ) %>% 
    style(hovertext = paste0("We built a duck fence on "                                         , format(as_date(`Duck Fence`), "%b %d, %Y"), ". Our first outside project!")                                                           , traces = 15 ) %>% 
    style(hovertext = paste0("We take a trip to Chicago on "                                     , format(as_date(`Chicago`), "%b %d, %Y"), ". Neil got to meet some of Karina's grad school peers.")                                    , traces = 16 ) %>% 
    style(hovertext = paste0("We built a duck pond on "                                          , format(as_date(`Duck Pond`), "%b %d, %Y"), ". A continuation of the duck fence project.")                                             , traces = 17 ) %>% 
    style(hovertext = paste0("We bought our dirt bikes on "                                      , format(as_date(`Dirt Bikes`), "%b %d, %Y"), ". We each have a Yamaha TTR230 and we love them!")                                       , traces = 18 ) %>% 
    style(hovertext = paste0("Neil moved in with Karina on "                                     , format(as_date(`Neil moves in`), "%b %d, %Y"), ".")                                                                                   , traces = 19 ) %>% 
    style(hovertext = paste0("We go scuba diving for the first time in Key Largo, FL on "        , format(as_date(`Scuba Diving`), "%b %d, %Y"), "!")                                                                                    , traces = 20 ) %>%
    style(hovertext = paste0("We got engaged on "                                                , format(as_date(`We Get Engaged!`), "%b %d, %Y"), "! Neil hired a photographer to capture the moment at Mt. Hood Meadows ski resort.") , traces = 21 ) %>%
    style(hovertext = paste0("We take our first trip as an engaged coupel to Tahiti on"          , format(as_date(`Tahiti Trip`), "%b %d, %Y"), ". We sailed on a Yach to different islabds for 10 days!")                               , traces = 22 ) %>% 
    style(hovertext = paste0("We bought a 1 acre piece of land in La Pine, OR on "               , format(as_date(`We Buy Property`), "%b %d, %Y"), ". Our plan is to build a getaway cabin on the land!")                               , traces = 23 ) %>%
    style(hoverinfo = "none", traces = 24:39) %>% 
    rangeslider(start = min(as.numeric(texts_combine_filter_graph_total$week_of)) - 30, end = max(as.numeric(texts_combine_filter_graph_total$week_of)) + 40, thickness = .10)
}



#Extract Regression Omnibus model p-values
overall_p <- function(my_model) {
  f <- summary(my_model)$fstatistic
  p <- pf(f[1],f[2],f[3],lower.tail=F)
  attributes(p) <- NULL
  return(p)
}



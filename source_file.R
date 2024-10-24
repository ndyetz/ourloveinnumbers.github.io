
#Get all sheets from Karina's data and input it into a list. Separate by relationship data and macro Data

#Relationship data - All follow same import logic
relationship_data <- my_import(start = 2, end = 8, skip_lines = 1)

#Macro_data - custom import logic. will combine into one list at the end.
macro_data_1      <- my_import(start = 10, end = 11, skip_lines = 2)
macro_data_2      <- my_import(start = 12, end = 16, skip_lines = 10)
macro_data_3      <- my_import(start = 17, end = 17, skip_lines = 2)
macro_data_4      <- my_import(start = 18, end = 18, skip_lines = 5)

macro_data <- c(macro_data_1[1:length(macro_data_1)], macro_data_2[1:length(macro_data_2)], macro_data_3[1:length(macro_data_3)], macro_data_4[1:length(macro_data_4)])
rm(macro_data_1);rm(macro_data_2);rm(macro_data_3);rm(macro_data_4) #All sets combined to macro_data. Removing clutter

#Neil and Karina text data 
texts_qual <- suppress(read_csv("texts.csv", skip = 1)); texts_qual <- texts_qual[1:(dim(texts_qual)[1]-1),] #<- removes the unnecessary last row with advertiser message
#quantitative data from karina's tmobile account
texts_quant <- read_excel("random data for fun charts v2.xlsx", sheet = "Text Quantity Summary")


#Financial data
finance <- read_excel("cleaned_financials_for_karina with categories.xlsx")



# Preserve original names, but change to easier column names
texts_qual_og_names <- names(texts_qual); names(texts_qual) <- c("conversation_name", "title", "message", "datetime", "attachment")

#Format names & convert date time via lubridate
texts_qual <- texts_qual %>% 
  mutate(source_neil = TRUE,
         conversation_name = "Neil & Karina", 
         title = ifelse(str_detect(tolower(title), "me") == TRUE, "Neil", "Karina"),
         datetime = parse_date_time(datetime, orders = "%A %b %d, %Y %I %M %p"), 
         dow = wday(datetime, label = TRUE, abbr = FALSE),
         date = lubridate::date(datetime), 
         time = hms::as_hms(datetime)) %>% 
  group_by(date) %>% 
  arrange(date) %>% 
  mutate(text_count_by_day = seq_along(date)) %>% 
  ungroup()

#str(texts) <- Check structure to make sure date conversions worked.


#Format texts quant
texts_quant_og_names <- names(texts_quant); names(texts_quant) <- c("text_num", "date", "direction")

texts_quant <- texts_quant %>% 
  mutate(date = as_date(date)) %>%  
  group_by(date) %>% 
  arrange(date) %>% 
  mutate(text_count_by_day = seq_along(date)) %>% 
  ungroup() %>% 
  mutate(source_karina = TRUE)


#Format Texts_combine
texts_combine <- texts_qual %>% 
  full_join(texts_quant, by = c("date", "text_count_by_day")) %>% 
  arrange(date, text_count_by_day) %>% 
  mutate(source_neil = ifelse(is.na(source_neil),FALSE, source_neil),
         source_karina = ifelse(is.na(source_karina), FALSE, source_karina))



# Data Check to make sure text sums make sense. 

text_agreement <- texts_combine %>% 
  filter(source_neil == TRUE, source_karina == TRUE) %>% 
  summarize(`# of texts agreed across data sources` = n()) %>% 
  pivot_longer(`# of texts agreed across data sources`) %>% 
  rename(n = "value")

neil_false <- texts_combine %>% 
  filter(source_neil == FALSE) %>% 
  summarize(`missing texts from Neil's source data` = n()) %>% 
  pivot_longer(everything()) %>% 
  rename(n = "value")

karina_false <- texts_combine %>% 
  filter(source_karina == FALSE) %>% 
  summarize(`missing texts from Karina's source data` = n()) %>% 
  #  mutate_all(as.character) %>%
  pivot_longer(everything()) %>% 
  rename(n = "value")

total_texts_sum <- texts_combine %>% 
  summarize(name = "Sum of texts", 
            n = sum(text_agreement$n, neil_false$n, karina_false$n))

final_text_info <- bind_rows(text_agreement, neil_false, karina_false, total_texts_sum)

texts_sum_check <- texts_combine %>% 
  summarize(name = "Does this match the total number of texts? PROBLEM IF FALSE! -->", 
            n = final_text_info$n[nrow(final_text_info)] == nrow(texts_combine)) %>% 
  mutate_all(as.character)

final_text_info <- final_text_info %>% 
  mutate_all(as.character) %>% 
  bind_rows(texts_sum_check)

rm(text_agreement);rm(neil_false); rm(karina_false); rm(total_texts_sum); rm(texts_sum_check)

#final_text_info # <- Un-comment to see any oddities



#Create text_quant dataset
texts_combine <- texts_combine %>% 
  mutate(`sent by` = ifelse(direction == "Karina", "Karina", NA),
         `sent by` = ifelse(direction == "Neil", "Neil", `sent by`),
         `sent by` = ifelse(is.na(direction), title, `sent by`))

#summarize some qual data
total <- texts_combine %>% 
  summarize(`sent by` = "Combined",
            `# of texts sent` = n(),
            `average character count` = mean(nchar(message), na.rm = TRUE)
  )

grouped <- texts_combine %>% 
  group_by(`sent by`) %>% 
  summarize(`# of texts sent` = n(),
            `average character count` = mean(nchar(message), na.rm = TRUE),
  )

text_quant <- bind_rows(grouped, total)

rm(total); rm(grouped)




# Text analyses. Think of words you'd like and insert them here.

#Overall
total <- texts_combine %>% 
  filter(!is.na(title)) %>% 
  summarize(
    `Sent by` = "Total",
    #Love = sum(str_count(tolower(message), "\\blove"), na.rm = TRUE),
    `Love you` = sum(str_count(tolower(message), "love you\\b"), na.rm = TRUE),
    #`I love you` = sum(str_count(tolower(message), "i love you"), na.rm = TRUE),
    Babe = sum(str_count(tolower(message), "\\bbabe"), na.rm = TRUE),
    Baby = sum(str_count(tolower(message), "\\bbaby"), na.rm = TRUE),
    Ben = sum(str_count(tolower(message), "\\bben\\b"), na.rm = TRUE),
    Laika = sum(str_count(tolower(message), "laika"), na.rm = TRUE),
    `Good night` = sum(str_count(tolower(message), "good night"), na.rm = TRUE) + sum(str_count(tolower(message),"goodnight"), na.rm = TRUE),
    `Good morning` = sum(str_count(tolower(message), "good morning"), na.rm = TRUE),
    Lol = sum(str_count(tolower(message), "\\blol\\b"), na.rm = TRUE)
  ) 

#Separate by sender (Karina or Neil)
sent_by <- texts_combine %>% 
  filter(!is.na(title)) %>% 
  dplyr::group_by(title) %>% 
  summarize(
    # Love = sum(str_count(tolower(message), "\\blove"), na.rm = TRUE) - sum(str_count(tolower(message), "love you\\b"), na.rm = TRUE),
    `Love you` = sum(str_count(tolower(message), "love you\\b"), na.rm = TRUE) - sum(str_count(tolower(message), "i love you"), na.rm = TRUE), # inclusive of "love you" WITHOUT "I love you"
    #  `I love you` = sum(str_count(tolower(message), "i love you"), na.rm = TRUE),
    Babe = sum(str_count(tolower(message), "\\bbabe"), na.rm = TRUE),
    Baby = sum(str_count(tolower(message), "\\bbaby"), na.rm = TRUE),
    Ben = sum(str_count(tolower(message), "\\bben\\b"), na.rm = TRUE),
    Laika = sum(str_count(tolower(message), "laika"), na.rm = TRUE),
    `Good night` = sum(str_count(tolower(message), "good night"), na.rm = TRUE) + sum(str_count(tolower(message),"goodnight"), na.rm = TRUE),
    `Good morning` = sum(str_count(tolower(message), "good morning"), na.rm = TRUE), 
    Lol = sum(str_count(tolower(message), "\\blol\\b"), na.rm = TRUE)
  ) %>% 
  ungroup() %>% 
  rename(`Sent by` = "title")

final_text_search <- bind_rows(sent_by, total)

#final_text_search

rm(total); rm(sent_by)


#Filter & Combine 
texts_combine_filter <- texts_combine %>% 
  filter(!is.na(title)) %>% 
  mutate(
    #  Love = str_count(tolower(message), "\\blove"),
    `Love you` = (str_count(tolower(message), "love you\\b") - str_count(tolower(message), "i love you")), 
    #`I love you` = str_count(tolower(message), "i love you"),
    Babe = str_count(tolower(message), "\\bbabe"),
    Baby = str_count(tolower(message), "\\bbaby"),
    Ben = str_count(tolower(message), "\\bben\\b"),
    Laika = str_count(tolower(message), "laika"),
    `Good night` = str_count(tolower(message), "good night") + str_count(tolower(message),"goodnight"),
    `Good morning` = str_count(tolower(message), "good morning"),
    `Lol` = str_count(tolower(message), "\\blol\\b"),
    week_of = date,
    week_of = ifelse(dow == "Sunday", week_of, NA)) %>% 
  fill(week_of, .direction = "down") %>% 
  mutate(
    week_of = ifelse(!is.na(date) & is.na(week_of), as_date("2022-10-09"), week_of),
    week_of = as_date(week_of)
  )



#Quant across time graph
texts_quant <- texts_quant %>% 
  mutate(
    week_of = as_date(date),
    dow = wday(date, label = TRUE, abbr = FALSE),
    week_of = as_date(ifelse(dow == "Sunday", week_of, NA))) %>% 
  fill(week_of, .direction = "down") %>% 
  mutate(
    week_of = ifelse(!is.na(date) & is.na(week_of), as_date("2022-10-09"), week_of),
    week_of = as_date(week_of)
  )

texts_quant_graph  <- texts_quant  %>% 
  arrange(direction, week_of) %>% 
  dplyr::group_by(direction, week_of, .add = TRUE) %>% 
  summarize( `# of texts sent` = n(), .groups = "keep") %>% 
  ungroup() %>% 
  rename(`Sent by` = "direction")


#Character COunt of texts, Instead of "characters" say "letters? for comprehension?
texts_char_time <- texts_combine_filter %>% 
  arrange(title, week_of) %>% 
  dplyr::group_by(title, week_of, .add = TRUE) %>% 
  summarize(`Average character count` = mean(nchar(message), na.rm = TRUE), .groups = "keep") %>% 
  ungroup() %>% 
  rename(`Sent by` = "title") %>% 
  filter(week_of != as_date("2022-10-09"))


#Check if things make sense
#QA Check

text_quant_graph <- text_quant %>% 
  pivot_longer(-`sent by`, values_to = "value") %>% 
  filter(`sent by` != "Combined" & name != "average character count") %>% 
  select(`Sent by` = "sent by", `# of texts sent` = "value") 



#text_quant %>% 
# kbl(digits = 0) %>%
#kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive")) # <- Un-comment these 3 lines to view table and look for errors


# Overall character count
text_character_graph <- text_quant %>% 
  pivot_longer(-`sent by`, values_to = "value") %>% 
  filter(`sent by` != "Combined" & name != "# of texts sent") %>% 
  select(`Sent by` = "sent by", `Average number of characters in text` = "value") 

texts_combine_filter_graph_groups  <- texts_combine_filter %>% 
  arrange(title, week_of) %>% 
  dplyr::group_by(title, week_of, .add = TRUE) %>% 
  summarize(
    #`Love` = sum(Love, na.rm = TRUE), 
    `Love you` = sum(`Love you`, na.rm = TRUE),
    #`I love you` = sum(`I love you`, na.rm = TRUE),
    `Babe` = sum(`Babe`, na.rm = TRUE),
    `Baby` = sum(`Baby`, na.rm = TRUE),
    `Ben` = sum(`Ben`, na.rm = TRUE),
    `Laika` = sum(`Laika`, na.rm = TRUE),
    `Good morning` = sum(`Good morning`, na.rm = TRUE),
    `Good night` = sum(`Good night`, na.rm = TRUE),
    `Lol` = sum(`Lol`, na.rm = TRUE),
    .groups = "keep") %>% 
  ungroup() %>% 
  pivot_longer(cols = `Love you`:`Lol`, names_to = "word") %>% 
  mutate(word = factor(word, levels = c("Baby",  "Babe", "Love you", "Ben", "Laika", "Good morning", "Good night", "Lol"),    labels = c(1,2,3,4,5,6,7,8)),
         word = factor(word, labels = c("Baby",  "Babe", "Love you", "Ben", "Laika", "Good morning", "Good night", "Lol" ),  levels = c(1,2,3,4,5,6,7,8)),
  )%>% 
  rename(`Word(s)` = "word", `Count of particular word(s) used in our texts` = "value")


texts_combine_filter_graph_total  <- texts_combine_filter %>% 
  dplyr::group_by(week_of) %>% 
  summarize(title = "Combined",
            #  `Love` = sum(Love, na.rm = TRUE), 
            `Love you` = sum(`Love you`, na.rm = TRUE),
            #  `I love you` = sum(`I love you`, na.rm = TRUE),
            `Babe` = sum(`Babe`, na.rm = TRUE),
            `Baby` = sum(`Baby`, na.rm = TRUE),
            `Ben` = sum(`Ben`, na.rm = TRUE),
            `Laika` = sum(`Laika`, na.rm = TRUE),
            `Good morning` = sum(`Good morning`, na.rm = TRUE),
            `Good night` = sum(`Good night`, na.rm = TRUE),
            `Lol` = sum(`Lol`, na.rm = TRUE)
  ) %>% 
  pivot_longer(cols = `Love you`:`Lol`, names_to = "word") %>% 
  mutate(word = factor(word, levels = c( "Baby","Babe",  "Love you",  "Ben", "Laika", "Good morning","Good night"  , "Lol"),  labels = c(1,2,7,4,5,6,3,8)),
         word = factor(word, labels = c("Baby" ,"Babe",  "Love you",  "Ben", "Laika", "Good morning", "Good night" , "Lol" ),  levels = c(1,2,3,4,5,6,7,8)),
  ) %>% 
  rename(`Word(s)` = "word", `Count of particular word(s) used in our texts` = "value")


texts_combine_filter_graph <- bind_rows(texts_combine_filter_graph_groups, texts_combine_filter_graph_total)



# Hood runs data
hood_runs <- relationship_data$`Snowboarding Runs at Meadows`

#Format
hood_runs <- hood_runs %>% 
  filter(str_detect(Runs, ".*[0-9].*") == FALSE) %>% 
  mutate(
    Date = parse_date_time(Date, orders = "%m/%d/%y%H:%M:%S"),
    Date2 = lubridate::date(Date)
  )


#Total runs & Vertical feet
hood_runs_tree <- hood_runs %>% 
  group_by(Runs) %>% 
  summarize(`Total Runs` = n(),
            `Vertical Ft. Gained` = sum(`Vertical Ft.`))


## Vertical feet and runs by date for lollipop graph
#Vertical feet
hood_vert_graph <- hood_runs %>% 
  group_by(Date2) %>% 
  summarize(`Vertical Ft. Gained` = sum(`Vertical Ft.`, na.rm = TRUE)) %>% 
  filter(Date2 >= Snowboarding) %>% 
  ungroup() %>% 
  mutate(Season = ifelse(Date2 < as_date("2023-05-27"), "2022-23 Season", "2023-24 Season")) %>% 
  select(Date = "Date2", everything()) 


max_date_vert <- hood_vert_graph %>% 
  filter(`Vertical Ft. Gained` == max(`Vertical Ft. Gained`)) %>% 
  select(Date)


proposal_date_vert <- hood_vert_graph %>% 
  filter(Date == as_date(`We Get Engaged!`)) %>% 
  select(`Vertical Ft. Gained`)

#Runs
hood_runs_graph <- hood_runs %>% 
  group_by(Date2) %>% 
  summarize(`Total Runs` = n()) %>% 
  filter(Date2 >= Snowboarding) %>% 
  ungroup() %>% 
  mutate(Season = ifelse(Date2 < as_date("2023-05-27"), "2022-23 Season", "2023-24 Season")) %>% 
  select(Date = "Date2", everything()) 


max_date_runs <- hood_runs_graph %>% 
  filter(`Total Runs` == max(`Total Runs`)) %>% 
  select(Date)

proposal_date_runs <- hood_runs_graph %>% 
  filter(Date == as_date(`We Get Engaged!`)) %>% 
  select(`Total Runs`)





#SP500 Absolute units

sp500 <- macro_data$`S&P 500`

texts_count_date <- texts_quant %>% 
  group_by(date) %>% 
  summarize(n_texts = n()) %>% 
  rename(Date = "date") 


sp_500_filter <- sp500 %>% 
  filter(Date > min(texts_count_date$Date) & Date < max(texts_count_date$Date))


textcount_sp500 <- texts_count_date %>%
  inner_join(sp_500_filter, by = "Date")



model <- lm( `Close/Last` ~ n_texts + Date, data = textcount_sp500)

confs <- confint(model)
mod_sum <- summary(model)

# Get Beta value 
estimate      <- as_tibble(summary(model)$coefficients[2])
estimate      <- estimate %>% rename(`Unstandardized Beta` = "value") %>%  mutate(name = "sp500"); 
estimatefinal <- estimate

#Get R
r      <- as_tibble(cor(textcount_sp500$n_texts, textcount_sp500$`Close/Last`)); 
r      <- r %>% rename(R = "value") %>%  mutate(name = "sp500"); 
rfinal <- r

#Get R^2
r2      <- as_tibble(summary(model)$r.squared); 
r2      <- r2 %>% rename(`R-squared` = "value" ) %>%  mutate(name = "sp500"); 
r2final <- r2

#Get f
f <- as_tibble(summary(model)$fstatistic[1])
f <- f %>% rename(`F-value` = "value") %>% mutate(name = "sp500")
ffinal <- f

#Get Numerator DF
numdf <- as_tibble(summary(model)$fstatistic[2])
numdf <- numdf %>% rename(`Numerator DF` = "value") %>% mutate(name = "sp500")
numdffinal <- numdf

#Get Denominator DF
dendf <- as_tibble(summary(model)$fstatistic[3])
dendf <- dendf %>% rename(`Denominator DF` = "value") %>% mutate(name = "sp500")
dendffinal <- dendf

#Get Omnibus P-value (overall_p() is a custom function)
pval <- as_tibble(overall_p(model))
pval <- pval %>% rename(`p-value` = "value") %>% mutate(name = "sp500")
pvalfinal <- pval



#SP500 Difference

sp500_all <- macro_data$`S&P 500`

sp500_all <- sp500_all %>% 
  mutate(diff = `Close/Last` - Open)

sp_500_diff <- sp500_all %>% 
  filter(Date > min(texts_count_date$Date) & Date < max(texts_count_date$Date)) 

textcount_sp500_diff <- texts_count_date %>%
  inner_join(sp_500_diff, by = "Date")


model <- lm( `diff` ~ n_texts + Date, data = textcount_sp500_diff)

confs   <- confint(model)
mod_sum <- summary(model)


# Get Beta value 
estimate      <- as_tibble(summary(model)$coefficients[2])
estimate      <- estimate %>% rename(`Unstandardized Beta` = "value") %>%  mutate(name = "sp500 difference"); 
estimatefinal <- bind_rows(estimatefinal, estimate)

#Get R
r      <- as_tibble(cor(textcount_sp500_diff$n_texts, textcount_sp500_diff$`diff`)); 
r      <- r %>% rename(R = "value") %>%  mutate(name = "sp500 difference"); 
rfinal <- bind_rows(rfinal, r)

#Get R^2
r2      <- as_tibble(summary(model)$r.squared); 
r2      <- r2 %>% rename(`R-squared` = "value" ) %>% mutate(name = "sp500 difference"); 
r2final <- bind_rows(r2final, r2)

#Get f
f <- as_tibble(summary(model)$fstatistic[1])
f <- f %>% rename(`F-value` = "value") %>% mutate(name = "sp500 difference");
ffinal <- bind_rows(ffinal, f)

#Get Numerator DF
numdf <- as_tibble(summary(model)$fstatistic[2])
numdf <- numdf %>% rename(`Numerator DF` = "value") %>% mutate(name = "sp500 difference");
numdffinal <- bind_rows(numdffinal, numdf)

#Get Denominator DF
dendf <- as_tibble(summary(model)$fstatistic[3])
dendf <- dendf %>% rename(`Denominator DF` = "value") %>% mutate(name = "sp500 difference");
dendffinal <- bind_rows(dendffinal, dendf)

#Get Omnibus P-value (overall_p() is a custom function)
pval <- as_tibble(overall_p(model))
pval <- pval %>% rename(`p-value` = "value") %>% mutate(name = "sp500 difference");
pvalfinal <- bind_rows(pvalfinal, pval)




# Dow Jones Absolute

dj_all <- macro_data$`Dow Jones`

dj_filter <- dj_all %>% 
  filter(Date > min(texts_count_date$Date) & Date < max(texts_count_date$Date)) 


textcount_dj <- texts_count_date %>%
  inner_join(dj_filter, by = "Date")


model <- lm( `Close` ~ n_texts + Date, data = textcount_dj)

confs <- confint(model)
mod_sum <- summary(model)


# Get Beta value 
estimate      <- as_tibble(summary(model)$coefficients[2])
estimate      <- estimate %>% rename(`Unstandardized Beta` = "value") %>%  mutate(name = "Dow Jones"); 
estimatefinal <- bind_rows(estimatefinal, estimate)

#Get R
r      <- as_tibble(cor(textcount_dj$n_texts, textcount_dj$`Close`)); 
r      <- r %>% rename(R = "value") %>%  mutate(name = "Dow Jones"); 
rfinal <- bind_rows(rfinal, r)

#Get R^2
r2      <- as_tibble(summary(model)$r.squared); 
r2      <- r2 %>% rename(`R-squared` = "value" ) %>% mutate(name = "Dow Jones"); 
r2final <- bind_rows(r2final, r2)

#Get f
f <- as_tibble(summary(model)$fstatistic[1])
f <- f %>% rename(`F-value` = "value") %>% mutate(name = "Dow Jones");
ffinal <- bind_rows(ffinal, f)

#Get Numerator DF
numdf <- as_tibble(summary(model)$fstatistic[2])
numdf <- numdf %>% rename(`Numerator DF` = "value") %>% mutate(name = "Dow Jones");
numdffinal <- bind_rows(numdffinal, numdf)

#Get Denominator DF
dendf <- as_tibble(summary(model)$fstatistic[3])
dendf <- dendf %>% rename(`Denominator DF` = "value") %>% mutate(name = "Dow Jones");
dendffinal <- bind_rows(dendffinal, dendf)

#Get Omnibus P-value (overall_p() is a custom function)
pval <- as_tibble(overall_p(model))
pval <- pval %>% rename(`p-value` = "value") %>% mutate(name = "Dow Jones");
pvalfinal <- bind_rows(pvalfinal, pval)



#Dow Jones Difference

dj_all <- dj_all %>% 
  mutate(diff = `Close` - Open)

dj_diff <- dj_all %>% 
  filter(Date > min(texts_count_date$Date) & Date < max(texts_count_date$Date)) 

textcount_dj_diff <- texts_count_date %>%
  inner_join(dj_diff, by = "Date")

model <- lm( `diff` ~ n_texts + Date, data = textcount_dj_diff)

confs <- confint(model)
mod_sum <- summary(model)

# Get Beta value 
estimate      <- as_tibble(summary(model)$coefficients[2])
estimate      <- estimate %>% rename(`Unstandardized Beta` = "value") %>%  mutate(name = "Dow Jones difference"); 
estimatefinal <- bind_rows(estimatefinal, estimate)


#Get R
r      <- as_tibble(cor(textcount_dj_diff$n_texts, textcount_dj_diff$`diff`)); 
r      <- r %>% rename(R = "value") %>%  mutate(name = "Dow Jones difference"); 
rfinal <- bind_rows(rfinal, r)

#Get R^2
r2      <- as_tibble(summary(model)$r.squared); 
r2      <- r2 %>% rename(`R-squared` = "value" ) %>% mutate(name = "Dow Jones difference"); 
r2final <- bind_rows(r2final, r2)

#Get f
f <- as_tibble(summary(model)$fstatistic[1])
f <- f %>% rename(`F-value` = "value") %>% mutate(name = "Dow Jones difference"); 
ffinal <- bind_rows(ffinal, f)

#Get Numerator DF
numdf <- as_tibble(summary(model)$fstatistic[2])
numdf <- numdf %>% rename(`Numerator DF` = "value") %>% mutate(name = "Dow Jones difference"); 
numdffinal <- bind_rows(numdffinal, numdf)

#Get Denominator DF
dendf <- as_tibble(summary(model)$fstatistic[3])
dendf <- dendf %>% rename(`Denominator DF` = "value") %>% mutate(name = "Dow Jones difference"); 
dendffinal <- bind_rows(dendffinal, dendf)

#Get Omnibus P-value (overall_p() is a custom function)
pval <- as_tibble(overall_p(model))
pval <- pval %>% rename(`p-value` = "value") %>% mutate(name = "Dow Jones difference"); 
pvalfinal <- bind_rows(pvalfinal, pval)






# S&P 500 Interaction 

textcount_sp500 <- textcount_sp500 %>% 
  mutate(SP500_Closing_Price = `Close/Last`,
         diff = `Close/Last` - Open,
         Number_of_texts_sent = n_texts,
         Date_f = factor(Date),
         `Days_since_first_text` = as.numeric(as_date(Date) - min(as_date(Date)))
  )

model <- lm( `SP500_Closing_Price` ~ Number_of_texts_sent + `Days_since_first_text` + Number_of_texts_sent*`Days_since_first_text`, data = textcount_sp500)
#summary(model)


confs_in <- confint(model)
mod_sum_in <- summary(model)


# Get Beta value 
estimate_in      <- as_tibble(summary(model)$coefficients[2])
estimate_in      <- estimate_in %>% rename(`Unstandardized Beta (texts)` = "value") %>%  mutate(name = "sp500"); 
estimatefinal_in <- estimate_in

# Get Beta value 
estimate_in2      <- as_tibble(summary(model)$coefficients[3])
estimate_in2      <- estimate_in2 %>% rename(`Unstandardized Beta (time)` = "value") %>%  mutate(name = "sp500"); 
estimatefinal_in2 <- estimate_in2

# Get Beta value 
estimate_in3      <- as_tibble(summary(model)$coefficients[4])
estimate_in3      <- estimate_in3 %>% rename(`Unstandardized Beta (texts*time)` = "value") %>%  mutate(name = "sp500"); 
estimatefinal_in3 <- estimate_in3



#Get R^2
r2_in      <- as_tibble(summary(model)$r.squared); 
r2_in      <- r2_in %>% rename(`R-squared` = "value" ) %>%  mutate(name = "sp500"); 
r2final_in <- r2_in

#Get f
f_in <- as_tibble(summary(model)$fstatistic[1])
f_in <- f_in %>% rename(`F-value` = "value") %>% mutate(name = "sp500")
ffinal_in <- f_in

#Get Numerator DF
numdf_in <- as_tibble(summary(model)$fstatistic[2])
numdf_in <- numdf_in %>% rename(`Numerator DF` = "value") %>% mutate(name = "sp500")
numdffinal_in <- numdf_in

#Get Denominator DF
dendf_in <- as_tibble(summary(model)$fstatistic[3])
dendf_in <- dendf_in %>% rename(`Denominator DF` = "value") %>% mutate(name = "sp500")
dendffinal_in <- dendf_in

#Get Omnibus P-value (overall_p() is a custom function)
pval_in <- as_tibble(overall_p(model))
pval_in <- pval_in %>% rename(`p-value` = "value") %>% mutate(name = "sp500")
pvalfinal_in <- pval_in






# S&P 500 Daily Difference Interaction 

model <- lm( `diff` ~ Number_of_texts_sent + `Days_since_first_text` + Number_of_texts_sent*`Days_since_first_text`, data = textcount_sp500)
#summary(model)


confs_in <- confint(model)
mod_sum_in <- summary(model)

# Get Beta value 
estimate_in      <- as_tibble(summary(model)$coefficients[2])
estimate_in      <- estimate_in %>% rename(`Unstandardized Beta (texts)` = "value") %>%  mutate(name = "sp500 difference"); 
estimatefinal_in <- bind_rows(estimatefinal_in, estimate_in)

# Get Beta value 
estimate_in2      <- as_tibble(summary(model)$coefficients[3])
estimate_in2      <- estimate_in2 %>% rename(`Unstandardized Beta (time)` = "value") %>%  mutate(name = "sp500 difference"); 
estimatefinal_in2 <- bind_rows(estimatefinal_in2, estimate_in2)

# Get Beta value 
estimate_in3      <- as_tibble(summary(model)$coefficients[4])
estimate_in3      <- estimate_in3 %>% rename(`Unstandardized Beta (texts*time)` = "value") %>%  mutate(name = "sp500 difference"); 
estimatefinal_in3 <- bind_rows(estimatefinal_in3 ,estimate_in3)



#Get R^2
r2_in      <- as_tibble(summary(model)$r.squared); 
r2_in      <- r2_in %>% rename(`R-squared` = "value" ) %>%  mutate(name = "sp500 difference"); 
r2final_in <- bind_rows(r2final_in, r2_in)

#Get f
f_in <- as_tibble(summary(model)$fstatistic[1])
f_in <- f_in %>% rename(`F-value` = "value") %>% mutate(name = "sp500 difference")
ffinal_in <- bind_rows(ffinal_in,f_in)

#Get Numerator DF
numdf_in <- as_tibble(summary(model)$fstatistic[2])
numdf_in <- numdf_in %>% rename(`Numerator DF` = "value") %>% mutate(name = "sp500 difference")
numdffinal_in <- bind_rows(numdffinal_in, numdf_in)

#Get Denominator DF
dendf_in <- as_tibble(summary(model)$fstatistic[3])
dendf_in <- dendf_in %>% rename(`Denominator DF` = "value") %>% mutate(name = "sp500 difference")
dendffinal_in <- bind_rows(dendffinal_in, dendf_in)

#Get Omnibus P-value (overall_p() is a custom function)
pval_in <- as_tibble(overall_p(model))
pval_in <- pval_in %>% rename(`p-value` = "value") %>% mutate(name = "sp500 difference")
pvalfinal_in <- bind_rows(pvalfinal_in, pval_in)





textcount_dj <- textcount_dj %>% 
  mutate(`Dow_Jones_Daily_Closing_Price` = `Close`,
         diff = `Close` - Open,
         Number_of_texts_sent = n_texts,
         Date_f = factor(Date),
         `Days_since_first_text` = as.numeric(as_date(Date) - min(as_date(Date)))
  )


#
## Dow Jones Interaction 
#
#model <- lm( `Dow_Jones_Daily_Closing_Price` ~ Number_of_texts_sent + `Days_since_first_text` + Number_of_texts_sent*`Days_since_first_text`, data = textcount_dj)
##summary(model)
#
#
#confs_in <- confint(model)
#mod_sum_in <- summary(model)
#
## Get Beta value 
#estimate_in      <- as_tibble(summary(model)$coefficients[2])
#estimate_in      <- estimate_in %>% rename(`Unstandardized Beta (texts)` = "value") %>%  mutate(name = "Dow Jones"); 
#estimatefinal_in <- bind_rows(estimatefinal_in, estimate_in)
#
## Get Beta value 
#estimate_in2      <- as_tibble(summary(model)$coefficients[3])
#estimate_in2      <- estimate_in2 %>% rename(`Unstandardized Beta (time)` = "value") %>%  mutate(name = "Dow Jones"); 
#estimatefinal_in2 <- bind_rows(estimatefinal_in2, estimate_in2)
#
## Get Beta value 
#estimate_in3      <- as_tibble(summary(model)$coefficients[4])
#estimate_in3      <- estimate_in3 %>% rename(`Unstandardized Beta (texts*time)` = "value") %>%  mutate(name = "Dow Jones"); 
#estimatefinal_in3 <- bind_rows(estimatefinal_in3 ,estimate_in3)
#
#
#
##Get R^2
#r2_in      <- as_tibble(summary(model)$r.squared); 
#r2_in      <- r2_in %>% rename(`R-squared` = "value" ) %>%  mutate(name = "Dow Jones"); 
#r2final_in <- bind_rows(r2final_in, r2_in)
#
##Get f
#f_in <- as_tibble(summary(model)$fstatistic[1])
#f_in <- f_in %>% rename(`F-value` = "value") %>% mutate(name = "Dow Jones")
#ffinal_in <- bind_rows(ffinal_in,f_in)
#
##Get Numerator DF
#numdf_in <- as_tibble(summary(model)$fstatistic[2])
#numdf_in <- numdf_in %>% rename(`Numerator DF` = "value") %>% mutate(name = "Dow Jones")
#numdffinal_in <- bind_rows(numdffinal_in, numdf_in)
#
##Get Denominator DF
#dendf_in <- as_tibble(summary(model)$fstatistic[3])
#dendf_in <- dendf_in %>% rename(`Denominator DF` = "value") %>% mutate(name = "Dow Jones")
#dendffinal_in <- bind_rows(dendffinal_in, dendf_in)
#
##Get Omnibus P-value (overall_p() is a custom function)
#pval_in <- as_tibble(overall_p(model))
#pval_in <- pval_in %>% rename(`p-value` = "value") %>% mutate(name = "Dow Jones")
#pvalfinal_in <- bind_rows(pvalfinal_in, pval_in)
#
#
#
#
## Dow Jones Daily Difference Interaction 
#
#model <- lm( `diff` ~ Number_of_texts_sent + `Days_since_first_text` + Number_of_texts_sent*`Days_since_first_text`, data = textcount_dj)
##summary(model)
#
#
#confs_in <- confint(model)
#mod_sum_in <- summary(model)
#
## Get Beta value 
#estimate_in      <- as_tibble(summary(model)$coefficients[2])
#estimate_in      <- estimate_in %>% rename(`Unstandardized Beta (texts)` = "value") %>%  mutate(name = "Dow Jones difference"); 
#estimatefinal_in <- bind_rows(estimatefinal_in, estimate_in)
#
## Get Beta value 
#estimate_in2      <- as_tibble(summary(model)$coefficients[3])
#estimate_in2      <- estimate_in2 %>% rename(`Unstandardized Beta (time)` = "value") %>%  mutate(name = "Dow Jones difference"); 
#estimatefinal_in2 <- bind_rows(estimatefinal_in2, estimate_in2)
#
## Get Beta value 
#estimate_in3      <- as_tibble(summary(model)$coefficients[4])
#estimate_in3      <- estimate_in3 %>% rename(`Unstandardized Beta (texts*time)` = "value") %>%  mutate(name = "Dow Jones difference"); 
#estimatefinal_in3 <- bind_rows(estimatefinal_in3 ,estimate_in3)
#
#
#
##Get R^2
#r2_in      <- as_tibble(summary(model)$r.squared); 
#r2_in      <- r2_in %>% rename(`R-squared` = "value" ) %>%  mutate(name = "Dow Jones difference"); 
#r2final_in <- bind_rows(r2final_in, r2_in)
#
##Get f
#f_in <- as_tibble(summary(model)$fstatistic[1])
#f_in <- f_in %>% rename(`F-value` = "value") %>% mutate(name = "Dow Jones difference")
#ffinal_in <- bind_rows(ffinal_in,f_in)
#
##Get Numerator DF
#numdf_in <- as_tibble(summary(model)$fstatistic[2])
#numdf_in <- numdf_in %>% rename(`Numerator DF` = "value") %>% mutate(name = "Dow Jones difference")
#numdffinal_in <- bind_rows(numdffinal_in, numdf_in)
#
##Get Denominator DF
#dendf_in <- as_tibble(summary(model)$fstatistic[3])
#dendf_in <- dendf_in %>% rename(`Denominator DF` = "value") %>% mutate(name = "Dow Jones difference")
#dendffinal_in <- bind_rows(dendffinal_in, dendf_in)
#
##Get Omnibus P-value (overall_p() is a custom function)
#pval_in <- as_tibble(overall_p(model))
#pval_in <- pval_in %>% rename(`p-value` = "value") %>% mutate(name = "Dow Jones difference")
#pvalfinal_in <- bind_rows(pvalfinal_in, pval_in)
#








#Financial data
finance <- finance %>% 
  filter(`Proposed Category` != "0") %>% 
  mutate(`Proposed Category` = ifelse(`Proposed Category` == "Zoos & Acquariums", "Zoos & Aquariums", `Proposed Category`)) 


#Set up for treemap plots
finance_summ <- finance %>%   
  group_by(`Proposed Category`) %>% 
  summarize(Amount = sum(Amount)) %>% 
  ungroup() %>% 
  arrange(desc(Amount)) %>% 
  mutate(Total = sum(Amount)) %>% 
  rowwise() %>% 
  mutate(Amount = round(Amount / Total * 100, 2)) %>% 
  ungroup() %>% 
  mutate(`Proposed Category` = ifelse(`Proposed Category` == "airbnb", "AirBnB", `Proposed Category`)) %>% 
  arrange(`Proposed Category`)

#table(finance$`Proposed Category`)

#Grouping by user
finance_summ_group <- finance %>%   
  group_by(User, `Proposed Category`) %>% 
  summarize(Amount = sum(Amount), .groups = "keep") %>% 
  ungroup() %>% 
  group_by(User) %>% 
  mutate(Total = sum(Amount)) %>% 
  arrange(desc(Amount)) %>%
  rowwise() %>% 
  mutate(Amount = round(Amount / Total * 100, 2)) %>% 
  ungroup() %>% 
  mutate(`Proposed Category` = ifelse(`Proposed Category` == "airbnb", "AirBnB", `Proposed Category`))

finance_summ_Neil <- finance_summ_group  %>% 
  filter(User == "Neil") %>% 
  arrange(`Proposed Category`) 

finance_summ_Karina <- finance_summ_group  %>% 
  filter(User == "Karina")

cats_all <- finance_summ_group %>% 
  select(`Proposed Category`)

cats_neil <- finance_summ_Neil %>% 
  select(`Proposed Category`)

cats_karina <- finance_summ_Karina %>% 
  select(`Proposed Category`)


cats_no_neil <- cats_all %>% 
  anti_join(cats_neil, by = "Proposed Category") %>% 
  mutate(User = "Neil",
         Amount = 0, 
         Total = 0 ) %>% 
  select(User, `Proposed Category`, Amount, Total )


cats_no_karina <- cats_all %>% 
  anti_join(cats_karina, by = "Proposed Category") %>% 
  mutate(User = "Karina",
         Amount = 0, 
         Total = 0 ) %>% 
  select(User, `Proposed Category`, Amount, Total )


finance_summ_Neil <- finance_summ_Neil %>% 
  bind_rows(cats_no_neil) %>% 
  arrange(`Proposed Category`) %>% 
  mutate(`Proposed Category` = factor(`Proposed Category`))



finance_summ_Karina <- finance_summ_Karina %>% 
  bind_rows(cats_no_karina) %>% 
  arrange(`Proposed Category`) %>% 
  mutate(`Proposed Category` = factor(`Proposed Category`))

#finance_summ_Neil <- arrange(finance_summ_Neil, desc(`Proposed Category`))


## Fincance Data format for Karina

#for_karina_mon <- finance %>% 
#  mutate(`Month Year` = format(ymd(`Transaction Date`), "%B %Y")) %>% 
#  group_by(User, `Proposed Category`, `Month Year`) %>% 
#  summarize(Amount = sum(Amount), .groups = "keep") %>% 
#  ungroup() %>% 
#  group_by(User, `Month Year`) %>% 
#  mutate(Total = sum(`Amount`)) %>% 
#  ungroup() %>% 
#  mutate(Amount = Amount / Total) %>% 
#  #select(-Total) %>% 
#  arrange(`Proposed Category`, `Month Year`, `User`)
#
#
#
#
#for_karina_quar <- finance %>% 
#  mutate(Quarter = zoo::as.yearqtr(`Transaction Date`, , format = "%Y-%m-%d"),
#         Quarter =  format(Quarter, format = "Q%q'%y")) %>% 
#  group_by(User, `Proposed Category`, `Quarter`) %>% 
#  summarize(Amount = sum(Amount), .groups = "keep") %>% 
#  ungroup() %>% 
#  group_by(User, Quarter) %>% 
#  mutate(Total = sum(`Amount`)) %>% 
#  ungroup() %>% 
#  mutate(Amount = Amount / Total) %>% 
#  #select(-Total) %>% 
#  arrange(`Proposed Category`, `User`)
#
#
#write_xlsx(for_karina_mon ,"finance_summed_month.xlsx"  , format_headers = FALSE) 
#write_xlsx(for_karina_quar,"finance_summed_quarter.xlsx", format_headers = FALSE) 


#for_karina_mon %>% 
#  group_by(`User`, `Month Year`) %>% 
#  summarize(sum = sum(Amount))
#
#
#for_karina_quar %>% 
#  group_by(`User`, `Quarter`) %>% 
#  summarize(sum = sum(Amount))





### Beta information

betas <- read_csv("https://raw.githubusercontent.com/ndyetz/ourloveinnumbers.github.io/refs/heads/main/final_betas.csv", show_col_types = FALSE)

betas <- betas %>% 
  mutate_if(is.numeric, round, digits = 2) %>% 
  select(`Asset Name`, Ticker, Beta, `Standard Error`)


sp500_betas <- read_csv("https://raw.githubusercontent.com/ndyetz/ourloveinnumbers.github.io/refs/heads/main/SP500_final_betas.csv", show_col_types = FALSE)
sp500_names <- read_csv("https://raw.githubusercontent.com/ndyetz/ourloveinnumbers.github.io/refs/heads/main/SP500.csv", show_col_types = FALSE)

sp500_names <- sp500_names %>% 
  mutate(ticker = ifelse(ticker == "BRK.B", "BRK-B", ticker),
         ticker = ifelse(ticker == "BF.B", "BF-B", ticker)
  )

sp500_betas <- sp500_betas %>% 
  mutate_if(is.numeric, round, digits = 2) %>% 
  left_join(sp500_names, by = c("Ticker" = "ticker")) %>% 
  select(Company = "company", Ticker, Beta, `Standard Error`) 



### Make charts for Karina from the Beta file she sent back

cus_spend_names <- c("type", 
                                                                                                                    "2022-September", "2022-October", "2022-November", "2022-December", 
  "2023-January", "2023-February", "2023-March", "2023-April", "2023-May", "2023-June", "2023-July", "2023-August", "2023-September", "2023-October", "2023-November", "2023-December", 
  "2024-January", "2024-February", "2024-March", "2024-April", "2024-May", "2024-June", "2024-July" 
  )

cus_spend <- read_excel("karina_betas.xlsx", sheet = "Cosolidated Customer Spending", range = "B4:Y6", col_names = cus_spend_names)


cus_spend_long <- cus_spend %>% 
  pivot_longer(-type, names_to = "Month-Year", values_to = "Total Customer Spending (Billions)") %>% 
  mutate(`Month-Year` = ym(`Month-Year`)#, 
        # `Month-Year` = format(as.Date(`Month-Year`), "%Y-%m")
         ) %>% 
  rename(`Spending Type` = "type")


cus_spend_ndgoods_service <- cus_spend_long %>% 
  filter(`Spending Type` != "Durable goods")

cus_spend_service <- cus_spend_long %>% 
  filter(`Spending Type` == "Services")



# Tinder File
tindersp500 <- read_excel("Tinder and S&P500.xlsx", sheet = "Sheet3")

tindersp500_long <- tindersp500 %>% 
  select(Date = "Date...1",  `Tinder (MTCH)` = "Tinder month over month change", `S&P 500` = "S&P500 month over month change") %>% 
  pivot_longer(-Date, values_to = "Month Over Month %Change", names_to = "Company") %>% 
  mutate(`Month Over Month %Change` = round(`Month Over Month %Change`, 2),
         Date = as_date(Date)) %>% 
  filter(!is.na(`Month Over Month %Change`))






# Creating the front image - overlay word cloud (alpha = 0.15), text quant graph and S&P 500 with different colors
#library(jpeg)
#library(ggpubr)
#
#
#together <- png::readPNG("together_no_date.PNG") 
#im <- png::readPNG("word_cloud.PNG") 
#
#cloud <- matrix(rgb(im[,,1],im[,,2],im[,,3], im[,,4] * 0.15), nrow=dim(im)[1])
#
#
#  ggplot(texts_quant_graph, aes(x = week_of, `# of texts sent`, color = `Sent by`)) + #<- only works if variable is labeled `Sent by`!
#    background_image(together) +
#
#    geom_line(linewidth = .2) + 
#    scale_color_manual(values = c( "#00BFC4", "black")) +
#    theme_void() +
# #  scale_y_continuous(expand = c(0, 0, .14, 0)) +
#   # labs(title= "" , x="", color = "&#9644; <b>Sender</b><br>&#9670; <b>Milestone</b><br>") +
#    theme(legend.position ="none")  +
#    background_image(cloud) 
#    #scale_x_date(date_labels="%b %Y",date_breaks  ="1 month")
#
#  
#  ggsave("together_graphs.png", height = 576, width = 1154, units = "px",  device='png')
#
#
#
#together <- png::readPNG("together_graphs.PNG") 
#
#
#ggplot(sp500, aes(x = Date, y = `Close/Last`) )+
#  background_image(together) +
#  geom_line(linewidth = .2, color = "#0086B9") + 
# # scale_color_manual(values = "deepskyblue") +
#  #  scale_y_continuous("S&P500", sec.axis = sec_axis(~(.), name = "Dow Jones"), limits = c(3500, max(sp500$`Close`)), , expand = c(0, 0, .14, 0)) +
#  theme_void() +
#  scale_y_continuous(limits = c(3500, max(sp500$`Close/Last`)), expand = c(0, 0, .0, 0)) +
#  #labs(title="", x="", color = "&#9644; <b>Close Price</b><br>&#9670; <b>Milestone</b><br>") +
#  theme(legend.position ="none")  +
#  scale_x_date(date_labels="%b %Y",date_breaks  ="1 month") 
#  
#ggsave("together_graphs2.png", height = 576, width = 1154, units = "px",  device='png')
#

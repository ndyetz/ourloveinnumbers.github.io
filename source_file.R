
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


p_sent <- ggplot(text_quant_graph, aes(x = `Sent by`, y = `# of texts sent` , fill = `Sent by`)) +
  geom_col(position = "dodge") +
  scale_y_continuous(expand = c(0, 0, .14, 0)) +
  geom_text(
    aes(label = paste0(`# of texts sent`, " Texts\n "), lineheight = .83),
    position = position_dodge(.8),
    vjust = 30,
    size = 5) +
  labs(title = "Total number of texts sent by Neil and Karina",
       x = "") +
  theme_minimal() +
  #  theme(axis.text.x = element_text(angle = 45,  hjust=1))  +
  theme(axis.text.x = element_text(size=11, face="bold", color = "black"), legend.title=element_blank()) 

#text_quant %>% 
# kbl(digits = 0) %>%
#kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive")) # <- Un-comment these 3 lines to view table and look for errors


# Overall character count
text_character_graph <- text_quant %>% 
  pivot_longer(-`sent by`, values_to = "value") %>% 
  filter(`sent by` != "Combined" & name != "# of texts sent") %>% 
  select(`Sent by` = "sent by", `Average number of characters in text` = "value") 

p_character_sent <- ggplot(text_character_graph, aes(x = `Sent by`, y = `Average number of characters in text` , fill = `Sent by`)) +
  geom_col(position = "dodge") +
  scale_y_continuous(expand = c(0, 0, .14, 0)) +
  geom_text(
    aes(label = paste0(round(`Average number of characters in text`, 2) , "\n "), lineheight = .83),
    position = position_dodge(.8),
    vjust = 30,
    size = 5) +
  labs(title = "Average number of text characters in Neil's and Karina's text messages",
       x = "") +
  theme_classic() +
  #  theme(axis.text.x = element_text(angle = 45,  hjust=1))  +
  theme(axis.text.x = element_text(size=11, face="bold", color = "black"), legend.title=element_blank()) 







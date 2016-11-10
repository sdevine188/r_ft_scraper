library(stringr)
library(rvest)
library(dplyr)
library(lubridate)
library(jsonlite)

# setwd
setwd("C:/Users/Stephen/Desktop/R/ft")

# example api call
# http://api.ft.com/content/c0dda1cc-664f-11e6-8310-ecf0bddad227?apiKey=hNIqHozr1JA4f2mU0uDZA2GkZfVzhpQC

# set initial variables
api_key <- "hNIqHozr1JA4f2mU0uDZA2GkZfVzhpQC"
current_date <- ymd(Sys.Date())
all_section_urls <- c("https://www.ft.com/global-economy", "https://www.ft.com/world/us", "https://www.ft.com/world/europe",
                      "https://www.ft.com/china", "https://www.ft.com/world/asia-pacific", "https://www.ft.com/world/mideast",
                      "https://www.ft.com/world/africa", "https://www.ft.com/world/americas", 
                      "https://www.ft.com/comment")
section_titles <- c("global economy", "us", "europe", "china", "asia", "middle east", "africa", "americas",
                    "comments")
all_article_urls <- c()
all_article_sections <- c()
all_section_text <- c()


get_article_text <- function(all_section_urls) {
       
        # loop through all sections getting article links for today
        for(section_url in 1:length(all_section_urls)) {
                current_section_title <- section_titles[section_url]
                print(str_c(current_section_title, " section"))
                html <- read_html(all_section_urls[section_url])
                article_links <- html %>% html_nodes(xpath = "//a[@class = 'card__title']") %>% html_attr("href")
                article_dates <- html %>% html_nodes(xpath = "//time[@class = 'card__timestamp__time']") %>% html_attr("datetime")
                article_dates <- lapply(article_dates, function(x) { ymd_hms(x) })
                article_dates <- do.call(c, article_dates)
                article_dates_index <- which(day(article_dates) == day(current_date))
                print(str_c(length(article_dates_index), " articles found"))
                if(length(article_dates_index) > 0) { 
                        article_links <- article_links[article_dates_index]
                } else { next }
                
                # build api urls
                article_urls <- str_c("http://api.ft.com", article_links, "?apiKey=hNIqHozr1JA4f2mU0uDZA2GkZfVzhpQC", sep = "")
                remove_html_tags <- function(html_string) {
                        return(str_replace_all(html_string, "<.*?>", ""))
                }
                all_article_urls <- c(all_article_urls, article_urls)
                all_article_sections <- c(all_article_sections, rep(current_section_title, length(article_urls)))
        }
        
        # remove duplicate urls
        print(str_c("initial all_article_urls length is ", length(all_article_urls)))
        dups <- duplicated(all_article_urls)
        dup_index <- which(dups == TRUE)
        if(length(dup_index > 0)) {
                all_article_urls <- all_article_urls[-dup_index]
                all_article_sections <- all_article_sections[-dup_index]
        }

        # remove any urls that don't have word content, which are junk links without api support
        content_links_index <- grepl("/content/", all_article_urls, ignore.case = TRUE)
        all_article_urls <- all_article_urls[content_links_index]
        all_article_sections <- all_article_sections[content_links_index]
        print(str_c("de-duplicated all_article_urls length is ", length(all_article_urls)))
        
        # loop through current section getting article text for today
        for(i in 1:length(all_article_urls)) {
                current_section_title <- all_article_sections[i]
                article_json <- fromJSON(all_article_urls[i])
                columnists <- c("Edward Luce", "John Gapper", "Janan Ganesh", "John Thornhill", "Wolfgang Münchau",
                                "Philip Stephens", "Gillian Tett", "Robert Shrimsley", "Chris Giles", 
                                "Martin Wolf", "Gideon Rachman", "David Pilling", "David Gardner", "Lawrence Summers")
                if(current_section_title == "comments" & !(article_json$byline %in% columnists)) {
                        print("comment author not in columnists")
                        next
                }
                article_text <- article_json$bodyXML
                article_text <- remove_html_tags(article_text)
                article_text <- str_c(current_section_title, " section.", "title: ", article_json$title, ". article: ", 
                                      article_text, "{{split}}", sep = " ")
                all_section_text <- c(all_section_text, article_text)
        }

        # final output
        print(str_c("complete: ", length(all_section_text), " unique articles"))
        
        writeLines(all_section_text, "ft.txt")
        print("ft.txt file created")
}

get_article_text(all_section_urls)





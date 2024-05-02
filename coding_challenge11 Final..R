# Load necessary libraries
library(tidyverse)
library(rvest)
library(polite)

# Function to get all unique relative links to other Wikipedia pages
# List of Wikipedia pages to analyze (example)
pages_to_analyze <- c(
  "https://en.wikipedia.org/wiki/SUNY_System",
  "https://en.wikipedia.org/wiki/East_Farmingdale")

# Initialize an empty data frame to store results
result_df <- data.frame(Page = character(), Links_Count = numeric(), stringsAsFactors = FALSE)
# Loop through each page, extract links count, and store in the data frame
for (page in pages_to_analyze) {
  links_count <- get_links_count(page)
  result_df <- rbind(result_df, data.frame(Page = page, Links_Count = links_count))
}
links <- function(page){
  rel_links <- page %>%
    html_elements("#content") %>%
    html_elements("p") %>%
    html_elements("a") %>%
    html_attr("href") %>%
    unique()
  rel_links[str_detect(rel_links, '/wiki/')]
}

# Function to get a page from a relative link
rel_get <- function(rel_link){
  url <- paste0("https://en.wikipedia.org", rel_link, collapse="")
  politely(read_html)(url)
}

# Function to get the title from a given page
title <- function(page){
  page %>% html_element("title") %>% html_text %>%
    str_remove(" - Wikipedia")
}

# Function to get links and their titles from a given page
 links_and_names <- function(page){
  rel_links <- page %>%
    html_elements("#content") %>%
    html_elements("p") %>%
    html_elements("a") %>%
    html_attr("href")
  links <- rel_links[str_detect(rel_links, '/wiki/')]
  names <- page %>%
    html_elements("#content") %>%
    html_elements("p") %>%
    html_elements("a") %>%
    html_text() 
  names <- names[str_detect(rel_links, "/wiki/")]
  out <- data.frame(name = names, url = links)
  unique(out)
}

# Function to get links, link names, and link count from a given relative link
get_links <- function(rel_link){
  page <- rel_get(rel_link)
  page_title <- title(page)
  links_names <- links_and_names(page)
  data.frame(title = page_title, links = links_names$url, link_name = links_names$name, link_count = nrow(links_names))
}

# Function to recursively get links from multiple pages
recursive_get <- function(rel_link){
  out <- get_links(rel_link)
  links_to_try <- out$links
  for(l in links_to_try){
    out2 <- recursive_get(l)
    out <- full_join(out, out2)
    rm(out2)
  }
  out
}

# Function to find 100 different Wikipedia pages and their link counts
find_100_pages <- function(){
  # Initialize an empty data frame to store results
  all_links <- data.frame()
  
  # Loop until we have 100 pages
  while (nrow(all_links) < 100) {
    # Get a random Wikipedia page
    random_page <- rand_page()
    
    # Extract links from the page
    links <- links(random_page)
    
    # Filter out links to other Wikipedia pages
    wiki_links <- grep("^/wiki/", links, value = TRUE)
    
    # Remove duplicates
    wiki_links <- unique(wiki_links)
    
    # Get link counts for these pages
    page_info <- lapply(wiki_links, get_links)
    page_info <- do.call(rbind, page_info)
    
    # Append to the results
    all_links <- bind_rows(all_links, page_info)
    
    # Remove duplicates again
    all_links <- unique(all_links)
  }
  
  # Sort the data by link count in descending order
  all_links <- arrange(all_links, desc(link_count))
  
  # Return the top 100 pages
  top_100 <- head(all_links, 100)
  return(top_100)
}

# Find 100 different Wikipedia pages and their link counts
example_page <- find_100_pages()

# Write the data to a CSV file
write_csv(example_page, "top_100_wikipedia_pages.csv")

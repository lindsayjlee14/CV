library(readr)
library(dplyr)
library(pagedown)
library(rmarkdown)
library(glue)
library(tidyverse)

# Load csv with position info
citation_data <- read_csv('publications.csv')


print_citation <- function(citation_data, section_id){citation_data %>% 
    filter(section == section_id) %>% 
    mutate(id = 1:n()) %>% 
    group_by(id) %>% 
    ungroup() %>% 
    mutate_all(~ifelse(is.na(.), 'N/A', .)) %>% 
    glue_data(
      "{authors}. ",
      "({date}). ",
      "{title}",
      "{journal}", 
      "{volume}",
      "({issue}), ",
      "{pages} ",
      "{link}",
      "\n\n\n",)}

print_citation(citation_data, 'Scientific Papers')
 
print_abstract <- function(citation_data, section_id){citation_data %>% 
    filter(section == section_id) %>% 
    mutate(id = 1:n()) %>% 
    group_by(id) %>% 
    ungroup() %>% 
    mutate_all(~ifelse(is.na(.), 'N/A', .)) %>% 
    glue_data(
      "{authors}. ",
      "{title} ",
      "{loc} ",
      "{date}. ",
      "\n\n\n",)}

print_abstract(citation_data, 'Abstracts Accepted for Oral Presentation')

print_abstract(citation_data, 'Abstracts Accepted for Poster Presentation')
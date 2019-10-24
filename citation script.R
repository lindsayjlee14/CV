
# Load csv with position info
citation_data <- read_csv('publications.csv')

print_citation <- function(citation_data, section_id)
  {citation_data %>% filter(section == section_id) %>% 
    arrange(desc(end)) %>% 
    glue_data(
      "{authors}",
      "{date}",
      "{title}",
      "{journal", 
      "{issue}",
      "{volume}",
      "{pages}",
      "{link}",
      )}


print_citation <-citation_data %>% filter(section == section_id) %>% 
    glue_data(
      "{authors}",
      "{date}",
      "{title}",
      "{journal", 
      "{issue}",
      "{volume}",
      "{pages}",
      "{link}",)

print_citation (citation_data, 'Scientific Papers')


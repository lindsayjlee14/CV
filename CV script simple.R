
# Helper function to print table without overwhelming output
print_head <- function(data, size = 5){head(data, size) %>% knitr::kable()}

# Load csv with position info
position_data <- read_csv('positions.csv')

# Add an id to keep track of each entry
position_data <- position_data %>% mutate(id = 1:n()) 

#print to check that there is an id number for each entry 
position_data %>% select(id, description_1, description_2, description_3, description_4, description_5 ) %>% print_head()

#transform individual position row into series of rows
pivoted_positions <- position_data %>% 
pivot_longer(starts_with('description'),names_to = 'description_num',values_to = 'description',
values_drop_na = FALSE) 
pivoted_positions %>% select(title, description_num, description) %>% print_head()

pos_w_descrip_list <- pivoted_positions %>% group_by(id) %>% 


# Wrap all descriptions into a list column
mutate(descriptions = list(description) ) %>% ungroup() %>%  filter(description_num == 'description_1') %>% select(-description_num, -description)


# If missing start or start is same as end
# date, just use end date. otw build range
positions_w_timeline <- pos_w_descrip_list %>% mutate(timeline = ifelse(is.na(start) | start == end, end, glue('{end} - {start}')))


positions_collapsed_bullets <- positions_w_timeline %>% mutate(description_bullets = map_chr(descriptions, ~paste('-', ., collapse = '\n')),)

positions_collapsed_bullets %>% 
  pull(description_bullets) %>% 
  head(3)  


positions_no_na <- positions_collapsed_bullets %>% 
  mutate_all(~ifelse(is.na(.), 'N/A', .))  
positions_no_na %>% 
  head(2) %>% 
  glue_data(
    "### {title}",
    "\n\n",
    "{loc}",
    "\n\n",
    "{institution}",
    "\n\n",
    "{timeline}", 
    "\n\n",
    "{description_bullets}",
    "\n\n\n"
  )






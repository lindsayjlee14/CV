
# Take a position dataframe and the section id desired
# and prints the section to markdown. 
print_section <- function(position_data, section_id){position_data %>% 
    filter(section == section_id) %>% 
    arrange(desc(end)) %>% 
    mutate(id = 1:n()) %>% 
    pivot_longer(
      starts_with('description'),
      names_to = 'description_num',
      values_to = 'description',
      values_drop_na = TRUE
    ) %>% 
    group_by(id) %>% 
    mutate(
      descriptions = list(description)
    ) %>% 
    ungroup() %>% 
    filter(description_num == 'description_1') %>% 
    mutate(
      timeline = ifelse(
        is.na(start) | start == end,
        end,
        glue('{end} - {start}')
      ),
      description_bullets = map_chr(descriptions, ~paste('-', ., collapse = '\n')),
    ) %>% 
    mutate_all(~ifelse(is.na(.), 'N/A', .)) %>% 
    glue_data(
      "### {title}",
      "\n\n",
      "{institution}",
      "\n\n",
      "{loc}",
      "\n\n",
      "{timeline}", 
      "\n\n",
      "{description_bullets}",
      "\n\n\n",
    )
}


#print paper citation

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


#print abstracts

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

# Construct a bar chart of skills
build_skill_bars <- function(skills, out_of = 5){
  bar_color <- "#969696"
  bar_background <- "#d9d9d9"
  skills %>% 
    mutate(width_percent = round(100*level/out_of)) %>% 
    glue_data(
      "<div class = 'skill-bar'",
      "style = \"background:linear-gradient(to right,",
      "{bar_color} {width_percent}%,",
      "{bar_background} {width_percent}% 100%)\" >",
      "{skill}",
      "</div>"
    )
}

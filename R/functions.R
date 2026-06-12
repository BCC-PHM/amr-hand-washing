get_codes <- function(.starts_with){
  all_qs |> 
    pivot_wider(names_from = "question_code",
                values_from = "response") |> 
    select(participant, starts_with(.starts_with)) |> 
    filter(participant == 1) |> 
    pivot_longer(cols = starts_with(.starts_with),
                 names_to = "question_code",
                 values_to = "response") |> 
    distinct(question_code) |>
    pull(question_code)
}

get_codes(.starts_with = "freq_step")
get_codes("handwashing_steps_importance")

get_labels <- function(.starts_with, .compare = c("within phase", "between phases")){
  codes <- get_codes(.starts_with)
  
  if(.compare == "within phase"){
    labels <- all_qs |> 
      filter(question_code %in% codes) |> 
      filter(participant == 1) |> 
      select(full_question) |> 
      separate_wider_delim(full_question,
                           delim = "?",
                           names = c("full_question_1", "full_question_2")) |> 
      select(full_question_2) |> 
      mutate(full_question_2 = str_trim(full_question_2)) |> 
      distinct(full_question_2) |> 
      pull(full_question_2)
  } else if(.compare == "between phases"){
    labels <- all_qs |> 
      filter(question_code %in% codes) |> 
      filter(participant == 1) |> 
      select(phase) |> 
      mutate(phase_name = case_when(phase == "T1" ~ "Pre-webinar",
                                    phase == "T2" ~ "Post-webinar",
                                    phase == "T3" ~ "Post-reinforcement",
                                    phase == "T4" ~ "Follow up")) |> 
      distinct(phase_name) |> 
      pull(phase_name)
  }
  labels
}

get_labels(.starts_with = "freq_step", .compare = "within phase")
get_labels(.starts_with = "handwashing_steps_importance", .compare = "between phases")

get_title <- function(.starts_with, .compare = c("within phase", "between phases")){
  codes <- get_codes(.starts_with)
  
  if(.compare == "within phase"){
    title <- all_qs |> 
      filter(question_code %in% codes) |> 
      filter(participant == 1) |> 
      select(full_question) |> 
      separate_wider_delim(full_question,
                           delim = "?",
                           names = c("full_question_1", "full_question_2")) |> 
      select(full_question_1) |> 
      mutate(full_question_1 = str_trim(full_question_1),
             full_question_1 = paste0(full_question_1, "?")) |> 
      pull(full_question_1)
  } else if(.compare == "between phases"){
    title <- all_qs |> 
      filter(question_code %in% codes) |> 
      filter(participant == 1) |> 
      select(full_question) |> 
      pull(full_question)
  }
  title[1]
}

get_title("element_reminder", "within phase")
get_title("handwashing_steps_importance", "between phases")

get_likert_scale <- function(.starts_with){
  codes <- get_codes(.starts_with)
  
  likert_type <- all_qs |> 
      filter(question_code %in% codes) |> 
      filter(participant == 1) |> 
      select(likert_scale) |> 
      pull(likert_scale)
    
  if(likert_type[1] == "likert_effectiveness"){
      likert_scale <- c("Not At All Effective",
                        "Slightly Effective",
                        "Moderately Effective",
                        "Effective",
                        "Very Effective")
  } else if(likert_type[1] == "likert_expectations"){
      likert_scale <- c("Far Below Expectations",
                        "Below Expectations",
                        "Met Expectations",
                        "Above Expectations",
                        "Far Above Expectations")
  } else if(likert_type[1] == "likert_frequency"){
      likert_scale <- c("Not At All",
                        "Occasionally",
                        "About Half The Time",
                        "Most Of The Time",
                        "Every Time")
  } else if(likert_type[1] == "likert_importance"){
      likert_scale <- c("Not At All Important",
                        "Slightly Important",
                        "Moderately Important",
                        "Important",
                        "Very Important")
  } else if(likert_type[1] == "likert_knowledge"){
      likert_scale <- c("No Knowledge",
                        "Low Knowledge",
                        "Moderate Knowledge",
                        "Good Knowledge",
                        "Very Good Knowledge")
  } else if(likert_type[1] == "likert_understanding"){
      likert_scale <- c("No Understanding",
                        "Low Understanding",
                        "Moderate Understanding",
                        "Good Understanding",
                        "Very Good Understanding")
    }
    
  likert_scale
}

get_likert_scale("element_reminder")
get_likert_scale("handwashing_steps_importance")

plot_likert <- function(.phase, .group, .starts_with, .compare = c("within phase", "between phases"), .order = c("question order", "score")){
  codes <- get_codes(.starts_with)
  labels <- get_labels(.starts_with, .compare)
  title <- get_title(.starts_with, .compare)
  likert_scale <- get_likert_scale(.starts_with)
  
  if(.compare == "within phase"){
    data <- all_qs |> 
      filter(question_code %in% codes,
             group %in% .group,
             phase %in% .phase) |> 
      select(-phase, -group, -full_question, -likert_scale) |> 
      pivot_wider(names_from = "question_code",
                  values_from = "response") |> 
      select(-participant) |> 
      mutate(across(everything(), ~ factor(.x, levels = likert_scale)))
    
    var_label(data) <- labels
    
    if(.order == "question order"){
      plot <- data |>
        gglikert_stacked() +
        scale_fill_brewer(palette = "YlGnBu") +
        ggtitle(title)
    } else if(.order == "score"){
      plot <- data |>
        gglikert_stacked(sort = "descending",
                         sort_method = "median") +
        scale_fill_brewer(palette = "YlGnBu") +
        ggtitle(title)
    }
  } else if(.compare == "between phases"){
    data <- all_qs |> 
      filter(question_code %in% codes,
             group %in% .group,
             phase %in% .phase) |> 
      select(-question_code) |> 
      pivot_wider(names_from = "phase",
                  values_from = "response") |> 
      mutate(across(starts_with("T", ignore.case = F), ~ factor(.x, levels = likert_scale))) |>
      select(-participant, -full_question, -likert_scale)

    labels <- c("group", labels)

    var_label(data) <- labels

    if(.order == "question order"){
      plot <- data |>
        gglikert(include = starts_with("T", ignore.case = F),
                         facet_rows = vars(group)) +
        scale_fill_brewer(palette = "YlGnBu") +
        ggtitle(title)
    } else if(.order == "score"){
      plot <- data |>
        gglikert(include = starts_with("T", ignore.case = F),
                         facet_rows = vars(group)) +
        scale_fill_brewer(palette = "YlGnBu") +
        ggtitle(title)
    }
  }
  data
}

plot_likert(c("T2"), c("Teachers/TAs"), "freq_step", "within phase", "question order")
plot_likert(c("T3"), c("Teachers/TAs"), "element_reminder", "within phase", "score")
plot_likert(c("T1", "T2", "T3", "T4"), c("Teachers/TAs", "Headteachers"), "handwashing_steps_importance", "between phases", "score")


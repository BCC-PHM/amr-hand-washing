get_codes <- function(.phase, .group, .starts_with){
  all_qs |> 
    filter(phase == .phase,
           group == .group) |> 
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

get_codes("T3", "Teachers/TAs", "element_")

get_labels <- function(.phase, .group, .starts_with, .compare = c("within phase", "between phases")){
  codes <- get_codes(.phase, .group, .starts_with)
  
  if(.compare == "within phase"){
    labels <- all_qs |> 
      filter(question_code %in% codes,
             group == .group,
             phase == .phase) |> 
      filter(participant == 1) |> 
      select(full_question) |> 
      separate_wider_delim(full_question,
                           delim = "?",
                           names = c("full_question_1", "full_question_2")) |> 
      select(full_question_2) |> 
      mutate(full_question_2 = str_trim(full_question_2)) |> 
      pull(full_question_2)
  }
  labels
}

get_labels("T3", "Teachers/TAs", "element_time", "within phase")

get_title <- function(.phase, .group, .starts_with, .compare = c("within phase", "between phases")){
  codes <- get_codes(.phase, .group, .starts_with)
  
  if(.compare == "within phase"){
    title <- all_qs |> 
      filter(question_code %in% codes,
             group == .group,
             phase == .phase) |> 
      filter(participant == 1) |> 
      select(full_question) |> 
      separate_wider_delim(full_question,
                           delim = "?",
                           names = c("full_question_1", "full_question_2")) |> 
      select(full_question_1) |> 
      mutate(full_question_1 = str_trim(full_question_1),
             full_question_1 = paste0(full_question_1, "?")) |> 
      pull(full_question_1)
  }
  title[1]
}

get_title("T3", "Teachers/TAs", "freq_step", "within phase")

get_likert_scale <- function(.phase, .group, .starts_with, .compare = c("within phase", "between phases")){
  codes <- get_codes(.phase, .group, .starts_with)
  
  if(.compare == "within phase"){
    likert_type <- all_qs |> 
      filter(question_code %in% codes,
             group == .group,
             phase == .phase) |> 
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
    
  }
  likert_scale
}

get_likert_scale("T3", "Teachers/TAs", "handwashing_steps_importance", "within phase")

plot_likert <- function(.phase, .group, .starts_with, .compare = c("within phase", "between phases"), .order = c("question order", "score")){
  codes <- get_codes(.phase, .group, .starts_with)
  labels <- get_labels(.phase, .group, .starts_with, .compare)
  title <- get_title(.phase, .group, .starts_with, .compare)
  likert_scale <- get_likert_scale(.phase, .group, .starts_with, .compare)
  
  if(.compare == "within phase"){
    data <- all_qs |> 
      filter(question_code %in% codes,
             group == .group,
             phase == .phase) |> 
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
  }
  plot
}

plot_likert("T2", "Teachers/TAs", "freq_step", "within phase", "question order")


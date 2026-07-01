library(tidyverse)
library(PHEindicatormethods)

pupil_quant <- read_excel("data/AMR Project PRE AND POST PUPIL DATA.xlsx") |> 
  select(starts_with("school_name") | starts_with("total_children") | starts_with("pre-") | starts_with("post-")) |> 
  filter(school_name == "TOTAL")

q_code_lookup_pupil <- read_excel("data/question_code_lookup_pupil.xlsx")

# pivot longer, separate phase from question code and add in full questions with left_join
pupil_quant <- pupil_quant |> 
  pivot_longer(cols = `pre-after_chatting`:`post-teacher_demo`,
               names_to = "question",
               values_to = "count") |> 
  separate_wider_delim(question,
                       delim = "-",
                       names = c("phase", "question_code")) |> 
  mutate(phase = paste0(phase, "-intervention"),
         phase = str_to_sentence(phase)) |> 
  left_join(q_code_lookup_pupil,
            by = "question_code")

# flip number where the answer is wrong, to get the number of children who answered yes
# then calculate percentage
pupil_quant <- pupil_quant |>
  mutate(total_children = as.numeric(total_children),
         count = as.numeric(count)) |> 
  mutate(count = case_when(correct == "wrong" ~ total_children - count,
                           .default = count)) |>
  phe_proportion(x = count,
                 n = total_children,
                 multiplier = 100)

pupil_quant |> 
  filter(!question_code %in% c("glo_gel", "colouring", "teacher_demo")) |> 
  separate_wider_delim(full_question,
                       delim = " - ",
                       names = c("question", "context")) |> 
  mutate(context = str_to_title(context)) |> 
  ggplot(aes(x = value,
         y = context,
         group = phase)) +
  geom_col(aes(fill = phase),
           position = position_dodge()) +
  geom_errorbar(aes(y = context,
                    xmin = lowercl,
                    xmax = uppercl),
                width = .2,
                position = position_dodge(0.9)) +
  theme_minimal() +
  custom_theming +
  xlab("Proportion of children who answered 'Yes'") +
  ggtitle("Do you think we should wash our hands...?")

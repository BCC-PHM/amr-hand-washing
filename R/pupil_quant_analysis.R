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
  # mutate(count = case_when(correct == "wrong" ~ total_children - count,
  #                          .default = count)) |>
  phe_proportion(x = count,
                 n = total_children,
                 multiplier = 100)


# Plots -------------------------------------------------------------------

# handwashing context questions

p1 <- pupil_quant |> 
  filter(!question_code %in% c("glo_gel", "colouring", "teacher_demo"),
         correct == "right") |> 
  separate_wider_delim(full_question,
                       delim = " - ",
                       names = c("question", "context")) |> 
  mutate(context = str_to_title(context),
         correct = "Correct answer = Yes") |> 
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
  facet_wrap(~correct,
             nrow = 2,
             strip.position = "right") +
  scale_fill_manual(values = c("#2c7fb8", "#a1dab4"),
                    breaks = c("Pre-intervention", "Post-intervention")) +
  theme_light() +
  theme(panel.grid.major.y = element_blank(),
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        plot.margin = margin(5.5, 5.5, 0, 5.5, "pt")) +
  labs(y = NULL,
       x = NULL,
       fill = NULL)

p2 <- pupil_quant |> 
  filter(!question_code %in% c("glo_gel", "colouring", "teacher_demo"),
         correct == "wrong") |> 
  separate_wider_delim(full_question,
                       delim = " - ",
                       names = c("question", "context")) |> 
  mutate(context = str_to_title(context),
         correct = "Correct answer = No") |> 
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
  facet_wrap(~correct,
             nrow = 2,
             strip.position = "right") +
  scale_fill_brewer("YlGnBu",
                    type = "qual") +
  theme_light() +
  scale_fill_manual(values = c("#2c7fb8", "#a1dab4"),
                    breaks = c("Pre-intervention", "Post-intervention")) +
  scale_x_continuous(labels = scales::label_percent(scale = 1)) +
  theme(panel.grid.major.y = element_blank()) +
  labs(y = NULL,
       fill = NULL,
       x = "Proportion of children answered correctly")

p1 / p2 +
  plot_layout(guides = "collect",
              axes = "collect_x") +
  plot_annotation(title = "Do you think we should wash our hands...?") &
  custom_theming

# activity enjoyment questions

pupil_quant |> 
  filter(question_code %in% c("glo_gel", "colouring", "teacher_demo")) |> 
  mutate(activity = str_remove(full_question,
                                    "Did you enjoy the "),
         activity = str_to_title(activity),
         activity = str_replace(activity,
                                     "Glo - Gel",
                                     "Glo-Gel")) |> 
  ggplot(aes(x = value,
             y = activity)) +
  geom_col(aes(fill = activity)) +
  geom_errorbar(aes(y = activity,
                    xmin = lowercl,
                    xmax = uppercl),
                width = .2) +
  scale_fill_manual(values = c("#2c7fb8", "#a1dab4", "#ffffcc")) +
  scale_x_continuous(labels = scales::label_percent(scale = 1)) +
  theme_light() +
  custom_theming +
  theme(panel.grid.major.y = element_blank(),
        legend.position = "none") +
  labs(y = NULL,
       x = "Proportion of children answered 'Yes'",
       fill = NULL,
       title = "Did you enjoy the...")

library(tidyverse)
library(lubridate)

commits <- read_rds("qfes_commits.rds")

plot_data <-
  commits |>
  filter(
    !grepl("oms-etl", repo)
  ) |>
  group_by(repo) |>
  mutate(
    date = as_date(time),
    first_commit = min(date)
  ) |>
  group_by(
    repo,
    date,
    first_commit
  ) |>
  summarise(
    n_commits = n(),
    .groups = "drop"
  ) |>
  arrange(
    first_commit
  ) |>
  mutate(
    id = as.numeric(as_factor(repo))
  )

project_lines <-
  plot_data |>
  ggplot(aes(x = date, y = id)) +
  geom_point() +
  geom_line(aes(group = id, size = NULL)) +
  theme_minimal() +
  theme(
    axis.line.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.text.y = element_blank(),
    axis.title.y = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank()
  ) +
  labs(
    title = "QFES Data Scientist commits on Github",
    subtitle = "4 authors, 103 projects, 890 context switches (18 > 180 days).",
    x = "commit date"
  )

ggsave("project_lines.png", 
  project_lines, 
  device = ragg::agg_png(),
  width = 9, 
  height = 12,
  units = "in")

# Q? how many projects
commits |>
  distinct(repo) |>
  nrow()
# 103!

# Q? How many context switches
normalised_commits <-
  commits |>
  mutate(
    normalised_author = case_when(
      grepl("miles", author, ignore.case = TRUE) ~ "miles",
      grepl("tess|thuy", author, ignore.case = TRUE) ~ "tess",
      grepl("anthony", author, ignore.case = TRUE) ~ "tony",
      grepl("russell", author, ignore.case = TRUE) ~ "russell",
      TRUE ~ "unknown"
    )
  ) 
  
normalised_commits|>
  group_by(
    normalised_author
  ) |>
  arrange(
    time
  ) |>
  mutate(
    lead_repo = lead(repo),
    context_switch = (repo != lead_repo) & !is.na(lead_repo)
  ) |>
  pull(context_switch) |>
  sum()
# 890 context switches

# Q? how many context switches were mine
normalised_commits |>
  filter(
    normalised_author == "miles"
  ) |>
  arrange(
    time
  ) |>
  mutate(
    lead_repo = lead(repo),
    context_switch = (repo != lead_repo) & !is.na(lead_repo)
  ) |>
  pull(context_switch) |>
  sum()
  # 355 context switches are mine

# Q? how many of these have a gap or 6 months or more between commits
commit_time_diff <-
  plot_data |>
  group_by(repo) |>
  mutate(
    lead_date = lead(date),
    commit_diff = as.numeric(lead_date - date, units = "days")
  ) |>
  ungroup()

commit_time_diff |>
  ggplot(
    aes(x = commit_diff)
  ) +
  geom_histogram(
    binwidth = 30
  ) +
  scale_y_log10() +
  theme_minimal()

commit_time_diff |>
  filter(
    commit_diff > 180
  ) |>
  nrow()

# 18 times!
library(sf)
sf:::merge.sf()

# Q? how many context swithces were observed

# density plot
p <-
  data.frame(
    A = rnorm(250, mean = 90, sd = 10),
    B = rnorm(n = 250, mean = 100, sd = 40)
  ) |>
  pivot_longer(
    cols = c(A, B),
    names_to = "option",
    values_to = "result"
  ) |>
  ggplot(
    aes(x = result, fill = option)
  ) +
  geom_density(alpha = 0.5)

ggsave("density.png", p, width = 800, height = 600, units = "px")

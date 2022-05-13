library(dplyr)
library(lubridate)
library(tidyverse)

date_format <- "%b %d,%Y"
folder_path <- "a_problem_with_presidents"

csv_file <- list.files(path = folder_path, pattern = ".csv")
presidents <- read_csv(paste0(folder_path, "/", csv_file))

cleaned_presidents <- presidents %>%
    dplyr::rename_all(., list(~ make.names(., unique = TRUE))) %>% # clean column names replacing space (' ') with .
    dplyr::rename_all(., list(~ tolower(.))) %>% # make column names lowercase
    dplyr::rename_all(., list(~ gsub("\\.", "_", .))) %>% # replace period (.) with underscore (_)
    dplyr::mutate(
        birth_date = lubridate::as_date(.data$birth_date, format = date_format), # change to date type
        death_date = lubridate::as_date(.data$death_date, format = date_format) # change to date type
    )

# exclude currently living presidents
days_lived_presidents <- cleaned_presidents %>%
    dplyr::mutate(
        # interval = lubridate::interval(.data$birth_date, replace_na(.data$death_date, lubridate::today())) # precalculate interval
        interval = lubridate::interval(.data$birth_date, .data$death_date) # precalculate interval
    ) %>%
    dplyr::mutate(
        year_of_birth = lubridate::year(.data$birth_date),
        lived_years = .data$interval %/% years(1),
        lived_months = .data$interval %/% months(1),
        lived_days = .data$interval %/% days(1)
    ) %>%
    dplyr::filter(., !is.na(.data$lived_days)) %>%
    dplyr::select(
        ., -c("interval")
    )

days_lived_nrow <- nrow(days_lived_presidents)

# days_lived_mode is NA if there is value with more than one occurence
days_lived_mode <- days_lived_presidents %>%
    dplyr::count(., .data$lived_days) %>%
    dplyr::arrange(dplyr::desc(.data$n)) %>%
    head(., 1) %>%
    dplyr::mutate(
        mode_lived_days = dplyr::case_when(
            .data$n > 1 ~ .data$lived_days,
            TRUE ~ NA_real_
        )
    ) %>%
    dplyr::select(., .data$mode_lived_days)

days_lived_stats <- days_lived_presidents %>%
    dplyr::select(., .data$lived_days) %>%
    dplyr::summarize(
        mean = mean(.data$lived_days),
        test = n(),
        # weighted_mean_lived_days = weighted.mean(.data$lived_days, sd(.data$lived_days)),
        median = median(.data$lived_days),
        max = max(.data$lived_days),
        min = min(.data$lived_days),
        standard_deviation = sd(.data$lived_days),
        sd2 = sqrt(sum((.data$lived_days - mean(.data$lived_days))^2 / (days_lived_nrow - 1)))
    )

days_lived_all <- days_lived_stats %>%
    cbind(., days_lived_mode)

# 10 longest lived
longest_lived_presidents <- days_lived_presidents %>%
    dplyr::arrange(dplyr::desc(lived_days)) %>%
    head(., 10)

# 10 shortest lived
shortest_lived_presidents <- days_lived_presidents %>%
    dplyr::arrange(lived_days) %>%
    head(., 10)

ggplot(data = longest_lived_presidents) +
    geom_point(mapping = aes(x = year_of_birth, y = lived_years))

ggplot(data = shortest_lived_presidents) +
    geom_point(mapping = aes(x = year_of_birth, y = lived_years))

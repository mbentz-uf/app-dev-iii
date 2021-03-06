library(dplyr)
library(lubridate)
library(plyr)
library(tidyverse)

get_mode <- function(df, column_name) {
    df_mode <- df %>%
        dplyr::count(., .data[[column_name]]) %>%
        dplyr::arrange(dplyr::desc(.data$n)) %>%
        head(., 1) %>%
        dplyr::mutate(
            mode = dplyr::case_when(
                .data$n > 1 ~ .data[[column_name]],
                TRUE ~ NA_real_
            )
        ) %>%
        dplyr::select(., mode)

    return(df_mode)
}

get_measures_for <- function(df, column_name, label) {
    nrow <- nrow(df)

    mode <- get_mode(df, column_name)

    measures_df <- df %>%
        dplyr::mutate(
            year_of_birth = plyr::round_any(.data$year_of_birth, 50, f = floor)
        ) %>%
        dplyr::group_by(., .data$year_of_birth) %>%
        dplyr::summarize(
            label = label,
            n = n(),
            mean = mean(.data[[column_name]]),
            median = median(.data[[column_name]]),
            max = max(.data[[column_name]]),
            min = min(.data[[column_name]]),
            standard_deviation = sd(.data[[column_name]]),
            sd2 = sqrt(sum((.data[[column_name]] - mean(.data[[column_name]]))^2 / (nrow - 1)))
        ) %>%
        cbind(., mode)

    return(measures_df)
}

get_weighted_mean <- function(df1, df2) {
    df1_nrow <- nrow(df1)
    df2_nrow <- nrow(df2)

    sum_of_means <- (df1_nrow * df1$mean) + (df2_nrow * df2$mean)
    weighted_mean <- sum_of_means / (df1_nrow + df2_nrow)

    return(weighted_mean)
}

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

# 10 longest lived
longest_lived_presidents <- days_lived_presidents %>%
    dplyr::arrange(dplyr::desc(lived_days)) %>%
    head(., 10)

# plot the 10 longest lived presidents
ggplot(data = longest_lived_presidents, mapping = aes(x = year_of_birth, y = lived_years)) +
    geom_point()

# 10 shortest lived
shortest_lived_presidents <- days_lived_presidents %>%
    dplyr::arrange(lived_days) %>%
    head(., 10)

# plot the 10 shortest lived presidents
ggplot(data = shortest_lived_presidents, mapping = aes(x = year_of_birth, y = lived_years)) +
    geom_point()

longest_lived_measures <- get_measures_for(longest_lived_presidents, "lived_days", "longest_lived")
shortest_lived_measures <- get_measures_for(shortest_lived_presidents, "lived_days", "shortest_lived")

# weighted mean
weighted_mean <- get_weighted_mean(longest_lived_measures, shortest_lived_measures)

# apply weighted means
longest_lived_data_set <- longest_lived_measures %>%
    cbind(., weighted_mean)

shortest_lived_data_set <- shortest_lived_measures %>%
    cbind(., weighted_mean)

# combine data sets
days_lived_data_set <- longest_lived_data_set %>%
    rbind(., shortest_lived_data_set)

plot <- ggplot(data = days_lived_data_set, mapping = aes(x = year_of_birth, y = mean, fill = label, color = label)) + # , y = year_of_birth)) +
    geom_bar(
        stat = "identity",
        position = position_dodge(),
        width = 30,
    ) +
    geom_point(
        show.legend = FALSE,
        position = position_dodge(30),
    ) +
    geom_errorbar(
        aes(
            ymin = mean - standard_deviation,
            ymax = mean + standard_deviation
        ),
        color = "black",
        width = 1,
        position = position_dodge(30),
    )

plot + labs(x = "Top 10", y = "Mean days")

count_plot <- ggplot(data = days_lived_data_set, mapping = aes(x = year_of_birth, y = n, fill = label, color = label)) + # , y = year_of_birth)) +
    geom_smooth(
        stat = "identity"
    )
count_plot + labs(x = "Top 10", y = "Frequency")

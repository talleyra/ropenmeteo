#' @title combine variables (internal)

combine_variables <- function(vec){

    if(is.null(vec)){
        return(NULL)
    }

    vars <- stringr::str_c(vec, collapse = ",")

    return(vars)
}

#' @title build url (internal)

open_meteo_build_url <- function(
    type = "archive",
    latitude = NULL,
    longitude = NULL,
    start_date = NULL,
    end_date = NULL,
    variables = NULL,
    models = NULL
){
    if(type == "archive") {base_url <- "https://archive-api.open-meteo.com"}
    if(type == "forecast"){base_url <- "https://api.open-meteo.com"}

    type <- glue::glue("/v1/{type}")
    
    url <- httr::modify_url(
        url = base_url,
        path = type,
        query = list(
            latitude   = latitude,
            longitude  = longitude,
            start_date = start_date,
            end_date   = end_date,
            hourly     = combine_variables(variables),
            models     = combine_variables(models),
            timezone   = "Europe/Berlin"
        ) |> purrr::compact()
    ) |>
    urltools::url_decode()

    return(url)
}


#' @title get hourly (!!) data form openmeteoapi
#' @param type chose "forecast" or "archive"
#' @param latitude latitude
#' @param longitude longitude
#' @param start_date a start date
#' @param end_date end date
#' @param variables a vector of variables
#' @export

open_meteo_get_data <- function(
    type = "archive",
    latitude = 46.49,
    longitude = 11.34,
    start_date = today() - 50,
    end_date = today(),
    variables,
    models = NULL
){
    url <- open_meteo_build_url(
        type = type,
        latitude = latitude,
        longitude = longitude,
        start_date = start_date,
        end_date = end_date,
        variables = variables,
        models = models
    )

    rlang::inform(
        glue::glue("request url: {url}")
    )

    resp <- httr::GET(url)
    cont <- httr::content(resp)

    rlang::inform(
        glue::glue(crayon::bold("Status code: {resp$status_code}."))
    )

    names_vars <- names(cont$hourly)
    data <- cont$hourly

    df <- purrr::map_dfc(
        names_vars, ~ tibble::tibble({{.x}} := purrr::pluck(data[[.x]]))
    ) |>
    tidyr::unnest(cols = everything()) |>
    dplyr::mutate(time = lubridate::ymd_hm(time)) |>
    tidyr::pivot_longer(
        -time
    ) |>
    dplyr::mutate(
        value = as.numeric(value),
        type = type
    )

    return(df)
}


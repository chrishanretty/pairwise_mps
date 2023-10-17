### PURPOSE OF THIS CODE: to take in the raw data supplied by
### Survation and produce a tidy data frame

here::i_am("R/001_tidy_survey_data.R")
suppressPackageStartupMessages(library(tidyverse))

### Read in the data
dat <- read.csv(here::here("data", "councillors_2023_09_14.csv"),
                check.names = FALSE)

### Make some changes to the names
### These are a result of copy/paste issues
names(dat) <- sub("^ ", "", names(dat))
names(dat) <- sub("I don't know these MPs well enough ", "", names(dat))

### Shorten remaining names
names(dat) <- dplyr::recode(names(dat),
                            "Response ID" = "id",
                            "Time Started" = "starttime",
                            "Date Submitted" = "submittime",
                            "Status" = "status",
                            "In which country is the council where you are an elected member located?" = "ctry",
                            "In which region is the council where you are an elected member located?" = "region",
                            "Which political party do you represent?" = "party",
                            "Independent group:Which political party do you represent?" = "ind_party",
                            "Another party / group:Which political party do you represent?" = "other_party",
                            "In politics people sometimes talk of left and right. Where would you place yourself on the following scale, where 0 is left and 10 is right?" = "self_lr")

### Reshape the data
dat <- dat |>
    pivot_longer(cols = starts_with("Here are two"),
                 names_to = "comparison",
                 values_to = "a_beats_b")

### Remove Northern Irish respondents
dat <- dat |>
    filter(ctry != "Northern Ireland")

### Remove missing responses
dat <- dat |>
    filter(a_beats_b != "")

### Tidy and split the comparisons
dat <- dat |>
    mutate(comparison = sub("Here are two MPs. Which of these is the more left-wing on economic issues?: ", "", comparison, fixed = TRUE),
           comparison = sub("Here are two MPs. Which of these is the more left-wing on economic issues? ", "", comparison, fixed = TRUE))

dat <- dat |>
    separate_wider_delim(comparison, delim = "|",
                         names = c("MP_A", "MP_B")) |>
    mutate(MP_A = str_trim(MP_A),
           MP_B = str_trim(MP_B))

### Remove D/K responses
nrow(dat)
dat <- dat |>
    filter(a_beats_b != "I don't know these MPs well enough")
nrow(dat)

### The structure is right, but some of the column names had errors
### Make sure there are parentheses at the end
dat <- dat |>
    mutate(MP_A = sub("([A-Za-z])$", "\\1)", MP_A),
           MP_B = sub("([A-Za-z])$", "\\1)", MP_B))

### Make sure no awkward characters
dat <- dat |>
    mutate(MP_A = sub(" ", "", MP_A),
           MP_B = sub(" ", "", MP_B))

### Make sure double spaces are replaced with single spaces
dat <- dat |>
    mutate(MP_A = gsub(" +", " ", MP_A)) |>
    mutate(MP_B = gsub(" +", " ", MP_B))

### Fix one specific thing in the comparison
dat <- dat |>
    mutate(a_beats_b = sub("Rishi SUNAK  (Richmond (Yorks))",
                           "Rishi SUNAK (Richmond (Yorks))",
                           a_beats_b,
                           fixed = TRUE))

### Turn the answers into ordered sequence
dat <- dat |>
    rowwise() |>
    mutate(dv = case_when(grepl(MP_A, a_beats_b, fixed = TRUE) &
                          grepl("much more", a_beats_b) ~ 1L,
                          grepl(MP_A, a_beats_b, fixed = TRUE) &
                          grepl("somewhat more", a_beats_b) ~ 2L,
                          grepl(MP_A, a_beats_b, fixed = TRUE) &
                          grepl("about the same", a_beats_b) ~ 3L,
                          grepl(MP_B, a_beats_b, fixed = TRUE) &
                          grepl("somewhat more", a_beats_b) ~ 4L,
                          grepl(MP_B, a_beats_b, fixed = TRUE) &
                          grepl("much more", a_beats_b) ~ 5L,
                          TRUE ~ NA_integer_))

               

### Merge with the canonical list
canon <- read.csv(here::here("data", "canonical_representation.csv")) |>
    dplyr::select(DisplayName, Party, PCON22CD, Person.ID)

dat <- left_join(dat, canon,
                 by = join_by(MP_A == DisplayName))

dat <- left_join(dat, canon,
                 by = join_by(MP_B == DisplayName),
                 suffix = c(".A", ".B"))

### Are there any people who weren't matched?
which(is.na(dat$Party.A))
which(is.na(dat$Party.B))

### Tidy respondent information
dat <- dat |>
    ungroup() |>
    mutate(resp_id = as.numeric(factor(id))) |>
    dplyr::select(-id)

dat <- dat |>
    mutate(party = dplyr::recode(party,
                                 "Independent councillor" = "Independent",
                                 "Independent group" = "Independent",
                                 "Scottish Conservative and Unionist Party" = "Conservative",
                                 "Scottish Green Party" = "Green Party",
                                 "Scottish Labour Party" = "Labour",
                                 "Scottish Liberal Democrats" = "Liberal Democrats"))
### Drop additional party information
dat <- dat |>
    dplyr::select(-ind_party, -other_party)

### Make sure the start and submission times are properly formatted
dat <- dat |>
    mutate(starttime = as.POSIXct(starttime, format = "%d/%m/%Y %H:%M"),
           submittime = as.POSIXct(submittime, format = "%d/%m/%Y %H:%M"))

### Turn some character variables to factors
dat <- dat |>
    mutate(status = factor(status),
           ctry = factor(ctry),
           region = factor(region),
           party = factor(party))

### Handle the left-right self-placement
dat <- dat |>
    mutate(self_lr = as.numeric(gsub("[^0-9]", "", self_lr)))

### Remove missing dependent variables
dat <- dat |>
    filter(!is.na(dv))

### Unclass
dat <- dat |> as.data.frame()

saveRDS(dat, file = here::here("working", "tidied_data.rds"))

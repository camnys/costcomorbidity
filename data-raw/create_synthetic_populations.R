###############################################################################
# CREATE SYNTHETIC EXAMPLE DATA FOR THE costcomorbidity PACKAGE
#
# Purpose
# -------
# This script creates the synthetic datasets distributed with the
# costcomorbidity R package:
#
#   - example_people: one row per synthetic person
#   - example_codes: long-format synthetic ICD-10 and ATC records
#
# The synthetic person-level characteristics and comorbidity indicators are
# generated from a restricted person-year dataset using {synthpop}.
# Representative ICD-10 and ATC codes are then assigned using mappings from
# {comorbidity} and {coder}.
#
# Confidentiality
# ---------------
# The restricted source dataset is not included in this repository or package.
# It must be loaded securely into the current R session as an object named
# `analysis` before this script is run.
#
# The script does not use or export original person identifiers, individual
# diagnosis histories, prescription histories, or observed event dates.
#
# Execution
# ---------
# Run this script from the root of the costcomorbidity package project.
#
# Outputs
# -------
#   data/example_people.rda
#   data/example_codes.rda
#   data-raw/example_code_lookup.csv
#
# Reproducibility
# ---------------
# Random-number seeds are set explicitly below. Results may still depend on
# the installed versions of {synthpop}, {comorbidity}, and {coder}.
###############################################################################


###############################################################################
# 0.1 PACKAGE AND PROJECT CHECKS
###############################################################################

# Confirm that the script is being run from the package root.
if (!file.exists("DESCRIPTION")) {
  stop(
    paste(
      "No DESCRIPTION file was found.",
      "Open costcomorbidity.Rproj and run this script from the package root."
    )
  )
}

package_name <- read.dcf(
  "DESCRIPTION",
  fields = "Package"
)[1, 1]

if (!identical(package_name, "costcomorbidity")) {
  stop(
    "This script must be run from the costcomorbidity package project."
  )
}


# Check that the required packages are installed.
required_packages <- c(
  "dplyr",
  "tidyr",
  "tibble",
  "synthpop",
  "comorbidity",
  "coder",
  "usethis"
)

missing_packages <- setdiff(
  required_packages,
  rownames(installed.packages())
)

if (length(missing_packages) > 0L) {
  stop(
    "Install these packages before continuing: ",
    paste(missing_packages, collapse = ", ")
  )
}


# Load the required packages.
invisible(
  lapply(
    required_packages,
    library,
    character.only = TRUE
  )
)


###############################################################################
# 0.2 CONFIDENTIAL SOURCE DATA
###############################################################################

# The restricted person-year dataset is not distributed with this package.
# Before running this script, load it securely into the current R session as
# an object called `analysis`.

if (!exists("analysis", inherits = TRUE)) {
  stop(
    paste(
      "The restricted source dataset is not loaded.",
      "Load it securely as an object called `analysis` before running",
      "this synthetic-data generation script."
    )
  )
}

if (!is.data.frame(analysis)) {
  stop(
    "`analysis` must be a data.frame, tibble, or data.table."
  )
}


###############################################################################
# 1. SYNTHESIS SETTINGS
###############################################################################

set.seed(100)

# Prediction year selected from the restricted development dataset.
source_year <- 2018L

# Artificial years used in the public example data.
example_prediction_year <- 2021L
example_baseline_year <- example_prediction_year - 1L

# Number of synthetic people.
n_synthetic <- 500L

# Column names in the restricted analysis dataset.
id_var   <- "lopnr"
year_var <- "tp1"
age_var  <- "age"
sex_var  <- "sex"


required_source_columns <- c(
  id_var,
  year_var,
  age_var,
  sex_var
)

missing_source_columns <- setdiff(
  required_source_columns,
  names(analysis)
)

if (length(missing_source_columns) > 0L) {
  stop(
    "These required columns are absent from analysis: ",
    paste(missing_source_columns, collapse = ", ")
  )
}


###############################################################################
# 2. IDENTIFY THE 17 + 31 + 46 COMORBIDITY VARIABLES
###############################################################################

# Charlson indicators.
cci_vars <- grep(
  pattern = "^cci_",
  x = names(analysis),
  value = TRUE
)

cci_vars <- setdiff(
  cci_vars,
  "cci_any"
)

# Elixhauser indicators.
eci_vars <- grep(
  pattern = "^eci_",
  x = names(analysis),
  value = TRUE
)

eci_vars <- setdiff(
  eci_vars,
  "eci_any"
)

# RxRisk indicators.
#
# In this analysis dataset, the category variables start with `rx_`.
# The two excluded variables are summary/non-category variables.
rxrisk_vars <- grep(
  pattern = "^rx_",
  x = names(analysis),
  value = TRUE
)

rxrisk_vars <- setdiff(
  rxrisk_vars,
  c(
    "rx_r___",
    "rx_r____1"
  )
)

# Fail early if the expected category counts are not found.
if (length(cci_vars) != 17L) {
  stop(
    "Expected 17 Charlson indicators, but found ",
    length(cci_vars),
    "."
  )
}

if (length(eci_vars) != 31L) {
  stop(
    "Expected 31 Elixhauser indicators, but found ",
    length(eci_vars),
    "."
  )
}

if (length(rxrisk_vars) != 46L) {
  stop(
    "Expected 46 RxRisk indicators, but found ",
    length(rxrisk_vars),
    "."
  )
}

comorbidity_vars <- unique(
  c(
    cci_vars,
    eci_vars,
    rxrisk_vars
  )
)

missing_comorbidity_columns <- setdiff(
  comorbidity_vars,
  names(analysis)
)

if (length(missing_comorbidity_columns) > 0L) {
  stop(
    "These comorbidity columns are absent from analysis: ",
    paste(missing_comorbidity_columns, collapse = ", ")
  )
}

message(
  "Found ",
  length(cci_vars), " Charlson, ",
  length(eci_vars), " Elixhauser, and ",
  length(rxrisk_vars), " RxRisk indicators."
)


###############################################################################
# 3. SELECT ONE PERSON-YEAR
###############################################################################

analysis_one_year <- analysis |>
  dplyr::filter(
    .data[[year_var]] == source_year
  ) |>
  tibble::as_tibble()

if (nrow(analysis_one_year) == 0L) {
  stop(
    "No rows were found for ",
    year_var,
    " = ",
    source_year,
    "."
  )
}

# There should be exactly one row per person in the selected person-year.
duplicated_ids <- analysis_one_year |>
  dplyr::count(
    .data[[id_var]],
    name = "n"
  ) |>
  dplyr::filter(n > 1L)

if (nrow(duplicated_ids) > 0L) {
  print(duplicated_ids)

  stop(
    "More than one row per person was found for ",
    year_var,
    " = ",
    source_year,
    "."
  )
}


###############################################################################
# 4. PREPARE THE SYNTHESIS VARIABLES
###############################################################################

# Convert a category indicator safely to integer 0/1.
#
# This avoids the common error in which as.integer(factor(c("0", "1")))
# returns 1/2 rather than 0/1.
to_binary_integer <- function(x, variable_name) {
  original_missing <- is.na(x)

  if (is.factor(x)) {
    x <- as.character(x)
  }

  if (is.logical(x)) {
    result <- as.integer(x)
  } else {
    result <- suppressWarnings(
      as.integer(x)
    )
  }

  conversion_failed <- (
    !original_missing &
      is.na(result)
  )

  if (any(conversion_failed)) {
    stop(
      "Variable `",
      variable_name,
      "` could not be converted safely to integer 0/1."
    )
  }

  invalid_values <- (
    !is.na(result) &
      !result %in% c(0L, 1L)
  )

  if (any(invalid_values)) {
    stop(
      "Variable `",
      variable_name,
      "` contains values other than 0 and 1."
    )
  }

  result
}

synthesis_source <- analysis_one_year |>
  dplyr::transmute(
    age = as.integer(.data[[age_var]]),
    sex = factor(.data[[sex_var]]),
    dplyr::across(
      dplyr::all_of(comorbidity_vars),
      ~ to_binary_integer(
        .x,
        dplyr::cur_column()
      )
    )
  ) |>
  tibble::as_tibble()

# Missing category indicators should not silently become absence of disease.
missing_indicator_counts <- colSums(
  is.na(
    synthesis_source[
      comorbidity_vars
    ]
  )
)

if (any(missing_indicator_counts > 0L)) {
  print(
    missing_indicator_counts[
      missing_indicator_counts > 0L
    ]
  )

  stop(
    "At least one comorbidity indicator contains missing values. ",
    "Resolve whether these mean absence or unavailable information before ",
    "creating public synthetic data."
  )
}

if (anyNA(synthesis_source$age)) {
  stop("Age contains missing values in the selected source year.")
}

if (anyNA(synthesis_source$sex)) {
  stop("Sex contains missing values in the selected source year.")
}

# Make the categorical nature of the 0/1 indicators explicit for synthpop.
synthesis_source <- synthesis_source |>
  dplyr::mutate(
    dplyr::across(
      dplyr::all_of(comorbidity_vars),
      ~ factor(
        .x,
        levels = c(0L, 1L)
      )
    )
  )


###############################################################################
# 5. ORDER CONDITIONS BY OBSERVED PREVALENCE
###############################################################################

binary_prevalence <- function(x) {
  if (is.factor(x)) {
    x <- as.character(x)
  }

  mean(
    as.integer(x),
    na.rm = TRUE
  )
}

observed_prevalence <- vapply(
  synthesis_source[
    comorbidity_vars
  ],
  binary_prevalence,
  numeric(1)
)

comorbidity_order <- names(
  sort(
    observed_prevalence,
    decreasing = TRUE
  )
)

# Age and sex are synthesised first. More common conditions are synthesised
# before rarer conditions.
synthesis_source <- synthesis_source |>
  dplyr::select(
    age,
    sex,
    dplyr::all_of(comorbidity_order)
  )


###############################################################################
# 6. HANDLE VARIABLES WITH NO VARIATION
###############################################################################

number_of_values <- vapply(
  synthesis_source,
  function(x) {
    length(
      unique(
        x[!is.na(x)]
      )
    )
  },
  integer(1)
)

constant_vars <- names(
  number_of_values[
    number_of_values <= 1L
  ]
)

constant_values <- lapply(
  synthesis_source[
    constant_vars
  ],
  function(x) {
    x[
      which(!is.na(x))[1L]
    ]
  }
)

model_vars <- setdiff(
  names(synthesis_source),
  constant_vars
)

if (length(model_vars) == 0L) {
  stop("All synthesis variables are constant.")
}

synthesis_model_data <- synthesis_source[
  model_vars
]

# Inspect the variables and classes before synthesis.
print(
  synthpop::codebook.syn(
    synthesis_model_data
  )
)


###############################################################################
# 7. GENERATE THE SYNTHETIC PERSON-LEVEL CATEGORY DATA
###############################################################################

synthetic_object <- synthpop::syn(
  data = synthesis_model_data,

  # A single method string is applied to all synthesised variables.
  method = "cart",

  # Variable names define the order of sequential synthesis.
  visit.sequence = model_vars,

  # Create one synthetic dataset.
  m = 1,

  # Number of synthetic people.
  k = n_synthetic,

  seed = 100,

  print.flag = TRUE
)

synthetic_categories <- synthetic_object$syn

# Reattach variables that had no variation in the source data.
for (variable in constant_vars) {
  synthetic_categories[[variable]] <- rep(
    constant_values[[variable]],
    n_synthetic
  )
}

# Restore the original synthesis-source column order.
synthetic_categories <- synthetic_categories[
  names(synthesis_source)
]

as_binary_integer <- function(x) {
  if (is.factor(x)) {
    x <- as.character(x)
  }

  result <- suppressWarnings(
    as.integer(x)
  )

  if (
    any(
      !is.na(result) &
      !result %in% c(0L, 1L)
    )
  ) {
    stop(
      "A synthesised indicator is not coded 0/1."
    )
  }

  result
}

synthetic_categories <- synthetic_categories |>
  tibble::as_tibble() |>
  dplyr::mutate(
    dplyr::across(
      dplyr::all_of(comorbidity_vars),
      as_binary_integer
    ),
    age = as.integer(age)
  )

# Add entirely artificial IDs and dates.
#
# Codes are assigned dates during `example_baseline_year`, and the index date
# is the first day of `example_prediction_year`.
synthetic_categories <- synthetic_categories |>
  dplyr::mutate(
    id = sprintf(
      "S%04d",
      dplyr::row_number()
    ),
    ascertainment_year = example_baseline_year,
    index_date = as.Date(
      sprintf(
        "%d-01-01",
        example_prediction_year
      )
    ),
    .before = 1
  )

# Force the final synthetic person to have no comorbidity codes.
# This is useful for documenting how the package handles people with no codes.
synthetic_categories[
  nrow(synthetic_categories),
  comorbidity_vars
] <- 0L

# Confirm that each index still has at least one positive condition.
if (
  !any(
    rowSums(
      synthetic_categories[
        cci_vars
      ]
    ) > 0L
  )
) {
  stop(
    "No Charlson conditions were generated. ",
    "Increase n_synthetic or select another source year."
  )
}

if (
  !any(
    rowSums(
      synthetic_categories[
        eci_vars
      ]
    ) > 0L
  )
) {
  stop(
    "No Elixhauser conditions were generated. ",
    "Increase n_synthetic or select another source year."
  )
}

if (
  !any(
    rowSums(
      synthetic_categories[
        rxrisk_vars
      ]
    ) > 0L
  )
) {
  stop(
    "No RxRisk conditions were generated. ",
    "Increase n_synthetic or select another source year."
  )
}


###############################################################################
# 8. CREATE THE EXPORTED PERSON DATASET
###############################################################################

example_people <- synthetic_categories |>
  dplyr::select(
    id,
    index_date,
    age,
    sex
  )

stopifnot(
  !anyDuplicated(example_people$id),
  inherits(example_people$index_date, "Date")
)


###############################################################################
# 9. HELPERS FOR DERIVING REPRESENTATIVE ICD-10 CODES
###############################################################################

# Charlson and Elixhauser output names from {comorbidity} are short labels
# such as mi, chf, and pvd. The analysis variables use cci_/eci_ prefixes.
normalise_indicator_name <- function(x) {
  x <- sub(
    pattern = "^(cci|eci|rx)_",
    replacement = "",
    x = x,
    ignore.case = TRUE
  )

  x <- tolower(x)

  gsub(
    pattern = "[^a-z0-9]",
    replacement = "",
    x = x
  )
}

match_analysis_indicators <- function(
    candidates,
    analysis_vars
) {
  analysis_names <- tibble::tibble(
    indicator = analysis_vars,
    match_key =
      normalise_indicator_name(
        analysis_vars
      )
  )

  if (
    anyDuplicated(
      analysis_names$match_key
    )
  ) {
    stop(
      "Some analysis indicators become duplicated after name normalisation."
    )
  }

  candidates |>
    dplyr::mutate(
      match_key =
        normalise_indicator_name(
          package_indicator
        )
    ) |>
    dplyr::left_join(
      analysis_names,
      by = "match_key"
    ) |>
    dplyr::select(
      -match_key
    )
}

derive_comorbidity_example_codes <- function(
    map,
    analysis_vars,
    index_name
) {
  # Load the ICD-10 dictionary supplied with {comorbidity}.
  data(
    "icd10_2011",
    package = "comorbidity",
    envir = environment()
  )

  icd_universe <- icd10_2011 |>
    tibble::as_tibble() |>
    dplyr::transmute(
      id = dplyr::row_number(),

      code = toupper(
        gsub(
          pattern = "[^A-Z0-9]",
          replacement = "",
          x = Code.clean
        )
      ),

      description = ICD.title
    ) |>
    dplyr::filter(
      !is.na(code),
      code != ""
    ) |>
    dplyr::distinct(
      code,
      .keep_all = TRUE
    )

  # Give each code its own artificial ID so that its activated categories
  # can be inspected directly.
  mapped <- comorbidity::comorbidity(
    x = icd_universe |>
      dplyr::select(
        id,
        code
      ),
    id = "id",
    code = "code",
    map = map,

    # Do not suppress milder hierarchical categories while selecting codes.
    assign0 = FALSE,

    labelled = FALSE,

    # The code dictionary was already cleaned above.
    tidy.codes = FALSE
  ) |>
    tibble::as_tibble()

  package_indicator_vars <- setdiff(
    names(mapped),
    "id"
  )

  candidate_codes <- mapped |>
    dplyr::left_join(
      icd_universe,
      by = "id"
    ) |>
    tidyr::pivot_longer(
      cols = dplyr::all_of(
        package_indicator_vars
      ),
      names_to = "package_indicator",
      values_to = "present"
    ) |>
    dplyr::mutate(
      present = as.integer(present)
    ) |>
    dplyr::filter(
      present == 1L
    ) |>
    dplyr::group_by(code) |>
    dplyr::mutate(
      n_categories_triggered =
        dplyr::n_distinct(
          package_indicator
        )
    ) |>
    dplyr::ungroup()

  # Prefer a code that activates the fewest categories, then prefer a short
  # and readable ICD-10 code.
  selected_codes <- candidate_codes |>
    dplyr::group_by(
      package_indicator
    ) |>
    dplyr::arrange(
      n_categories_triggered,
      nchar(code),
      code,
      .by_group = TRUE
    ) |>
    dplyr::summarise(
      example_code = dplyr::first(code),
      code_description =
        dplyr::first(description),
      n_categories_triggered =
        dplyr::first(
          n_categories_triggered
        ),
      n_candidate_codes =
        dplyr::n_distinct(code),
      .groups = "drop"
    ) |>
    match_analysis_indicators(
      analysis_vars = analysis_vars
    ) |>
    dplyr::mutate(
      index = index_name,
      code_system = "ICD10",
      package_source = "comorbidity",
      mapping = map,

      # Overlap is recorded, but it is not automatically an error.
      requires_review = is.na(indicator),

      note = dplyr::if_else(
        n_categories_triggered > 1L,
        paste0(
          "The selected ICD-10 code activates ",
          n_categories_triggered,
          " categories under this mapping."
        ),
        NA_character_
      )
    ) |>
    dplyr::select(
      index,
      indicator,
      package_indicator,
      code_system,
      example_code,
      code_description,
      mapping,
      package_source,
      n_candidate_codes,
      n_categories_triggered,
      requires_review,
      note
    )

  selected_codes
}


###############################################################################
# 10. DERIVE CHARLSON AND ELIXHAUSER REPRESENTATIVE CODES
###############################################################################

cci_candidates <- derive_comorbidity_example_codes(
  map = "charlson_icd10_quan",
  analysis_vars = cci_vars,
  index_name = "charlson"
)

eci_candidates <- derive_comorbidity_example_codes(
  map = "elixhauser_icd10_quan",
  analysis_vars = eci_vars,
  index_name = "elixhauser"
)

# Validate name matching.
cci_unmatched <- cci_candidates |>
  dplyr::filter(
    is.na(indicator)
  )

eci_unmatched <- eci_candidates |>
  dplyr::filter(
    is.na(indicator)
  )

if (nrow(cci_unmatched) > 0L) {
  print(cci_unmatched)
  stop("At least one Charlson category did not match an analysis variable.")
}

if (nrow(eci_unmatched) > 0L) {
  print(eci_unmatched)
  stop("At least one Elixhauser category did not match an analysis variable.")
}

stopifnot(
  nrow(cci_candidates) == 17L,
  nrow(eci_candidates) == 31L,
  dplyr::n_distinct(cci_candidates$indicator) == 17L,
  dplyr::n_distinct(eci_candidates$indicator) == 31L
)


###############################################################################
# 11. DERIVE RXRISK REPRESENTATIVE ATC CODES
###############################################################################

# Expand the Pratt definitions using {coder}'s ATC codebook.
rxrisk_codebook <- coder::codebook(
  object = coder::rxriskv,
  coding = "atc",
  cc_args = list(
    regex = "atc_pratt"
  )
)

rxrisk_all_codes <- rxrisk_codebook$all_codes |>
  tibble::as_tibble()

required_codebook_columns <- c(
  "code",
  "description",
  "group"
)

missing_codebook_columns <- setdiff(
  required_codebook_columns,
  names(rxrisk_all_codes)
)

if (length(missing_codebook_columns) > 0L) {
  stop(
    "The coder codebook does not contain the expected columns: ",
    paste(missing_codebook_columns, collapse = ", ")
  )
}

# Select one representative ATC code per category returned by the codebook.
rxrisk_candidates <- rxrisk_all_codes |>
  dplyr::transmute(
    code = toupper(code),
    description = description,
    package_indicator = group
  ) |>
  dplyr::filter(
    !is.na(code),
    code != ""
  ) |>
  dplyr::distinct() |>
  dplyr::group_by(code) |>
  dplyr::mutate(
    n_categories_triggered =
      dplyr::n_distinct(
        package_indicator
      )
  ) |>
  dplyr::ungroup() |>
  dplyr::mutate(
    full_atc_code =
      nchar(code) == 7L
  ) |>
  dplyr::group_by(
    package_indicator
  ) |>
  dplyr::arrange(
    n_categories_triggered,
    dplyr::desc(full_atc_code),
    code,
    .by_group = TRUE
  ) |>
  dplyr::summarise(
    example_code = dplyr::first(code),
    code_description =
      dplyr::first(description),
    n_categories_triggered =
      dplyr::first(
        n_categories_triggered
      ),
    n_candidate_codes =
      dplyr::n_distinct(code),
    .groups = "drop"
  )


###############################################################################
# 12. MATCH RXRISK PACKAGE LABELS TO THE ANALYSIS VARIABLES
###############################################################################

get_variable_label <- function(
    variable_name,
    data = analysis
) {
  label <- attr(
    data[[variable_name]],
    "label",
    exact = TRUE
  )

  if (
    is.null(label) ||
    length(label) == 0L
  ) {
    return(NA_character_)
  }

  label <- as.character(label)[1L]

  if (
    is.na(label) ||
    trimws(label) == ""
  ) {
    return(NA_character_)
  }

  label
}

rxrisk_analysis_dictionary <- tibble::tibble(
  indicator = rxrisk_vars,

  analysis_label_raw = vapply(
    rxrisk_vars,
    get_variable_label,
    character(1)
  )
) |>
  dplyr::mutate(
    # Remove the common label prefix but retain the raw label for audit.
    analysis_label = trimws(
      sub(
        pattern = "^RxRisk Category:\\s*",
        replacement = "",
        x = analysis_label_raw,
        ignore.case = TRUE
      )
    )
  )

if (
  anyNA(
    rxrisk_analysis_dictionary$analysis_label
  )
) {
  print(
    rxrisk_analysis_dictionary |>
      dplyr::filter(
        is.na(analysis_label)
      )
  )

  stop(
    "At least one RxRisk analysis variable has no usable variable label."
  )
}

normalise_condition_name <- function(x) {
  x <- tolower(x)

  x <- sub(
    pattern = "^rxrisk category:\\s*",
    replacement = "",
    x = x,
    ignore.case = TRUE
  )

  # Standardise known spelling differences.
  x <- gsub(
    "ischaemic",
    "ischemic",
    x
  )

  x <- gsub(
    "oesophageal",
    "esophageal",
    x
  )

  x <- gsub(
    "hyperkalaemia",
    "hyperkalemia",
    x
  )

  x <- gsub(
    "hyperlipidaemia",
    "hyperlipidemia",
    x
  )

  x <- gsub(
    "malignancies",
    "malignancy",
    x
  )

  gsub(
    pattern = "[^a-z0-9]",
    replacement = "",
    x = x
  )
}

rxrisk_dictionary_for_join <- rxrisk_analysis_dictionary |>
  dplyr::mutate(
    match_key =
      normalise_condition_name(
        analysis_label
      )
  )

if (
  anyDuplicated(
    rxrisk_dictionary_for_join$match_key
  )
) {
  print(
    rxrisk_dictionary_for_join |>
      dplyr::count(
        match_key
      ) |>
      dplyr::filter(
        n > 1L
      )
  )

  stop(
    "Some RxRisk labels become duplicated after normalisation."
  )
}

rxrisk_lookup <- rxrisk_candidates |>
  dplyr::mutate(
    match_key =
      normalise_condition_name(
        package_indicator
      )
  ) |>
  dplyr::left_join(
    rxrisk_dictionary_for_join |>
      dplyr::select(
        indicator,
        analysis_label,
        analysis_label_raw,
        match_key
      ),
    by = "match_key"
  ) |>
  dplyr::mutate(
    index = "rxrisk",
    code_system = "ATC",
    mapping = "atc_pratt",
    package_source = "coder",
    requires_review = is.na(indicator),

    note = dplyr::if_else(
      n_categories_triggered > 1L,
      paste0(
        "The selected ATC code activates ",
        n_categories_triggered,
        " RxRisk categories."
      ),
      NA_character_
    )
  ) |>
  dplyr::select(
    index,
    indicator,
    analysis_label,
    analysis_label_raw,
    package_indicator,
    code_system,
    example_code,
    code_description,
    mapping,
    package_source,
    n_candidate_codes,
    n_categories_triggered,
    requires_review,
    note
  )

rxrisk_unmatched <- rxrisk_lookup |>
  dplyr::filter(
    is.na(indicator)
  )

if (nrow(rxrisk_unmatched) > 0L) {
  print(rxrisk_unmatched)

  stop(
    "At least one RxRisk codebook category did not match an analysis label."
  )
}


###############################################################################
# 13. ADD HEPATITIS B WHEN IT IS ABSENT FROM codebook()$all_codes
###############################################################################

all_rxrisk_package_groups <- coder::rxriskv |>
  as.data.frame() |>
  tibble::as_tibble() |>
  dplyr::pull(group)

missing_rxrisk_package_groups <- setdiff(
  all_rxrisk_package_groups,
  rxrisk_lookup$package_indicator
)

rxrisk_lookup_complete <- rxrisk_lookup

if (
  "Hepatitis B" %in%
  missing_rxrisk_package_groups
) {
  hb_indicator <- rxrisk_analysis_dictionary |>
    dplyr::filter(
      tolower(analysis_label) ==
        "hepatitis b"
    ) |>
    dplyr::pull(indicator)

  if (length(hb_indicator) != 1L) {
    stop(
      "Could not identify exactly one analysis variable for Hepatitis B."
    )
  }

  hb_definition <- coder::rxriskv |>
    as.data.frame() |>
    tibble::as_tibble() |>
    dplyr::filter(
      group == "Hepatitis B"
    )

  if (
    nrow(hb_definition) != 1L ||
    hb_definition$atc_pratt != "J05AF(08|1[01])"
  ) {
    stop(
      "The current Hepatitis B atc_pratt definition differs from the ",
      "definition expected by this generation script."
    )
  }

  # Explicit codes represented by J05AF(08|1[01]).
  hb_codes <- c(
    "J05AF08",
    "J05AF10",
    "J05AF11"
  )

  hb_classification_matrix <- coder::classify(
    hb_codes,
    coder::rxriskv,
    cc_args = list(
      regex = "atc_pratt"
    )
  ) |>
    as.matrix()

  if (
    !"Hepatitis B" %in%
    colnames(
      hb_classification_matrix
    )
  ) {
    stop(
      "coder::classify() did not return a Hepatitis B category."
    )
  }

  hb_validation <- tibble::tibble(
    code = hb_codes,
    hepatitis_b =
      hb_classification_matrix[
        ,
        "Hepatitis B"
      ],
    n_categories_triggered =
      rowSums(
        hb_classification_matrix
      )
  )

  if (
    !all(
      hb_validation$hepatitis_b
    )
  ) {
    print(hb_validation)

    stop(
      "At least one expected Hepatitis B code did not activate Hepatitis B."
    )
  }

  # All three valid Hepatitis B codes overlap with another category.
  # J05AF08 is retained as a deterministic representative example.
  hb_example_code <- "J05AF08"

  hb_activated_categories <- colnames(
    hb_classification_matrix
  )[
    hb_classification_matrix[
      hb_codes == hb_example_code,
    ]
  ]

  hb_other_categories <- setdiff(
    hb_activated_categories,
    "Hepatitis B"
  )

  hb_analysis_info <- rxrisk_analysis_dictionary |>
    dplyr::filter(
      indicator == hb_indicator
    )

  hb_lookup_row <- tibble::tibble(
    index = "rxrisk",
    indicator =
      hb_analysis_info$indicator,
    analysis_label =
      hb_analysis_info$analysis_label,
    analysis_label_raw =
      hb_analysis_info$analysis_label_raw,
    package_indicator = "Hepatitis B",
    code_system = "ATC",
    example_code = hb_example_code,
    code_description = NA_character_,
    mapping = "atc_pratt",
    package_source = "coder",
    n_candidate_codes =
      sum(
        hb_validation$hepatitis_b
      ),
    n_categories_triggered =
      hb_validation$n_categories_triggered[
        hb_validation$code ==
          hb_example_code
      ],
    requires_review = FALSE,
    note = paste0(
      "This code also activates: ",
      paste(
        hb_other_categories,
        collapse = "; "
      ),
      " under coder::rxriskv using atc_pratt."
    )
  )

  rxrisk_lookup_complete <- dplyr::bind_rows(
    rxrisk_lookup,
    hb_lookup_row
  ) |>
    dplyr::arrange(
      package_indicator
    )
}

remaining_missing_rxrisk_groups <- setdiff(
  all_rxrisk_package_groups,
  rxrisk_lookup_complete$package_indicator
)

if (
  length(
    remaining_missing_rxrisk_groups
  ) > 0L
) {
  stop(
    "These RxRisk package categories are still missing: ",
    paste(
      remaining_missing_rxrisk_groups,
      collapse = ", "
    )
  )
}

stopifnot(
  nrow(rxrisk_lookup_complete) == 46L,
  dplyr::n_distinct(
    rxrisk_lookup_complete$indicator
  ) == 46L,
  dplyr::n_distinct(
    rxrisk_lookup_complete$package_indicator
  ) == 46L,
  !anyNA(
    rxrisk_lookup_complete$indicator
  ),
  !anyNA(
    rxrisk_lookup_complete$example_code
  )
)

missing_analysis_rxrisk_indicators <- setdiff(
  rxrisk_vars,
  rxrisk_lookup_complete$indicator
)

if (
  length(
    missing_analysis_rxrisk_indicators
  ) > 0L
) {
  stop(
    "These analysis RxRisk indicators are not represented in the lookup: ",
    paste(
      missing_analysis_rxrisk_indicators,
      collapse = ", "
    )
  )
}


###############################################################################
# 14. VALIDATE EVERY REPRESENTATIVE CODE AGAINST ITS SOURCE PACKAGE
###############################################################################

validate_comorbidity_lookup <- function(
    lookup,
    map
) {
  test_data <- lookup |>
    dplyr::mutate(
      validation_id =
        dplyr::row_number()
    ) |>
    dplyr::select(
      validation_id,
      code = example_code
    )

  classification <- comorbidity::comorbidity(
    x = test_data,
    id = "validation_id",
    code = "code",
    map = map,
    assign0 = FALSE,
    labelled = FALSE,
    tidy.codes = TRUE
  ) |>
    tibble::as_tibble()

  expected_present <- vapply(
    seq_len(
      nrow(lookup)
    ),
    function(i) {
      expected_column <-
        lookup$package_indicator[i]

      if (
        !expected_column %in%
        names(classification)
      ) {
        return(FALSE)
      }

      as.integer(classification[[expected_column]][i]) == 1L
    },
    logical(1)
  )

  if (!all(expected_present)) {
    print(
      lookup[
        !expected_present,
      ]
    )

    stop(
      "At least one representative ICD-10 code did not activate its ",
      "expected category."
    )
  }

  invisible(TRUE)
}

validate_comorbidity_lookup(
  lookup = cci_candidates,
  map = "charlson_icd10_quan"
)

validate_comorbidity_lookup(
  lookup = eci_candidates,
  map = "elixhauser_icd10_quan"
)

rxrisk_validation_matrix <- coder::classify(
  rxrisk_lookup_complete$example_code,
  coder::rxriskv,
  cc_args = list(
    regex = "atc_pratt"
  )
) |>
  as.matrix()

rxrisk_expected_present <- vapply(
  seq_len(
    nrow(
      rxrisk_lookup_complete
    )
  ),
  function(i) {
    expected_column <-
      rxrisk_lookup_complete$package_indicator[i]

    if (
      !expected_column %in%
      colnames(
        rxrisk_validation_matrix
      )
    ) {
      return(FALSE)
    }

    isTRUE(
      rxrisk_validation_matrix[
        i,
        expected_column
      ]
    )
  },
  logical(1)
)

if (
  !all(
    rxrisk_expected_present
  )
) {
  print(
    rxrisk_lookup_complete[
      !rxrisk_expected_present,
    ]
  )

  stop(
    "At least one representative ATC code did not activate its expected ",
    "RxRisk category."
  )
}


###############################################################################
# 15. COMBINE AND SAVE THE COMPLETE AUDIT LOOKUP
###############################################################################

# Add the RxRisk-only label columns to the ICD-based tables so that the
# combined audit file has a consistent structure.
cci_lookup <- cci_candidates |>
  dplyr::mutate(
    analysis_label =
      NA_character_,
    analysis_label_raw =
      NA_character_
  )

eci_lookup <- eci_candidates |>
  dplyr::mutate(
    analysis_label =
      NA_character_,
    analysis_label_raw =
      NA_character_
  )

example_code_lookup <- dplyr::bind_rows(
  cci_lookup,
  eci_lookup,
  rxrisk_lookup_complete
) |>
  dplyr::select(
    index,
    indicator,
    analysis_label,
    analysis_label_raw,
    package_indicator,
    code_system,
    example_code,
    code_description,
    mapping,
    package_source,
    n_candidate_codes,
    n_categories_triggered,
    requires_review,
    note
  ) |>
  dplyr::arrange(
    index,
    indicator
  )

lookup_counts <- example_code_lookup |>
  dplyr::count(
    index
  )

print(lookup_counts)

stopifnot(
  nrow(example_code_lookup) ==
    17L + 31L + 46L,

  sum(
    example_code_lookup$index ==
      "charlson"
  ) == 17L,

  sum(
    example_code_lookup$index ==
      "elixhauser"
  ) == 31L,

  sum(
    example_code_lookup$index ==
      "rxrisk"
  ) == 46L,

  !anyNA(
    example_code_lookup$indicator
  ),

  !anyNA(
    example_code_lookup$example_code
  ),

  !anyDuplicated(
    example_code_lookup[
      c(
        "index",
        "indicator"
      )
    ]
  )
)

write.csv(
  example_code_lookup,
  file =
    "data-raw/example_code_lookup.csv",
  row.names = FALSE,
  na = ""
)


###############################################################################
# 16. CONVERT THE SYNTHETIC INDICATORS INTO LONG-FORM CODE RECORDS
###############################################################################

make_example_code_rows <- function(
    synthetic_data,
    lookup,
    baseline_year,
    seed = 100L
) {
  required_indicators <- unique(
    lookup$indicator
  )

  missing_indicators <- setdiff(
    required_indicators,
    names(synthetic_data)
  )

  if (
    length(
      missing_indicators
    ) > 0L
  ) {
    stop(
      "These indicators are absent from synthetic_data: ",
      paste(
        missing_indicators,
        collapse = ", "
      )
    )
  }

  duplicate_lookup_indicators <- lookup |>
    dplyr::count(
      indicator
    ) |>
    dplyr::filter(
      n != 1L
    )

  if (
    nrow(
      duplicate_lookup_indicators
    ) > 0L
  ) {
    print(
      duplicate_lookup_indicators
    )

    stop(
      "Each analysis indicator must occur exactly once in the lookup."
    )
  }

  code_rows <- synthetic_data |>
    dplyr::select(
      id,
      dplyr::all_of(
        required_indicators
      )
    ) |>
    tidyr::pivot_longer(
      cols = dplyr::all_of(
        required_indicators
      ),
      names_to = "indicator",
      values_to = "present"
    ) |>
    dplyr::mutate(
      present =
        as.integer(present)
    ) |>
    dplyr::filter(
      present == 1L
    ) |>
    dplyr::left_join(
      lookup |>
        dplyr::select(
          index,
          indicator,
          code_system,
          example_code,
          package_indicator
        ),
      by = "indicator"
    )

  if (
    anyNA(
      code_rows$example_code
    )
  ) {
    missing <- code_rows |>
      dplyr::filter(
        is.na(example_code)
      ) |>
      dplyr::distinct(
        indicator
      )

    print(missing)

    stop(
      "Some positive indicators have no representative example code."
    )
  }

  start_date <- as.Date(
    sprintf(
      "%d-01-01",
      baseline_year
    )
  )

  end_date <- as.Date(
    sprintf(
      "%d-12-31",
      baseline_year
    )
  )

  number_of_days <- as.integer(
    end_date -
      start_date
  ) + 1L

  set.seed(seed)

  code_rows |>
    dplyr::mutate(
      code_date =
        start_date +
        sample.int(
          number_of_days,
          size = dplyr::n(),
          replace = TRUE
        ) -
        1L
    )
}

example_code_rows_audit <- make_example_code_rows(
  synthetic_data =
    synthetic_categories,
  lookup =
    example_code_lookup,
  baseline_year =
    example_baseline_year,
  seed = 100L
)

# The public user-facing table contains only the fields users need.
#
# Duplicate person-system-code combinations can arise when the same raw code
# represents related categories across indices. These are collapsed to one
# record.
example_codes <- example_code_rows_audit |>
  dplyr::transmute(
    id,
    code_system,
    code = example_code,
    code_date
  ) |>
  dplyr::distinct(
    id,
    code_system,
    code,
    .keep_all = TRUE
  ) |>
  dplyr::arrange(
    id,
    code_system,
    code_date,
    code
  )


###############################################################################
# 17. VALIDATE THE TWO EXPORTED EXAMPLE DATASETS
###############################################################################

# Each person occurs once in the person table.
stopifnot(
  !anyDuplicated(
    example_people$id
  )
)

# Each code belongs to a person in example_people.
stopifnot(
  all(
    example_codes$id %in%
      example_people$id
  )
)

# All code dates occur before the person's index date.
date_validation <- example_codes |>
  dplyr::left_join(
    example_people |>
      dplyr::select(
        id,
        index_date
      ),
    by = "id"
  )

stopifnot(
  all(
    date_validation$code_date <
      date_validation$index_date
  )
)

# All code dates occur in the artificial baseline year.
stopifnot(
  all(
    format(
      example_codes$code_date,
      "%Y"
    ) ==
      as.character(
        example_baseline_year
      )
  )
)

# Only the expected code systems are present.
stopifnot(
  all(
    example_codes$code_system %in%
      c(
        "ICD10",
        "ATC"
      )
  )
)

# Confirm that at least one person has no code record.
no_code_people <- example_people |>
  dplyr::anti_join(
    example_codes |>
      dplyr::distinct(id),
    by = "id"
  )

if (
  nrow(
    no_code_people
  ) == 0L
) {
  stop(
    "The example data contain no person without code records."
  )
}

# Basic output summaries.
print(example_people)
print(example_codes)

print(
  example_codes |>
    dplyr::count(
      code_system
    )
)

print(
  example_codes |>
    dplyr::count(id) |>
    dplyr::arrange(
      dplyr::desc(n)
    )
)

###############################################################################
# 19. SAVE THE PACKAGE DATASETS
###############################################################################

# These calls create:
#   data/example_people.rda
#   data/example_codes.rda
#
# They must be run from the root of the costcomorbidity package.
usethis::use_data(
  example_people,
  overwrite = TRUE
)

usethis::use_data(
  example_codes,
  overwrite = TRUE
)

message(
  "Finished successfully. Created package datasets: ",
  "example_people and example_codes."
)



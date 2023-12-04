# # [SETUP] -----------------------------------------------------------------
# # - Packages ----------------------------------------------------------------
# # CRAN packages
# chr_pkg <- c(
#   'devtools' #GitHub packages (temp)
#   , 'readr' #Read data (temp)
#   , 'tidyr', 'dplyr' #Data wrangling
#   , 'ggplot2' #Data visualization
#   , 'vctrs' #Data frame subclasses
#   , 'numbers' #Lowest common multiple
# )
#
# # Git packages
# chr_git <- c(
#   'CaoBittencourt' = 'atlas.match', #Matching
#   'coolbutuseless' = 'hershey' #Vector letters
# )
#
# # Activate / install CRAN packages
# lapply(
#   chr_pkg
#   , function(pkg){
#
#     if(!require(pkg, character.only = T)){
#
#       install.packages(pkg)
#
#     }
#
#     require(pkg, character.only = T)
#
#   }
# )
#
# # Activate / install Git packages
# Map(
#   function(git, profile){
#
#     if(!require(git, character.only = T)){
#
#       install_github(
#         paste0(profile, '/', git)
#         , dependencies = T
#         , upgrade = F
#         , force = T
#       )
#
#     }
#
#     require(git, character.only = T)
#
#   }
#   , git = chr_git
#   , profile = names(chr_git)
# )
#
# chr_pkg <- c(
#   'devtools' #GitHub packages
# )

# # - Data ------------------------------------------------------------------
# # Occupations data frame
# df_occupations <- read_csv('/home/Cao/Storage/github/atlas-research/data/occupations/df_occupations_2022.csv')
#
# # Questionnaire
# df_questionnaire <- read_csv('/home/Cao/Storage/github/atlas-research/data/efa/questionnaire_efa_equamax_14factors_60items.csv')
#
# # My own professional profile
# df_input <- read_csv('/home/Cao/Storage/github/atlas-research/data/questionnaires/questionnaire_Cao.csv')

# - Alphabets ------------------------------------------------------------
# Cyrillic alphabet
c(
  # Uppercase letters
  'Ah' = 34,
  'Beh' = 35,
  'Veh' = 55,
  'Geh' = 40,
  'Deh' = 37,
  'Yeh' = 60,
  'Zheh' = 41,
  'Zeh' = 59,
  'Ee' = 42,
  'Ee Kratkoyeh' = 38,
  'Kah' = 44,
  'Ehl' = 45,
  'Ehm' = 46,
  'Ehn' = 47,
  'Oh' = 48,
  'Peh' = 49,
  'Ehr' = 51,
  'Ehs' = 52,
  'Teh' = 53,
  'Oo' = 58,
  'Ehf' = 39,
  'Khah' = 57,
  'Tseh' = 65,
  'Cheh' = 43,
  'Shah' = 50,
  'Schyah' = 56,
  'Tvyordiy Znahk' = 62,
  'I' = 5,
  'Myagkeey Znahk' = 64,
  'Eh' = 36,
  'Yoo' = 54,
  'Yah' = 63,

  # Lowercase letters
  'ah' = 66,
  'beh' = 67,
  'veh' = 87,
  'geh' = 72,
  'deh' = 69,
  'yeh' = 92,
  'zheh' = 73,
  'zeh' = 91,
  'ee' = 74,
  'ee kratkoyeh' = 70,
  'kah' = 76,
  'ehl' = 77,
  'ehm' = 78,
  'ehn' = 79,
  'oh' = 80,
  'peh' = 81,
  'ehr' = 83,
  'ehs' = 84,
  'teh' = 85,
  'oo' = 90,
  'ehf' = 71,
  'khah' = 89,
  'tseh' = 6,
  'cheh' = 75,
  'shah' = 82,
  'schyah' = 88,
  'tvyordiy znahk' = 93,
  'i' = 7,
  'myagkeey znahk' = 95,
  'eh' = 68,
  'yoo' = 86,
  'yah' = 94

) -> chr_cyrillic

# Greek alphabet
c(
  # Uppercase letters
  'Alpha' = 34,
  'Beta' = 35,
  'Gamma' = 40,
  'Delta' = 37,
  'Epsilon' = 38,
  'Zeta' = 59,
  'Eta' = 41,
  'Theta' = 50,
  'Iota' = 42,
  'Kappa' = 44,
  'Lambda' = 45,
  'Mu' = 46,
  'Nu' = 47,
  'Xi' = 57,
  'Omicron' = 48,
  'Pi' = 49,
  'Rho' = 51,
  'Sigma' = 52,
  'Tau' = 53,
  'Upsilon' = 54,
  'Phi' = 39,
  'Chi' = 36,
  'Psi' = 58,
  'Omega' = 56,

  # Lowercase letters
  'alpha' = 66,
  'beta' = 67,
  'gamma' = 72,
  'delta' = 69,
  'epsilon' = 70,
  'zeta' = 91,
  'eta' = 73,
  'theta' = 82,
  'iota' = 74,
  'kappa' = 76,
  'lambda' = 77,
  'mu' = 78,
  'nu' = 79,
  'xi' = 89,
  'omicron' = 80,
  'pi' = 81,
  'rho' = 83,
  'sigma' = 84,
  'tau' = 85,
  'upsilon' = 86,
  'phi' = 71,
  'chi' = 68,
  'psi' = 90,
  'omega' = 88

) -> chr_greek

# Latin alphabet
c(
  # Uppercase letters
  'A' = 34,
  'B' = 35,
  'C' = 36,
  'D' = 37,
  'E' = 38,
  'F' = 39,
  'G' = 40,
  'H' = 41,
  'I' = 42,
  'J' = 43,
  'K' = 44,
  'L' = 45,
  'M' = 46,
  'N' = 47,
  'O' = 48,
  'P' = 49,
  'Q' = 50,
  'R' = 51,
  'S' = 52,
  'T' = 53,
  'U' = 54,
  'V' = 55,
  'W' = 56,
  'X' = 57,
  'Y' = 58,
  'Z' = 59,

  # Lowercase letters
  'a' = 66,
  'b' = 67,
  'c' = 68,
  'd' = 69,
  'e' = 70,
  'f' = 71,
  'g' = 72,
  'h' = 73,
  'i' = 74,
  'j' = 75,
  'k' = 76,
  'l' = 77,
  'm' = 78,
  'n' = 79,
  'o' = 80,
  'p' = 81,
  'q' = 82,
  'r' = 83,
  's' = 84,
  't' = 85,
  'u' = 86,
  'v' = 87,
  'w' = 88,
  'x' = 89,
  'y' = 90,
  'z' = 91,

  # Numbers
  '0' = 17,
  '1' = 18,
  '2' = 19,
  '3' = 20,
  '4' = 21,
  '5' = 22,
  '6' = 23,
  '7' = 24,
  '8' = 25,
  '9' = 26

) -> chr_latin

bind_rows(
  list(

    'cyrillic' =
      as_tibble(
        chr_cyrillic
        , rownames =
          'character'
        , alphabet = 'cyrillic'
      ) %>%
      rename(
        glyph = 2
      )

    , 'greek' =
      as_tibble(
        chr_greek
        , rownames =
          'character'
        , alphabet = 'greek'
      ) %>%
      rename(
        glyph = 2
      )

    , 'latin' =
      as_tibble(
        chr_latin
        , rownames =
          'character'
        , alphabet = 'latin'
      ) %>%
      rename(
        glyph = 2
      )

  )
  , .id = 'font'
) -> df_alphabets

rm(chr_cyrillic)
rm(chr_greek)
rm(chr_latin)


# [FUNCTIONS] -------------------------------------------------------------
# - Letters data frame ----------------------------------------------------
fun_letters_data <- function(
    chr_font = c('cyrillic', 'greek', 'latin')
    , int_glyph = NULL
    , dbl_scale_ub = 100
    , dbl_scale_lb = 0
    , lgc_upside_down = F
){

  # Arguments validation
  stopifnot(
    "'chr_font' must be 'cyrillic', 'greek', and/or 'latin'." =
      chr_font %in% c('cyrillic', 'greek', 'latin')
  )

  stopifnot(
    "'int_glyph' must be either NULL or an integer vector." =
      any(
        is.numeric(int_glyph),
        is.null(int_glyph)
      )
  )

  stopifnot(
    "'dbl_scale_ub' must be numeric." =
      is.numeric(dbl_scale_ub)
  )

  stopifnot(
    "'dbl_scale_lb' must be numeric." =
      is.numeric(dbl_scale_lb)
  )

  stopifnot(
    "'lgc_upside_down' must be either TRUE or FALSE." =
      all(
        is.logical(lgc_upside_down),
        !is.na(lgc_upside_down)
      )
  )

  # Data wrangling
  dbl_scale_ub[[1]] -> dbl_scale_ub
  dbl_scale_lb[[1]] -> dbl_scale_lb

  chr_font[chr_font == 'latin'] <- 'rowmans'

  hershey::hershey %>%
    filter(
      font %in%
        chr_font
    ) -> df_letters

  df_letters %>%
    drop_na() ->
    df_letters

  # 'rowmans' font to 'latin'
  df_letters %>%
    mutate(
      font =
        if_else(
          font == 'rowmans'
          , 'latin'
          , font
        )
    ) -> df_letters

  # Correct glyph names
  df_letters %>%
    select(
      -char
    ) %>%
    inner_join(
      df_alphabets
    ) -> df_letters

  if(length(int_glyph)){

    df_letters %>%
      filter(
        glyph %in%
          ceiling(
            int_glyph
          )
      ) -> df_letters

  }

  # Upside down characters
  if(lgc_upside_down){

    df_letters %>%
      group_by(font) %>%
      mutate(
        glyph = -glyph,
        character = paste0(
          character,
          '_upside_down'
        ),
        y = -y
      ) %>%
      bind_rows(
        df_letters
      ) -> df_letters

  }

  # Normalize font scales
  df_letters %>%
    group_by(font) %>%
    mutate(
      .after = y
      , item_score =
        y / (max(y) - min(y)) -
        min(y) / (max(y) - min(y))
      , item_score =
        if_else(
          !is.na(item_score)
          , item_score
          , 0.5
        )
      , item_score =
        dbl_scale_ub *
        item_score
    ) %>%
    group_by(
      glyph,
      font
    ) %>%
    mutate(
      .before = item_score
      , item = paste0(
        'item_'
        , 1:n()
      )
    ) %>%
    ungroup() ->
    df_letters

  # Glyph unique id
  df_letters %>%
    mutate(
      .before = 1
      , id_glyph =
        paste0(
          font,
          '_',
          glyph
        )
    ) -> df_letters

  # Add 'df_letters' subclass
  df_letters %>%
    new_data_frame(
      class = c(
        class(df_letters),
        'df_letters'
      )
    ) -> df_letters

  # Output
  return(df_letters)

}

# - Plot letters -----------------------------------------------------------
fun_letters_plot <- function(
    df_letters
    , list_letters_match = NULL
    , dbl_scale_ub = 100
    , dbl_scale_lb = 0
){

  # Arguments validation
  stopifnot(
    "'df_letters' must be a data frame with the 'df_letters' subclass." =
      any(class(df_letters) == 'df_letters')
  )

  stopifnot(
    "'list_letters_match' must be either NULL or a list with the 'list_letters_match' subclass." =
      any(
        is.null(list_letters_match)
        , all(
          any(class(list_letters_match) == 'list_letters_match'),
          any(class(list_letters_match) == 'list')
        )
      )
  )

  stopifnot(
    "'dbl_scale_ub' must be numeric and greater than 'dbl_scale_lb'." =
      all(
        is.numeric(dbl_scale_ub)
        , dbl_scale_ub >
          dbl_scale_lb
      )
  )

  stopifnot(
    "'dbl_scale_lb' must be numeric." =
      is.numeric(dbl_scale_lb)
  )

  # Auxiliary function
  fun_letters_plot_aux <- function(df_letter){

    # Plot letter
    df_letter %>%
      ggplot(aes(
        x = x,
        y =
          dbl_scale_ub[[1]] -
          item_score
      )) +
      geom_path(aes(
        group = stroke
      )) +
      geom_point() +
      coord_equal() +
      scale_y_reverse() +
      ylim(
        dbl_scale_ub[[1]],
        dbl_scale_lb[[1]]
      ) +
      theme_minimal() ->
      plt_letter

    # Output
    return(plt_letter)

  }

  # Plot df_letters
  df_letters %>%
    split(.$id_glyph) %>%
    map(
      fun_letters_plot_aux
    ) -> list_plt_letters

  if(!is.null(list_letters_match)){


    # Data wrangling
    list_letters_match %>%
      map(
        ~ .x %>%
          select(

          )
      )
    df_letters_profile %>%
      slice(1) %>%
      pivot_longer(
        cols = -1
        , names_to = 'item'
        , values_to = 'item_score'
      ) %>%
      arrange(
        item_score
      ) %>%
      mutate(
        item =
          paste0(
            'item_'
            , 1:n()
          )
      ) %>%
      right_join(
        fun_letters_data() %>%
          filter(
            glyph == 86 &
              font == 'latin'
          ) %>%
          fun_letters_profiles(
            int_items = 60
            , lgc_pivot_long = T
          ) %>%
          select(
            x, item
          )
      )

    # Plot profiles
    Map(
      function(match, letter){

        # Plot profile
        letter +
          geom_point(
            data =
              match
          ) -> plt_letter

        # Output
        return(plt_letter)


      }
      , match = list_letters_match
      , letter = rep(
        list_plt_letters
        , length(
          list_letters_match
        )
      )
    ) -> list_plt_letters

  }


  df_letters_profile

  # Plot df_letters_long

  fun_letters_data(
    int_glyph = 86
    , chr_font = 'latin'
    , lgc_upside_down = F
  ) %>%
    ggplot(aes(
      x = x,
      y = 100 - item_score,
      # y = y,
      group = stroke
    )) +
    geom_path() +
    geom_point() +
    coord_equal() +
    theme_minimal() +
    scale_y_reverse() +
    ylim(100, 0)

  df_input %>%
    slice(1) %>%
    pivot_longer(
      cols = -1
      , names_to = 'item'
      , values_to = 'item_score'
    ) %>%
    arrange(
      item_score
    ) %>%
    mutate(
      item =
        paste0(
          'item_'
          , 1:n()
        )
    ) %>%
    right_join(
      fun_letters_data() %>%
        filter(
          glyph == 86 &
            font == 'latin'
        ) %>%
        fun_letters_profiles(
          int_items = 60
          , lgc_pivot_long = T
        ) %>%
        select(
          x, item
        )
    ) %>%
    ggplot(aes(
      x = x,
      y = item_score
    )) +
    geom_path(
      data =
        fun_letters_data() %>%
        filter(
          glyph == 86 &
            font == 'latin'
        ) %>%
        fun_letters_profiles(
          int_items = 9
          , lgc_pivot_long = T
          , chr_id_col =
            'occupation'
        )
      , aes(
        x = x,
        y = item_score,
        group = stroke,
        color = character
      )
    ) +
    geom_point() +
    coord_equal() +
    theme_minimal() +
    scale_y_reverse() +
    ylim(100, 0)

  fun_letters_data() %>%
    filter(
      glyph == 86 &
        font == 'latin'
    ) %>%
    ggplot(aes(
      x = x,
      y = 100 - item_score,
      group = stroke
    )) +
    geom_path() +
    geom_point(
      data =
        df_input %>%
        slice(1) %>%
        pivot_longer(
          cols = -1
          , names_to = 'item'
          , values_to = 'item_score'
        ) %>%
        arrange(
          item_score
        ) %>%
        mutate(
          item =
            paste0(
              'item_'
              , 1:n()
            )
        ) %>%
        right_join(
          fun_letters_data() %>%
            filter(
              glyph == 86 &
                font == 'latin'
            ) %>%
            fun_letters_profiles(
              int_items = 60
              , lgc_pivot_long = T
            ) %>%
            select(
              x, item
            )
        )
      , color = 'red'
      , size = 2.2
    ) +
    # coord_equal() +
    scale_y_reverse() +
    ylim(100, 0) +
    theme_minimal()

  df_input %>%
    slice(1) %>%
    pivot_longer(
      cols = -1
      , names_to = 'item'
      , values_to = 'item_score'
    ) %>%
    arrange(
      item_score
    ) %>%
    mutate(
      item =
        paste0(
          'item_'
          , 1:n()
        )
    ) %>%
    right_join(
      fun_letters_data() %>%
        filter(
          glyph == 86 &
            font == 'latin'
        ) %>%
        fun_letters_profiles(
          int_items = 60
          , lgc_pivot_long = T
        ) %>%
        select(
          x, item
        )
    )

  fun_letters_data() %>%
    filter(
      glyph == 86 &
        font == 'latin'
    ) %>%
    fun_letters_profiles(
      int_items = 9
      , lgc_pivot_long = T
    ) %>%
    ggplot(aes(
      x = x,
      y = 100 - item_score,
      # y = y,
      group = stroke
    )) +
    geom_path() +
    geom_point() +
    coord_equal() +
    theme_minimal() +
    scale_y_reverse() +
    ylim(100, 0)


  fun_letters_data() %>%
    filter(
      glyph == 86 &
        font == 'latin'
    ) %>%
    select(
      item,
      item_score,
      stroke,
      x,
      y
    )

  fun_letters_data() %>%
    filter(
      glyph == 86 &
        font == 'latin'
    ) %>%
    fun_letters_profiles(
      int_items = 9
      , lgc_pivot_long = T
    ) %>%
    arrange(
      stroke
    ) %>%
    select(
      item,
      item_score,
      stroke,
      x,
      y
    ) %>%
    ggplot(aes(
      x = x,
      y = 100 - item_score,
      # y = y,
      group = stroke
    )) +
    geom_path() +
    geom_point() +
    coord_equal() +
    theme_minimal() +
    scale_y_reverse() +
    ylim(100, 0)

  fun_letters_data() %>%
    filter(
      glyph == 86 &
        font == 'latin'
    ) %>%
    fun_letters_profiles(
      int_items = 9
      , lgc_pivot_long = T
    ) %>%
    select(
      item,
      item_score,
      stroke,
      x,
      y
    )

  # Output
  return(list_plt_letters)

}

# - Letters to professional profiles ------------------------------------------
fun_letters_profiles <- function(
    df_letters
    , int_items
    , lgc_pivot_long = F
    , chr_id_col = NULL
){

  # Arguments validation
  stopifnot(
    "'df_letters' must be a data frame with the 'df_letters' subclass." =
      any(class(df_letters) == 'df_letters')
  )

  stopifnot(
    "'int_items' must be numeric." =
      is.numeric(int_items)
  )

  stopifnot(
    "'lgc_pivot_long' must be either TRUE or FALSE." =
      all(
        is.logical(lgc_pivot_long),
        !is.na(lgc_pivot_long)
      )
  )

  stopifnot(
    "'chr_id_col' must be either NULL or a character string." =
      any(
        is.character(chr_id_col)
        , is.null(chr_id_col)
      )
  )

  # Data wrangling
  int_items[[1]] -> int_items

  lgc_pivot_long[[1]] -> lgc_pivot_long

  chr_id_col[[1]] -> chr_id_col

  # Lowest common multiple of row number
  df_letters %>%
    group_by(
      id_glyph
    ) %>%
    mutate(
      lcm_rows =
        numbers::mLCM(c(
          int_items,
          n()
        )),
      rep_rows =
        lcm_rows /
        n()
    ) %>%
    ungroup() ->
    df_letters

  # Convert letters to professional profiles
  df_letters %>%
    group_by(
      id_glyph
    ) %>%
    slice(rep(
      1:n(),
      first(
        rep_rows
      )
    )) %>%
    arrange(
      item_score
    ) %>%
    mutate(
      item =
        rep(
          paste0('item_', 1:int_items)
          , each = n() / int_items
        )
    ) %>%
    group_by(
      id_glyph,
      glyph,
      font,
      item,
      character
    ) %>%
    reframe(
      item_score =
        mean(
          item_score
        )
      , across(
        .fns = first
      )
    ) -> df_letters_profile

  rm(df_letters)
  rm(int_items)

  # Pivot
  if(!lgc_pivot_long){

    df_letters_profile %>%
      pivot_wider(
        id_cols = c(
          'id_glyph',
          'glyph',
          'font',
          'character'
        )
        , names_from = 'item'
        , values_from = 'item_score'
      ) -> df_letters_profile

    # Add 'df_letters_profile' subclass
    df_letters_profile %>%
      new_data_frame(
        class = c(
          class(df_letters_profile),
          'df_letters_profile'
        )
      ) -> df_letters_profile

  } else {

    # Add 'df_letters_profile_long' subclass
    df_letters_profile %>%
      new_data_frame(
        class = c(
          class(df_letters_profile),
          'df_letters_profile_long'
        )
      ) -> df_letters_profile

  }

  # ID column
  if(!is.null(chr_id_col)){

    df_letters_profile$
      id_glyph ->
      df_letters_profile[[
        chr_id_col
      ]]

  }

  # Output
  return(df_letters_profile)

}

# - Letter matching ----------------------------------------------
fun_letters_similarity <- function(
    df_letters_profile
    , df_query_rows
    , chr_method = c(
      'bvls'
      , 'logit'
      , 'probit'
      , 'pearson'
      , 'knn'
    )
    , dbl_scale_ub = 100
    , dbl_scale_lb = 0
    , chr_id_col = NULL
    , lgc_sort = F
){

  # Arguments validation
  stopifnot(
    "'df_letters_profile' must be a data frame with the 'df_letters_profile' subclass." =
      any(class(df_letters_profile) == 'df_letters_profile')
  )

  stopifnot(
    "'df_query_rows' must be a data frame." =
      is.data.frame(df_query_rows)
  )

  stopifnot(
    "'chr_method' must be one of the following methods: 'bvls', 'logit', 'probit', 'pearson', or 'knn'." =
      any(
        chr_method == 'bvls',
        chr_method == 'logit',
        chr_method == 'probit',
        chr_method == 'pearson',
        chr_method == 'knn'
      )
  )

  stopifnot(
    "'dbl_scale_ub' must be numeric and greater than 'dbl_scale_lb'." =
      all(
        is.numeric(dbl_scale_ub)
        , dbl_scale_ub >
          dbl_scale_lb
      )
  )

  stopifnot(
    "'dbl_scale_lb' must be numeric." =
      is.numeric(dbl_scale_lb)
  )

  stopifnot(
    "'lgc_sort' must be either TRUE or FALSE." =
      all(
        is.logical(lgc_sort)
        , !is.na(lgc_sort)
      )
  )

  stopifnot(
    "'chr_id_col' must be either NULL or a character string with the name of an ID column present in both 'df_letters_profile' and 'df_query_rows'." =
      any(
        is.null(chr_id_col)
        , all(
          is.character(chr_id_col)
          , chr_id_col %in% names(df_query_rows)
          , chr_id_col %in% names(df_letters_profile)
        )
      )
  )

  # Data wrangling
  chr_id_col[[1]] -> chr_id_col

  # Reorder items
  df_query_rows %>%
    select(
      chr_id_col,
      where(is.numeric)
    ) %>%
    pivot_longer(
      cols = where(is.numeric)
      , names_to = 'item'
      , values_to = 'item_score'
    ) %>%
    group_by(across(
      chr_id_col
    )) %>%
    arrange(
      item_score
    ) %>%
    mutate(
      item =
        paste0(
          'item_',
          1:n()
        )
    ) %>%
    ungroup() %>%
    pivot_wider(
      names_from = 'item',
      values_from = 'item_score'
    ) -> df_query_rows

  # Apply matching function
  fun_match_similarity(
    df_data_rows = df_letters_profile
    , df_query_rows = df_query_rows
    , chr_method = chr_method
    , dbl_scale_ub = dbl_scale_ub
    , dbl_scale_lb = dbl_scale_lb
    , chr_id_col = chr_id_col
    , lgc_sort = lgc_sort
  ) -> list_letters_match

  # Data wrangling
  list_letters_match$
    list_similarity %>%
    map(
      ~ .x %>%
        as_tibble(
          rownames = 'id_glyph'
        ) %>%
        rename(
          similarity = value
        )
    ) -> list_similarity

  rm(list_letters_match)

  # Add 'list_letters_match' subclass
  c(
    class(list_similarity)
    , 'list_letters_match'
  ) -> class(
    list_similarity
  )

  # Output
  return(list_similarity)

}

# # [TEST] ------------------------------------------------------------------
# # - Letters vs occupations match ------------------------------------------
# fun_letters_similarity(
#   df_letters_profile =
#     fun_letters_data() %>%
#     fun_letters_profiles(
#       int_items = 120
#       , chr_id_col =
#         'occupation'
#       , lgc_pivot_long = F
#     )
#   , df_query_rows =
#     df_occupations %>%
#     select(
#       occupation,
#       starts_with('skl_'),
#       starts_with('abl_'),
#       starts_with('knw_')
#     ) %>%
#     slice(1)
#   , chr_method =
#     'bvls'
#   , dbl_scale_ub = 100
#   , dbl_scale_lb = 0
#   , chr_id_col =
#     'occupation'
#   , lgc_sort = F
# ) -> list_letters_match
#
# # fun_letters_data() %>%
# #   fun_letters_profiles(
# #     int_items = 120
# #     , chr_id_col =
# #       'occupation'
# #     , lgc_pivot_long = F
# #   ) %>%
# #   class()
# #
# #
# # df_occupations %>%
# #   select(
# #     occupation,
# #     starts_with('skl_'),
# #     starts_with('abl_'),
# #     starts_with('knw_')
# #   ) %>%
# #   pivot_longer(
# #     cols = -1
# #     , names_to = 'item'
# #     , values_to = 'item_score'
# #   ) %>%
# #   group_by(
# #     occupation
# #   ) %>%
# #   arrange(
# #     item_score
# #   ) %>%
# #   mutate(
# #     item = paste0('item_', 1:n())
# #   ) %>%
# #   pivot_wider(
# #     id_cols = 1
# #     , names_from = 'item'
# #     , values_from = 'item_score'
# #   ) %>%
# #   ungroup() -> dsds
# #
# # fun_letters_data() %>%
# #   fun_letters_profiles(
# #     int_items =
# #       ncol(dsds) - 1
# #     , chr_id_col =
# #       'occupation'
# #     , lgc_pivot_long = F
# #   ) -> lalala
# #
# # fun_letters_data(
# #   chr_font = 'greek'
# #   , int_glyph = 64
# #   , dbl_scale_ub = 100
# #   , dbl_scale_lb = 0
# #   , lgc_upside_down = F
# # )
# #
# # lalala %>%
# #   filter(
# #     font == 'greek'
# #     , glyph == 64
# #   ) %>%
# #   pull(
# #     item_score
# #   )
# #
# # library(atlas.gene)
# # library(atlas.comp)
# #
# # # dsds %>%
# # #   group_by(
# # #     occupation
# # #   ) %>%
# # #   pivot_longer(
# # #     cols = -1,
# # #     names_to = 'item',
# # #     values_to = 'item_score'
# # #   ) %>%
# # lalala %>%
# #   group_by(
# #     glyph,
# #     font
# #   ) %>%
# #   mutate(
# #     generality =
# #       fun_gene_generality(
# #         dbl_profile = item_score
# #         , dbl_scale_ub = 100
# #         , dbl_scale_lb = 0
# #       ) %>%
# #       as.numeric()
# #     , competence =
# #       fun_comp_competence(
# #         dbl_profile = item_score
# #         , dbl_scale_ub = 100
# #         , dbl_scale_lb = 0
# #         , generality
# #       )
# #   ) %>%
# #   slice(1) %>%
# #   ungroup() %>%
# #   mutate(
# #     gene_class =
# #       fun_class_classifier(
# #         dbl_var = generality
# #         , dbl_scale_lb = 0
# #         , dbl_scale_ub = 1
# #         , int_levels = 6
# #         , chr_class_labels = rev(c(
# #           'very generalist',
# #           'generalist',
# #           'somewhat generalist',
# #           'somewhat specialist',
# #           'specialist',
# #           'very specialist'
# #         ))
# #       )
# #     , comp_class =
# #       fun_class_classifier(
# #         dbl_var = competence
# #         , dbl_scale_lb = 0
# #         , dbl_scale_ub = 1
# #         , int_levels = 6
# #         , chr_class_labels = rev(c(
# #           'very competent',
# #           'competent',
# #           'somewhat competent',
# #           'somewhat incompetent',
# #           'incompetent',
# #           'very incompetent'
# #         ))
# #       )
# #   ) %>%
# #   group_by(
# #     # comp_class,
# #     gene_class
# #   ) %>%
# #   tally() %>%
# #   print(
# #     n = Inf
# #   )
# #
# # reframe(
# #   min = min(generality),
# #   max = max(generality)
# # )
# #
# # select(
# #   occupation
# #   , generality
# # ) %>%
# #   fun_plot.density(aes(
# #     x = generality
# #   )
# #   , .list_axis.x.args = list(
# #     limits = c(-.19, 1.19)
# #     , breaks = seq(0, 1, .25)
# #   )
# #   , .fun_format.x = percent
# #   )
# #
# # atlas.gene::fun_gene_generality(rep(0,120), 0)
# #
# # atlas.comp::fun_comp_competence()
# #
# # atlas.match::fun_match_similarity(
# #   df_data_rows =
# #     lalala %>%
# #     filter(
# #       font == 'cyrillic'
# #     )
# #   , df_query_rows =
# #     dsds %>%
# #     slice(1)
# #   # , chr_method = 'logit'
# #   , chr_method = 'pearson'
# #   , dbl_scale_ub = 100
# #   , dbl_scale_lb = 0
# #   , chr_id_col =
# #     'occupation'
# # ) -> dsdsds
# #
# # dsdsds
# #
# # dsdsds$
# #   df_similarity %>%
# #   select(
# #     glyph,
# #     font,
# #     similarity
# #   ) %>%
# #   arrange(desc(
# #     similarity
# #   ))
# #
# # fun_letters_data(
# #   chr_font = 'cyrillic'
# #   , int_glyph = 19
# #   , dbl_scale_ub = 100
# #   , dbl_scale_lb = 0
# #   , lgc_upside_down = F
# # ) %>%
# #   # filter(
# #   #   glyph < 0
# #   # ) %>%
# #   fun_letters_plot()
# #
# # fun_letters_data(
# #   chr_font = 'cyrillic'
# #   , int_glyph = 69
# #   , dbl_scale_ub = 100
# #   , dbl_scale_lb = 0
# #   , lgc_upside_down = F
# # ) %>%
# #   mutate(
# #     .before = 1
# #     , occupation =
# #       paste0(
# #         glyph, '_', font
# #       )
# #   ) %>%
# #   ggplot(aes(
# #     x = x,
# #     y = item_score,
# #     # group = stroke,
# #     color = occupation
# #   )) +
# #   geom_path(aes(
# #     group = stroke
# #   )
# #   , linewidth = 1.19
# #   ) +
# #   geom_point(
# #     data =
# #       dsds %>%
# #       slice(1) %>%
# #       pivot_longer(
# #         cols = -1
# #         , names_to = 'item'
# #         , values_to = 'item_score'
# #       ) %>%
# #       right_join(
# #         fun_letters_data(
# #           int_glyph = 69
# #           , chr_font = 'cyrillic'
# #           , lgc_upside_down = F
# #         ) %>%
# #           # filter(
# #           #   glyph < 0
# #           # ) %>%
# #           fun_letters_profiles(
# #             int_items = 120
# #             , lgc_pivot_long = T
# #           ) %>%
# #           select(
# #             x, item
# #           )
# #       )
# #     , size = 3
# #     , alpha = 0.9
# #   ) +
# #   coord_equal() +
# #   theme_minimal() +
# #   scale_y_reverse() +
# #   ylim(0, 100)
# #
# # bind_rows(
# #   dsds %>%
# #     slice(1) %>%
# #     pivot_longer(
# #       cols = -1
# #       , names_to = 'item'
# #       , values_to = 'item_score'
# #     ) %>%
# #     right_join(
# #       fun_letters_data(
# #         int_glyph = 69
# #         , chr_font = 'cyrillic'
# #         , lgc_upside_down = F
# #       ) %>%
# #         # filter(
# #         #   glyph < 0
# #         # ) %>%
# #         fun_letters_profiles(
# #           int_items = 120
# #           , lgc_pivot_long = T
# #         ) %>%
# #         select(
# #           x, item
# #         )
# #     )
# #
# # ) %>%
# #   ggplot(aes(
# #     x = x,
# #     y = item_score,
# #     group = stroke,
# #     color = occupation
# #   )) +
# #   geom_path() +
# #   geom_point() +
# #   facet_grid(
# #     rows = vars(occupation)
# #   )
# # coord_equal() +
# #   theme_minimal() +
# #   # scale_y_reverse() +
# #   ylim(100, 0)
# #
# # dsds %>%
# #   slice(1) %>%
# #   pivot_longer(
# #     cols = -1
# #     , names_to = 'item'
# #     , values_to = 'item_score'
# #   ) %>%
# #   right_join(
# #     fun_letters_data(
# #       int_glyph = 69
# #       , chr_font = 'cyrillic'
# #       , lgc_upside_down = F
# #     ) %>%
# #       # filter(
# #       #   glyph < 0
# #       # ) %>%
# #       fun_letters_profiles(
# #         int_items = 120
# #         , lgc_pivot_long = T
# #       ) %>%
# #       select(
# #         x, item
# #       )
# #   ) %>%
# #   ggplot(aes(
# #     x = x,
# #     y = item_score,
# #     # y = y,
# #     # group = stroke
# #   )) +
# #   # geom_path() +
# #   geom_point() +
# #   coord_equal() +
# #   theme_minimal() +
# #   scale_y_reverse() +
# #   ylim(100, 0)
# #
# # dsdsds$
# #   df_similarity %>%
# #   select(
# #     glyph,
# #     font,
# #     similarity
# #   ) %>%
# #   arrange(desc(
# #     similarity
# #   )) %>%
# #   fun_plot.density(aes(
# #     x = similarity
# #   )
# #   , .list_axis.x.args = list(
# #     limits = c(-0.19, 1.19)
# #     , breaks = seq(0, 1, 0.25)
# #   )
# #   , .fun_format.x = percent
# #   )
# #
# # fun_letters_data() %>%
# #   filter(
# #     glyph == -1,
# #     font == 'cyrillic'
# #   ) %>%
# #   fun_letters_profiles(
# #     int_items = 120
# #   ) %>%
# #   atlas.match::fun_match_similarity(
# #     df_query_rows =
# #       dsds %>%
# #       slice(1)
# #     , chr_method = 'bvls'
# #     , dbl_scale_ub = 100
# #     , dbl_scale_lb = 0
# #     , chr_id_col =
# #       'occupation'
# #   ) -> lalala
# #
# # lalala$
# #   df_similarity$
# #   similarity

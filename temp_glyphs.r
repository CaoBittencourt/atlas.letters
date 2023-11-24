library(atlas.letters)

fun_letters_data(
  lgc_upside_down = F
) -> df_letters

df_letters %>%
  split(.$font) %>%
  map(
    ~ fun_letters_plot(.x) +
      facet_wrap(
        facets = vars(glyph)
        , ncol = 20
      )
  ) -> list_alphabets

list_alphabets$cyrillic
c(
  # Uppercase letters
  'Ah' = 34,
  'Beh' = 35,
  'Veh' = 55,
  'Geh' = 40,
  'Deh' = 37,
  'Yeh' = 60,
  'Zheh' = 50,
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

list_alphabets$greek
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

list_alphabets$latin
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

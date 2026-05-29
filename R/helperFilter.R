#' Erste Silbe einer IPA-Transkription extrahieren
#'
#' Gibt die erste Silbe einer durch Punkte segmentierten IPA-Transkription
#' zurück. Silbengrenzen werden durch `"."` markiert.
#'
#' @param ipa_string Character. Eine IPA-Transkription, bei der Silbengrenzen
#'   durch `"."` getrennt sind (z.B. `"ˈhaʊ.zən"`).
#'
#' @return Character. Die erste Silbe. Wenn kein Punkt vorhanden ist, wird der
#'   gesamte String zurückgegeben.
#'
#' @export
#'
#' @examples
#' extract_first_syllable("ˈhaʊ.zən")  # "ˈhaʊ"
#' extract_first_syllable("haʊs")       # "haʊs"
extract_first_syllable <- function(ipa_string) {
  strsplit(ipa_string, "\\.")[[1]][1]
}

#' Letzte Silbe einer IPA-Transkription extrahieren
#'
#' Gibt die letzte Silbe einer durch Punkte segmentierten IPA-Transkription
#' zurück.
#'
#' @param ipa_string Character. Eine IPA-Transkription mit `"."` als
#'   Silbentrennzeichen.
#'
#' @return Character. Die letzte Silbe. Wenn kein Punkt vorhanden ist, wird der
#'   gesamte String zurückgegeben.
#'
#' @export
#'
#' @examples
#' extract_last_syllable("ˈhaʊ.zən")  # "zən"
#' extract_last_syllable("haʊs")       # "haʊs"
extract_last_syllable <- function(ipa_string) {
  parts <- strsplit(ipa_string, "\\.")[[1]]
  parts[length(parts)]
}

#' Betonte Silbe einer IPA-Transkription extrahieren
#'
#' Gibt die primär betonte Silbe zurück, erkennbar am Hauptakzentzeichen `ˈ`.
#' Falls kein Punkt (keine Silbensegmentierung) vorhanden ist, wird der gesamte
#' String als betont zurückgegeben. Falls kein `ˈ` gefunden wird, gilt die
#' erste Silbe als Fallback.
#'
#' @param ipa_string Character. Eine IPA-Transkription mit `"."` als
#'   Silbentrennzeichen und `ˈ` als Hauptakzentmarkierung.
#'
#' @return Character. Die betonte Silbe oder Fallback-Wert.
#'
#' @export
#'
#' @examples
#' extract_stressed_syllable("ˈhaʊ.zən")   # "ˈhaʊ"
#' extract_stressed_syllable("haʊs")        # "haʊs"
#' extract_stressed_syllable("haʊ.ˈzən")   # "ˈzən"
extract_stressed_syllable <- function(ipa_string) {
  if (!grepl("\\.", ipa_string)) {
    return(ipa_string)
  }

  parts <- strsplit(ipa_string, "\\.")[[1]]

  stressed_idx <- grep("ˈ", parts)
  if (length(stressed_idx) > 0) {
    return(parts[stressed_idx[1]])
  }

  return(parts[1])
}

#' Nebenakzentuierte Silben einer IPA-Transkription extrahieren
#'
#' Gibt alle Silben mit Nebenakzentmarkierung `ˌ` zurück. Wenn keine Silben
#' segmentiert sind oder keine Nebenakzentsilbe vorhanden ist, wird ein leerer
#' Vektor zurückgegeben.
#'
#' @param ipa_string Character. Eine IPA-Transkription mit `"."` als
#'   Silbentrennzeichen und `ˌ` als Nebenakzentmarkierung.
#'
#' @return Character vector. Die nebenakzentuierten Silben, oder `character(0)`
#'   wenn keine vorhanden sind.
#'
#' @export
#'
#' @examples
#' extract_secondary_stressed_syllables("ˌhaʊ.ˈzən.ˌbaw")  # c("ˌhaʊ", "ˌbaw")
#' extract_secondary_stressed_syllables("ˈhaʊs")            # character(0)
extract_secondary_stressed_syllables <- function(ipa_string) {
  if (!grepl("\\.", ipa_string)) {
    return(character(0))
  }

  parts <- strsplit(ipa_string, "\\.")[[1]]
  secondary_idx <- grep("ˌ", parts)

  if (length(secondary_idx) > 0) {
    return(parts[secondary_idx])
  }

  return(character(0))
}

#' IPA-Labels nach Silbentyp filtern
#'
#' Filtert einen Vektor von IPA-Transkriptionen nach dem Inhalt einer bestimmten
#' Silbenposition. Unterstützte Silbentypen: `"Erste Silbe"`, `"Letzte Silbe"`,
#' `"Betonte Silben"`. Für Nebenakzentsilben siehe [filter_by_secondary_stress()].
#'
#' @param labels Character vector. IPA-Transkriptionen.
#' @param syllable_type Character. Silbenposition: `"Erste Silbe"`,
#'   `"Letzte Silbe"` oder `"Betonte Silben"`.
#' @param operator Character. Vergleichsoperator: `"beginnt mit"`,
#'   `"endet mit"` oder `"enthält"`.
#' @param value Character. IPA-Zeichenkette, nach der gefiltert wird.
#'
#' @return Character vector. Nur die Labels, bei denen die gewählte Silbe den
#'   Filter erfüllt.
#'
#' @seealso [filter_by_secondary_stress()], [apply_single_filter()]
#'
#' @export
#'
#' @examples
#' labels <- c("ˈhaʊ.zən", "ˈkaʊ.fən", "ˈlaʊ.fən")
#' filter_by_syllable(labels, "Erste Silbe", "beginnt mit", "ˈh")
#' # "ˈhaʊ.zən"
filter_by_syllable <- function(labels, syllable_type, operator, value) {
  filtered <- labels

  for (label in labels) {
    syllable_content <- switch(
      syllable_type,
      "Erste Silbe"    = extract_first_syllable(label),
      "Letzte Silbe"   = extract_last_syllable(label),
      "Betonte Silben" = extract_stressed_syllable(label),
      ""
    )

    match <- switch(
      operator,
      "beginnt mit" = grepl(paste0("^", value), syllable_content),
      "endet mit"   = grepl(paste0(value, "$"), syllable_content),
      "enthält"     = grepl(value, syllable_content),
      FALSE
    )

    if (!match) {
      filtered <- filtered[filtered != label]
    }
  }

  return(filtered)
}

#' IPA-Labels nach nebenakzentuierter Silbe filtern
#'
#' Behält nur Labels, bei denen mindestens eine Silbe mit Nebenakzent (`ˌ`)
#' den angegebenen Operator-Wert-Vergleich erfüllt.
#'
#' @param labels Character vector. IPA-Transkriptionen.
#' @param operator Character. Vergleichsoperator: `"beginnt mit"`,
#'   `"endet mit"` oder `"enthält"`.
#' @param value Character. IPA-Zeichenkette, nach der gefiltert wird.
#'
#' @return Character vector. Nur die Labels mit einer passenden
#'   nebenakzentuierten Silbe.
#'
#' @seealso [filter_by_syllable()], [apply_single_filter()]
#'
#' @export
#'
#' @examples
#' labels <- c("ˌhaʊ.ˈzən", "ˈkaʊ.fən", "ˌlaʊ.ˈfən")
#' filter_by_secondary_stress(labels, "beginnt mit", "ˌh")
#' # "ˌhaʊ.ˈzən"
filter_by_secondary_stress <- function(labels, operator, value) {
  filtered <- c()

  for (label in labels) {
    secondary_syllables <- extract_secondary_stressed_syllables(label)

    if (length(secondary_syllables) > 0) {
      for (syllable in secondary_syllables) {
        match <- switch(
          operator,
          "beginnt mit" = grepl(paste0("^", value), syllable),
          "endet mit"   = grepl(paste0(value, "$"), syllable),
          "enthält"     = grepl(value, syllable),
          FALSE
        )

        if (match) {
          filtered <- c(filtered, label)
          break
        }
      }
    }
  }

  return(filtered)
}

#' Einen einzelnen IPA-Filter auf einen Labelvektor anwenden
#'
#' Filtert einen Vektor von IPA-Labels anhand eines einzelnen Filterkriteriums
#' bestehend aus Silbenposition, Operator und Wert.
#'
#' @param labels Character vector. IPA-Transkriptionen.
#' @param filter_silbe Character. Silbenposition: `"Alle Silben"`,
#'   `"Erste Silbe"`, `"Letzte Silbe"`, `"Betonte Silben"` oder
#'   `"Silbe mit Nebenakzent"`.
#' @param filter_operator Character. Vergleichsoperator: `"beginnt mit"`,
#'   `"endet mit"` oder `"enthält"`.
#' @param filter_value Character. IPA-Zeichenkette, nach der gefiltert wird.
#'
#' @return Character vector. Gefilterter Labelvektor.
#'
#' @seealso [apply_multiple_filters()]
#'
#' @export
#'
#' @examples
#' labels <- c("ˈhaʊ.zən", "ˈkaʊ.fən", "ˈlaʊ.fən")
#' apply_single_filter(labels, "Alle Silben", "enthält", "haʊ")
#' # "ˈhaʊ.zən"
apply_single_filter <- function(
  labels,
  filter_silbe,
  filter_operator,
  filter_value
) {
  if (filter_silbe == "Alle Silben") {
    return(switch(
      filter_operator,
      "beginnt mit" = labels[grepl(paste0("^", filter_value), labels)],
      "endet mit"   = labels[grepl(paste0(filter_value, "$"), labels)],
      "enthält"     = labels[grepl(filter_value, labels)],
      labels
    ))
  } else if (filter_silbe == "Silbe mit Nebenakzent") {
    return(filter_by_secondary_stress(labels, filter_operator, filter_value))
  } else {
    return(filter_by_syllable(labels, filter_silbe, filter_operator, filter_value))
  }
}

#' Mehrere IPA-Filter auf einen Labelvektor anwenden
#'
#' Wendet eine Reihe von Filterkriterien sequenziell (UND-Verknüpfung) auf
#' einen Vektor von IPA-Transkriptionen an. Wird im Sortiermodul
#' (`createAnalysis`) eingesetzt, um die angezeigten IPA-Tokens einzuschränken.
#'
#' @param labels Character vector. IPA-Transkriptionen.
#' @param filters_df Data frame mit den Spalten `name` (Character),
#'   `silbe` (Character), `operator` (Character) und `value` (Character).
#'   Jede Zeile beschreibt einen Filter.
#' @param active_filter_names Character vector oder `NULL`. Namen der
#'   anzuwendenden Filter aus `filters_df$name`. Falls `NULL`, werden alle
#'   Filter angewendet.
#'
#' @return Character vector. Labels, die alle aktiven Filter erfüllen.
#'
#' @seealso [apply_single_filter()]
#'
#' @export
#'
#' @examples
#' labels <- c("ˈhaʊ.zən", "ˈkaʊ.fən", "ˈlaʊ.fən")
#' filters <- data.frame(
#'   name     = "h-Filter",
#'   silbe    = "Erste Silbe",
#'   operator = "beginnt mit",
#'   value    = "ˈh"
#' )
#' apply_multiple_filters(labels, filters, active_filter_names = "h-Filter")
#' # "ˈhaʊ.zən"
apply_multiple_filters <- function(
  labels,
  filters_df,
  active_filter_names = NULL
) {
  if (nrow(filters_df) == 0) {
    return(labels)
  }

  if (!is.null(active_filter_names) && length(active_filter_names) > 0) {
    filters_df <- filters_df |> dplyr::filter(name %in% active_filter_names)
  }

  if (nrow(filters_df) == 0) {
    return(labels)
  }

  result <- labels
  for (i in seq_len(nrow(filters_df))) {
    filter_row <- filters_df[i, ]
    result <- apply_single_filter(
      result,
      filter_row$silbe,
      filter_row$operator,
      filter_row$value
    )
    if (length(result) == 0) break
  }

  return(result)
}

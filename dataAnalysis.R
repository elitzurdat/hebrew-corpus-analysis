# ============================================================================
#  HEBREW CORPUS ANALYSIS WITH STANZA IN R
#  A step-by-step annotated script for PhD students
# ============================================================================
#
#  WHAT THIS SCRIPT DOES:
#  This script takes a Hebrew speech corpus (Excel format), processes it with
#  the Stanza NLP library (via Python), and produces:
#    - Frequency tables: word tokens & types, POS distributions, within-POS
#      word lists, and utterance-level word counts
#    - Summary statistics for utterance length (mean length of utterance / MLU)
#    - Five plots visualizing the above
#    - CSV exports of all tables
#
#  WHO IS THIS FOR:
#  PhD students beginning to work with NLP tools and corpus linguistics in R.
#  We use the `reticulate` package as the bridge between R and Python, which
#  lets us call Python libraries (like Stanza) from within R. This is useful
#  because most modern NLP tools are Python-first, but R is often more
#  convenient for statistics and visualization.
#
#  DATA FORMAT EXPECTED:
#  An .xlsx file with 8 columns:
#    1. מס' רציף    – running ID
#    2. גיל בהקלטה  – age at recording (CHAT format: YY;MM;DD)
#    3. זמן בהקלטה  – time within recording
#    4. הערות        – notes
#    5. דובר         – speaker code (1=אמא, 2=אבא, 3=תמר [target child], 4=אחות)
#    6. מבע           – utterance (Hebrew text) ← THIS IS WHAT WE ANALYZE
#    7. כתיב פונטי  – phonetic transcription (when applicable)
#    8. מילת מטרה   – target word
#
#  PREREQUISITES (run once before using this script):
#  In Python (inside your conda env "heb_nlp"):
#    pip install stanza
#    python -c "import stanza; stanza.download('he')"
#  In R:
#    install.packages(c("reticulate", "readxl", "tidyverse", "ggplot2",
#                        "scales", "tidytext"))
#
#  AUTHOR NOTE:
#  Written for use with Stanza v1.9+. Stanza is trained on the Hebrew Treebank
#  (HTB), which is newswire Hebrew. Child speech and CDS may cause some
#  degraded POS/lemma accuracy — keep this in mind when interpreting results.
# ============================================================================


# ── SECTION 0: Load packages ─────────────────────────────────────────────────
#
# reticulate: The R-Python bridge. It lets us import Python modules and call
#   their functions as if they were R functions. Think of it as a translator
#   sitting between two people who speak different languages.
#
# readxl: Reads .xlsx files into R data frames. Base R can't do this natively.
#
# tidyverse: A collection of packages for data wrangling and visualization.
#   The key ones here are:
#     - dplyr:   for data manipulation (filter, mutate, group_by, summarise...)
#     - tidyr:   for reshaping data (pivot_longer, pivot_wider)
#     - ggplot2: for plotting
#     - readr:   for write_csv
#     - stringr: for string operations
#
# scales: Extends ggplot2 with useful axis formatters (e.g., percent labels)
#
# tidytext: Provides reorder_within() — useful for ordering bars within facets

library(reticulate)
library(readxl)
library(tidyverse)
library(ggplot2)
library(scales)
library(tidytext)   # for reorder_within()
library(progress)


# ── SECTION 1: Initialize Stanza via reticulate ───────────────────────────────
#
# use_condaenv(): Tells reticulate which Python environment to use.
#   You probably created "heb_nlp" as a conda environment that has Stanza
#   installed. Always specify this explicitly — otherwise reticulate may pick
#   up the wrong Python and fail to find Stanza.
#
# stanza$Pipeline('he'):
#   This creates an NLP pipeline for Hebrew. Under the hood, it loads several
#   trained neural models:
#     - tokenize:  splits raw text into sentences and tokens (words)
#     - mwt:       multi-word token expansion (important for Hebrew: e.g.,
#                  "שאכלתי" → "ש" + "אכלתי")
#     - pos:       assigns a Universal POS tag (UPOS) and language-specific
#                  tag (XPOS) to each token
#     - lemma:     finds the dictionary form of each word
#     - depparse:  builds a dependency parse tree (relations between words)
#     - ner:       named entity recognition (persons, locations, etc.)
#
#   For this script we mainly use tokenize, mwt, pos, and lemma.
#   All models run on CPU by default (slower but no GPU required).

use_condaenv("heb_nlp")
stanza <- import("stanza")

message("Loading Stanza Hebrew pipeline — this may take ~10 seconds...")
nlp <- stanza$Pipeline('he')
message("Stanza loaded successfully.")

# Speaker code → readable name mapping.
# We store this as a named character vector in R.
# The names (1, 2, 3, 4) match the numeric speaker codes in the data.
speaker_labels <- c(
  "1" = "אמא",
  "2" = "אבא",
  "3" = "תמר",   # target child
  "4" = "אחות"
)


# ── SECTION 2: Load and clean the data ───────────────────────────────────────
#
# read_excel() reads the first sheet of the .xlsx file.
# The Hebrew column names are unwieldy to type repeatedly, so we rename them
# with rename() using positional indexing (1, 2, 3...) — safer than typing
# Hebrew headers since encoding can cause issues.
#
# filter(!is.na(utterance)) removes rows with no text.
# nchar(trimws(utterance)) > 0 removes rows that are whitespace-only.
# trimws() strips leading/trailing spaces before counting characters.
#
# mutate() adds new columns:
#   - speaker as character (so it can be used as a factor/label, not a number)
#   - speaker_name using our lookup vector above
#   - utt_word_count: number of whitespace-delimited tokens per utterance
#     NOTE: this is a rough count from the raw text, NOT from Stanza's
#     tokenization. We use str_count(utterance, "\\S+") which counts
#     sequences of non-whitespace characters. This is fast and useful for
#     MLU-style analyses before we even run NLP.

# Hebrew Wh-words and question markers
wh_words <- c("מה", "מי", "איפה", "איפוא", "מתי", "למה", "מדוע",
              "איך", "כיצד", "כמה", "האם", "הם", "אם")



df <- read_excel("20180613080504.xlsx") |>
  rename(
    id        = 1,
    age       = 2,
    time      = 3,
    notes     = 4,
    speaker   = 5,
    utterance = 6,
    phonetic  = 7,
    target    = 8
  ) |>
  filter(
    !is.na(utterance),
    nchar(trimws(utterance)) > 0
  ) |>
  mutate(
    speaker        = as.character(speaker),
    speaker_name = ifelse(speaker %in% names(speaker_labels),
                            speaker_labels[speaker],
                            paste0("Speaker_", speaker)),  # fallback instead of NA
    utt_word_count = str_count(utterance, "\\S+"),
    # Detect questions from the raw text — more reliable than relying on
    # Stanza's PUNCT tokenization, which can miss or mis-tag ? in child speech.
    # str_detect() returns TRUE/FALSE; we do this BEFORE any NLP filtering.
    is_question = str_detect(utterance, "\\?") |
      str_detect(utterance, paste0("^(", paste(wh_words, collapse="|"), ")\\b"))
  )

# Quick sanity check: how many utterances per speaker?
message("\nUtterance counts per speaker:")
print(count(df, speaker_name))


# ── SECTION 3: Utterance word-count summaries ─────────────────────────────────
#
# Before running the heavy NLP, we compute descriptive statistics on utterance
# length. This is useful as a first-pass MLU (Mean Length of Utterance) proxy.
#
# We use group_by() + summarise() — one of the most important patterns in dplyr.
# group_by() partitions the data frame into groups;
# summarise() collapses each group into a single summary row.
#
# Functions used:
#   mean()   – arithmetic mean
#   sd()     – standard deviation
#   median() – median (more robust than mean for skewed data)
#   min(), max() – range
#   n()      – number of rows in the group (= number of utterances)


# Per-age summary: how does utterance length change across recording sessions?
# This is useful if you have data from multiple recording points.
utt_length_by_age <- df |>
  group_by(speaker, speaker_name, age) |>
  summarise(
    n_utterances = n(),
    mean_wc      = round(mean(utt_word_count), 2),
    sd_wc        = round(sd(utt_word_count),   2),
    .groups = "drop"
  ) |>
  arrange(speaker, age)

message("\nUtterance length by age:")
print(utt_length_by_age)


# ── SECTION 4: Stanza parsing — building the token table ─────────────────────
#
# This is the core NLP step. We send each utterance to Stanza and get back
# a structured linguistic annotation for every word.
#
# HOW STANZA WORKS (conceptually):
#   nlp("some text") returns a Document object.
#   A Document contains Sentences.
#   Each Sentence contains Words (tokens after MWT expansion).
#   Each Word has attributes: .text, .lemma, .upos, .xpos, .feats, .id, etc.
#
#   We "unpack" these nested Python objects into a flat R tibble, where
#   each row = one word token.
#
# ABOUT THE FUNCTION BELOW:
#   parse_utterances() takes a vector of utterance strings and their IDs,
#   iterates over them, calls nlp() on each, then extracts word-level info.
#
#   We use list() to accumulate rows and bind_rows() at the end — this is
#   more efficient than rbind() inside a loop, which copies the whole data
#   frame on every iteration (slow for large corpora).
#
# NOTE ON NULL HANDLING:
#   word$feats can be NULL in Python (no morphological features tagged).
#   R doesn't like NULL inside tibbles, so we check with is.null() and
#   substitute NA_character_ (a typed missing value for character columns).


# count_morphemes_from_feats()
#
# For a single `feats` string like "Gender=Fem|Number=Sing|Person=1",
# this function estimates the number of morphemes it represents.
#
# The logic follows a simplified version of CHAT/CLAN morpheme-counting rules
# adapted for Hebrew UD annotation:
#
#   Base assumption: every content word = 1 morpheme (the stem)
#   Each inflectional feature adds 1 morpheme on top of the stem, EXCEPT:
#     - Gender and Number together = 1 morpheme (agreement affix is one suffix)
#     - Person alone = 1 morpheme (verb agreement)
#     - Tense/Aspect/Mood = 1 morpheme (TAM morpheme)
#     - Definite = 1 morpheme (ה definiteness prefix)
#     - Poss (possessive) = 1 morpheme
#
#   Function words (ADP, DET, CCONJ, SCONJ, PART, AUX) = 1 morpheme flat,
#   since their features reflect agreement rather than independent morphemes.
#
# This is an approximation — full morpheme counting would require a dedicated
# morphological analyzer (e.g., YAP). But for MLU estimation across speakers
# it gives a meaningful signal beyond simple word count.
#
# Arguments:
#   feats_str : character, the feats string from Stanza (or NA)
#   upos      : character, the UPOS tag for this token
#
# Returns: integer, estimated morpheme count for this token

count_morphemes_from_feats <- function(feats_str, upos) {

  # ── Function words: always 1 morpheme ─────────────────────────────────────
  # These are grammatical words whose internal structure we don't decompose:
  # prepositions (ADP), determiners (DET), conjunctions (CCONJ/SCONJ),
  # particles (PART), and punctuation.
  function_pos <- c("ADP", "DET", "CCONJ", "SCONJ", "PART",
                    "PUNCT", "SYM", "X")
  if (upos %in% function_pos) return(1L)

  # ── No features: just the lexeme ──────────────────────────────────────────
  if (is.na(feats_str) || feats_str == "") return(1L)

  # Parse the feats string into a named vector for easy lookup
  # e.g. "HebBinyan=PAAL|Tense=Past|Person=1|Number=Sing|Gender=Masc"
  # → c(HebBinyan="PAAL", Tense="Past", Person="1", ...)
  feats_pairs <- str_split(feats_str, "\\|")[[1]]
  feat_names  <- str_extract(feats_pairs, "^[^=]+")
  feat_values <- str_extract(feats_pairs, "[^=]+$")
  names(feat_values) <- feat_names

  # ── VERBS: root + binyan + TAM + agreement ────────────────────────────────
  #
  # Hebrew verb morphology has four strata:
  #
  #   1. Root (שורש)   — the consonantal skeleton, e.g. כ.ת.ב
  #                      Always present in any verb form. = 1 morpheme
  #
  #   2. Binyan (בניין) — the morphological template that wraps around the root,
  #                      encoding voice and often aspect/causativity:
  #                      PA'AL, NIF'AL, PI'EL, PU'AL, HIF'IL, HUF'AL, HITPA'EL
  #                      Stanza tags this as HebBinyan in feats. = 1 morpheme
  #
  #   3. TAM            — tense/aspect/mood morpheme (past, present, future,
  #                      infinitive, imperative). = 1 morpheme
  #
  #   4. Agreement      — person + gender + number, realized as a single
  #                      portmanteau suffix (כתב-תם = past + 2pl.masc). = 1 morpheme
  #
  # Total for a fully inflected verb like "כתבתם" = 4 morphemes
  # Compare to our previous count which gave 3 (stem + TAM + agreement)

  if (upos == "VERB") {
    morpheme_count <- 1L  # Root — always present

    # Binyan: Stanza tags this as HebBinyan=PAAL/NIFAL/PIEL etc.
    # If tagged, it's a real morpheme. If absent (e.g. foreign words,
    # some edge cases) we don't add it.
    if ("HebBinyan" %in% feat_names) {
      morpheme_count <- morpheme_count + 1L
    }

    # TAM morpheme
    tam_feats <- c("Tense", "VerbForm", "Aspect", "Mood")
    if (any(tam_feats %in% feat_names)) {
      morpheme_count <- morpheme_count + 1L
    }

    # Agreement: person + gender + number bundled as one portmanteau suffix
    # (In Hebrew you can't separate "3rd person" from "masculine plural" —
    # כתב-ו encodes all three at once)
    agr_feats <- c("Person", "Gender", "Number")
    if (any(agr_feats %in% feat_names)) {
      morpheme_count <- morpheme_count + 1L
    }

    # Reflexive/reciprocal — HITPA'EL already captured in binyan, but
    # if Stanza tags Reflex separately it's worth noting
    if ("Reflex" %in% feat_names && !("HebBinyan" %in% feat_names)) {
      morpheme_count <- morpheme_count + 1L
    }

    return(morpheme_count)
  }

  # ── AUX: same structure as VERB ───────────────────────────────────────────
  # Auxiliaries in Hebrew (היה, יהיה) are fully inflected verbs,
  # so they get the same morpheme decomposition.
  if (upos == "AUX") {
    morpheme_count <- 1L  # root
    if ("HebBinyan" %in% feat_names) morpheme_count <- morpheme_count + 1L
    if (any(c("Tense", "VerbForm", "Aspect", "Mood") %in% feat_names))
      morpheme_count <- morpheme_count + 1L
    if (any(c("Person", "Gender", "Number") %in% feat_names))
      morpheme_count <- morpheme_count + 1L
    return(morpheme_count)
  }

  # ── All other open-class words: lexeme + inflectional features ────────────
  #
  # For nouns, adjectives, pronouns, numerals, adverbs:
  # The lexeme IS the base morpheme (= 1). On top of it sit inflectional
  # affixes that Stanza tags as features.
  #
  # We are explicit that morpheme_count starts at 1 = the lexeme itself.
  # This is what was implicit before ("stem = 1") — we make it a named
  # concept now so it's pedagogically transparent.

  morpheme_count <- 1L  # Lexeme / stem

  # Gender + Number = one agreement/inflection suffix
  # (ילד → ילד-ה, ילד → ילד-ים: each step adds one morpheme)
  has_gender <- "Gender" %in% feat_names
  has_number <- "Number" %in% feat_names
  if (has_gender | has_number) morpheme_count <- morpheme_count + 1L

  # Definiteness = ה- prefix (a genuine separate morpheme in Hebrew,
  # even though it's written attached)
  if ("Definite" %in% feat_names &&
      feat_values["Definite"] == "Def") {
    morpheme_count <- morpheme_count + 1L
  }

  # Possessive suffix (ביתי, ביתך, ביתו...)
  if ("Poss" %in% feat_names) morpheme_count <- morpheme_count + 1L

  # Pronoun-specific: person marking
  if (upos == "PRON" && "Person" %in% feat_names) {
    # Person on pronouns is part of the pronoun itself (not an extra morpheme)
    # UNLESS gender/number are already counted — then skip to avoid double count
    if (!has_gender && !has_number) morpheme_count <- morpheme_count + 1L
  }

  return(morpheme_count)
}

# parse_utterances() — with progress bar and time estimate
#
# progress_bar$new() creates a bar object. The format string controls what
# gets printed. The tokens you can use are:
#   :bar    – the animated bar itself
#   :current / :total – counts
#   :percent – completion percentage
#   :elapsed – time elapsed since start
#   :eta     – estimated time remaining (computed from current rate)
#   :tick_rate – utterances per second
#
# pb$tick() advances the bar by one step. We call it once per utterance,
# so total = length(utterances). The bar updates in-place on the same
# terminal line (no scrolling flood of output).
#
# We also record wall-clock time with proc.time() so we can report the
# true total at the end, since :elapsed inside the bar resets per call.

parse_utterances <- function(utterances, ids) {
  n  <- length(utterances)
  pb <- progress_bar$new(
    format = "  [:bar] :current/:total (:percent) | elapsed: :elapsed | eta: :eta | :tick_rate utt/sec",
    total  = n, clear = FALSE, width = 80
  )
  t_start <- proc.time()[["elapsed"]]
  rows    <- list()

  for (i in seq_along(utterances)) {
    pb$tick()
    doc <- nlp(utterances[[i]])

    for (sent in doc$sentences) {
      for (word in sent$words) {
        feats_val <- if (is.null(word$feats)) NA_character_ else word$feats
        upos_val  <- word$upos

        rows[[length(rows) + 1]] <- tibble(
          utt_id      = ids[[i]],
          text        = word$text,
          lemma       = word$lemma,
          upos        = upos_val,
          xpos        = word$xpos,
          feats       = feats_val,
          # Morpheme estimate for this single token
          n_morphemes = count_morphemes_from_feats(feats_val, upos_val)
        )
      }
    }
  }

  t_total <- round(proc.time()[["elapsed"]] - t_start, 1)
  message("  Done: ", n, " utterances in ", t_total, "s",
          " (~", round(t_total / n, 2), "s/utt)")

  bind_rows(rows)
}

# ── Run parsing speaker by speaker ───────────────────────────────────────────
#
# IMPORTANT NOTE on group_modify():
#   group_modify() automatically re-attaches the grouping columns (speaker,
#   speaker_name) to whatever data frame your function returns. This means
#   you must NOT add those columns yourself inside the function body —
#   doing so creates duplicate columns and throws an error.
#   Rule of thumb: inside group_modify(), only return columns that are NOT
#   the grouping variables.
#
# IMPORTANT NOTE on NA speakers:
#   If speaker_labels[speaker] returns NA for some rows, those utterances
#   will form their own NA group. We filter them out below with a warning
#   so they don't silently disappear into the output.

# Check for unmapped speaker codes before parsing
unmapped <- df |> filter(is.na(speaker_name))
if (nrow(unmapped) > 0) {
  warning(nrow(unmapped), " utterances have unrecognized speaker codes and will be skipped: ",
          paste(unique(unmapped$speaker), collapse = ", "))
}

tokens_all <- df |>
  filter(!is.na(speaker_name)) |>       # drop unmapped speakers
  group_by(speaker, speaker_name) |>
  group_modify(~ {
    message("  Parsing speaker: ", .y$speaker_name,
            " (", nrow(.x), " utterances)")
    parse_utterances(.x$utterance, .x$id)
    # No mutate() here — group_modify adds speaker & speaker_name automatically
  }) |>
  ungroup()

# Join utterance-level info back onto the token table so we have
# age, word count, etc. available per token
tokens_all <- tokens_all |>
  left_join(
    df |> select(id, age, utt_word_count),
    by = c("utt_id" = "id")
  )


message("Parsing complete. Total tokens: ", nrow(tokens_all))


# Remove punctuation tokens from all frequency analyses.
# We do this in one place so every downstream table and plot is clean —
# no need to filter repeatedly inside each summarise() call.
#
# UPOS "PUNCT" covers all punctuation Stanza tokenizes: periods, commas,
# colons, question marks, etc. We drop them here because:
#   (a) they inflate token counts without reflecting lexical content
#   (b) TTR and POS distributions become more meaningful without them
#   (c) we've already captured question marking at the utterance level above
#       so we lose nothing analytically by dropping "?" as a token

tokens_all <- tokens_all |>
  filter(upos != "PUNCT")



message("Tokens after removing PUNCT: ", nrow(tokens_all))

write_csv(tokens_all, "tokens_all.csv")  # export the full token table for inspection)

# Aggregate token-level counts back to utterance level.
# For each utterance we compute:
#   n_stanza_tokens  : number of Stanza tokens (after MWT, minus PUNCT)
#                      This is already a better MLU base than raw word count
#                      because MWT splits agglutinated prefixes (ו, ה, ב, כ, ל, מ)
#   n_morphemes_feats: sum of per-token morpheme estimates from feats analysis
#                      This adds inflectional morphology on top of MWT splitting

# Step 3 — run this first to add the columns to df
utt_morpheme_counts <- tokens_all |>
  group_by(utt_id) |>
  summarise(
    n_stanza_tokens   = n(),
    n_morphemes_feats = sum(n_morphemes),
    .groups = "drop"
  )

df <- df |>
  left_join(utt_morpheme_counts, by = c("id" = "utt_id")) |>
  mutate(
    n_stanza_tokens   = replace_na(n_stanza_tokens,   0L),
    n_morphemes_feats = replace_na(n_morphemes_feats, 0L)
  )

# Now this will work
utt_length_summary <- df |>
  filter(!is.na(speaker_name)) |>
  group_by(speaker, speaker_name) |>
  summarise(
    n_utterances   = n(),
    mlu_words_mean = round(mean(utt_word_count),    2),
    mlu_words_sd   = round(sd(utt_word_count),      2),
    mlu_mwt_mean   = round(mean(n_stanza_tokens),   2),
    mlu_mwt_sd     = round(sd(n_stanza_tokens),     2),
    mlu_morph_mean = round(mean(n_morphemes_feats), 2),
    mlu_morph_sd   = round(sd(n_morphemes_feats),   2),
    .groups = "drop"
  )

print(utt_length_summary)
# ── SECTION 5: Frequency tables ───────────────────────────────────────────────

# ── 5a. Word-level summary per speaker ──────────────────────────────────────
#
# Tokens: total number of word occurrences (running words)
# Types:  number of DISTINCT word forms (vocabulary size)
# TTR:    Type-Token Ratio = types / tokens
#         A higher TTR indicates greater lexical diversity.
#         Note: TTR is sensitive to sample size — larger samples tend to
#         have lower TTR. For proper comparison, consider MATTR or MTLD.
#
# We use tolower() when counting types to avoid treating "מה" and "מה"
# as different types (though in Hebrew, capitalization is not relevant —
# but Stanza sometimes returns inconsistent casing for proper nouns).

word_summary <- tokens_all |>
  group_by(speaker, speaker_name) |>
  summarise(
    tokens = n(),
    types  = n_distinct(tolower(text)),
    ttr    = round(types / tokens, 3),
    .groups = "drop"
  )

message("\nWord summary per speaker:")
print(word_summary)

# ── 5b. Word frequency list per speaker ─────────────────────────────────────
#
# This gives us, for each speaker, how often each word form appeared.
# Useful for: identifying high-frequency function words, tracking vocabulary
# growth, comparing lexical profiles across speakers.

word_freq <- tokens_all |>
  group_by(speaker, speaker_name, text) |>
  summarise(freq = n(), .groups = "drop") |>
  arrange(speaker, desc(freq))

# ── 5c. POS token and type counts per speaker ────────────────────────────────
#
# UPOS tags follow the Universal Dependencies scheme:
#   NOUN, VERB, ADJ, ADV, PRON, DET, ADP (prepositions), CCONJ, SCONJ,
#   AUX, PART, NUM, PUNCT, X (unknown/foreign), INTJ, PROPN
#
# tokens: how many times each POS appeared
# types:  how many distinct word forms carry that POS
# ttr:    lexical diversity within the POS category

pos_type_count <- tokens_all |>
  group_by(speaker, speaker_name, upos) |>
  summarise(
    tokens = n(),
    types  = n_distinct(tolower(text)),
    ttr    = round(n_distinct(tolower(text)) / n(), 3),
    .groups = "drop"
  ) |>
  arrange(speaker, desc(tokens))

message("\nPOS token/type counts per speaker:")
print(pos_type_count)

# ── 5d. POS frequency per speaker (for plotting) ─────────────────────────────
pos_freq <- tokens_all |>
  group_by(speaker, speaker_name, upos) |>
  summarise(freq = n(), .groups = "drop") |>
  arrange(speaker, desc(freq))

# ── 5e. Within-POS word frequency lists per speaker ──────────────────────────
#
# This is the most granular table: for each speaker × POS combination,
# which words appear and how often?
# E.g.: what NOUNs does the child use, and how frequently?

pos_word_freq <- tokens_all |>
  group_by(speaker, speaker_name, upos, text) |>
  summarise(freq = n(), .groups = "drop") |>
  arrange(speaker, upos, desc(freq))

# ── 5f. Utterance word count distribution ────────────────────────────────────
#
# This table keeps the raw per-utterance word counts alongside metadata.
# Useful for: computing MLU, looking at distributions, modeling growth.

utt_wc_detail <- df |>
  select(id, age, speaker, speaker_name, utterance, utt_word_count)


# ── 5g. Question utterance counts and proportions per speaker ────────────────
#
# We work from df (utterance level), not tokens_all, since is_question
# was defined on the raw text before NLP.

question_summary <- df |>
  filter(!is.na(speaker_name)) |>
  group_by(speaker, speaker_name) |>
  summarise(
    n_utterances  = n(),
    n_questions   = sum(is_question),
    pct_questions = round(100 * mean(is_question), 1),
    .groups = "drop"
  )

message("\nQuestion utterances per speaker:")
print(question_summary)

# The actual question utterances, for inspection
question_utterances <- df |>
  filter(is_question, !is.na(speaker_name)) |>
  select(id, age, speaker_name, utterance, utt_word_count)

message("\nTotal question utterances: ", nrow(question_utterances))

# Export
write_csv(question_summary,    "freq_question_summary.csv")
write_csv(question_utterances, "freq_question_utterances.csv")

# ── SECTION 6: Export all tables to CSV ───────────────────────────────────────
#
# write_csv() from readr (part of tidyverse) writes a tidy UTF-8 CSV.
# Always use UTF-8 for Hebrew data. If you open these in Excel on Windows,
# you may need to import as UTF-8 explicitly (Excel sometimes guesses wrong).

# write_csv()       → UTF-8, no BOM  (good for R, bad for Excel)
# write_excel_csv() → UTF-8 with BOM (Excel opens it correctly by double-click)

write_excel_csv(word_summary,       "freq_word_summary.csv")
write_excel_csv(word_freq,          "freq_word_list.csv")
write_excel_csv(pos_freq,           "freq_pos.csv")
write_excel_csv(pos_type_count,     "freq_pos_type_token.csv")
write_excel_csv(pos_word_freq,      "freq_pos_word_list.csv")
write_excel_csv(utt_length_summary, "freq_utt_length_summary.csv")
write_excel_csv(utt_length_by_age,  "freq_utt_length_by_age.csv")
write_excel_csv(utt_wc_detail,      "freq_utt_wc_detail.csv")
write_excel_csv(question_summary,   "freq_question_summary.csv")
write_excel_csv(question_utterances,"freq_question_utterances.csv")

message("\nAll tables exported as CSV.")

# Full token-level POS analysis — one row per word token, all speakers.
# Useful for manual checking of Stanza's tagging decisions, POS corrections,
# and morpheme counts before trusting the summary tables.


tokens_all_readable <- tokens_all |>
  left_join(
    df |> select(id, utterance),
    by = c("utt_id" = "id")
  ) |>
  select(
    utt_id, utterance,           # context: which utterance this token came from
    speaker, speaker_name, age,  # who said it
    text, lemma, upos, xpos,     # Stanza's analysis
    feats,                       # morphological features string
    n_morphemes                  # your morpheme count
  ) |>
  arrange(utt_id)

write_excel_csv(tokens_all_readable, "pos_analysis_full.csv")
message("Full POS analysis exported: ", nrow(tokens_all_readable), " tokens")

# ── SECTION 7: Graphics ───────────────────────────────────────────────────────
#
# All plots use ggplot2, which follows a "grammar of graphics" philosophy:
#   1. Start with ggplot(data, aes(...)) — define the data and aesthetics
#      (x axis, y axis, fill color, group, etc.)
#   2. Add geoms (geometric objects) — geom_col(), geom_point(), etc.
#   3. Add scales, facets, labels, themes to style the plot
#
# We save each plot with ggsave(). dpi=180 gives good quality for screens
# and presentations; use dpi=300 for print.

# ── Plot 1: Token and type counts by speaker ─────────────────────────────────
#
# pivot_longer() reshapes the wide summary table into long format, which is
# what ggplot2 expects when you want to map a column to fill color.
# We turn two columns (tokens, types) into two rows per speaker, with a
# "measure" column indicating which is which.

word_summary_long <- word_summary |>
  pivot_longer(
    cols      = c(tokens, types),
    names_to  = "measure",
    values_to = "count"
  )

p1 <- ggplot(word_summary_long,
             aes(x = speaker_name, y = count, fill = measure)) +
  geom_col(position = "dodge", width = 0.6) +
  # position="dodge" places bars side by side instead of stacked
  scale_fill_manual(
    values = c("tokens" = "#4E79A7", "types" = "#F28E2B"),
    labels = c("Tokens (total words)", "Types (distinct words)")
  ) +
  labs(
    title   = "Word Tokens and Types per Speaker",
    x       = NULL,
    y       = "Count",
    fill    = NULL
  ) +
  theme_minimal(base_size = 13) +
  theme(axis.text = element_text(family = "Arial"))
# On Mac, "Arial" usually supports Hebrew. On Linux try "DejaVu Sans".

# ── Plot 2: Type-Token Ratio per speaker ─────────────────────────────────────
#
# TTR is a simple lexical richness metric. Higher = more diverse vocabulary.
# Caution: it's confounded by sample size, so compare speakers with similar
# token counts.

p2 <- ggplot(word_summary, aes(x = speaker_name, y = ttr, fill = speaker_name)) +
  geom_col(width = 0.5, show.legend = FALSE) +
  # Add the actual value on top of each bar
  geom_text(aes(label = ttr), vjust = -0.4, size = 4) +
  scale_fill_brewer(palette = "Set2") +
  scale_y_continuous(limits = c(0, 1)) +
  labs(
    title = "Type-Token Ratio per Speaker",
    x     = NULL,
    y     = "TTR (0–1)"
  ) +
  theme_minimal(base_size = 13)

# ── Plot 3: POS distribution per speaker (proportional stacked bar) ──────────
#
# We compute proportion within each speaker so the bars are all the same
# height — this makes it easy to compare POS composition regardless of
# differences in total token count.

p3 <- pos_freq |>
  group_by(speaker_name) |>
  mutate(prop = freq / sum(freq)) |>
  ungroup() |>
  ggplot(aes(x = speaker_name, y = prop, fill = upos)) +
  geom_col(width = 0.6) +
  scale_y_continuous(labels = percent_format()) +
  scale_fill_viridis_d(option = "turbo", name = "POS (UPOS)") +
  labs(
    title = "POS Distribution per Speaker (proportional)",
    x     = NULL,
    y     = "Proportion of tokens"
  ) +
  theme_minimal(base_size = 13)

# ── Plot 4: Top-10 words per speaker (faceted bar chart) ─────────────────────
#
# slice_max() keeps only the top N rows by a variable within each group.
# reorder_within() from {tidytext} is needed when you want bars ordered
# within each facet independently — without it, ggplot would use a single
# global ordering, which scrambles within-facet bars.
# scale_x_reordered() goes with reorder_within() to clean up axis labels.

top10_words <- word_freq |>
  group_by(speaker_name) |>
  slice_max(freq, n = 10, with_ties = FALSE) |>
  ungroup()

p4 <- ggplot(top10_words,
             aes(x     = reorder_within(text, freq, speaker_name),
                 y     = freq,
                 fill  = speaker_name)) +
  geom_col(show.legend = FALSE) +
  scale_x_reordered() +
  scale_fill_brewer(palette = "Set2") +
  coord_flip() +  # horizontal bars are easier to read for word labels
  facet_wrap(~speaker_name, scales = "free_y") +
  labs(
    title = "Top 10 Words per Speaker",
    x     = NULL,
    y     = "Frequency"
  ) +
  theme_minimal(base_size = 12)

# ── Plot 5: POS × Speaker heatmap (tokens, types, TTR) ───────────────────────
#
# geom_tile() draws filled rectangles — one per cell in the grid.
# We encode TTR as fill color (blue gradient) and print the raw counts
# as text inside each cell.
# paste0() concatenates strings: paste0("a", "b") → "ab"

p5 <- pos_type_count |>
  ggplot(aes(x = speaker_name, y = upos, fill = ttr)) +
  geom_tile(color = "white", linewidth = 0.5) +
  geom_text(
    aes(label = paste0(tokens, "\n(", types, ")")),
    size  = 2.8,
    color = "black"
  ) +
  scale_fill_gradient(low = "#deebf7", high = "#08519c", name = "TTR") +
  labs(
    title    = "Tokens (n) and Types per POS × Speaker",
    subtitle = "Cell: tokens (types) | Color fill = TTR",
    x        = NULL,
    y        = "Universal POS"
  ) +
  theme_minimal(base_size = 12) +
  theme(panel.grid = element_blank())

# ── Plot 6: Utterance word count — box plot per speaker ──────────────────────
#
# A box plot shows the distribution of utterance lengths per speaker:
#   - the box covers the interquartile range (IQR: 25th–75th percentile)
#   - the line inside is the median
#   - whiskers extend to 1.5 × IQR
#   - dots beyond the whiskers are outliers
#
# geom_jitter() overlays the raw data points with horizontal random noise
# (jitter) to avoid overplotting. alpha controls transparency.

p6 <- ggplot(df, aes(x = speaker_name, y = utt_word_count, fill = speaker_name)) +
  geom_boxplot(outlier.shape = NA, alpha = 0.6, width = 0.5) +
  # outlier.shape = NA hides the outlier dots from the boxplot itself
  # because we'll show all points via geom_jitter below
  geom_jitter(aes(color = speaker_name),
              width = 0.18, alpha = 0.3, size = 1.2) +
  scale_fill_brewer(palette  = "Set2") +
  scale_color_brewer(palette = "Set2") +
  labs(
    title    = "Utterance Length Distribution per Speaker",
    subtitle = "Box = IQR; line = median; dots = individual utterances",
    x        = NULL,
    y        = "Words per utterance"
  ) +
  theme_minimal(base_size = 13) +
  theme(legend.position = "none")

# ── Plot 7: Mean utterance length per recording session (age) ────────────────
#
# This is a longitudinal view — how does mean utterance length (MLU proxy)
# change over the recording sessions for each speaker?
# We use geom_line() + geom_point() to show the trajectory.
# The error bars show ±1 SD around the mean.
#
# geom_errorbar() needs ymin and ymax; we compute these in-line with mutate().
# position_dodge() spreads overlapping speaker lines horizontally so they
# don't obscure each other.

p7 <- utt_length_by_age |>
  mutate(
    ymin = mean_wc - sd_wc,
    ymax = mean_wc + sd_wc
  ) |>
  ggplot(aes(x = age, y = mean_wc,
             group = speaker_name, color = speaker_name)) +
  geom_errorbar(aes(ymin = ymin, ymax = ymax),
                width = 0.2,
                position = position_dodge(0.3),
                alpha = 0.5) +
  geom_line(position = position_dodge(0.3), linewidth = 0.8) +
  geom_point(position = position_dodge(0.3), size = 2.5) +
  scale_color_brewer(palette = "Set2", name = "Speaker") +
  labs(
    title    = "Mean Utterance Length (words) by Recording Session",
    subtitle = "Error bars = ±1 SD",
    x        = "Age at recording (YY;MM;DD)",
    y        = "Mean words per utterance"
  ) +
  theme_minimal(base_size = 12) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
# Rotate x labels 45° so the age strings don't overlap each other


# ── Plot 8: Question utterance proportion per speaker ────────────────────────

p8 <- ggplot(question_summary,
             aes(x = speaker_name, y = pct_questions, fill = speaker_name)) +
  geom_col(width = 0.5, show.legend = FALSE) +
  geom_text(aes(label = paste0(n_questions, "\n(", pct_questions, "%)")),
            vjust = -0.3, size = 3.5) +
  scale_fill_brewer(palette = "Set2") +
  scale_y_continuous(limits = c(0, 100)) +
  labs(
    title    = "Question Utterances per Speaker",
    subtitle = "Bar label: count (% of speaker's utterances)",
    x        = NULL,
    y        = "% question utterances"
  ) +
  theme_minimal(base_size = 13)

print(p8)
ggsave("plot8_questions.png", p8, width = 6, height = 4, dpi = 180)

# ── Plot 9: MLU comparison across three counting units ───────────────────────
#
# pivot_longer() reshapes the three mean columns into one column (mlu_value)
# with a measure label column — the shape ggplot needs for grouped bars.

mlu_long <- utt_length_summary |>
  select(speaker_name,
         `Words\n(raw)`           = mlu_words_mean,
         `Tokens\n(MWT)`          = mlu_mwt_mean,
         `Morphemes\n(feats)`     = mlu_morph_mean) |>
  pivot_longer(-speaker_name, names_to = "measure", values_to = "mlu") |>
  mutate(measure = factor(measure,
                          levels = c("Words\n(raw)",
                                     "Tokens\n(MWT)",
                                     "Morphemes\n(feats)")))

p9 <- ggplot(mlu_long,
             aes(x = speaker_name, y = mlu, fill = measure)) +
  geom_col(position = "dodge", width = 0.65) +
  geom_text(aes(label = round(mlu, 1)),
            position = position_dodge(0.65),
            vjust = -0.4, size = 3.2) +
  scale_fill_manual(
    values = c("Words\n(raw)"       = "#4E79A7",
               "Tokens\n(MWT)"      = "#F28E2B",
               "Morphemes\n(feats)" = "#59A14F"),
    name = "MLU unit"
  ) +
  labs(
    title    = "Mean Length of Utterance: Three Counting Units",
    subtitle = "Words = raw space-split | MWT = Stanza tokens after prefix splitting | Morphemes = MWT + inflectional feats",
    x        = NULL,
    y        = "Mean count per utterance"
  ) +
  theme_minimal(base_size = 13)

print(p9)

# ── Print all plots ───────────────────────────────────────────────────────────
print(p1)
print(p2)
print(p3)
print(p4)
print(p5)
print(p6)
print(p7)

# ── Save all plots ────────────────────────────────────────────────────────────
ggsave("plot1_token_type.png",       p1, width = 7,  height = 4, dpi = 180)
ggsave("plot2_ttr.png",              p2, width = 6,  height = 4, dpi = 180)
ggsave("plot3_pos_dist.png",         p3, width = 8,  height = 5, dpi = 180)
ggsave("plot4_top10_words.png",      p4, width = 10, height = 7, dpi = 180)
ggsave("plot5_pos_heatmap.png",      p5, width = 8,  height = 6, dpi = 180)
ggsave("plot6_utt_length_box.png",   p6, width = 7,  height = 5, dpi = 180)
ggsave("plot7_mlu_by_session.png",   p7, width = 10, height = 5, dpi = 180)

message("\nAll done! Tables (.csv) and plots (.png) saved to working directory.")
message("Working directory: ", getwd())

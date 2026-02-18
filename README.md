# Hebrew Corpus Analysis — R + Stanza

R script for morphosyntactic analysis of Hebrew speech corpora using 
the Stanza NLP library. Developed for teaching computational corpus 
linguistics at Bar-Ilan University / Tel Aviv University.

## What the script does
- Parses Hebrew utterances with Stanza (tokenization, POS, lemma, morphology)
- Computes frequency tables: word tokens/types, POS distributions, within-POS lists
- Calculates MLU across three counting units: words, MWT tokens, morphemes
- Detects question utterances
- Exports all tables as Excel-friendly CSVs
- Produces 9 diagnostic plots

## Requirements
- R 4.x + RStudio
- Python 3.10 via conda (environment: `heb_nlp`)
- See `workshop_windows_setup.md` for Windows-specific instructions

## Data
Data files are not included in this repository.
Contact [elitzurd@tauex.tau.ac.il] for access.

## Author
Elitzur Dattner  
Bar-Ilan University / Tel Aviv University

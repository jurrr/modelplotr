cran-comments
================
Jurriaan Nagelkerke
2019-04-16

## Adjustments after first submission:

  - removed authors from DESCRIPTION (<Authors@R> remains)
  - removed Tensorflow dependency in Vignette example
  - downsized examples / dontrun examples for examples that need
    tensorflow installation/long runtime
  - shortened description in DESCRIPTION
  - changed print/cat lines into message/warning lines
  - removed writing to user filespace in writing to tempdir()

## R CMD check results

There were no ERRORs or WARNINGs or NOTES.

## Test environments
* local OS X install, R 3.6.0
* ubuntu 16.04 (gihub actions) R 3.6.0 and R 4.0.0
* windows-latest (github actions) R release
# macOS-latest, (github actions) R release

## R CMD check results

There were no ERRORs or WARNINGs.

There was 3 NOTES:

checking installed package size ... NOTE
    installed size is 13.9Mb
    sub-directories of 1Mb or more:
      libs  13.2Mb

checking for future file timestamps ... NOTE
  unable to verify current time

checking examples ... NOTE
  Examples with CPU (user + system) or elapsed time > 5s
                       user system elapsed
  estimate_parameters 6.642  0.218   6.734

## Downstream dependencies
There are currently no downstream dependencies for this package.

## Submitter's comment: The feedback from last submission has been adressed:

Fix runtime if vignette

Put examples in donttest since it takes more than 5 seconds


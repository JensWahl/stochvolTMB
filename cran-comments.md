## Test environments
* local OS X install, R 3.6.0
* ubuntu 16.04 (gihub actions) R 3.6.0 and R 4.0.0
* windows-latest (github actions) R release
# macOS-latest, (github actions) R release
# solaris-x86 through rhub

## R CMD check results

There were no ERRORs or WARNINGs.

There were 2 NOTES:

checking installed package size ... NOTE
    installed size is 20.1Mb
    sub-directories of 1Mb or more:
      libs  19.5Mb

checking for future file timestamps ... NOTE
  unable to verify current time


## Downstream dependencies
There are currently no downstream dependencies for this package.

## Submitter's comment: The feedback from last submission has been adressed:

Error on solaris-x86 regarding log and exp of int is fixed by casting to Type


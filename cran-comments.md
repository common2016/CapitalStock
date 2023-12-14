## Test environments
 * local macOS 14.1, R 4.3.1
 * Debian Linux  (on rhub),  devel
 * Windows Server 2022 x64(on rhub), release
    There are 2 NOTES.
    
  > N  checking for non-standard things in the check directory
  > Found the following files/directories:
  >   ''NULL''
  
  As noted in [R-hub issue #560](https://github.com/r-hub/rhub/issues/560), this seems to be an Rhub issue and so can likely be ignored.
  
  > N  checking for detritus in the temp directory
  > Found the following files/directories:
  >   'lastMiKTeXException'
  
  As noted in [R-hub issue #503](https://github.com/r-hub/rhub/issues/503), this could be due to a bug/crash in MiKTeX and can likely be ignored.   

## R CMD check results
There were no ERRORs and WARNINGs.

## Downstream dependencies

There are currently no downstream dependencies.



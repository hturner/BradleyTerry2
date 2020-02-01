## General comments

This is a minor update: 
 * updating tests to avoid failures due to minor discrepancies
 * fixing a minor bug

## Test environments

* Local
    * Ubuntu 18.04.2, R 3.6.2
    
* R-hub
    * Debian Linux, R-devel, GCC [x]
    * Windows Server 2008 R2 SP1, R-release, 32/64 bit
    * macOS 10.11 El Capitan, R-release (experimental)
    
### Check results

I get a warning on R-hub macOS, 

* checking top-level files ... WARNING
Conversion of ‘README.md’ failed:
pandoc: Could not fetch https://www.r-pkg.org/badges/version/BradleyTerry2
TlsExceptionHostPort (HandshakeFailed (Error_Misc "user error (unexpected type received. expecting handshake and got: Alert [(AlertLevel_Fatal,HandshakeFailure)])")) "www.r-pkg.org" 443

This seems to due to missing https support in an older version of pandoc.
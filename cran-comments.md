## General comments

This is a minor update: 
 * standardizing tests to use RNGversion("2.10") - package already depended on this R version
 * fixing some minor bugs

## Test environments

* Local
    * Ubuntu 18.04.2, R 3.6.0
    * Windows 8, R 3.6.0
    
* R-hub
    * Debian Linux, R-devel, GCC
    * macOS 10.11 El Capitan, R-release (experimental)
    
### Check results

I get a warning on R-hub macOS, 

* checking top-level files ... WARNING
Conversion of ‘README.md’ failed:
pandoc: Could not fetch https://www.r-pkg.org/badges/version/BradleyTerry2
TlsExceptionHostPort (HandshakeFailed (Error_Misc "user error (unexpected type received. expecting handshake and got: Alert [(AlertLevel_Fatal,HandshakeFailure)])")) "www.r-pkg.org" 443

This seems to due to missing https support in an older version of pandoc.
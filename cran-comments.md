## General comments

This is a minor update to address a bug revealed by R-devel NEWS item

 * model.matrix(*, contrasts.arg = CC) now warns about invalid contrasts.args.
 
In addition, urls have been updated and this version fixes a couple of instances where `if` statements could have arguments of length > 1.

## Test environments

* Local
    * Ubuntu 18.04.2, R 3.5.2
    * Ubuntu 18.04.2, R-devel (2019-02-24 r76155)
    * Windows 8, R 3.5.2
    
* R-hub
    * macOS 10.11 El Capitan, R-release (experimental)
    
### Check results

I get a warning on R-hub macOS, 

* checking top-level files ... WARNING
Conversion of ‘README.md’ failed:
pandoc: Could not fetch https://www.r-pkg.org/badges/version/BradleyTerry2
TlsExceptionHostPort (HandshakeFailed (Error_Misc "user error (unexpected type received. expecting handshake and got: Alert [(AlertLevel_Fatal,HandshakeFailure)])")) "www.r-pkg.org" 443

This seems to due to missing https support in an older version of pandoc.
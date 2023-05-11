# `kda-tool` Changelog

## 1.1

### Improvements

*   Update to new signing API
*   Allow user to specify the node scheme (i.e. http/https)
*   Add logging verbosity controls, increased logging
*   Add ability to sign and verify arbitrary signatures
*   Allow transactions to be tested before signing with --no-verify-sigs
*   Add -s option to local that gives shortened status output
*   Return an error code if any local transaction doesn't have status "success".

### Bug Fixes

*   Fix handling of holes with a single element array
*   Fix bug in template array handling

## 1.0 (2022-11-09)

Initial release


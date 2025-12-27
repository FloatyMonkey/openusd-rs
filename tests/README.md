# Compliance tests

To run the tests, download the *sample implementations, and compliance tools* from [AOUSD](https://aousd.org/news/core-spec-announcement). This should provide the zip file *core-spec-supplemental-release_dec2025.zip*. Extract it and copy all folders under `/composition/tests/assets` to this repository's `/tests/data` folder. Finally uncomment the code in `/tests/pcp_*.rs` files to enable the compliance tests. Some of these are expected to fail as the implementation is still work in progress.

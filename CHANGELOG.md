# Change Log

All notable changes to this project will be documented in this file.

## [Unreleased][unreleased]
### Added

- Add new `aws_metadata:make_client` and `aws_metadata:get_client` functions
  that fetch credentials from the AWS metadata service.  Credentials are
  automatically updated in the background before they expire.  Application
  code should always call `aws_metadata:get_client` before using a client with
  the `aws-erlang` library.

[unreleased]: https://github.com/jkakar/aws-erlang-metadata/compare/v0.0.1...HEAD

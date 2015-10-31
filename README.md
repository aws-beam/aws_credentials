# AWS instance metadata support for aws-erlang

[![Build Status](https://travis-ci.org/jkakar/aws-erlang-metadata.svg)](https://travis-ci.org/jkakar/aws-erlang-metadata)

A thin wrapper around [aws-erlang](https://github.com/jkakar/aws-erlang) that
adds support for managing credentials retrieved from instance metadata.
Credentials are automatically refreshed in the background and up-to-date each
time they're retrieved.

Here is an example:

```erlang
Client = aws_metadata:get_client(),
{ok, Output, _Response} = aws_kinesis:list_streams(Client, #{}, []),
io:format("~p~n", [Output]).
```

or shortened with a macro:

```erlang
-define(AWS_CLIENT, aws_metadata:get_client()).

{ok, Output, _Response} = aws_kinesis:list_streams(?AWS_CLIENT, #{}, []),
io:format("~p~n", [Output]).
```

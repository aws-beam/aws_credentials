# AWS instance metadata support for aws-erlang

[![Build Status](https://travis-ci.org/jkakar/aws-erlang-metadata.svg)](https://travis-ci.org/jkakar/aws-erlang-metadata)

Fetch up-to-date credentials from the EC2 instance metadata service to use
with [aws-erlang](https://github.com/jkakar/aws-erlang).  Credentials are
automatically refreshed in the background.

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

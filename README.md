aws_credentials
===============
This is a library to retrieve AWS credentials from a variety of possible
sources in the following default order:

1. Erlang environment variables
2. OS environment variables
3. An AWS [credentials file][1]
4. ECS task credentials
5. EC2 metadata

Dependencies
------------
This library depends on the following libraries:

* [iso8601][2]
* [jsone][6]
* [eini][3]

Implementation
--------------
Each possible location for credentials is an Erlang module which implements the
[aws_credentials_provider][4] behaviour.  The first provider to respond with
(apparently) valid credentials stops evaluation of later providers.

### Adding a new provider ###
If you want to add a new provider, you simply implement the behaviour and add
the module name to the default list of modules to attempt.

### Provider return values ###
Providers are expected to return either `{error, Reason :: term()}` or
`{ok, Credentials :: map(), Expiration :: infinity | binary() | pos_integer()}`. 
The Credentials map typically looks like the following:

```erlang
#{
  provider_source => Provider :: atom(),
  access_key_id => AccessKey :: binary(),
  secret_access_key => SecretKey :: binary(),
  token => Token :: binary(),
  region => Region :: binary()
}
```
Not all providers will populate all map keys.  

The expiration time from a provider can either be expressed as the atom
`infinity`, epoch seconds as an integer or as an ISO8601 time/date string
(e.g., "2019-12T23:23:45Z"). If you specify the expiration as `infinity`
the library will *never* attempt to refresh credentials.

When credentials have been found, they will be cached in a gen_server
until the credential's expiration time. 5 minutes before expiration time
the gen_server will attempt to acquire new credentials, so credentials 
will automatically be refreshed in the background.

Usage
-----
Include the library in your rebar.config file, and then...

```erlang
{ok, _} = application:ensure_all_started(aws_credentials),
{ok, Credentials} = aws_credentials:get_credentials(),
```

By default if the library is unable to obtain credentials, it will fail to
start with a `bad_match` exception, however if you set `fail_if_unavailable`
to `false` then the library will ignore the exception and attempt to
fetch credentials again after 5 seconds.

It is best practice to **not** cache these credentials inside of your own
application. Always use the aws_credentials library to retrieve them - this way
you will always get "fresh" credentials from the internal state of the
application.

### Choosing certain credentials providers ###

If you want to change the order of providers used to retrieve credentials, you
can change the list of modules in your erlang environment variables as in
this example:

```erlang
  {aws_credentials, [{provider_list, [aws_credentials_ecs]}]
  },
```

Different credential providers may have other settings which you can use to
change their behaviors.  See the documentation for each provider for more
details.

License and copyright
---------------------
This project is licensed under the terms of the Apache 2 license. It is a
heavily modified fork of the original [aws-erlang-metadata][5] project.

Copyright (C) 2018 by Mark R. Allen.

[1]: https://docs.aws.amazon.com/cli/latest/userguide/cli-config-files.html
[2]: https://github.com/erlsci/iso8601
[3]: https://github.com/aws-beam/eini
[4]: src/aws_credentials_provider.erl
[5]: https://github.com/aws-beam/aws-erlang-metadata
[6]: https://github.com/sile/jsone

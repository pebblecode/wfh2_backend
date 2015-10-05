# wfh2

An OTP application

## Build

    $ rebar3 compile

## Start shell

	$ rebar3 shell --config test.config (see below)

## Configuration

In order to avoid checking in api keys, the app will need to be configured
before startup. I found it useful to copy the file config/sys.config to the root
folder.  You can then fill in the api keys. Note that due to slacks slash
command system, an api key per slash comamnd is necessary, hence the value for
that key in the config file is an array that takes all the keys for the
individual actions.


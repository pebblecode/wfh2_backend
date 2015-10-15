[![Stories in Ready](https://badge.waffle.io/pebblecode/wfh2_backend.png?label=ready&title=Ready)](https://waffle.io/pebblecode/wfh2_backend)
[![Build Status](https://travis-ci.org/pebblecode/wfh2_backend.svg?branch=master)](https://travis-ci.org/pebblecode/wfh2_backend)

# wfh2

An OTP application

## Build

    $ ./rebar3 compile

## Start shell

	$ ./rebar3 shell --config test.config (see below)

## Configuration

In order to avoid checking in api keys, the app will need to be configured
before startup. I found it useful to copy the file config/sys.config to the root
folder.  You can then fill in the api keys. Note that due to slacks slash
command system, an api key per slash comamnd is necessary, hence the value for
that key in the config file is an array that takes all the keys for the
individual actions.

## Release / Deployment

_Oh the shame_

Currently this app is deployed to spiderpig. This is a machine sitting inside
pebble's network. The process is as follows:

- Increment the version in `rebar.config` to the desired version number.
- Commit this to master
- Tag that commit with a version number matching the one in `rebar.config`
- Build a tar of the solution: `rebar3 as prod tar`
- Copy the created archive to the `wfhservice` user home folder on spiderpig,
  ideally using that user's permissions.
- Extract that tar archive to a new folder in the user's home folder (need to
  create directory first, as the archive assumes deploy folder as root).
- Stop the running service: attach to the running screen session:

		$ screen -r -d
		$ q().

- Remove the symbolic link target wfh2
- Create symbolic link from new release to wfh2
- Copy ~wfhservice/prod.config to wfh2/releases/`{release_version}`/sys.config
- Start the service with `~wfhservice/wfh2/bin/wfh2 console`
- Disconnect the screen session: `<CTRL>a <CTRL>d`


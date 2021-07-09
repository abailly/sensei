# What is sensei?

_sensei_ is a tool for monitoring and, hopefully, improving one's workflow and work process. It allows one to record various _events_ and take notes while working in an as smooth and seamless as possible way, and then provides an API and an interface to explore those events.

# Install

Currently, sensei must be installed from sources. Assuming you have installed [stack](http://haskellstack.org) then you can clone the [GitHub](https://github.com/abailly/sensei) repository and follow the instructions below for local, ie. on a single machine, use.

1. `cd ui && npm install && npm run build` builds the frontend UI
2. `stack install` install the main program into the path
3. `ln -s ~/.local/bin/sensei-exe ~/.local/bin/ep` creates the command-line interface

You will need to [configure](#configuration) authentification and your user profile.

# Configuration

At startup, sensei is configured without authentication and with a default user profile. To write and read data, one must be authenticated which is handled using [JSON Web Tokens](https://jwt.io/).

First generates a new [JSON Web Key](https://datatracker.ietf.org/doc/html/rfc7517) that will be used to sign tokens:

```
$ ep user --create-key
```

Then create a new token:

```
$ ep user --create-token
```

These commands should have create two files in the current user [XDG](https://specifications.freedesktop.org/basedir-spec/basedir-spec-latest.html) configuration directory (usually `$HOME/.config/sensei`):

* a `sensei.jwk` file containing the JWK,
* a `client.json` file containing the default client configuration file which holds the generated `authToken`.

# Usage

When installed locally and assuming you have followed the [install](#install) procedure, sensei works by launching a _daemon_ if it's not already started, and then communicating with it.

To start the daemon, just run

```
$ ep query
```

The daemon will be launched on port 23456 and available at [localhost:23456](http://localhost:23456).

See the [API Documentation](swagger.md) for more details.

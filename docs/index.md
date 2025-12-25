# What is sensei?

_sensei_ is a personal assistant for taking notes, writing blog posts, monitoring and, hopefully, improving one's workflow and work process. It allows one to record various _events_ and take notes while working in an as smooth and seamless as possible way, and then provides an API and an interface to explore those events.

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

# API

_sensei_ exposes a REST-like API that is used by the `ep` command-line client but can be accessed by any other HTTP client. See the [API Documentation](swagger.md) for more details.

## Authentication

All calls to the API (except for the `/login` endpoint, obviously) must be authenticated. Authentication can be provided by two different means:

* Using a [Bearer token](https://datatracker.ietf.org/doc/html/rfc6750): The user must provide an `Authorization` header with a `Bearer XXX` content, where `XXX` is a valid JSON Web Token. See [Configuraiton](#configuration) section for a simple way to generate such a token,
* Using a [Cookie](https://developer.mozilla.org/en-US/docs/Web/HTTP/Cookies): The cookie is named `JWT-Cookie` and its content is simply a JWT.

The latter is used with login-based authentication: When the user logs in through the [Web page](#web-application), the cookie is set by the server.

### Retrieving a token

A token can be created either locally, using the previously mentioned `--create-token` option, but it can also be fetched from the server:

The command

```
$ ep auth --get-token
Enter password:
```

will update the client's configuration with a freshly generated token signed by ther server. The user is asked to enter her password because the client will obviosly *not use* an existing token to authenticate with the server.

### Setting or changing password

To set or change one's password, run the following command:

```
$ ep auth --set-password
Enter password:
```

This will update the user's profile with the given password, using the client's configured token for authenticating with the server.

# Web application

TBD

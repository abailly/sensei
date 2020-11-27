# sensei

# Configuration

## Project directory
Change the value of the current directory in `App.hs`
i.e:
```haskell
daemonizeServer =
  daemonize $
    -- TODO fix this silly hardcoded path
    -- this is so because I want to ensure the server is started in a location
    -- where it can find the FE resources...
    withCurrentDirectory "PATH_TO_PROJECT_DIR" $ startServer
```

# User Profile

User profile can be set using the command-line tool `ep`.

First create a JSON file containing the user's profile:

```
$ cat > profile.json
{
  "userStartOfDay": "08:00:00",
  "userProfileVersion": 2,
  "userEndOfDay": "18:30:00",
  "userName": "arnaud",
  "userTimezone": "+01:00",
  "userCommands": {
    "docker": "/usr/local/bin/docker",
    "dotnet": "/usr/local/share/dotnet/dotnet",
    "npm": "/usr/local/bin/npm",
    "az": "/usr/local/bin/az",
    "g": "/usr/bin/git",
    "stak": "/Users/arnaud/.local/bin/stack"
  },
  "userFlowTypes": {
    "Learning": "#ff8822",
    "Flowing": "#00dd22",
    "Troubleshooting": "#ee1111",
    "Rework": "#4500dd",
    "Meeting": "#fff203",
    "Experimenting": "#0022dd"
  }
}
^D
```

Then feed that data to the local server:

```
$  ep -U -c profile.json
```

The configuration is currently stored in a JSON file inside XDG configuration directory for `sensei` application.

```
$ cat ~/.config/sensei/config.json | jq .
{
  "userStartOfDay": "08:00:00",
  "userProfileVersion": 2,
  ...
}
```

This can also be retrieved from the command-line:

```
$  ep -U
{
  "userStartOfDay": "08:00:00",
  "userProfileVersion": 2,
  "userEndOfDay": "18:30:00",
  ...
}
```

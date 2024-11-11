# sensei

A personal information management tool for developers

# User Profile

User profile can be set using the command-line tool `ep`.

First create a JSON file containing the user's profile:

```
$ cat > profile.json
{
  "userStartOfDay": "08:00:00",
  "userProfileVersion": 4,
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
  "userProfileVersion": 4,
  ...
}
```

This can also be retrieved from the command-line:

```
$  ep -U
{
  "userStartOfDay": "08:00:00",
  "userProfileVersion": 4,
  "userEndOfDay": "18:30:00",
  ...
}
```

# Migration (prior to v 0.13.0)

Before v 0.13.0, the data were handle into a plain json formated text file.
In order to retrieve your flows and notes, you have to put your `.sensei.log` into the shared directory:
```bash
cp ~/.sensei.log ~/.local/share/sensei/sensei.log
```

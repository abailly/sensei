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

## Sensei logs
Add the file `sensei.log` in your home directory:
```bash
touch ~/.sensei.log
```


# User Profile

User profile can be set using the (local) server's API.

First create a JSON file containing the user's profile:

```
$ cat > profile.json
{
  "userStartOfDay": "08:00:00",
  "userEndOfDay": "18:30:00",
  "userName": "alice",
  "userTimezone": "+01:00",
  "userFlowTypes": [
    "Experimenting",
    "Troubleshooting",
    "Flowing",
    "Rework",
    "Meeting",
    "Learning"
  ]
}
^D
```

Then feed that data to the local server:

```
$  curl -v -X PUT -d @profile.json -H 'Content-type: application/json' -H 'X-API-Version: 0.3.3' http://localhost:23456/users/alice
...
< HTTP/1.1 200 OK
```

The configuration is currently stored in a JSON file inside XDG configuration directory for `sensei` application.

```
$ cat ~/.config/sensei/config.json | jq .
{
  "userStartOfDay": "08:00:00",
  "userEndOfDay": "18:30:00",
  "userName": "arnaud",
  "userTimezone": "+01:00",
  "userFlowTypes": [
    "Experimenting",
    "Troubleshooting",
    "Flowing",
    "Rework",
    "Meeting",
    "Learning"
  ]
}
```

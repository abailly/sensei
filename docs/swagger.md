# Sensei API
An API for storing and querying data about one's coding habits and patterns

## Version: 0.33.0

**License:** All Rights Reserved

### /api/flows/{user}/{ref}

#### GET
##### Summary

Query flows

##### Parameters

| Name | Located in | Description | Required | Schema |
| ---- | ---------- | ----------- | -------- | ---- |
| user | path |  | Yes | string |
| ref | path |  | Yes | string |

##### Responses

| Code | Description | Schema |
| ---- | ----------- | ------ |
| 200 |  | [Event](#event) |
| 404 | `user` or `ref` not found |  |

### /api/flows/{user}/latest/timestamp

#### PATCH
##### Summary

Changes the latest recorded flow's start time by some amount of time. If the resulting timestamp happens before the previous flow's start time, it raises an error. Returns the updated Flow.

##### Parameters

| Name | Located in | Description | Required | Schema |
| ---- | ---------- | ----------- | -------- | ---- |
| user | path |  | Yes | string |
| body | body |  | Yes | [TimeDifference](#timedifference) |

##### Responses

| Code | Description | Schema |
| ---- | ----------- | ------ |
| 200 |  | [Event](#event) |
| 400 | Invalid `body` |  |
| 404 | `user` not found |  |

### /api/flows/{user}/summary

#### GET
##### Summary

Retrieve flows summary, eg. time spend in flows by type, for a given time period.  The time period is given by query arguments `from` and `to`, with `from` being  inclusive lower bound and `to` being exclusive upper bound.  `from` must be a valid ISO8601 date _before_ `to`.

##### Parameters

| Name | Located in | Description | Required | Schema |
| ---- | ---------- | ----------- | -------- | ---- |
| user | path |  | Yes | string |
| from | query |  | No | date |
| to | query |  | No | date |
| period | query |  | No | string |

##### Responses

| Code | Description | Schema |
| ---- | ----------- | ------ |
| 200 |  | [FlowSummary](#flowsummary) |
| 400 | Invalid `period` or `to` or `from` |  |
| 404 | `user` not found |  |

### /api/flows/{user}/{day}/notes

#### GET
##### Summary

Retrieve timestamped notes for some day, or all notes if no day is given.

##### Parameters

| Name | Located in | Description | Required | Schema |
| ---- | ---------- | ----------- | -------- | ---- |
| user | path |  | Yes | string |
| day | path |  | Yes | date |

##### Responses

| Code | Description | Schema |
| ---- | ----------- | ------ |
| 200 |  | [ [NoteView](#noteview) ] |
| 404 | `user` or `day` not found |  |

### /api/flows/{user}/{day}/commands

#### GET
##### Summary

Retrieve sequence of executed commands for some day.

##### Parameters

| Name | Located in | Description | Required | Schema |
| ---- | ---------- | ----------- | -------- | ---- |
| user | path |  | Yes | string |
| day | path |  | Yes | date |

##### Responses

| Code | Description | Schema |
| ---- | ----------- | ------ |
| 200 |  | [ [CommandView](#commandview) ] |
| 404 | `user` or `day` not found |  |

### /api/flows/{user}/{day}

#### GET
##### Summary

Retrieve timeline of flows for a given day.

##### Parameters

| Name | Located in | Description | Required | Schema |
| ---- | ---------- | ----------- | -------- | ---- |
| user | path |  | Yes | string |
| day | path |  | Yes | date |

##### Responses

| Code | Description | Schema |
| ---- | ----------- | ------ |
| 200 |  | [ [FlowView](#flowview) ] |
| 404 | `user` or `day` not found |  |

### /api/flows/{user}

#### GET
##### Summary

Retrieve timeline of flows, grouped by some time slice (Day, Week, Month...).  If no 'group' param is given, returns _all_ flows.

##### Parameters

| Name | Located in | Description | Required | Schema |
| ---- | ---------- | ----------- | -------- | ---- |
| user | path |  | Yes | string |
| group | query |  | No | [ string ] |

##### Responses

| Code | Description | Schema |
| ---- | ----------- | ------ |
| 200 |  | [ [GroupViews](#groupviews) ] |
| 400 | Invalid `group` |  |
| 404 | `user` not found |  |

### /api/notes/{user}

#### GET
##### Summary

Run a full-text search on all notes, retrieving matching notes.

##### Parameters

| Name | Located in | Description | Required | Schema |
| ---- | ---------- | ----------- | -------- | ---- |
| user | path |  | Yes | string |
| search | query |  | No | string |

##### Responses

| Code | Description | Schema |
| ---- | ----------- | ------ |
| 200 |  | [ [NoteView](#noteview) ] |
| 400 | Invalid `search` |  |
| 404 | `user` not found |  |

### /api/log

#### POST
##### Summary

Record a new `Event` in the log.

##### Parameters

| Name | Located in | Description | Required | Schema |
| ---- | ---------- | ----------- | -------- | ---- |
| body | body |  | Yes | [ [Event](#event) ] |

##### Responses

| Code | Description | Schema |
| ---- | ----------- | ------ |
| 200 |  | [  ] |
| 400 | Invalid `body` |  |

### /api/log/{user}

#### GET
##### Summary

Retrieve the complete log of all events pertaining to a given user, most recent first

##### Parameters

| Name | Located in | Description | Required | Schema |
| ---- | ---------- | ----------- | -------- | ---- |
| user | path |  | Yes | string |
| page | query |  | No | integer |

##### Responses

| Code | Description | Schema |
| ---- | ----------- | ------ |
| 200 |  | [ [Event](#event) ] |
| 400 | Invalid `page` |  |
| 404 | `user` not found |  |

### /api/users/{user}/token

#### GET
##### Summary

Retrieve a fresh signed JWT token for given user.

##### Parameters

| Name | Located in | Description | Required | Schema |
| ---- | ---------- | ----------- | -------- | ---- |
| user | path |  | Yes | string |

##### Responses

| Code | Description | Schema |
| ---- | ----------- | ------ |
| 200 |  | [SerializedToken](#serializedtoken) |
| 404 | `user` not found |  |

### /api/users/{user}

#### GET
##### Summary

Retrieve a user's profile.

##### Parameters

| Name | Located in | Description | Required | Schema |
| ---- | ---------- | ----------- | -------- | ---- |
| user | path |  | Yes | string |

##### Responses

| Code | Description | Schema |
| ---- | ----------- | ------ |
| 200 |  | [UserProfile](#userprofile) |
| 404 | `user` not found |  |

#### PUT
##### Summary

Define current user's profile.

##### Parameters

| Name | Located in | Description | Required | Schema |
| ---- | ---------- | ----------- | -------- | ---- |
| user | path |  | Yes | string |
| body | body |  | Yes | [UserProfile](#userprofile) |

##### Responses

| Code | Description | Schema |
| ---- | ----------- | ------ |
| 200 |  | [Hex](#hex) |
| 400 | Invalid `body` |  |
| 404 | `user` not found |  |

### /api/versions

#### GET
##### Summary

Get the server executable and storage versions

##### Responses

| Code | Description | Schema |
| ---- | ----------- | ------ |
| 200 |  | [Versions](#versions) |

### Models

#### Event

| Name | Type | Description | Required |
| ---- | ---- | ----------- | -------- |
| EventFlow | [Flow](#flow) |  | No |
| EventTrace | [Trace](#trace) |  | No |
| EventNote | [NoteFlow](#noteflow) |  | No |

#### Flow

| Name | Type | Description | Required |
| ---- | ---- | ----------- | -------- |
| _flowType | [FlowType](#flowtype) |  | Yes |
| _flowUser | string |  | Yes |
| _flowTimestamp | [UTCTime](#utctime) |  | Yes |
| _flowDir | string |  | Yes |

#### FlowType

| Name | Type | Description | Required |
| ---- | ---- | ----------- | -------- |
| FlowType | string |  |  |

#### UTCTime

| Name | Type | Description | Required |
| ---- | ---- | ----------- | -------- |
| UTCTime | string |  |  |

**Example**
<pre>2016-07-22T00:00:00Z</pre>

#### Trace

| Name | Type | Description | Required |
| ---- | ---- | ----------- | -------- |
| _traceUser | string |  | Yes |
| _traceTimestamp | [UTCTime](#utctime) |  | Yes |
| _traceDirectory | string |  | Yes |
| _traceProcess | string |  | Yes |
| _traceArgs | [ string ] |  | Yes |
| _traceExitCode | integer |  | Yes |
| _traceElapsed | number |  | Yes |

#### NoteFlow

| Name | Type | Description | Required |
| ---- | ---- | ----------- | -------- |
| _noteUser | string |  | Yes |
| _noteTimestamp | [UTCTime](#utctime) |  | Yes |
| _noteDir | string |  | Yes |
| _noteContent | string |  | Yes |

#### TimeDifference

A time difference, positive or negative, expressed as a number of seconds

| Name | Type | Description | Required |
| ---- | ---- | ----------- | -------- |
| TimeDifference | number | A time difference, positive or negative, expressed as a number of seconds |  |

#### FlowSummary

| Name | Type | Description | Required |
| ---- | ---- | ----------- | -------- |
| summaryPeriod | [  ] |  | Yes |
| summaryFlows | [ [  ] ] |  | Yes |
| summaryCommands | [ [  ] ] |  | Yes |

#### LocalTime

| Name | Type | Description | Required |
| ---- | ---- | ----------- | -------- |
| LocalTime | string |  |  |

**Example**
<pre>2016-07-22T07:40:00</pre>

#### NoteView

| Name | Type | Description | Required |
| ---- | ---- | ----------- | -------- |
| noteStart | [LocalTime](#localtime) |  | Yes |
| noteView | string |  | Yes |

#### CommandView

| Name | Type | Description | Required |
| ---- | ---- | ----------- | -------- |
| commandStart | [LocalTime](#localtime) |  | Yes |
| commandProcess | string |  | Yes |
| commandElapsed | number |  | Yes |

#### FlowView

| Name | Type | Description | Required |
| ---- | ---- | ----------- | -------- |
| flowStart | [LocalTime](#localtime) |  | Yes |
| flowEnd | [LocalTime](#localtime) |  | Yes |
| viewType | [FlowType](#flowtype) |  | Yes |

#### GroupViews

| Name | Type | Description | Required |
| ---- | ---- | ----------- | -------- |
| NoViews | [  ] | _Example:_ `[]` | No |
| Leaf | [FlowView](#flowview) |  | No |
| GroupLevel | object |  | No |

#### Group

| Name | Type | Description | Required |
| ---- | ---- | ----------- | -------- |
| Group | string |  |  |

#### SerializedToken

A JWT Token in its serialized form, eg. 3 sequneces of base64-encoded strings separated by dots  which contain JSON objects. See <https://jwt.io/introduction> for more details.

| Name | Type | Description | Required |
| ---- | ---- | ----------- | -------- |
| SerializedToken | string | A JWT Token in its serialized form, eg. 3 sequneces of base64-encoded strings separated by dots  which contain JSON objects. See <https://jwt.io/introduction> for more details. |  |

#### UserProfile

| Name | Type | Description | Required |
| ---- | ---- | ----------- | -------- |
| userName | string |  | Yes |
| userTimezone | [TimeZone](#timezone) |  | Yes |
| userStartOfDay | [TimeOfDay](#timeofday) |  | Yes |
| userEndOfDay | [TimeOfDay](#timeofday) |  | Yes |
| userFlowTypes | object |  | No |
| userCommands | object |  | No |
| userPassword | [  ] |  | Yes |

#### TimeZone

| Name | Type | Description | Required |
| ---- | ---- | ----------- | -------- |
| TimeZone | string |  |  |

#### TimeOfDay

| Name | Type | Description | Required |
| ---- | ---- | ----------- | -------- |
| TimeOfDay | string |  |  |

**Example**
<pre>12:33:15</pre>

#### Color

An RGB color represented as an hexadecimal string

| Name | Type | Description | Required |
| ---- | ---- | ----------- | -------- |
| Color | string | An RGB color represented as an hexadecimal string |  |

#### Base64

A base64-encoded bytestring.

| Name | Type | Description | Required |
| ---- | ---- | ----------- | -------- |
| Base64 | string | A base64-encoded bytestring. |  |

#### Hex

A hex-encoded bytestring.

| Name | Type | Description | Required |
| ---- | ---- | ----------- | -------- |
| Hex | string | A hex-encoded bytestring. |  |

#### Versions

| Name | Type | Description | Required |
| ---- | ---- | ----------- | -------- |
| serverVersion | string |  | Yes |
| clientVersion | string |  | Yes |
| serverStorageVersion | integer |  | Yes |
| clientStorageVersion | integer |  | Yes |

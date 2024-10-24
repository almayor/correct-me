## correct-me

Submitting, reviewing, and improving message phrasing with built-in spellcheck.

## GET /api/alternatives

### Get list of alternatives by author


### Returns a list of URLs to alternatives filtered by author ID


### Authentication

This part of the API is protected by the following authentication mechanisms:

 * [Basic Authentication](https://en.wikipedia.org/wiki/Basic_access_authentication)
 * JSON Web Tokens ([JWTs](https://en.wikipedia.org/wiki/JSON_Web_Token))


Clients must supply the following data

One of the following:

 * Cookies automatically set by browsers, plus a header
 * A JWT Token signed with this server's key


### GET Parameters:

- author_id
     - **Values**: *1, 2, 3, ...*
     - **Description**: Filter alternatives by ID of their author


### Response:

- Status code 200
- Headers: []

- Supported content types are:

    - `application/json;charset=utf-8`
    - `application/json`

- Example (`application/json;charset=utf-8`, `application/json`):

```json
["/api/alternatives/301","/api/alternatives/305","/api/alternatives/323"]
```

## GET /api/alternatives/:alternativeId

### Get an alternative


### Returns the details of a specific alternative


### Authentication

This part of the API is protected by the following authentication mechanisms:

 * [Basic Authentication](https://en.wikipedia.org/wiki/Basic_access_authentication)
 * JSON Web Tokens ([JWTs](https://en.wikipedia.org/wiki/JSON_Web_Token))


Clients must supply the following data

One of the following:

 * Cookies automatically set by browsers, plus a header
 * A JWT Token signed with this server's key


### Captures:

- *alternativeId*: The ID of the alternative

### Response:

- Status code 200
- Headers: []

- Supported content types are:

    - `application/json;charset=utf-8`
    - `application/json`

- Example (`application/json;charset=utf-8`, `application/json`):

```json
{"id":301,"author_id":3,"phrase_id":24,"text":"This is an altrnative to a frase.","created_at":"2023-05-15T12:45:00Z","spell_check":[{"code":1,"col":11,"len":10,"pos":11,"row":0,"s":["alternative"],"word":"altrnative"},{"code":1,"col":27,"len":5,"pos":27,"row":0,"s":["phrase","place","praise","price"],"word":"prase"}]}
```

## PATCH /api/alternatives/:alternativeId/choose

### Choose an alternative


### Marks a specific alternative as chosen and returns the URL to the phrase


### Authentication

This part of the API is protected by the following authentication mechanisms:

 * [Basic Authentication](https://en.wikipedia.org/wiki/Basic_access_authentication)
 * JSON Web Tokens ([JWTs](https://en.wikipedia.org/wiki/JSON_Web_Token))


Clients must supply the following data

One of the following:

 * Cookies automatically set by browsers, plus a header
 * A JWT Token signed with this server's key


### Captures:

- *alternativeId*: The ID of the alternative

### Response:

- Status code 200
- Headers: []

- Supported content types are:

    - `application/json;charset=utf-8`
    - `application/json`

- A uri leading to a user with ID equal to 3 (`application/json;charset=utf-8`, `application/json`):

```json
"/api/users/3"
```

- A uri leading to a phrase with ID equal to 24 (`application/json;charset=utf-8`, `application/json`):

```json
"/api/phrases/24"
```

- A uri leading to an alternative with ID equal to 301 (`application/json;charset=utf-8`):

```json
"/api/alternatives/301"
```

## GET /api/phrases

### Get list of phrases


### Returns a list of URLs to phrases


### Authentication

This part of the API is protected by the following authentication mechanisms:

 * [Basic Authentication](https://en.wikipedia.org/wiki/Basic_access_authentication)
 * JSON Web Tokens ([JWTs](https://en.wikipedia.org/wiki/JSON_Web_Token))


Clients must supply the following data

One of the following:

 * Cookies automatically set by browsers, plus a header
 * A JWT Token signed with this server's key


### GET Parameters:

- open
     - **Values**: **
     - **Description**: Filter by open phrases (for which no alternative has been chosen)
     - This parameter is a **flag**. This means no value is expected to be associated to this parameter.


### Response:

- Status code 200
- Headers: []

- Supported content types are:

    - `application/json;charset=utf-8`
    - `application/json`

- Example (`application/json;charset=utf-8`, `application/json`):

```json
["/api/alternatives/301","/api/alternatives/305","/api/alternatives/323"]
```

## POST /api/phrases

### Create a new phrase


### Creates a new phrase and returns its URL


### Authentication

This part of the API is protected by the following authentication mechanisms:

 * [Basic Authentication](https://en.wikipedia.org/wiki/Basic_access_authentication)
 * JSON Web Tokens ([JWTs](https://en.wikipedia.org/wiki/JSON_Web_Token))


Clients must supply the following data

One of the following:

 * Cookies automatically set by browsers, plus a header
 * A JWT Token signed with this server's key


### Request:

- Supported content types are:

    - `application/json;charset=utf-8`
    - `application/json`

- Example (`application/json;charset=utf-8`, `application/json`):

```json
{"text":"This is a new phrase."}
```

### Response:

- Status code 201
- Headers: []

- Supported content types are:

    - `application/json;charset=utf-8`
    - `application/json`

- A uri leading to a user with ID equal to 3 (`application/json;charset=utf-8`, `application/json`):

```json
"/api/users/3"
```

- A uri leading to a phrase with ID equal to 24 (`application/json;charset=utf-8`, `application/json`):

```json
"/api/phrases/24"
```

- A uri leading to an alternative with ID equal to 301 (`application/json;charset=utf-8`):

```json
"/api/alternatives/301"
```

## GET /api/phrases/:phrase_id

### Get a phrase


### Returns the details of a specific phrase


### Authentication

This part of the API is protected by the following authentication mechanisms:

 * [Basic Authentication](https://en.wikipedia.org/wiki/Basic_access_authentication)
 * JSON Web Tokens ([JWTs](https://en.wikipedia.org/wiki/JSON_Web_Token))


Clients must supply the following data

One of the following:

 * Cookies automatically set by browsers, plus a header
 * A JWT Token signed with this server's key


### Captures:

- *phrase_id*: The ID of the phrase

### Response:

- Status code 200
- Headers: []

- Supported content types are:

    - `application/json;charset=utf-8`
    - `application/json`

- An open phrase (`application/json;charset=utf-8`, `application/json`):

```json
{"id":1,"author_id":123,"text":"This is a sample phrase.","created_at":"2022-08-10T18:00:00Z","is_open":true,"chosen_alt_id":null,"num_alts":1,"spell_check":[]}
```

- A closed phrase (`application/json;charset=utf-8`, `application/json`):

```json
{"id":2,"author_id":123,"text":"Phrase with a speling error","created_at":"2023-10-01T18:00:10Z","is_open":false,"chosen_alt_id":1012,"num_alts":3,"spell_check":[{"code":1,"col":14,"len":7,"pos":14,"row":0,"s":["spelling"],"word":"speling"}]}
```

## GET /api/phrases/:phrase_id/alternatives

### Get alternatives for a phrase


### Returns a list of URLs to alternatives for a specific phrase


### Authentication

This part of the API is protected by the following authentication mechanisms:

 * [Basic Authentication](https://en.wikipedia.org/wiki/Basic_access_authentication)
 * JSON Web Tokens ([JWTs](https://en.wikipedia.org/wiki/JSON_Web_Token))


Clients must supply the following data

One of the following:

 * Cookies automatically set by browsers, plus a header
 * A JWT Token signed with this server's key


### Captures:

- *phrase_id*: The ID of the phrase

### Response:

- Status code 200
- Headers: []

- Supported content types are:

    - `application/json;charset=utf-8`
    - `application/json`

- Example (`application/json;charset=utf-8`, `application/json`):

```json
["/api/alternatives/301","/api/alternatives/305","/api/alternatives/323"]
```

## POST /api/phrases/:phrase_id/alternatives

### Create an alternative for a phrase


### Creates a new alternative for a specific phrase and returns its URL


### Authentication

This part of the API is protected by the following authentication mechanisms:

 * [Basic Authentication](https://en.wikipedia.org/wiki/Basic_access_authentication)
 * JSON Web Tokens ([JWTs](https://en.wikipedia.org/wiki/JSON_Web_Token))


Clients must supply the following data

One of the following:

 * Cookies automatically set by browsers, plus a header
 * A JWT Token signed with this server's key


### Captures:

- *phrase_id*: The ID of the phrase

### Request:

- Supported content types are:

    - `application/json;charset=utf-8`
    - `application/json`

- Example (`application/json;charset=utf-8`, `application/json`):

```json
{"text":"This is a new alternative."}
```

### Response:

- Status code 201
- Headers: []

- Supported content types are:

    - `application/json;charset=utf-8`
    - `application/json`

- A uri leading to a user with ID equal to 3 (`application/json;charset=utf-8`, `application/json`):

```json
"/api/users/3"
```

- A uri leading to a phrase with ID equal to 24 (`application/json;charset=utf-8`, `application/json`):

```json
"/api/phrases/24"
```

- A uri leading to an alternative with ID equal to 301 (`application/json;charset=utf-8`):

```json
"/api/alternatives/301"
```

## GET /api/users

### Get list of users


### Returns a list of URLs to users


### Authentication

This part of the API is protected by the following authentication mechanisms:

 * [Basic Authentication](https://en.wikipedia.org/wiki/Basic_access_authentication)
 * JSON Web Tokens ([JWTs](https://en.wikipedia.org/wiki/JSON_Web_Token))


Clients must supply the following data

One of the following:

 * Cookies automatically set by browsers, plus a header
 * A JWT Token signed with this server's key


### Response:

- Status code 200
- Headers: []

- Supported content types are:

    - `application/json;charset=utf-8`
    - `application/json`

- Example (`application/json;charset=utf-8`, `application/json`):

```json
["/api/alternatives/301","/api/alternatives/305","/api/alternatives/323"]
```

## POST /api/users

### Register a new user


### Creates a new user and returns its URL


### Request:

- Supported content types are:

    - `application/json;charset=utf-8`
    - `application/json`

- Example (`application/json;charset=utf-8`, `application/json`):

```json
{"user_name":"New User","password":"s3cur3P@ssw0rd!"}
```

### Response:

- Status code 201
- Headers: []

- Supported content types are:

    - `application/json;charset=utf-8`
    - `application/json`

- A uri leading to a user with ID equal to 3 (`application/json;charset=utf-8`, `application/json`):

```json
"/api/users/3"
```

- A uri leading to a phrase with ID equal to 24 (`application/json;charset=utf-8`, `application/json`):

```json
"/api/phrases/24"
```

- A uri leading to an alternative with ID equal to 301 (`application/json;charset=utf-8`):

```json
"/api/alternatives/301"
```

## GET /api/users/:user_id

### Get a user


### Returns the details of a specific user


### Authentication

This part of the API is protected by the following authentication mechanisms:

 * [Basic Authentication](https://en.wikipedia.org/wiki/Basic_access_authentication)
 * JSON Web Tokens ([JWTs](https://en.wikipedia.org/wiki/JSON_Web_Token))


Clients must supply the following data

One of the following:

 * Cookies automatically set by browsers, plus a header
 * A JWT Token signed with this server's key


### Captures:

- *user_id*: The ID of the user

### Response:

- Status code 200
- Headers: []

- Supported content types are:

    - `application/json;charset=utf-8`
    - `application/json`

- Example (`application/json;charset=utf-8`, `application/json`):

```json
{"id":3,"user_name":"John Doe","created_at":"2021-01-01T14:30:00Z"}
```

## GET /api/users/:user_id/phrases

### Get user's phrases


### Returns a list of URLs to phrases created by a specific user


### Authentication

This part of the API is protected by the following authentication mechanisms:

 * [Basic Authentication](https://en.wikipedia.org/wiki/Basic_access_authentication)
 * JSON Web Tokens ([JWTs](https://en.wikipedia.org/wiki/JSON_Web_Token))


Clients must supply the following data

One of the following:

 * Cookies automatically set by browsers, plus a header
 * A JWT Token signed with this server's key


### Captures:

- *user_id*: The ID of the user

### GET Parameters:

- open
     - **Values**: **
     - **Description**: Filter by open phrases (for which no alternative has been chosen)
     - This parameter is a **flag**. This means no value is expected to be associated to this parameter.


### Response:

- Status code 200
- Headers: []

- Supported content types are:

    - `application/json;charset=utf-8`
    - `application/json`

- Example (`application/json;charset=utf-8`, `application/json`):

```json
["/api/alternatives/301","/api/alternatives/305","/api/alternatives/323"]
```


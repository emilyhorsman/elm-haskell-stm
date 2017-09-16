# elm-haskell-stm

## Running the Haskell back-end

(`$` character denotes the beginning of a prompt, it should not be included.)

### Install Stack

```
$ curl -sSL https://get.haskellstack.org/ | sh
```

Consult the [installation guide](https://docs.haskellstack.org/en/stable/README/#how-to-install) if you’re on Windows.

### Install elm-haskell-stm

```
$ git clone https://github.com/emilyhorsman/elm-haskell-stm.git
$ cd elm-haskell-stm
$ stack setup
$ stack build
```

### Run the elm-haskell-stm server

```
$ stack exec elm-haskell-stm-exe
```

## Running the Elm front-end

```
$ elm-package install
$ elm-reactor
```

## Development Standards

* All Haskell is run through [stylish-haskell](https://github.com/jaspervdj/stylish-haskell) and [hlint](https://hackage.haskell.org/package/hlint)
* All Elm is run through [elm-format](https://github.com/avh4/elm-format)

## Architecture

### Source code

- `app/Main.hs` is merely an entry point and has no application logic.
- `src/Lib.hs` contains the server `Application`, creates the central channel, and handles each WebSocket connection.
- `src/HighScores.hs` contains the application logic and handles the message channels.

### Server boots up

- The server creates a new `TChan` for `CentralMessage`s.
  This is called the central channel.
  There is one for the entire server.
- The server forks a thread that will run for the entire lifecycle.
  This thread holds the server state and reads from the central message channel.
  This is called the central thread.

### New client

- A browser opens a WebSocket connection to the application.
- The web server forks a thread to handle this connection.
- `Lib.wsApp` is called and accepts the connection.
    - `wsApp` creates a new `TChan` for `ClientMessage`s.
      This is a client channel.
      There is one per WebSocket connection.
    - `wsApp` forks two threads that will run for the lifecycle of this client.
    - One thread reads from the newly created channel.
    - One thread reads from the WebSocket connection.
    - `wsApp` writes a `NewUser` `CentralMessage` to the central channel.
      It writes a reference to the client channel and WebSocket connection with the message.
        - `HighScores.processCentralChan` running on the central thread reads the `NewUser` message.
        - The central thread adds the client channel to the server state.
        - The central thread writes an `AssignUsername` message to the client channel.
            - The client thread reads the `AssignUsername` message and writes to the WebSocket connection.
                - The Elm application receives a `WSReceiveMessage` message in its `update` function.
        - The central thread writes an `AssignScore` client message for each existing score to the current client channel.
            - The client thread reads the `AssignScore` message and writes to the WebSocket connection.
                - The Elm application receives a `WSReceiveMessage` message in its `update` function.
        - The central thread writes an `AssignScore` client message about the new user to all other client channels.
            - Each of the other client threads reads this `AssignScore` message and writes to their WebSocket connections.
                - The Elm applications each receive a `WSReceiveMessage` message in their `update` function.

### User clicks shape

- The Elm application receives a `Click` message in its `update` function.
- The `update` function returns a `Cmd` to send the associated username across the WebSocket connection.
- The thread reading that connection writes an `Event` central message to the central channel.
- The central thread reads the `Event` message.
- The central thread updates the score and writes an `AssignScore` client message to all client channels.
    - Each client thread reads this `AssignScore` message and writes to their WebSocket connections.
        - The Elm applications each receive a `WSReceiveMessage` message in their `update` function.

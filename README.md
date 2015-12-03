# surrogate

![Alt text](/screens/surrogate.png)

Surrogate is a simple single page proxy downloader app. It is a basic ChicagoBoss App written in erlang. **Right now it only supports rapidagator download links with a premium account**, but I would like it to support more premium services, standard http, and ftp downloads. 

## Requirements
- Erlang OTP 17.0+
- Linux (strongly recommended)
- Node

## Installation
This section may change when ChicagoBoss goes to 1.0.

- `git clone https://github.com/ChicagoBoss/ChicagoBoss.git`
- `cd ChicagoBoss/`
- `make compile`
- `cd ..`
- `git clone https://github.com/jbreindel/surrogate.git`
- `cd surrogate/`
- `make compile`

## Design
Most of the application centers around the `/lib/manager.erl` module. This is the main download event loop. It holds the current speeds of the downloads and spawns acquisition, and download processes. The front end is done with server side templates for the tables and receives event notifications through the web socket. There is a thin layer of abstraction between the manager and the web socket modules located in `/lib/subscriber.erl` used to allow other erlang processes attach to the manager without needing to worry about json serialization.

## TODO
- **Bug fixes**
- OTP gen_* refactor
- Mobile styles
- Desktop notifications
- Cancel download functionality
- Standard http download support

### Contributing
Accepting pull requests. I'd appreciate it if others would help with other premium services as I don't subscribe to many of them.

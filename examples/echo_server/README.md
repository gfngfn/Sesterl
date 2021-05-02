
## How to Compile and Run

```console
# Generate `rebar.config`
$ sesterl config ./

# Compile sources
$ rebar3 sesterl compile

# Run
$ rebar3 shell
```

Then you can see `http://localhost:8080` on your browser or some CLI tool:

```
$ curl "http://localhost:8080"
Hello, Sesterl! (no text was given, 2)
$ curl "http://localhost:8080/?text"
Hello, Sesterl! (no text was given, 1)
$ curl "http://localhost:8080/?text=foo"
foo
$ curl "http://localhost:8080/?text=Hello%20World"
Hello World
$ curl "http://localhost:8080/users/taro"
taro
```

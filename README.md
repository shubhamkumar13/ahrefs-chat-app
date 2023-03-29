# Pre-requistes
1. Install opam
    ```bash
    sudo apt install opam
    ```
2. Create a switch for OCaml 4.14.0
    ```bash
    opam init -y
    opam sw create 4.14.0
    eval $(opam env)
    ```
4. Clone the repo 
    ```bash
    git clone git@github.com:shubhamkumar13/ahrefs-chat-app.git
    ```

5. Install the opam packages using the `opam_packages.export`
    ```bash
    cd /path/to/ahrefs-chat-app
    opam sw import opam_packages.export
    ```
    N.B. : 
        If there are errors while importing the packages where
        compilation errors related to `conf-gmp` and `conf-libev` 
        occur. Run the following commands

        sudo apt install libgmp-dev libev-dev

# How to use this app

1. Build the app
    ```bash
    cd /path/to/ahrefs-chat-app
    dune build @all
    ```
2. To start the server 
    ```bash
    _build/default/bin/server.exe
    ```
3. To start the client (on a different terminal)
    ```bash
    _build/default/bin/client.exe
    ```

## Things to do :
- add configurations which allows port to manually be whatever
- add roundtrip time

## Unimportant but would be cool :
- if the server cannot connect to a port try another port or kill the port

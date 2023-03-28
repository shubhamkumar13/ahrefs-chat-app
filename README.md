# Prerequistes
1. Install opam
    ```bash
    sudo apt install opam
    ```
2. Create a switch for OCaml 4.14.0
    ```bash
    opam init
    opam sw create 4.14.0
    eval $(opam env)
    ```
3. Install the opam packages using the `opam_packages.export`
    ```bash
    opam sw import opam_packages.export
    ```


# How to use this app

1. Clone the repo 
    ```bash
    git clone git@github.com:shubhamkumar13/ahrefs-chat-app.git
    ```
2. Build the app
    ```bash
    dune build @all
    ```
3. To start the server 
    ```bash
    _build/default/bin/server.exe
    ```
4. To start the client (on a different terminal)
    ```bash
    _build/default/bin/client.exe
    ```
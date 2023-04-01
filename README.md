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
        related to `conf-gmp` and `conf-libev` run the following commands

        sudo apt install libgmp-dev libev-dev

# How to use this app

1. Build the app
    ```bash
    cd /path/to/ahrefs-chat-app
    dune build @all
    ```
2. To start a server
   1. `dune exec main`
   2. as soon as you execute the above command 
      you should see a prompt as shown below
      <!-- ![prompt](media/first_page.png) -->
      ```bash
      ❯ dune exec main
        Choose which port you want to run your application in

        {Please make sure that the port selected
        should be same for server and client, thank you} :

                     =>
       ```
      type in the port number that you want,
      do remember that the port for server and client
      should be same.
   3. Let's choose `8080` and press enter, this would bring you down
      to the next option menu as shown below
      <!-- ![prompt](media/second_page.png) -->
      ```bash
      ❯ dune exec main
        Choose which port you want to run your application in

            {Please make sure that the port selected
            should be same for server and client, thank you} :

                    => 8080
        Choose which application you want to run :

            1. For running a server choose 1

            2. For running a client choose 2

                    => 
      ```
   4. Now if you haven't started a server choosing 
      option 2 would end up with refusal of connection.
      So choose 1, by typing 1 press enter after `=> 1`.
      You will se a prompt :
      `Waiting for socket connection :`
      which would change to :
      ```bash
        Waiting for socket connection :
        Client connected : 127.0.0.1
        Enter message :
      ```
   5. For choosing to run a client enter `=> 2`
      This will show the following prompt
      ```bash
      Enter message :

      ```
      now you can send and receive messages from client and server. Fin.
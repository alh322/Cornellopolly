1. Install Opam
Mac: Make sure you have Homebrew or MacPorts installed. Run the command: brew install opam if you have Homebrew or sudo port install opam if you have MacPorts.
Windows: Run this command from Ubuntu: sudo apt install opam

2. Initialize OPAM
Linux, Mac, WSL2: Run opam init --bare -a -y
WSL1: Run opam init --bare -a -y --disable-sandboxing

3. Create an OPAM Switch
Run command: opam switch create cs3110-2023sp ocaml-base-compiler.4.14.0
- Then, run command: eval $(opam env)
- Then, run command: opam switch list
- Then, install OPAM packages: opam install -y utop odoc ounit2 qcheck bisect_ppx menhir ocaml-lsp-server ocamlformat ocamlformat-rpc

4. Install Required Libraries:
- opam install dune
- opam install yojson
- opam install ansiterminal

5. Build Project
- from a Terminal, run: make build

6. Run Game
- from a Terminal, run: make play
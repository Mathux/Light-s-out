# Light-s-out
This is a generalisation of the game Lights Out written in Ocaml.

### Goal
Pressing any of the lights will toggle it and the adjacent lights. 
The goal of the puzzle is to switch all the lights off, preferably in as few button presses as possible.

### Usage

Ocaml must be installed.
In a command line :

```bash
make
```

To play this game, just do :

```bash
./Algo n m p t
```

n : height > 0
m : width > 0
p : number of colors > 1
t : size of blocks > 0 (optionnal)

### Command in game

To press a tile, you can click on it or press the key a or w.
You can activate and desactivate borders with the key p.


### Solvers

I implemented two solvers.
The first one solve the entire system and give you one solution. Then you can choose to show all the solution or not. This solution is computed in a time proportional to (nm)^3.
The second one is faster but it gives you one solution. This solution is computed in a time proportionnal to n^3.

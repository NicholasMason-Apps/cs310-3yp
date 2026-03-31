# Developing Games in Haskell

For my Third Year Project as part of my 3rd year Computer Science course at the University of Warwick

# Specification

For the specification, please see the `3YP_Specification.pdf` file inside the repository

# Installation

If you do not have GHC and Stack installed, please do so

## Linux

### 1. Install SDL2 via apt

`sudo apt install libsdl2-dev libsdl2-ttf-dev libsdl2-image-dev libsdl2-mixer-dev libsdl2-gfx-dev libsdl2-net-dev`

### 2. Build and run

You can run each version of the project as follows:

- `./run.sh gloss` - 2D implementation using Gloss
- `./run.sh sdl` - 2D implementation using SDL
- `./run.sh raylib` - 3D implementation
    - If you are using WSL, please build Raylib with the WSL flag: `./run.sh -wsl raylib`

## Windows

Install WSL if you have not already, and follow the Linux steps
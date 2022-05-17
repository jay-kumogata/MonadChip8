# MonadChip8

## Introduction

MonadChip8 is a CHIP-8 emulator written in Haskell.

<img src="https://github.com/jay-kumogata/MonadChip8/blob/master/screenshots/AMABIE04.png" width="300">

## How to Play

Please execute the following from the Stack (version 2.7.3) environment.

	> stack setup
	> stack install random
	> stack install gloss
	> stack ghc MonadChip8.hs
	> ./MonadChip8 PONG

In case of Windows environment, the following [procedure](https://stackoverflow.com/questions/42072958/haskell-with-opengl-unknown-glut-entry-glutinit) is required. 

## How to control
The keys are mapped as follows.
  
	Original |1|2|3|C| Mapping to |4|5|6|7|
	         |4|5|6|D|            |R|T|Y|U|
	         |7|8|9|E|            |F|G|H|J|
	         |A|0|B|F|            |V|B|N|M|

## Specification
### Memory
- RAM (200H - F10H)
- Hexadecimal font (F10H -F60H)

### Registers
- Data Registers (V0 .. VF)
- Address Registers (I)
- Timers (Delay and Sound)
- Stack (16 word length and stack pointer)

### Graphics
- Sprite (CHIP-8 Mode: 8 x 1 .. 15)
- Collision Flag
- Hexadecimal font
  
### Instruction set
- CHIP-8 instructions (assignment, arithmetic, conditional branch, subroutine call, draw sprite, etc.)

### Keyboard
- Hexadecimal keyboard

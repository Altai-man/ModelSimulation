# Tool for modelling of 3rd Generation MTS.

### Requirements

- `dev-haskell/lens-4.13`
- `dev-haskell/monadrandom-0.4`

### Compilation

Just `ghc -O2 -Wall --make -rtsopts Main.hs` it.
I recommend `./Main +RTS -H750m` command for running.

### To change the simulation strategy

1. Change appropriate values in `Constants.hs`
2. It can be probability of user types, their home locations, work locations, speeds, sizes, whatever.
3. Recompile and run!

### To add new user type

1. Add your MU variant in `Types.hs`
2. Write generation values in `City.hs`
3. Write probabilities and speeds for your type in `Constants.hs`.
4. Write "update" function for MU, that will define behevoir of MUs with your type.
5. Include this function in `Update.hs` dispatcher.
6. Recompile and run!

### Credentials

Math model of this code is actually was described in "Mobility Modeling in Third Generation Mobile Telecommunication Systems" paper that was written by J.G.Markoulidakis, G.L.Lyberopoulos, D.F.Tsirkas and E.D.Sykas, it's just my "for fun" haskell implementation.
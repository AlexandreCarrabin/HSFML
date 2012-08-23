HSFML
=====
This is the first release of the Haskell binding for SFML2.

The graphics, system and window packages are completed.
The audio and network packages are still missing.

Installation
------------
To build the binding with the examples do
```
runhaskell Setup.hs configure -fexamples
runhaskell Setup.hs build
runhaskell Setup.hs install
```

To build the haddock documentation do
```
runhaskell Setup.hs configure -fhaddock-hack
runhaskell Setup.hs haddock
```

Please feel free to make any remarks regarding code or signal any bugs.

For more informations on SFML see:
SFML Website : http://www.sfml-dev.org
SFML Github : https://github.com/LaurentGomila/SFML


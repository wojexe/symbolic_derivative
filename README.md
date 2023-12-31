# Symbolic derivative

Haskell program that symbolically calculates partial derivatives of a given function of three variables. The function is defined by an expression containing arithmetic operations, functions (sin, cos, tg, exp, ln), as well as brackets, constants, and variables (x, y, z), e.g., sin(1/x) * exp(y^3 - 2).

## How to run

```sh
# Clone the repo
git clone https://github.com/wojexe/symbolic_derivative.git

cd symbolic_derivative

# Compile with `ghc` without aux files
ghc main.hs -no-keep-o-files -no-keep-hi-files
```


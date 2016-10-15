This file loads Proof General.  It is required by the
individual prover modes.  Loading order of PG is:

1. proof-site (variables, autoloads & stubs for mode functions)
2. stub <PA>-mode function sets proof-assistant-symbol and related variables
3. prover-dependent variables defined in pg-custom
4. stub explicitly loads <PA>/<PA>.el and execute real mode function
5. <PA>.el requires this file, rest of PG loaded here
6. further modules loaded by autoloads/prover-specific requires.

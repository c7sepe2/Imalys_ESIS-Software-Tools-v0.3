**[Home](../README.md) «» [Manual](../manual/README.md) «» [Tutorial](../tutorial/README.md) «» [Background](../background/README.md) «» [Source](../source)**

------

## Replace

The [replace](../manual/12_Replace.md) command performs two functions:

1. Variables defined under *replace* can be used in the [process chain](../manual/0_Execute.md). The variables can simplify the input.

2. If the variables are passed as a comma-separated list, the [replace](../manual/12_Replace.md) command iterates the process chain for each variable. By skillfully selecting the variables, large areas or long time series can be processed iteratively only by a single call of *Imalys* (examples in [tutorial](../tutorial/7_Variables.md)).

Variables always consist of a “$” character and a (one-digit) number. 
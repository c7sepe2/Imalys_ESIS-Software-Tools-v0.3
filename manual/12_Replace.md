**[Home](../README.md) «» [Manual](../manual/README.md) «» [Tutorial](../tutorial/README.md) «» [Background](../background/README.md) «» [Source](../source)**

------

## *Replace* variables of the process chain

To manage long process chains and to repeat identical processes with different parameters *Replace* allows to use variables within the process chain. Variables must be declared at the very beginning of the process chain. The declaration is formatted as "variable-ID = value". The ID consists of a “$” sign followed by one figure from 0 to 9. The value can be each expression including figures, strings and comma separated lists.

To repeat the whole process chain, variables can be given as list of different values, delimited by colons and included in parantheses »{var1, var2, var3, ...}«. If *Replace* finds such a list, the whole process chain is repeated for each variable in the list. If more than one variable is passed as a list, the number of variables in all lists must be the same. Each run uses the values at the same position in the lists. During the repetition variables with only one entry remain unchanged.

Imalys allows variables that are passed as a list, e.g. to select bands during image [import](3_Import.md) and other variables that repeat a process. The difference are the curly brackets “{” and “}”. Only variables in brackets repeat the process chain.

------

### Example

The following list uses two variables:

```
IMALYS [replace]
replace
	$1 = c7934
	$2 = München
home
	directory = ~/.imalys
	clear = true
	log = ~/ESIS/Results/$1_$2
compile
	search = ~/ESIS/Takes/$1*2022*.tif
reduce
	select = compile
	execute = bestof
	retain = bands
export
	select = bestof
	target = ~/ESIS/Results/$1_$2_2022.tif
```

The process chain expands to:

```
IMALYS [replace]
replace
	$1 = c7934
	$2 = München
home
	directory = ~/.imalys
	clear = true
	log = ~/ESIS/Results/c7934_München
compile
	search = ~/ESIS/Takes/c7934*2022*.tif
reduce
	select = compile
	execute = bestof
	retain = bands
export
	select = bestof
	target = ~/ESIS/Results/c7934_München_2022.tif
```

This example uses the tk100 map sheet ID *c7934* and the name of  the city of *Munich* (Germany) as variables. The *search* process under [compile](4_Compile.md) selects all TIFF-images of Munich taken at 2022 from the *~/ESIS/Takes* directory. [Reduce](5_Reduce.md) combines them with *bestof* to one multispectral image. The result is stored as TIFF-file at the ESIS/results directory using [export](11_Export.md). 

```
IMALYS [repeat]
replace
	$1 = {c3922, c4738, c5142, c7934}
	$2 = {Hannover, Leipzig, Chemnitz, München}
home
... as first example ...
```

The second example shows how to repeat the first example for 4 different cities. The variables have to be extended to variable lists, everything else remains unchanged. The whole process is repeated for 4 times. Each run uses the values at the same position in the lists. The number of entries for a variable list is not limited. 

------

[Top](12_Replace.md)
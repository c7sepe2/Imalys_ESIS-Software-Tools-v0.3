## Replace	

**replace variables given at the process chain**

To manage long process chains and to repeat identical processes with different parameters *Replace* allows to use variables within the process chain. Variables must be declared at the very beginning of the process chain. The declaration is formatted as "variable-ID = value". The ID consists of a “$” sign followed by one figure from 1 to 9. The value can be each expression including figures and strings.

Varables can be given as list of different values, delimited by colons. If *Replace* finds a variable list, the whole process chain is repeated for each variable in the list. If more than one variable is passed as a list, the number of variables in all lists must be the same. Imalys repeats the process for each list item. During the repetition variables with only one entry remain unchanged.

------

### Replace

**Replace locally defined variables**

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

This example uses the map sheet ID *c7934* and the name of  the city of *Munich* (Germany) as variables. The *search* process under [compile](4_Compile.md) selects all TIFF-images of Munich taken at 2022 from the *~/ESIS/Takes* directory. [Reduce](5_Reduce.md) combines them with *bestof* to one multispectral image. The result is stored as TIFF-file at the ESIS/results directory using [export](11_Export.md). 

```
IMALYS [repeat]
replace
	$1 = c3922, c4738, c5142, c7934
	$2 = Hannover, Leipzig, Chemnitz, München
home
... as first example ...
```

The second example shows how to repeat the first example for 4 different cities. The variables have to be extended to variable lists, everything else remains unchanged. The whole process is repeated for 4 times. Each run uses the values at the same position in the lists. The number of entries for a variable list is not limited. 

-----

[Previous](11_Export.md)	–	[Index](README.md)	–	[Next](0_Execute.md)
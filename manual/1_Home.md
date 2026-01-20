**[Home](../README.md) «» [Manual](../manual/README.md) «» [Tutorial](../tutorial/README.md) «» [Background](../background/README.md) «» [Source](../source)**

[TOC]

------

## *Home:* Create or select an *Imalys* working directory

```
IMALYS [working directory, messages]
home
	directory = ~/.imalys
	clear = true
	log = ~/ESIS/results
```

*In this example the default working directory "~/.imalys" is used for processing and the messages (log = …) are directed to the "results" directory.*

------

Each *Imalys* call needs the name of a working *directory*. *Imalys* uses large matrix transformations. To get sufficient speed, a quick local memory should be assigned as a working directory. If several instances of *Imalys* are called at the same time, each call needs separate working directory. "IMALYS" at the beginning of the first line is mandatory. The remainder of the line is ignored and can be used for hints. 

Each raster or vector file that *Imalys* is to process must first be copied into the working directory using the [import](3_Import.md) and / or [compile](4_Compile.md) command. Both processes combine and transform the passed data. 

------

### *Directory:* Assign or create a working directory

```
IMALYS [working directory, messages]
home
	directory = ~/.imalys
```

*Imalys* will initially save all results at the working directory. The results will be named as the commands. If no working directory is given, *Imalys* tries to create a `~/.imalys/` directory at the users home.

------

### *Clear:* Clear the working directory

```
IMALYS [working directory, messages]
home
	directory = ~/.imalys
	clear = false | true
```

*One of the two alternatives of "clear" must be passed. "False" is default and can be skipped.*

Most processes will produce various intermediate results. If the final result of the last run is stored at a separate directory, the working directory should be cleared at the beginning of each process chain.

------

### *Log:* Set a message directory

```
IMALYS [working directory, messages]
home
	directory = ~/.imalys
	clear = true
	log = ~/ESIS/results
```

*Imalys* reports each command, returns an activity log for all processes and lists error messages. The error list includes error messages from external processes called by *Imalys*. We strongly recommend to store final results and messages at the same place. If no *log* is given, the messages are stored at the working directory.

[Top](1_Home.md)

**[Home](../README.md) «» [Manual](../manual/README.md) «» [Tutorial](../tutorial/README.md) «» [Background](../background/README.md) «» [Source](../source)**

------

## Run Imalys

Imalys is designed to translate traits defined for ESIS into standard and well-defined data products. To this end, Imalys contains commands and functions that allow extensive analysis and transformation of image and vector data, as in a GIS system. However, Imalys is not controlled via a user interface but with console commands. 

Imalys was designed to run long process chains without manual intervention and to automatically repeat the results with changing input data, so that large data sets can be processed in manageable sections. This is only possible if the necessary steps are coded. We have endeavored to keep this code very simple and manageable, even though the underlying processes are complex.

*Imalys* must be called using a shell or terminal. To simplify the call, all commands and parameters for an entire process chain are passed by one text file. The call consist of the executable file *xImalys* and the name of the text file (process chain) containing commands and parameters. 

```
~$ xImalys ~/ESIS/Hooks/Munich
```

The example calls *Imalys* with the process chain *Munich*.

The *Imalys* process chain is controlled by 12 commands and their parameters. The commands in the process chain also serve as filename of the results and as field names in tables. Therefore the command names are short and can have a much wider meaning in general usage. 

Each command needs a separate line. Each parameter must be given as a “name = value” pair according to a dictionary. Some parameters run subprocesses others assign values. Everything after the "#" sign is ignored. The following chapters explain each command and parameter and give hints for their application. The background of the processes and dependencies among them is explained at the [background](../background/README.md) section. 

A process chain could look like this:

```
IMALYS [process chain "Munich"]
home
	directory = ~/.imalys
	clear = true
	log = ~/ESIS/Results	
#	––––––––––––––––––––––––––––––––––––––––
#	comment ...
#	––––––––––––––––––––––––––––––––––––––––
compile
	search = ~/ESIS/Takes/*c7934*2022*.tif
reduce
	select = compile
	execute = bestof
	retain = bands
export
	select = bestof
	target = ~/ESIS/Results/c7934_Munich_2022.tif
```

The example combines all images with *c7934* and *2022* as part of the filename (*search =* ) into one optimized multispectral image of Munich (*execute = bestof*) from the year 2022. The working directory is set to default (*directory = ~/.imalys*), the directory is emptied at the beginning (*clear = true*), the logs (*log =* …) and the results (*target =* …) are saved at the same place (*~/ESIS/results*).

[Top](1_Execute.md)

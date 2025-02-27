## Run Imalys

*Imalys* includes a large number of commands and parameters due to the variety required. [Commands](README.md) execute basic functions, parameters select processes and accept inputs. *Imalys* must be called using a shell or terminal. To simplify the call, all commands and parameters for an entire process chain are passed by one text file. The call consist of the executable file *xImalys* and the name of the text file (process chain) containing commands and parameters. 

```
$ xImalys ~/ESIS/Hooks/Munich
```

The example calls *Imalys* with the process chain *Munich*.

The *Imalys* process chain is controlled by 12 commands and their parameters. The commands in the process chain also serve as filename of the results and as field names in tables. Therefore the names are short and can have a much wider meaning in general usage. 

Each command needs a separate line. Each parameter must be given as “name = value” pair according to a dictionary. Some parameters run subprocesses others assign values. The following lessons explain each command and parameter and give hints for their application. The background of the processes and dependencies among them are explained at [background](../background/README.md). 

A process chain could look like this:

```
IMALYS [process chain "Munich"]
home
	directory = ~/.imalys
	clear = true
	log = ~/ESIS/Results
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

The example combines all images with *c7934* and *2022* in the file name (*search =* ) into one multispectral image of Munich (*execute = bestof*) from the year 2022. The working directory is set to default (*directory = ~/.imalys*), the directory is emptied at the beginning (*clear = true*), the logs (*log =* ) and the results (*target =* ) are saved at the same directory (*~/ESIS/Results*).

-----

[Previous](12_Replace.md)	–	[Index](README.md)	–	[Next](1_Home.md)

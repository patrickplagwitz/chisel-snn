This is a DSL-based generator for SNN accelerator designs for FPGAs.

A paper describing this work will be presented at DSD '24.
If you want to reference our work, please cite the paper as follows:
```
@inproceedings{plagwitz2024snnchisel,
  title={DSL-based SNN Accelerator Design using Chisel},
  author = {Plagwitz, Patrick and Hannig, Frank and Teich, J{\"u}rgen and Keszocze, Oliver},
  booktitle = {2024 27th Euromicro Conference on Digital System Design (DSD)},
  year={2024},
  organization={IEEE}
}
```

Installation
====================
Dependencies
---------------
Scala and Python need to be installed.
Verilator and firtool will be downloaded and built automatically during the next steps.

Procedure
------------
First, set up all dependencies by calling
```bash
$ source setup.sh
```
in the root folder.

Now, a classifier design can be built using:
```
$ sbt run
```

Modify the Main.scala file to change the configuration that is built or to perform a DSE.

Tests
========================
After you have completed the installation steps, you can try running some integration tests by issuing the following commands to make sure everything works as expected.
```bash
$ sbt test
```

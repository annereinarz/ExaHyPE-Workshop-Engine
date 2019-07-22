# This is the ExaHyPE Teaching Code #

This is the main repository of the ExaHyPE teaching code project.

## Mini installation guide ##

On Ivy muc we have to load several modules. Copy this to your .bashrc

    module switch intel/19.0
    module unload mpi.intel/2019
    module load mpi.intel/2019
    module load tbb/2019
    module load python/3.6_intel
    module load gcc

Copy and paste these commands to start with a working ExaHyPE application and initialize the demo application _Euler_ADERDG_:

    git clone https://github.com/annereinarz/ExaHyPE-Workshop-Engine
    cd ExaHyPE-Workshop-Engine
    ./Submodules/updateSubmodules.sh
    ./Toolkit/toolkit.sh Demonstrators/EulerADERDG/EulerADERDG.exahype

Now you are ready to compile and run an ExaHyPE application [according to the guidebook](http://www5.in.tum.de/exahype/guidebook.pdf):

    cd Demonstrators/EulerADERDG
    make
    ./ExaHyPE-Euler EulerADERDG.exahype

After the simulation finished you will find some files with the suffix .vtk in The folder Demonstrators/Euler_ADERDG, which you can open in Paraview.

## Requirements ##
If above doesn't work you might want to try these fixes

If you don't have a g++ compiler install it via apt-get
```sudo apt-get install g++```

You might need to install [Java 1.8](https://www.digitalocean.com/community/tutorials/how-to-install-java-with-apt-get-on-ubuntu-16-04) jre and jdk

To run programms on multiple threads it is **necessary** that you have Intel TBB installed. You can find the most recent version at [TBB](https://github.com/01org/tbb/releases) or run the [installer script](https://gitlab.lrz.de/Ferienakademie18/ExaHyPE-Teaching/blob/master/installTBB.sh).

The installer script will return you some output similar to this (The paths might differ on your machine)
``` bash
add this to you .bashrc (/home/user/.bashrc):
export TBB_INC="-I /home/user/ExaHyPE-Teaching/tbb/include"
export TBB_SHLIB="-L /home/user/ExaHyPE-Teaching/tbb/build/linux_intel64_gcc_cc5.4.0_libc2.23_kernel4.15.0_release -ltbb"
and hit
source /home/user/.bashrc
```
Follow the instructions and add the lines 
``` bash
export TBB_INC="-I /home/user/ExaHyPE-Teaching/tbb/include"
export TBB_SHLIB="-L /home/user/ExaHyPE-Teaching/tbb/build/linux_intel64_gcc_cc5.4.0_libc2.23_kernel4.15.0_release -ltbb"
```
to ```/home/user/.bashrc``` (If the file doesn't exists create it)
after that open a new terminal or apply the .bashrc to your current shell:
```source /home/user/.bashrc```

To be able to look at the generated _.vtu_ output it is **necessary** to have [Paraview](https://www.paraview.org/download) installed.

## Support ##
We only support default Linux systems like Ubuntu and Mint.

With a few adjustions you can get ExaHyPE running on a Mac: Before compilation hit:
```bash
export SHAREDMEM=None
```
and in the file ```./ExaHyPE/Makefile``` comment out all lines containing a ```-lrt```

There currently is no support for Windows, please use a virtual machine like [VirtualBox](https://www.virtualbox.org/)


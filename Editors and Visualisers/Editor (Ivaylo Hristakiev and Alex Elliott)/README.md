# GP Editor

## Introduction

Authors: Ivaylo Hristakiev and Alex Elliott (19 Jan 2015)

This is the working repository for the GP2 Editor, the integrated development environment being constructed for <a href="https://www.cs.york.ac.uk/plasma/wiki/index.php?title=GP_(Graph_Programs)">GP2</a>, an experimental graph programming language developed by the University of York.

## Compiling and Installing on Linux

The project uses CMake to produce build files across the supported platforms. The CMake build system currently requires:

- CMake itself
- Qt version 4.x (distributed with the Editor)
- The OGDF library (distributed with the Editor)
- The Boost C++ libraries (distributed with the Editor)

The executable relies on the <a href="https://github.com/UoYCS-plasma/GP2/tree/master/Compiler">GP2 Compiler</a> being present.

#### Building the GP2 Compiler

Start by obtaining the latest version of the Compiler
```
$ mkdir GP2
$ cd GP2
$ mkdir GP2-Compiler
$ cd GP2-Compiler
$ git clone https://github.com/UoYCS-plasma/GP2.git
```

Follow the install instructions from <a href="https://github.com/UoYCS-plasma/GP2/tree/master/Compiler">here</a>

#### Building OGDF and Boost
The required version can be downloaded from <a href="https://www.dropbox.com/s/pzgm1ge843pbgoz/ogdf.v2012.07.zip?dl=0">here</a>
```
$ cd GP2
$ mkdir OGDF-build
$ unzip -a ogdf.v2012.07.zip
$ cd OGDF-source
$ cmake -DCMAKE_INSTALL_PREFIX=../OGDF-build
$ make
$ make install
```
Next is Boost. http://sourceforge.net/projects/boost/files/boost/1.55.0/
All you need to do is download the zip and extract it in the local root directory. No build required.

#### Building Qt4

Download **Qt 4.8** from  http://download.qt.io/archive/qt/4.8/4.8.5/ and extract it in the local GP2 directory
```
$ cd GP2
$ mkdir qt4
$ unzip -a qt-everywhere-opensource-src-4.8.5.zip
$ cd qt-everywhere-opensource-src-4.8.5
$ ./configure -prefix ../qt4/
$ make/gmake (the configure script will tell you which command to run)
$ make/gmake install
```

This step takes about an hour.

#### Building the Editor

First, obtain the code from https://github.com/UoYCS-plasma/GP2-editor.git

```
$ cd GP2
$ git clone https://github.com/UoYCS-plasma/GP2-editor.git
```

Next, edit the **CMakeLists.txt** and edit occurrences marked with _Change this_ .  Also note the comment about exporting CMAKE_PREFIX_PATH. This ensures CMake searches in the right places for the libraries (otherwise it will search /usr/local/... and find nothing).

```
$ cd GP2
$ mkdir GP2-build
$ cd GP2-build
$ cmake ../GP2-editor
$ make
```

If everything went to plan, GP2 will install nicely and you can run the executable 'gpdeveloper' to open the editor!

## Updating the Editor

```
cd GP2/GP2-editor
cp CMakeLists.txt ..
git checkout -- CMakeLists.txt
git pull
cp ../CMakeLists.txt .
```

Note: It is always useful to check how much is the CMakeLists.txt different by using
```
git diff CMakeLists.txt
```
If anything else than location of libraries is different, getting the latest CMakeLists.txt and reediting it is best.
``
git reset CMakeLists.txt
``

## Rebuilding the Editor

```
cd GP2/GP2-build 
cmake ../GP2-editor
make
./gpdeveloper &
```

## Updating the Compiler

```
cd GP2/GP2-Compiler
git pull
cd src
make clean
make build
```


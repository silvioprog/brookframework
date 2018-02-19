# Development of the new Brook framework.

* Version number: 4.0
* Codename: Tardigrade
* Core (under [libbrook](https://github.com/risoflora/libbrook/tree/new_api)):
    * static objects - direct objects or library linking, no external library required
    * ~~shared - dynamic loading, single external library file required, `libbrook.so` or `brook.dll`~~ (no more required! \o/)

# Supported platforms:

Tested on:

* Windows 32 and 64 bit (Windows 7 64 bit)
* Linux 32 and 64 bit (Xubuntu 16.04 64 bit)
* ARM 32 bit (ARMv5; installed Android 5 / ARMv7; installed Android 6)

(future testings: Orange PI i96 256 MB Cortex-A5 32bit; minimal Linux core)

# Suported compilers

Successfully compiled for Windows 32/64 bit and Linux 64 bit using:
* Delphi XE family (Delphi Tokyo)
* Lazarus / Free Pascal (Lazarus 1.8.0 / FPC 3.0.4)

(future testings: Delphi and FPC building for ARM)

# Dependence linking

* Static ~~and shared~~ 32/64 objects/libraries generated in:
    * MSVC from Visual Studio 2017 version 15.5
    * MinGW-w64 from MSYS

(future testings: Delphi C++ compiler)

~~Planning library distribution:~~

* ~~APT (Debian based systems)~~
* ~~Pacman (MinGW)~~
* ~~Nuget (Visual Studio)~~

# Distribution / installation

* Github branch

Planning package distribution/installation:
 
* Github releasing
* GetIt (Delphi)
* OPM (Lazarus)

# Current available

Units:

* ~~`BrookUtils` - useful routines and structures.~~ (Done!)
* `BrookString` - low-level string handling. (In progress...)

Classes:

* `TBrookString` - used to represent a HTML body, POST payload and more. (In progress...)

Packages:

* `BrookFramework.dpk` - Delphi package. (In progress...)
* `BrookFramework.lpk` - Lazarus package. (In progress...)

Examples:

* `BrookUtils_Example` - utility functions example. (In progress...)
* `BrookString_Example` - string handling example. (In progress...)

Testings:

* `TestUtils` - utility testings. (In progress...)
* `TestString` - string handle testings. (In progress...)

Documentation:

* `BrookString` - string handling reference [HTML/PDF]. (In progress...)

Bugs & Problems:

* Free Pascal common missing routines/types - all issued at its bugtracker.
* Delphi unit encoding - unwanted UTF8 BOM when saving unit files.
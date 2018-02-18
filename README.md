# Development of the new Brook framework.

This new version will be called Tardigrade and made under [libbrook](https://github.com/risoflora/libbrook/tree/new_api) library.

# Supported platforms:

Tested on:

* Windows 32 and 64 bit (Windows 7 64 bit)
* Linux 32 and 64 bit (Xubuntu 16.04 64 bit)
* ARM 32 bit (ARMv5; installed Android 5 / ARMv7; installed Android 6)

(future testings: Orange PI i96 256 MB Cortex-A5 32bit; minimal Core Linux)

# Suported compilers

Successfully tested for Windows 32/64 bit and Linux 64 bit using:
* Delphi XE family (Delphi Tokyo)
* Lazarus / Free Pascal (Lazarus 1.8.0 / FPC 3.0.4)

(future testings: Delphi and FPC building for ARM)

# Dependence linking

* Static and shared 32/64 libraries generated in:
    * MSVC from Visual Studio 2017 version 15.5
    * MinGW-w64 from MSYS

(future testings: Delphi C++ compiler)

Planning library distribution:

* APT (Debian based systems)
* Pacman (MinGW)
* Nuget (Visual Studio)

# Distribution / installation

* Github branch

Planning package distribution/installation:
 
* Github releases
* GetIt (Delphi)
* OPM (Lazarus)

# Current available

Units:

* `BrookUtils` - utility routines and structures.
* `BrookString` - low-level string handling.

Classes:

* `TBrookString` - used to represent a HTML body, POST payload and more.

Packages:

* `BrookFramework.dpk` - Delphi package.
* `BrookFramework.lpk` - Lazarus package.

Testings:

* `TestUtils` - utility testings.
* `TestString` - string handle testings.

* Documentation:

* `BrookString` - string handling reference [HTML/PDF].

Bugs/problems:

* Free Pascal common missing routines/types - all issued at its bugtracker.
* Delphi unit encoding - unwanted UTF8 BOM when saving unit files.
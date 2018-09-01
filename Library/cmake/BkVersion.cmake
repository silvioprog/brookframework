#.rst:
# BkVersion
# ---------
#
# Versioning variables.
#
# Variables containing the Brook library version.
#
# ::
#
#   BK_VERSION_MAJOR - Major version.
#   BK_VERSION_MINOR - Minor version.
#   BK_VERSION_PATCH - Patch version.
#   SOVERSION - Composed by "BK_VERSION_MAJOR".
#   VERSION - Composed by "BK_VERSION_MAJOR.BK_VERSION_MINOR.BK_VERSION_PATCH".
#   VERSION_SUFFIX - Composed by "-BK_VERSION_MAJOR" (Windows only).

#   _                     _
#  | |__  _ __ ___   ___ | | __
#  | '_ \| '__/ _ \ / _ \| |/ /
#  | |_) | | | (_) | (_) |   <
#  |_.__/|_|  \___/ \___/|_|\_\
#
#   –– wrapper library to integrate Sagui and Cordel libraries to Brook framework.
#
# Copyright (c) 2018 Silvio Clecio <silvioprog@gmail.com>
#
# This file is part of Brook library.
#
# Brook library is free software: you can redistribute it and/or modify
# it under the terms of the GNU Lesser General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# Brook library is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU Lesser General Public License for more details.
#
# You should have received a copy of the GNU Lesser General Public License
# along with Brook library.  If not, see <http://www.gnu.org/licenses/>.
#

if (__BK_VERSION_INCLUDED)
    return()
endif ()
set(__BK_VERSION_INCLUDED ON)

if (EXISTS ${BK_INCLUDE_DIR}/brook.h)
    set(_version_list MAJOR MINOR PATCH)
    foreach (v ${_version_list})
        set(_regex_version "^#define BK_VERSION_${v} ([0-9])")
        file(STRINGS "${BK_INCLUDE_DIR}/brook.h" BK_VERSION_${v} REGEX "${_regex_version}")
        string(REGEX REPLACE "${_regex_version}" "\\1" BK_VERSION_${v} "${BK_VERSION_${v}}")
        unset(_regex_version)
    endforeach ()
    set(SOVERSION ${BK_VERSION_MAJOR})
    set(VERSION ${BK_VERSION_MAJOR}.${BK_VERSION_MINOR}.${BK_VERSION_PATCH})
    if (WIN32)
        set(VERSION_SUFFIX -${BK_VERSION_MAJOR})
    endif ()
    unset(_version_list)
endif ()
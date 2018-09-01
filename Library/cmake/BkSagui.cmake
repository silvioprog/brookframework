#.rst:
# BkSagui
# -------
#
# Build Sagui library.
#
# Build Sagui library from Brook library building.
#
# ::
#
#   SAGUI_INCLUDE_DIR - Directory of includes.
#   SAGUI_ARCHIVE_LIB - AR archive library.
#

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

if (__BK_SAGUI_INCLUDED)
    return()
endif ()
set(__BK_SAGUI_INCLUDED ON)

set(SAGUI_NAME "sagui")
set(SAGUI_VER "1.0.3")
set(SAGUI_FULL_NAME "${SAGUI_NAME}-${SAGUI_VER}")
set(SAGUI_URL "https://github.com/risoflora/libsagui/archive/v${SAGUI_VER}.tar.gz")
set(SAGUI_SHA256 "930c2879650dda26fd149c63415bb69dbf63d8ed058b9907e963311a9d922ac3")
if (CMAKE_C_COMPILER)
    set(SAGUI_OPTIONS -DCMAKE_C_COMPILER=${CMAKE_C_COMPILER})
endif ()
if (CMAKE_RC_COMPILER)
    set(SAGUI_OPTIONS ${SAGUI_OPTIONS} -DCMAKE_RC_COMPILER=${CMAKE_RC_COMPILER})
endif ()
if (CMAKE_SYSTEM_NAME)
    set(SAGUI_OPTIONS ${SAGUI_OPTIONS} -DCMAKE_SYSTEM_NAME=${CMAKE_SYSTEM_NAME})
endif ()
if (UNIX)
    set(SAGUI_OPTIONS ${SAGUI_OPTIONS} -DCMAKE_POSITION_INDEPENDENT_CODE=ON)
endif ()
if (ANDROID)
    set(SAGUI_OPTIONS ${SAGUI_OPTIONS}
            -DCMAKE_ANDROID_ARM_MODE=${CMAKE_ANDROID_ARM_MODE}
            -DCMAKE_SYSTEM_VERSION=${CMAKE_SYSTEM_VERSION}
            -DCMAKE_ANDROID_ARCH_ABI=${CMAKE_ANDROID_ARCH_ABI}
            -DCMAKE_ANDROID_STANDALONE_TOOLCHAIN=${CMAKE_ANDROID_STANDALONE_TOOLCHAIN})
endif ()
set(SAGUI_OPTIONS ${SAGUI_OPTIONS}
        -DCMAKE_BUILD_TYPE=${CMAKE_BUILD_TYPE}
        -DCMAKE_INSTALL_PREFIX=${CMAKE_BINARY_DIR}/${SAGUI_FULL_NAME}
        -DBUILD_TESTING=OFF
        -DSG_HTTPS_SUPPORT=${BK_HTTPS_SUPPORT}
        -DSG_BUILD_EXAMPLES=OFF)

ExternalProject_Add(${SAGUI_FULL_NAME}
        URL ${SAGUI_URL}
        URL_HASH SHA256=${SAGUI_SHA256}
        TIMEOUT 15
        DOWNLOAD_DIR ${CMAKE_SOURCE_DIR}/lib
        DOWNLOAD_NAME ${SAGUI_NAME}-${SAGUI_VER}.tar.gz
        PREFIX ${CMAKE_BINARY_DIR}/${SAGUI_FULL_NAME}
        SOURCE_DIR ${CMAKE_SOURCE_DIR}/lib/${SAGUI_FULL_NAME}
        CMAKE_ARGS ${SAGUI_OPTIONS}
        LOG_DOWNLOAD ON
        LOG_CONFIGURE ON
        LOG_BUILD ON
        LOG_INSTALL ON)

ExternalProject_Get_Property(${SAGUI_FULL_NAME} INSTALL_DIR)
set(SAGUI_INCLUDE_DIR ${INSTALL_DIR}/include)
set(SAGUI_ARCHIVE_LIB ${INSTALL_DIR}/lib/lib${SAGUI_NAME}.a)
unset(INSTALL_DIR)
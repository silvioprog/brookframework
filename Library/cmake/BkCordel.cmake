#.rst:
# BkCordel
# --------
#
# Build Cordel library.
#
# Build Cordel library from Brook library building.
#
# ::
#
#   CORDEL_INCLUDE_DIR - Directory of includes.
#   CORDEL_ARCHIVE_LIB - AR archive library.
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

if (__BK_CORDEL_INCLUDED)
    return()
endif ()
set(__BK_CORDEL_INCLUDED ON)

set(CORDEL_NAME "cordel")
set(CORDEL_VER "1.0.0")
set(CORDEL_FULL_NAME "${CORDEL_NAME}-${CORDEL_VER}")
#set(CORDEL_URL "https://github.com/risoflora/libcordel/archive/v${CORDEL_VER}.tar.gz")
set(CORDEL_URL "http://177.22.249.86:9090/libcordel-1.0.0.tar.gz")
set(CORDEL_SHA256 "636b4d24a3e5705fd0d9c80b5b3e6938e39d4f64e5c3fe483aada6e8030f4282")
if (CMAKE_C_COMPILER)
    set(CORDEL_OPTIONS -DCMAKE_C_COMPILER=${CMAKE_C_COMPILER})
endif ()
if (CMAKE_RC_COMPILER)
    set(CORDEL_OPTIONS ${CORDEL_OPTIONS} -DCMAKE_RC_COMPILER=${CMAKE_RC_COMPILER})
endif ()
if (CMAKE_SYSTEM_NAME)
    set(CORDEL_OPTIONS ${CORDEL_OPTIONS} -DCMAKE_SYSTEM_NAME=${CMAKE_SYSTEM_NAME})
endif ()
if (UNIX)
    set(CORDEL_OPTIONS ${CORDEL_OPTIONS} -DCMAKE_POSITION_INDEPENDENT_CODE=ON)
endif ()
if (ANDROID)
    set(CORDEL_OPTIONS ${CORDEL_OPTIONS}
            -DCMAKE_ANDROID_ARM_MODE=${CMAKE_ANDROID_ARM_MODE}
            -DCMAKE_SYSTEM_VERSION=${CMAKE_SYSTEM_VERSION}
            -DCMAKE_ANDROID_ARCH_ABI=${CMAKE_ANDROID_ARCH_ABI}
            -DCMAKE_ANDROID_STANDALONE_TOOLCHAIN=${CMAKE_ANDROID_STANDALONE_TOOLCHAIN})
endif ()
set(CORDEL_OPTIONS ${CORDEL_OPTIONS}
        -DCMAKE_BUILD_TYPE=${CMAKE_BUILD_TYPE}
        -DCMAKE_INSTALL_PREFIX=${CMAKE_BINARY_DIR}/${CORDEL_FULL_NAME}
        -DBUILD_TESTING=OFF
        -DCL_BUILD_EXAMPLES=OFF)

ExternalProject_Add(${CORDEL_FULL_NAME}
        URL ${CORDEL_URL}
        URL_HASH SHA256=${CORDEL_SHA256}
        TIMEOUT 15
        DOWNLOAD_DIR ${CMAKE_SOURCE_DIR}/lib
        DOWNLOAD_NAME ${CORDEL_NAME}-${CORDEL_VER}.tar.gz
        PREFIX ${CMAKE_BINARY_DIR}/${CORDEL_FULL_NAME}
        SOURCE_DIR ${CMAKE_SOURCE_DIR}/lib/${CORDEL_FULL_NAME}
        CMAKE_ARGS ${CORDEL_OPTIONS}
        LOG_DOWNLOAD ON
        LOG_CONFIGURE ON
        LOG_BUILD ON
        LOG_INSTALL ON)

ExternalProject_Get_Property(${CORDEL_FULL_NAME} INSTALL_DIR)
set(CORDEL_INCLUDE_DIR ${INSTALL_DIR}/include)
set(CORDEL_ARCHIVE_LIB ${INSTALL_DIR}/lib/lib${CORDEL_NAME}.a)
unset(INSTALL_DIR)
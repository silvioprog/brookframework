/*
 *   _                     _
 *  | |__  _ __ ___   ___ | | __
 *  | '_ \| '__/ _ \ / _ \| |/ /
 *  | |_) | | | (_) | (_) |   <
 *  |_.__/|_|  \___/ \___/|_|\_\
 *
 *   –– wrapper library to integrate Sagui and Cordel libraries to Brook framework.
 *
 * Copyright (c) 2018 Silvio Clecio <silvioprog@gmail.com>
 *
 * This file is part of Brook library.
 *
 * Brook library is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * Brook library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with Brook library.  If not, see <http://www.gnu.org/licenses/>.
 */

/**
 * \mainpage
 * \li \ref bk_api
 */

/** \defgroup bk_api API reference
 * The API reference grouped by feature.
 * */

#ifndef BROOK_H
#define BROOK_H

#ifdef __cplusplus
extern "C" {
#endif

#ifndef BK_EXTERN
# ifdef _WIN32
#  ifdef BUILDING_LIBBROOK
#   define BK_EXTERN __declspec(dllexport) extern
#  else
#   define BK_EXTERN __declspec(dllimport) extern
#  endif
# else
#  define BK_EXTERN extern
# endif
#endif

#define BK_VERSION_MAJOR 1
#define BK_VERSION_MINOR 0
#define BK_VERSION_PATCH 0
#define BK_VERSION_HEX ((BK_VERSION_MAJOR << 16) | (BK_VERSION_MINOR <<  8) | (BK_VERSION_PATCH))

/**
* \ingroup bk_api
* \defgroup bk_utils Utilities
* All utility functions of the library.
* \{
*/

/**
 * Returns the library version number.
 * \return Library version packed into a single integer.
 */
BK_EXTERN unsigned int bk_version(void);

/**
 * Returns the library version number as string.
 * \return Library version packed into a null-terminated string.
 */
BK_EXTERN const char *bk_version_str(void);

/** \} */

#ifdef __cplusplus
}
#endif

#endif /* BROOK_H */

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

#include "bk_macros.h"
#include "brook.h"

#define BK_VERSION_STR xstr(SG_VERSION_MAJOR) "." xstr(SG_VERSION_MINOR) "." xstr(SG_VERSION_PATCH)

/* Version. */

unsigned int bk_version(void) {
    return BK_VERSION_HEX;
}

const char *bk_version_str(void) {
    return BK_VERSION_STR;
}

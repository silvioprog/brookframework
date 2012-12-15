(*
  Brook DB Consts unit.

  Copyright (C) 2013 Silvio Clecio.

  http://brookframework.org

  All contributors:
  Plase see the file CONTRIBUTORS.txt, included in this
  distribution.

  See the file LICENSE.txt, included in this distribution,
  for details about the copyright.

  This library is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
*)

unit BrookDBConsts;

{$i brook.inc}

interface

uses
  BrookConsts;

const
  BROOK_FT_NULL = 'null';
  BROOK_FT_STRING = 'string';
  BROOK_FT_BOOLEAN = 'boolean';
  BROOK_FT_DATE = 'date';
  BROOK_FT_FLOAT = 'float';
  BROOK_FT_INT = 'int';
  BROOK_SQL_SELECT_TOKEN = 'select';
  BROOK_SQL_FROM_TOKEN = 'from';
  BROOK_SQL_WHERE_TOKEN = 'where';
  BROOK_SQL_NOTHING_WHERE_TOKEN = BROOK_SQL_WHERE_TOKEN + SP + '1=2';
  BROOK_SQL_ORDER_BY_TOKEN = 'order by';
  BROOK_SQL_INSERT_TOKEN = 'insert into';
  BROOK_SQL_VALUES_TOKEN = 'values';
  BROOK_SQL_UPDATE_TOKEN = 'update';
  BROOK_SQL_SET_TOKEN = 'set';
  BROOK_SQL_DELETE_TOKEN = 'delete';
  BROOK_SQL_EQ_PARAM_TOKEN = ' = :';
  BROOK_SQL_LOWER_TOKEN = 'lower';
  BROOK_SQL_LIKE_TOKEN = 'like';
  BROOK_SQL_AND_TOKEN = 'and';

var
  BROOK_DEFAULT_LIBRARY_PARAM: string = 'library';
  BROOK_DEFAULT_KEY_NAME: string = 'id';

implementation

end.

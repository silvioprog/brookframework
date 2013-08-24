(*
  Brook Classes unit.

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

unit BrookClasses;

{$i brook.inc}

interface

uses
  BrookConsts;

type
  { Is the main interface for Brook. }
  IBrookInterface = interface(IInterface)[BROOK_GUID]
  end;

  { Is the main object for Brook. }
  TBrookObject = class(TObject)
  end;

  { Is the main class for Brook. }
  TBrookClass = class of TBrookObject;

  { Is the main interfaced object for Brook. }
  TBrookInterfacedObject = class(TInterfacedObject)
  end;

implementation

end.

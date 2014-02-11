(*
  Brook Http Defs unit.

  Copyright (C) 2013 Silvio Clecio.

  http://silvioprog.github.io/brookframework

  All contributors:
  Plase see the file CONTRIBUTORS.txt, included in this
  distribution.

  See the file LICENSE.txt, included in this distribution,
  for details about the copyright.

  This library is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
*)

unit BrookHttpDefs;

{$i brook.inc}

interface

uses
  HTTPDefs;

type
  { Alias to @code(TRequest). }
  TBrookRequest = TRequest;

  { Alias to @code(TResponse). }
  TBrookResponse = TResponse;

  { Alias to @code(TUploadedFile). }
  TBrookUploadedFile = TUploadedFile;

  { Alias to @code(TUploadedFiles). }
  TBrookUploadedFiles = TUploadedFiles;

implementation

end.


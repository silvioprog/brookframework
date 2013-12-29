(*
  Brook DB Utils unit.

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

unit BrookDBUtils;

{$i brook.inc}

interface

uses
  BrookConsts, DB, SysUtils, StrUtils;

type
  { Set of data types for @code(BrookDataSetToHTMLTable) and
    @code(BrookDataSetToHTML5Table). }
  TBrookUnavailableFields = set of TFieldType;

  { Informs the writing stage of the table. }
  TBrookHTMLTableWritingState = (wtHeadTR, wtHeadTD, wtBodyTR, wtBodyTD);

  { Callback for @code(BrookDataSetToHTMLTable). }
  TBrookDataSetToHTMLTableCallback = function(ADataSet: TDataSet;
    const AWritingType: TBrookHTMLTableWritingState;
    const APosition, AMax: Integer; var AData: string): string of object;

  { Callback for @code(BrookDataSetToHTML5Table). }
  TBrookDataSetToHTML5TableCallback = TBrookDataSetToHTMLTableCallback;

{ Converts dataset into HTML table. }
function BrookDataSetToHTMLTable(
  ADataSet: TDataSet; const ATitles: array of string;
  const AClass: string = ES; const ABorderWidth: Integer = 1;
  const AUnavailableFields: TBrookUnavailableFields = [];
  ACallback: TBrookDataSetToHTML5TableCallback = nil): string;
{ Converts dataset into HTML-5 table. }
function BrookDataSetToHTML5Table(
  ADataSet: TDataSet; const ATitles: array of string;
  const ACaption: string = ES; const AFoot: string = ES;
  const AClass: string = ES; const ABorderWidth: Integer = 1;
  const AUnavailableFields: TBrookUnavailableFields = [];
  ACallback: TBrookDataSetToHTML5TableCallback = nil): string;
{ Converts FieldDefs into HTML form. }
function BrookFieldDefsToHTMLForm(
  AFieldDefs: TFieldDefs; const AAction: string = '#'; const AMethod: string = 'post';
  const ALineBreak: Boolean = False; const AClass: string = ES;
  const ALabelAbove: Boolean = False): string;

implementation

function BrookDataSetToHTMLTable(
  ADataSet: TDataSet; const ATitles: array of string;
  const AClass: string; const ABorderWidth: Integer;
  const AUnavailableFields: TBrookUnavailableFields;
  ACallback: TBrookDataSetToHTML5TableCallback): string;
var
  VField: TField;
  VClass: string = ES;
  I, L, C, RC: Integer;
  VCallbackData: string;
begin
  if AClass = ES then
    VClass := SP
  else
    VClass := ' class="'+AClass+'" ';
  Result := '<table'+VClass+'border="'+
    IntToStr(ABorderWidth)+'" >'+LF+HT;
  VCallbackData := '<tr>';
  RC := ADataSet.RecordCount;
  if Assigned(ACallback) then
    VCallbackData := ACallback(ADataSet, wtHeadTR, 0, RC,
      VCallbackData);
  Result += VCallbackData+LF;
  L := High(ATitles);
  C := Pred(ADataSet.Fields.Count);
  if L = -1 then
    for I := 0 to C do
    begin
      VField := ADataSet.Fields[I];
      if VField.DataType in AUnavailableFields then
        Continue;
      VCallbackData := '<td>'+VField.DisplayLabel+'</td>';
      if Assigned(ACallback) then
        VCallbackData := ACallback(ADataSet, wtHeadTD, I, C, VCallbackData);
      Result += HT+HT+VCallbackData+LF;
    end
  else
    for I := 0 to L do
    begin
      VCallbackData := '<td>'+ATitles[I]+'</td>';
      if Assigned(ACallback) then
        VCallbackData := ACallback(ADataSet, wtHeadTD, I, L, VCallbackData);
      Result += HT+HT+VCallbackData+LF;
    end;
  Result += HT+'</tr>'+LF;
  ADataSet.First;
  while not ADataSet.EOF do
  begin
    VCallbackData := '<tr>';
    if Assigned(ACallback) then
      VCallbackData := ACallback(ADataSet, wtBodyTR, ADataSet.RecNo, RC,
        VCallbackData);
    Result += HT+VCallbackData+LF;
    for I := 0 to C do
    begin
      VField := ADataSet.Fields[I];
      if VField.DataType in AUnavailableFields then
        Continue;
      VCallbackData := '<td>'+VField.AsString+'</td>';
      if Assigned(ACallback) then
        VCallbackData := ACallback(ADataSet, wtBodyTD, I, C, VCallbackData);
      Result += HT+HT+VCallbackData+LF;
    end;
    Result += HT+'</tr>'+LF;
    ADataSet.Next;
  end;
  Result += '</table>';
end;

function BrookDataSetToHTML5Table(
  ADataSet: TDataSet; const ATitles: array of string;
  const ACaption: string; const AFoot: string; const AClass: string;
  const ABorderWidth: Integer;
  const AUnavailableFields: TBrookUnavailableFields;
  ACallback: TBrookDataSetToHTML5TableCallback): string;
var
  VField: TField;
  VFoot: string = ES;
  VClass: string = ES;
  I, L, C, RC: Integer;
  VCaption: string = ES;
  VCallbackData: string;
begin
  if ACaption <> ES then
    VCaption := '<caption>' + ACaption + '</caption>' + LF;
  if AClass = ES then
    VClass := ' '
  else
    VClass := ' class="'+AClass+'" ';
  Result := '<table'+VClass+'border="'+
    IntToStr(ABorderWidth)+'" >'+LF+VCaption+HT+'<thead>'+LF+HT+HT;
  VCallbackData := '<tr>';
  RC := ADataSet.RecordCount;
  if Assigned(ACallback) then
    VCallbackData := ACallback(ADataSet, wtHeadTR, 0, RC, VCallbackData);
  Result += VCallbackData+LF;
  L := High(ATitles);
  C := Pred(ADataSet.Fields.Count);
  if L = -1 then
    for I := 0 to C do
    begin
      VField := ADataSet.Fields[I];
      if VField.DataType in AUnavailableFields then
        Continue;
      VCallbackData := '<td>'+VField.DisplayLabel+'</td>';
      if Assigned(ACallback) then
        VCallbackData := ACallback(ADataSet, wtHeadTD, I, C, VCallbackData);
      Result += HT+HT+HT+HT+VCallbackData+LF;
    end
  else
    for I := 0 to L do
    begin
      VCallbackData := '<td>'+ATitles[I]+'</td>';
      if Assigned(ACallback) then
        VCallbackData := ACallback(ADataSet, wtHeadTD, I, L, VCallbackData);
      Result += VCallbackData+LF;
    end;
  Result += HT+HT+'</tr>'+LF+HT+'</thead>'+LF+HT+'<tbody>'+LF;
  ADataSet.First;
  while not ADataSet.EOF do
  begin
    VCallbackData := '<tr>';
    if Assigned(ACallback) then
      VCallbackData := ACallback(ADataSet, wtBodyTR, ADataSet.RecNo, RC,
        VCallbackData);
    Result += HT+HT+VCallbackData+LF;
    for I := 0 to C do
    begin
      VField := ADataSet.Fields[I];
      if VField.DataType in AUnavailableFields then
        Continue;
      VCallbackData := '<td>'+VField.AsString+'</td>';
      if Assigned(ACallback) then
        VCallbackData := ACallback(ADataSet, wtBodyTD, I, C, VCallbackData);
      Result += HT+HT+HT+HT+VCallbackData+LF;
    end;
    Result += HT+HT+'</tr>'+LF;
    ADataSet.Next;
  end;
  if AFoot = ES then
    VFoot := LF
  else
    VFoot := LF+AFoot+LF;
  Result += HT+'</tbody>'+VFoot+'</table>';
end;

function BrookFieldDefsToHTMLForm(
  AFieldDefs: TFieldDefs; const AAction: string; const AMethod: string;
  const ALineBreak: Boolean; const AClass: string;
  const ALabelAbove: Boolean): string;
var
  I: Integer;
  VEls: string = ES;
  VField: TFieldDef;
  VCaption, VName, VRequired, VMaxLength, VLineBreak, VClass: string;
begin
  for I := 0 to Pred(AFieldDefs.Count) do
  begin
    VField := AFieldDefs[I];
    VCaption := UpperCase(Copy(VField.DisplayName, 1, 1)) +
      Copy(VField.DisplayName, 2, MaxInt);
    VName := LowerCase(VField.Name);
    VRequired := IfThen(VField.Required, ' required', ES);
    VMaxLength := IfThen(VField.Size > 0, ' maxlength="'+
      IntToStr(VField.Size)+'"', ES);
    VLineBreak := IfThen(ALineBreak, BR, ES);
    if ALabelAbove then
    begin
      if VField.FieldClass = TBooleanField then
        VEls +=
          '	<label for="'+VName+'">'+VCaption+'</label>'+LF+
          '	<input type="checkbox" name="'+VName+'" id="'+VName+'"'+
          VMaxLength+VRequired+' />'+VLineBreak+LF
      else
      if VField.FieldClass = TMemoField then
        VEls +=
          '	<label for="'+VName+'">'+VCaption+'</label>'+LF+
          '	<textarea name="'+VName+'" id="'+VName+
          '" cols="30" rows="10"'+VMaxLength+VRequired+' ></textarea>'+
          VLineBreak+LF
      else
        VEls +=
          '	<label for="'+VName+'">'+VCaption+'</label>'+LF+
          '	<input type="text" name="'+VName+'" id="'+VName+'"'+VMaxLength+
          VRequired+' />'+VLineBreak+LF;
    end
    else
    begin
      if VField.FieldClass = TBooleanField then
        VEls +=
          '	<label for="'+VName+'">'+LF+
          '		'+VCaption+' <input type="checkbox" name="'+VName+
          '" id="'+VName+'"'+VMaxLength+VRequired+' />'+LF+
          '	</label>'+VLineBreak+LF
      else
      if VField.FieldClass = TMemoField then
        VEls +=
          '	<label for="'+VName+'">'+LF+
          '		'+VCaption+' <textarea name="'+VName+'" id="'+VName+
          '" cols="30" rows="10"'+VMaxLength+VRequired+' ></textarea>'+LF+
          '	</label>'+VLineBreak+LF
      else
        VEls +=
          '	<label for="'+VName+'">'+LF+
          '		'+VCaption+' <input type="text" name="'+VName+
          '" id="'+VName+'"'+VMaxLength+VRequired+' />'+LF+
          '	</label>'+VLineBreak+LF;
    end;
  end;
  if AClass = ES then
    VClass := SP
  else
    VClass := ' class="'+AClass+'" ';
  Result :=
    '<form'+VClass+'action="'+AAction+'" method="'+AMethod+'" >'+LF+
      VEls+
    '	<input type="submit" class="btn" />'+LF+
    '</form>';
end;

end.

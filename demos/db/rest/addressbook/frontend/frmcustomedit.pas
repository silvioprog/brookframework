unit frmCustomEdit;

{$mode objfpc}{$H+}

interface

uses
  Forms, ExtCtrls, Buttons, SysUtils, Controls, Dialogs, Grids, LJGridUtils,
  FPJSON, HttpUtils;

type
  TfrCustomEdit = class(TForm)
    btSubmit: TBitBtn;
    btCancel: TBitBtn;
    pnClient: TPanel;
    pnBottom: TPanel;
  protected
    class function ProcessRequest(const AHttpResult: THttpResult): Boolean;
    class function FillPattern(const APattern: string;
      AParams: TJSONObject): string;
  public
    class procedure Refresh(AGrid: TCustomStringGrid; const AUrl: string);
    class procedure Refresh(AGrid: TCustomStringGrid; const APattern: string;
      const AArgs: TJSONObject);
    class function Add(const AUrl: string): Boolean;
    class function Add(const APattern: string;
      const AArgs: TJSONObject): Boolean;
    class function Edit(const APattern: string;
      const AArgs: TJSONObject): Boolean;
    class function Delete(const APattern, AMsg: string;
      const AArgs: TJSONObject): Boolean;
    class function Execute(var AData: TJSONObject): Boolean;
    class function Question(const ACaption, AMsg: string): Boolean;
  end;

implementation

{$R *.lfm}

uses
  Serializer;

class function TfrCustomEdit.ProcessRequest(
  const AHttpResult: THttpResult): Boolean;
begin
  Result := (AHttpResult.Code = 200) or (AHttpResult.Code = 201) or
    (AHttpResult.Code = 204) or (AHttpResult.Code = 404);
  if not Result then
    ShowMessageFmt('ERROR: Text: %s; code: %d.',
      [AHttpResult.Text, AHttpResult.Code]);
end;

class function TfrCustomEdit.FillPattern(const APattern: string;
  AParams: TJSONObject): string;
var
  I: Integer;
  VName, VValue: string;
begin
  Result := APattern;
  for I := 0 to Pred(AParams.Count) do
  begin
    VName := ':' + AParams.Names[I];
    VValue := AParams.Items[I].AsString;
    Result := StringReplace(Result, VName, VValue, [rfIgnoreCase]);
  end;
end;

class procedure TfrCustomEdit.Refresh(AGrid: TCustomStringGrid;
  const AUrl: string);
var
  VData: TJSONArray;
begin
  VData := TJSONArray.Create;
  try
    ClearGrid(AGrid);
    ProcessRequest(HttpRequest(AUrl, VData));
    LoadJSON(AGrid, VData, False, False);
  finally
    VData.Free;
  end;
end;

class procedure TfrCustomEdit.Refresh(AGrid: TCustomStringGrid;
  const APattern: string; const AArgs: TJSONObject);
var
  VData: TJSONArray;
begin
  VData := TJSONArray.Create;
  try
    ClearGrid(AGrid);
    if AArgs.Count = 0 then
      Exit;
    ProcessRequest(HttpRequest(TfrCustomEdit.FillPattern(
      APattern, AArgs), VData));
    LoadJSON(AGrid, VData, False, False);
  finally
    VData.Free;
  end;
end;

class function TfrCustomEdit.Add(const AUrl: string): Boolean;
var
  VData: TJSONObject = nil;
begin
  Result := Self.Execute(VData);
  if not Result then
    Exit;
  Result := ProcessRequest(HttpRequest(VData, AUrl));
  if not Result then
    Exit;
  FreeAndNil(VData);
end;

class function TfrCustomEdit.Add(const APattern: string;
  const AArgs: TJSONObject): Boolean;
var
  VData: TJSONObject = nil;
begin
  Result := Self.Execute(VData);
  if not Result then
    Exit;
  Result := ProcessRequest(HttpRequest(VData,
    TfrCustomEdit.FillPattern(APattern, AArgs)));
  if not Result then
    Exit;
  FreeAndNil(VData);
end;

class function TfrCustomEdit.Edit(const APattern: string;
  const AArgs: TJSONObject): Boolean;
var
  VRow: TJSONObject;
begin
  VRow := AArgs.Clone as TJSONObject;
  try
    Result := Self.Execute(VRow) and ProcessRequest(HttpRequest(VRow,
      TfrCustomEdit.FillPattern(APattern, AArgs), rmPut));
  finally
    VRow.Free;
  end;
end;

class function TfrCustomEdit.Delete(const APattern, AMsg: string;
  const AArgs: TJSONObject): Boolean;
begin
  Result := Self.Question('Deleting', AMsg) and
    ProcessRequest(HttpRequest(TfrCustomEdit.FillPattern(APattern, AArgs),
      '', nil, rmDelete));
end;

class function TfrCustomEdit.Execute(var AData: TJSONObject): Boolean;
var
  VForm: TCustomForm;
begin
  VForm := Self.Create(nil);
  try
    if Assigned(AData) then
      Unserialize(AData, VForm);
    Result := VForm.ShowModal = mrOK;
    if not Result then
      Exit;
    if not Assigned(AData) then
      AData := TJSONObject.Create;
    Serialize(VForm, AData);
  finally
    VForm.Free;
  end;
end;

class function TfrCustomEdit.Question(const ACaption, AMsg: string): Boolean;
begin
  Result := MessageDlg(ACaption, AMsg, mtConfirmation, mbYesNo, 0) = mrYes;
end;

end.


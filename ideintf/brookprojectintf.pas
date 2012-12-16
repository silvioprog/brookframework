(*
  Brook Project Intf unit.

  Copyright (C) 2012 Silvio Clecio.

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

unit BrookProjectIntf;

{$i brook.inc}

interface

uses
  frmBrookNewProject, frmBrookNewPlugin, frmBrookActEdit, frmBrookTable2HTMLForm,
  ProjectIntf, NewItemIntf, LazIDEIntf, Classes, SysUtils, Controls, ComCtrls,
  Forms, Dialogs;

type

  { TBrookSimpleProjectDescriptor }

  TBrookSimpleProjectDescriptor = class(TProjectDescriptor)
  public
    constructor Create; override;
    function CreateStartFiles({%H-}AProject: TLazProject): TModalResult; override;
    function InitProject(AProject: TLazProject): TModalResult; override;
    function GetLocalizedName: string; override;
    function GetLocalizedDescription: string; override;
  end;

  { TBrookProjectDescriptor }

  TBrookProjectDescriptor = class(TProjectDescriptor)
  public
    constructor Create; override;
    function CreateStartFiles({%H-}AProject: TLazProject): TModalResult; override;
    function InitProject(AProject: TLazProject): TModalResult; override;
    function DoInitDescriptor: TModalResult; override;
    function GetLocalizedName: string; override;
    function GetLocalizedDescription: string; override;
  end;

  { TBrookTable2HTMLFormDescriptor }

  TBrookTable2HTMLFormDescriptor = class(TProjectDescriptor)
  public
    constructor Create; override;
    function DoInitDescriptor: TModalResult; override;
    function GetLocalizedName: string; override;
    function GetLocalizedDescription: string; override;
  end;

  { TBrookFileDescPascalUnit }

  TBrookFileDescPascalUnit = class(TFileDescPascalUnit)
  private
    FQuiet: Boolean;
  end;

  { TBrookBrokersFileDescPascalUnit }

  TBrookBrokersFileDescPascalUnit = class(TBrookFileDescPascalUnit)
  private
    FAppType: Integer;
    FAppDefCharset: Integer;
    FFullPlg: Boolean;
  public
    constructor Create; override;
    function Init(var ANewFilename: string; ANewOwner: TObject;
       var ANewSource: string; AQuiet: Boolean): TModalResult; override;
    function CreateSource({%H-}const AFileName,{%H-}ASourceName,
      {%H-}AResourceName: string): string; override;
    function GetLocalizedName: string; override;
    function GetLocalizedDescription: string; override;
  end;

  { TBrookActionFileDescPascalUnit }

  TBrookActionFileDescPascalUnit = class(TBrookFileDescPascalUnit)
  private
    FActName: string;
    FActPattern: string;
    FActDefault: Boolean;
  public
    constructor Create; override;
    function Init(var ANewFilename: string; ANewOwner: TObject;
       var ANewSource: string;{%H-}AQuiet: Boolean): TModalResult; override;
    function CreateSource({%H-}const AFileName,{%H-}ASourceName,
      {%H-}AResourceName: string): string; override;
    function GetLocalizedName: string; override;
    function GetLocalizedDescription: string; override;
    property ActName: string read FActName write FActName;
    property ActPattern: string read FActPattern write FActPattern;
    property ActDefault: Boolean read FActDefault write FActDefault;
  end;

const
  le = LineEnding;

resourcestring
  SBrookIDEItemCategoryName = 'Brook framework';
  SBrookAppName = 'Full CGI/FastCGI Application';
  SBrookAppDesc = 'Create a full CGI or FastCGI application.';
  SBrookSimpleAppName = 'Simple CGI application';
  SBrookSimpleAppDesc = 'Create a simple CGI application.';
  SBrookTable2HTMLFormName = 'Table to HTML form';
  SBrookTable2HTMLFormDesc = 'Create a HTML form from a database table.';
  SBrookBrokersName = 'Brokers unit';
  SBrookBrokersDesc = 'Create a brokers unit.';
  SBrookActionName = 'Action unit';
  SBrookActionDesc = 'Create a action unit.';

procedure Register;
function BrookNewProjectDlg: TfrBrookNewProject;
function BrookGetExpertsConfigPath: string;
function BrookGetExpertsConfigFileName: string;

implementation

var
  _NewProjectDlg: TfrBrookNewProject;

const
  PAGE_404_TPL =
    '<html><head><title>Page not found</title><style>body{margin:0;'+
    'padding:30px;font:12px/1.5 Helvetica,Arial,Verdana,sans-serif;}h1{mar'+
    'gin:0;font-size:48px;font-weight:normal;line-height:48px;}strong{disp'+
    'lay:inline-block;width:65px;}</style></head><body><h1>404 - Page not '+
    'found</h1><br />Go to <a href="%s">home page</a> ...'+
    '</body></html>';
  PAGE_500_TPL =
    '<html><head><title>Internal server error</title><style>body{margin:0;'+
    'padding:30px;font:12px/1.5 Helvetica,Arial,Verdana,sans-serif;}h1{mar'+
    'gin:0;font-size:48px;font-weight:normal;line-height:48px;}strong{disp'+
    'lay:inline-block;width:65px;}</style></head><body><h1>500 - Internal '+
    'server error</h1><br />%s'+
    '</body></html>';

procedure Register;
begin
  RegisterNewItemCategory(TNewIDEItemCategory.Create(SBrookIDEItemCategoryName));
  RegisterProjectDescriptor(TBrookSimpleProjectDescriptor.Create,
    SBrookIDEItemCategoryName);
  RegisterProjectDescriptor(TBrookProjectDescriptor.Create,
    SBrookIDEItemCategoryName);
  RegisterProjectDescriptor(TBrookTable2HTMLFormDescriptor.Create,
    SBrookIDEItemCategoryName);
  RegisterProjectFileDescriptor(TBrookBrokersFileDescPascalUnit.Create,
    SBrookIDEItemCategoryName);
  RegisterProjectFileDescriptor(TBrookActionFileDescPascalUnit.Create,
    SBrookIDEItemCategoryName);
end;

function BrookNewProjectDlg: TfrBrookNewProject;
begin
  if not Assigned(_NewProjectDlg) then
    _NewProjectDlg := TfrBrookNewProject.Create(Application);
  Result := _NewProjectDlg;
end;

procedure BrookFreeNewProjectDlg;
begin
  FreeAndNil(_NewProjectDlg);
end;

function BrookGetExpertsConfigPath: string;
begin
  if Assigned(LazarusIDE) then
    Result := IncludeTrailingPathDelimiter(LazarusIDE.GetPrimaryConfigPath)
  else
    Result := '';
end;

function BrookGetExpertsConfigFileName: string;
begin
  Result := BrookGetExpertsConfigPath + 'brook.xml';
end;

{ TBrookSimpleProjectDescriptor }

constructor TBrookSimpleProjectDescriptor.Create;
begin
  inherited Create;
  Name := SBrookSimpleAppName;
end;

function TBrookSimpleProjectDescriptor.CreateStartFiles(
  AProject: TLazProject): TModalResult;
var
  VActItem: TBrookActionFileDescPascalUnit;
  VPlgItem: TBrookBrokersFileDescPascalUnit;
begin
  VActItem := ProjectFileDescriptors.FindByName(
    SBrookActionName) as TBrookActionFileDescPascalUnit;
  VActItem.FQuiet := True;
  VActItem.ActName := 'MyAction';
  VActItem.ActPattern := '*';
  VActItem.ActDefault := False;
  LazarusIDE.DoNewEditorFile(VActItem, '', '',
    [nfIsPartOfProject, nfOpenInEditor, nfCreateDefaultSrc]);
  VPlgItem := ProjectFileDescriptors.FindByName(
    SBrookBrokersName) as TBrookBrokersFileDescPascalUnit;
  VPlgItem.FQuiet := True;
  VPlgItem.FFullPlg := False;
  VPlgItem.FAppType := 0;
  VPlgItem.FAppDefCharset := 0;
  LazarusIDE.DoNewEditorFile(VPlgItem, '', '',
    [nfIsPartOfProject, nfOpenInEditor, nfCreateDefaultSrc]);
  Result := mrOK;
end;

function TBrookSimpleProjectDescriptor.InitProject(
  AProject: TLazProject): TModalResult;
var
  VProject: TLazProjectFile;
begin
  VProject := AProject.CreateProjectFile('cgi1.lpr');
  VProject.IsPartOfProject := True;
  AProject.AddFile(VProject, False);
  AProject.AddPackageDependency('BrookRT');
  AProject.Flags := AProject.Flags - [pfMainUnitHasCreateFormStatements, pfRunnable];
  AProject.LazCompilerOptions.Win32GraphicApp := False;
  AProject.LazCompilerOptions.TargetFileName := 'cgi1';
  AProject.LazCompilerOptions.TargetFilenameApplyConventions := False;
  AProject.SessionStorage := pssInProjectInfo;
  AProject.MainFileID := 0;
  AProject.MainFile.SetSourceText(
    'program cgi1;'+le+
    ''+le+
    '{$mode objfpc}{$H+}'+le+
    ''+le+
    'uses'+le+
    '  BrookApplication, Brokers;'+le+
    ''+le+
    'begin'+le+
    '  BrookApp.Run;'+le+
    'end.');
  AProject.Title := '';
  Result := mrOK;
end;

function TBrookSimpleProjectDescriptor.GetLocalizedName: string;
begin
  Result := SBrookSimpleAppName;
end;

function TBrookSimpleProjectDescriptor.GetLocalizedDescription: string;
begin
  Result := SBrookSimpleAppDesc;
end;

{ TBrookProjectDescriptor }

constructor TBrookProjectDescriptor.Create;
begin
  inherited Create;
  Name := SBrookAppName;
end;

function TBrookProjectDescriptor.CreateStartFiles(
  AProject: TLazProject): TModalResult;

  procedure CreateHTMLFile(const F, S: string);
  begin
    if FileExists(F) and (MessageDlg('Confirmation', Format('Replace file "%s"?',
      [F]), mtConfirmation, mbYesNo, 0) <> mrYes) then
      Exit;
    with TFileStream.Create(F, fmCreate) do
      try
        Write(Pointer(S)^, Length(S));
      finally
        Free;
      end;
  end;

var
  I: Integer;
  VItem: TListItem;
  VHTMLsDir: string;
  VDlg: TfrBrookNewProject;
  VPage404, VPage500: TFileName;
  VActItem: TBrookActionFileDescPascalUnit;
  VPlgItem: TBrookBrokersFileDescPascalUnit;
begin
  VDlg := BrookNewProjectDlg;
  VActItem := ProjectFileDescriptors.FindByName(
    SBrookActionName) as TBrookActionFileDescPascalUnit;
  for I := 0 to Pred(VDlg.lvActions.Items.Count) do
  begin
    VActItem.FQuiet := True;
    VItem := VDlg.lvActions.Items[I];
    VActItem.ActName := VItem.Caption;
    VActItem.ActPattern := VItem.SubItems[0];
    VActItem.ActDefault := StrToBool(VItem.SubItems[1]);
    LazarusIDE.DoNewEditorFile(VActItem, '', '',
      [nfIsPartOfProject, nfOpenInEditor, nfCreateDefaultSrc]);
  end;
  VPlgItem := ProjectFileDescriptors.FindByName(
    SBrookBrokersName) as TBrookBrokersFileDescPascalUnit;
  VPlgItem.FQuiet := True;
  VPlgItem.FFullPlg := True;
  VPlgItem.FAppType := VDlg.rgAppType.ItemIndex;
  VPlgItem.FAppDefCharset := VDlg.rgCharset.ItemIndex;
  LazarusIDE.DoNewEditorFile(VPlgItem, '', '',
    [nfIsPartOfProject, nfOpenInEditor, nfCreateDefaultSrc]);
  if VDlg.edPubHTMLDir.Text <> '' then
  begin
    VHTMLsDir := IncludeTrailingPathDelimiter(VDlg.edPubHTMLDir.Text);
    VPage404 := VHTMLsDir + '404.html';
    VPage500 := VHTMLsDir + '500.html';
    CreateHTMLFile(VPage404, PAGE_404_TPL);
    CreateHTMLFile(VPage500, PAGE_500_TPL);
    LazarusIDE.DoOpenEditorFile(VPage404, -1, -1,
      [ofProjectLoading, ofRegularFile, ofAddToProject]);
    LazarusIDE.DoOpenEditorFile(VPage500, -1, -1,
      [ofProjectLoading, ofRegularFile, ofAddToProject]);
  end;
  Result := LazarusIDE.DoOpenEditorFile(AProject.MainFile.FileName, -1, -1,
    [ofProjectLoading, ofRegularFile]);
  BrookFreeNewProjectDlg;
end;

function TBrookProjectDescriptor.DoInitDescriptor: TModalResult;
begin
  Result := BrookNewProjectDlg.ShowModal;
  if Result <> mrOK then
    BrookFreeNewProjectDlg;
end;

function TBrookProjectDescriptor.InitProject(
  AProject: TLazProject): TModalResult;
var
  S: string;
  VDlg: TfrBrookNewProject;
  VProject: TLazProjectFile;
begin
  VDlg := BrookNewProjectDlg;
  VProject := AProject.CreateProjectFile(VDlg.edAppName.Text + '.lpr');
  VProject.IsPartOfProject := True;
  AProject.AddFile(VProject, False);
  AProject.AddPackageDependency('BrookRT');
  case VDlg.rgAppType.ItemIndex of
    0: AProject.LazCompilerOptions.TargetFileName := VDlg.edAppName.Text;
    1: AProject.LazCompilerOptions.TargetFileName := VDlg.edAppName.Text + '.fcgi';
  end;
  AProject.Flags := AProject.Flags - [pfMainUnitHasCreateFormStatements, pfRunnable];
  AProject.LazCompilerOptions.Win32GraphicApp := False;
  AProject.LazCompilerOptions.TargetFilenameApplyConventions := False;
  AProject.SessionStorage := pssInProjectInfo;
  AProject.MainFileID := 0;
  S :=
    'program '+VDlg.edAppName.Text+';'+le+
    ''+le+
    '{$mode objfpc}{$H+}'+le+
    ''+le+
    'uses'+le+
    '  BrookApplication, Brokers;'+le+
    ''+le+
    'begin'+le+
    '  BrookApp.Run;'+le+
    'end.';
  AProject.MainFile.SetSourceText(S);
  Result := mrOK;
end;

function TBrookProjectDescriptor.GetLocalizedName: string;
begin
  Result := SBrookAppName;
end;

function TBrookProjectDescriptor.GetLocalizedDescription: string;
begin
  Result := SBrookAppDesc;
end;

{ TBrookTable2HTMLFormDescriptor }

constructor TBrookTable2HTMLFormDescriptor.Create;
begin
  inherited Create;
  Name := SBrookTable2HTMLFormName;
end;

function TBrookTable2HTMLFormDescriptor.DoInitDescriptor: TModalResult;
begin
  TfrBrookTable2HTMLForm.Execute;
  Result := mrCancel;
end;

function TBrookTable2HTMLFormDescriptor.GetLocalizedName: string;
begin
  Result := SBrookTable2HTMLFormName;
end;

function TBrookTable2HTMLFormDescriptor.GetLocalizedDescription: string;
begin
  Result := SBrookTable2HTMLFormDesc;
end;

{ TBrookBrokersFileDescPascalUnit }

constructor TBrookBrokersFileDescPascalUnit.Create;
begin
  inherited Create;
  Name := SBrookBrokersName;
  DefaultFileName := 'brokers.pas';
  DefaultSourceName := 'Brokers';
  FQuiet := True;
end;

function TBrookBrokersFileDescPascalUnit.Init(var ANewFilename: string;
  ANewOwner: TObject; var ANewSource: string; AQuiet: Boolean): TModalResult;
begin
  Result := inherited Init(ANewFilename, ANewOwner, ANewSource, AQuiet);
  if AQuiet or FQuiet then
    Exit;
  FAppType := TfrBrookNewPlugin.Execute;
  if FAppType = -1 then
    Result := mrCancel;
end;

function TBrookBrokersFileDescPascalUnit.CreateSource(const AFileName,
  ASourceName, AResourceName: string): string;
var
  VDlg: TfrBrookNewProject;
  VCharset, VBroker: string;
begin
  case FAppType of
    0: VBroker := 'BrookFCLCGIBroker';
    1: VBroker := 'BrookFCLFCGIBroker';
  end;
  case FAppDefCharset of
    0: VCharset := 'HTTP_CHARSET_UTF_8';
    1: VCharset := 'HTTP_CHARSET_ISO_8859_1';
  end;
  if FFullPlg then
  begin
    VDlg := BrookNewProjectDlg;
    Result :=
    'unit Brokers;'+le+
    ''+le+
    '{$mode objfpc}{$H+}'+le+
    ''+le+
    'interface'+le+
    ''+le+
    'uses'+le+
    '  '+VBroker+', BrookHTTPConsts, BrookUtils, Classes, SysUtils;'+le+
    ''+le+
    'const'+le+
    '  PUBLIC_HTML = '+QuotedStr(IncludeTrailingPathDelimiter(VDlg.edPubHTMLDir.Text))+';'+le+
    ''+le+
    'implementation'+le+
    ''+le+
    'initialization'+le+
    '  BrookSettings.Charset := ' + VCharset + ';'+le+
    '  BrookSettings.Page404:= PUBLIC_HTML + ''404.html'';'+le+
    '  BrookSettings.Page500 := PUBLIC_HTML + ''500.html'';'+le+
    ''+le+
    'end.';
  end
  else
    Result :=
    'unit Brokers;'+le+
    ''+le+
    '{$mode objfpc}{$H+}'+le+
    ''+le+
    'interface'+le+
    ''+le+
    'uses'+le+
    '  '+VBroker+', BrookUtils'+';'+le+
    ''+le+
    'implementation'+le+
    ''+le+
    'initialization'+le+
    '  BrookSettings.Page404 :='+le+
    '    ''<html><head><title>Page not found</title></head><body>'' +'+le+
    '    ''<h1>404 - Page not found</h1></body></html>'';'+le+
    '  BrookSettings.Page500 :='+le+
    '    ''<html><head><title>Internal server error</title></head><body>'' +'+le+
    '    ''<h1>500 - Internal server error</h1>'' +'+le+
    '    ''<p style="color: red;" >Error: %s</p></body></html>'';'+le+
    ''+le+
    'end.';
  FAppType := 0;
  FAppDefCharset := 0;
  FFullPlg := False;
  FQuiet := False;
end;

function TBrookBrokersFileDescPascalUnit.GetLocalizedName: string;
begin
  Result := SBrookBrokersName;
end;

function TBrookBrokersFileDescPascalUnit.GetLocalizedDescription: string;
begin
  Result := SBrookBrokersDesc;
end;

{ TBrookActionFileDescPascalUnit }

constructor TBrookActionFileDescPascalUnit.Create;
begin
  inherited Create;
  Name := SBrookActionName;
end;

function TBrookActionFileDescPascalUnit.Init(var ANewFilename: string;
  ANewOwner: TObject; var ANewSource: string; AQuiet: Boolean): TModalResult;
begin
  Result := inherited Init(ANewFilename, ANewOwner, ANewSource, AQuiet);
  if AQuiet or FQuiet then
    Exit;
  if not TfrBrookActEdit.Execute('Add', FActName, FActPattern,
    FActDefault, True) then
    Result := mrCancel;
end;

function TBrookActionFileDescPascalUnit.CreateSource(const AFileName,
  ASourceName, AResourceName: string): string;
var
  VActDefault: ShortString = '';
begin
  if FActName = '' then
    FActName := ASourceName;
  FActName := UpperCase(Copy(FActName, 1, 1)) + Copy(FActName, 2, MaxInt);
  if FActPattern = '' then
    FActPattern := '/' + LowerCase(ASourceName) + '/';
  if FActDefault then
    VActDefault := ', True';
  Result :=
    'unit '+ASourceName+';'+le+
    ''+le+
    '{$mode objfpc}{$H+}'+le+
    ''+le+
    'interface'+le+
    ''+le+
    'uses'+le+
    '  BrookAction;'+le+
    ''+le+
    'type'+le+
    '  T'+FActName+' = class(TBrookAction)'+le+
    '  public'+le+
    '    procedure Get; override;'+le+
    '  end;'+le+
    ''+le+
    'implementation'+le+
    ''+le+
    'procedure T'+FActName+'.Get;'+le+
    'begin'+le+
    '  Write(''Your content here ...'');'+le+
    'end;'+le+
    ''+le+
    'initialization'+le+
    '  T'+FActName+'.Register('+QuotedStr(FActPattern)+VActDefault+');'+le+
    ''+le+
    'end.';
  FActName := '';
  FActPattern := '';
  FActDefault := False;
  FQuiet := False;
end;

function TBrookActionFileDescPascalUnit.GetLocalizedName: string;
begin
  Result := SBrookActionName;
end;

function TBrookActionFileDescPascalUnit.GetLocalizedDescription: string;
begin
  Result := SBrookActionDesc;
end;

finalization
  BrookFreeNewProjectDlg;

end.

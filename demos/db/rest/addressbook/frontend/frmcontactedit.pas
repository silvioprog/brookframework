unit frmContactEdit;

{$mode objfpc}{$H+}

interface

uses
  frmCustomEdit, StdCtrls;

type
  TfrContactEdit = class(TfrCustomEdit)
    input_name: TEdit;
    lbName: TLabel;
  end;

implementation

{$R *.lfm}

end.


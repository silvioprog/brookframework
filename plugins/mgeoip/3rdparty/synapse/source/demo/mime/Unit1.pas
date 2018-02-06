unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, mimemess, mimepart, ComCtrls;

type
  TForm1 = class(TForm)
    Edit1: TEdit;
    Button1: TButton;
    Label1: TLabel;
    Memo1: TMemo;
    Button2: TButton;
    Label2: TLabel;
    TreeView1: TTreeView;
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure TreeView1Change(Sender: TObject; Node: TTreeNode);
  private
    { Private declarations }
    procedure AddMimeNode(const parent: TTreeNode; const part: TMimepart);
  public
    Mime:TMimemess;
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.DFM}

procedure TForm1.AddMimeNode(const parent: TTreeNode; const part: TMimepart);
var
  s: string;
  node: TTreeNode;
  n: integer;
begin
  s := format('%-24s %-15s %-s',[part.primary + '/' + part.secondary,part.filename,part.description]);
  node := TreeView1.Items.AddChild(parent, s);
  node.Data := part;
  for n := 0 to part.GetSubPartCount - 1 do
    AddMimeNode(node, part.getsubpart(n));
end;

procedure TForm1.Button1Click(Sender: TObject);
var
  n:integer;
  s:string;
begin
  mime.Clear;
  memo1.Clear;
  mime.Lines.LoadFromFile(edit1.text);
  mime.DecodeMessage;
  ShowMessage(datetimetostr(mime.Header.Date));

  Treeview1.Items.Clear;
  AddMimeNode(nil, mime.MessagePart);
  Treeview1.FullExpand;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  mime:=TMimemess.create;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  mime.free;
end;

procedure TForm1.Button2Click(Sender: TObject);
var
  f:string;
begin
  with TMimePart(Treeview1.Selected.data) do
    begin
      f:=filename;
      if f=''
        then f:='mimedemo.txt';
      f:='c:/'+f;
      Decodepart;
      decodedlines.SaveToFile(f);
    end;
end;

procedure TForm1.TreeView1Change(Sender: TObject; Node: TTreeNode);
begin
  memo1.Lines.assign(TMimepart(Node.Data).Lines);
end;

end.

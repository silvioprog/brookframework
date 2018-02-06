object TestSFTPForm: TTestSFTPForm
  Left = 207
  Top = 107
  Width = 696
  Height = 480
  Caption = 'Test SFTP'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 8
    Top = 8
    Width = 22
    Height = 13
    Caption = 'Host'
  end
  object Label2: TLabel
    Left = 284
    Top = 8
    Width = 19
    Height = 13
    Caption = 'Port'
  end
  object Label3: TLabel
    Left = 8
    Top = 32
    Width = 26
    Height = 13
    Caption = 'Login'
  end
  object Label4: TLabel
    Left = 192
    Top = 32
    Width = 46
    Height = 13
    Caption = 'Password'
  end
  object Label5: TLabel
    Left = 8
    Top = 64
    Width = 48
    Height = 13
    Caption = 'Current dir'
  end
  object HostEdit: TEdit
    Left = 48
    Top = 4
    Width = 209
    Height = 21
    TabOrder = 0
    Text = '212.24.37.138'
  end
  object PortEdit: TEdit
    Left = 316
    Top = 4
    Width = 49
    Height = 21
    TabOrder = 1
    Text = '22'
  end
  object LoginEdit: TEdit
    Left = 48
    Top = 28
    Width = 113
    Height = 21
    TabOrder = 2
    Text = 'atv'
  end
  object PasswordEdit: TEdit
    Left = 252
    Top = 28
    Width = 113
    Height = 21
    PasswordChar = '*'
    TabOrder = 3
    Text = 'atv04040702'
  end
  object CurrentDirEdit: TEdit
    Left = 64
    Top = 60
    Width = 301
    Height = 21
    TabOrder = 5
  end
  object ConnectButton: TButton
    Left = 380
    Top = 4
    Width = 93
    Height = 25
    Caption = 'Connect'
    TabOrder = 4
    OnClick = ConnectButtonClick
  end
  object FileListBox: TListBox
    Left = 8
    Top = 92
    Width = 673
    Height = 353
    Font.Charset = RUSSIAN_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Courier New'
    Font.Style = []
    ItemHeight = 14
    ParentFont = False
    TabOrder = 12
    OnDblClick = FileListBoxDblClick
  end
  object SendFileButton: TButton
    Left = 476
    Top = 68
    Width = 67
    Height = 21
    Caption = 'Send file...'
    TabOrder = 7
    OnClick = SendFileButtonClick
  end
  object GetFileButton: TButton
    Left = 544
    Top = 68
    Width = 67
    Height = 21
    Caption = 'Get file'
    TabOrder = 8
    OnClick = GetFileButtonClick
  end
  object DeleteButton: TButton
    Left = 612
    Top = 68
    Width = 67
    Height = 21
    Caption = 'Delete'
    TabOrder = 9
    OnClick = DeleteButtonClick
  end
  object ReloadButton: TButton
    Left = 368
    Top = 60
    Width = 57
    Height = 21
    Caption = 'Reload'
    TabOrder = 6
    OnClick = ReloadButtonClick
  end
  object ProgressBar: TProgressBar
    Left = 476
    Top = 45
    Width = 153
    Height = 15
    Min = 0
    Max = 100
    TabOrder = 10
  end
  object AbortButton: TButton
    Left = 632
    Top = 44
    Width = 45
    Height = 17
    Caption = 'Abort'
    TabOrder = 11
    OnClick = AbortButtonClick
  end
  object OpenDialog: TOpenDialog
    Options = [ofHideReadOnly, ofFileMustExist, ofEnableSizing]
    Left = 492
    Top = 12
  end
  object SaveDialog: TSaveDialog
    Options = [ofOverwritePrompt, ofHideReadOnly, ofEnableSizing]
    Left = 572
    Top = 12
  end
end

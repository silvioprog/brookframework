unit SimpleSFTP;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,ssl_cryptlib,
  StdCtrls, blcksock, syncobjs, winsock, Math, CryptLib;

//  Example of SFTP client implementation. Based on
//  http://www.snailbook.com/docs/sftp.txt and PuTTY's source code.
//  Not tested carefully but directory listing and file transfer seems to work.
//  Requires cl32.dll (CryptLib) and Synapse 37b6 or newer !!!
//  If somebody knows how to extract file type information from file attributes
//  packet (I'm not sure that code in TSimpleSFTP.ParseFileNamePacket will work
//  in any case) then please let know to Sergey Gagarin (serg@screens.ru)
  
const
  //Really developing was started for version 6 (all constants and file
  //attributes are from version 6, but server I've had for
  //testing was version 3, so it was tested for version 3 only !!!
  //Thanks to PuTTY source code, it was very usefull !
  //Please note that not all capabilities were tested carefully !!! 
  SFTP_PROTOCOLCURRENTVERSION=3;

  //sftp packet types
  SSH_FXP_INIT                =1;
  SSH_FXP_VERSION             =2;
  SSH_FXP_OPEN                =3;
  SSH_FXP_CLOSE               =4;
  SSH_FXP_READ                =5;
  SSH_FXP_WRITE               =6;
  SSH_FXP_LSTAT               =7;
  SSH_FXP_FSTAT               =8;
  SSH_FXP_SETSTAT             =9;
  SSH_FXP_FSETSTAT           =10;
  SSH_FXP_OPENDIR            =11;
  SSH_FXP_READDIR            =12;
  SSH_FXP_REMOVE             =13;
  SSH_FXP_MKDIR              =14;
  SSH_FXP_RMDIR              =15;
  SSH_FXP_REALPATH           =16;
  SSH_FXP_STAT               =17;
  SSH_FXP_RENAME             =18;
  SSH_FXP_READLINK           =19;
  SSH_FXP_SYMLINK            =20;
  //server responce types
  SSH_FXP_STATUS            =101;
  SSH_FXP_HANDLE            =102;
  SSH_FXP_DATA              =103;
  SSH_FXP_NAME              =104;
  SSH_FXP_ATTRS             =105;
  //extended packet types
  SSH_FXP_EXTENDED          =200;
  SSH_FXP_EXTENDED_REPLY    =201;
//  RESERVED_FOR_EXTENSIONS           210-255

  //file attributes flags (for protocol version 6, but processed
  //SSH_FILEXFER_ATTR_SIZE, SSH_FILEXFER_ATTR_PERMISSIONS
  //and SSH_FILEXFER_ATTR_ACCESSTIME only !!! (no extensions)
  //also flag 2 is processed but not used
  SSH_FILEXFER_ATTR_SIZE              =$00000001;
  SSH_FILEXFER_ATTR_PERMISSIONS       =$00000004;
  SSH_FILEXFER_ATTR_ACCESSTIME        =$00000008;
  SSH_FILEXFER_ATTR_CREATETIME        =$00000010;
  SSH_FILEXFER_ATTR_MODIFYTIME        =$00000020;
  SSH_FILEXFER_ATTR_ACL               =$00000040;
  SSH_FILEXFER_ATTR_OWNERGROUP        =$00000080;
  SSH_FILEXFER_ATTR_SUBSECOND_TIMES   =$00000100;
  SSH_FILEXFER_ATTR_BITS              =$00000200;
  SSH_FILEXFER_ATTR_ALLOCATION_SIZE   =$00000400;
  SSH_FILEXFER_ATTR_TEXT_HINT         =$00000800;
  SSH_FILEXFER_ATTR_MIME_TYPE         =$00001000;
  SSH_FILEXFER_ATTR_LINK_COUNT        =$00002000;
  SSH_FILEXFER_ATTR_UNTRANLATED_NAME  =$00004000;
  SSH_FILEXFER_ATTR_EXTENDED          =$80000000;

  //file types (not present in version 3, but roughly "simulated"
  //in method ParseFileNamePacket (unfortunately, permissions field seems to contain no file type info)
  SSH_FILEXFER_TYPE_REGULAR          =1;
  SSH_FILEXFER_TYPE_DIRECTORY        =2;
  SSH_FILEXFER_TYPE_SYMLINK          =3;
  SSH_FILEXFER_TYPE_SPECIAL          =4;
  SSH_FILEXFER_TYPE_UNKNOWN          =5;
  SSH_FILEXFER_TYPE_SOCKET           =6;
  SSH_FILEXFER_TYPE_CHAR_DEVICE      =7;
  SSH_FILEXFER_TYPE_BLOCK_DEVICE     =8;
  SSH_FILEXFER_TYPE_FIFO             =9;

  //permissions
  S_IRUSR  =$0000400;
  S_IWUSR  =$0000200;
  S_IXUSR  =$0000100;
  S_IRGRP  =$0000040;
  S_IWGRP  =$0000020;
  S_IXGRP  =$0000010;
  S_IROTH  =$0000004;
  S_IWOTH  =$0000002;
  S_IXOTH  =$0000001;
  S_ISUID  =$0004000;
  S_ISGID  =$0002000;
  S_ISVTX  =$0001000;
  //file type bits in permissions field
  S_IFMT   =$0170000;// bitmask for the file type bitfields
  S_IFSOCK =$0140000;// socket
  S_IFLNK  =$0120000;// symbolic link
  S_IFREG  =$0100000;// regular file
  S_IFBLK  =$0060000;// block device
  S_IFDIR  =$0040000;// directory
  S_IFCHR  =$0020000;// character device
  S_IFIFO  =$0010000;// fifo

  //file attributes
  SSH_FILEXFER_ATTR_FLAGS_READONLY         =$00000001;
  SSH_FILEXFER_ATTR_FLAGS_SYSTEM           =$00000002;
  SSH_FILEXFER_ATTR_FLAGS_HIDDEN           =$00000004;
  SSH_FILEXFER_ATTR_FLAGS_CASE_INSENSITIVE =$00000008;
  SSH_FILEXFER_ATTR_FLAGS_ARCHIVE          =$00000010;
  SSH_FILEXFER_ATTR_FLAGS_ENCRYPTED        =$00000020;
  SSH_FILEXFER_ATTR_FLAGS_COMPRESSED       =$00000040;
  SSH_FILEXFER_ATTR_FLAGS_SPARSE           =$00000080;
  SSH_FILEXFER_ATTR_FLAGS_APPEND_ONLY      =$00000100;
  SSH_FILEXFER_ATTR_FLAGS_IMMUTABLE        =$00000200;
  SSH_FILEXFER_ATTR_FLAGS_SYNC             =$00000400;
  SSH_FILEXFER_ATTR_FLAGS_TRANSLATION_ERR  =$00000800;

  //file access type
  ACE4_READ_DATA         =$00000001;
  ACE4_LIST_DIRECTORY    =$00000001;
  ACE4_WRITE_DATA        =$00000002;
  ACE4_ADD_FILE          =$00000002;
  ACE4_APPEND_DATA       =$00000004;
  ACE4_ADD_SUBDIRECTORY  =$00000004;
  ACE4_READ_NAMED_ATTRS  =$00000008;
  ACE4_WRITE_NAMED_ATTRS =$00000010;
  ACE4_EXECUTE           =$00000020;
  ACE4_DELETE_CHILD      =$00000040;
  ACE4_READ_ATTRIBUTES   =$00000080;
  ACE4_WRITE_ATTRIBUTES  =$00000100;
  ACE4_DELETE            =$00010000;
  ACE4_READ_ACL          =$00020000;
  ACE4_WRITE_ACL         =$00040000;
  ACE4_WRITE_OWNER       =$00080000;
  ACE4_SYNCHRONIZE       =$00100000;

  //open file flags
  SSH_FXF_ACCESS_DISPOSITION        = $00000007;
  SSH_FXF_CREATE_NEW                = $00000000;
  SSH_FXF_CREATE_TRUNCATE           = $00000001;
  SSH_FXF_OPEN_EXISTING             = $00000002;
  SSH_FXF_OPEN_OR_CREATE            = $00000003;
  SSH_FXF_TRUNCATE_EXISTING         = $00000004;
  SSH_FXF_ACCESS_APPEND_DATA        = $00000008;
  SSH_FXF_ACCESS_APPEND_DATA_ATOMIC = $00000010;
  SSH_FXF_ACCESS_TEXT_MODE          = $00000020;
  SSH_FXF_ACCESS_READ_LOCK          = $00000040;
  SSH_FXF_ACCESS_WRITE_LOCK         = $00000080;
  SSH_FXF_ACCESS_DELETE_LOCK        = $00000100;
  SSH_FXF_NOFOLLOW                  = $00000200;

  //open file flags for protocol version 3 (as in PuTTY)
  SSH_FXF_READ                              =$00000001;
  SSH_FXF_WRITE                             =$00000002;
  SSH_FXF_APPEND                            =$00000004;
  SSH_FXF_CREAT                             =$00000008;
  SSH_FXF_TRUNC                             =$00000010;
  SSH_FXF_EXCL                              =$00000020;

  //rename flags
  SSH_FXP_RENAME_OVERWRITE  =$00000001;
  SSH_FXP_RENAME_ATOMIC     =$00000002;
  SSH_FXP_RENAME_NATIVE     =$00000004;

  //error codes
  SSH_FX_OK                            =0;
  SSH_FX_EOF                           =1;
  SSH_FX_NO_SUCH_FILE                  =2;
  SSH_FX_PERMISSION_DENIED             =3;
  SSH_FX_FAILURE                       =4;
  SSH_FX_BAD_MESSAGE                   =5;
  SSH_FX_NO_CONNECTION                 =6;
  SSH_FX_CONNECTION_LOST               =7;
  SSH_FX_OP_UNSUPPORTED                =8;
  SSH_FX_INVALID_HANDLE                =9;
  SSH_FX_NO_SUCH_PATH                  =10;
  SSH_FX_FILE_ALREADY_EXISTS           =11;
  SSH_FX_WRITE_PROTECT                 =12;
  SSH_FX_NO_MEDIA                      =13;
  SSH_FX_NO_SPACE_ON_FILESYSTEM        =14;
  SSH_FX_QUOTA_EXCEEDED                =15;
  SSH_FX_UNKNOWN_PRINCIPLE             =16;
  SSH_FX_LOCK_CONFlICT                 =17;
  SSH_FX_DIR_NOT_EMPTY                 =18;
  SSH_FX_NOT_A_DIRECTORY               =19;
  SSH_FX_INVALID_FILENAME              =20;
  SSH_FX_LINK_LOOP                     =21;

type
  TSimpleSFTP=class;//main class

  TSFTPFileAttributes=record //complete structure for protocol version 6
    FileName:string;
    LongName:string;//present in version 3 only !
    valid_attribute_flags:DWORD;
    file_type:byte;//                 always present
    size:int64;//                     present only if flag SIZE
    allocation_size:int64;//          present only if flag ALLOCATION_SIZE
    owner:string;//                   present only if flag OWNERGROUP
    group:string;//                   present only if flag OWNERGROUP
    permissions:DWORD;//              present only if flag PERMISSIONS
    atime:int64;//                    present only if flag ACCESSTIME
    atime_nseconds:DWORD;//           present only if flag SUBSECOND_TIMES
    createtime:int64;//               present only if flag CREATETIME
    createtime_nseconds:DWORD;//      present only if flag SUBSECOND_TIMES
    mtime:int64;//                    present only if flag MODIFYTIME
    mtime_nseconds:DWORD;//           present only if flag SUBSECOND_TIMES
    acl:string;//                     present only if flag ACL
    attrib_bits:DWORD;//              present only if flag BITS
    text_hint:byte;//                 present only if flag TEXT_HINT
    mime_type:string;//               present only if flag MIME_TYPE
    link_count:DWORD;//               present only if flag LINK_COUNT
    untranslated_name:string;//       present only if flag UNTRANSLATED_NAME
    extended_count:DWORD;//           present only if flag EXTENDED - parsed but not used here !
//    extended_type:string;//         not used here !
//    extended_data:string;//
  end;
  PSFTPFileAttributes=^TSFTPFileAttributes;
  //'atime', 'createtime', and 'mtime' - seconds from Jan 1, 1970 in UTC

  TSFTPFileList=class(TObject)
  protected
    FList:TList;
    function GetFile(i:Integer):PSFTPFileAttributes;
  public
    constructor Create;
    destructor Destroy;override;
    procedure Clear;
    function Count:Integer;
    procedure Add(FileRecord:TSFTPFileAttributes);
    procedure Delete(i:Integer);
    procedure Exchange(i,j:Integer);
    procedure Sort(Compare:TListSortCompare);

    property Files[i:Integer]:PSFTPFileAttributes read GetFile;default;
  end;

  TSimpleSFTPProgressCallback=function (UserData:Pointer;
    Current,Total:Int64):Boolean of object;//returns False to abort
  TSimpleSMPTEvent=procedure (Sender:TSimpleSFTP) of object;  

  TSimpleSFTP=class(TObject)
  private
    //just utils to set file times for local files
    procedure GetLocalFileTimes(FileName:string;var AccessTime,CreateTime,ModifyTime:Int64);
    procedure SetLocalFileTimes(FileName:string;AccessTime,CreateTime,ModifyTime:Int64);
    //The only way we read server's data ! Do not use other socket read operations ! 
    procedure ReceiveBuffer(Buffer:PChar;BufferSize:Integer);
  protected
    FSocket:TTCPBlockSocket;
    FTimeout:DWORD;// timeout for data waitng (miliseconds)
    FProtocolVersion:DWORD;
    FRequestID:DWORD;
    FEndOfLine:string;//processed but not used 
    FBufferSize:DWORD;
    FRemotePathSeparator:string;
    FCurrentDir:string;

    procedure DoError(ErrorMessage:string);
    procedure ResetSessionParams;
    //file names and attributes processing
    function ValidateRemoteDirName(RemoteDir:string):string;
    function ParseFileNamePacket(FileList:TSFTPFileList;PacketData:string;ProcessAttributes:Boolean=True):Integer;
    function ParseFileAttributes(AtributesString:string;var FieldOffset:Integer):TSFTPFileAttributes;
    function BuildAttributesString(FileAttributes:PSFTPFileAttributes):string;
    function BuildBlankAttributesString(IsDir:Boolean=False):string;
    //sftp packet constructing and parsing
    function BuildPacket(PaketType:Byte;Data:array of Pointer;DataSize:array of DWORD;
      IsFixedSize:array of Boolean;SendRequestID:Boolean=True):string;
    procedure AddDataToPacket(var PacketString:string;Data:array of Pointer;
      DataSize:array of DWORD;IsFixedSize:array of Boolean);//to build packet step by step
    function ParsePacketStrings(Data:string;Offset:Integer=0):TStringList;
    function GetStatus(PacketData:string):DWORD;//get status from server's SSH_FXP_STATUS packet
    function CheckStatus(PacketType:DWORD;PacketData:string;ErrorString:string):Boolean;

    procedure SendPacket(Packet:string);
    function ReceivePacket(RequestID:DWORD;var PacketType:Byte;ReceiveRequestID:Boolean=True):string;

    procedure Init;
    //internal file/dir operations
    function SetRealPath(DirName:string):string;
    function OpenFile(FileName:string;FileOpenFlags:DWORD):string;
    function CloseFile(FileHandle:string):Boolean;
    function OpenDir(DirName:string):string;
    function CloseDir(DirHandle:string):Boolean;
    function ReadFile(FileHandle:string;FileOffset:Int64;ReadSize:DWORD):string;
    procedure WriteFile(FileHandle:string;FileOffset:Int64;FileData:Pointer;DataSize:DWORD);
    procedure ReadDir(DirHandle:string;FileList:TSFTPFileList);
    //internal file attributes operations
    procedure InternalGetFileAtributes(PacketType:BYTE;FileID:string;//name or handle
      AttributeFlags:DWORD;var Attributes:TSFTPFileAttributes);
    procedure GetFileAtributesByHandle(FileHandle:string;var Attributes:TSFTPFileAttributes);
    procedure SetFileAtributesByHandle(FileHandle:string;Attributes:PSFTPFileAttributes);
    procedure GetFileTimesByHandle(FileHandle:string;var AccessTime,CreateTime,ModifyTime:Int64);
    procedure SetFileTimesByHandle(FileHandle:string;AccessTime,CreateTime,ModifyTime:Int64);
    function GetFileSizeByHandle(FileHandle:string):Int64;
  public
    constructor Create;virtual;
    destructor Destroy;override;

    procedure Connect(Host,Port,UserName,Password:string);
    procedure Disconnect;

    //file operation
    function PutFile(LocalFileName,RemoteDir:string;PreserveFileTimes:Boolean=True;Overwrite:Boolean=True;
      Append:Boolean=False;SourceStartPos:Int64=0;Callback:TSimpleSFTPProgressCallback=nil;UserData:Pointer=nil):Int64;
    function GetFile(RemoteDir,RemoteFileName,LocalFileName:string;PreserveFileTimes:Boolean=True;Overwrite:Boolean=True;
      Append:Boolean=False;SourceStartPos:Int64=0;Callback:TSimpleSFTPProgressCallback=nil;UserData:Pointer=nil):Int64;
    procedure DeleteFile(FileName:string);
    procedure RenameFile(OldName,NewName:string;FailIfExists:Boolean);
    function FileExists(FileName:string):Boolean;

    //dir operation (not all tested :-) )
    function GetCurrentDir:string;
    function SetCurrentDir(DirName:string):string;
    procedure ListDir(DirName:string;FileList:TSFTPFileList);
    procedure CreateDir(DirName:string;Attributes:PSFTPFileAttributes=nil);
    procedure DeleteDir(DirName:string);

    //file attributes opearations
    procedure GetFileAtributes(FileName:string;var Attributes:TSFTPFileAttributes;FollowLink:Boolean=True);
    procedure SetFileAtributes(FileName:string;Attributes:PSFTPFileAttributes);
    procedure GetFileTimes(FileName:string;var AccessTime,CreateTime,ModifyTime:Int64);
    procedure SetFileTimes(FileName:string;AccessTime,CreateTime,ModifyTime:Int64);
    function GetFileSize(FileName:string):Int64;

    property Socket:TTCPBlockSocket read FSocket;
  end;

implementation

const //error messages
  STRING_NOTIMPLEMENTED='Not implemented';
  STRING_INVALIDOUTPACKETDATA='Invalid out packet data';
  STRING_INVALIDINPACKETDATA='Invalid in packet data';
  STRING_UNEXPECTEDPACKETTYPE='Unexpected packet type';
  STRING_UNABLETOINIT='Unable to init';
  STRING_INVALIDBUFFERSIZE='Invalid buffer size';
  STRING_INVALIDFILEPOS='Invalid file position';
  STRING_FILETRANSFERABORTED='File transfer aborted';
  STRING_UNABLETOOPENFILE='Unable to open file';
  STRING_UNABLETOOPENDIR='Unable to open directory';
  STRING_UNABLETOCLOSEHANDLE='Unable to close handle';
  STRING_UNABLETOREADFILE='Unable to read file';
  STRING_UNABLETOREADDIR='Unable to read directory';
  STRING_UNABLETOWRITETOFILE='Unable to write to file';
  STRING_UNABLETODELETEFILE='Unable to delete file';
  STRING_UNABLETORENAMEFILE='Unable to rename file';
  STRING_UNABLETOCREATEDIR='Unable to create directory';
  STRING_UNABLETODELETEDIR='Unable to delete directory';
  STRING_UNABLETOSETFILEATTRIBUTES='Unable to set file attributes';
  STRING_INVALIDFILENAMECOUNT='Invalid file name count';
  STRING_RECEIVETIMEOUT='Receive timeout';
  STRING_UNEXPECTEDSSHMESSAGE='Unexpected SSH message';
  STRING_INVALIDCHANNELID='Invalid channel ID';
  STRING_INVALIDPROTOCOLVERSION='Invalid protocol version';
  STRING_UNABLETOSETPATH='Unable to set path';
  STRING_UNABLETOSETBUFFERSIZE='Unable to set buffer size';
  STRING_UNABLETOGETFILEATTRIBUTES='Unable to get file attributes';
  STRING_UNABLETOGETFILESIZE='Unable to get file size';
  STRING_UNABLETOGETFILETIMES='Unable to get file times';
  STRING_UNABLETOSETFILETIMES='Unable to set file times';
  STRING_UNABLETORECEIVEPACKETDATA='Unable to receive packet data';
  STRING_UNABLETOSENDPACKETDATA='Unable to receive packet data';
  STRING_UNKNOWNERROR='Unknown error';


//************************************************************************
//************************ File time converting utils ********************
//************************************************************************
//  FileTime - number of 100-nanosecond intervals since January 1, 1601
//  SFTPFileTime - number of seconds since January 1, 1970
//  day_diff=134774
const
  DAY_DIFF:Int64=134774;
  SECONDS_IN_DAY:Int64=86400;

function FileTimeToSFTPFileTime(FileTime:Int64):Int64;
begin
  Result:=(FileTime div 10000000)-DAY_DIFF*SECONDS_IN_DAY;
end;

function SFTPFileTimeToFileTime(FileTime:Int64):Int64;
begin
  Result:=(FileTime+DAY_DIFF*SECONDS_IN_DAY)*10000000;
end;

//************************************************************************
//************** Some utils to work with SFTP packet fields **************
//************************************************************************

function PutDataToString(Buffer:Pointer;Size:Integer):string;
begin
  SetLength(Result,Size);
  CopyMemory(@Result[1],Buffer,Size);
end;

function InvertDWORD(Value:DWORD):DWORD;//SFTP uses inverted byte order !!!
begin
  Result:=((Value and $FF) shl 24) or ((Value and $FF00) shl 8) or
    ((Value and $FF0000) shr 8) or ((Value and $FF000000) shr 24);
end;

function InvertInt64(Value:Int64):Int64;
begin
  PDWORD(@Result)^:=InvertDWORD(PDWORD(Integer(@Value)+SizeOf(DWORD))^);
  PDWORD(Integer(@Result)+SizeOf(DWORD))^:=InvertDWORD(PDWORD(@Value)^);
end;

function PutDWORD(Value:DWORD):string;
begin
  Value:=InvertDWORD(Value);
  Result:=PutDataToString(@Value,SizeOf(Value));
end;

function GetDWORD(Buffer:Pointer):DWORD;
begin
  Result:=InvertDWORD(PDWORD(Buffer)^);
end;

function PutFixedPacketField(Buffer:Pointer;FieldSize:Integer):string;
var CurDWORD:DWORD;
begin //fixed size fields (DWORD, QWORD) are stored without field size
  SetLength(Result,FieldSize);
  case FieldSize of
  SizeOf(DWORD): Result:=PutDWORD(PDWORD(Buffer)^);
  SizeOf(Int64):
    Result:=PutDWORD(PDWORD(PChar(Buffer)+SizeOf(DWORD))^)+PutDWORD(PDWORD(Buffer)^);
  else Result:=PutDataToString(Buffer,FieldSize);
  end;
end;

function PutStringPacketField(Buffer:string):string;
begin //string fields are stored with their length
  Result:=PutDWORD(Length(Buffer))+Buffer;
end;

procedure GetFixedPacketField(PacketData:string;var FieldOffset:Integer;
  Buffer:Pointer;FieldSize:Integer);
var CurDWORD:DWORD;
begin
  case FieldSize of
  SizeOf(DWORD):
  begin
    CurDWORD:=GetDWORD(@PacketData[FieldOffset]);
    CopyMemory(Buffer,@CurDWORD,SizeOf(DWORD));
  end;
  SizeOf(Int64):
  begin
    CurDWORD:=GetDWORD(@PacketData[FieldOffset]);
    CopyMemory(PChar(Buffer)+SizeOf(DWORD),@CurDWORD,SizeOf(DWORD));
    CurDWORD:=GetDWORD(@PacketData[FieldOffset+SizeOf(DWORD)]);
    CopyMemory(Buffer,@CurDWORD,SizeOf(DWORD));
  end;
  else CopyMemory(Buffer,@PacketData[FieldOffset],FieldSize);
  end;
  Inc(FieldOffset,FieldSize);
end;

function GetStringPacketField(PacketData:string;var FieldOffset:Integer):string;
var FieldSize:DWORD;
begin
  FieldSize:=GetDWORD(@PacketData[FieldOffset]);
  Inc(FieldOffset,SizeOf(FieldSize));
  SetLength(Result,FieldSize);
  CopyMemory(@Result[1],@PacketData[FieldOffset],FieldSize);
  Inc(FieldOffset,FieldSize);
end;

//****************************************************************
//*********************** TSFTPFileList **************************
//****************************************************************
//  list of file names and attributes

constructor TSFTPFileList.Create;
begin
  inherited Create;
  FList:=TList.Create;
end;

destructor TSFTPFileList.Destroy;
begin
  Clear;
  FList.Free;
  inherited Destroy;
end;

function TSFTPFileList.GetFile(i:Integer):PSFTPFileAttributes;
begin
  Result:=PSFTPFileAttributes(FList[i]);
end;

procedure TSFTPFileList.Clear;
var i:Integer;
begin
  for i:=FList.Count-1 downto 0 do Delete(i);
end;

function TSFTPFileList.Count:Integer;
begin
  Result:=FList.Count;
end;

procedure TSFTPFileList.Add(FileRecord:TSFTPFileAttributes);
var NewRecord:PSFTPFileAttributes;
begin
  New(NewRecord);
  NewRecord^:=FileRecord;
  FList.Add(NewRecord);
end;

procedure TSFTPFileList.Delete(i:Integer);
begin
  Dispose(PSFTPFileAttributes(FList[i]));
  FList.Delete(i);
end;

procedure TSFTPFileList.Exchange(i,j:Integer);
begin
  FList.Exchange(i,j);
end;

procedure TSFTPFileList.Sort(Compare:TListSortCompare);
begin
  FList.Sort(Compare);
end;

//************************************************************************
//****************************** TSimpleSFTP *****************************
//************************************************************************

constructor TSimpleSFTP.Create;
begin
  inherited Create;
  FSocket:=TTCPBlockSocket.CreateWithSSL(TSSLCryptLib);
  FSocket.RaiseExcept:=True;
  FTimeout:=60000;
  ResetSessionParams;
end;

destructor TSimpleSFTP.Destroy;
begin
  Disconnect;
  try
    FSocket.Free;
  except
  end;  
  inherited Destroy;
end;

procedure TSimpleSFTP.DoError(ErrorMessage:string);
begin
  if Trim(ErrorMessage)='' then ErrorMessage:=STRING_UNKNOWNERROR;
  raise Exception.Create(ErrorMessage);
end;

procedure TSimpleSFTP.ResetSessionParams;
begin
  FProtocolVersion:=SFTP_PROTOCOLCURRENTVERSION;
  FRequestID:=5;
  FEndOfLine:=#13#10;
  FBufferSize:=32768;
  FRemotePathSeparator:='/';
  FCurrentDir:='.';
end;

procedure TSimpleSFTP.Connect(Host,Port,UserName,Password:string);
var NoDelay:Boolean;
begin //setup proxy settings, ... before connecting
  FSocket.RaiseExcept:=True;
  try
    FSocket.Connect(Host,Port);
    //CryptLib manual recommends to disable the Nagle algorithm
    NoDelay:=True;
    setsockopt(FSocket.Socket,IPPROTO_TCP,TCP_NODELAY,@NoDelay,SizeOf(NoDelay));
    //do ssh handshake
    FSocket.SSL.SSLType:=LT_SSHv2;
    FSocket.SSL.Username:=UserName;
    FSocket.SSL.Password:=Password;
    FSocket.SSL.SSHChannelType:='subsystem';
    FSocket.SSL.SSHChannelArg1:='sftp';
    FSocket.SSLDoConnect;
    //negotiate protocol version
    ResetSessionParams;
    Init;
  except
    Disconnect;
    raise;
  end;
end;

procedure TSimpleSFTP.Disconnect;
begin
  try
    FSocket.RaiseExcept:=False;
    if FSocket.Socket<>INVALID_SOCKET then FSocket.CloseSocket;
  except
  end;  
end;

procedure TSimpleSFTP.GetLocalFileTimes(FileName:string;var AccessTime,CreateTime,ModifyTime:Int64);
var Handle:THandle;
begin //CreateTime is not used in version 3
  CreateTime:=0;
  AccessTime:=0;
  ModifyTime:=0;
  Handle:=CreateFile(PChar(FileName),GENERIC_READ,FILE_SHARE_READ+FILE_SHARE_WRITE,nil,OPEN_EXISTING,0,0);
  try
    if Handle=INVALID_HANDLE_VALUE then DoError(STRING_UNABLETOGETFILETIMES+': '+FileName);
    if not Windows.GetFileTime(Handle,@CreateTime,@AccessTime,@ModifyTime) then Exit;
    CreateTime:=FileTimeToSFTPFileTime(CreateTime);
    AccessTime:=FileTimeToSFTPFileTime(AccessTime);
    ModifyTime:=FileTimeToSFTPFileTime(ModifyTime);
  finally
    CloseHandle(Handle);
  end;
end;

procedure TSimpleSFTP.SetLocalFileTimes(FileName:string;AccessTime,CreateTime,ModifyTime:Int64);
var Handle:THandle;AccessTimeP,CreateTimeP,ModifyTimeP:Pointer;
begin //CreateTime is not used in version 3 (may be set CreateTime:=ModifyTime ?)
  if CreateTime<>0 then CreateTime:=SFTPFileTimeToFileTime(CreateTime);
  if AccessTime<>0 then AccessTime:=SFTPFileTimeToFileTime(AccessTime);
  if ModifyTime<>0 then ModifyTime:=SFTPFileTimeToFileTime(ModifyTime);
  if AccessTime=0 then AccessTimeP:=nil else AccessTimeP:=@AccessTime;
  if CreateTime=0 then CreateTimeP:=nil else CreateTimeP:=@CreateTime;
  if ModifyTime=0 then ModifyTimeP:=nil else ModifyTimeP:=@ModifyTime;
  Handle:=CreateFile(PChar(FileName),GENERIC_WRITE,FILE_SHARE_READ+FILE_SHARE_WRITE,nil,OPEN_EXISTING,0,0);
  try
    if Handle=INVALID_HANDLE_VALUE then DoError(STRING_UNABLETOSETFILETIMES+': '+FileName);
    if not Windows.SetFileTime(Handle,CreateTimeP,AccessTimeP,ModifyTimeP) then Exit;
  finally
    CloseHandle(Handle);
  end;
end;

function TSimpleSFTP.BuildPacket(PaketType:Byte;Data:array of Pointer;
  DataSize:array of DWORD;IsFixedSize:array of Boolean;SendRequestID:Boolean=True):string;
var i:Integer;CurField:string;FieldSize:DWORD;
begin //always increases FRequestID !
  if (Length(Data)<>Length(DataSize)) or (Length(Data)<>Length(IsFixedSize))
    then DoError(STRING_INVALIDOUTPACKETDATA);
  Result:='';
  //store packet fields
  for i:=Low(Data) to High(Data) do
  begin
    FieldSize:=DataSize[i];
    if FieldSize>0 then
    begin
      CurField:=PutDataToString(Data[i],FieldSize);
      //if not IsFixedSize then store field size too 
      if not IsFixedSize[i] then CurField:=PutDWORD(FieldSize)+CurField;
      Result:=Result+CurField;
    end;
  end;
  //store packet request id
  if SendRequestID then Result:=PutDWORD(FRequestID)+Result;
  //store packet type
  Result:=Char(PaketType)+Result;
  //store packet size
  Result:=PutDWORD(Length(Result))+Result;
  Inc(FRequestID);
end;

procedure TSimpleSFTP.AddDataToPacket(var PacketString:string;Data:array of Pointer;
  DataSize:array of DWORD;IsFixedSize:array of Boolean);
var i:Integer;CurField:string;FieldSize:DWORD;
begin //just add field to packet string (not increases FRequestID , but modifies packet size)
  if (Length(Data)<>Length(DataSize)) or (Length(Data)<>Length(IsFixedSize))
    or (PacketString='') then DoError(STRING_INVALIDOUTPACKETDATA);
  //store packet fields
  for i:=Low(Data) to High(Data) do
  begin
    FieldSize:=DataSize[i];
    if FieldSize>0 then
    begin
      CurField:=PutDataToString(Data[i],FieldSize);
      if not IsFixedSize[i] then CurField:=PutDWORD(FieldSize)+CurField;
      PacketString:=PacketString+CurField;
    end;
  end;
  //set new packet size
  FieldSize:=Length(PacketString);
  CopyMemory(@PacketString[1],@FieldSize,SizeOf(FieldSize));
end;

function TSimpleSFTP.ParsePacketStrings(Data:string;Offset:Integer=0):TStringList;
var CurPos,CurSize:DWORD;
begin //assumed that packet contains string fields only starting from Offset
  if Offset>0 then Data:=Copy(Data,Offset,Length(Data));
  Result:=TStringList.Create;
  try
    CurPos:=1;
    while CurPos<(Length(Data)-SizeOf(CurSize)+1) do
    begin
      CurSize:=GetDWORD(@Data[CurPos]);
      Result.Add(Copy(Data,CurPos+SizeOf(CurSize),CurSize));
      CurPos:=CurPos+SizeOf(CurSize)+CurSize;
    end;  
  except
    Result.Free;
    raise;
  end;
end;

procedure TSimpleSFTP.SendPacket(Packet:string);
var SentLength,CurDataSize:Integer;StartTime:DWORD;
begin
  FSocket.SendBuffer(@Packet[1],Length(Packet));
end;

procedure TSimpleSFTP.ReceiveBuffer(Buffer:PChar;BufferSize:Integer);
begin
  FSocket.RecvBufferEx(Buffer,BufferSize,FTimeout);
end;

function TSimpleSFTP.ReceivePacket(RequestID:DWORD;var PacketType:Byte;ReceiveRequestID:Boolean=True):string;
var PacketSize,CurRequestID,CurSize,CurDataSize:DWORD;PacketData:string;CurPacketType:BYTE;
  CurData:AnsiString;StartTime:DWORD;ReceivedLength:Integer;CurBuffer:string;
begin
  Result:='';
  while True do 
  begin
    //receive packet size
    ReceiveBuffer(@PacketSize,SizeOf(PacketSize));
    PacketSize:=GetDWORD(@PacketSize);
    //receive packet type
    ReceiveBuffer(@CurPacketType,SizeOf(CurPacketType));
    CurSize:=SizeOf(CurPacketType);
    //receive request id
    if ReceiveRequestID then
    begin
      ReceiveBuffer(@CurRequestID,SizeOf(CurRequestID));
      CurRequestID:=GetDWORD(@CurRequestID);
      CurSize:=CurSize+SizeOf(CurRequestID);
    end;
    //receive packet data
    SetLength(Result,PacketSize-CurSize);
    ReceiveBuffer(@Result[1],Length(Result));
    //check RequestID and PacketType (-1 and 0 means any ...)
    if (not ReceiveRequestID) or (RequestID=-1) or (RequestID=CurRequestID) then
    begin
      if (PacketType<>0) and (PacketType<>CurPacketType) then
        DoError(STRING_UNEXPECTEDPACKETTYPE+': '+IntToStr(PacketType)+'<>'+IntToStr(CurPacketType));
      PacketType:=CurPacketType;
      Break;
    end;  
  end;
end;

procedure TSimpleSFTP.Init;
var PacketData:string;PacketStrings:TStringList;i,FieldOffset:Integer;PacketType:Byte;
  TmpProtocolVersion,CurProtocolVersion:DWORD;
begin //negotiate protocol version (we support version 3 only!)
  PacketType:=SSH_FXP_INIT;
  TmpProtocolVersion:=InvertDWORD(FProtocolVersion);
  SendPacket(BuildPacket(PacketType,[@TmpProtocolVersion],[SizeOf(TmpProtocolVersion)],[True],False));
  PacketType:=SSH_FXP_VERSION;
  PacketData:=ReceivePacket(FRequestID-1,PacketType,False);
  FieldOffset:=1;
  //get protocol version
  GetFixedPacketField(PacketData,FieldOffset,@CurProtocolVersion,SizeOf(CurProtocolVersion));
  if FProtocolVersion<CurProtocolVersion then
    DoError(STRING_INVALIDPROTOCOLVERSION+': '+IntToStr(FProtocolVersion)+'<'+IntToStr(CurProtocolVersion));
  FProtocolVersion:=CurProtocolVersion;
  //parse extensions (if any)
  if FieldOffset<Length(PacketData) then
  begin
    PacketStrings:=ParsePacketStrings(PacketData,FieldOffset);
    try
      i:=0;
      while i<(PacketStrings.Count-1) do
      begin //iterate pairs of string for extensions
        if PacketStrings[i]='newline' then
        begin
          FEndOfLine:=PacketStrings[i+1];
          Break;
        end;
        Inc(i,2);
      end;
    finally
      PacketStrings.Free;
    end;
  end;
  FCurrentDir:=SetCurrentDir(FCurrentDir);
end;

function TSimpleSFTP.SetCurrentDir(DirName:string):string;
var DirHandle:string;
begin
  if DirName='' then DirName:='.';
  //if DirName not started from '/' then build fill dir name: FCurrentDir+'/'+DirName
  if Copy(DirName,1,Length(FRemotePathSeparator))<>FRemotePathSeparator then
    DirName:=ValidateRemoteDirName(FCurrentDir)+DirName;
  Result:=SetRealPath(DirName);
  //try open new dir (just to check if it exists)
  DirHandle:=OpenDir(Result);
  CloseDir(DirHandle);
  FCurrentDir:=Result;
end;

function TSimpleSFTP.GetCurrentDir:string;
begin
  Result:=SetCurrentDir('.');
end;

function TSimpleSFTP.SetRealPath(DirName:string):string;
var PacketType:BYTE;PacketString,PacketData:string;FileList:TSFTPFileList;
begin
  PacketType:=SSH_FXP_REALPATH;
  DirName:=DirName;
  PacketString:=BuildPacket(PacketType,[@DirName[1]],[Length(DirName)],[False]);
  SendPacket(PacketString);
  PacketType:=0;
  PacketData:=ReceivePacket(FRequestID-1,PacketType,True);
  CheckStatus(PacketType,PacketData,STRING_UNABLETOSETPATH+': '+DirName);
  if PacketType<>SSH_FXP_NAME then DoError(STRING_UNABLETOSETPATH+': '+DirName);
  FileList:=TSFTPFileList.Create;
  try
    ParseFileNamePacket(FileList,PacketData,False);
    if FileList.Count=0 then DoError(STRING_UNABLETOSETPATH+': '+DirName);
    Result:=FileList[0].FileName;
  finally
    FileList.Free;
  end;  
end;

function TSimpleSFTP.FileExists(FileName:string):Boolean;
var Attributes:TSFTPFileAttributes;FileOpenFlags:DWORD;FileHandle:string;
begin //catches exception !
  try
    if FProtocolVersion>3 then FileOpenFlags:=SSH_FXF_OPEN_EXISTING
      else FileOpenFlags:=SSH_FXF_READ;
    FileHandle:=OpenFile(FileName,FileOpenFlags);
    CloseFile(FileHandle);
    Result:=True;
  except
    Result:=False;
  end;
end;

function TSimpleSFTP.GetFileSizeByHandle(FileHandle:string):Int64;
var Attributes:TSFTPFileAttributes;
begin
  GetFileAtributesByHandle(FileHandle,Attributes);
  if (Attributes.valid_attribute_flags and SSH_FILEXFER_ATTR_SIZE)<>0 then Result:=Attributes.size
    else DoError(STRING_UNABLETOGETFILESIZE);
end;

function TSimpleSFTP.GetFileSize(FileName:string):Int64;
var Attributes:TSFTPFileAttributes;
begin
  GetFileAtributes(FileName,Attributes);
  if (Attributes.valid_attribute_flags and SSH_FILEXFER_ATTR_SIZE)<>0 then Result:=Attributes.size
    else DoError(STRING_UNABLETOGETFILESIZE);
end;

procedure TSimpleSFTP.GetFileTimes(FileName:string;var AccessTime,CreateTime,ModifyTime:Int64);
var Attributes:TSFTPFileAttributes;
begin
  GetFileAtributes(FileName,Attributes);
  AccessTime:=0;
  CreateTime:=0;
  ModifyTime:=0;
  if FProtocolVersion>3 then
  begin
    if (Attributes.valid_attribute_flags and SSH_FILEXFER_ATTR_ACCESSTIME)<>0 then
      AccessTime:=Attributes.atime;
    if (Attributes.valid_attribute_flags and SSH_FILEXFER_ATTR_CREATETIME)<>0 then
      CreateTime:=Attributes.createtime;
    if (Attributes.valid_attribute_flags and SSH_FILEXFER_ATTR_MODIFYTIME)<>0 then
      ModifyTime:=Attributes.mtime;
  end
  else
  begin
    if (Attributes.valid_attribute_flags and SSH_FILEXFER_ATTR_ACCESSTIME)<>0 then
    begin
      AccessTime:=Attributes.atime;
      ModifyTime:=Attributes.mtime;
    end;
  end;
end;

procedure TSimpleSFTP.GetFileTimesByHandle(FileHandle:string;var AccessTime,CreateTime,ModifyTime:Int64);
var Attributes:TSFTPFileAttributes;
begin
  GetFileAtributesByHandle(FileHandle,Attributes);
  AccessTime:=0;
  CreateTime:=0;
  ModifyTime:=0;
  if FProtocolVersion>3 then
  begin
    if (Attributes.valid_attribute_flags and SSH_FILEXFER_ATTR_ACCESSTIME)<>0 then
      AccessTime:=Attributes.atime;
    if (Attributes.valid_attribute_flags and SSH_FILEXFER_ATTR_CREATETIME)<>0 then
      CreateTime:=Attributes.createtime;
    if (Attributes.valid_attribute_flags and SSH_FILEXFER_ATTR_MODIFYTIME)<>0 then
      ModifyTime:=Attributes.mtime;
  end
  else
  begin
    if (Attributes.valid_attribute_flags and SSH_FILEXFER_ATTR_ACCESSTIME)<>0 then
    begin
      AccessTime:=Attributes.atime;
      ModifyTime:=Attributes.mtime;
    end;
  end;
end;

function TSimpleSFTP.ValidateRemoteDirName(RemoteDir:string):string;
begin //just add trailing '/' if needed
  if (RemoteDir<>'') and (Copy(RemoteDir,Length(RemoteDir)-Length(FRemotePathSeparator)+1,
    Length(FRemotePathSeparator))<>FRemotePathSeparator)
    then Result:=RemoteDir+FRemotePathSeparator else Result:=RemoteDir;
end;

function TSimpleSFTP.PutFile(LocalFileName,RemoteDir:string;PreserveFileTimes:Boolean=True;Overwrite:Boolean=True;
  Append:Boolean=False;SourceStartPos:Int64=0;Callback:TSimpleSFTPProgressCallback=nil;UserData:Pointer=nil):Int64;
var FileHandle,Buffer:string;FileStream:TFileStream;CurrentRemoteOffset,TotalSize:Int64;CurRead:Integer;
  FileOpenFlags:DWORD;AccessTime,CreateTime,ModifyTime:Int64;RemoteFileName:string;
begin //returns sent size (indeed if sent size is not file size then raise exception :-) )
  Result:=0;
  FileOpenFlags:=0;
  if FProtocolVersion>3 then
  begin
    if Overwrite then FileOpenFlags:=(FileOpenFlags or SSH_FXF_CREATE_NEW)
      else FileOpenFlags:=(FileOpenFlags or SSH_FXF_OPEN_OR_CREATE);
    if Append and not Overwrite then FileOpenFlags:=FileOpenFlags or SSH_FXF_ACCESS_APPEND_DATA;
  end
  else
  begin //as in PuTTY
    FileOpenFlags:=SSH_FXF_WRITE;
    if Overwrite then FileOpenFlags:=FileOpenFlags or SSH_FXF_WRITE or SSH_FXF_CREAT;
    if not Append then FileOpenFlags:=FileOpenFlags or SSH_FXF_TRUNC;
  end;
  RemoteFileName:=ValidateRemoteDirName(RemoteDir)+ExtractFileName(LocalFileName);
  //open remote file
  FileHandle:=OpenFile(RemoteFileName,FileOpenFlags);
  try
    if Append then CurrentRemoteOffset:=GetFileSizeByHandle(FileHandle) else CurrentRemoteOffset:=0;
    if FBufferSize<=0 then DoError(STRING_INVALIDBUFFERSIZE);
    SetLength(Buffer,FBufferSize);
    //open local file
    FileStream:=TFileStream.Create(LocalFileName,fmOpenRead or fmShareDenyNone);
    try
      TotalSize:=FileStream.Size-SourceStartPos;
      //local file offset
      FileStream.Seek(SourceStartPos,soFromBeginning);
      if FileStream.Position<>SourceStartPos then
        DoError(STRING_INVALIDFILEPOS+': '+LocalFileName+' ('+IntToStr(SourceStartPos)+')');
      while Result<TotalSize do
      begin
        //read local file
        CurRead:=FileStream.Read(Buffer[1],FBufferSize);
        if CurRead>0 then //write remote file
          WriteFile(FileHandle,CurrentRemoteOffset,@Buffer[1],CurRead);
        CurrentRemoteOffset:=CurrentRemoteOffset+CurRead;
        Result:=Result+CurRead;
        if CurRead<FBufferSize then Break;
        //call progress procedure and abort if it returns false
        if Assigned(Callback) and not Callback(UserData,Result,TotalSize) then
          DoError(STRING_FILETRANSFERABORTED+': '+LocalFileName);
      end;
      //end of remote file (send empty data packet) - it seems not necessary
      WriteFile(FileHandle,CurrentRemoteOffset,@Buffer[1],0);
    finally
      FileStream.Free;
    end;
  finally
    try
      CloseFile(FileHandle);
    except
    end;  
  end;
  if PreserveFileTimes then
  begin
    GetLocalFileTimes(LocalFileName,AccessTime,CreateTime,ModifyTime);
    SetFileTimes(RemoteFileName,AccessTime,CreateTime,ModifyTime);
  end;
end;

function TSimpleSFTP.GetFile(RemoteDir,RemoteFileName,LocalFileName:string;PreserveFileTimes:Boolean=True;Overwrite:Boolean=True;
  Append:Boolean=False;SourceStartPos:Int64=0;Callback:TSimpleSFTPProgressCallback=nil;UserData:Pointer=nil):Int64;
var FileHandle,Buffer:string;FileStream:TFileStream;CurrentRemoteOffset,TotalSize:Int64;
  CurRead,CurNeedRead:Integer;FileOpenFlags:DWORD;AccessTime,CreateTime,ModifyTime:Int64;
begin //returns received size (transfer breaking on server's EOF or small received buffer size)
  Result:=0;
  FileOpenFlags:=0;
  if FProtocolVersion>3 then FileOpenFlags:=SSH_FXF_OPEN_EXISTING
    else FileOpenFlags:=SSH_FXF_READ;
  RemoteFileName:=ValidateRemoteDirName(RemoteDir)+RemoteFileName;
  //open remote file
  FileHandle:=OpenFile(RemoteFileName,FileOpenFlags);
  try
    if FBufferSize<=0 then DoError(STRING_INVALIDBUFFERSIZE);
    FileOpenFlags:=0;
    if Overwrite then
    begin
      SysUtils.DeleteFile(LocalFileName);
      FileOpenFlags:=fmCreate;
    end
    else FileOpenFlags:=fmOpenWrite;
    //open local file
    FileStream:=TFileStream.Create(LocalFileName,FileOpenFlags);
    try
      //local file offset
      if Append and not Overwrite then FileStream.Seek(0,soFromEnd);
      //remote file size
      TotalSize:=GetFileSizeByHandle(FileHandle)-SourceStartPos;
      if TotalSize<0 then DoError(STRING_INVALIDFILEPOS+': '+RemoteFileName+' ('+IntToStr(SourceStartPos)+')');
      while Result<TotalSize do
      begin
        //read remote file
        Buffer:=ReadFile(FileHandle,SourceStartPos,FBufferSize);
        CurRead:=Length(Buffer);
        if CurRead>0 then //write local file
          FileStream.Write(Buffer[1],CurRead);
        Inc(SourceStartPos,CurRead);
        Inc(Result,CurRead);
        if CurRead<FBufferSize then Break;
        if Assigned(Callback) and not Callback(UserData,Result,TotalSize) then
          DoError(STRING_FILETRANSFERABORTED+': '+RemoteFileName);
      end;
    finally
      FileStream.Free;
    end;
  finally
    try
      CloseFile(FileHandle);
    except
    end;  
  end;
  if PreserveFileTimes then
  begin
    GetFileTimes(RemoteFileName,AccessTime,CreateTime,ModifyTime);
    SetLocalFileTimes(LocalFileName,AccessTime,CreateTime,ModifyTime);
  end;
end;

function TSimpleSFTP.OpenFile(FileName:string;FileOpenFlags:DWORD):string;
var FileAccess,FileAccessSize:DWORD;PacketType:BYTE;Atributes:TSFTPFileAttributes;
  AtributesString,PacketString,PacketData:string;FieldOffset:Integer;
begin //returns file handle
  PacketType:=SSH_FXP_OPEN;
  if FProtocolVersion>3 then FileAccessSize:=SizeOf(FileAccess) else FileAccessSize:=0;
  FileAccess:=ACE4_WRITE_DATA+ACE4_WRITE_ATTRIBUTES;
  AtributesString:=BuildBlankAttributesString;
  FileAccess:=InvertDWORD(FileAccess);
  FileOpenFlags:=InvertDWORD(FileOpenFlags);
  PacketString:=BuildPacket(PacketType,[@FileName[1],@FileAccess,@FileOpenFlags,@AtributesString[1]],
    [Length(FileName),FileAccessSize,SizeOf(FileOpenFlags),Length(AtributesString)],
    [False,True,True,True]);
  SendPacket(PacketString);
  PacketType:=0;
  PacketData:=ReceivePacket(FRequestID-1,PacketType);
  CheckStatus(PacketType,PacketData,STRING_UNABLETOOPENFILE+': '+FileName);
  if (PacketType<>SSH_FXP_HANDLE) then DoError(STRING_UNABLETOOPENFILE+': '+FileName);
  //get file handle
  FieldOffset:=1;
  Result:=GetStringPacketField(PacketData,FieldOffset);
end;

function TSimpleSFTP.OpenDir(DirName:string):string;
var PacketType:BYTE;PacketString,PacketData:string;FieldOffset:Integer;
begin
  PacketType:=SSH_FXP_OPENDIR;
  PacketString:=BuildPacket(PacketType,[@DirName[1]],[Length(DirName)],[False]);
  SendPacket(PacketString);
  PacketType:=0;
  PacketData:=ReceivePacket(FRequestID-1,PacketType);
  CheckStatus(PacketType,PacketData,STRING_UNABLETOOPENDIR+': '+DirName);
  if (PacketType<>SSH_FXP_HANDLE) then DoError(STRING_UNABLETOOPENDIR+': '+DirName);
  //get dir handle
  FieldOffset:=1;
  Result:=GetStringPacketField(PacketData,FieldOffset);
end;

function TSimpleSFTP.CloseFile(FileHandle:string):Boolean;
var PacketType:BYTE;PacketString,PacketData:string;
begin
  PacketType:=SSH_FXP_CLOSE;
  PacketString:=BuildPacket(PacketType,[@FileHandle[1]],[Length(FileHandle)],[False]);
  SendPacket(PacketString);
  PacketType:=0;
  PacketData:=ReceivePacket(FRequestID-1,PacketType);
  CheckStatus(PacketType,PacketData,STRING_UNABLETOCLOSEHANDLE+': '+FileHandle);
  Result:=True;
end;

function TSimpleSFTP.CloseDir(DirHandle:string):Boolean;
begin
  Result:=CloseFile(DirHandle);
end;

function TSimpleSFTP.ReadFile(FileHandle:string;FileOffset:Int64;ReadSize:DWORD):string;
var PacketType:BYTE;PacketString,PacketData:string;FieldOffset:Integer;
begin
  Result:='';
  PacketType:=SSH_FXP_READ;
  FileOffset:=InvertInt64(FileOffset);
  ReadSize:=InvertDWORD(ReadSize);
  PacketString:=BuildPacket(PacketType,[@FileHandle[1],@FileOffset,@ReadSize],
    [Length(FileHandle),SizeOf(FileOffset),SizeOf(ReadSize)],[False,True,True]);
  SendPacket(PacketString);
  PacketType:=0;
  PacketData:=ReceivePacket(FRequestID-1,PacketType);
  case PacketType of
  SSH_FXP_STATUS:
  begin //server can say "EOF" or error
    if GetStatus(PacketData)=SSH_FX_EOF then Exit
      else CheckStatus(PacketType,PacketData,STRING_UNABLETOREADFILE+': '+FileHandle);
  end;    
  SSH_FXP_DATA:
  begin 
    FieldOffset:=1;
    Result:=GetStringPacketField(PacketData,FieldOffset);
  end;
  else DoError(STRING_UNABLETOREADFILE+': '+FileHandle);
  end;
end;

procedure TSimpleSFTP.ReadDir(DirHandle:string;FileList:TSFTPFileList);
var PacketType:BYTE;PacketString,PacketData:string;
begin
  FileList.Clear;
  PacketType:=SSH_FXP_READDIR;
  PacketString:=BuildPacket(PacketType,[@DirHandle[1]],[Length(DirHandle)],[False]);
  while True do
  begin
    SendPacket(PacketString);
    PacketType:=0;
    PacketData:=ReceivePacket(FRequestID-1,PacketType);
    case PacketType of
    SSH_FXP_STATUS:
    begin  //server can say "EOF" or error
      if GetStatus(PacketData)=SSH_FX_EOF then Break
        else CheckStatus(PacketType,PacketData,STRING_UNABLETOREADDIR+': '+DirHandle);
    end;
    SSH_FXP_NAME:
    begin //PacketData can contain 1 or more file info
      ParseFileNamePacket(FileList,PacketData);
    end;
    else DoError(STRING_UNABLETOREADDIR+': '+DirHandle+' ('+STRING_UNEXPECTEDPACKETTYPE+
      ' '+IntToStr(PacketType)+')');
    end;
  end;
end;

function TSimpleSFTP.ParseFileNamePacket(FileList:TSFTPFileList;PacketData:string;
  ProcessAttributes:Boolean=True):Integer;
var NameCount,i:DWORD;FileAttributes:TSFTPFileAttributes;FileName,LongName:string;FieldOffset:Integer;
begin //returns count of file records added to FileList
  Result:=0;
  FieldOffset:=1;
  //get file record count
  GetFixedPacketField(PacketData,FieldOffset,@NameCount,SizeOf(NameCount));
  //get names and attributes
  for i:=1 to NameCount do
  begin
    FileName:=GetStringPacketField(PacketData,FieldOffset);
    if FProtocolVersion<=3 then LongName:=GetStringPacketField(PacketData,FieldOffset);
    if ProcessAttributes then FileAttributes:=ParseFileAttributes(PacketData,FieldOffset);
    FileAttributes.FileName:=FileName;
    if (FileAttributes.permissions and S_IFMT)<>0 then
    begin //we trying to check file type bits in permissions, but it seems to contain no file type bits
      if (FileAttributes.permissions and S_IFLNK)<>0 then FileAttributes.file_type:=SSH_FILEXFER_TYPE_SYMLINK
        else if (FileAttributes.permissions and S_IFREG)<>0 then FileAttributes.file_type:=SSH_FILEXFER_TYPE_REGULAR
          else if (FileAttributes.permissions and S_IFDIR)<>0 then FileAttributes.file_type:=SSH_FILEXFER_TYPE_DIRECTORY
            else FileAttributes.file_type:=SSH_FILEXFER_TYPE_UNKNOWN;
    end
    else
    begin
      if (FProtocolVersion<=3) and (LongName<>'') then
      begin //try to parse long file name (assumed it has the form of "ls -l" listing);
        FileAttributes.LongName:=LongName;
        case LongName[1] of //just simple file_type emulation :-) (it works with my server)
        '-': FileAttributes.file_type:=SSH_FILEXFER_TYPE_REGULAR;
        'd': FileAttributes.file_type:=SSH_FILEXFER_TYPE_DIRECTORY;
        'l': FileAttributes.file_type:=SSH_FILEXFER_TYPE_SYMLINK;
        '/': FileAttributes.file_type:=SSH_FILEXFER_TYPE_DIRECTORY;//in SetRealPath
        else FileAttributes.file_type:=SSH_FILEXFER_TYPE_UNKNOWN;
        end;
      end;  
    end;  
    FileList.Add(FileAttributes);
    Inc(Result);
  end;
end;

procedure ResetFileAttributes(FileAttributes:PSFTPFileAttributes);
begin
  with FileAttributes^ do
  begin
    FileName:='';
    LongName:='';
    valid_attribute_flags:=0;
    file_type:=0;//                always present (not in version 3)
    size:=0;//                     present only if flag SIZE
    allocation_size:=0;//          present only if flag ALLOCATION_SIZE
    owner:='';//                   present only if flag OWNERGROUP
    group:='';//                   present only if flag OWNERGROUP
    permissions:=0;//              present only if flag PERMISSIONS
    atime:=0;//                    present only if flag ACCESSTIME
    atime_nseconds:=0;//           present only if flag SUBSECOND_TIMES
    createtime:=0;//               present only if flag CREATETIME
    createtime_nseconds:=0;//      present only if flag SUBSECOND_TIMES
    mtime:=0;//                    present only if flag MODIFYTIME
    mtime_nseconds:=0;//           present only if flag SUBSECOND_TIMES
    acl:='';//                     present only if flag ACL
    attrib_bits:=0;//              present only if flag BITS
    text_hint:=0;//                present only if flag TEXT_HINT
    mime_type:='';//               present only if flag MIME_TYPE
    link_count:=0;//               present only if flag LINK_COUNT
    untranslated_name:='';//       present only if flag UNTRANSLATED_NAME
    extended_count:=0;//           present only if flag EXTENDED
//    extended_type:string;
//    extended_data:string;
  end;
end;

function TSimpleSFTP.ParseFileAttributes(AtributesString:string;var FieldOffset:Integer):TSFTPFileAttributes;
var TmpInt64:Int64;TmpDWORD:DWORD;TmpString:string;i:Integer;
  procedure CopyFixedAttribute(CopyFlag:DWORD;CopyTo:Pointer;CopySize:Integer);
  begin
    if (Result.valid_attribute_flags and CopyFlag)<>0 then
      GetFixedPacketField(AtributesString,FieldOffset,CopyTo,CopySize);
  end;
  procedure CopyStringAttribute(CopyFlag:DWORD;var CopyTo:string);
  begin
    if (Result.valid_attribute_flags and CopyFlag)<>0 then
      CopyTo:=GetStringPacketField(AtributesString,FieldOffset);
  end;
begin //version 3 parsing - like in PuTTY
  ResetFileAttributes(@Result);
  with Result do
  begin
    GetFixedPacketField(AtributesString,FieldOffset,@valid_attribute_flags,SizeOf(valid_attribute_flags));
    if FProtocolVersion>3 then
      GetFixedPacketField(AtributesString,FieldOffset,@file_type,SizeOf(file_type));
    CopyFixedAttribute(SSH_FILEXFER_ATTR_SIZE,@size,SizeOf(size));
    if FProtocolVersion<=3 then
      CopyFixedAttribute(2,@TmpInt64,SizeOf(TmpInt64));
    if FProtocolVersion>3 then
    begin
      CopyFixedAttribute(SSH_FILEXFER_ATTR_ALLOCATION_SIZE,@allocation_size,SizeOf(allocation_size));
      CopyStringAttribute(SSH_FILEXFER_ATTR_OWNERGROUP,owner);
      CopyStringAttribute(SSH_FILEXFER_ATTR_OWNERGROUP,group);
    end;
    CopyFixedAttribute(SSH_FILEXFER_ATTR_PERMISSIONS,@permissions,SizeOf(permissions));
    if FProtocolVersion>3 then
    begin
      CopyFixedAttribute(SSH_FILEXFER_ATTR_ACCESSTIME,@atime,SizeOf(atime));
      CopyFixedAttribute(SSH_FILEXFER_ATTR_ACCESSTIME or SSH_FILEXFER_ATTR_SUBSECOND_TIMES,@atime_nseconds,SizeOf(atime_nseconds));
      CopyFixedAttribute(SSH_FILEXFER_ATTR_CREATETIME,@createtime,SizeOf(createtime));
      CopyFixedAttribute(SSH_FILEXFER_ATTR_CREATETIME or SSH_FILEXFER_ATTR_SUBSECOND_TIMES,@createtime_nseconds,SizeOf(createtime_nseconds));
      CopyFixedAttribute(SSH_FILEXFER_ATTR_MODIFYTIME,@mtime,SizeOf(mtime));
      CopyFixedAttribute(SSH_FILEXFER_ATTR_MODIFYTIME or SSH_FILEXFER_ATTR_SUBSECOND_TIMES,@mtime_nseconds,SizeOf(mtime_nseconds));
    end
    else
    begin
      TmpDWORD:=0;
      CopyFixedAttribute(SSH_FILEXFER_ATTR_ACCESSTIME,@TmpDWORD,SizeOf(TmpDWORD));
      atime:=TmpDWORD;
      TmpDWORD:=0;
      CopyFixedAttribute(SSH_FILEXFER_ATTR_ACCESSTIME,@TmpDWORD,SizeOf(TmpDWORD));
      mtime:=TmpDWORD;
    end;
    if FProtocolVersion>3 then
    begin
      CopyStringAttribute(SSH_FILEXFER_ATTR_ACL,acl);
      if FProtocolVersion>4 then
        CopyFixedAttribute(SSH_FILEXFER_ATTR_BITS,@attrib_bits,SizeOf(attrib_bits));
      CopyFixedAttribute(SSH_FILEXFER_ATTR_TEXT_HINT,@text_hint,SizeOf(text_hint));
      CopyStringAttribute(SSH_FILEXFER_ATTR_MIME_TYPE,mime_type);
      CopyFixedAttribute(SSH_FILEXFER_ATTR_LINK_COUNT,@link_count,SizeOf(link_count));
      CopyStringAttribute(SSH_FILEXFER_ATTR_UNTRANLATED_NAME,untranslated_name);
    end;
    extended_count:=0;
    CopyFixedAttribute(SSH_FILEXFER_ATTR_EXTENDED,@extended_count,SizeOf(extended_count));
    for i:=1 to extended_count do
    begin //parsed but not used
      GetStringPacketField(AtributesString,FieldOffset);//extended_type
      GetStringPacketField(AtributesString,FieldOffset);//extended_data
    end;
  end;
end;

function TSimpleSFTP.BuildBlankAttributesString(IsDir:Boolean=False):string;
var FileAttributes:TSFTPFileAttributes;
begin
  ResetFileAttributes(@FileAttributes);
  if IsDir then FileAttributes.file_type:=SSH_FILEXFER_TYPE_DIRECTORY
    else FileAttributes.file_type:=SSH_FILEXFER_TYPE_REGULAR;
  Result:=BuildAttributesString(@FileAttributes);
end;

function TSimpleSFTP.BuildAttributesString(FileAttributes:PSFTPFileAttributes):string;
var TmpInt64:Int64;TmpDWORD:DWORD;
  procedure AddFixedAttributeString(CopyFlag:DWORD;CurAttribute:Pointer;CurSize:Integer);
  begin
    if (FileAttributes^.valid_attribute_flags and CopyFlag)<>0 then
      Result:=Result+PutFixedPacketField(CurAttribute,CurSize);
  end;
  procedure AddStringAttributeString(CopyFlag:DWORD;CurAttribute:string);
  begin
    if (FileAttributes^.valid_attribute_flags and CopyFlag)<>0 then
      Result:=Result+PutStringPacketField(CurAttribute);
  end;
begin //version 3 - like in PuTTY
  Result:='';
  with FileAttributes^ do
  begin
    Result:=Result+PutFixedPacketField(@valid_attribute_flags,SizeOf(valid_attribute_flags));
    if FProtocolVersion>3 then
      Result:=Result+PutFixedPacketField(@file_type,SizeOf(file_type));
    AddFixedAttributeString(SSH_FILEXFER_ATTR_SIZE,@size,SizeOf(size));
    if FProtocolVersion<=3 then
      AddFixedAttributeString(2,@TmpInt64,SizeOf(TmpInt64));
    if FProtocolVersion>3 then
    begin
      AddFixedAttributeString(SSH_FILEXFER_ATTR_ALLOCATION_SIZE,@allocation_size,SizeOf(allocation_size));
      AddStringAttributeString(SSH_FILEXFER_ATTR_OWNERGROUP,owner);
      AddStringAttributeString(SSH_FILEXFER_ATTR_OWNERGROUP,group);
    end;  
    AddFixedAttributeString(SSH_FILEXFER_ATTR_PERMISSIONS,@permissions,SizeOf(permissions));
    if FProtocolVersion>3 then
    begin
      AddFixedAttributeString(SSH_FILEXFER_ATTR_ACCESSTIME,@atime,SizeOf(atime));
      AddFixedAttributeString(SSH_FILEXFER_ATTR_ACCESSTIME or SSH_FILEXFER_ATTR_SUBSECOND_TIMES,@atime_nseconds,SizeOf(atime_nseconds));
      AddFixedAttributeString(SSH_FILEXFER_ATTR_CREATETIME,@createtime,SizeOf(createtime));
      AddFixedAttributeString(SSH_FILEXFER_ATTR_CREATETIME or SSH_FILEXFER_ATTR_SUBSECOND_TIMES,@createtime_nseconds,SizeOf(createtime_nseconds));
      AddFixedAttributeString(SSH_FILEXFER_ATTR_MODIFYTIME,@mtime,SizeOf(mtime));
      AddFixedAttributeString(SSH_FILEXFER_ATTR_MODIFYTIME or SSH_FILEXFER_ATTR_SUBSECOND_TIMES,@mtime_nseconds,SizeOf(mtime_nseconds));
    end
    else
    begin
      TmpDWORD:=atime;
      AddFixedAttributeString(SSH_FILEXFER_ATTR_ACCESSTIME,@TmpDWORD,SizeOf(TmpDWORD));
      TmpDWORD:=mtime;
      AddFixedAttributeString(SSH_FILEXFER_ATTR_ACCESSTIME,@TmpDWORD,SizeOf(TmpDWORD));
    end;
    if FProtocolVersion>3 then
    begin
      AddStringAttributeString(SSH_FILEXFER_ATTR_ACL,acl);
      AddFixedAttributeString(SSH_FILEXFER_ATTR_BITS,@attrib_bits,SizeOf(attrib_bits));
      AddFixedAttributeString(SSH_FILEXFER_ATTR_TEXT_HINT,@text_hint,SizeOf(text_hint));
      AddStringAttributeString(SSH_FILEXFER_ATTR_MIME_TYPE,mime_type);
      AddFixedAttributeString(SSH_FILEXFER_ATTR_LINK_COUNT,@link_count,SizeOf(link_count));
      AddStringAttributeString(SSH_FILEXFER_ATTR_UNTRANLATED_NAME,untranslated_name);
    end;
    extended_count:=0;
    AddFixedAttributeString(SSH_FILEXFER_ATTR_EXTENDED,@extended_count,SizeOf(extended_count));
  end;
end;

procedure TSimpleSFTP.WriteFile(FileHandle:string;FileOffset:Int64;FileData:Pointer;DataSize:DWORD);
var PacketType:BYTE;PacketString,PacketData:string;InvertedDataSize:DWORD;
begin
  PacketType:=SSH_FXP_WRITE;
  FileOffset:=InvertInt64(FileOffset);
  InvertedDataSize:=InvertDWORD(DataSize);
  PacketString:=BuildPacket(PacketType,[@FileHandle[1],@FileOffset,@InvertedDataSize,FileData],
    [Length(FileHandle),SizeOf(FileOffset),SizeOf(DataSize),DataSize],[False,True,True,True]);
  SendPacket(PacketString);
  PacketType:=SSH_FXP_STATUS;
  PacketData:=ReceivePacket(FRequestID-1,PacketType);
  CheckStatus(PacketType,PacketData,STRING_UNABLETOWRITETOFILE+': '+FileHandle);
end;

procedure TSimpleSFTP.DeleteFile(FileName:string);
var PacketType:BYTE;PacketString,PacketData:string;
begin
  PacketType:=SSH_FXP_REMOVE;
  PacketString:=BuildPacket(PacketType,[@FileName[1]],[Length(FileName)],[False]);
  SendPacket(PacketString);
  PacketType:=SSH_FXP_STATUS;
  PacketData:=ReceivePacket(FRequestID-1,PacketType);
  CheckStatus(PacketType,PacketData,STRING_UNABLETODELETEFILE+': '+FileName);
end;

procedure TSimpleSFTP.RenameFile(OldName,NewName:string;FailIfExists:Boolean);
var PacketType:BYTE;PacketString,PacketData:string;RenameFlags:DWORD;
begin
  PacketType:=SSH_FXP_RENAME;
  RenameFlags:=SSH_FXP_RENAME_NATIVE;
  if not FailIfExists then RenameFlags:=RenameFlags+SSH_FXP_RENAME_OVERWRITE;
  RenameFlags:=InvertDWORD(RenameFlags);
  PacketString:=BuildPacket(PacketType,[@OldName[1],@NewName[1],@RenameFlags],
    [Length(OldName),Length(NewName),SizeOf(RenameFlags)],[False,False,True]);
  SendPacket(PacketString);
  PacketType:=SSH_FXP_STATUS;
  PacketData:=ReceivePacket(FRequestID-1,PacketType);
  CheckStatus(PacketType,PacketData,STRING_UNABLETORENAMEFILE+': '+OldName+
    ' -> '+NewName);
end;

procedure TSimpleSFTP.CreateDir(DirName:string;Attributes:PSFTPFileAttributes=nil);
var PacketType:BYTE;PacketString,AttributesString,PacketData:string;
begin
  PacketType:=SSH_FXP_MKDIR;
  if Assigned(Attributes) then AttributesString:=BuildAttributesString(Attributes)
    else AttributesString:=BuildBlankAttributesString(True);
  PacketString:=BuildPacket(PacketType,[@DirName[1],@AttributesString[1]],
    [Length(DirName),Length(AttributesString)],[False,True]);
  SendPacket(PacketString);
  PacketType:=SSH_FXP_STATUS;
  PacketData:=ReceivePacket(FRequestID-1,PacketType);
  CheckStatus(PacketType,PacketData,STRING_UNABLETOCREATEDIR+': '+DirName);
end;

procedure TSimpleSFTP.DeleteDir(DirName:string);
var PacketType:BYTE;PacketString,PacketData:string;
begin
  PacketType:=SSH_FXP_RMDIR;
  PacketString:=BuildPacket(PacketType,[@DirName[1]],[Length(DirName)],[False]);
  SendPacket(PacketString);
  PacketType:=SSH_FXP_STATUS;
  PacketData:=ReceivePacket(FRequestID-1,PacketType);
  CheckStatus(PacketType,PacketData,STRING_UNABLETODELETEDIR+': '+DirName);
end;

procedure TSimpleSFTP.ListDir(DirName:string;FileList:TSFTPFileList);
var DirHandle:string;
begin
  DirHandle:=OpenDir(DirName);
  try
    ReadDir(DirHandle,FileList);
  finally
    try
      CloseDir(DirHandle);
    except
    end;  
  end;
end;

procedure TSimpleSFTP.InternalGetFileAtributes(PacketType:BYTE;FileID:string;//name or handle
  AttributeFlags:DWORD;var Attributes:TSFTPFileAttributes);
var PacketString,AttributesString,PacketData:string;FieldOffset,AttributeFlagsSize:Integer;
begin
  if FProtocolVersion>3 then AttributeFlagsSize:=SizeOf(AttributeFlags) else AttributeFlagsSize:=0;
  AttributeFlags:=InvertDWORD(AttributeFlags);
  PacketString:=BuildPacket(PacketType,[@FileID[1],@AttributeFlags],
    [Length(FileID),AttributeFlagsSize],[False,True]);
  SendPacket(PacketString);
  PacketType:=0;
  PacketData:=ReceivePacket(FRequestID-1,PacketType);
  CheckStatus(PacketType,PacketData,STRING_UNABLETOGETFILEATTRIBUTES+': '+FileID);
  if PacketType<>SSH_FXP_ATTRS then DoError(STRING_UNABLETOGETFILEATTRIBUTES+': '+FileID);
  FieldOffset:=1;
  Attributes:=ParseFileAttributes(PacketData,FieldOffset);
end;

procedure TSimpleSFTP.GetFileAtributes(FileName:string;var Attributes:TSFTPFileAttributes;FollowLink:Boolean=True);
var PacketType:BYTE;AttributeFlags:DWORD;
begin
  if FollowLink then PacketType:=SSH_FXP_STAT else PacketType:=SSH_FXP_LSTAT;
  AttributeFlags:=SSH_FILEXFER_ATTR_SIZE or SSH_FILEXFER_ATTR_PERMISSIONS
    or SSH_FILEXFER_ATTR_ACCESSTIME;
  InternalGetFileAtributes(PacketType,FileName,AttributeFlags,Attributes);
end;

procedure TSimpleSFTP.GetFileAtributesByHandle(FileHandle:string;var Attributes:TSFTPFileAttributes);
var AttributeFlags:DWORD;
begin
  AttributeFlags:=SSH_FILEXFER_ATTR_SIZE or SSH_FILEXFER_ATTR_PERMISSIONS
    or SSH_FILEXFER_ATTR_ACCESSTIME;
  InternalGetFileAtributes(SSH_FXP_FSTAT,FileHandle,AttributeFlags,Attributes);
end;

procedure TSimpleSFTP.SetFileAtributes(FileName:string;Attributes:PSFTPFileAttributes);
var PacketType:BYTE;PacketString,AttributesString,PacketData:string;
begin
  PacketType:=SSH_FXP_SETSTAT;
  AttributesString:=BuildAttributesString(Attributes);
  PacketString:=BuildPacket(PacketType,[@FileName[1],@AttributesString[1]],
    [Length(FileName),Length(AttributesString)],[False,True]);
  SendPacket(PacketString);
  PacketType:=SSH_FXP_STATUS ;
  PacketData:=ReceivePacket(FRequestID-1,PacketType);
  CheckStatus(PacketType,PacketData,STRING_UNABLETOSETFILEATTRIBUTES+': '+FileName);
end;

procedure TSimpleSFTP.SetFileAtributesByHandle(FileHandle:string;Attributes:PSFTPFileAttributes);
var PacketType:BYTE;PacketString,AttributesString,PacketData:string;
begin
  PacketType:=SSH_FXP_FSETSTAT;
  AttributesString:=BuildAttributesString(Attributes);
  PacketString:=BuildPacket(PacketType,[@FileHandle[1],@AttributesString[1]],
    [Length(FileHandle),Length(AttributesString)],[False,True]);
  SendPacket(PacketString);
  PacketType:=SSH_FXP_STATUS ;
  PacketData:=ReceivePacket(FRequestID-1,PacketType);
  CheckStatus(PacketType,PacketData,STRING_UNABLETOSETFILEATTRIBUTES+': '+FileHandle);
end;

procedure TSimpleSFTP.SetFileTimes(FileName:string;AccessTime,CreateTime,ModifyTime:Int64);
var PacketType:BYTE;PacketString,AttributesString,PacketData:string;Attributes:TSFTPFileAttributes;
begin
  ResetFileAttributes(@Attributes);
  Attributes.valid_attribute_flags:=SSH_FILEXFER_ATTR_ACCESSTIME;
  if FProtocolVersion>3 then
    Attributes.valid_attribute_flags:=Attributes.valid_attribute_flags or
      SSH_FILEXFER_ATTR_CREATETIME or SSH_FILEXFER_ATTR_MODIFYTIME;
  Attributes.atime:=AccessTime;
  Attributes.createtime:=CreateTime;
  Attributes.mtime:=ModifyTime;
  SetFileAtributes(FileName,@Attributes);
end;

procedure TSimpleSFTP.SetFileTimesByHandle(FileHandle:string;AccessTime,CreateTime,ModifyTime:Int64);
var PacketType:BYTE;PacketString,AttributesString,PacketData:string;Attributes:TSFTPFileAttributes;
begin
  ResetFileAttributes(@Attributes);
  Attributes.valid_attribute_flags:=SSH_FILEXFER_ATTR_ACCESSTIME;
  if FProtocolVersion>3 then
    Attributes.valid_attribute_flags:=Attributes.valid_attribute_flags or
      SSH_FILEXFER_ATTR_CREATETIME or SSH_FILEXFER_ATTR_MODIFYTIME;
  Attributes.atime:=AccessTime;
  Attributes.createtime:=CreateTime;
  Attributes.mtime:=ModifyTime;
  SetFileAtributesByHandle(FileHandle,@Attributes);
end;

function TSimpleSFTP.GetStatus(PacketData:string):DWORD;
begin //assumed PacketType=SSH_FXP_STATUS
  Result:=GetDWORD(@PacketData[1]);
end;

function TSimpleSFTP.CheckStatus(PacketType:DWORD;PacketData:string;ErrorString:string):Boolean;
var Status:DWORD;i,FieldOffset:Integer;
begin
  Result:=False;
  if PacketType<>SSH_FXP_STATUS then Exit;
  FieldOffset:=1;
  GetFixedPacketField(PacketData,FieldOffset,@Status,SizeOf(Status));
  if Status<>SSH_FX_OK then
  begin //expected strings with error description ?
    while FieldOffset<Length(PacketData) do
      ErrorString:=ErrorString+#13#10+GetStringPacketField(PacketData,FieldOffset);
    DoError(ErrorString);
  end
  else Result:=True;
end;

end.

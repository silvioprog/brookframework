{$MODE DELPHI}

Program testssl;

uses
  blcksock, synassl;

var
  sock: TTCPBlockSocket;
begin
  sock := TTCPBlockSocket.create;
  try
    sock.SSLEnabled:=True;
    writeln('Used OpenSSL library:');
    writeln(SSLLibFile);
    writeln(SSLUtilFile);
    sock.Connect(paramstr(1),paramstr(2));
    if sock.lasterror <> 0 then
    begin
      writeln('Error connecting!');
      exit;
    end;
    writeln;
    writeln('SSL version: ', sock.SSLGetSSLVersion);
    writeln('Cipher: ', sock.SSLGetCiphername);
    writeln('Cipher bits: ', sock.SSLGetCipherBits);
    writeln('Cipher alg. bits: ', sock.SSLGetCipherAlgBits);
    writeln('Certificate verify result: ', sock.SslGetVerifyCert);
    writeln('Certificate peer name: ', sock.SSLGetPeerName);
    writeln(sock.SSLGetCertInfo);
    sock.closesocket;
  finally
    sock.free;
  end;
end.


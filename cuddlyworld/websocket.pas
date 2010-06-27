{$MODE OBJFPC} { -*- text -*- }
{$INCLUDE settings.inc}
unit websocket;

interface

uses
   network, sysutils;

type
   TWebSocketState = (wsGET, wsURLPath, wsURLUsername, wsURLPassword,
                      wsFieldTrailingEnd, wsFieldEnd,
                      wsFieldNameStart, wsFieldName, wsFieldSeparator, wsFieldValue,
                      wsHandshakeEnd, wsHandshakeEndKey,
                      wsFrameType, wsTextData, wsError);

   TWebSocket = class(TNetworkSocket)
    protected
      FState: TWebSocketState;
      FBuffer: AnsiString;
      FUsername: AnsiString;
      FPassword: AnsiString;
      FOrigin: AnsiString;
      FHostName: AnsiString;
      FPort: Word;
      FResource: AnsiString;
      FCurrentFieldName, FCurrentFieldValue, FHandshakeKey1, FHandshakeKey2, FHandshakeKey3: AnsiString;
      function InternalRead(c: Byte): Boolean; override;
      function HandleMessage(s: AnsiString): Boolean; virtual; abstract;
      procedure WriteFrame(s: AnsiString);
      procedure Handshake(); virtual;
      procedure CheckField(); virtual;
    public
      constructor Create(AListener: TListenerSocket; AOrigin: AnsiString; AHostName: AnsiString; APort: Word; AResource: AnsiString);
      destructor Destroy(); override;
    end;

implementation

uses
   md5;

constructor TWebSocket.Create(AListener: TListenerSocket; AOrigin: AnsiString; AHostName: AnsiString; APort: Word; AResource: AnsiString);
begin
   inherited Create(AListener);
   FState := wsGET;
   FOrigin := AOrigin;
   FHostName := AHostName;
   FPort := APort;
   FResource := AResource;
end;

destructor TWebSocket.Destroy();
begin
   inherited;
end;

function TWebSocket.InternalRead(c: Byte): Boolean;
begin
   Result := True;
   case FState of
     wsGET:
      case c of
       $2F: FState := wsURLPath;
      end;
     wsURLPath:
      case c of
       $2F: FState := wsURLUsername;
      end;
     wsURLUsername:
      case c of
       Ord('a')..Ord('z'), Ord('A')..Ord('Z'): FUsername := FUsername + Chr(c);
       $2F: FState := wsURLPassword;
       else
          FState := wsError;
          Result := False;
      end;
     wsURLPassword:
      case c of
       Ord('a')..Ord('z'), Ord('A')..Ord('Z'): FPassword := FPassword + Chr(c);
       $20: FState := wsFieldTrailingEnd;
       else
          FState := wsError;
          Result := False;
      end;
     wsFieldTrailingEnd:
      case c of
       $0D: FState := wsFieldEnd;
      end;
     wsFieldEnd:
      case c of
       $0A: FState := wsFieldNameStart;
       else
          FState := wsError;
          Result := False;
      end;
     wsFieldNameStart:
      case c of
       $0D: FState := wsHandshakeEnd;
       Ord(':'): FState := wsFieldTrailingEnd;
       else
          FCurrentFieldName := Chr(c);
          FState := wsFieldName;
      end;
     wsFieldName:
      case c of
       Ord(':'): FState := wsFieldSeparator;
       $0D: FState := wsFieldEnd;
       else
          FCurrentFieldName := FCurrentFieldName + Chr(c);
      end;
     wsFieldSeparator:
      case c of
       Ord(' '): begin FState := wsFieldValue; FCurrentFieldValue := ''; end;
       $0D: FState := wsFieldEnd;
       else
          FState := wsError;
          Result := False;
      end;
     wsFieldValue:
      case c of
       $0D: begin FState := wsFieldEnd; CheckField(); end;
       else
          FCurrentFieldValue := FCurrentFieldValue + Chr(c);
      end;
     wsHandshakeEnd:
      case c of
       $0A: FState := wsHandshakeEndKey;
       else
          FState := wsError;
          Result := False;
      end;
     wsHandshakeEndKey:
      begin
         FHandshakeKey3 := FHandshakeKey3 + Chr(c);
         if (Length(FHandshakeKey3) >= 8) then
         begin
            Handshake();
            FState := wsFrameType;
         end;
      end;
     wsFrameType:
      case c of
       $00: begin FBuffer := ''; FState := wsTextData; end;
       else
          FState := wsError;
          Result := False;
      end;
     wsTextData:
      case c of
       $FF: begin Result := HandleMessage(FBuffer); FState := wsFrameType; end;
       else
          FBuffer := FBuffer + Chr(c);
      end;
     else Assert(False);
   end;
end;

procedure TWebSocket.Handshake();

   function GetKeyFromString(S: AnsiString): Cardinal;
   var
      NS: AnsiString;
      Index: Cardinal;
   begin
      NS := '';
      for Index := 1 to Length(S) do
         if (S[Index] in ['0'..'9']) then
            NS := NS + S[Index];
      Result := StrToQWord(NS); { there's no StrToCardinal() }
   end;

   function CountSpacesInString(S: AnsiString): Cardinal;
   var
      Index: Cardinal;
   begin
      Result := 0;
      for Index := 1 to Length(S) do
         if (S[Index] = ' ') then
            Inc(Result);
   end;

var
   KeyNumber1, KeyNumber2, Spaces1, Spaces2, Part1, Part2, Index: Cardinal;
   Challenge, Response: AnsiString;
   Digest: TMD5Digest;
begin
   KeyNumber1 := GetKeyFromString(FHandshakeKey1);
   KeyNumber2 := GetKeyFromString(FHandshakeKey2);
   Spaces1 := CountSpacesInString(FHandshakeKey1);
   Spaces2 := CountSpacesInString(FHandshakeKey2);
   Part1 := KeyNumber1 div Spaces1;
   Part2 := KeyNumber2 div Spaces2;
   Challenge := Chr((Part1 >> 24) and $FF) + Chr((Part1 >> 16) and $FF) + Chr((Part1 >> 8) and $FF) + Chr((Part1 >> 0) and $FF) +
                Chr((Part2 >> 24) and $FF) + Chr((Part2 >> 16) and $FF) + Chr((Part2 >> 8) and $FF) + Chr((Part2 >> 0) and $FF) +
                FHandshakeKey3;
   Digest := MDString(Challenge, MD_VERSION_5);
   Response := '';
   for Index := Low(Digest) to High(Digest) do
      Response := Response + Chr(Digest[Index]);
   Write('HTTP/1.1 101 WebSocket Protocol Handshake'#13#10);
   Write('Connection: Upgrade'#13#10);
   Write('Sec-WebSocket-Location: ws://' + FHostName + ':' + IntToStr(FPort) + '/' + FResource + '/' + FUsername + '/' + FPassword + #13#10);
   Write('Sec-WebSocket-Origin: ' + FOrigin + #13#10);
   Write('Upgrade: WebSocket'#13#10);
   Write(#13#10);
   Write(Response);
end;

procedure TWebSocket.CheckField();
begin
   if (FCurrentFieldName = 'Sec-WebSocket-Key1') then
      FHandshakeKey1 := FCurrentFieldValue
   else
   if (FCurrentFieldName = 'Sec-WebSocket-Key2') then
      FHandshakeKey2 := FCurrentFieldValue;
   { ... Host, Origin, Sec-WebSocket-Protocol }
end;

procedure TWebSocket.WriteFrame(s: AnsiString);
begin
   Write([$00]);
   if (Length(s) > 0) then
      Write(s);
   Write([$FF]);
end;

end.

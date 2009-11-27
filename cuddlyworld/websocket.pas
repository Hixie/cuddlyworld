{$MODE OBJFPC} { -*- text -*- }
{$INCLUDE settings.inc}
unit websocket;

interface

uses
   network, sysutils;

type
   TWebSocketState = (wsGET, wsURLPath, wsURLUsername, wsURLPassword,                      
                      wsHandshakeEnd0, { $0D => wsHandshakeEnd1 }
                      wsHandshakeEnd1, { $0A => wsHandshakeEnd2; $0D => wsHandshakeEnd1; else => wsHandshakeEnd0 }
                      wsHandshakeEnd2, { $0D => wsHandshakeEnd3; else => wsHandshakeEnd0 }
                      wsHandshakeEnd3, { $0A => wsFrameType; $0D => wsHandshakeEnd1; else => wsHandshakeEnd0 }
                      wsFrameType, { $00 => wsTextData; else => error }
                      wsTextData, { $FF => wsFrameType; else => buffer }
                      wsError);

   TWebSocket = class(TNetworkSocket)
    protected
      FState: TWebSocketState;
      FBuffer: AnsiString;
      FUsername: AnsiString;
      FPassword: AnsiString;
      FOrigin: AnsiString;
      FLocation: AnsiString;
      function InternalRead(c: Byte): Boolean; override;
      function HandleMessage(s: AnsiString): Boolean; virtual; abstract;
      procedure WriteFrame(s: AnsiString);
      procedure Handshake(); virtual;
    public
      constructor Create(AListener: TListenerSocket; AOrigin: AnsiString; ALocation: AnsiString);
      destructor Destroy(); override;
    end;

implementation

constructor TWebSocket.Create(AListener: TListenerSocket; AOrigin: AnsiString; ALocation: AnsiString);
begin
   inherited Create(AListener);
   FState := wsGET;
   FOrigin := AOrigin;
   FLocation := ALocation;
end;

destructor TWebSocket.Destroy();
begin
   inherited;
end;

function TWebSocket.InternalRead(c: Byte): Boolean;
begin
   Result := True;
   // This should be tweaked to be a true Handshake parser instead of the hack it is now
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
       $2F: FState := wsURLPassword;
       Ord('a')..Ord('z'), Ord('A')..Ord('Z'): FUsername := FUsername + Chr(c);
       else
          FState := wsError;
          Result := False;
      end;
     wsURLPassword:
      case c of
       $20: FState := wsHandshakeEnd0;
       Ord('a')..Ord('z'), Ord('A')..Ord('Z'): FPassword := FPassword + Chr(c);
       else
          FState := wsError;
          Result := False;
      end;
    wsHandshakeEnd0:
      case c of
       $0D: FState := wsHandshakeEnd1;
       else FState := wsHandshakeEnd0;
      end;
    wsHandshakeEnd1:
      case c of
       $0D: FState := wsHandshakeEnd1;
       $0A: FState := wsHandshakeEnd2;
       else FState := wsHandshakeEnd0;
      end;
    wsHandshakeEnd2:
      case c of
       $0D: FState := wsHandshakeEnd3;
       else FState := wsHandshakeEnd0;
      end;
    wsHandshakeEnd3:
      case c of
       $0D: FState := wsHandshakeEnd1;
       $0A: begin Handshake(); FState := wsFrameType; end;
       else FState := wsHandshakeEnd0;
      end;
    wsFrameType:
      case c of
       $00: begin
          FBuffer := '';
          FState := wsTextData;
       end;
       else
          FState := wsError;
          Result := False;
      end;
    wsTextData:
      case c of
       $FF: begin
          Result := HandleMessage(FBuffer);
          FState := wsFrameType;
       end;
       else
          FBuffer := FBuffer + Chr(c);
      end;
    else Assert(False);
   end;
end;

procedure TWebSocket.Handshake();
begin
   Write([$48,$54,$54,$50,$2F,$31,$2E,$31,$20,$31,$30,$31,$20,$57,$65,
          $62,$20,$53,$6F,$63,$6B,$65,$74,$20,$50,$72,$6F,$74,$6F,$63,
          $6F,$6C,$20,$48,$61,$6E,$64,$73,$68,$61,$6B,$65,$0D,$0A,$55,
          $70,$67,$72,$61,$64,$65,$3A,$20,$57,$65,$62,$53,$6F,$63,$6B,
          $65,$74,$0D,$0A,$43,$6F,$6E,$6E,$65,$63,$74,$69,$6F,$6E,$3A,
          $20,$55,$70,$67,$72,$61,$64,$65,$0D,$0A]);
   Write('WebSocket-Origin: ' + FOrigin);
   Write([$0D,$0A]);
   Write('WebSocket-Location: ' + FLocation + '/' + FUsername + '/' + FPassword);
   Write([$0D,$0A]);
   Write([$0D,$0A]);
end;

procedure TWebSocket.WriteFrame(s: AnsiString);
begin
   Write([$00]);
   if (Length(s) > 0) then
      Write(s);
   Write([$FF]);
end;

end.

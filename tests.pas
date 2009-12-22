{$MODE OBJFPC} { -*- text -*- }
{$INCLUDE settings.inc}
program tests;
uses
   sysutils, storable, world, player, cuddlycamp;

type
   TExpectation = record
      Target: AnsiString;
      IgnoreUntilFound: Boolean;
   end;

   TTestProxy = class
      FPosition: Cardinal;
      FExpectations: array of TExpectation;
      procedure Check(Message: AnsiString);
      procedure Expect(Expectation: TExpectation);
      procedure HandleAvatarMessage(Message: AnsiString);
      procedure HandleForceDisconnect();
   end;

procedure TTestProxy.Check(Message: AnsiString);
begin
   if (FPosition >= Length(FExpectations)) then
   begin
      raise Exception.Create('Failed: expected nothing but got "' + Message + '".');
   end;
   if (Message <> FExpectations[FPosition].Target) then
   begin
      if (FExpectations[FPosition].IgnoreUntilFound) then
         Exit;
      raise Exception.Create('Failed: expected "' + FExpectations[FPosition].Target + '" but got "' + Message + '".');
   end;
   Inc(FPosition);
   if (FPosition >= Length(FExpectations)) then
   begin
      SetLength(FExpectations, 0);
      FPosition := 0;
   end;
end;

procedure TTestProxy.ExpectString(Message: AnsiString);
begin
   SetLength(FExpectations, Length(FExpectations)+1);
   Assert(FExpectations[Length(FExpectations)-1].Target = '');
   Assert(FExpectations[Length(FExpectations)-1].IgnoreUntilFound = False);
   FExpectations[Length(FExpectations)-1].Target := Message;
end;

procedure TTestProxy.HandleAvatarMessage(Message: AnsiString);
begin
   Check(Message);
end;

procedure TTestProxy.HandleForceDisconnect();
begin
   Check('=Disconnected=');
end;

var
   TestWorld: TWorld;
   TestPlayer: TPlayer;
   Proxy: TTestProxy;
begin
   Writeln('CuddlyWorld Tests initializing...');
   {$IFDEF DEBUG} Writeln('CuddlyWorld debugging enabled.'); {$ENDIF}
   Proxy := TTestProxy.Create();
   TestWorld := TWorld.Create();
   try
      InitEden(TestWorld);
      TestPlayer := TPlayer.Create('Tester', '');
      TestPlayer.Adopt(@Proxy.HandleAvatarMessage, @Proxy.HandleForceDisconnect);
      TestWorld.AddPlayer(TestPlayer);
      TestPlayer.AnnounceAppearance();
      Proxy.ExpectString('');
      TestPlayer.DoLook();
      TestPlayer.Perform('look');
   finally
      TestWorld.Free();
      Proxy.Free();
   end;
   Writeln('CuddlyWorld Tests complete.');
end.
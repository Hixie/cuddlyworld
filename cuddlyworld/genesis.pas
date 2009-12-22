{$MODE OBJFPC} { -*- text -*- }
{$INCLUDE settings.inc}
program genesis;
uses
   sysutils, storable, world, cuddlycamp;

var
   FWorld: TWorld;
begin
   Writeln('CuddlyWorld Genesis initializing...');
   {$IFDEF DEBUG} Writeln('CuddlyWorld debugging enabled.'); {$ENDIF}
   FWorld := TWorld.Create();
   try
      InitEden(FWorld);
      StoreObjectToFile(kWorldFileName, FWorld, kSaveDataVersion);
   finally
      FWorld.Free();
   end;
   Writeln('CuddlyWorld Genesis complete.');
end.
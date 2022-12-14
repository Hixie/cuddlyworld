{$MODE OBJFPC} { -*- delphi -*- }
{$INCLUDE settings.inc}
program genesis;
uses
   {$IFDEF DEBUG} debug, {$ENDIF}
   sysutils, storable, world, cuddlycamp;

var
   TheWorld: TWorld;
begin
   Writeln('CuddlyWorld Genesis initializing...');
   {$IFDEF DEBUG} Writeln('CuddlyWorld debugging enabled.'); {$ENDIF}
   TheWorld := InitEden();
   StoreObjectToFile(kWorldFileName, TheWorld, kSaveDataVersion);
   TheWorld.Free();
   Writeln('CuddlyWorld Genesis complete.');
end.

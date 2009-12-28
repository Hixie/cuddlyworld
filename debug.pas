{$MODE OBJFPC} { -*- text -*- }
{$INCLUDE settings.inc}
unit debug;

interface

implementation

uses
   heaptrc;

initialization
   QuickTrace := False;
   //KeepReleased := True; { causes hang? }
end.
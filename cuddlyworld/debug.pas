{$MODE OBJFPC} { -*- text -*- }
{$INCLUDE settings.inc}
{$IFNDEF DEBUG} {$FATAL This unit should only be included in debug mode.} {$ENDIF}
unit debug;

interface

const
   HeapInfoSize = 128;

type
   PHeapInfo = ^THeapInfo;
   THeapInfo = String[HeapInfoSize];

function SetHeapInfo(S: THeapInfo): THeapInfo;

implementation

uses
   heaptrc, lineinfo;

var
   CurrentHeapInfo: THeapInfo = '';
   ReportAllocations: Boolean = False;

function SetHeapInfo(S: THeapInfo): THeapInfo;
begin
   Result := CurrentHeapInfo;
   CurrentHeapInfo := S;
end;

procedure HeapInfoFiller(P: Pointer);
var
   FunctionName, SourceFile: ShortString;
   LineNumber: Longint;
   Frame: Pointer;
begin
   if (ReportAllocations) then
   begin
      ReportAllocations := False;
      FunctionName := '';
      SourceFile := '';
      LineNumber := 0;
      Frame := Get_Frame;
      while (Assigned(Frame) and
             GetLineInfo(PtrUInt(Get_Caller_Addr(Frame)), FunctionName, SourceFile, LineNumber) and 
             ((SourceFile = '') or (SourceFile = 'heaptrc.pas') or (SourceFile = 'debug.pas'))) do
         Frame := Get_Caller_Frame(Frame);
      if (SourceFile <> '') then
      begin
         if (FunctionName <> '') then
            Writeln('Allocating memory for ', FunctionName, '() in ', SourceFile, ' line ', LineNumber)
         else
            Writeln('Allocating memory for unknown function in ', SourceFile, ' line ', LineNumber);
      end;
      ReportAllocations := True;
   end;
   PHeapInfo(P)^ := CurrentHeapInfo;
end;

procedure HeapInfoDisplayer(var PText: Text; P: Pointer);
begin
   if (PHeapInfo(P)^ <> '') then
   begin
      Writeln(PText, 'Debug log: ', PHeapInfo(P)^);
      Writeln(PText);
   end;
end;

initialization
   SetHeapExtraInfo(SizeOf(THeapInfo), @HeapInfoFiller, @HeapInfoDisplayer);
   SetHeapInfo('');
   //QuickTrace := False; { ridiculously slow }
   //ReportAllocations := True; { ridiculously verbose }
   //KeepReleased := True; { causes hang? }
finalization
   SetHeapInfo('Finalisation');
end.
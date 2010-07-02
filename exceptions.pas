{$MODE OBJFPC} { -*- text -*- }
{$INCLUDE settings.inc}
unit exceptions;

interface

uses
   sysutils, unixtype, errors;

type

   EKernelError = class(Exception)
      constructor Create(AErrorCode: cint);
   end;
   ESocketError = class(Exception)
      constructor Create(AErrorCode: cint);
   end;

   TReportExceptionEvent = procedure(E: Exception) of object;

const
   KernelErrorMsg: String = 'kernel error %d: %s';
   SocketErrorMsg: String = 'socket error %d: %s';

procedure ReportException(E: Exception);
procedure ReportExceptionAndFree(E: Exception); { for unraised exceptions }
function SetReportExceptionMethod(AReportExceptionMethod: TReportExceptionEvent): TReportExceptionEvent;

procedure NotImplemented();

implementation

var
   ReportExceptionMethod: TReportExceptionEvent = nil;

constructor EKernelError.Create(AErrorCode: cint);
begin
   inherited Create(Format(KernelErrorMsg, [AErrorCode, StrError(AErrorCode)]));
end;

constructor ESocketError.Create(AErrorCode: cint);
begin
   inherited Create(Format(SocketErrorMsg, [AErrorCode, StrError(AErrorCode)]));
end;

procedure ReportException(E: Exception);
begin
   if (Assigned(ReportExceptionMethod)) then
      ReportExceptionMethod(E)
   else
   begin
      Writeln(E.Message);
      Dump_Stack(Output, Get_Frame);
   end;
end;

procedure ReportExceptionAndFree(E: Exception);
begin
   ReportException(E);
   E.Free();
end;

function SetReportExceptionMethod(AReportExceptionMethod: TReportExceptionEvent): TReportExceptionEvent;
begin
   Result := ReportExceptionMethod;
   ReportExceptionMethod := AReportExceptionMethod;
end;

procedure NotImplemented();
begin
   raise Exception.Create('Not Implemented');
end;


procedure AssertionHandler(const Message, FileName: ShortString; LineNo: Longint; ErrorAddr: Pointer);
var
   CompleteMessage: AnsiString;
   E: Exception;
begin
   if (Message <> '') then
      CompleteMessage := 'Assertion "' + Message + '" failed on line ' + IntToStr(LineNo) + ' of ' + FileName
   else
      CompleteMessage := 'Assertion failed on line ' + IntToStr(LineNo) + ' of ' + FileName;
   E := EAssertionFailed.Create(CompleteMessage);
   ReportException(E);
   raise E;
end;

initialization
   AssertErrorProc := @AssertionHandler;
end.
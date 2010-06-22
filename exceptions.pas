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
      Writeln(E.Message);
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
begin
   if (Length(Message) > 0) then
      Writeln('Assertion "', Message, '" failed on line ', LineNo, ' of ', FileName)
   else
      Writeln('Assertion failed on line ', LineNo, ' of ', FileName);
   raise EAssertionFailed.Create(Message);
end;

initialization
   AssertErrorProc := @AssertionHandler;
end.
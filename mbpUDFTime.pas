{$I mbpDEFINES.INC}
//---------------------------------------------------------------------------

unit mbpUDFTime;
//---------------------------------------------------------------------------

interface

uses
 Windows, mbpECMA_167, mbpUDFTypes;

function udf_time_to_stamp(dest: Ptimestamp; tv_sec: int64; tv_usec: int32): Ptimestamp;
function UDFtimestampToFILETIME(ts: timestamp; filetime: PFileTime): Boolean;
function Now_UTC: Int64;
//---------------------------------------------------------------------------

implementation

uses
  Math, {DateUtils, }sysUtils;

const
  SECS_PER_HOUR = 60 * 60;
  SECS_PER_DAY  = SECS_PER_HOUR * 24;

  __mon_yday: array [0..1, 0..12] of Uint16 =
  (
    // Normal years
    (0, 31, 59, 90, 120, 151, 181, 212, 243, 273, 304, 334, 365),
    // Leap years.
    (0, 31, 60, 91, 121, 152, 182, 213, 244, 274, 305, 335, 366)
  );

  UTCDateDelta = 25569;
//---------------------------------------------------------------------------

function _DIV(a, b: int32): int32;
begin
  if (a mod b < 0) then
  begin
    Result := a div b - 1;
  end
  else
  begin
    Result := a div b;
  end;
end;
//---------------------------------------------------------------------------

function LEAPS_THRU_END_OF(y: int32): int32;
begin
  Result := _DIV(y, 4) - _DIV(y, 100) + _DIV(y, 400);
end;
//---------------------------------------------------------------------------

function __isleap(year: int32): int32;
begin
  // Nonzero if YEAR is a leap year (every 4 years, except every 100th isn't, and every 400th is)

  Result := Ord((year mod 4 = 0) and ((year mod 100 <> 0) or (year mod 400 = 0)));
end;
//---------------------------------------------------------------------------

function udf_time_to_stamp(dest: Ptimestamp; tv_sec: int64; tv_usec: int32): Ptimestamp;
var
  days, rem, y, yg: int32;
  ydays: Uint16;
  ip: PUint16;
  offset: int16;
begin
  if (dest = nil) then
  begin
    Result := nil;
    Exit;
  end;

  offset := 0;

  dest.typeAndTimezone := $1000 or (offset and $0FFF);

  Inc(tv_sec, offset * 60);
  days := tv_sec div SECS_PER_DAY;
  rem := tv_sec mod SECS_PER_DAY;
  dest.hour := rem div SECS_PER_HOUR;
  rem := rem mod SECS_PER_HOUR;
  dest.minute := rem div 60;
  dest.second := rem mod 60;
  y := 1970;

  if (__isleap(y) <> 0) then
  begin
    ydays := 366;
  end
  else
  begin
    ydays := 365;
  end;

  while ((days < 0) or (days >= ydays)) do
  begin
    yg := y + (days div 365) - Ord(days mod 365 < 0);

    // Adjust DAYS and Y to match the guessed year.
    days := days - ((yg - y) * 365 + LEAPS_THRU_END_OF(yg - 1) - LEAPS_THRU_END_OF(y - 1));

    y := yg;
  end;
  dest.year := y;
  ip := @__mon_yday[__isleap(y)][0];

  y := 11;
  while (days < PUint16(PAnsiChar(ip) + (y * SizeOf(Uint16)))^) do
  begin
    Dec(y);
    Continue;
  end;
  days := days - PUint16(PAnsiChar(ip) + (y * SizeOf(Uint16)))^;
  dest.month := y + 1;
  dest.day := days + 1;

  dest.centiseconds := tv_usec div 10000;
  dest.hundredsOfMicroseconds := (tv_usec - dest.centiseconds * 10000) div 100;
  dest.microseconds := (tv_usec - dest.centiseconds * 10000 - dest.hundredsOfMicroseconds * 100);

  Result := dest;
end;
//---------------------------------------------------------------------------

function UDFtimestampToFILETIME(ts: timestamp; filetime: PFileTime): Boolean;
var
  systemtime: TSystemTime;
begin
  systemtime.wYear := ts.year;
  systemtime.wMonth := ts.month;
  systemtime.wDay := ts.day;
  systemtime.wHour := ts.hour;
  systemtime.wMinute := ts.minute;
  systemtime.wSecond := ts.second;
  systemtime.wMilliseconds := ts.centiseconds * 10;

  SystemTimeToFileTime(systemtime, filetime^);

  Result := True;
end;
//---------------------------------------------------------------------------

function DateTimeToUTC(const AValue: TDateTime): Int64;
begin
  Result := Round((AValue - UTCDateDelta) * SecsPerDay);
end;
//---------------------------------------------------------------------------

function Now_UTC: Int64;
var
  systemtime: TSystemTime;
begin
  GetSystemTime(systemtime);
  Result := DateTimeToUTC(SystemTimeToDateTime(systemtime));
end;
//---------------------------------------------------------------------------

end.

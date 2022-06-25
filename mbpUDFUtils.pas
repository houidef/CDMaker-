{$I mbpDEFINES.INC}
//---------------------------------------------------------------------------

unit mbpUDFUtils;
//---------------------------------------------------------------------------

interface

function StrScanW(const Str: PWideChar; AnsiChar: WideChar): PWideChar;
function StrRScanW(const Str: PWideChar; AnsiChar: WideChar): PWideChar;
function StrEndW(Str: PWideChar): PWideChar;
function StrDupW(Str: PWideChar): PWideChar;
function StrLCopyW(Dest, Source: PWideChar; MaxLen: Cardinal): PWideChar;
function StrCopyW(Dest, Source: PWideChar): PWideChar;
//---------------------------------------------------------------------------

implementation

uses
  mbpUDFTypes;

function StrScanW(const Str: PWideChar; AnsiChar: WideChar): PWideChar;
begin
  Result := Str;
  while (Result^ <> AnsiChar) do
  begin
    if (Result^ = #0) then
    begin
      Result := nil;
      Exit;
    end;
    Inc(Result);
  end;
end;
//---------------------------------------------------------------------------

function StrRScanW(const Str: PWideChar; AnsiChar: WideChar): PWideChar;
var
  MostRecentFound: PWideChar;
begin
  if (AnsiChar = #0) then
  begin
    Result := StrEndW(Str);
  end
  else
  begin
    Result := nil;
    MostRecentFound := Str;
    while (True) do
    begin
      while (MostRecentFound^ <> AnsiChar) do
      begin
        if (MostRecentFound^ = #0) then
        begin
          Exit;
        end;
        Inc(MostRecentFound);
      end;
      Result := MostRecentFound;
      Inc(MostRecentFound);
    end;
  end;
end;
//---------------------------------------------------------------------------

function StrEndW(Str: PWideChar): PWideChar;
begin
  Result := Str;

  While (Result^ <> #0) do
  begin
    Inc(Result);
  end;
end;
//---------------------------------------------------------------------------

function StrDupW(Str: PWideChar): PWideChar;
var
  NumberOfBytes: int32; 
begin
  NumberOfBytes := (Length(Str) + 1) * SizeOf(WideChar);

  GetMem(Result, NumberOfBytes);
  Move(Str^, Result^, NumberOfBytes);
end;
//---------------------------------------------------------------------------

function StrLCopyW(Dest, Source: PWideChar; MaxLen: Cardinal): PWideChar;
var
  Count: Cardinal;
begin
  Result := Dest;
  Count := 0;
  while ((Count < MaxLen) and (Source^ <> #0)) do
  begin
    Dest^ := Source^;
    Inc(Source);
    Inc(Dest);
    Inc(Count);
  end;
  Dest^ := #0;
end;
//---------------------------------------------------------------------------

function StrCopyW(Dest, Source: PWideChar): PWideChar;
begin
  Result := StrLCopyW(Dest, Source, MaxInt);
end;
//---------------------------------------------------------------------------

end.

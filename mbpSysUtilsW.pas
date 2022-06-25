{$I mbpDEFINES.INC}

unit mbpSysUtilsW;

interface

uses
  Windows, Math, SysUtils;

function StrLCopyW(Dest, Source: PWideChar; MaxLen: Cardinal): PWideChar;
function StrCopyW(Dest, Source: PWideChar): PWideChar;
function StrCatW(Dest: PWideChar; const Source: PWideChar): PWideChar;
function StrPosW(Str, SubStr: PWideChar): PWideChar;
function StrEndW(Str: PWideChar): PWideChar;
function StrRScanW(const Str: PWideChar; AnsiChar: WideChar): PWideChar;
function StrLCompW(Str1, Str2: PWideChar; MaxLen: Cardinal): Integer;
function StrLenW(Str: PWideChar): Cardinal;
function StrCompW(Str1, Str2: PWideChar): Integer;
function FileCreateEx(const FileName: WideString): Integer;
function FileOpenEx(const FileName: WideString; Mode: LongWord): Integer;
function StrPasW(const Str: PWideChar): WideString;
function StrPCopyW(Dest: PWideChar; const Source: AnsiString): PWideChar;
function StrPLCopyW(Dest: PWideChar; const Source: AnsiString; MaxLen: Cardinal): PWideChar;
function CreateFileEx(lpFileName: PWideChar; dwDesiredAccess, dwShareMode: DWORD;
                     lpSecurityAttributes: PSecurityAttributes; dwCreationDisposition, dwFlagsAndAttributes: DWORD;
                     hTemplateFile: THandle): THandle;
function StrUpperCaseW(const S: WideString): WideString;                     

var
  ISWin32PlatformUnicode: Boolean;

implementation

function StrLCopyW(Dest, Source: PWideChar; MaxLen: Cardinal): PWideChar;
var
  Count: Cardinal;
begin
  Result := Dest;
  Count := 0;
  While (Count < MaxLen) and (Source^ <> #0) do
  begin
    try
      Dest^ := Source^;
    except
      dest := Source;
    end;
    Inc(Source);
    Inc(Dest);
    Inc(Count);
  end;
  Dest^ := #0;
end;

function StrCopyW(Dest, Source: PWideChar): PWideChar;
begin
  Result := StrLCopyW(Dest, Source, MaxInt);
end;

function StrCatW(Dest: PWideChar; const Source: PWideChar): PWideChar;
begin
  Result := Dest;
  StrCopyW(StrEndW(Dest), Source);
end;

function StrPosW(Str, SubStr: PWideChar): PWideChar;
var
  PSave: PWideChar;
  P: PWideChar;
  PSub: PWideChar;
begin
  Result := nil;
  if (Str <> nil) and (Str^ <> #0) and (SubStr <> nil) and (SubStr^ <> #0) then
  begin
    P := Str;
    while P^ <> #0 do
    begin
      if P^ = SubStr^ then
      begin
        PSave := P;
        PSub := SubStr;
        while (P^ = PSub^) do
        begin
          Inc(P);
          Inc(PSub);
          if (PSub^ = #0) then
          begin
            Result := PSave;
            exit;
          end;
          if (P^ = #0) then
            exit;
        end;
        P := PSave;
      end;
      Inc(P);
    end;
  end;
end;

function StrEndW(Str: PWideChar): PWideChar;
begin
  Result := Str;
  while Result^ <> #0 do
    Inc(Result);
end;

function StrRScanW(const Str: PWideChar; AnsiChar: WideChar): PWideChar;
var
  MostRecentFound: PWideChar;
begin
  if AnsiChar = #0 then
    Result := StrEndW(Str)
  else
  begin
    Result := nil;
    MostRecentFound := Str;
    while True do
    begin
      while MostRecentFound^ <> AnsiChar do
      begin
        if MostRecentFound^ = #0 then
          Exit;
        Inc(MostRecentFound);
      end;
      Result := MostRecentFound;
      Inc(MostRecentFound);
    end;
  end;
end;

function StrCompW_EX(Str1, Str2: PWideChar; MaxLen: Cardinal; dwCmpFlags: Cardinal): Integer;
var
  Len1, Len2: Integer;
begin
  if MaxLen = Cardinal(MaxInt) then begin
  Len1 := -1;
  Len2 := -1;
  end else begin
  Len1 := Min(StrLenW(Str1), MaxLen);
  Len2 := Min(StrLenW(Str2), MaxLen);
  end;
  Result := CompareStringW(GetThreadLocale, dwCmpFlags, Str1, Len1, Str2, Len2) - 2; //
end;

function StrLCompW(Str1, Str2: PWideChar; MaxLen: Cardinal): Integer;
begin
  Result := StrCompW_EX(Str1, Str2, MaxLen, 0);
end;

function StrLenW(Str: PWideChar): Cardinal;
begin
  Result := StrEndW(Str) - Str;
end;

function StrCompW(Str1, Str2: PWideChar): Integer;
begin
  Result := StrLCompW(Str1, Str2, MaxInt);
end;

function FileCreateEx(const FileName: WideString): Integer;
begin
  Result := Integer(CreateFileEx(PWideChar(FileName), GENERIC_READ or GENERIC_WRITE, 0, nil, CREATE_ALWAYS, FILE_ATTRIBUTE_NORMAL, 0)); //
end;

function FileOpenEx(const FileName: WideString; Mode: LongWord): Integer;
const
  AccessMode: array[0..2] of LongWord = (GENERIC_READ, GENERIC_WRITE, GENERIC_READ or GENERIC_WRITE);
  ShareMode: array[0..4] of LongWord = (0, 0, FILE_SHARE_READ, FILE_SHARE_WRITE, FILE_SHARE_READ or FILE_SHARE_WRITE);
begin
  Result := Integer(CreateFileEx(PWideChar(FileName), AccessMode[Mode and 3], ShareMode[(Mode and $F0) shr 4], nil, OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, 0));
end;

function StrPasW(const Str: PWideChar): WideString;
begin
  Result := Str;
end;

function StrPCopyW(Dest: PWideChar; const Source: AnsiString): PWideChar;
begin
  Result := StrPLCopyW(Dest, Source, MaxInt);
end;

function StrPLCopyW(Dest: PWideChar; const Source: AnsiString; MaxLen: Cardinal): PWideChar;
begin
  Result := StrLCopyW(Dest, PWideChar(WideString(Source)), MaxLen);
end;

function CreateFileEx(lpFileName: PWideChar; dwDesiredAccess, dwShareMode: DWORD;
                     lpSecurityAttributes: PSecurityAttributes; dwCreationDisposition, dwFlagsAndAttributes: DWORD;
                     hTemplateFile: THandle): THandle;
begin
  if IsWin32PlatformUnicode then
    Result := CreateFileW(lpFileName, dwDesiredAccess, dwShareMode, lpSecurityAttributes, dwCreationDisposition, dwFlagsAndAttributes, hTemplateFile)
  else
    Result := CreateFileA(PAnsiChar(AnsiString(lpFileName)), dwDesiredAccess, dwShareMode, lpSecurityAttributes, dwCreationDisposition, dwFlagsAndAttributes, hTemplateFile)
end;

function StrUpperCaseW(const S: WideString): WideString;
var
  Len: Integer;
begin
  Len := Length(S);
  SetString(Result, PWideChar(S), Len);
  if Len > 0 then CharUpperBuffW(Pointer(Result), Len);
end;

initialization
  //IsWin32PlatformUnicode := (Win32Platform = VER_PLATFORM_WIN32_NT);

end.

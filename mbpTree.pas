(***********************************************************************
  Date        : Sep 2003 - Oct 2008
  Author      : Ehsan Khan
  Description :
  Copyright   : 2002-08 Binary Magic, All rights reserved.
***********************************************************************)

{$I mbpDEFINES.INC}

unit mbpTree;

interface

uses
  {$IFDEF LINUX}
  Linux,   
  {$ELSE}
  Windows,
  {$ENDIF}
  Classes, SysUtils, mbpTreeTypes;

//-----------------------------------------------------------------------------

function CreateDir(var CDFiles: TCDFiles; dest_path: PAnsiChar; dir_name: PAnsiChar): pDirEntry;
function CreateDirW(var CDFiles: TCDFiles; dest_path: PWideChar; dir_name: PWideChar): pDirEntry;

function CreateDir2(var CDFiles: TCDFiles; var dest_path: TDirEntry; fd: TWin32FindDataW): pDirEntry;

function InsertDir(var CDFiles: TCDFiles; dest_path: PAnsiChar; file_path: PAnsiChar; file_specs: PAnsiChar; Recursive: Boolean; SavePath: Boolean): pDirEntry;
function InsertDirW(var CDFiles: TCDFiles; dest_path: PWideChar; file_path: PWideChar; file_specs: PWideChar; Recursive: Boolean; SavePath: Boolean): pDirEntry;

function FindDirW(var CDFiles: TCDFiles; dir_path: PWideChar): pDirEntry;
function FindFileInternal(var CDFiles: TCDFiles; Dir: AnsiString; FileToFind: AnsiString): Integer;

function InsertFile(var CDFiles: TCDFiles; dest_path: PAnsiChar; FileName: PAnsiChar): pFileEntry;
function InsertFileW(var CDFiles: TCDFiles; dest_path: PWideChar; FileName: PWideChar): pFileEntry;

function InsertFileWithName(var CDFiles: TCDFiles; DestPath: PAnsiChar; FileName: PAnsiChar; LongNameOnDisc: PAnsiChar; ShortNameOnDisc: PAnsiChar): pFileEntry;
function InsertFileWithNameW(var CDFiles: TCDFiles; DestPath: PWideChar; FileName: PWideChar; LongNameOnDisc: PWideChar; ShortNameOnDisc: PWideChar): pFileEntry;

function InsertMemoryFile (var CDFiles: TCDFiles; DestPath: PAnsiChar; FindFileData: TWin32FindDataA; OnGetData: TGetDataEvent): pFileEntry;
function InsertMemoryFileW(var CDFiles: TCDFiles; DestPath: PWideChar; FindFileData: TWin32FindDataW; OnGetData: TGetDataEvent): pFileEntry;

function InsertFileInternal(var CDFiles: TCDFiles; var dest_path: TDirEntry; fd: TWin32FindDataW; Path: PWideChar; Address: LongWord; Unicode: Boolean): pFileEntry;

procedure ClearAll(var CDFiles: TCDFiles);

procedure SetError(var CDFiles: TCDFiles; Err: LongWord);
function SetBootImage(var CDFiles: TCDFiles; BootImage: PAnsiChar): Boolean;
function GetBootImage(var CDFiles: TCDFiles): PAnsiChar;

function GetReplaceFile(var CDFiles: TCDFiles): Boolean;
procedure SetReplaceFile(var CDFiles: TCDFiles; ReplaceFile: Boolean);
function GetMCDBVersion: AnsiString;

function GetComponentStatus(var CDFiles: TCDFiles): Word;
procedure mwrite(buffer: Pointer; size: LongWord; var CDFiles: TCDFiles);

function GetDirCount(var CDFiles: TCDFiles): LongWord;
function GetFileCount(var CDFiles: TCDFiles): LongWord;

implementation

uses
  Math, mbpSysUtilsW;
(******************************************************************************

******************************************************************************)
function NewFile(var CDFiles: TCDFiles): pFileEntry;
var
  pfile: pFileEntry;
begin
  pfile := pFileEntry(AllocMem(sizeof(TFileEntry)));
  CDFiles.files[CDFiles.FileCounter] := pfile;
  Inc(CDFiles.FileCounter);
  CDFiles.Prepared := False;
  pfile.mfcbFunction := nil;
  pfile.Path := nil;
  Result :=  pfile;
end;

(******************************************************************************

******************************************************************************)
function NewDir(var CDFiles: TCDFiles): pDirEntry;
var
  dir: pDirEntry;
begin
  dir := pDirEntry(AllocMem(sizeof(TDirEntry)));
  CDFiles.dirs[CDFiles.DirCounter] := dir;
  CDFiles.PathTableEntries[CDFiles.DirCounter] := dir;
  Inc(CDFiles.DirCounter);
  CDFiles.Prepared := False;
  Result :=  dir;
end;

(******************************************************************************

******************************************************************************)
procedure ClearAll(var CDFiles: TCDFiles);
var
  i: Integer;
begin
  GetTimeZoneInformation(CDFiles.TimeZoneInfo);
  CDFiles.TimeZoneDifference := Floor((-CDFiles.TimeZoneInfo.Bias / 15)+(-CDFiles.TimeZoneInfo.DaylightBias / 15));
  for i:=0 to CDFiles.FileCounter-1 do
  begin
    if (CDFiles.files[i].LongName <> nil) then
      FreeMem(CDFiles.files[i].LongName);
    if (CDFiles.files[i].LongNameWIN32 <> nil) then
      FreeMem(CDFiles.files[i].LongNameWIN32);
    if (CDFiles.files[i].Path <> nil) then
      FreeMem(CDFiles.files[i].Path);
    FreeMem(CDFiles.files[i]);
  end;
  for i:=0 to CDFiles.DirCounter-1 do
  begin
    if (CDFiles.dirs[i].LongName <> nil) then
      FreeMem(CDFiles.dirs[i].LongName);
    FreeMem(CDFiles.dirs[i]);
  end;
  CDFiles.FileCounter := 0;
  CDFiles.DirCounter := 0;
  if (CDFiles.isom <> nil) then
    FreeMem(CDFiles.isom);
  CDFiles.isom := nil;
  //CDFiles.first_sector;
  CDFiles.RootDir := NewDir(CDFiles);
  CDFiles.RootDir.Parent := CDFiles.RootDir;
  StrCopy(CDFiles.BootImage, '');
  StrCopy(CDFiles.RootDir.ShortName, 'ROOT_DIR');

  CDFiles.IsoHeaderSize := 0;
end;

(******************************************************************************

******************************************************************************)
function GetLastEntry(dir: pDirEntry): pFileEntry;
var
  pfile, f: pFileEntry;
begin
  pfile := dir.files;
  f := pfile;
  while (pfile <> nil) do
  begin
    f := pfile;
    pfile := pfile.NextFile;
  end;
  Result :=  f;
end;

(******************************************************************************

******************************************************************************)
procedure StrRCat(dest: PWideChar; s: PWideChar);
var
  tmpstr: array [0..2048-1] of WideChar;
begin
  StrCopyW(tmpstr, s);
  StrCatW(tmpstr, dest);
  StrCopyW(dest, tmpstr);
end;

//------------------------------------------------------------------------------
procedure FindDataAtoW(FindFileData: TWin32FindDataA; var FindFileDataW: TWin32FindDataW);
begin
  FillChar(FindFileDataW, sizeof(TWin32FindDataW), 0);
  Move(FindFileData, FindFileDataW, sizeof(TWin32FindDataA));
  MultiByteToWideChar(0, 0, FindFileData.cFileName, -1, FindFileDataW.cFileName, MAX_PATH);
  MultiByteToWideChar(0, 0, FindFileData.cAlternateFileName, -1, FindFileDataW.cAlternateFileName, 14);
end;

//------------------------------------------------------------------------------
function FindFirstFileX(lpFileName: PWideChar; var lpFindFileData: TWin32FindDataW; Unicode: Boolean): THandle;
var
  hFind: THandle;
  FindData: TWin32FindDataA;
  Path: array [0..4096-1] of AnsiChar;
begin
  if Unicode then
    hFind :=  FindFirstFileW(lpFileName, lpFindFileData)
  else
  begin
    WideCharToMultiByte(0, 0, lpFileName, -1, Path, 4096, nil, nil);
    hFind :=  FindFirstFileA(Path, FindData);
    if (hFind <> INVALID_HANDLE_VALUE) then
      FindDataAtoW(FindData, lpFindFileData);
  end;

  Result :=  hFind;
end;

//------------------------------------------------------------------------------
function FindNextFileX(hFindFile: THandle; var lpFindFileData: TWin32FindDataW; Unicode: Boolean): Boolean;
var
  retval: Boolean;
  FindData: TWin32FindDataA;
begin
  if (Unicode) then
    retval := FindNextFileW(hFindFile, lpFindFileData)
  else
  begin
    retval := FindNextFileA(hFindFile, FindData);
    if (retval <> False) then
      FindDataAtoW(FindData, lpFindFileData);
  end;

  Result :=  retval;
end;
//------------------------------------------------------------------------------
function FindCloseX(hFindFile: THandle): Boolean;
begin
{$IFNDEF LINUX}
  result := Windows.FindClose(hFindFile);
{$ENDIF}
end;
(******************************************************************************

******************************************************************************)
procedure BuildPath(var CDFiles: TCDFiles; dir: pDirEntry; Path: PWideChar);
begin
  if (dir = CDFiles.RootDir) then
  begin
    StrCopyW(Path, WideChar('\'));
    Exit;
  end
  else
  begin
    StrCopyW(Path, dir.LongName);
    StrCatW(Path, WideChar('\'));

    while (dir.Parent <> CDFiles.RootDir) do
    begin
      dir := dir.Parent;
      StrRCat(Path, WideChar('\'));
      StrRCat(Path, dir.LongName);
    end;
    StrRCat(Path, WideChar('\'));
  end;
end;

(******************************************************************************

******************************************************************************)
// Commented temporarily for killing D4 hints
(*
procedure BuildDirs(var CDFiles: TCDFiles; dir: pDirEntry; Path: PWideChar);
begin
  if (dir = CDFiles.RootDir) then
  begin
    StrCopyW(Path, WideChar('\'));
    Exit;
  end
  else
  begin
    StrCopyW(Path, dir.LongName);
    StrCatW(Path, WideChar('\'));
    while (dir.Parent <> CDFiles.RootDir) do
    begin
      dir := dir.Parent;
      StrRCat(Path, WideChar('\'));
      StrRCat(Path, dir.LongName);
    end;
    StrRCat(Path, WideChar('\'));
  end;
end;
*)

(******************************************************************************

******************************************************************************)
function FindFile(dest_path: pDirEntry; filename: PWideChar): pFileEntry;
var
  pfile: pFileEntry;
begin
  pfile := dest_path.files;
  while (pfile <> nil) do
  begin
    if (lstrcmpiW(pfile.LongName, filename) = 0) then
    begin
      Result :=  pfile;
      Exit;
    end;
    pfile := pfile.NextFile;
  end;

  Result :=  nil;
end;

(******************************************************************************

******************************************************************************)
{$IFOPT O+} // required for D5
  {$DEFINE OPTIMIZATIONWASDEFINED}
  {$O-}
{$ENDIF}
function InsertFileInternal(var CDFiles: TCDFiles; var dest_path: TDirEntry; fd: TWin32FindDataW; Path: PWideChar; Address: LongWord; Unicode: Boolean): pFileEntry;
var
  pfile, f: pFileEntry;
  lft: TFileTime;
  FileName: array [0..512-1] of WideChar;
  l: Integer;
  Skip: Boolean;

begin
  if Assigned(CDFiles.cbAddFileEvent) then
  begin
    Skip := False;
    CDFiles.cbAddFileEvent(nil, Path, fd, Skip);
    if (Skip) then
    begin
      result := Pointer(1);
      exit;
    end;
  end;

  if StrLenW(fd.cFileName) > 217 then
  begin
    result := Pointer(1);
    exit;
  end;
  StrCopyW(FileName, fd.cFileName);

  if ((fd.dwFileAttributes and FILE_ATTRIBUTE_DIRECTORY) <> FILE_ATTRIBUTE_DIRECTORY) then
  begin
    {if (StrPosW(FileName, WideString(';1')) = nil) then
      StrCatW(FileName, WideString(';1'));}
  end;
  pfile := FindFile(@dest_path, FileName);
  if (CDFiles.ReplaceFile = False) and (pfile <> nil) then
  begin
    Result := pfile;
    Exit;
  end;

  if (pfile = nil) then
  begin
    pfile := NewFile(CDFiles);
    f := GetLastEntry(@dest_path);
    if (f = nil) then
      dest_path.files := pfile
    else
      f.NextFile := pfile;
  end;

  pfile.FileNameLengthJ := lstrlenW(fd.cFileName);
  pfile.FileNameLength := lstrlenW(fd.cAlternateFileName);
  if (pfile.FileNameLength = 0) then
  begin
    lstrcpynW(fd.cAlternateFileName, fd.cFileName, 13);
    pfile.FileNameLength := lstrlenW(fd.cAlternateFileName);
  end;
  FileTimeToLocalFileTime(fd.ftLastWriteTime, lft);
  FileTimeToSystemTime(lft, pfile.Time);
  StrUpperCaseW(fd.cAlternateFileName);
  WideCharToMultiByte(0, 0, fd.cAlternateFileName, -1, pfile.ShortName, lstrlenW(fd.cAlternateFileName)+1, nil, nil);
  if (pfile.LongName = nil) then
    pfile.LongName := PWideChar(AllocMem((pfile.FileNameLengthJ+5) * sizeof(WideChar)));
  pfile.FileSize := (Int64(fd.nFileSizeHigh) shl 32) + fd.nFileSizeLow;
  StrCopyW(pfile.LongName, fd.cFileName);
  //
  if (pfile.LongNameWIN32 = nil) then
  begin
    pfile.LongNameWIN32 := AllocMem((StrLenW(fd.cFileName) + 1) * SizeOf(WideChar));
    StrCopyW(pfile.LongNameWIN32, fd.cFileName);
  end;
  //
  if ((fd.dwFileAttributes and FILE_ATTRIBUTE_DIRECTORY) <> FILE_ATTRIBUTE_DIRECTORY) then
  begin
    if (StrPosW(fd.cAlternateFileName, WideChar('.')) = nil) then
    begin
      StrCat(pfile.ShortName, '.');
      Inc(pfile.FileNameLength);
    end;
    {if (StrPosW(pfile.LongName, WideString(';1')) = nil) then
    begin
      StrCatW(pfile.LongName, WideString(';1'));
      pfile.FileNameLengthJ := pfile.FileNameLengthJ + 2;
    end;
    if (StrPos(pfile.ShortName, ';1') = nil) then
    begin
      StrCat(pfile.ShortName, ';1');
      pfile.FileNameLength := pfile.FileNameLength + 2;
    end;
    }
  end;

  if (Path <> nil) then
  begin
    l := lstrlenW(Path);
    if (l <> 0) then
    begin
      if (pfile.Path <> nil) then
        FreeMem(pfile.Path);
      pfile.Path := PWideChar(AllocMem((l+5)*sizeof(WideChar)));
      StrCopyW(pfile.Path, Path);
    end;
  end
  else
    pfile.Path := nil;

  pfile.Unicode := Unicode;
  if (Address = 0) then
  begin
    pfile.Address := 0;
    pfile.Imported := False;
  end
  else
  begin
     pfile.Address := Address;
     pfile.Imported := True;
  end;
  pfile.attributes := fd.dwFileAttributes;

  Result :=  pfile;
end;
{$IFDEF OPTIMIZATIONWASDEFINED}
{$O+}
{$UNDEF OPTIMIZATIONWASDEFINED}
{$ENDIF}

(******************************************************************************

******************************************************************************)
function InsertFile(var CDFiles: TCDFiles; dest_path: PAnsiChar; FileName: PAnsiChar): pFileEntry;
var
  hFind: THandle;
  FindFileData: TWin32FindDataA;
  FindFileDataW: TWin32FindDataW;
  dest_pathW: array [0..2048-1] of WideChar;
  file_nameW: array [0..512-1] of WideChar;
  dir: pDirEntry;
  l: Integer;
begin
  hFind := FindFirstFileA(FileName, FindFileData);
  FindCloseX(hFind);
  if (hFind <> INVALID_HANDLE_VALUE) and ((FindFileData.dwFileAttributes and FILE_ATTRIBUTE_DIRECTORY) <> FILE_ATTRIBUTE_DIRECTORY) then
  begin
     MultiByteToWideChar(0, 0, FileName, -1, file_nameW, 512);
     l := 512;
     MultiByteToWideChar(0, 0, dest_path, -1, dest_pathW, 512);
     if (l >= 2) then
     begin
       if (dest_path[l-1] = '\') then
       begin
         l := l - 1;
         dest_path[l] := #0;
       end;
     end;
     dir := FindDirW(CDFiles, dest_pathW);
     if (dir <> nil) then
     begin
       FindDataAtoW(FindFileData, FindFileDataW);
       Result :=  InsertFileInternal(CDFiles, dir^, FindFileDataW, file_nameW, 0, False);
       Exit;
     end;
  end;
  Result :=  nil;
end;

(******************************************************************************
 
******************************************************************************)
function InsertFileW(var CDFiles: TCDFiles; dest_path: PWideChar; FileName: PWideChar): pFileEntry;
var
  hFind: THandle;
  FindFileData: TWin32FindDataW;
  dir: pDirEntry;
  l: Integer;
begin
  hFind := FindFirstFileW(FileName, FindFileData);
  FindCloseX(hFind);
  if (hFind <> INVALID_HANDLE_VALUE) then
  begin
    l := lstrlenW(dest_path);
    if (l >= 2) then
    begin
      if (dest_path[l-1] = '\') then
      begin
        l := l-1;
        dest_path[l] := #0;
      end;
    end;
    dir := FindDirW(CDFiles, dest_path);
    if (dir <> nil) then
    begin
      Result :=  InsertFileInternal(CDFiles, dir^, FindFileData, FileName, 0, True);
      Exit;
    end;
  end;
  Result :=  nil;
end;

(******************************************************************************

******************************************************************************)
function InsertFileWithName2(var CDFiles: TCDFiles; DestPath: PWideChar; FileName: PWideChar; LongNameOnDisc: PWideChar; ShortNameOnDisc: PWideChar; Unicode: Boolean): pFileEntry;
var
  hFind: THandle;
  FindFileData: TWin32FindDataW;
  dir: pDirEntry;
  l: Integer;
begin
  hFind := FindFirstFileX(FileName, FindFileData, Unicode);
  FindCloseX(hFind);
  if (hFind <> INVALID_HANDLE_VALUE) then
  begin
    l := lstrlenW(DestPath);
    if (l >= 2) then
    begin
      if (DestPath[l-1] = '\') then
      begin
        l := l-1;
        DestPath[l] := #0;
      end;
    end;
    dir := FindDirW(CDFiles, DestPath);
    if (dir <> nil) then
    begin
      StrCopyW(FindFileData.cFileName, LongNameOnDisc);
      StrCopyW(FindFileData.cAlternateFileName, ShortNameOnDisc);
      Result := InsertFileInternal(CDFiles, dir^, FindFileData, FileName, 0, Unicode);
      Exit;
    end;
  end;
  Result :=  nil;
end;

(******************************************************************************

******************************************************************************)
function InsertFileWithNameW(var CDFiles: TCDFiles; DestPath: PWideChar; FileName: PWideChar; LongNameOnDisc: PWideChar; ShortNameOnDisc: PWideChar): pFileEntry;
begin
  Result :=  InsertFileWithName2(CDFiles, DestPath, FileName, LongNameOnDisc, ShortNameOnDisc, True);
end;

(******************************************************************************

******************************************************************************)
function InsertFileWithName(var CDFiles: TCDFiles; DestPath: PAnsiChar; FileName: PAnsiChar; LongNameOnDisc: PAnsiChar; ShortNameOnDisc: PAnsiChar): pFileEntry;
var
  str1: array [0..2048-1] of WideChar;
  str2: array [0..2048-1] of WideChar;
  str3: array [0..256-1] of WideChar;
  str4: array [0..64-1] of WideChar;
begin
  MultiByteToWideChar(0, 0, DestPath, -1, str1, 2048);
  MultiByteToWideChar(0, 0, FileName, -1, str2, 2048);
  MultiByteToWideChar(0, 0, LongNameOnDisc, -1, str3, 256);
  MultiByteToWideChar(0, 0, ShortNameOnDisc, -1, str4, 64);
  Result :=  InsertFileWithName2(CDFiles, str1, str2, str3, str4, False);
end;

(******************************************************************************

******************************************************************************)
function InsertMemoryFileW(var CDFiles: TCDFiles; DestPath: PWideChar; FindFileData: TWin32FindDataW; OnGetData: TGetDataEvent): pFileEntry;
var
  dir: pDirEntry;
  f: pFileEntry;
  l: Integer;
begin
  l := lstrlenW(DestPath);
  if (l >= 2) then
  begin
    if (DestPath[l-1] = '\') then
    begin
      l := l-1;
      DestPath[l] := #0;
    end;
  end;
  dir := FindDirW(CDFiles, DestPath);
  if (dir <> nil) then
  begin
    f := InsertFileInternal(CDFiles, dir^, FindFileData, nil, 0, True);
    if (f <> nil) then
    begin
      f.mfcbFunction := OnGetData;
    end;
    Result :=  f;
    Exit;
  end;
  Result :=  nil;
end;

(******************************************************************************

******************************************************************************)
function InsertMemoryFile (var CDFiles: TCDFiles; DestPath: PAnsiChar; FindFileData: TWin32FindDataA; OnGetData: TGetDataEvent): pFileEntry;
var
  strw: array [0..2048-1] of WideChar;
  FindFileDataW: TWin32FindDataW;
begin
  FindDataAtoW(FindFileData, FindFileDataW);
  MultiByteToWideChar(0, 0, DestPath, -1, strw, 2048);
  Result :=  InsertMemoryFileW(CDFiles, strw, FindFileDataW, OnGetData);
end;

(******************************************************************************

******************************************************************************)
function CreateDir2(var CDFiles: TCDFiles; var dest_path: TDirEntry; fd: TWin32FindDataW): pDirEntry;
var
  d: pDirEntry;
  tmpf: pFileEntry;
  Path: array [0..4096-1] of WideChar;
  ret: Integer;
begin
  BuildPath(CDFiles, @dest_path, Path);
  StrCatW(Path, fd.cFileName);
  //Writeln(Path);
  d := FindDirW(CDFiles, Path);
  if (d = nil) then
  begin
    d := NewDir(CDFiles);
    tmpf := InsertFileInternal(CDFiles, dest_path, fd, nil, 0, True);
    if (tmpf = nil) then
    begin
      result := nil;
      exit;
    end;
    tmpf.DirRecord := d;
    WideCharToMultiByte(0, 0, fd.cAlternateFileName, -1, d.ShortName, lstrlenW(fd.cAlternateFileName)+1, nil, nil);
    d.DirNameLengthJ := lstrlenW(fd.cFileName);
    d.DirNameLength := lstrlenW(fd.cAlternateFileName);
    d.LongName := PWideChar(AllocMem((d.DirNameLengthJ+5)*sizeof(WideChar)));
    if (d.DirNameLength = 0) then
    begin
      lstrcpynW(fd.cAlternateFileName, fd.cFileName, 13);
      fd.cAlternateFileName[13] := #0;
      d.DirNameLength := lstrlenW(fd.cAlternateFileName);
      ret := WideCharToMultiByte(0, 0, fd.cAlternateFileName, -1, d.ShortName, 13, nil, nil);
      if (ret < 1) then
        StrCopy(d.ShortName, '________.___');
      end;
      StrUpper(d.ShortName);
      StrCopyW(d.LongName, fd.cFileName);
      d.Parent := @dest_path;
  end;
  Result :=  d;
end;

(******************************************************************************
 
******************************************************************************)
function CreateDirW(var CDFiles: TCDFiles; dest_path: PWideChar; dir_name: PWideChar): pDirEntry;
var
  fd: TWin32FindDataW;
  d: pDirEntry;
  st: TSystemTime;
begin
  fd.dwFileAttributes := FILE_ATTRIBUTE_DIRECTORY;
  GetSystemTime(st);
  SystemTimeToFileTime(st, fd.ftLastWriteTime);
  StrCopyW(fd.cFileName, dir_name);
  FillChar(fd.cAlternateFileName, 28, 0);
  lstrcpynW(fd.cAlternateFileName, dir_name, 12);
  d := FindDirW(CDFiles, dest_path);
  if (d = nil) then
  begin
    Result :=  nil;
    Exit;
  end;
  Result := CreateDir2(CDFiles, d^, fd);
end;

(******************************************************************************

******************************************************************************)
function CreateDir(var CDFiles: TCDFiles; dest_path: PAnsiChar; dir_name: PAnsiChar): pDirEntry;
var
  str1: array [0..2048-1] of WideChar;
  str2: array [0..256-1] of WideChar;
begin
  MultiByteToWideChar(0, 0, dest_path, -1, str1, 2048);
  MultiByteToWideChar(0, 0, dir_name, -1, str2, 255);
  Result :=  CreateDirW(CDFiles, str1, str2);
end;

(******************************************************************************

******************************************************************************)
function GetFirstDir(dir_path: PWideChar; dir_to_find: PWideChar; i: Integer): Integer;
var
  j, k, l: Integer;
begin
  j := 0;
  l := lstrlenW(dir_path);
  while (dir_path^ = '\') do
  begin
    Inc(dir_path);
    Inc(j);
  end;
  if (j = l) then
  begin
    dir_to_find[0] := #0;
    Result :=  i+j;
    exit;
  end;
  k := 0;
  while ((k < l) and (dir_path^ <> #0) and (dir_path^ <> '\')) do
  begin
    dir_to_find[k] := dir_path^;
    Inc(k);
    Inc(dir_path);
  end;
  dir_to_find[k] := #0;
  Result :=  j+k+i;
end;

(******************************************************************************
 
******************************************************************************)
function FindDir2(var CDFiles: TCDFiles; dest_path: pDirEntry; dir: PWideChar): pDirEntry;
var
  f: pFileEntry;
  i, l: Integer;
  d: pDirEntry;
  dir_to_find: array [0..4096-1] of WideChar;
begin
  i := 0;
  d := dest_path;
  i := GetFirstDir(@dir[i], dir_to_find, i);
  l := lstrlenW(dir)-1;
  f := d.files;
  d := nil;
  while (f <> nil) do
  begin
  try
    if (lstrcmpiW(f.LongName, dir_to_find) = 0) then
    begin
      d := pDirEntry(f.DirRecord);
      if (i >= l) then
      begin
        Result :=  d;
        Exit;
      end;
      Result :=  FindDir2(CDFiles, d, @dir[i]);
      Exit;
    end;
  except
   f := f;
  end;
    f := f.NextFile;
  end;
  Result :=  d;
end;

(******************************************************************************

******************************************************************************)
function FindDirW(var CDFiles: TCDFiles; dir_path: PWideChar): pDirEntry;
begin
  if ((lstrcmpW(dir_path, WideChar('\')) = 0) or (lstrcmpW(dir_path, '') = 0)) then
  begin
    Result :=  CDFiles.RootDir;
  end
  else
  begin
    Result :=  FindDir2(CDFiles, CDFiles.RootDir, dir_path);
  end;
end;

(******************************************************************************

******************************************************************************)
function MakeDirs(var CDFiles: TCDFiles; dest_path: pDirEntry; dir: PWideChar): pDirEntry;
var
  i, l: Integer;
  d: pDirEntry;
  dir_to_find: array [0..4096-1] of WideChar;
  destpath: array [0..4096-1] of WideChar;
begin
  i := 0;
  l := lstrlenW(dir)-1;
  if (l = -1) then
  begin
    Result :=  CDFiles.RootDir;
    Exit;
  end;
  d := dest_path;
  repeat
    i := GetFirstDir(@dir[i], dir_to_find, i);
    BuildPath(CDFiles, d, destpath);
    d := CreateDirW(CDFiles, destpath, dir_to_find);
  until not((i <> l) and (dir[i] <> #0));
  Result :=  d;
end;

(******************************************************************************
 
******************************************************************************)
// Commented temporarily for killing D4 hints
(*
procedure MakeDir2(var CDFiles: TCDFiles; dir_name: PWideChar);
var
  prev_dir: array [0..4096-1] of WideChar;
  dir: array [0..4096-1] of WideChar;
  ind: PWideChar;
  index: Integer;
  d1, d2: pDirEntry;
begin
  index := lstrlenW(dir_name);
  if (dir_name[index-1] = '\') then
    dir_name[index-1] := #0;
  d1 := FindDirW(CDFiles, dir_name);

  if (d1 = nil) then
  begin
    ind := StrRScanW(dir_name, WideChar('\'));
    if (ind = nil) then
      ind := StrRScanW(dir_name, WideChar('/'));
    index := ind-dir_name;

    lstrcpynW(prev_dir, dir_name, index);    //  Previous directory
    prev_dir[index] := #0;
    d2 := FindDirW(CDFiles, prev_dir);
    if (d2 = nil) then
    begin
      MakeDir2(CDFiles, prev_dir);
    end;
    if (dir_name[0] = '\') then
    begin
      StrCopyW(dir, @dir_name[index+1]);
    end
    else
      StrCopyW(dir, @dir_name[index]);

    if (lstrcmpW(dir, '') <> 0) then
     CreateDirW(CDFiles, prev_dir, dir);
  end;
end;
*)

(******************************************************************************

******************************************************************************)
// Commented temporarily for killing D4 hints
(*
function ReMoveEntry(var CDFiles: TCDFiles; entry: pFileEntry): Boolean;
begin
  Result :=  False;
end;
*)

(******************************************************************************

******************************************************************************)
// Commented temporarily for killing D4 hints
(*
function RemoveEmptyDir(var CDFiles: TCDFiles; dir: pDirEntry): Boolean; // under development :1200
var
  i,j: Integer;
  //pDir: pDirEntry;
begin
  if (dir = nil) then
  begin
    Result :=  False;
    Exit;
  end;
  for i := 0 to CDFiles.DirCounter-1 do
  begin
    if (CDFiles.dirs[i] = dir) then
    begin
      Move(CDFiles.dirs[i+1], CDFiles.dirs[i], (CDFiles.DirCounter-i) * 4);
      Move(CDFiles.PathTableEntries[i+1]^, CDFiles.PathTableEntries[i]^, (CDFiles.DirCounter-i) * 4);
      FreeMem(dir);
      Dec(CDFiles.DirCounter);
      for j := 0 to CDFiles.FileCounter-1 do
      begin
        if (CDFiles.files[j].DirRecord = dir) then
        begin
          Move(CDFiles.dirs[j+1], CDFiles.files[j], (CDFiles.DirCounter-j) * 4);
          Dec(CDFiles.FileCounter);
          //dir := nil;
          Result :=  True;
          Exit;
        end;
      end;
      Result :=  True;
      Exit;
    end;
  end;
  Result :=  False;
end;
*)

(******************************************************************************

******************************************************************************)
function InsertDirW2(var CDFiles: TCDFiles; dest_path: pDirEntry; file_path: PWideChar; file_specs: PWideChar; Recursive: Boolean; SavePath: Boolean; Unicode: Boolean): pDirEntry;
var
  hFind: THandle;
  dir: pDirEntry;
  //rdir: pDirEntry;
  l: Integer;
  pathW: array [0..4096-1] of WideChar;
  dirs_to_create: array [0..4096-1] of WideChar;
  ptr: PWideChar;
  next_path: array [0..4096-1] of WideChar;
  next_dest_path: array [0..4096-1] of WideChar;
  retval: Boolean;
  FindFileDataW: TWin32FindDataW;
begin
  l := lstrlenW(file_path);
  if (l <> 0) then
  begin
    if (file_path[l-1] = '\') then
    begin
      file_path[l-1] := #0;
      l := l-1;
    end;
  end;
  if (SavePath) then
  begin
    if (l > 2) then
    begin
      if (file_path[1] = ':') then
        StrCopyW(dirs_to_create, @file_path[2])
      else
      if (StrLCompW(WideString('\\'), file_path, 2) = 0) then
      begin
        StrCopyW(dirs_to_create, @file_path[2]); // reMove
        ptr := StrPosW(dirs_to_create, '\');
        if (ptr <> nil) then
        begin
          StrCopyW(dirs_to_create, ptr+1);       // reMove computer name
          ptr := StrPosW(dirs_to_create, '\');
          if (ptr <> nil) then
            StrCopyW(dirs_to_create, ptr+1);    // reMove share name
        end;
      end;
      if (lstrcmpW(WideChar('\'), dirs_to_create) <> 0) then
        dest_path := MakeDirs(CDFiles, dest_path, dirs_to_create)
      else
        dest_path := CDFiles.RootDir;
    end;
    if (dest_path = nil) then
    begin
      Result :=  nil;
      Exit;
    end;
  end;
  StrCopyW(pathW, file_path);
  StrCatW(pathW, WideString('\*.*'));
  if (Recursive = True) then
  begin
    hFind := FindFirstFileX(pathW, FindFileDataW, Unicode);
    if (hFind <> INVALID_HANDLE_VALUE) then
    begin
      repeat
        if ((FindFileDataW.dwFileAttributes and FILE_ATTRIBUTE_DIRECTORY) = FILE_ATTRIBUTE_DIRECTORY) then
        begin
          if ((lstrcmpW(FindFileDataW.cFileName, WideChar('.')) = 0) or (lstrcmpW(FindFileDataW.cFileName, WideString('..')) = 0)) then
            Continue;
          dir := CreateDir2(CDFiles, dest_path^, FindFileDataW);
          if (dir = nil) then
          begin
            result := nil;
            exit;
          end;
          StrCopyW(next_path, file_path);
          StrCatW(next_path, WideChar('\'));
          StrCatW(next_path, FindFileDataW.cFileName);
          {rdir :=}
          if (InsertDirW2(CDFiles, dir, next_path, file_specs, True, False, Unicode) = nil) then
          begin
            result := nil;
            FindCloseX(hFind);
            exit;
          end;
          {if (rdir.files = nil) then
             ReMoveEmptyDir(CDFiles, rdir);} // in version 1.2.0+
        end;
      until not (FindNextFileX(hFind, FindFileDataW, Unicode) <> False);
    end;
    FindCloseX(hFind);
  end;
  //  -------------------------------------------------------
  StrCopyW(pathW, file_path);
  StrCatW(pathW, WideChar('\'));
  StrCatW(pathW, file_specs);

  hFind := FindFirstFileX(pathW, FindFileDataW, Unicode);
  if (hFind <> INVALID_HANDLE_VALUE) then
  begin
    repeat
      if ((FindFileDataW.dwFileAttributes and FILE_ATTRIBUTE_DIRECTORY) <> FILE_ATTRIBUTE_DIRECTORY) then
      begin
        StrCopyW(next_dest_path, file_path);
        StrCatW(next_dest_path, WideChar('\'));
        StrCatW(next_dest_path, FindFileDataW.cFileName);
        if (InsertFileInternal(CDFiles, dest_path^, FindFileDataW, next_dest_path, 0, Unicode) = nil) then
        begin
          result := nil;
          FindCloseX(hFind);
          exit;
        end;
      end;
      retval := FindNextFileX(hFind, FindFileDataW, Unicode);
    until not (retval <> False);
  end;
  FindCloseX(hFind);
  Result :=  dest_path;
end;

(******************************************************************************

******************************************************************************)
function InsertDir(var CDFiles: TCDFiles; dest_path: PAnsiChar; file_path: PAnsiChar; file_specs: PAnsiChar; Recursive: Boolean; SavePath: Boolean): pDirEntry;
var
  dir: pDirEntry;
  tmp1: array [0..2048-1] of WideChar;
  tmp2: array [0..512-1] of WideChar;
begin
  MultiByteToWideChar(0, 0, dest_path, -1, tmp1, 2048);
  dir := FindDirW(CDFiles, tmp1);
  if (dir <> nil) then
  begin
    MultiByteToWideChar(0, 0, file_path, -1, tmp1, 2048);
    MultiByteToWideChar(0, 0, file_specs, -1, tmp2, 512);
    Result :=  InsertDirW2(CDFiles, dir, tmp1, tmp2, Recursive, SavePath, False);
  end
  else
    Result :=  nil;
end;

(******************************************************************************

******************************************************************************)
function InsertDirW(var CDFiles: TCDFiles; dest_path: PWideChar; file_path: PWideChar; file_specs: PWideChar; Recursive: Boolean; SavePath: Boolean): pDirEntry;
var
  dir: pDirEntry;
begin
  dir := FindDirW(CDFiles, dest_path);
  if (dir <> nil) then
    Result :=  InsertDirW2(CDFiles, dir, file_path, file_specs, Recursive, SavePath, True)
  else
    Result :=  nil;
end;

(******************************************************************************

******************************************************************************)
procedure SetError(var CDFiles: TCDFiles; Err: LongWord);
begin
  CDFiles.ErrorNumber := Err;
end;

(******************************************************************************

******************************************************************************)
{function GetBootImageW(CDFiles: PCDFiles): PWideChar;
begin
  Result := PWideChar(CDFiles.BootImage);
end;}

(******************************************************************************

******************************************************************************)
function SetBootImage(var CDFiles: TCDFiles; BootImage: PAnsiChar): Boolean;
var
  f: TFileStream;
begin
  if (BootImage = nil) then
  begin
    CDFiles.BootImage[0] := #0;
    Result :=  True;
    Exit;
  end;
  try
    f := TFileStream.Create(String(BootImage), fmOpenRead);
  except
    CDFiles.BootImage[0] := #0;
    Result :=  False;
    Exit;
  end;
  CDFiles.BootImageSize := f.Size;
  f.Free;

  if (not ((CDFiles.BootImageSize = 2048) or (CDFiles.BootImageSize = 1228800) or (CDFiles.BootImageSize = 1474560) or (CDFiles.BootImageSize = 2949120))) then
  begin
    CDFiles.BootImage[0] := #0;
    Result :=  False;
    Exit;
  end;
  StrCopy(CDFiles.BootImage, BootImage);
  Result :=  True;
end;

(******************************************************************************

******************************************************************************)
function GetBootImage(var CDFiles: TCDFiles): PAnsiChar;
begin
  Result :=  @CDFiles.BootImage[0];
end;

(******************************************************************************

******************************************************************************)
function GetReplaceFile(var CDFiles: TCDFiles): Boolean;
begin
  Result := CDFiles.ReplaceFile;
end;

(******************************************************************************

******************************************************************************)
procedure SetReplaceFile(var CDFiles: TCDFiles; ReplaceFile: Boolean);
begin
  CDFiles.ReplaceFile := ReplaceFile;
end;

(******************************************************************************

******************************************************************************)
function GetMCDBVersion: AnsiString;
begin
  {$ifdef TRIAL}
  Result := '1.21(RC2) TRIAL (20081020)';
  {$else}
  Result := '1.21(RC2) REGTD (20081020)';
  {$endif}
end;

(*******************************************************************************

*******************************************************************************)
procedure mwrite(buffer: Pointer; size: LongWord; var CDFiles: TCDFiles);
{var
  bytesWritten: LongWord;}
begin

  if (CDFiles.fisoheader = nil) then
  begin
    Move(buffer^, CDFiles.isom[CDFiles.IsoHeaderSize], size);
    //WriteLn(IntToStr(size)+' - ' + Inttostr(CDFiles.IsoHeaderSize)+' - Max:'+inttostr(CDFiles.FirstDataSector*2048)+'\'+inttostr(CDFiles.IsoHeaderSize+size));
    CDFiles.IsoHeaderSize := CDFiles.IsoHeaderSize + Integer(size);
  end
  else
  begin
    try
    {bytesWritten := }CDFiles.fisoheader.Write(buffer, size);
    except
      {$IFDEF CONDEBUG}
      writeln('err here');
      {$ENDIF}
    end;
  end;
  if CDFiles.IsoHeaderSize = $c110 then
  begin
    CDFiles.IsoHeaderSize := CDFiles.IsoHeaderSize + 1;
    CDFiles.IsoHeaderSize := CDFiles.IsoHeaderSize - 1;
  end;
end;

(*******************************************************************************
*                                                                              *
*******************************************************************************)
function GetComponentStatus(var CDFiles: TCDFiles): Word;
begin
  {if ((CDFiles.Device <> nil) and (CDFiles.Device.ComponentState <> 0)) then
    Result := CDFiles.Device.ComponentState
  else
  begin
    if (CDFiles <> nil) then}
      Result := CDFiles.ComponentStatus;
    {else
      Result := 0;
  end;}
end;

(*******************************************************************************
*                                                                              *
*******************************************************************************)
function GetDirCount(var CDFiles: TCDFiles): LongWord;
begin
  Result := CDFiles.DirCounter;
end;

(*******************************************************************************
*                                                                              *
*******************************************************************************)
function GetFileCount(var CDFiles: TCDFiles): LongWord;
begin
  Result := CDFiles.FileCounter;
end;

(*******************************************************************************
*                                                                              *
*******************************************************************************)
function FindFileInternal2(var CDFiles: TCDFiles; Dir: PWideChar; FileToFind: PWideChar): Integer;
var
  d: PDirEntry;
  f: PFileEntry;
  i: Integer;
begin
   result := -1;
   d := FindDirW(CDFiles, Dir);
   if (d <> nil) then
   begin
     f := FindFile(d, FileToFind);
     if f <> nil then
     begin
       for I := 0 to CDFiles.FileCounter - 1 do
       begin
         if f = CDFiles.Files[i] then
           result := i;
       end;
     end;
   end;
end;

(*******************************************************************************
*                                                                              *
*******************************************************************************)
function FindFileInternal(var CDFiles: TCDFiles; Dir: AnsiString; FileToFind: AnsiString): Integer;
var
  dest_pathW: array [0..2048-1] of WideChar;
  file_nameW: array [0..512-1] of WideChar;
begin
   MultiByteToWideChar(0, 0, @FileToFind[1], -1, file_nameW, Length(FileToFind)+1);
   MultiByteToWideChar(0, 0, @Dir[1], -1, dest_pathW, Length(Dir)+1);
   result := FindFileInternal2(CDFiles, dest_pathW, file_nameW);
end;
(*******************************************************************************
*                                                                              *
*******************************************************************************)
function FindFileInternalW(var CDFiles: TCDFiles; Dir: WideString; FileToFind: WideString): Integer;
begin
  result := FindFileInternal2(CDFiles, @Dir[1], @FileToFind[1]);
end;

end.

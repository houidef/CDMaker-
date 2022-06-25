(*******************************************************************************
  Unit        : bmCDBurner.PAS
  Date        : Sep 2002 - October 2008
  Author      : Ehsan Khan
  Description :
  Copyright   : 2002-08 Binary Magic, All rights reserved.
*******************************************************************************)
unit mbpCDBurner;

interface

uses
  {$IFDEF LINUX}
  Linux,
  {$ELSE}
  Windows,
  {$ENDIF}
  Sysutils, Classes, mbpTree, mbpTreeTypes, mbpDeviceTypes, mbpCommonLib, mbpSCSILib, mbpConsts, mbpSysUtilsW;

{$I mbpDEFINES.INC}

type
  TWriteThread = class(TThread)
  private
    ErrorInBurn: Boolean;
    cbWriteDone: TWriteDoneEvent;
    procedure WriteDone;
  protected
    procedure Execute; override;
  public
    CDFiles: PCDFiles;
    Device: PSCSIDEVICE;
    function GetFirst: Cardinal;
    procedure Lock;
    procedure UnLock;
    function CanStart: Boolean;
    constructor Create(var CDFiles_: TCDFiles; var Device_: TSCSIDevice); overload;
  end;

function  Burn(var CDFiles: TCDFiles; var device: TSCSIDEVICE; OnWriteDone: TWriteDoneEvent; Reserved: Pointer): Boolean;
function  BurnISOImage(var CDFiles: TCDFiles; var Device: TSCSIDevice; FileName: PAnsiChar; OnWriteDone: TWriteDoneEvent; Reserved: Pointer): Boolean;
function  BurnISOImageW(var CDFiles: TCDFiles; var Device: TSCSIDevice; FileName: PWideChar; OnWriteDone: TWriteDoneEvent; Reserved: Pointer): Boolean;
function  BuildISOImage(var CDFiles: TCDFiles; FileName: PAnsiChar; OnWriteDone: TWriteDoneEvent; Reserved: Pointer): Boolean;
function  BuildISOImageW(var CDFiles: TCDFiles; FileName: PWideChar; OnWriteDone: TWriteDoneEvent; Reserved: Pointer): Boolean;
function  ImportSession(var CDFiles: TCDFiles; var Device: TSCSIDevice; SessionNo: Byte; DestDir: PAnsiChar; ISOFileName: AnsiString = ''): Integer;
function  ImportSessionW(var CDFiles: TCDFiles; var Device: TSCSIDevice; SessionNo: Byte; DestDir: PWideChar; ISOFileName: AnsiString = ''): Integer;
function  ReadVolumeLabelOfDisc(var device: TSCSIDevice; SessionNo: Byte): WideString;
function  MountISOImage(var CDFiles: TCDFiles; var Device: TScsiDevice; ISOFileName: AnsiString): Boolean;
{function  InitializeMCDBurner(var CDFiles: TCDFiles; var Device: TSCSIDevice): Boolean;}
procedure SetCacheSize(var CDFiles: TCDFiles; CacheSize: Integer);
function  GetCachePosition(var CDFiles: TCDFiles): LongWord;
function  GetErrorNumber(var CDFiles: TCDFiles; var Device: TScsiDevice): LongWord;
function  GetErrorString(var CDFiles: TCDFiles; var Device: TScsiDevice): PAnsiChar;
function  GetErrorStringW(var CDFiles: TCDFiles; var Device: TScsiDevice): PWideChar;

//---------------------------------------------------------------------------

implementation

uses
  mbpISO9660, mbpCache;
(*  bmDrvLib, Windows, bmConst, SysUtils, SysUtilsW, SyncObjs;  *)

(*******************************************************************************
*                               Import session                                 *
*******************************************************************************)
procedure CopyChar2(s: PAnsiChar; l: Integer);
var
  i: Integer;
  tmp: AnsiChar;
begin
  i := 0;
  while (i < l) do
  begin
    tmp := s[i];
    s[i] := s[Succ(i)];
    s[Succ(i)] := tmp;
    Inc(i, 2);
  end;
  s[i] := #0;
  i := 0;
  while (i < l) do
  begin
    if (s[i] = ';') then
    begin
      while (i < l) do
      begin
        s[i] := #0;
        Inc(i);
      end;
    end;
    Inc(i, 2);
  end;
end;

(*******************************************************************************)

(*******************************************************************************)
procedure ISO9660TimeToFileTime(dd: TDirectoryDescriptor; var fd: TWin32FindDataW);
var
  st: TSystemTime;
begin
  st.wYear := dd.year + 1900;
  st.wMonth := dd.month;
  st.wDay := dd.day;
  st.wHour := dd.hour;
  st.wMinute := dd.min;
  st.wSecond := dd.sec;
  st.wMilliseconds := 0;
  SystemTimeToFileTime(st, fd.ftLastWriteTime);
end;

(*******************************************************************************)

(*******************************************************************************)
function ImportSessionDirectory(var CDFiles: TCDFiles; var Device: TSCSIDevice; DirLocation: LongWord; Size: Word; var directory: TDirEntry; ISOFileName: AnsiString): Integer;
var
  NumberOfSectorsToProcess: Integer;
  i: Integer;
  fd: TWin32FindDataW;
  dd: TDirectoryDescriptor;
  tmpd: pDirEntry;
  lb: array [0..2048-1] of AnsiChar;
  read10Result: Boolean;
  res: Integer;
  //tmpstr: array [0..2048-1] of WideChar;
label
  again;
begin
  NumberOfSectorsToProcess := Sectors(Size);
  i:=0;
  FillChar(fd, SizeOf(fd), 0);
  if ISOFileName = '' then
    read10Result := Read10(Device, DirLocation, 1, @CDFiles.tmpBuffer1)
  else
  begin
    //read10Result := Read10(Device, DirLocation, 1, @CDFiles.tmpBuffer1)
    CDFiles.ISOFileStream.Seek(DirLocation*2048, soFromBeginning);
    //Seek(CDFiles.ISOFileHandle, DirLocation*2048);
    res := CDFiles.ISOFileStream.Read(CDFiles.tmpBuffer1, 2048);
    //BlockRead(CDFiles.ISOFileHandle, CDFiles.tmpBuffer1, 2048, res);
    if Res = 2048 then
      read10Result := true
    else
      read10Result := false;
  end;

  if (read10Result = True) then
  begin
    Move(CDFiles.tmpBuffer1, lb, 2048);
    Move(lb[i], dd, 33);
    i := dd.LenDR;
    Move(lb[i], dd, 33);
    Inc(i, dd.LenDR);
again:
    while (i < 2048-33) do
    begin
      Move(lb[i], dd, 33);
      if (dd.LenDR = 0) then Break;
      fd.cAlternateFileName[0] := #0;
      FillChar(fd, SizeOf(fd), 0);
      Move(lb[i+33], dd.FileName, dd.FileIdentifierLen);
      if (CDFiles.ISO9660v1999FS) then
      begin
        FillChar(fd.cFileName, SizeOf(fd.cFileName), 0); 
        MultiByteToWideChar(0, 0, dd.FileName, dd.FileIdentifierLen, fd.cFileName, 2048);
      end
      else
      begin
        CopyChar2(dd.FileName, dd.FileIdentifierLen);
        Move(dd.FileName, fd.cFileName, dd.FileIdentifierLen);
      end;
      ISO9660TimeToFileTime(dd, fd);
      if ((dd.FileFlag and 2) = 2) then
      begin
        fd.dwFileAttributes := FILE_ATTRIBUTE_DIRECTORY;
        tmpd := CreateDir2(CDFiles, directory, fd);
        fd.nFileSizeLow := 0;
        fd.nFileSizeHigh := 0;
        ImportSessionDirectory(CDFiles, Device, dd.AddressL, dd.DataLengthL, tmpd^, ISOFileName);
      end
      else
      begin
        //WriteLn('file :' + WideString(dd.FileName));
        {$IFDEF CONDEBUG}
          WriteLn('file :' + dd.FileName);
        {$ENDIF}
        fd.dwFileAttributes := FILE_ATTRIBUTE_NORMAL;
        fd.nFileSizeLow := dd.DataLengthL;
        fd.nFileSizeHigh := 0;
        InsertFileInternal(CDFiles, directory, fd, nil, dd.AddressL, True);
      end;
      Inc(i, dd.LenDR);
    end;
  end;
  Dec(NumberOfSectorsToProcess);
  Inc(DirLocation);
  if (NumberOfSectorsToProcess > 0) then
  begin
    if ISOFileName <> '' then
    begin
      CDFiles.ISOFileStream.Seek(DirLocation*2048, soFromBeginning);
      CDFiles.ISOFileStream.Read(CDFiles.tmpBuffer1, 2048);
    end
    else
    begin
      Read10(Device, DirLocation, 1, @CDFiles.tmpBuffer1);
    end;
    Move(CDFiles.tmpBuffer1, lb, 2048);
    i := 0;
    goto again;
  end;
  Result := NumberOfSectorsToProcess;
end;

(*******************************************************************************)

(*******************************************************************************)
function ImportSessionW(var CDFiles: TCDFiles; var Device: TSCSIDevice; SessionNo: Byte; DestDir: PWideChar; ISOFileName: AnsiString = ''): Integer;
var
  LastSession: Byte;
  ivd: TVolumeDescriptor;
  directory: pDirEntry;
  SessionStartAddress, i: LongWord;
begin
  CDFiles.ComponentStatus := CS_IMPORTINGSESSION;
  LastSession := 1;
  if (ISOFileName = '') then
  begin
    if (SessionNo = 0) then
      LastSession := SessionsOnDisc(Device)
    else
      LastSession := SessionNo;
  end;
  FillChar(ivd, SizeOf(ivd), 255);
  directory := FindDirW(CDFiles, DestDir);
  if (directory = nil) then
    directory := CDFiles.RootDir;
  if (ISOFileName = '') then
  begin
    SetWriteParams(Device, Device.TestWrite, Device.BurnProof, False, ord(DiscIs(Device)));

    if (ReadTrackInformation(Device, LastSession)) then
    begin
      SessionStartAddress :=  Device.TrackInformation.TrackStartAddress;

      for i := SessionStartAddress+16 to Pred(SessionStartAddress+32) do
      begin
        if (Read10(Device, i, 1, @ivd) <> False) then
        begin
          if ((StrLComp(ivd.identifier, 'CD001', 5) = 0) and (ivd.vdType = 2)) then
          begin
            {$IFDEF CONDEBUG}
            WriteLn('Found Joliet at ' + IntToStr(i));
            {$ENDIF}
            ImportSessionDirectory(CDFiles, device, ivd.RootRec.AddressL, ivd.RootRec.DataLengthL, directory^, ISOFileName);
            Break;
          end;
          if ((StrLComp(ivd.identifier, 'CD001', 5) = 0) and (ivd.vdType = 1)) then
          begin
            {$IFDEF CONDEBUG}
            WriteLn('Found ISO9660 at ' + IntToStr(i));
            {$ENDIF}
            ImportSessionDirectory(CDFiles, device, ivd.RootRec.AddressL, ivd.RootRec.DataLengthL, directory^, ISOFileName);
            Break;
          end;

        end;
      end;
    end;
  end
  else
  begin
    CDFiles.ISOFileStream := TFileStream.Create(String(ISOFileName), fmOpenRead);

    //AssignFile(CDFiles.ISOFileHandle, ISOFileName);
    //Reset(CDFiles.ISOFileHandle, 1);
    for i := 0 to 32 do
    begin
      CDFiles.ISOFileStream.Read(ivd, 2048);
      //BlockRead(CDFiles.ISOFileHandle, ivd, 1*2048);
      if ((StrLComp(ivd.identifier, 'CD001', 5) = 0) and (ivd.vdType = 2)) then
      begin
        {$IFDEF CONDEBUG}
        WriteLn('Found Joliet at ' + IntToStr(i));
        {$ENDIF}
        ImportSessionDirectory(CDFiles, device, ivd.RootRec.AddressL, ivd.RootRec.DataLengthL, directory^, ISOFileName);
        Break;
      end;
      if ((StrLComp(ivd.identifier, 'CD001', 5) = 0) and (ivd.vdType = 1)) then
      begin
        {$IFDEF CONDEBUG}
        WriteLn('Found ISO9660 at ' + IntToStr(i));
        {$ENDIF}
        ImportSessionDirectory(CDFiles, device, ivd.RootRec.AddressL, ivd.RootRec.DataLengthL, directory^, ISOFileName);
        Break;
      end;

    end;
  end;
  CDFiles.ComponentStatus := CS_IDLE;
  Result := LastSession;
end;

(*******************************************************************************)

(*******************************************************************************)
function ImportSession(var CDFiles: TCDFiles; var device: TSCSIDevice; SessionNo: Byte; DestDir: PAnsiChar; ISOFileName: AnsiString = ''): Integer;
var
  tmpstr: array [0..2048-1] of WideChar;
begin
  MultiByteToWideChar(0, 0, DestDir, -1, tmpstr, Succ(StrLen(DestDir)));
  Result := ImportSessionW(CDFiles, device, SessionNo, tmpstr, ISOFileName);
end;

function ReadVolumeLabelOfDisc(var device: TSCSIDevice; SessionNo: Byte): WideString;
var
  LastSession: Byte;
  ivd: TVolumeDescriptor;
  SessionStartAddress, i: LongWord;
  vol: array[0..100] of WideChar;
begin
  FillChar(ivd, SizeOf(ivd), 255);
  if (SessionNo = 0) then
    LastSession := SessionsOnDisc(Device)
  else
    LastSession := SessionNo;
  Result := '';
  SetWriteParams(Device, Device.TestWrite, Device.BurnProof, False, ord(DiscIs(Device)));
  if (ReadTrackInformation(Device, LastSession)) then
  begin
    SessionStartAddress :=  Device.TrackInformation.TrackStartAddress;
    for i := SessionStartAddress+16 to Pred(SessionStartAddress+32) do
    begin
        if (Read10(Device, i, 1, @ivd) <> False) then
        begin
          if ((StrLComp(ivd.identifier, 'CD001', 5) = 0) and (ivd.vdType = 2)) then
          begin
            {$IFDEF CONDEBUG}
            WriteLn('Found Joliet at ' + IntToStr(i));
            {$ENDIF}
            CopyChar2(ivd.IDVolume, 32);
            Move(ivd.IDVolume, vol, 32);
            Result := WideString(Vol);
            Break;
          end;
          if ((StrLComp(ivd.identifier, 'CD001', 5) = 0) and (ivd.vdType = 1)) then
          begin
            {$IFDEF CONDEBUG}
            WriteLn('Found Joliet at ' + IntToStr(i));
            {$ENDIF}
            CopyChar2(ivd.IDVolume, 32);
            Move(ivd.IDVolume, vol, 32);
            Result := WideString(Vol);
            Break;
          end;

        end;
    end;
  end;

end;

(*******************************************************************************)

(*******************************************************************************)
{function InitializeMCDBurner(var CDFiles: TCDFiles; var Device: TScsiDevice): Boolean;
begin
  Device.BurnProof := True;
  //Device.HaID := 127;
  //Device.Target := 127;
  //Device.Lun := 127;
  //..CDFiles.Device := @Device;
  //Device.DriveLetter := #$7f;
  Device.CloseFH := True; // WS
  Result := InitializeASPI(Device);
  ClearAll(CDFiles);
  CDFiles.CacheSize := 8 * 1024 * 1024;
end;}

(*******************************************************************************)

(*******************************************************************************)
function StartBurn(var CDFiles: TCDFiles; device: PSCSIDEVICE; OnWriteDone: TWriteDoneEvent; Reserved: Pointer): Boolean;
const
  dvdrsr: array [0..11] of Byte = ($00, $A2, $00, $08, $00, $00, $03, $E8, $54, $00, $00, $10);
  dvdrsr2: array [0..11] of Byte = ($00, $82, $00, $08, $00, $00, $00, $00, $54, $00, $00, $10);
  dvdrw: array [0..11] of Byte = ($00, $82, $00, $08, $00, $00, $00, $00, $4c, $00, $00, $10);
var
  //IdWriteThread: Longword;
  //hWriteThread: THandle;
  CacheThread: TCacheThread;
  WriteThread: TWriteThread;
begin
  if Device <> nil then
    QPerfDeviceInit(Device^);
  if ((CDFiles.SaveToISOImage = False) and (CDFiles.BurnISOImage = False)) then
  begin
    SetWriteParams(Device^, Device.TestWrite, Device.BurnProof, False, Ord(Device.Medium));
    ReadDiscInformation(Device^, @Device.DiscInformation, SizeOf(Device.DiscInformation));

    if ((Device.Medium = mtDVDRWSR) or (Device.Medium = mtDVDRWRO)) then
    begin
      if (Device.Medium = mtDVDRWSR) then
      begin
        Move(dvdrsr, CDFiles.tmpBuffer2, SizeOf(dvdrsr));
        if (not FormatUnit(Device^, $11, @CDFiles.tmpBuffer2[0], 12)) then
        begin
          Move(dvdrsr2, CDFiles.tmpBuffer2, SizeOf(dvdrsr2));
          FormatUnit(Device^, $11, @CDFiles.tmpBuffer2[0], 12);
        end;
      end
      else
      begin
        Move(dvdrw, CDFiles.tmpBuffer2, SizeOf(dvdrw));
        FormatUnit(Device^, $11, @CDFiles.tmpBuffer2[0], 12);;
      end;

      repeat
        ReadDiscInformation(Device^, @Device.BGFDI, $2a);
        if Device.LastSenseInfo.AddSenseCode = 4 then
          Sleep(5000);
      until (Device.LastSenseInfo.AddSenseCode <> 04);
    end;
    CDFiles.VDImageSize := 0;
    if ((TMedium(Device.Medium) = mtDVDPLUSRW) or (TMedium(Device.Medium) = mtDVDRAM) or (TMedium(Device.Medium) = mtBDRE)) then  //..BDRE
    begin
      CDFiles.FirstWritableSector := GetLastRecordedAddress(Device^);
      CDFiles.VDImageSize := CDFiles.FirstWritableSector;
    end
    else if (ReadTrackInformation(Device^, TracksOnDisc(Device^)) = True) then
    begin
      if ((Integer(Device.Medium) = Ord(mtDVDRWSR)) or (Integer(Device.Medium) = Ord(mtDVDRWRO))) then
        CDFiles.FirstWritableSector := Device.TrackInformation.NextWritableAddress
      else
        CDFiles.FirstWritableSector := Device.TrackInformation.NextWritableAddress;
      if (CDFiles.FirstWritableSector = 1000) then
        CDFiles.FirstWritableSector := 0;
    end
    else if (ReadTrackInformation(Device^, $01) = True) then
    begin
      CDFiles.FirstWritableSector := Device.TrackInformation.TrackStartAddress;
    end;
  end;

  if (CDFiles.UDFBridge) then
  begin
    if (not CDFiles.SaveToISOImage) then
    begin
      CDFiles.UDFBridgeInfo.SessionStarting_LSA := Device.TrackInformation.TrackStartAddress;
    end
    else
    begin
      CDFiles.UDFBridgeInfo.SessionStarting_LSA := 0;
    end;
  end;

  if (not CDFiles.BurnISOImage) then
    BuildHeader(CDFiles);

  CDFiles.cbWriteDone := OnWriteDone;
  CDFiles.Aborted := False;

  CacheThread := TCacheThread.Create(CDFiles);
  CacheThread.FreeOnTerminate := True;
  while (not CacheThread.CanStart) do
  begin
    if (CDFiles.Aborted <> False) then
    begin
      if Assigned(CDFiles.cbWriteDone) then
      begin
        if CDFiles.hWNDFE <> 0 then
          PostMessage(CDFiles.hWNDFE, WM_WRITEDONE, 0, 0)
        else
          CDFiles.cbWriteDone(Reserved, 0, True);

      end;
      Result := False;
      Exit;
    end;
    Sleep(100);
  end;

  WriteThread := TWriteThread.Create(CDFiles, device^);
  WriteThread.FreeOnTerminate := True;
  WriteThread.Resume;
  Result := True;
end;

(*******************************************************************************)

(*******************************************************************************)
function Burn(var CDFiles: TCDFiles; var device: TSCSIDEVICE; OnWriteDone: TWriteDoneEvent; Reserved: Pointer): Boolean;
begin
  CDFiles.ISOFileName[0] := #0;
  CDFiles.SaveToISOImage := False;
  CDFiles.BurnISOImage := False;
  CDFiles.ISOFileName[0] := #0;
  Device.Medium := DiscIs(Device);
  Result := StartBurn(CDFiles, @device, OnWriteDone, Reserved);
end;


(*******************************************************************************)

(*******************************************************************************)
function BurnISOImage(var CDFiles: TCDFiles; var device: TSCSIDEVICE; FileName: PAnsiChar; OnWriteDone: TWriteDoneEvent; Reserved: Pointer): Boolean;
var
  tmpstr: AnsiString;
  tmpWStr: WideString;
begin
  tmpstr := FileName;
  tmpWStr := WideString(tmpStr);
  move(tmpWStr[1], CDFiles.ISOFileName[0], StrLen(FileName)*2+2);
  CDFiles.SaveToISOImage := False;
  CDFiles.BurnISOImage := True;
  CDFiles.TotalImageSize := 0;
  CDFiles.ISOImage := CreateFileA(FileName, GENERIC_READ, FILE_SHARE_READ, nil, OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, 0); // non-unicode

  if (CDFiles.ISOImage = INVALID_Handle_VALUE) then
  begin
    CDFiles.ErrorNumber := ERR_CANT_OPEN_FILE;
    Result := False;
    Exit;
  end;
  Device.Medium := DiscIs(Device);
  Result := StartBurn(CDFiles, @device, OnWriteDone, Reserved);
end;

(*******************************************************************************)

(*******************************************************************************)
function BurnISOImageW(var CDFiles: TCDFiles; var device: TSCSIDEVICE; FileName: PWideChar; OnWriteDone: TWriteDoneEvent; Reserved: Pointer): Boolean;
begin
  Move(FileName[0], CDFiles.ISOFileName[0], StrLenW(FileName)*2+2);
  CDFiles.SaveToISOImage := False;
  CDFiles.BurnISOImage := True;
  CDFiles.TotalImageSize := 0;
  CDFiles.ISOImage := CreateFileW(FileName, GENERIC_READ, FILE_SHARE_READ, nil, OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, 0); // non-unicode

  if (CDFiles.ISOImage = INVALID_Handle_VALUE) then
  begin
    CDFiles.ErrorNumber := ERR_CANT_OPEN_FILE;
    Result := False;
    Exit;
  end;
  Device.Medium := DiscIs(Device);
  Result := StartBurn(CDFiles, @device, OnWriteDone, Reserved);
end;

(*******************************************************************************)

(*******************************************************************************)
function BuildISOImage(var CDFiles: TCDFiles; FileName: PAnsiChar; OnWriteDone: TWriteDoneEvent; Reserved: Pointer): Boolean;
var
  tmpstr: AnsiString;
  tmpWStr: WideString;
begin
  tmpstr := FileName;
  tmpWStr := WideString(tmpStr);
  move(tmpWStr[1], CDFiles.ISOFileName[0], StrLen(FileName)*2+2);
  CDFiles.BurnISOImage := False;
  CDFiles.ISOImage := CreateFileA(FileName, GENERIC_WRITE, FILE_SHARE_WRITE, nil, CREATE_ALWAYS, FILE_ATTRIBUTE_NORMAL, 0); // non-unicode
  if (CDFiles.ISOImage = INVALID_Handle_VALUE) then
  begin
    CDFiles.ErrorNumber := ERR_CANT_CREATE_FILE;
    Result := False;
    Exit;
  end;
  CDFiles.SaveToISOImage := True;
  Result := StartBurn(CDFiles, nil, OnWriteDone, Reserved);
end;

(*******************************************************************************)

(*******************************************************************************)
function BuildISOImageW(var CDFiles: TCDFiles; FileName: PWideChar; OnWriteDone: TWriteDoneEvent; Reserved: Pointer): Boolean;
begin
  Move(FileName[0], CDFiles.ISOFileName[0], StrLenW(FileName)*2+2);
  CDFiles.BurnISOImage := False;
  CDFiles.ISOImage := CreateFileW(FileName, GENERIC_WRITE, FILE_SHARE_WRITE, nil, CREATE_ALWAYS, FILE_ATTRIBUTE_NORMAL, 0); // non-unicode
  if (CDFiles.ISOImage = INVALID_Handle_VALUE) then
  begin
    //..CDFiles.FileName := FileName;
    CDFiles.ErrorNumber := ERR_CANT_CREATE_FILE;
    Result := False;
    Exit;
  end;
  CDFiles.SaveToISOImage := True;
  Result := StartBurn(CDFiles, nil, OnWriteDone, Reserved);
end;

(*******************************************************************************)

(*******************************************************************************)
procedure SetCacheSize(var CDFiles: TCDFiles; CacheSize: Integer);
begin
  if (CacheSize < 2 * 1024 * 1024) then
    CacheSize := 2 * 1024 * 1024;
  CDFiles.CacheSize := CacheSize;
end;

(*******************************************************************************)

(*******************************************************************************)
function GetCachePosition(var CDFiles: TCDFiles): LongWord;
begin
  Result := CDFiles.PacketCount * PacketSize;
end;

(*******************************************************************************

*******************************************************************************)
function GetErrorNumber(var CDFiles: TCDFiles; var Device: TScsiDevice): LongWord;
var
  ret: LongWord;
begin
  if (CDFiles.ErrorNumber = 0) then
  begin
    ret := Device.DeviceError;
    Device.DeviceError := 0;
  end
  else
  begin
    ret := CDFiles.ErrorNumber;
    CDFiles.ErrorNumber := 0;
  end;
  Result := ret;
  Exit;
end;

(*******************************************************************************

*******************************************************************************)
function GetErrorString(var CDFiles: TCDFiles; var Device: TScsiDevice): PAnsiChar;
var
  ErrorNumber: Integer;
  str: array [0..256-1] of AnsiChar;
begin
  ErrorNumber := GetErrorNumber(CDFiles, Device);
  case ErrorNumber of
    0:
      StrCopy(Device.ErrStr, 'Unknown/No Error!');
    1:
      StrCopy(Device.ErrStr, 'Data not prepared ');
    2:
      if (CDFiles.FileName <> nil) then
      begin
        StrCopy(str, StrPCopy(str, AnsiString(WideCharToString(CDFiles.FileName))));
        StrPCopy(Device.ErrStr, AnsiString(Format('Error opening file \%s\', [str])));
      end
      else
        StrPCopy(Device.ErrStr, AnsiString(Format('Error opening file', [])));
    3:
      if (CDFiles.FileName <> nil) then
      begin
        StrCopy(str, StrPCopy(str, AnsiString(WideCharToString(CDFiles.FileName))));
        StrPCopy(Device.ErrStr, AnsiString(Format('Error creating file \%s\', [str])));
        end
        else
          StrPCopy(Device.ErrStr, AnsiString(Format('Error creating file', [str])));
    4:
      StrCopy(Device.ErrStr, AnsiString('Aborted by user'));
    else
      StrPCopy(Device.ErrStr, AnsiString(Format('Hardware Error %u', [ErrorNumber])));
  end;
  Result := Device.ErrStr;
end;

(*******************************************************************************

*******************************************************************************)
function GetErrorStringW(var CDFiles: TCDFiles; var Device: TScsiDevice): PWideChar;
var
  ErrorNumber: Integer;
  tmp: array [0..48-1] of WideChar;
begin
  ErrorNumber := GetErrorNumber(CDFiles, Device);

  case ErrorNumber of
    0:
      StrCopyW(Device.ErrStrW, WideString('Unknown/No Error!'));
    1:
      StrCopyW(Device.ErrStrW, WideString('Data not prepared'));
    2:
      StrCopyW(Device.ErrStrW, PWideChar(WideString('Error opening file \''') + WideString(CDFiles.FileName) + WideString('\''')));
    3:
      if (CDFiles.FileName <> nil) then
      begin
        StrCopyW(Device.ErrStrW, WideString('Error creating file \'''));
        StrCatW(Device.ErrStrW, CDFiles.FileName);
        StrCatW(Device.ErrStrW, WideString('\'''));
      end
      else
        StrCopyW(Device.ErrStrW, WideString('Error creating file'));
    4:
      StrCopyW(Device.ErrStrW, WideString('Aborted by user'));
    else
      StrCopyW(Device.ErrStrW, WideString('Hardware Error \'''));
      StrPCopyW(tmp, AnsiString(IntToStr(ErrorNumber)));
      StrCatW(Device.ErrStrW, tmp);
  end;
  Result := Device.ErrStrW;
end;

{ TWriteThread }

(*******************************************************************************)

(*******************************************************************************)
constructor TWriteThread.Create(var CDFiles_: TCDFiles; var Device_: TSCSIDevice);
begin
  inherited Create(True);
  FreeOnTerminate := True;
  CDFiles := @CDFiles_;
  Device := @Device_;
  //..Priority := tpLower;

  Resume;
end;

(*******************************************************************************
*                                                                              *
*******************************************************************************)
procedure TWriteThread.Lock;
begin
  CDFiles.CriticalSection.Enter;
end;

(*******************************************************************************
*                                                                              *
*******************************************************************************)
procedure TWriteThread.Unlock;
begin
  CDFiles.CriticalSection.Leave;
end;

(*******************************************************************************
*                                                                              *
*******************************************************************************)
function TWriteThread.GetFirst: Cardinal;
var
  ret: Cardinal;
begin
  if ((CDFiles.RemainingBlocks = 0) or Boolean(CDFiles.Finished)) then // Execute method controls termination of this proc
  begin
    Result := 0;
    Exit;
  end;
  if (CDFiles.PacketCount = 0) then
  begin
    Result := $ffffffff;
    Exit;
  end;
  if (CDFiles.PacketCount > 0) then
  begin
    while (CDFiles.BusyInReading) do
    begin
      Sleep(2);
      if (CDFiles.Aborted = True) then
      begin
        Result := 0;
        Exit;
      end;
    end;

    Lock;
    CDFiles.BusyInGetFirst := True;
    if (CDFiles.PacketCount <> 0) then
    begin
      ret := CDFiles.Packets[0].Size;
      cdfiles.WritingFile := CDFiles.Packets[0].WritingFile;
      Move(CDFiles.Packets[0].Packet[0], CDFiles.tmpBuffer1[0], ret);
      if CDFiles.Packets[0].Packet <> nil then
      begin
        FreeMem(CDFiles.Packets[0].Packet);
      end
      else
        Beep;
      Move(CDFiles.Packets[1], CDFiles.Packets[0], SizeOf(CDFiles.Packets)); //CDFiles.PacketCount*SizeOf(Pred(CDFiles.Packets[0])));
      CDFiles.BytesAvailable := CDFiles.BytesAvailable - ret;
      CDFiles.RemainingBlocks := CDFiles.RemainingBlocks - Sectors(ret);
      Dec(CDFiles.PacketCount);
      CDFiles.BusyInGetFirst := False;
    end
    else
    begin
      if Boolean(CDFiles.Finished) then
        ret := 0
      else
        ret := $ffffffff;
    end;
    Unlock;
  end
  else
  begin
    if Boolean(CDFiles.Finished) then
      ret := 0
    else
      ret := $ffffffff;
  end;
  Result := ret;
end;

(*******************************************************************************)

(*******************************************************************************)
function TWriteThread.CanStart: Boolean;
begin
   Result := Boolean(CDFiles.CanStart);
end;
    
(*******************************************************************************)

(*******************************************************************************)
procedure TWriteThread.Execute;
const
  dvdrw: array[0..11] of AnsiChar = (#$00, #$82, #$00, #$08, #$00, #$00, #$00, #$00, #$4C, #$00, #$00, #$10);
  dvdrsr: array[0..11] of AnsiChar = (#$00, #$82, #$00, #$08, #$00, #$00, #$00, #$00, #$54, #$00, #$00, #$10);
  buf2: array[0..11] of Byte = ($00, $82, $00, $08, $00, $23, $05, $40, $98, $00, $00, $00);
  //dvdrsr: array[0..11] of AnsiChar = (#$00, #$A2, #$00, #$08, #$00, #$00, #$03, #$E8, #$54, #$00, #$00, #$10);
var
  tries: Integer;
  size, BytesWritten: LongWord;
  time: TSystemTime;
  Tracks: Byte;
  RdBuffer: Integer;
  NOBUFCAP, DAO: Boolean;
  Medium: Byte;
  address: LongWord;
  {$IFDEF TRIAL}
  j: LongWord;
  {$ENDIF}
  WriteSpeed, ReadSpeed: LongWord;
label
  again, again2, CloseDisc;
begin
  tries := 0;
  NOBUFCAP := False;
  RdBuffer:=2;
  ErrorInBurn := False;
  DAO := False;
  address:=0;
  Tracks := 0;
  Medium := 0;

  CDFiles.ErrorNumber := 0;
  CDFiles.ComponentStatus := CS_WRITING;
  if (Device <> nil) then
  begin
    Device.DeviceBufferPosition := 0;
    Device.DeviceBufferSize := 0;
    Medium := Ord(Device.Medium);
  end;
  if (CDFiles.SaveToISOImage = False) then
  begin
    GetSystemTime(time);
    FlushCache(Device^, False);
    Rewind(Device^);
    SetWriteParams(Device^, Device.TestWrite, Device.BurnProof, False, Medium);
    ReadDiscInformation(Device^, @Device.DiscInformation, SizeOf(Device.DiscInformation));

    if (ReadBufferCapacity(Device^)) then
    begin
      if (Device.LastSenseInfo.AddSenseCode = 20) then
        NOBUFCAP := True;
    end;

    if (CDFiles.BurnISOImage = True) then
    begin
      address := 0;
    end
    else
    begin
      address := CDFiles.FirstWritableSector;
    end;

    if (Integer(Medium) <> Ord(mtDVDPLUSR)) then
      SetWriteParams(Device^, Device.TestWrite, Device.BurnProof, False, Medium)
    else
      SetWriteParams(Device^, Device.TestWrite, Device.BurnProof, not Device.FinalizeDisc, Medium);
    SetCDSpeed(Device^, Device.ReadSpeed, Device.WriteSpeed);


    if ((Integer(Medium) = Ord(mtDVDR)) or (Integer(Medium) = Ord(mtDVDRWRO)) or (Integer(Medium) = Ord(mtDVDRWSR)) or (Integer(Medium) = Ord(mtDVDPLUSR)) or (Integer(Medium) = Ord(mtDVDPLUSRW)) or (Integer(Medium) = Ord(mtDVDPLUSRDL)) or (Integer(Medium) = Ord(mtDVDR))
    or (Integer(Medium) = Ord(mtBDRE)) or (Integer(Medium) = Ord(mtBDRSR)) or (Integer(Medium) = Ord(mtBDRRR)) or (Integer(Medium) = Ord(mtHDDVDR)) or (Integer(Medium) = Ord(mtHDDVDRAM))) then
    begin
      WriteSpeed := GetWriteSpeed(Device^);
      ReadSpeed := GetReadSpeed(Device^);
      //WriteSpeed := Trunc(WriteSpeed);
      //ReadSpeed := Trunc(ReadSpeed * 1385);
      if (not ReadTrackInformation(Device^, TracksOnDisc(Device^))) then
        device.TrackInformation.TrackSize := address + CDFiles.TotalImageSize;
      SetPerformance(Device^, 0, device.TrackInformation.TrackSize, ReadSpeed, WriteSpeed, 1000, 1000);
    end;
    //WriteSpeed := GetWriteSpeed(Device^);

    Tracks := SessionsOnDisc(Device^);

    //WriteSpeed := GetWriteSpeed(Device^);
    if (Integer(Medium) = Ord(mtDVDR)) then
    begin
      //SetPerformance(Device, 10000, CDFiles.TotalImageSize);
      SendDVDStructureTimeStamp(Device^, time);
      ReserveTrack(Device^, CDFiles.TotalImageSize);
    end;
    // WS.
    Device.CloseFH := False;
  end;
  CDFiles.BlocksWritten := 0;

  size := GetFirst;
  if ((not CDFiles.SaveToISOImage) and (not CDFiles.BurnISOImage)) then
  begin
    if ((Integer(Medium) = Ord(mtDVDPLUSRW)) or (Integer(Medium) = Ord(mtDVDRWRO)) or (Integer(Medium) = Ord(mtDVDRWSR)) or (Integer(Medium) = Ord(mtDVDRAM))) then
    begin
      FlushCache(Device^, False);
again2:
      if (address <> 0) then if (Write10(Device^, 0, Trunc(size / 2048), CDFiles.tmpBuffer1, size) <> True) then
      begin
        if ((Device.LastSenseInfo.AddSenseCode = $30) and (Device.LastSenseInfo.AddSenQual = $05) and (Integer(Medium) = Ord(mtDVDPLUSRW))) then
        begin
          if (FormatUnit(Device^, $11, @buf2[0], 12)) then
          begin
            repeat
              Sleep(5000);
              ReadDiscInformation(Device^, Device.tmpBuffer, $2a);
            until (Device.LastSenseInfo.AddSenseCode <> 04);
          end;

          Inc(tries);
          if (tries = 1) then
            goto again2;
        end;
        if ((Device.TargetBusy) or (Device.LastSenseInfo.AddSenseCode = $4)) then
        begin
          Sleep(200);
          Inc(tries);
          if (tries < 5000) then
            goto again2;
        end;
        if ((Device.TargetBusy) or (Device.LastSenseInfo.AddSenseCode = $0) or (Device.LastSenseInfo.AddSenseCode = $29)) then
        begin
          if (Device.LastSenseInfo.AddSenseCode = $29) then
            Sleep(1000);
          Sleep(200);
          Inc(tries);
          if (tries < 100) then
            goto again2;
        end;

        CDFiles.Aborted := True;
        Sleep(100);
        CDFiles.ComponentStatus := CS_BURNERROR_ABORTING;
        ErrorInBurn := TRUE;
        goto CloseDisc;
      end;
      FlushCache(Device^, False);
    end;
  end;

  repeat
    {$IFDEF TRIAL}
    if ((address >= 65536) and (size > 0) and (size <> $FFFFFFFF)) then
    begin
      for j := 0 to Pred(size) do
      begin
        cdfiles.tmpBuffer1[j] := AnsiChar(0);
      end;
    end;
    {$ENDIF}

    if ((size > 0) and (size <> $FFFFFFFF)) then
    begin
      if (CDFiles.SaveToISOImage = True) then
        WriteFile(CDFiles.ISOImage, CDFiles.tmpBuffer1, size, BytesWritten, nil)
      else
      begin
        tries := 0;
again:
        if (Write10(Device^, address, Trunc(size/2048), CDFiles.tmpBuffer1, size) <> True) then
        begin
          if ((Device.TargetBusy) or (Device.LastSenseInfo.AddSenseCode = $4) or (Device.LastSenseInfo.AddSenseCode = 00)) then
          begin
            Sleep(100);
            Inc(tries, 1);
            if (tries < 5000) then
              goto again;
          end;
          CDFiles.Aborted := True;
          Sleep(100);
          CDFiles.ComponentStatus := CS_BURNERROR_ABORTING;
          ErrorInBurn := True;
          goto CloseDisc;
        end;
      end;
      Dec(RdBuffer);
      if ((RdBuffer < 0) and (not NOBUFCAP) and (CDFiles.SaveToISOImage = False)) then
      begin
        ReadBufferCapacity(Device^);
        RdBuffer := 5;
      end;
      address := address + Trunc(size / 2048);
      CDFiles.BlocksWritten := CDFiles.BlocksWritten + Trunc(size / 2048);
    end;

    size := GetFirst;
    if (size = $FFFFFFFF) then
      Sleep(10);
  until not (size <> 0);
  CDFiles.ComponentStatus := CS_CLOSINGTRACK;
CloseDisc:
  if (Device <> nil) then
    Device.CloseFH := True;
  if (CDFiles.SaveToISOImage = False) then
  begin
    if ((DAO = True) and (Integer(Medium) = Ord(mtDVDR))) then
    begin
      FlushCache(Device^, False);
      Device.DeviceBufferPosition := 0;
      Device.DeviceBufferSize := 0;
    end
    else
    if ((DAO = False) and (Integer(Medium) = Ord(mtDVDR))) then
    begin
      FlushCache(Device^, False);
      Device.DeviceBufferPosition := 0;
      Device.DeviceBufferSize := 0;
      CloseDVDTrack(Device^, False, 1, 0, 0, Tracks+01);
      CloseDVDTrack(Device^, False, 1, 0, 0, Tracks+02);
      CloseDVDTrack(Device^, False, 2, 0, 0, Tracks+03);
    end
    else
    if ((DAO = True) and (Integer(Medium) = Ord(mtDVDPLUSR))) then
    begin
      FlushCache(Device^, False);
      Device.DeviceBufferPosition := 0;
      Device.DeviceBufferSize := 0;
      CloseDVDTrack(Device^, False, 1, 0, 0, Tracks+01);
      CloseDVDTrack(Device^, False, 2, 0, 0, Tracks+03);
    end
    else
    if ((DAO = False) and (Integer(Medium) = Ord(mtDVDPLUSR))) then
    begin
      FlushCache(Device^, False); 
      Device.DeviceBufferPosition := 0;
      Device.DeviceBufferSize := 0;
      CloseDVDTrack(Device^, False, 1, 0, 0, Tracks+01);
      CloseDVDTrack(Device^, False, 2, 0, 0, $ff);
    end
    // WS
    else
    if ((DAO = False) and (Integer(Medium) = Ord(mtDVDPLUSRDL))) then
    begin
      FlushCache(Device^, False);
      Device.DeviceBufferPosition := 0;
      Device.DeviceBufferSize := 0;
      CloseDVDTrack(Device^, False, 1, 0, 0, Tracks+01);
      if (Device.FinalizeDisc) then
      begin
        CloseDVDTrack(Device^, False, 6, 0, 0, 0);
      end
      else
      begin
        CloseDVDTrack(Device^, False, 2, 0, 0, 0);
      end;
    end
    // WS.
    else
    if (Integer(Medium) = Ord(mtDVDPLUSRW)) then
    begin
      FlushCache(Device^, False);
      Device.DeviceBufferPosition := 0;
      Device.DeviceBufferSize := 0;
      CloseDVDTrack(Device^, False, 3, 0, 0, $ff);
    end
    else
    if ((Integer(Medium) = Ord(mtDVDRWRO)) or (Integer(Medium) = Ord(mtDVDRWSR))) then
    begin
      FlushCache(Device^, False);
      Device.DeviceBufferPosition := 0;
      Device.DeviceBufferSize := 0;
      CloseDVDTrack(Device^, False, 1, 0, 0, 1);
      CloseDVDTrack(Device^, True,  2, 0, 0, 0);
      repeat
        Sleep(5000);
        ReadDiscInformation(Device^, @Device.tmpBuffer, $2a);
      until not (Device.LastSenseInfo.AddSenseCode = 04);
    end
    else
    begin
      FlushCache(Device^, True);
      if (Integer(Medium) <> Ord(mtDVDRAM)) then
        repeat
          Sleep(5000);
          ReadDiscInformation(Device^, @Device.tmpBuffer, $2a);
        until not (Device.LastSenseInfo.AddSenseCode = 04);
      if (Integer(Medium) <> Ord(mtDVDRAM)) then
        repeat
          TestUnitReady(Device^);
          Sleep(5000);
        until not (Device.LastSenseInfo.AddSenseCode = 04);
      Device.DeviceBufferPosition := 0;
      Device.DeviceBufferSize := 0;
      if ((Integer(Medium) = Ord(mtCDR)) or (Integer(Medium) = Ord(mtCDRW))) then
      begin
        SetWriteParams(Device^, Device.TestWrite, Device.BurnProof, not Device.FinalizeDisc, Medium);
        if (CloseTrack(Device^, False, True) = False) then
          CloseTrack(Device^, True, True);
      end
      else
      begin
        if (CloseTrack(Device^, True, False) = False) then
          CloseTrack(Device^, True, True);
        SetWriteParams(Device^, Device.TestWrite, Device.BurnProof, not Device.FinalizeDisc, Medium);
        if (CloseTrack(Device^, False, True) = False) then
          CloseTrack(Device^, False, False);
      end;
    end;
    if (Integer(Medium) <> Ord(mtDVDRAM)) then
      repeat
        ReadDiscInformation(Device^, @Device.tmpBuffer, $2a);
        if (Device.LastSenseInfo.AddSenseCode = 04) then
          Sleep(5000);
      until not (Device.LastSenseInfo.AddSenseCode = 04);
    UnlockMedium(Device^);
    LoadMedium(Device^, False);
    Rewind(Device^);

    TestUnitReady(Device^);
  end
  else
  begin
    CloseHandle(CDFiles.ISOImage);
  end;

  while (not CDFIles.CacheThreadTerminated) do
  begin
    Sleep(100);
  end;
  cbWriteDone := CDFiles.cbWriteDone;

  Lock;
  if (CDFiles.ErrorNumber <> 0) then
    ErrorInBurn := True;
  {$IFDEF CONDEBUG}
  Writeln(' ************** DONE ******************** (',CDFiles.ErrorNumber,')');
  {$ENDIF}
  CDFiles.ComponentStatus := CS_IDLE;
  if Assigned(cbWriteDone) then
  begin
    if CDFiles.hWNDFE <> 0 then
      PostMessage(CDFiles.hWNDFE, WM_WRITEDONE, 0, Integer(ErrorInBurn))
    else
      cbWriteDone(Self, 0, ErrorInBurn)
      //Synchronize(WriteDone);
  end;

  Unlock;

end;

(*******************************************************************************)

(*******************************************************************************)
procedure TWriteThread.WriteDone;
begin
  cbWriteDone(Self, 0, ErrorInBurn)
end;

function MountISOImage(var CDFiles: TCDFiles; var Device: TScsiDevice; ISOFileName: AnsiString): Boolean;
begin
  result := ImportSession(CDFiles, Device, 0, '\', ISOFileName) <> 0;
  CDFiles.ISOFileStream.Destroy;
//  CloseFile(CDFiles.ISOFileHandle);
end;
end.

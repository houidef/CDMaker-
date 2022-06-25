(*******************************************************************************
  Unit        : bmCache.PAS
  Date        : Sep 2002 - Oct 2008
  Author      : Ehsan Khan
  Co-Author(s): Wasim Sabir
  Description :
  Copyright   : 2002-08 Binary Magic, All rights reserved.
*******************************************************************************)

unit mbpCache;

interface

uses
 {$IFDEF LINUX}
  Linux,
 {$ELSE}
  Windows,
 {$ENDIF}

  Classes, mbpTreeTypes, mbpConsts, mbpCommonLib;

  {$I mbpDEFINES.INC}
  
type
  TCacheThread = class(TThread)
  private
    procedure OnTerminateEvent(Sender: TObject);
  protected
    procedure Execute; override;
  public
    CDFiles: PCDFiles;
    function GetFirstXX: Cardinal;
    procedure Lock;
    procedure unLock;
    function CanStart: Boolean;
    constructor Create(var CDFiles_: TCDFiles); overload;
    {procedure Abort;}
  end;

const
  MaxBytesToRead = 2048 * 32;
  PacketSize = MaxBytesToRead;

implementation

uses
  SysUtils, mbpSysUtilsW, mbpUDFBridge;

(*******************************************************************************)

(*******************************************************************************)
function TCacheThread.CanStart: Boolean;
begin
   Result := Boolean(CDFiles.CanStart);
end;

(*******************************************************************************)

(*******************************************************************************)
constructor TCacheThread.Create(var CDFiles_: TCDFiles);
begin
  inherited Create(True);

  CDFiles := @CDFiles_;
  OnTerminate := OnTerminateEvent;
  CDFiles.MaxPackets := Trunc(CDFiles.CacheSize / MaxBytesToRead);
  CDFiles.ISOHeader := CDFiles.isom;
  CDFiles.ISOHeaderSize := CDFiles.IsoHeaderSize;
  CDFiles.CanStart := #0;
  CDFiles.RemainingBlocks := CDFiles.TotalImageSize;
  CDFiles.vPriority := tpLower;
  Priority := tpLower;
  CDFiles.NoOfFiles := CDFiles.FileCounter;
  {$IFDEF WINDOWS}
  CDFiles.hCacheThread := Self.Handle;
  {$ENDIF}
  CDFiles.IdCacheThread := Self.ThreadID;
  CDFiles.VerifySize := 0;
  if (CDFiles.hCacheThread = INVALID_HANDLE_VALUE) then
  begin
    {$IFDEF CONDEBUG}
    WriteLn('Thread Creation Failed');
    {$ENDIF}
    Exit;
  end;
  CDFiles.CacheThreadTerminated := False;
  Resume;
end;

var
  files: array [0..MAXFILESONDISC-1] of pFileEntry;
  
procedure TCacheThread.Execute;
var
  hFile: THandle;
  f: pFileEntry;
  bf: TFileEntry;
  //dwError: LongWord;
  NoOfFiles: Integer;
  BytesLeft: Int64;
  SizeLow, SizeHigh: LongWord;
  Position: Integer;
  cbGetData: TGetDataEvent;
  BytesToRead, BytesRead: LongWord;
  i: Integer;
  AvailableBytesInPacket: Integer;
  src: array [0..2048-1] of AnsiChar;
  srcW: PWideChar;
  tmpw: array [0..2048-1] of WideChar;
  UDFBridgeInfo: PUDFBridgeInformation;
label
  label1;
begin
  hFile := 0;
  BytesToRead := 0;
  f := nil;
  srcW := '';
  CDFiles.VerifySize := 0;
  (*******************************  ISO Header ******************************)
  CDFiles.PacketCount := 0;
  CDFiles.BytesAvailable := 0;
  CDFiles.Aborted := False;
  CDFiles.Finished := #0;
  BytesLeft := CDFiles.ISOHeaderSize;
  Position := 0;
  Sleep(20);
  if (CDFiles.BurnISOImage = True) then
  begin
    hFile := CDFiles.ISOImage;
    SizeLow := GetFileSize(hFile, @SizeHigh);
    BytesLeft := int64(SizeHigh) * Int64($FFFFFFFF) + Int64(SizeLow);
    CDFiles.TotalImageSize := Sectors(BytesLeft);
    CDFiles.RemainingBlocks := CDFiles.TotalImageSize;
  end;
  while (BytesLeft > 0) do
  begin
    if (BytesLeft < PacketSize) then
      BytesToRead := BytesLeft
    else
      BytesToRead := PacketSize;
    while (CDFiles.BusyInGetFirst) do
      Sleep(2);
    Lock;
    CDFiles.BusyInReading := True;
    CDFiles.Packets[CDFiles.PacketCount].Packet := AllocMem(PacketSize);
    if (CDFiles.Packets[CDFiles.PacketCount].Packet = nil) then
    begin
      CDFiles.Packets[CDFiles.PacketCount].Packet := PAnsiChar(#0); // only for debugging
    end;
    if (CDFiles.BurnISOImage = True) then
    begin
      ReadFile(hFile, CDFiles.tmpBuffer2[0], BytesToRead, BytesRead, nil);
      Move(CDFiles.tmpBuffer2, CDFiles.Packets[CDFiles.PacketCount].Packet[0], BytesToRead);
      CDFiles.Packets[CDFiles.PacketCount].Size := BytesToRead;
      Inc(CDFiles.PacketCount);
    end
    else
    begin
      if (BytesToRead = PacketSize) then
      begin
        Move(CDFiles.ISOHeader[Position], CDFiles.Packets[CDFiles.PacketCount].Packet[0], BytesToRead);
        CDFiles.Packets[CDFiles.PacketCount].Size := BytesToRead;
        Inc(CDFiles.PacketCount);
      end
      else
      begin
        Move(CDFiles.ISOHeader[Position], CDFiles.tmpBuffer2[0], BytesToRead);
        Position := BytesToRead;
      end;
    end;
    BytesLeft := BytesLeft - BytesToRead;
    CDFiles.BytesAvailable := CDFiles.BytesAvailable + BytesToRead;
    Unlock;
    CDFiles.BusyInReading := False;
    while (CDFiles.PacketCount >= CDFiles.MaxPackets) do
    begin
      CDFiles.CanStart := #1;
      Sleep(20);
      if (CDFiles.Aborted = True) then
      begin
        Lock;
        CDFiles.Finished := #1;
        Unlock;
        if (CDFiles.BurnISOImage = True) then
        begin
          CloseHandle(CDFiles.ISOImage);
          CDFiles.ISOImage := INVALID_HANDLE_VALUE;
        end;
        //CDFiles.ErrorNumber := 1;
        Terminate;
        Exit;
      end;
    end;
    Position := Position + Integer(BytesToRead);
  end;
  if (CDFiles.BurnISOImage = True) then
  begin
    CloseHandle(CDFiles.ISOImage);
    CDFiles.ISOImage := INVALID_HANDLE_VALUE;
    goto label1;
  end;
  Position := BytesToRead;
  AvailableBytesInPacket := PacketSize - Position;
  if (AvailableBytesInPacket = 0) then
    if ((CDFiles.NoOfFiles <> 0) or (CDFiles.BootImage[0] <> #0) or (CDFiles.UDFBridge)) then
    begin
      CDFiles.Packets[CDFiles.PacketCount].Packet := AllocMem(PacketSize);
      Position := 0;
      AvailableBytesInPacket := PacketSize;
    end;
  if (CDFiles.isom <> nil) then
    FreeMem(CDFiles.isom);
  CDFiles.isom := nil;
  (**************************************************************************)
  NoOfFiles := 0;
  for i := 0 to Pred(CDFiles.NoOfFiles) do
  begin
    f := CDFiles.files[i];
    srcW := f.path;
    if (((srcW <> nil) or (Assigned(f.mfcbFunction))) and (f.FileSize <> 0) and (f.DirRecord = nil)) then
    begin
      files[NoOfFiles] := CDFiles.files[i];
      files[NoOfFiles].FileId := i;
      Inc(NoOfFiles);
    end;
  end;
  UDFBridgeInfo := @CDFiles.UDFBridgeInfo;
  for i := -2 to Pred(NoOfFiles) do
  begin
    if (i = -2) then
    begin
      if (CDFiles.UDFBridge) then
      begin
        UDFBridgeInfo := @CDFiles.UDFBridgeInfo;
        BytesLeft := UDFBridgeInfo.NumberOfBlocksRequiredForFileDirStructures * BlockSize;

        while (BytesLeft > 0) do
        begin
          if (BytesLeft < AvailableBytesInPacket) then
          begin
            BytesToRead := BytesLeft;
          end
          else
          begin
            BytesToRead := AvailableBytesInPacket;
          end;

          while (CDFiles.BusyInGetFirst) do
          begin
            Sleep(2);
          end;
          CDFiles.BusyInReading := True;

          Lock;

          if (Int64(BytesToRead) = AvailableBytesInPacket) then
          begin
            if (BytesToRead = PacketSize) then
            begin
              BuildPacket(CDFiles^, CDFiles.Packets[CDFiles.PacketCount].Packet, BytesToRead div BlockSize);
            end
            else
            begin
              BuildPacket(CDFiles^, @CDFiles.tmpBuffer2[Position], BytesToRead div BlockSize);
              Move(CDFiles.tmpBuffer2, CDFiles.Packets[CDFiles.PacketCount].Packet[0], Position + Int64(BytesToRead));
            end;
            Inc(Position, BytesToRead);
            Dec(AvailableBytesInPacket, BytesToRead);

            CDFiles.Packets[CDFiles.PacketCount].Size := Position;
            ///Inc(CDFiles.CurrentImageSize, (CDFiles.Packets[CDFiles.PacketCount].Size / BlockSize));
            Inc(CDFiles.PacketCount);

            Position := 0;
            if ((CDFiles.NoOfFiles <> 0) or (CDFiles.BootImage[0] <> AnsiChar(0)) or (BytesLeft > 0)) then
            begin
              CDFiles.Packets[CDFiles.PacketCount].Packet := AllocMem(PacketSize);
              AvailableBytesInPacket := PacketSize;
            end;
          end
          else
          begin
            BuildPacket(CDFiles^, @CDFiles.tmpBuffer2[Position], BytesToRead div BlockSize);
            Inc(Position, BytesToRead);
            Dec(AvailableBytesInPacket, BytesToRead);
          end;

          BytesLeft := BytesLeft - BytesToRead;
          CDFiles.BytesAvailable := CDFiles.BytesAvailable + BytesToRead;
          Unlock;
          CDFiles.BusyInReading := False;
          while (CDFiles.PacketCount >= (CDFiles.MaxPackets)) do
          begin
            CDFiles.CanStart := #1;
            Sleep(20);
            if (CDFiles.Aborted = True) then
            begin
              CDFiles.Finished := #1;
              Terminate;
              Exit;
            end;
          end;
        end;
      end;

      Continue;
    end;

    if (i = -1) then
    begin
      if (CDFiles.BootImage[0] = #0) then
        Continue;
      MultiByteToWideChar(0, 0, CDFiles.BootImage, -1, tmpw, 2048);
      bf.FileSize := CDFiles.BootImageSize;
      bf.DirRecord := nil;
      bf.unicode := False;
      bf.mfcbFunction := nil;
      srcW := tmpw;
      f := @bf;
    end
    else
    begin
      f := files[i];
      srcW := f.path;
    end;
    if (((srcW <> WideString(#0)) or (Assigned(f.mfcbFunction))) and (f.FileSize <> 0) and (f.DirRecord = nil)) then
    begin
      CDFiles.FileName := srcW;
      if (hFile <> INVALID_HANDLE_VALUE) then
      begin
        CloseHandle(hFile);
        hFile := INVALID_HANDLE_VALUE;
      end;
      if (not Assigned(f.mfcbFunction)) then
      begin
        FillChar(src, 2048, 0);
        WideCharToMultiByte(0, 0, srcW, -1, src, 2048, nil, nil);
        if (f.unicode) then
          hFile := CreateFileW(srcW, GENERIC_READ, FILE_SHARE_READ, nil, OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, 0)
        else
        begin
          hFile := CreateFileA(src, GENERIC_READ, FILE_SHARE_READ, nil, OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, 0); // non-unicode
        end;
        CDFiles.ReadingFile := f.FileId;
        if (i >= 0) then
        begin
          {$IFDEF CONDEBUG}
          WriteLn('Reading ' + IntToStr(i) + '/' + IntToStr(CDFiles.NoOfFiles) + ' ' + CDFiles.files[i].LongName + ' from ' + src + ' ' + IntToStr(f.FileSize));
          //WriteLn('Reading ' + IntToStr(i) + ' = ' + IntToStr(f.address) + ' ' + CDFiles.files[i].LongName + ' from ' + src + ' ' + v);
          {$ENDIF}
        end;

        if (hFile = INVALID_HANDLE_VALUE) then
        begin
          Lock;
          CDFiles.ErrorNumber := ERR_CANT_OPEN_FILE;
          CDFiles.Finished := #1;
          Unlock;
          CDFiles.Aborted := True;
          Terminate;
          Exit;
        end;
      end;
      BytesLeft := f.FileSize;
      CDFiles.VerifySize := CDFiles.VerifySize + Sectors(f.FileSize);
      repeat
        if (BytesLeft < AvailableBytesInPacket) then
        begin
          BytesLeft := Sectors(BytesLeft) * 2048;
          BytesToRead := BytesLeft;
        end
        else
          BytesToRead := AvailableBytesInPacket;
        cbGetData := f.mfcbFunction;
        if not Assigned(f.mfcbFunction) then
          ReadFile(hFile, CDFiles.tmpBuffer2[Position], BytesToRead, BytesRead, nil)
        else
        begin
          BytesRead := BytesToRead;
          cbGetData(Self, f, (f.FileSize - BytesLeft), @CDFiles.tmpBuffer2[Position], BytesRead);
        end;

        while (CDFiles.BusyInGetFirst) do
        begin
          if (CDFiles.Aborted = True) then
          begin
            Lock;
            CDFiles.Finished := #1;
            CloseHandle(hFile);
            Unlock;
            //.. CDFiles.ErrorNumber := 1;
            Terminate;
            Exit;
          end;
          Sleep(2);
        end;
        Lock;
        CDFiles.BusyInReading := True;
        if (CDFiles.BusyInGetFirst) then   // for testing only
        begin
          {$IFDEF CONDEBUG}
          WriteLn('');
          WriteLn('Possible error !!!');
          WriteLn('break here');
          {$ENDIF}
          Beep;
          Beep;
          Beep;
          Beep;
        end;
        Position := Position + Integer(BytesToRead);
        AvailableBytesInPacket := AvailableBytesInPacket - Integer(BytesToRead);
        if ((AvailableBytesInPacket = 0) or ((i = Pred(NoOfFiles)) and (BytesLeft - BytesToRead <= 0))) then // either the last dir OR packet completely filled
        begin
          CDFiles.Packets[CDFiles.PacketCount].WritingFile := f.FileId;
          Move(CDFiles.tmpBuffer2, CDFiles.Packets[CDFiles.PacketCount].Packet[0], Position);
          CDFiles.Packets[CDFiles.PacketCount].Size := Position;
          if (((i = Pred(NoOfFiles)) and (BytesLeft - BytesToRead <= 0))) then
            CDFiles.Packets[CDFiles.PacketCount].Size := PacketSize;
          Inc(CDFiles.PacketCount);
          if (not ((i = Pred(NoOfFiles)) and (BytesLeft - BytesToRead <= 0))) then
            CDFiles.Packets[CDFiles.PacketCount].Packet := AllocMem(PacketSize);
          Position := 0;
          AvailableBytesInPacket := PacketSize;
        end;
        BytesLeft := BytesLeft - BytesToRead;
        CDFiles.BytesAvailable := CDFiles.BytesAvailable + BytesToRead;
        CDFiles.BusyInReading := False;
        Unlock;

        if ((CDFiles.PacketCount < (CDFiles.MaxPackets / 4) ) and (CDFiles.vPriority <> tpHighest)) then
        begin
          CDFiles.vPriority := tpHighest;
          Priority := tpHighest;
        end
        else
        if ((CDFiles.PacketCount < (CDFiles.MaxPackets / 2) ) and (CDFiles.vPriority <> tpNormal)) then
        begin
          CDFiles.vPriority := tpNormal;
          Priority := tpNormal;
        end
        else
        if (CDFiles.vPriority <> tpLowest) then // slow down during last half of list being filled
        begin
          CDFiles.vPriority := tpLowest;
          Priority := tpLowest;
        end;
        while (CDFiles.PacketCount >= CDFiles.MaxPackets - 1) do
        begin
          if (CDFiles.vPriority <> tpIdle) then
          begin
            CDFiles.vPriority := tpIdle;
            Priority := tpIdle;
          end;
          CDFiles.CanStart := #1;
          Sleep(20);
          if (CDFiles.Aborted = True) then
          begin
            Lock;
            CDFiles.Finished := #1;
            CloseHandle(hFile);
            Unlock;
            //CDFiles.ErrorNumber := 1;
            Terminate;
            Exit;
          end;
        end;
      until (BytesLeft = 0);
      if (hFile <> INVALID_HANDLE_VALUE) then
      begin
        CloseHandle(hFile);
        hFile := INVALID_HANDLE_VALUE;
      end;
    end;
  end;

  if (Position <> 0) then
  begin
    Lock;
    CDFiles.BusyInReading := True;
    Move(CDFiles.tmpBuffer2, CDFiles.Packets[CDFiles.PacketCount].Packet[0], Position);
    CDFiles.Packets[CDFiles.PacketCount].Size := PacketSize;
    Inc(CDFiles.PacketCount);
    CDFiles.BusyInReading := False;
    Unlock;
  end;

  if ((hFile <> INVALID_HANDLE_VALUE) and (hFile <> INVALID_HANDLE_VALUE)) then
  begin
    CloseHandle(hFile);
    hFile := 0;
  end
  else
    hFile := 0;

  if (CDFiles.UDFBridge) then
  begin
    Lock;
    if (UDFBridgeInfo.IsNewPacketRequiredForLastAVDP) then
    begin
      CDFiles.Packets[CDFiles.PacketCount].Packet := AllocMem(PacketSize);
      BuildAVDP(CDFiles^, CDFiles.Packets[CDFiles.PacketCount].Packet + (PacketSize - BlockSize),
                UDFBridgeInfo.LastAnchorVolumeDescriptorPointer_LSAWRTSession);
      CDFiles.Packets[CDFiles.PacketCount].Size := PacketSize;
      Inc(CDFiles.PacketCount);
    end
    else
    begin
      BuildAVDP(CDFiles^, CDFiles.Packets[CDFiles.PacketCount - 1].Packet + (PacketSize - BlockSize),
                UDFBridgeInfo.LastAnchorVolumeDescriptorPointer_LSAWRTSession);
      CDFiles.Packets[CDFiles.PacketCount - 1].Size := PacketSize;
    end;
    Unlock;
  end;

label1:   // label
  CDFiles.CanStart := #1;
  while (CDFiles.RemainingBlocks > 0) do
  begin
    if (CDFiles.Aborted = True) then
    begin
      Lock;
      CDFiles.Finished := #1;
      if (hFile <> INVALID_HANDLE_VALUE) then
      CloseHandle(hFile);
      Unlock;
      //CDFiles.ErrorNumber := 1;
      Terminate;
      CDFiles.CacheThreadTerminated := true;
      Exit;
    end;
    Sleep(100);
  end;
  CDFiles.Finished := #1;
  Sleep(1000);
  //CDFiles.ErrorNumber := 0;
  Terminate;
  CDFiles.CacheThreadTerminated := true;
  Exit;
end;

(*******************************************************************************
*                                                                              *
*******************************************************************************)
function TCacheThread.GetFirstXX: Cardinal;
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
      Move(CDFiles.Packets[0].Packet, CDFiles.tmpBuffer1, ret);
      FreeMem(CDFiles.Packets[0].Packet);
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

(*******************************************************************************
*                                                                              *
*******************************************************************************)
procedure TCacheThread.OnTerminateEvent;
var
  i: Integer;
begin
  Lock;
  if ((CDFiles.PacketCount <> 0) and (CDFiles.Packets[CDFiles.PacketCount].Packet <> nil)) then // re-check
    FreeMem(CDFiles.Packets[CDFiles.PacketCount].Packet);
  for i:=0 to Pred(CDFiles.PacketCount) do
  begin
    if (CDFiles.Packets[i].Packet <> nil) then
      FreeMem(CDFiles.Packets[i].Packet);
  end;
  CDFiles.PacketCount := 0;
  CDFiles.CacheThreadTerminated := True;
  Unlock;
end;

(*******************************************************************************
*                                                                              *
*******************************************************************************)
procedure TCacheThread.Lock;
begin
  CDFiles.CriticalSection.Enter;
end;

(*******************************************************************************
*                                                                              *
*******************************************************************************)
procedure TCacheThread.Unlock;
begin
  CDFiles.CriticalSection.Leave;
end;

end.

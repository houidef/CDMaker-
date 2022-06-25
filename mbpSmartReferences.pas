unit mbpSmartReferences;

interface

uses
  Windows, Messages, SysUtils, Classes, Math, SyncObjs, Dialogs, mbpCommonLib, mbpTreeTypes, mbpConsts;

{$I mbpDEFINES.INC}

type
  TSmartReferenceThread = class(TThread)
  private
    CDFiles: PCDFiles;
    procedure OnTerminateEvent(Sender: TObject);
  protected
    procedure Execute; override;
  public
    constructor Create(var CDFiles_: TCDFiles); overload;
  end;
  function GenerateSmartReferences(var CDFiles: TCDFiles): Boolean;
  function CalcHash(CDFiles: PCDFiles; FileEntry: pFileEntry; CalcRefProgress: Boolean): Boolean;
  
implementation

uses mbpHash;

var
  files: array [0..MAXFILESONDISC-1] of pFileEntry;

(*******************************************************************************)

(*******************************************************************************)
constructor TSmartReferenceThread.Create(var CDFiles_: TCDFiles);
begin
  inherited Create(True);
  FreeOnTerminate := True;
  CDFiles := @CDFiles_;
  OnTerminate := OnTerminateEvent;
  Priority := tpLower;
  CDFiles.NoOfFiles := CDFiles.FileCounter;
  CDFiles.hSmartReferenceThread := Self.Handle;
  CDFiles.IdSmartReferenceThread := Self.ThreadID;

  if (CDFiles.hCacheThread = INVALID_HANDLE_VALUE) then
  begin
    {$IFDEF CONDEBUG}
    WriteLn('Thread Creation Failed');
    {$ENDIF}
    Exit;
  end;
  //CDFiles.CacheThreadTerminated := False;
  Resume;
end;

procedure TSmartReferenceThread.OnTerminateEvent;
begin

end;

(*******************************************************************************

*******************************************************************************)
function SortFunction( aa: pFileEntry; bb: pFileEntry): Integer;
begin
  if (aa.FileSize < bb.FileSize) then
  begin
    Result := -1;
    exit;
  end
  else if (aa.FileSize > bb.FileSize) then
  begin
    Result := 1;
    exit;
  end;
  result := 0;
end;

(*******************************************************************************

*******************************************************************************)
procedure QuickSort(var base: array of pFileEntry; L, R: Integer);
var
  I, J: Integer;
  P, T: Pointer;
begin
  repeat
    I := L;
    J := R;
    P := base[(L + R) shr 1];
    repeat
      while SortFunction(base[I], P) < 0 do
        Inc(I);
      while (SortFunction(base[J], P) > 0) do
        Dec(J);
      if I <= J then
      begin
        T := base[I];
        base[I] := base[J];
        base[J] := T;
        Inc(I);
        Dec(J);
      end;
    until I > J;
    if L < J then
      QuickSort(base, L, J);
    L := I;
  until I >= R;
end;

function CalcHash(CDFiles: PCDFiles; FileEntry: pFileEntry; CalcRefProgress: Boolean): Boolean;
var
  md5: TMD5Stream;
  fs: TFileStream;
  BytesRead: Integer;
begin
  result := false;
  fs := TFileStream.Create(FileEntry.Path, fmShareDenyNone or fmOpenRead);
  md5 := TMD5Stream.Create;
  while true do
  begin
    BytesRead := fs.Read(CDFiles.tmpBuffer1[0], SizeOf(CDFiles.tmpBuffer1));
    if BytesRead <> 0 then
      md5.Write(CDFiles.tmpBuffer1[0], BytesRead);
    if CalcRefProgress then
    begin
      CDFiles.SmartReferencesProgress := CDFiles.SmartReferencesProgress + Sectors(BytesRead);
    end;
    if (BytesRead <> SizeOf(CDFiles.tmpBuffer1)) then
    begin
      FileEntry.Hash := md5.DigestString;
      break;
    end;
    if CDFiles.Aborted then
    begin
      CDFiles.ComponentStatus := CS_ABORTING;
      Break;
    end;
    
  end;
  fs.Destroy;
  md5.Destroy;
end;

procedure TSmartReferenceThread.Execute;
var
  i, i1, i2: Integer;
  NoOfFiles: Integer;
  f1, f2: pFileEntry;
  CanSaveBytes: Int64;
  //Count: Integer;
begin
  NoOfFiles := 0;
  CDFiles.ErrorNumber := 0;
  CanSaveBytes := 0;
  CDFiles.Aborted := False;
  for i:= 0 to CDFiles.FileCounter - 1 do
  begin
    if (((CDFiles.files[i].Path <> nil) or (Assigned(CDFiles.files[i].mfcbFunction))) and (CDFiles.files[i].FileSize <> 0) and (CDFiles.files[i].DirRecord = nil)) then // and not marked for no smartref
    begin
      files[NoOfFiles] := CDFiles.files[i];
      Inc(NoOfFiles);
    end;
  end;
  //Count := 0;
  if NoOfFiles > 1 then
    QuickSort(files, 0, NoOfFiles-1);
  CDFiles.SmartReferencesProgress := 0;
  for i1 := 0 to NoOfFiles-1 do
  begin
    f1 := Files[i1];
    if f1.ReferenceEntry <> nil then
    begin
      //writeln('i1 here ', i1, ' ', f1.FileSize, ' <> ', f2.FileSize);
      continue;
    end;
    for i2 := i1+1 to NoOfFiles-1 do
    begin
      f2 := Files[i2];
      if f2.ReferenceEntry <> nil then
      begin
        //CDFiles.SmartReferencesProgress := CDFiles.SmartReferencesProgress + Sectors(f2.FileSize);
        continue;
      end;
      if f1.FileSize <> f2.FileSize then
      begin
        //writeln('i2 here ', i2, ' ', f1.FileSize, ' <> ', f2.FileSize);
        if i1+1 = i2 then
          CDFiles.SmartReferencesProgress := CDFiles.SmartReferencesProgress + Sectors(f2.FileSize);
        break;
      end;
      if f1.Hash = '' then
        CalcHash(CDFiles, f1, true);
      if f2.Hash = '' then
        CalcHash(CDFiles, f2, true);
      if (f1.Hash = f2.Hash) then
      begin
        //Writeln(' ', f2.Hash:18, '=', Files[i2].FileSize:10, ' ', AnsiString(Files[i2].LongName):40,' ', i2:3,  ' ', AnsiString(Files[i1].LongName):40, '=', i1:3);
        CanSaveBytes := CanSaveBytes + Sectors(f2.FileSize) * 2048;
        f2.ReferenceEntry := f1;
        if f2.Path <> nil then
        begin
          FreeMem(f2.Path);
          f2.Path := nil;
        end;
        //Inc(Count);
      end
      else
      begin
        //Writeln(' ', f2.Hash:18, '!', Files[i2].FileSize:10, ' ', AnsiString(Files[i2].LongName):40,' ', i2:3,  ' ', AnsiString(Files[i1].LongName):40, '!', i1:3);
        //CDFiles.SmartReferencesProgress := CDFiles.SmartReferencesProgress + Sectors(Files[i2].FileSize);
      end;
      CDFiles.SmartReferencesProgress := CDFiles.SmartReferencesProgress + Sectors(Files[i2].FileSize);
      if CDFiles.Aborted then
      begin
        CDFiles.ComponentStatus := CS_ABORTING;
        CDFiles.ErrorNumber := ERR_ABORTED_BY_USER;
        Break;
      end;

    end;
    if CDFiles.Aborted then
    begin
      Break;
    end;
  end;
  //writeln('Count = ', Count, ' CanSave = ', CanSaveBytes);
  //writeln('Finally ',CDFiles.SmartReferencesProgress, ' / ',CDFiles.SmartReferencesSize);
  CDFiles.SmartReferencesProgress := CDFiles.SmartReferencesSize;
  CDFiles.ComponentStatus := CS_IDLE;
  if Assigned(CDFiles.cbGenSmRefEvent) then
  begin
    if CDFiles.hWNDFE <> 0 then
      PostMessage(CDFiles.hWNDFE, WM_REFGENDONE, CDFiles.ErrorNumber, CanSaveBytes)
    else
      CDFiles.cbGenSmRefEvent(Self, CDFiles.ErrorNumber <> 0, CanSaveBytes);
  end;
  //writeln('Exiting from Ref Gen event');
end;

function GenerateSmartReferences(var CDFiles: TCDFiles): Boolean;
var
  i: Integer;
  NoOfFiles: Integer;
  TotalSize: Cardinal;
  SmartRefThread: TSmartReferenceThread;
begin
  CDFiles.ComponentStatus := CS_GEN_SM_REFERENCES;
  NoOfFiles := 0;
  TotalSize := 0;
  for i:= 0 to CDFiles.FileCounter - 1 do
  begin
    if (((CDFiles.files[i].Path <> nil) or (Assigned(CDFiles.files[i].mfcbFunction))) and (CDFiles.files[i].FileSize <> 0) and (CDFiles.files[i].DirRecord = nil)) then // and not marked for no smartref
    begin
      TotalSize := TotalSize + Sectors(CDFiles.files[i].FileSize);
      files[NoOfFiles] := CDFiles.files[i];
      Inc(NoOfFiles);
    end;
  end;
  CDFiles.SmartReferencesSize := TotalSize;
  SmartRefThread := TSmartReferenceThread.Create(CDFiles);
  SmartRefThread.Resume;
  result := true;
end;
end.

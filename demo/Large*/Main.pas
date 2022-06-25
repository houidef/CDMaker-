unit Main;

interface

//{$WARN UNIT_PLATFORM OFF}

uses
  Windows, StdCtrls, Dialogs, Menus, ExtCtrls, CheckLst, Controls, ComCtrls, Classes, Forms, Buttons,
  mbpMCDBPro, FileCtrl;

type
  TfrmMain = class(TForm)
    Panel2: TPanel;
    Panel3: TPanel;
    Memo1: TMemo;                         
    Panel1: TPanel;
    Panel4: TPanel;
    Panel5: TPanel;
    Label1: TLabel;
    cbDrives: TComboBox;
    tmrWrite: TTimer;
    od: TOpenDialog;
    Panel6: TPanel;
    Panel7: TPanel;
    files: TCheckListBox;
    Panel8: TPanel;
    chkSavePath: TCheckBox;
    Panel9: TPanel;
    lbDir: TDirectoryListBox;
    Splitter2: TSplitter;
    Panel10: TPanel;
    btnClearLayout: TBitBtn;
    lbFiles: TFileListBox;
    Splitter3: TSplitter;
    DriveComboBox1: TDriveComboBox;
    Panel11: TPanel;
    btnBurnISO: TBitBtn;
    btnBurn: TBitBtn;
    Panel13: TPanel;
    lblStatus: TLabel;
    pb: TProgressBar;
    lblSize: TLabel;
    scs: TProgressBar;
    dbs: TProgressBar;
    Splitter1: TSplitter;
    Label2: TLabel;
    cbSpeed: TComboBox;
    PopupMenu2: TPopupMenu;
    ClearAll1: TMenuItem;
    mcdb: TMCDBurnerPro;
    tmrErase: TTimer;
    Panel14: TPanel;
    pbWriteSpeed: TProgressBar;
    lMaxSpeed: TLabel;
    bGSR: TCheckBox;
    btnOtherOptions: TBitBtn;
    Panel15: TPanel;
    btnLoad: TBitBtn;
    btnEject: TBitBtn;
    btnQuickErase: TBitBtn;
    btnFullErase: TBitBtn;
    btnQuickInfo: TBitBtn;
    btnAbortBurn: TBitBtn;
    btnVerify: TButton;
    Button1: TButton;
    procedure FormCreate(Sender: TObject);
    procedure tmrWriteTimer(Sender: TObject);
    procedure filesDragOver(Sender, Source: TObject; X, Y: Integer;
      State: TDragState; var Accept: Boolean);
    procedure filesDragDrop(Sender, Source: TObject; X, Y: Integer);
    procedure cbDrivesChange(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure btnLoadClick(Sender: TObject);
    procedure btnEjectClick(Sender: TObject);
    procedure btnQuickEraseClick(Sender: TObject);
    procedure btnQuickInfoClick(Sender: TObject);
    procedure ClearAll1Click(Sender: TObject);
    procedure btnClearLayoutClick(Sender: TObject);
    procedure filesDblClick(Sender: TObject);
    procedure btnBurnClick(Sender: TObject);
    procedure btnAbortBurnClick(Sender: TObject);
    procedure btnOtherOptionsClick(Sender: TObject);
    procedure Label2Click(Sender: TObject);
    procedure cbSpeedDropDown(Sender: TObject);
    procedure btnBurnISOClick(Sender: TObject);
    procedure tmrEraseTimer(Sender: TObject);
    procedure btnVerifyClick(Sender: TObject);
    procedure mcdbVerifyDone(Sender: TObject; DeviceId: Byte; FileId: Integer; Failed: Boolean);
    procedure mcdbGenerateSmartReferencesDone(Sender: TObject; Failed: Boolean; CanSaveBytes: Integer);
    procedure mcdbWriteDone(Sender: TObject; DeviceId: Byte;
      WriteFailed: Boolean);
    procedure mcdbEraseDone(Sender: TObject; DeviceId: Byte;
      EraseFailed: Boolean);
    procedure mcdbAddFile(Sender: TObject; Path: PWideChar;
      var fd: _WIN32_FIND_DATAW; var Skip: Boolean);
  private
    { Private declarations }
    procedure AddFilesToCDLayout;
    procedure SetSelectedWriteSpeed;
    procedure UpdateProgress(ComponentStatus: Word);
  public
    { Public declarations }
    procedure UpdateControls(Enabled: Boolean);
    procedure Burn(Sender: TObject);
  end;

var
  frmMain: TfrmMain;

implementation

uses
  SysUtils, ShellAPI, Math, Options,
  mbpConsts, mbpDeviceTypes;

const
  status: array [CS_IDLE..CS_GEN_SM_REFERENCES] of PChar = (
  ' IDLE ', ' WRITING ', ' ERASING ', ' BACKGROUND FORMAT ', ' CLOSING TRACK ', ' CLOSING DISC ', ' PREPARING ',
  ' ABORTING ', ' IMPORTING SESSION ', ' BURN ERROR: ABORTING ', ' VERIFYING ', ' GEN. SM. REF ');

  mediums: array [0..19] of PChar = (
  'mtUNKNOWN', 'mtCDROM', 'mtCDR', 'mtCDRW', 'mtDVDROM', 'mtDVDR', 'mtDVDRAM', 'mtDVDRW', 'mtDVDRWRO', 'mtDVDRWSR',
  'mtDVDPLUSRW', 'mtDVDPLUSR', 'mtDVDPLUSRDL', 'mtBDROM', 'mtBDR', 'mtBDRRR', 'mtBDRE', 'mtDDCDROM', 'mtDDCDR', 'mtDDCDRW');
  BoolStr: array[False..True] of String = ('False', 'True');

var
  incr: Integer = 2;

{$R *.dfm}

procedure TfrmMain.FormCreate(Sender: TObject);
var
  i: Integer;
  str: String;
begin
  cbSpeed.Items.Add('Max');
  cbSpeed.ItemIndex := 0;
  mcdb.InitializeASPI(False, '', 'c:\mcdbp.log');

  for i := 0 to Pred(mcdb.DeviceCount) do
  begin
    cbDrives.Items.Add(mcdb.GetDeviceName(i));
  end;
  cbDrives.Items.Add('ISOFile: Don''t Burn, Save as ISO Image ...');
  cbDrives.ItemIndex := 0;
  cbDrives.ItemIndex := cbDrives.Items.Count-1;
  if (cbDrives.Items.Count > 0) then
  begin
    mcdb.SelectDevice(0);
    mcdb.UnlockDrive;
  end;

  str := 'Magic CD/DVD Burner Pro Demo - Version ';
  Caption := str + mcdb.Version;

end;
//---------------------------------------------------------------------------
procedure TfrmMain.UpdateProgress(ComponentStatus: Word);
var
  m, w: Int64;
  buf: String;
begin
  if (ComponentStatus = CS_GEN_SM_REFERENCES) then
  begin
    m := mcdb.SmartReferencesSize;
    w := mcdb.SmartReferencesProgress;
    pb.Max := m;
    pb.Position := w;
    if (w <> 0) then
      buf := FormatFloat('#,##0.00', (w * 2048) / 1024 / 1024) + '/' + FormatFloat('#,##0.00', (m * 2048) / 1024 / 1024) + ' MB (' + FormatFloat('0.00', w / m * 100) + '%)'
    else
      buf := FormatFloat('#,##0.00', (w * 2048) / 1024 / 1024) + '/' + FormatFloat('#,##0.00', (m * 2048) / 1024 / 1024) + ' MB';
    lblSize.Caption := buf;
  end
  else if (mcdb.ComponentStatus = CS_VERIFYING) then
  begin
    m := mcdb.VerifySize;
    w := mcdb.VerifyProgress;
    pb.Max := m;
    pb.Position := w;
    if (w <> 0) then
      buf := FormatFloat('#,##0.00', (w * 2048) / 1024 / 1024) + '/' + FormatFloat('#,##0.00', (m * 2048) / 1024 / 1024) + ' MB (' + FormatFloat('0.00', w / m * 100) + '%) @ ' +FormatFloat('0.00', mcdb.BMReadSpeed)+' KB/s'
    else
      buf := FormatFloat('#,##0.00', (w * 2048) / 1024 / 1024) + '/' + FormatFloat('#,##0.00', (m * 2048) / 1024 / 1024) + ' MB';
    pbWriteSpeed.Position := mcdb.BMReadSpeed;
    lblSize.Caption := buf;
  end
  else if (mcdb.ComponentStatus = CS_WRITING) or (mcdb.ComponentStatus = CS_CLOSINGTRACK) then
  begin
    m := mcdb.ImageSize;
    w := mcdb.BlocksWritten;
    pb.Max := m;
    pb.Position := w;
    if (w <> 0) then
      buf := FormatFloat('#,##0.00', (w * 2048) / 1024 / 1024) + '/' + FormatFloat('#,##0.00', (m * 2048) / 1024 / 1024) + ' MB (' + FormatFloat('0.00', w / m * 100) + '%) @ ' +FormatFloat('0.00', mcdb.BMWriteSpeed)+' KB/s'
    else
      buf := FormatFloat('#,##0.00', (w * 2048) / 1024 / 1024) + '/' + FormatFloat('#,##0.00', (m * 2048) / 1024 / 1024) + ' MB';
    pbWriteSpeed.Position := mcdb.BMWriteSpeed;
    lblSize.Caption := buf;
    m := mcdb.CacheSize;
    w := mcdb.CachePosition;
    scs.Max := m;
    scs.Position := w;
    m := mcdb.DeviceBufferSize;
    w := mcdb.DeviceBufferPosition;
    dbs.Max := m;
    dbs.Position := w;
  end;
  lblStatus.Caption := status[mcdb.ComponentStatus];

end;
procedure TfrmMain.tmrWriteTimer(Sender: TObject);
begin
  UpdateProgress(mcdb.ComponentStatus);
end;
//---------------------------------------------------------------------------

procedure TfrmMain.filesDragOver(Sender, Source: TObject; X, Y: Integer;
  State: TDragState; var Accept: Boolean);
begin
  Accept := (Source is TDirectoryListBox) or (Source is TFileListBox);
end;
//---------------------------------------------------------------------------

procedure TfrmMain.filesDragDrop(Sender, Source: TObject; X, Y: Integer);
var
  i: Integer;
  tmp: String;
begin
  if (Source is TDirectoryListBox) then
  begin
    tmp := lbDir.GetItemPath(lbDir.ItemIndex);
    if (tmp[Length(tmp)] =  '\') then
    begin
        tmp := Copy(tmp, 0, Length(tmp) - 1);
    end;
    files.Items.Add(tmp);
  end;
  if (Source is TFileListBox) then
  begin
    for i := 0 to Pred(lbFiles.Items.Count) do
    begin
      if (lbFiles.Selected[i]) then
      begin
        tmp := lbDir.Directory;
        if (tmp[Length(tmp)] = '\') then
        begin
           tmp := Copy(tmp, 0, Length(tmp) - 1);
        end;
        tmp := tmp + '\' + ExtractFileName(lbFiles.Items.Strings[i]);
        files.Items.Add(tmp);
      end;
    end;
  end;
end;
//---------------------------------------------------------------------------

procedure TfrmMain.cbDrivesChange(Sender: TObject);
begin
  if cbDrives.ItemIndex <> cbDrives.Items.Count-1 then
  begin
    mcdb.SelectDevice(cbDrives.ItemIndex);
    if (mcdb.DeviceIsBurner) then
      btnBurn.Enabled := True
    else
      btnBurn.Enabled := False;
  end;
  btnBurn.Enabled := True
end;
//---------------------------------------------------------------------------

procedure TfrmMain.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Action := caFree;
end;
//---------------------------------------------------------------------------

procedure TfrmMain.btnLoadClick(Sender: TObject);
begin
  mcdb.LoadMedium(True);
end;
//---------------------------------------------------------------------------

procedure TfrmMain.btnEjectClick(Sender: TObject);
begin
  mcdb.FlushCache;
  mcdb.EjectMedium(True)
end;
//---------------------------------------------------------------------------

procedure TfrmMain.btnQuickEraseClick(Sender: TObject);
begin
  if mcdb.TestUnitReady then
  begin
    if mcdb.Erasable then
    begin
      UpdateControls(False);
      SetSelectedWriteSpeed;
      pb.Max := 100;
      tmrErase.Enabled := True;
      Memo1.Lines.Add('>>> ERASING DISC ...');
      mcdb.EraseDisc((Sender as TButton).Tag = -1);
    end
    else
    begin
      Memo1.Lines.Add('>>> DISC IS NOT ERASABLE.');
    end;
   end
   else
   begin
      Memo1.Lines.Add('>>> DRIVE NOT READY.');
   end;
end;
//---------------------------------------------------------------------------

procedure TfrmMain.btnQuickInfoClick(Sender: TObject);
begin
  Memo1.Lines.Add('>>> IS TestUnitReady? ' + BoolStr[mcdb.TestUnitReady]);
  Memo1.Lines.Add('');
  Memo1.Lines.Add('>>> DISC IN DRIVE IS ' + mediums[Ord(mcdb.DiscIs)]);
  Memo1.Lines.Add('>>> IS DISC Erasable? ' + BoolStr[mcdb.Erasable]);
  Memo1.Lines.Add('>>> IS DISC Writable? ' + BoolStr[mcdb.Writable]);
  Memo1.Lines.Add('>>> SessionsOnDisc: ' + IntToStr(mcdb.SessionsOnDisc));
  Memo1.Lines.Add('>>> GetTotalBlocksOnDisc: ' + FormatFloat('#,##0.00', mcdb.TotalBlocksOnDisc * 0.001953125) + ' MB');
  Memo1.Lines.Add('>>> GetFreeBlocksOnDisc: ' + FormatFloat('#,##0.00', mcdb.FreeBlocksOnDisc * 0.001953125) + ' MB');

  Memo1.Lines.Add('>>> GetMaxWriteSpeed: ' + FormatFloat(' #,##0 KB/s', mcdb.MaxWriteSpeed));
  Memo1.Lines.Add('>>> GetReadSpeed: ' + FormatFloat(' #,##0 KB/s', mcdb.ReadSpeed));
  Memo1.Lines.Add('>>> GetWriteSpeed: ' + FormatFloat(' #,##0 KB/s', mcdb.WriteSpeed));
  Memo1.Lines.Add('');
  Memo1.Lines.Add('>>> GetDeviceMaxReadSpeed: ' + FormatFloat(' #,##0 KB/s', mcdb.DeviceMaxReadSpeed));
  Memo1.Lines.Add('>>> GetDeviceMaxWriteSpeed: ' + FormatFloat(' #,##0 KB/s', mcdb.DeviceMaxWriteSpeed));
  Memo1.Lines.Add('>>> DEVICE CAPABLE OF DC_TEST_WRITE? ' + BoolStr[mcdb.DeviceCan(DC_TEST_WRITE)]);
  Memo1.Lines.Add('>>> DEVICE CAPABLE OF DC_UNDERRUNPROTECTION? ' + BoolStr[mcdb.DeviceCan(DC_UNDERRUNPROTECTION)]);
  Memo1.Lines.Add('>>> DEVICE CAPABLE OF READ_CDR? ' + BoolStr[mcdb.DeviceCan(DC_READ_CDR)]);
  Memo1.Lines.Add('>>> DEVICE CAPABLE OF READ_CDRW? ' + BoolStr[mcdb.DeviceCan(DC_READ_CDRW)]);
  Memo1.Lines.Add('>>> DEVICE CAPABLE OF READ_DVDRAM? ' + BoolStr[mcdb.DeviceCan(DC_READ_DVDRAM)]);
  Memo1.Lines.Add('>>> DEVICE CAPABLE OF READ_DVDR? ' + BoolStr[mcdb.DeviceCan(DC_READ_DVDR)]);
  Memo1.Lines.Add('>>> DEVICE CAPABLE OF READ_DVDRW? ' + BoolStr[mcdb.DeviceCan(DC_READ_DVDRW)]);
  Memo1.Lines.Add('>>> DEVICE CAPABLE OF READ_DVDPLUSR? ' + BoolStr[mcdb.DeviceCan(DC_READ_DVDPLUSR)]);
  Memo1.Lines.Add('>>> DEVICE CAPABLE OF READ_DVDPLUSRDL? ' + BoolStr[mcdb.DeviceCan(DC_READ_DVDPLUSRDL)]);
  Memo1.Lines.Add('>>> DEVICE CAPABLE OF READ_DVDPLUSRW? ' + BoolStr[mcdb.DeviceCan(DC_READ_DVDPLUSRW)]);
  Memo1.Lines.Add('>>> DEVICE CAPABLE OF READ_HDDVDROM? ' + BoolStr[mcdb.DeviceCan(DC_READ_HDDVDROM)]);
  Memo1.Lines.Add('>>> DEVICE CAPABLE OF READ_HDDVDR? ' + BoolStr[mcdb.DeviceCan(DC_READ_HDDVDR)]);
  Memo1.Lines.Add('>>> DEVICE CAPABLE OF READ_HDDVDRAM? ' + BoolStr[mcdb.DeviceCan(DC_READ_HDDVDRAM)]);
  Memo1.Lines.Add('>>> DEVICE CAPABLE OF READ_BDROM? ' + BoolStr[mcdb.DeviceCan(DC_READ_BDROM)]);
  Memo1.Lines.Add('>>> DEVICE CAPABLE OF READ_BDR? ' + BoolStr[mcdb.DeviceCan(DC_READ_BDR)]);
  Memo1.Lines.Add('>>> DEVICE CAPABLE OF READ_BDRE? ' + BoolStr[mcdb.DeviceCan(DC_READ_BDRE)]);

  Memo1.Lines.Add('>>> DEVICE CAPABLE OF READ_HDDVDROM? ' + BoolStr[mcdb.DeviceCan(DC_READ_HDDVDROM)]);
  Memo1.Lines.Add('>>> DEVICE CAPABLE OF WRITE_CDR? ' + BoolStr[mcdb.DeviceCan(DC_WRITE_CDR)]);
  Memo1.Lines.Add('>>> DEVICE CAPABLE OF WRITE_CDRW? ' + BoolStr[mcdb.DeviceCan(DC_WRITE_CDRW)]);
  Memo1.Lines.Add('>>> DEVICE CAPABLE OF WRITE_DVDRAM? ' + BoolStr[mcdb.DeviceCan(DC_WRITE_DVDRAM)]);
  Memo1.Lines.Add('>>> DEVICE CAPABLE OF WRITE_DVDR? ' + BoolStr[mcdb.DeviceCan(DC_WRITE_DVDR)]);
  Memo1.Lines.Add('>>> DEVICE CAPABLE OF WRITE_DVDRW? ' + BoolStr[mcdb.DeviceCan(DC_WRITE_DVDRW)]);
  Memo1.Lines.Add('>>> DEVICE CAPABLE OF WRITE_DVDPLUSR? ' + BoolStr[mcdb.DeviceCan(DC_WRITE_DVDPLUSR)]);
  Memo1.Lines.Add('>>> DEVICE CAPABLE OF WRITE_DVDPLUSRDL? ' + BoolStr[mcdb.DeviceCan(DC_WRITE_DVDPLUSRDL)]);
  Memo1.Lines.Add('>>> DEVICE CAPABLE OF WRITE_DVDPLUSRW? ' + BoolStr[mcdb.DeviceCan(DC_WRITE_DVDPLUSRW)]);
  Memo1.Lines.Add('>>> DEVICE CAPABLE OF WRITE_HDDVDR? ' + BoolStr[mcdb.DeviceCan(DC_WRITE_HDDVDR)]);
  Memo1.Lines.Add('>>> DEVICE CAPABLE OF WRITE_HDDVDRAM? ' + BoolStr[mcdb.DeviceCan(DC_WRITE_HDDVDRAM)]);
  Memo1.Lines.Add('>>> DEVICE CAPABLE OF WRITE_BDR? ' + BoolStr[mcdb.DeviceCan(DC_WRITE_BDR)]);
  Memo1.Lines.Add('>>> DEVICE CAPABLE OF WRITE_BDRE? ' + BoolStr[mcdb.DeviceCan(DC_WRITE_BDRE)]);
end;
//---------------------------------------------------------------------------

procedure TfrmMain.ClearAll1Click(Sender: TObject);
begin
  Memo1.Clear;
end;
//---------------------------------------------------------------------------

procedure TfrmMain.btnClearLayoutClick(Sender: TObject);
begin
  files.Items.Clear;
end;
//---------------------------------------------------------------------------

procedure TfrmMain.filesDblClick(Sender: TObject);
begin
  if (files.ItemIndex <> -1) then
  begin
    files.Items.Delete(files.ItemIndex);
  end;
end;
//---------------------------------------------------------------------------
procedure TfrmMain.Burn(Sender: TObject);
var
  s: String;
  st: TSYSTEMTIME;
  FileName: String;
begin
  if cbDrives.ItemIndex = cbDrives.Items.Count-1 then
  begin
    FileName := 'm:\mcdb.iso';

    if InputQuery('Build ISO', 'Enter ISO File Name to be built', FileName) then
    begin
      mcdb.Prepare;

      if mcdb.BuildISOImage(FileName) then
      begin
        tmrWrite.Enabled := True;
        UpdateControls(False);
        GetSystemTime(st);
        Memo1.Lines.Add(Format('%0.2d:%0.2d:%0.3d', [st.wMinute, st.wSecond, st.wMilliseconds])+' >>> BUILDING ISO FILE ...');
        btnAbortBurn.Visible := True;
      end
      else
      begin
        Memo1.Lines.Add('>>> BUILDING ISO FAILED. Error: ' + mcdb.ErrorString);
      end;
    end;
  end
  else
  begin
    if (frmOptions.chkImportSession.Checked) then
    begin
      mcdb.ImportSession(StrToInt(frmOptions.edtImportSession.Text), '\');
    end;
    SetSelectedWriteSpeed;
    if (frmOptions.chkBootImage.Checked) then
    begin
      mcdb.BootImage := frmOptions.edtBootImage.Text;
    end;

    lMaxSpeed.Caption := FormatFloat('#0', mcdb.WriteSpeed)+' kB/s';
    pbWriteSpeed.Max := mcdb.WriteSpeed;
    mcdb.Prepare;

    if mcdb.Burn then
    begin
      tmrWrite.Enabled := True;
      UpdateControls(False);
      GetSystemTime(st);
      Memo1.Lines.Add(Format('%0.2d:%0.2d:%0.3d', [st.wMinute, st.wSecond, st.wMilliseconds])+' >>> WRITING DISC ... ');
      btnAbortBurn.Visible := True;
    end
    else
    begin
      s := '>>> WRITE FAILED. Error: ' + mcdb.ErrorString;
      Memo1.Lines.Add(s);
    end;
  end;
end;

procedure TfrmMain.mcdbGenerateSmartReferencesDone(Sender: TObject;
  Failed: Boolean; CanSaveBytes: Integer);
var
  st: TSYSTEMTIME;
begin
  GetSystemTime(st);
  if not Failed then
  begin
    tmrWriteTimer(Sender);
    Memo1.Lines.Add(Format('%0.2d:%0.2d:%0.3d', [st.wMinute, st.wSecond, st.wMilliseconds])+' >>> SMART REFERENCE GENERATION DONE, SAVES '+IntToStr(CanSaveBytes div 1024 div 1024)+' MB');
    tmrWrite.Enabled := False;
    Burn(Self);
  end
  else
  begin
    Memo1.Lines.Add(' >>> SMART REFERENCE GENERATION FAILED, ABORTING');

  end;
end;

procedure TfrmMain.btnBurnClick(Sender: TObject);
begin
  mcdb.ClearAll;
  if bGSR.Checked then
  begin
    AddFilesToCDLayout;
    tmrWrite.Enabled := True;
    btnAbortBurn.Visible := True;
    mcdb.GenerateSmartReferences;
  end
  else
  begin
    AddFilesToCDLayout;
    //mcdb.InsertDir( '\', 'c:\pe\bartpe\', '*.*', true, false);
    mcdb.InsertDir( '\', 'c:\ultra', '*.*', true, false);
//    mcdb.BootImage := 'C:\pe\BartPE\bootsect.bin';
    Burn(Self);
  end;

end;
//---------------------------------------------------------------------------
procedure TfrmMain.btnAbortBurnClick(Sender: TObject);
begin
  mcdb.AbortOperation;
end;

//---------------------------------------------------------------------------
procedure TfrmMain.btnOtherOptionsClick(Sender: TObject);
begin
  frmOptions.ShowModal;
end;

//---------------------------------------------------------------------------
procedure TfrmMain.AddFilesToCDLayout;
var
  s: String;
  i: Integer;
begin
  //mcdb.InsertDir('\', 'c:\111', '*.*', True, true);
  for i := 0 to Pred(files.Items.Count) do
  begin
    s := files.Items.Strings[i];

    if (DirectoryExists(s)) then
    begin
       if (mcdb.InsertDir('\', PChar(s), '*.*', True, chkSavePath.Checked) = nil) then
         Memo1.Lines.Add('Failed or Skipped at ... ');
    end
    else
    if (FileExists(s)) then
    begin
       mcdb.InsertFile('\', PChar(s));
    end;
  end;

end;

//---------------------------------------------------------------------------
procedure TfrmMain.Label2Click(Sender: TObject);
begin
  ShellExecute(Self.Handle, 'open', PChar('http://www.binarymagics.com'), nil, nil, SW_SHOWNORMAL);
end;

//---------------------------------------------------------------------------
procedure TfrmMain.cbSpeedDropDown(Sender: TObject);
var
  li: Integer;
  speed, MaxSpeed: Integer;
  ActSpd: LongWord;
  Divider: Double;
  str : String;
begin
  li := cbSpeed.ItemIndex;
  cbspeed.Items.Clear;
  cbSpeed.Items.Add('Max');
  if (mcdb.DiscIs >= mtDVDROM) and (mcdb.DiscIs <= mtDVDPLUSRDL) then
    Divider := 1385
  else if (mcdb.DiscIs >= mtBDROM) and (mcdb.DiscIs <= mtHDDVDRAM) then
    Divider := 4495.5
  else
    Divider := 176.46;
  MaxSpeed := Round(mcdb.MaxWriteSpeed / divider);

  for speed := MaxSpeed downto 1 do
  begin
    mcdb.SetDeviceSpeed(0, Round(speed * divider));
    ActSpd := mcdb.WriteSpeed;
    str := FormatFloat('0x', Round(ActSpd / divider)) + FormatFloat(' (#,##0 KB/s)', ActSpd);
    if cbSpeed.Items.IndexOf(str) < 0 then
      cbSpeed.Items.Add(str);
  end;
  if li = -1 then
    cbSpeed.ItemIndex := 0
  else
    if li <= cbSpeed.Items.Count then
      cbSpeed.ItemIndex := li
    else
      cbSpeed.ItemIndex := 0;
end;
//---------------------------------------------------------------------------

procedure TfrmMain.SetSelectedWriteSpeed;
var
  Speed: Integer;
  Divider: Double;
  str : String;
begin
  if (mcdb.DiscIs >= mtDVDROM) then
    Divider := 1385
  else
    Divider := 176.46;

  str := cbSpeed.Items[cbSpeed.ItemIndex];
  if (str = 'Max') then
    Speed := 0
  else
  begin
    str := Copy(str, 1, Pos('x', str) - 1);
    Speed := StrToInt(str);
  end;

  mcdb.SetDeviceSpeed(Round(Speed * Divider), Round(Speed * Divider));
end;
//---------------------------------------------------------------------------

procedure TfrmMain.btnBurnISOClick(Sender: TObject);
var
  st: TSYSTEMTIME;
  s: String;
begin
  if (od.Execute) then
  begin
  
    if mcdb.BurnISOImage(od.FileName) then
    begin
      tmrWrite.Enabled := True;
      UpdateControls(False);
      GetSystemTime(st);
      Memo1.Lines.Add(Format('%0.2d:%0.2d:%0.3d', [st.wMinute, st.wSecond, st.wMilliseconds])+' >>> WRITING ISO IMAGE TO DISC ... ');
      btnAbortBurn.Visible := True;
    end
    else
    begin
      s := '>>> WRITE ISO IMAGE START FAILED. Error: ' + mcdb.ErrorString;
      Memo1.Lines.Add(s);
    end;


  end;
end;
//---------------------------------------------------------------------------

procedure TfrmMain.UpdateControls(Enabled: Boolean);
begin
  cbDrives.Enabled := Enabled;
  cbSpeed.Enabled := Enabled;
  btnLoad.Enabled := Enabled;
  btnEject.Enabled := Enabled;
  btnQuickErase.Enabled := Enabled;
  btnFullErase.Enabled := Enabled;
  btnQuickInfo.Enabled := Enabled;
  btnOtherOptions.Enabled := Enabled;
  btnBurnISO.Enabled := Enabled;
  btnBurn.Enabled := Enabled;
  btnVerify.Enabled := Enabled;

  // AMD
  if Enabled then
  begin
    mcdb.FlushCache;
    mcdb.EjectMedium(True);
  end;
end;
//---------------------------------------------------------------------------

procedure TfrmMain.tmrEraseTimer(Sender: TObject);
begin
  pb.Position := pb.Position + incr;
  if pb.Position = 100 then
  begin
    incr := -1;
  end
  else
  if pb.Position = 0 then
  begin
    incr := 1;
  end;
  lblStatus.Caption := status[mcdb.ComponentStatus];
end;
//---------------------------------------------------------------------------

procedure TfrmMain.btnVerifyClick(Sender: TObject);
begin
  //mcdb.ClearAll;
  //mcdb.InsertDir('\', 'C:\111', '*.*', True, True);
  //mcdb.Prepare;
  pbWriteSpeed.Max := mcdb.ReadSpeed;
  lMaxSpeed.Caption := FormatFloat('#0', mcdb.ReadSpeed)+' kB/s';
  tmrWrite.Enabled := True;
  mcdb.VerifyDisc(True, True);
  btnAbortBurn.Visible := True;
  //mcdb.GenerateSmartReferences;
  {md5 := TMD5Stream.Create;
  md5.Write(str, 10);
  s2 := md5.DigestString;

  WriteLn(s2, 10);
  for i:=1 to 16 do
  begin
    Write(IntToHex(Ord(s2[i]), 2));
  end;
  Writeln;
  WriteLn(IntToHex(md5.Digest[0], 9), IntToHex(md5.Digest[1], 4), IntToHex(md5.Digest[2], 4), IntToHex(md5.Digest[3], 4));}
  
end;

procedure TfrmMain.mcdbVerifyDone(Sender: TObject; DeviceId: Byte;
  FileId: Integer; Failed: Boolean);
begin
  if (FileId = -1) then // Final Results
  begin
    if Failed then
      Memo1.Lines.Add('VERIFING DONE, AT LEAST ONE FILE FAILED')
    else
      Memo1.Lines.Add('VERIFING DONE, ALL FILES OKAY');
    btnAbortBurn.Visible := False;
  end
  else
  begin
    if Failed then
      Memo1.Lines.Add('VERIFY FAILED FOR '+mcdb.FileName(FileId))
    else
      Memo1.Lines.Add('VERIFING DONE FOR '+mcdb.FileName(FileId));
  end;
end;

procedure TfrmMain.mcdbWriteDone(Sender: TObject; DeviceId: Byte; WriteFailed: Boolean);
var
  s: String;
  st: TSYSTEMTIME;
begin
  pb.Position := 0;
  btnAbortBurn.Visible := False;
  GetSystemTime(st);
  if (WriteFailed) then
  begin
    s := Format('%0.2d:%0.2d:%0.3d', [st.wMinute, st.wSecond, st.wMilliseconds])+' >>> WRITE DONE WITH ERROR (';
    s := s + mcdb.ErrorString;
    s := s + ').';
    Memo1.Lines.Add(s);
  end
  else
  begin
    Memo1.Lines.Add(Format('%0.2d:%0.2d:%0.3d', [st.wMinute, st.wSecond, st.wMilliseconds])+' >>> WRITE DONE.');
  end;
  UpdateControls(True);
  UpdateProgress(0);
  tmrWrite.Enabled := False;
end;

procedure TfrmMain.mcdbAddFile(Sender: TObject; Path: PWideChar;
  var fd: _WIN32_FIND_DATAW; var Skip: Boolean);
begin
  //Memo1.Lines.Add(String(fd.cFileName)+'  '+ IntToStr(  lstrlenW(fd.cFileName) ));
{  if lstrlenW(fd.cFileName) > 107 then
  begin
    // Either make it shorter or Skip
  end;}
  //CharUpperBuffW(fd.cFileName, lstrlenW(fd.cFileName));
end;

procedure TfrmMain.mcdbEraseDone(Sender: TObject; DeviceId: Byte; EraseFailed: Boolean);
begin
  tmrErase.Enabled := False;
  pb.Position := 0;
  if (EraseFailed) then
  begin
    Memo1.Lines.Add('>>> ERASE DONE WITH ERROR.');
  end
  else
  begin
    Memo1.Lines.Add('>>> ERASE DONE.');
  end;
  UpdateControls(True);
end;

end.

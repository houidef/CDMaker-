unit Main;

interface

uses
  StdCtrls, Controls, Classes, ExtCtrls, Forms, mbpMCDBPro;

type
  TfrmMain = class(TForm)
    Memo1: TMemo;
    Label1: TLabel;
    cbDrives: TComboBox;
    btnBurn: TButton;
    btnErase: TButton;
    mcdb: TMCDBurnerPro;
    procedure btnBurnClick(Sender: TObject);
    procedure cbDrivesChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure btnEraseClick(Sender: TObject);
    procedure mcdbWriteDone(Sender: TObject; WriteFailed: Boolean);
    procedure mcdbEraseDone(Sender: TObject; EraseFailed: Boolean);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmMain: TfrmMain;

implementation

{$R *.dfm}

procedure TfrmMain.btnBurnClick(Sender: TObject);
begin
  {Note: Just 4 simple steps are required to burn a disc.}
  mcdb.ClearAll;                                                // 1. Clear Disc Layout
  Memo1.Lines.Add('>>> INSERTING FILE ''.\Soap Bubbles.bmp''');
  mcdb.UDFBridge := true;
  mcdb.CreateDir('\', 'AUDIO_TS');
  mcdb.CreateDir('\', 'VIDEO_TS');
  // Insert the file (sorted by name)
  mcdb.InsertFile('\VIDEO_TS', 'c:\1\VIDEO_TS\VIDEO_TS.BUP');
  mcdb.InsertFile('\VIDEO_TS', 'c:\1\VIDEO_TS\VIDEO_TS.IFO');
  mcdb.InsertFile('\VIDEO_TS', 'c:\1\VIDEO_TS\VIDEO_TS.VOB');
  mcdb.InsertFile('\VIDEO_TS', 'c:\1\VIDEO_TS\VTS_01_0.BUP');
  mcdb.InsertFile('\VIDEO_TS', 'c:\1\VIDEO_TS\VTS_01_0.IFO');
  mcdb.InsertFile('\VIDEO_TS', 'c:\1\VIDEO_TS\VTS_01_1.VOB');
  mcdb.Prepare;                                                 // 3. Commit your inputs
  //mcdb.Burn;                                                    // 4. Start Burn operation
  mcdb.BuildISOImage('c:\delphivideo.iso');
end;
//---------------------------------------------------------------------------

procedure TfrmMain.cbDrivesChange(Sender: TObject);
begin
  mcdb.SelectDevice(cbDrives.ItemIndex);
  if (mcdb.DeviceIsBurner) then
  begin
    btnBurn.Enabled := True;
    btnErase.Enabled := True;
  end
  else
  begin
    btnBurn.Enabled := False;
    btnErase.Enabled := False;
  end;
end;
//---------------------------------------------------------------------------

procedure TfrmMain.FormCreate(Sender: TObject);
var
  i: Integer;
begin
  mcdb.InitializeASPI(False);

  for i := 0 to Pred(mcdb.DeviceCount) do
  begin
    cbDrives.Items.Add(mcdb.GetDeviceName(i));
  end;
  if (cbDrives.Items.Count <> 0) then
  begin
    cbDrives.ItemIndex := 0;
    cbDrivesChange(Sender);
  end;
end;
//---------------------------------------------------------------------------

procedure TfrmMain.btnEraseClick(Sender: TObject);
begin
  mcdb.EraseDisc(True);
end;
//---------------------------------------------------------------------------

procedure TfrmMain.mcdbWriteDone(Sender: TObject; WriteFailed: Boolean);
begin
  if (not WriteFailed) then
  begin
    Memo1.Lines.Add('>>> WRITE DONE.');
  end
  else
  begin
    Memo1.Lines.Add('>>> WRITE DONE WITH ERROR: ' + mcdb.ErrorString);
  end;
end;
//---------------------------------------------------------------------------

procedure TfrmMain.mcdbEraseDone(Sender: TObject; EraseFailed: Boolean);
begin
  if (EraseFailed) then
  begin
    Memo1.Lines.Add('>>> ERASE DONE WITH ERROR: ' + mcdb.ErrorString);
  end
  else
  begin
    Memo1.Lines.Add('>>> ERASE DONE.');
  end;
end;
//---------------------------------------------------------------------------

end.

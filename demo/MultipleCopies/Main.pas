unit Main;

interface

uses
  StdCtrls, Controls, Classes, ExtCtrls, Forms,
  mbpMCDBPro;

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
    procedure mcdbWriteDone(Sender: TObject; DeviceId: Byte;
      WriteFailed: Boolean);
    procedure mcdbEraseDone(Sender: TObject; DeviceId: Byte;
      EraseFailed: Boolean);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

type
  TBurnThread = class(TThread)
  private
    procedure StartBurn;
  protected
    procedure Execute; override;
  public
    constructor Create; overload;
  end;  

var
  frmMain: TfrmMain;

implementation

uses
  Windows, Dialogs;

{$R *.dfm}

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

procedure TfrmMain.btnBurnClick(Sender: TObject);
var
  BurnThread: TThread;
begin
  BurnThread := TBurnThread.Create;
  BurnThread.Resume;
end;
//---------------------------------------------------------------------------

procedure TfrmMain.btnEraseClick(Sender: TObject);
begin
  mcdb.EraseDisc(True);
end;
//---------------------------------------------------------------------------

//---------------------------------------------------------------------------

constructor TBurnThread.Create;
begin
  inherited Create(True);
  FreeOnTerminate := True;
end;
//---------------------------------------------------------------------------

procedure TBurnThread.Execute;
begin
  Synchronize(StartBurn);
end;
//---------------------------------------------------------------------------

procedure TBurnThread.StartBurn;
begin
  frmMain.mcdb.ClearAll;
  frmMain.mcdb.InsertFile('\', '.\Soap Bubbles.bmp');
  frmMain.Memo1.Lines.Add('>>> INSERTING FILE ''.\Soap Bubbles.bmp''');
  frmMain.mcdb.Prepare;
  frmMain.mcdb.Burn;
end;
//---------------------------------------------------------------------------

procedure TfrmMain.mcdbWriteDone(Sender: TObject; DeviceId: Byte;
  WriteFailed: Boolean);
var
  BurnThread: TThread;
begin
  if (not WriteFailed) then
  begin
    Memo1.Lines.Add('>>> WRITE DONE.');
  end
  else
  begin
    Memo1.Lines.Add('>>> WRITE DONE WITH ERROR: ' + mcdb.ErrorString);
  end;

  if (MessageBox(Self.Handle, 'Burn another disc? If yes, then please replace media in drive before proceeding.', 'Magic CD/DVD Burner (Pro)', MB_OKCANCEL) = IDOK) then
  begin
    BurnThread := TBurnThread.Create;
    BurnThread.Resume;
  end;
end;
//---------------------------------------------------------------------------


procedure TfrmMain.mcdbEraseDone(Sender: TObject; DeviceId: Byte;
  EraseFailed: Boolean);
begin
  if (EraseFailed) then
  begin
    frmMain.Memo1.Lines.Add('>>> ERASE DONE WITH ERROR: ' + mcdb.ErrorString);
  end
  else
  begin
    frmMain.Memo1.Lines.Add('>>> ERASE DONE.');
  end;
end;

end.

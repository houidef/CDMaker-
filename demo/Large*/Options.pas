unit Options;

interface

uses
  Controls, StdCtrls, Classes, Forms, ExtCtrls, Dialogs, Buttons;

type
  TfrmOptions = class(TForm)
    chkImportSession: TCheckBox;
    edtImportSession: TEdit;
    edtVolumeID: TEdit;
    Label1: TLabel;
    GroupBox1: TGroupBox;
    chkTestWrite: TCheckBox;
    chkUDFBridge: TCheckBox;
    chkFinalizeDisc: TCheckBox;
    chkJolietFS: TCheckBox;
    chkUnderrunProtection: TCheckBox;
    Label2: TLabel;
    edtCacheSize: TEdit;
    chkBootImage: TCheckBox;
    edtBootImage: TEdit;
    dlg: TOpenDialog;
    SpeedButton1: TSpeedButton;
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure SpeedButton1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmOptions: TfrmOptions;

implementation

uses
  SysUtils, Main;

{$R *.dfm}

procedure TfrmOptions.FormCreate(Sender: TObject);
begin
  chkTestWrite.Checked := frmMain.mcdb.TestWrite;
  chkUDFBridge.Checked := frmMain.mcdb.UDFBridge;
  chkFinalizeDisc.Checked := frmMain.mcdb.FinalizeDisc;
  chkJolietFS.Checked := frmMain.mcdb.JolietFileSystem;
  chkUnderrunProtection.Checked := frmMain.mcdb.BurnProof;
  edtVolumeID.Text := frmMain.mcdb.VolumeID;
  edtCacheSize.Text := IntToStr(frmMain.mcdb.CacheSize);
  edtBootImage.Text := frmMain.mcdb.BootImage;
  if (edtBootImage.Text <> '') then
  begin
    chkBootImage.Checked := True;
  end;
end;
//---------------------------------------------------------------------------

procedure TfrmOptions.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  frmMain.mcdb.TestWrite := chkTestWrite.Checked;
  frmMain.mcdb.UDFBridge := chkUDFBridge.Checked;
  frmMain.mcdb.FinalizeDisc := chkFinalizeDisc.Checked;
  frmMain.mcdb.JolietFileSystem := chkJolietFS.Checked;
  frmMain.mcdb.BurnProof := chkUnderrunProtection.Checked;
  frmMain.mcdb.VolumeID := edtVolumeID.Text;
  frmMain.mcdb.CacheSize := StrToIntDef(edtCacheSize.Text, 2); // Component cache size should be at least 2 MB
end;
//---------------------------------------------------------------------------

procedure TfrmOptions.SpeedButton1Click(Sender: TObject);
begin
  dlg.FileName := edtBootImage.Text;
  if dlg.Execute then
  begin
    edtBootImage.Text := dlg.FileName;
  end;
end;
//---------------------------------------------------------------------------

end.

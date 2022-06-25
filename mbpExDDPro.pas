{*******************************************************************************
  Unit        : mbpExDDPro.pas
  Date        : Mar 2002 - Nov 2002
  Description :
  Copyright   : 2001-02 Binary Magic, All rights reserved.
{******************************************************************************}

unit mbpExDDPro;

interface

uses
  Classes, Windows, Messages, SysUtils, Forms, Controls, ShellApi;

{$I mbpDEFINES.INC}
{$DEBUGINFO OFF}
{$LOCALSYMBOLS OFF}

type
  TDroppedEvent = procedure (Sender: TObject; ItemsCount: Integer) of object;
  TExDragDropPro = class(TComponent)
  private
    FItems: TStrings;
    FWndHandle: THandle;
    DefProc: Integer;
    FOnDropped: TDroppedEvent;
    FControl : TWinControl;
    ItemsCount: Integer;
    procedure DropFiles( hDropHandle: LongInt );
    procedure WndProc( var Msg: TMessage );
    procedure SetControl(Value : TWinControl);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property Items: TStrings read FItems;
    property Control: TWinControl read FControl write SetControl;
    property OnDropped: TDroppedEvent read FOnDropped write FOnDropped;
  end;

procedure Register;

implementation
var
  ProcInstance: Pointer;

procedure Register;
begin
  RegisterComponents('Magic CD Burner (Pro)', [TExDragDropPro]);
end;

constructor TExDragDropPro.Create( AOwner: TComponent );
begin
  FItems := TStringList.Create;
  ItemsCount := 0;
  FWndHandle := 0;
  inherited Create( AOwner );
end;

destructor TExDragDropPro.Destroy;
begin
  if FWndHandle <> 0 then
  begin
    SetWindowLong(FWndHandle, GWL_WNDPROC, DefProc);
    FreeObjectInstance(ProcInstance);
  end;
  FItems.Free;
  inherited Destroy;
end;

procedure TExDragDropPro.SetControl(Value:TWinControl);
begin
  if Value = nil then
  begin
    SetWindowLong( FWndHandle, GWL_WNDPROC, DefProc );
    FreeObjectInstance(ProcInstance);
    FControl := nil;
    exit;
  end
  else
  if Value <> FControl then
  begin
    FControl := Value;
    if FControl is TWinControl then
    begin
      FWndHandle := FControl.Handle;
      ProcInstance := MakeObjectInstance( WndProc );
      DefProc := GetWindowLong(FWndHandle, GWL_WNDPROC);
      SetWindowLong( FWndHandle, GWL_WNDPROC, Longint( ProcInstance ));
      DragAcceptFiles( FWndHandle, True );
    end;
  end;
end;

procedure TExDragDropPro.DropFiles( hDropHandle: LongInt );
var
  FullName: PAnsiChar;
  EntryNum: Integer;
begin
  FullName := AllocMem( 2048 );
  EntryNum := 0;
  FItems.Clear;
  ItemsCount := DragQueryFile( hDropHandle, $FFFFFFFF, nil, 2048 );
  while ( EntryNum < ItemsCount ) do
  begin
   DragQueryFileA( hDropHandle, EntryNum, FullName, 2048 );
   FItems.Add( String(StrPas( FullName )));
   Inc( EntryNum );
  end;
  StrDispose( FullName );
  if Assigned(FOnDropped) then
    FOnDropped(Self, ItemsCount);
end;

procedure TExDragDropPro.WndProc( var Msg: TMessage );
begin
  with Msg do
  begin
    if Msg = WM_DROPFILES then
      DropFiles( HDrop( wParam ))
    else
      Result := CallWindowProc( Pointer(DefProc), FWndHandle, Msg, WParam, LParam);
  end;
end;

end.

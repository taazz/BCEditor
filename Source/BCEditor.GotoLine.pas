unit BCEditor.GotoLine;

interface {********************************************************************}

uses
  Classes,
  Forms, Controls, StdCtrls;

type
  TGotoLineDialog = class(TForm)
    FBCancel: TButton;
    FBOk: TButton;
    FGroupBox: TGroupBox;
    FLine: TEdit;
    FLLine: TLabel;
    procedure FormShow(Sender: TObject);
    procedure FormHide(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure FormCreate(Sender: TObject);
  public
    Line: Integer;
    Max: Integer;
    Min: Integer;
    function Execute(): Boolean;
  end;

implementation {***************************************************************}

uses
  Windows,
  SysUtils, Types, Math,
  BCEditor.Language;

{$R *.dfm}

{ TDGotoLine ******************************************************************}

function TGotoLineDialog.Execute(): Boolean;
begin
  Result := ShowModal() = mrOk;
end;

procedure TGotoLineDialog.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  if (ModalResult = mrOk) then
  begin
    if ((StrToInt(FLine.Text) < Min) or (Max < StrToInt(FLine.Text))) then
    begin
      FLine.SelectAll();
      ActiveControl := FLine;
      MessageBeep(MB_ICONERROR);
    end;
  end;
end;

procedure TGotoLineDialog.FormCreate(Sender: TObject);
begin
  Caption := SBCEditorGotoLineCaption;
  FGroupBox.Caption := '';
  FLLine.Caption := SBCEditorGotoLineLine + ':';
  FBOk.Caption := SBCEditorOk;
  FBCancel.Caption := SBCEditorCancel;
end;

procedure TGotoLineDialog.FormHide(Sender: TObject);
begin
  if (ModalResult = mrOk) then
    Line := StrToInt(FLine.Text);
end;

procedure TGotoLineDialog.FormShow(Sender: TObject);
begin
  if (Owner is TControl) then
  begin
    Left := Math.Max(0, Math.Min(Screen.Width - Width, TControl(Owner).ClientToScreen(Point(0, 0)).X + TControl(Owner).Width div 2 - Width div 2));
    Top := Math.Max(0, Math.Min(Screen.Width - Width, TControl(Owner).ClientToScreen(Point(0, 0)).Y + TControl(Owner).Height div 2 - Height div 2));
  end;

  FLine.Text := IntToStr(Line);
  FLine.MaxLength := Length(IntToStr(Max));
end;

end.

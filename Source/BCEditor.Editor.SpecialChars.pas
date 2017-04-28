unit BCEditor.Editor.SpecialChars;

interface {********************************************************************}

uses
  Classes,
  Graphics,
  BCEditor.Types, BCEditor.Consts;

type
  TBCEditorSpecialChars = class(TPersistent)
  strict private
    FColor: TColor;
    FOnChange: TNotifyEvent;
    FVisible: Boolean;
    procedure DoChange;
    procedure SetColor(const AValue: TColor);
    procedure SetVisible(const AValue: Boolean);
  protected
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  public
    constructor Create();
    procedure Assign(ASource: TPersistent); override;
  published
    property Color: TColor read FColor write SetColor default clSpecialChar;
    property Visible: Boolean read FVisible write SetVisible default False;
  end;

implementation {***************************************************************}

{ TBCEditorSpecialChars *******************************************************}

constructor TBCEditorSpecialChars.Create();
begin
  inherited;

  FColor := clSpecialChar;
  FVisible := False;
end;

procedure TBCEditorSpecialChars.Assign(ASource: TPersistent);
begin
  if Assigned(ASource) and (ASource is TBCEditorSpecialChars) then
  with ASource as TBCEditorSpecialChars do
  begin
    Self.FColor := FColor;
    Self.FVisible := FVisible;
    Self.DoChange;
  end
  else
    inherited Assign(ASource);
end;

procedure TBCEditorSpecialChars.DoChange();
begin
  if (Assigned(FOnChange)) then
    FOnChange(Self);
end;

procedure TBCEditorSpecialChars.SetColor(const AValue: TColor);
begin
  if (AValue <> FColor) then
  begin
    FColor := AValue;
    DoChange;
  end;
end;

procedure TBCEditorSpecialChars.SetVisible(const AValue: Boolean);
begin
  if (AValue <> FVisible) then
  begin
    FVisible := AValue;
    DoChange;
  end;
end;

end.

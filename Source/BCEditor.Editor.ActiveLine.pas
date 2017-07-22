unit BCEditor.Editor.ActiveLine;

interface {********************************************************************}

uses
  Classes,
  Graphics,
  BCEditor.Consts;

type
  TBCEditorActiveLine = class(TPersistent)
  strict private
    FColor: TColor;
    FOnChange: TNotifyEvent;
    FVisible: Boolean;
    procedure DoChange();
    procedure SetColor(const AValue: TColor);
    procedure SetVisible(const AValue: Boolean);
  protected
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  public
    constructor Create();
    procedure Assign(ASource: TPersistent); override;
  published
    property Color: TColor read FColor write SetColor default clActiveLineBackground;
    property Visible: Boolean read FVisible write SetVisible default True;
  end;

implementation {***************************************************************}

{ TBCEditorActiveLine.Create **************************************************}

constructor TBCEditorActiveLine.Create();
begin
  inherited;

  FColor := clActiveLineBackground;
  FOnChange := nil;
  FVisible := True;
end;

procedure TBCEditorActiveLine.Assign(ASource: TPersistent);
begin
  if Assigned(ASource) and (ASource is TBCEditorActiveLine) then
  with ASource as TBCEditorActiveLine do
  begin
    Self.FColor := FColor;
    Self.FVisible := FVisible;
    Self.DoChange();
  end
  else
    inherited Assign(ASource);
end;

procedure TBCEditorActiveLine.DoChange();
begin
  if (Assigned(FOnChange)) then
    FOnChange(Self);
end;

procedure TBCEditorActiveLine.SetColor(const AValue: TColor);
begin
  if FColor <> AValue then
  begin
    FColor := AValue;
    DoChange();
  end;
end;

procedure TBCEditorActiveLine.SetVisible(const AValue: Boolean);
begin
  if FVisible <> AValue then
  begin
    FVisible := AValue;
    DoChange();
  end;
end;

end.

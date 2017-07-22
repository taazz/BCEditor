unit BCEditor.Editor.MatchingPair;

interface {********************************************************************}

uses
  Classes,
  Graphics,
  BCEditor.Types, BCEditor.Consts;

type
  TBCEditorMatchingPair = class(TPersistent)
  strict private
    FColor: TColor;
    FEnabled: Boolean;
    FOnChange: TNotifyEvent;
    procedure DoChange();
    procedure SetColor(const AValue: TColor);
    procedure SetEnabled(const AValue: Boolean);
  protected
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  public
    procedure Assign(ASource: TPersistent); override;
    constructor Create();
  published
    property Color: TColor read FColor write SetColor default clMatchingPair;
    property Enabled: Boolean read FEnabled write SetEnabled default True;
  end;

implementation {***************************************************************}

{ TBCEditorMatchingPair *******************************************************}

procedure TBCEditorMatchingPair.Assign(ASource: TPersistent);
begin
  if Assigned(ASource) and (ASource is TBCEditorMatchingPair) then
  with ASource as TBCEditorMatchingPair do
  begin
    Self.FEnabled := FEnabled;
    Self.FColor := FColor;
    Self.DoChange();
  end
  else
    inherited Assign(ASource);
end;

constructor TBCEditorMatchingPair.Create();
begin
  inherited;

  FColor := clMatchingPair;
  FEnabled := True;
  FOnChange := nil;
end;

procedure TBCEditorMatchingPair.DoChange();
begin
  if (Assigned(FOnChange)) then
    FOnChange(Self);
end;

procedure TBCEditorMatchingPair.SetColor(const AValue: TColor);
begin
  if (AValue <> FColor) then
  begin
    FColor := AValue;
    DoChange();
  end;
end;

procedure TBCEditorMatchingPair.SetEnabled(const AValue: Boolean);
begin
  if (AValue <> FEnabled) then
  begin
    FEnabled := AValue;
    DoChange();
  end;
end;

end.

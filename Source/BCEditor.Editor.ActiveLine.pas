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
    procedure DoChange(ASender: TObject);
    procedure SetColor(const AValue: TColor);
    procedure SetVisible(const AValue: Boolean);
  public
    constructor Create();
    procedure Assign(ASource: TPersistent); override;
  published
    property Color: TColor read FColor write SetColor default clActiveLineBackground;
    property Visible: Boolean read FVisible write SetVisible default True;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

implementation {***************************************************************}

{ TBCEditorActiveLine.Create **************************************************}

constructor TBCEditorActiveLine.Create();
begin
  inherited;

  FColor := clActiveLineBackground;
  FVisible := True;
end;

procedure TBCEditorActiveLine.Assign(ASource: TPersistent);
begin
  if Assigned(ASource) and (ASource is TBCEditorActiveLine) then
  with ASource as TBCEditorActiveLine do
  begin
    Self.FColor := FColor;
    Self.FVisible := FVisible;
    Self.DoChange(Self);
  end
  else
    inherited Assign(ASource);
end;

procedure TBCEditorActiveLine.DoChange(ASender: TObject);
begin
  if Assigned(FOnChange) then
    FOnChange(ASender);
end;

procedure TBCEditorActiveLine.SetColor(const AValue: TColor);
begin
  if FColor <> AValue then
  begin
    FColor := AValue;
    DoChange(Self);
  end;
end;

procedure TBCEditorActiveLine.SetVisible(const AValue: Boolean);
begin
  if FVisible <> AValue then
  begin
    FVisible := AValue;
    DoChange(Self);
  end;
end;

end.

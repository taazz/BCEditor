unit BCEditor.Editor.Selection;

interface {********************************************************************}

uses
  Classes,
  Graphics,
  BCEditor.Types, BCEditor.Consts;

type
  TBCEditorSelection = class(TPersistent)
  type
    TColors = class(TPersistent)
    strict private
      FBackground: TColor;
      FForeground: TColor;
      FOnChange: TNotifyEvent;
      procedure SetBackground(AValue: TColor);
      procedure SetForeground(AValue: TColor);
    public
      constructor Create;
      procedure Assign(ASource: TPersistent); override;
    published
      property Background: TColor read FBackground write SetBackground default clSelectionColor;
      property Foreground: TColor read FForeground write SetForeground default clHighLightText;
      property OnChange: TNotifyEvent read FOnChange write FOnChange;
    end;

  strict private const
    DefaultOptions = [soTermsCaseSensitive];
  strict private
    FColors: TColors;
    FOptions: TBCEditorSelectionOptions;
    procedure SetColors(const AValue: TColors);
    procedure SetOptions(AValue: TBCEditorSelectionOptions);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Assign(ASource: TPersistent); override;
    procedure SetOption(const AOption: TBCEditorSelectionOption; const AEnabled: Boolean);
  published
    property Colors: TColors read FColors write SetColors;
    property Options: TBCEditorSelectionOptions read FOptions write SetOptions default DefaultOptions;
  end;

implementation {***************************************************************}

{ TBCEditorSelection.TColors **************************************************}

constructor TBCEditorSelection.TColors.Create;
begin
  inherited;

  FBackground := clSelectionColor;
  FForeground := clHighLightText;
end;

procedure TBCEditorSelection.TColors.Assign(ASource: TPersistent);
begin
  if Assigned(ASource) and (ASource is TBCEditorSelection.TColors) then
  with ASource as TBCEditorSelection.TColors do
  begin
    Self.FBackground := FBackground;
    Self.FForeground := FForeground;
    if Assigned(Self.FOnChange) then
      Self.FOnChange(Self);
  end
  else
    inherited Assign(ASource);
end;

procedure TBCEditorSelection.TColors.SetBackground(AValue: TColor);
begin
  if FBackground <> AValue then
  begin
    FBackground := AValue;
    if Assigned(FOnChange) then
      FOnChange(Self);
  end;
end;

procedure TBCEditorSelection.TColors.SetForeground(AValue: TColor);
begin
  if FForeground <> AValue then
  begin
    FForeground := AValue;
    if Assigned(FOnChange) then
      FOnChange(Self);
  end;
end;

{ TBCEditorSelection **********************************************************}

procedure TBCEditorSelection.Assign(ASource: TPersistent);
begin
  if Assigned(ASource) and (ASource is TBCEditorSelection) then
  with ASource as TBCEditorSelection do
  begin
    Self.FColors.Assign(FColors);
    Self.FOptions := FOptions;
  end
  else
    inherited Assign(ASource);
end;

constructor TBCEditorSelection.Create();
begin
  inherited;

  FColors := TColors.Create();
  FOptions := DefaultOptions;
end;

destructor TBCEditorSelection.Destroy();
begin
  FColors.Free();

  inherited;
end;

procedure TBCEditorSelection.SetColors(const AValue: TBCEditorSelection.TColors);
begin
  FColors.Assign(AValue);
end;

procedure TBCEditorSelection.SetOption(const AOption: TBCEditorSelectionOption; const AEnabled: Boolean);
begin
   if AEnabled then
    Include(FOptions, AOption)
  else
    Exclude(FOptions, AOption);
end;

procedure TBCEditorSelection.SetOptions(AValue: TBCEditorSelectionOptions);
begin
  if FOptions <> AValue then
    FOptions := AValue;
end;

end.

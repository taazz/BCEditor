unit BCEditor.Editor.Caret;

interface {********************************************************************}

uses
  Classes, Types,
  Graphics,
  BCEditor.Types;

type
  TBCEditorCaretChangedEvent = procedure(ASender: TObject; ACaretPos: TPoint) of object;

  TBCEditorCaret = class(TPersistent)
  type
    TOffsets = class(TPersistent)
    strict private
      FLeft: Integer;
      FOnChange: TNotifyEvent;
      FTop: Integer;
      procedure DoChange(ASender: TObject);
      procedure SetLeft(const AValue: Integer);
      procedure SetTop(const AValue: Integer);
    public
      constructor Create;
      procedure Assign(ASource: TPersistent); override;
    published
      property Left: Integer read FLeft write SetLeft default 0;
      property Top: Integer read FTop write SetTop default 0;
      property OnChange: TNotifyEvent read FOnChange write FOnChange;
    end;

    TMultiEdit = class(TPersistent)
    type
      TColors = class(TPersistent)
      strict private
        FBackground: TColor;
        FForeground: TColor;
      public
        constructor Create;
        procedure Assign(ASource: TPersistent); override;
      published
        property Background: TColor read FBackground write FBackground default clBlack;
        property Foreground: TColor read FForeground write FForeground default clWhite;
      end;

    strict private const
      DefaultOptions = [meoShowActiveLine, meoShowGhost];
    strict private
      FColors: TBCEditorCaret.TMultiEdit.TColors;
      FEnabled: Boolean;
      FOnChange: TNotifyEvent;
      FOptions: TBCEditorCaretMultiEditOptions;
      procedure DoChange;
      procedure SetColors(AValue: TBCEditorCaret.TMultiEdit.TColors);
      procedure SetEnabled(AValue: Boolean);
      procedure SetOptions(const AValue: TBCEditorCaretMultiEditOptions);
    public
      constructor Create;
      destructor Destroy; override;
      procedure Assign(ASource: TPersistent); override;
    published
      property Colors: TBCEditorCaret.TMultiEdit.TColors read FColors write SetColors;
      property Enabled: Boolean read FEnabled write SetEnabled default True;
      property Options: TBCEditorCaretMultiEditOptions read FOptions write SetOptions default DefaultOptions;
      property OnChange: TNotifyEvent read FOnChange write FOnChange;
    end;

  strict private const
    DefaultOptions = [];
  strict private
    FMultiEdit: TBCEditorCaret.TMultiEdit;
    FOffsets: TBCEditorCaret.TOffsets;
    FOnChange: TNotifyEvent;
    FOptions: TBCEditorCaretOptions;
    procedure DoChange(ASender: TObject);
    procedure SetMultiEdit(AValue: TBCEditorCaret.TMultiEdit);
    procedure SetOffsets(AValue: TBCEditorCaret.TOffsets);
    procedure SetOnChange(AValue: TNotifyEvent);
    procedure SetOptions(const AValue: TBCEditorCaretOptions);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Assign(ASource: TPersistent); override;
    procedure SetOption(const AOption: TBCEditorCaretOption; const AEnabled: Boolean);
  published
    property MultiEdit: TBCEditorCaret.TMultiEdit read FMultiEdit write SetMultiEdit;
    property Offsets: TBCEditorCaret.TOffsets read FOffsets write SetOffsets;
    property Options: TBCEditorCaretOptions read FOptions write SetOptions default DefaultOptions;
    property OnChange: TNotifyEvent read FOnChange write SetOnChange;
  end;

implementation {***************************************************************}

{ TBCEditorCaret.TOffsets *****************************************************}

constructor TBCEditorCaret.TOffsets.Create;
begin
  inherited;

  FLeft := 0;
  FTop := 0;
end;

procedure TBCEditorCaret.TOffsets.Assign(ASource: TPersistent);
begin
  if Assigned(ASource) and (ASource is TBCEditorCaret.TOffsets) then
  with ASource as TBCEditorCaret.TOffsets do
  begin
    Self.FLeft := FLeft;
    Self.FTop := FTop;
    Self.DoChange(Self);
  end
  else
    inherited Assign(ASource);
end;

procedure TBCEditorCaret.TOffsets.DoChange(ASender: TObject);
begin
  if Assigned(FOnChange) then
    FOnChange(ASender);
end;

procedure TBCEditorCaret.TOffsets.SetLeft(const AValue: Integer);
begin
  if FLeft <> AValue then
  begin
    FLeft := AValue;
    DoChange(Self);
  end;
end;

procedure TBCEditorCaret.TOffsets.SetTop(const AValue: Integer);
begin
  if FTop <> AValue then
  begin
    FTop := AValue;
    DoChange(Self);
  end;
end;

{ TBCEditorCaret.TMultiEdit.TColors *******************************************}

constructor TBCEditorCaret.TMultiEdit.TColors.Create;
begin
  inherited;

  FBackground := clBlack;
  FForeground := clWhite;
end;

procedure TBCEditorCaret.TMultiEdit.TColors.Assign(ASource: TPersistent);
begin
  if Assigned(ASource) and (ASource is TBCEditorCaret.TMultiEdit.TColors) then
  with ASource as TBCEditorCaret.TMultiEdit.TColors do
  begin
    Self.FBackground := FBackground;
    Self.FForeground := FForeground;
  end
  else
    inherited Assign(ASource);
end;

{ TBCEditorCaret.TMultiEdit ***************************************************}

constructor TBCEditorCaret.TMultiEdit.Create;
begin
  inherited;

  FColors := TBCEditorCaret.TMultiEdit.TColors.Create;
  FEnabled := True;
  FOptions := DefaultOptions;
end;

destructor TBCEditorCaret.TMultiEdit.Destroy;
begin
  FColors.Free;

  inherited;
end;

procedure TBCEditorCaret.TMultiEdit.Assign(ASource: TPersistent);
begin
  if Assigned(ASource) and (ASource is TBCEditorCaret.TMultiEdit) then
  with ASource as TBCEditorCaret.TMultiEdit do
  begin
    Self.FColors.Assign(FColors);
    Self.FEnabled := FEnabled;
    Self.FOptions := FOptions;
    Self.DoChange;
  end
  else
    inherited Assign(ASource);
end;

procedure TBCEditorCaret.TMultiEdit.DoChange;
begin
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

procedure TBCEditorCaret.TMultiEdit.SetColors(AValue: TBCEditorCaret.TMultiEdit.TColors);
begin
  FColors.Assign(AValue);
end;

procedure TBCEditorCaret.TMultiEdit.SetEnabled(AValue: Boolean);
begin
  if FEnabled <> AValue then
  begin
    FEnabled := AValue;
    DoChange;
  end;
end;

procedure TBCEditorCaret.TMultiEdit.SetOptions(const AValue: TBCEditorCaretMultiEditOptions);
begin
  if FOptions <> AValue then
  begin
    FOptions := AValue;
    DoChange;
  end;
end;

{ TBCEditorCaret **************************************************************}

constructor TBCEditorCaret.Create;
begin
  inherited;

  FMultiEdit := TBCEditorCaret.TMultiEdit.Create;
  FOffsets := TBCEditorCaret.TOffsets.Create;
  FOptions := DefaultOptions;
end;

destructor TBCEditorCaret.Destroy;
begin
  FMultiEdit.Free;
  FOffsets.Free;

  inherited;
end;

procedure TBCEditorCaret.Assign(ASource: TPersistent);
begin
  if Assigned(ASource) and (ASource is TBCEditorCaret) then
  with ASource as TBCEditorCaret do
  begin
    Self.FMultiEdit.Assign(FMultiEdit);
    Self.FOffsets.Assign(FOffsets);
    Self.FOptions := FOptions;
    Self.DoChange(Self);
  end
  else
    inherited Assign(ASource);
end;

procedure TBCEditorCaret.DoChange(ASender: TObject);
begin
  if Assigned(FOnChange) then
    FOnChange(ASender);
end;

procedure TBCEditorCaret.SetMultiEdit(AValue: TBCEditorCaret.TMultiEdit);
begin
  FMultiEdit.Assign(AValue);
end;

procedure TBCEditorCaret.SetOffsets(AValue: TBCEditorCaret.TOffsets);
begin
  FOffsets.Assign(AValue);
end;

procedure TBCEditorCaret.SetOnChange(AValue: TNotifyEvent);
begin
  FOnChange := AValue;
  FOffsets.OnChange := AValue;
  FMultiEdit.OnChange := AValue;
end;

procedure TBCEditorCaret.SetOption(const AOption: TBCEditorCaretOption; const AEnabled: Boolean);
begin
  if AEnabled then
    Include(FOptions, AOption)
  else
    Exclude(FOptions, AOption);
end;

procedure TBCEditorCaret.SetOptions(const AValue: TBCEditorCaretOptions);
begin
  if FOptions <> AValue then
  begin
    FOptions := AValue;
    DoChange(Self);
  end;
end;

end.

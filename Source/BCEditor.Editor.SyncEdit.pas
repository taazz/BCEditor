unit BCEditor.Editor.SyncEdit;

interface {********************************************************************}

uses
  Classes,
  Graphics,
  BCEditor.Editor.Glyph, BCEditor.Types, BCEditor.Consts;

type
  TBCEditorSyncEdit = class(TPersistent)
  type

    TColors = class(TPersistent)
    strict private
      FBackground: TColor;
      FEditBorder: TColor;
      FWordBorder: TColor;
    public
      constructor Create;
      procedure Assign(ASource: TPersistent); override;
    published
      property Background: TColor read FBackground write FBackground default clSyncEditBackground;
      property EditBorder: TColor read FEditBorder write FEditBorder default clWindowText;
      property WordBorder: TColor read FWordBorder write FWordBorder default clHighlight;
    end;

  strict private const
    DefaultShortCut = 24650; // (Shift+Ctrl+J)
  strict private
    FActivator: TBCEditorGlyph;
    FActive: Boolean;
    FBlockArea: TBCEditorTextArea;
    FBlockSelected: Boolean;
    FColors: TBCEditorSyncEdit.TColors;
    FEditArea: TBCEditorTextArea;
    FEditWidth: Integer;
    FEnabled: Boolean;
    FInEditor: Boolean;
    FOnChange: TNotifyEvent;
    FOptions: TBCEditorSyncEditOptions;
    FShortCut: TShortCut;
    FSyncItems: TList;
    procedure DoChange(ASender: TObject);
    procedure SetActivator(const AValue: TBCEditorGlyph);
    procedure SetActive(AValue: Boolean);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Abort;
    procedure Assign(ASource: TPersistent); override;
    procedure ClearSyncItems;
    procedure MoveBeginPositionChar(ACount: Integer);
    procedure MoveEndPositionChar(ACount: Integer);
    procedure SetOption(const AOption: TBCEditorSyncEditOption; const AEnabled: Boolean);
    property Active: Boolean read FActive write SetActive default False;
    property BlockArea: TBCEditorTextArea read FBlockArea write FBlockArea;
    property BlockSelected: Boolean read FBlockSelected write FBlockSelected default False;
    property EditArea: TBCEditorTextArea read FEditArea write FEditArea;
    property EditWidth: Integer read FEditWidth write FEditWidth;
    property InEditor: Boolean read FInEditor write FInEditor default False;
    property SyncItems: TList read FSyncItems write FSyncItems;
  published
    property Activator: TBCEditorGlyph read FActivator write SetActivator;
    property Colors: TColors read FColors write FColors;
    property Enabled: Boolean read FEnabled write FEnabled default True;
    property Options: TBCEditorSyncEditOptions read FOptions write FOptions default [seCaseSensitive];
    property ShortCut: TShortCut read FShortCut write FShortCut default DefaultShortCut;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

implementation {***************************************************************}

uses
  Menus;

{ TBCEditorSyncEdit.TColors ***************************************************}

constructor TBCEditorSyncEdit.TColors.Create;
begin
  inherited;

  FBackground := clSyncEditBackground;
  FEditBorder := clWindowText;
  FWordBorder := clHighlight;
end;

procedure TBCEditorSyncEdit.TColors.Assign(ASource: TPersistent);
begin
  if Assigned(ASource) and (ASource is TBCEditorSyncEdit.TColors) then
  with ASource as TBCEditorSyncEdit.TColors do
  begin
    Self.FBackground := FBackground;
    Self.FEditBorder := FEditBorder;
    Self.FWordBorder := FWordBorder;
  end
  else
    inherited Assign(ASource);
end;

{ TBCEditorSyncEdit ***********************************************************}

constructor TBCEditorSyncEdit.Create;
begin
  inherited Create;

  FActive := False;
  FBlockSelected := False;
  FEnabled := True;
  FInEditor := False;
  FShortCut := DefaultShortCut;
  FOptions := [seCaseSensitive];
  FSyncItems := TList.Create;
  FColors := TBCEditorSyncEdit.TColors.Create;
  FActivator := TBCEditorGlyph.Create(HInstance, BCEDITOR_SYNCEDIT, clFuchsia);
end;

destructor TBCEditorSyncEdit.Destroy;
begin
  ClearSyncItems;
  FSyncItems.Free;
  FColors.Free;
  FActivator.Free;
  inherited;
end;

procedure TBCEditorSyncEdit.Abort;
begin
  FActive := False;
end;

procedure TBCEditorSyncEdit.Assign(ASource: TPersistent);
begin
  if Assigned(ASource) and (ASource is TBCEditorSyncEdit) then
  with ASource as TBCEditorSyncEdit do
  begin
    Self.Enabled := FEnabled;
    Self.FShortCut := FShortCut;
    Self.FActivator.Assign(FActivator);
    Self.DoChange(Self);
  end
  else
    inherited Assign(ASource);
end;

procedure TBCEditorSyncEdit.ClearSyncItems;
var
  LIndex: Integer;
begin
  for LIndex := FSyncItems.Count - 1 downto 0 do
    Dispose(PBCEditorTextPosition(FSyncItems.Items[LIndex]));
  FSyncItems.Clear;
end;

procedure TBCEditorSyncEdit.DoChange(ASender: TObject);
begin
  if Assigned(FOnChange) then
    FOnChange(ASender);
end;

procedure TBCEditorSyncEdit.MoveBeginPositionChar(ACount: Integer);
begin
  Inc(FEditArea.BeginPosition.Char, ACount);
end;

procedure TBCEditorSyncEdit.MoveEndPositionChar(ACount: Integer);
begin
  Inc(FEditArea.EndPosition.Char, ACount);
end;

procedure TBCEditorSyncEdit.SetActivator(const AValue: TBCEditorGlyph);
begin
  FActivator.Assign(AValue);
end;

procedure TBCEditorSyncEdit.SetActive(AValue: Boolean);
begin
  FActive := AValue;
  DoChange(Self);
end;

procedure TBCEditorSyncEdit.SetOption(const AOption: TBCEditorSyncEditOption; const AEnabled: Boolean);
begin
  if AEnabled then
    Include(FOptions, AOption)
  else
    Exclude(FOptions, AOption);
end;

end.

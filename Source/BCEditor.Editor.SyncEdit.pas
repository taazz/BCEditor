unit BCEditor.Editor.SyncEdit;

interface {********************************************************************}

uses
  Classes,
  Graphics,
  BCEditor.Types, BCEditor.Consts;

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
    DefaultOptions = [seoButton, seoCaseSensitive];
    DefaultShortCut = 24650; // (Shift+Ctrl+J)
  strict private
    FColors: TBCEditorSyncEdit.TColors;
    FEnabled: Boolean;
    FOnChange: TNotifyEvent;
    FOptions: TBCEditorSyncEditOptions;
    FShortCut: TShortCut;
    procedure DoChange(ASender: TObject);
  protected
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  public
    procedure Assign(ASource: TPersistent); override;
    constructor Create();
    destructor Destroy(); override;
    procedure SetOption(const AOption: TBCEditorSyncEditOption; const AEnabled: Boolean);
  published
    property Colors: TColors read FColors write FColors;
    property Enabled: Boolean read FEnabled write FEnabled default True;
    property Options: TBCEditorSyncEditOptions read FOptions write FOptions default DefaultOptions;
    property ShortCut: TShortCut read FShortCut write FShortCut default DefaultShortCut;
  end;

implementation {***************************************************************}

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

procedure TBCEditorSyncEdit.Assign(ASource: TPersistent);
begin
  if Assigned(ASource) and (ASource is TBCEditorSyncEdit) then
  with ASource as TBCEditorSyncEdit do
  begin
    Self.Enabled := FEnabled;
    Self.FShortCut := FShortCut;
    Self.DoChange(Self);
  end
  else
    inherited Assign(ASource);
end;

constructor TBCEditorSyncEdit.Create();
begin
  inherited Create();

  FColors := TBCEditorSyncEdit.TColors.Create();
  FEnabled := True;
  FOptions := DefaultOptions;
  FShortCut := DefaultShortCut;
end;

destructor TBCEditorSyncEdit.Destroy();
begin
  FColors.Free();

  inherited;
end;

procedure TBCEditorSyncEdit.DoChange(ASender: TObject);
begin
  if Assigned(FOnChange) then
    FOnChange(ASender);
end;

procedure TBCEditorSyncEdit.SetOption(const AOption: TBCEditorSyncEditOption; const AEnabled: Boolean);
begin
  if (AEnabled) then
    Include(FOptions, AOption)
  else
    Exclude(FOptions, AOption);
end;

end.

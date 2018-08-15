unit BCEditor.Properties;

interface {********************************************************************}

uses
  Classes, UITypes, Generics.Collections, RegularExpressions,
  Graphics, Controls, ImgList,
  BCEditor.Consts, BCEditor.Types;

type
  TBCEditorCompletionProposalCloseEvent = procedure(ASender: TObject; var ASelectedItem: string) of object;
  TBCEditorCompletionProposalValidateEvent = procedure(ASender: TObject; AShift: TShiftState; AEndToken: Char) of object;

  TBCEditorCompletionProposalItem = class(TPersistent)
  strict private
    FImageIndex: Integer;
    FValue: string;
  public
    procedure Assign(ASource: TPersistent); override;
    constructor Create();
  published
    property ImageIndex: Integer read FImageIndex write FImageIndex default -1;
    property Value: string read FValue write FValue;
  end;

  TBCEditorCompletionProposalItems = class(TObjectList<TBCEditorCompletionProposalItem>)
  public
    function Add(): TBCEditorCompletionProposalItem; reintroduce;
    procedure Assign(ASource: TBCEditorCompletionProposalItems);
    function Insert(const AIndex: Integer): TBCEditorCompletionProposalItem; reintroduce;
  end;

  TBCEditorCompletionProposalColumn = class(TPersistent)
  strict private
    FAutoWidth: Boolean;
    FFont: TFont;
    FItems: TBCEditorCompletionProposalItems;
    FVisible: Boolean;
    FWidth: Integer;
    procedure SetFont(const AValue: TFont);
  public
    procedure Assign(ASource: TPersistent); override;
    constructor Create();
    destructor Destroy(); override;
    property Items: TBCEditorCompletionProposalItems read FItems write FItems;
  published
    property AutoWidth: Boolean read FAutoWidth write FAutoWidth default True;
    property Font: TFont read FFont write SetFont;
    property Visible: Boolean read FVisible write FVisible default True;
    property Width: Integer read FWidth write FWidth default 0;
  end;

  TBCEditorCompletionProposalColumns = class(TObjectList<TBCEditorCompletionProposalColumn>)
  protected
    function IsStored(): Boolean;
  public
    function Add(): TBCEditorCompletionProposalColumn; reintroduce;
    procedure Assign(const ASource: TBCEditorCompletionProposalColumns);
    function Insert(const AIndex: Integer): TBCEditorCompletionProposalColumn; reintroduce;
  end;

  TBCEditorCompletionProposalShowEvent = procedure(Sender: TObject;
    const AColumns: TBCEditorCompletionProposalColumns;
    const AInput: string; var ACanExecute: Boolean) of object;

  TBCEditorCompletionProposal = class(TPersistent)
  type
    TTrigger = class(TPersistent)
    strict private const
      DefaultChars = '.';
      DefaultEnabled = False;
      DefaultInterval = 1000;
    strict private
      FChars: string;
      FCompletionProposal: TBCEditorCompletionProposal;
      FEnabled: Boolean;
      FInterval: Integer;
    protected
      function IsStored(): Boolean;
    public
      procedure Assign(ASource: TPersistent); override;
      constructor Create(const ACompletionProposal: TBCEditorCompletionProposal);
    published
      property Chars: string read FChars write FChars;
      property Enabled: Boolean read FEnabled write FEnabled default False;
      property Interval: Integer read FInterval write FInterval default 1000;
    end;

  strict private const
    DefaultCloseChars = '()[]. ';
    DefaultEnabled = True;
    DefaultCompletionColumnIndex = 0;
    DefaultOptions = [cpoAutoConstraints, cpoAddHighlighterKeywords, cpoFiltered,
      cpoParseItemsFromText, cpoUseHighlighterColumnFont];
    DefaultVisibleLines = 8;
    DefaultWidth = 260;
  strict private
    FCloseChars: string;
    FColumns: TBCEditorCompletionProposalColumns;
    FCompletionColumnIndex: Integer;
    FConstraints: TSizeConstraints;
    FEnabled: Boolean;
    FImages: TCustomImageList;
    FOptions: TBCEditorCompletionProposalOptions;
    FOwner: TComponent;
    FTrigger: TTrigger;
    FVisibleLines: Integer;
    FWidth: Integer;
    procedure SetImages(const AValue: TCustomImageList);
  protected
    function GetOwner(): TPersistent; override;
    function IsCloseCharsStored(): Boolean;
    function IsColumnsStored(): Boolean;
    function IsStored(): Boolean;
    function IsTriggersStored(): Boolean;
  public
    procedure Assign(ASource: TPersistent); override;
    procedure ChangeScale(M, D: Integer);
    constructor Create(AOwner: TComponent);
    destructor Destroy(); override;
  published
    property CloseChars: string read FCloseChars write FCloseChars stored IsCloseCharsStored;
    property Columns: TBCEditorCompletionProposalColumns read FColumns write FColumns stored IsColumnsStored;
    property CompletionColumnIndex: Integer read FCompletionColumnIndex write FCompletionColumnIndex default DefaultCompletionColumnIndex;
    property Constraints: TSizeConstraints read FConstraints write FConstraints;
    property Enabled: Boolean read FEnabled write FEnabled default DefaultEnabled;
    property Images: TCustomImageList read FImages write SetImages;
    property Options: TBCEditorCompletionProposalOptions read FOptions write FOptions default DefaultOptions;
    property Trigger: TBCEditorCompletionProposal.TTrigger read FTrigger write FTrigger stored IsTriggersStored;
    property VisibleLines: Integer read FVisibleLines write FVisibleLines default DefaultVisibleLines;
    property Width: Integer read FWidth write FWidth default DefaultWidth;
  end;

  TBCEditorColors = class(TPersistent)
  type
    TActiveLine = class(TPersistent)
    private
      FBackground: TColor;
      FColors: TBCEditorColors;
      procedure SetBackground(AValue: TColor);
    protected
      function IsStored(): Boolean;
    public
      procedure Assign(ASource: TPersistent); override;
      constructor Create(const AColors: TBCEditorColors);
    published
      property Background: TColor read FBackground write SetBackground default clActiveLineBackground;
    end;

    TBookmark = class(TPersistent)
    private
      FBorder: TColor;
      FColors: TBCEditorColors;
      FCover: TColor;
      FNumber: TColor;
      FRingLeft: TColor;
      FRingMiddle: TColor;
      FRingRight: TColor;
      procedure SetBorder(AValue: TColor);
      procedure SetCover(AValue: TColor);
      procedure SetNumber(AValue: TColor);
      procedure SetRingLeft(AValue: TColor);
      procedure SetRingMiddle(AValue: TColor);
      procedure SetRingRight(AValue: TColor);
    protected
      function IsStored(): Boolean;
    public
      procedure Assign(ASource: TPersistent); override;
      constructor Create(const AColors: TBCEditorColors);
    published
      property Border: TColor read FBorder write SetBorder default clBookmarkBorder;
      property Cover: TColor read FCover write SetCover default clBookmarkCover;
      property Number: TColor read FNumber write SetNumber default clBookmarkNumber;
      property RingLeft: TColor read FRingLeft write SetRingLeft default clBookmarkRingLeft;
      property RingMiddle: TColor read FRingMiddle write SetRingMiddle default clBookmarkRingMiddle;
      property RingRight: TColor read FRingRight write SetRingRight default clBookmarkRingRight;
    end;

    TCodeFolding = class(TPersistent)
    private
      FBackground: TColor;
      FColors: TBCEditorColors;
      FForeground: TColor;
      procedure SetBackground(AValue: TColor);
      procedure SetForeground(AValue: TColor);
    protected
      function IsStored(): Boolean;
    public
      procedure Assign(ASource: TPersistent); override;
      constructor Create(const AColors: TBCEditorColors);
    published
      property Background: TColor read FBackground write SetBackground default clCodeFoldingBackground;
      property Foreground: TColor read FForeground write SetForeground default clCodeFoldingForeground;
    end;

    TFoundText = class(TPersistent)
    private
      FBackground: TColor;
      FColors: TBCEditorColors;
      FForeground: TColor;
      procedure SetBackground(AValue: TColor);
      procedure SetForeground(AValue: TColor);
    protected
      function IsStored(): Boolean;
    public
      procedure Assign(ASource: TPersistent); override;
      constructor Create(const AColors: TBCEditorColors);
    published
      property Background: TColor read FBackground write SetBackground default clFoundTextBackground;
      property Foreground: TColor read FForeground write SetForeground default clFoundTextForeground;
    end;

    TLineNumbers = class(TPersistent)
    private
      FBackground: TColor;
      FColors: TBCEditorColors;
      FForeground: TColor;
      procedure SetBackground(AValue: TColor);
      procedure SetForeground(AValue: TColor);
    protected
      function IsStored(): Boolean;
    public
      procedure Assign(ASource: TPersistent); override;
      constructor Create(const AColors: TBCEditorColors);
    published
      property Background: TColor read FBackground write SetBackground default clLineNumbersBackground;
      property Foreground: TColor read FForeground write SetForeground default clLineNumbersForeground;
    end;

    TLineState = class(TPersistent)
    private
      FColors: TBCEditorColors;
      FModified: TColor;
      FLoaded: TColor;
      FSaved: TColor;
      procedure SetModified(AValue: TColor);
      procedure SetLoaded(AValue: TColor);
      procedure SetSaved(AValue: TColor);
    protected
      function IsStored(): Boolean;
    public
      procedure Assign(ASource: TPersistent); override;
      constructor Create(const AColors: TBCEditorColors);
    published
      property Modified: TColor read FModified write SetModified default clLineStateModified;
      property Loaded: TColor read FLoaded write SetLoaded default clLineStateLoaded;
      property Saved: TColor read FSaved write SetSaved default clLineStateSaved;
    end;

    TMarks = class(TPersistent)
    private
      FBackground: TColor;
      FColors: TBCEditorColors;
      procedure SetBackground(AValue: TColor);
    protected
      function IsStored(): Boolean;
    public
      procedure Assign(ASource: TPersistent); override;
      constructor Create(const AColors: TBCEditorColors);
    published
      property Background: TColor read FBackground write SetBackground default clMarksBackground;
    end;

    TMatchingPairs = class(TPersistent)
    private
      FBackground: TColor;
      FColors: TBCEditorColors;
      procedure SetBackground(AValue: TColor);
    protected
      function IsStored(): Boolean;
    public
      procedure Assign(ASource: TPersistent); override;
      constructor Create(const AColors: TBCEditorColors);
    published
      property Background: TColor read FBackground write SetBackground default clMatchingPairBackground;
    end;

    TSelection = class(TPersistent)
    private
      FBackground: TColor;
      FColors: TBCEditorColors;
      FForeground: TColor;
      procedure SetBackground(AValue: TColor);
      procedure SetForeground(AValue: TColor);
    protected
      function IsStored(): Boolean;
    public
      procedure Assign(ASource: TPersistent); override;
      constructor Create(const AColors: TBCEditorColors);
    published
      property Background: TColor read FBackground write SetBackground default clSelectionBackground;
      property Foreground: TColor read FForeground write SetForeground default clSelectionForeground;
    end;

    TSpecialChars = class(TPersistent)
    private
      FForeground: TColor;
      FColors: TBCEditorColors;
      procedure SetForeground(AValue: TColor);
    protected
      function IsStored(): Boolean;
    public
      procedure Assign(ASource: TPersistent); override;
      constructor Create(const AColors: TBCEditorColors);
    published
      property Foreground: TColor read FForeground write SetForeground default clSpecialCharsForeground;
    end;

    TSyncEdit = class(TPersistent)
    private
      FBackground: TColor;
      FColors: TBCEditorColors;
      FOverlay: TColor;
      procedure SetBackground(AValue: TColor);
      procedure SetOverlays(AValue: TColor);
    protected
      function IsStored(): Boolean;
    public
      procedure Assign(ASource: TPersistent); override;
      constructor Create(const AColors: TBCEditorColors);
    published
      property Background: TColor read FBackground write SetBackground default clSyncEditBackground;
      property Overlay: TColor read FOverlay write SetOverlays default clSyncEditOverlay;
    end;

    TSyncEditButton = class(TPersistent)
    private
      FColors: TBCEditorColors;
      FBackground: TColor;
      FPen: TColor;
      FText: TColor;
      procedure SetBackground(AValue: TColor);
      procedure SetPen(AValue: TColor);
      procedure SetText(AValue: TColor);
    protected
      function IsStored(): Boolean;
    public
      procedure Assign(ASource: TPersistent); override;
      constructor Create(const AColors: TBCEditorColors);
    published
      property Background: TColor read FBackground write SetBackground default clSyncEditButtonBackground;
      property Pen: TColor read FPen write SetPen default clSyncEditButtonPen;
      property Text: TColor read FText write SetText default clSyncEditButtonText;
    end;
  private
    FBookmark: TBookmark;
    FCodeFolding: TCodeFolding;
    FActiveLine: TActiveLine;
    FFoundText: TFoundText;
    FLineNumbers: TLineNumbers;
    FLineState: TLineState;
    FMarks: TMarks;
    FMatchingPairs: TMatchingPairs;
    FOnChange: TNotifyEvent;
    FSelection: TSelection;
    FSpecialChars: TSpecialChars;
    FSyncEdit: TSyncEdit;
    FSyncEditButton: TSyncEditButton;
    procedure DoChange();
    procedure SetActiveLine(AValue: TActiveLine);
    procedure SetBookmark(AValue: TBookmark);
    procedure SetCodeFolding(AValue: TCodeFolding);
    procedure SetFoundText(AValue: TFoundText);
    procedure SetLineNumbers(AValue: TLineNumbers);
    procedure SetLineState(AValue: TLineState);
    procedure SetMarks(AValue: TMarks);
    procedure SetMatchingPairs(AValue: TMatchingPairs);
    procedure SetSelection(AValue: TSelection);
    procedure SetSpecialChars(AValue: TSpecialChars);
    procedure SetSyncEdit(AValue: TSyncEdit);
    procedure SetSyncEditButton(AValue: TSyncEditButton);
  protected
    function IsStored(): Boolean;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  public
    procedure Assign(ASource: TPersistent); override;
    constructor Create();
    destructor Destroy(); override;
  published
    property ActiveLine: TActiveLine read FActiveLine write SetActiveLine;
    property Bookmark: TBookmark read FBookmark write SetBookmark;
    property CodeFolding: TCodeFolding read FCodeFolding write SetCodeFolding;
    property FoundText: TFoundText read FFoundText write SetFoundText;
    property LineNumbers: TLineNumbers read FLineNumbers write SetLineNumbers;
    property LineState: TLineState read FLineState write SetLineState;
    property Marks: TMarks read FMarks write SetMarks;
    property MatchingPairs: TMatchingPairs read FMatchingPairs write SetMatchingPairs;
    property Selection: TSelection read FSelection write SetSelection;
    property SpecialChars: TSpecialChars read FSpecialChars write SetSpecialChars;
    property SyncEdit: TSyncEdit read FSyncEdit write SetSyncEdit;
    property SyncEditButton: TSyncEditButton read FSyncEditButton write SetSyncEditButton;
  end;

  TBCEditorLeftMargin = class(TPersistent)
  type
    TBookMarks = class(TPersistent)
    strict private const
      DefaultVisible = True;
    strict private
      FLeftMargin: TBCEditorLeftMargin;
      FVisible: Boolean;
      procedure SetVisible(AValue: Boolean);
    protected
      function IsStored(): Boolean;
    public
      procedure Assign(ASource: TPersistent); override;
      constructor Create(const ALeftMargin: TBCEditorLeftMargin);
    published
      property Visible: Boolean read FVisible write SetVisible default DefaultVisible;
    end;

    TCodeFolding = class(TPersistent)
    type
    strict private const
      DefaultDelayInterval = 300 {ms};
      DefaultOptions = [cfoHighlightIndentGuides, cfoShowTreeLine];
      DefaultVisible = False;
    strict private
      FDelayInterval: Cardinal;
      FLeftMargin: TBCEditorLeftMargin;
      FMouseOverHint: Boolean;
      FOptions: TBCEditorCodeFoldingOptions;
      FVisible: Boolean;
      procedure SetOptions(AValue: TBCEditorCodeFoldingOptions);
      procedure SetVisible(const AValue: Boolean);
    protected
      function IsStored(): Boolean;
    public
      procedure Assign(ASource: TPersistent); override;
      constructor Create(const ALeftMargin: TBCEditorLeftMargin);
    published
      property DelayInterval: Cardinal read FDelayInterval write FDelayInterval default DefaultDelayInterval;
      property Options: TBCEditorCodeFoldingOptions read FOptions write SetOptions default DefaultOptions;
      property Visible: Boolean read FVisible write SetVisible default DefaultVisible;
    end;

    TLineNumbers = class(TPersistent)
    strict private const
      DefaultOffset = 1;
      DefaultOptions = [lnoIntens];
      DefaultVisible = True;
    strict private
      FLeftMargin: TBCEditorLeftMargin;
      FOffset: Integer;
      FOptions: TBCEditorLeftMarginLineNumberOptions;
      FVisible: Boolean;
      procedure SetOptions(const AValue: TBCEditorLeftMarginLineNumberOptions);
      procedure SetOffset(const AValue: Integer);
      procedure SetVisible(const AValue: Boolean);
    protected
      function IsStored(): Boolean;
    public
      procedure Assign(ASource: TPersistent); override;
      constructor Create(const ALeftMargin: TBCEditorLeftMargin);
    published
      property Offset: Integer read FOffset write SetOffset default DefaultOffset;
      property Options: TBCEditorLeftMarginLineNumberOptions read FOptions write SetOptions default DefaultOptions;
      property Visible: Boolean read FVisible write SetVisible default DefaultVisible;
    end;

    TLineState = class(TPersistent)
    strict private const
      DefaultVisible = True;
    strict private
      FLeftMargin: TBCEditorLeftMargin;
      FVisible: Boolean;
      procedure SetVisible(const AValue: Boolean);
    protected
      function IsStored(): Boolean;
    public
      procedure Assign(ASource: TPersistent); override;
      constructor Create(const ALeftMargin: TBCEditorLeftMargin);
    published
      property Visible: Boolean read FVisible write SetVisible default DefaultVisible;
    end;

    TMarks = class(TPersistent)
    strict private const
      DefaultDefaultImageIndex = -1;
      DefaultVisible = False;
    strict private
      FDefaultImageIndex: Integer;
      FImages: TCustomImageList;
      FLeftMargin: TBCEditorLeftMargin;
      FVisible: Boolean;
      procedure SetImages(const AValue: TCustomImageList);
      procedure SetVisible(AValue: Boolean);
    protected
      function IsStored(): Boolean;
    public
      procedure Assign(ASource: TPersistent); override;
      constructor Create(const ALeftMargin: TBCEditorLeftMargin);
    published
      property DefaultImageIndex: Integer read FDefaultImageIndex write FDefaultImageIndex default -1;
      property Images: TCustomImageList read FImages write SetImages;
      property Visible: Boolean read FVisible write SetVisible default False;
    end;

  strict private
    FBookMarks: TBookmarks;
    FCodeFolding: TCodeFolding;
    FLineNumbers: TLineNumbers;
    FLineState: TLineState;
    FMarks: TMarks;
    FOnChange: TNotifyEvent;
    procedure DoChange();
    procedure SetBookMarks(AValue: TBookmarks);
    procedure SetCodeFolding(AValue: TCodeFolding);
    procedure SetLineNumbers(AValue: TLineNumbers);
    procedure SetLineState(AValue: TLineState);
    procedure SetMarks(AValue: TMarks);
  protected
    function IsStored(): Boolean;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  public
    procedure Assign(ASource: TPersistent); override;
    constructor Create(AOwner: TComponent);
    destructor Destroy(); override;
  published
    property Bookmarks: TBookmarks read FBookMarks write SetBookMarks;
    property CodeFolding: TCodeFolding read FCodeFolding write SetCodeFolding;
    property LineNumbers: TLineNumbers read FLineNumbers write SetLineNumbers;
    property LineState: TLineState read FLineState write SetLineState;
    property Marks: TMarks read FMarks write SetMarks;
  end;

  TBCEditorMinimap = class(TPersistent)
  strict private const
    DefaultFontSize = 1;
    DefaultVisible = False;
    DefaultWidth = 140;
  strict private
    FFontSize: Integer;
    FOnChange: TNotifyEvent;
    FVisible: Boolean;
    FWidth: Integer;
    procedure DoChange();
    procedure SetFontSize(const AValue: Integer);
    procedure SetVisible(const AValue: Boolean);
    procedure SetWidth(const AValue: Integer);
  protected
    procedure ChangeScale(M, D: Integer);
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  public
    procedure Assign(ASource: TPersistent); override;
    constructor Create();
  published
    property FontSize: Integer read FFontSize write SetFontSize default DefaultFontSize;
    property Visible: Boolean read FVisible write SetVisible default DefaultVisible;
    property Width: Integer read FWidth write SetWidth default DefaultWidth;
  end;

implementation  {**************************************************************}

uses
  Windows,
  Math, SysUtils, Character,
  Forms, Menus;

{ TBCEditorCompletionProposalItems.TItem **************************************}

procedure TBCEditorCompletionProposalItem.Assign(ASource: TPersistent);
begin
  Assert(ASource is TBCEditorCompletionProposalItem);

  inherited;

  FImageIndex := TBCEditorCompletionProposalItem(ASource).FImageIndex;
  FValue := TBCEditorCompletionProposalItem(ASource).FValue;
end;

constructor TBCEditorCompletionProposalItem.Create();
begin
  inherited;

  FImageIndex := -1;
end;

{ TBCEditorCompletionProposal.TItems ******************************************}

function TBCEditorCompletionProposalItems.Add(): TBCEditorCompletionProposalItem;
begin
  Result := TBCEditorCompletionProposalItem.Create();
  inherited Add(Result);
end;

procedure TBCEditorCompletionProposalItems.Assign(ASource: TBCEditorCompletionProposalItems);
var
  LIndex: Integer;
begin
  Clear();
  for LIndex := 0 to ASource.Count do
    Add().Assign(ASource[LIndex]);
end;

function TBCEditorCompletionProposalItems.Insert(const AIndex: Integer): TBCEditorCompletionProposalItem;
begin
  Result := TBCEditorCompletionProposalItem.Create();
  inherited Insert(AIndex, Result);
end;

{ TBCEditorCompletionProposalColumn ********************************}

procedure TBCEditorCompletionProposalColumn.Assign(ASource: TPersistent);
begin
  Assert(ASource is TBCEditorCompletionProposalColumn);

  inherited;

  FAutoWidth := TBCEditorCompletionProposalColumn(ASource).FAutoWidth;
  FFont.Assign(TBCEditorCompletionProposalColumn(ASource).FFont);
  FItems.Assign(TBCEditorCompletionProposalColumn(ASource).FItems);
  FWidth := TBCEditorCompletionProposalColumn(ASource).FWidth;
end;

constructor TBCEditorCompletionProposalColumn.Create();
begin
  inherited;

  FAutoWidth := True;
  FFont := TFont.Create;
  FFont.Name := 'Courier New';
  FFont.Size := 8;
  FItems := TBCEditorCompletionProposalItems.Create();
  FVisible := True;
  FWidth := 0;
end;

destructor TBCEditorCompletionProposalColumn.Destroy();
begin
  FFont.Free();
  FItems.Free();

  inherited;
end;

procedure TBCEditorCompletionProposalColumn.SetFont(const AValue: TFont);
begin
  FFont.Assign(AValue);
end;

{ TBCEditorCompletionProposalColumns ****************************************}

function TBCEditorCompletionProposalColumns.Add(): TBCEditorCompletionProposalColumn;
begin
  Result := TBCEditorCompletionProposalColumn.Create();
  inherited Add(Result);
end;

procedure TBCEditorCompletionProposalColumns.Assign(const ASource: TBCEditorCompletionProposalColumns);
var
  LIndex: Integer;
begin
  Clear();
  for LIndex := 0 to ASource.Count - 1 do
    Add().Assign(ASource[LIndex]);
end;

function TBCEditorCompletionProposalColumns.Insert(const AIndex: Integer): TBCEditorCompletionProposalColumn;
begin
  Result := TBCEditorCompletionProposalColumn.Create();
  inherited Insert(AIndex, Result);
end;

function TBCEditorCompletionProposalColumns.IsStored(): Boolean;
begin
  Result := Count <> 1;
end;

{ TBCEditorCompletionProposal.TTrigger ****************************************}

procedure TBCEditorCompletionProposal.TTrigger.Assign(ASource: TPersistent);
begin
  Assert(ASource is TBCEditorCompletionProposal.TTrigger);

  inherited;

  FChars := TBCEditorCompletionProposal.TTrigger(ASource).FChars;
  FEnabled := TBCEditorCompletionProposal.TTrigger(ASource).FEnabled;
  FInterval := TBCEditorCompletionProposal.TTrigger(ASource).FInterval;
end;

constructor TBCEditorCompletionProposal.TTrigger.Create(const ACompletionProposal: TBCEditorCompletionProposal);
begin
  inherited Create();

  FCompletionProposal := ACompletionProposal;

  FChars := DefaultChars;
  FEnabled := DefaultEnabled;
  FInterval := DefaultInterval;
end;

function TBCEditorCompletionProposal.TTrigger.IsStored(): Boolean;
begin
  Result := (FChars <> DefaultChars)
    or (FEnabled <> DefaultEnabled)
    or (FInterval <> DefaultInterval);
end;

{ TBCEditorCompletionProposal *************************************************}

procedure TBCEditorCompletionProposal.Assign(ASource: TPersistent);
begin
  Assert(ASource is TBCEditorCompletionProposal);

  inherited;

  FCloseChars := TBCEditorCompletionProposal(ASource).FCloseChars;
  FColumns.Assign(TBCEditorCompletionProposal(ASource).FColumns);
  FEnabled := TBCEditorCompletionProposal(ASource).FEnabled;
  FImages := TBCEditorCompletionProposal(ASource).FImages;
  FOptions := TBCEditorCompletionProposal(ASource).FOptions;
  FTrigger.Assign(TBCEditorCompletionProposal(ASource).FTrigger);
  FVisibleLines := TBCEditorCompletionProposal(ASource).FVisibleLines;
  FWidth := TBCEditorCompletionProposal(ASource).FWidth;
end;

procedure TBCEditorCompletionProposal.ChangeScale(M, D: Integer);
begin
  FWidth := FWidth * M div D;
end;

constructor TBCEditorCompletionProposal.Create(AOwner: TComponent);
begin
  inherited Create;

  FOwner := AOwner;
  FCloseChars := DefaultCloseChars;
  FColumns := TBCEditorCompletionProposalColumns.Create();
  FColumns.Add(); { default column }
  FCompletionColumnIndex := DefaultCompletionColumnIndex;
  FConstraints := TSizeConstraints.Create(nil);
  FEnabled := DefaultEnabled;
  FOptions := DefaultOptions;
  FTrigger := TTrigger.Create(Self);
  FVisibleLines := DefaultVisibleLines;
  FWidth := DefaultWidth;
end;

destructor TBCEditorCompletionProposal.Destroy();
begin
  FTrigger.Free();
  FColumns.Free();
  FConstraints.Free();

  inherited;
end;

function TBCEditorCompletionProposal.GetOwner(): TPersistent;
begin
  Result := FOwner;
end;

function TBCEditorCompletionProposal.IsCloseCharsStored(): Boolean;
begin
  Result := FCloseChars <> DefaultCloseChars;
end;

function TBCEditorCompletionProposal.IsColumnsStored(): Boolean;
begin
  Result := FColumns.IsStored();
end;

function TBCEditorCompletionProposal.IsStored(): Boolean;
begin
  Result := IsCloseCharsStored()
    or IsColumnsStored()
    or (FCompletionColumnIndex <> DefaultCompletionColumnIndex)
    or (FConstraints.MaxHeight <> 0) or (FConstraints.MaxWidth <> 0) or (FConstraints.MinHeight <> 0) or (FConstraints.MinWidth <> 0)
    or (FEnabled <> DefaultEnabled)
    or (FOptions <> DefaultOptions)
    or IsTriggersStored()
    or (FVisibleLines <> DefaultVisibleLines)
    or (FWidth <> DefaultWidth);
end;

function TBCEditorCompletionProposal.IsTriggersStored(): Boolean;
begin
  Result := FTrigger.IsStored();
end;

procedure TBCEditorCompletionProposal.SetImages(const AValue: TCustomImageList);
begin
  if FImages <> AValue then
  begin
    FImages := AValue;
    if Assigned(FImages) then
      FImages.FreeNotification(FOwner);
  end;
end;

{ TBCEditorColors.TActiveLine ************************************************}

procedure TBCEditorColors.TActiveLine.Assign(ASource: TPersistent);
begin
  Assert(ASource is TBCEditorColors.TActiveLine);

  inherited;

  FBackground := TBCEditorColors.TActiveLine(ASource).FBackground;

  FColors.DoChange();
end;

constructor TBCEditorColors.TActiveLine.Create(const AColors: TBCEditorColors);
begin
  inherited Create();

  FColors := AColors;

  FBackground := clActiveLineBackground;
end;

function TBCEditorColors.TActiveLine.IsStored(): Boolean;
begin
  Result := (FBackground <> clActiveLineBackground);
end;

procedure TBCEditorColors.TActiveLine.SetBackground(AValue: TColor);
begin
  if (AValue <> FBackground) then
  begin
    FBackground := AValue;
    FColors.DoChange();
  end;
end;

{ TBCEditorColors.TBookmark ***************************************************}

procedure TBCEditorColors.TBookmark.Assign(ASource: TPersistent);
begin
  Assert(ASource is TBCEditorColors.TBookmark);

  inherited;

  FBorder := TBCEditorColors.TBookmark(ASource).FBorder;
  FCover := TBCEditorColors.TBookmark(ASource).FCover;
  FNumber := TBCEditorColors.TBookmark(ASource).FNumber;
  FRingLeft := TBCEditorColors.TBookmark(ASource).FRingLeft;
  FRingMiddle := TBCEditorColors.TBookmark(ASource).FRingMiddle;
  FRingRight := TBCEditorColors.TBookmark(ASource).FRingRight;

  FColors.DoChange();
end;

constructor TBCEditorColors.TBookmark.Create(const AColors: TBCEditorColors);
begin
  inherited Create();

  FColors := AColors;

  FBorder := clBookmarkBorder;
  FCover := clBookmarkCover;
  FNumber := clBookmarkNumber;
  FRingLeft := clBookmarkRingLeft;
  FRingMiddle := clBookmarkRingMiddle;
  FRingRight := clBookmarkRingRight;
end;

function TBCEditorColors.TBookmark.IsStored(): Boolean;
begin
  Result := (FBorder <> clBookmarkBorder)
    or (FCover <> clBookmarkCover)
    or (FNumber <> clBookmarkNumber)
    or (FRingLeft <> clBookmarkRingLeft)
    or (FRingMiddle <> clBookmarkRingMiddle)
    or (FRingRight <> clBookmarkRingRight);
end;

procedure TBCEditorColors.TBookmark.SetBorder(AValue: TColor);
begin
  if (AValue <> FBorder) then
  begin
    FBorder := AValue;
    FColors.DoChange();
  end;
end;

procedure TBCEditorColors.TBookmark.SetCover(AValue: TColor);
begin
  if (AValue <> FCover) then
  begin
    FCover := AValue;
    FColors.DoChange();
  end;
end;

procedure TBCEditorColors.TBookmark.SetNumber(AValue: TColor);
begin
  if (AValue <> FNumber) then
  begin
    FNumber := AValue;
    FColors.DoChange();
  end;
end;

procedure TBCEditorColors.TBookmark.SetRingLeft(AValue: TColor);
begin
  if (AValue <> FRingLeft) then
  begin
    FRingLeft := AValue;
    FColors.DoChange();
  end;
end;

procedure TBCEditorColors.TBookmark.SetRingMiddle(AValue: TColor);
begin
  if (AValue <> FRingMiddle) then
  begin
    FRingMiddle := AValue;
    FColors.DoChange();
  end;
end;

procedure TBCEditorColors.TBookmark.SetRingRight(AValue: TColor);
begin
  if (AValue <> FRingRight) then
  begin
    FRingRight := AValue;
    FColors.DoChange();
  end;
end;

{ TBCEditorColors.TCodeFolding ************************************************}

procedure TBCEditorColors.TCodeFolding.Assign(ASource: TPersistent);
begin
  Assert(ASource is TBCEditorColors.TCodeFolding);

  inherited;

  FBackground := TBCEditorColors.TCodeFolding(ASource).FBackground;
  FForeground := TBCEditorColors.TCodeFolding(ASource).FForeground;

  FColors.DoChange();
end;

constructor TBCEditorColors.TCodeFolding.Create(const AColors: TBCEditorColors);
begin
  inherited Create();

  FColors := AColors;

  FBackground := clCodeFoldingBackground;
  FForeground := clCodeFoldingForeground;
end;

function TBCEditorColors.TCodeFolding.IsStored(): Boolean;
begin
  Result := (FBackground <> clCodeFoldingBackground)
    or (FForeground <> clCodeFoldingForeground);
end;

procedure TBCEditorColors.TCodeFolding.SetBackground(AValue: TColor);
begin
  if (AValue <> FBackground) then
  begin
    FBackground := AValue;
    FColors.DoChange();
  end;
end;

procedure TBCEditorColors.TCodeFolding.SetForeground(AValue: TColor);
begin
  if (AValue <> FForeground) then
  begin
    FForeground := AValue;
    FColors.DoChange();
  end;
end;

{ TBCEditorColors.TFoundText ************************************************}

procedure TBCEditorColors.TFoundText.Assign(ASource: TPersistent);
begin
  Assert(ASource is TBCEditorColors.TFoundText);

  inherited;

  FBackground := TBCEditorColors.TFoundText(ASource).FBackground;
  FForeground := TBCEditorColors.TFoundText(ASource).FForeground;

  FColors.DoChange();
end;

constructor TBCEditorColors.TFoundText.Create(const AColors: TBCEditorColors);
begin
  inherited Create();

  FColors := AColors;

  FBackground := clFoundTextBackground;
  FForeground := clFoundTextForeground;
end;

function TBCEditorColors.TFoundText.IsStored(): Boolean;
begin
  Result := (FBackground <> clFoundTextBackground)
    or (FForeground <> clFoundTextForeground);
end;

procedure TBCEditorColors.TFoundText.SetBackground(AValue: TColor);
begin
  if (AValue <> FBackground) then
  begin
    FBackground := AValue;
    FColors.DoChange();
  end;
end;

procedure TBCEditorColors.TFoundText.SetForeground(AValue: TColor);
begin
  if (AValue <> FForeground) then
  begin
    FForeground := AValue;
    FColors.DoChange();
  end;
end;

{ TBCEditorColors.TLineNumbers ************************************************}

procedure TBCEditorColors.TLineNumbers.Assign(ASource: TPersistent);
begin
  Assert(ASource is TBCEditorColors.TLineNumbers);

  inherited;

  FBackground := TBCEditorColors.TLineNumbers(ASource).FBackground;
  FForeground := TBCEditorColors.TLineNumbers(ASource).FForeground;

  FColors.DoChange();
end;

constructor TBCEditorColors.TLineNumbers.Create(const AColors: TBCEditorColors);
begin
  inherited Create();

  FColors := AColors;

  FBackground := clLineNumbersBackground;
  FForeground := clLineNumbersForeground;
end;

function TBCEditorColors.TLineNumbers.IsStored(): Boolean;
begin
  Result := (FBackground <> clLineNumbersBackground)
    or (FForeground <> clLineNumbersForeground);
end;

procedure TBCEditorColors.TLineNumbers.SetBackground(AValue: TColor);
begin
  if (AValue <> FBackground) then
  begin
    FBackground := AValue;
    FColors.DoChange();
  end;
end;

procedure TBCEditorColors.TLineNumbers.SetForeground(AValue: TColor);
begin
  if (AValue <> FForeground) then
  begin
    FForeground := AValue;
    FColors.DoChange();
  end;
end;

{ TBCEditorColors.TLineState ************************************************}

procedure TBCEditorColors.TLineState.Assign(ASource: TPersistent);
begin
  Assert(ASource is TBCEditorColors.TLineState);

  inherited;

  FModified := TBCEditorColors.TLineState(ASource).FModified;
  FLoaded := TBCEditorColors.TLineState(ASource).FLoaded;
  FSaved := TBCEditorColors.TLineState(ASource).FSaved;

  FColors.DoChange();
end;

constructor TBCEditorColors.TLineState.Create(const AColors: TBCEditorColors);
begin
  inherited Create();

  FColors := AColors;

  FModified := clLineStateModified;
  FLoaded := clLineStateLoaded;
  FSaved := clLineStateSaved;
end;

function TBCEditorColors.TLineState.IsStored(): Boolean;
begin
  Result := (FModified <> clLineStateModified)
    or (FLoaded <> clLineStateLoaded)
    or (FSaved <> clLineStateSaved);
end;

procedure TBCEditorColors.TLineState.SetModified(AValue: TColor);
begin
  if (AValue <> FModified) then
  begin
    FModified := AValue;
    FColors.DoChange();
  end;
end;

procedure TBCEditorColors.TLineState.SetLoaded(AValue: TColor);
begin
  if (AValue <> FLoaded) then
  begin
    FLoaded := AValue;
    FColors.DoChange();
  end;
end;

procedure TBCEditorColors.TLineState.SetSaved(AValue: TColor);
begin
  if (AValue <> FSaved) then
  begin
    FSaved := AValue;
    FColors.DoChange();
  end;
end;

{ TBCEditorColors.TMarks ******************************************************}

procedure TBCEditorColors.TMarks.Assign(ASource: TPersistent);
begin
  Assert(ASource is TBCEditorColors.TMarks);

  inherited;

  FBackground := TBCEditorColors.TMarks(ASource).FBackground;

  FColors.DoChange();
end;

constructor TBCEditorColors.TMarks.Create(const AColors: TBCEditorColors);
begin
  inherited Create();

  FColors := AColors;

  FBackground := clMarksBackground;
end;

function TBCEditorColors.TMarks.IsStored(): Boolean;
begin
  Result := (FBackground <> clMarksBackground);
end;

procedure TBCEditorColors.TMarks.SetBackground(AValue: TColor);
begin
  if (AValue <> FBackground) then
  begin
    FBackground := AValue;
    FColors.DoChange();
  end;
end;

{ TBCEditorColors.TMatchingPairs **********************************************}

procedure TBCEditorColors.TMatchingPairs.Assign(ASource: TPersistent);
begin
  Assert(ASource is TBCEditorColors.TMatchingPairs);

  inherited;

  FBackground := TBCEditorColors.TMatchingPairs(ASource).FBackground;

  FColors.DoChange();
end;

constructor TBCEditorColors.TMatchingPairs.Create(const AColors: TBCEditorColors);
begin
  inherited Create();

  FColors := AColors;

  FBackground := clMatchingPairBackground;
end;

function TBCEditorColors.TMatchingPairs.IsStored(): Boolean;
begin
  Result := (FBackground <> clMatchingPairBackground);
end;

procedure TBCEditorColors.TMatchingPairs.SetBackground(AValue: TColor);
begin
  if (AValue <> FBackground) then
  begin
    FBackground := AValue;
    FColors.DoChange();
  end;
end;

{ TBCEditorColors.TSelection **************************************************}

procedure TBCEditorColors.TSelection.Assign(ASource: TPersistent);
begin
  Assert(ASource is TBCEditorColors.TSelection);

  inherited;

  FBackground := TBCEditorColors.TSelection(ASource).FBackground;
  FForeground := TBCEditorColors.TSelection(ASource).FForeground;

  FColors.DoChange();
end;

constructor TBCEditorColors.TSelection.Create(const AColors: TBCEditorColors);
begin
  inherited Create();

  FColors := AColors;

  FBackground := clSelectionBackground;
  FForeground := clSelectionForeground;
end;

procedure TBCEditorColors.TSelection.SetBackground(AValue: TColor);
begin
  if (AValue <> FBackground) then
  begin
    FBackground := AValue;
    FColors.DoChange();
  end;
end;

function TBCEditorColors.TSelection.IsStored(): Boolean;
begin
  Result := (FBackground <> clSelectionBackground)
    or (FForeground <> clSelectionForeground);
end;

procedure TBCEditorColors.TSelection.SetForeground(AValue: TColor);
begin
  if (AValue <> FForeground) then
  begin
    FForeground := AValue;
    FColors.DoChange();
  end;
end;

{ TBCEditorColors.TSpecialChars ************************************************}

procedure TBCEditorColors.TSpecialChars.Assign(ASource: TPersistent);
begin
  Assert(ASource is TBCEditorColors.TSpecialChars);

  inherited;

  FForeground := TBCEditorColors.TSpecialChars(ASource).FForeground;

  FColors.DoChange();
end;

constructor TBCEditorColors.TSpecialChars.Create(const AColors: TBCEditorColors);
begin
  inherited Create();

  FColors := AColors;

  FForeground := clSpecialCharsForeground;
end;

function TBCEditorColors.TSpecialChars.IsStored(): Boolean;
begin
  Result := (FForeground <> clSpecialCharsForeground);
end;

procedure TBCEditorColors.TSpecialChars.SetForeground(AValue: TColor);
begin
  if (AValue <> FForeground) then
  begin
    FForeground := AValue;
    FColors.DoChange();
  end;
end;

{ TBCEditorColors.TSyncEdit ***************************************************}

procedure TBCEditorColors.TSyncEdit.Assign(ASource: TPersistent);
begin
  Assert(ASource is TBCEditorColors.TSyncEdit);

  inherited;

  FBackground := TBCEditorColors.TSyncEdit(ASource).FBackground;
  FOverlay := TBCEditorColors.TSyncEdit(ASource).FOverlay;

  FColors.DoChange();
end;

constructor TBCEditorColors.TSyncEdit.Create(const AColors: TBCEditorColors);
begin
  inherited Create();

  FColors := AColors;

  FBackground := clSyncEditBackground;
  FOverlay := clSyncEditOverlay;
end;

function TBCEditorColors.TSyncEdit.IsStored(): Boolean;
begin
  Result := (FBackground <> clSyncEditBackground)
    or (FOverlay <> clSyncEditOverlay);
end;

procedure TBCEditorColors.TSyncEdit.SetBackground(AValue: TColor);
begin
  if (AValue <> FBackground) then
  begin
    FBackground := AValue;
    FColors.DoChange();
  end;
end;

procedure TBCEditorColors.TSyncEdit.SetOverlays(AValue: TColor);
begin
  if (AValue <> FOverlay) then
  begin
    FOverlay := AValue;
    FColors.DoChange();
  end;
end;

{ TBCEditorColors.TSyncEditButton ***************************************************}

procedure TBCEditorColors.TSyncEditButton.Assign(ASource: TPersistent);
begin
  Assert(ASource is TBCEditorColors.TSyncEditButton);

  inherited;

  FBackground := TBCEditorColors.TSyncEditButton(ASource).FBackground;
  FPen := TBCEditorColors.TSyncEditButton(ASource).FPen;
  FText := TBCEditorColors.TSyncEditButton(ASource).FText;

  FColors.DoChange();
end;

constructor TBCEditorColors.TSyncEditButton.Create(const AColors: TBCEditorColors);
begin
  inherited Create();

  FColors := AColors;

  FBackground := clSyncEditButtonBackground;
  FPen := clSyncEditButtonPen;
  FText := clSyncEditButtonText;
end;

function TBCEditorColors.TSyncEditButton.IsStored(): Boolean;
begin
  Result := (FPen <> clSyncEditButtonPen)
    or (FText <> clSyncEditButtonText);
end;

procedure TBCEditorColors.TSyncEditButton.SetBackground(AValue: TColor);
begin
  if (AValue <> FBackground) then
  begin
    FBackground := AValue;
    FColors.DoChange();
  end;
end;

procedure TBCEditorColors.TSyncEditButton.SetPen(AValue: TColor);
begin
  if (AValue <> FPen) then
  begin
    FPen := AValue;
    FColors.DoChange();
  end;
end;

procedure TBCEditorColors.TSyncEditButton.SetText(AValue: TColor);
begin
  if (AValue <> FText) then
  begin
    FText := AValue;
    FColors.DoChange();
  end;
end;

{ TBCEditorColors *************************************************************}

procedure TBCEditorColors.Assign(ASource: TPersistent);
begin
  Assert(ASource is TBCEditorColors);

  inherited;

  FActiveLine.Assign(TBCEditorColors(ASource).FActiveLine);
  FBookmark.Assign(TBCEditorColors(ASource).FBookmark);
  FCodeFolding.Assign(TBCEditorColors(ASource).FCodeFolding);
  FFoundText.Assign(TBCEditorColors(ASource).FFoundText);
  FLineNumbers.Assign(TBCEditorColors(ASource).FLineNumbers);
  FLineState.Assign(TBCEditorColors(ASource).FLineState);
  FSelection.Assign(TBCEditorColors(ASource).FSelection);
  FMarks.Assign(TBCEditorColors(ASource).FMarks);
  FMatchingPairs.Assign(TBCEditorColors(ASource).FMatchingPairs);
  FSpecialChars.Assign(TBCEditorColors(ASource).FSpecialChars);
  FSyncEdit.Assign(TBCEditorColors(ASource).FSyncEdit);
  FSyncEditButton.Assign(TBCEditorColors(ASource).FSyncEditButton);
end;

constructor TBCEditorColors.Create();
begin
  inherited;

  FActiveLine := TActiveLine.Create(Self);
  FBookmark := TBookmark.Create(Self);
  FCodeFolding := TCodeFolding.Create(Self);
  FFoundText := TFoundText.Create(Self);
  FLineNumbers := TLineNumbers.Create(Self);
  FLineState := TLineState.Create(Self);
  FMarks := TMarks.Create(Self);
  FMatchingPairs := TMatchingPairs.Create(Self);
  FSelection := TSelection.Create(Self);
  FSpecialChars := TSpecialChars.Create(Self);
  FSyncEdit := TSyncEdit.Create(Self);
  FSyncEditButton := TSyncEditButton.Create(Self);
end;

destructor TBCEditorColors.Destroy();
begin
  FActiveLine.Free();
  FBookmark.Free();
  FCodeFolding.Free();
  FFoundText.Free();
  FLineNumbers.Free();
  FLineState.Free();
  FSelection.Free();
  FMarks.Free();
  FMatchingPairs.Free();
  FSpecialChars.Free();
  FSyncEdit.Free();
  FSyncEditButton.Free();

  inherited;
end;

procedure TBCEditorColors.DoChange();
begin
  if (Assigned(FOnChange)) then
    FOnChange(Self);
end;

function TBCEditorColors.IsStored(): Boolean;
begin
  Result := FCodeFolding.IsStored()
    or FActiveLine.IsStored()
    or FFoundText.IsStored()
    or FLineNumbers.IsStored()
    or FLineState.IsStored()
    or FSelection.IsStored()
    or FMarks.IsStored()
    or FMatchingPairs.IsStored()
    or FSpecialChars.IsStored()
    or FSyncEdit.IsStored();
end;

procedure TBCEditorColors.SetActiveLine(AValue: TActiveLine);
begin
  FActiveLine.Assign(AValue);
end;

procedure TBCEditorColors.SetBookmark(AValue: TBookmark);
begin
  FBookmark.Assign(AValue);
end;

procedure TBCEditorColors.SetCodeFolding(AValue: TCodeFolding);
begin
  FCodeFolding.Assign(AValue);
end;

procedure TBCEditorColors.SetFoundText(AValue: TFoundText);
begin
  FFoundText.Assign(AValue);
end;

procedure TBCEditorColors.SetLineNumbers(AValue: TLineNumbers);
begin
  FLineNumbers.Assign(AValue);
end;

procedure TBCEditorColors.SetLineState(AValue: TLineState);
begin
  FLineState.Assign(AValue);
end;

procedure TBCEditorColors.SetMarks(AValue: TMarks);
begin
  FMarks.Assign(AValue);
end;

procedure TBCEditorColors.SetMatchingPairs(AValue: TMatchingPairs);
begin
  FMatchingPairs.Assign(AValue);
end;

procedure TBCEditorColors.SetSelection(AValue: TSelection);
begin
  FSelection.Assign(AValue);
end;

procedure TBCEditorColors.SetSpecialChars(AValue: TSpecialChars);
begin
  FSpecialChars.Assign(AValue);
end;

procedure TBCEditorColors.SetSyncEdit(AValue: TSyncEdit);
begin
  FSyncEdit.Assign(AValue);
end;

procedure TBCEditorColors.SetSyncEditButton(AValue: TSyncEditButton);
begin
  FSyncEditButton.Assign(AValue);
end;

{ TBCEditorLeftMargin.TBookmarks **********************************************}

procedure TBCEditorLeftMargin.TBookmarks.Assign(ASource: TPersistent);
begin
  Assert(ASource is TBCEditorLeftMargin.TBookmarks);

  inherited;

  TBCEditorLeftMargin.TBookmarks(ASource).FVisible := FVisible;

  FLeftMargin.DoChange();
end;

constructor TBCEditorLeftMargin.TBookmarks.Create(const ALeftMargin: TBCEditorLeftMargin);
begin
  inherited Create();

  FLeftMargin := ALeftMargin;

  FVisible := DefaultVisible;
end;

function TBCEditorLeftMargin.TBookmarks.IsStored(): Boolean;
begin
  Result := FVisible <> DefaultVisible;
end;

procedure TBCEditorLeftMargin.TBookmarks.SetVisible(AValue: Boolean);
begin
  if FVisible <> AValue then
  begin
    FVisible := AValue;
    FLeftMargin.DoChange();
  end;
end;

{ TBCEditorLeftMargin.TCodeFolding ********************************************}

procedure TBCEditorLeftMargin.TCodeFolding.Assign(ASource: TPersistent);
begin
  Assert(ASource is TBCEditorLeftMargin.TCodeFolding);

  inherited;

  FVisible := TBCEditorLeftMargin.TCodeFolding(ASource).FVisible;
  FOptions := TBCEditorLeftMargin.TCodeFolding(ASource).FOptions;

  FLeftMargin.DoChange();
end;

constructor TBCEditorLeftMargin.TCodeFolding.Create(const ALeftMargin: TBCEditorLeftMargin);
begin
  inherited Create();

  FLeftMargin := ALeftMargin;

  FDelayInterval := DefaultDelayInterval;
  FOptions := DefaultOptions;
  FVisible := DefaultVisible;

  FMouseOverHint := False;
end;

function TBCEditorLeftMargin.TCodeFolding.IsStored(): Boolean;
begin
  Result := (FDelayInterval <> DefaultDelayInterval)
    or (FOptions <> DefaultOptions)
    or (FVisible <> DefaultVisible);
end;

procedure TBCEditorLeftMargin.TCodeFolding.SetOptions(AValue: TBCEditorCodeFoldingOptions);
begin
  if (AValue <> FOptions) then
  begin
    FOptions := AValue;
    FLeftMargin.DoChange();
  end;
end;

procedure TBCEditorLeftMargin.TCodeFolding.SetVisible(const AValue: Boolean);
begin
  if (AValue <> FVisible) then
  begin
    FVisible := AValue;
    FLeftMargin.DoChange();
  end;
end;

{ TBCEditorLeftMargin.TLineNumbers ********************************************}

procedure TBCEditorLeftMargin.TLineNumbers.Assign(ASource: TPersistent);
begin
  Assert(ASource is TBCEditorLeftMargin.TLineNumbers);

  inherited;

  FOptions := TBCEditorLeftMargin.TLineNumbers(ASource).FOptions;
  FOffset := TBCEditorLeftMargin.TLineNumbers(ASource).FOffset;
  FVisible := TBCEditorLeftMargin.TLineNumbers(ASource).FVisible;

  FLeftMargin.DoChange();
end;

constructor TBCEditorLeftMargin.TLineNumbers.Create(const ALeftMargin: TBCEditorLeftMargin);
begin
  inherited Create();

  FLeftMargin := ALeftMargin;

  FOptions := DefaultOptions;
  FOffset := DefaultOffset;
  FVisible := DefaultVisible;
end;

function TBCEditorLeftMargin.TLineNumbers.IsStored(): Boolean;
begin
  Result := (FOptions <> DefaultOptions)
    or (FOffset <> DefaultOffset)
    or (FVisible <> DefaultVisible);
end;

procedure TBCEditorLeftMargin.TLineNumbers.SetOptions(const AValue: TBCEditorLeftMarginLineNumberOptions);
begin
  if (AValue <> FOptions) then
  begin
    FOptions := AValue;
    FLeftMargin.DoChange();
  end;
end;

procedure TBCEditorLeftMargin.TLineNumbers.SetOffset(const AValue: Integer);
begin
  if (AValue <> FOffset) then
  begin
    FOffset := Max(0, AValue);
    FLeftMargin.DoChange();
  end;
end;

procedure TBCEditorLeftMargin.TLineNumbers.SetVisible(const AValue: Boolean);
begin
  if (AValue <> FVisible) then
  begin
    FVisible := AValue;
    FLeftMargin.DoChange();
  end;
end;

{ TBCEditorLeftMargin.TLineState **********************************************}

procedure TBCEditorLeftMargin.TLineState.Assign(ASource: TPersistent);
begin
  Assert(ASource is TBCEditorLeftMargin.TLineState);

  inherited;

  FVisible := TBCEditorLeftMargin.TLineState(ASource).FVisible;

  FLeftMargin.DoChange();
end;

constructor TBCEditorLeftMargin.TLineState.Create(const ALeftMargin: TBCEditorLeftMargin);
begin
  inherited Create();

  FLeftMargin := ALeftMargin;

  FVisible := DefaultVisible;
end;

function TBCEditorLeftMargin.TLineState.IsStored(): Boolean;
begin
  Result := FVisible <> DefaultVisible;
end;

procedure TBCEditorLeftMargin.TLineState.SetVisible(const AValue: Boolean);
begin
  if (AValue <> FVisible) then
  begin
    FVisible := AValue;
    FLeftMargin.DoChange();
  end;
end;

{ TBCEditorLeftMargin.TMarks **************************************************}

procedure TBCEditorLeftMargin.TMarks.Assign(ASource: TPersistent);
begin
  Assert(ASource is TBCEditorLeftMargin.TMarks);

  inherited;

  FDefaultImageIndex := TBCEditorLeftMargin.TMarks(ASource).FDefaultImageIndex;
  FImages := TBCEditorLeftMargin.TMarks(ASource).FImages;
  FVisible := TBCEditorLeftMargin.TMarks(ASource).FVisible;

  FLeftMargin.DoChange();
end;

constructor TBCEditorLeftMargin.TMarks.Create(const ALeftMargin: TBCEditorLeftMargin);
begin
  inherited Create();

  FLeftMargin := ALeftMargin;

  FDefaultImageIndex := DefaultDefaultImageIndex;
  FVisible := DefaultVisible;
end;

function TBCEditorLeftMargin.TMarks.IsStored(): Boolean;
begin
  Result := (FDefaultImageIndex <> DefaultDefaultImageIndex)
    or (FImages.Count > 0)
    or (FVisible <> DefaultVisible);
end;

procedure TBCEditorLeftMargin.TMarks.SetImages(const AValue: TCustomImageList);
begin
  if (AValue <> FImages) then
  begin
    FImages := AValue;
    if Assigned(FImages) then
      FImages.FreeNotification(nil);
    FLeftMargin.DoChange();
  end;
end;

procedure TBCEditorLeftMargin.TMarks.SetVisible(AValue: Boolean);
begin
  if FVisible <> AValue then
  begin
    FVisible := AValue;
    FLeftMargin.DoChange();
  end;
end;

{ TBCEditorLeftMargin *********************************************************}

procedure TBCEditorLeftMargin.Assign(ASource: TPersistent);
begin
  Assert(ASource is TBCEditorLeftMargin);

  inherited;

  FBookmarks.Assign(TBCEditorLeftMargin(ASource).FBookmarks);
  FCodeFolding.Assign(TBCEditorLeftMargin(ASource).FCodeFolding);
  FLineNumbers.Assign(TBCEditorLeftMargin(ASource).FLineNumbers);
  FLineState.Assign(TBCEditorLeftMargin(ASource).FLineState);
  FMarks.Assign(TBCEditorLeftMargin(ASource).FMarks);

  DoChange();
end;

constructor TBCEditorLeftMargin.Create(AOwner: TComponent);
begin
  inherited Create;

  FBookmarks := TBookmarks.Create(Self);
  FCodeFolding := TCodeFolding.Create(Self);
  FLineState := TLineState.Create(Self);
  FLineNumbers := TLineNumbers.Create(Self);
  FMarks := TMarks.Create(Self);
  FOnChange := nil;
end;

destructor TBCEditorLeftMargin.Destroy();
begin
  FBookmarks.Free();
  FCodeFolding.Free();
  FLineState.Free();
  FLineNumbers.Free();
  FMarks.Free();

  inherited;
end;

procedure TBCEditorLeftMargin.DoChange();
begin
  if (Assigned(FOnChange)) then
    FOnChange(Self);
end;

function TBCEditorLeftMargin.IsStored(): Boolean;
begin
  Result := FBookmarks.IsStored()
    or FCodeFolding.IsStored();
end;

procedure TBCEditorLeftMargin.SetBookMarks(AValue: TBookmarks);
begin
  FBookmarks.Assign(AValue);
end;

procedure TBCEditorLeftMargin.SetCodeFolding(AValue: TCodeFolding);
begin
  FCodeFolding.Assign(AValue);
end;

procedure TBCEditorLeftMargin.SetLineNumbers(AValue: TLineNumbers);
begin
  FLineNumbers.Assign(AValue);
end;

procedure TBCEditorLeftMargin.SetLineState(AValue: TLineState);
begin
  FLineState.Assign(AValue);
end;

procedure TBCEditorLeftMargin.SetMarks(AValue: TMarks);
begin
  FMarks.Assign(AValue);
end;

{ TBCEditorMinimap ************************************************************}

procedure TBCEditorMinimap.Assign(ASource: TPersistent);
begin
  Assert(ASource is TBCEditorMinimap);

  inherited;

  FFontSize := TBCEditorMinimap(ASource).FFontSize;
  FVisible := TBCEditorMinimap(ASource).FVisible;
  FWidth := TBCEditorMinimap(ASource).FWidth;

  DoChange();
end;

procedure TBCEditorMinimap.ChangeScale(M, D: Integer);
begin
  FWidth := FWidth * M div D;
end;

constructor TBCEditorMinimap.Create();
begin
  inherited;

  FFontSize := DefaultFontSize;
  FVisible := DefaultVisible;
  FWidth := DefaultWidth;
end;

procedure TBCEditorMinimap.DoChange();
begin
  if (Assigned(FOnChange)) then
    FOnChange(Self);
end;

procedure TBCEditorMinimap.SetFontSize(const AValue: Integer);
var
  LValue: Integer;
begin
  LValue := Min(100, Max(1, AValue));
  if (LValue <> FFontSize) then
  begin
    FFontSize := LValue;
    DoChange();
  end;
end;

procedure TBCEditorMinimap.SetVisible(const AValue: Boolean);
begin
  if (AValue <> FVisible) then
  begin
    FVisible := AValue;
    DoChange();
  end;
end;

procedure TBCEditorMinimap.SetWidth(const AValue: Integer);
begin
  if (AValue <> FWidth) then
  begin
    FWidth := AValue;
    DoChange();
  end;
end;

end.

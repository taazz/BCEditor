unit BCEditor.Properties;

interface {********************************************************************}

uses
  Classes, UITypes, Generics.Collections, RegularExpressions,
  Graphics, Controls, ImgList,
  BCEditor.Consts, BCEditor.Types;

type
  TBCEditorCompletionProposalCloseEvent = procedure(Sender: TObject; var ASelectedItem: string) of object;
  TBCEditorCompletionProposalValidateEvent = procedure(ASender: TObject; Shift: TShiftState; EndToken: Char) of object;

  TBCEditorCompletionProposalItems = class(TCollection)
    type

      TItem = class(TCollectionItem)
      strict private
        FImageIndex: Integer;
        FValue: string;
      public
        procedure Assign(ASource: TPersistent); override;
        constructor Create(ACollection: TCollection); override;
      published
        property ImageIndex: Integer read FImageIndex write FImageIndex default -1;
        property Value: string read FValue write FValue;
      end;

    strict private
      FOwner: TPersistent;
      function GetItem(AIndex: Integer): TItem;
      procedure SetItem(AIndex: Integer; AValue: TItem);
    protected
      function GetOwner: TPersistent; override;
    public
      constructor Create(AOwner: TPersistent);
      function Add: TItem;
      function FindItemID(AID: Integer): TItem;
      function Insert(AIndex: Integer): TItem;
      property Items[AIndex: Integer]: TItem read GetItem write SetItem; default;
    end;

  TBCEditorCompletionProposalColumns = class(TCollection)
  type

    TColumn = class(TCollectionItem)
    type

      TTitle = class(TPersistent)
      type

        TColors = class(TPersistent)
        strict private
          FBackground: TColor;
          FBottomBorder: TColor;
          FRightBorder: TColor;
        public
          procedure Assign(ASource: TPersistent); override;
          constructor Create();
        published
          property Background: TColor read FBackground write FBackground default clWindow;
          property BottomBorder: TColor read FBottomBorder write FBottomBorder default clBtnFace;
          property RightBorder: TColor read FRightBorder write FRightBorder default clBtnFace;
        end;

      strict private
        FCaption: string;
        FColors: TTitle.TColors;
        FFont: TFont;
        FVisible: Boolean;
        procedure SetFont(const AValue: TFont);
      public
        procedure Assign(ASource: TPersistent); override;
        constructor Create();
        destructor Destroy(); override;
      published
        property Caption: string read FCaption write FCaption;
        property Colors: TTitle.TColors read FColors write FColors;
        property Font: TFont read FFont write SetFont;
        property Visible: Boolean read FVisible write FVisible default False;
      end;

    strict private
      FAutoWidth: Boolean;
      FFont: TFont;
      FItems: TBCEditorCompletionProposalItems;
      FTitle: TTitle;
      FVisible: Boolean;
      FWidth: Integer;
      procedure SetFont(const AValue: TFont);
    public
      procedure Assign(ASource: TPersistent); override;
      constructor Create(ACollection: TCollection); override;
      destructor Destroy(); override;
      property Items: TBCEditorCompletionProposalItems read FItems write FItems;
    published
      property AutoWidth: Boolean read FAutoWidth write FAutoWidth default True;
      property Font: TFont read FFont write SetFont;
      property Title: TTitle read FTitle write FTitle;
      property Visible: Boolean read FVisible write FVisible default True;
      property Width: Integer read FWidth write FWidth default 0;
    end;

  strict private
    FOwner: TPersistent;
    function GetItem(AIndex: Integer): TColumn;
    procedure SetItem(AIndex: Integer; AValue: TColumn);
  protected
    function GetOwner: TPersistent; override;
  public
    constructor Create(AOwner: TPersistent);
    function Add: TColumn;
    function FindItemID(AID: Integer): TColumn;
    function Insert(AIndex: Integer): TColumn;
    property Items[AIndex: Integer]: TColumn read GetItem write SetItem; default;
  end;

  TBCEditorCompletionProposalShowEvent = procedure(Sender: TObject;
    const AColumns: TBCEditorCompletionProposalColumns;
    const AInput: string; var ACanExecute: Boolean) of object;

  TBCEditorCompletionProposal = class(TPersistent)
  type
    TTrigger = class(TPersistent)
    strict private
      FChars: string;
      FCompletionProposal: TBCEditorCompletionProposal;
      FEnabled: Boolean;
      FInterval: Integer;
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
    DefaultOptions = [cpoAutoConstraints, cpoAddHighlighterKeywords, cpoFiltered,
      cpoParseItemsFromText, cpoUseHighlighterColumnFont];
  strict private
    FCloseChars: string;
    FColumns: TBCEditorCompletionProposalColumns;
    FCompletionColumnIndex: Integer;
    FConstraints: TSizeConstraints;
    FEnabled: Boolean;
    FImages: TCustomImageList;
    FOptions: TBCEditorCompletionProposalOptions;
    FOwner: TComponent;
    FTrigger: TBCEditorCompletionProposal.TTrigger;
    FVisibleLines: Integer;
    FWidth: Integer;
    procedure SetImages(const AValue: TCustomImageList);
  protected
    function GetOwner: TPersistent; override;
  public
    constructor Create(AOwner: TComponent);
    destructor Destroy(); override;
    procedure Assign(ASource: TPersistent); override;
    procedure ChangeScale(M, D: Integer);
  published
    property CloseChars: string read FCloseChars write FCloseChars;
    property Columns: TBCEditorCompletionProposalColumns read FColumns write FColumns;
    property CompletionColumnIndex: Integer read FCompletionColumnIndex write FCompletionColumnIndex default 0;
    property Constraints: TSizeConstraints read FConstraints write FConstraints;
    property Enabled: Boolean read FEnabled write FEnabled default True;
    property Images: TCustomImageList read FImages write SetImages;
    property Options: TBCEditorCompletionProposalOptions read FOptions write FOptions default DefaultOptions;
    property Trigger: TBCEditorCompletionProposal.TTrigger read FTrigger write FTrigger;
    property VisibleLines: Integer read FVisibleLines write FVisibleLines default 8;
    property Width: Integer read FWidth write FWidth default 260;
  end;

  TBCEditorColors = class(TPersistent)
  type
    TCodeFolding = class(TPersistent)
    strict private const
      DefaultBackground = $00F4F4F4;
      DefaultForeground = $00CC9999;
    private
      FBackground: TColor;
      FColors: TBCEditorColors;
      FForeground: TColor;
      procedure SetBackground(AValue: TColor);
      procedure SetForeground(AValue: TColor);
    public
      procedure Assign(ASource: TPersistent); override;
      constructor Create(const AColors: TBCEditorColors);
    published
      property Background: TColor read FBackground write SetBackground default DefaultBackground;
      property Foreground: TColor read FForeground write SetForeground default DefaultForeground;
    end;

    TCurrentLine = class(TPersistent)
    strict private const
      DefaultBackground = $00E6FFFA;
    private
      FBackground: TColor;
      FColors: TBCEditorColors;
      procedure SetBackground(AValue: TColor);
    public
      procedure Assign(ASource: TPersistent); override;
      constructor Create(const AColors: TBCEditorColors);
    published
      property Background: TColor read FBackground write SetBackground default DefaultBackground;
    end;

    TFoundText = class(TPersistent)
    strict private const
      DefaultBackground = $0078AAFF;
      DefaultForeground = clWindowText;
    private
      FBackground: TColor;
      FColors: TBCEditorColors;
      FForeground: TColor;
      procedure SetBackground(AValue: TColor);
      procedure SetForeground(AValue: TColor);
    public
      procedure Assign(ASource: TPersistent); override;
      constructor Create(const AColors: TBCEditorColors);
    published
      property Background: TColor read FBackground write SetBackground default DefaultBackground;
      property Foreground: TColor read FForeground write SetForeground default DefaultForeground;
    end;

    TLineNumbers = class(TPersistent)
    strict private const
      DefaultBackground = $00F4F4F4;
      DefaultForeground = $00CC9999;
    private
      FBackground: TColor;
      FColors: TBCEditorColors;
      FForeground: TColor;
      procedure SetBackground(AValue: TColor);
      procedure SetForeground(AValue: TColor);
    public
      procedure Assign(ASource: TPersistent); override;
      constructor Create(const AColors: TBCEditorColors);
    published
      property Background: TColor read FBackground write SetBackground default DefaultBackground;
      property Foreground: TColor read FForeground write SetForeground default DefaultForeground;
    end;

    TLineState = class(TPersistent)
    strict private const
      DefaultBackground = $00F4F4F4;
    private
      FColors: TBCEditorColors;
      FModified: TColor;
      FLoaded: TColor;
      FSaved: TColor;
      procedure SetModified(AValue: TColor);
      procedure SetLoaded(AValue: TColor);
      procedure SetSaved(AValue: TColor);
    public
      procedure Assign(ASource: TPersistent); override;
      constructor Create(const AColors: TBCEditorColors);
    published
      property Modified: TColor read FModified write SetModified default clYellow;
      property Loaded: TColor read FLoaded write SetLoaded default DefaultBackground;
      property Saved: TColor read FSaved write SetSaved default clLime;
    end;

    TMarks = class(TPersistent)
    private
      FBackground: TColor;
      FColors: TBCEditorColors;
      procedure SetBackground(AValue: TColor);
    public
      procedure Assign(ASource: TPersistent); override;
      constructor Create(const AColors: TBCEditorColors);
    published
      property Background: TColor read FBackground write SetBackground default clBtnFace;
    end;

    TMatchingPairs = class(TPersistent)
    strict private const
      DefaultBackground = clAqua;
    private
      FBackground: TColor;
      FColors: TBCEditorColors;
      procedure SetBackground(AValue: TColor);
    public
      procedure Assign(ASource: TPersistent); override;
      constructor Create(const AColors: TBCEditorColors);
    published
      property Background: TColor read FBackground write SetBackground default DefaultBackground;
    end;

    TSelection = class(TPersistent)
    strict private const
      DefaultBackground = clHighlight;
      DefaultForeground = clHighlightText;
    private
      FBackground: TColor;
      FColors: TBCEditorColors;
      FForeground: TColor;
      procedure SetBackground(AValue: TColor);
      procedure SetForeground(AValue: TColor);
    public
      procedure Assign(ASource: TPersistent); override;
      constructor Create(const AColors: TBCEditorColors);
    published
      property Background: TColor read FBackground write SetBackground default DefaultBackground;
      property Foreground: TColor read FForeground write SetForeground default DefaultForeground;
    end;

    TSpecialChars = class(TPersistent)
    strict private const
      DefaultForeground = clGrayText;
    private
      FForeground: TColor;
      FColors: TBCEditorColors;
      procedure SetForeground(AValue: TColor);
    public
      procedure Assign(ASource: TPersistent); override;
      constructor Create(const AColors: TBCEditorColors);
    published
      property Foreground: TColor read FForeground write SetForeground default DefaultForeground;
    end;

    TSyncEdit = class(TPersistent)
    strict private const
      DefaultBackground = $00FCFDCD;
      DefaultOverlay = clHighlight;
    private
      FBackground: TColor;
      FColors: TBCEditorColors;
      FOverlays: TColor;
      procedure SetBackground(AValue: TColor);
      procedure SetOverlays(AValue: TColor);
    public
      procedure Assign(ASource: TPersistent); override;
      constructor Create(const AColors: TBCEditorColors);
    published
      property Background: TColor read FBackground write SetBackground default DefaultBackground;
      property Overlays: TColor read FOverlays write SetOverlays default DefaultOverlay;
    end;
  private
    FCodeFolding: TCodeFolding;
    FCurrentLine: TCurrentLine;
    FFoundText: TFoundText;
    FLineNumbers: TLineNumbers;
    FLineState: TLineState;
    FMarks: TMarks;
    FMatchingPairs: TMatchingPairs;
    FOnChange: TNotifyEvent;
    FSelection: TSelection;
    FSpecialChars: TSpecialChars;
    FSyncEdit: TSyncEdit;
    procedure DoChange();
    procedure SetCodeFolding(AValue: TCodeFolding);
    procedure SetCurrentLine(AValue: TCurrentLine);
    procedure SetFoundText(AValue: TFoundText);
    procedure SetLineNumbers(AValue: TLineNumbers);
    procedure SetLineState(AValue: TLineState);
    procedure SetMarks(AValue: TMarks);
    procedure SetMatchingPairs(AValue: TMatchingPairs);
    procedure SetSelection(AValue: TSelection);
    procedure SetSpecialChars(AValue: TSpecialChars);
    procedure SetSyncEdit(AValue: TSyncEdit);
  protected
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  public
    procedure Assign(ASource: TPersistent); override;
    constructor Create();
    destructor Destroy(); override;
  published
    property CodeFolding: TCodeFolding read FCodeFolding write SetCodeFolding;
    property CurrentLine: TCurrentLine read FCurrentLine write SetCurrentLine;
    property FoundText: TFoundText read FFoundText write SetFoundText;
    property LineNumbers: TLineNumbers read FLineNumbers write SetLineNumbers;
    property LineState: TLineState read FLineState write SetLineState;
    property Marks: TMarks read FMarks write SetMarks;
    property MatchingPairs: TMatchingPairs read FMatchingPairs write SetMatchingPairs;
    property Selection: TSelection read FSelection write SetSelection;
    property SpecialChars: TSpecialChars read FSpecialChars write SetSpecialChars;
    property SyncEdit: TSyncEdit read FSyncEdit write SetSyncEdit;
  end;

  TBCEditorLeftMargin = class(TPersistent)
  type
    TBookMarks = class(TPersistent)
    strict private
      FLeftMargin: TBCEditorLeftMargin;
      FVisible: Boolean;
      procedure SetVisible(AValue: Boolean);
    public
      procedure Assign(ASource: TPersistent); override;
      constructor Create(const ALeftMargin: TBCEditorLeftMargin);
    published
      property Visible: Boolean read FVisible write SetVisible default True;
    end;

    TCodeFolding = class(TPersistent)
    type
    strict private const
      DefaultOptions = [cfoHighlightIndentGuides, cfoShowTreeLine];
    strict private
      FDelayInterval: Cardinal;
      FLeftMargin: TBCEditorLeftMargin;
      FMouseOverHint: Boolean;
      FOptions: TBCEditorCodeFoldingOptions;
      FVisible: Boolean;
      procedure SetOptions(AValue: TBCEditorCodeFoldingOptions);
      procedure SetVisible(const AValue: Boolean);
    public
      procedure Assign(ASource: TPersistent); override;
      constructor Create(const ALeftMargin: TBCEditorLeftMargin);
    published
      property DelayInterval: Cardinal read FDelayInterval write FDelayInterval default 300;
      property Options: TBCEditorCodeFoldingOptions read FOptions write SetOptions default DefaultOptions;
      property Visible: Boolean read FVisible write SetVisible default False;
    end;

    TMarks = class(TPersistent)
    strict private
      FDefaultImageIndex: Integer;
      FImages: TCustomImageList;
      FLeftMargin: TBCEditorLeftMargin;
      FShortCuts: Boolean;
      FVisible: Boolean;
      procedure SetImages(const AValue: TCustomImageList);
      procedure SetVisible(AValue: Boolean);
    public
      procedure Assign(ASource: TPersistent); override;
      constructor Create(const ALeftMargin: TBCEditorLeftMargin);
    published
      property DefaultImageIndex: Integer read FDefaultImageIndex write FDefaultImageIndex default -1;
      property Images: TCustomImageList read FImages write SetImages;
      property Visible: Boolean read FVisible write SetVisible default False;
    end;

    TLineNumbers = class(TPersistent)
    strict private const
      DefaultOptions = [lnoIntens];
      DefaultStartFrom = 1;
      DefaultVisible = True;
    strict private
      FLeftMargin: TBCEditorLeftMargin;
      FOptions: TBCEditorLeftMarginLineNumberOptions;
      FStartFrom: Integer;
      FVisible: Boolean;
      procedure SetOptions(const AValue: TBCEditorLeftMarginLineNumberOptions);
      procedure SetStartFrom(const AValue: Integer);
      procedure SetVisible(const AValue: Boolean);
    public
      procedure Assign(ASource: TPersistent); override;
      constructor Create(const ALeftMargin: TBCEditorLeftMargin);
    published
      property Options: TBCEditorLeftMarginLineNumberOptions read FOptions write SetOptions default DefaultOptions;
      property StartFrom: Integer read FStartFrom write SetStartFrom default DefaultStartFrom;
      property Visible: Boolean read FVisible write SetVisible default DefaultVisible;
    end;

    TLineState = class(TPersistent)
    strict private const
      DefaultVisible = True;
    strict private
      FLeftMargin: TBCEditorLeftMargin;
      FVisible: Boolean;
      procedure SetVisible(const AValue: Boolean);
    public
      procedure Assign(ASource: TPersistent); override;
      constructor Create(const ALeftMargin: TBCEditorLeftMargin);
    published
      property Visible: Boolean read FVisible write SetVisible default DefaultVisible;
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

  TBCEditorTabs = class(TPersistent)
  strict private const
    DefaultOptions = [toSelectedBlockIndent];
    DefaultWidth = 2;
  strict private
    FOnChange: TNotifyEvent;
    FOptions: TBCEditorTabOptions;
    FWidth: Integer;
    procedure DoChange();
    procedure SetOptions(const AValue: TBCEditorTabOptions);
    procedure SetWidth(const AValue: Integer);
  protected
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  public
    procedure Assign(ASource: TPersistent); override;
    constructor Create();
  published
    property Options: TBCEditorTabOptions read FOptions write SetOptions default DefaultOptions;
    property Width: Integer read FWidth write SetWidth default DefaultWidth;
  end;

implementation  {**************************************************************}

uses
  Windows,
  Math, SysUtils, Character,
  Menus;

{ TBCEditorCompletionProposalItems.TItem **************************************}

procedure TBCEditorCompletionProposalItems.TItem.Assign(ASource: TPersistent);
begin
  Assert(ASource is TBCEditorCompletionProposalItems.TItem);

  inherited;

  FImageIndex := TBCEditorCompletionProposalItems.TItem(ASource).FImageIndex;
  FValue := TBCEditorCompletionProposalItems.TItem(ASource).FValue;
end;

constructor TBCEditorCompletionProposalItems.TItem.Create(ACollection: TCollection);
begin
  inherited;

  FImageIndex := -1;
end;

{ TBCEditorCompletionProposal.TItems ******************************************}

constructor TBCEditorCompletionProposalItems.Create(AOwner: TPersistent);
begin
  inherited Create(TItem);

  FOwner := AOwner;
end;

function TBCEditorCompletionProposalItems.Add: TBCEditorCompletionProposalItems.TItem;
begin
  Result := inherited Add as TBCEditorCompletionProposalItems.TItem;
end;

function TBCEditorCompletionProposalItems.FindItemID(AID: Integer): TBCEditorCompletionProposalItems.TItem;
begin
  Result := inherited FindItemID(AID) as TBCEditorCompletionProposalItems.TItem;
end;

function TBCEditorCompletionProposalItems.GetItem(AIndex: Integer): TBCEditorCompletionProposalItems.TItem;
begin
  Result := inherited GetItem(AIndex) as TBCEditorCompletionProposalItems.TItem;
end;

function TBCEditorCompletionProposalItems.GetOwner: TPersistent;
begin
  Result := FOwner;
end;

function TBCEditorCompletionProposalItems.Insert(AIndex: Integer): TBCEditorCompletionProposalItems.TItem;
begin
  Result := inherited Insert(AIndex) as TBCEditorCompletionProposalItems.TItem;
end;

procedure TBCEditorCompletionProposalItems.SetItem(AIndex: Integer; AValue: TBCEditorCompletionProposalItems.TItem);
begin
  inherited SetItem(AIndex, AValue);
end;

{ TBCEditorCompletionProposalColumns.TColumn.TTitle.TColors *****************}

procedure TBCEditorCompletionProposalColumns.TColumn.TTitle.TColors.Assign(ASource: TPersistent);
begin
  Assert(ASource is TBCEditorCompletionProposalColumns.TColumn.TTitle.TColors);

  inherited;

  FBackground := TBCEditorCompletionProposalColumns.TColumn.TTitle.TColors(ASource).FBackground;
  FBottomBorder := TBCEditorCompletionProposalColumns.TColumn.TTitle.TColors(ASource).FBottomBorder;
  FRightBorder := TBCEditorCompletionProposalColumns.TColumn.TTitle.TColors(ASource).FRightBorder;
end;

constructor TBCEditorCompletionProposalColumns.TColumn.TTitle.TColors.Create();
begin
  inherited Create();

  FBackground := clWindow;
  FBottomBorder := clBtnFace;
  FRightBorder := clBtnFace;
end;

{ TBCEditorCompletionProposalColumns.TColumn.TTitle *************************}

procedure TBCEditorCompletionProposalColumns.TColumn.TTitle.Assign(ASource: TPersistent);
begin
  Assert(ASource is TBCEditorCompletionProposalColumns.TColumn.TTitle);

  inherited;

  FCaption := TBCEditorCompletionProposalColumns.TColumn.TTitle(ASource).FCaption;
  FColors.Assign(TBCEditorCompletionProposalColumns.TColumn.TTitle(ASource).FColors);
  FFont.Assign(TBCEditorCompletionProposalColumns.TColumn.TTitle(ASource).FFont);
  FVisible := TBCEditorCompletionProposalColumns.TColumn.TTitle(ASource).FVisible;
end;

constructor TBCEditorCompletionProposalColumns.TColumn.TTitle.Create();
begin
  inherited Create();

  FColors := TColors.Create;
  FFont := TFont.Create;
  FFont.Name := 'Courier New';
  FFont.Size := 8;
  FVisible := False;
end;

destructor TBCEditorCompletionProposalColumns.TColumn.TTitle.Destroy();
begin
  FColors.Free();
  FFont.Free();

  inherited;
end;

procedure TBCEditorCompletionProposalColumns.TColumn.TTitle.SetFont(const AValue: TFont);
begin
  FFont.Assign(AValue);
end;

{ TBCEditorCompletionProposalColumns.TColumn ********************************}

procedure TBCEditorCompletionProposalColumns.TColumn.Assign(ASource: TPersistent);
begin
  Assert(ASource is TBCEditorCompletionProposalColumns.TColumn);

  inherited;

  FAutoWidth := TBCEditorCompletionProposalColumns.TColumn(ASource).FAutoWidth;
  FFont.Assign(TBCEditorCompletionProposalColumns.TColumn(ASource).FFont);
  FItems.Assign(TBCEditorCompletionProposalColumns.TColumn(ASource).FItems);
  FTitle.Assign(TBCEditorCompletionProposalColumns.TColumn(ASource).FTitle);
  FWidth := TBCEditorCompletionProposalColumns.TColumn(ASource).FWidth;
end;

constructor TBCEditorCompletionProposalColumns.TColumn.Create(ACollection: TCollection);
begin
  inherited;

  FAutoWidth := True;
  FFont := TFont.Create;
  FFont.Name := 'Courier New';
  FFont.Size := 8;
  FItems := TBCEditorCompletionProposalItems.Create(Self);
  FTitle := TTitle.Create;
  FVisible := True;
  FWidth := 0;
end;

destructor TBCEditorCompletionProposalColumns.TColumn.Destroy();
begin
  FFont.Free();
  FItems.Free();
  FTitle.Free();

  inherited;
end;

procedure TBCEditorCompletionProposalColumns.TColumn.SetFont(const AValue: TFont);
begin
  FFont.Assign(AValue);
end;

{ TBCEditorCompletionProposalColumns ****************************************}

constructor TBCEditorCompletionProposalColumns.Create(AOwner: TPersistent);
begin
  inherited Create(TColumn);

  FOwner := AOwner;
end;

function TBCEditorCompletionProposalColumns.Add: TColumn;
begin
  Result := inherited Add as TColumn;
end;

function TBCEditorCompletionProposalColumns.FindItemID(AID: Integer): TColumn;
begin
  Result := inherited FindItemID(AID) as TColumn;
end;

function TBCEditorCompletionProposalColumns.GetItem(AIndex: Integer): TColumn;
begin
  Result := inherited GetItem(AIndex) as TColumn;
end;

function TBCEditorCompletionProposalColumns.GetOwner: TPersistent;
begin
  Result := FOwner;
end;

function TBCEditorCompletionProposalColumns.Insert(AIndex: Integer): TColumn;
begin
  Result := inherited Insert(AIndex) as TColumn;
end;

procedure TBCEditorCompletionProposalColumns.SetItem(AIndex: Integer; AValue: TColumn);
begin
  inherited SetItem(AIndex, AValue);
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

  FChars := '.';
  FEnabled := False;
  FInterval := 1000;
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
  FColumns := TBCEditorCompletionProposalColumns.Create(Self);
  FColumns.Add; { default column }
  FCompletionColumnIndex := 0;
  FEnabled := True;
  FOptions := DefaultOptions;
  FTrigger := TTrigger.Create(Self);
  FVisibleLines := 8;
  FWidth := 260;
  FConstraints := TSizeConstraints.Create(nil);
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

procedure TBCEditorCompletionProposal.SetImages(const AValue: TCustomImageList);
begin
  if FImages <> AValue then
  begin
    FImages := AValue;
    if Assigned(FImages) then
      FImages.FreeNotification(FOwner);
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

  FBackground := DefaultBackground;
  FForeground := DefaultForeground;
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

{ TBCEditorColors.TCurrentLine ************************************************}

procedure TBCEditorColors.TCurrentLine.Assign(ASource: TPersistent);
begin
  Assert(ASource is TBCEditorColors.TCurrentLine);

  inherited;

  FBackground := TBCEditorColors.TCurrentLine(ASource).FBackground;

  FColors.DoChange();
end;

constructor TBCEditorColors.TCurrentLine.Create(const AColors: TBCEditorColors);
begin
  inherited Create();

  FColors := AColors;

  FBackground := DefaultBackground;
end;

procedure TBCEditorColors.TCurrentLine.SetBackground(AValue: TColor);
begin
  if (AValue <> FBackground) then
  begin
    FBackground := AValue;
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

  FBackground := DefaultBackground;
  FForeground := DefaultForeground;
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

  FBackground := DefaultBackground;
  FForeground := DefaultForeground;
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

  FModified := clYellow;
  FLoaded := DefaultBackground;
  FSaved := clLime;
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

  FBackground := clBtnFace;
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

  FBackground := DefaultBackground;
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

  FBackground := DefaultBackground;
  FForeground := DefaultForeground;
end;

procedure TBCEditorColors.TSelection.SetBackground(AValue: TColor);
begin
  if (AValue <> FBackground) then
  begin
    FBackground := AValue;
    FColors.DoChange();
  end;
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

  FForeground := DefaultForeground;
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
  FOverlays := TBCEditorColors.TSyncEdit(ASource).FOverlays;

  FColors.DoChange();
end;

constructor TBCEditorColors.TSyncEdit.Create(const AColors: TBCEditorColors);
begin
  inherited Create();

  FColors := AColors;

  FBackground := DefaultBackground;
  FOverlays := DefaultOverlay;
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
  if (AValue <> FOverlays) then
  begin
    FOverlays := AValue;
    FColors.DoChange();
  end;
end;

{ TBCEditorColors *************************************************************}

procedure TBCEditorColors.Assign(ASource: TPersistent);
begin
  Assert(ASource is TBCEditorColors);

  inherited;

  FCodeFolding.Assign(TBCEditorColors(ASource).FCodeFolding);
  FCurrentLine.Assign(TBCEditorColors(ASource).FCurrentLine);
  FFoundText.Assign(TBCEditorColors(ASource).FFoundText);
  FLineNumbers.Assign(TBCEditorColors(ASource).FLineNumbers);
  FLineState.Assign(TBCEditorColors(ASource).FLineState);
  FSelection.Assign(TBCEditorColors(ASource).FSelection);
  FMarks.Assign(TBCEditorColors(ASource).FMarks);
  FMatchingPairs.Assign(TBCEditorColors(ASource).FMatchingPairs);
  FSpecialChars.Assign(TBCEditorColors(ASource).FSpecialChars);
  FSyncEdit.Assign(TBCEditorColors(ASource).FSyncEdit);
end;

constructor TBCEditorColors.Create();
begin
  inherited;

  FCodeFolding := TCodeFolding.Create(Self);
  FCurrentLine := TCurrentLine.Create(Self);
  FFoundText := TFoundText.Create(Self);
  FLineNumbers := TLineNumbers.Create(Self);
  FLineState := TLineState.Create(Self);
  FMarks := TMarks.Create(Self);
  FMatchingPairs := TMatchingPairs.Create(Self);
  FSelection := TSelection.Create(Self);
  FSpecialChars := TSpecialChars.Create(Self);
  FSyncEdit := TSyncEdit.Create(Self);
end;

destructor TBCEditorColors.Destroy();
begin
  FCodeFolding.Free();
  FCurrentLine.Free();
  FFoundText.Free();
  FLineNumbers.Free();
  FLineState.Free();
  FSelection.Free();
  FMarks.Free();
  FMatchingPairs.Free();
  FSpecialChars.Free();
  FSyncEdit.Free();

  inherited;
end;

procedure TBCEditorColors.DoChange();
begin
  if (Assigned(FOnChange)) then
    FOnChange(Self);
end;

procedure TBCEditorColors.SetCodeFolding(AValue: TCodeFolding);
begin
  FCodeFolding.Assign(AValue);
end;

procedure TBCEditorColors.SetCurrentLine(AValue: TCurrentLine);
begin
  FCurrentLine.Assign(AValue);
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

  FVisible := True;
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

  FVisible := False;
  FOptions := DefaultOptions;
  FDelayInterval := 300;

  FMouseOverHint := False;
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

{ TBCEditorLeftMargin.TMarks **************************************************}

procedure TBCEditorLeftMargin.TMarks.Assign(ASource: TPersistent);
begin
  Assert(ASource is TBCEditorLeftMargin.TMarks);

  inherited;

  FDefaultImageIndex := TBCEditorLeftMargin.TMarks(ASource).FDefaultImageIndex;
  FImages := TBCEditorLeftMargin.TMarks(ASource).FImages;
  FShortCuts := TBCEditorLeftMargin.TMarks(ASource).FShortCuts;
  FVisible := TBCEditorLeftMargin.TMarks(ASource).FVisible;

  FLeftMargin.DoChange();
end;

constructor TBCEditorLeftMargin.TMarks.Create(const ALeftMargin: TBCEditorLeftMargin);
begin
  inherited Create();

  FLeftMargin := ALeftMargin;

  FDefaultImageIndex := -1;
  FShortCuts := True;
  FVisible := False;
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

{ TBCEditorLeftMargin.TLineNumbers ********************************************}

procedure TBCEditorLeftMargin.TLineNumbers.Assign(ASource: TPersistent);
begin
  Assert(ASource is TBCEditorLeftMargin.TLineNumbers);

  inherited;

  FOptions := TBCEditorLeftMargin.TLineNumbers(ASource).FOptions;
  FStartFrom := TBCEditorLeftMargin.TLineNumbers(ASource).FStartFrom;
  FVisible := TBCEditorLeftMargin.TLineNumbers(ASource).FVisible;

  FLeftMargin.DoChange();
end;

constructor TBCEditorLeftMargin.TLineNumbers.Create(const ALeftMargin: TBCEditorLeftMargin);
begin
  inherited Create();

  FLeftMargin := ALeftMargin;

  FOptions := DefaultOptions;
  FStartFrom := DefaultStartFrom;
  FVisible := DefaultVisible;
end;

procedure TBCEditorLeftMargin.TLineNumbers.SetOptions(const AValue: TBCEditorLeftMarginLineNumberOptions);
begin
  if (AValue <> FOptions) then
  begin
    FOptions := AValue;
    FLeftMargin.DoChange();
  end;
end;

procedure TBCEditorLeftMargin.TLineNumbers.SetStartFrom(const AValue: Integer);
begin
  if (AValue <> FStartFrom) then
  begin
    FStartFrom := Max(0, AValue);
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

procedure TBCEditorLeftMargin.TLineState.SetVisible(const AValue: Boolean);
begin
  if (AValue <> FVisible) then
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
  FMarks.Assign(TBCEditorLeftMargin(ASource).FMarks);
  FLineNumbers.Assign(TBCEditorLeftMargin(ASource).FLineNumbers);

  DoChange();
end;

constructor TBCEditorLeftMargin.Create(AOwner: TComponent);
begin
  inherited Create;

  FBookmarks := TBookmarks.Create(Self);
  FCodeFolding := TCodeFolding.Create(Self);
  FMarks := TMarks.Create(Self);
  FLineState := TLineState.Create(Self);
  FLineNumbers := TLineNumbers.Create(Self);
  FOnChange := nil;
end;

destructor TBCEditorLeftMargin.Destroy();
begin
  FBookmarks.Free();
  FCodeFolding.Free();
  FMarks.Free();
  FLineState.Free();
  FLineNumbers.Free();

  inherited;
end;

procedure TBCEditorLeftMargin.DoChange();
begin
  if (Assigned(FOnChange)) then
    FOnChange(Self);
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

{ TBCEditorTabs ***************************************************************}

procedure TBCEditorTabs.Assign(ASource: TPersistent);
begin
  Assert(ASource is TBCEditorTabs);

  inherited;

  FOptions := TBCEditorTabs(ASource).FOptions;
  FWidth := TBCEditorTabs(ASource).FWidth;

  DoChange();
end;

constructor TBCEditorTabs.Create();
begin
  inherited;

  FOptions := DefaultOptions;
  FWidth := DefaultWidth;
end;

procedure TBCEditorTabs.DoChange();
begin
  if (Assigned(FOnChange)) then
    FOnChange(Self);
end;

procedure TBCEditorTabs.SetOptions(const AValue: TBCEditorTabOptions);
begin
  if (FOptions <> AValue) then
  begin
    FOptions := AValue;
    DoChange();
  end;
end;

procedure TBCEditorTabs.SetWidth(const AValue: Integer);
begin
  if ((FWidth <> AValue) and (1 <= AValue) and (AValue < 256)) then
  begin
    FWidth := AValue;
    DoChange();
  end;
end;

end.

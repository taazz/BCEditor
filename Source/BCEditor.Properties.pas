unit BCEditor.Properties;

interface {********************************************************************}

uses
  Classes, UITypes, Generics.Collections, RegularExpressions,
  Graphics, Controls, ImgList,
  BCEditor.Consts, BCEditor.Types;

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

  TBCEditorCompletionProposalCloseEvent = procedure(Sender: TObject; var ASelectedItem: string) of object;
  TBCEditorCompletionProposalPopupWindowValidateEvent = procedure(ASender: TObject; Shift: TShiftState; EndToken: Char) of object;

  TBCEditorCompletionProposalItems = class(TCollection)
    type

      TItem = class(TCollectionItem)
      strict private
        FImageIndex: Integer;
        FValue: string;
      public
        constructor Create(ACollection: TCollection); override;
        procedure Assign(ASource: TPersistent); override;
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
          constructor Create;
          procedure Assign(ASource: TPersistent); override;
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
        constructor Create;
        destructor Destroy; override;
        procedure Assign(ASource: TPersistent); override;
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
      constructor Create(ACollection: TCollection); override;
      destructor Destroy; override;
      procedure Assign(ASource: TPersistent); override;
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
    TColors = class(TPersistent)
    strict private
      FBackground: TColor;
      FForeground: TColor;
      FSelectedBackground: TColor;
      FSelectedText: TColor;
    public
      constructor Create;
      procedure Assign(ASource: TPersistent); override;
    published
      property Background: TColor read FBackground write FBackground default clWindow;
      property Foreground: TColor read FForeground write FForeground default clWindowText;
      property SelectedBackground: TColor read FSelectedBackground write FSelectedBackground default clHighlight;
      property SelectedText: TColor read FSelectedText write FSelectedText default clHighlightText;
    end;

    TTrigger = class(TPersistent)
    strict private
      FChars: string;
      FEnabled: Boolean;
      FInterval: Integer;
    public
      constructor Create;
      procedure Assign(ASource: TPersistent); override;
    published
      property Chars: string read FChars write FChars;
      property Enabled: Boolean read FEnabled write FEnabled default False;
      property Interval: Integer read FInterval write FInterval default 1000;
    end;

  strict private const
    DefaultCloseChars = '()[]. ';
    DefaultOptions = [cpoAutoConstraints, cpoAddHighlighterKeywords, cpoFiltered,
      cpoParseItemsFromText, cpoUseHighlighterColumnFont];
    DefaultSecondaryShortCut = 0;
    DefaultShortCut = 16416; // (Ctrl+Space)
  strict private
    FCloseChars: string;
    FColors: TBCEditorCompletionProposal.TColors;
    FColumns: TBCEditorCompletionProposalColumns;
    FCompletionColumnIndex: Integer;
    FConstraints: TSizeConstraints;
    FEnabled: Boolean;
    FImages: TCustomImageList;
    FOptions: TBCEditorCompletionProposalOptions;
    FOwner: TComponent;
    FSecondaryShortCut: TShortCut;
    FShortCut: TShortCut;
    FTrigger: TBCEditorCompletionProposal.TTrigger;
    FVisibleLines: Integer;
    FWidth: Integer;
    procedure SetImages(const AValue: TCustomImageList);
  protected
    function GetOwner: TPersistent; override;
  public
    constructor Create(AOwner: TComponent);
    destructor Destroy; override;
    procedure Assign(ASource: TPersistent); override;
    procedure ChangeScale(M, D: Integer);
    procedure SetOption(const AOption: TBCEditorCompletionProposalOption; const AEnabled: Boolean);
  published
    property CloseChars: string read FCloseChars write FCloseChars;
    property Colors: TBCEditorCompletionProposal.TColors read FColors write FColors;
    property Columns: TBCEditorCompletionProposalColumns read FColumns write FColumns;
    property CompletionColumnIndex: Integer read FCompletionColumnIndex write FCompletionColumnIndex default 0;
    property Constraints: TSizeConstraints read FConstraints write FConstraints;
    property Enabled: Boolean read FEnabled write FEnabled default True;
    property Images: TCustomImageList read FImages write SetImages;
    property Options: TBCEditorCompletionProposalOptions read FOptions write FOptions default DefaultOptions;
    property SecondaryShortCut: TShortCut read FSecondaryShortCut write FSecondaryShortCut default DefaultSecondaryShortCut;
    property ShortCut: TShortCut read FShortCut write FShortCut default DefaultShortCut;
    property Trigger: TBCEditorCompletionProposal.TTrigger read FTrigger write FTrigger;
    property VisibleLines: Integer read FVisibleLines write FVisibleLines default 8;
    property Width: Integer read FWidth write FWidth default 260;
  end;

  TBCEditorLeftMargin = class(TPersistent)
  type
    TColors = class(TPersistent)
    strict private
      FBackground: TColor;
      FBookmarkPanelBackground: TColor;
      FForeground: TColor;
      FLineStateModified: TColor;
      FLineStateLoaded: TColor;
    public
      constructor Create();
      procedure Assign(ASource: TPersistent); override;
    published
      property Background: TColor read FBackground write FBackground default clLeftMarginBackground;
      property BookmarkPanelBackground: TColor read FBookmarkPanelBackground write FBookmarkPanelBackground default clBtnFace;
      property Foreground: TColor read FForeground write FForeground default clLeftMarginForeground;
      property LineStateModified: TColor read FLineStateModified write FLineStateModified default clYellow;
      property LineStateLoaded: TColor read FLineStateLoaded write FLineStateLoaded default clLime;
    end;

    TBookMarks = class(TPersistent)
    strict private
      FOnChange: TNotifyEvent;
      FOwner: TComponent;
      FShortCuts: Boolean;
      FVisible: Boolean;
      procedure DoChange;
      procedure SetVisible(AValue: Boolean);
    public
      constructor Create(AOwner: TComponent);
      procedure Assign(ASource: TPersistent); override;
    published
      property ShortCuts: Boolean read FShortCuts write FShortCuts default True;
      property Visible: Boolean read FVisible write SetVisible default True;
      property OnChange: TNotifyEvent read FOnChange write FOnChange;
    end;

    TMarks = class(TPersistent)
    strict private
      FDefaultImageIndex: Integer;
      FImages: TCustomImageList;
      FOnChange: TNotifyEvent;
      FOwner: TComponent;
      FShortCuts: Boolean;
      FVisible: Boolean;
      procedure DoChange;
      procedure SetImages(const AValue: TCustomImageList);
      procedure SetVisible(AValue: Boolean);
    protected
      property OnChange: TNotifyEvent read FOnChange write FOnChange;
    public
      constructor Create(AOwner: TComponent);
      procedure Assign(ASource: TPersistent); override;
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
      FOnChange: TNotifyEvent;
      FOptions: TBCEditorLeftMarginLineNumberOptions;
      FStartFrom: Integer;
      FVisible: Boolean;
      procedure DoChange;
      procedure SetOptions(const AValue: TBCEditorLeftMarginLineNumberOptions);
      procedure SetStartFrom(const AValue: Integer);
      procedure SetVisible(const AValue: Boolean);
    protected
      property OnChange: TNotifyEvent read FOnChange write FOnChange;
    public
      constructor Create();
      procedure Assign(ASource: TPersistent); override;
      procedure SetOption(const AOption: TBCEditorLeftMarginLineNumberOption; const AEnabled: Boolean);
    published
      property Options: TBCEditorLeftMarginLineNumberOptions read FOptions write SetOptions default DefaultOptions;
      property StartFrom: Integer read FStartFrom write SetStartFrom default DefaultStartFrom;
      property Visible: Boolean read FVisible write SetVisible default DefaultVisible;
    end;

    TLineState = class(TPersistent)
    strict private const
      DefaultVisible = True;
    strict private
      FOnChange: TNotifyEvent;
      FVisible: Boolean;
      procedure DoChange;
      procedure SetOnChange(AValue: TNotifyEvent);
      procedure SetVisible(const AValue: Boolean);
    protected
      property OnChange: TNotifyEvent read FOnChange write SetOnChange;
    public
      constructor Create();
      procedure Assign(ASource: TPersistent); override;
    published
      property Visible: Boolean read FVisible write SetVisible default DefaultVisible;
    end;

  strict private
    FBookMarks: TBookmarks;
    FColors: TColors;
    FLineNumbers: TLineNumbers;
    FLineState: TLineState;
    FMarks: TMarks;
    FOnChange: TNotifyEvent;
    procedure DoChange();
    procedure SetBookMarks(const AValue: TBookmarks);
    procedure SetColors(const AValue: TColors);
    procedure SetMarks(const AValue: TMarks);
    procedure SetOnChange(AValue: TNotifyEvent);
  protected
    property OnChange: TNotifyEvent read FOnChange write SetOnChange;
  public
    constructor Create(AOwner: TComponent);
    destructor Destroy; override;
    procedure Assign(ASource: TPersistent); override;
  published
    property Bookmarks: TBCEditorLeftMargin.TBookmarks read FBookMarks write SetBookMarks;
    property Colors: TColors read FColors write SetColors;
    property LineNumbers: TLineNumbers read FLineNumbers write FLineNumbers;
    property LineState: TLineState read FLineState write FLineState;
    property Marks: TMarks read FMarks write SetMarks;
  end;

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

  TBCEditorReplace = class(TPersistent)
  strict private const
    DefaultOptions = [];
  strict private
    FEngine: TBCEditorSearchEngine;
    FOnChange: TNotifyEvent;
    FOptions: TBCEditorReplaceOptions;
    FPattern: string;
    FReplaceText: string;
    procedure SetEngine(const AValue: TBCEditorSearchEngine);
    procedure SetOptions(const AValue: TBCEditorReplaceOptions);
    procedure DoChange();
  protected
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  public
    procedure Assign(ASource: TPersistent); override;
    constructor Create();
    property Pattern: string read FPattern write FPattern;
    property ReplaceText: string read FReplaceText write FReplaceText;
  published
    property Engine: TBCEditorSearchEngine read FEngine write SetEngine default seNormal;
    property Options: TBCEditorReplaceOptions read FOptions write SetOptions default DefaultOptions;
  end;

  TBCEditorSearch = class(TPersistent)
  public type
    TWrapAroundEvent = function(ASender: TObject; const APattern: string; const ABackwards: Boolean): Boolean of object;

    THighlighter = class(TPersistent)
    type

      TColors = class(TPersistent)
      strict private
        FBackground: TColor;
        FForeground: TColor;
        FOnChange: TNotifyEvent;
        procedure DoChange;
        procedure SetBackground(const AValue: TColor);
        procedure SetForeground(const AValue: TColor);
      public
        constructor Create;
        procedure Assign(ASource: TPersistent); override;
      published
        property Background: TColor read FBackground write SetBackground default clSearchHighlighter;
        property Foreground: TColor read FForeground write SetForeground default clWindowText;
        property OnChange: TNotifyEvent read FOnChange write FOnChange;
      end;

    strict private
      FColors: TColors;
      FOnChange: TNotifyEvent;
      procedure DoChange;
      procedure SetColors(const AValue: TColors);
      procedure SetOnChange(AValue: TNotifyEvent);
    public
      constructor Create;
      destructor Destroy; override;
      procedure Assign(ASource: TPersistent); override;
    published
      property Colors: TColors read FColors write SetColors;
      property OnChange: TNotifyEvent read FOnChange write SetOnChange;
    end;

  strict private const
    DefaultOptions = [];
  strict private
    FEngineType: TBCEditorSearchEngine;
    FHighlighter: THighlighter;
    FOnChange: TNotifyEvent;
    FOnWrapAround: TWrapAroundEvent;
    FOptions: TBCEditorSearchOptions;
    FPattern: string;
    FVisible: Boolean;
    procedure SetEngineType(const AValue: TBCEditorSearchEngine);
    procedure SetHighlighter(const AValue: THighlighter);
    procedure SetOnChange(const AValue: TNotifyEvent);
    procedure SetPattern(const AValue: string);
  protected
    property OnChange: TNotifyEvent read FOnChange write SetOnChange;
  public
    procedure Assign(ASource: TPersistent); override;
    constructor Create();
    destructor Destroy(); override;
    property Pattern: string read FPattern write SetPattern;
    property Visible: Boolean read FVisible write FVisible;
  published
    property Engine: TBCEditorSearchEngine read FEngineType write SetEngineType default seNormal;
    property Highlighter: THighlighter read FHighlighter write SetHighlighter;
    property OnWrapAround: TWrapAroundEvent read FOnWrapAround write FOnWrapAround;
    property Options: TBCEditorSearchOptions read FOptions write FOptions default DefaultOptions;
  end;

  TBCEditorSelection = class(TPersistent)
  type
    TColors = class(TPersistent)
    strict private
      FBackground: TColor;
      FForeground: TColor;
      FOnChange: TNotifyEvent;
      procedure DoChange();
      procedure SetBackground(AValue: TColor);
      procedure SetForeground(AValue: TColor);
    protected
      property OnChange: TNotifyEvent read FOnChange write FOnChange;
    public
      procedure Assign(ASource: TPersistent); override;
      constructor Create();
    published
      property Background: TColor read FBackground write SetBackground default clSelectionColor;
      property Foreground: TColor read FForeground write SetForeground default clHighLightText;
    end;

  strict private const
    DefaultOptions = [soHighlightWholeLine, soTripleClickLineSelect];
  strict private
    FColors: TColors;
    FOnChange: TNotifyEvent;
    FOptions: TBCEditorSelectionOptions;
    procedure DoChange();
    procedure SetColors(const AValue: TColors);
    procedure SetOptions(AValue: TBCEditorSelectionOptions);
  protected
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  public
    procedure Assign(ASource: TPersistent); override;
    constructor Create();
    destructor Destroy; override;
  published
    property Colors: TColors read FColors write SetColors;
    property Options: TBCEditorSelectionOptions read FOptions write SetOptions default DefaultOptions;
  end;

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
    constructor Create;
    procedure Assign(ASource: TPersistent); override;
  published
    property Options: TBCEditorTabOptions read FOptions write SetOptions default DefaultOptions;
    property Width: Integer read FWidth write SetWidth default DefaultWidth;
  end;

implementation  {**************************************************************}

uses
  Windows,
  Math, SysUtils, Character,
  Menus;

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

{ TBCEditorCompletionProposalItems.TItem **************************************}

constructor TBCEditorCompletionProposalItems.TItem.Create(ACollection: TCollection);
begin
  inherited;

  FImageIndex := -1;
end;

procedure TBCEditorCompletionProposalItems.TItem.Assign(ASource: TPersistent);
begin
  if ASource is TBCEditorCompletionProposalItems.TItem then
  with ASource as TBCEditorCompletionProposalItems.TItem do
  begin
    Self.FImageIndex := FImageIndex;
    Self.FValue := FValue;
  end
  else
    inherited Assign(ASource);
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

constructor TBCEditorCompletionProposalColumns.TColumn.TTitle.TColors.Create;
begin
  inherited;

  FBackground := clWindow;
  FBottomBorder := clBtnFace;
  FRightBorder := clBtnFace;
end;

procedure TBCEditorCompletionProposalColumns.TColumn.TTitle.TColors.Assign(ASource: TPersistent);
begin
  if ASource is TBCEditorCompletionProposalColumns.TColumn.TTitle.TColors then
  with ASource as TBCEditorCompletionProposalColumns.TColumn.TTitle.TColors do
  begin
    Self.FBackground := FBackground;
    Self.FBottomBorder := FBottomBorder;
    Self.FRightBorder := FRightBorder;
  end
  else
    inherited Assign(ASource);
end;

{ TBCEditorCompletionProposalColumns.TColumn.TTitle *************************}

constructor TBCEditorCompletionProposalColumns.TColumn.TTitle.Create;
begin
  inherited;

  FColors := TColors.Create;
  FFont := TFont.Create;
  FFont.Name := 'Courier New';
  FFont.Size := 8;
  FVisible := False;
end;

destructor TBCEditorCompletionProposalColumns.TColumn.TTitle.Destroy;
begin
  FColors.Free;
  FFont.Free;

  inherited;
end;

procedure TBCEditorCompletionProposalColumns.TColumn.TTitle.Assign(ASource: TPersistent);
begin
  if ASource is TTitle then
  with ASource as TTitle do
  begin
    Self.FCaption := FCaption;
    Self.FColors.Assign(FColors);
    Self.FFont.Assign(FFont);
    Self.FVisible := FVisible;
  end
  else
    inherited Assign(ASource);
end;

procedure TBCEditorCompletionProposalColumns.TColumn.TTitle.SetFont(const AValue: TFont);
begin
  FFont.Assign(AValue);
end;

{ TBCEditorCompletionProposalColumns.TColumn ********************************}

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

destructor TBCEditorCompletionProposalColumns.TColumn.Destroy;
begin
  FFont.Free;
  FItems.Free;
  FTitle.Free;

  inherited;
end;

procedure TBCEditorCompletionProposalColumns.TColumn.Assign(ASource: TPersistent);
begin
  if ASource is TColumn then
  with ASource as TColumn do
  begin
    Self.FAutoWidth := FAutoWidth;
    Self.FFont.Assign(FFont);
    Self.FItems.Assign(FItems);
    Self.FTitle.Assign(FTitle);
    Self.FWidth := FWidth;
  end
  else
    inherited Assign(ASource);
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

{ TBCEditorCompletionProposal.TColors *****************************************}

constructor TBCEditorCompletionProposal.TColors.Create;
begin
  inherited;

  FBackground := clWindow;
  FForeground := clWindowText;
  FSelectedBackground := clHighlight;
  FSelectedText := clHighlightText;
end;

procedure TBCEditorCompletionProposal.TColors.Assign(ASource: TPersistent);
begin
  if ASource is TBCEditorCompletionProposal.TColors then
  with ASource as TBCEditorCompletionProposal.TColors do
  begin
    Self.FBackground := FBackground;
    Self.FForeground := FForeground;
    Self.FSelectedBackground := FSelectedBackground;
    Self.FSelectedText := FSelectedText;
  end
  else
    inherited Assign(ASource);
end;

{ TBCEditorCompletionProposal.TTrigger ****************************************}

constructor TBCEditorCompletionProposal.TTrigger.Create;
begin
  inherited;

  FChars := '.';
  FEnabled := False;
  FInterval := 1000;
end;

procedure TBCEditorCompletionProposal.TTrigger.Assign(ASource: TPersistent);
begin
  if ASource is TBCEditorCompletionProposal.TTrigger then
  with ASource as TBCEditorCompletionProposal.TTrigger do
  begin
    Self.FChars := FChars;
    Self.FEnabled := FEnabled;
    Self.FInterval := FInterval;
  end
  else
    inherited Assign(ASource);
end;

{ TBCEditorCompletionProposal *************************************************}

constructor TBCEditorCompletionProposal.Create(AOwner: TComponent);
begin
  inherited Create;

  FOwner := AOwner;
  FCloseChars := DefaultCloseChars;
  FColors := TColors.Create;
  FColumns := TBCEditorCompletionProposalColumns.Create(Self);
  FColumns.Add; { default column }
  FCompletionColumnIndex := 0;
  FEnabled := True;
  FOptions := DefaultOptions;
  FSecondaryShortCut := DefaultSecondaryShortCut;
  FShortCut := DefaultShortCut;
  FTrigger := TTrigger.Create;
  FVisibleLines := 8;
  FWidth := 260;
  FConstraints := TSizeConstraints.Create(nil);
end;

destructor TBCEditorCompletionProposal.Destroy;
begin
  FColors.Free;
  FTrigger.Free;
  FColumns.Free;
  FConstraints.Free;

  inherited;
end;

procedure TBCEditorCompletionProposal.Assign(ASource: TPersistent);
begin
  if ASource is TBCEditorCompletionProposal then
  with ASource as TBCEditorCompletionProposal do
  begin
    Self.FCloseChars := FCloseChars;
    Self.FColors.Assign(FColors);
    Self.FColumns.Assign(FColumns);
    Self.FEnabled := FEnabled;
    Self.FImages := FImages;
    Self.FOptions := FOptions;
    Self.FSecondaryShortCut := FSecondaryShortCut;
    Self.FShortCut := FShortCut;
    Self.FTrigger.Assign(FTrigger);
    Self.FVisibleLines := FVisibleLines;
    Self.FWidth := FWidth;
  end
  else
    inherited Assign(ASource);
end;

procedure TBCEditorCompletionProposal.ChangeScale(M, D: Integer);
begin
  FWidth := FWidth * M div D;
end;

function TBCEditorCompletionProposal.GetOwner: TPersistent;
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

procedure TBCEditorCompletionProposal.SetOption(const AOption: TBCEditorCompletionProposalOption; const AEnabled: Boolean);
begin
  if AEnabled then
    Include(FOptions, AOption)
  else
    Exclude(FOptions, AOption);
end;

{ TBCEditorLeftMargin.TColors *************************************************}

procedure TBCEditorLeftMargin.TColors.Assign(ASource: TPersistent);
begin
  if ASource is TBCEditorLeftMargin.TColors then
  with ASource as TBCEditorLeftMargin.TColors do
  begin
    Self.FBackground := FBackground;
    Self.FBookmarkPanelBackground := FBookmarkPanelBackground;
    Self.FLineStateModified := FLineStateModified;
    Self.FLineStateLoaded := FLineStateLoaded;
  end
  else
    inherited Assign(ASource);
end;

constructor TBCEditorLeftMargin.TColors.Create();
begin
  inherited;

  FBackground := clLeftMarginBackground;
  FBookmarkPanelBackground := clBtnFace;
  FForeground := clLeftMarginForeground;
  FLineStateModified := clYellow;
  FLineStateLoaded := clLime;
end;

{ TBCEditorLeftMargin.TBookmarks **********************************************}

constructor TBCEditorLeftMargin.TBookmarks.Create(AOwner: TComponent);
begin
  inherited Create;

  FOwner := AOwner;
  FShortCuts := True;
  FVisible := True;
end;

procedure TBCEditorLeftMargin.TBookmarks.Assign(ASource: TPersistent);
begin
  if Assigned(ASource) and (ASource is TBCEditorLeftMargin.TBookmarks) then
  with ASource as TBCEditorLeftMargin.TBookmarks do
  begin
    Self.FShortCuts := FShortCuts;
    Self.FVisible := FVisible;
    if Assigned(Self.FOnChange) then
      Self.FOnChange(Self);
  end
  else
    inherited Assign(ASource);
end;

procedure TBCEditorLeftMargin.TBookmarks.DoChange;
begin
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

procedure TBCEditorLeftMargin.TBookmarks.SetVisible(AValue: Boolean);
begin
  if FVisible <> AValue then
  begin
    FVisible := AValue;
    DoChange;
  end;
end;

{ TBCEditorLeftMargin.TMarks **************************************************}

constructor TBCEditorLeftMargin.TMarks.Create(AOwner: TComponent);
begin
  inherited Create();

  FOwner := AOwner;
  FDefaultImageIndex := -1;
  FShortCuts := True;
  FVisible := False;
end;

procedure TBCEditorLeftMargin.TMarks.Assign(ASource: TPersistent);
begin
  if Assigned(ASource) and (ASource is TBCEditorLeftMargin.TMarks) then
  with ASource as TBCEditorLeftMargin.TMarks do
  begin
    Self.FDefaultImageIndex := FDefaultImageIndex;
    Self.FImages := FImages;
    Self.FShortCuts := FShortCuts;
    Self.FVisible := FVisible;
    if Assigned(Self.FOnChange) then
      Self.FOnChange(Self);
  end
  else
    inherited Assign(ASource);
end;

procedure TBCEditorLeftMargin.TMarks.DoChange;
begin
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

procedure TBCEditorLeftMargin.TMarks.SetImages(const AValue: TCustomImageList);
begin
  if (AValue <> FImages) then
  begin
    FImages := AValue;
    if Assigned(FImages) then
      FImages.FreeNotification(FOwner);
    DoChange();
  end;
end;

procedure TBCEditorLeftMargin.TMarks.SetVisible(AValue: Boolean);
begin
  if FVisible <> AValue then
  begin
    FVisible := AValue;
    DoChange;
  end;
end;

{ TBCEditorLeftMargin.TLineNumbers ********************************************}

procedure TBCEditorLeftMargin.TLineNumbers.Assign(ASource: TPersistent);
begin
  if Assigned(ASource) and (ASource is TBCEditorLeftMargin.TLineNumbers) then
  with ASource as TBCEditorLeftMargin.TLineNumbers do
  begin
    Self.FOptions := FOptions;
    Self.FStartFrom := FStartFrom;
    Self.FVisible := FVisible;

    Self.DoChange;
  end
  else
    inherited Assign(ASource);
end;

constructor TBCEditorLeftMargin.TLineNumbers.Create();
begin
  inherited;

  FOptions := DefaultOptions;
  FStartFrom := DefaultStartFrom;
  FVisible := DefaultVisible;
end;

procedure TBCEditorLeftMargin.TLineNumbers.DoChange;
begin
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

procedure TBCEditorLeftMargin.TLineNumbers.SetOption(const AOption: TBCEditorLeftMarginLineNumberOption; const AEnabled: Boolean);
begin
  if AEnabled then
    Include(FOptions, AOption)
  else
    Exclude(FOptions, AOption);
end;

procedure TBCEditorLeftMargin.TLineNumbers.SetOptions(const AValue: TBCEditorLeftMarginLineNumberOptions);
begin
  if FOptions <> AValue then
  begin
    FOptions := AValue;
    DoChange
  end;
end;

procedure TBCEditorLeftMargin.TLineNumbers.SetStartFrom(const AValue: Integer);
begin
  if (AValue <> FStartFrom) then
  begin
    FStartFrom := Max(0, AValue);
    DoChange();
  end;
end;

procedure TBCEditorLeftMargin.TLineNumbers.SetVisible(const AValue: Boolean);
begin
  if (AValue <> FVisible) then
  begin
    FVisible := AValue;
    DoChange();
  end;
end;

{ TBCEditorLeftMargin.TLineState **********************************************}

constructor TBCEditorLeftMargin.TLineState.Create();
begin
  inherited;

  FVisible := DefaultVisible;
end;

procedure TBCEditorLeftMargin.TLineState.Assign(ASource: TPersistent);
begin
  if Assigned(ASource) and (ASource is TBCEditorLeftMargin.TLineState) then
  with ASource as TBCEditorLeftMargin.TLineState do
  begin
    Self.FVisible := FVisible;
    Self.DoChange;
  end
  else
    inherited Assign(ASource);
end;

procedure TBCEditorLeftMargin.TLineState.DoChange;
begin
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

procedure TBCEditorLeftMargin.TLineState.SetVisible(const AValue: Boolean);
begin
  if FVisible <> AValue then
  begin
    FVisible := AValue;
    DoChange
  end;
end;

procedure TBCEditorLeftMargin.TLineState.SetOnChange(AValue: TNotifyEvent);
begin
  FOnChange := AValue;
end;

{ TBCEditorLeftMargin *********************************************************}

constructor TBCEditorLeftMargin.Create(AOwner: TComponent);
begin
  inherited Create;

  FColors := TColors.Create;

  FBookmarks := TBookmarks.Create(AOwner);
  FMarks := TMarks.Create(AOwner);
  FLineState := TLineState.Create;
  FLineNumbers := TLineNumbers.Create;
  FOnChange := nil;
end;

destructor TBCEditorLeftMargin.Destroy();
begin
  FBookmarks.Free();
  FMarks.Free();
  FColors.Free();
  FLineState.Free();
  FLineNumbers.Free();

  inherited;
end;

procedure TBCEditorLeftMargin.Assign(ASource: TPersistent);
begin
  if ASource is TBCEditorLeftMargin then
  with ASource as TBCEditorLeftMargin do
  begin
    Self.FBookmarks.Assign(FBookmarks);
    Self.FMarks.Assign(FMarks);
    Self.FColors.Assign(FColors);
    Self.FLineNumbers.Assign(FLineNumbers);
    Self.DoChange;
  end
  else
    inherited Assign(ASource);
end;

procedure TBCEditorLeftMargin.DoChange();
begin
  if (Assigned(FOnChange)) then
    FOnChange(Self);
end;

procedure TBCEditorLeftMargin.SetBookMarks(const AValue: TBookmarks);
begin
  FBookmarks.Assign(AValue);
end;

procedure TBCEditorLeftMargin.SetColors(const AValue: TColors);
begin
  FColors.Assign(AValue);
end;

procedure TBCEditorLeftMargin.SetMarks(const AValue: TMarks);
begin
  FMarks.Assign(AValue);
end;

procedure TBCEditorLeftMargin.SetOnChange(AValue: TNotifyEvent);
begin
  FOnChange := AValue;

  FBookmarks.OnChange := AValue;
  FLineState.OnChange := AValue;
  FLineNumbers.OnChange := AValue;
  FMarks.OnChange := AValue;
end;

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

{ TBCEditorReplace ************************************************************}

procedure TBCEditorReplace.Assign(ASource: TPersistent);
begin
  if Assigned(ASource) and (ASource is TBCEditorReplace) then
  with ASource as TBCEditorReplace do
  begin
    Self.FEngine := Engine;
    Self.FOptions := Options;
  end
  else
    inherited Assign(ASource);
end;

constructor TBCEditorReplace.Create();
begin
  inherited;

  FEngine := seNormal;
  FOptions := DefaultOptions;
end;

procedure TBCEditorReplace.DoChange();
begin
  if (Assigned(FOnChange)) then
    FOnChange(Self);
end;

procedure TBCEditorReplace.SetOptions(const AValue: TBCEditorReplaceOptions);
begin
  if (AValue <> FOptions) then
  begin
    FOptions := AValue;
    DoChange();
  end;
end;

procedure TBCEditorReplace.SetEngine(const AValue: TBCEditorSearchEngine);
begin
  if FEngine <> AValue then
  begin
    FEngine := AValue;
    DoChange();
  end;
end;

{ TBCEditorSearch.THighlighter.TColors ****************************************}

constructor TBCEditorSearch.THighlighter.TColors.Create;
begin
  inherited;

  FBackground := clSearchHighlighter;
  FForeground := clWindowText;
end;

procedure TBCEditorSearch.THighlighter.TColors.Assign(ASource: TPersistent);
begin
  if ASource is TColors then
  with ASource as TColors do
  begin
    Self.FBackground := FBackground;
    Self.FForeground := FForeground;
    Self.DoChange;
  end
  else
    inherited Assign(ASource);
end;

procedure TBCEditorSearch.THighlighter.TColors.DoChange;
begin
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

procedure TBCEditorSearch.THighlighter.TColors.SetBackground(const AValue: TColor);
begin
  if FBackground <> AValue then
  begin
    FBackground := AValue;
    DoChange;
  end;
end;

procedure TBCEditorSearch.THighlighter.TColors.SetForeground(const AValue: TColor);
begin
  if FForeground <> AValue then
  begin
    FForeground := AValue;
    DoChange;
  end;
end;

{ TBCEditorSearch.THighlighter **************************************************}

constructor TBCEditorSearch.THighlighter.Create;
begin
  inherited;

  FColors := TColors.Create;
end;

destructor TBCEditorSearch.THighlighter.Destroy;
begin
  FColors.Free;
  inherited;
end;

procedure TBCEditorSearch.THighlighter.Assign(ASource: TPersistent);
begin
  if Assigned(ASource) and (ASource is THighlighter) then
  with ASource as THighlighter do
  begin
    Self.FColors.Assign(Colors);
    Self.DoChange;
  end
  else
    inherited Assign(ASource);
end;

procedure TBCEditorSearch.THighlighter.DoChange;
begin
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

procedure TBCEditorSearch.THighlighter.SetColors(const AValue: TColors);
begin
  FColors.Assign(AValue);
end;

procedure TBCEditorSearch.THighlighter.SetOnChange(AValue: TNotifyEvent);
begin
  FOnChange := AValue;
  FColors.OnChange := FOnChange;
end;

{ TBCEditorSearch *************************************************************}

procedure TBCEditorSearch.Assign(ASource: TPersistent);
begin
  if Assigned(ASource) and (ASource is TBCEditorSearch) then
  with ASource as TBCEditorSearch do
  begin
    Self.FEngineType := FEngineType;
    Self.FHighlighter.Assign(FHighlighter);
    Self.FOptions := FOptions;
    if (Assigned(FOnChange)) then
      FOnChange(Self);
  end
  else
    inherited Assign(ASource);
end;

constructor TBCEditorSearch.Create();
begin
  inherited;

  FEngineType := seNormal;
  FHighlighter := THighlighter.Create;
  FOptions := DefaultOptions;
end;

destructor TBCEditorSearch.Destroy();
begin
  FHighlighter.Free();

  inherited;
end;

procedure TBCEditorSearch.SetEngineType(const AValue: TBCEditorSearchEngine);
begin
  if (FEngineType <> AValue) then
  begin
    FEngineType := AValue;
    if (Assigned(FOnChange)) then
      FOnChange(Self);
  end;
end;

procedure TBCEditorSearch.SetHighlighter(const AValue: THighlighter);
begin
  FHighlighter.Assign(AValue);
end;

procedure TBCEditorSearch.SetOnChange(const AValue: TNotifyEvent);
begin
  FOnChange := AValue;
  FHighlighter.OnChange := FOnChange;
end;

procedure TBCEditorSearch.SetPattern(const AValue: string);
begin
  if (AValue <> FPattern) then
  begin
    FPattern := AValue;
    if (Assigned(FOnChange)) then
      FOnChange(Self);
  end;
end;

{ TBCEditorSelection.TColors **************************************************}

procedure TBCEditorSelection.TColors.Assign(ASource: TPersistent);
begin
  if Assigned(ASource) and (ASource is TBCEditorSelection.TColors) then
    with ASource as TBCEditorSelection.TColors do
    begin
      Self.FBackground := FBackground;
      Self.FForeground := FForeground;
      DoChange();
    end
  else
    inherited Assign(ASource);
end;

constructor TBCEditorSelection.TColors.Create();
begin
  inherited;

  FBackground := clSelectionColor;
  FForeground := clHighLightText;
end;

procedure TBCEditorSelection.TColors.DoChange();
begin
  if (Assigned(FOnChange)) then
    FOnChange(Self);
end;

procedure TBCEditorSelection.TColors.SetBackground(AValue: TColor);
begin
  if FBackground <> AValue then
  begin
    FBackground := AValue;
    DoChange();
  end;
end;

procedure TBCEditorSelection.TColors.SetForeground(AValue: TColor);
begin
  if FForeground <> AValue then
  begin
    FForeground := AValue;
    DoChange();
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

procedure TBCEditorSelection.DoChange();
begin
  if (Assigned(FOnChange)) then
    FOnChange(Self);
end;

procedure TBCEditorSelection.SetColors(const AValue: TBCEditorSelection.TColors);
begin
  FColors.Assign(AValue);
  DoChange();
end;

procedure TBCEditorSelection.SetOptions(AValue: TBCEditorSelectionOptions);
begin
  if FOptions <> AValue then
  begin
    FOptions := AValue;
    DoChange();
  end;
end;

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

{ TBCEditorTabs ***************************************************************}

constructor TBCEditorTabs.Create;
begin
  inherited;

  FOptions := DefaultOptions;
  FWidth := DefaultWidth;
end;

procedure TBCEditorTabs.Assign(ASource: TPersistent);
begin
  if (ASource is TBCEditorTabs) then
  begin
    FOptions := TBCEditorTabs(ASource).FOptions;
    FWidth := TBCEditorTabs(ASource).FWidth;
    DoChange();
  end
  else
    inherited Assign(ASource);
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

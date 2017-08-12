unit BCEditor.Properties;

interface {********************************************************************}

uses
  Classes, UITypes, Generics.Collections, RegularExpressions,
  Graphics, Controls, ImgList,
  BCEditor.Consts, BCEditor.Types;

type
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
    destructor Destroy; override;
    procedure Assign(ASource: TPersistent); override;
    procedure ChangeScale(M, D: Integer);
    procedure SetOption(const AOption: TBCEditorCompletionProposalOption; const AEnabled: Boolean);
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
      property Background: TColor read FBackground write SetBackground default clLeftMarginBackground;
      property Foreground: TColor read FForeground write SetForeground default clLeftMarginForeground;
    end;

    TCurrentLine = class(TPersistent)
    private
      FBackground: TColor;
      FColors: TBCEditorColors;
      procedure SetBackground(AValue: TColor);
    public
      procedure Assign(ASource: TPersistent); override;
      constructor Create(const AColors: TBCEditorColors);
    published
      property Background: TColor read FBackground write SetBackground default clActiveLineBackground;
    end;

    TFoundText = class(TPersistent)
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
      property Background: TColor read FBackground write SetBackground default clSearchHighlighter;
      property Foreground: TColor read FForeground write SetForeground default clWindowText;
    end;

    TLineNumbers = class(TPersistent)
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
      property Background: TColor read FBackground write SetBackground default clLeftMarginBackground;
      property Foreground: TColor read FForeground write SetForeground default clLeftMarginForeground;
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
    public
      procedure Assign(ASource: TPersistent); override;
      constructor Create(const AColors: TBCEditorColors);
    published
      property Modified: TColor read FModified write SetModified default clYellow;
      property Loaded: TColor read FLoaded write SetLoaded default clLeftMarginBackground;
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
    private
      FBackground: TColor;
      FColors: TBCEditorColors;
      procedure SetBackground(AValue: TColor);
    public
      procedure Assign(ASource: TPersistent); override;
      constructor Create(const AColors: TBCEditorColors);
    published
      property Background: TColor read FBackground write SetBackground default clMatchingPair;
    end;

    TSelection = class(TPersistent)
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
      property Background: TColor read FBackground write SetBackground default clSelectionColor;
      property Foreground: TColor read FForeground write SetForeground default clHighlightText;
    end;

    TSpecialChars = class(TPersistent)
    private
      FForeground: TColor;
      FColors: TBCEditorColors;
      procedure SetForeground(AValue: TColor);
    public
      procedure Assign(ASource: TPersistent); override;
      constructor Create(const AColors: TBCEditorColors);
    published
      property Foreground: TColor read FForeground write SetForeground default clSpecialChar;
    end;

    TSyncEdit = class(TPersistent)
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
      property Background: TColor read FBackground write SetBackground default clSyncEditBackground;
      property Overlays: TColor read FOverlays write SetOverlays default clHighlight;
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
      FOnChange: TNotifyEvent;
      FOwner: TComponent;
      FVisible: Boolean;
      procedure DoChange;
      procedure SetVisible(AValue: Boolean);
    public
      constructor Create(AOwner: TComponent);
      procedure Assign(ASource: TPersistent); override;
    published
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
    FLineNumbers: TLineNumbers;
    FLineState: TLineState;
    FMarks: TMarks;
    FOnChange: TNotifyEvent;
    procedure DoChange();
    procedure SetBookMarks(const AValue: TBookmarks);
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
    property LineNumbers: TLineNumbers read FLineNumbers write FLineNumbers;
    property LineState: TLineState read FLineState write FLineState;
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
  FColumns := TBCEditorCompletionProposalColumns.Create(Self);
  FColumns.Add; { default column }
  FCompletionColumnIndex := 0;
  FEnabled := True;
  FOptions := DefaultOptions;
  FTrigger := TTrigger.Create;
  FVisibleLines := 8;
  FWidth := 260;
  FConstraints := TSizeConstraints.Create(nil);
end;

destructor TBCEditorCompletionProposal.Destroy;
begin
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
    Self.FColumns.Assign(FColumns);
    Self.FEnabled := FEnabled;
    Self.FImages := FImages;
    Self.FOptions := FOptions;
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

{ TBCEditorColors.TCodeFolding ************************************************}

procedure TBCEditorColors.TCodeFolding.Assign(ASource: TPersistent);
begin
  inherited;

  FBackground := TBCEditorColors.TCodeFolding(ASource).FBackground;
  FForeground := TBCEditorColors.TCodeFolding(ASource).FForeground;

  FColors.DoChange();
end;

constructor TBCEditorColors.TCodeFolding.Create(const AColors: TBCEditorColors);
begin
  inherited Create();

  FColors := AColors;

  FBackground := clLeftMarginBackground;
  FForeground := clLeftMarginForeground;
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
  inherited;

  FBackground := TBCEditorColors.TCurrentLine(ASource).FBackground;

  FColors.DoChange();
end;

constructor TBCEditorColors.TCurrentLine.Create(const AColors: TBCEditorColors);
begin
  inherited Create();

  FColors := AColors;

  FBackground := clActiveLineBackground;
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
  inherited;

  FBackground := TBCEditorColors.TFoundText(ASource).FBackground;
  FForeground := TBCEditorColors.TFoundText(ASource).FForeground;

  FColors.DoChange();
end;

constructor TBCEditorColors.TFoundText.Create(const AColors: TBCEditorColors);
begin
  inherited Create();

  FColors := AColors;

  FBackground := clSearchHighlighter;
  FForeground := clWindowText;
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
  inherited;

  FBackground := TBCEditorColors.TLineNumbers(ASource).FBackground;
  FForeground := TBCEditorColors.TLineNumbers(ASource).FForeground;

  FColors.DoChange();
end;

constructor TBCEditorColors.TLineNumbers.Create(const AColors: TBCEditorColors);
begin
  inherited Create();

  FColors := AColors;

  FBackground := clLeftMarginBackground;
  FForeground := clLeftMarginForeground;
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
  FLoaded := clLeftMarginBackground;
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

{ TBCEditorColors.TMatchingPairs ******************************************************}

procedure TBCEditorColors.TMatchingPairs.Assign(ASource: TPersistent);
begin
  inherited;

  FBackground := TBCEditorColors.TMatchingPairs(ASource).FBackground;

  FColors.DoChange();
end;

constructor TBCEditorColors.TMatchingPairs.Create(const AColors: TBCEditorColors);
begin
  inherited Create();

  FColors := AColors;

  FBackground := clMatchingPair;
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
  inherited;

  FBackground := TBCEditorColors.TSelection(ASource).FBackground;
  FForeground := TBCEditorColors.TSelection(ASource).FForeground;

  FColors.DoChange();
end;

constructor TBCEditorColors.TSelection.Create(const AColors: TBCEditorColors);
begin
  inherited Create();

  FBackground := clSelectionColor;
  FForeground := clHighlightText;
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
  inherited;

  FForeground := TBCEditorColors.TSpecialChars(ASource).FForeground;

  FColors.DoChange();
end;

constructor TBCEditorColors.TSpecialChars.Create(const AColors: TBCEditorColors);
begin
  inherited Create();

  FColors := AColors;

  FForeground := clSpecialChar;
end;

procedure TBCEditorColors.TSpecialChars.SetForeground(AValue: TColor);
begin
  if (AValue <> FForeground) then
  begin
    FForeground := AValue;
    FColors.DoChange();
  end;
end;

{ TBCEditorColors.TSyncEdit ************************************************}

procedure TBCEditorColors.TSyncEdit.Assign(ASource: TPersistent);
begin
  inherited;

  FBackground := TBCEditorColors.TSyncEdit(ASource).FBackground;
  FOverlays := TBCEditorColors.TSyncEdit(ASource).FOverlays;

  FColors.DoChange();
end;

constructor TBCEditorColors.TSyncEdit.Create(const AColors: TBCEditorColors);
begin
  inherited Create();

  FColors := AColors;

  FBackground := clSyncEditBackground;
  FOverlays := clHighlight;
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

constructor TBCEditorLeftMargin.TBookmarks.Create(AOwner: TComponent);
begin
  inherited Create;

  FOwner := AOwner;
  FVisible := True;
end;

procedure TBCEditorLeftMargin.TBookmarks.Assign(ASource: TPersistent);
begin
  if Assigned(ASource) and (ASource is TBCEditorLeftMargin.TBookmarks) then
  with ASource as TBCEditorLeftMargin.TBookmarks do
  begin
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

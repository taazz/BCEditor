unit BCEditor.Highlighter;

interface {********************************************************************}

uses
  Classes, SysUtils, Generics.Collections,
  Controls, Graphics, StdCtrls,
  {$IF Defined(VER270)} JSON, {$ELSE} DBXJSON, {$ENDIF}
  BCEditor.Consts, BCEditor.Types;

type
  TBCEditorHighlighter = class;

  TBCEditorCodeFoldingRegionItem = class(TCollectionItem)
    strict private
      FBeginWithBreakChar: Boolean;
      FBreakCharFollows: Boolean;
      FBreakIfNotFoundBeforeNextRegion: string;
      FCloseAtNextToken: Boolean;
      FCloseToken: string;
      FCloseTokenBeginningOfLine: Boolean;
      FCloseTokenLength: Integer;
      FNoSubs: Boolean;
      FOpenIsClose: Boolean;
      FOpenToken: string;
      FOpenTokenBeginningOfLine: Boolean;
      FOpenTokenBreaksLine: Boolean;
      FOpenTokenCanBeFollowedBy: string;
      FOpenTokenEnd: string;
      FOpenTokenLength: Integer;
      FParentRegionItem: TBCEditorCodeFoldingRegionItem;
      FSharedClose: Boolean;
      FShowGuideLine: Boolean;
      FSkipIfFoundAfterOpenTokenList: TStringList;
      FTokenEndIsPreviousLine: Boolean;
    protected
      procedure LoadFromJSON(const AJSON: TJSONObject);
    public
      procedure Clear();
      constructor Create(ACollection: TCollection); override;
      destructor Destroy(); override;
      property BeginWithBreakChar: Boolean read FBeginWithBreakChar write FBeginWithBreakChar;
      property BreakCharFollows: Boolean read FBreakCharFollows write FBreakCharFollows default True;
      property BreakIfNotFoundBeforeNextRegion: string read FBreakIfNotFoundBeforeNextRegion write FBreakIfNotFoundBeforeNextRegion;
      property CloseAtNextToken: Boolean read FCloseAtNextToken write FCloseAtNextToken;
      property CloseToken: string read FCloseToken write FCloseToken;
      property CloseTokenBeginningOfLine: Boolean read FCloseTokenBeginningOfLine write FCloseTokenBeginningOfLine default False;
      property CloseTokenLength: Integer read FCloseTokenLength write FCloseTokenLength;
      property NoSubs: Boolean read FNoSubs write FNoSubs default False;
      property OpenIsClose: Boolean read FOpenIsClose write FOpenIsClose default False;
      property OpenToken: string read FOpenToken write FOpenToken;
      property OpenTokenBeginningOfLine: Boolean read FOpenTokenBeginningOfLine write FOpenTokenBeginningOfLine default False;
      property OpenTokenBreaksLine: Boolean read FOpenTokenBreaksLine write FOpenTokenBreaksLine default False;
      property OpenTokenCanBeFollowedBy: string read FOpenTokenCanBeFollowedBy write FOpenTokenCanBeFollowedBy;
      property OpenTokenEnd: string read FOpenTokenEnd write FOpenTokenEnd;
      property OpenTokenLength: Integer read FOpenTokenLength write FOpenTokenLength;
      property ParentRegionItem: TBCEditorCodeFoldingRegionItem read FParentRegionItem write FParentRegionItem;
      property SharedClose: Boolean read FSharedClose write FSharedClose default False;
      property ShowGuideLine: Boolean read FShowGuideLine write FShowGuideLine default True;
      property SkipIfFoundAfterOpenTokenList: TStringList read FSkipIfFoundAfterOpenTokenList write FSkipIfFoundAfterOpenTokenList;
      property TokenEndIsPreviousLine: Boolean read FTokenEndIsPreviousLine write FTokenEndIsPreviousLine default False;
    end;

  TBCEditorCodeFoldingAllRanges = class;
  TBCEditorCodeFoldingSkipRegions = class;

  TBCEditorCodeFoldingRegion = class(TCollection)
  strict private
    FCloseToken: string;
    FEscapeChar: Char;
    FFoldTags: Boolean;
    FHighlighter: TBCEditorHighlighter;
    FOpenToken: string;
    FSkipRegions: TBCEditorCodeFoldingSkipRegions;
    FStringEscapeChar: Char;
    function GetItem(AIndex: Integer): TBCEditorCodeFoldingRegionItem;
  protected
    procedure LoadFromJSON(const AJSON: TJSONObject);
    procedure LoadFoldRegionFromJSON(const AJSON: TJSONObject);
    procedure LoadSkipRegionFromJSON(const AJSON: TJSONObject);
  public
    function Add(const AOpenToken: string; const ACloseToken: string): TBCEditorCodeFoldingRegionItem;
    procedure Clear();
    function Contains(const AOpenToken: string; const ACloseToken: string): Boolean;
    constructor Create(const AHighlighter: TBCEditorHighlighter; const AItemClass: TCollectionItemClass);
    destructor Destroy; override;
    property CloseToken: string read FCloseToken write FCloseToken;
    property EscapeChar: Char read FEscapeChar write FEscapeChar default BCEDITOR_NONE_CHAR;
    property FoldTags: Boolean read FFoldTags write FFoldTags default False;
    property Items[AIndex: Integer]: TBCEditorCodeFoldingRegionItem read GetItem; default;
    property OpenToken: string read FOpenToken write FOpenToken;
    property SkipRegions: TBCEditorCodeFoldingSkipRegions read FSkipRegions;
    property StringEscapeChar: Char read FStringEscapeChar write FStringEscapeChar default BCEDITOR_NONE_CHAR;
  end;
  TBCEditorCodeFoldingRegions = class(TObjectList<TBCEditorCodeFoldingRegion>);

  TBCEditorCodeFoldingSkipRegions = class(TCollection)
  type

    TItem = class(TCollectionItem)
    strict private
      FCloseToken: string;
      FOpenToken: string;
      FRegionType: TBCEditorRangeItemType;
      FSkipEmptyChars: Boolean;
      FSkipIfNextCharIsNot: Char;
    public
      property OpenToken: string read FOpenToken write FOpenToken;
      property CloseToken: string read FCloseToken write FCloseToken;
      property RegionType: TBCEditorRangeItemType read FRegionType write FRegionType;
      property SkipEmptyChars: Boolean read FSkipEmptyChars write FSkipEmptyChars;
      property SkipIfNextCharIsNot: Char read FSkipIfNextCharIsNot write FSkipIfNextCharIsNot default BCEDITOR_NONE_CHAR;
    end;

  strict private
    function GetSkipRegionItem(AIndex: Integer): TItem;
  public
    function Add(const AOpenToken, ACloseToken: string): TItem;
    function Contains(const AOpenToken, ACloseToken: string): Boolean;
    property SkipRegionItems[AIndex: Integer]: TItem read GetSkipRegionItem; default;
  end;

  TBCEditorCodeFoldingRanges = class(TPersistent)
  type

    TRange = class
    strict private
      FAllCodeFoldingRanges: TBCEditorCodeFoldingAllRanges;
      FBeginLine: Integer;
      FCollapsed: Boolean;
      FCollapsedBy: Integer;
      FEndLine: Integer;
      FFoldRangeLevel: Integer;
      FIndentLevel: Integer;
      FIsExtraTokenFound: Boolean;
      FParentCollapsed: Boolean;
      FRegionItem: TBCEditorCodeFoldingRegionItem;
      FSubCodeFoldingRanges: TBCEditorCodeFoldingRanges;
      FUndoListed: Boolean;
    public
      constructor Create;
      destructor Destroy; override;
      function Collapsable: Boolean;
      procedure MoveBy(LineCount: Integer);
      procedure MoveChildren(By: Integer);
      procedure SetParentCollapsedOfSubCodeFoldingRanges(AParentCollapsed: Boolean; ACollapsedBy: Integer);
      procedure Widen(LineCount: Integer);
      property AllCodeFoldingRanges: TBCEditorCodeFoldingAllRanges read FAllCodeFoldingRanges write FAllCodeFoldingRanges;
      property BeginLine: Integer read FBeginLine write FBeginLine;
      property Collapsed: Boolean read FCollapsed write FCollapsed default False;
      property CollapsedBy: Integer read FCollapsedBy write FCollapsedBy;
      property EndLine: Integer read FEndLine write FEndLine;
      property FoldRangeLevel: Integer read FFoldRangeLevel write FFoldRangeLevel;
      property IndentLevel: Integer read FIndentLevel write FIndentLevel;
      property IsExtraTokenFound: Boolean read FIsExtraTokenFound write FIsExtraTokenFound default False;
      property ParentCollapsed: Boolean read FParentCollapsed write FParentCollapsed;
      property RegionItem: TBCEditorCodeFoldingRegionItem read FRegionItem write FRegionItem;
      property SubCodeFoldingRanges: TBCEditorCodeFoldingRanges read FSubCodeFoldingRanges;
      property UndoListed: Boolean read FUndoListed write FUndoListed default False;
    end;

  strict private
    FList: TList;
    function GetCount: Integer;
    function GetItem(AIndex: Integer): TRange;
  public
    constructor Create;
    destructor Destroy; override;
    function Add(AAllCodeFoldingRanges: TBCEditorCodeFoldingAllRanges; ABeginLine, AIndentLevel, AFoldRangeLevel: Integer;
      ARegionItem: TBCEditorCodeFoldingRegionItem; AEndLine: Integer = 0): TRange;
    procedure Clear;
    property Count: Integer read GetCount;
    property Items[AIndex: Integer]: TRange read GetItem; default;
  end;

  TBCEditorCodeFoldingAllRanges = class(TBCEditorCodeFoldingRanges)
  strict private
    FList: TObjectList<TBCEditorCodeFoldingRanges.TRange>;
    function GetAllCount: Integer;
    function GetItem(AIndex: Integer): TBCEditorCodeFoldingRanges.TRange;
    procedure SetItem(AIndex: Integer; Value: TBCEditorCodeFoldingRanges.TRange);
  public
    constructor Create;
    destructor Destroy; override;
    procedure ClearAll;
    procedure Delete(AIndex: Integer); overload;
    procedure Delete(FoldRange: TBCEditorCodeFoldingRanges.TRange); overload;
    procedure SetParentCollapsedOfSubCodeFoldingRanges(AFoldRange: TBCEditorCodeFoldingRanges.TRange);
    procedure UpdateFoldRanges;
    property AllCount: Integer read GetAllCount;
    property Items[AIndex: Integer]: TBCEditorCodeFoldingRanges.TRange read GetItem write SetItem; default;
    property List: TObjectList<TBCEditorCodeFoldingRanges.TRange> read FList;
  end;

  EBCEditorHighlighterJSON = class(Exception);

  TBCEditorHighlighter = class(TObject)
  type
    TAttribute = class;
    TToken = class;
    TRange = class;
    TDefaultParser = class;
    TDelimitersParser = class;
    TTokenNodeList = class;
    TBaseParser = class;
    TParser = class;
    TSymbolParsers = array [AnsiChar] of TBaseParser;

    TMatchingPairToken = record
      OpenToken: string;
      CloseToken: string;
    end;

    TAttribute = class(TPersistent)
    strict private
      procedure Changed; virtual;
    strict private
      FBackground: TColor;
      FBackgroundDefault: TColor;
      FElement: string;
      FEscapeChar: Char;
      FFontStyles: TFontStyles;
      FFontStylesDefault: TFontStyles;
      FForeground: TColor;
      FForegroundDefault: TColor;
      FName: string;
      FOnChange: TNotifyEvent;
      FParentBackground: Boolean;
      FParentForeground: Boolean;
      procedure Clear();
      function GetBackgroundColorStored: Boolean;
      function GetFontStylesStored: Boolean;
      function GetForegroundColorStored: Boolean;
      procedure SetBackground(const AValue: TColor);
      procedure SetFontStyles(const AValue: TFontStyles);
      procedure SetForeground(const AValue: TColor);
    protected
      procedure LoadFromJSON(const AJSON: TJSONObject);
    public
      procedure Assign(ASource: TPersistent); override;
      procedure AssignColorAndStyle(ASource: TAttribute);
      constructor Create(const AttributeName: string);
      procedure InternalSaveDefaultValues;
      property Name: string read FName write FName;
      property OnChange: TNotifyEvent read FOnChange write FOnChange;
    published
      property Background: TColor read FBackground write SetBackground stored GetBackgroundColorStored;
      property Element: string read FElement write FElement;
      property EscapeChar: Char read FEscapeChar write FEscapeChar default BCEDITOR_NONE_CHAR;
      property FontStyles: TFontStyles read FFontStyles write SetFontStyles stored GetFontStylesStored;
      property Foreground: TColor read FForeground write SetForeground stored GetForegroundColorStored;
      property ParentBackground: Boolean read FParentBackground write FParentBackground;
      property ParentForeground: Boolean read FParentForeground write FParentForeground;
    end;

    PElement = ^TElement;
    TElement = record
      Background: TColor;
      Foreground: TColor;
      Name: string;
      FontStyles: TFontStyles;
    end;

    TElements = class(TList<TElement>)
    strict private
      FHighlighter: TBCEditorHighlighter;
      FName: string;
      function GetElement(AName: string): PElement;
      procedure LoadFromJSON(const AJSON: TJSONObject);
    public
      constructor Create(const AHighlighter: TBCEditorHighlighter);
      function IndexOf(const AName: string): Integer;
      procedure LoadFromFile(const AFileName: string);
      procedure LoadFromResource(const ResName: string; const ResType: PChar);
      procedure LoadFromStream(const AStream: TStream);
      property Element[Name: string]: PElement read GetElement; default;
      property Name: string read FName;
    end;

    TAbstractRule = class(TObject)
    strict private
      FTokenType: TBCEditorRangeType;
    public
      property TokenType: TBCEditorRangeType read FTokenType write FTokenType;
    end;

    TAbstractToken = class(TObject)
    strict private
      FAttribute: TAttribute;
      FBreakType: TBCEditorBreakType;
      FOpenRule: TAbstractRule;
    public
      procedure Clear;
      constructor Create; reintroduce; overload;
      constructor Create(const AToken: TAbstractToken); reintroduce; overload;
      constructor Create(const AHighlighterAttribute: TAttribute); reintroduce; overload;
      property Attribute: TAttribute read FAttribute write FAttribute;
      property BreakType: TBCEditorBreakType read FBreakType write FBreakType;
      property OpenRule: TAbstractRule read FOpenRule write FOpenRule;
    end;

    TMultiToken = class(TAbstractToken)
    strict private
      FSymbols: TStringList;
      function GetSymbol(const AIndex: Integer): string;
      procedure SetSymbol(const AIndex: Integer; const ASymbol: string);
    public
      function AddSymbol(const ASymbol: string): Integer;
      procedure Clear;
      constructor Create; reintroduce; overload;
      constructor Create(const AHighlighterAttribute: TAttribute); reintroduce; overload;
      constructor Create(const AMultiToken: TMultiToken); reintroduce; overload;
      procedure DeleteSymbol(const AIndex: Integer);
      destructor Destroy; override;
      function SymbolCount: Integer;
      property Symbols[const AIndex: Integer]: string read GetSymbol write SetSymbol;
    end;

    TToken = class(TAbstractToken)
    strict private
      FClosingToken: TToken;
      FSymbol: string;
      FTemporary: Boolean;
    public
      procedure Clear;
      constructor Create; overload;
      constructor Create(const AHighlighterAttribute: TAttribute); overload;
      constructor Create(const AMultiToken: TMultiToken; const AIndex: Integer); overload;
      constructor Create(const AToken: TToken); overload;
      property ClosingToken: TToken read FClosingToken write FClosingToken;
      property Symbol: string read FSymbol write FSymbol;
      property Temporary: Boolean read FTemporary write FTemporary;
    end;

    TTokenNode = class(TObject)
    strict private
      FBreakType: TBCEditorBreakType;
      FChar: Char;
      FNextNodes: TTokenNodeList;
      FToken: TToken;
    public
      constructor Create(const AChar: Char); overload;
      constructor Create(const AChar: Char; const AToken: TToken; const ABreakType: TBCEditorBreakType); overload;
      destructor Destroy; override;
      property BreakType: TBCEditorBreakType read FBreakType write FBreakType;
      property Char: Char read FChar write FChar;
      property NextNodes: TTokenNodeList read FNextNodes write FNextNodes;
      property Token: TToken read FToken write FToken;
    end;

    TTokenNodeList = class(TObject)
    strict private
      FNodeList: TObjectList<TTokenNode>;
    public
      procedure AddNode(const ANode: TTokenNode);
      constructor Create;
      destructor Destroy; override;
      function FindNode(const AChar: Char): TTokenNode;
      function GetCount: Integer;
      function GetNode(const AIndex: Integer): TTokenNode;
      procedure SetNode(const AIndex: Integer; const AValue: TTokenNode);
      property Count: Integer read GetCount;
      property Nodes[const Aindex: Integer]: TTokenNode read GetNode write SetNode;
    end;

    TRule = class(TAbstractRule)
    private
      FAttribute: TAttribute;
      FStyle: string;
    protected
      FParent: TRange;
    public
      constructor Create;
      destructor Destroy; override;
      property Attribute: TAttribute read FAttribute;
      property Parent: TRange read FParent write FParent;
      property Style: string read FStyle;
    end;

    TKeyList = class(TRule)
    strict private
      FKeyList: TStringList;
      FSyncEdit: Boolean;
    protected
      procedure LoadFromJSON(const AJSON: TJSONObject);
    public
      constructor Create;
      destructor Destroy; override;
      property KeyList: TStringList read FKeyList write FKeyList;
      property SyncEdit: Boolean read FSyncEdit write FSyncEdit;
    end;

    TSet = class(TRule)
    strict private
      FCharSet: TBCEditorAnsiCharSet;
    protected
      procedure LoadFromJSON(const AJSON: TJSONObject);
    public
      constructor Create(ACharSet: TBCEditorAnsiCharSet = []);
      property CharSet: TBCEditorAnsiCharSet read FCharSet write FCharSet;
    end;

    TCaseFunction = function(AChar: Char): Char;
    TStringCaseFunction = function(const AString: string): string;

    TRange = class(TRule)
    strict private
      FAlternativeCloseArray: TStringList;
      FCaseFunct: TCaseFunction;
      FCaseSensitive: Boolean;
      FCloseOnEndOfLine: Boolean;
      FCloseOnTerm: Boolean;
      FCloseParent: Boolean;
      FCloseToken: TMultiToken;
      FClosingToken: TToken;
      FDefaultSymbols: TDefaultParser;
      FDefaultTermSymbol: TDelimitersParser;
      FDefaultToken: TToken;
      FDelimiters: TBCEditorAnsiCharSet;
      FHighlighter: TBCEditorHighlighter;
      FKeyList: TObjectList<TKeyList>;
      FOpenBeginningOfLine: Boolean;
      FOpenToken: TMultiToken;
      FParent: TRange;
      FPrepared: Boolean;
      FRanges: TObjectList<TRange>;
      FSets: TObjectList<TSet>;
      FSkipWhitespace: Boolean;
      FStringCaseFunct: TStringCaseFunction;
      FSymbolParsers: TSymbolParsers;
      FTokens: TObjectList<TToken>;
      FUseDelimitersForText: Boolean;
      function GetKeyList(const AIndex: Integer): TKeyList;
      function GetKeyListCount: Integer;
      function GetRange(const AIndex: Integer): TRange;
      function GetRangeCount: Integer;
      function GetSet(const AIndex: Integer): TSet;
      function GetSetCount: Integer;
      function GetToken(const AIndex: Integer): TToken;
      procedure SetCaseSensitive(const AValue: Boolean);
    protected
      procedure LoadFromJSON(const AJSON: TJSONObject; const ASkipBeforeSubRules: Boolean = False);
    public
      procedure AddKeyList(NewKeyList: TKeyList);
      procedure AddRange(NewRange: TRange);
      procedure AddSet(NewSet: TSet);
      procedure AddToken(const AToken: TToken);
      procedure AddTokenRange(const AOpenToken: string; AOpenTokenBreakType: TBCEditorBreakType; const ACloseToken: string;
        ACloseTokenBreakType: TBCEditorBreakType);
      procedure Clear;
      constructor Create(const AHighlighter: TBCEditorHighlighter;
        const AParent: TRange = nil;
        const AOpenToken: string = ''; const ACloseToken: string = ''); virtual;
      destructor Destroy; override;
      function FindToken(const AString: string): TToken;
      procedure Prepare(AParent: TRange);
      procedure Reset;
      procedure SetDelimiters(const ADelimiters: TBCEditorAnsiCharSet);
      property AlternativeCloseList: TStringList read FAlternativeCloseArray;
      property CaseFunct: TCaseFunction read FCaseFunct;
      property CaseSensitive: Boolean read FCaseSensitive write SetCaseSensitive;
      property CloseOnEndOfLine: Boolean read FCloseOnEndOfLine write FCloseOnEndOfLine;
      property CloseOnTerm: Boolean read FCloseOnTerm write FCloseOnTerm;
      property CloseParent: Boolean read FCloseParent write FCloseParent;
      property CloseToken: TMultiToken read FCloseToken write FCloseToken;
      property ClosingToken: TToken read FClosingToken write FClosingToken;
      property DefaultToken: TToken read FDefaultToken;
      property Delimiters: TBCEditorAnsiCharSet read FDelimiters write FDelimiters;
      property KeyList[const AIndex: Integer]: TKeyList read GetKeyList;
      property KeyListCount: Integer read GetKeyListCount;
      property OpenBeginningOfLine: Boolean read FOpenBeginningOfLine write FOpenBeginningOfLine;
      property OpenToken: TMultiToken read FOpenToken write FOpenToken;
      property Prepared: Boolean read FPrepared;
      property RangeCount: Integer read GetRangeCount;
      property Ranges[const AIndex: Integer]: TRange read GetRange;
      property SetCount: Integer read GetSetCount;
      property Sets[const AIndex: Integer]: TSet read GetSet;
      property SkipWhitespace: Boolean read FSkipWhitespace write FSkipWhitespace;
      property StringCaseFunct: TStringCaseFunction read FStringCaseFunct;
      property SymbolParsers: TSymbolParsers read FSymbolParsers;
      property Tokens[const AIndex: Integer]: TToken read GetToken;
      property UseDelimitersForText: Boolean read FUseDelimitersForText write FUseDelimitersForText;
    end;

    TComments = class(TObject)
    strict private
      FChars: TBCEditorAnsiCharSet;
      FBlockComments: TBCEditorArrayOfString;
      FLineComments: TBCEditorArrayOfString;
      procedure AddChars(const AToken: string);
    public
      procedure AddBlockComment(const AOpenToken: string; const ACloseToken: string);
      procedure AddLineComment(const AToken: string);
      procedure Clear;
      destructor Destroy; override;
      property Chars: TBCEditorAnsiCharSet read FChars write FChars;
      property BlockComments: TBCEditorArrayOfString read FBlockComments;
      property LineComments: TBCEditorArrayOfString read FLineComments;
    end;

    TBaseParser = class abstract
    public
      function GetToken(const ARange: TRange;
        const ALineText: PChar; const ALineLength: Integer;
        var AChar: Integer; out AToken: TToken): Boolean; virtual; abstract;
    end;

    TParser = class(TBaseParser)
    strict private
      FHeadNode: TTokenNode;
      FSets: TList;
    public
      procedure AddSet(ASet: TSet); virtual;
      procedure AddTokenNode(const AString: string; AToken: TToken; ABreakType: TBCEditorBreakType); virtual;
      constructor Create(AChar: Char; AToken: TToken; ABreakType: TBCEditorBreakType); reintroduce; overload; virtual;
      constructor Create(ASet: TSet); reintroduce; overload; virtual;
      destructor Destroy; override;
      function GetToken(const ARange: TRange;
        const ALineText: PChar; const ALineLength: Integer;
        var AChar: Integer; out AToken: TToken): Boolean; override;
      property HeadNode: TTokenNode read FHeadNode;
      property Sets: TList read FSets;
    end;

    TDefaultParser = class(TBaseParser)
    strict private
      FToken: TToken;
    public
      constructor Create(AToken: TToken); reintroduce; virtual;
      destructor Destroy; override;
      function GetToken(const ARange: TRange;
        const ALineText: PChar; const ALineLength: Integer;
        var AChar: Integer; out AToken: TToken): Boolean; override;
    end;

    TDelimitersParser = class(TBaseParser)
    strict private
      FToken: TToken;
    public
      constructor Create(AToken: TToken); virtual;
      destructor Destroy; override;
      function GetToken(const ARange: TRange;
        const ALineText: PChar; const ALineLength: Integer;
        var AChar: Integer; out AToken: TToken): Boolean; override;
    end;

    PTokenFind = ^TTokenFind;
    TTokenFind = record
    private
      FAttribute: TAttribute;
      FChar: Integer;
      FLength: Integer;
      FLineChar: Integer;
      FLineLength: Integer;
      FLineText: PChar;
      FRange: TRange;
      FText: PChar;
      FToken: TToken;
    public
      property Attribute: TAttribute read FAttribute;
      property Char: Integer read FChar;
      property Length: Integer read FLength;
      property Range: TRange read FRange;
      property Text: PChar read FText;
    end;

    TDetection = class
    strict private
      FDefaultExtension: string;
      FExtensions: TStringList;
      FFirstLinePattern: string;
    protected
      procedure Clear();
      procedure LoadFromJSON(const AJSON: TJSONObject);
    public
      constructor Create();
      destructor Destroy(); override;
      property DefaultExtension: string read FDefaultExtension;
      property Extensions: TStringList read FExtensions;
      property FirstLinePattern: string read FFirstLinePattern;
    end;

  strict private
    FAllDelimiters: TBCEditorAnsiCharSet;
    FAttributes: TStringList;
    FCodeFoldingRegions: TBCEditorCodeFoldingRegions;
    FColors: TElements;
    FComments: TComments;
    FCompletionProposalSkipRegions: TBCEditorCodeFoldingSkipRegions;
    FDetection: TDetection;
    FDirectory: string;
    FEditor: TCustomControl;
    FFilename: string;
    FFoldCloseKeyChars: TBCEditorAnsiCharSet;
    FFoldOpenKeyChars: TBCEditorAnsiCharSet;
    FMainRules: TRange;
    FMatchingPairHighlight: Boolean;
    FMatchingPairs: TList<TMatchingPairToken>;
    FMultiHighlighter: Boolean;
    FName: string;
    FOnChange: TNotifyEvent;
    FSample: string;
    FSkipCloseKeyChars: TBCEditorAnsiCharSet;
    FSkipOpenKeyChars: TBCEditorAnsiCharSet;
    FWordBreakChars: TBCEditorAnsiCharSet;
    procedure AddAllAttributes(ARange: TRange);
    procedure UpdateAttributes(ARange: TRange; AParentRange: TRange);
  strict private
    procedure DoChange();
    function GetAttribute(AIndex: Integer): TAttribute;
    procedure AddAttribute(AHighlighterAttribute: TAttribute);
    procedure LoadFromJSON(const AJSON: TJSONObject);
    procedure SetWordBreakChars(AChars: TBCEditorAnsiCharSet);
  protected
    procedure LoadCompletionProposalFromJSON(const AJSON: TJSONObject);
    procedure LoadMatchingPairFromJSON(const AJSON: TJSONObject);
    property Directory: string read FDirectory;
    property Editor: TCustomControl read FEditor;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  public
    constructor Create(const AEditor: TCustomControl);
    destructor Destroy(); override;
    procedure FindClose(const AFind: TTokenFind);
    function FindFirstToken(const ABeginRange: TRange; const AText: PChar;
      const ALength, AFirstChar: Integer; out AFind: TTokenFind): Boolean; overload;
    function FindNextToken(var AFind: TTokenFind): Boolean;
    procedure AddKeyChar(AKeyCharType: TBCEditorKeyCharType; AChar: Char);
    procedure AddKeywords(var AStringList: TStringList);
    procedure Clear();
    procedure LoadFromFile(const AFilename: string);
    procedure LoadFromResource(const AResourceName: string; const AResourceType: PChar);
    procedure LoadFromStream(const AStream: TStream);
    procedure UpdateColors();
    property Attribute[AIndex: Integer]: TAttribute read GetAttribute;
    property Attributes: TStringList read FAttributes;
    property CodeFoldingRegions: TBCEditorCodeFoldingRegions read FCodeFoldingRegions;
    property Colors: TElements read FColors write FColors;
    property Comments: TComments read FComments write FComments;
    property CompletionProposalSkipRegions: TBCEditorCodeFoldingSkipRegions read FCompletionProposalSkipRegions write FCompletionProposalSkipRegions;
    property Detection: TDetection read FDetection;
    property Filename: string read FFilename;
    property FoldCloseKeyChars: TBCEditorAnsiCharSet read FFoldCloseKeyChars write FFoldCloseKeyChars;
    property FoldOpenKeyChars: TBCEditorAnsiCharSet read FFoldOpenKeyChars write FFoldOpenKeyChars;
    property MainRules: TRange read FMainRules;
    property MatchingPairHighlight: Boolean read FMatchingPairHighlight write FMatchingPairHighlight default True;
    property MatchingPairs: TList<TMatchingPairToken> read FMatchingPairs;
    property MultiHighlighter: Boolean read FMultiHighlighter write FMultiHighlighter;
    property Name: string read FName;
    property Sample: string read FSample write FSample;
    property SkipCloseKeyChars: TBCEditorAnsiCharSet read FSkipCloseKeyChars write FSkipCloseKeyChars;
    property SkipOpenKeyChars: TBCEditorAnsiCharSet read FSkipOpenKeyChars write FSkipOpenKeyChars;
    property WordBreakChars: TBCEditorAnsiCharSet read FWordBreakChars write SetWordBreakChars;
  end;

implementation {***************************************************************}

uses
  Types, IOUtils, TypInfo,
  GraphUtil,
  BCEditor, BCEditor.Properties;

resourcestring
  SBCEditorErrorInHighlighterParse = 'JSON parse error on line %d column %d: %s';
  SBCEditorErrorInHighlighterImport = 'Error in highlighter import: %s';
  SBCEditorHighlighterJSONInvalidPair = 'Invalid JSON value type for pair "%s" (Expected type: %s, Found type: %s)';
  SBCEditorHighlighterJSONInvalidValue = 'Invalid JSON value type for item #%d (Expected type: %s, Found type: %s)';
  SBCEditorHighlighterJSONItemNotFound = 'Missing item #%d';
  SBCEditorHighlighterJSONPairNotFound = 'Missing pair "%s" in JSON';
  SBCEditorHighlighterJSONSyntaxError = 'Syntax Error in JSON (Line: %d, Column: %d)';

type
  TCustomBCEditor = class(BCEditor.TCustomBCEditor);

function CaseNone(AChar: Char): Char;
begin
  Result := AChar;
end;

function CaseStringNone(const AString: string): string;
begin
  Result := AString;
end;

function CreateJSONObjectFromText(const AJSON: string): TJSONObject;
var
  LChar: Integer;
  LLine: Integer;
  LPos: Integer;
  LStringList: TStringList;
begin
  if (AJSON = '') then
    raise EBCEditorHighlighterJSON.CreateFmt(SBCEditorHighlighterJSONSyntaxError, [1, 1])
  else
  begin
    Result := TJSONObject.Create();
    LPos := Result.Parse(BytesOf(AJSON), 0);
    if (LPos < 0) then
    begin
      LPos := - LPos;
      LLine := 0;
      LStringList := TStringList.Create();
      LStringList.Text := AJSON;
      while ((LStringList.Count > 0) and (LPos > Length(LStringList[0]))) do
      begin
        Dec(LPos, Length(LStringList[0]) + Length(LStringList.LineBreak));
        LStringList.Delete(0);
        Inc(LLine);
      end;
      LChar := LPos;
      raise Exception.CreateFmt(SBCEditorHighlighterJSONSyntaxError, [LLine + 1, LChar + 1]);
      LStringList.Free();
      FreeAndNil(Result);
    end;
  end;
end;

function CreateJSONObjectFromFile(const AFilename: string): TJSONObject;
var
  LStringList: TStringList;
begin
  if (not TFile.Exists(AFilename)) then
    Result := nil
  else
  begin
    LStringList := TStringList.Create();
    LStringList.LoadFromFile(AFilename);
    Result := CreateJSONObjectFromText(LStringList.Text);
    LStringList.Free();
  end;
end;

function CreateJSONObjectFromStream(const AStream: TStream): TJSONObject;
var
  LStringList: TStringList;
begin
  if (AStream.Size = 0) then
    Result := CreateJSONObjectFromText('')
  else
  begin
    LStringList := TStringList.Create();
    LStringList.LoadFromStream(AStream);
    Result := CreateJSONObjectFromText(LStringList.Text);
    LStringList.Free();
  end;
end;

function JSONValueType(const AJsonValue: TClass): string;
begin
  if (AJsonValue = TJSONTrue) then
    Result := 'True'
  else if (AJsonValue = TJSONString) then
    Result := 'String'
  else if (AJsonValue = TJSONObject) then
    Result := 'Object'
  else if (AJsonValue = TJSONNull) then
    Result := 'Null'
  else if (AJsonValue = TJSONFalse) then
    Result := 'False'
  else if (AJsonValue = TJSONArray) then
    Result := 'Array'
  else
    raise Exception.Create('Unknown TJSONValue class: ' + AJsonValue.ClassName);
end;

function GetJSONValue(const AParentArray: TJSONArray; const AIndex: Integer;
  const AClassType: TClass; const ARequired: Boolean = False): TJSONValue; overload;
var
  LJsonValue: TJSONValue;
begin
  if (not Assigned(AParentArray)) then
    Result := nil
  else
  begin
    LJsonValue := AParentArray.Get(AIndex);
    if (ARequired and not Assigned(LJsonValue)) then
      raise EBCEditorHighlighterJSON.CreateFmt(SBCEditorHighlighterJSONItemNotFound, [AIndex]);
    if (ARequired and (LJsonValue.ClassType <> AClassType)) then
      raise EBCEditorHighlighterJSON.CreateFmt(SBCEditorHighlighterJSONInvalidValue, [AIndex, JSONValueType(AClassType), JSONValueType(LJsonValue.ClassType)]);
    Result := LJsonValue;
  end;
end;

function GetJSONValue(const AParentObject: TJSONObject; const AName: string;
  const AClassType: TClass; const ARequired: Boolean = False): TJSONValue; overload;
var
  LPair: TJSONPair;
begin
  if (not Assigned(AParentObject)) then
    Result := nil
  else
  begin
    LPair := AParentObject.Get(AName);
    if (ARequired and not Assigned(LPair)) then
      raise EBCEditorHighlighterJSON.CreateFmt(SBCEditorHighlighterJSONPairNotFound, [AName]);
    if (ARequired and (LPair.JsonValue.ClassType <> AClassType)) then
      raise EBCEditorHighlighterJSON.CreateFmt(SBCEditorHighlighterJSONInvalidPair, [AName, JSONValueType(AClassType), JSONValueType(LPair.JsonValue.ClassType)]);
    if (not Assigned(LPair)) then
      Result := nil
    else
      Result := LPair.JsonValue;
  end;
end;

function GetJSONValue(const AParentObject: TJSONObject; const AIndex: Integer;
  const AClassType: TClass; const AName: string = ''): TJSONValue; overload;
var
  LPair: TJSONPair;
begin
  if (not Assigned(AParentObject)) then
    Result := nil
  else
  begin
    LPair := AParentObject.Get(AIndex);
    if (not Assigned(LPair)) then
      raise EBCEditorHighlighterJSON.CreateFmt(SBCEditorHighlighterJSONPairNotFound, ['#' + IntToStr(AIndex)]);
    if ((AName <> '') and (LPair.JsonValue.ClassType <> AClassType)) then
      raise EBCEditorHighlighterJSON.CreateFmt(SBCEditorHighlighterJSONInvalidPair, ['#' + IntToStr(AIndex), JSONValueType(AClassType), JSONValueType(LPair.JsonValue.ClassType)]);
    if (not Assigned(LPair) or (AName <> '') and (LPair.JsonString.Value() <> AName)) then
      Result := nil
    else
      Result := LPair.JsonValue;
  end;
end;

function GetJSONArray(const AParentObject: TJSONObject; const AName: string;
  const ARequired: Boolean = False): TJSONArray; overload; inline;
begin
  Result := GetJSONValue(AParentObject, AName, TJSONArray, ARequired) as TJSONArray;
end;

function GetJSONArray(const AParentObject: TJSONObject; const AIndex: Integer;
  const AName: string = ''): TJSONArray; overload; inline;
begin
  Result := GetJSONValue(AParentObject, AIndex, TJSONArray, AName) as TJSONArray;
end;

function GetJSONBoolean(const AParentArray: TJSONArray; const AIndex: Integer;
  const ADefault: Boolean = False): Boolean; overload;
var
  LJSONValue: TJSONValue;
begin
  LJSONValue := GetJSONValue(AParentArray, AIndex, TJSONValue);
  if (LJSONValue is TJSONTrue) then
    Result := True
  else if (LJSONValue is TJSONFalse) then
    Result := False
  else
    Result := ADefault;
end;

function GetJSONBoolean(const AParentObject: TJSONObject; const AName: string;
  const ADefault: Boolean = False): Boolean; overload;
var
  LJSONValue: TJSONValue;
begin
  LJSONValue := GetJSONValue(AParentObject, AName, TJSONValue);
  if (LJSONValue is TJSONTrue) then
    Result := True
  else if (LJSONValue is TJSONFalse) then
    Result := False
  else
    Result := ADefault;
end;

function GetJSONString(const AParentArray: TJSONArray; const AIndex: Integer;
  const ADefault: string = ''; const ARequired: Boolean = False): string; overload;
var
  LJSONString: TJSONString;
begin
  LJSONString := GetJSONValue(AParentArray, AIndex, TJSONString, ARequired) as TJSONString;
  if (not Assigned(LJSONString)) then
    Result := ADefault
  else
    Result := LJSONString.Value();
end;

function GetJSONString(const AParentArray: TJSONArray; const AIndex: Integer;
  const ARequired: Boolean): string; overload;
var
  LJSONString: TJSONString;
begin
  LJSONString := GetJSONValue(AParentArray, AIndex, TJSONString, ARequired) as TJSONString;
  Result := LJSONString.Value();
end;

function GetJSONString(const AParentObject: TJSONObject; const AName: string;
  const ADefault: string = ''): string; overload;
var
  LJSONString: TJSONString;
begin
  LJSONString := GetJSONValue(AParentObject, AName, TJSONString) as TJSONString;
  if (not Assigned(LJSONString)) then
    Result := ADefault
  else
    Result := LJSONString.Value();
end;

function GetJSONString(const AParentObject: TJSONObject; const AName: string;
  const ARequired: Boolean): string; overload;
var
  LJSONString: TJSONString;
begin
  LJSONString := GetJSONValue(AParentObject, AName, TJSONString, ARequired) as TJSONString;
  Result := LJSONString.Value();
end;

function GetJSONObject(const AParentArray: TJSONArray; const AIndex: Integer): TJSONObject; overload; inline;
begin
  Result := GetJSONValue(AParentArray, AIndex, TJSONObject) as TJSONObject;
end;

function GetJSONObject(const AParentObject: TJSONObject; const AIndex: Integer): TJSONObject; overload; inline;
begin
  Result := GetJSONValue(AParentObject, AIndex, TJSONObject) as TJSONObject;
end;

function GetJSONObject(const AParentObject: TJSONObject; const AName: string): TJSONObject; overload; inline;
begin
  Result := GetJSONValue(AParentObject, AName, TJSONObject) as TJSONObject;
end;

function StringToColorDef(const AString: string; const DefaultColor: TColor): Integer;
begin
  if Trim(AString) = '' then
    Result := DefaultColor
  else
  if Pos('clWeb', AString) = 1 then
    Result := WebColorNameToColor(AString)
  else
    Result := StringToColor(AString);
end;

function StrToSet(const AString: string): TBCEditorAnsiCharSet;
var
  LIndex: Integer;
begin
  Result := [];
  for LIndex := 1 to Length(AString) do
    Result := Result + [AString[LIndex]];
end;

function StrToStrDef(const AString: string; const AStringDef: string): string;
begin
  if Trim(AString) = '' then
    Result := AStringDef
  else
    Result := AString
end;

function StrToFontStyle(const AString: string): TFontStyles;
begin
  Result := [];
  if Pos('Bold', AString) > 0 then
    Include(Result, fsBold);
  if Pos('Italic', AString) > 0 then
    Include(Result, fsItalic);
  if Pos('Underline', AString) > 0 then
    Include(Result, fsUnderline);
  if Pos('StrikeOut', AString) > 0 then
    Include(Result, fsStrikeOut);
end;

function StrToBreakType(const AString: string): TBCEditorBreakType;
begin
  if AString = 'Any' then
    Result := btAny
  else
  if (AString = 'Term') or (AString = '') then
    Result := btTerm
  else
    Result := btUnspecified;
end;

function StrToRegionType(const AString: string): TBCEditorRangeItemType;
begin
  if (AString = 'SingleLine') then
    Result := ritSingleLineComment
  else if (AString = 'MultiLine') then
    Result := ritMultiLineComment
  else if (AString = 'SingleLineString') then
    Result := ritSingleLineString
  else
    Result := ritMultiLineString;
end;

function StrToRangeType(const AString: string): TBCEditorRangeType;
var
  LIndex: Integer;
begin
  LIndex := GetEnumValue(TypeInfo(TBCEditorRangeType), 'tt' + AString);
  if LIndex = -1 then
    Result := ttUnspecified
  else
    Result := TBCEditorRangeType(LIndex);
end;

{ TBCEditorCodeFoldingRegion.TItem ********************************************}

procedure TBCEditorCodeFoldingRegionItem.Clear();
begin
  FNoSubs := False;
  FBreakCharFollows := True;
  FBreakIfNotFoundBeforeNextRegion := '';
  FCloseTokenBeginningOfLine := False;
  FOpenIsClose := False;
  FOpenTokenBeginningOfLine := False;
  FOpenTokenBreaksLine := False;
  FSharedClose := False;
  FSkipIfFoundAfterOpenTokenList.Clear();
end;

constructor TBCEditorCodeFoldingRegionItem.Create(ACollection: TCollection);
begin
  inherited Create(ACollection);

  FSkipIfFoundAfterOpenTokenList := TStringList.Create();

  Clear();
end;

destructor TBCEditorCodeFoldingRegionItem.Destroy();
begin
  FSkipIfFoundAfterOpenTokenList.Free();

  inherited;
end;

procedure TBCEditorCodeFoldingRegionItem.LoadFromJSON(const AJSON: TJSONObject);
var
  LIndex: Integer;
  LJSONArray: TJSONArray;
  LSkip: string;
begin
  Clear();

  if (Assigned(AJSON)) then
  begin
    FBeginWithBreakChar := GetJSONBoolean(AJSON, 'BeginWithBreakChar', FBeginWithBreakChar);
    FBreakCharFollows := GetJSONBoolean(AJSON, 'BreakCharFollows', FBreakCharFollows);
    FBreakIfNotFoundBeforeNextRegion := GetJSONString(AJSON, 'BreakIfNotFoundBeforeNextRegion', FBreakIfNotFoundBeforeNextRegion);
    FCloseTokenBeginningOfLine := GetJSONBoolean(AJSON, 'CloseTokenBeginningOfLine', FCloseTokenBeginningOfLine);
    FNoSubs := GetJSONBoolean(AJSON, 'NoSubs', FNoSubs);
    FOpenIsClose := GetJSONBoolean(AJSON, 'OpenIsClose', FOpenIsClose);
    FOpenTokenBreaksLine := GetJSONBoolean(AJSON, 'OpenTokenBreaksLine', FOpenTokenBreaksLine);
    FOpenTokenBeginningOfLine := GetJSONBoolean(AJSON, 'OpenTokenBeginningOfLine', FOpenTokenBeginningOfLine);
    FOpenTokenCanBeFollowedBy := GetJSONString(AJSON, 'OpenTokenCanBeFollowedBy', FOpenTokenCanBeFollowedBy);
    FOpenTokenEnd := GetJSONString(AJSON, 'OpenTokenEnd', FOpenTokenEnd);
    FSharedClose := GetJSONBoolean(AJSON, 'SharedClose', FSharedClose);
    FShowGuideLine := GetJSONBoolean(AJSON, 'ShowGuideLine', FShowGuideLine);
    LJSONArray := GetJSONArray(AJSON, 'SkipIfFoundAfterOpenToken');
    if (Assigned(LJSONArray)) then
      for LIndex := 0 to LJSONArray.Size - 1 do
      begin
        LSkip := GetJSONString(LJSONArray, LIndex);
        if (LSkip <> '') then
          FSkipIfFoundAfterOpenTokenList.Add(LSkip);
      end;
    FTokenEndIsPreviousLine := GetJSONBoolean(AJSON, 'TokenEndIsPreviousLine', FTokenEndIsPreviousLine);
  end;
end;

{ TBCEditorCodeFoldingRegion **************************************************}

function TBCEditorCodeFoldingRegion.Add(const AOpenToken: string; const ACloseToken: string): TBCEditorCodeFoldingRegionItem;
begin
  Result := TBCEditorCodeFoldingRegionItem(inherited Add);
  with Result do
  begin
    OpenToken := AOpenToken;
    OpenTokenLength := Length(AOpenToken);
    CloseToken := ACloseToken;
    CloseTokenLength := Length(ACloseToken);
  end;
end;

procedure TBCEditorCodeFoldingRegion.Clear();
begin
  FCloseToken := '';
  FEscapeChar := BCEDITOR_NONE_CHAR;
  FFoldTags := False;
  FOpenToken := '';
  FSkipRegions.Clear();
  FStringEscapeChar := BCEDITOR_NONE_CHAR;
end;

function TBCEditorCodeFoldingRegion.Contains(const AOpenToken: string; const ACloseToken: string): Boolean;
var
  LIndex: Integer;
  LItem: TBCEditorCodeFoldingRegionItem;
begin
  Result := False;
  for LIndex := 0 to Count - 1 do
  begin
    LItem := Items[LIndex];
    if (LItem.OpenToken = AOpenToken) and (LItem.CloseToken = ACloseToken) then
      Exit(True);
  end;
end;

constructor TBCEditorCodeFoldingRegion.Create(const AHighlighter: TBCEditorHighlighter;
  const AItemClass: TCollectionItemClass);
begin
  inherited Create(AItemClass);

  FHighlighter := AHighlighter;

  FSkipRegions := TBCEditorCodeFoldingSkipRegions.Create(TBCEditorCodeFoldingSkipRegions.TItem);

  Clear();
end;

destructor TBCEditorCodeFoldingRegion.Destroy();
begin
  FSkipRegions.Free();

  inherited;
end;

function TBCEditorCodeFoldingRegion.GetItem(AIndex: Integer): TBCEditorCodeFoldingRegionItem;
begin
  Result := TBCEditorCodeFoldingRegionItem(inherited Items[AIndex]);
end;

procedure TBCEditorCodeFoldingRegion.LoadFromJSON(const AJSON: TJSONObject);
var
  LString: string;
begin
  Clear();

  if (Assigned(AJSON)) then
  begin
    FCloseToken := GetJSONString(AJSON, 'CloseToken');
    LString := GetJSONString(AJSON, 'EscapeChar'); if (Length(LString) = 1) then FEscapeChar := LString[1];
    FFoldTags := GetJSONBoolean(AJSON, 'FoldTags', FoldTags);
    FOpenToken := GetJSONString(AJSON, 'OpenToken');
    LString := GetJSONString(AJSON, 'StringEscapeChar'); if (Length(LString) = 1) then FStringEscapeChar := LString[1];

    FHighlighter.MatchingPairHighlight := GetJSONBoolean(AJSON, 'MatchingPairHighlight', FHighlighter.MatchingPairHighlight);
  end;
end;

procedure TBCEditorCodeFoldingRegion.LoadFoldRegionFromJSON(const AJSON: TJSONObject);
var
  LCloseToken: string;
  LCodeFoldingObject: TJSONObject;
  LFilename: string;
  LFoldRegionArray: TJSONArray;
  LIndex: Integer;
  LJSONItem: TJSONObject;
  LJSONObject: TJSONObject;
  LOpenToken: string;
  LRangesItem: TJSONObject;
  LRegionItem: TBCEditorCodeFoldingRegionItem;
begin
  if (Assigned(AJSON)) then
  begin
    LFoldRegionArray := GetJSONArray(AJSON, 'FoldRegion');
    if (Assigned(LFoldRegionArray)) then
      for LIndex := 0 to LFoldRegionArray.Size - 1 do
      begin
        LJSONItem := GetJSONObject(LFoldRegionArray, LIndex);

        LOpenToken := GetJSONString(LJSONItem, 'OpenToken');
        LCloseToken := GetJSONString(LJSONItem, 'CloseToken');

        if (FHighlighter.MultiHighlighter) then
        begin
          { Multi highlighter code folding fold region include }
          LFilename := GetJSONString(LJSONItem, 'File');
          if (TFile.Exists(FHighlighter.Directory + LFilename)) then
          begin
            LJSONObject := CreateJSONObjectFromFile(FHighlighter.Directory + LFilename) as TJSONObject;
            if (Assigned(LJSONObject)) then
            begin
              LCodeFoldingObject := GetJSONObject(LJSONObject, 'CodeFolding');
              LRangesItem := GetJSONObject(LCodeFoldingObject, 0);
              if (Assigned(LRangesItem)) then
                LoadFoldRegionFromJSON(LRangesItem);
              LJSONObject.Free();
            end;
          end;
          { Skip duplicates }
          if (Contains(LOpenToken, LCloseToken)) then
            Continue;
        end;

        LRegionItem := Add(LOpenToken, LCloseToken);
        LRegionItem.LoadFromJSON(GetJSONObject(LJSONItem, 'Properties'));

        if (LOpenToken <> '') then
          FHighlighter.AddKeyChar(ctFoldOpen, LOpenToken[1]);
        if (LRegionItem.BreakIfNotFoundBeforeNextRegion <> '') then
          FHighlighter.AddKeyChar(ctFoldOpen, LRegionItem.BreakIfNotFoundBeforeNextRegion[1]);
        if (LCloseToken <> '') then
          FHighlighter.AddKeyChar(ctFoldClose, LCloseToken[1]);
      end;
  end;
end;

procedure TBCEditorCodeFoldingRegion.LoadSkipRegionFromJSON(const AJSON: TJSONObject);
var
  LCloseToken: string;
  LCodeFoldingObject: TJSONObject;
  LFilename: string;
  LIndex: Integer;
  LItem: TJSONObject;
  LJSONObject: TJSONObject;
  LOpenToken: string;
  LRangesArray: TJSONArray;
  LRangesItem: TJSONObject;
  LRegionItem: TBCEditorCodeFoldingRegionItem;
  LSkipRegionArray: TJSONArray;
  LSkipRegionItem: TBCEditorCodeFoldingSkipRegions.TItem;
  LSkipRegionType: TBCEditorRangeItemType;
  LString: string;
begin
  if (Assigned(AJSON)) then
  begin
    LSkipRegionArray := GetJSONArray(AJSON, 'SkipRegion');
    if (Assigned(LSkipRegionArray)) then
    begin
      for LIndex := 0 to LSkipRegionArray.Size - 1 do
      begin
        LItem := GetJSONObject(LSkipRegionArray, LIndex);
        if (Assigned(LItem)) then
        begin
          LOpenToken := GetJSONString(LItem, 'OpenToken');
          LCloseToken := GetJSONString(LItem, 'CloseToken');

          if (FHighlighter.MultiHighlighter) then
          begin
            { Multi highlighter code folding skip region include }
            LFilename := GetJSONString(LItem, 'File');
            if (TFile.Exists(FHighlighter.Directory + LFilename)) then
            begin
              LJSONObject := CreateJSONObjectFromFile(FHighlighter.Directory + LFilename);
              if (Assigned(LJSONObject)) then
              begin
                LCodeFoldingObject := GetJSONObject(LJSONObject, 'CodeFolding');
                if (Assigned(LCodeFoldingObject)) then
                begin
                  LRangesArray := GetJSONArray(LCodeFoldingObject, 'Ranges');
                  LRangesItem := GetJSONObject(LRangesArray, 0);
                  LoadSkipRegionFromJSON(LRangesItem);
                end;
                LJSONObject.Free();
              end;
            end;
            { Skip duplicates }
            if (SkipRegions.Contains(LOpenToken, LCloseToken)) then
              Continue;
          end;

          LSkipRegionType := StrToRegionType(GetJSONString(LItem, 'RegionType'));
          if ((LSkipRegionType = ritMultiLineComment)
            and Assigned(FHighlighter.Editor)
            and (cfoFoldMultilineComments in TCustomBCEditor(FHighlighter.Editor).LeftMargin.CodeFolding.Options)) then
          begin
            LRegionItem := Add(LOpenToken, LCloseToken);
            LRegionItem.NoSubs := True;
            FHighlighter.AddKeyChar(ctFoldOpen, LOpenToken[1]);
            if LCloseToken <> '' then
              FHighlighter.AddKeyChar(ctFoldClose, LCloseToken[1]);
          end
          else
          begin
            LSkipRegionItem := SkipRegions.Add(LOpenToken, LCloseToken);
            LSkipRegionItem.RegionType := LSkipRegionType;
            LSkipRegionItem.SkipEmptyChars := GetJSONBoolean(LItem, 'SkipEmptyChars', LSkipRegionItem.SkipEmptyChars);
            LSkipRegionItem.SkipIfNextCharIsNot := BCEDITOR_NONE_CHAR;
            LString := GetJSONString(LItem, 'NextCharIsNot');
            if (LString <> '') then LSkipRegionItem.SkipIfNextCharIsNot := LString[1];
            if (LOpenToken <> '') then
              FHighlighter.AddKeyChar(ctSkipOpen, LOpenToken[1]);
            if (LCloseToken <> '') then
              FHighlighter.AddKeyChar(ctSkipClose, LCloseToken[1]);
          end;
        end;
      end;
    end;
  end;
end;

{ TBCEditorCodeFoldingSkipRegions *********************************************}

function TBCEditorCodeFoldingSkipRegions.Add(const AOpenToken, ACloseToken: string): TItem;
begin
  Result := TItem(inherited Add);
  with Result do
  begin
    OpenToken := AOpenToken;
    CloseToken := ACloseToken;
  end;
end;

function TBCEditorCodeFoldingSkipRegions.Contains(const AOpenToken, ACloseToken: string): Boolean;
var
  LIndex: Integer;
  LSkipRegion: TItem;
begin
  Result := False;
  for LIndex := 0 to Count - 1 do
  begin
    LSkipRegion := SkipRegionItems[LIndex];
    if (LSkipRegion.OpenToken = AOpenToken) and (LSkipRegion.CloseToken = ACloseToken) then
      Exit(True);
  end;
end;

function TBCEditorCodeFoldingSkipRegions.GetSkipRegionItem(AIndex: Integer): TItem;
begin
  Result := TItem(inherited Items[AIndex]);
end;

{ TBCEditorCodeFoldingRanges.TRange *******************************************}

function TBCEditorCodeFoldingRanges.TRange.Collapsable: Boolean;
begin
  Result := (FBeginLine < FEndLine) or RegionItem.TokenEndIsPreviousLine and (FBeginLine = FEndLine);
end;

constructor TBCEditorCodeFoldingRanges.TRange.Create();
begin
  inherited;

  FSubCodeFoldingRanges := TBCEditorCodeFoldingRanges.Create;
  FCollapsed := False;
  FCollapsedBy := -1;
  FIsExtraTokenFound := False;
  FUndoListed := False;
end;

destructor TBCEditorCodeFoldingRanges.TRange.Destroy;
begin;
  FSubCodeFoldingRanges.Clear;
  FSubCodeFoldingRanges.Free;
  FSubCodeFoldingRanges := nil;

  inherited;
end;

procedure TBCEditorCodeFoldingRanges.TRange.MoveBy(LineCount: Integer);
begin
  Inc(FBeginLine, LineCount);
  Inc(FEndLine, LineCount);
end;

procedure TBCEditorCodeFoldingRanges.TRange.MoveChildren(By: Integer);
var
  LCodeFoldingRange: TRange;
  LIndex: Integer;
begin
  for LIndex := 0 to FSubCodeFoldingRanges.Count - 1 do
  begin
    LCodeFoldingRange := FSubCodeFoldingRanges[LIndex];
    if Assigned(LCodeFoldingRange) then
    begin
      LCodeFoldingRange.MoveChildren(By);

      with FAllCodeFoldingRanges.List do
      if LCodeFoldingRange.FParentCollapsed then
        Move(IndexOf(LCodeFoldingRange), IndexOf(LCodeFoldingRange) + By);
    end;
  end;
end;

procedure TBCEditorCodeFoldingRanges.TRange.SetParentCollapsedOfSubCodeFoldingRanges(AParentCollapsed: Boolean; ACollapsedBy: Integer);
var
  LCodeFoldingRange: TRange;
  LIndex: Integer;
begin
  if Assigned(FSubCodeFoldingRanges) then
  for LIndex := 0 to FSubCodeFoldingRanges.Count - 1 do
  begin
    LCodeFoldingRange := FSubCodeFoldingRanges[LIndex];
    LCodeFoldingRange.SetParentCollapsedOfSubCodeFoldingRanges(AParentCollapsed, ACollapsedBy);

    if (LCodeFoldingRange.FCollapsedBy = -1) or (LCodeFoldingRange.FCollapsedBy = ACollapsedBy) then
    begin
      LCodeFoldingRange.FParentCollapsed := AParentCollapsed;

      if not AParentCollapsed then
        LCodeFoldingRange.FCollapsedBy := -1
      else
        LCodeFoldingRange.FCollapsedBy := ACollapsedBy;
    end;
  end;
end;

procedure TBCEditorCodeFoldingRanges.TRange.Widen(LineCount: Integer);
begin
  Inc(FEndLine, LineCount);
end;

{ TBCEditorCodeFoldingRanges **************************************************}

constructor TBCEditorCodeFoldingRanges.Create;
begin
  inherited;

  FList := TList.Create;
end;

destructor TBCEditorCodeFoldingRanges.Destroy;
begin
  FList.Clear;
  FList.Free;
  FList := nil;

  inherited;
end;

function TBCEditorCodeFoldingRanges.Add(AAllCodeFoldingRanges: TBCEditorCodeFoldingAllRanges; ABeginLine, AIndentLevel, AFoldRangeLevel: Integer;
  ARegionItem: TBCEditorCodeFoldingRegionItem; AEndLine: Integer): TRange;
begin
  Result := TRange.Create;
  with Result do
  begin
    BeginLine := ABeginLine;
    EndLine := AEndLine;
    IndentLevel := AIndentLevel;
    FoldRangeLevel := AFoldRangeLevel;
    AllCodeFoldingRanges := AAllCodeFoldingRanges;
    RegionItem := ARegionItem;
  end;
  FList.Add(Result);
  AAllCodeFoldingRanges.List.Add(Result);
end;

procedure TBCEditorCodeFoldingRanges.Clear;
begin
  FList.Clear;
end;

function TBCEditorCodeFoldingRanges.GetCount: Integer;
begin
  Result := FList.Count;
end;

function TBCEditorCodeFoldingRanges.GetItem(AIndex: Integer): TRange;
begin
  Result := FList[AIndex];
end;

{ TBCEditorCodeFoldingAllRanges ***********************************************}

constructor TBCEditorCodeFoldingAllRanges.Create;
begin
  inherited;

  FList := TObjectList<TBCEditorCodeFoldingRanges.TRange>.Create;
end;

destructor TBCEditorCodeFoldingAllRanges.Destroy;
begin
  FList.Free();

  inherited;
end;

procedure TBCEditorCodeFoldingAllRanges.ClearAll;
begin
  Clear;
  FList.Clear();
end;

procedure TBCEditorCodeFoldingAllRanges.Delete(AIndex: Integer);
begin
  FList.Delete(AIndex);
end;

procedure TBCEditorCodeFoldingAllRanges.Delete(FoldRange: TBCEditorCodeFoldingRanges.TRange);
var
  LIndex: Integer;
begin
  for LIndex := 0 to FList.Count - 1 do
  if FList[LIndex] = FoldRange then
  begin
    TBCEditorCodeFoldingRanges.TRange(FList[LIndex]).Free;
    FList[LIndex] := nil;
    FList.Delete(LIndex);
    Break;
  end;
end;

function TBCEditorCodeFoldingAllRanges.GetAllCount: Integer;
begin
  Result := FList.Count;
end;

function TBCEditorCodeFoldingAllRanges.GetItem(AIndex: Integer): TBCEditorCodeFoldingRanges.TRange;
begin
  if Cardinal(AIndex) < Cardinal(FList.Count) then
    Result := FList.List[AIndex]
  else
    Result := nil;
end;

procedure TBCEditorCodeFoldingAllRanges.SetItem(AIndex: Integer; Value: TBCEditorCodeFoldingRanges.TRange);
begin
  FList[AIndex] := Value;
end;

procedure TBCEditorCodeFoldingAllRanges.SetParentCollapsedOfSubCodeFoldingRanges(AFoldRange: TBCEditorCodeFoldingRanges.TRange);
var
  LFoldRange: TBCEditorCodeFoldingRanges.TRange;
  LIndex: Integer;
begin
  for LIndex := 0 to AllCount - 1 do
  begin
    LFoldRange := GetItem(LIndex);
    if LFoldRange = AFoldRange then
      Continue;
    if LFoldRange.BeginLine > AFoldRange.EndLine then
      Break;
    if (LFoldRange.EndLine > AFoldRange.EndLine) and (LFoldRange.EndLine <> AFoldRange.EndLine) then
      LFoldRange.ParentCollapsed := True;
  end;
end;

procedure TBCEditorCodeFoldingAllRanges.UpdateFoldRanges;
var
  LFoldRange: TBCEditorCodeFoldingRanges.TRange;
  LIndex: Integer;
begin
  for LIndex := 0 to AllCount - 1 do
  begin
    LFoldRange := GetItem(LIndex);
    if Assigned(LFoldRange) then
      LFoldRange.ParentCollapsed := False;
  end;
  for LIndex := 0 to AllCount - 1 do
  begin
    LFoldRange := GetItem(LIndex);
    if Assigned(LFoldRange) and not LFoldRange.ParentCollapsed then
      SetParentCollapsedOfSubCodeFoldingRanges(LFoldRange);
  end;
end;

{ TBCEditorHighlighter.TAttribute *********************************************}

procedure TBCEditorHighlighter.TAttribute.Assign(ASource: TPersistent);
begin
  if Assigned(ASource) and (ASource is TAttribute) then
    with ASource as TAttribute do
    begin
      Self.FName := FName;
      Self.AssignColorAndStyle(ASource as TAttribute);
    end
  else
    inherited Assign(ASource);
end;

procedure TBCEditorHighlighter.TAttribute.AssignColorAndStyle(ASource: TAttribute);
var
  IsChanged: Boolean;
begin
  IsChanged := False;
  if FBackground <> ASource.FBackground then
  begin
    FBackground := ASource.FBackground;
    IsChanged := True;
  end;
  if FForeground <> ASource.FForeground then
  begin
    FForeground := ASource.FForeground;
    IsChanged := True;
  end;
  if FFontStyles <> ASource.FFontStyles then
  begin
    FFontStyles := ASource.FFontStyles;
    IsChanged := True;
  end;
  FParentForeground := ASource.ParentForeground;
  FParentBackground := ASource.ParentBackground;
  if IsChanged then
    Changed;
end;

procedure TBCEditorHighlighter.TAttribute.Changed;
begin
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

procedure TBCEditorHighlighter.TAttribute.Clear();
begin
  FBackground := clNone;
  FBackgroundDefault := clNone;
  FElement := '';
  FEscapeChar := BCEDITOR_NONE_CHAR;
  FFontStyles := [];
  FFontStylesDefault := [];
  FForeground := clNone;
  FForegroundDefault := clNone;
  FParentBackground := False;
  FParentForeground := False;
end;

constructor TBCEditorHighlighter.TAttribute.Create(const AttributeName: string);
begin
  inherited Create();

  FName := AttributeName;

  Clear();
end;

function TBCEditorHighlighter.TAttribute.GetBackgroundColorStored: Boolean;
begin
  Result := FBackground <> FBackgroundDefault;
end;

function TBCEditorHighlighter.TAttribute.GetFontStylesStored: Boolean;
begin
  Result := FFontStyles <> FFontStylesDefault;
end;

function TBCEditorHighlighter.TAttribute.GetForegroundColorStored: Boolean;
begin
  Result := FForeground <> FForegroundDefault;
end;

procedure TBCEditorHighlighter.TAttribute.InternalSaveDefaultValues;
begin
  FForegroundDefault := FForeground;
  FBackgroundDefault := FBackground;
  FFontStylesDefault := FFontStyles;
end;

procedure TBCEditorHighlighter.TAttribute.LoadFromJSON(const AJSON: TJSONObject);
var
  LString: string;
begin
  Clear();

  if (Assigned(AJSON)) then
  begin
    FElement := GetJSONString(AJSON, 'Element', True);
    FParentBackground := GetJSONBoolean(AJSON, 'ParentBackground', True);
    FParentForeground := GetJSONBoolean(AJSON, 'ParentForeground', False);
    LString := GetJSONString(AJSON, 'EscapeChar'); if (Length(LString) = 1) then EscapeChar := LString[1];
  end;
end;

procedure TBCEditorHighlighter.TAttribute.SetBackground(const AValue: TColor);
begin
  if FBackground <> AValue then
  begin
    FBackground := AValue;
    Changed;
  end;
end;

procedure TBCEditorHighlighter.TAttribute.SetFontStyles(const AValue: TFontStyles);
begin
  if FFontStyles <> AValue then
  begin
    FFontStyles := AValue;
    Changed;
  end;
end;

procedure TBCEditorHighlighter.TAttribute.SetForeground(const AValue: TColor);
begin
  if FForeground <> AValue then
  begin
    FForeground := AValue;
    Changed;
  end;
end;

{ TBCEditorHighlighter.TColors ************************************************}

constructor TBCEditorHighlighter.TElements.Create(const AHighlighter: TBCEditorHighlighter);
begin
  inherited Create;

  FHighlighter := AHighlighter;
end;

function TBCEditorHighlighter.TElements.GetElement(AName: string): PElement;
var
  LIndex: Integer;
begin
  LIndex := IndexOf(AName);
  if (LIndex < 0) then
    Result := nil
  else
    Result := @List[LIndex];
end;

function TBCEditorHighlighter.TElements.IndexOf(const AName: string): Integer;
var
  LIndex: Integer;
begin
  Result := -1;
  for LIndex := 0 to Count - 1 do
    if (Items[LIndex].Name = AName) then
      Exit(LIndex);
end;

procedure TBCEditorHighlighter.TElements.LoadFromFile(const AFileName: string);
var
  LStream: TStream;
begin
  LStream := TFileStream.Create(AFileName, fmOpenRead);
  LoadFromStream(LStream);
  LStream.Free();
end;

procedure TBCEditorHighlighter.TElements.LoadFromJSON(const AJSON: TJSONObject);
var
  LColorsObject: TJSONObject;
  LEditorColorsObject: TJSONObject;
  LEditorObject: TJSONObject;
  LElement: TElement;
  LElementsArray: TJSONArray;
  LFontObject: TJSONObject;
  LIndex: Integer;
  LItem: TJSONObject;
  LSizeObject: TJSONObject;
  LSize: Integer;
begin
  Clear();

  if (Assigned(AJSON)) then
  begin
    LColorsObject := GetJSONObject(AJSON, 'Colors');
    LEditorObject := GetJSONObject(LColorsObject, 'Editor');
    if (Assigned(LEditorObject)) then
      with TCustomBCEditor(FHighlighter.Editor) do
      begin
        LEditorColorsObject := GetJSONObject(LEditorObject, 'Colors');
        Color := StringToColor(GetJSONString(LEditorColorsObject, 'Background', ColorToString(Color)));
        Colors.ActiveLine.Background := StringToColor(GetJSONString(LEditorColorsObject, 'ActiveLineBackground', ColorToString(Colors.ActiveLine.Background)));
        Colors.CodeFolding.Background := StringToColor(GetJSONString(LEditorColorsObject, 'CodeFoldingBackground', ColorToString(Colors.CodeFolding.Background)));
        Colors.CodeFolding.Foreground := StringToColor(GetJSONString(LEditorColorsObject, 'CodeFoldingFoldingLine', ColorToString(Colors.CodeFolding.Foreground)));
        Colors.Marks.Background := StringToColor(GetJSONString(LEditorColorsObject, 'LeftMarginBookmarkPanel', ColorToString(Colors.Marks.Background)));
        Colors.LineNumbers.Background := StringToColor(GetJSONString(LEditorColorsObject, 'LeftMarginBackground', ColorToString(Colors.LineNumbers.Background)));
        Colors.LineNumbers.Foreground := StringToColor(GetJSONString(LEditorColorsObject, 'LeftMarginLineNumbers', ColorToString(Colors.LineNumbers.Foreground)));
        Colors.LineState.Loaded := StringToColor(GetJSONString(LEditorColorsObject, 'LeftMarginBackground', ColorToString(Colors.LineState.Loaded)));
        Colors.LineState.Modified := StringToColor(GetJSONString(LEditorColorsObject, 'LeftMarginLineStateModified', ColorToString(Colors.LineState.Modified)));
        Colors.LineState.Saved := StringToColor(GetJSONString(LEditorColorsObject, 'LeftMarginLineStateNormal', ColorToString(Colors.LineState.Saved)));
        Colors.MatchingPairs.Background := StringToColor(GetJSONString(LEditorColorsObject, 'MatchingPairMatched', ColorToString(Colors.MatchingPairs.Background)));
        Colors.FoundText.Background := StringToColor(GetJSONString(LEditorColorsObject, 'SearchHighlighterBackground', ColorToString(Colors.FoundText.Background)));
        Colors.FoundText.Foreground := StringToColor(GetJSONString(LEditorColorsObject, 'SearchHighlighterForeground', ColorToString(Colors.FoundText.Foreground)));
        Colors.Selection.Background := StringToColor(GetJSONString(LEditorColorsObject, 'SelectionBackground', ColorToString(Colors.Selection.Background)));
        Colors.Selection.Foreground := StringToColor(GetJSONString(LEditorColorsObject, 'SelectionForeground', ColorToString(Colors.Selection.Foreground)));
        Colors.SpecialChars.Foreground := StringToColor(GetJSONString(LEditorColorsObject, 'SpecialCharForeground', ColorToString(Colors.SpecialChars.Foreground)));
        Colors.SyncEdit.Background := StringToColor(GetJSONString(LEditorColorsObject, 'SyncEditBackground', ColorToString(Colors.SyncEdit.Background)));
        Colors.SyncEdit.Overlay := StringToColor(GetJSONString(LEditorColorsObject, 'SyncEditWordBorder', ColorToString(Colors.SyncEdit.Overlay)));

        LFontObject := GetJSONObject(LEditorObject, 'Fonts');
        if (Assigned(LFontObject)) then
        begin
          Font.Name := GetJSONString(LEditorObject, 'Text', Font.Name);
          if (cpoUseHighlighterColumnFont in CompletionProposal.Options) then
            for LIndex := 0 to CompletionProposal.Columns.Count - 1 do
              CompletionProposal.Columns[LIndex].Font.Name := GetJSONString(LFontObject, 'CompletionProposal', CompletionProposal.Columns[LIndex].Font.Name);
        end;
        LSizeObject := GetJSONObject(LEditorObject, 'FontSizes');
        if Assigned(LSizeObject) then
        begin
          if (TryStrToInt(GetJSONString(LSizeObject, 'Text'), LSize)) then Font.Size := LSize;
          if (cpoUseHighlighterColumnFont in CompletionProposal.Options) then
            for LIndex := 0 to CompletionProposal.Columns.Count - 1 do
              if (TryStrToInt(GetJSONString(LSizeObject, 'CompletionProposal'), LSize)) then CompletionProposal.Columns[LIndex].Font.Size := LSize;
        end;
      end;

    LElementsArray := GetJSONArray(LColorsObject, 'Elements');
    for LIndex := 0 to LElementsArray.Size - 1 do
    begin
      LItem := GetJSONObject(LElementsArray, LIndex);
      if (Assigned(LItem)) then
      begin
        LElement.Background := StringToColor(GetJSONString(LItem, 'Background', 'clNone'));
        LElement.Foreground := StringToColor(GetJSONString(LItem, 'Foreground', 'clNone'));
        LElement.Name := GetJSONString(LItem, 'Name');
        LElement.FontStyles := StrToFontStyle(GetJSONString(LItem, 'Style'));
        if ((LElement.Name <> '') and (IndexOf(LElement.Name) < 0)) then
          Add(LElement);
      end;
    end;
  end;

  FHighlighter.UpdateColors();
end;

procedure TBCEditorHighlighter.TElements.LoadFromResource(const ResName: string; const ResType: PChar);
var
  LStream: TResourceStream;
begin
  LStream := TResourceStream.Create(HInstance, PChar(ResName), ResType);
  LoadFromStream(LStream);
  LStream.Free();
end;

procedure TBCEditorHighlighter.TElements.LoadFromStream(const AStream: TStream);
var
  LJSON: TJSONObject;
begin
  LJSON := CreateJSONObjectFromStream(AStream);
  if (Assigned(LJSON)) then
  begin
    LoadFromJSON(LJSON);
    LJSON.Free();
  end;
end;

{ TBCEditorHighlighter.TAbstractToken *****************************************}

procedure TBCEditorHighlighter.TAbstractToken.Clear;
begin
  FBreakType := btUnspecified;
end;

constructor TBCEditorHighlighter.TAbstractToken.Create;
begin
  inherited;

  FAttribute := nil;
  FOpenRule := nil;
  FBreakType := btUnspecified;
end;

constructor TBCEditorHighlighter.TAbstractToken.Create(const AToken: TAbstractToken);
begin
  inherited Create;
  FAttribute := AToken.Attribute;
  FBreakType := AToken.BreakType;
end;

constructor TBCEditorHighlighter.TAbstractToken.Create(const AHighlighterAttribute: TAttribute);
begin
  Create;
  FAttribute := AHighlighterAttribute;
end;

{ TBCEditorHighlighter.TMultiToken ********************************************}

function TBCEditorHighlighter.TMultiToken.AddSymbol(const ASymbol: string): Integer;
begin
  Result := FSymbols.Add(ASymbol);
end;

procedure TBCEditorHighlighter.TMultiToken.Clear;
begin
  FSymbols.Clear;
end;

constructor TBCEditorHighlighter.TMultiToken.Create;
begin
  inherited;

  FSymbols := TStringList.Create;
  BreakType := btUnspecified;
end;

constructor TBCEditorHighlighter.TMultiToken.Create(const AHighlighterAttribute: TAttribute);
begin
  inherited;

  Create;
end;

constructor TBCEditorHighlighter.TMultiToken.Create(const AMultiToken: TMultiToken);
begin
  inherited Create(AMultiToken as TAbstractToken);

  Create;
end;

destructor TBCEditorHighlighter.TMultiToken.Destroy;
begin
  FSymbols.Free;
  FSymbols := nil;
  inherited;
end;

procedure TBCEditorHighlighter.TMultiToken.DeleteSymbol(const AIndex: Integer);
begin
  if (AIndex > -1) and (AIndex < FSymbols.Count) then
    FSymbols.Delete(AIndex)
end;

function TBCEditorHighlighter.TMultiToken.GetSymbol(const AIndex: Integer): string;
begin
  Result := '';
  if (AIndex > -1) and (AIndex < FSymbols.Count) then
    Result := FSymbols[AIndex]
end;

procedure TBCEditorHighlighter.TMultiToken.SetSymbol(const AIndex: Integer; const ASymbol: string);
begin
  if (AIndex > -1) and (AIndex < FSymbols.Count) then
    FSymbols[AIndex] := ASymbol
end;

function TBCEditorHighlighter.TMultiToken.SymbolCount: Integer;
begin
  Result := FSymbols.Count;
end;

{ TBCEditorHighlighter.TToken *************************************************}

procedure TBCEditorHighlighter.TToken.Clear;
begin
  Symbol := '';
end;

constructor TBCEditorHighlighter.TToken.Create;
begin
  inherited Create;

  Symbol := '';
  FTemporary := False;
end;

constructor TBCEditorHighlighter.TToken.Create(const AHighlighterAttribute: TAttribute);
begin
  inherited Create(AHighlighterAttribute);
  Symbol := '';
end;

constructor TBCEditorHighlighter.TToken.Create(const AMultiToken: TMultiToken; const AIndex: Integer);
begin
  inherited Create(AMultiToken as TAbstractToken);

  Symbol := AMultiToken.Symbols[AIndex];
end;

constructor TBCEditorHighlighter.TToken.Create(const AToken: TToken);
begin
  inherited Create(AToken as TAbstractToken);

  Symbol := AToken.Symbol;
end;

{ TBCEditorHighlighter.TTokenNode *********************************************}

constructor TBCEditorHighlighter.TTokenNode.Create(const AChar: Char);
begin
  inherited Create;

  FChar := AChar;
  FNextNodes := TBCEditorHighlighter.TTokenNodeList.Create;
  FToken := nil;
end;

constructor TBCEditorHighlighter.TTokenNode.Create(const AChar: Char; const AToken: TToken; const ABreakType: TBCEditorBreakType);
begin
  Create(AChar);
  FBreakType := ABreakType;
  FToken := AToken;
end;

destructor TBCEditorHighlighter.TTokenNode.Destroy;
begin
  FNextNodes.Free;
  FNextNodes := nil;
  inherited;
end;

{ TBCEditorHighlighter.TTokenNodeList *****************************************}

procedure TBCEditorHighlighter.TTokenNodeList.AddNode(const ANode: TTokenNode);
begin
  FNodeList.Add(ANode);
end;

constructor TBCEditorHighlighter.TTokenNodeList.Create;
begin
  inherited;

  FNodeList := TObjectList<TTokenNode>.Create;
end;

destructor TBCEditorHighlighter.TTokenNodeList.Destroy;
begin
  FNodeList.Free();

  inherited;
end;

function TBCEditorHighlighter.TTokenNodeList.FindNode(const AChar: Char): TTokenNode;
var
  LIndex: Integer;
  LTokenNode: TBCEditorHighlighter.TTokenNode;
begin
  Result := nil;
  for LIndex := FNodeList.Count - 1 downto 0 do
  begin
    LTokenNode := FNodeList.List[LIndex];
    if LTokenNode.Char = AChar then
      Exit(LTokenNode);
  end;
end;

function TBCEditorHighlighter.TTokenNodeList.GetCount: Integer;
begin
  Result := FNodeList.Count;
end;

function TBCEditorHighlighter.TTokenNodeList.GetNode(const AIndex: Integer): TTokenNode;
begin
  Result := FNodeList[AIndex];
end;

procedure TBCEditorHighlighter.TTokenNodeList.SetNode(const AIndex: Integer; const AValue: TTokenNode);
begin
  if AIndex < FNodeList.Count then
    FNodeList[AIndex].Free;
  FNodeList[AIndex] := AValue;
end;

{ TBCEditorHighlighter.TRule **************************************************}

constructor TBCEditorHighlighter.TRule.Create;
begin
  inherited;

  FAttribute := TAttribute.Create('');
end;

destructor TBCEditorHighlighter.TRule.Destroy;
begin
  FAttribute.Free;
  FAttribute := nil;

  inherited;
end;

{ TBCEditorHighlighter.TKeyList ***********************************************}

constructor TBCEditorHighlighter.TKeyList.Create;
begin
  inherited;

  FKeyList := TStringList.Create;
  FKeyList.Sorted := True;
  FAttribute.Foreground := clWindowText;
  FAttribute.Background := clWindow;
end;

destructor TBCEditorHighlighter.TKeyList.Destroy;
begin
  FKeyList.Free;
  FKeyList := nil;

  inherited;
end;

procedure TBCEditorHighlighter.TKeyList.LoadFromJSON(const AJSON: TJSONObject);
var
  LIndex: Integer;
  LString: string;
  LSyncEditObject: TJSONObject;
  LWordArray: TJSONArray;
begin
  if (Assigned(AJSON)) then
  begin
    TokenType := StrToRangeType(GetJSONString(AJSON, 'Type', True));
    LWordArray := GetJSONArray(AJSON, 'Words', True);
    for LIndex := 0 to LWordArray.Size - 1 do
    begin
      LString := GetJSONString(LWordArray, LIndex);
      if (LString <> '') then
        KeyList.Add(LString);
    end;
    Attribute.LoadFromJSON(GetJSONObject(AJSON, 'Attributes'));
    LSyncEditObject := GetJSONObject(AJSON, 'SyncEdit');
    if (Assigned(LSyncEditObject)) then
      SyncEdit := GetJSONBoolean(LSyncEditObject, 'Enabled', SyncEdit);
  end;
end;
{ TBCEditorHighlighter.TSet ***************************************************}

constructor TBCEditorHighlighter.TSet.Create(ACharSet: TBCEditorAnsiCharSet = []);
begin
  inherited Create;

  FCharSet := ACharSet;
  FAttribute.Foreground := clWindowText;
  FAttribute.Background := clWindow;
end;

procedure TBCEditorHighlighter.TSet.LoadFromJSON(const AJSON: TJSONObject);
begin
  if (Assigned(AJSON)) then
  begin
    CharSet := StrToSet(GetJSONString(AJSON, 'Symbols', True));

    Attribute.LoadFromJSON(GetJSONObject(AJSON, 'Attributes'));
  end;
end;

{ TBCEditorHighlighter.TRange *************************************************}

procedure TBCEditorHighlighter.TRange.AddKeyList(NewKeyList: TKeyList);
begin
  FKeyList.Add(NewKeyList);
  NewKeyList.Parent := Self;
end;

procedure TBCEditorHighlighter.TRange.AddRange(NewRange: TRange);
begin
  FRanges.Add(NewRange);
  NewRange.Parent := Self;
end;

procedure TBCEditorHighlighter.TRange.AddSet(NewSet: TSet);
begin
  FSets.Add(NewSet);
  NewSet.Parent := Self;
end;

procedure TBCEditorHighlighter.TRange.AddToken(const AToken: TToken);
var
  LCompare: Integer;
  LHigh: Integer;
  LLow: Integer;
  LMiddle: Integer;
  LToken: TToken;
begin
  LLow := 0;
  LHigh := FTokens.Count - 1;

  while LLow <= LHigh do
  begin
    LMiddle := LLow + (LHigh - LLow) shr 1;
    LToken := FTokens.Items[LMiddle];
    LCompare := CompareStr(LToken.Symbol, AToken.Symbol);

    if LCompare < 0 then
      LLow := LMiddle + 1
    else
    if LCompare > 0 then
      LHigh := LMiddle - 1
    else
      Exit;
  end;

  FTokens.Insert(LLow, AToken);
end;

procedure TBCEditorHighlighter.TRange.AddTokenRange(const AOpenToken: string; AOpenTokenBreakType: TBCEditorBreakType; const ACloseToken: string;
  ACloseTokenBreakType: TBCEditorBreakType);
begin
  FOpenToken.AddSymbol(AOpenToken);
  FOpenToken.BreakType := AOpenTokenBreakType;
  FCloseToken.AddSymbol(ACloseToken);
  FCloseToken.BreakType := ACloseTokenBreakType;
end;

procedure TBCEditorHighlighter.TRange.Clear;
var
  LAnsiChar: AnsiChar;
  LIndex: Integer;
begin
  OpenToken.Clear;
  CloseToken.Clear;
  CloseOnTerm := False;
  CloseOnEndOfLine := False;
  CloseParent := False;
  Reset;

  for LAnsiChar := Low(FSymbolParsers) to High(FSymbolParsers) do
    if (Assigned(FSymbolParsers[LAnsiChar])) then
      FSymbolParsers[LAnsiChar] := nil;

  if Assigned(FRanges) then
    for LIndex := 0 to FRanges.Count - 1 do
      TRange(FRanges[LIndex]).Clear;

  FRanges.Clear();
  FTokens.Clear();
  FKeyList.Clear();
  FSets.Clear();
end;

constructor TBCEditorHighlighter.TRange.Create(const AHighlighter: TBCEditorHighlighter;
  const AParent: TRange = nil;
  const AOpenToken: string = ''; const ACloseToken: string = '');
begin
  inherited Create;

  FHighlighter := AHighlighter;
  FParent := AParent;

  FOpenToken := TMultiToken.Create;
  FCloseToken := TMultiToken.Create;
  AddTokenRange(AOpenToken, btUnspecified, ACloseToken, btUnspecified);

  SetCaseSensitive(False);

  FAlternativeCloseArray := TStringList.Create();

  FPrepared := False;

  FRanges := TObjectList<TRange>.Create;
  FKeyList := TObjectList<TKeyList>.Create;
  FSets := TObjectList<TSet>.Create;
  FTokens := TObjectList<TToken>.Create;

  FDelimiters := BCEDITOR_DEFAULT_DELIMITERS;

  FAttribute.Foreground := clWindowText;
  FAttribute.Background := clWindow;
end;

destructor TBCEditorHighlighter.TRange.Destroy;
begin
  Clear;
  Reset;

  FAlternativeCloseArray.Free();
  FOpenToken.Free;
  FOpenToken := nil;
  FCloseToken.Free;
  FCloseToken := nil;
  FAttribute.Free;
  FAttribute := nil;
  FKeyList.Free;
  FSets.Free;
  FTokens.Free;
  FTokens := nil;
  FRanges.Free;
  FRanges := nil;

  inherited;
end;

function TBCEditorHighlighter.TRange.FindToken(const AString: string): TToken;
var
  LCompare: Integer;
  LHigh: Integer;
  LLow: Integer;
  LMiddle: Integer;
  LToken: TToken;
begin
  Result := nil;

  LLow := 0;
  LHigh := FTokens.Count - 1;

  while LLow <= LHigh do
  begin
    LMiddle := LLow + (LHigh - LLow) shr 1;

    LToken := FTokens.Items[LMiddle];
    LCompare := CompareStr(LToken.Symbol, AString);

    if LCompare = 0 then
      Exit(LToken)
    else
    if LCompare < 0 then
      LLow := LMiddle + 1
    else
      LHigh := LMiddle - 1;
  end;
end;

function TBCEditorHighlighter.TRange.GetKeyList(const AIndex: Integer): TKeyList;
begin
  Result := FKeyList[AIndex];
end;

function TBCEditorHighlighter.TRange.GetKeyListCount: Integer;
begin
  Result := FKeyList.Count;
end;

function TBCEditorHighlighter.TRange.GetRange(const AIndex: Integer): TRange;
begin
  Result := FRanges[AIndex];
end;

function TBCEditorHighlighter.TRange.GetRangeCount: Integer;
begin
  Result := FRanges.Count;
end;

function TBCEditorHighlighter.TRange.GetSet(const AIndex: Integer): TSet;
begin
  Result := FSets.List[AIndex];
end;

function TBCEditorHighlighter.TRange.GetSetCount: Integer;
begin
  Result := FSets.Count;
end;

function TBCEditorHighlighter.TRange.GetToken(const AIndex: Integer): TToken;
begin
  Result := FTokens[AIndex];
end;

procedure TBCEditorHighlighter.TRange.LoadFromJSON(const AJSON: TJSONObject;
  const ASkipBeforeSubRules: Boolean = False);
var
  LAlternativeCloseArray: TJSONArray;
  LCloseBreakType: string;
  LCloseToken: string;
  LFilename: string;
  LFileObject: TJSONObject;
  LHighlighterObject: TJSONObject;
  LIncludeRange: string;
  LIndex: Integer;
  LIndex2: Integer;
  LKeyListArray: TJSONArray;
  LKeyListObject: TJSONObject;
  LMainRulesObject: TJSONObject;
  LName: string;
  LNewKeyList: TKeyList;
  LNewRange: TRange;
  LNewSet: TSet;
  LOpenBreakType: string;
  LOpenToken: string;
  LPair: TJSONPair;
  LPropertiesObject: TJSONObject;
  LRangeArray: TJSONArray;
  LRangeObject: TJSONObject;
  LJSONString: TJSONString;
  LSetArray: TJSONArray;
  LSetObject: TJSONObject;
  LString: string;
  LSubRuleArray: TJSONArray;
  LSubRuleObject: TJSONObject;
  LSubRulesObject: TJSONObject;
  LTokenRangeObject: TJSONObject;
begin
  if (Assigned(AJSON)) then
  begin
    LFilename := GetJSONString(AJSON, 'File');
    if (FHighlighter.MultiHighlighter and TFile.Exists(FHighlighter.Directory + LFilename)) then
    begin
      LFileObject := CreateJSONObjectFromText(FHighlighter.Directory + LFilename);
      if (Assigned(LFileObject)) then
      begin
        LHighlighterObject := GetJSONObject(LFileObject, 'Highlighter');
        LMainRulesObject := GetJSONObject(LHighlighterObject, 'MainRules');

        LName := GetJSONString(LMainRulesObject, 'Name');
        LIncludeRange := GetJSONString(AJSON, 'IncludeRange');
        { You can include MainRules... }
        if (Assigned(FParent)
          and (LName <> '') and (LName = LIncludeRange)) then
          FParent.LoadFromJSON(LMainRulesObject, True)
        else
        { or SubRules... }
        begin
          LSubRulesObject := GetJSONObject(LMainRulesObject, 'SubRules');
          if (Assigned(LSubRulesObject)) then
            for LIndex := 0 to LSubRulesObject.Size - 1 do
            begin
              LSubRuleArray := GetJSONArray(LSubRulesObject, LIndex, 'Range');
              if (Assigned(LSubRuleArray)) then
                for LIndex2 := 0 to LSubRuleArray.Size - 1 do
                begin
                  LSubRuleObject := GetJSONObject(LSubRuleArray, LIndex2);
                  if (Assigned(LSubRuleObject)) then
                  begin
                    LName := GetJSONString(LSubRuleObject, 'Name');
                    if ((LName <> '') and (LName = LIncludeRange)) then
                    begin
                      LoadFromJSON(LSubRuleObject);
                      Break;
                    end;
                  end;
                end;
            end;
        end;
      end;
    end
    else
    begin
      if (not ASkipBeforeSubRules) then
      begin
        Clear();
        FAttribute.LoadFromJSON(GetJSONObject(AJSON, 'Attributes'));
        FCaseSensitive := GetJSONBoolean(AJSON, 'CaseSensitive', FCaseSensitive);
        LJSONString := GetJSONValue(AJSON, 'Delimiters', TJSONString) as TJSONString;
        if (Assigned(LJSONString)) then
          FDelimiters := StrToSet(LJSONString.Value());
        LJSONString := GetJSONValue(AJSON, 'Type', TJSONString) as TJSONString;
        if (Assigned(LJSONString)) then
          TokenType := StrToRangeType(LJSONString.Value());

        LPropertiesObject := GetJSONObject(AJSON, 'Properties');
        if (Assigned(LPropertiesObject)) then
        begin
          FCloseOnEndOfLine := GetJSONBoolean(LPropertiesObject, 'CloseOnEndOfLine', FCloseOnEndOfLine);
          FCloseOnTerm := GetJSONBoolean(LPropertiesObject, 'CloseOnTerm', FCloseOnTerm);
          FCloseParent := GetJSONBoolean(LPropertiesObject, 'CloseParent', FCloseParent);
          FOpenBeginningOfLine := GetJSONBoolean(LPropertiesObject, 'OpenBeginningOfLine', FOpenBeginningOfLine);
          FSkipWhitespace := GetJSONBoolean(LPropertiesObject, 'SkipWhitespace', FSkipWhitespace);
          FUseDelimitersForText := GetJSONBoolean(LPropertiesObject, 'UseDelimitersForText', FUseDelimitersForText);

          LAlternativeCloseArray := GetJSONArray(LPropertiesObject, 'AlternativeClose');
          if (Assigned(LAlternativeCloseArray)) then
          begin
            AlternativeCloseList.Clear();
            for LIndex := 0 to LAlternativeCloseArray.Size - 1 do
            begin
              LString := GetJSONString(LAlternativeCloseArray, LIndex);
              if (LString <> '') then
                AlternativeCloseList.Add(LString);
            end;
          end;
        end;

        OpenToken.Clear();
        OpenToken.BreakType := btUnspecified;
        CloseToken.Clear();
        CloseToken.BreakType := btUnspecified;

        LTokenRangeObject := GetJSONObject(AJSON, 'TokenRange');
        if (Assigned(LTokenRangeObject)) then
        begin
          LOpenToken := GetJSONString(LTokenRangeObject, 'Open');
          LCloseToken := GetJSONString(LTokenRangeObject, 'Close');
          LOpenBreakType := GetJSONString(LTokenRangeObject, 'OpenBreakType');
          LCloseBreakType := GetJSONString(LTokenRangeObject, 'CloseBreakType');

          AddTokenRange(LOpenToken, StrToBreakType(LOpenBreakType), LCloseToken, StrToBreakType(LCloseBreakType));

          case (TokenType) of
            ttLineComment: FHighlighter.Comments.AddLineComment(LOpenToken);
            ttBlockComment: FHighlighter.Comments.AddBlockComment(LOpenToken, LCloseToken);
          end;
        end;
      end;
      { Sub rules }
      LSubRulesObject := GetJSONObject(AJSON, 'SubRules');

      if (Assigned(LSubRulesObject)) then
      begin
        for LIndex := 0 to LSubRulesObject.Size - 1 do
        begin
          LPair := LSubRulesObject.Get(LIndex);
          if (LPair.JsonString.Value() = 'Range') then
          begin
            LRangeArray := GetJSONArray(LSubRulesObject, LIndex, 'Range');
            if (Assigned(LRangeArray)) then
              for LIndex2 := 0 to LRangeArray.Size - 1 do
              begin
                LRangeObject := GetJSONObject(LRangeArray, LIndex2);
                if (Assigned(LRangeObject)) then
                begin
                  LNewRange := TRange.Create(FHighlighter, Self);
                  LNewRange.LoadFromJSON(LRangeObject);
                  AddRange(LNewRange);
                end;
              end;
          end
          else if (LPair.JsonString.Value() = 'KeyList') then
          begin
            LKeyListArray := GetJSONArray(LSubRulesObject, LIndex, 'KeyList');
            if (Assigned(LKeyListArray)) then
            begin
              for LIndex2 := 0 to LKeyListArray.Size - 1 do
              begin
                LKeyListObject := GetJSONObject(LKeyListArray, LIndex2);
                if (Assigned(LKeyListObject)) then
                begin
                  LNewKeyList := TKeyList.Create();
                  LNewKeyList.LoadFromJSON(LKeyListObject);
                  AddKeyList(LNewKeyList);
                end;
              end
            end;
          end
          else if (LPair.JsonString.Value() = 'Set') then
          begin
            LSetArray := GetJSONArray(LSubRulesObject, LIndex);
            if (Assigned(LSetArray)) then
            begin
              for LIndex2 := 0 to LSetArray.Size - 1 do
              begin
                LSetObject := GetJSONObject(LSetArray, LIndex2);
                if (Assigned(LSetObject)) then
                begin
                  LNewSet := TSet.Create();
                  LNewSet.LoadFromJSON(LSetObject);
                  AddSet(LNewSet);
                end;
              end
            end;
          end;
        end;
      end;
    end;
  end;
end;

procedure TBCEditorHighlighter.TRange.Prepare(AParent: TRange);
var
  LBreakType: TBCEditorBreakType;
  LFirstChar: Char;
  LIndex: Integer;
  LIndex2: Integer;
  LLength: Integer;
  LSymbol: string;

  function InsertTokenDefault(AToken: TToken; ARules: TRange; AAttribute: TAttribute): TToken;
  begin
    Result := ARules.FindToken(AToken.Symbol);
    if not Assigned(Result) then
      Result := AToken
    else
      AToken.Free;
    ARules.AddToken(Result);
    if not Assigned(Result.Attribute) then
      Result.Attribute := AAttribute;
  end;

  procedure InsertToken(AToken: TToken; ARules: TRange);
  var
    LToken: TToken;
  begin
    LToken := ARules.FindToken(AToken.Symbol);
    if not Assigned(LToken) then
      ARules.AddToken(AToken)
    else
    begin
      LToken.Attribute := AToken.Attribute;
      AToken.Free;
    end;
  end;

var
  LAnsiChar: AnsiChar;
  LChar: Char;
  LKeyList: TKeyList;
  LRange: TRange;
  LSet: TSet;
  LTempToken: TToken;
  LToken: TToken;
begin
  Reset;
  FDefaultToken := TToken.Create(Attribute);
  if Assigned(FDefaultTermSymbol) then
  begin
    FDefaultTermSymbol.Free;
    FDefaultTermSymbol := nil;
  end;
  FDefaultTermSymbol := TDelimitersParser.Create(TToken.Create(Attribute));
  FDefaultSymbols := TDefaultParser.Create(TToken.Create(Attribute));

  FDelimiters := FDelimiters + BCEDITOR_ABSOLUTE_DELIMITERS;

  if Assigned(FRanges) then
  for LIndex := 0 to FRanges.Count - 1 do
  begin
    LRange := FRanges[LIndex];

    for LIndex2 := 0 to LRange.FOpenToken.SymbolCount - 1 do
    begin
      LTempToken := TToken.Create(LRange.OpenToken, LIndex2);
      LToken := InsertTokenDefault(LTempToken, Self, LRange.Attribute);
      LToken.OpenRule := LRange;

      LTempToken := TToken.Create(LRange.CloseToken, LIndex2);
      LToken.ClosingToken := InsertTokenDefault(LTempToken, LRange, LRange.Attribute);
    end;
    LRange.Prepare(Self);
  end;

  if Assigned(FKeyList) then
  for LIndex := 0 to FKeyList.Count - 1 do
  begin
    LKeyList := FKeyList[LIndex];

    for LIndex2 := 0 to LKeyList.KeyList.Count - 1 do
    begin
      LTempToken := TToken.Create(LKeyList.Attribute);
      LTempToken.Symbol := LKeyList.KeyList[LIndex2];
      InsertToken(LTempToken, Self);
    end;
  end;

  if Assigned(FTokens) then
  for LIndex := 0 to FTokens.Count - 1 do
  begin
    LTempToken := FTokens[LIndex];
    LLength := Length(LTempToken.Symbol);
    if LLength < 1 then
      Continue;

    LSymbol := LTempToken.Symbol;
    LFirstChar := LSymbol[1];

    if CharInSet(LFirstChar, FDelimiters) then
      LBreakType := btAny
    else
    if LTempToken.BreakType <> btUnspecified then
      LBreakType := LTempToken.BreakType
    else
      LBreakType := btTerm;

    LChar := CaseFunct(LFirstChar);
    if Ord(LChar) < 256 then
    begin
      LAnsiChar := AnsiChar(LChar);
      if not Assigned(SymbolParsers[LAnsiChar]) then
      begin
        if LLength = 1 then
          FSymbolParsers[LAnsiChar] := TParser.Create(LFirstChar, LTempToken, LBreakType)
        else
          FSymbolParsers[LAnsiChar] := TParser.Create(LFirstChar, FDefaultToken, LBreakType);
      end;
      if CharInSet(LSymbol[LLength], FDelimiters) then
        LBreakType := btAny;
      if LLength <> 1 then
      begin
        Assert(SymbolParsers[LAnsiChar] is TParser);
        TParser(SymbolParsers[LAnsiChar]).AddTokenNode(StringCaseFunct(Copy(LSymbol, 2, LLength - 1)), LTempToken,
          LBreakType);
      end;
    end;
  end;

  if Assigned(FSets) then
    if FSets.Count > 0 then
    for LIndex := 0 to 255 do
    begin
      LAnsiChar := AnsiChar(CaseFunct(Char(LIndex)));
      for LIndex2 := 0 to FSets.Count - 1 do
      begin
        LSet := FSets.List[LIndex2];
        if CharInSet(LAnsiChar, LSet.CharSet) then
          if not Assigned(SymbolParsers[LAnsiChar]) then
            FSymbolParsers[LAnsiChar] := TParser.Create(LSet)
          else
          begin
            Assert(SymbolParsers[LAnsiChar] is TParser);
            TParser(SymbolParsers[LAnsiChar]).AddSet(LSet);
          end;
      end;
    end;

  for LIndex := 0 to 255 do
  begin
    LAnsiChar := AnsiChar(LIndex);
    if not Assigned(SymbolParsers[LAnsiChar]) then
    begin
      if CharInSet(LAnsiChar, FDelimiters) then
        FSymbolParsers[LAnsiChar] := FDefaultTermSymbol
      else
        FSymbolParsers[LAnsiChar] := FDefaultSymbols;
    end;
  end;

  FPrepared := True;
end;

procedure TBCEditorHighlighter.TRange.Reset;
var
  LAnsiChar: AnsiChar;
  LIndex: Integer;
  LParser: TBaseParser;
begin
  if not FPrepared then
    Exit;

  for LIndex := 0 to 255 do
  begin
    LAnsiChar := AnsiChar(LIndex);
    LParser := SymbolParsers[LAnsiChar];
    if Assigned(LParser) and (LParser <> FDefaultTermSymbol) and (LParser <> FDefaultSymbols) then
      LParser.Free;
  end;

  FDefaultToken.Free;
  FDefaultToken := nil;
  FDefaultTermSymbol.Free;
  FDefaultTermSymbol := nil;
  FDefaultSymbols.Free;
  FDefaultSymbols := nil;

  if Assigned(FRanges) then
    for LIndex := 0 to FRanges.Count - 1 do
      FRanges[LIndex].Reset;

  FTokens.Clear();
  FPrepared := False;
end;

procedure TBCEditorHighlighter.TRange.SetCaseSensitive(const AValue: Boolean);
begin
  FCaseSensitive := AValue;
  if not AValue then
  begin
    FCaseFunct := UpCase;
    FStringCaseFunct := AnsiUpperCase;
  end
  else
  begin
    FCaseFunct := CaseNone;
    FStringCaseFunct := CaseStringNone;
  end;
end;

procedure TBCEditorHighlighter.TRange.SetDelimiters(const ADelimiters: TBCEditorAnsiCharSet);
var
  LIndex: Integer;
begin
  Delimiters := ADelimiters;
  for LIndex := 0 to RangeCount - 1 do
    Ranges[LIndex].SetDelimiters(ADelimiters);
end;

{ TBCEditorHighlighter.TComments **********************************************}

procedure TBCEditorHighlighter.TComments.AddChars(const AToken: string);
var
  LIndex: Integer;
begin
  for LIndex := 1 to Length(AToken) do
    FChars := FChars + [AToken[LIndex]];
end;

procedure TBCEditorHighlighter.TComments.AddBlockComment(const AOpenToken: string; const ACloseToken: string);
var
  LIndex: Integer;
  LLength: Integer;
begin
  LLength := Length(FBlockComments);

  for LIndex := 0 to LLength - 1 do
  begin
    if (FBlockComments[LIndex] = AOpenToken) and (FBlockComments[LIndex + 1] = ACloseToken) then
      Exit;
  end;

  SetLength(FBlockComments, LLength + 2);
  FBlockComments[LLength] := AOpenToken;
  FBlockComments[LLength + 1] := ACloseToken;

  AddChars(AOpenToken);
  AddChars(ACloseToken);
end;

procedure TBCEditorHighlighter.TComments.AddLineComment(const AToken: string);
var
  LIndex: Integer;
  LLength: Integer;
begin
  LLength := Length(FLineComments);

  for LIndex := 0 to LLength - 1 do
    if FLineComments[LIndex] = AToken then
      Exit;

  SetLength(FLineComments, LLength + 1);
  FLineComments[LLength] := AToken;

  AddChars(AToken);
end;

procedure TBCEditorHighlighter.TComments.Clear;
begin
  SetLength(FBlockComments, 0);
  SetLength(FLineComments, 0);
  FChars := [];
end;

destructor TBCEditorHighlighter.TComments.Destroy;
begin
  Clear;

  inherited Destroy;
end;

{ TBCEditorHighlighter.TParser ************************************************}

procedure TBCEditorHighlighter.TParser.AddSet(ASet: TSet);
begin
  Sets.Add(ASet);
end;

procedure TBCEditorHighlighter.TParser.AddTokenNode(const AString: string; AToken: TToken; ABreakType: TBCEditorBreakType);
var
  LChar: Char;
  LIndex: Integer;
  LLength: Integer;
  LTokenNode: TTokenNode;
  LTokenNodeList: TTokenNodeList;
begin
  LTokenNodeList := HeadNode.NextNodes;
  LTokenNode := nil;
  LLength := Length(AString);
  for LIndex := 1 to LLength do
  begin
    LChar := AString[LIndex];
    LTokenNode := LTokenNodeList.FindNode(LChar);
    if not Assigned(LTokenNode) then
    begin
      LTokenNode := TTokenNode.Create(LChar);
      LTokenNodeList.AddNode(LTokenNode);
    end;
    LTokenNodeList := LTokenNode.NextNodes;
  end;
  LTokenNode.BreakType := ABreakType;
  LTokenNode.Token := AToken;
end;

function TBCEditorHighlighter.TParser.GetToken(const ARange: TRange;
  const ALineText: PChar; const ALineLength: Integer;
  var AChar: Integer; out AToken: TToken): Boolean;
var
  LAllowedDelimiters: TBCEditorAnsiCharSet;
  LBeginChar: Integer;
  LCurrentTokenNode: TTokenNode;
  LFindTokenNode: TTokenNode;
  LIndex: Integer;
  LLinePos: PChar;
  LLineEndPos: PChar;
  LNextChar: Integer;
  LPreviousChar: Integer;
  LSet: TSet;
  LStartTokenNode: TTokenNode;
begin
  Result := False;

  LBeginChar := AChar;
  if Assigned(HeadNode) then
  begin
    LCurrentTokenNode := HeadNode;
    LNextChar := LBeginChar;
    LStartTokenNode := nil;
    repeat
      if Assigned(LStartTokenNode) then
      begin
        LCurrentTokenNode := LStartTokenNode;
        AChar := LNextChar;
        LStartTokenNode := nil;
      end;
      if Assigned(LCurrentTokenNode.Token) then
        LFindTokenNode := LCurrentTokenNode
      else
        LFindTokenNode := nil;
      LPreviousChar := AChar;
      while ((LCurrentTokenNode.NextNodes.Count > 0) and (AChar + 1 < ALineLength)) do
      begin
        Inc(AChar);
        LCurrentTokenNode := LCurrentTokenNode.NextNodes.FindNode(ARange.CaseFunct(ALineText[AChar]));
        if not Assigned(LCurrentTokenNode) then
        begin
          Dec(AChar);
          Break;
        end;

        if Assigned(LCurrentTokenNode.Token) then
        begin
          LFindTokenNode := LCurrentTokenNode;
          LPreviousChar := AChar;
        end;

        if not Assigned(LStartTokenNode) then
          if CharInSet(LCurrentTokenNode.Char, ARange.Delimiters) then
          begin
            LStartTokenNode := LCurrentTokenNode;
            LNextChar := AChar;
          end;
      end;

      AChar := LPreviousChar;

      if (not Assigned(LFindTokenNode) or not Assigned(LFindTokenNode.Token)
        or ((LFindTokenNode.Token.Attribute.EscapeChar <> BCEDITOR_NONE_CHAR)
          and (LBeginChar > 0)
          and (ALineText[LBeginChar - 1] = LFindTokenNode.Token.Attribute.EscapeChar))) then
        Continue;

      if (AChar <= ALineLength) then
        Inc(AChar);

      if ((LFindTokenNode.BreakType = btAny)
        or (AChar = ALineLength)
        or CharInSet(ALineText[AChar], ARange.Delimiters)) then
      begin
        AToken := LFindTokenNode.Token;
        Exit(True);
      end;
    until not Assigned(LStartTokenNode);
  end;

  LAllowedDelimiters := ARange.Delimiters;
  for LIndex := 0 to Sets.Count - 1 do
    LAllowedDelimiters := LAllowedDelimiters - TSet(Sets.List[LIndex]).CharSet;

  if (AChar < ALineLength) then
    for LIndex := 0 to Sets.Count - 1 do
    begin
      AChar := LBeginChar;
      LSet := TSet(Sets.List[LIndex]);
      LLinePos := @ALineText[AChar];
      LLineEndPos := @ALineText[ALineLength];
      repeat
        Inc(AChar);
        Inc(LLinePos);
      until (not CharInSet(LLinePos^, LSet.CharSet) or (LLinePos = LLineEndPos));

      if CharInSet(LLinePos^, LAllowedDelimiters) then
      begin
        AToken := TToken.Create(LSet.Attribute);
        AToken.Temporary := True;
        Exit(True);
      end;
    end;
  AChar := LBeginChar + 1;
end;

constructor TBCEditorHighlighter.TParser.Create(AChar: Char; AToken: TToken; ABreakType: TBCEditorBreakType);
begin
  inherited Create;

  FHeadNode := TTokenNode.Create(AChar, AToken, ABreakType);
  FSets := TList.Create;
end;

constructor TBCEditorHighlighter.TParser.Create(ASet: TSet);
begin
  inherited Create;

  FSets := TList.Create;
  AddSet(ASet);
end;

destructor TBCEditorHighlighter.TParser.Destroy;
begin
  if Assigned(FHeadNode) then
  begin
    FHeadNode.Free;
    FHeadNode := nil;
  end;
  FSets.Clear;
  FSets.Free;
  FSets := nil;
  inherited;
end;

{ TBCEditorHighlighter.TDefaultParser *****************************************}

constructor TBCEditorHighlighter.TDefaultParser.Create(AToken: TToken);
begin
  FToken := AToken;
end;

destructor TBCEditorHighlighter.TDefaultParser.Destroy;
begin
  FToken.Free;
  FToken := nil;
  inherited;
end;

function TBCEditorHighlighter.TDefaultParser.GetToken(const ARange: TRange;
  const ALineText: PChar; const ALineLength: Integer;
  var AChar: Integer; out AToken: TToken): Boolean;
begin
  Inc(AChar);
  Result := False;
end;

{ TBCEditorHighlighter.TDelimitersParser **************************************}

constructor TBCEditorHighlighter.TDelimitersParser.Create(AToken: TToken);
begin
  inherited Create;
  FToken := AToken;
end;

destructor TBCEditorHighlighter.TDelimitersParser.Destroy;
begin
  FToken.Free;
  FToken := nil;
  inherited;
end;

function TBCEditorHighlighter.TDelimitersParser.GetToken(const ARange: TRange;
  const ALineText: PChar; const ALineLength: Integer;
  var AChar: Integer; out AToken: TToken): Boolean;
begin
  if (AChar < ALineLength) then
    Inc(AChar);
  AToken := FToken;
  Result := True;
end;

{ TBCEditorHighlighter.TDetection *********************************************}

procedure TBCEditorHighlighter.TDetection.Clear();
begin
  FDefaultExtension := '';
  FExtensions.Clear();
  FFirstLinePattern := '';
end;

constructor TBCEditorHighlighter.TDetection.Create();
begin
  inherited Create();

  FExtensions := TStringList.Create();
  Clear();
end;

destructor TBCEditorHighlighter.TDetection.Destroy();
begin
  FExtensions.Free();

  inherited;
end;

procedure TBCEditorHighlighter.TDetection.LoadFromJSON(const AJSON: TJSONObject);
var
  LExtensionsArray: TJSONArray;
  LIndex: Integer;
  LString: string;
begin
  Clear();

  if (Assigned(AJSON)) then
  begin
    FDefaultExtension := GetJSONString(AJSON, 'Default');
    FFirstLinePattern := GetJSONString(AJSON, 'FirstLinePattern');

    LExtensionsArray := GetJSONArray(AJSON, 'Extensions');
    if (Assigned(LExtensionsArray)) then
      for LIndex := 0 to LExtensionsArray.Size - 1 do
      begin
        LString := GetJSONString(LExtensionsArray, LIndex);
        if ((LString <> '') and (LString[1] = '.')) then
          FExtensions.Add(LString);
      end;
  end;
end;

{ TBCEditorHighlighter ********************************************************}

procedure TBCEditorHighlighter.AddAllAttributes(ARange: TRange);
var
  LIndex: Integer;
begin
  AddAttribute(ARange.Attribute);
  for LIndex := 0 to ARange.KeyListCount - 1 do
    AddAttribute(ARange.KeyList[LIndex].Attribute);
  for LIndex := 0 to ARange.SetCount - 1 do
    AddAttribute(ARange.Sets[LIndex].Attribute);
  for LIndex := 0 to ARange.RangeCount - 1 do
    AddAllAttributes(ARange.Ranges[LIndex]);
end;

procedure TBCEditorHighlighter.AddKeyChar(AKeyCharType: TBCEditorKeyCharType; AChar: Char);
begin
  case AKeyCharType of
    ctFoldOpen: FFoldOpenKeyChars := FFoldOpenKeyChars + [AChar];
    ctFoldClose: FFoldCloseKeyChars := FFoldCloseKeyChars + [AChar];
    ctSkipOpen: FSkipOpenKeyChars := FSkipOpenKeyChars + [AChar];
    ctSkipClose: FSkipCloseKeyChars := FSkipCloseKeyChars + [AChar];
  end;
end;

procedure TBCEditorHighlighter.AddKeywords(var AStringList: TStringList);
var
  LIndex: Integer;
  LIndex2: Integer;
begin
  if not Assigned(AStringList) then
    Exit;
  for LIndex := 0 to FMainRules.KeyListCount - 1 do
    for LIndex2 := 0 to FMainRules.KeyList[LIndex].KeyList.Count - 1 do
      AStringList.Add(FMainRules.KeyList[LIndex].KeyList[LIndex2]);
end;

procedure TBCEditorHighlighter.AddAttribute(AHighlighterAttribute: TAttribute);
begin
  FAttributes.AddObject(AHighlighterAttribute.Name, AHighlighterAttribute);
end;

procedure TBCEditorHighlighter.Clear();
begin
  FAttributes.Clear;
  FCodeFoldingRegions.Clear();
  FComments.Clear();
  FCompletionProposalSkipRegions.Clear();
  FFoldOpenKeyChars := [];
  FFoldCloseKeyChars := [];
  FMainRules.Clear();
  FMatchingPairs.Clear();
  FSample := '';
  FSkipOpenKeyChars := [];
  FSkipCloseKeyChars := [];
  DoChange();
end;

constructor TBCEditorHighlighter.Create(const AEditor: TCustomControl);
begin
  inherited Create();

  FEditor := AEditor;
  FWordBreakChars := BCEDITOR_WORD_BREAK_CHARACTERS;

  FAttributes := TStringList.Create;
  FAttributes.Duplicates := dupIgnore;
  FAttributes.Sorted := False;

  FCodeFoldingRegions := TBCEditorCodeFoldingRegions.Create();

  FComments := TComments.Create;

  FCompletionProposalSkipRegions := TBCEditorCodeFoldingSkipRegions.Create(TBCEditorCodeFoldingSkipRegions.TItem);

  FMainRules := TRange.Create(Self);
  FMainRules.Parent := FMainRules;

  FColors := TElements.Create(Self);
  FMatchingPairs := TList<TMatchingPairToken>.Create();
  FMatchingPairHighlight := True;

  FAllDelimiters := BCEDITOR_DEFAULT_DELIMITERS + BCEDITOR_ABSOLUTE_DELIMITERS;

  FDetection := TDetection.Create();
end;

procedure TBCEditorHighlighter.DoChange();
begin
  if (Assigned(FOnChange)) then
    FOnChange(Self);
end;

destructor TBCEditorHighlighter.Destroy();
begin
  Clear();

  FCodeFoldingRegions.Free();
  FComments.Free();
  FDetection.Free();
  FMainRules.Free();
  FAttributes.Free();
  FCompletionProposalSkipRegions.Free();
  FMatchingPairs.Free();
  FColors.Free();

  inherited;
end;

procedure TBCEditorHighlighter.FindClose(const AFind: TTokenFind);
begin
  if (Assigned(AFind.FToken) and AFind.FToken.Temporary) then
    AFind.FToken.Free();
end;

function TBCEditorHighlighter.FindFirstToken(const ABeginRange: TRange;
  const AText: PChar; const ALength, AFirstChar: Integer; out AFind: TTokenFind): Boolean;
begin
  if (not Assigned(AText) or (ALength = 0)) then
    Result := False
  else
  begin
    AFind.FChar := AFirstChar;
    AFind.FLineChar := 0;
    AFind.FLineText := AText;
    AFind.FLineLength := ALength;
    if (not Assigned(ABeginRange)) then
      AFind.FRange := MainRules
    else
      AFind.FRange := ABeginRange;
    AFind.FLength := 0;
    AFind.FToken := nil;

    if (Assigned(AFind.FRange) and not AFind.FRange.Prepared) then
    begin
      FAttributes.Clear();
      AddAllAttributes(MainRules);
      FMainRules.Prepare(FMainRules);
    end;

    Result := FindNextToken(AFind);
  end;
end;

function TBCEditorHighlighter.FindNextToken(var AFind: TTokenFind): Boolean;
var
  LCloseParent: Boolean;
  LChar: Integer;
  LDelimiters: TBCEditorAnsiCharSet;
  LIndex: Integer;
  LKeywordText: string;
  LParser: TBaseParser;
begin
  Result := AFind.FLineChar + AFind.FLength < AFind.FLineLength;

  if (Result) then
  begin
    Inc(AFind.FChar, AFind.FLength);
    Inc(AFind.FLineChar, AFind.FLength);
    AFind.FText := @AFind.FLineText[AFind.FLineChar];

    if (Assigned(AFind.FToken) and AFind.FToken.Temporary) then
    begin
      AFind.FToken.Free();
      AFind.FToken := nil;
    end;

    if (Assigned(AFind.FRange) and (AFind.FRange.AlternativeCloseList.Count > 0)) then
      for LIndex := 0 to AFind.FRange.AlternativeCloseList.Count - 1 do
      begin
        LKeywordText := AFind.FRange.AlternativeCloseList[LIndex];
        if ((AFind.FLineLength - AFind.FLineChar >= Length(LKeywordText))
          and (StrLComp(@AFind.FLineText[AFind.FLineChar], PChar(LKeywordText), Length(LKeywordText)) = 0)) then
        begin
          AFind.FRange := AFind.FRange.Parent;
          Break;
        end;
      end;

    if Assigned(AFind.FRange) then
    begin
      LChar := AFind.FLineChar;

      LCloseParent := AFind.FRange.CloseParent;
      if (AFind.FRange.CloseOnTerm and CharInSet(AFind.FLineText[LChar], AFind.FRange.Delimiters) and
        not (AFind.FRange.SkipWhitespace and CharInSet(AFind.FLineText[LChar], BCEDITOR_ABSOLUTE_DELIMITERS))) then
      begin
        AFind.FRange := AFind.FRange.Parent;
        if Assigned(AFind.FRange) then
          if LCloseParent then
            AFind.FRange := AFind.FRange.Parent;
      end;

      if (Ord(AFind.FLineText[LChar]) < 256) then
        LParser := AFind.FRange.SymbolParsers[AnsiChar(AFind.FRange.CaseFunct(AFind.FLineText[LChar]))]
      else
        LParser := AFind.FRange.SymbolParsers['a'];

      if (not Assigned(LParser)) then
        Inc(LChar)
      else if (not LParser.GetToken(AFind.FRange, AFind.FLineText, AFind.FLineLength, LChar, AFind.FToken)) then
      begin
        AFind.FToken := AFind.FRange.DefaultToken;

        if (AFind.FRange.UseDelimitersForText) then
          LDelimiters := AFind.FRange.Delimiters
        else
          LDelimiters := FAllDelimiters;

        if (Ord(AFind.FLineText[LChar - 1]) < 256) then
          while ((LChar < AFind.FLineLength)
            and (Ord(AFind.FLineText[LChar]) < 256)
            and not CharInSet(AFind.FLineText[LChar], LDelimiters)) do
            Inc(LChar)
        else
          while ((LChar < AFind.FLineLength)
            and (Ord(AFind.FLineText[LChar]) >= 256)
            and not CharInSet(AFind.FLineText[LChar], LDelimiters)) do
            Inc(LChar)
      end
      else if (AFind.FRange.ClosingToken = AFind.FToken) then
        AFind.FRange := AFind.FRange.Parent
      else if (Assigned(AFind.FToken) and Assigned(AFind.FToken.OpenRule) and (AFind.FToken.OpenRule is TRange)) then
      begin
        AFind.FRange := TRange(AFind.FToken.OpenRule);
        AFind.FRange.ClosingToken := AFind.FToken.ClosingToken;
        if (AFind.FRange.OpenBeginningOfLine and (LChar > 0)) then
        begin
          AFind.FRange := AFind.FRange.Parent;
          AFind.FToken := AFind.FRange.DefaultToken;
        end;
      end;
    end;

    AFind.FLength := LChar - AFind.FLineChar;
    if (not Assigned(AFind.FToken)) then
      AFind.FAttribute := nil
    else
      AFind.FAttribute := AFind.FToken.Attribute;
  end;
end;

function TBCEditorHighlighter.GetAttribute(AIndex: Integer): TAttribute;
begin
  Result := nil;
  if (AIndex >= 0) and (AIndex < FAttributes.Count) then
    Result := TAttribute(FAttributes.Objects[AIndex]);
end;

procedure TBCEditorHighlighter.LoadFromFile(const AFilename: string);
var
  LStream: TStream;
begin
  FFilename := TPath.GetFullPath(AFilename);
  FDirectory := IncludeTrailingPathDelimiter(TPath.GetDirectoryName(FFilename));
  FName := TPath.GetFileNameWithoutExtension(AFilename);

  LStream := TFileStream.Create(AFilename, fmOpenRead);
  LoadFromStream(LStream);
  LStream.Free();
end;

procedure TBCEditorHighlighter.LoadFromJSON(const AJSON: TJSONObject);
var
  LCodeFoldingObject: TJSONObject;
  LCodeFoldingRangeObject: TJSONObject;
  LCodeFoldingRangesArray: TJSONArray;
  LCodeFoldingRegion: TBCEditorCodeFoldingRegion;
  LHighlighterObject: TJSONObject;
  LIndex: Integer;
  LSampleArray: TJSONArray;
begin
  Clear();

  if (Assigned(AJSON)) then
  begin
    LSampleArray := GetJSONArray(AJSON, 'Sample');
    if (Assigned(LSampleArray)) then
      for LIndex := 0 to LSampleArray.Size - 1 do
      begin
        if (LIndex > 0) then FSample := FSample + #13#10;
        FSample := FSample + GetJSONString(LSampleArray, LIndex);
      end;

    LHighlighterObject := GetJSONObject(AJSON, 'Highlighter');
    FMainRules.LoadFromJSON(GetJSONObject(LHighlighterObject, 'MainRules'));

    LCodeFoldingObject := GetJSONObject(AJSON, 'CodeFolding');
    if (Assigned(LCodeFoldingObject)) then
    begin
      LCodeFoldingRangesArray := GetJSONArray(LCodeFoldingObject, 'Ranges');
      if (Assigned(LCodeFoldingRangesArray)) then
        for LIndex := 0 to LCodeFoldingRangesArray.Size - 1 do
        begin
          LCodeFoldingRangeObject := GetJSONObject(LCodeFoldingRangesArray, LIndex);
          if (Assigned(LCodeFoldingRangeObject)) then
          begin
            LCodeFoldingRegion := TBCEditorCodeFoldingRegion.Create(Self, TBCEditorCodeFoldingRegionItem);
            LCodeFoldingRegion.LoadFromJSON(LCodeFoldingRangeObject);
            LCodeFoldingRegion.LoadSkipRegionFromJSON(LCodeFoldingRangeObject);
            LCodeFoldingRegion.LoadFoldRegionFromJSON(LCodeFoldingRangeObject);
            FCodeFoldingRegions.Add(LCodeFoldingRegion);
          end;
        end;
    end;

    LoadMatchingPairFromJSON(GetJSONObject(AJSON, 'MatchingPair'));
    LoadCompletionProposalFromJSON(GetJSONObject(AJSON, 'CompletionProposal'));

    FDetection.LoadFromJSON(GetJSONObject(AJSON, 'Detection'));
  end;

  UpdateColors();
end;

procedure TBCEditorHighlighter.LoadFromResource(const AResourceName: string; const AResourceType: PChar);
var
  LStream: TResourceStream;
begin
  LStream := TResourceStream.Create(HInstance, PChar(AResourceName), AResourceType);
  LoadFromStream(LStream);
  LStream.Free();
end;

procedure TBCEditorHighlighter.LoadFromStream(const AStream: TStream);
var
  LJSON: TJSONObject;
begin
  LJSON := CreateJSONObjectFromStream(AStream);
  if (Assigned(LJSON)) then
  begin
    LoadFromJSON(LJSON);
    LJSON.Free();
  end;
end;

procedure TBCEditorHighlighter.LoadCompletionProposalFromJSON(const AJSON: TJSONObject);
var
  LCloseToken: string;
  LCompletionProposalObject: TJSONObject;
  LFilename: string;
  LIndex: Integer;
  LItem: TJSONObject;
  LJSONObject: TJSONObject;
  LOpenToken: string;
  LSkipRegionArray: TJSONArray;
  LSkipRegionItem: TBCEditorCodeFoldingSkipRegions.TItem;
begin
  if (Assigned(AJSON)) then
  begin
    { Skip regions }
    LSkipRegionArray := GetJSONArray(AJSON, 'SkipRegion');
    for LIndex := 0 to LSkipRegionArray.Size - 1 do
    begin
      LItem := GetJSONObject(LSkipRegionArray, LIndex);

      LOpenToken := GetJSONString(LItem, 'OpenToken');
      LCloseToken := GetJSONString(LItem, 'CloseToken');

      if (MultiHighlighter) then
      begin
        { Multi highlighter code folding skip region include }
        LFilename := GetJSONString(LItem, 'File');
        if (TFile.Exists(Directory + LFilename)) then
        begin
          LJSONObject := CreateJSONObjectFromFile(Directory + LFilename);
          if (Assigned(LJSONObject)) then
          begin
            LCompletionProposalObject := GetJSONObject(LItem, 'CompletionProposal');
            LoadCompletionProposalFromJSON(LCompletionProposalObject);
            LJSONObject.Free();
          end;
        end;
        { Skip duplicates }
        if (CompletionProposalSkipRegions.Contains(LOpenToken, LCloseToken)) then
          Continue;
      end;

      LSkipRegionItem := CompletionProposalSkipRegions.Add(LOpenToken, LCloseToken);
      LSkipRegionItem.RegionType := StrToRegionType(GetJSONString(LItem, 'RegionType'));
      LSkipRegionItem.SkipEmptyChars := GetJSONBoolean(LItem, 'SkipEmptyChars', LSkipRegionItem.SkipEmptyChars);
    end;
  end;
end;

procedure TBCEditorHighlighter.LoadMatchingPairFromJSON(const AJSON: TJSONObject);
var
  LFilename: string;
  LIndex: Integer;
  LItem: TJSONObject;
  LJSONObject: TJSONObject;
  LMatchingPairObject: TJSONObject;
  LPairsArray: TJSONArray;
  LTokenMatch: TMatchingPairToken;
begin
  if (Assigned(AJSON)) then
  begin
    { Matching token pairs }
    LPairsArray := GetJSONArray(AJSON, 'Pairs');
    if (Assigned(LPairsArray)) then
      for LIndex := 0 to LPairsArray.Size - 1 do
      begin
        LItem := GetJSONObject(LPairsArray, LIndex);

        if (MultiHighlighter) then
        begin
          { Multi highlighter code folding fold region include }
          LFilename := GetJSONString(LItem, 'File');
          if (TFile.Exists(Directory + LFilename)) then
          begin
            LJSONObject := CreateJSONObjectFromFile(Directory + LFilename);
            if (Assigned(LJSONObject)) then
            begin
              LMatchingPairObject := GetJSONObject(LJSONObject, 'MatchingPair');
              LoadMatchingPairFromJSON(LMatchingPairObject);
              LJSONObject.Free;
            end;
          end;
        end;

        LTokenMatch.OpenToken := GetJSONString(LItem, 'OpenToken');
        LTokenMatch.CloseToken := GetJSONString(LItem, 'CloseToken');
        if ((LTokenMatch.OpenToken <> '') and (LTokenMatch.CloseToken <> '')) then
          MatchingPairs.Add(LTokenMatch)
      end;
  end;
end;

procedure TBCEditorHighlighter.SetWordBreakChars(AChars: TBCEditorAnsiCharSet);
begin
  FWordBreakChars := AChars;
end;

procedure TBCEditorHighlighter.UpdateAttributes(ARange: TRange; AParentRange: TRange);
var
  LIndex: Integer;

  procedure SetAttributes(AAttribute: TAttribute; AParentRange: TRange);
  var
    LElement: PElement;
  begin
    LElement := FColors[AAttribute.Element];

    if AAttribute.ParentBackground and Assigned(AParentRange) then
      AAttribute.Background := AParentRange.Attribute.Background
    else
    if Assigned(LElement) then
      AAttribute.Background := LElement^.Background;
    if AAttribute.ParentForeground and Assigned(AParentRange) then
      AAttribute.Foreground := AParentRange.Attribute.Foreground
    else
    if Assigned(LElement) then
      AAttribute.Foreground := LElement^.Foreground;
    if Assigned(LElement) then
      AAttribute.FontStyles := LElement^.FontStyles;
  end;

begin
  SetAttributes(ARange.Attribute, AParentRange);

  for LIndex := 0 to ARange.KeyListCount - 1 do
    SetAttributes(ARange.KeyList[LIndex].Attribute, ARange);
  for LIndex := 0 to ARange.SetCount - 1 do
    SetAttributes(ARange.Sets[LIndex].Attribute, ARange);

  if ARange.RangeCount > 0 then
  for LIndex := 0 to ARange.RangeCount - 1 do
    UpdateAttributes(ARange.Ranges[LIndex], ARange);
end;

procedure TBCEditorHighlighter.UpdateColors();
var
  LFontDummy: TFont;
begin
  UpdateAttributes(MainRules, nil);
  DoChange();
  if (Assigned(Editor)) then
  begin
    LFontDummy := TFont.Create;
    try
      LFontDummy.Name := TCustomBCEditor(Editor).Font.Name;
      LFontDummy.Size := TCustomBCEditor(Editor).Font.Size;
      TCustomBCEditor(Editor).Font.Assign(LFontDummy);
    finally
      LFontDummy.Free;
    end;
  end;
end;

end.


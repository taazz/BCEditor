unit BCEditor.Types;

interface {********************************************************************}

uses
  SysUtils, Types,
  Graphics, Controls,
  BCEditor.Consts;

type
  TBCEditorArrayOfString = array of string;
  TBCEditorArrayOfSingle = array of Single;

  TBCEditorCaretChangedEvent = procedure(ASender: TObject; ACaretPos: TPoint) of object;

  TBCEditorMarksPanelClick = procedure(ASender: TObject; const ALine: Integer) of object;
  TBCEditorHintEvent = procedure(ASender: TObject; const AX, AY: Integer; const APos: TPoint; const AIndex: Integer; var AHint: string) of object;

  TBCEditorReplaceAction = (raCancel, raSkip, raReplace, raReplaceAll);

  TBCEditorOption = (
    eoAutoIndent, { Will indent the caret on new lines with the same amount of leading white space as the preceding line }
    eoBeyondEndOfFile, { Allows the cursor to go beyond the end of file into the white space }
    eoBeyondEndOfLine, { Allows the cursor to go beyond the last character into the white space }
    eoAcceptFiles, { Allows the editor accept OLE file drops }
    eoMiddleClickScrolling, { Scrolling by mouse move after wheel click. }
    eoTrimTrailingLines, { Empty lines at the end of text will be removed while saving }
    eoTrimTrailingSpaces { Spaces at the end of lines will be removed while saving }
  );
  TBCEditorOptions = set of TBCEditorOption;

  TBCEditorTextEntryMode = (temInsert, temOverwrite);

  TBCEditorTabOption = (
    toPreviousLineIndent,
    toSelectedBlockIndent,
    toTabsToSpaces
  );

  TBCEditorSelectionOption = (
    soExpandRealNumbers,
    soTermsCaseSensitive,
    soToEndOfLine,
    soTripleClickLineSelect
  );

  TBCEditorSearchEvent = (
    seInvalidate,
    seChange
  );

  TBCEditorReplaceChanges = (
    rcEngineUpdate
  );

  TBCEditorSearchOption = (
    soBackwards,
    soCaseSensitive,
    soEntireScope,
    soHighlightResults,
    soSearchOnTyping,
    soWholeWordsOnly,
    soWrapAround
  );
  TBCEditorSyncEditOption = (
    seoButton,
    seoCaseSensitive
  );
  TBCEditorSyncEditOptions = set of TBCEditorSyncEditOption;

  TBCEditorReplaceOption = (
    roBackwards,
    roCaseSensitive,
    roEntireScope,
    roPrompt,
    roReplaceAll,
    roSelectedOnly,
    roWholeWordsOnly
  );

  TBCEditorSearchEngine = (
    seNormal,
    seRegularExpression
  );

  TBCEditorSearchMapOption = (
    moShowActiveLine
  );

  TBCEditorCompletionProposalOption = (
    cpoAutoInvoke,
    cpoAutoConstraints,
    cpoAddHighlighterKeywords,
    cpoCaseSensitive,
    cpoFiltered,
    cpoParseItemsFromText,
    cpoResizeable,
    cpoShowShadow,
    cpoUseHighlighterColumnFont
  );

  TBCEditorLeftMarginBookMarkPanelOption = (
    bpoToggleBookmarkByClick,
    bpoToggleMarkByClick
  );

  PBCEditorLinesPosition = ^TBCEditorLinesPosition;
  TBCEditorLinesPosition = packed record
    Char: Integer;
    Line: Integer;
    class operator Equal(a, b: TBCEditorLinesPosition): Boolean; inline;
    class operator GreaterThan(a, b: TBCEditorLinesPosition): Boolean; inline;
    class operator GreaterThanOrEqual(a, b: TBCEditorLinesPosition): Boolean; inline;
    class operator Implicit(a: TBCEditorLinesPosition): TPoint; inline;
    class operator Implicit(a: TPoint): TBCEditorLinesPosition; inline;
    class operator LessThan(a, b: TBCEditorLinesPosition): Boolean; inline;
    class operator LessThanOrEqual(a, b: TBCEditorLinesPosition): Boolean; inline;
    class operator NotEqual(a, b: TBCEditorLinesPosition): Boolean; inline;
    function ToString(): string; inline;
  end;

  TBCEditorLinesArea = record
    BeginPosition: TBCEditorLinesPosition;
    EndPosition: TBCEditorLinesPosition;
    function Contains(Position: TBCEditorLinesPosition): Boolean; inline;
    class operator Equal(a, b: TBCEditorLinesArea): Boolean; inline;
    function IntersectWith(a: TBCEditorLinesArea): Boolean; inline;
    function IsEmpty(): Boolean; inline;
    class operator NotEqual(a, b: TBCEditorLinesArea): Boolean; inline;
    function ToString(): string; inline;
    class function Union(const a, b: TBCEditorLinesArea): TBCEditorLinesArea; overload; static;
    procedure Union(const a: TBCEditorLinesArea); overload;
  end;

  TBCEditorRowsPosition = packed record
    Column: Integer;
    Row: Integer;
    class operator Equal(a, b: TBCEditorRowsPosition): Boolean; inline;
    class operator NotEqual(a, b: TBCEditorRowsPosition): Boolean; inline;
    function ToString(): string; inline;
  end;

  TBCEditorBreakType = (
    btUnspecified,
    btAny,
    btTerm
  );
  TBCEditorRangeType = (
    ttUnspecified,
    ttAddress,
    ttAssemblerComment,
    ttAssemblerReservedWord,
    ttAttribute,
    ttBlockComment,
    ttCharacter,
    ttDirective,
    ttHexNumber,
    ttHighlightedBlock,
    ttHighlightedBlockSymbol,
    ttLineComment,
    ttMethod,
    ttMethodName,
    ttNumber,
    ttReservedWord,
    ttString,
    ttSymbol
  );


  TBCEditorKeyPressWEvent = procedure(ASender: TObject; var AKey: Char) of object;

  TBCEditorContextHelpEvent = procedure(ASender: TObject; AWord: string) of object;

  TBCEditorMouseCursorEvent = procedure(ASender: TObject; const ALineCharPos: TBCEditorLinesPosition; var ACursor: TCursor) of object;

  TBCEditorTabConvertProc = function(const ALine: string; ATabWidth: Integer; var AHasTabs: Boolean;
    const ATabChar: Char = BCEDITOR_SPACE_CHAR): string;

  TBCEditorLeftMarginLineNumberOption = (
    lnoIntens,
    lnoAfterLastLine
  );

  TBCEditorSearchMapAlign = (saLeft, saRight);

  TBCEditorUndoOption = (
    uoGroupUndo,
    uoUndoAfterLoad,
    uoUndoAfterSave
  );

  TBCEditorCase = (cNone = -1, cUpper = 0, cLower = 1, cOriginal = 2);

  TBCEditorKeyCharType = (ctFoldOpen, ctFoldClose, ctSkipOpen, ctSkipClose);

  TBCEditorSortOrder = (soAsc, soDesc);

  TBCEditorCodeFoldingChanges = (fcEnabled, fcRefresh, fcRescan);

  TBCEditorCodeFoldingOption = (
    cfoFoldMultilineComments,
    cfoHighlightFoldingLine,
    cfoHighlightIndentGuides,
    cfoShowTreeLine
  );

  TBCEditorCodeFoldingOptions = set of TBCEditorCodeFoldingOption;
  TBCEditorCompletionProposalOptions = set of TBCEditorCompletionProposalOption;
  TBCEditorLeftMarginLineNumberOptions = set of TBCEditorLeftMarginLineNumberOption;
  TBCEditorSearchOptions = set of TBCEditorSearchOption;
  TBCEditorSearchMapOptions = set of TBCEditorSearchMapOption;
  TBCEditorLeftMarginBookMarkPanelOptions = set of TBCEditorLeftMarginBookMarkPanelOption;
  TBCEditorReplaceOptions = set of TBCEditorReplaceOption;
  TBCEditorSelectionOptions = set of TBCEditorSelectionOption;
  TBCEditorTabOptions = set of TBCEditorTabOption;
  TBCEditorUndoOptions = set of TBCEditorUndoOption;

  TBCEditorRangeItemType = (ritUnspecified, ritMultiLineString, ritSingleLineString, ritMultiLineComment, ritSingleLineComment);

function Max(const A, B: TBCEditorLinesPosition): TBCEditorLinesPosition; overload; inline;
function Min(const A, B: TBCEditorLinesPosition): TBCEditorLinesPosition; overload; inline;
function LinesArea(const ABeginPosition, AEndPosition: TBCEditorLinesPosition): TBCEditorLinesArea; inline;
function LinesPosition(const AChar, ALine: Integer): TBCEditorLinesPosition; overload; inline;
function RowsPosition(const AColumn, ARow: Integer): TBCEditorRowsPosition; inline;

const
  InvalidLinesArea: TBCEditorLinesArea = ( BeginPosition: ( Char: -1; Line: -1; ); EndPosition: ( Char: -1; Line: -1; ) );
  InvalidLinesPosition: TBCEditorLinesPosition = ( Char: -1; Line: -1; );
  InvalidRowsPosition: TBCEditorRowsPosition = ( Column: -1; Row: -1; );

implementation {***************************************************************}

function Max(const A, B: TBCEditorLinesPosition): TBCEditorLinesPosition;
begin
  if (A > B) then
    Result := A
  else
    Result := B;
end;

function Min(const A, B: TBCEditorLinesPosition): TBCEditorLinesPosition;
begin
  if (A < B) then
    Result := A
  else
    Result := B;
end;

function LinesArea(const ABeginPosition, AEndPosition: TBCEditorLinesPosition): TBCEditorLinesArea;
begin
  Result.BeginPosition := ABeginPosition;
  Result.EndPosition := AEndPosition;
end;

function LinesPosition(const AChar, ALine: Integer): TBCEditorLinesPosition;
begin
  Result.Char := AChar;
  Result.Line := ALine;
end;

function Point(const APosition: TBCEditorLinesPosition): TPoint;
begin
  Result.X := APosition.Char;
  Result.Y := APosition.Line;
end;

function RowsPosition(const AColumn, ARow: Integer): TBCEditorRowsPosition;
begin
  Result.Column := AColumn;
  Result.Row := ARow;
end;

{ TBCEditorRowsPosition *******************************************************}

class operator TBCEditorRowsPosition.Equal(a, b: TBCEditorRowsPosition): Boolean;
begin
  Result := (a.Row = b.Row) and (a.Column = b.Column);
end;

class operator TBCEditorRowsPosition.NotEqual(a, b: TBCEditorRowsPosition): Boolean;
begin
  Result := (a.Row <> b.Row) or (a.Column <> b.Column);
end;

function TBCEditorRowsPosition.ToString(): string;
begin
  Result := '(' + IntToStr(Column) + ',' + IntToStr(Row) + ')';
end;

{ TBCEditorLinesPosition ******************************************************}

class operator TBCEditorLinesPosition.Equal(a, b: TBCEditorLinesPosition): Boolean;
begin
  Result := (a.Line = b.Line) and (a.Char = b.Char);
end;

class operator TBCEditorLinesPosition.GreaterThan(a, b: TBCEditorLinesPosition): Boolean;
begin
  Result := (a.Line > b.Line) or (a.Line = b.Line) and (a.Char > b.Char);
end;

class operator TBCEditorLinesPosition.GreaterThanOrEqual(a, b: TBCEditorLinesPosition): Boolean;
begin
  Result := (a.Line > b.Line) or (a.Line = b.Line) and (a.Char >= b.Char);
end;

class operator TBCEditorLinesPosition.Implicit(a: TBCEditorLinesPosition): TPoint;
begin
  Result.X := a.Char;
  Result.Y := a.Line;
end;

class operator TBCEditorLinesPosition.Implicit(a: TPoint): TBCEditorLinesPosition;
begin
  Result.Char := a.X;
  Result.Line := a.Y;
end;

class operator TBCEditorLinesPosition.LessThan(a, b: TBCEditorLinesPosition): Boolean;
begin
  Result := (a.Line < b.Line) or (a.Line = b.Line) and (a.Char < b.Char);
end;

class operator TBCEditorLinesPosition.LessThanOrEqual(a, b: TBCEditorLinesPosition): Boolean;
begin
  Result := (a.Line < b.Line) or (a.Line = b.Line) and (a.Char <= b.Char);
end;

class operator TBCEditorLinesPosition.NotEqual(a, b: TBCEditorLinesPosition): Boolean;
begin
  Result := (a.Line <> b.Line) or (a.Char <> b.Char);
end;

function TBCEditorLinesPosition.ToString(): string;
begin
  Result := '(' + IntToStr(Char) + ',' + IntToStr(Line) + ')';
end;

{ TBCEditorLinesArea **********************************************************}

function TBCEditorLinesArea.Contains(Position: TBCEditorLinesPosition): Boolean;
begin
  Result := (BeginPosition <= Position) and (Position < EndPosition);
end;

class operator TBCEditorLinesArea.Equal(a, b: TBCEditorLinesArea): Boolean;
begin
  Result := (a.BeginPosition = b.BeginPosition) and (a.EndPosition = b.EndPosition);
end;

function TBCEditorLinesArea.IntersectWith(a: TBCEditorLinesArea): Boolean;
begin
  Result := (Self.BeginPosition <= a.EndPosition) and (Self.EndPosition >= a.BeginPosition);
end;

function TBCEditorLinesArea.IsEmpty(): Boolean;
begin
  Result := (BeginPosition = EndPosition) or (EndPosition = InvalidLinesPosition);
end;

class operator TBCEditorLinesArea.NotEqual(a, b: TBCEditorLinesArea): Boolean;
begin
  Result := (a.BeginPosition <> b.BeginPosition) or (a.EndPosition <> b.EndPosition);
end;

function TBCEditorLinesArea.ToString(): string;
begin
  Result := '(' + BeginPosition.ToString() + '-' + EndPosition.ToString() + ')';
end;

class function TBCEditorLinesArea.Union(const a, b: TBCEditorLinesArea): TBCEditorLinesArea;
begin
  Result.BeginPosition := Min(a.BeginPosition, b.BeginPosition);
  Result.EndPosition := Max(a.EndPosition, b.EndPosition);
end;

procedure TBCEditorLinesArea.Union(const a: TBCEditorLinesArea);
begin
  BeginPosition := Min(a.BeginPosition, BeginPosition);
  EndPosition := Max(a.BeginPosition, EndPosition);
end;

end.

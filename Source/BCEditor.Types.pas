unit BCEditor.Types;

interface {********************************************************************}

uses
  SysUtils, Types,
  Graphics, Controls,
  BCEditor.Consts;

type
  TBCEditorArrayOfString = array of string;

  TBCEditorBreakType = (
    btUnspecified,
    btAny,
    btTerm
  );

  TBCEditorCase = (cNone = -1, cUpper = 0, cLower = 1, cOriginal = 2);

  TBCEditorCodeFoldingOption = (
    cfoFoldMultilineComments,
    cfoHighlightFoldingLine,
    cfoHighlightIndentGuides,
    cfoShowTreeLine
  );
  TBCEditorCodeFoldingOptions = set of TBCEditorCodeFoldingOption;

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
  TBCEditorCompletionProposalOptions = set of TBCEditorCompletionProposalOption;

  TBCEditorOption = (
    eoAutoIndent, { Will indent the caret on new lines with the same amount of leading white space as the preceding line }
    eoBeyondEndOfFile, { Allows the cursor to go beyond the end of file into the white space }
    eoBeyondEndOfLine, { Allows the cursor to go beyond the last character into the white space }
    eoDropFiles, { Allows the editor accept OLE file drops }
    eoHighlightActiveLine, { Highlights the background of the line with the caret }
    eoHighlightAllFoundTexts, { Highlight all found texts }
    eoHighlightMatchingPairs, { Highlights the background two matching pairs like quoters or brackets }
    eoMiddleClickScrolling, { Scrolling by mouse move after wheel click. }
    eoShowSpecialChars, { Shows special chars (#0, space, tab and line break) }
    eoTrimTrailingLines, { Empty lines at the end of text will be removed while saving }
    eoTrimTrailingSpaces { Spaces at the end of lines will be removed while saving }
  );
  TBCEditorOptions = set of TBCEditorOption;

  TBCEditorFindOption = (
    foBackwards,
    foCaseSensitive,
    foEntireScope,
    foRegExpr,
    foSelection,
    foWholeWordsOnly,
    foWrapAround
  );
  TBCEditorFindOptions = set of TBCEditorFindOption;

  TBCEditorKeyCharType = (ctFoldOpen, ctFoldClose, ctSkipOpen, ctSkipClose);

  TBCEditorSortOrder = (soAsc, soDesc);

  TBCEditorLeftMarginLineNumberOption = (
    lnoIntens,
    lnoAfterLastLine
  );
  TBCEditorLeftMarginLineNumberOptions = set of TBCEditorLeftMarginLineNumberOption;

  TBCEditorLeftMarginBookMarkPanelOption = (
    bpoToggleBookmarkByClick,
    bpoToggleMarkByClick
  );
  TBCEditorLeftMarginBookMarkPanelOptions = set of TBCEditorLeftMarginBookMarkPanelOption;

  TBCEditorRangeItemType = (ritUnspecified, ritMultiLineString, ritSingleLineString, ritMultiLineComment, ritSingleLineComment);

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

  TBCEditorReplaceAction = (raCancel, raSkip, raReplace, raReplaceAll);

  TBCEditorReplaceOption = (
    roBackwards,
    roCaseSensitive,
    roEntireScope,
    roPrompt,
    roRegExpr,
    roReplaceAll,
    roSelection,
    roWholeWordsOnly,
    roWrapAround
  );
  TBCEditorReplaceOptions = set of TBCEditorReplaceOption;

  TBCEditorSelectionOption = (
    soDoubleClickRealNumbers,
    soHighlightWholeLine,
    soTripleClickLineSelect
  );
  TBCEditorSelectionOptions = set of TBCEditorSelectionOption;

  TBCEditorSyncEditOption = (
    seoShowButton,
    seoCaseSensitive
  );
  TBCEditorSyncEditOptions = set of TBCEditorSyncEditOption;

  TBCEditorTabOption = (
    toPreviousLineIndent,
    toSelectedBlockIndent,
    toTabsToSpaces
  );
  TBCEditorTabOptions = set of TBCEditorTabOption;

  TBCEditorTextEntryMode = (
    temInsert,
    temOverwrite
  );

  TBCEditorUndoOption = (
    uoGroupUndo,
    uoUndoAfterLoad,
    uoUndoAfterSave
  );
  TBCEditorUndoOptions = set of TBCEditorUndoOption;

  TBCEditorLinesPosition = record
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

  TBCEditorRowsPosition = record
    Column: Integer;
    Row: Integer;
    class operator Equal(a, b: TBCEditorRowsPosition): Boolean; inline;
    class operator NotEqual(a, b: TBCEditorRowsPosition): Boolean; inline;
    function ToString(): string; inline;
  end;

  TBCEditorCaretChangedEvent = procedure(ASender: TObject; ACaretPos: TPoint) of object;
  TBCEditorContextHelpEvent = procedure(ASender: TObject; AWord: string) of object;
  TBCEditorFindExecutedEvent = procedure(ASender: TObject; const AErrorMessage: string) of object;
  TBCEditorFindWrapAroundEvent = function(ASender: TObject; const APattern: string; const ABackwards: Boolean): Boolean of object;
  TBCEditorHintEvent = procedure(ASender: TObject; const AX, AY: Integer; const APos: TPoint; const ACharIndex: Integer; var AHint: string) of object;
  TBCEditorMarksPanelClick = procedure(ASender: TObject; const ALine: Integer) of object;
  TBCEditorMouseCursorEvent = procedure(ASender: TObject; const ALineCharPos: TBCEditorLinesPosition; var ACursor: TCursor) of object;
  TBCEditorReplacePromptEvent = procedure(ASender: TObject; const AArea: TBCEditorLinesArea; const ABackwards: Boolean; const AReplaceText: string; var AAction: TBCEditorReplaceAction) of object;

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

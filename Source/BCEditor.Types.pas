unit BCEditor.Types;

interface {********************************************************************}

uses
  Windows,
  Classes, SysUtils,
  Forms, Graphics, Controls,
  BCEditor.Consts;

type
  TBCEditorArrayOfString = array of string;
  TBCEditorArrayOfSingle = array of Single;

  TBCEditorCharMethod = function(const AChar: Char): Boolean of object;

  TBCEditorDropFilesEvent = procedure(ASender: TObject; APos: TPoint; AFiles: TStrings) of object;

  TBCEditorReplaceAction = (raCancel, raSkip, raReplace, raReplaceAll);

  TBCEditorTokenAddon = (taNone, taDoubleUnderline, taUnderline, taWaveLine);

  TBCEditorCreateFileStreamEvent = procedure(ASender: TObject; const AFileName: string; var AStream: TStream) of object;

  TBCEditorOption = (
    eoAutoIndent, { Will indent the caret on new lines with the same amount of leading white space as the preceding line }
    eoDragDropEditing, { Allows you to select a block of text and drag it within the document to another location }
    eoDropFiles, { Allows the editor accept OLE file drops }
    eoTrimTrailingLines, { Empty lines at the end of text will be removed while saving }
    eoTrimTrailingSpaces { Spaces at the end of lines will be removed while saving }
  );
  TBCEditorOptions = set of TBCEditorOption;

  TBCEditorCaretOption = (
    coRightMouseClickMove { When clicking with the right mouse for a popup menu, move the cursor to that location }
  );
  TBCEditorCaretMultiEditOption = (
    meoShowActiveLine,
    meoShowGhost { Ghost caret follows mouse cursor when moved }
  );

  TBCEditorTextEntryMode = (temInsert, temOverwrite);

  TBCEditorScrollOption = (
    soHalfPage, { When scrolling with page-up and page-down commands, only scroll a half page at a time }
    soHintFollows, { The scroll hint follows the mouse when scrolling vertically }
    soBeyondEndOfFile, { Allows the cursor to go beyond the end of file into the white space }
    soBeyondEndOfLine, { Allows the cursor to go beyond the last character into the white space }
    soShowVerticalScrollHint, { Shows a hint of the visible line numbers when scrolling vertically }
    soWheelClickMove { Scrolling by mouse move after wheel click. }
  );

  TBCEditorTabOption = (
    toPreviousLineIndent,
    toSelectedBlockIndent,
    toTabsToSpaces
  );

  TBCEditorSelectionOption = (
    soExpandRealNumbers,
    soTermsCaseSensitive,
    soToEndOfLine,
    soTripleClickRowSelect
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
    seCaseSensitive
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

  PBCEditorTextPosition = ^TBCEditorLinesPosition;
  TBCEditorLinesPosition = packed record
    Char: Integer;
    Line: Integer;
    class operator Equal(a, b: TBCEditorLinesPosition): Boolean; inline;
    class operator GreaterThan(a, b: TBCEditorLinesPosition): Boolean; inline;
    class operator GreaterThanOrEqual(a, b: TBCEditorLinesPosition): Boolean; inline;
    class operator LessThan(a, b: TBCEditorLinesPosition): Boolean; inline;
    class operator LessThanOrEqual(a, b: TBCEditorLinesPosition): Boolean; inline;
    class operator NotEqual(a, b: TBCEditorLinesPosition): Boolean; inline;
    function ToString(): string; inline;
  end;

  TBCEditorLinesArea = record
    BeginPosition: TBCEditorLinesPosition;
    EndPosition: TBCEditorLinesPosition;
    function Containts(Position: TBCEditorLinesPosition): Boolean; inline;
    class operator Equal(a, b: TBCEditorLinesArea): Boolean; inline;
    function IsEmpty(): Boolean; inline;
    class operator NotEqual(a, b: TBCEditorLinesArea): Boolean; inline;
    function ToString(): string; inline;
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
    ttMailtoLink,
    ttMethod,
    ttMethodName,
    ttNumber,
    ttReservedWord,
    ttString,
    ttSymbol,
    ttWebLink
  );


  TBCEditorKeyPressWEvent = procedure(ASender: TObject; var AKey: Char) of object;

  TBCEditorContextHelpEvent = procedure(ASender: TObject; AWord: string) of object;

  TBCEditorMouseCursorEvent = procedure(ASender: TObject; const ALineCharPos: TBCEditorLinesPosition; var ACursor: TCursor) of object;

  TBCEditorTabConvertProc = function(const ALine: string; ATabWidth: Integer; var AHasTabs: Boolean;
    const ATabChar: Char = BCEDITOR_SPACE_CHAR): string;

  TBCEditorLeftMarginLineNumberOption = (
    lnoIntens,
    lnoLeadingZeros,
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
    cfoAutoPadding,
    cfoFoldMultilineComments,
    cfoHighlightFoldingLine,
    cfoHighlightIndentGuides,
    cfoShowCollapsedLine,
    cfoShowTreeLine,
    cfoUncollapseByHintClick
  );

  TBCEditorScrollHintFormat = (shfTopLineOnly, shfTopToBottom);

  TBCEditorCaretOptions = set of TBCEditorCaretOption;
  TBCEditorCaretMultiEditOptions = set of TBCEditorCaretMultiEditOption;
  TBCEditorCodeFoldingOptions = set of TBCEditorCodeFoldingOption;
  TBCEditorCompletionProposalOptions = set of TBCEditorCompletionProposalOption;
  TBCEditorLeftMarginLineNumberOptions = set of TBCEditorLeftMarginLineNumberOption;
  TBCEditorSearchOptions = set of TBCEditorSearchOption;
  TBCEditorSearchMapOptions = set of TBCEditorSearchMapOption;
  TBCEditorLeftMarginBookMarkPanelOptions = set of TBCEditorLeftMarginBookMarkPanelOption;
  TBCEditorReplaceOptions = set of TBCEditorReplaceOption;
  TBCEditorScrollOptions = set of TBCEditorScrollOption;
  TBCEditorSelectionOptions = set of TBCEditorSelectionOption;
  TBCEditorTabOptions = set of TBCEditorTabOption;
  TBCEditorUndoOptions = set of TBCEditorUndoOption;

  TBCEditorRangeItemType = (ritUnspecified, ritMultiLineString, ritSingleLineString, ritMultiLineComment, ritSingleLineComment);

  TBCEditorQuadColor = packed record
  case Boolean of
    True: (Blue, Green, Red, Alpha: Byte);
    False: (Quad: Cardinal);
  end;
  PBCEditorQuadColor = ^TBCEditorQuadColor;

function Empty(const AArea: TBCEditorLinesArea): Boolean; inline;
function Max(const A, B: TBCEditorLinesPosition): TBCEditorLinesPosition; overload; inline;
function Min(const A, B: TBCEditorLinesPosition): TBCEditorLinesPosition; overload; inline;
function LinesArea(const ABeginPosition, AEndPosition: TBCEditorLinesPosition): TBCEditorLinesArea; inline;
function LinesPosition(const AChar, ALine: Integer): TBCEditorLinesPosition; overload; inline;
function LinesPosition(const APos: TPoint): TBCEditorLinesPosition; overload; inline;
function Point(const APosition: TBCEditorLinesPosition): TPoint; overload; inline;
function RowsPosition(const AColumn: Integer; const ARow: Integer): TBCEditorRowsPosition; inline;

const
  InvalidLinesArea: TBCEditorLinesArea = ( BeginPosition: ( Char: -1; Line: -1; ); EndPosition: ( Char: -1; Line: -1; ) );
  InvalidLinesPosition: TBCEditorLinesPosition = ( Char: -1; Line: -1; );
  InvalidRowsPosition: TBCEditorRowsPosition = ( Column: -1; Row: -1; );

implementation {***************************************************************}

function Empty(const AArea: TBCEditorLinesArea): Boolean; inline;
begin
  Result := AArea.BeginPosition = AArea.EndPosition;
end;

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

function LinesPosition(const APos: TPoint): TBCEditorLinesPosition;
begin
  Result.Char := APos.X;
  Result.Line := APos.Y;
end;

function Point(const APosition: TBCEditorLinesPosition): TPoint;
begin
  Result.X := APosition.Char;
  Result.Y := APosition.Line;
end;

function RowsPosition(const AColumn: Integer; const ARow: Integer): TBCEditorRowsPosition;
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

function TBCEditorLinesArea.Containts(Position: TBCEditorLinesPosition): Boolean;
begin
  Result := (BeginPosition <= Position) and (Position <= EndPosition);
end;

class operator TBCEditorLinesArea.Equal(a, b: TBCEditorLinesArea): Boolean;
begin
  Result := (a.BeginPosition = b.BeginPosition) and (a.EndPosition = b.EndPosition);
end;

class operator TBCEditorLinesArea.NotEqual(a, b: TBCEditorLinesArea): Boolean;
begin
  Result := (a.BeginPosition <> b.BeginPosition) or (a.EndPosition <> b.EndPosition);
end;

function TBCEditorLinesArea.IsEmpty(): Boolean;
begin
  Result := BeginPosition = EndPosition;
end;

function TBCEditorLinesArea.ToString(): string;
begin
  Result := '(' + BeginPosition.ToString() + '-' + EndPosition.ToString() + ')';
end;

end.

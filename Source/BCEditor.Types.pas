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

  TBCEditorCaretStyle = (csVerticalLine, csHorizontalLine, csHalfBlock, csBlock);

  TBCEditorDropFilesEvent = procedure(ASender: TObject; APos: TPoint; AFiles: TStrings) of object;

  TBCEditorPaintEvent = procedure(ASender: TObject; ACanvas: TCanvas) of object;

  TBCEditorReplaceAction = (raCancel, raSkip, raReplace, raReplaceAll);

  TBCEditorMarkPanelPaintEvent = procedure(ASender: TObject; ACanvas: TCanvas; const ARect: TRect; const AFirstLine: Integer; const ALastLine: Integer) of object;
  TBCEditorMarkPanelLinePaintEvent = procedure(ASender: TObject; ACanvas: TCanvas; const ARect: TRect; const ALineNumber: Integer) of object;

  TBCEditorLinePaintEvent = procedure(ASender: TObject; ACanvas: TCanvas; const ARect: TRect; const ALineNumber: Integer) of object;

  TBCEditorCustomLineColorsEvent = procedure(ASender: TObject;
    const ALine: Integer; var AForeground, ABackground: TColor) of object;

  TBCEditorTokenAddon = (taNone, taDoubleUnderline, taUnderline, taWaveLine);

  TBCEditorCustomDrowTokenEvent = procedure(ASender: TObject;
    const APos: TPoint; const AAttribute: Pointer;
    var AText: PChar; var ALength: Integer;
    var AForegroundColor, ABackgroundColor: TColor; var AStyles: TFontStyles;
    var ABorderColor: TColor;
    var ATokenAddon: TBCEditorTokenAddon; var ATokenAddonColor: TColor) of object;

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
    soPastEndOfFile, { Allows the cursor to go past the end of file }
    soPastEndOfLine, { Allows the cursor to go past the last character into the white space at the end of a line }
    soShowVerticalScrollHint, { Shows a hint of the visible line numbers when scrolling vertically }
    soWheelClickMove { Scrolling by mouse move after wheel click. }
  );

  TBCEditorTabOption = (
    toPreviousLineIndent,
    toSelectedBlockIndent,
    toTabsToSpaces
  );

  PBCEditorSelectionMode = ^TBCEditorSelectionMode;
  TBCEditorSelectionMode = (
    smColumn,
    smNormal
  );

  TBCEditorSelectionOption = (
    soALTSetsColumnMode,
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

  TBCEditorRightMarginOption = (
    rmoMouseMove,
    rmoShowMovingHint
  );

  PBCEditorTextPosition = ^TBCEditorTextPosition;
  TBCEditorTextPosition = packed record
    class operator Equal(a, b: TBCEditorTextPosition): Boolean; inline;
    class operator GreaterThan(a, b: TBCEditorTextPosition): Boolean; inline;
    class operator GreaterThanOrEqual(a, b: TBCEditorTextPosition): Boolean; inline;
    class operator LessThan(a, b: TBCEditorTextPosition): Boolean; inline;
    class operator LessThanOrEqual(a, b: TBCEditorTextPosition): Boolean; inline;
    class operator NotEqual(a, b: TBCEditorTextPosition): Boolean; inline;
    function ToString(): string; inline;
    case Integer of
      0: (
        Char: Integer;     // Must be before Line, since the operators using Combined
        Line: Integer; );  // Must be after Char, since the operators using Combined
      1: (
        Combinded: Int64; );
  end;

  TBCEditorTextArea = record
    BeginPosition: TBCEditorTextPosition;
    EndPosition: TBCEDitorTextPosition;
    function Containts(Position: TBCEditorTextPosition): Boolean; inline;
    class operator Equal(a, b: TBCEditorTextArea): Boolean; inline;
    function IsEmpty(): Boolean; inline;
    class operator NotEqual(a, b: TBCEditorTextArea): Boolean; inline;
    function ToString(): string; inline;
  end;

  PBCEditorDisplayPosition = ^TBCEditorDisplayPosition;
  TBCEditorDisplayPosition = packed record
    class operator Equal(a, b: TBCEditorDisplayPosition): Boolean; inline;
    class operator NotEqual(a, b: TBCEditorDisplayPosition): Boolean; inline;
    function ToString(): string; inline;
    case Integer of
      0: (
        Column: Integer;  // Must be before Row, since the operators using Combined
        Row: Integer; );  // Must be after Column, since the operators using Combined
      1: (
        Combinded: Int64; );
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

  TBCEditorMouseCursorEvent = procedure(ASender: TObject; const ALineCharPos: TBCEditorTextPosition; var ACursor: TCursor) of object;

  TBCEditorTabConvertProc = function(const ALine: string; ATabWidth: Integer; var AHasTabs: Boolean;
    const ATabChar: Char = BCEDITOR_SPACE_CHAR): string;

  TBCEditorLeftMarginLineNumberOption = (
    lnoIntens,
    lnoLeadingZeros,
    lnoAfterLastLine
  );

  TBCEditorMatchingPairOption = (
    mpoHighlightAfterToken,
    mpoUseMatchedColor
  );

  TBCEditorSearchMapAlign = (saLeft, saRight);

  TBCEditorUndoOption = (
    uoGroupUndo,
    uoUndoAfterLoad,
    uoUndoAfterSave
  );

  TBCEditorCase = (cNone=-1, cUpper=0, cLower=1, cAlternating=2, cSentence=3, cTitle=4, cOriginal=5);

  TBCEditorKeyCharType = (ctFoldOpen, ctFoldClose, ctSkipOpen, ctSkipClose);

  TBCEditorSortOrder = (soAsc, soDesc);

  TBCEditorWordWrapWidth = (wwwPage, wwwRightMargin);

  TBCEditorCodeFoldingMarkStyle = (msCircle, msSquare, msTriangle);
  TBCEditorCodeFoldingHintIndicatorMarkStyle = (imsThreeDots, imsTriangle);
  TBCEditorCodeFoldingChanges = (fcEnabled, fcRefresh, fcRescan);

  TBCEditorCodeFoldingOption = (
    cfoAutoPadding,
    cfoAutoWidth,
    cfoFoldMultilineComments,
    cfoHighlightFoldingLine,
    cfoHighlightIndentGuides,
    cfoHighlightMatchingPair,
    cfoShowCollapsedLine,
    cfoShowIndentGuides,
    cfoShowTreeLine,
    cfoUncollapseByHintClick
  );

  TBCEditorTokenInfoOption = (
    tioAutoSize
  );

  TBCEditorLeftMarginBorderStyle = (mbsNone, mbsMiddle, mbsRight);

  TBCEditorScrollHintFormat = (shfTopLineOnly, shfTopToBottom);

  TBCEditorCodeFoldingHintIndicatorOption = (hioShowBorder, hioShowMark);

  TBCEditorCaretOptions = set of TBCEditorCaretOption;
  TBCEditorCaretMultiEditOptions = set of TBCEditorCaretMultiEditOption;
  TBCEditorCodeFoldingOptions = set of TBCEditorCodeFoldingOption;
  TBCEditorCodeFoldingHintIndicatorOptions = set of TBCEditorCodeFoldingHintIndicatorOption;
  TBCEditorCompletionProposalOptions = set of TBCEditorCompletionProposalOption;
  TBCEditorLeftMarginLineNumberOptions = set of TBCEditorLeftMarginLineNumberOption;
  TBCEditorMatchingPairOptions = set of TBCEditorMatchingPairOption;
  TBCEditorSearchOptions = set of TBCEditorSearchOption;
  TBCEditorSearchMapOptions = set of TBCEditorSearchMapOption;
  TBCEditorLeftMarginBookMarkPanelOptions = set of TBCEditorLeftMarginBookMarkPanelOption;
  TBCEditorReplaceOptions = set of TBCEditorReplaceOption;
  TBCEditorRightMarginOptions = set of TBCEditorRightMarginOption;
  TBCEditorScrollOptions = set of TBCEditorScrollOption;
  TBCEditorSelectionOptions = set of TBCEditorSelectionOption;
  TBCEditorTabOptions = set of TBCEditorTabOption;
  TBCEditorTokenInfoOptions = set of TBCEditorTokenInfoOption;
  TBCEditorUndoOptions = set of TBCEditorUndoOption;

  TBCEditorRangeItemType = (ritUnspecified, ritMultiLineString, ritSingleLineString, ritMultiLineComment, ritSingleLineComment);

  TBCEditorQuadColor = packed record
  case Boolean of
    True: (Blue, Green, Red, Alpha: Byte);
    False: (Quad: Cardinal);
  end;
  PBCEditorQuadColor = ^TBCEditorQuadColor;

function Empty(const AArea: TBCEditorTextArea): Boolean; inline;
function Max(const A, B: TBCEditorTextPosition): TBCEditorTextPosition; overload; inline;
function Min(const A, B: TBCEditorTextPosition): TBCEditorTextPosition; overload; inline;
function Point(const APosition: TBCEditorTextPosition): TPoint; overload; inline;
function TextArea(const ABeginPosition, AEndPosition: TBCEditorTextPosition): TBCEditorTextArea; inline;
function TextPosition(const AChar, ALine: Integer): TBCEditorTextPosition; overload; inline;
function TextPosition(const APos: TPoint): TBCEditorTextPosition; overload; inline;

const
  InvalidTextArea: TBCEditorTextArea = ( BeginPosition: ( Char: -1; Line: -1; ); EndPosition: ( Char: -1; Line: -1; ) );
  InvalidTextPosition: TBCEditorTextPosition = ( Char: -1; Line: -1; );

implementation {***************************************************************}

function Empty(const AArea: TBCEditorTextArea): Boolean; inline;
begin
  Result := AArea.BeginPosition = AArea.EndPosition;
end;

function Max(const A, B: TBCEditorTextPosition): TBCEditorTextPosition;
begin
  if (A > B) then
    Result := A
  else
    Result := B;
end;

function Min(const A, B: TBCEditorTextPosition): TBCEditorTextPosition;
begin
  if (A < B) then
    Result := A
  else
    Result := B;
end;

function Point(const APosition: TBCEditorTextPosition): TPoint;
begin
  Result.X := APosition.Char;
  Result.Y := APosition.Line;
end;

function TextArea(const ABeginPosition, AEndPosition: TBCEditorTextPosition): TBCEditorTextArea;
begin
  Result.BeginPosition := ABeginPosition;
  Result.EndPosition := AEndPosition;
end;

function TextPosition(const AChar, ALine: Integer): TBCEditorTextPosition;
begin
  Result.Char := AChar;
  Result.Line := ALine;
end;

function TextPosition(const APos: TPoint): TBCEditorTextPosition;
begin
  Result.Char := APos.X;
  Result.Line := APos.Y;
end;

{ TBCEditorDisplayPosition ****************************************************}

class operator TBCEditorDisplayPosition.Equal(a, b: TBCEditorDisplayPosition): Boolean;
begin
  Result := a.Combinded = b.Combinded;
end;

class operator TBCEditorDisplayPosition.NotEqual(a, b: TBCEditorDisplayPosition): Boolean;
begin
  Result := a.Combinded <> b.Combinded;
end;

function TBCEditorDisplayPosition.ToString(): string;
begin
  Result := '(' + IntToStr(Column) + ',' + IntToStr(Row) + ')';
end;

{ TBCEditorTextPosition *******************************************************}

class operator TBCEditorTextPosition.Equal(a, b: TBCEditorTextPosition): Boolean;
begin
  Result := a.Combinded = b.Combinded;
end;

class operator TBCEditorTextPosition.GreaterThan(a, b: TBCEditorTextPosition): Boolean;
begin
  Result := a.Combinded > b.Combinded;
end;

class operator TBCEditorTextPosition.GreaterThanOrEqual(a, b: TBCEditorTextPosition): Boolean;
begin
  Result := a.Combinded >= b.Combinded;
end;

class operator TBCEditorTextPosition.LessThan(a, b: TBCEditorTextPosition): Boolean;
begin
  Result := a.Combinded < b.Combinded;
end;

class operator TBCEditorTextPosition.LessThanOrEqual(a, b: TBCEditorTextPosition): Boolean;
begin
  Result := a.Combinded <= b.Combinded;
end;

class operator TBCEditorTextPosition.NotEqual(a, b: TBCEditorTextPosition): Boolean;
begin
  Result := a.Combinded <> b.Combinded;
end;

function TBCEditorTextPosition.ToString(): string;
begin
  Result := '(' + IntToStr(Char) + ',' + IntToStr(Line) + ')';
end;

{ TBCEditorTextPosition *******************************************************}

function TBCEditorTextArea.Containts(Position: TBCEditorTextPosition): Boolean;
begin
  Result := (BeginPosition <= Position) and (Position <= EndPosition);
end;

class operator TBCEditorTextArea.Equal(a, b: TBCEditorTextArea): Boolean;
begin
  Result := (a.BeginPosition = b.BeginPosition) and (a.EndPosition = b.EndPosition);
end;

class operator TBCEditorTextArea.NotEqual(a, b: TBCEditorTextArea): Boolean;
begin
  Result := (a.BeginPosition <> b.BeginPosition) or (a.EndPosition <> b.EndPosition);
end;

function TBCEditorTextArea.IsEmpty(): Boolean;
begin
  Result := BeginPosition = EndPosition;
end;

function TBCEditorTextArea.ToString(): string;
begin
  Result := '(' + BeginPosition.ToString() + '-' + EndPosition.ToString() + ')';
end;

end.

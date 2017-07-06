unit BCEditor.Editor;

interface {********************************************************************}

uses
  Windows, Messages, ActiveX,
  Classes, SysUtils, Contnrs, UITypes, StrUtils, Generics.Collections,
  Forms, StdActns, Controls, Graphics, StdCtrls, ExtCtrls, Dialogs, Consts,
  BCEditor.Consts, BCEditor.Editor.ActiveLine,
  BCEditor.Editor.Marks, BCEditor.Editor.Caret, BCEditor.Editor.CodeFolding,
  BCEditor.Types, BCEditor.Editor.CompletionProposal,
  BCEditor.Editor.CompletionProposal.PopupWindow,
  BCEditor.Editor.KeyCommands, BCEditor.Editor.LeftMargin, BCEditor.Editor.MatchingPair,
  BCEditor.Editor.Replace, BCEditor.Editor.Scroll, BCEditor.Editor.Search,
  BCEditor.Editor.Selection, BCEditor.Editor.SpecialChars,
  BCEditor.Editor.Tabs, BCEditor.Editor.WordWrap,
  BCEditor.Highlighter, BCEditor.KeyboardHandler, BCEditor.Lines,
  BCEditor.PaintHelper, BCEditor.Editor.SyncEdit, BCEditor.Utils;

type
  TCustomBCEditor = class(TCustomControl, IDropSource, IDropTarget)
  private type
    TBCEditorCodeFolding = class(BCEditor.Editor.CodeFolding.TBCEditorCodeFolding);
    TBCEditorHighlighter = class(BCEditor.Highlighter.TBCEditorHighlighter);
    TBCEditorLeftMargin = class(BCEditor.Editor.LeftMargin.TBCEditorLeftMargin);
    TBCEditorLines = class(BCEditor.Lines.TBCEditorLines);
    TBCEditorReplace = class(BCEditor.Editor.Replace.TBCEditorReplace);
    TBCEditorSearch = class(BCEditor.Editor.Search.TBCEditorSearch);
    TBCEditorSpecialChars = class(BCEditor.Editor.SpecialChars.TBCEditorSpecialChars);
    TBCEditorSyncEdit = class(BCEditor.Editor.SyncEdit.TBCEditorSyncEdit);
    TBCEditorTabs = class(BCEditor.Editor.Tabs.TBCEditorTabs);

    TDropData = class(TInterfacedObject, IDataObject, IEnumFORMATETC)
    private
      FEditor: TCustomBCEditor;
      FEnumFormatEtcIndex: Integer;
    protected
      function Clone(out Enum: IEnumFormatEtc): HResult; stdcall;
      function DAdvise(const formatetc: TFormatEtc; advf: Longint;
        const advSink: IAdviseSink; out dwConnection: Longint): HResult; stdcall;
      function DUnadvise(dwConnection: Longint): HResult; stdcall;
      function EnumDAdvise(out enumAdvise: IEnumStatData): HResult; stdcall;
      function EnumFormatEtc(dwDirection: Longint; out enumFormatEtc:
        IEnumFormatEtc): HResult; stdcall;
      function GetCanonicalFormatEtc(const formatetc: TFormatEtc;
        out formatetcOut: TFormatEtc): HResult; stdcall;
      function GetData(const formatetcIn: TFormatEtc; out medium: TStgMedium):
        HResult; stdcall;
      function GetDataHere(const formatetc: TFormatEtc; out medium: TStgMedium):
        HResult; stdcall;
      function Next(celt: Longint; out elt; pceltFetched: PLongint): HResult; stdcall;
      function QueryGetData(const formatetc: TFormatEtc): HResult; stdcall;
      function Reset(): HResult; stdcall;
      function SetData(const formatetc: TFormatEtc; var medium: TStgMedium;
        fRelease: BOOL): HResult; stdcall;
      function Skip(celt: Longint): HResult; stdcall;
      property Editor: TCustomBCEditor read FEditor;
    public
      constructor Create(const AEditor: TCustomBCEditor);
    end;

    TMatchingPairResult = record
      CloseTokenArea: TBCEditorLinesArea;
      OpenTokenArea: TBCEditorLinesArea;
      FState: (mpsClear, mpsFound, mpsNotFound);
    end;

    TMouseCapture = (mcNone, mcAll, mcSyncEditButton, mcMarks, mcLineNumbers, mcLineState, mcCodeFolding, mcText);

    PPaintTokenData = ^TPaintTokenData;
    TPaintTokenData = record
    type
      TPart = record
      type
        TPartType = (ptNormal, ptSyncEdit, ptMatchingPair, ptSelection,
          ptSearchResult, ptSearchResultInSection);
      public
        BeginPosition: TBCEditorLinesPosition;
        EndPosition: TBCEditorLinesPosition;
        PartType: TPartType;
      end;
    public
      LineBackgroundColor: TColor;
      LineForegroundColor: TColor;
      Parts: TList<TPart>;
      Previous: record
        BackgroundColor: TColor;
        FontStyles: TFontStyles;
      end;
      SearchResultIndex: Integer;
      SelArea: TBCEditorLinesArea;
    end;

    TProcessAction = (paNone, paPaint, paMouseDown, paMouseDouble, paMouseTriple,
      paMouseMove, paMouseUp, paHint);

    TRow = record
    type
      TFlags = set of (rfFirstRowOfLine, rfLastRowOfLine, rfHasTabs);
    public
      BeginRange: Pointer;
      Char: Integer;
      Columns: Integer;
      Flags: TFlags;
      Length: Integer;
      Line: Integer;
      Width: Integer;
    end;

    TRows = class(TList<TRow>)
    private
      FCaretPosition: TBCEditorRowsPosition;
      FEditor: TCustomBCEditor;
      FMaxColumns: Integer;
      FMaxColumnsRow: Integer;
      FMaxWidth: Integer;
      FMaxWidthRow: Integer;
      function GetCaretPosition(): TBCEditorRowsPosition;
      function GetBORPosition(ARow: Integer): TBCEditorLinesPosition;
      function GetEORPosition(ARow: Integer): TBCEditorLinesPosition;
      function GetMaxColumns(): Integer;
      function GetMaxWidth(): Integer;
      function GetRowArea(ARow: Integer): TBCEditorLinesArea;
      function GetText(ARow: Integer): string;
    public
      procedure Add(const AFlags: TRow.TFlags; const ALine: Integer;
        const AChar, ALength, AColumns, AWidth: Integer; const ABeginRange: Pointer);
      procedure Clear();
      constructor Create(const AEditor: TCustomBCEditor);
      procedure Delete(ARow: Integer);
      procedure Insert(ARow: Integer;
        const AFlags: TRow.TFlags; const ALine: Integer;
        const AChar, ALength, AColumns, AWidth: Integer; const ABeginRange: Pointer);
      property CaretPosition: TBCEditorRowsPosition read GetCaretPosition write FCaretPosition;
      property BORPosition[Row: Integer]: TBCEditorLinesPosition read GetBORPosition;
      property EORPosition[Row: Integer]: TBCEditorLinesPosition read GetEORPosition;
      property MaxColumns: Integer read GetMaxColumns;
      property MaxWidth: Integer read GetMaxWidth;
      property RowArea[Row: Integer]: TBCEditorLinesArea read GetRowArea;
      property Text[Row: Integer]: string read GetText; default;
    end;

    TState = set of (esFontChanged, esSizeChanged, esRowsChanged,
      esCaretInvalid, esCaretChanged, esSelectionChanged,
      esScrolled, esLinesCleared, esLinesDeleted, esLinesInserted,
      esLinesUpdated, esIgnoreNextChar,
      esFindind, esReplacing,
      esUpdating, esUpdatingScrollBars, esBuildingRows, esHandlingMouse,
      esCodeFoldingInvalid, esSysFontChanged, esHighlighterChanged,
      esWaitForDrag, esDragging, esMouseDouble);

  private const
    DefaultOptions = [eoAutoIndent, eoDropFiles];
    DefaultUndoOptions = [uoGroupUndo];
    UM_FREE_COMPLETIONPROPOSAL_POPUPWINDOW = WM_USER;
  private
    FActiveLine: TBCEditorActiveLine;
    FAllCodeFoldingRanges: TBCEditorCodeFolding.TAllRanges;
    FBookmarkList: TBCEditorMarkList;
    FBorderStyle: TBorderStyle;
    FCaret: TBCEditorCaret;
    FCaretCreated: Boolean;
    FCaretPos: TPoint; // Caret position in pixel - NOT the related to ClientPos
    FCaretVisible: Boolean;
    FChainedEditor: TCustomBCEditor;
    FCodeFolding: TBCEditorCodeFolding;
    FCodeFoldingWidth: Integer;
    FCodeFoldingCollapsedMarkWidth: Integer;
    FColor: TColor;
    FCommandDrop: Boolean;
    FCompletionProposal: TBCEditorCompletionProposal;
    FCompletionProposalPopupWindow: TBCEditorCompletionProposalPopupWindow;
    FCompletionProposalTimer: TTimer;
    FCurrentMatchingPair: TMatchingPairResult;
    FDoubleClickTime: Cardinal;
    FFontPitchFixed: Boolean;
    FHideSelection: Boolean;
    FHideSelectionBeforeSearch: Boolean;
    FHideScrollBars: Boolean;
    FHighlightedFoldRange: TBCEditorCodeFolding.TRanges.TRange;
    FHighlighter: TBCEditorHighlighter;
    FHintWindow: THintWindow;
    FHookedCommandHandlers: TObjectList;
    FHorzTextPos: Integer;
    FHWheelAccumulator: Integer;
    FImages: TImageList;
    FInsertPos: TPoint;
    FInsertPosCache: TBitmap;
    FInsertPosBitmap: TBitmap;
    FItalicOffsetCache: array [AnsiChar] of Byte;
    FKeyboardHandler: TBCEditorKeyboardHandler;
    FKeyCommands: TBCEditorKeyCommands;
    FLastDoubleClickTime: Cardinal;
    FLastKey: Word;
    FLastShiftState: TShiftState;
    FLastSortOrder: TBCEditorSortOrder;
    FLeftMargin: TBCEditorLeftMargin;
    FLeftMarginWidth: Integer;
    FLineBreakSignWidth: Integer;
    FLineHeight: Integer;
    FLineNumbersWidth: Integer;
    FLines: TBCEditorLines;
    FLineStateWidth: Integer;
    FMarkList: TBCEditorMarkList;
    FMarksPanelWidth: Integer;
    FMatchingPair: TBCEditorMatchingPair;
    FMaxDigitWidth: Integer;
    FMouseCapture: TMouseCapture;
    FMouseDownPoint: TPoint;
    FMouseMoveHintPoint: TPoint;
    FMouseMoveScrollCursors: array [0 .. 7] of HCursor;
    FMouseMoveScrolling: Boolean;
    FMouseMoveScrollingPoint: TPoint;
    FMouseMoveScrollTimer: TTimer;
    FOldMouseMovePoint: TPoint;
    FOldSelectionAvailable: Boolean;
    FOnAfterBookmarkPlaced: TNotifyEvent;
    FOnAfterDeleteBookmark: TNotifyEvent;
    FOnAfterDeleteMark: TNotifyEvent;
    FOnAfterMarkPlaced: TNotifyEvent;
    FOnBeforeCompletionProposalExecute: TBCEditorCompletionProposalEvent;
    FOnBeforeDeleteMark: TBCEditorMarkEvent;
    FOnBeforeMarkPlaced: TBCEditorMarkEvent;
    FOnCaretChanged: TBCEditorCaretChangedEvent;
    FOnChainCaretMoved: TNotifyEvent;
    FOnChainLinesCleared: TNotifyEvent;
    FOnChainLinesDeleted: TBCEditorLines.TChangeEvent;
    FOnChainLinesInserted: TBCEditorLines.TChangeEvent;
    FOnChainLinesUpdated: TBCEditorLines.TChangeEvent;
    FOnChange: TNotifyEvent;
    FOnCommandProcessed: TBCEditorProcessCommandEvent;
    FOnCompletionProposalCanceled: TNotifyEvent;
    FOnCompletionProposalSelected: TBCEditorCompletionProposalPopupWindowSelectedEvent;
    FOnContextHelp: TBCEditorContextHelpEvent;
    FOnHint: TBCEditorHintEvent;
    FOnKeyPressW: TBCEditorKeyPressWEvent;
    FOnModified: TNotifyEvent;
    FOnProcessCommand: TBCEditorProcessCommandEvent;
    FOnProcessUserCommand: TBCEditorProcessCommandEvent;
    FOnReplaceText: TBCEditorReplaceEvent;
    FOnRightMarginMouseUp: TNotifyEvent;
    FOnSelectionChanged: TNotifyEvent;
    FOptions: TBCEditorOptions;
    FOriginalLines: TBCEditorLines;
    FPaintHelper: TBCEditorPaintHelper;
    FReplace: TBCEditorReplace;
    FRows: TCustomBCEditor.TRows;
    FScroll: TBCEditorScroll;
    FScrollDeltaX: Integer;
    FScrollDeltaY: Integer;
    FScrollTimer: TTimer;
    FSearch: TBCEditorSearch;
    FSearchFindDialog: TFindDialog;
    FSearchReplaceDialog: TReplaceDialog;
    FSearchResults: TList<TBCEditorLinesArea>;
    FSearchStatus: string;
    FSelectedCaseCycle: TBCEditorCase;
    FSelectedCaseText: string;
    FSelection: TBCEditorSelection;
    FSpaceWidth: Integer;
    FSpecialChars: TBCEditorSpecialChars;
    FSpecialCharsNullText: string;
    FSpecialCharsSpaceText: string;
    FState: TState;
    FSyncEdit: TBCEditorSyncEdit;
    FSyncEditButtonHot: TPicture;
    FSyncEditButtonNormal: TPicture;
    FSyncEditButtonPushed: TPicture;
    FTabSignWidth: Integer;
    FTabs: TBCEditorTabs;
    FTextEntryMode: TBCEditorTextEntryMode;
    FTextWidth: Integer;
    FTopRow: Integer;
    FUpdateCount: Integer;
    FVisibleRows: Integer;
    FWantReturns: Boolean;
    FWheelScrollLines: UINT;
    FWindowProducedMessage: Boolean;
    FWordWrap: TBCEditorWordWrap;
    procedure ActiveLineChanged(Sender: TObject);
    procedure AfterLinesUpdate(Sender: TObject);
    procedure BeforeLinesUpdate(Sender: TObject);
    procedure BookmarkListChange(Sender: TObject);
    procedure CheckIfAtMatchingKeywords;
    procedure ClearCodeFolding();
    function ClientToLines(const X, Y: Integer; const AForCaret: Boolean = False): TBCEditorLinesPosition; {$IFNDEF Debug} inline; {$ENDIF}
    function ClientToRows(const X, Y: Integer; const AForCaret: Boolean = False): TBCEditorRowsPosition;
    procedure CMCursorChanged(var Message: TMessage); message CM_CURSORCHANGED;
    procedure CMSysFontChanged(var Message: TMessage); message CM_SYSFONTCHANGED;
    function CodeFoldingCollapsableFoldRangeForLine(const ALine: Integer): TBCEditorCodeFolding.TRanges.TRange;
    function CodeFoldingFoldRangeForLineTo(const ALine: Integer): TBCEditorCodeFolding.TRanges.TRange;
    procedure CodeFoldingOnChange(AEvent: TBCEditorCodeFoldingChanges);
    procedure CollapseCodeFoldingRange(const ARange: TBCEditorCodeFolding.TRanges.TRange);
    procedure CompletionProposalTimerHandler(EndLine: TObject);
    function ComputeIndentText(const IndentCount: Integer): string;
    procedure ComputeScroll(const APoint: TPoint);
    function ComputeTextColumns(const AText: PChar; const ALength, AColumn: Integer): Integer; {$IFNDEF Debug} inline; {$ENDIF}
    procedure DeleteChar;
    procedure DeleteLastWordOrBeginningOfLine(const ACommand: TBCEditorCommand);
    procedure DeleteLine;
    procedure DeleteLineFromRows(const ALine: Integer);
    procedure DeleteWordOrEndOfLine(const ACommand: TBCEditorCommand);
    procedure DoBackspace();
    procedure DoBlockComment;
    procedure DoChar(const AChar: Char);
    procedure DoCutToClipboard;
    procedure DoEditorBottom(const ACommand: TBCEditorCommand); {$IFNDEF Debug} inline; {$ENDIF}
    procedure DoEditorTop(const ACommand: TBCEditorCommand); {$IFNDEF Debug} inline; {$ENDIF}
    procedure DoEndKey(const ASelectionCommand: Boolean);
    procedure DoHomeKey(const ASelectionCommand: Boolean);
    procedure DoImeStr(AData: Pointer);
    procedure DoInsertText(const AText: string);
    procedure DoLineBreak();
    procedure DoLineComment;
    procedure DoPageTopOrBottom(const ACommand: TBCEditorCommand);
    procedure DoPageUpOrDown(const ACommand: TBCEditorCommand);
    procedure DoPasteFromClipboard;
    function DoReplaceText(): Integer;
    procedure DoScanCodeFoldingRanges();
    procedure DoScroll(const ACommand: TBCEditorCommand);
    function DoSearch(AArea: TBCEditorLinesArea; var APosition: TBCEditorLinesPosition): Boolean;
    function DoSearchFind(const First: Boolean; const Action: TSearchFind): Boolean;
    procedure DoSearchFindClose(Sender: TObject);
    procedure DoSearchFindExecute(Sender: TObject);
    function DoSearchNext(APosition: TBCEditorLinesPosition;
      out ASearchResult: TBCEditorLinesArea; const WrapAround: Boolean = False): Boolean;
    function DoSearchPrevious(APosition: TBCEditorLinesPosition;
      out ASearchResult: TBCEditorLinesArea; const WrapAround: Boolean = False): Boolean;
    function DoSearchReplace(const Action: TSearchReplace): Boolean;
    procedure DoSearchReplaceExecute(Sender: TObject);
    procedure DoSearchReplaceFind(Sender: TObject);
    procedure DoSetBookmark(const ACommand: TBCEditorCommand; AData: Pointer);
    procedure DoShiftTabKey;
    procedure DoSyncEdit;
    procedure DoTabKey;
    procedure DoToggleBookmark(const APosition: TBCEditorLinesPosition);
    procedure DoToggleMark(const APosition: TBCEditorLinesPosition);
    procedure DoToggleSelectedCase(const ACommand: TBCEditorCommand);
    procedure DoWordLeft(const ACommand: TBCEditorCommand);
    procedure DoWordRight(const ACommand: TBCEditorCommand);
    procedure ExpandCodeFoldingRange(const ARange: TBCEditorCodeFolding.TRanges.TRange);
    function FindHookedCommandEvent(const AHookedCommandEvent: TBCEditorHookedCommandEvent): Integer;
    procedure FindWords(const AWord: string; AList: TList; ACaseSensitive: Boolean; AWholeWordsOnly: Boolean);
    procedure FontChanged(ASender: TObject);
    function GetCanPaste: Boolean;
    function GetCanRedo: Boolean;
    function GetCanUndo: Boolean;
    function GetCaretPos(): TPoint;
    function GetCharAt(APos: TPoint): Char; {$IFNDEF Debug} inline; {$ENDIF}
    function GetHookedCommandHandlersCount: Integer;
    function GetLeadingExpandedLength(const AStr: string; const ABorder: Integer = 0): Integer;
    function GetLineIndentLevel(const ALine: Integer): Integer;
    function GetModified(): Boolean;
    function GetMouseMoveScrollCursorIndex: Integer;
    function GetMouseMoveScrollCursors(const AIndex: Integer): HCursor;
    function GetSearchResultCount(): Integer; {$IFNDEF Debug} inline; {$ENDIF}
    function GetSelectionAvailable(): Boolean;
    function GetSelectionBeginPosition(): TBCEditorLinesPosition;
    function GetSelectionEndPosition(): TBCEditorLinesPosition;
    function GetSelStart(): Integer;
    function GetSelText(): string;
    function GetText: string;
    function GetTextBetween(ABeginPosition, AEndPosition: TBCEditorLinesPosition): string;
    function GetUndoOptions(): TBCEditorUndoOptions;
    function GetVisibleArea(): TBCEditorLinesArea;
    function GetWordAt(ALinesPos: TPoint): string; {$IFNDEF Debug} inline; {$ENDIF}
    function GetWordAtLinesPosition(const ALinesPosition: TBCEditorLinesPosition): string;
    procedure HighlighterChanged(ASender: TObject);
    procedure InitCodeFolding();
    procedure InsertLine();
    procedure InsertLineIntoRows(const ALine: Integer; const ANewLine: Boolean); overload;
    function InsertLineIntoRows(const ALine: Integer; const ARow: Integer): Integer; overload;
    procedure InvalidateCaret();
    function IsKeywordAtPosition(const APosition: TBCEditorLinesPosition;
      const APOpenKeyWord: PBoolean = nil): Boolean;
    function IsWordSelected: Boolean;
    function LeftSpaceCount(const AText: string; AWantTabs: Boolean = False): Integer;
    function LeftTrimLength(const AText: string): Integer;
    procedure LinesChanged();
    procedure MouseMoveScrollTimerHandler(ASender: TObject);
    procedure MoveCaretAndSelection(ABeforeLinesPosition, AAfterLinesPosition: TBCEditorLinesPosition;
      const ASelectionCommand: Boolean);
    procedure MoveCaretHorizontally(const AColumns: Integer; const SelectionCommand: Boolean);
    procedure MoveCaretVertically(const ARows: Integer; const SelectionCommand: Boolean);
    function NextWordPosition(const ALinesPosition: TBCEditorLinesPosition): TBCEditorLinesPosition; overload;
    procedure PaintMouseMoveScrollPoint;
    procedure PaintSyncItems;
    function PreviousWordPosition(const ALinesPosition: TBCEditorLinesPosition): TBCEditorLinesPosition; overload;
    function Process(const AAction: TProcessAction;
      const AButton: TMouseButton; const AShift: TShiftState; const X, Y: Integer): Boolean;
    function ProcessToken(const AAction: TProcessAction;
      const AButton: TMouseButton; const AShift: TShiftState; const X, Y: Integer;
      var ARect: TRect;
      const ALinesPosition: TBCEditorLinesPosition;
      const ARowsPosition: TBCEditorRowsPosition;
      const AText: PChar; const ALength: Integer;
      const AToken: TBCEditorHighlighter.PFind = nil;
      const ARange: TBCEditorCodeFolding.TRanges.TRange = nil;
      const APaintData: PPaintTokenData = nil): Boolean;
    procedure ReplaceChanged(AEvent: TBCEditorReplaceChanges);
    function RowsToClient(ARowsPosition: TBCEditorRowsPosition): TPoint;
    function RowsToLines(const ARowsPosition: TBCEditorRowsPosition): TBCEditorLinesPosition;
    procedure ScrollChanged(ASender: TObject);
    procedure ScrollTimerHandler(ASender: TObject);
    procedure SearchChanged(AEvent: TBCEditorSearchEvent);
    procedure SelectionChanged(ASender: TObject);
    procedure SetActiveLine(const AValue: TBCEditorActiveLine);
    procedure SetBorderStyle(const AValue: TBorderStyle);
    procedure SetCaretPos(AValue: TPoint);
    procedure SetCodeFolding(const AValue: TBCEditorCodeFolding);
    procedure SetColor(const AValue: TColor);
    procedure SetDefaultKeyCommands;
    procedure SetHideScrollBars(AValue: Boolean);
    procedure SetHideSelection(AValue: Boolean);
    procedure SetHorzTextPos(AValue: Integer);
    procedure SetInsertPos(AValue: TPoint);
    procedure SetKeyCommands(const AValue: TBCEditorKeyCommands);
    procedure SetLeftMargin(const AValue: TBCEditorLeftMargin);
    procedure SetModified(const AValue: Boolean);
    procedure SetMouseCapture(const AValue: TMouseCapture);
    procedure SetMouseMoveScrollCursors(const AIndex: Integer; const AValue: HCursor);
    procedure SetOptions(const AValue: TBCEditorOptions);
    procedure SetScroll(const AValue: TBCEditorScroll);
    procedure SetSearch(const AValue: TBCEditorSearch);
    procedure SetSelectedWord;
    procedure SetSelection(const AValue: TBCEditorSelection);
    procedure SetSelectionBeginPosition(const AValue: TBCEditorLinesPosition);
    procedure SetSelectionEndPosition(const AValue: TBCEditorLinesPosition);
    procedure SetSelLength(const AValue: Integer); {$IFNDEF Debug} inline; {$ENDIF}
    procedure SetSelStart(const AValue: Integer); {$IFNDEF Debug} inline; {$ENDIF}
    procedure SetSelText(const AValue: string); {$IFNDEF Debug} inline; {$ENDIF}
    procedure SetSpecialChars(const AValue: TBCEditorSpecialChars);
    procedure SetSyncEdit(const AValue: TBCEditorSyncEdit);
    procedure SetTabs(const AValue: TBCEditorTabs);
    procedure SetText(const AValue: string); {$IFNDEF Debug} inline; {$ENDIF}
    procedure SetTopRow(const AValue: Integer);
    procedure SetUndoOptions(AOptions: TBCEditorUndoOptions);
    procedure SetWordBlock(const ALinesPosition: TBCEditorLinesPosition);
    procedure SetWordWrap(const AValue: TBCEditorWordWrap);
    procedure SpecialCharsChanged(ASender: TObject);
    procedure SyncEditChanged(ASender: TObject);
    procedure TabsChanged(ASender: TObject);
    function TokenWidth(const AText: PChar; const ALength: Integer;
      const AColumn: Integer; const AToken: TBCEditorHighlighter.TFind): Integer; // inline takes the double time. Why???
    procedure UMFreeCompletionProposalPopupWindow(var AMessage: TMessage); message UM_FREE_COMPLETIONPROPOSAL_POPUPWINDOW;
    procedure UpdateLinesBeginRanges(const ALine: Integer);
    procedure UpdateRows(const ALine: Integer);
    procedure UpdateScrollBars(const AUpdateRows: Boolean = True;
      const AUpdateWhileUpdating: Boolean = False);
    procedure UpdateWordWrap(const AValue: Boolean);
    procedure WMCaptureChanged(var AMessage: TMessage); message WM_CAPTURECHANGED;
    procedure WMChar(var AMessage: TWMChar); message WM_CHAR;
    procedure WMClear(var AMessage: TMessage); message WM_CLEAR;
    procedure WMCopy(var AMessage: TMessage); message WM_COPY;
    procedure WMCut(var AMessage: TMessage); message WM_CUT;
    procedure WMEraseBkgnd(var AMessage: TMessage); message WM_ERASEBKGND;
    procedure WMGetDlgCode(var AMessage: TWMGetDlgCode); message WM_GETDLGCODE;
    procedure WMGetText(var AMessage: TWMGetText); message WM_GETTEXT;
    procedure WMGetTextLength(var AMessage: TWMGetTextLength); message WM_GETTEXTLENGTH;
    procedure WMHScroll(var AMessage: TWMScroll); message WM_HSCROLL;
    procedure WMIMEChar(var AMessage: TMessage); message WM_IME_CHAR;
    procedure WMIMEComposition(var AMessage: TMessage); message WM_IME_COMPOSITION;
    procedure WMIMENotify(var AMessage: TMessage); message WM_IME_NOTIFY;
    procedure WMKillFocus(var AMessage: TWMKillFocus); message WM_KILLFOCUS;
    procedure WMMouseHWheel(var AMessage: TWMMouseWheel); message WM_MOUSEHWHEEL;
    procedure WMNCPaint(var AMessage: TMessage); message WM_NCPAINT;
    procedure WMPaint(var AMessage: TWMPaint); message WM_PAINT;
    procedure WMPaste(var AMessage: TMessage); message WM_PASTE;
    procedure WMSetCursor(var AMessage: TWMSetCursor); message WM_SETCURSOR;
    procedure WMSetFocus(var AMessage: TWMSetFocus); message WM_SETFOCUS;
    procedure WMSetText(var AMessage: TWMSetText); message WM_SETTEXT;
    procedure WMSettingChange(var AMessage: TWMSettingChange); message WM_SETTINGCHANGE;
    procedure WMTimer(var Msg: TWMTimer); message WM_TIMER;
    procedure WMUndo(var AMessage: TMessage); message WM_UNDO;
    procedure WMVScroll(var AMessage: TWMScroll); message WM_VSCROLL;
    procedure WordWrapChanged(ASender: TObject);
  protected
    function DragEnter(const dataObj: IDataObject; grfKeyState: Longint;
      pt: TPoint; var dwEffect: Longint): HResult; stdcall;
    function DragLeave(): HResult; stdcall;
    function DragOver(grfKeyState: Longint; pt: TPoint; var dwEffect: Longint): HResult; reintroduce; overload; stdcall;
    function Drop(const dataObj: IDataObject; grfKeyState: Longint; pt: TPoint;
      var dwEffect: Longint): HResult; stdcall;
    function GiveFeedback(dwEffect: Longint): HResult; stdcall;
    function QueryContinueDrag(fEscapePressed: BOOL; grfKeyState: Longint): HResult; stdcall;
  protected
    procedure ChainLinesCaretChanged(ASender: TObject);
    procedure ChainLinesCleared(ASender: TObject);
    procedure ChainLinesDeleted(ASender: TObject; const ALine: Integer);
    procedure ChainLinesInserted(ASender: TObject; const ALine: Integer);
    procedure ChainLinesUpdated(ASender: TObject; const ALine: Integer);
    procedure ChangeScale(M, D: Integer); override;
    procedure ClearBookmarks;
    procedure ClearMarks;
    procedure ClearMatchingPair;
    procedure ClearUndo;
    procedure CollapseCodeFoldingLevel(const AFirstLevel: Integer; const ALastLevel: Integer);
    function CollapseCodeFoldingLines(const AFirstLine: Integer = -1; const ALastLine: Integer = -1): Integer;
    function CreateLines(): BCEditor.Lines.TBCEditorLines;
    procedure CreateParams(var AParams: TCreateParams); override;
    procedure CreateWnd(); override;
    function DeleteBookmark(const ALine: Integer; const AIndex: Integer): Boolean; overload;
    procedure DeleteBookmark(ABookmark: TBCEditorMark); overload;
    procedure DeleteMark(AMark: TBCEditorMark);
    procedure DestroyWnd(); override;
    procedure DoBlockIndent(const ACommand: TBCEditorCommand);
    procedure DoCompletionProposal(); virtual;
    procedure DoCopyToClipboard(const AText: string);
    procedure DoKeyPressW(var AMessage: TWMKey);
    function DoMouseWheelDown(Shift: TShiftState; MousePos: TPoint): Boolean; override;
    function DoMouseWheelUp(Shift: TShiftState; MousePos: TPoint): Boolean; override;
    procedure DoOnCommandProcessed(ACommand: TBCEditorCommand; const AChar: Char; AData: Pointer);
    procedure DoOnProcessCommand(var ACommand: TBCEditorCommand; var AChar: Char; AData: Pointer); virtual;
    function DoOnReplaceText(const APattern, AReplaceText: string;
      APosition: TBCEditorLinesPosition): TBCEditorReplaceAction;
    function DoSearchMatchNotFoundWrapAroundDialog: Boolean; virtual;
    procedure DoSearchStringNotFoundDialog; virtual;
    procedure DoTripleClick;
    procedure DragCanceled(); override;
    procedure DragOver(ASource: TObject; X, Y: Integer; AState: TDragState; var AAccept: Boolean); overload; override;
    procedure ExpandCodeFoldingLevel(const AFirstLevel: Integer; const ALastLevel: Integer);
    function ExpandCodeFoldingLines(const AFirstLine: Integer = -1; const ALastLine: Integer = -1): Integer;
    function FindFirst(): Boolean;
    function FindNext(const AHandleNotFound: Boolean = True): Boolean;
    function FindPrevious(const AHandleNotFound: Boolean = True): Boolean;
    function GetBookmark(const AIndex: Integer; var ALinesPosition: TBCEditorLinesPosition): Boolean;
    function GetReadOnly: Boolean; virtual;
    function GetSelLength: Integer;
    procedure GotoBookmark(const AIndex: Integer);
    procedure GotoLineAndCenter(const ALine: Integer; const AChar: Integer = 1);
    procedure GotoNextBookmark;
    procedure GotoPreviousBookmark;
    function IsCommentChar(const AChar: Char): Boolean;
    function IsEmptyChar(const AChar: Char): Boolean; {$IFNDEF Debug} inline; {$ENDIF}
    function IsWordBreakChar(const AChar: Char): Boolean; {$IFNDEF Debug} inline; {$ENDIF}
    procedure KeyDown(var AKey: Word; AShift: TShiftState); override;
    procedure KeyPressW(var AKey: Char);
    procedure KeyUp(var AKey: Word; AShift: TShiftState); override;
    procedure LeftMarginChanged(ASender: TObject);
    procedure LineDeleted(ASender: TObject; const ALine: Integer);
    procedure LineDeleting(ASender: TObject; const ALine: Integer);
    procedure LineInserted(ASender: TObject; const ALine: Integer);
    procedure LinesCaretChanged(ASender: TObject);
    procedure LinesCleared(ASender: TObject);
    procedure LinesHookChanged;
    function LinesToRows(const ALinesPosition: TBCEditorLinesPosition): TBCEditorRowsPosition;
    procedure LineUpdated(ASender: TObject; const ALine: Integer); virtual;
    procedure MarkListChange(ASender: TObject);
    procedure MouseDown(AButton: TMouseButton; AShift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(AShift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(AButton: TMouseButton; AShift: TShiftState; X, Y: Integer); override;
    procedure NotifyHookedCommandHandlers(AAfterProcessing: Boolean; var ACommand: TBCEditorCommand; var AChar: Char; AData: Pointer);
    procedure Paint(); override;
    procedure ReadState(Reader: TReader); override;
    procedure Resize(); override;
    procedure ScanCodeFoldingRanges(); virtual;
    procedure ScanMatchingPair();
    procedure SetBookmark(const AIndex: Integer; const ALinesPosition: TBCEditorLinesPosition);
    procedure SetCaretAndSelection(ACaretPosition: TBCEditorLinesPosition;
      ASelArea: TBCEditorLinesArea);
    procedure SetLineColor(const ALine: Integer; const AForegroundColor, ABackgroundColor: TColor);
    procedure SetLineColorToDefault(const ALine: Integer);
    procedure SetMark(const AIndex: Integer; const ALinesPosition: TBCEditorLinesPosition;
      const AImageIndex: Integer);
    procedure SetName(const AValue: TComponentName); override;
    procedure SetOption(const AOption: TBCEditorOption; const AEnabled: Boolean);
    procedure SetReadOnly(const AValue: Boolean); virtual;
    procedure SetUndoOption(const AOption: TBCEditorUndoOption; const AEnabled: Boolean);
    procedure SetUpdateState(AUpdating: Boolean); virtual;
    procedure SetWantReturns(const AValue: Boolean);
    procedure ScrollToCaret(ACenterVertical: Boolean = False; AScrollAlways: Boolean = False);
    procedure ToggleBookmark(const AIndex: Integer = -1);
    procedure UpdateCaret();
    function WordBegin(const ALinesPosition: TBCEditorLinesPosition): TBCEditorLinesPosition; overload;
    function WordEnd(): TBCEditorLinesPosition; overload; {$IFNDEF Debug} inline; {$ENDIF}
    function WordEnd(const ALinesPosition: TBCEditorLinesPosition): TBCEditorLinesPosition; overload;
    property AllCodeFoldingRanges: TBCEditorCodeFolding.TAllRanges read FAllCodeFoldingRanges;
    property Color: TColor read FColor write SetColor default clWindow;
    property HideScrollBars: Boolean read FHideScrollBars write SetHideScrollBars default True;
    property HideSelection: Boolean read FHideSelection write SetHideSelection default True;
    property HorzTextPos: Integer read FHorzTextPos write SetHorzTextPos;
    property LineHeight: Integer read FLineHeight;
    property MouseCapture: TMouseCapture read FMouseCapture write SetMouseCapture;
    property OnAfterBookmarkPlaced: TNotifyEvent read FOnAfterBookmarkPlaced write FOnAfterBookmarkPlaced;
    property OnAfterDeleteBookmark: TNotifyEvent read FOnAfterDeleteBookmark write FOnAfterDeleteBookmark;
    property OnAfterDeleteMark: TNotifyEvent read FOnAfterDeleteMark write FOnAfterDeleteMark;
    property OnAfterMarkPlaced: TNotifyEvent read FOnAfterMarkPlaced write FOnAfterMarkPlaced;
    property OnBeforeCompletionProposalExecute: TBCEditorCompletionProposalEvent read FOnBeforeCompletionProposalExecute write FOnBeforeCompletionProposalExecute;
    property OnBeforeDeleteMark: TBCEditorMarkEvent read FOnBeforeDeleteMark write FOnBeforeDeleteMark;
    property OnBeforeMarkPlaced: TBCEditorMarkEvent read FOnBeforeMarkPlaced write FOnBeforeMarkPlaced;
    property OnCaretChanged: TBCEditorCaretChangedEvent read FOnCaretChanged write FOnCaretChanged;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property OnCommandProcessed: TBCEditorProcessCommandEvent read FOnCommandProcessed write FOnCommandProcessed;
    property OnCompletionProposalCanceled: TNotifyEvent read FOnCompletionProposalCanceled write FOnCompletionProposalCanceled;
    property OnCompletionProposalSelected: TBCEditorCompletionProposalPopupWindowSelectedEvent read FOnCompletionProposalSelected write FOnCompletionProposalSelected;
    property OnContextHelp: TBCEditorContextHelpEvent read FOnContextHelp write FOnContextHelp;
    property OnHint: TBCEditorHintEvent read FOnHint write FOnHint;
    property OnKeyPress: TBCEditorKeyPressWEvent read FOnKeyPressW write FOnKeyPressW;
    property OnModified: TNotifyEvent read FOnModified write FOnModified;
    property OnProcessCommand: TBCEditorProcessCommandEvent read FOnProcessCommand write FOnProcessCommand;
    property OnProcessUserCommand: TBCEditorProcessCommandEvent read FOnProcessUserCommand write FOnProcessUserCommand;
    property OnReplaceText: TBCEditorReplaceEvent read FOnReplaceText write FOnReplaceText;
    property OnRightMarginMouseUp: TNotifyEvent read FOnRightMarginMouseUp write FOnRightMarginMouseUp;
    property OnSelectionChanged: TNotifyEvent read FOnSelectionChanged write FOnSelectionChanged;
    property Options: TBCEditorOptions read FOptions write SetOptions default DefaultOptions;
    property PaintHelper: TBCEditorPaintHelper read FPaintHelper;
    property TopRow: Integer read FTopRow write SetTopRow;
    property UndoOptions: TBCEditorUndoOptions read GetUndoOptions write SetUndoOptions default DefaultUndoOptions;
    property VisibleArea: TBCEditorLinesArea read GetVisibleArea;
    property VisibleRows: Integer read FVisibleRows;
    property WordWrap: TBCEditorWordWrap read FWordWrap write SetWordWrap;
  public
    procedure ActivateHint(const X, Y: Integer; const AHint: string);
    procedure AddHighlighterKeywords(AStringList: TStrings);
    procedure AddKeyCommand(ACommand: TBCEditorCommand; AShift: TShiftState; AKey: Word;
      ASecondaryShift: TShiftState = []; ASecondaryKey: Word = 0);
    procedure AddKeyDownHandler(AHandler: TKeyEvent);
    procedure AddKeyPressHandler(AHandler: TBCEditorKeyPressWEvent);
    procedure AddKeyUpHandler(AHandler: TKeyEvent);
    procedure AddMouseCursorHandler(AHandler: TBCEditorMouseCursorEvent);
    procedure AddMouseDownHandler(AHandler: TMouseEvent);
    procedure AddMouseUpHandler(AHandler: TMouseEvent);
    procedure Assign(ASource: TPersistent); override;
    procedure BeginUndoBlock();
    procedure BeginUpdate();
    procedure ChainEditor(AEditor: TCustomBCEditor);
    function CharIndexToPos(const ACharIndex: Integer): TPoint; {$IFNDEF Debug} inline; {$ENDIF}
    procedure Clear();
    function ClientToPos(const X, Y: Integer): TPoint; {$IFNDEF Debug} inline; {$ENDIF}
    function ClientToText(const X, Y: Integer): TPoint; deprecated 'Use ClientToPos'; // 2017-05-13
    function CharAtCursor(): Char; deprecated 'Use CharAt[CaretPos]'; // 2017-04-05
    procedure CommandProcessor(ACommand: TBCEditorCommand; AChar: Char; AData: Pointer);
    procedure CopyToClipboard();
    constructor Create(AOwner: TComponent); override;
    procedure CutToClipboard;
    destructor Destroy(); override;
    procedure DoRedo(); {$IFNDEF Debug} inline; {$ENDIF} deprecated 'Use Redo()'; // 2017-02-12
    procedure DoUndo(); {$IFNDEF Debug} inline; {$ENDIF} deprecated 'Use Undo()'; // 2017-02-12
    procedure DragDrop(ASource: TObject; X, Y: Integer); override;
    procedure EndUndoBlock();
    procedure EndUpdate();
    function ExecuteAction(Action: TBasicAction): Boolean; override;
    procedure ExecuteCommand(ACommand: TBCEditorCommand; AChar: Char; AData: Pointer); virtual;
    procedure ExportToHTML(const AFileName: string; const ACharSet: string = ''; AEncoding: TEncoding = nil); overload;
    procedure ExportToHTML(AStream: TStream; const ACharSet: string = ''; AEncoding: TEncoding = nil); overload;
    procedure FindAll;
    procedure HookEditorLines(ALines: TBCEditorLines; AUndo, ARedo: TBCEditorLines.TUndoList);
    procedure LoadFromFile(const AFileName: string; AEncoding: TEncoding = nil); deprecated 'Use Lines.LoadFromFile'; // 2017-03-10
    procedure LoadFromStream(AStream: TStream; AEncoding: TEncoding = nil); deprecated 'Use Lines.LoadFromStream'; // 2017-03-10
    procedure Notification(AComponent: TComponent; AOperation: TOperation); override;
    procedure PasteFromClipboard();
    function PosToCharIndex(const APos: TPoint): Integer;
    procedure Redo(); {$IFNDEF Debug} inline; {$ENDIF}
    procedure RegisterCommandHandler(const AHookedCommandEvent: TBCEditorHookedCommandEvent; AHandlerData: Pointer);
    procedure RemoveChainedEditor;
    procedure RemoveKeyDownHandler(AHandler: TKeyEvent);
    procedure RemoveKeyPressHandler(AHandler: TBCEditorKeyPressWEvent);
    procedure RemoveKeyUpHandler(AHandler: TKeyEvent);
    procedure RemoveMouseCursorHandler(AHandler: TBCEditorMouseCursorEvent);
    procedure RemoveMouseDownHandler(AHandler: TMouseEvent);
    procedure RemoveMouseUpHandler(AHandler: TMouseEvent);
    function ReplaceText(): Integer;
    procedure SaveToFile(const AFileName: string; AEncoding: TEncoding = nil);
    procedure SaveToStream(AStream: TStream; AEncoding: TEncoding = nil);
    function SearchStatus: string;
    procedure SelectAll;
    function SelectedText(): string; deprecated 'Use SelText'; // 2017-03-16
    procedure Sort(const ASortOrder: TBCEditorSortOrder = soAsc; const ACaseSensitive: Boolean = False);
    function SplitTextIntoWords(AStringList: TStrings; const ACaseSensitive: Boolean): string;
    function TextCaretPosition(): TBCEditorLinesPosition; deprecated 'Use CaretPos'; // 2017-02-12
    procedure ToggleSelectedCase(const ACase: TBCEditorCase = cNone);
    function TranslateKeyCode(const ACode: Word; const AShift: TShiftState; var AData: Pointer): TBCEditorCommand;
    procedure Undo(); {$IFNDEF Debug} inline; {$ENDIF}
    procedure UnhookEditorLines;
    function UpdateAction(Action: TBasicAction): Boolean; override;
    procedure UnregisterCommandHandler(AHookedCommandEvent: TBCEditorHookedCommandEvent);
    procedure WndProc(var AMessage: TMessage); override;
    function WordAtCursor(): string; deprecated 'Use WordAt[CaretPos]'; // 2017-03-13
    property ActiveLine: TBCEditorActiveLine read FActiveLine write SetActiveLine;
    property Bookmarks: TBCEditorMarkList read FBookmarkList;
    property BorderStyle: TBorderStyle read FBorderStyle write SetBorderStyle default bsSingle;
    property CanPaste: Boolean read GetCanPaste;
    property CanRedo: Boolean read GetCanRedo;
    property CanUndo: Boolean read GetCanUndo;
    property Caret: TBCEditorCaret read FCaret write FCaret;
    property CaretPos: TPoint read GetCaretPos write SetCaretPos;
    property CharAt[Pos: TPoint]: Char read GetCharAt;
    property CodeFolding: TBCEditorCodeFolding read FCodeFolding write SetCodeFolding;
    property CompletionProposal: TBCEditorCompletionProposal read FCompletionProposal write FCompletionProposal;
    property Highlighter: TBCEditorHighlighter read FHighlighter;
    property InsertPos: TPoint read FInsertPos write SetInsertPos;
    property KeyCommands: TBCEditorKeyCommands read FKeyCommands write SetKeyCommands stored False;
    property LeftMargin: TBCEditorLeftMargin read FLeftMargin write SetLeftMargin;
    property Lines: TBCEditorLines read FLines;
    property Marks: TBCEditorMarkList read FMarkList;
    property MatchingPair: TBCEditorMatchingPair read FMatchingPair write FMatchingPair;
    property Modified: Boolean read GetModified write SetModified;
    property MouseMoveScrollCursors[const AIndex: Integer]: HCursor read GetMouseMoveScrollCursors write SetMouseMoveScrollCursors;
    property ParentColor default False;
    property ParentFont default False;
    property ReadOnly: Boolean read GetReadOnly write SetReadOnly default False;
    property Replace: TBCEditorReplace read FReplace write FReplace;
    property Scroll: TBCEditorScroll read FScroll write SetScroll;
    property Search: TBCEditorSearch read FSearch write SetSearch;
    property SearchResultCount: Integer read GetSearchResultCount;
    property Selection: TBCEditorSelection read FSelection write SetSelection;
    property SelectionAvailable: Boolean read GetSelectionAvailable;
    property SelectionBeginPosition: TBCEditorLinesPosition read GetSelectionBeginPosition write SetSelectionBeginPosition;
    property SelectionEndPosition: TBCEditorLinesPosition read GetSelectionEndPosition write SetSelectionEndPosition;
    property SelLength: Integer read GetSelLength write SetSelLength;
    property SelStart: Integer read GetSelStart write SetSelStart; // 0-based
    property SelText: string read GetSelText write SetSelText;
    property SpecialChars: TBCEditorSpecialChars read FSpecialChars write SetSpecialChars;
    property SyncEdit: TBCEditorSyncEdit read FSyncEdit write SetSyncEdit;
    property Tabs: TBCEditorTabs read FTabs write SetTabs;
    property TabStop default True;
    property Text: string read GetText write SetText;
    property TextBetween[ALinesBeginPosition, ALinesEndPosition: TBCEditorLinesPosition]: string read GetTextBetween;
    property TextEntryMode: TBCEditorTextEntryMode read FTextEntryMode write FTextEntryMode default temInsert;
    property WantReturns: Boolean read FWantReturns write SetWantReturns default True;
    property WordAt[ATextPos: TPoint]: string read GetWordAt;
    property UpdateCount: Integer read FUpdateCount;
  end;

  TBCEditor = class(TCustomBCEditor)
  published
    property ActiveLine;
    property Align;
    property Anchors;
    property BorderStyle;
    property Caret;
    property CodeFolding;
    property CompletionProposal;
    property Constraints;
    property Ctl3D;
    property Cursor;
    property Enabled;
    property Font;
    property Height;
    property HideScrollBars;
    property HideSelection;
    property Highlighter;
    property ImeMode;
    property ImeName;
    property KeyCommands;
    property LeftMargin;
    property Lines;
    property MatchingPair;
    property Name;
    property OnAfterBookmarkPlaced;
    property OnAfterDeleteBookmark;
    property OnAfterDeleteMark;
    property OnAfterMarkPlaced;
    property OnBeforeCompletionProposalExecute;
    property OnBeforeDeleteMark;
    property OnBeforeMarkPlaced;
    property OnCaretChanged;
    property OnChange;
    property OnClick;
    property OnCommandProcessed;
    property OnCompletionProposalCanceled;
    property OnCompletionProposalSelected;
    property OnContextHelp;
    property OnContextPopup;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnHint;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnModified;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnMouseWheel;
    property OnMouseWheelDown;
    property OnMouseWheelUp;
    property OnProcessCommand;
    property OnProcessUserCommand;
    property OnReplaceText;
    property OnRightMarginMouseUp;
    property OnSelectionChanged;
    property OnStartDock;
    property OnStartDrag;
    property Options;
    property ParentColor;
    property ParentCtl3D;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ReadOnly;
    property Replace;
    property Scroll;
    property Search;
    property Selection;
    property ShowHint;
    property SpecialChars;
    property SyncEdit;
    property TabOrder;
    property Tabs;
    property TabStop;
    property Tag;
    property TextEntryMode;
    property UndoOptions;
    property Visible;
    property WantReturns;
    property Width;
    property WordWrap;
  end;

  EBCEditorBaseException = class(Exception);

implementation {***************************************************************}

{$R BCEditor.res}

uses
  ShellAPI, Imm, CommCtrl,
  Math, Types, Character, RegularExpressions, ComObj, SysConst,
  Clipbrd, Menus, Themes, ImgList,
  BCEditor.Language, BCEditor.Export.HTML;

resourcestring
  SBCEditorLineIsNotVisible = 'Line %d is not visible';

type
  TUnprotectedWinControl = class(TWinControl);

const
  iiBookmark0 = 0;
  iiCodeFoldingExpanded = 10;
  iiCodeFoldingCollapsed = 11;
  iiCodeFoldingLine = 12;
  iiCodeFoldingEndLine = 13;

  tiCodeFolding = 0;
  tiShowHint = 1;

  scNone = -1;
  scNorth = 0;
  scNorthEast = 1;
  scEast = 2;
  scSouthEast = 3;
  scSouth = 4;
  scSouthWest = 5;
  scWest = 6;
  scNorthWest = 7;

var
  GLineWidth: Integer;
  GPadding: Integer;
  GRightMarginHintWindow: THintWindow;
  GScrollHintWindow: THintWindow;

function GetScrollHint: THintWindow;
begin
  if not Assigned(GScrollHintWindow) then
  begin
    GScrollHintWindow := THintWindow.Create(Application);
    GScrollHintWindow.DoubleBuffered := True;
  end;
  Result := GScrollHintWindow;
end;

function GetRightMarginHint: THintWindow;
begin
  if not Assigned(GRightMarginHintWindow) then
  begin
    GRightMarginHintWindow := THintWindow.Create(Application);
    GRightMarginHintWindow.DoubleBuffered := True;
  end;
  Result := GRightMarginHintWindow;
end;

function IsCombiningDiacriticalMark(const AChar: Char): Boolean;
begin
  case Word(AChar) of
    $0300..$036F, $1DC0..$1DFF, $20D0..$20FF:
      Result := True
  else
    Result := False;
  end;
end;

function MessageDialog(const AMessage: string; ADlgType: TMsgDlgType; AButtons: TMsgDlgButtons): Integer;
begin
  with CreateMessageDialog(AMessage, ADlgType, AButtons) do
  try
    HelpContext := 0;
    HelpFile := '';
    Position := poMainFormCenter;
    Result := ShowModal;
  finally
    Free;
  end;
end;

function MiddleColor(AColor1, AColor2: TColor): TColor;
var
  LBlue: Byte;
  LBlue1: Byte;
  LBlue2: Byte;
  LGreen: Byte;
  LGreen1: Byte;
  LGreen2: Byte;
  LRed: Byte;
  LRed1: Byte;
  LRed2: Byte;
begin
  LRed1 := GetRValue(AColor1);
  LRed2 := GetRValue(AColor2);
  LGreen1 := GetRValue(AColor1);
  LGreen2 := GetRValue(AColor2);
  LBlue1 := GetRValue(AColor1);
  LBlue2 := GetRValue(AColor2);

  LRed := (LRed1 + LRed2) div 2;
  LGreen := (LGreen1 + LGreen2) div 2;
  LBlue := (LBlue1 + LBlue2) div 2;

  Result := RGB(LRed, LGreen, LBlue);
end;

{ TCustomBCEditor.TDropData ***************************************************}

function TCustomBCEditor.TDropData.Clone(out Enum: IEnumFormatEtc): HResult;
begin
  Enum := TDropData.Create(FEditor);
  Result := S_OK;
end;

constructor TCustomBCEditor.TDropData.Create(const AEditor: TCustomBCEditor);
begin
  inherited Create();

  FEditor := AEditor;
end;

function TCustomBCEditor.TDropData.DAdvise(const formatetc: TFormatEtc; advf: Longint;
  const advSink: IAdviseSink; out dwConnection: Longint): HResult;
begin
  Result := OLE_E_ADVISENOTSUPPORTED;
end;

function TCustomBCEditor.TDropData.DUnadvise(dwConnection: Longint): HResult;
begin
  Result := OLE_E_ADVISENOTSUPPORTED;
end;

function TCustomBCEditor.TDropData.EnumDAdvise(out enumAdvise: IEnumStatData): HResult;
begin
  Result := OLE_E_ADVISENOTSUPPORTED;
end;

function TCustomBCEditor.TDropData.EnumFormatEtc(dwDirection: Longint; out enumFormatEtc:
  IEnumFormatEtc): HResult;
begin
  case (dwDirection) of
    DATADIR_GET:
      begin
        enumFormatEtc := Self;
        Result := S_OK;
      end;
    else
      raise ERangeError.Create(SRangeError);
  end;
end;

function TCustomBCEditor.TDropData.GetCanonicalFormatEtc(const formatetc: TFormatEtc;
  out formatetcOut: TFormatEtc): HResult;
begin
  MoveMemory(@formatetcOut, @formatetc, SizeOf(formatetc));
  formatetcOut.ptd := nil;
  Result := DATA_S_SAMEFORMATETC;
end;

function TCustomBCEditor.TDropData.GetData(const formatetcIn: TFormatEtc; out medium: TStgMedium):
  HResult;
var
  LText: string;
begin
  if (formatetcin.lindex <> -1) then
    Result := DV_E_LINDEX
  else if (formatetcin.tymed <> TYMED_HGLOBAL) then
    Result := DV_E_TYMED
  else
  begin
    Result := S_OK;
    case (formatetcin.cfFormat) of
      CF_UNICODETEXT: LText := FEditor.SelText;
      else Result := DV_E_FORMATETC;
    end;

    if (Result = S_OK) then
    begin
      FillChar(medium, SizeOf(medium), 0);
      medium.tymed := TYMED_HGLOBAL;
      medium.hGlobal := GlobalAlloc(GMEM_MOVEABLE + GMEM_DDESHARE, SizeOf(LText[1]) * Length(LText));
      MoveMemory(GlobalLock(medium.hGlobal), PChar(LText), Length(LText) * SizeOf(LText[1]));
    end;
  end;
end;

function TCustomBCEditor.TDropData.GetDataHere(const formatetc: TFormatEtc; out medium: TStgMedium):
  HResult;
var
  LText: string;
begin
  LText := FEditor.SelText;

  if (formatetc.lindex <> -1) then
    Result := DV_E_LINDEX
  else if (formatetc.tymed <> TYMED_HGLOBAL) then
    Result := DV_E_TYMED
  else if (GlobalSize(medium.hGlobal) < SIZE_T(Length(LText) * SizeOf(LText[1]))) then
    Result := STG_E_MEDIUMFULL
  else
  begin
    MoveMemory(GlobalLock(medium.hGlobal), PChar(LText), Length(LText) * SizeOf(LText[1]));
    Result := S_OK;
  end;
end;

function TCustomBCEditor.TDropData.Next(celt: Longint; out elt; pceltFetched: PLongint): HResult;
type
  TFormatEtcArray2 = array [0 .. $FFFF] of FORMATETC;
var
  Formats: ^TFormatEtcArray2;
begin
  if ((celt = 0) or (celt > 1) and not Assigned(pceltFetched)
    or (FEnumFormatEtcIndex = 1)) then
    Result := S_FALSE
  else
  begin
    Formats := @elt;

    case (FEnumFormatEtcIndex) of
      0:
        begin
          Formats^[0].cfFormat := CF_UNICODETEXT;
          Formats^[0].ptd := nil;
          Formats^[0].dwAspect := DVASPECT_CONTENT;
          Formats^[0].lindex := -1;
          Formats^[0].tymed := TYMED_HGLOBAL;
        end;
      else
        raise ERangeError.Create('Index: ' + IntToStr(FEnumFormatEtcIndex));
    end;
    Inc(FEnumFormatEtcIndex);
    if (Assigned(pceltFetched)) then
      Inc(pceltFetched^);

    if (celt = 1) then
      Result := S_OK
    else
      Result := Next(celt - 1, Formats^[1], pceltFetched);
  end;
end;

function TCustomBCEditor.TDropData.QueryGetData(const formatetc: TFormatEtc): HResult;
var
  LFormat: TFormatEtc;
begin
  if (formatetc.lindex <> -1) then
    Result := DV_E_LINDEX
  else if (formatetc.tymed <> TYMED_HGLOBAL) then
    Result := DV_E_TYMED
  else
  begin
    Reset();
    repeat
      Result := Next(1, LFormat, nil);
    until ((Result <> S_OK) or (LFormat.cfFormat = formatetc.cfFormat));

    if (Result = S_FALSE) then
      Result := DV_E_FORMATETC;
  end;
end;

function TCustomBCEditor.TDropData.Reset(): HResult;
begin
  FEnumFormatEtcIndex := 0;

  Result := S_OK;
end;

function TCustomBCEditor.TDropData.SetData(const formatetc: TFormatEtc; var medium: TStgMedium;
  fRelease: BOOL): HResult;
begin
  Result := E_FAIL;
end;

function TCustomBCEditor.TDropData.Skip(celt: Longint): HResult;
begin
  Result := S_FALSE;
end;

{ TCustomBCEditor.TRows *******************************************************}

procedure TCustomBCEditor.TRows.Add(const AFlags: TRow.TFlags; const ALine: Integer;
  const AChar, ALength, AColumns, AWidth: Integer; const ABeginRange: Pointer);
begin
  Insert(Count, AFlags, ALine, AChar, ALength, AColumns, AWidth, ABeginRange);
end;

procedure TCustomBCEditor.TRows.Clear();
begin
  inherited;

  FCaretPosition := InvalidRowsPosition;
  FMaxColumns := -1;
  FMaxColumnsRow := -1;
end;

constructor TCustomBCEditor.TRows.Create(const AEditor: TCustomBCEditor);
begin
  inherited Create();

  FEditor := AEditor;

  FMaxColumns := -1;
  FMaxColumnsRow := -1;
end;

procedure TCustomBCEditor.TRows.Delete(ARow: Integer);
begin
  inherited;

  if (FMaxColumnsRow = ARow) then
  begin
    FMaxColumns := -1;
    FMaxColumnsRow := -1;
  end
  else if (FMaxColumnsRow > ARow) then
    Dec(FMaxColumnsRow);
  if (FMaxWidthRow = ARow) then
  begin
    FMaxWidth := -1;
    FMaxWidthRow := -1;
  end
  else if (FMaxWidthRow > ARow) then
    Dec(FMaxWidthRow);
end;

function TCustomBCEditor.TRows.GetCaretPosition(): TBCEditorRowsPosition;
begin
  if (FCaretPosition = InvalidRowsPosition) then
    FCaretPosition := FEditor.LinesToRows(FEditor.FLines.CaretPosition);

  Result := FCaretPosition;
end;

function TCustomBCEditor.TRows.GetBORPosition(ARow: Integer): TBCEditorLinesPosition;
var
  LChar: Integer;
  LRow: Integer;
begin
  if (ARow < Count) then
  begin
    LChar := 0;
    LRow := FEditor.FLines.Items[Items[ARow].Line].FirstRow;
    while (LRow < ARow) do
    begin
      Inc(LChar, Items[LRow].Length);
      Inc(LRow);
    end;
    Result := LinesPosition(LChar, Items[ARow].Line);
  end
  else
    Result := FEditor.FLines.BOLPosition[(ARow - Count) + FEditor.FLines.Count];
end;

function TCustomBCEditor.TRows.GetEORPosition(ARow: Integer): TBCEditorLinesPosition;
begin
  Assert((0 <= ARow) and (ARow < Count));

  if (not (rfLastRowOfLine in Items[ARow].Flags)) then
    Result := LinesPosition(Items[ARow].Char + Items[ARow].Length - 1, Items[ARow].Line)
  else
    Result := FEditor.FLines.EOLPosition[Items[ARow].Line];
end;

function TCustomBCEditor.TRows.GetMaxColumns(): Integer;
var
  LRow: Integer;
begin
  if ((FMaxColumns < 0) and (Count > 0)) then
    for LRow := 0 to Count - 1 do
      if (Items[LRow].Columns > FMaxColumns) then
      begin
        FMaxColumnsRow := LRow;
        FMaxColumns := Items[LRow].Columns;
      end;

  Result := FMaxColumns;
end;

function TCustomBCEditor.TRows.GetMaxWidth(): Integer;
var
  LRow: Integer;
begin
  if ((FMaxWidth < 0) and (Count > 0)) then
    for LRow := 0 to Count - 1 do
      if (Items[LRow].Width > FMaxWidth) then
      begin
        FMaxWidthRow := LRow;
        FMaxWidth := Items[LRow].Width;
      end;

  Result := FMaxWidth;
end;

function TCustomBCEditor.TRows.GetRowArea(ARow: Integer): TBCEditorLinesArea;
begin
  Result.BeginPosition := BORPosition[ARow];
  Result.EndPosition := EORPosition[ARow];
end;

function TCustomBCEditor.TRows.GetText(ARow: Integer): string;
begin
  Assert((0 <= ARow) and (ARow < Count),
    'ARow: ' + IntToStr(ARow) + #13#10
    + 'Count: ' + IntToStr(Count));

  Result := Copy(FEditor.FLines.Items[Items[ARow].Line].Text, 1 + Items[ARow].Char, Items[ARow].Length);
end;

procedure TCustomBCEditor.TRows.Insert(ARow: Integer; const AFlags: TRow.TFlags;
  const ALine: Integer; const AChar, ALength, AColumns, AWidth: Integer; const ABeginRange: Pointer);
var
  LItem: TRow;
  LPos: PChar;
  LEndPos: PChar;
begin
  Assert((0 <= ARow) and (ARow <= Count));

  LItem.BeginRange := ABeginRange;
  LItem.Flags := AFlags;
  LItem.Char := AChar;
  LItem.Length := ALength;
  LItem.Line := ALine;
  LItem.Columns := AColumns;
  LItem.Width := AWidth;

  if ((ALength > 0) and (lfHasTabs in FEditor.FLines.Items[ALine].Flags)) then
  begin
    LPos := @FEditor.FLines.Items[ALine].Text[1 + AChar];
    LEndPos := @LPos[ALength - 1];
    while (LPos <= LEndPos) do
    begin
      if (LPos^ = BCEDITOR_TAB_CHAR) then
      begin
        Include(LItem.Flags, rfHasTabs);
        break;
      end;
      Inc(LPos);
    end;
  end;

  inherited Insert(ARow, LItem);

  if ((FMaxColumns >= 0) and (AColumns > FMaxColumns)) then
  begin
    FMaxColumns := AColumns;
    FMaxColumnsRow := ARow;
  end
  else if (FMaxColumnsRow >= ARow) then
    Inc(FMaxColumnsRow);

  if ((FMaxWidth >= 0) and (AWidth > FMaxWidth)) then
  begin
    FMaxWidth := AWidth;
    FMaxWidthRow := ARow;
  end
  else if (FMaxWidthRow >= ARow) then
    Inc(FMaxWidthRow);
end;

{ TBCCustomEditor *************************************************************}

procedure TCustomBCEditor.ActivateHint(const X, Y: Integer; const AHint: string);
var
  LRect: TRect;
  LScreen: TPoint;
  LHint: string;
begin
  if (not Assigned(FHintWindow)) then
  begin
    FHintWindow := THintWindow.Create(Self);
    FHintWindow.Color := clInfoBk;
  end;

  LScreen := ClientToScreen(Point(X, Y));
  LRect := Rect(
    LScreen.X,
    LScreen.Y,
    LScreen.X + 6 + FHintWindow.Canvas.TextWidth(AHint),
    LScreen.Y + 4 + FHintWindow.Canvas.TextHeight(AHint));

  LRect := ClientRect;
  LHint := AHint;
  FHintWindow.Canvas.TextRect(LRect, LHint, [tfCalcRect, tfNoPrefix, tfWordBreak]);
  LRect := Rect(
    LScreen.X,
    LScreen.Y,
    LScreen.X + 6 + LRect.Width,
    LScreen.Y + 4 + LRect.Height);

  FHintWindow.ActivateHint(LRect, AHint);
end;

procedure TCustomBCEditor.ActiveLineChanged(Sender: TObject);
begin
  Invalidate();
end;

procedure TCustomBCEditor.AddHighlighterKeywords(AStringList: TStrings);
var
  LChar: Char;
  LIndex: Integer;
  LKeywordStringList: TStringList;
  LStringList: TStringList;
  LWord: string;
  LWordList: string;
begin
  LStringList := TStringList.Create;
  LKeywordStringList := TStringList.Create;
  LWordList := AStringList.Text;
  try
    FHighlighter.AddKeywords(LKeywordStringList);
    for LIndex := 0 to LKeywordStringList.Count - 1 do
    begin
      LWord := LKeywordStringList.Strings[LIndex];
      if Length(LWord) > 1 then
      begin
        LChar := LWord[1];
        if LChar.IsLower or LChar.IsUpper or (LChar = BCEDITOR_UNDERSCORE) then
          if Pos(LWord + BCEDITOR_CARRIAGE_RETURN + BCEDITOR_LINEFEED, LWordList) = 0 then { No duplicates }
            LWordList := LWordList + LWord + BCEDITOR_CARRIAGE_RETURN + BCEDITOR_LINEFEED;
      end;
    end;
    LStringList.Text := LWordList;
    LStringList.Sort;
    AStringList.Assign(LStringList);
  finally
    LKeywordStringList.Free;
    LStringList.Free;
  end;
end;

procedure TCustomBCEditor.AddKeyCommand(ACommand: TBCEditorCommand; AShift: TShiftState; AKey: Word;
  ASecondaryShift: TShiftState; ASecondaryKey: Word);
var
  LKeyCommand: TBCEditorKeyCommand;
begin
  LKeyCommand := KeyCommands.NewItem;
  with LKeyCommand do
  begin
    Command := ACommand;
    Key := AKey;
    SecondaryKey := ASecondaryKey;
    ShiftState := AShift;
    SecondaryShiftState := ASecondaryShift;
  end;
end;

procedure TCustomBCEditor.AddKeyDownHandler(AHandler: TKeyEvent);
begin
  FKeyboardHandler.AddKeyDownHandler(AHandler);
end;

procedure TCustomBCEditor.AddKeyPressHandler(AHandler: TBCEditorKeyPressWEvent);
begin
  FKeyboardHandler.AddKeyPressHandler(AHandler);
end;

procedure TCustomBCEditor.AddKeyUpHandler(AHandler: TKeyEvent);
begin
  FKeyboardHandler.AddKeyUpHandler(AHandler);
end;

procedure TCustomBCEditor.AddMouseCursorHandler(AHandler: TBCEditorMouseCursorEvent);
begin
  FKeyboardHandler.AddMouseCursorHandler(AHandler);
end;

procedure TCustomBCEditor.AddMouseDownHandler(AHandler: TMouseEvent);
begin
  FKeyboardHandler.AddMouseDownHandler(AHandler);
end;

procedure TCustomBCEditor.AddMouseUpHandler(AHandler: TMouseEvent);
begin
  FKeyboardHandler.AddMouseUpHandler(AHandler);
end;

procedure TCustomBCEditor.AfterLinesUpdate(Sender: TObject);
begin
  EndUpdate();
end;

procedure TCustomBCEditor.Assign(ASource: TPersistent);
begin
  if Assigned(ASource) and (ASource is TCustomBCEditor) then
    with ASource as TCustomBCEditor do
    begin
      Self.FActiveLine.Assign(FActiveLine);
      Self.FCaret.Assign(FCaret);
      Self.FCodeFolding.Assign(FCodeFolding);
      Self.FCompletionProposal.Assign(FCompletionProposal);
      Self.FKeyCommands.Assign(FKeyCommands);
      Self.FLeftMargin.Assign(FLeftMargin);
      Self.FMatchingPair.Assign(FMatchingPair);
      Self.FReplace.Assign(FReplace);
      Self.FScroll.Assign(FScroll);
      Self.FSearch.Assign(FSearch);
      Self.FSelection.Assign(FSelection);
      Self.FSpecialChars.Assign(FSpecialChars);
      Self.FSyncEdit.Assign(FSyncEdit);
      Self.FTabs.Assign(FTabs);
      Self.FWordWrap.Assign(FWordWrap);
    end
  else
    inherited Assign(ASource);
end;

procedure TCustomBCEditor.BeforeLinesUpdate(Sender: TObject);
begin
  BeginUpdate();
end;

procedure TCustomBCEditor.BeginUndoBlock;
begin
  FLines.BeginUpdate();
end;

procedure TCustomBCEditor.BeginUpdate();
begin
  if (FUpdateCount = 0) then SetUpdateState(True);
  Inc(FUpdateCount);
end;

procedure TCustomBCEditor.BookmarkListChange(Sender: TObject);
begin
  Invalidate;
end;

procedure TCustomBCEditor.ChainEditor(AEditor: TCustomBCEditor);
begin
  if Highlighter.FileName = '' then
    Highlighter.LoadFromFile(AEditor.Highlighter.FileName);
  if Highlighter.Colors.FileName = '' then
    Highlighter.Colors.LoadFromFile(AEditor.Highlighter.Colors.FileName);

  HookEditorLines(AEditor.FLines, AEditor.FLines.UndoList, AEditor.FLines.RedoList);
  InitCodeFolding;
  FChainedEditor := AEditor;
  AEditor.FreeNotification(Self);
end;

procedure TCustomBCEditor.ChainLinesCaretChanged(ASender: TObject);
begin
  if Assigned(FOnChainCaretMoved) then
    FOnChainCaretMoved(ASender);
  FOriginalLines.OnCaretChanged(ASender);
end;

procedure TCustomBCEditor.ChainLinesCleared(ASender: TObject);
begin
  if Assigned(FOnChainLinesCleared) then
    FOnChainLinesCleared(ASender);
  FOriginalLines.OnCleared(ASender);
end;

procedure TCustomBCEditor.ChainLinesDeleted(ASender: TObject; const ALine: Integer);
begin
  if Assigned(FOnChainLinesDeleted) then
    FOnChainLinesDeleted(ASender, ALine);
  FOriginalLines.OnDeleted(ASender, ALine);
end;

procedure TCustomBCEditor.ChainLinesInserted(ASender: TObject; const ALine: Integer);
begin
  if Assigned(FOnChainLinesInserted) then
    FOnChainLinesInserted(ASender, ALine);
  FOriginalLines.OnInserted(ASender, ALine);
end;

procedure TCustomBCEditor.ChainLinesUpdated(ASender: TObject; const ALine: Integer);
begin
  if Assigned(FOnChainLinesUpdated) then
    FOnChainLinesUpdated(ASender, ALine);
  FOriginalLines.OnUpdated(ASender, ALine);
end;

procedure TCustomBCEditor.ChangeScale(M, D: Integer);
begin
  FCompletionProposal.ChangeScale(M, D);
end;

function TCustomBCEditor.CharAtCursor(): Char;
begin
  if (FLines.Count = 0) then
    Result := BCEDITOR_NONE_CHAR
  else
    Result := FLines.Char[FLines.CaretPosition];
end;

function TCustomBCEditor.CharIndexToPos(const ACharIndex: Integer): TPoint;
begin
  Result := Point(FLines.PositionOf(ACharIndex));
end;

procedure TCustomBCEditor.CheckIfAtMatchingKeywords;
var
  LIsKeyWord: Boolean;
  LNewFoldRange: TBCEditorCodeFolding.TRanges.TRange;
  LOpenKeyWord: Boolean;
begin
  LIsKeyWord := IsKeywordAtPosition(FLines.CaretPosition, @LOpenKeyWord);

  LNewFoldRange := nil;

  if LIsKeyWord and LOpenKeyWord then
    LNewFoldRange := TBCEditorCodeFolding.TRanges.TRange(FLines.Items[FLines.CaretPosition.Line].CodeFolding.BeginRange)
  else if LIsKeyWord and not LOpenKeyWord then
    LNewFoldRange := CodeFoldingFoldRangeForLineTo(FLines.CaretPosition.Line);

  if LNewFoldRange <> FHighlightedFoldRange then
  begin
    FHighlightedFoldRange := LNewFoldRange;
    Invalidate;
  end;
end;

procedure TCustomBCEditor.Clear();
begin
  FLines.Clear();
  FRows.Clear();
  ClearCodeFolding();
  ClearMatchingPair();
  ClearBookmarks;
  FMarkList.Clear;
  HorzTextPos := 0;
  TopRow := 0;
  FState := FState + [esScrolled];
  InvalidateCaret();
  Invalidate();
end;

procedure TCustomBCEditor.ClearBookmarks;
begin
  while FBookmarkList.Count > 0 do
    DeleteBookmark(FBookmarkList[0]);
end;

procedure TCustomBCEditor.ClearCodeFolding();
var
  LLine: Integer;
begin
  ExpandCodeFoldingLines();
  FAllCodeFoldingRanges.ClearAll();

  for LLine := 0 to FLines.Count - 1 do
  begin
    FLines.SetCodeFoldingBeginRange(LLine, nil);
    FLines.SetCodeFoldingEndRange(LLine, nil);
    FLines.SetCodeFoldingTreeLine(LLine, False);
  end;
end;

procedure TCustomBCEditor.ClearMarks();
begin
  while FMarkList.Count > 0 do
    DeleteMark(FMarkList[0]);
end;

procedure TCustomBCEditor.ClearMatchingPair();
begin
  FCurrentMatchingPair.FState := mpsClear;
end;

procedure TCustomBCEditor.ClearUndo();
begin
  FLines.ClearUndo();
end;

function TCustomBCEditor.ClientToLines(const X, Y: Integer; const AForCaret: Boolean = False): TBCEditorLinesPosition;
begin
  Result := RowsToLines(ClientToRows(X, Y, AForCaret));
end;

function TCustomBCEditor.ClientToRows(const X, Y: Integer; const AForCaret: Boolean = False): TBCEditorRowsPosition;
var
  LColumn: Integer;
  LIndex: Integer;
  LItemWidth: Integer;
  LLeft: Integer;
  LMiddle: Integer;
  LRight: Integer;
  LRow: Integer;
  LText: string;
  LToken: TBCEditorHighlighter.TFind;
  LTokenWidth: Integer;
  LWidths: array of Integer;
  LX: Integer;
begin
  LRow := Max(0, TopRow + Y div LineHeight);

  LTokenWidth := 0;
  LItemWidth := 0;

  if (X <= FLeftMarginWidth) then
    Result := RowsPosition(0, LRow)
  else if (FRows.Count = 0) then
  begin
    LX := X - FLeftMarginWidth + HorzTextPos;
    if (AForCaret) then
      Inc(LX, FSpaceWidth div 2);
    Result := RowsPosition(LX div FSpaceWidth, LRow - FRows.Count);
  end
  else if (LRow >= FRows.Count) then
  begin
    LX := X - FLeftMarginWidth + HorzTextPos;
    if (AForCaret) then
      Inc(LX, FSpaceWidth div 2);
    Result := RowsPosition(LX div FSpaceWidth, LRow - FRows.Count + FLines.Count);
  end
  else
  begin
    LX := X - FLeftMarginWidth + HorzTextPos;

    LColumn := 0;

    FPaintHelper.BeginDrawing(Canvas);
    try
      if ((LRow < FRows.Count)
        and FHighlighter.FindFirstToken(FRows.Items[LRow].BeginRange, FRows[LRow], LToken)) then
        repeat
          LTokenWidth := TokenWidth(LToken.Text, LToken.Length, LColumn, LToken);

          if (LX < LItemWidth + LTokenWidth) then
            break;

          Inc(LItemWidth, LTokenWidth);
          Inc(LColumn, ComputeTextColumns(LToken.Text, LToken.Length, LColumn));
        until (not FHighlighter.FindNextToken(LToken));

      if (LX < LItemWidth + LTokenWidth) then
      begin
        SetLength(LWidths, LToken.Length + 1);
        for LIndex := 1 to Length(LWidths) - 2 do
          LWidths[LIndex] := -1;
        LWidths[0] := LItemWidth;
        LWidths[Length(LWidths) - 1] := LItemWidth + LTokenWidth;

        LLeft := 0;
        LRight := Length(LWidths) - 1;
        while (LRight - LLeft >= 2) do
        begin
          LMiddle := (LLeft + LRight) div 2;

          if (LWidths[LMiddle] < 0) then
            LWidths[LMiddle] := LItemWidth + TokenWidth(LToken.Text, LMiddle, LColumn, LToken);

          case (Sign(LWidths[LMiddle] - LX)) of
            -1: LLeft := LMiddle;
            0:
              begin
                Result := RowsPosition(LColumn + LMiddle, LRow);
                LText := FRows[LRow];
                while (Result.Column < Length(LText) - 1)
                  and ((LText[1 + Result.Column + 1].GetUnicodeCategory in [TUnicodeCategory.ucCombiningMark, TUnicodeCategory.ucNonSpacingMark])
                    or (LText[1 + Result.Column].GetUnicodeCategory = TUnicodeCategory.ucNonSpacingMark)
                      and not IsCombiningDiacriticalMark(LText[1 + Result.Column])) do
                  Inc(Result.Column);
                Exit(Result);
              end;
            1: LRight := LMiddle;
          end;
        end;

        if (LWidths[LLeft] < 0) then
          LWidths[LLeft] := LItemWidth + TokenWidth(LToken.Text, LLeft, LColumn, LToken);
        if (LWidths[LRight] < 0) then
          LWidths[LRight] := LItemWidth + TokenWidth(LToken.Text, LRight, LColumn, LToken);

        if ((LX - LWidths[LLeft]) < (LWidths[LRight] - LX)) then
          Result := RowsPosition(LColumn + LLeft, LRow)
        else
          Result := RowsPosition(LColumn + LRight, LRow);

        if (LRow < FRows.Count) then
        begin
          LText := FRows[LRow];
          while (Result.Column < LText.Length - 1)
            and ((LText[1 + Result.Column + 1].GetUnicodeCategory in [TUnicodeCategory.ucCombiningMark, TUnicodeCategory.ucNonSpacingMark])
              or (LText[1 + Result.Column].GetUnicodeCategory = TUnicodeCategory.ucNonSpacingMark)
                and not IsCombiningDiacriticalMark(LText[1 + Result.Column])) do
            Inc(Result.Column);
        end;
      end
      else if (not AForCaret) then
        Result := RowsPosition(LColumn + (LX - LItemWidth) div FSpaceWidth, LRow)
      else
        Result := RowsPosition(LColumn + (LX - LItemWidth + FSpaceWidth div 2) div FSpaceWidth, LRow)
    finally
      FPaintHelper.EndDrawing();
    end;
  end;
end;

function TCustomBCEditor.ClientToPos(const X, Y: Integer): TPoint;
begin
  Result := Point(ClientToLines(X, Y));
end;

function TCustomBCEditor.ClientToText(const X, Y: Integer): TPoint;
begin
  Result := ClientToPos(X, Y);
end;

procedure TCustomBCEditor.CMCursorChanged(var Message: TMessage);
begin
  inherited;

  Perform(WM_SETCURSOR, Handle, HTCLIENT);
end;

procedure TCustomBCEditor.CMSysFontChanged(var Message: TMessage);
var
  LIcon: TIcon;
  LIconId: Integer;
  LRect: TRect;
  LResData: HGLOBAL;
  LResInfo: HRSRC;
  LResource: Pointer;
begin
  if (Assigned(FSyncEditButtonNormal)) then FSyncEditButtonNormal.Free();
  if (Assigned(FSyncEditButtonHot)) then FSyncEditButtonHot.Free();
  if (Assigned(FSyncEditButtonPushed)) then FSyncEditButtonPushed.Free();
  if (Assigned(FHintWindow)) then FreeAndNil(FHintWindow);

  LRect.Left := 0;
  LRect.Top := 0;
  LRect.Width := GetSystemMetrics(SM_CXSMICON) + 2 * GetSystemMetrics(SM_CXEDGE);
  LRect.Height := GetSystemMetrics(SM_CYSMICON) + 2 * GetSystemMetrics(SM_CYEDGE);

  FSyncEditButtonNormal := TPicture.Create();
  FSyncEditButtonNormal.Bitmap.Width := LRect.Width;
  FSyncEditButtonNormal.Bitmap.Height := LRect.Height;
  FSyncEditButtonHot := TPicture.Create();
  FSyncEditButtonHot.Bitmap.Width := LRect.Width;
  FSyncEditButtonHot.Bitmap.Height := LRect.Height;
  FSyncEditButtonPushed := TPicture.Create();
  FSyncEditButtonPushed.Bitmap.Width := LRect.Width;
  FSyncEditButtonPushed.Bitmap.Height := LRect.Height;

  FSyncEditButtonNormal.Bitmap.Canvas.Brush.Color := clBtnFace;
  FSyncEditButtonNormal.Bitmap.Canvas.FillRect(LRect);
  DrawEdge(FSyncEditButtonNormal.Bitmap.Canvas.Handle, LRect, BDR_RAISEDINNER, BF_RECT);
  FSyncEditButtonHot.Bitmap.Canvas.Brush.Color := clBtnFace;
  FSyncEditButtonHot.Bitmap.Canvas.FillRect(LRect);
  DrawEdge(FSyncEditButtonHot.Bitmap.Canvas.Handle, LRect, BDR_RAISED, BF_RECT);
  FSyncEditButtonPushed.Bitmap.Canvas.Brush.Color := clBtnFace;
  FSyncEditButtonPushed.Bitmap.Canvas.FillRect(LRect);
  DrawEdge(FSyncEditButtonPushed.Bitmap.Canvas.Handle, LRect, BDR_SUNKENOUTER, BF_RECT);

  LIcon := TIcon.Create();
  LIcon.Width := GetSystemMetrics(SM_CXSMICON);
  LIcon.Height := GetSystemMetrics(SM_CYSMICON);

  LResInfo := FindResource(HInstance, BCEDITOR_SYNCEDIT, RT_GROUP_ICON);
  LResData := LoadResource(HInstance, LResInfo);
  LResource := LockResource(LResData);
  LIconId := LookupIconIdFromDirectoryEx(LResource, TRUE, GetSystemMetrics(SM_CXSMICON), GetSystemMetrics(SM_CYSMICON), LR_DEFAULTCOLOR);
  LResInfo := FindResource(HInstance, MAKEINTRESOURCE(LIconId), RT_ICON);
  LResData := LoadResource(HInstance, LResInfo);
  LIcon.Handle := CreateIconFromResourceEx(
    LockResource(LResData), SizeOfResource(HInstance, LResInfo),
    TRUE, $00030000, GetSystemMetrics(SM_CXSMICON), GetSystemMetrics(SM_CYSMICON), LR_DEFAULTCOLOR);

  FSyncEditButtonNormal.Bitmap.Canvas.Draw(GetSystemMetrics(SM_CXEDGE), GetSystemMetrics(SM_CYEDGE), LIcon);
  FSyncEditButtonHot.Bitmap.Canvas.Draw(GetSystemMetrics(SM_CXEDGE), GetSystemMetrics(SM_CYEDGE), LIcon);
  FSyncEditButtonPushed.Bitmap.Canvas.Draw(GetSystemMetrics(SM_CXEDGE), GetSystemMetrics(SM_CYEDGE), LIcon);

  LIcon.Free();

  FMarksPanelWidth := GetSystemMetrics(SM_CXSMICON) + GetSystemMetrics(SM_CXSMICON) div 4;

  FState := FState + [esSysFontChanged];

  inherited;
end;

function TCustomBCEditor.CodeFoldingCollapsableFoldRangeForLine(const ALine: Integer): TBCEditorCodeFolding.TRanges.TRange;
var
  LRange: TBCEditorCodeFolding.TRanges.TRange;
begin
  LRange := TBCEditorCodeFolding.TRanges.TRange(FLines.Items[ALine].CodeFolding.BeginRange);
  if (not Assigned(LRange) or not LRange.Collapsable()) then
    Result := nil
  else
    Result := LRange;
end;

function TCustomBCEditor.CodeFoldingFoldRangeForLineTo(const ALine: Integer): TBCEditorCodeFolding.TRanges.TRange;
var
  LRange: TBCEditorCodeFolding.TRanges.TRange;
begin
  Result := nil;

  LRange := FLines.Items[ALine].CodeFolding.EndRange;
  if Assigned(LRange) then
    if (LRange.EndLine = ALine) and not LRange.ParentCollapsed then
      Result := LRange;
end;

procedure TCustomBCEditor.CodeFoldingOnChange(AEvent: TBCEditorCodeFoldingChanges);
begin
  if AEvent = fcEnabled then
  begin
    if not FCodeFolding.Visible then
      ExpandCodeFoldingLines
    else
      InitCodeFolding;
  end
  else
  if AEvent = fcRescan then
  begin
    InitCodeFolding;
    if FHighlighter.FileName <> '' then
      FHighlighter.LoadFromFile(FHighlighter.FileName);
  end;

  Invalidate;
end;

procedure TCustomBCEditor.CollapseCodeFoldingLevel(const AFirstLevel: Integer; const ALastLevel: Integer);
var
  LFirstLine: Integer;
  LLastLine: Integer;
  LLevel: Integer;
  LLine: Integer;
  LRange: TBCEditorCodeFolding.TRanges.TRange;
  LRangeLevel: Integer;
begin
  if (SelectionAvailable) then
  begin
    LFirstLine := FLines.SelArea.BeginPosition.Line;
    LLastLine := FLines.SelArea.EndPosition.Line;
  end
  else
  begin
    LFirstLine := FLines.BOFPosition.Line;
    LLastLine := FLines.EOFPosition.Line;
  end;

  BeginUpdate();

  LLevel := -1;
  for LLine := LFirstLine to LLastLine do
  begin
    LRange := TBCEditorCodeFolding.TRanges.TRange(FLines.Items[LLine].CodeFolding.BeginRange);
    if (Assigned(LRange)) then
    begin
      if (LLevel = -1) then
        LLevel := LRange.FoldRangeLevel;
      LRangeLevel := LRange.FoldRangeLevel - LLevel;
      if ((AFirstLevel <= LRangeLevel) and (LRangeLevel <= ALastLevel)
        and not LRange.Collapsed and LRange.Collapsable) then
        CollapseCodeFoldingRange(LRange);
    end;
  end;

  CheckIfAtMatchingKeywords();

  EndUpdate();
end;

function TCustomBCEditor.CollapseCodeFoldingLines(const AFirstLine: Integer = -1; const ALastLine: Integer = -1): Integer;
var
  LFirstLine: Integer;
  LLastLine: Integer;
  LLine: Integer;
  LRange: TBCEditorCodeFolding.TRanges.TRange;
begin
  if (AFirstLine >= 0) then
    LFirstLine := AFirstLine
  else
    LFirstLine := 0;
  if (ALastLine >= 0) then
    LLastLine := ALastLine
  else if (AFirstLine >= 0) then
    LLastLine := AFirstLine
  else
    LLastLine := FLines.Count - 1;

  BeginUpdate();

  Result := 0;
  for LLine := LFirstLine to LLastLine do
  begin
    LRange := TBCEditorCodeFolding.TRanges.TRange(FLines.Items[LLine].CodeFolding.BeginRange);
    if (Assigned(LRange) and not LRange.Collapsed and LRange.Collapsable) then
    begin
      CollapseCodeFoldingRange(LRange);
      Inc(Result);
    end;
  end;

  CheckIfAtMatchingKeywords();

  EndUpdate();
end;

procedure TCustomBCEditor.CollapseCodeFoldingRange(const ARange: TBCEditorCodeFolding.TRanges.TRange);
var
  LLine: Integer;
begin
  if (not ARange.Collapsed) then
  begin
    if ((ARange.BeginLine < FLines.CaretPosition.Line) and (FLines.CaretPosition.Line <= ARange.EndLine)) then
      FLines.CaretPosition := LinesPosition(FLines.CaretPosition.Char, ARange.BeginLine);

    for LLine := ARange.BeginLine + 1 to ARange.EndLine do
      DeleteLineFromRows(LLine);

    ARange.Collapsed := True;
    ARange.SetParentCollapsedOfSubCodeFoldingRanges(True, ARange.FoldRangeLevel);

    Invalidate();
  end;
end;

procedure TCustomBCEditor.CommandProcessor(ACommand: TBCEditorCommand; AChar: Char; AData: Pointer);
var
  LCollapsedCount: Integer;
  LLine: Integer;
  LNewSelectionArea: TBCEditorLinesArea;
begin
  { First the program event handler gets a chance to process the command }
  DoOnProcessCommand(ACommand, AChar, AData);

  if ACommand <> ecNone then
  begin
    { Notify hooked command handlers before the command is executed inside of the class }
    NotifyHookedCommandHandlers(False, ACommand, AChar, AData);

    if (FCodeFolding.Visible) then
      case (ACommand) of
        ecBackspace, ecDeleteChar, ecDeleteWord, ecDeleteLastWord, ecDeleteLine,
        ecClear, ecLineBreak, ecChar, ecString, ecImeStr, ecCut, ecPaste,
        ecBlockIndent, ecBlockUnindent, ecTab:
          if (SelectionAvailable) then
          begin
            LNewSelectionArea := FLines.SelArea;
            LCollapsedCount := 0;
            for LLine := LNewSelectionArea.BeginPosition.Line to LNewSelectionArea.EndPosition.Line do
              LCollapsedCount := ExpandCodeFoldingLines(LLine + 1);
            if LCollapsedCount <> 0 then
            begin
              Inc(LNewSelectionArea.EndPosition.Line, LCollapsedCount);
              LNewSelectionArea.EndPosition.Char := Length(FLines.Items[LNewSelectionArea.EndPosition.Line].Text);
            end;
            FLines.BeginUpdate();
            try
              FLines.SelArea := LNewSelectionArea;
            finally
              FLines.EndUpdate();
            end;
          end
          else
            ExpandCodeFoldingLines(FLines.CaretPosition.Line + 1);
      end;

    if ACommand < ecUserFirst then
      ExecuteCommand(ACommand, AChar, AData);

    { Notify hooked command handlers after the command was executed inside of the class }
    NotifyHookedCommandHandlers(True, ACommand, AChar, AData);
  end;
  DoOnCommandProcessed(ACommand, AChar, AData);
end;

procedure TCustomBCEditor.CompletionProposalTimerHandler(EndLine: TObject);
begin
  FCompletionProposalTimer.Enabled := False;
  DoCompletionProposal;
end;

function TCustomBCEditor.ComputeIndentText(const IndentCount: Integer): string;
begin
  if (not (eoAutoIndent in FOptions)) then
    Result := ''
  else if (toTabsToSpaces in FTabs.Options) then
    Result := StringOfChar(BCEDITOR_SPACE_CHAR, IndentCount)
  else
  begin
    Result := StringOfChar(BCEDITOR_TAB_CHAR, IndentCount div FTabs.Width);
    Result := Result + StringOfChar(BCEDITOR_SPACE_CHAR, IndentCount mod FTabs.Width);
  end;
end;

procedure TCustomBCEditor.ComputeScroll(const APoint: TPoint);
var
  LCursorIndex: Integer;
  LScrollBounds: TRect;
  LScrollBoundsLeft: Integer;
  LScrollBoundsRight: Integer;
begin
  Assert(LineHeight > 0);

  if FMouseMoveScrolling then
  begin
    if (APoint.X < ClientRect.Left) or (APoint.X > ClientRect.Right) or (APoint.Y < ClientRect.Top) or
      (APoint.Y > ClientRect.Bottom) then
    begin
      FMouseMoveScrollTimer.Enabled := False;
      Exit;
    end;

    LCursorIndex := GetMouseMoveScrollCursorIndex;
    case LCursorIndex of
      scNorthWest, scWest, scSouthWest:
        FScrollDeltaX := (APoint.X - FMouseMoveScrollingPoint.X) div FSpaceWidth - 1;
      scNorthEast, scEast, scSouthEast:
        FScrollDeltaX := (APoint.X - FMouseMoveScrollingPoint.X) div FSpaceWidth + 1;
      else
        FScrollDeltaX := 0;
    end;

    case LCursorIndex of
      scNorthWest, scNorth, scNorthEast:
        FScrollDeltaY := (APoint.Y - FMouseMoveScrollingPoint.Y) div LineHeight - 1;
      scSouthWest, scSouth, scSouthEast:
        FScrollDeltaY := (APoint.Y - FMouseMoveScrollingPoint.Y) div LineHeight + 1;
    else
      FScrollDeltaY := 0;
    end;

    FMouseMoveScrollTimer.Enabled := (FScrollDeltaX <> 0) or (FScrollDeltaY <> 0);
  end
  else
  begin
    if not inherited MouseCapture and not Dragging then
    begin
      FScrollTimer.Enabled := False;
      Exit;
    end;

    LScrollBoundsLeft := FLeftMarginWidth;
    LScrollBoundsRight := LScrollBoundsLeft + FTextWidth + 4;

    LScrollBounds := Bounds(LScrollBoundsLeft, 0, LScrollBoundsRight, VisibleRows * LineHeight);

    if BorderStyle = bsNone then
      InflateRect(LScrollBounds, -2, -2);

    if APoint.X < LScrollBounds.Left then
      FScrollDeltaX := (APoint.X - LScrollBounds.Left) div FSpaceWidth - 1
    else if APoint.X >= LScrollBounds.Right then
      FScrollDeltaX := (APoint.X - LScrollBounds.Right) div FSpaceWidth + 1
    else
      FScrollDeltaX := 0;

    if APoint.Y < LScrollBounds.Top then
      FScrollDeltaY := (APoint.Y - LScrollBounds.Top) div LineHeight - 1
    else
    if APoint.Y >= LScrollBounds.Bottom then
      FScrollDeltaY := (APoint.Y - LScrollBounds.Bottom) div LineHeight + 1
    else
      FScrollDeltaY := 0;

    FScrollTimer.Enabled := (FScrollDeltaX <> 0) or (FScrollDeltaY <> 0);
  end;
end;

function TCustomBCEditor.ComputeTextColumns(const AText: PChar;
  const ALength, AColumn: Integer): Integer;
begin
  if (Assigned(AText) and (AText^ = BCEDITOR_TAB_CHAR)) then
    Result := FTabs.Width - AColumn mod FTabs.Width
  else
    Result := ALength;
end;

procedure TCustomBCEditor.CopyToClipboard();
begin
  if (SelectionAvailable) then
    DoCopyToClipboard(SelText);
end;

constructor TCustomBCEditor.Create(AOwner: TComponent);
var
  LIndex: Integer;
  LLogFont: TLogFont;
  LNonClientMetrics: TNonClientMetrics;
begin
  inherited;

  Width := 185;
  Height := 89;
  Color := clWindow;
  DoubleBuffered := False;
  ControlStyle := ControlStyle + [csSetCaption, csOpaque, csNeedsBorderPaint];

  FColor := clWindow;
  FBorderStyle := bsSingle;
  FDoubleClickTime := GetDoubleClickTime();
  FHintWindow := nil;
  FHWheelAccumulator := 0;
  FInsertPos := InvalidCaretPos;
  FLastSortOrder := soDesc;
  FMouseCapture := mcNone;
  FMouseMoveHintPoint := Point(-1, -1);
  FOldMouseMovePoint := Point(-1, -1);
  FOldSelectionAvailable := False;
  FSelectedCaseText := '';

  { Code folding }
  FAllCodeFoldingRanges := TBCEditorCodeFolding.TAllRanges.Create;
  FCodeFolding := TBCEditorCodeFolding.Create;
  FCodeFolding.OnChange := CodeFoldingOnChange;
  { Matching pair }
  FMatchingPair := TBCEditorMatchingPair.Create;
  { Special chars }
  FSpecialChars := TBCEditorSpecialChars.Create;
  FSpecialChars.OnChange := SpecialCharsChanged;
  { Caret }
  FCaret := TBCEditorCaret.Create;
  FCaretCreated := False;
  FCaretVisible := False;
  { Text buffer }
  FLines := TBCEditorLines(CreateLines());
  FOriginalLines := FLines;
  FLines.OnAfterUpdate := AfterLinesUpdate;
  FLines.OnBeforeUpdate := BeforeLinesUpdate;
  FLines.OnCaretChanged := LinesCaretChanged;
  FLines.OnCleared := LinesCleared;
  FLines.OnDeleted := LineDeleted;
  FLines.OnDeleting := LineDeleting;
  FLines.OnInserted := LineInserted;
  FLines.OnUpdated := LineUpdated;
  FLines.OnSelChange := SelectionChanged;
  FRows := TCustomBCEditor.TRows.Create(Self);
  { Font }
  LNonClientMetrics.cbSize := SizeOf(LNonClientMetrics);
  if (SystemParametersInfo(SPI_GETNONCLIENTMETRICS, SizeOf(LNonClientMetrics), @LNonClientMetrics, 0)
    and (GetObject(Font.Handle, SizeOf(LLogFont), @LLogFont) <> 0)) then
  begin
    LLogFont.lfQuality := LNonClientMetrics.lfMessageFont.lfQuality;
    Font.Handle := CreateFontIndirect(LLogFont);
  end;
  Font.Name := 'Courier New';
  Font.Size := Font.Size + 1;
  Font.OnChange := FontChanged;
  { Painting }
  FLineHeight := 0;
  FTextWidth := MaxInt;
  FVisibleRows := -1;
  FPaintHelper := TBCEditorPaintHelper.Create(Font);
  ParentFont := False;
  ParentColor := False;
  FCommandDrop := False;
  FLineStateWidth := GetSystemMetrics(SM_CXSMICON) div 4;
  FImages := nil;
  FInsertPosCache := nil;
  FInsertPosBitmap := nil;
  { Active line, selection }
  FSelection := TBCEditorSelection.Create;
  FHideSelection := True;
  { Bookmarks }
  FBookmarkList := TBCEditorMarkList.Create(Self);
  FBookmarkList.OnChange := BookmarkListChange;
  { Marks }
  FMarkList := TBCEditorMarkList.Create(Self);
  FMarkList.OnChange := MarkListChange;
  { Tabs }
  TabStop := True;
  FTabs := TBCEditorTabs.Create;
  FTabs.OnChange := TabsChanged;
  { Text }
  FTextEntryMode := temInsert;
  FKeyboardHandler := TBCEditorKeyboardHandler.Create;
  FKeyCommands := TBCEditorKeyCommands.Create(Self);
  SetDefaultKeyCommands;
  FWantReturns := True;
  FTopRow := 0;
  FOptions := DefaultOptions;
  { Completion proposal }
  FCompletionProposal := TBCEditorCompletionProposal.Create(Self);
  FCompletionProposalTimer := TTimer.Create(Self);
  FCompletionProposalTimer.Enabled := False;
  FCompletionProposalTimer.OnTimer := CompletionProposalTimerHandler;
  { Search }
  FSearch := TBCEditorSearch.Create;
  FSearch.OnChange := SearchChanged;
  FSearchResults := TList<TBCEditorLinesArea>.Create();
  FSearchFindDialog := nil;
  FSearchReplaceDialog := nil;
  FReplace := TBCEditorReplace.Create;
  FReplace.OnChange := ReplaceChanged;
  { Scroll }
  FHideScrollBars := True;
  FScroll := TBCEditorScroll.Create;
  FScroll.OnChange := ScrollChanged;
  FScrollTimer := TTimer.Create(Self);
  FScrollTimer.Enabled := False;
  FScrollTimer.Interval := 100;
  FScrollTimer.OnTimer := ScrollTimerHandler;
  FMouseMoveScrollTimer := TTimer.Create(Self);
  FMouseMoveScrollTimer.Enabled := False;
  FMouseMoveScrollTimer.Interval := 100;
  FMouseMoveScrollTimer.OnTimer := MouseMoveScrollTimerHandler;
  { Active line }
  FActiveLine := TBCEditorActiveLine.Create;
  FActiveLine.OnChange := ActiveLineChanged;
  { Word wrap }
  FWordWrap := TBCEditorWordWrap.Create;
  FWordWrap.OnChange := WordWrapChanged;
  { Sync edit }
  FSyncEdit := TBCEditorSyncEdit.Create;
  FSyncEdit.OnChange := SyncEditChanged;
  FSyncEditButtonHot := nil;
  FSyncEditButtonNormal := nil;
  FSyncEditButtonPushed := nil;
  { FLeftMargin }
  FLeftMargin := TBCEditorLeftMargin.Create(Self);
  FLeftMargin.OnChange := LeftMarginChanged;
  { Do update character constraints }
  TabsChanged(nil);
  { Highlighter }
  FHighlighter := TBCEditorHighlighter.Create(Self);
  FHighlighter.OnChange := HighlighterChanged;
  { Mouse wheel scroll cursors }
  for LIndex := 0 to 7 do
    FMouseMoveScrollCursors[LIndex] := LoadCursor(HInstance, PChar(BCEDITOR_MOUSE_MOVE_SCROLL + IntToStr(LIndex)));

  if (not SystemParametersInfo(SPI_GETWHEELSCROLLLINES, 0, @FWheelScrollLines, 0)) then
    FWheelScrollLines := 3;
end;

function TCustomBCEditor.CreateLines(): BCEditor.Lines.TBCEditorLines;
begin
  Result := BCEditor.Lines.TBCEditorLines.Create(Self);
end;

procedure TCustomBCEditor.CreateParams(var AParams: TCreateParams);
const
  LBorderStyles: array [TBorderStyle] of DWORD = (0, WS_BORDER);
  LClassStylesOff = CS_VREDRAW or CS_HREDRAW;
begin
  StrDispose(WindowText);
  WindowText := nil;

  inherited CreateParams(AParams);

  with AParams do
  begin
    WindowClass.Style := WindowClass.Style and not LClassStylesOff;
    Style := Style or LBorderStyles[FBorderStyle] or WS_CLIPCHILDREN;

    if NewStyleControls and Ctl3D and (FBorderStyle = bsSingle) then
    begin
      Style := Style and not WS_BORDER;
      ExStyle := ExStyle or WS_EX_CLIENTEDGE;
    end;
  end;
end;

procedure TCustomBCEditor.CreateWnd();
begin
  inherited;

  OleCheck(RegisterDragDrop(Handle, Self));

  if (eoDropFiles in FOptions) and not (csDesigning in ComponentState) then
    DragAcceptFiles(Handle, True);

  EnableScrollBar(Handle, SB_VERT, ESB_ENABLE_BOTH);
  EnableScrollBar(Handle, SB_HORZ, ESB_ENABLE_BOTH);

  FCaretCreated := True;
  FState := FState + [esFontChanged, esSizeChanged, esScrolled];
end;

procedure TCustomBCEditor.CutToClipboard;
begin
  CommandProcessor(ecCut, BCEDITOR_NONE_CHAR, nil);
end;

function TCustomBCEditor.DeleteBookmark(const ALine: Integer; const AIndex: Integer): Boolean;
var
  LBookmark: TBCEditorMark;
  LIndex: Integer;
begin
  Result := False;
  LIndex := 0;
  while LIndex < FBookmarkList.Count do
  begin
    LBookmark := FBookmarkList.Items[LIndex];
    if LBookmark.Line = ALine then
    begin
      if LBookmark.Index = AIndex then
        Result := True;
      DeleteBookmark(LBookmark);
    end
    else
      Inc(LIndex);
  end;
end;

procedure TCustomBCEditor.DeleteBookmark(ABookmark: TBCEditorMark);
begin
  if Assigned(ABookmark) then
  begin
    FBookmarkList.Remove(ABookmark);
    if Assigned(FOnAfterDeleteBookmark) then
      FOnAfterDeleteBookmark(Self);
  end;
end;

procedure TCustomBCEditor.DeleteChar();
begin
  if (SelectionAvailable) then
    SelText := ''
  else if ((FLines.CaretPosition.Line < FLines.Count)
    and (FLines.CaretPosition.Char < Length(FLines.Items[FLines.CaretPosition.Line].Text))) then
    FLines.DeleteText(LinesArea(FLines.CaretPosition, LinesPosition(FLines.CaretPosition.Char + 1, FLines.CaretPosition.Line)))
  else if (FLines.CaretPosition.Line < FLines.Count - 1) then
    FLines.DeleteText(LinesArea(FLines.CaretPosition, FLines.BOLPosition[FLines.CaretPosition.Line + 1]));
end;

procedure TCustomBCEditor.DeleteLastWordOrBeginningOfLine(const ACommand: TBCEditorCommand);
var
  LNewCaretPosition: TBCEditorLinesPosition;
begin
  if (ACommand = ecDeleteLastWord) then
    LNewCaretPosition := PreviousWordPosition(FLines.CaretPosition)
  else
    LNewCaretPosition := FLines.BOLPosition[FLines.CaretPosition.Line];
  if (LNewCaretPosition <> FLines.CaretPosition) then
    if (FLines.CaretPosition.Line < FLines.Count) then
      FLines.DeleteText(LinesArea(LNewCaretPosition, Min(FLines.CaretPosition, FLines.EOLPosition[FLines.CaretPosition.Line])))
    else
      FLines.CaretPosition := LNewCaretPosition;
end;

procedure TCustomBCEditor.DeleteLine();
begin
  if (SelectionAvailable) then
    FLines.SelArea := LinesArea(FLines.CaretPosition, FLines.CaretPosition)
  else if (FLines.CaretPosition.Line < FLines.Count) then
    FLines.Delete(FLines.CaretPosition.Line);
end;

procedure TCustomBCEditor.DeleteLineFromRows(const ALine: Integer);
var
  LDeletedRows: Integer;
  LLastRow: Integer;
  LLine: Integer;
  LRow: Integer;
begin
  if (FRows.Count > 0) then
  begin
    ClearMatchingPair();

    if (FLines.Items[ALine].FirstRow >= 0) then
    begin
      LLastRow := FLines.Items[ALine].FirstRow;
      while (not (rfLastRowOfLine in FRows.Items[LLastRow].Flags)) do
        Inc(LLastRow);

      LDeletedRows := LLastRow - FLines.Items[ALine].FirstRow + 1;

      if (not (esCaretInvalid in FState)) then
      begin
        Dec(FCaretPos.Y, LDeletedRows * LineHeight);
        UpdateCaret();
      end;

      for LRow := LLastRow + 1 to FRows.Count - 1 do
        FRows.List[LRow].Line := FRows.List[LRow].Line - 1;

      for LRow := LLastRow downto FLines.Items[ALine].FirstRow do
        FRows.Delete(LRow);

      for LLine := ALine to FLines.Count - 1 do
        FLines.SetFirstRow(LLine, FLines.Items[LLine].FirstRow - LDeletedRows);

      if (UpdateCount > 0) then
        Include(FState, esRowsChanged)
      else
      begin
        ClearMatchingPair();
        UpdateScrollBars();
        Invalidate();
      end;
    end;
  end;
end;

procedure TCustomBCEditor.DeleteMark(AMark: TBCEditorMark);
begin
  if Assigned(AMark) then
  begin
    if Assigned(FOnBeforeDeleteMark) then
      FOnBeforeDeleteMark(Self, AMark);
    FMarkList.Remove(AMark);
    if Assigned(FOnAfterDeleteMark) then
      FOnAfterDeleteMark(Self);
  end
end;

procedure TCustomBCEditor.DeleteWordOrEndOfLine(const ACommand: TBCEditorCommand);
var
  LEndPosition: TBCEditorLinesPosition;
begin
  if (FLines.CaretPosition.Line < FLines.Count) then
  begin
    case (ACommand) of
      ecDeleteWord:
        if ((FLines.CaretPosition.Char < Length(FLines.Items[FLines.CaretPosition.Line].Text))
          and not IsWordBreakChar(FLines.Char[FLines.CaretPosition])) then
        begin
          LEndPosition := WordEnd(FLines.CaretPosition);
          while ((LEndPosition.Char < Length(FLines.Items[LEndPosition.Line].Text)) and IsEmptyChar(FLines.Char[LEndPosition])) do
            Inc(LEndPosition.Char);
        end
        else
          LEndPosition := NextWordPosition(FLines.CaretPosition);
      ecDeleteEndOfLine:
        LEndPosition := FLines.EOLPosition[FLines.CaretPosition.Line];
      else raise ERangeError.Create('ACommand: ' + IntToStr(Ord(ACommand)));
    end;

    if (LEndPosition > FLines.CaretPosition) then
      FLines.DeleteText(LinesArea(FLines.CaretPosition, LEndPosition));
  end;
end;

destructor TCustomBCEditor.Destroy();
begin
  ClearCodeFolding();
  FCodeFolding.Free;
  FAllCodeFoldingRanges.Free;
  FHighlighter.Free;
  FHighlighter := nil;
  if Assigned(FChainedEditor) or (FLines <> FOriginalLines) then
    RemoveChainedEditor;
  if Assigned(FCompletionProposalPopupWindow) then
    FCompletionProposalPopupWindow.Free;
  { Do not use FreeAndNil, it first nil and then frees causing problems with code accessing FHookedCommandHandlers
    while destruction }
  FHookedCommandHandlers.Free;
  FHookedCommandHandlers := nil;
  FBookmarkList.Free;
  FMarkList.Free;
  FKeyCommands.Free;
  FKeyCommands := nil;
  FKeyboardHandler.Free;
  FSelection.Free;
  FLeftMargin.Free;
  FLeftMargin := nil; { Notification has a check }
  FWordWrap.Free;
  FPaintHelper.Free;
  if (Assigned(FImages)) then
    FImages.Free();
  if (Assigned(FInsertPosCache)) then
    FInsertPosCache.Free();
  if (Assigned(FInsertPosBitmap)) then
    FInsertPosBitmap.Free();
  FOriginalLines.Free;
  FActiveLine.Free;
  FScroll.Free;
  FSearch.Free;
  FSearchResults.Free();
  FReplace.Free;
  FTabs.Free;
  FSpecialChars.Free;
  FCaret.Free;
  FMatchingPair.Free;
  FCompletionProposal.Free;
  FSyncEdit.Free;
  if (Assigned(FSyncEditButtonHot)) then
    FSyncEditButtonHot.Free();
  if (Assigned(FSyncEditButtonNormal)) then
    FSyncEditButtonNormal.Free();
  if (Assigned(FSyncEditButtonPushed)) then
    FSyncEditButtonPushed.Free();
  FRows.Free();
  if (Assigned(FHintWindow)) then
    FHintWindow.Free();

  inherited;
end;

procedure TCustomBCEditor.DestroyWnd();
begin
  if (eoDropFiles in FOptions) and not (csDesigning in ComponentState) then
    DragAcceptFiles(Handle, False);

  RevokeDragDrop(Handle);

  inherited;
end;

procedure TCustomBCEditor.DoBackspace();
var
  LBackCounterLine: Integer;
  LLength: Integer;
  LNewCaretPosition: TBCEditorLinesPosition;
  LRange: TBCEditorCodeFolding.TRanges.TRange;
  LSpaceCount1: Integer;
  LSpaceCount2: Integer;
  LVisualSpaceCount1: Integer;
  LVisualSpaceCount2: Integer;
begin
  FLines.BeginUpdate();
  try
    if (SelectionAvailable) then
    begin
      if FSyncEdit.Active then
      begin
        if FLines.CaretPosition.Char < FSyncEdit.EditArea.BeginPosition.Char then
          Exit;
        FSyncEdit.MoveEndPositionChar(-FLines.SelArea.EndPosition.Char + FLines.SelArea.BeginPosition.Char);
      end;
      SelText := '';
    end
    else if (FLines.CaretPosition > FLines.BOFPosition) then
    begin
      if (FSyncEdit.Active) then
      begin
        if FLines.CaretPosition.Char <= FSyncEdit.EditArea.BeginPosition.Char then
          Exit;
        FSyncEdit.MoveEndPositionChar(-1);
      end;

      if ((FLines.CaretPosition.Line < FLines.Count)
        and (FLines.CaretPosition.Char > Length(FLines.Items[FLines.CaretPosition.Line].Text))) then
      begin
        if (Length(FLines.Items[FLines.CaretPosition.Line].Text) > 0) then
          FLines.CaretPosition := FLines.EOLPosition[FLines.CaretPosition.Line]
        else
        begin
          LSpaceCount1 := FLines.CaretPosition.Char;
          LSpaceCount2 := 0;
          if LSpaceCount1 > 0 then
          begin
            LBackCounterLine := FLines.CaretPosition.Line;
            while LBackCounterLine >= 0 do
            begin
              LSpaceCount2 := LeftSpaceCount(FLines.Items[LBackCounterLine].Text);
              if LSpaceCount2 < LSpaceCount1 then
                Break;
              Dec(LBackCounterLine);
            end;
            if (LBackCounterLine = -1) and (LSpaceCount2 > LSpaceCount1) then
              LSpaceCount2 := 0;
          end;
          if LSpaceCount2 = LSpaceCount1 then
            LSpaceCount2 := 0;

          FLines.CaretPosition := LinesPosition(FLines.CaretPosition.Char - (LSpaceCount1 - LSpaceCount2), FLines.CaretPosition.Line);
        end;
      end
      else if ((FLines.CaretPosition.Line < FLines.Count)
        and (FLines.CaretPosition.Char > 0)) then
      begin
        LSpaceCount1 := LeftSpaceCount(FLines.Items[FLines.CaretPosition.Line].Text);
        LSpaceCount2 := 0;
        if ((FLines.CaretPosition.Char < Length(FLines.Items[FLines.CaretPosition.Line].Text) - 1)
          and (FLines.Char[FLines.CaretPosition] = BCEDITOR_SPACE_CHAR)
            or (LSpaceCount1 <> FLines.CaretPosition.Char)) then
        begin
          LNewCaretPosition := LinesPosition(FLines.CaretPosition.Char - 1, FLines.CaretPosition.Line);
          if (FLines.Char[LNewCaretPosition].IsSurrogate()) then
            Dec(LNewCaretPosition.Char);
        end
        else
        begin
          LVisualSpaceCount1 := GetLeadingExpandedLength(FLines.Items[FLines.CaretPosition.Line].Text);
          LVisualSpaceCount2 := 0;
          LBackCounterLine := FLines.CaretPosition.Line - 1;
          while LBackCounterLine >= 0 do
          begin
            LVisualSpaceCount2 := GetLeadingExpandedLength(FLines.Items[LBackCounterLine].Text);
            if LVisualSpaceCount2 < LVisualSpaceCount1 then
            begin
              LSpaceCount2 := LeftSpaceCount(FLines.Items[LBackCounterLine].Text);
              Break;
            end;
            Dec(LBackCounterLine);
          end;

          if ((LSpaceCount2 > 0)
            and ((LBackCounterLine >= 0) or (LSpaceCount2 <= LSpaceCount1))
            and (LSpaceCount2 <> LSpaceCount1)) then
          begin
            LNewCaretPosition := FLines.CaretPosition;

            LLength := GetLeadingExpandedLength(FLines.Items[FLines.CaretPosition.Line].Text, LNewCaretPosition.Char);
            while ((LNewCaretPosition.Char > 0) and (LLength > LVisualSpaceCount2)) do
            begin
              Dec(LNewCaretPosition.Char);
              LLength := GetLeadingExpandedLength(FLines.Items[FLines.CaretPosition.Line].Text, LNewCaretPosition.Char);
            end;
          end
          else
          begin
            LNewCaretPosition := LinesPosition(FLines.CaretPosition.Char - 1, FLines.CaretPosition.Line);
            LVisualSpaceCount2 := LVisualSpaceCount1 - (LVisualSpaceCount1 mod FTabs.Width);
            if (LVisualSpaceCount2 = LVisualSpaceCount1) then
              LVisualSpaceCount2 := Max(LVisualSpaceCount2 - FTabs.Width, 0);

            LLength := GetLeadingExpandedLength(FLines.Items[FLines.CaretPosition.Line].Text, LNewCaretPosition.Char - 1);
            while (LNewCaretPosition.Char > 0) and (LLength > LVisualSpaceCount2) do
            begin
              Dec(LNewCaretPosition.Char);
              LLength := GetLeadingExpandedLength(FLines.Items[FLines.CaretPosition.Line].Text, LNewCaretPosition.Char);
            end;
          end;
        end;

        FLines.Backspace(LinesArea(LNewCaretPosition, FLines.CaretPosition));
      end
      else if (FLines.CaretPosition.Line >= FLines.Count) then
        if (FLines.CaretPosition.Char > 0) then
          FLines.CaretPosition := LinesPosition(0, FLines.CaretPosition.Line)
        else if (FLines.CaretPosition.Line = FLines.Count) then
          FLines.CaretPosition := FLines.EOLPosition[FLines.CaretPosition.Line - 1]
        else
          FLines.CaretPosition := LinesPosition(0, FLines.CaretPosition.Line - 1)
      else if (FLines.CaretPosition.Line > 0) then
      begin
        LNewCaretPosition := FLines.EOLPosition[FLines.CaretPosition.Line - 1];

        LRange := CodeFoldingFoldRangeForLineTo(LNewCaretPosition.Line);
        if (Assigned(LRange) and LRange.Collapsed) then
        begin
          LNewCaretPosition.Line := LRange.BeginLine;
          Inc(LNewCaretPosition.Char, Length(FLines.Items[LNewCaretPosition.Line].Text) + 1);
        end;

        FLines.DeleteText(LinesArea(LNewCaretPosition, FLines.CaretPosition));
      end
      else
        FLines.CaretPosition := FLines.BOFPosition;
    end;

    if (FSyncEdit.Active) then
      DoSyncEdit();
  finally
    FLines.EndUpdate();
  end;
end;

procedure TCustomBCEditor.DoBlockComment();
var
  LArea: TBCEditorLinesArea;
  LCommentIndex: Integer;
  LCommentLength: Integer;
  LIndentText: string;
  LIndex: Integer;
  LLinesDeleted: Integer;
  LText: string;
begin
  LCommentLength := Length(FHighlighter.Comments.BlockComments);

  if (LCommentLength = 0) then
    // No BlockComment defined in the Highlighter
  else
  begin
    LArea.BeginPosition := Min(FLines.SelArea.BeginPosition, FLines.SelArea.EndPosition);
    LArea.EndPosition := Max(FLines.SelArea.BeginPosition, FLines.SelArea.EndPosition);

    if (LArea.EndPosition <> FLines.BOFPosition) then
    begin
      LText := Trim(FLines.TextIn[LArea]);

      LCommentIndex := -2;
      LIndex := 0;
      while (LIndex + 1 < LCommentLength) do
        if ((Length(LText) >= Length(FHighlighter.Comments.BlockComments[LIndex]) + Length(FHighlighter.Comments.BlockComments[LIndex + 1]))
          and (LeftStr(LText, Length(FHighlighter.Comments.BlockComments[LIndex])) = FHighlighter.Comments.BlockComments[LIndex])
          and (RightStr(LText, Length(FHighlighter.Comments.BlockComments[LIndex + 1])) = FHighlighter.Comments.BlockComments[LIndex + 1])) then
        begin
          LCommentIndex := LIndex;
          break;
        end
        else
          Inc(LIndex, 2);

      if (LCommentIndex < 0) then
      begin
        LArea.BeginPosition.Char := 0;
        if (LArea.EndPosition.Line < FLines.Count - 1) then
          LArea.EndPosition := FLines.BOLPosition[LArea.EndPosition.Line]
        else
          LArea.EndPosition := FLines.EOLPosition[LArea.EndPosition.Line];

        LText := Trim(FLines.TextIn[LArea]);

        LCommentIndex := -2;
        LIndex := 0;
        while (LIndex + 1 < LCommentLength) do
          if ((Length(LText) >= Length(FHighlighter.Comments.BlockComments[LIndex]) + Length(FHighlighter.Comments.BlockComments[LIndex + 1]))
            and (LeftStr(LText, Length(FHighlighter.Comments.BlockComments[LIndex])) = FHighlighter.Comments.BlockComments[LIndex])
            and (RightStr(LText, Length(FHighlighter.Comments.BlockComments[LIndex + 1])) = FHighlighter.Comments.BlockComments[LIndex + 1])) then
          begin
            LCommentIndex := LIndex;
            break;
          end
          else
            Inc(LIndex, 2);
      end;


      FLines.BeginUpdate();
      try
        if (LCommentIndex >= 0) then
        begin
          LText := FLines.TextIn[LArea];

          LArea.BeginPosition := FLines.PositionOf(LeftTrimLength(LText), LArea.BeginPosition);
          LArea.EndPosition := FLines.PositionOf(Length(Trim(LText)), LArea.BeginPosition);

          LLinesDeleted := 0;
          FLines.DeleteText(LinesArea(LArea.BeginPosition, LinesPosition(LArea.BeginPosition.Char + Length(FHighlighter.Comments.BlockComments[LIndex]), LArea.BeginPosition.Line)));
          if (Trim(FLines.Items[LArea.BeginPosition.Line].Text) = '') then
          begin
            FLines.Delete(LArea.BeginPosition.Line);
            Dec(LArea.EndPosition.Line);
            LArea.BeginPosition.Char := 0;
            Inc(LLinesDeleted);
          end;

          FLines.DeleteText(LinesArea(LArea.EndPosition, LinesPosition(LArea.EndPosition.Char, LArea.EndPosition.Line)));
          if (Trim(FLines.Items[LArea.EndPosition.Line].Text) = '') then
          begin
            FLines.Delete(LArea.EndPosition.Line);
            Dec(LArea.EndPosition.Line);
            Inc(LLinesDeleted);
          end;

          if ((LLinesDeleted = 2) and (LArea.EndPosition >= LArea.BeginPosition)) then
            FLines.DeleteIndent(LArea.BeginPosition, LinesPosition(LArea.BeginPosition.Char, LArea.EndPosition.Line), ComputeIndentText(Tabs.Width));
        end;

        Inc(LCommentIndex, 2);

        if (LCommentIndex < LCommentLength) then
        begin
          LIndentText := ComputeIndentText(LeftSpaceCount(FLines.Items[LArea.BeginPosition.Line].Text));

          FLines.InsertText(LArea.BeginPosition, LIndentText + FHighlighter.Comments.BlockComments[LCommentIndex] + FLines.LineBreak);
          Inc(LArea.EndPosition.Line);

          if ((LArea.EndPosition.Char = 0) and (LArea.EndPosition.Line > LArea.BeginPosition.Line)) then
            LArea.EndPosition := FLines.EOLPosition[LArea.EndPosition.Line - 1];
          FLines.InsertText(LArea.EndPosition, FLines.LineBreak + LIndentText + FHighlighter.Comments.BlockComments[LCommentIndex + 1]);

          FLines.InsertIndent(FLines.BOLPosition[LArea.BeginPosition.Line + 1], LinesPosition(LArea.BeginPosition.Char, LArea.EndPosition.Line + 1), ComputeIndentText(Tabs.Width));
          Inc(LArea.EndPosition.Line);
        end;

        if (LArea.EndPosition.Line < FLines.Count - 1) then
        begin
          LArea.EndPosition := FLines.BOLPosition[LArea.EndPosition.Line + 1];
          SetCaretAndSelection(LArea.EndPosition, LArea);
        end
        else
          FLines.CaretPosition := FLines.BOFPosition;
      finally
        FLines.EndUpdate();
      end;
    end;
  end;
end;

procedure TCustomBCEditor.DoBlockIndent(const ACommand: TBCEditorCommand);
var
  LIndentText: string;
  LTextArea: TBCEditorLinesArea;
begin
  if (FLines.Count > 0) then
  begin
    LTextArea.BeginPosition := FLines.BOLPosition[Min(FLines.SelArea.BeginPosition, FLines.SelArea.EndPosition).Line];
    LTextArea.EndPosition := LinesPosition(LTextArea.BeginPosition.Char, Max(FLines.SelArea.BeginPosition, FLines.SelArea.EndPosition).Line);
    if (LTextArea.EndPosition = LTextArea.BeginPosition) then
      if (LTextArea.EndPosition.Line < FLines.Count - 1) then
        LTextArea.EndPosition := FLines.BOLPosition[LTextArea.EndPosition.Line + 1]
      else
        LTextArea.EndPosition := FLines.EOLPosition[LTextArea.EndPosition.Line];

    LIndentText := ComputeIndentText(FTabs.Width);

    FLines.BeginUpdate();
    try
      case (ACommand) of
        ecBlockIndent:
          FLines.InsertIndent(LTextArea.BeginPosition, LTextArea.EndPosition, LIndentText);
        ecBlockUnindent:
          FLines.DeleteIndent(LTextArea.BeginPosition, LTextArea.EndPosition, LIndentText);
        else raise ERangeError.Create('ACommand: ' + IntToStr(Ord(ACommand)));
      end;

      if (not SelectionAvailable) then
      begin
        LTextArea.BeginPosition.Char := 0;
        if (LTextArea.EndPosition.Char > 0) then
          LTextArea.EndPosition.Char := Length(FLines.Items[LTextArea.EndPosition.Line].Text);
        SetCaretAndSelection(LTextArea.EndPosition, LTextArea);
      end;
    finally
      FLines.EndUpdate();
    end;
  end;
end;

procedure TCustomBCEditor.DoChar(const AChar: Char);
begin
  DoInsertText(AChar);
end;

procedure TCustomBCEditor.DoCompletionProposal();
var
  LCanExecute: Boolean;
  LColumnIndex: Integer;
  LControl: TWinControl;
  LCurrentInput: string;
  LIndex: Integer;
  LItem: TBCEditorCompletionProposalItems.TItem;
  LItems: TStrings;
  LPoint: TPoint;
begin
  Assert(FCompletionProposal.CompletionColumnIndex < FCompletionProposal.Columns.Count);

  if (esCaretInvalid in FState) then
    LPoint := FCaretPos
  else
    LPoint := RowsToClient(FRows.CaretPosition);
  Inc(LPoint.Y, LineHeight);

  FCompletionProposalPopupWindow := TBCEditorCompletionProposalPopupWindow.Create(Self);
  with FCompletionProposalPopupWindow do
  begin
    LControl := Self;
    while Assigned(LControl) and not (LControl is TCustomForm) do
      LControl := LControl.Parent;
    if LControl is TCustomForm then
      PopupParent := TCustomForm(LControl);
    OnCanceled := FOnCompletionProposalCanceled;
    OnSelected := FOnCompletionProposalSelected;
    Assign(FCompletionProposal);

    LItems := TStringList.Create;
    try
      if cpoParseItemsFromText in FCompletionProposal.Options then
        SplitTextIntoWords(LItems, False);
      if cpoAddHighlighterKeywords in FCompletionProposal.Options then
        AddHighlighterKeywords(LItems);
      Items.Clear;
      for LIndex := 0 to LItems.Count - 1 do
      begin
        LItem := Items.Add;
        LItem.Value := LItems[LIndex];
        { Add empty items for columns }
        for LColumnIndex := 1 to FCompletionProposal.Columns.Count - 1 do
          FCompletionProposal.Columns[LColumnIndex].Items.Add;
      end;
    finally
      LItems.Free;
    end;

    LCurrentInput := GetCurrentInput();
    LCanExecute := True;
    if Assigned(FOnBeforeCompletionProposalExecute) then
      FOnBeforeCompletionProposalExecute(Self, FCompletionProposal.Columns,
        LCurrentInput, LCanExecute);
    if LCanExecute then
      Execute(LCurrentInput, LPoint)
    else
    begin
      FCompletionProposalPopupWindow.Free;
      FCompletionProposalPopupWindow := nil;
    end;
  end;
end;

procedure TCustomBCEditor.DoCopyToClipboard(const AText: string);
var
  ClipboardData: Pointer;
  Global: HGLOBAL;
  Opened: Boolean;
  Retry: Integer;
begin
  if (AText <> '') then
  begin
    Retry := 0;
    repeat
      Opened := OpenClipboard(Handle);
      if (not Opened) then
      begin
        Sleep(50);
        Inc(Retry);
      end;
    until (Opened or (Retry = 10));

    if (not Opened) then
      raise EClipboardException.CreateFmt(SCannotOpenClipboard, [SysErrorMessage(GetLastError)])
    else
      try
        EmptyClipboard();
        Global := GlobalAlloc(GMEM_MOVEABLE or GMEM_DDESHARE, (Length(AText) + 1) * SizeOf(Char));
        if (Global <> 0) then
        try
          ClipboardData := GlobalLock(Global);
          if (Assigned(ClipboardData)) then
          begin
            StrPCopy(ClipboardData, AText);
            SetClipboardData(CF_UNICODETEXT, Global);
          end;
        finally
          GlobalUnlock(Global);
        end;
      finally
        CloseClipboard();
      end;
  end;
end;

procedure TCustomBCEditor.DoCutToClipboard;
begin
  if not ReadOnly and SelectionAvailable then
  begin
    DoCopyToClipboard(SelText);
    SelText := '';
  end;
end;

procedure TCustomBCEditor.DoEditorBottom(const ACommand: TBCEditorCommand);
begin
  MoveCaretAndSelection(FLines.CaretPosition, FLines.EOFPosition, ACommand = ecSelectionEditorBottom);
end;

procedure TCustomBCEditor.DoEditorTop(const ACommand: TBCEditorCommand);
begin
  MoveCaretAndSelection(FLines.CaretPosition, FLines.BOFPosition, ACommand = ecSelectionEditorTop);
end;

procedure TCustomBCEditor.DoEndKey(const ASelectionCommand: Boolean);
var
  LNewCaretPosition: TBCEditorLinesPosition;
begin
  if (FRows.CaretPosition.Row < FRows.Count) then
    LNewCaretPosition := FRows.EORPosition[FRows.CaretPosition.Row]
  else
    LNewCaretPosition := FLines.BOLPosition[FLines.CaretPosition.Line];
  MoveCaretAndSelection(FLines.CaretPosition, LNewCaretPosition, ASelectionCommand);
end;

procedure TCustomBCEditor.DoHomeKey(const ASelectionCommand: Boolean);
var
  LLeftSpaceCount: Integer;
  LNewCaretPosition: TBCEditorLinesPosition;
begin
  LNewCaretPosition := FLines.CaretPosition;
  if (FWordWrap.Enabled) then
    LNewCaretPosition := FRows.BORPosition[FRows.CaretPosition.Row]
  else if (FLines.CaretPosition.Line < FLines.Count) then
  begin
    LLeftSpaceCount := LeftSpaceCount(FLines.Items[LNewCaretPosition.Line].Text);
    if (LNewCaretPosition.Char > LLeftSpaceCount) then
      LNewCaretPosition.Char := LLeftSpaceCount
    else
      LNewCaretPosition.Char := 0;
  end
  else
    LNewCaretPosition := FLines.BOLPosition[LNewCaretPosition.Line];

  MoveCaretAndSelection(FLines.CaretPosition, LNewCaretPosition, ASelectionCommand);
end;

procedure TCustomBCEditor.DoImeStr(AData: Pointer);
begin
  DoInsertText(StrPas(PChar(AData)));
end;

procedure TCustomBCEditor.DoInsertText(const AText: string);
begin
  BeginUpdate();
  try
    if (SelectionAvailable) then
    begin
      BeginUpdate();
      SelText := AText;
      FLines.SelArea := LinesArea(FLines.SelArea.EndPosition, FLines.SelArea.EndPosition);
      EndUpdate();
    end
    else if ((FTextEntryMode = temOverwrite)
      and (FLines.CaretPosition.Line < FLines.Count)
      and (FLines.CaretPosition.Char < Length(FLines.Items[FLines.CaretPosition.Line].Text))) then
    begin
      FLines.ReplaceText(LinesArea(FLines.CaretPosition, LinesPosition(FLines.CaretPosition.Char + 1, FLines.CaretPosition.Line)), AText);
      if (FSyncEdit.Active) then
        FSyncEdit.MoveEndPositionChar(Length(AText));
    end
    else
    begin
      FLines.InsertText(FLines.CaretPosition, AText);
      if (FSyncEdit.Active) then
        FSyncEdit.MoveEndPositionChar(Length(AText));
    end;

    if (FSyncEdit.Active) then
      DoSyncEdit();
  finally
    EndUpdate();
  end;
end;

procedure TCustomBCEditor.DoKeyPressW(var AMessage: TWMKey);
var
  LForm: TCustomForm;
  LKey: Char;
begin
  LKey := Char(AMessage.CharCode);

  if FCompletionProposal.Enabled and FCompletionProposal.Trigger.Enabled then
  begin
    if Pos(LKey, FCompletionProposal.Trigger.Chars) > 0 then
    begin
      FCompletionProposalTimer.Interval := FCompletionProposal.Trigger.Interval;
      FCompletionProposalTimer.Enabled := True;
    end
    else
      FCompletionProposalTimer.Enabled := False;
  end;

  LForm := GetParentForm(Self);
  if Assigned(LForm) and (LForm <> TWinControl(Self)) and LForm.KeyPreview and (LKey <= High(AnsiChar)) and
    TUnprotectedWinControl(LForm).DoKeyPress(AMessage) then
    Exit;

  if csNoStdEvents in ControlStyle then
    Exit;

  if Assigned(FOnKeyPressW) then
    FOnKeyPressW(Self, LKey);

  if LKey <> BCEDITOR_NONE_CHAR then
    KeyPressW(LKey);
end;

procedure TCustomBCEditor.DoLineBreak();
var
  LInsertText: string;
begin
  if (SelectionAvailable) then
    SelText := ''
  else if (FLines.CaretPosition.Line >= FLines.Count) then
    FLines.CaretPosition := FLines.BOLPosition[FLines.CaretPosition.Line + 1]
  else if (FTextEntryMode = temInsert) then
  begin
    LInsertText := FLines.LineBreak;
    if ((FLines.CaretPosition.Char > 0) and (eoAutoIndent in FOptions)) then
      LInsertText := LInsertText + ComputeIndentText(Min(FRows.CaretPosition.Column, LeftSpaceCount(FLines.Items[FLines.CaretPosition.Line].Text, True)));
    FLines.InsertText(FLines.CaretPosition, LInsertText);
  end
  else
  begin
    if ((FLines.CaretPosition.Char > 0) and (eoAutoIndent in FOptions)) then
      FLines.CaretPosition := LinesPosition(Min(FLines.CaretPosition.Char, LeftSpaceCount(FLines.Items[FLines.CaretPosition.Line].Text, True)), FLines.CaretPosition.Line + 1)
    else
      FLines.CaretPosition := FLines.BOLPosition[FLines.CaretPosition.Line + 1];
  end;
end;

procedure TCustomBCEditor.DoLineComment();
var
  LArea: TBCEditorLinesArea;
  LComment: Integer;
  LCommentsCount: Integer;
  LCurrentComment: Integer;
  LOpenToken: string;
begin
  LCommentsCount := Length(FHighlighter.Comments.LineComments);
  if (LCommentsCount > 0) then
  begin
    LArea.BeginPosition := FLines.BOLPosition[Min(FLines.SelArea.BeginPosition, FLines.SelArea.EndPosition).Line];
    LArea.EndPosition := LinesPosition(LArea.BeginPosition.Char, Max(FLines.SelArea.BeginPosition, FLines.SelArea.EndPosition).Line);

    if (LArea.BeginPosition.Line < FLines.Count) then
    begin
      LCurrentComment := -1;
      for LComment := LCommentsCount - 1 downto 0 do
        if (Copy(FLines.Items[LArea.BeginPosition.Line].Text, 1 + LArea.BeginPosition.Char, Length(FHighlighter.Comments.LineComments[LComment])) = FHighlighter.Comments.LineComments[LComment]) then
          LCurrentComment := LComment;
      if (LCurrentComment < 0) then
        LOpenToken := ''
      else
        LOpenToken := FHighlighter.Comments.LineComments[LCurrentComment];

      FLines.BeginUpdate();
      try
        if (LCurrentComment >= 0) then
        begin
          FLines.DeleteIndent(LArea.BeginPosition, LArea.EndPosition,
            FHighlighter.Comments.LineComments[LCurrentComment]);
        end;

        if ((LCurrentComment < 0)
          or (LArea.BeginPosition.Line <> LArea.EndPosition.Line) and (LCurrentComment < LCommentsCount - 1)) then
        begin
          Inc(LCurrentComment);

          FLines.InsertIndent(LArea.BeginPosition, LArea.EndPosition,
            FHighlighter.Comments.LineComments[LCurrentComment]);
        end;

        if (not SelectionAvailable) then
        begin
          LArea.BeginPosition.Char := 0;
          if (LArea.EndPosition.Char > 0) then
            LArea.EndPosition := FLines.EOLPosition[LArea.EndPosition.Line];
          SetCaretAndSelection(LArea.EndPosition, LArea);
        end;
      finally
        FLines.EndUpdate();
      end;
    end;
  end;
end;

function TCustomBCEditor.DoMouseWheelDown(Shift: TShiftState; MousePos: TPoint): Boolean;
begin
  Result := inherited;

  if (not Result) then
  begin
    if (ssCtrl in Shift) then
      TopRow := TopRow + VisibleRows shr Ord(soHalfPage in FScroll.Options)
    else if (ssShift in Shift) then
    begin
      if (FRows.CaretPosition.Row < FRows.Count - 2) then
        MoveCaretVertically(FWheelScrollLines, False);
    end
    else
      TopRow := TopRow + Integer(FWheelScrollLines);
    Result := True;
  end;
end;

function TCustomBCEditor.DoMouseWheelUp(Shift: TShiftState; MousePos: TPoint): Boolean;
begin
  Result := inherited;

  if (not Result) then
  begin
    if (ssCtrl in Shift) then
      TopRow := TopRow - VisibleRows shr Ord(soHalfPage in FScroll.Options)
    else if (ssShift in Shift) then
    begin
      if (FRows.CaretPosition.Row > 0) then
        MoveCaretVertically(- FWheelScrollLines, False);
    end
    else
      TopRow := TopRow - Integer(FWheelScrollLines);
    Result := True;
  end;
end;

procedure TCustomBCEditor.DoOnCommandProcessed(ACommand: TBCEditorCommand; const AChar: Char; AData: Pointer);
begin
  if Assigned(FOnCommandProcessed) then
    FOnCommandProcessed(Self, ACommand, AChar, AData);
end;

procedure TCustomBCEditor.DoOnProcessCommand(var ACommand: TBCEditorCommand; var AChar: Char; AData: Pointer);
begin
  if ACommand < ecUserFirst then
  begin
    if Assigned(FOnProcessCommand) then
      FOnProcessCommand(Self, ACommand, AChar, AData);
  end
  else
  if Assigned(FOnProcessUserCommand) then
    FOnProcessUserCommand(Self, ACommand, AChar, AData);
end;

function TCustomBCEditor.DoOnReplaceText(const APattern, AReplaceText: string;
  APosition: TBCEditorLinesPosition): TBCEditorReplaceAction;
begin
  if (not Assigned(FOnReplaceText)) then
    Result := raCancel
  else
    FOnReplaceText(Self, APattern, AReplaceText, APosition, Result);
end;

procedure TCustomBCEditor.DoPageTopOrBottom(const ACommand: TBCEditorCommand);
var
  LNewRow: Integer;
  LNewCaretPosition: TBCEditorLinesPosition;
begin
  case (ACommand) of
    ecPageTop,
    ecSelectionPageTop:
      LNewRow := TopRow;
    ecPageBottom,
    ecSelectionPageBottom:
      LNewRow := TopRow + VisibleRows - 1;
    else raise ERangeError.Create('ACommand: ' + IntToStr(Ord(ACommand)));
  end;

  LNewCaretPosition := RowsToLines(RowsPosition(FRows.CaretPosition.Column, LNewRow));
  if (not (soBeyondEndOfFile in Scroll.Options)) then
    LNewCaretPosition.Line := Min(LNewCaretPosition.Line, FLines.Count - 1);

  MoveCaretAndSelection(FLines.CaretPosition, LNewCaretPosition, ACommand in [ecSelectionPageTop, ecSelectionPageBottom]);
end;

procedure TCustomBCEditor.DoPageUpOrDown(const ACommand: TBCEditorCommand);
var
  LRowCount: Integer;
begin
  case (ACommand) of
    ecPageUp,
    ecSelectionPageUp:
      LRowCount := - (VisibleRows shr Ord(soHalfPage in FScroll.Options));
    ecPageDown,
    ecSelectionPageDown:
      LRowCount := VisibleRows shr Ord(soHalfPage in FScroll.Options);
    else raise ERangeError.Create('ACommand: ' + IntToStr(Ord(ACommand)));
  end;

  BeginUpdate();
  TopRow := TopRow + LRowCount;
  MoveCaretVertically(LRowCount, ACommand in [ecSelectionPageUp, ecSelectionPageDown]);
  EndUpdate();
end;

procedure TCustomBCEditor.DoPasteFromClipboard();
var
  ClipboardData: Pointer;
  Global: HGLOBAL;
  Opened: Boolean;
  Retry: Integer;
  Text: string;
begin
  if (IsClipboardFormatAvailable(CF_UNICODETEXT)) then
  begin
    Retry := 0;
    repeat
      Opened := OpenClipboard(Handle);
      if (not Opened) then
      begin
        Sleep(50);
        Inc(Retry);
      end;
    until (Opened or (Retry = 10));

    if (not Opened) then
      raise EClipboardException.CreateFmt(SCannotOpenClipboard, [SysErrorMessage(GetLastError)])
    else
    begin
      try
        Global := GetClipboardData(CF_UNICODETEXT);
        if (Global <> 0) then
        begin
          ClipboardData := GlobalLock(Global);
          if (Assigned(ClipboardData)) then
            Text := StrPas(PChar(ClipboardData));
          GlobalUnlock(Global);
        end;
      finally
        CloseClipboard();
      end;

      FLines.BeginUpdate();
      try
        FLines.UndoGroupBreak();
        DoInsertText(Text);
      finally
        FLines.EndUpdate();
      end;
    end;
  end;
end;

procedure TCustomBCEditor.DoRedo();
begin
  Redo();
end;

function TCustomBCEditor.DoReplaceText(): Integer;
var
  LActionReplace: TBCEditorReplaceAction;
  LFindLength: Integer;
  LFindEndPosition: TBCEditorLinesPosition;
  LPromptReplace: Boolean;
  LSearch: TBCEditorLines.TSearch;
  LSearchPosition: TBCEditorLinesPosition;
  LSearchResult: TBCEditorLinesArea;
  LSuccess: Boolean;
begin
  if (Length(Replace.Pattern) = 0) then
    Result := 0
  else
  begin
    Result := 0;

    FSearchResults.Clear();
    ClearCodeFolding();

    LPromptReplace := (roPrompt in FReplace.Options) and Assigned(OnReplaceText);

    Include(FState, esReplacing);
    if (LPromptReplace) then
      FLines.UndoList.BeginUpdate()
    else
      FLines.BeginUpdate();
    try
      LSearch := TBCEditorLines.TSearch.Create(FLines,
        Replace.Area,
        roCaseSensitive in Replace.Options, roWholeWordsOnly in Replace.Options, Replace.Engine = seRegularExpression, roBackwards in Replace.Options,
        Replace.Pattern, Replace.ReplaceText);

      if (FLines.Count = 0) then
        LSearchPosition := FLines.BOFPosition
      else
      begin
        LSearchPosition := FLines.CaretPosition;
        LSearchPosition.Line := Min(LSearchPosition.Line, FLines.Count - 1);
        LSearchPosition.Char := Min(LSearchPosition.Char, Length(FLines[LSearchPosition.Line]));
      end;

      if (roReplaceAll in Replace.Options) then
        LActionReplace := raReplaceAll
      else
        LActionReplace := raReplace;

      repeat
        LSuccess := LSearch.Find(LSearchPosition, LFindLength);
        if (not LSuccess) then
          LActionReplace := raCancel;

        if ((LActionReplace <> raCancel) and LPromptReplace) then
        begin
          LFindEndPosition := FLines.PositionOf(LFindLength, LSearchPosition);
          SetCaretAndSelection(LFindEndPosition, LinesArea(LSearchPosition, LFindEndPosition));
          LActionReplace := DoOnReplaceText(Replace.Pattern, Replace.ReplaceText, LSearchResult.BeginPosition);
        end;
        if (LActionReplace in [raReplace, raReplaceAll]) then
        begin
          LSearch.Replace();
          Inc(Result);
        end;
      until (LActionReplace = raCancel);

      FSearchStatus := LSearch.ErrorMessage;
      LSearch.Free();
    finally
      if (LPromptReplace) then
        FLines.UndoList.EndUpdate()
      else
        FLines.EndUpdate();
      Exclude(FState, esReplacing);

      InitCodeFolding();

      if (LPromptReplace and CanFocus) then
        SetFocus();
    end;
  end;
end;

procedure TCustomBCEditor.DoScanCodeFoldingRanges();
var
  LIndex: Integer;
  LLine: Integer;
  LRange: TBCEditorCodeFolding.TRanges.TRange;
begin
  if (FCodeFolding.Visible) then
  begin
    for LLine := 0 to FLines.Count - 1 do
    begin
      FLines.SetCodeFoldingBeginRange(LLine, nil);
      FLines.SetCodeFoldingEndRange(LLine, nil);
      FLines.SetCodeFoldingTreeLine(LLine, False);
    end;

    FAllCodeFoldingRanges.ClearAll();

    ScanCodeFoldingRanges();

    Exclude(FState, esCodeFoldingInvalid);

    for LIndex := FAllCodeFoldingRanges.AllCount - 1 downto 0 do
    begin
      LRange := FAllCodeFoldingRanges[LIndex];
      if (Assigned(LRange)
        and not LRange.ParentCollapsed
        and ((LRange.BeginLine <> LRange.EndLine)
          or LRange.RegionItem.TokenEndIsPreviousLine)) then
      begin
        FLines.SetCodeFoldingBeginRange(LRange.BeginLine, LRange);

        if LRange.Collapsable then
        begin
          for LLine := LRange.BeginLine + 1 to LRange.EndLine - 1 do
            FLines.SetCodeFoldingTreeLine(LLine, True);

          FLines.SetCodeFoldingEndRange(LRange.EndLine, LRange);
        end;
      end;
    end;

    Invalidate();
  end;
end;

procedure TCustomBCEditor.DoScroll(const ACommand: TBCEditorCommand);
var
  LRow: Integer;
begin
  LRow := FRows.CaretPosition.Row;
  if ((LRow >= TopRow) and (LRow < TopRow + VisibleRows)) then
    if ACommand = ecScrollUp then
    begin
      TopRow := TopRow - 1;
      if LRow > TopRow + VisibleRows - 1 then
        MoveCaretVertically((TopRow + VisibleRows - 1) - LRow, False);
    end
    else
    begin
      TopRow := TopRow + 1;
      if LRow < TopRow then
        MoveCaretVertically(TopRow - LRow, False);
    end;

  ScrollToCaret();
end;

function TCustomBCEditor.DoSearch(AArea: TBCEditorLinesArea; var APosition: TBCEditorLinesPosition): Boolean;
var
  LFoundLength: Integer;
  LSearch: TBCEditorLines.TSearch;
  LSearchResult: TBCEditorLinesArea;
begin
  if ((Length(Search.Pattern) = 0) or AArea.IsEmpty()) then
  begin
    FSearchStatus := SBCEditorPatternIsEmpty;
    Result := False;
  end
  else
  begin
    FSearchStatus := '';
    FSearchResults.Clear();

    if ((soBackwards in Search.Options) and (APosition = FLines.BOFPosition)) then
      Result := False
    else
    begin
      LSearch := TBCEditorLines.TSearch.Create(FLines,
        AArea,
        soCaseSensitive in Search.Options, soWholeWordsOnly in Search.Options, Search.Engine = seRegularExpression, soBackwards in Search.Options,
        Search.Pattern, '');

      repeat
        if (soBackwards in Search.Options) then
          if (APosition.Char > 0) then
            Dec(APosition.Char)
          else
            APosition := FLines.EOLPosition[APosition.Line - 1];

        Result := LSearch.Find(APosition, LFoundLength);

        if (Result) then
        begin
          LSearchResult.BeginPosition := APosition;
          LSearchResult.EndPosition := FLines.PositionOf(LFoundLength, LSearchResult.BeginPosition);
          if (soBackwards in Search.Options) then
            FSearchResults.Insert(0, LSearchResult)
          else
            FSearchResults.Add(LSearchResult);
        end;

        if (Result and not (soBackwards in Search.Options)) then
          APosition := FLines.PositionOf(1, APosition);
      until (not Result or (AArea.BeginPosition >= AArea.EndPosition));

      FSearchStatus := LSearch.ErrorMessage;
      LSearch.Free();

      Result := FSearchResults.Count > 0;
    end;
  end;
end;

function TCustomBCEditor.DoSearchFind(const First: Boolean; const Action: TSearchFind): Boolean;
begin
  if (not Assigned(FSearchFindDialog)) then
  begin
    FSearchFindDialog := TFindDialog.Create(Self);
    FSearchFindDialog.Options := FSearchFindDialog.Options - [frMatchCase, frWholeWord] + [frDown];
    if (soBackwards in Search.Options) then
      FSearchFindDialog.Options := FSearchFindDialog.Options - [frDown];
    if (soCaseSensitive in Search.Options) then
      FSearchFindDialog.Options := FSearchFindDialog.Options + [frMatchCase];
    if (soWholeWordsOnly in Search.Options) then
      FSearchFindDialog.Options := FSearchFindDialog.Options + [frWholeWord];
    FSearchFindDialog.OnFind := DoSearchFindExecute;
    FSearchFindDialog.OnClose := DoSearchFindClose;
  end;

  FHideSelectionBeforeSearch := HideSelection;
  HideSelection := False;

  FSearchFindDialog.Execute();

  Result := True;
end;

procedure TCustomBCEditor.DoSearchFindClose(Sender: TObject);
begin
  HideSelection := FHideSelectionBeforeSearch;
end;

procedure TCustomBCEditor.DoSearchFindExecute(Sender: TObject);
begin
  Search.Engine := seNormal;
  Search.Pattern :=  TFindDialog(Sender).FindText;
  if (frDown in TFindDialog(Sender).Options) then
    Search.Options := Search.Options - [soBackwards]
  else
    Search.Options := Search.Options + [soBackwards];
  if (frMatchCase in TFindDialog(Sender).Options) then
    Search.Options := Search.Options + [soCaseSensitive]
  else
    Search.Options := Search.Options - [soCaseSensitive];
  if (frWholeWord in TFindDialog(Sender).Options) then
    Search.Options := Search.Options + [soWholeWordsOnly]
  else
    Search.Options := Search.Options - [soWholeWordsOnly];

  FindNext();
end;

function TCustomBCEditor.DoSearchMatchNotFoundWrapAroundDialog: Boolean;
begin
  Result := MessageDialog(Format(SBCEditorSearchMatchNotFound, [FLines.LineBreak + FLines.LineBreak]), mtConfirmation, [mbYes, mbNo]) = mrYes;
end;

function TCustomBCEditor.DoSearchNext(APosition: TBCEditorLinesPosition;
  out ASearchResult: TBCEditorLinesArea; const WrapAround: Boolean = False): Boolean;
var
  LIndex: Integer;
  LLeft: Integer;
  LMiddle: Integer;
  LRight: Integer;
begin
  Result := FSearchResults.Count > 0;

  if (Result) then
  begin
    if (APosition <= FSearchResults[0].BeginPosition) then
      LIndex := 0
    else if (APosition <= FSearchResults[FSearchResults.Count - 1].BeginPosition) then
    begin
      LIndex := -1;

      LLeft := 0;
      LRight := FSearchResults.Count - 1;

      while (LIndex < 0) do
      begin
        LMiddle := (LLeft + LRight) div 2;
        if (FSearchResults[LMiddle].BeginPosition < APosition) then
          LLeft := LMiddle + 1
        else if ((FSearchResults[LMiddle - 1].BeginPosition < APosition)
          and (APosition <= FSearchResults[LMiddle].BeginPosition)) then
          LIndex := LMiddle
        else
          LRight := LMiddle - 1;
      end;
    end
    else if (WrapAround or DoSearchMatchNotFoundWrapAroundDialog) then
      LIndex := 0
    else
      LIndex := -1;

    Result := LIndex >= 0;

    if (Result) then
      ASearchResult := FSearchResults[LIndex];
  end;
end;

function TCustomBCEditor.DoSearchPrevious(APosition: TBCEditorLinesPosition;
  out ASearchResult: TBCEditorLinesArea; const WrapAround: Boolean = False): Boolean;
var
  LIndex: Integer;
  LLeft: Integer;
  LMiddle: Integer;
  LRight: Integer;
begin
  Result := FSearchResults.Count > 0;

  if (Result) then
  begin
    if (APosition > FSearchResults[FSearchResults.Count - 1].BeginPosition) then
      LIndex := FSearchResults.Count - 1
    else if (APosition > FSearchResults[0].BeginPosition) then
    begin
      LIndex := -1;

      LLeft := 0;
      LRight := FSearchResults.Count - 1;

      while (LIndex < 0) do
      begin
        LMiddle := (LLeft + LRight) div 2;
        if (FSearchResults[LMiddle].BeginPosition < APosition) then
          LLeft := LMiddle + 1
        else if ((FSearchResults[LMiddle - 1].BeginPosition < APosition)
          and (APosition <= FSearchResults[LMiddle].BeginPosition)) then
          LIndex := LMiddle - 1
        else
          LRight := LMiddle - 1;
      end;
    end
    else if (WrapAround or DoSearchMatchNotFoundWrapAroundDialog) then
      LIndex := FSearchResults.Count - 1
    else
      LIndex := -1;

    Result := LIndex >= 0;
    if (Result) then
      ASearchResult := FSearchResults[LIndex];
  end;
end;

function TCustomBCEditor.DoSearchReplace(const Action: TSearchReplace): Boolean;
begin
  if (not Assigned(FSearchReplaceDialog)) then
  begin
    FSearchReplaceDialog := TReplaceDialog.Create(Self);
    FSearchReplaceDialog.FindText := Replace.Pattern;
    FSearchReplaceDialog.ReplaceText := Replace.ReplaceText;
    FSearchReplaceDialog.Options := FSearchReplaceDialog.Options - [frMatchCase, frWholeWord, frReplaceAll] + [frDown];
    if (roBackwards in Replace.Options) then
      FSearchReplaceDialog.Options := FSearchReplaceDialog.Options - [frDown];
    if (roCaseSensitive in Replace.Options) then
      FSearchReplaceDialog.Options := FSearchReplaceDialog.Options + [frMatchCase];
    if (roReplaceAll in Replace.Options) then
      FSearchReplaceDialog.Options := FSearchReplaceDialog.Options + [frReplaceAll];
    if (roWholeWordsOnly in Replace.Options) then
      FSearchReplaceDialog.Options := FSearchReplaceDialog.Options + [frWholeWord];
    FSearchReplaceDialog.OnClose := DoSearchFindClose;
    FSearchReplaceDialog.OnFind := DoSearchReplaceFind;
    FSearchReplaceDialog.OnReplace := DoSearchReplaceExecute;
  end;

  FHideSelectionBeforeSearch := HideSelection;
  HideSelection := False;

  FSearchReplaceDialog.Execute();

  Result := True;
end;

procedure TCustomBCEditor.DoSearchReplaceExecute(Sender: TObject);
begin
  Replace.Engine := seNormal;
  Replace.Pattern := TReplaceDialog(Sender).FindText;
  Replace.ReplaceText := TReplaceDialog(Sender).ReplaceText;
  if (frMatchCase in TReplaceDialog(Sender).Options) then
    Replace.Options := Replace.Options + [roCaseSensitive]
  else
    Replace.Options := Replace.Options - [roCaseSensitive];
  if (frWholeWord in TReplaceDialog(Sender).Options) then
    Replace.Options := Replace.Options + [roWholeWordsOnly]
  else
    Replace.Options := Replace.Options - [roWholeWordsOnly];
  if (frReplaceAll in TReplaceDialog(Sender).Options) then
    Replace.Options := Replace.Options + [roReplaceAll]
  else
    Replace.Options := Replace.Options - [roReplaceAll];

  if (SelectionAvailable) then
    Replace.Area := LinesArea(FLines.SelArea.BeginPosition, FLines.EOFPosition)
  else
    Replace.Area := LinesArea(FLines.CaretPosition, FLines.EOFPosition);

  DoReplaceText();
end;

procedure TCustomBCEditor.DoSearchReplaceFind(Sender: TObject);
begin
  Search.Engine := seNormal;
  if (frDown in TReplaceDialog(Sender).Options) then
    Search.Options := Search.Options - [soBackwards]
  else
    Search.Options := Search.Options + [soBackwards];
  if (frMatchCase in TReplaceDialog(Sender).Options) then
    Search.Options := Search.Options + [soCaseSensitive]
  else
    Search.Options := Search.Options - [soCaseSensitive];
  if (frWholeWord in TReplaceDialog(Sender).Options) then
    Search.Options := Search.Options + [soWholeWordsOnly]
  else
    Search.Options := Search.Options - [soWholeWordsOnly];
  Search.Pattern := TReplaceDialog(Sender).FindText;

  FindNext();
end;

procedure TCustomBCEditor.DoSearchStringNotFoundDialog;
begin
  MessageDialog(Format(SBCEditorSearchStringNotFound, [Search.Pattern]), mtInformation, [mbOK]);
end;

procedure TCustomBCEditor.DoSetBookmark(const ACommand: TBCEditorCommand; AData: Pointer);
var
  LIndex: Integer;
  LLinesCaretPosition: TBCEditorLinesPosition;
begin
  LLinesCaretPosition := FLines.CaretPosition;
  LIndex := ACommand - ecSetBookmark1;
  if Assigned(AData) then
    LLinesCaretPosition := TBCEditorLinesPosition(AData^);
  if not DeleteBookmark(LLinesCaretPosition.Line, LIndex) then
    SetBookmark(LIndex, LLinesCaretPosition);
end;

procedure TCustomBCEditor.DoShiftTabKey;
var
  LNewCaretPosition: TBCEditorLinesPosition;
  LTabWidth: Integer;
begin
  if ((toSelectedBlockIndent in FTabs.Options) and SelectionAvailable) then
    DoBlockIndent(ecBlockUnindent)
  else
  begin
    if (toTabsToSpaces in FTabs.Options) then
      LTabWidth := FTabs.Width
    else
      LTabWidth := 1;
    LNewCaretPosition := LinesPosition(Max(0, FLines.CaretPosition.Char - LTabWidth + 1), FLines.CaretPosition.Line);

    if ((LNewCaretPosition <> FLines.CaretPosition)
      and (Copy(FLines.Items[FLines.CaretPosition.Line].Text, 1 + LNewCaretPosition.Char, LTabWidth) = BCEDITOR_TAB_CHAR)) then
      FLines.DeleteText(LinesArea(LNewCaretPosition, FLines.CaretPosition));
  end;
end;

procedure TCustomBCEditor.DoSyncEdit;
var
  LDifference: Integer;
  LEditText: string;
  LIndex1: Integer;
  LIndex2: Integer;
  LLinesBeginPosition: TBCEditorLinesPosition;
  LLinesCaretPosition: TBCEditorLinesPosition;
  LLinesEndPosition: TBCEditorLinesPosition;
  LLinesSameLinePosition: TBCEditorLinesPosition;
begin
  LLinesCaretPosition := FLines.CaretPosition;

  FLines.BeginUpdate();
  try
    LEditText := Copy(FLines.Items[FSyncEdit.EditArea.BeginPosition.Line].Text, 1 + FSyncEdit.EditArea.BeginPosition.Char,
      FSyncEdit.EditArea.EndPosition.Char - FSyncEdit.EditArea.BeginPosition.Char);
    LDifference := Length(LEditText) - FSyncEdit.EditWidth;
    for LIndex1 := 0 to FSyncEdit.SyncItems.Count - 1 do
    begin
      LLinesBeginPosition := PBCEditorTextPosition(FSyncEdit.SyncItems.Items[LIndex1])^;

      if (LLinesBeginPosition.Line = FSyncEdit.EditArea.BeginPosition.Line) and
        (LLinesBeginPosition.Char < FSyncEdit.EditArea.BeginPosition.Char) then
      begin
        FSyncEdit.MoveBeginPositionChar(LDifference);
        FSyncEdit.MoveEndPositionChar(LDifference);
        Inc(LLinesCaretPosition.Char, LDifference);
      end;

      if (LLinesBeginPosition.Line = FSyncEdit.EditArea.BeginPosition.Line) and
        (LLinesBeginPosition.Char > FSyncEdit.EditArea.BeginPosition.Char) then
      begin
        Inc(LLinesBeginPosition.Char, LDifference);
        PBCEditorTextPosition(FSyncEdit.SyncItems.Items[LIndex1])^.Char := LLinesBeginPosition.Char;
      end;

      LLinesEndPosition := LLinesBeginPosition;
      Inc(LLinesEndPosition.Char, FSyncEdit.EditWidth);

      FLines.DeleteText(LinesArea(LLinesBeginPosition, LLinesEndPosition));
      FLines.InsertText(LLinesBeginPosition, LEditText);

      LIndex2 := LIndex1 + 1;
      if LIndex2 < FSyncEdit.SyncItems.Count then
      begin
        LLinesSameLinePosition := PBCEditorTextPosition(FSyncEdit.SyncItems.Items[LIndex2])^;

        while (LIndex2 < FSyncEdit.SyncItems.Count) and (LLinesSameLinePosition.Line = LLinesBeginPosition.Line) do
        begin
          PBCEditorTextPosition(FSyncEdit.SyncItems.Items[LIndex2])^.Char := LLinesSameLinePosition.Char + LDifference;

          Inc(LIndex2);
          if LIndex2 < FSyncEdit.SyncItems.Count then
            LLinesSameLinePosition := PBCEditorTextPosition(FSyncEdit.SyncItems.Items[LIndex2])^;
        end;
      end;
    end;
    FSyncEdit.EditWidth := FSyncEdit.EditArea.EndPosition.Char - FSyncEdit.EditArea.BeginPosition.Char;
    FLines.CaretPosition := LLinesCaretPosition;
  finally
    FLines.EndUpdate();
  end;
end;

procedure TCustomBCEditor.DoTabKey();
var
  LChangeScrollPastEndOfLine: Boolean;
  LCharCount: Integer;
  LLengthAfterLine: Integer;
  LPreviousLine: Integer;
  LPreviousLineCharCount: Integer;
  LRowsPosition: TBCEditorRowsPosition;
  LTabText: string;
  LLinesCaretPosition: TBCEditorLinesPosition;
begin
  if ((FLines.SelArea.BeginPosition.Line <> FLines.SelArea.EndPosition.Line)
    and (toSelectedBlockIndent in FTabs.Options)) then
    DoBlockIndent(ecBlockIndent)
  else if (SelectionAvailable or
    (FLines.CaretPosition.Line >= FLines.Count)) then
  begin
    if (not (toTabsToSpaces in FTabs.Options)) then
    begin
      LTabText := StringOfChar(BCEDITOR_TAB_CHAR, FTabs.Width div FTabs.Width);
      LTabText := LTabText + StringOfChar(BCEDITOR_TAB_CHAR, FTabs.Width mod FTabs.Width);
    end
    else
      LTabText := StringOfChar(BCEDITOR_SPACE_CHAR, FTabs.Width - (FRows.CaretPosition.Column - 1) mod FTabs.Width);
    DoInsertText(LTabText);
  end
  else
  begin
    FLines.BeginUpdate();
    try
      LLinesCaretPosition := FLines.CaretPosition;

      LRowsPosition := FRows.CaretPosition;
      LLengthAfterLine := Max(0, LRowsPosition.Column - FRows.Items[LRowsPosition.Row].Columns);

      if LLengthAfterLine > 1 then
        LCharCount := LLengthAfterLine
      else
        LCharCount := FTabs.Width;

      if toPreviousLineIndent in FTabs.Options then
        if Trim(FLines.Items[LLinesCaretPosition.Line].Text) = '' then
        begin
          LPreviousLine := LLinesCaretPosition.Line - 1;
          while (LPreviousLine >= 0) and (FLines.Items[LPreviousLine].Text = '') do
            Dec(LPreviousLine);
          LPreviousLineCharCount := LeftSpaceCount(FLines.Items[LPreviousLine].Text, True);
          if LPreviousLineCharCount > LLinesCaretPosition.Char + 1 then
            LCharCount := LPreviousLineCharCount - LeftSpaceCount(FLines.Items[LLinesCaretPosition.Line].Text, True)
        end;

      if LLengthAfterLine > 1 then
        LLinesCaretPosition := FLines.BOLPosition[LLinesCaretPosition.Line];

      if (not (toTabsToSpaces in FTabs.Options)) then
      begin
        LTabText := StringOfChar(BCEDITOR_TAB_CHAR, LCharCount div FTabs.Width);
        LTabText := LTabText + StringOfChar(BCEDITOR_TAB_CHAR, LCharCount mod FTabs.Width);
      end
      else
        LTabText := StringOfChar(BCEDITOR_SPACE_CHAR, LCharCount - (LRowsPosition.Column - 1) mod FTabs.Width);

      if FTextEntryMode = temInsert then
        FLines.InsertText(LLinesCaretPosition, LTabText);

      LChangeScrollPastEndOfLine := not (soBeyondEndOfLine in FScroll.Options);
      try
        if LChangeScrollPastEndOfLine then
          FScroll.SetOption(soBeyondEndOfLine, True);
        if FTextEntryMode = temOverwrite then
          LTabText := StringReplace(LTabText, BCEDITOR_TAB_CHAR, StringOfChar(BCEDITOR_SPACE_CHAR, FTabs.Width),
            [rfReplaceAll]);
        FLines.CaretPosition := LinesPosition(LLinesCaretPosition.Char + Length(LTabText), FLines.CaretPosition.Line);
      finally
        if LChangeScrollPastEndOfLine then
          FScroll.SetOption(soBeyondEndOfLine, False);
      end;
    finally
      FLines.EndUpdate();
    end;
  end;
end;

procedure TCustomBCEditor.DoToggleBookmark(const APosition: TBCEditorLinesPosition);
var
  LIndex: Integer;
  LMark: TBCEditorMark;
  LMarkIndex: Integer;
begin
  LMarkIndex := 0;
  for LIndex := 0 to FBookmarkList.Count - 1 do
  begin
    LMark := FBookmarkList.Items[LIndex];
    if LMark.Line = APosition.Line then
    begin
      DeleteBookmark(LMark);
      Exit;
    end;
    LMarkIndex := Max(LMarkIndex, LMark.Index + 1);
  end;
  LMarkIndex := Min(BCEDITOR_BOOKMARKS - 1, LMarkIndex);
  SetBookmark(LMarkIndex, APosition);
end;

procedure TCustomBCEditor.DoToggleMark(const APosition: TBCEditorLinesPosition);
var
  LIndex: Integer;
  LMark: TBCEditorMark;
  LMarkIndex: Integer;
begin
  LMarkIndex := 0;
  for LIndex := 0 to FMarkList.Count - 1 do
  begin
    LMark := FMarkList.Items[LIndex];
    if LMark.Line = APosition.Line then
    begin
      DeleteMark(LMark);
      Exit;
    end;
    LMarkIndex := Max(LMarkIndex, LMark.Index + 1);
  end;
  SetMark(LMarkIndex, APosition, FLeftMargin.Marks.DefaultImageIndex);
end;

procedure TCustomBCEditor.DoToggleSelectedCase(const ACommand: TBCEditorCommand);
var
  LSelectedText: string;
begin
  if (SelectionAvailable) then
  begin
    LSelectedText := SelText;
    case (ACommand) of
      ecUpperCase:
        SelText := AnsiUpperCase(LSelectedText);
      ecLowerCase:
        SelText := AnsiLowerCase(LSelectedText);
      else ERangeError.Create('ACommand: ' + IntToStr(Ord(ACommand)));
    end;
  end;
end;

procedure TCustomBCEditor.DoTripleClick();
begin
  FLines.SelArea := FLines.LineArea[FLines.CaretPosition.Line];

  FLastDoubleClickTime := 0;
end;

procedure TCustomBCEditor.DoUndo();
begin
  Undo();
end;

procedure TCustomBCEditor.DoWordLeft(const ACommand: TBCEditorCommand);
var
  LNewCaretPosition: TBCEditorLinesPosition;
begin
  if ((FLines.CaretPosition.Line = 0) and (FLines.Count = 0)) then
    FLines.CaretPosition := FLines.BOFPosition
  else if (FLines.CaretPosition.Line < FLines.Count) then
  begin
    LNewCaretPosition := FLines.CaretPosition;
    if (LNewCaretPosition.Line >= FLines.Count) then
      LNewCaretPosition := FLines.EOLPosition[FLines.Count - 1];
    if ((LNewCaretPosition.Char = 0)
      or (LNewCaretPosition.Char >= Length(FLines.Items[LNewCaretPosition.Line].Text))
      or IsWordBreakChar(FLines.Items[LNewCaretPosition.Line].Text[1 + LNewCaretPosition.Char - 1])) then
      LNewCaretPosition := PreviousWordPosition(LNewCaretPosition);
    if ((LNewCaretPosition.Char > 0)
      and ((LNewCaretPosition = FLines.CaretPosition) or (LNewCaretPosition.Char < Length(FLines.Items[LNewCaretPosition.Line].Text)))
      and not IsWordBreakChar(FLines.Items[LNewCaretPosition.Line].Text[1 + LNewCaretPosition.Char - 1])) then
      LNewCaretPosition := WordBegin(LNewCaretPosition);
    MoveCaretAndSelection(FLines.CaretPosition, LNewCaretPosition, ACommand = ecSelectionWordLeft);
  end
  else if (FLines.CaretPosition.Line = FLines.Count) then
    FLines.CaretPosition := FLines.EOLPosition[FLines.CaretPosition.Line - 1]
  else
    FLines.CaretPosition := FLines.BOLPosition[FLines.CaretPosition.Line - 1];
end;

procedure TCustomBCEditor.DoWordRight(const ACommand: TBCEditorCommand);
var
  LNewCaretPosition: TBCEditorLinesPosition;
begin
  LNewCaretPosition := FLines.CaretPosition;
  if (LNewCaretPosition.Line < FLines.Count) then
  begin
    if ((LNewCaretPosition.Char < Length(FLines.Items[LNewCaretPosition.Line].Text))
      and not IsWordBreakChar(FLines.Char[LNewCaretPosition])) then
    begin
      LNewCaretPosition := WordEnd();
      Inc(LNewCaretPosition.Char);
      while ((LNewCaretPosition.Char < Length(FLines.Items[LNewCaretPosition.Line].Text))) and IsEmptyChar(FLines.Char[LNewCaretPosition]) do
        Inc(LNewCaretPosition.Char);
    end;
    if ((LNewCaretPosition.Char >= Length(FLines.Items[LNewCaretPosition.Line].Text))
      or IsWordBreakChar(FLines.Char[LNewCaretPosition])) then
      LNewCaretPosition := NextWordPosition(LNewCaretPosition);
    MoveCaretAndSelection(FLines.CaretPosition, LNewCaretPosition, ACommand = ecSelectionWordRight);
  end;
end;

procedure TCustomBCEditor.DragCanceled();
begin
  inherited;

  InsertPos := InvalidCaretPos;
end;

procedure TCustomBCEditor.DragDrop(ASource: TObject; X, Y: Integer);
begin
  if (not ReadOnly) then
  begin
    inherited;

    InsertPos := InvalidCaretPos;
  end;
end;

function TCustomBCEditor.DragEnter(const dataObj: IDataObject; grfKeyState: Longint;
  pt: TPoint; var dwEffect: Longint): HResult;
var
  LFormat: FORMATETC;
  LMedium: STGMEDIUM;
begin
  if (dwEffect = DROPEFFECT_NONE) then
    Result := E_INVALIDARG
  else
  begin
    LFormat.cfFormat := CF_UNICODETEXT;
    LFormat.ptd := nil;
    LFormat.dwAspect := DVASPECT_CONTENT;
    LFormat.lindex := -1;
    LFormat.tymed := TYMED_HGLOBAL;

    if (dataObj.QueryGetData(LFormat) = S_OK) then
      Result := DragOver(grfKeyState, pt, dwEffect)
    else
    begin
      LFormat.cfFormat := CF_HDROP;
      LFormat.ptd := nil;
      LFormat.dwAspect := DVASPECT_CONTENT;
      LFormat.lindex := -1;
      LFormat.tymed := TYMED_HGLOBAL;

      if (not (eoDropFiles in FOptions) or (dataObj.QueryGetData(LFormat) <> S_OK)) then
        Result := E_UNEXPECTED
      else
      begin
        OleCheck(dataObj.GetData(LFormat, LMedium));
        if (DragQueryFile(LMedium.hGlobal, $FFFFFFFF, nil, 0) <> 1) then
          Result := E_UNEXPECTED
        else
          Result := DragOver(grfKeyState, pt, dwEffect);
      end;
    end;
  end;
end;

function TCustomBCEditor.DragLeave(): HResult;
begin
  InsertPos := InvalidCaretPos;

  Result := S_OK;
end;

function TCustomBCEditor.DragOver(grfKeyState: Longint; pt: TPoint; var dwEffect: Longint): HResult;
var
  LPosition: TBCEditorLinesPosition;
  LScreen: TPoint;
begin
  if (ReadOnly
    or (pt.X <= FLeftMarginWidth)) then
  begin
    InsertPos := InvalidCaretPos;
    dwEffect := DROPEFFECT_NONE;
  end
  else
  begin
    LScreen := ScreenToClient(pt);
    LPosition := ClientToLines(LScreen.X, LScreen.Y);
    if (FLines.SelArea.Containts(LPosition)) then
    begin
      InsertPos := InvalidCaretPos;
      dwEffect := DROPEFFECT_NONE;
    end
    else
    begin
      InsertPos := Point(LPosition);
      if (grfKeyState and MK_CONTROL <> 0) then
        dwEffect := DROPEFFECT_COPY
      else if (grfKeyState and MK_SHIFT <> 0) then
        dwEffect := DROPEFFECT_MOVE
      else if (esDragging in FState) then
        dwEffect := DROPEFFECT_MOVE
      else
        dwEffect := DROPEFFECT_COPY;
    end;
  end;
  Result := S_OK;
end;

procedure TCustomBCEditor.DragOver(ASource: TObject; X, Y: Integer;
  AState: TDragState; var AAccept: Boolean);
begin
  if (ReadOnly) then
    AAccept := False
  else
    inherited;
end;

function TCustomBCEditor.Drop(const dataObj: IDataObject; grfKeyState: Longint; pt: TPoint;
  var dwEffect: Longint): HResult;
var
  LFilename: string;
  LFormat: FORMATETC;
  LLen: UINT;
  LMedium: STGMEDIUM;
  LNewPosition: TBCEditorLinesPosition;
  LOldPosition: TBCEditorLinesPosition;
  LText: string;
begin
  if (dwEffect and (DROPEFFECT_COPY or DROPEFFECT_MOVE) = 0) then
    Result := E_INVALIDARG
  else
  begin
    LText := '';

    if (grfKeyState and MK_CONTROL <> 0) then
      dwEffect := DROPEFFECT_COPY
    else if (grfKeyState and MK_SHIFT <> 0) then
      dwEffect := DROPEFFECT_MOVE
    else if (esDragging in FState) then
      dwEffect := DROPEFFECT_MOVE
    else
      dwEffect := DROPEFFECT_COPY;

    LFormat.cfFormat := CF_UNICODETEXT;
    LFormat.ptd := nil;
    LFormat.dwAspect := DVASPECT_CONTENT;
    LFormat.lindex := -1;
    LFormat.tymed := TYMED_HGLOBAL;
    Result := dataObj.QueryGetData(LFormat);
    if (Result = S_OK) then
    begin
      OleCheck(dataObj.GetData(LFormat, LMedium));
      SetString(LText, PChar(GlobalLock(LMedium.hGlobal)), GlobalSize(LMedium.hGlobal) div SizeOf(LText[1]));
      FLines.CaretPosition := LinesPosition(InsertPos);
      SelText := LText;
    end
    else
    begin
      LFormat.cfFormat := CF_HDROP;
      LFormat.ptd := nil;
      LFormat.dwAspect := DVASPECT_CONTENT;
      LFormat.lindex := -1;
      LFormat.tymed := TYMED_HGLOBAL;
      Result := dataObj.QueryGetData(LFormat);
      if (Result <> S_OK) then
        Result := E_UNEXPECTED
      else
      begin
        OleCheck(dataObj.GetData(LFormat, LMedium));
        LLen := DragQueryFile(LMedium.hGlobal, 0, nil, 0);
        SetLength(LFilename, LLen + 1);
        Assert(DragQueryFile(LMedium.hGlobal, 0, PChar(LFilename), LLen + 1) = LLen);
        SetLength(LFilename, LLen);
        LOldPosition := LinesPosition(InsertPos);
        LNewPosition := FLines.InsertFile(LinesPosition(InsertPos), LFilename);
        SetCaretAndSelection(LNewPosition, LinesArea(LOldPosition, LNewPosition));
        InsertPos := InvalidCaretPos;
      end;
    end;

    if (not Assigned(LMedium.unkForRelease)) then
      ReleaseStgMedium(LMedium)
    else
      IUnknown(LMedium.unkForRelease)._Release();

    if (Result = S_OK) then
      Result := DragLeave();
  end;
end;

procedure TCustomBCEditor.EndUndoBlock;
begin
  FLines.EndUpdate();
end;

procedure TCustomBCEditor.EndUpdate();
begin
  Dec(FUpdateCount);
  if (FUpdateCount = 0) then SetUpdateState(False);
end;

procedure TCustomBCEditor.ExpandCodeFoldingLevel(const AFirstLevel: Integer; const ALastLevel: Integer);
var
  LFirstLine: Integer;
  LLastLine: Integer;
  LLevel: Integer;
  LLine: Integer;
  LRange: TBCEditorCodeFolding.TRanges.TRange;
  LRangeLevel: Integer;
begin
  if (SelectionAvailable) then
  begin
    LFirstLine := FLines.SelArea.BeginPosition.Line;
    LLastLine := FLines.SelArea.EndPosition.Line;
  end
  else
  begin
    LFirstLine := 0;
    LLastLine := FLines.Count - 1;
  end;

  BeginUpdate();

  LLevel := -1;
  for LLine := LFirstLine to LLastLine do
  begin
    LRange := TBCEditorCodeFolding.TRanges.TRange(FLines.Items[LLine].CodeFolding.BeginRange);
    if (Assigned(LRange)) then
    begin
      if LLevel = -1 then
        LLevel := LRange.FoldRangeLevel;
      LRangeLevel := LRange.FoldRangeLevel - LLevel;
      if ((AFirstLevel <= LRangeLevel) and (LRangeLevel <= ALastLevel)
        and LRange.Collapsed) then
        ExpandCodeFoldingRange(LRange);
    end;
  end;

  EndUpdate();
end;

function TCustomBCEditor.ExpandCodeFoldingLines(const AFirstLine: Integer = -1; const ALastLine: Integer = -1): Integer;
var
  LFirstLine: Integer;
  LLastLine: Integer;
  LLine: Integer;
  LRange: TBCEditorCodeFolding.TRanges.TRange;
begin
  if (AFirstLine >= 0) then
    LFirstLine := AFirstLine
  else
    LFirstLine := 0;
  if (ALastLine >= -1) then
    LLastLine := ALastLine
  else if (AFirstLine >= 0) then
    LLastLine := AFirstLine
  else
    LLastLine := FLines.Count - 1;

  Result := 0;
  for LLine := LFirstLine to LLastLine do
  begin
    LRange := TBCEditorCodeFolding.TRanges.TRange(FLines.Items[LLine].CodeFolding.BeginRange);
    if (Assigned(LRange) and LRange.Collapsed) then
    begin
      ExpandCodeFoldingRange(LRange);
      Inc(Result);
    end;
  end;
end;

procedure TCustomBCEditor.ExpandCodeFoldingRange(const ARange: TBCEditorCodeFolding.TRanges.TRange);
var
  LLine: Integer;
begin
  if (ARange.Collapsed) then
  begin
    ARange.Collapsed := False;
    ARange.SetParentCollapsedOfSubCodeFoldingRanges(False, ARange.FoldRangeLevel);

    for LLine := ARange.BeginLine + 1 to ARange.EndLine do
      InsertLineIntoRows(LLine, False);

    Invalidate();
  end;
end;

function TCustomBCEditor.ExecuteAction(Action: TBasicAction): Boolean;
begin
  Result := True;

  if (Action is TEditCut) then
    ExecuteCommand(ecCut, #0, nil)
  else if (Action is TEditCopy) then
    ExecuteCommand(ecCopy, #0, nil)
  else if (Action is TEditPaste) then
    ExecuteCommand(ecPaste, #0, nil)
  else if (Action is TEditDelete) then
    ExecuteCommand(ecBackspace, #0, nil)
  else if (Action is TEditSelectAll) then
    ExecuteCommand(ecSelectAll, #0, nil)
  else if (Action is TEditUndo) then
    ExecuteCommand(ecUndo, #0, nil)
  else if (Action is TSearchFindFirst) then
    DoSearchFind(True, TSearchFindFirst(Action))
  else if (Action is TSearchFind) then
    DoSearchFind(Search.Pattern = '', TSearchFind(Action))
  else if (Action is TSearchReplace) then
    DoSearchReplace(TSearchReplace(Action))
  else if (Action is TSearchFindNext) then
    ExecuteCommand(ecSearchNext, #0, nil)
  else
    Result := inherited;
end;

procedure TCustomBCEditor.ExecuteCommand(ACommand: TBCEditorCommand; AChar: Char; AData: Pointer);
begin
  case ACommand of
    ecLeft, ecSelectionLeft:
      if not FSyncEdit.Active or FSyncEdit.Active and (FLines.CaretPosition.Char > FSyncEdit.EditArea.BeginPosition.Char) then
        MoveCaretHorizontally(-1, ACommand = ecSelectionLeft);
    ecRight, ecSelectionRight:
      if not FSyncEdit.Active or FSyncEdit.Active and (FLines.CaretPosition.Char < FSyncEdit.EditArea.EndPosition.Char) then
        MoveCaretHorizontally(1, ACommand = ecSelectionRight);
    ecLineBegin, ecSelectionLineBegin:
      DoHomeKey(ACommand = ecSelectionLineBegin);
    ecLineEnd, ecSelectionLineEnd:
      DoEndKey(ACommand = ecSelectionLineEnd);
    ecUp, ecSelectionUp:
      MoveCaretVertically(-1, ACommand = ecSelectionUp);
    ecDown, ecSelectionDown:
      MoveCaretVertically(1, ACommand = ecSelectionDown);
    ecPageUp, ecSelectionPageUp, ecPageDown, ecSelectionPageDown:
      DoPageUpOrDown(ACommand);
    ecPageTop, ecSelectionPageTop, ecPageBottom, ecSelectionPageBottom:
      DoPageTopOrBottom(ACommand);
    ecEditorTop, ecSelectionEditorTop:
      DoEditorTop(ACommand);
    ecEditorBottom, ecSelectionEditorBottom:
      DoEditorBottom(ACommand);
    ecGotoXY, ecSelectionGotoXY:
      if Assigned(AData) then
        MoveCaretAndSelection(FLines.CaretPosition, TBCEditorLinesPosition(AData^), ACommand = ecSelectionGotoXY);
    ecToggleBookmark:
      DoToggleBookmark(FLines.CaretPosition);
    ecGotoNextBookmark:
      GotoNextBookmark;
    ecGotoPreviousBookmark:
      GotoPreviousBookmark;
    ecGotoBookmark1 .. ecGotoBookmark9:
      if FLeftMargin.Bookmarks.ShortCuts then
        GotoBookmark(ACommand - ecGotoBookmark1);
    ecSetBookmark1 .. ecSetBookmark9:
      if FLeftMargin.Bookmarks.ShortCuts then
        DoSetBookmark(ACommand, AData);
    ecWordLeft, ecSelectionWordLeft:
      DoWordLeft(ACommand);
    ecWordRight, ecSelectionWordRight:
      DoWordRight(ACommand);
    ecSelectionWord:
      SetSelectedWord;
    ecSelectAll:
      SelectAll;
    ecBackspace:
      if not ReadOnly then
        DoBackspace;
    ecDeleteChar:
      if not ReadOnly then
        DeleteChar;
    ecDeleteWord, ecDeleteEndOfLine:
      if not ReadOnly then
        DeleteWordOrEndOfLine(ACommand);
    ecDeleteLastWord, ecDeleteBeginningOfLine:
      if not ReadOnly then
        DeleteLastWordOrBeginningOfLine(ACommand);
    ecDeleteLine:
      if not ReadOnly then
        DeleteLine;
    ecSearchFindFirst:
      DoSearchFind(True, nil);
    ecSearchFind:
      DoSearchFind(Search.Pattern = '', nil);
    ecSearchReplace:
      DoSearchReplace(nil);
    ecSearchNext:
      FindNext;
    ecSearchPrevious:
      FindPrevious;
    ecClear:
      if not ReadOnly then
        Clear;
    ecInsertLine:
      if not ReadOnly then
        InsertLine;
    ecLineBreak:
      if not ReadOnly then
        DoLineBreak;
    ecTab:
      if not ReadOnly then
        DoTabKey;
    ecShiftTab:
      if not ReadOnly then
        DoShiftTabKey;
    ecChar:
      if not ReadOnly and (AChar >= BCEDITOR_SPACE_CHAR) and (AChar <> BCEDITOR_CTRL_BACKSPACE) then
        DoChar(AChar);
    ecUpperCase, ecLowerCase:
      if not ReadOnly then
        DoToggleSelectedCase(ACommand);
    ecUndo:
      if not ReadOnly then
        Undo();
    ecRedo:
      if not ReadOnly then
        Redo();
    ecCut:
      if (not ReadOnly and SelectionAvailable) then
        DoCutToClipboard;
    ecCopy:
      CopyToClipboard;
    ecPaste:
      if not ReadOnly then
        DoPasteFromClipboard();
    ecScrollUp, ecScrollDown:
      DoScroll(ACommand);
    ecScrollLeft:
      begin
        HorzTextPos := HorzTextPos - 1;
        Update;
      end;
    ecScrollRight:
      begin
        HorzTextPos := HorzTextPos + 1;
        Update;
      end;
    ecInsertMode:
      TextEntryMode := temInsert;
    ecOverwriteMode:
      TextEntryMode := temOverwrite;
    ecToggleMode:
      if FTextEntryMode = temInsert then
        TextEntryMode := temOverwrite
      else
        TextEntryMode := temInsert;
    ecBlockIndent,
    ecBlockUnindent:
      if not ReadOnly then
        DoBlockIndent(ACommand);
    ecContextHelp:
      if Assigned(FOnContextHelp) then
        FOnContextHelp(Self, WordAt[CaretPos]);
    ecBlockComment:
      if not ReadOnly then
        DoBlockComment;
    ecLineComment:
      if not ReadOnly then
        DoLineComment;
    ecImeStr:
      if not ReadOnly then
        DoImeStr(AData);
    ecCompletionProposal:
      DoCompletionProposal();
  end;
end;

procedure TCustomBCEditor.ExportToHTML(const AFileName: string; const ACharSet: string = '';
  AEncoding: TEncoding = nil);
var
  LFileStream: TFileStream;
begin
  LFileStream := TFileStream.Create(AFileName, fmCreate);
  try
    ExportToHTML(LFileStream, ACharSet, AEncoding);
  finally
    LFileStream.Free;
  end;
end;

procedure TCustomBCEditor.ExportToHTML(AStream: TStream; const ACharSet: string = '';
  AEncoding: TEncoding = nil);
begin
  with TBCEditorExportHTML.Create(FLines, FHighlighter, Font, FTabs.Width, ACharSet) do
  try
    SaveToStream(AStream, AEncoding);
  finally
    Free;
  end;
end;

procedure TCustomBCEditor.FindAll;
begin
  if (FCaret.MultiEdit.Enabled) then
    SetFocus();
end;

function TCustomBCEditor.FindFirst(): Boolean;
var
  LArea: TBCEditorLinesArea;
  LPosition: TBCEditorLinesPosition;
  LSearchResult: TBCEditorLinesArea;
begin
  if (SelectionAvailable and (FSearch.InSelection.Active)) then
    LArea := FLines.SelArea
  else
    LArea := FLines.Area;

  Include(FState, esFindind);
  try
    if (FLines.Count = 0) then
      LPosition := FLines.BOFPosition
    else
    begin
      LPosition := FLines.CaretPosition;
      LPosition.Line := Min(LPosition.Line, FLines.Count - 1);
      LPosition.Char := Min(LPosition.Char, Length(FLines[LPosition.Line]));
    end;

    Result := DoSearch(LArea, LPosition);
    if (Result) then
    begin
      if (soEntireScope in FSearch.Options) then
        if (soBackwards in FSearch.Options) then
          Result := DoSearchPrevious(FLines.EOFPosition, LSearchResult)
        else
          Result := DoSearchNext(FLines.BOFPosition, LSearchResult)
      else
        if (soBackwards in FSearch.Options) then
          Result := DoSearchPrevious(FLines.CaretPosition, LSearchResult, soWrapAround in Search.Options)
        else
          Result := DoSearchNext(FLines.CaretPosition, LSearchResult, soWrapAround in Search.Options);

      if (Result) then
        if (soBackwards in Search.Options) then
          SetCaretAndSelection(LSearchResult.BeginPosition, LSearchResult)
        else
          SetCaretAndSelection(LSearchResult.EndPosition, LSearchResult);
    end;
  finally
    Exclude(FState, esFindind);
  end;
end;

function TCustomBCEditor.FindHookedCommandEvent(const AHookedCommandEvent: TBCEditorHookedCommandEvent): Integer;
var
  LHookedCommandHandler: TBCEditorHookedCommandHandler;
begin
  Result := GetHookedCommandHandlersCount - 1;
  while Result >= 0 do
  begin
    LHookedCommandHandler := TBCEditorHookedCommandHandler(FHookedCommandHandlers[Result]);
    if LHookedCommandHandler.Equals(AHookedCommandEvent) then
      Break;
    Dec(Result);
  end;
end;

function TCustomBCEditor.FindNext(const AHandleNotFound: Boolean = True): Boolean;
var
  LSearchResult: TBCEditorLinesArea;
begin
  if (FSearchResults.Count = 0) then
    Result := FindFirst()
  else
  begin
    Include(FState, esFindind);
    BeginUpdate();
    try
      if (soBackwards in Search.Options) then
        Result := DoSearchPrevious(FLines.CaretPosition, LSearchResult, soWrapAround in FSearch.Options)
      else      
        Result := DoSearchNext(FLines.CaretPosition, LSearchResult, soWrapAround in FSearch.Options);
        
      if (Result) then
      begin
        if (LSearchResult.BeginPosition.Line >= TopRow + VisibleRows - 1) then
          GotoLineAndCenter(LSearchResult.EndPosition.Line, LSearchResult.EndPosition.Char);

        if (soBackwards in Search.Options) then
          SetCaretAndSelection(LSearchResult.BeginPosition, LSearchResult)
        else
          SetCaretAndSelection(LSearchResult.EndPosition, LSearchResult);
      end
      else if (AHandleNotFound and (Search.Pattern <> '')) then
        DoSearchStringNotFoundDialog();
    finally
      EndUpdate();
      Exclude(FState, esFindind);
    end;
  end;
end;

function TCustomBCEditor.FindPrevious(const AHandleNotFound: Boolean = True): Boolean;
var
  LSearchResult: TBCEditorLinesArea;
begin
  if (FSearchResults.Count = 0) then
    Result := FindFirst()
  else
  begin
    Include(FState, esFindind);
    BeginUpdate();
    try
      if (soBackwards in Search.Options) then
        Result := DoSearchNext(FLines.CaretPosition, LSearchResult, soWrapAround in FSearch.Options)
      else
        Result := DoSearchPrevious(FLines.CaretPosition, LSearchResult, soWrapAround in FSearch.Options);

      if (Result) then
      begin
        if (LSearchResult.BeginPosition.Line >= TopRow + VisibleRows - 1) then
          GotoLineAndCenter(LSearchResult.EndPosition.Line, LSearchResult.EndPosition.Char);

        if (soBackwards in Search.Options) then
          SetCaretAndSelection(LSearchResult.BeginPosition, LSearchResult)
        else
          SetCaretAndSelection(LSearchResult.EndPosition, LSearchResult);
      end
      else if (AHandleNotFound and (Search.Pattern <> '')) then
        DoSearchStringNotFoundDialog();
    finally
      Exclude(FState, esFindind);
    end;
  end;
end;

procedure TCustomBCEditor.FindWords(const AWord: string; AList: TList; ACaseSensitive: Boolean; AWholeWordsOnly: Boolean);

  function AreCharsSame(APChar1, APChar2: PChar): Boolean;
  begin
    if ACaseSensitive then
      Result := APChar1^ = APChar2^
    else
      Result := CaseUpper(APChar1^) = CaseUpper(APChar2^)
  end;

  function IsWholeWord(FirstChar, LastChar: PChar): Boolean;
  begin
    Result := IsWordBreakChar(FirstChar^) and IsWordBreakChar(LastChar^);
  end;

var
  LEndPosWord: PChar;
  LFirstChar: Integer;
  LFirstLine: Integer;
  LLastChar: Integer;
  LLastLine: Integer;
  LLine: Integer;
  LLineBeginPos: PChar;
  LLineEndPos: PChar;
  LLinePos: PChar;
  LPBookmarkText: PChar;
  LPosWord: PChar;
  LPTextPosition: PBCEditorTextPosition;
begin
  if FSearch.InSelection.Active then
  begin
    LFirstLine := FSearch.InSelection.SelectionBeginPosition.Line;
    LFirstChar := FSearch.InSelection.SelectionBeginPosition.Char - 1;
    LLastLine := FSearch.InSelection.SelectionEndPosition.Line;
    LLastChar := FSearch.InSelection.SelectionEndPosition.Char;
  end
  else
  begin
    LFirstLine := 0;
    LFirstChar := -1;
    LLastLine := FLines.Count - 1;
    LLastChar := -1;
  end;

  for LLine := LFirstLine to LLastLine do
  begin
    if (FLines.Items[LLine].Text <> '') then
    begin
      LLinePos := @FLines.Items[LLine].Text[1];
      LLineBeginPos := LLinePos;
      LLineEndPos := @FLines.Items[LLine].Text[Length(FLines.Items[LLine].Text)];
      if (LLine = LFirstLine) and (LFirstChar >= 0) then
        Inc(LLinePos, LFirstChar);
      while (LLinePos <= LLineEndPos) do
      begin
        if (AreCharsSame(LLinePos, PChar(AWord))) then { If the first character is a match }
        begin
          LPosWord := @AWord[1];
          LEndPosWord := @AWord[Length(AWord)];
          LPBookmarkText := LLinePos;
          { Check if the keyword found }
          while ((LLinePos <= LLineEndPos) and (LPosWord^ <= LEndPosWord) and AreCharsSame(LLinePos, LPosWord)) do
          begin
            Inc(LLinePos);
            Inc(LPosWord);
          end;
          if ((LLinePos > LEndPosWord)
            and (not AWholeWordsOnly or AWholeWordsOnly and IsWholeWord(LPBookmarkText - 1, LLinePos))) then
          begin
            Dec(LLinePos);
            New(LPTextPosition);
            LPTextPosition^.Char := LPBookmarkText - LLineBeginPos;
            LPTextPosition^.Line := LLine;
            AList.Add(LPTextPosition)
          end
          else
            LLinePos := LPBookmarkText; { Not found, return pointer back }
        end;

        Inc(LLinePos);

        if ((LLine = LLastLine) and (LLastChar >= 0)) then
          if (LLineBeginPos - LLinePos > LLastChar + 1) then
            Break;
      end;
    end;
  end;
end;

procedure TCustomBCEditor.FontChanged(ASender: TObject);
begin
  FState := FState + [esFontChanged, esScrolled];
  Invalidate();
end;

function TCustomBCEditor.GetBookmark(const AIndex: Integer; var ALinesPosition: TBCEditorLinesPosition): Boolean;
var
  LBookmark: TBCEditorMark;
begin
  Result := False;
  LBookmark := FBookmarkList.Find(AIndex);
  if Assigned(LBookmark) then
  begin
    ALinesPosition.Char := LBookmark.Char;
    ALinesPosition.Line := LBookmark.Line;
    Result := True;
  end;
end;

function TCustomBCEditor.GetCanPaste: Boolean;
begin
  Result := not ReadOnly and (IsClipboardFormatAvailable(CF_TEXT) or IsClipboardFormatAvailable(CF_UNICODETEXT));
end;

function TCustomBCEditor.GetCanRedo: Boolean;
begin
  Result := not ReadOnly and FLines.CanRedo;
end;

function TCustomBCEditor.GetCanUndo: Boolean;
begin
  Result := not ReadOnly and FLines.CanUndo;
end;

function TCustomBCEditor.GetCaretPos(): TPoint;
begin
  Result := Point(FLines.CaretPosition);
end;

function TCustomBCEditor.GetCharAt(APos: TPoint): Char;
begin
  Result := FLines.Char[LinesPosition(APos)];
end;

function TCustomBCEditor.GetHookedCommandHandlersCount: Integer;
begin
  if Assigned(FHookedCommandHandlers) then
    Result := FHookedCommandHandlers.Count
  else
    Result := 0;
end;

function TCustomBCEditor.GetLeadingExpandedLength(const AStr: string; const ABorder: Integer = 0): Integer;
var
  LChar: PChar;
  LLength: Integer;
begin
  Result := 0;
  LChar := PChar(AStr);
  if ABorder > 0 then
    LLength := Min(PInteger(LChar - 2)^, ABorder)
  else
    LLength := PInteger(LChar - 2)^;
  while LLength > 0 do
  begin
    if LChar^ = BCEDITOR_TAB_CHAR then
      Inc(Result, FTabs.Width - (Result mod FTabs.Width))
    else
    if (CharInSet(LChar^, [BCEDITOR_NONE_CHAR, BCEDITOR_SPACE_CHAR])) then
      Inc(Result)
    else
      Exit;
    Inc(LChar);
    Dec(LLength);
  end;
end;

function TCustomBCEditor.GetLineIndentLevel(const ALine: Integer): Integer;
var
  LLineEndPos: PChar;
  LLinePos: PChar;
begin
  Assert((0 <= ALine) and (ALine < FLines.Count));

  Result := 0;
  if (FLines.Items[ALine].Text <> '') then
  begin
    LLinePos := @FLines.Items[ALine].Text[1];
    LLineEndPos := @FLines.Items[ALine].Text[Length(FLines.Items[ALine].Text)];
    while ((LLinePos <= LLineEndPos) and CharInSet(LLinePos^, [BCEDITOR_NONE_CHAR, BCEDITOR_TAB_CHAR, BCEDITOR_SPACE_CHAR])) do
    begin
      if (LLinePos^ <> BCEDITOR_TAB_CHAR) then
        Inc(Result)
      else
        Inc(Result, FTabs.Width - Result mod FTabs.Width);
      Inc(LLinePos);
    end;
  end;
end;

function TCustomBCEditor.GetModified(): Boolean;
begin
  Result := FLines.Modified;
end;

function TCustomBCEditor.GetMouseMoveScrollCursorIndex: Integer;
var
  LBottomY: Integer;
  LCursorPoint: TPoint;
  LLeftX: Integer;
  LRightX: Integer;
  LTopY: Integer;
begin
  Result := scNone;

  GetCursorPos(LCursorPoint);
  LCursorPoint := ScreenToClient(LCursorPoint);

  LLeftX := FMouseMoveScrollingPoint.X - FScroll.Indicator.Width;
  LRightX := FMouseMoveScrollingPoint.X + 4;
  LTopY := FMouseMoveScrollingPoint.Y - FScroll.Indicator.Height;
  LBottomY := FMouseMoveScrollingPoint.Y + 4;

  if LCursorPoint.Y < LTopY then
  begin
    if LCursorPoint.X < LLeftX then
      Exit(scNorthWest)
    else
    if (LCursorPoint.X >= LLeftX) and (LCursorPoint.X <= LRightX) then
      Exit(scNorth)
    else
      Exit(scNorthEast)
  end;

  if LCursorPoint.Y > LBottomY then
  begin
    if LCursorPoint.X < LLeftX then
      Exit(scSouthWest)
    else
    if (LCursorPoint.X >= LLeftX) and (LCursorPoint.X <= LRightX) then
      Exit(scSouth)
    else
      Exit(scSouthEast)
  end;

  if LCursorPoint.X < LLeftX then
    Exit(scWest);

  if LCursorPoint.X > LRightX then
    Exit(scEast);
end;

function TCustomBCEditor.GetMouseMoveScrollCursors(const AIndex: Integer): HCursor;
begin
  Result := 0;
  if (AIndex >= Low(FMouseMoveScrollCursors)) and (AIndex <= High(FMouseMoveScrollCursors)) then
    Result := FMouseMoveScrollCursors[AIndex];
end;

function TCustomBCEditor.GetReadOnly: Boolean;
begin
  Result := FLines.ReadOnly;
end;

function TCustomBCEditor.GetSearchResultCount: Integer;
begin
  Result := FSearchResults.Count;
end;

function TCustomBCEditor.GetSelectionAvailable(): Boolean;
begin
  Result := not FLines.SelArea.IsEmpty();
end;

function TCustomBCEditor.GetSelectionBeginPosition: TBCEditorLinesPosition;
begin
  Result := FLines.SelArea.BeginPosition;
end;

function TCustomBCEditor.GetSelectionEndPosition: TBCEditorLinesPosition;
begin
  Result := FLines.SelArea.EndPosition;
end;

function TCustomBCEditor.GetSelLength(): Integer;
begin
  if (FLines.SelArea.IsEmpty()) then
    Result := 0
  else
    Result := FLines.CharIndexOf(Max(FLines.SelArea.BeginPosition, FLines.SelArea.EndPosition))
      - FLines.CharIndexOf(Min(FLines.SelArea.BeginPosition, FLines.SelArea.EndPosition));
end;

function TCustomBCEditor.GetSelStart(): Integer;
begin
  Result := FLines.CharIndexOf(Min(FLines.SelArea.BeginPosition, FLines.SelArea.EndPosition));
end;

function TCustomBCEditor.GetSelText(): string;
begin
  Result := FLines.TextIn[LinesArea(Min(FLines.SelArea.BeginPosition, FLines.SelArea.EndPosition), Max(FLines.SelArea.BeginPosition, FLines.SelArea.EndPosition))];
end;

function TCustomBCEditor.GetText(): string;
begin
  Result := FLines.Text;
end;

function TCustomBCEditor.GetTextBetween(ABeginPosition, AEndPosition: TBCEditorLinesPosition): string;
begin
  Result := FLines.TextIn[LinesArea(ABeginPosition, AEndPosition)];
end;

function TCustomBCEditor.GetUndoOptions(): TBCEditorUndoOptions;
begin
  Result := [];
  if (loUndoGrouped in FLines.Options) then
    Result := Result + [uoGroupUndo];
  if (loUndoAfterLoad in FLines.Options) then
    Result := Result + [uoUndoAfterLoad];
  if (loUndoAfterSave in FLines.Options) then
    Result := Result + [uoUndoAfterSave];
end;

function TCustomBCEditor.GetVisibleArea(): TBCEditorLinesArea;
begin
  Result.BeginPosition := FRows.BORPosition[TopRow];
  Result.EndPosition := Min(FRows.BORPosition[TopRow + VisibleRows + 1], FLines.EOFPosition);
end;

function TCustomBCEditor.GetWordAt(ALinesPos: TPoint): string;
begin
  Result := GetWordAtLinesPosition(LinesPosition(ALinesPos));
end;

function TCustomBCEditor.GetWordAtLinesPosition(const ALinesPosition: TBCEditorLinesPosition): string;
var
  LBeginPosition: TBCEditorLinesPosition;
  LEndPosition: TBCEditorLinesPosition;
begin
  if ((ALinesPosition.Line >= FLines.Count)
    or (ALinesPosition.Char >= Length(FLines.Items[ALinesPosition.Line].Text))) then
    Result := ''
  else
  begin
    LEndPosition := Min(ALinesPosition, FLines.EOLPosition[ALinesPosition.Line]);
    if ((LEndPosition.Char > 0)
      and not IsWordBreakChar(FLines.Char[LEndPosition])
      and IsWordBreakChar(FLines.Char[LEndPosition])) then
      Dec(LEndPosition.Char);
    if (IsWordBreakChar(FLines.Char[LEndPosition])) then
      Result := ''
    else
    begin
      LBeginPosition := WordBegin(LEndPosition);
      LEndPosition := WordEnd(LEndPosition);
      Result := Copy(FLines.Items[LBeginPosition.Line].Text, 1 + LBeginPosition.Char, LEndPosition.Char - LBeginPosition.Char + 1);
    end;
  end;
end;

function TCustomBCEditor.GiveFeedback(dwEffect: Longint): HResult;
begin
  Result := DRAGDROP_S_USEDEFAULTCURSORS;
end;

procedure TCustomBCEditor.GotoBookmark(const AIndex: Integer);
var
  LBookmark: TBCEditorMark;
  LTextPosition: TBCEditorLinesPosition;
begin
  LBookmark := FBookmarkList.Find(AIndex);
  if Assigned(LBookmark) then
  begin
    LTextPosition.Char := LBookmark.Char - 1;
    LTextPosition.Line := LBookmark.Line;

    GotoLineAndCenter(LTextPosition.Line, LTextPosition.Char);
  end;
end;

procedure TCustomBCEditor.GotoLineAndCenter(const ALine: Integer; const AChar: Integer = 1);
var
  LIndex: Integer;
  LRange: TBCEditorCodeFolding.TRanges.TRange;
begin
  if (FCodeFolding.Visible) then
    for LIndex := 0 to FAllCodeFoldingRanges.AllCount - 1 do
    begin
      LRange := FAllCodeFoldingRanges[LIndex];
      if LRange.BeginLine >= ALine then
        Break
      else if LRange.Collapsed then
        ExpandCodeFoldingRange(LRange);
    end;

  FLines.CaretPosition := LinesPosition(AChar, ALine);
  TopRow := Max(0, FRows.CaretPosition.Row - VisibleRows div 2);
end;

procedure TCustomBCEditor.GotoNextBookmark;
var
  LIndex: Integer;
  LMark: TBCEditorMark;
begin
  for LIndex := 0 to FBookmarkList.Count - 1 do
  begin
    LMark := FBookmarkList.Items[LIndex];
    if (LMark.Line > FLines.CaretPosition.Line) or
      (LMark.Line = FLines.CaretPosition.Line) and (LMark.Char - 1 > FLines.CaretPosition.Char) then
    begin
      GotoBookmark(LMark.Index);
      Exit;
    end;
  end;
  if FBookmarkList.Count > 0 then
    GotoBookmark(FBookmarkList.Items[0].Index);
end;

procedure TCustomBCEditor.GotoPreviousBookmark;
var
  LIndex: Integer;
  LMark: TBCEditorMark;
begin
  for LIndex := FBookmarkList.Count - 1 downto 0 do
  begin
    LMark := FBookmarkList.Items[LIndex];
    if (LMark.Line < FLines.CaretPosition.Line) or
      (LMark.Line = FLines.CaretPosition.Line) and (LMark.Char - 1 < FLines.CaretPosition.Char) then
    begin
      GotoBookmark(LMark.Index);
      Exit;
    end;
  end;
  if FBookmarkList.Count > 0 then
    GotoBookmark(FBookmarkList.Items[FBookmarkList.Count - 1].Index);
end;

procedure TCustomBCEditor.HighlighterChanged(ASender: TObject);
var
  LElement: TBCEditorHighlighter.PElement;
begin
  FRows.Clear();

  LElement := FHighlighter.Colors.GetElement(BCEDITOR_ATTRIBUTE_ELEMENT_EDITOR);
  if (Assigned(LElement) and (LElement^.Foreground <> clNone)) then
    Font.Color := LElement^.Foreground
  else
    Font.Color := clWindowText;
  if (Assigned(LElement) and (LElement^.Background <> clNone)) then
    FColor := LElement^.Background
  else
    FColor := clWindow;

  Include(FState, esHighlighterChanged);
  InvalidateCaret();
  Invalidate();
end;

procedure TCustomBCEditor.HookEditorLines(ALines: TBCEditorLines; AUndo, ARedo: TBCEditorLines.TUndoList);
var
  LOldWrap: Boolean;
begin
  Assert(not Assigned(FChainedEditor));
  Assert(FLines = FOriginalLines);

  LOldWrap := WordWrap.Enabled;
  UpdateWordWrap(False);

  if Assigned(FChainedEditor) then
    RemoveChainedEditor
  else
  if FLines <> FOriginalLines then
    UnhookEditorLines;

  with ALines do
  begin
    FOnChainLinesCleared := OnCleared; OnCleared := ChainLinesCleared;
    FOnChainLinesDeleted := OnDeleted; OnDeleted := ChainLinesDeleted;
    FOnChainLinesInserted := OnInserted; OnInserted := ChainLinesInserted;
    FOnChainLinesUpdated := OnUpdated; OnUpdated := ChainLinesUpdated;
  end;

  FLines := ALines;
  LinesHookChanged;

  UpdateWordWrap(LOldWrap);
end;

procedure TCustomBCEditor.InitCodeFolding();
begin
  ClearCodeFolding();

  if (HandleAllocated) then
  begin
    Include(FState, esCodeFoldingInvalid);
    KillTimer(Handle, tiCodeFolding);
    SetTimer(Handle, tiCodeFolding, FCodeFolding.DelayInterval, nil);
  end;
end;

procedure TCustomBCEditor.InsertLine();
begin
  if (SelectionAvailable) then
    SelText := FLines.LineBreak
  else
    FLines.InsertText(FLines.CaretPosition, FLines.LineBreak);
end;

procedure TCustomBCEditor.InsertLineIntoRows(const ALine: Integer; const ANewLine: Boolean);
var
  LCodeFolding: Integer;
  LInsertedRows: Integer;
  LLine: Integer;
  LRange: TBCEditorCodeFolding.TRanges.TRange;
  LRow: Integer;
begin
  if (FRows.Count > 0) then
  begin
    for LCodeFolding := 0 to FAllCodeFoldingRanges.AllCount - 1 do
    begin
      LRange := FAllCodeFoldingRanges[LCodeFolding];
      if (Assigned(LRange)
        and LRange.Collapsed
        and (LRange.BeginLine < ALine) and (ALine <= LRange.EndLine)) then
        Exit;
    end;

    LLine := ALine + 1;
    while ((LLine < FLines.Count) and (FLines.Items[LLine].FirstRow < 0)) do
      Inc(LLine);
    if (LLine < FLines.Count) then
      LRow := FLines.Items[LLine].FirstRow
    else
      LRow := FRows.Count;

    LInsertedRows := InsertLineIntoRows(ALine, LRow);

    if (not (esCaretInvalid in FState)) then
    begin
      Inc(FCaretPos.Y, LInsertedRows * LineHeight);
      UpdateCaret();
    end;

    if (ANewLine) then
      for LRow := LRow + LInsertedRows to FRows.Count - 1 do
        FRows.List[LRow].Line := FRows.List[LRow].Line + 1;
  end;
end;

function TCustomBCEditor.InsertLineIntoRows(const ALine: Integer; const ARow: Integer): Integer;
var
  LBeginRange: TBCEditorHighlighter.TRange;
  LChar: Integer;
  LColumn: Integer;
  LFlags: TRow.TFlags;
  LLine: Integer;
  LMaxRowWidth: Integer;
  LRow: Integer;
  LRowLength: Integer;
  LRowWidth: Integer;
  LToken: TBCEditorHighlighter.TFind;
  LTokenBeginPos: PChar;
  LTokenEndPos: PChar;
  LTokenPos: PChar;
  LTokenPrevPos: PChar;
  LTokenRowBeginPos: PChar;
  LTokenRowText: string;
  LTokenRowWidth: Integer;
  LTokenWidth: Integer;
begin
  ClearMatchingPair();

  FPaintHelper.BeginDrawing(Canvas);
  try
    if (not WordWrap.Enabled) then
    begin
      LColumn := 0;
      LRowWidth := 0;
      if (FHighlighter.FindFirstToken(FLines.Items[ALine].BeginRange, FLines.Items[ALine].Text, LToken)) then
        repeat
          Inc(LColumn, ComputeTextColumns(LToken.Text, LToken.Length, LColumn));
          Inc(LRowWidth, TokenWidth(LToken.Text, LToken.Length, LColumn, LToken));
        until (not FHighlighter.FindNextToken(LToken));

      FRows.Insert(ARow, [rfFirstRowOfLine, rfLastRowOfLine], ALine, 0,
        Length(FLines.Items[ALine].Text), LColumn, LRowWidth, FLines.Items[ALine].BeginRange);
      Result := 1;
    end
    else
    begin
      LRow := ARow;
      LFlags := [rfFirstRowOfLine];
      LMaxRowWidth := FTextWidth;
      LRowWidth := 0;
      LRowLength := 0;
      LColumn := 0;
      LChar := 0;
      LBeginRange := FLines.Items[ALine].BeginRange;
      if (FHighlighter.FindFirstToken(FLines.Items[ALine].BeginRange, FLines.Items[ALine].Text, LToken)) then
        repeat
          LTokenWidth := TokenWidth(LToken.Text, LToken.Length, LColumn, LToken);

          if (LRowWidth + LTokenWidth <= LMaxRowWidth) then
          begin
            { no row break in token }
            Inc(LRowLength, LToken.Length);
            Inc(LRowWidth, LTokenWidth);
            Inc(LColumn, ComputeTextColumns(LToken.Text, LToken.Length, LColumn));
          end
          else if (LRowLength > 0) then
          begin
            { row break before token }
            FRows.Insert(LRow, LFlags, ALine, LChar, LRowLength, LColumn, LRowWidth, LBeginRange);
            Exclude(LFlags, rfFirstRowOfLine);
            Inc(LChar, LRowLength);
            Inc(LRow);

            LBeginRange := LToken.Range;
            LRowLength := LToken.Length;
            LRowWidth := LTokenWidth;
            LColumn := ComputeTextColumns(LToken.Text, LToken.Length, LColumn);
          end
          else
          begin
            { row break inside token }
            LTokenBeginPos := LToken.Text;
            LTokenPos := LTokenBeginPos;
            LTokenEndPos := @LTokenPos[LToken.Length];

            repeat
              LTokenRowBeginPos := LTokenPos;

              Inc(LTokenPos);

              repeat
                LTokenPrevPos := LTokenPos;

                LTokenRowWidth := TokenWidth(LToken.Text, LTokenPos - LTokenRowBeginPos, LColumn, LToken);

                if (LTokenRowWidth < LMaxRowWidth) then
                  repeat
                    Inc(LTokenPos);
                  until ((LTokenPos > LTokenEndPos)
                    or (Char((LTokenPos - 1)^).GetUnicodeCategory() <> TUnicodeCategory.ucNonSpacingMark) or IsCombiningDiacriticalMark((LTokenPos - 1)^)
                      and not (Char(LTokenPos^).GetUnicodeCategory in [TUnicodeCategory.ucCombiningMark, TUnicodeCategory.ucNonSpacingMark]));
              until ((LTokenPos > LTokenEndPos) or (LTokenRowWidth >= LMaxRowWidth));

              if (LTokenRowWidth >= LMaxRowWidth) then
              begin
                LTokenPos := LTokenPrevPos;

                LRowLength := LTokenPos - LTokenRowBeginPos - 1;
                FRows.Insert(LRow, LFlags, ALine, LChar, LRowLength, LColumn, LTokenRowWidth, LBeginRange);
                Exclude(LFlags, rfFirstRowOfLine);
                Inc(LChar, LRowLength);
                Inc(LRow);

                LBeginRange := LToken.Range;
                LRowLength := 0;
                LRowWidth := 0;
                LColumn := 0;
              end
              else
              begin
                LRowLength := LTokenPos - LTokenRowBeginPos;
                LRowWidth := LTokenRowWidth;
                SetString(LTokenRowText, PChar(@LToken.Text[LTokenRowBeginPos - LTokenBeginPos]), LRowLength);
                LColumn := ComputeTextColumns(PChar(LTokenRowText), Length(LTokenRowText), LColumn);
              end;
            until ((LTokenPos > LTokenEndPos) or (LTokenRowWidth < LMaxRowWidth));
          end;
        until (not FHighlighter.FindNextToken(LToken));

      if ((LRowLength > 0) or (FLines.Items[ALine].Text = '')) then
      begin
        FRows.Insert(LRow, LFlags + [rfLastRowOfLine], ALine, LChar, LRowLength, LColumn, LRowWidth, LBeginRange);
        Inc(LRow);
      end;

      Result := LRow - ARow;
    end;

    FLines.SetFirstRow(ALine, ARow);
    for LLine := ALine + 1 to FLines.Count - 1 do
      if (FLines.Items[LLine].FirstRow >= 0) then
        FLines.SetFirstRow(LLine, FLines.Items[LLine].FirstRow + Result);
  finally
    FPaintHelper.EndDrawing();
  end;

  if (not (esBuildingRows in FState)) then
    if (UpdateCount > 0) then
      Include(FState, esRowsChanged)
    else
    begin
      UpdateScrollBars();
      Invalidate();
    end;
end;

procedure TCustomBCEditor.InvalidateCaret();
begin
  FRows.CaretPosition := InvalidRowsPosition;
  FCaretPos := InvalidCaretPos;
  SetInsertPos(InvalidCaretPos);
  FState := FState + [esCaretInvalid];

  if (UpdateCount = 0) then
    Invalidate();
end;

function TCustomBCEditor.IsCommentChar(const AChar: Char): Boolean;
begin
  Result := Assigned(FHighlighter) and CharInSet(AChar, FHighlighter.Comments.Chars);
end;

function TCustomBCEditor.IsEmptyChar(const AChar: Char): Boolean;
begin
  Result := CharInSet(AChar, BCEDITOR_EMPTY_CHARACTERS);
end;

function TCustomBCEditor.IsKeywordAtPosition(const APosition: TBCEditorLinesPosition;
  const APOpenKeyWord: PBoolean = nil): Boolean;
var
  LLineEndPos: PChar;
  LLinePos: PChar;

  function CheckToken(const AKeyword: string; const ABeginWithBreakChar: Boolean): Boolean;
  var
    LWordAtCaret: PChar;
  begin
    LWordAtCaret := LLinePos;
    if ABeginWithBreakChar then
      Dec(LWordAtCaret);

    Result := (LLineEndPos + 1 - LWordAtCaret >= Length(AKeyword))
      and (StrLIComp(PChar(AKeyword), LWordAtCaret, Length(AKeyword)) = 0);

    if (Assigned(APOpenKeyWord)) then
      APOpenKeyWord^ := Result;
  end;

var
  LFoldRegion: TBCEditorCodeFolding.TRegion;
  LFoldRegionItem: TBCEditorCodeFoldingRegionItem;
  LIndex1: Integer;
  LIndex2: Integer;
  LLineBeginPos: PChar;
begin
  Result := False;

  if (FCodeFolding.Visible
    and Assigned(FHighlighter)
    and (Length(FHighlighter.CodeFoldingRegions) > 0)
    and (FLines.CaretPosition.Line < FLines.Count)
    and (FLines.Items[FLines.CaretPosition.Line].Text <> '')
    and (FLines.CaretPosition.Char > 0)) then
  begin
    LLineBeginPos := @FLines.Items[FLines.CaretPosition.Line].Text[1];
    LLinePos := @FLines.Items[FLines.CaretPosition.Line].Text[Min(FLines.CaretPosition.Char, Length(FLines.Items[FLines.CaretPosition.Line].Text))];
    LLineEndPos := @FLines.Items[FLines.CaretPosition.Line].Text[Length(FLines.Items[FLines.CaretPosition.Line].Text)];

    if (not IsWordBreakChar(LLinePos^)) then
    begin
      while ((LLinePos >= LLineBeginPos) and not IsWordBreakChar(LLinePos^)) do
        Dec(LLinePos);
      Inc(LLinePos);
    end;

    for LIndex1 := 0 to Length(FHighlighter.CodeFoldingRegions) - 1 do
    begin
      LFoldRegion := FHighlighter.CodeFoldingRegions[LIndex1];

      for LIndex2 := 0 to LFoldRegion.Count - 1 do
      begin
        LFoldRegionItem := LFoldRegion.Items[LIndex2];
        if CheckToken(LFoldRegionItem.OpenToken, LFoldRegionItem.BeginWithBreakChar) then
          Exit(True);

        if LFoldRegionItem.OpenTokenCanBeFollowedBy <> '' then
          if CheckToken(LFoldRegionItem.OpenTokenCanBeFollowedBy, LFoldRegionItem.BeginWithBreakChar) then
            Exit(True);

        if CheckToken(LFoldRegionItem.CloseToken, LFoldRegionItem.BeginWithBreakChar) then
          Exit(True);
      end;
    end;
  end;
end;

function TCustomBCEditor.IsWordBreakChar(const AChar: Char): Boolean;
begin
  Result := FLines.IsWordBreakChar(AChar);
end;

function TCustomBCEditor.IsWordSelected(): Boolean;
begin
  Result := SelectionAvailable and (SelText = GetWordAtLinesPosition(FLines.SelArea.BeginPosition));
end;

procedure TCustomBCEditor.KeyDown(var AKey: Word; AShift: TShiftState);
var
  LChar: Char;
  LData: Pointer;
  LEditorCommand: TBCEditorCommand;
  LSecondaryShortCutKey: Word;
  LSecondaryShortCutShift: TShiftState;
  LShortCutKey: Word;
  LShortCutShift: TShiftState;
begin
  inherited;

  if AKey = 0 then
  begin
    Include(FState, esIgnoreNextChar);
    Exit;
  end;

  if FSyncEdit.Enabled then
  begin
    if FSyncEdit.Active then
      if (AKey = BCEDITOR_CARRIAGE_RETURN_KEY) or (AKey = BCEDITOR_ESCAPE_KEY) then
      begin
        FSyncEdit.Active := False;
        AKey := 0;
        Exit;
      end;

    ShortCutToKey(FSyncEdit.ShortCut, LShortCutKey, LShortCutShift);
    if (AShift = LShortCutShift) and (AKey = LShortCutKey) then
    begin
      FSyncEdit.Active := not FSyncEdit.Active;
      AKey := 0;
      Exit;
    end;
  end;

  FKeyboardHandler.ExecuteKeyDown(Self, AKey, AShift);

  LData := nil;
  LChar := BCEDITOR_NONE_CHAR;
  try
    LEditorCommand := TranslateKeyCode(AKey, AShift, LData);

    if not ReadOnly and FCompletionProposal.Enabled and not Assigned(FCompletionProposalPopupWindow) then
    begin
      ShortCutToKey(FCompletionProposal.ShortCut, LShortCutKey, LShortCutShift);
      ShortCutToKey(FCompletionProposal.SecondaryShortCut, LSecondaryShortCutKey, LSecondaryShortCutShift);

      if ((AKey = LShortCutKey) and (AShift = LShortCutShift)
        or (AKey = LSecondaryShortCutKey) and (AShift = LSecondaryShortCutShift)
        or (AKey <> LShortCutKey) and not (ssAlt in AShift) and not (ssCtrl in AShift) and (cpoAutoInvoke in FCompletionProposal.Options) and Chr(AKey).IsLetter) then
      begin
        LEditorCommand := ecCompletionProposal;
        if not (cpoAutoInvoke in FCompletionProposal.Options) then
        begin
          AKey := 0;
          Include(FState, esIgnoreNextChar);
        end;
      end;
    end;

    if FSyncEdit.Active then
    begin
      case LEditorCommand of
        ecChar, ecBackspace, ecCopy, ecCut, ecLeft, ecSelectionLeft, ecRight, ecSelectionRight:
          ;
        ecLineBreak:
          FSyncEdit.Active := False;
      else
        LEditorCommand := ecNone;
      end;
    end;

    if LEditorCommand <> ecNone then
    begin
      AKey := 0;
      Include(FState, esIgnoreNextChar);
      CommandProcessor(LEditorCommand, LChar, LData);
    end
    else
      Exclude(FState, esIgnoreNextChar);
  finally
    if Assigned(LData) then
      FreeMem(LData);
  end;
end;

procedure TCustomBCEditor.KeyPressW(var AKey: Char);
begin
  if not (esIgnoreNextChar in FState) then
  begin
    FKeyboardHandler.ExecuteKeyPress(Self, AKey);
    CommandProcessor(ecChar, AKey, nil);
  end
  else
    Exclude(FState, esIgnoreNextChar);
end;

procedure TCustomBCEditor.KeyUp(var AKey: Word; AShift: TShiftState);
begin
  inherited;

  if FCodeFolding.Visible then
    CheckIfAtMatchingKeywords;

  FKeyboardHandler.ExecuteKeyUp(Self, AKey, AShift);
end;

procedure TCustomBCEditor.LeftMarginChanged(ASender: TObject);
begin
  Include(FState, esSizeChanged);
  Invalidate();
end;

function TCustomBCEditor.LeftSpaceCount(const AText: string; AWantTabs: Boolean = False): Integer;
var
  LTextEndPos: PChar;
  LTextPos: PChar;
begin
  if ((AText = '') or not (eoAutoIndent in FOptions)) then
    Result := 0
  else
  begin
    LTextPos := @AText[1];
    LTextEndPos := @AText[Length(AText)];
    Result := 0;
    while ((LTextPos <= LTextEndPos) and (LTextPos^ <= BCEDITOR_SPACE_CHAR)) do
    begin
      if ((LTextPos^ = BCEDITOR_TAB_CHAR) and AWantTabs) then
        Inc(Result, FTabs.Width - Result mod FTabs.Width)
      else
        Inc(Result);
      Inc(LTextPos);
    end;
  end;
end;

function TCustomBCEditor.LeftTrimLength(const AText: string): Integer;
begin
  Result := 0;
  while ((Result < Length(AText)) and (AText[1 + Result] <= BCEDITOR_SPACE_CHAR)) do
    Inc(Result);
end;

procedure TCustomBCEditor.LineDeleted(ASender: TObject; const ALine: Integer);
var
  LIndex: Integer;

  procedure UpdateMarks(AMarkList: TBCEditorMarkList);
  var
    LMark: TBCEditorMark;
    LMarkIndex: Integer;
  begin
    for LMarkIndex := 0 to AMarkList.Count - 1 do
    begin
      LMark := AMarkList[LMarkIndex];
      if LMark.Line >= LIndex then
        LMark.Line := LMark.Line - 1;
    end;
  end;

begin
  ClearMatchingPair();

  UpdateMarks(FBookmarkList);
  UpdateMarks(FMarkList);

  if (ALine < FLines.Count - 1) then
    FLines.SetBeginRange(ALine + 1, FLines.Items[ALine].BeginRange);

  Modified := True;

  if (UpdateCount > 0) then
    Include(FState, esLinesDeleted)
  else
  begin
    LinesChanged();

    if (HandleAllocated) then
      UpdateScrollBars();
    Invalidate();

    if (Assigned(OnChange)) then
      OnChange(Self);

    if (HandleAllocated) then
    begin
      Include(FState, esCodeFoldingInvalid);
      KillTimer(Handle, tiCodeFolding);
      SetTimer(Handle, tiCodeFolding, FCodeFolding.DelayInterval, nil);
    end;
  end;
end;

procedure TCustomBCEditor.LineDeleting(ASender: TObject; const ALine: Integer);
begin
  DeleteLineFromRows(ALine);
end;

procedure TCustomBCEditor.LineInserted(ASender: TObject; const ALine: Integer);

  procedure UpdateMarks(AMarkList: TBCEditorMarkList);
  var
    LIndex: Integer;
    LMark: TBCEditorMark;
  begin
    for LIndex := 0 to AMarkList.Count - 1 do
    begin
      LMark := AMarkList[LIndex];
      if LMark.Line >= ALine then
        LMark.Line := LMark.Line + 1;
    end;
  end;

begin
  ClearMatchingPair();

  UpdateMarks(FBookmarkList);
  UpdateMarks(FMarkList);
  UpdateLinesBeginRanges(ALine);

  InsertLineIntoRows(ALine, True);

  Modified := True;

  if (UpdateCount > 0) then
    Include(FState, esLinesInserted)
  else
  begin
    LinesChanged();
    if (HandleAllocated) then
      UpdateScrollBars();
    Invalidate();

    if (Assigned(OnChange)) then
      OnChange(Self);

    if (HandleAllocated) then
    begin
      Include(FState, esCodeFoldingInvalid);
      KillTimer(Handle, tiCodeFolding);
      SetTimer(Handle, tiCodeFolding, FCodeFolding.DelayInterval, nil);
    end;
  end;
end;

procedure TCustomBCEditor.LinesCaretChanged(ASender: TObject);
begin
  ClearMatchingPair();
  InvalidateCaret();

  if (FUpdateCount > 0) then
    Include(FState, esCaretChanged)
  else
  begin
    if (Assigned(FOnCaretChanged)) then
      FOnCaretChanged(Self, CaretPos);
    ScrollToCaret();
  end;
end;

procedure TCustomBCEditor.LinesCleared(ASender: TObject);
begin
  ClearCodeFolding;
  ClearMatchingPair();
  ClearBookmarks;
  FMarkList.Clear;
  FRows.Clear();

  Modified := True;

  if (UpdateCount > 0) then
    Include(FState, esLinesCleared)
  else
  begin
    LinesChanged();
    InitCodeFolding();

    if (HandleAllocated) then
      UpdateScrollBars();
    InvalidateCaret();
    Invalidate();

    if (Assigned(OnChange)) then
      OnChange(Self);

    if (HandleAllocated) then
    begin
      Include(FState, esCodeFoldingInvalid);
      KillTimer(Handle, tiCodeFolding);
      SetTimer(Handle, tiCodeFolding, FCodeFolding.DelayInterval, nil);
    end;
  end;
end;

procedure TCustomBCEditor.LinesChanged();
begin
  if (FLeftMargin.LineNumbers.Visible
    and (VisibleRows > 0)) then
    FLineNumbersWidth := 2 * GPadding + Max(2, Length(IntToStr(FLines.Count + VisibleRows + 1))) * FMaxDigitWidth;
end;

procedure TCustomBCEditor.LinesHookChanged;
begin
  UpdateScrollBars;
  Invalidate;
end;

function TCustomBCEditor.LinesToRows(const ALinesPosition: TBCEditorLinesPosition): TBCEditorRowsPosition;
var
  LChar: Integer;
  LColumn: Integer;
  LLinePos: PChar;
  LLineEndPos: PChar;
  LRow: Integer;
begin
  if (FLines.Count = 0) then
    Result := RowsPosition(ALinesPosition.Char, ALinesPosition.Line)
  else if (ALinesPosition.Line >= FLines.Count) then
    Result := RowsPosition(ALinesPosition.Char, FRows.Count + ALinesPosition.Line - FLines.Count)
  else if ((FRows.Count >= 0) and (FLines.Items[ALinesPosition.Line].FirstRow < 0)) then
    // FRows.Count >= 0 is not needed, but BuildRows must be called to initialize FLines.FirstRow
    raise ERangeError.CreateFmt(SBCEditorLineIsNotVisible, [ALinesPosition.Line])
  else
  begin
    LRow := FLines.Items[ALinesPosition.Line].FirstRow;
    LChar := ALinesPosition.Char;
    while ((LChar >= FRows.Items[LRow].Length) and not (rfLastRowOfLine in FRows.Items[LRow].Flags)) do
    begin
      Dec(LChar, FRows.Items[LRow].Length);
      Inc(LRow);
    end;

    if (not (rfHasTabs in FRows.Items[LRow].Flags)) then
      Result := RowsPosition(ALinesPosition.Char - FRows.Items[LRow].Char, LRow)
    else
    begin
      LColumn := 0;
      LLinePos := @FLines[ALinesPosition.Line][1 + FRows.Items[LRow].Char];
      LLineEndPos := @FLines[ALinesPosition.Line][Min(1 + FRows.Items[LRow].Char + LChar, Length(FLines[ALinesPosition.Line]))];
      while (LLinePos <= LLineEndPos) do
      begin
        Inc(LColumn, ComputeTextColumns(LLinePos, 1, LColumn));
        Inc(LLinePos);
      end;
      if (Length(FLines[ALinesPosition.Line]) < LChar) then
        Inc(LColumn, LChar - Length(FLines[ALinesPosition.Line]));

      Result := RowsPosition(LColumn, LRow);
    end;
  end;
end;

procedure TCustomBCEditor.LineUpdated(ASender: TObject; const ALine: Integer);
begin
  ClearMatchingPair();
  UpdateRows(ALine);
  UpdateLinesBeginRanges(ALine);

  Modified := True;

  if (UpdateCount > 0) then
    Include(FState, esLinesUpdated)
  else
  begin
    LinesChanged();
    Invalidate();

    if (Assigned(OnChange)) then
      OnChange(Self);

    if (HandleAllocated) then
    begin
      Include(FState, esCodeFoldingInvalid);
      KillTimer(Handle, tiCodeFolding);
      SetTimer(Handle, tiCodeFolding, FCodeFolding.DelayInterval, nil);
    end;
  end;
end;

procedure TCustomBCEditor.LoadFromFile(const AFileName: string; AEncoding: TEncoding = nil);
begin
  FLines.LoadFromFile(AFileName, AEncoding);
end;

procedure TCustomBCEditor.LoadFromStream(AStream: TStream; AEncoding: TEncoding = nil);
begin
  FLines.LoadFromStream(AStream, AEncoding);
end;

procedure TCustomBCEditor.MarkListChange(ASender: TObject);
begin
  Invalidate;
end;

var
  Nils: Integer = 0;

procedure TCustomBCEditor.MouseDown(AButton: TMouseButton; AShift: TShiftState; X, Y: Integer);
var
  LAction: TProcessAction;
begin
  Inc(Nils);

  KillTimer(Handle, tiShowHint);
  if (Assigned(FHintWindow)) then
    FreeAndNil(FHintWindow);

  FLines.UndoGroupBreak();

  FMouseDownPoint := Point(X, Y);

  inherited;

  FKeyboardHandler.ExecuteMouseDown(Self, AButton, AShift, X, Y);

  if (GetTickCount() < FLastDoubleClickTime + FDoubleClickTime) then
  begin
    LAction := paMouseTriple;
    FLastDoubleClickTime := 0;
    Include(FState, esMouseDouble);
  end
  else if (ssDouble in AShift) then
  begin
    LAction := paMouseDouble;
    FLastDoubleClickTime := GetTickCount();
    Include(FState, esMouseDouble);
  end
  else
    LAction := paMouseDown;

  if (AButton <> mbMiddle) then
  begin
    Include(FState, esHandlingMouse);
    Process(LAction, AButton, AShift, X, Y);
    Exclude(FState, esHandlingMouse);
  end
  else if (soMiddleClickMove in FScroll.Options) then
  begin
    FMouseMoveScrolling := not FMouseMoveScrolling;
    if (FMouseMoveScrolling) then
      FMouseMoveScrollingPoint := Point(X, Y);
    Invalidate();
  end;
end;

procedure TCustomBCEditor.MouseMove(AShift: TShiftState; X, Y: Integer);
var
  LMsg: TMsg;
begin
  if (Assigned(FHintWindow)
    and (Point(X, Y) <> FMouseMoveHintPoint)) then
    FreeAndNil(FHintWindow);

  inherited;

  if (PeekMessage(LMsg, Handle, WM_MOUSEMOVE, WM_MOUSEMOVE, PM_NOREMOVE)
    and (LMsg.Message = WM_MOUSEMOVE)
    and (KeysToShiftState(LMsg.wParam) = AShift)) then
    // Do nothing - handle this message within the next equal message
  else if (FMouseMoveScrolling) then
    ComputeScroll(Point(X, Y))
  else
  begin
    Include(FState, esHandlingMouse);
    Process(paMouseMove, mbLeft, AShift, X, Y);
    Exclude(FState, esHandlingMouse);

    if ((ssLeft in AShift) and (MouseCapture <> mcNone) and (Point(X, Y) <> FOldMouseMovePoint)) then
    begin
      FOldMouseMovePoint := Point(X, Y);
      ComputeScroll(FOldMouseMovePoint);
      FLastSortOrder := soDesc;
    end;

    if (not Assigned(FHintWindow)
      and (Point(X, Y) <> FMouseMoveHintPoint)
      and (AShift * [ssLeft, ssRight, ssMiddle] = [])) then
      if (ClientRect.Contains(Point(X, Y))) then
      begin
        SetTimer(Handle, tiShowHint, Application.HintPause, nil);
        FMouseMoveHintPoint := Point(X, Y);
        if (MouseCapture = mcNone) then
          MouseCapture := mcAll;
      end
      else
      begin
        KillTimer(Handle, tiShowHint);
        if (MouseCapture = mcAll) then
          MouseCapture := mcNone;
      end;
  end;
end;

procedure TCustomBCEditor.MouseMoveScrollTimerHandler(ASender: TObject);
var
  LCursorPoint: TPoint;
begin
  BeginUpdate();

  GetCursorPos(LCursorPoint);
  LCursorPoint := ScreenToClient(LCursorPoint);
  if FScrollDeltaX <> 0 then
  begin
    if GetKeyState(VK_SHIFT) < 0 then
      HorzTextPos := HorzTextPos + FTextWidth
    else
      HorzTextPos := HorzTextPos + FScrollDeltaX;
  end;
  if FScrollDeltaY <> 0 then
  begin
    if GetKeyState(VK_SHIFT) < 0 then
      TopRow := TopRow + FScrollDeltaY * VisibleRows
    else
      TopRow := TopRow + FScrollDeltaY;
  end;

  EndUpdate();
  ComputeScroll(LCursorPoint);
end;

procedure TCustomBCEditor.MouseUp(AButton: TMouseButton; AShift: TShiftState; X, Y: Integer);
begin
  KillTimer(Handle, tiShowHint);

  inherited;

  FKeyboardHandler.ExecuteMouseUp(Self, AButton, AShift, X, Y);

  Include(FState, esHandlingMouse);
  Process(paMouseUp, AButton, AShift, X, Y);
  Exclude(FState, esHandlingMouse);

  Exclude(FState, esWaitForDrag);
  FMouseMoveScrollTimer.Enabled := False;
  FScrollTimer.Enabled := False;
  MouseCapture := mcNone;
  Exclude(FState, esMouseDouble);
end;

procedure TCustomBCEditor.MoveCaretAndSelection(ABeforeLinesPosition, AAfterLinesPosition: TBCEditorLinesPosition;
  const ASelectionCommand: Boolean);
begin
  if (not (soBeyondEndOfLine in FScroll.Options)) then
    if (AAfterLinesPosition.Line < FLines.Count) then
      AAfterLinesPosition.Char := Min(AAfterLinesPosition.Char, Length(FLines[AAfterLinesPosition.Line]))
    else
      AAfterLinesPosition.Char := 0;
  if (not (soBeyondEndOfLine in FScroll.Options)) then
    AAfterLinesPosition.Line := Max(0, Min(AAfterLinesPosition.Line, FLines.Count - 1));
  if (not ASelectionCommand) then
    FLines.CaretPosition := AAfterLinesPosition
  else
    SetCaretAndSelection(AAfterLinesPosition, LinesArea(FLines.SelArea.BeginPosition, AAfterLinesPosition));
end;

procedure TCustomBCEditor.MoveCaretHorizontally(const AColumns: Integer;
  const SelectionCommand: Boolean);
var
  LLineEndPos: PChar;
  LLinePos: PChar;
  LLineTextLength: Integer;
  LNewCaretPosition: TBCEditorLinesPosition;
begin
  if (FLines.CaretPosition.Char + AColumns >= 0) then
    if (FLines.CaretPosition.Line < FLines.Count) then
    begin
      LLineTextLength := Length(FLines.Items[FLines.CaretPosition.Line].Text);

      LNewCaretPosition := LinesPosition(Max(0, FLines.CaretPosition.Char + AColumns), FLines.CaretPosition.Line);
      if (not (soBeyondEndOfLine in FScroll.Options) or WordWrap.Enabled) then
        LNewCaretPosition.Char := Min(LNewCaretPosition.Char, LLineTextLength);

      { Skip combined and non-spacing marks }
      if ((0 < LLineTextLength) and (LNewCaretPosition.Char < LLineTextLength)) then
      begin
        LLinePos := @FLines.Items[FLines.CaretPosition.Line].Text[1 + LNewCaretPosition.Char];
        LLineEndPos := @FLines.Items[FLines.CaretPosition.Line].Text[Length(FLines.Items[FLines.CaretPosition.Line].Text)];
        while ((LLinePos <= LLineEndPos)
          and ((LLinePos^.GetUnicodeCategory in [TUnicodeCategory.ucCombiningMark, TUnicodeCategory.ucNonSpacingMark])
            or ((LLinePos - 1)^ <> BCEDITOR_NONE_CHAR)
              and ((LLinePos - 1)^.GetUnicodeCategory = TUnicodeCategory.ucNonSpacingMark)
              and not IsCombiningDiacriticalMark((LLinePos - 1)^))) do
        begin
          Dec(LLinePos);
          Dec(LNewCaretPosition.Char);
        end;
      end;

      MoveCaretAndSelection(FLines.SelArea.BeginPosition, LNewCaretPosition, SelectionCommand);
    end
    else if ((soBeyondEndOfLine in FScroll.Options) and not WordWrap.Enabled) then
      MoveCaretAndSelection(FLines.SelArea.BeginPosition, LinesPosition(FLines.CaretPosition.Char + AColumns, FLines.CaretPosition.Line), SelectionCommand);
end;

procedure TCustomBCEditor.MoveCaretVertically(const ARows: Integer; const SelectionCommand: Boolean);
var
  LNewCaretPosition: TBCEditorRowsPosition;
  LX: Integer;
begin
  if (esCaretInvalid in FState) then
    LX := FCaretPos.X
  else
    LX := RowsToClient(FRows.CaretPosition).X;

  LNewCaretPosition := FRows.CaretPosition;
  if ((ARows < 0) or (soBeyondEndOfFile in Scroll.Options)) then
    LNewCaretPosition.Row := Max(0, LNewCaretPosition.Row + ARows)
  else
    LNewCaretPosition.Row := Max(0, Min(FRows.Count - 1, LNewCaretPosition.Row + ARows));
  LNewCaretPosition.Column := ClientToRows(LX, LNewCaretPosition.Row * LineHeight, True).Column;

  if (not (soBeyondEndOfLine in FScroll.Options) or WordWrap.Enabled) then
    if (LNewCaretPosition.Row < FRows.Count) then
      if (not (rfLastRowOfLine in FRows.Items[LNewCaretPosition.Row].Flags)) then
        LNewCaretPosition.Column := Min(LNewCaretPosition.Column, FRows.Items[LNewCaretPosition.Row].Length - 1)
      else
        LNewCaretPosition.Column := Min(LNewCaretPosition.Column, FRows.Items[LNewCaretPosition.Row].Length)
    else
      LNewCaretPosition.Column := 0;

  MoveCaretAndSelection(FLines.CaretPosition, RowsToLines(LNewCaretPosition), SelectionCommand);
end;

function TCustomBCEditor.NextWordPosition(const ALinesPosition: TBCEditorLinesPosition): TBCEditorLinesPosition;
begin
  if (ALinesPosition.Line >= FLines.Count) then
    Result := FLines.EOFPosition
  else
  begin
    Result := Min(ALinesPosition, FLines.EOLPosition[ALinesPosition.Line]);
    if (Result.Char < Length(FLines.Items[Result.Line].Text)) then
      while ((Result.Char < Length(FLines.Items[Result.Line].Text)) and IsWordBreakChar(FLines.Char[Result])) do
        Inc(Result.Char)
    else if (Result.Line < FLines.Count - 1) then
    begin
      Result := FLines.BOLPosition[Result.Line + 1];
      while ((Result.Char + 1 < Length(FLines.Items[Result.Line].Text)) and IsWordBreakChar(FLines.Items[Result.Line].Text[Result.Char + 1])) do
        Inc(Result.Char);
    end
  end;
end;

procedure TCustomBCEditor.Notification(AComponent: TComponent; AOperation: TOperation);
begin
  inherited Notification(AComponent, AOperation);

  if AOperation = opRemove then
  begin
    if AComponent = FChainedEditor then
      RemoveChainedEditor;

    if Assigned(FLeftMargin) and Assigned(FLeftMargin.Bookmarks) and Assigned(FLeftMargin.Bookmarks.Images) then
      if (AComponent = FLeftMargin.Bookmarks.Images) then
      begin
        FLeftMargin.Bookmarks.Images := nil;
        Invalidate;
      end;
  end;
end;

procedure TCustomBCEditor.NotifyHookedCommandHandlers(AAfterProcessing: Boolean;
  var ACommand: TBCEditorCommand; var AChar: Char; AData: Pointer);
var
  LHandled: Boolean;
  LHookedCommandHandler: TBCEditorHookedCommandHandler;
  LIndex: Integer;
begin
  LHandled := False;
  for LIndex := 0 to GetHookedCommandHandlersCount - 1 do
  begin
    LHookedCommandHandler := TBCEditorHookedCommandHandler(FHookedCommandHandlers[LIndex]);
    LHookedCommandHandler.Event(Self, AAfterProcessing, LHandled, ACommand, AChar, AData, LHookedCommandHandler.Data);
  end;
  if LHandled then
    ACommand := ecNone;
end;

procedure TCustomBCEditor.Paint();

  procedure BuildRows(const AUpdateScrollBars: Boolean);
  const
    RowToInsert = -3;
  var
    LCodeFolding: Integer;
    LLine: Integer;
    LRange: TBCEditorCodeFolding.TRanges.TRange;
    LRow: Integer;
  begin
    HandleNeeded();

    Include(FState, esBuildingRows);
    try
      for LLine := 0 to FLines.Count - 1 do
        FLines.SetFirstRow(LLine, RowToInsert);

      for LCodeFolding := 0 to FAllCodeFoldingRanges.AllCount - 1 do
      begin
        LRange := FAllCodeFoldingRanges[LCodeFolding];
        if (Assigned(LRange) and LRange.Collapsed) then
          for LLine := LRange.BeginLine + 1 to LRange.EndLine do
            FLines.SetFirstRow(LLine, -1);
      end;

      LRow := 0;
      for LLine := 0 to FLines.Count - 1 do
        if (FLines.Items[LLine].FirstRow = RowToInsert) then
          Inc(LRow, InsertLineIntoRows(LLine, LRow));
    finally
      Exclude(FState, esBuildingRows);
    end;

    InitCodeFolding();

    if (UpdateCount > 0) then
      Include(FState, esRowsChanged)
    else
    begin
      UpdateScrollBars();
      Invalidate();
    end;
  end;

  function EnumFontsFamiliesProc(var lpelf: TEnumLogFont; var lpntm: TNewTextMetric;
    FontType: Integer; lParam: LPARAM): Integer; stdcall;
  begin;
    Result := Integer(lpelf.elfLogFont.lfPitchAndFamily and FIXED_PITCH <> 0);
  end;

var
  LBitmap: TBitmap;
  LIndex: Integer;
  LInsertPos: TPoint;
  LPoints: array [0 .. 2] of TPoint;
  LRect: TRect;
  LScrollBarInfo: TScrollBarInfo;
  LText: string;
  LTextWidth: Integer;
  LVisibleRows: Integer;
  LWidth: Integer;
  LY: Integer;
begin
  if ((FState * [esFontChanged] <> [])
    or ((FState * [esSizeChanged] <> []) and FWordWrap.Enabled)) then
    FRows.Clear();

  if (FState * [esFontChanged, esSysFontChanged, esHighlighterChanged] <> []) then
  begin
    FPaintHelper.BeginDrawing(Canvas);
    try
      FPaintHelper.Font := Font;

      FFontPitchFixed := EnumFontFamilies(Canvas.Handle, PChar(Font.Name),
        @EnumFontsFamiliesProc, 0);

      FPaintHelper.Style := [];
      FLineHeight := FPaintHelper.TextHeight(BCEDITOR_SPACE_CHAR, 1);

      FMaxDigitWidth := FPaintHelper.TextWidth('0', 1);
      for LIndex := 1 to 9 do
        FMaxDigitWidth := Max(FMaxDigitWidth, FPaintHelper.TextWidth(PChar(IntToStr(LIndex)), 1));
      LinesChanged();

      FSpaceWidth := FPaintHelper.TextWidth(BCEDITOR_SPACE_CHAR, 1);
      FTabSignWidth := FPaintHelper.TextWidth(#187, 1);
      FLineBreakSignWidth := FPaintHelper.TextWidth(#182, 1);
      FCodeFoldingCollapsedMarkWidth := FPaintHelper.TextWidth(BCEDITOR_CODEFOLDING_COLLAPSEDMARK, StrLen(BCEDITOR_CODEFOLDING_COLLAPSEDMARK));

      FCodeFoldingWidth := Min(FLineHeight, GetSystemMetrics(SM_CXSMICON));

      if (Assigned(FImages)) then
        FImages.Free();
      FImages := TImageList.Create(Self);
      FImages.ColorDepth := cd32Bit;
      FImages.Height := Min(FLineHeight, GetSystemMetrics(SM_CXSMICON));
      FImages.Width := FImages.Height;

      // Bookmarks
      for LIndex := 0 to BCEDITOR_BOOKMARKS - 1 do
      begin
        LBitmap := TBitmap.Create();
        LBitmap.Handle := CreateCompatibleBitmap(Canvas.Handle, FImages.Width, FImages.Height);
        LBitmap.TransparentColor := clTransparent;

        LRect := Rect(0, 0, FImages.Width, FImages.Height);
        if (FLeftMargin.Colors.BookmarkPanelBackground <> clNone) then
          LBitmap.Canvas.Brush.Color := FLeftMargin.Colors.BookmarkPanelBackground
        else if (FLeftMargin.Colors.Background <> clNone) then
          LBitmap.Canvas.Brush.Color := FLeftMargin.Colors.Background
        else
          LBitmap.Canvas.Brush.Color := Color;
        LBitmap.Canvas.FillRect(LRect);

        LRect.Inflate(- GPadding - GLineWidth, - GPadding - GLineWidth);
        LBitmap.Canvas.Brush.Color := clBookmarkCover;
        Inc(LRect.Left, GLineWidth);
        LBitmap.Canvas.FillRect(LRect);

        LBitmap.Canvas.Font.Assign(GetParentForm(Self).Font);
        LBitmap.Canvas.Font.Height := - (FImages.Height - 2 * GPadding - 2 * GLineWidth);
        LBitmap.Canvas.Font.Color := clBookmarkNumber;
        LBitmap.Canvas.Brush.Style := bsClear;
        Inc(LRect.Left, GLineWidth);
        Dec(LRect.Top, 2 * GLineWidth);
        if (LIndex < BCEDITOR_BOOKMARKS - 1) then
          LText := IntToStr(LIndex + 1)
        else
          LText := '0';
        LBitmap.Canvas.TextRect(LRect, LText, [tfCenter, tfVerticalCenter]);

        LBitmap.Canvas.Pen.Color := clBookmarkBorder;
        LBitmap.Canvas.Pen.Width := GLineWidth;
        LBitmap.Canvas.Brush.Style := bsClear;
        LBitmap.Canvas.Rectangle(GPadding + GLineWidth, GPadding, FImages.Width - GPadding, FImages.Height - 2 * GPadding);

        LY := GPadding + 2 * GLineWidth;
        repeat
          LBitmap.Canvas.Pen.Color := clBookmarkRingLeft;
          LBitmap.Canvas.MoveTo(GPadding, LY);
          LBitmap.Canvas.LineTo(GPadding + 1 * GLineWidth, LY);
          LBitmap.Canvas.Pen.Color := clBookmarkRingMiddle;
          LBitmap.Canvas.LineTo(GPadding + 2 * GLineWidth, LY);
          LBitmap.Canvas.Pen.Color := clBookmarkRingRight;
          LBitmap.Canvas.LineTo(GPadding + 3 * GLineWidth, LY);
          Inc(LY, 2 * GLineWidth);
        until (LY >= FImages.Height - 2 * GPadding - 2 * GLineWidth);

        ImageList_Add(FImages.Handle, LBitmap.Handle, LBitmap.MaskHandle);
        LBitmap.Free();
      end;

      // Collapsed / Expanded Code Folding Mark
      LBitmap := TBitmap.Create();
      LBitmap.Handle := CreateCompatibleBitmap(Canvas.Handle, FImages.Width, FImages.Height);
      LBitmap.TransparentColor := clTransparent;
      LBitmap.Canvas.Brush.Color := LBitmap.TransparentColor;
      LBitmap.Canvas.FillRect(Rect(0, 0, LBitmap.Width, LBitmap.Height));
      LBitmap.Canvas.Brush.Color := FLeftMargin.Colors.Background;
      if (FLeftMargin.Colors.Foreground <> clNone) then
        LBitmap.Canvas.Pen.Color := FLeftMargin.Colors.Foreground
      else
        LBitmap.Canvas.Pen.Color := Font.Color;
      LBitmap.Canvas.Pen.Width := GLineWidth;
      LBitmap.Canvas.FillRect(Rect(0, 0, LBitmap.Width, LBitmap.Height));
      LBitmap.Canvas.Rectangle(GPadding + 2 * GLineWidth, GPadding + 2 * GLineWidth, FImages.Width - GPadding - 2 * GLineWidth - 1, FImages.Height - GPadding - 2 * GLineWidth - 1);
      LBitmap.Canvas.MoveTo(GPadding + 4 * GLineWidth, (2 * FImages.Height - GLineWidth) div 4);
      LBitmap.Canvas.LineTo(FImages.Width - GPadding - 4 * GLineWidth - 1, (2 * FImages.Height - GLineWidth) div 4);
      ImageList_Add(FImages.Handle, LBitmap.Handle, LBitmap.MaskHandle);
      LBitmap.Canvas.MoveTo((2 * FImages.Width - GLineWidth) div 4, GPadding + 4 * GLineWidth);
      LBitmap.Canvas.LineTo((2 * FImages.Width - GLineWidth) div 4, FImages.Height - GPadding - 4 * GLineWidth - 1);
      ImageList_Add(FImages.Handle, LBitmap.Handle, LBitmap.MaskHandle);
      LBitmap.Free();

      // Code Folding Line / End Line
      LBitmap := TBitmap.Create();
      LBitmap.Handle := CreateCompatibleBitmap(Canvas.Handle, FImages.Width, FImages.Height);
      LBitmap.TransparentColor := clTransparent;
      LBitmap.Canvas.Brush.Color := LBitmap.TransparentColor;
      LBitmap.Canvas.FillRect(Rect(0, 0, LBitmap.Width, LBitmap.Height));
      if (FLeftMargin.Colors.Foreground <> clNone) then
        LBitmap.Canvas.Pen.Color := FLeftMargin.Colors.Foreground
      else
        LBitmap.Canvas.Pen.Color := Font.Color;
      LBitmap.Canvas.Pen.Width := GLineWidth;
      LBitmap.Canvas.MoveTo((2 * FImages.Width - GLineWidth) div 4, 0);
      LBitmap.Canvas.LineTo((2 * FImages.Width - GLineWidth) div 4, FImages.Height);
      ImageList_Add(FImages.Handle, LBitmap.Handle, LBitmap.MaskHandle);
      LBitmap.Canvas.MoveTo((2 * FImages.Width - GLineWidth) div 4, FImages.Height - 1);
      LBitmap.Canvas.LineTo(FImages.Width - GPadding, FImages.Height - 1);
      ImageList_Add(FImages.Handle, LBitmap.Handle, LBitmap.MaskHandle);
      LBitmap.Free();

      // Insert Mark
      if (Assigned(FInsertPosBitmap)) then
        FInsertPosBitmap.Free();
      FInsertPosBitmap := TBitmap.Create();
      FInsertPosBitmap.Handle := CreateCompatibleBitmap(Canvas.Handle, 3 * GLineWidth, FImages.Height);
      FInsertPosBitmap.TransparentColor := clTransparent;
      FInsertPosBitmap.Canvas.Brush.Color := FInsertPosBitmap.TransparentColor;
      FInsertPosBitmap.Canvas.FillRect(Rect(0, 0, FInsertPosBitmap.Width, FInsertPosBitmap.Height));
      FInsertPosBitmap.Canvas.Pen.Color := Font.Color;
      FInsertPosBitmap.Canvas.Brush.Color := Font.Color;
      LPoints[0] := Point(0, GPadding);
      LPoints[1] := Point(GLineWidth, GPadding + GLineWidth);
      LPoints[2] := Point(GLineWidth, GPadding);
      FInsertPosBitmap.Canvas.Polygon(LPoints);
      LPoints[0] := Point(2 * GLineWidth - 1, GPadding);
      LPoints[1] := Point(2 * GLineWidth - 1, GPadding + GLineWidth);
      LPoints[2] := Point(3 * GLineWidth - 1, GPadding);
      FInsertPosBitmap.Canvas.Polygon(LPoints);
      for LIndex := 0 to GLineWidth - 1 do
      begin
        FInsertPosBitmap.Canvas.MoveTo(GLineWidth + LIndex, GPadding);
        FInsertPosBitmap.Canvas.LineTo(GLineWidth + LIndex, LineHeight - GPadding);
      end;
      LPoints[0] := Point(0, LineHeight - 1 - GPadding);
      LPoints[1] := Point(GLineWidth, LineHeight - 1 - GLineWidth - GPadding);
      LPoints[2] := Point(GLineWidth, LineHeight - 1 - GPadding);
      FInsertPosBitmap.Canvas.Polygon(LPoints);
      LPoints[0] := Point(2 * GLineWidth - 1, LineHeight - 1 - GPadding);
      LPoints[1] := Point(2 * GLineWidth - 1, LineHeight - 1 - GLineWidth - GPadding);
      LPoints[2] := Point(3 * GLineWidth - 1, LineHeight - 1 - GPadding);
      FInsertPosBitmap.Canvas.Polygon(LPoints);
    finally
      FPaintHelper.EndDrawing();
    end;
  end;

  if (FState * [esFontChanged, esSizeChanged] <> []) then
  begin
    InvalidateCaret();

    if (HandleAllocated and (FSpaceWidth > 0) and (LineHeight > 0)) then
    begin
      LScrollBarInfo.cbSize := SizeOf(LScrollBarInfo);
      GetScrollBarInfo(Handle, Integer(OBJID_VSCROLL), LScrollBarInfo);
      if (LScrollBarInfo.rgstate[0] <> STATE_SYSTEM_INVISIBLE) then
        LTextWidth := ClientWidth - FLeftMarginWidth - GetSystemMetrics(SM_CYVSCROLL)
      else
        LTextWidth := ClientWidth - FLeftMarginWidth;

      LScrollBarInfo.cbSize := SizeOf(LScrollBarInfo);
      GetScrollBarInfo(Handle, Integer(OBJID_HSCROLL), LScrollBarInfo);
      if (LScrollBarInfo.rgstate[0] <> STATE_SYSTEM_INVISIBLE) then
        LVisibleRows := Max(1, (ClientHeight - GetSystemMetrics(SM_CYHSCROLL)) div LineHeight)
      else
        LVisibleRows := Max(1, ClientHeight div LineHeight);

      FillChar(FItalicOffsetCache, SizeOf(FItalicOffsetCache), 0);

      if ((LTextWidth <> FTextWidth) or (LVisibleRows <> VisibleRows)) then
      begin
        if (FWordWrap.Enabled and (LTextWidth <> FTextWidth)) then
          FRows.Clear();

        FTextWidth := LTextWidth;
        FVisibleRows := LVisibleRows;

        LinesChanged();
      end;
    end;
  end;

  if (FState * [esFontChanged] <> []) then
  begin
    if (FFontPitchFixed) then
      LWidth := GetSystemMetrics(SM_CXEDGE)
    else
      LWidth := 0;
    CreateCaret(Handle, 0, LWidth, LineHeight);
  end;

  if (FCurrentMatchingPair.FState = mpsClear) then
    ScanMatchingPair();

  FLeftMarginWidth := 0;
  if (FLeftMargin.MarksPanel.Visible) then
    Inc(FLeftMarginWidth, FMarksPanelWidth);
  if (FLeftMargin.LineNumbers.Visible) then
    Inc(FLeftMarginWidth, FLineNumbersWidth);
  if (FLeftMargin.LineState.Visible) then
    Inc(FLeftMarginWidth, FLineStateWidth);
  if (FCodeFolding.Visible) then
    Inc(FLeftMarginWidth, FCodeFoldingWidth);
  FTextWidth := ClientWidth - FLeftMarginWidth;

  if ((FRows.Count = 0) and (FLines.Count > 0)) then
    BuildRows(False);

  Process(paPaint, mbLeft, [], -1, -1);

  if (FInsertPos <> InvalidCaretPos) then
  begin
    LInsertPos := FInsertPos;
    FInsertPos := InvalidCaretPos;
    SetInsertPos(LInsertPos);
  end;

  if (FState * [esCaretInvalid] <> []) then
  begin
    if ((FCaretPos = InvalidCaretPos)
      and ((FLines.CaretPosition.Line >= FLines.Count)
        or (FLines.CaretPosition.Char >= Length(FLines.Items[FLines.CaretPosition.Line].Text)))
      and (FRows.CaretPosition.Row in [TopRow .. TopRow + VisibleRows + 1])) then
      FCaretPos := RowsToClient(FRows.CaretPosition);

    UpdateCaret();
  end;

  if (FState * [esScrolled] <> []) then
    UpdateScrollBars();

  FState := FState - [esScrolled, esFontChanged, esSysFontChanged, esSizeChanged, esHighlighterChanged];
end;

procedure TCustomBCEditor.PaintMouseMoveScrollPoint;
var
  LHalfWidth: Integer;
begin
  LHalfWidth := FScroll.Indicator.Width div 2;
  FScroll.Indicator.Draw(Canvas, FMouseMoveScrollingPoint.X - LHalfWidth, FMouseMoveScrollingPoint.Y - LHalfWidth);
end;

procedure TCustomBCEditor.PaintSyncItems;
var
  LIndex: Integer;
  LLength: Integer;
  LOldBrushStyle: TBrushStyle;
  LOldPenColor: TColor;
  LTextPosition: TBCEditorLinesPosition;

  procedure DrawRectangle(ATextPosition: TBCEditorLinesPosition);
  var
    LRect: TRect;
    LRowsPosition: TBCEditorRowsPosition;
  begin
    LRect.Top := (ATextPosition.Line - TopRow + 1) * LineHeight;
    LRect.Bottom := LRect.Top + LineHeight;
    LRowsPosition := LinesToRows(ATextPosition);
    LRect.Left := RowsToClient(LRowsPosition).X;
    Inc(LRowsPosition.Column, LLength);
    LRect.Right := RowsToClient(LRowsPosition).X;
    Canvas.Rectangle(LRect);
  end;

begin
  if not Assigned(FSyncEdit.SyncItems) then
    Exit;

  LLength := FSyncEdit.EditArea.EndPosition.Char - FSyncEdit.EditArea.BeginPosition.Char;

  LOldPenColor := Canvas.Pen.Color;
  LOldBrushStyle := Canvas.Brush.Style;
  Canvas.Brush.Style := bsClear;
  Canvas.Pen.Color := FSyncEdit.Colors.EditBorder;
  DrawRectangle(FSyncEdit.EditArea.BeginPosition);

  for LIndex := 0 to FSyncEdit.SyncItems.Count - 1 do
  begin
    LTextPosition := PBCEditorTextPosition(FSyncEdit.SyncItems.Items[LIndex])^;

    if LinesToRows(LTextPosition).Row > TopRow + VisibleRows then
      Exit
    else
    if LinesToRows(LTextPosition).Row + 1 >= TopRow then
    begin
      Canvas.Pen.Color := FSyncEdit.Colors.WordBorder;
      DrawRectangle(LTextPosition);
    end;
  end;
  Canvas.Pen.Color := LOldPenColor;
  Canvas.Brush.Style := LOldBrushStyle;
end;

function TCustomBCEditor.ProcessToken(const AAction: TProcessAction;
  const AButton: TMouseButton; const AShift: TShiftState; const X, Y: Integer;
  var ARect: TRect;
  const ALinesPosition: TBCEditorLinesPosition;
  const ARowsPosition: TBCEditorRowsPosition;
  const AText: PChar; const ALength: Integer;
  const AToken: TBCEditorHighlighter.PFind = nil;
  const ARange: TBCEditorCodeFolding.TRanges.TRange = nil;
  const APaintData: PPaintTokenData = nil): Boolean;
var
  LEndPosition: TBCEditorLinesPosition;

  procedure AddPart(const APartBeginPosition, APartEndPosition: TBCEditorLinesPosition;
    const APartType: TPaintTokenData.TPart.TPartType);
  var
    LIndex: Integer;
    LPart: TPaintTokenData.TPart;
  begin
    LIndex := APaintData^.Parts.Count - 1;
    while (LIndex >= 0) do
    begin
      if (APaintData^.Parts.List[LIndex].BeginPosition = APartBeginPosition) then
      begin
        if (APaintData^.Parts.List[LIndex].BeginPosition = APartBeginPosition) then
        begin
          if (APaintData^.Parts.List[LIndex].EndPosition = APartEndPosition) then
            APaintData^.Parts.List[LIndex].PartType := APartType
          else if (APaintData^.Parts.List[LIndex].EndPosition > APartEndPosition) then
          begin
            APaintData^.Parts.List[LIndex].BeginPosition := APartEndPosition;

            LPart.BeginPosition := APartBeginPosition;
            LPart.EndPosition := APartEndPosition;
            LPart.PartType := APartType;
            APaintData^.Parts.Insert(LIndex, LPart);
          end
          else
          begin
            APaintData^.Parts.List[LIndex].EndPosition := APartEndPosition;
            APaintData^.Parts.List[LIndex].PartType := APartType;
            while ((LIndex < APaintData^.Parts.Count) and (APaintData^.Parts.List[LIndex].EndPosition < APartEndPosition)) do
              APaintData^.Parts.Delete(LIndex);
            if (LIndex < APaintData^.Parts.Count) then
              APaintData^.Parts.List[LIndex].BeginPosition := APartEndPosition;
          end;
          exit;
        end
      end
      else if (APaintData^.Parts.List[LIndex].BeginPosition < APartBeginPosition) then
      begin
        while ((LIndex >= 0) and (APaintData^.Parts.List[LIndex].BeginPosition > APartBeginPosition)) do
        begin
          APaintData^.Parts.Delete(LIndex);
          Dec(LIndex);
        end;
        if ((LIndex > 0) and (APaintData^.Parts.List[LIndex - 1].EndPosition > APartBeginPosition)) then
          APaintData^.Parts.List[LIndex - 1].EndPosition := APartBeginPosition;
        Inc(LIndex);
        break;
      end;

      Dec(LIndex);
    end;

    if (LIndex < 0) then
      LIndex := 0;

    LPart.BeginPosition := APartBeginPosition;
    LPart.EndPosition := APartEndPosition;
    LPart.PartType := APartType;
    if ((APaintData^.Parts.Count > 0) and (LIndex < APaintData^.Parts.Count)) then
      APaintData^.Parts.Insert(LIndex, LPart)
    else
      APaintData^.Parts.Add(LPart);
  end;

  procedure ApplyPart(const AArea: TBCEditorLinesArea; APartType: TPaintTokenData.TPart.TPartType);
  begin
    if ((AArea.BeginPosition <= ALinesPosition) and (ALinesPosition < AArea.EndPosition)) then
      AddPart(ALinesPosition, Min(LEndPosition, AArea.EndPosition), APartType)
    else if ((ALinesPosition < AArea.BeginPosition) and (AArea.BeginPosition < LEndPosition)) then
      AddPart(AArea.BeginPosition, Min(LEndPosition, AArea.EndPosition), APartType);
  end;

  procedure CompleteParts();
  var
    LIndex: Integer;
    LPosition: TBCEditorLinesPosition;
  begin
    LPosition := ALinesPosition;
    LIndex := 0;

    while (LPosition < LEndPosition) do
      if (LIndex = APaintData^.Parts.Count) then
      begin
        AddPart(LPosition, LEndPosition, ptNormal);
        exit;
      end
      else if (LPosition < APaintData^.Parts.List[LIndex].BeginPosition) then
      begin
        AddPart(LPosition, APaintData^.Parts.List[LIndex].BeginPosition, ptNormal);
        Inc(LIndex);
        LPosition := LinesPosition(APaintData^.Parts.List[LIndex].EndPosition.Char, APaintData^.Parts.List[LIndex].EndPosition.Line);
      end
      else
      begin
        LPosition := LinesPosition(APaintData^.Parts.List[LIndex].EndPosition.Char, APaintData^.Parts.List[LIndex].EndPosition.Line);
        Inc(LIndex);
      end;
  end;

var
  LAddon: TBCEditorTokenAddon;
  LAddOnColor: TColor;
  LBackgroundColor: TColor;
  LBorderColor: TColor;
  LBrush: TBrush;
  LChar: Integer;
  LCollapsedMarkRect: TRect;
  LCursorPosition: TBCEditorLinesPosition;
  LEffect: Longint;
  LFontStyles: TFontStyles;
  LForegroundColor: TColor;
  LHint: string;
  LIsLineBreakToken: Boolean;
  LIsTabToken: Boolean;
  LLeft: Integer;
  LLength: Integer;
  LLine: Integer;
  LOptions: Longint;
  LPartBackgroundColor: TColor;
  LPartForegroundColor: TColor;
  LPartIndex: Integer;
  LPartLength: Integer;
  LPartText: PChar;
  LRect: TRect;
  LSelArea: TBCEditorLinesArea;
  LSelLength: Integer;
  LSelStartAfter: Integer;
  LSelStartBefore: Integer;
  LStep: Integer;
  LText: PChar;
  LSize: TSize;
begin
  Result := False;

  LIsLineBreakToken := not Assigned(AText) and (ALinesPosition.Line < FLines.Count - 1);
  LIsTabToken := Assigned(AText) and (AText^ = BCEDITOR_TAB_CHAR);

  if (not LIsLineBreakToken) then
  begin
    LText := AText;
    LLength := ALength;
  end
  else if (FSpecialChars.Visible
    and (0 <= ARowsPosition.Row) and (ARowsPosition.Row < FRows.Count)
    and (rfLastRowOfLine in FRows.Items[ARowsPosition.Row].Flags)
    and (ALinesPosition.Line < FLines.Count - 1)) then
  begin
    LText := #182;
    LLength := 1;
  end
  else
  begin
    LText := #0;
    LLength := 0;
  end;

  if (Assigned(AToken) and Assigned(AToken^.Attribute)) then
    LFontStyles := AToken^.Attribute.FontStyles
  else
    LFontStyles := [];
  if (Assigned(LText)) then
    case (LText^) of
      BCEDITOR_NONE_CHAR:
        begin
          if (ALength > Length(FSpecialCharsNullText)) then
            if (FSpecialChars.Visible) then
              FSpecialCharsNullText := StringOfChar(Char(#127), ALength)
            else
              FSpecialCharsNullText := StringOfChar(#32, ALength);
          LText := PChar(FSpecialCharsNullText);
          if (FSpecialChars.Visible) then
            LFontStyles := LFontStyles - [fsBold];
        end;
      BCEDITOR_TAB_CHAR:
        begin
          if (FSpecialChars.Visible) then
            LText := #187
          else
            LText := #32;
          if (FSpecialChars.Visible) then
            LFontStyles := LFontStyles - [fsBold];
        end;
      BCEDITOR_LINEFEED,
      BCEDITOR_CARRIAGE_RETURN,
      BCEDITOR_SPACE_CHAR:
        begin
          if (ALength > Length(FSpecialCharsSpaceText)) then
            if (FSpecialChars.Visible) then
              FSpecialCharsSpaceText := StringOfChar(Char(#183), ALength)
            else
              FSpecialCharsSpaceText := StringOfChar(#32, ALength);
          LText := PChar(FSpecialCharsSpaceText);
          if (FSpecialChars.Visible) then
            LFontStyles := LFontStyles + [fsBold];
        end;
    end;
  FPaintHelper.Style := LFontStyles;

  LRect := ARect;
  if (not Assigned(AText)) then
    LRect.Right := ARect.Right
  else if (LIsTabToken) then
    LRect.Right := LRect.Left + (FTabs.Width - ARowsPosition.Column mod FTabs.Width) * FTabSignWidth
  else if (LLength = 0) then
    LRect.Right := LRect.Left
  else
    LRect.Right := LRect.Left + FPaintHelper.TextWidth(LText, LLength);

  if (not Assigned(ARange)) then
    LCollapsedMarkRect := Rect(-1, -1, -1, -1)
  else
  begin
    LCollapsedMarkRect := Rect(
      LRect.Left + FSpaceWidth,
      LRect.Top + GLineWidth,
      LRect.Left + FSpaceWidth + 2 * GLineWidth + FCodeFoldingCollapsedMarkWidth + 2 * GLineWidth,
      LRect.Bottom - GLineWidth);
    if (FSpecialChars.Visible) then
    begin
      Inc(LCollapsedMarkRect.Left, FLineBreakSignWidth);
      Inc(LCollapsedMarkRect.Right, FLineBreakSignWidth);
    end;
  end;

  if ((LRect.Right >= ARect.Left) and (LRect.Left <= ARect.Right)) then
    case (AAction) of
      paPaint:
        begin
          LEndPosition := LinesPosition(ALinesPosition.Char + LLength, ALinesPosition.Line);


          if (not Assigned(APaintData)) then
            LForegroundColor := clNone
          else if (APaintData^.LineForegroundColor <> clNone) then
            LForegroundColor := APaintData^.LineForegroundColor
          else if (FSpecialChars.Visible
            and (LIsLineBreakToken or Assigned(LText) and CharInSet(LText^, [BCEDITOR_NONE_CHAR, BCEDITOR_TAB_CHAR, BCEDITOR_LINEFEED, BCEDITOR_CARRIAGE_RETURN, BCEDITOR_SPACE_CHAR]))) then
            if (FSpecialChars.Color <> clNone) then
              LForegroundColor := FSpecialChars.Color
            else
              LForegroundColor := clSpecialChar
          else if (LIsLineBreakToken) then
            LForegroundColor := clNone
          else if (Assigned(AToken) and Assigned(AToken^.Attribute) and (AToken^.Attribute.Foreground <> clNone)) then
            LForegroundColor := AToken^.Attribute.Foreground
          else
            LForegroundColor := clWindowText;

          if (not Assigned(APaintData)) then
            LBackgroundColor := clNone
          else if (APaintData^.LineBackgroundColor <> clNone) then
            LBackgroundColor := APaintData^.LineBackgroundColor
          else if (ActiveLine.Visible
            and (ALinesPosition.Line = FLines.CaretPosition.Line)) then
            LBackgroundColor := ActiveLine.Color
          else if (LIsLineBreakToken) then
            LBackgroundColor := clWindow
          else if (Assigned(AToken) and Assigned(AToken^.Attribute) and (AToken^.Attribute.Background <> clNone)) then
            LBackgroundColor := AToken^.Attribute.Background
          else
            LBackgroundColor := clWindow;

          if (FSyncEdit.BlockSelected
            and (FSyncEdit.BlockArea.BeginPosition < FSyncEdit.BlockArea.EndPosition)) then
            ApplyPart(FSyncEdit.BlockArea, ptSyncEdit);

          if (Assigned(FSearchResults) and not FSearch.InSelection.Active
            and (APaintData^.SearchResultIndex < FSearchResults.Count)) then
            repeat
              if ((ALinesPosition <= FSearchResults[APaintData^.SearchResultIndex].BeginPosition)
                or (FSearchResults[APaintData^.SearchResultIndex].EndPosition < LEndPosition)) then
                ApplyPart(FSearchResults[APaintData^.SearchResultIndex], ptSearchResult);

              if (FSearchResults[APaintData^.SearchResultIndex].EndPosition <= LEndPosition) then
                Inc(APaintData^.SearchResultIndex)
              else
                break;
            until ((APaintData^.SearchResultIndex = FSearchResults.Count)
              or (FSearchResults[APaintData^.SearchResultIndex].BeginPosition > LEndPosition));

          if (FCurrentMatchingPair.FState = mpsFound) then
          begin
            ApplyPart(FCurrentMatchingPair.OpenTokenArea, ptMatchingPair);
            ApplyPart(FCurrentMatchingPair.CloseTokenArea, ptMatchingPair);
          end;

          if (not APaintData^.SelArea.IsEmpty()) then
            ApplyPart(APaintData^.SelArea, ptSelection);

          if (Assigned(FSearchResults) and FSearch.InSelection.Active
            and (APaintData^.SearchResultIndex < FSearchResults.Count)) then
            repeat
              if ((ALinesPosition <= FSearchResults[APaintData^.SearchResultIndex].BeginPosition)
                or (FSearchResults[APaintData^.SearchResultIndex].EndPosition < LEndPosition)) then
                ApplyPart(FSearchResults[APaintData^.SearchResultIndex], ptSearchResult);

              if (FSearchResults[APaintData^.SearchResultIndex].EndPosition <= LEndPosition) then
                Inc(APaintData^.SearchResultIndex)
              else
                break;
            until ((APaintData^.SearchResultIndex = FSearchResults.Count)
              or (FSearchResults[APaintData^.SearchResultIndex].BeginPosition > LEndPosition));

          if (APaintData^.Parts.Count > 0) then
            CompleteParts();


          LBorderColor := clNone;
          LAddon := taNone;
          LAddOnColor := clNone;
          LPartForegroundColor := LForegroundColor;
          LPartBackgroundColor := LBackgroundColor;

          LPartIndex := 0;
          repeat
            if (APaintData^.Parts.Count = 0) then
            begin
              LPartText := LText;
              LPartLength := LLength;
            end
            else
            begin
              if (LPartIndex > 0) then
                LRect.Left := LRect.Right;

              LPartText := @LText[APaintData^.Parts[LPartIndex].BeginPosition.Char - ALinesPosition.Char];
              LPartLength := APaintData^.Parts[LPartIndex].EndPosition.Char - APaintData^.Parts[LPartIndex].BeginPosition.Char;

              case (APaintData^.Parts[LPartIndex].PartType) of
                ptNormal:
                  begin
                    LPartForegroundColor := LForegroundColor;
                    LPartBackgroundColor := LBackgroundColor;
                  end;
                ptSyncEdit:
                  begin
                    LPartForegroundColor := LForegroundColor;
                    if (FSyncEdit.Colors.Background <> clNone) then
                      LPartBackgroundColor := FSyncEdit.Colors.Background
                    else
                      LPartBackgroundColor := clSyncEditBackground;
                  end;
                ptMatchingPair:
                  begin
                    LPartForegroundColor := LForegroundColor;
                    if (FMatchingPair.Colors.Matched <> clNone) then
                      LPartBackgroundColor := FMatchingPair.Colors.Matched
                    else
                      LPartBackgroundColor := LBackgroundColor;
                  end;
                ptSelection:
                  begin
                    if (not Focused() and HideSelection) then
                      LPartForegroundColor := clWindowText
                    else if (FSelection.Colors.Foreground <> clNone) then
                      LPartForegroundColor := FSelection.Colors.Foreground
                    else
                      LPartForegroundColor := clHighlightText;
                    if (not Focused() and HideSelection) then
                      LPartBackgroundColor := cl3DLight
                    else if (FSelection.Colors.Background <> clNone) then
                      LPartBackgroundColor := FSelection.Colors.Background
                    else
                      LPartBackgroundColor := clSelectionColor;
                  end;
                ptSearchResult:
                  begin
                    if (FSearch.Highlighter.Colors.Foreground <> clNone) then
                      LPartForegroundColor := FSearch.Highlighter.Colors.Foreground
                    else
                      LPartForegroundColor := LForegroundColor;
                    if (FSearch.Highlighter.Colors.Background <> clNone) then
                      LPartBackgroundColor := FSearch.Highlighter.Colors.Background
                    else
                      LPartBackgroundColor := LBackgroundColor;
                  end;
                ptSearchResultInSection:
                  begin
                    if (FSearch.InSelection.Background <> clNone) then
                      LPartBackgroundColor := FSearch.InSelection.Background
                    else if (FSearch.Highlighter.Colors.Background <> clNone) then
                      LPartBackgroundColor := FSearch.Highlighter.Colors.Background
                    else
                      LPartBackgroundColor := LBackgroundColor;
                  end;
                else raise ERangeError.Create('PartType: ' + IntToStr(Ord(APaintData^.Parts[LPartIndex].PartType)));
              end;

              if (LIsTabToken) then
                // Tab-Tokens have one part only - and they are computed before
              else if (LIsLineBreakToken) then
                // LineBreak-Tokens have one part only - and they are computed before
              else
                LRect.Right := LRect.Left + FPaintHelper.TextWidth(LPartText, LPartLength);
            end;

            FPaintHelper.ForegroundColor := LPartForegroundColor;
            FPaintHelper.BackgroundColor := LPartBackgroundColor;

            LLeft := LRect.Left;
            if (LRect.Left < FLeftMarginWidth) then
            begin
              LRect.Left := FLeftMarginWidth;
              LOptions := ETO_CLIPPED;
            end
            else
              LOptions := 0;

            if (LRect.Left <= LRect.Right) then
              if (LIsTabToken) then
                FPaintHelper.ExtTextOut(LLeft + (LRect.Width - FTabSignWidth) div 2, LRect.Top,
                  LOptions + ETO_OPAQUE, LRect, LPartText, LPartLength, nil)
              else if (LIsLineBreakToken or not Assigned(AText)) then
                FPaintHelper.ExtTextOut(LLeft, LRect.Top,
                  LOptions + ETO_OPAQUE, LRect, LPartText, LPartLength, nil)
              else if (not (fsItalic in LFontStyles)) then
                FPaintHelper.ExtTextOut(LLeft, LRect.Top,
                  LOptions + ETO_OPAQUE, LRect, LPartText, LPartLength, nil)
              else if (not (fsItalic in APaintData^.Previous.FontStyles)
                or (LPartBackgroundColor <> APaintData^.Previous.BackgroundColor)) then
                FPaintHelper.ExtTextOut(LLeft, LRect.Top,
                  LOptions + ETO_OPAQUE, Rect(LRect.Left, LRect.Top, ARect.Right, LRect.Bottom), LPartText, LPartLength, nil)
              else
                FPaintHelper.ExtTextOut(LRect.Left, LRect.Top,
                  LOptions, LRect, LPartText, LPartLength, nil);

            APaintData^.Previous.BackgroundColor := LPartBackgroundColor;

            Inc(LPartIndex);
          until ((APaintData^.Parts.Count = 0) or (LPartIndex = APaintData^.Parts.Count));

          APaintData^.Previous.FontStyles := LFontStyles;
          APaintData^.Parts.Clear();

    //      if (LBorderColor <> clNone) then
    //      begin
    //        LOldPenColor := Canvas.Pen.Color;
    //        Canvas.Pen.Color := LBorderColor;
    //        Canvas.Rectangle(ARect);
    //        Canvas.MoveTo(ARect.Left, ARect.Bottom - 1);
    //        Canvas.LineTo(ARect.Right - 1, ARect.Bottom - 1);
    //        Canvas.LineTo(ARect.Right - 1, ARect.Top);
    //        Canvas.LineTo(ARect.Left, ARect.Top);
    //        Canvas.LineTo(ARect.Left, ARect.Bottom - 1);
    //        Canvas.Pen.Color := LOldPenColor;
    //      end;
    //
    //      if (LAddon <> taNone) then
    //      begin
    //        LOldPenColor := Canvas.Pen.Color;
    //        if (LAddOnColor <> clNone) then
    //          Canvas.Pen.Color := LAddOnColor
    //        else
    //          Canvas.Pen.Color := LForegroundColor;
    //        case (LAddon) of
    //          taDoubleUnderline,
    //          taUnderline:
    //            begin
    //              if (LAddon = taDoubleUnderline) then
    //              begin
    //                Canvas.MoveTo(ARect.Left, ARect.Bottom - 3);
    //                Canvas.LineTo(ARect.Right, ARect.Bottom - 3);
    //              end;
    //              Canvas.MoveTo(ARect.Left, ARect.Bottom - 1);
    //              Canvas.LineTo(ARect.Right, ARect.Bottom - 1);
    //            end;
    //          taWaveLine:
    //            begin
    //              LStep := 0;
    //              while LStep < ARect.Right - 4 do
    //              begin
    //                Canvas.MoveTo(ARect.Left + LStep, ARect.Bottom - 3);
    //                Canvas.LineTo(ARect.Left + LStep + 2, ARect.Bottom - 1);
    //                Canvas.LineTo(ARect.Left + LStep + 4, ARect.Bottom - 3);
    //                Inc(LStep, 4);
    //              end;
    //            end;
    //        end;
    //        Canvas.Pen.Color := LOldPenColor;
    //      end;

          if (Assigned(ARange)
            and (LCollapsedMarkRect.Right >= ARect.Left)
            and (LCollapsedMarkRect.Left < ARect.Right)) then
          begin
            FPaintHelper.ForegroundColor := FCodeFolding.Colors.Foreground;
            LBrush := TBrush.Create();
            LBrush.Color := FCodeFolding.Colors.Foreground;
            FrameRect(Canvas.Handle, LCollapsedMarkRect, LBrush.Handle);
            LBrush.Free();
            FPaintHelper.ExtTextOut(LCollapsedMarkRect.Left, LCollapsedMarkRect.Top,
              0, LCollapsedMarkRect, BCEDITOR_CODEFOLDING_COLLAPSEDMARK, Length(BCEDITOR_CODEFOLDING_COLLAPSEDMARK), nil);
          end;
        end;
      paMouseDown,
      paMouseMove,
      paMouseUp,
      paHint:
        if ((LRect.Left <= X) and (X < LRect.Right)
          and (MouseCapture in [mcNone, mcAll, mcText])) then
        begin
          LLine := ALinesPosition.Line;
          if (not Assigned(AText)) then
          begin
            if (not (soBeyondEndOfFile in FScroll.Options) and (ALinesPosition.Line >= FLines.Count)) then
              LLine := Max(0, FLines.Count - 1);
            if (not (soBeyondEndOfLine in FScroll.Options)) then
              LChar := 0
            else
              LChar := (X + FSpaceWidth div 2 - LRect.Left) div FSpaceWidth;
          end
          else if (LIsTabToken) then
            if (X <= LRect.Left + (LRect.Right - LRect.Left) div 2) then
              LChar := 0
            else
              LChar := 1
          else
          begin
            LChar := 1;
            while (X >= LRect.Left + FPaintHelper.TextWidth(LText, LChar)) do
              Inc(LChar);
            if (X <= LRect.Left + FPaintHelper.TextWidth(LText, LChar - 1) + FPaintHelper.TextWidth(@LText[LChar - 1], 1) div 2) then
              Dec(LChar);
          end;

          LCursorPosition := LinesPosition(ALinesPosition.Char + LChar, LLine);
          case (AAction) of
            paMouseDown:
              if (AButton = mbLeft) then
                if (FLines.SelArea.Containts(LCursorPosition)) then
                begin
                  Include(FState, esWaitForDrag);
                  FLastDoubleClickTime := 0;
                end
                else
                begin
                  if (LCollapsedMarkRect.Contains(Point(X, Y))) then
                    MoveCaretAndSelection(FLines.SelArea.BeginPosition, FLines.EOLPosition[ALinesPosition.Line],
                      (ssShift in AShift))
                  else
                    MoveCaretAndSelection(FLines.SelArea.BeginPosition, LCursorPosition,
                      (ssShift in AShift));
                  MouseCapture := mcText;
                end;
            paMouseMove:
              begin
                if (LCollapsedMarkRect.Contains(Point(X, Y))) then
                  Cursor := crDefault
                else
                  Cursor := crIBeam;
                if (AShift * [ssLeft, ssRight, ssMiddle] = [ssLeft]) then
                  if (not (esWaitForDrag in FState)) then
                  begin
                    if (not (esMouseDouble in FState)) then
                      MoveCaretAndSelection(FLines.SelArea.BeginPosition, LCursorPosition,
                        (ssLeft in AShift));
                  end
                  else if ((Abs(FMouseDownPoint.X - X) >= GetSystemMetrics(SM_CXDRAG))
                    or (Abs(FMouseDownPoint.Y - Y) >= GetSystemMetrics(SM_CYDRAG))) then
                  begin
                    Exclude(FState, esWaitForDrag);
                    Include(FState, esDragging);
                    try
                      LSelStartBefore := SelStart;
                      LSelLength := SelLength;
                      if (Succeeded(DoDragDrop(TDropData.Create(Self), Self, DROPEFFECT_COPY or DROPEFFECT_MOVE, LEffect))
                        and (LEffect = DROPEFFECT_MOVE)) then
                      begin
                        LSelStartAfter := SelStart;
                        BeginUpdate();
                        try
                          if (LSelStartBefore < LSelStartAfter) then
                          begin
                            LSelArea.BeginPosition := FLines.PositionOf(LSelStartBefore);
                            LSelArea.EndPosition := FLines.PositionOf(LSelLength, LSelArea.BeginPosition);
                            FLines.DeleteText(LSelArea);
                            LSelArea.BeginPosition := FLines.PositionOf(LSelStartAfter - LSelLength);
                            LSelArea.EndPosition := FLines.PositionOf(LSelLength, LSelArea.BeginPosition);
                            FLines.SelArea := LSelArea;
                          end
                          else
                          begin
                            LSelArea := FLines.SelArea;
                            LSelArea.BeginPosition := FLines.PositionOf(LSelStartBefore + LSelLength);
                            LSelArea.EndPosition := FLines.PositionOf(LSelLength, LSelArea.BeginPosition);
                            FLines.DeleteText(LSelArea);
                            FLines.SelArea := LSelArea;
                          end;
                        finally
                          EndUpdate();
                        end;
                      end;
                    finally
                      Exclude(FState, esDragging);
                    end;
                  end;
              end;
            paMouseUp:
              if (LCollapsedMarkRect.Contains(Point(X, Y))) then
              else
              begin
                if (AButton = mbRight) then
                begin
                  FLines.CaretPosition := LCursorPosition;
                  if (Assigned(PopupMenu)) then
                    PopupMenu.Popup(X, Y);
                end;
                if (FCodeFolding.Visible) then
                  CheckIfAtMatchingKeywords();
              end;
            paHint:
              if (LCollapsedMarkRect.Contains(Point(X, Y))) then
                ActivateHint(X, Y + FLineHeight,
                  Format(SBCEditorCodeFoldingCollapsedMark, [ARange.EndLine - ARange.BeginLine]))
              else if (Assigned(FOnHint)) then
              begin
                FOnHint(Self,
                  LRect.Left, LRect.Top + FLineHeight,
                  Point(ALinesPosition.Char + LChar, ALinesPosition.Line),
                  FLines.CharIndexOf(LinesPosition(ALinesPosition.Char + LChar, ALinesPosition.Line)),
                  LHint);
                Result := LHint <> '';
                if (Result) then
                  ActivateHint(LRect.Left, LRect.Top + FLineHeight, LHint);
              {$IFDEF Nils}
              end
              else if (Assigned(AToken)) then
              begin
                LHint := 'Position: ' + LCursorPosition.ToString() + #10;
                LHint := LHint + 'Area: ' + ALinesPosition.ToString() + ' - ' + LinesPosition(ALinesPosition.Char + ALength - 1, ALinesPosition.Line).ToString() + #10;
                if (Assigned(AToken.Attribute) and (AToken.Attribute.Element <> BCEDITOR_ATTRIBUTE_ELEMENT_EDITOR)) then
                  LHint := LHint + 'Element: ' + AToken.Attribute.Element + #10;
                if (FLines.ValidPosition(LCursorPosition) and IsWordBreakChar(FLines.Char[LCursorPosition])) then
                  LHint := LHint + 'IsWordBreakChar: True' + #10;
                LHint := Trim(LHint);
                if (LHint <> '') then
                  ActivateHint(LRect.Left, LRect.Top + FLineHeight, Trim(LHint));
              {$ENDIF}
              end;
          end;
          Result := True;
        end;
      paMouseDouble:
        if ((LRect.Left <= X) and (X < LRect.Right)) then
        begin
          if (AButton = mbLeft) then
            if (LCollapsedMarkRect.Contains(Point(X, Y))) then
            begin
              ExpandCodeFoldingRange(ARange);
              FLines.CaretPosition := FLines.EOLPosition[ALinesPosition.Line];
              FLastDoubleClickTime := 0;
            end
            else
              SetWordBlock(ALinesPosition);
          Result := True;
        end;
    end;

  if ((FState * [esCaretInvalid] <> [])
    and (ALinesPosition.Line = FLines.CaretPosition.Line)
    and (ALinesPosition.Char <= FLines.CaretPosition.Char) and (FLines.CaretPosition.Char < ALinesPosition.Char + ALength)) then
  begin
    LLength := FLines.CaretPosition.Char - ALinesPosition.Char;
    if (LLength = 0) then
      FCaretPos := Point(ARect.Left, ARect.Top)
    else
      FCaretPos := Point(ARect.Left + FPaintHelper.TextWidth(LText, LLength), ARect.Top);
  end;

  if (Assigned(ARange)) then
    ARect.Left := Max(ARect.Left, LCollapsedMarkRect.Right)
  else if (not LIsLineBreakToken) then
    ARect.Left := Max(ARect.Left, LRect.Right)
  else if (FSpecialChars.Visible) then
    ARect.Left := Max(ARect.Left, LRect.Left + FLineBreakSignWidth);
end;

procedure TCustomBCEditor.PasteFromClipboard();
begin
  CommandProcessor(ecPaste, BCEDITOR_NONE_CHAR, nil);
end;

function TCustomBCEditor.PosToCharIndex(const APos: TPoint): Integer;
begin
  Result := FLines.CharIndexOf(LinesPosition(APos));
end;

function TCustomBCEditor.PreviousWordPosition(const ALinesPosition: TBCEditorLinesPosition): TBCEditorLinesPosition;
begin
  if (ALinesPosition.Line < FLines.Count) then
    Result := Min(ALinesPosition, FLines.EOLPosition[ALinesPosition.Line])
  else
    Result := FLines.EOFPosition;

  if (Result.Char > 0) then
    while ((Result.Char > 0) and IsWordBreakChar(FLines.Items[Result.Line].Text[1 + Result.Char - 1])) do
      Dec(Result.Char)
  else if (Result.Line > 0) then
    Result := FLines.EOLPosition[Result.Line - 1]
  else
    Result := FLines.BOFPosition;
end;

function TCustomBCEditor.Process(const AAction: TProcessAction;
  const AButton: TMouseButton; const AShift: TShiftState; const X, Y: Integer): Boolean;

  function ProcessMarks(var ARect: TRect; const ALine, ARow: Integer): Boolean;
  var
    LBookmark: TBCEditorMark;
    LIndex: Integer;
    LLeft: Integer;
    LMark: TBCEditorMark;
    LRect: TRect;
  begin
    Result := False;

    LRect := ARect;
    LRect.Right := LRect.Left + FMarksPanelWidth;

    case (AAction) of
      paPaint:
        begin
          if (FLeftMargin.Colors.BookmarkPanelBackground <> clNone) then
            FPaintHelper.BackgroundColor := FLeftMargin.Colors.BookmarkPanelBackground
          else if (FLeftMargin.Colors.Background <> clNone) then
            FPaintHelper.BackgroundColor := FLeftMargin.Colors.Background
          else
            FPaintHelper.BackgroundColor := Color;
          FPaintHelper.FillRect(LRect);

          if ((ARow < FRows.Count)
            and (rfFirstRowOfLine in FRows.Items[ARow].Flags)) then
          begin
            LLeft := LRect.Left;

            LBookmark := nil;
            for LIndex := FBookmarkList.Count - 1 downto 0 do
              if (FBookmarkList[LIndex].Line = ALine) then
                LBookmark := FBookmarkList[LIndex];
            if (Assigned(LBookmark)) then
              case (AAction) of
                paPaint:
                  FImages.Draw(Canvas, LLeft, LRect.Top, iiBookmark0 + LBookmark.Index);
              end;

            LMark := nil;
            for LIndex := FMarkList.Count - 1 downto 0 do
              if (FMarkList[LIndex].Line = ALine) then
                LBookmark := FMarkList[LIndex];
            if (Assigned(LMark)) then
            begin
              if (Assigned(LBookmark)) then
                Inc(LLeft, GetSystemMetrics(SM_CXSMICON) div 4);

              if (Assigned(LMark)) then
                case (AAction) of
                  paPaint:
                    FLeftMargin.Marks.Images.Draw(Canvas, LLeft, LRect.Top, LMark.ImageIndex);
                end;
            end;
          end;
        end;
      paMouseDown:
        if ((AButton = mbLeft)
          and LRect.Contains(Point(X, Y))
          and (MouseCapture in [mcNone, mcAll, mcMarks])) then
        begin
          MouseCapture := mcMarks;
          Result := True;
        end;
      paMouseMove:
        if (LRect.Contains(Point(X, Y))
          and (MouseCapture in [mcNone, mcAll, mcMarks])) then
        begin
          MouseCapture := mcMarks;
          Cursor := crDefault;
          Result := True;
        end
        else if (MouseCapture = mcMarks) then
        begin
          if (not LRect.Contains(Point(X, Y))) then
            MouseCapture := mcNone;
          Result := True;
        end;
      paMouseUp:
        if ((AButton = mbLeft)
          and LRect.Contains(Point(X, Y))
          and (MouseCapture in [mcNone, mcAll, mcMarks])
          and (ALine <> -1)) then
        begin
          if (FLeftMargin.Bookmarks.Visible and (bpoToggleBookmarkByClick in FLeftMargin.MarksPanel.Options)) then
            DoToggleBookmark(FLines.BOLPosition[ALine])
          else if (FLeftMargin.Marks.Visible and (bpoToggleMarkByClick in FLeftMargin.MarksPanel.Options)) then
            DoToggleMark(FLines.BOLPosition[ALine]);
          MouseCapture := mcNone;
          Result := True;
        end;
    end;

    ARect.Left := LRect.Right;
  end;

  function ProcessLineNumber(var ARect: TRect; const ALine, ARow: Integer): Boolean;
  var
    LRect: TRect;
    LText: string;
  begin
    Result := False;

    LRect := ARect;
    LRect.Right := LRect.Left + FLineNumbersWidth;

    case (AAction) of
      paPaint:
        begin
          if (FLeftMargin.Colors.Foreground <> clNone) then
            FPaintHelper.ForegroundColor := FLeftMargin.Colors.Foreground
          else
            FPaintHelper.ForegroundColor := Font.Color;
          if (FLeftMargin.Colors.Background <> clNone) then
            FPaintHelper.BackgroundColor := FLeftMargin.Colors.Background
          else
            FPaintHelper.BackgroundColor := Color;

          if ((ALine < 0) and not (lnoAfterLastLine in FLeftMargin.LineNumbers.Options)
            or (0 <= ARow) and (ARow < FRows.Count) and not (rfFirstRowOfLine in FRows.Items[ARow].Flags)) then
          begin
            LText := '';
            FPaintHelper.Style := [];
          end
          else if (((FRows.Count = 0) or (rfFirstRowOfLine in FRows.Items[ARow].Flags))
            and ((ALine = 0)
              or (ALine = FLines.CaretPosition.Line)
              or ((ALine + 1) mod 10 = 0)
              or not (lnoIntens in FLeftMargin.LineNumbers.Options))) then
          begin
            LText := IntToStr(ALine + FLeftMargin.LineNumbers.StartFrom);
            FPaintHelper.Style := [];
          end
          else if ((ALine + 1) mod 5 = 0) then
          begin
            LText := '-';
            FPaintHelper.Style := [];
          end
          else
          begin
            LText := #183;
            FPaintHelper.Style := [fsBold];
          end;

          FPaintHelper.ExtTextOut(
            LRect.Right - FPaintHelper.TextWidth(PChar(LText), Length(LText)) - GPadding,
            LRect.Top,
            ETO_OPAQUE, LRect, PChar(LText), Length(LText), nil);
        end;
      paMouseMove:
        if (LRect.Contains(Point(X, Y))
          and (MouseCapture in [mcNone, mcAll, mcLineNumbers])) then
        begin
          Cursor := crDefault;
          Result := True;
        end;
    end;

    ARect.Left := LRect.Right;
  end;

  function ProcessLineState(var ARect: TRect; const ALine, ARow: Integer): Boolean;
  var
    LRect: TRect;
  begin
    Result := False;

    LRect := ARect;
    LRect.Right := LRect.Left + FLineStateWidth;

    case (AAction) of
      paPaint:
        begin
          if (ARow < FRows.Count) then
            case (FLines.Items[ALine].State) of
              lsModified:
                if (FLeftMargin.Colors.LineStateModified <> clNone) then
                  FPaintHelper.BackgroundColor := FLeftMargin.Colors.LineStateModified
                else if (FLeftMargin.Colors.Background <> clNone) then
                  FPaintHelper.BackgroundColor := FLeftMargin.Colors.Background
                else
                  FPaintHelper.BackgroundColor := Color;
              lsSaved:
                if (FLeftMargin.Colors.LineStateNormal <> clNone) then
                  FPaintHelper.BackgroundColor := FLeftMargin.Colors.LineStateNormal
                else if (FLeftMargin.Colors.Background <> clNone) then
                  FPaintHelper.BackgroundColor := FLeftMargin.Colors.Background
                else
                  FPaintHelper.BackgroundColor := Color;
              else
                if (FLeftMargin.Colors.Background <> clNone) then
                  FPaintHelper.BackgroundColor := FLeftMargin.Colors.Background
                else
                  FPaintHelper.BackgroundColor := Color;
            end
          else
            if (FLeftMargin.Colors.Background <> clNone) then
              FPaintHelper.BackgroundColor := FLeftMargin.Colors.Background
            else
              FPaintHelper.BackgroundColor := Color;
          FPaintHelper.FillRect(LRect);
        end;
      paMouseMove:
        if (LRect.Contains(Point(X, Y))
          and (MouseCapture in [mcNone, mcAll, mcLineState])) then
        begin
          Cursor := crDefault;
          Result := True;
        end;
    end;

    ARect.Left := LRect.Right;
  end;

  function ProcessCodeFolding(var ARect: TRect; const ALine, ARow: Integer): Boolean;
  var
    LRange: TBCEditorCodeFolding.TRanges.TRange;
    LRect: TRect;
  begin
    Result := False;

    LRect := ARect;
    LRect.Right := LRect.Left + FCodeFoldingWidth;

    if (AAction = paPaint) then
    begin
      if (FCodeFolding.Colors.Background <> clNone) then
        FPaintHelper.BackgroundColor := FCodeFolding.Colors.Background
      else
        FPaintHelper.BackgroundColor := Color;
      FPaintHelper.FillRect(LRect);
    end;

    if (AAction = paMouseMove) then
    begin
      if (LRect.Contains(Point(X, Y))
        and (MouseCapture in [mcNone, mcAll, mcCodeFolding])) then
      begin
        Cursor := crDefault;
        Result := True;
      end;
    end
    else if (ALine >= 0) then
    begin
      LRange := CodeFoldingCollapsableFoldRangeForLine(ALine);

      if (not Assigned(LRange) and (cfoShowTreeLine in FCodeFolding.Options)) then
      begin
        if (FLines.Items[ALine].CodeFolding.TreeLine) then
          case (AAction) of
            paPaint:
              FImages.Draw(Canvas, LRect.Left, LRect.Top, iiCodeFoldingLine);
          end
        else if (Assigned(FLines.Items[ALine].CodeFolding.EndRange)) then
          case (AAction) of
            paPaint:
              FImages.Draw(Canvas, LRect.Left, LRect.Top, iiCodeFoldingEndLine);
          end;
      end
      else if (Assigned(LRange) and LRange.Collapsable) then
      begin
        if (not LRange.Collapsed) then
          case (AAction) of
            paPaint:
              FImages.Draw(Canvas, LRect.Left, LRect.Top, iiCodeFoldingExpanded);
            paMouseDown:
              if ((AButton = mbLeft)
                and LRect.Contains(Point(X, Y))
                and (MouseCapture in [mcNone, mcAll, mcCodeFolding])) then
              begin
                CollapseCodeFoldingRange(LRange);
                Result := True;
              end;
          end
        else
          case (AAction) of
            paPaint:
              FImages.Draw(Canvas, LRect.Left, LRect.Top, iiCodeFoldingCollapsed);
            paMouseDown:
              if ((AButton = mbLeft)
                and LRect.Contains(Point(X, Y))
                and (MouseCapture in [mcNone, mcAll, mcCodeFolding])) then
              begin
                ExpandCodeFoldingRange(LRange);
                Result := True;
              end;
          end;
      end;
    end;

    ARect.Left := LRect.Right;
  end;

  function ProcessSyncEditButton(): Boolean;
  var
    LRect: TRect;
    LRow: Integer;
  begin
    Result := False;

    if (not FSyncEdit.Active and SelectionAvailable) then
      LRow := LinesToRows(SelectionEndPosition).Row
    else if (FSyncEdit.BlockSelected) then
    begin
      LRow := LinesToRows(FSyncEdit.BlockArea.BeginPosition).Row;
      LRow := Max(LRow, TopRow);
      LRow := Min(LRow, TopRow + VisibleRows);
    end
    else
      LRow := -1;

    if (LRow <> -1) then
    begin
      LRect.Left := 2 * GetSystemMetrics(SM_CXEDGE);
      LRect.Top := (LRow - TopRow) * LineHeight;
      LRect.Right := LRect.Left + FSyncEditButtonNormal.Width;
      LRect.Bottom := LRect.Top + FSyncEditButtonNormal.Height;

      case (AAction) of
        paPaint:
          if (not FSyncEdit.BlockSelected) then
            Canvas.Draw(LRect.Left, LRect.Top, FSyncEditButtonNormal.Bitmap)
          else
            Canvas.Draw(LRect.Left, LRect.Top, FSyncEditButtonPushed.Bitmap);
        paMouseDown:
          if ((AButton = mbLeft)
            and LRect.Contains(Point(X, Y))
            and (MouseCapture in [mcNone, mcAll, mcSyncEditButton])) then
          begin
            if (not FSyncEdit.BlockSelected) then
              Canvas.Draw(LRect.Left, LRect.Top, FSyncEditButtonPushed.Bitmap)
            else
              Canvas.Draw(LRect.Left, LRect.Top, FSyncEditButtonPushed.Bitmap);
            MouseCapture := mcSyncEditButton;
            Result := True;
          end;
        paMouseMove:
          if (LRect.Contains(Point(X, Y))
            and (MouseCapture in [mcNone, mcAll, mcSyncEditButton])) then
          begin
            if (MouseCapture <> mcSyncEditButton) then
            begin
              if (not FSyncEdit.BlockSelected) then
                Canvas.Draw(LRect.Left, LRect.Top, FSyncEditButtonHot.Bitmap)
              else
                Canvas.Draw(LRect.Left, LRect.Top, FSyncEditButtonPushed.Bitmap);
              MouseCapture := mcSyncEditButton;
            end;
          end
          else if (MouseCapture = mcSyncEditButton) then
          begin
            if (not FSyncEdit.BlockSelected) then
              Canvas.Draw(LRect.Left, LRect.Top, FSyncEditButtonNormal.Bitmap)
            else
              Canvas.Draw(LRect.Left, LRect.Top, FSyncEditButtonPushed.Bitmap);
            if (not LRect.Contains(Point(X, Y))) then
              MouseCapture := mcNone;
          end;
        paMouseUp:
          if ((AButton = mbLeft)
            and LRect.Contains(Point(X, Y))
            and (MouseCapture in [mcNone, mcAll, mcSyncEditButton])) then
          begin
            FSyncEdit.Active := not FSyncEdit.BlockSelected;
            MouseCapture := mcNone;
            Result := True;
          end;
      end;
    end;
  end;

var
  LColumn: Integer;
  LLine: Integer;
  LPaintTokenData: TPaintTokenData;
  LRange: TBCEditorCodeFolding.TRanges.TRange;
  LRect: TRect;
  LRow: Integer;
  LToken: TBCEditorHighlighter.TFind;
begin
  Result := False;

  if (LineHeight > 0) then
  begin
    LPaintTokenData.LineForegroundColor := clNone;
    LPaintTokenData.LineBackgroundColor := clNone;
    LPaintTokenData.Parts := TList<TPaintTokenData.TPart>.Create();
    LPaintTokenData.Previous.BackgroundColor := clNone;
    LPaintTokenData.Previous.FontStyles := [];
    LPaintTokenData.SearchResultIndex := 0;
    LPaintTokenData.SelArea.BeginPosition := Min(FLines.SelArea.BeginPosition, FLines.SelArea.EndPosition);
    LPaintTokenData.SelArea.EndPosition := Max(FLines.SelArea.BeginPosition, FLines.SelArea.EndPosition);

    FPaintHelper.BeginDrawing(Canvas);
    try
      if ((AAction <> paPaint)
        and FSyncEdit.Enabled) then
        Result := Result or ProcessSyncEditButton();

      for LRow := TopRow to TopRow + VisibleRows + 1 do
        if ((AAction = paPaint)
          or (Y div FLineHeight = LRow)) then
        begin
          if (LRow < FRows.Count) then
            LLine := FRows.Items[LRow].Line
          else
            LLine := -1;

          LPaintTokenData.Previous.FontStyles := [];

          LRect := Rect(
            0, (LRow - FTopRow) * FLineHeight,
            ClientWidth, (LRow - FTopRow + 1) * FLineHeight);

          if (FLeftMargin.MarksPanel.Visible) then
            Result := Result or ProcessMarks(LRect, LLine, LRow);

          if (FLeftMargin.LineNumbers.Visible) then
            Result := Result or ProcessLineNumber(LRect, LLine, LRow);

          if (FLeftMargin.LineState.Visible) then
            Result := Result or ProcessLineState(LRect, LLine, LRow);

          if (FCodeFolding.Visible) then
            Result := Result or ProcessCodeFolding(LRect, LLine, LRow);

          Dec(LRect.Left, FHorzTextPos);

          if (not Result) then
            if (LRow >= FRows.Count) then
              Result := ProcessToken(AAction, AButton, AShift, X, Y, LRect,
                FRows.BORPosition[LRow], RowsPosition(0, LRow),
                nil, 0, nil, nil,
                @LPaintTokenData)
            else if (AAction = paMouseTriple) then
            begin
              if (soTripleClickLineSelect in FSelection.Options) then
              begin
                FLines.SelArea := FRows.RowArea[LRow];
                FLastDoubleClickTime := 0;
                Result := True;
              end;
            end
            else
            begin
              if (AAction = paPaint) then
              begin
                if ((LLine >= FLines.Count) or (FLines.Items[LLine].Foreground = clNone)) then
                  LPaintTokenData.LineForegroundColor := clNone
                else
                  LPaintTokenData.LineForegroundColor := FLines.Items[LLine].Foreground;
                if ((LLine >= FLines.Count) or (FLines.Items[LLine].Background = clNone)) then
                  LPaintTokenData.LineBackgroundColor := clNone
                else
                  LPaintTokenData.LineBackgroundColor := FLines.Items[LLine].Background;
              end;

              LColumn := 0;
              if (FHighlighter.FindFirstToken(FRows.Items[LRow].BeginRange, FRows[LRow], LToken)) then
                repeat
                  Result := Result or ProcessToken(AAction, AButton, AShift, X, Y, LRect,
                    LinesPosition(FRows.Items[LRow].Char + LToken.Char, LLine),
                    RowsPosition(LColumn, LRow),
                    LToken.Text, LToken.Length,
                    @LToken,
                    nil,
                    @LPaintTokenData);

                  if (LToken.Text^ <> BCEDITOR_TAB_CHAR) then
                    Inc(LColumn, LToken.Length)
                  else
                    LColumn := FTabs.Width - LColumn mod FTabs.Width;
                until ((LRect.Left > ClientWidth)
                  or not FHighlighter.FindNextToken(LToken));

              if (LRect.Left <= ClientWidth) then
              begin
                if (not FCodeFolding.Visible
                  or not (rfLastRowOfLine in FRows.Items[LRow].Flags)) then
                  LRange := nil
                else
                begin
                  LRange := CodeFoldingCollapsableFoldRangeForLine(LLine);
                  if (Assigned(LRange) and (not LRange.Collapsed or LRange.ParentCollapsed)) then
                    LRange := nil;
                end;
                Result := Result or ProcessToken(AAction, AButton, AShift, X, Y, LRect,
                  FRows.EORPosition[LRow], RowsPosition(FRows.Items[LRow].Length, LRow),
                  nil, 0, nil, LRange,
                  @LPaintTokenData);
              end;
            end;

          LRect.Left := 0;
          LRect.Right := Width;
        end;

      if ((AAction = paPaint)
        and FSyncEdit.Enabled) then
        Result := Result or ProcessSyncEditButton();

      if (FSyncEdit.Enabled) then
        PaintSyncItems();

      if FMouseMoveScrolling then
        PaintMouseMoveScrollPoint;
    finally
      FPaintHelper.EndDrawing();
    end;

    LPaintTokenData.Parts.Free();

    FOldSelectionAvailable := SelectionAvailable;
  end;
end;

function TCustomBCEditor.QueryContinueDrag(fEscapePressed: BOOL; grfKeyState: Longint): HResult;
begin
  if (fEscapePressed) then
    Result := DRAGDROP_S_CANCEL
  else if (grfKeyState and MK_LBUTTON = 0) then
    Result := DRAGDROP_S_DROP
  else
    Result := S_OK;
end;

procedure TCustomBCEditor.ReadState(Reader: TReader);
begin
  inherited;

  if (eoTrimTrailingLines in Options) then
    FLines.Options := FLines.Options + [loTrimTrailingLines]
  else
    FLines.Options := FLines.Options - [loTrimTrailingLines];
  if (eoTrimTrailingSpaces in Options) then
    FLines.Options := FLines.Options + [loTrimTrailingSpaces]
  else
    FLines.Options := FLines.Options - [loTrimTrailingSpaces];
end;

procedure TCustomBCEditor.Redo();
begin
  FLines.Redo();
end;

procedure TCustomBCEditor.RegisterCommandHandler(const AHookedCommandEvent: TBCEditorHookedCommandEvent;
  AHandlerData: Pointer);
begin
  if not Assigned(AHookedCommandEvent) then
    Exit;
  if not Assigned(FHookedCommandHandlers) then
    FHookedCommandHandlers := TObjectList.Create;
  if FindHookedCommandEvent(AHookedCommandEvent) = -1 then
    FHookedCommandHandlers.Add(TBCEditorHookedCommandHandler.Create(AHookedCommandEvent, AHandlerData))
end;

procedure TCustomBCEditor.RemoveChainedEditor;
begin
  if Assigned(FChainedEditor) then
    RemoveFreeNotification(FChainedEditor);
  FChainedEditor := nil;

  UnhookEditorLines;
end;

procedure TCustomBCEditor.RemoveKeyDownHandler(AHandler: TKeyEvent);
begin
  FKeyboardHandler.RemoveKeyDownHandler(AHandler);
end;

procedure TCustomBCEditor.RemoveKeyPressHandler(AHandler: TBCEditorKeyPressWEvent);
begin
  FKeyboardHandler.RemoveKeyPressHandler(AHandler);
end;

procedure TCustomBCEditor.RemoveKeyUpHandler(AHandler: TKeyEvent);
begin
  FKeyboardHandler.RemoveKeyUpHandler(AHandler);
end;

procedure TCustomBCEditor.RemoveMouseCursorHandler(AHandler: TBCEditorMouseCursorEvent);
begin
  FKeyboardHandler.RemoveMouseCursorHandler(AHandler);
end;

procedure TCustomBCEditor.RemoveMouseDownHandler(AHandler: TMouseEvent);
begin
  FKeyboardHandler.RemoveMouseDownHandler(AHandler);
end;

procedure TCustomBCEditor.RemoveMouseUpHandler(AHandler: TMouseEvent);
begin
  FKeyboardHandler.RemoveMouseUpHandler(AHandler);
end;

procedure TCustomBCEditor.ReplaceChanged(AEvent: TBCEditorReplaceChanges);
begin
  case AEvent of
    rcEngineUpdate:
      begin
        FLines.CaretPosition := FLines.BOFPosition;
        FSearch.Engine := FReplace.Engine;
      end;
  end;
end;

function TCustomBCEditor.ReplaceText(): Integer;
begin
  if (SelectionAvailable and (roSelectedOnly in Replace.Options)) then
    Replace.Area := FLines.SelArea
  else if (roEntireScope in FReplace.Options) then
    Replace.Area := FLines.Area
  else if (roBackwards in FReplace.Options) then
    Replace.Area := LinesArea(FLines.BOFPosition, FLines.CaretPosition)
  else
    Replace.Area := LinesArea(FLines.CaretPosition, FLines.EOFPosition);

  Result := DoReplaceText();
end;

procedure TCustomBCEditor.Resize();
begin
  inherited;

  FState := FState + [esSizeChanged, esScrolled];
end;

function TCustomBCEditor.RowsToClient(ARowsPosition: TBCEditorRowsPosition): TPoint;
var
  LCharColumns: Integer;
  LColumn: Integer;
  LEOL: Boolean;
  LLinePos: PChar;
  LRow: Integer;
  LRowColumns: Integer;
  LRowPos: Integer;
  LToken: TBCEditorHighlighter.TFind;
  LTokenColumns: Integer;
  LTokenWidth: Integer;
begin
  if ((FRows.Count = 0)
    or (ARowsPosition.Column = 0)
    or (ARowsPosition.Row >= FRows.Count)
    or (FRows.Items[ARowsPosition.Row].Length = 0)) then
    Result := Point(FLeftMarginWidth + ARowsPosition.Column * FSpaceWidth - HorzTextPos, (ARowsPosition.Row - TopRow) * LineHeight)
  else
  begin
    FPaintHelper.BeginDrawing(Canvas);
    try
      LRow := ARowsPosition.Row;

      LRowColumns := 0;
      LRowPos := 0;
      LTokenColumns := 0;
      LColumn := 0;

      if (not FHighlighter.FindFirstToken(FRows.Items[LRow].BeginRange, FRows[LRow], LToken)) then
        LEOL := True
      else
      begin
        LEOL := True;
        repeat
          LTokenColumns := ComputeTextColumns(LToken.Text, LToken.Length, LColumn);
          LTokenWidth := TokenWidth(LToken.Text, LToken.Length, LColumn, LToken);

          if (LRowColumns + LTokenColumns > ARowsPosition.Column) then
          begin
            LEOL := False;
            break;
          end;

          Inc(LRowColumns, LTokenColumns);
          Inc(LRowPos, LTokenWidth);
          Inc(LColumn, LTokenColumns);
        until (not FHighlighter.FindNextToken(LToken));
      end;

      if ((LRowColumns < ARowsPosition.Column) and (LTokenColumns > 0)
        and not LEOL) then
      begin
        LLinePos := LToken.Text;
        while ((LRowColumns < ARowsPosition.Column) and (LTokenColumns > 0)) do
        begin
          LCharColumns := ComputeTextColumns(LLinePos, 1, LColumn);
          Inc(LRowColumns, LCharColumns);
          Inc(LRowPos, TokenWidth(LLinePos, 1, LColumn, LToken));
          Inc(LColumn, LCharColumns);
          Inc(LLinePos);
        end;
      end;

      if (LRowColumns < ARowsPosition.Column) then
        Inc(LRowPos, (ARowsPosition.Column - LRowColumns) * FSpaceWidth);
    finally
      FPaintHelper.EndDrawing();
    end;

    Result := Point(FLeftMarginWidth + LRowPos - HorzTextPos, (ARowsPosition.Row - TopRow) * LineHeight);
  end;
end;

function TCustomBCEditor.RowsToLines(const ARowsPosition: TBCEditorRowsPosition): TBCEditorLinesPosition;
var
  LChar: Integer;
  LColumn: Integer;
  LLine: Integer;
  LLineEndPos: PChar;
  LLinePos: PChar;
begin
  Assert((ARowsPosition.Column >= 0) and (ARowsPosition.Row >= 0),
    'ARowsPosition: ' + ARowsPosition.ToString());

  if (FRows.Count = 0) then
    Result := LinesPosition(ARowsPosition.Column, ARowsPosition.Row)
  else if (ARowsPosition.Row >= FRows.Count) then
    Result := LinesPosition(ARowsPosition.Column, ARowsPosition.Row - FRows.Count + FLines.Count)
  else
  begin
    LLine := FRows.Items[ARowsPosition.Row].Line;

    if (not (rfHasTabs in FRows.Items[ARowsPosition.Row].Flags)) then
    begin
      LChar := FRows.Items[ARowsPosition.Row].Char + ARowsPosition.Column;
      if (LChar >= Length(FLines[LLine])) then
      begin
        LLinePos := nil;
        LLineEndPos := nil;
      end
      else
      begin
        LLinePos := @FLines[LLine][1 + LChar];
        LLineEndPos := @FLines[LLine][Min(FRows.Items[ARowsPosition.Row].Length, Length(FLines[LLine]))];
      end;
    end
    else
    begin
      LLinePos := @FLines[LLine][1 + FRows.Items[ARowsPosition.Row].Char];
      LLineEndPos := @FLines[LLine][Min(FRows.Items[ARowsPosition.Row].Length, Length(FLines[LLine]))];
      LColumn := 0;
      LChar := 0;
      while ((LColumn < ARowsPosition.Column) and (LLinePos < LLineEndPos)) do
      begin
        Inc(LColumn, ComputeTextColumns(LLinePos, 1, LColumn));
        Inc(LChar);
        Inc(LLinePos);
      end;
      Inc(LChar, ARowsPosition.Column - LColumn);
    end;

    if (Assigned(LLinePos)) then
      while ((LLinePos <= LLineEndPos)
        and ((LLinePos^.GetUnicodeCategory in [TUnicodeCategory.ucCombiningMark, TUnicodeCategory.ucNonSpacingMark])
          or ((LLinePos - 1)^ <> BCEDITOR_NONE_CHAR)
            and ((LLinePos - 1)^.GetUnicodeCategory = TUnicodeCategory.ucNonSpacingMark)
            and not IsCombiningDiacriticalMark((LLinePos - 1)^))) do
      begin
        Inc(LChar);
        Inc(LLinePos);
      end;

    Result := LinesPosition(LChar, LLine);
  end;
end;

procedure TCustomBCEditor.SaveToFile(const AFileName: string; AEncoding: TEncoding = nil);
var
  LFileStream: TFileStream;
begin
  LFileStream := TFileStream.Create(AFileName, fmCreate);
  try
    SaveToStream(LFileStream, AEncoding);
  finally
    LFileStream.Free;
  end;
end;

procedure TCustomBCEditor.SaveToStream(AStream: TStream; AEncoding: TEncoding = nil);
begin
  FLines.SaveToStream(AStream, AEncoding);
  SetModified(False);
end;

procedure TCustomBCEditor.ScanCodeFoldingRanges;
const
  DEFAULT_CODE_FOLDING_RANGE_INDEX = 0;
var
  LBeginningOfLine: Boolean;
  LCodeFoldingRangeIndexList: TList;
  LCurrentCodeFoldingRegion: TBCEditorCodeFolding.TRegion;
  LFoldCount: Integer;
  LFoldRanges: TBCEditorCodeFolding.TRanges;
  LLastFoldRange: TBCEditorCodeFolding.TRanges.TRange;
  LLine: Integer;
  LLineEndPos: PChar;
  LLinePos: PChar;
  LOpenTokenFoldRangeList: TList;
  LOpenTokenSkipFoldRangeList: TList;
  LPBookmarkText: PChar;
  LPBookmarkText2: PChar;

  function IsValidChar(Character: PChar): Boolean;
  begin
    Result := Character^.IsLower or Character^.IsUpper or Character^.IsNumber or
      CharInSet(Character^, BCEDITOR_CODE_FOLDING_VALID_CHARACTERS);
  end;

  function IsWholeWord(FirstChar, LastChar: PChar): Boolean;
  begin
    Result := not IsValidChar(FirstChar) and not IsValidChar(LastChar);
  end;

  function SkipEmptySpace(): Boolean;
  begin
    while ((LLinePos <= LLineEndPos) and (LLinePos^ < BCEDITOR_EXCLAMATION_MARK)) do
      Inc(LLinePos);
    Result := LLinePos > LLineEndPos;
  end;

  function CountCharsBefore(APText: PChar; const Character: Char): Integer;
  var
    LPText: PChar;
  begin
    Result := 0;
    LPText := APText - 1;
    while LPText^ = Character do
    begin
      Inc(Result);
      Dec(LPText);
    end;
  end;

  function OddCountOfStringEscapeChars(APText: PChar): Boolean;
  begin
    Result := False;
    if LCurrentCodeFoldingRegion.StringEscapeChar <> BCEDITOR_NONE_CHAR then
      Result := Odd(CountCharsBefore(APText, LCurrentCodeFoldingRegion.StringEscapeChar));
  end;

  function EscapeChar(APText: PChar): Boolean;
  begin
    Result := False;
    if LCurrentCodeFoldingRegion.EscapeChar <> BCEDITOR_NONE_CHAR then
      Result := APText^ = LCurrentCodeFoldingRegion.EscapeChar;
  end;

  function IsNextSkipChar(APText: PChar; ASkipRegionItem: TBCEditorCodeFolding.TSkipRegions.TItem): Boolean;
  begin
    Result := False;
    if ASkipRegionItem.SkipIfNextCharIsNot <> BCEDITOR_NONE_CHAR then
      Result := APText^ = ASkipRegionItem.SkipIfNextCharIsNot;
  end;

  function SkipRegionsClose: Boolean;
  var
    LSkipRegionItem: TBCEditorCodeFolding.TSkipRegions.TItem;
    LTokenEndPos: PChar;
    LTokenPos: PChar;
    LTokenText: string;
  begin
    Result := False;
    { Note! Check Close before Open because close and open keys might be same. }
    if ((LOpenTokenSkipFoldRangeList.Count > 0)
      and CharInSet(LLinePos^, FHighlighter.SkipCloseKeyChars)
      and not OddCountOfStringEscapeChars(LLinePos)) then
    begin
      LSkipRegionItem := LOpenTokenSkipFoldRangeList.Last;
      if (LSkipRegionItem.CloseToken <> LSkipRegionItem.CloseToken) then
      begin
        LTokenText := LSkipRegionItem.CloseToken;
        LTokenPos := @LTokenText[1];
        LTokenEndPos := @LTokenText[Length(LTokenText)];
        LPBookmarkText := LLinePos;
        { Check if the close keyword found }
        while ((LLinePos <= LLineEndPos) and (LTokenPos <= LTokenEndPos)
          and ((LLinePos^ = LTokenPos^) or (LSkipRegionItem.SkipEmptyChars and (LLinePos^ < BCEDITOR_EXCLAMATION_MARK)))) do
        begin
          if (not CharInSet(LLinePos^, [BCEDITOR_NONE_CHAR, BCEDITOR_SPACE_CHAR, BCEDITOR_TAB_CHAR])) then
            Inc(LTokenPos);
          Inc(LLinePos);
        end;
        if (LTokenPos >= LTokenEndPos) then { If found, pop skip region from the stack }
        begin
          LOpenTokenSkipFoldRangeList.Delete(LOpenTokenSkipFoldRangeList.Count - 1);
          Result := True;
        end
        else
          LLinePos := LPBookmarkText; { Skip region close not found, return pointer back }
      end;
    end;
  end;

  function SkipRegionsOpen: Boolean;
  var
    LCount: Integer;
    LIndex: Integer;
    LSkipRegionItem: TBCEditorCodeFolding.TSkipRegions.TItem;
    LTokenEndPos: PChar;
    LTokenPos: PChar;
    LTokenText: string;
  begin
    Result := False;

    if CharInSet(LLinePos^, FHighlighter.SkipOpenKeyChars) then
      if LOpenTokenSkipFoldRangeList.Count = 0 then
      begin
        LCount := LCurrentCodeFoldingRegion.SkipRegions.Count - 1;
        for LIndex := 0 to LCount do
        begin
          LSkipRegionItem := LCurrentCodeFoldingRegion.SkipRegions[LIndex];
          if ((LLinePos^ = LSkipRegionItem.OpenToken[1])
            and not OddCountOfStringEscapeChars(LLinePos)
            and not IsNextSkipChar(LLinePos + Length(LSkipRegionItem.OpenToken), LSkipRegionItem)) then
          begin
            LTokenText := LSkipRegionItem.OpenToken;
            if (LTokenText <> '') then
            begin
              LTokenPos := @LTokenText[1];
              LTokenEndPos := @LTokenText[Length(LTokenText)];
              LPBookmarkText := LLinePos;
              { Check, if the open keyword found }
              while ((LLinePos <= LLineEndPos) and (LTokenPos <= LTokenEndPos)
                and ((LLinePos^ = LTokenPos^) or (LSkipRegionItem.SkipEmptyChars and (LLinePos^ < BCEDITOR_EXCLAMATION_MARK)))) do
              begin
                if (not LSkipRegionItem.SkipEmptyChars
                  or LSkipRegionItem.SkipEmptyChars and not CharInSet(LLinePos^, [BCEDITOR_NONE_CHAR, BCEDITOR_SPACE_CHAR, BCEDITOR_TAB_CHAR])) then
                  Inc(LTokenPos);
                Inc(LLinePos);
              end;

              if (LTokenPos > LTokenEndPos) then { If found, skip single line comment or push skip region into stack }
              begin
                if LSkipRegionItem.RegionType = ritSingleLineString then
                begin
                  LTokenText := LSkipRegionItem.CloseToken;
                  if (LTokenText <> '') then
                  begin
                    LTokenPos := @LTokenText[1];
                    LTokenEndPos := @LTokenText[Length(LTokenText)];
                    while ((LLinePos <= LLineEndPos) and (LTokenPos <= LTokenEndPos)
                      and ((LLinePos^ <> LTokenPos^) or OddCountOfStringEscapeChars(LLinePos))) do
                      Inc(LLinePos);
                    Inc(LLinePos);
                  end;
                end
                else if LSkipRegionItem.RegionType = ritSingleLineComment then
                  { Single line comment skip until next line }
                  Exit(True)
                else
                  LOpenTokenSkipFoldRangeList.Add(LSkipRegionItem);
                Dec(LLinePos); { The end of the while loop will increase }
                Break;
              end
              else
                LLinePos := LPBookmarkText; { Skip region open not found, return pointer back }
            end;
          end;
        end;
      end;
  end;

  procedure RegionItemsClose;

    procedure SetCodeFoldingRangeToLine(ARange: TBCEditorCodeFolding.TRanges.TRange);
    var
      LIndex: Integer;
    begin
      if ARange.RegionItem.TokenEndIsPreviousLine then
      begin
        LIndex := LLine;
        while (LIndex > 0) and (FLines.Items[LIndex - 1].Text = '') do
          Dec(LIndex);
        ARange.EndLine := LIndex
      end
      else
        ARange.EndLine := LLine;
    end;

  var
    LCodeFoldingRange: TBCEditorCodeFolding.TRanges.TRange;
    LCodeFoldingRangeLast: TBCEditorCodeFolding.TRanges.TRange;
    LIndex: Integer;
    LIndexDecrease: Integer;
    LItemIndex: Integer;
    LTokenEndPos: PChar;
    LTokenPos: PChar;
    LTokenText: string;
  begin
    if ((LOpenTokenSkipFoldRangeList.Count = 0) 
      and (LOpenTokenFoldRangeList.Count > 0)
      and CharInSet(CaseUpper(LLinePos^), FHighlighter.FoldCloseKeyChars)) then
    begin
      LIndexDecrease := 1;
      {$if defined(VER250)}
      LCodeFoldingRange := nil;
      {$endif}
      repeat
        LIndex := LOpenTokenFoldRangeList.Count - LIndexDecrease;
        if LIndex < 0 then
          Break;
        LCodeFoldingRange := LOpenTokenFoldRangeList.Items[LIndex];

        if LCodeFoldingRange.RegionItem.CloseTokenBeginningOfLine and not LBeginningOfLine then
          Exit;
        LTokenText := LCodeFoldingRange.RegionItem.CloseToken;
        if (LTokenText <> '') then
        begin
          LTokenPos := @LTokenText[1];
          LTokenEndPos := @LTokenText[Length(LTokenText)];
          LPBookmarkText := LLinePos;
          { Check if the close keyword found }
          while ((LLinePos <= LLineEndPos) and (LTokenPos <= LTokenEndPos)
            and (CaseUpper(LLinePos^) = LTokenPos^)) do
          begin
            Inc(LLinePos);
            Inc(LTokenPos);
          end;

          if (LTokenPos > LTokenEndPos) then { If found, pop skip region from the stack }
          begin
            if not LCodeFoldingRange.RegionItem.BreakCharFollows or
              LCodeFoldingRange.RegionItem.BreakCharFollows and IsWholeWord(LPBookmarkText - 1, LLinePos) then
            begin
              LOpenTokenFoldRangeList.Remove(LCodeFoldingRange);
              Dec(LFoldCount);

              if ((LCodeFoldingRange.RegionItem.BreakIfNotFoundBeforeNextRegion <> '')
                and not LCodeFoldingRange.IsExtraTokenFound) then
              begin
                LLinePos := LPBookmarkText;
                Exit;
              end;
              SetCodeFoldingRangeToLine(LCodeFoldingRange);
              { Check if the code folding ranges have shared close }
              if LOpenTokenFoldRangeList.Count > 0 then
                for LItemIndex := LOpenTokenFoldRangeList.Count - 1 downto 0 do
                begin
                  LCodeFoldingRangeLast := LOpenTokenFoldRangeList.Items[LItemIndex];
                  if Assigned(LCodeFoldingRangeLast.RegionItem) and LCodeFoldingRangeLast.RegionItem.SharedClose then
                  begin
                    LTokenText := LCodeFoldingRangeLast.RegionItem.CloseToken;
                    LTokenPos := @LTokenText[1];
                    LTokenEndPos := @LTokenText[Length(LTokenText)];
                    LLinePos := LPBookmarkText;
                    while ((LLinePos <= LLineEndPos) and (LTokenPos <= LTokenEndPos)
                      and (CaseUpper(LLinePos^) = LTokenPos^)) do
                    begin
                      Inc(LLinePos);
                      Inc(LTokenPos);
                    end;
                    if (LTokenPos > LTokenEndPos) then
                    begin
                      SetCodeFoldingRangeToLine(LCodeFoldingRangeLast);
                      LOpenTokenFoldRangeList.Remove(LCodeFoldingRangeLast);
                      Dec(LFoldCount);
                    end;
                  end;
                end;
              LLinePos := LPBookmarkText; { Go back where we were }
            end
            else
              LLinePos := LPBookmarkText; { Region close not found, return pointer back }
          end
          else
            LLinePos := LPBookmarkText; { Region close not found, return pointer back }
        end;

        Inc(LIndexDecrease);
      until Assigned(LCodeFoldingRange) and ((LCodeFoldingRange.RegionItem.BreakIfNotFoundBeforeNextRegion = '') or
        (LOpenTokenFoldRangeList.Count - LIndexDecrease < 0));
    end;
  end;

  function RegionItemsOpen: Boolean;
  var
    LArrayIndex: Integer;
    LIndex: Integer;
    LLineTempPos: PChar;
    LRange: TBCEditorCodeFolding.TRanges.TRange;
    LRegionItem: TBCEditorCodeFoldingRegionItem;
    LSkipIfFoundAfterOpenToken: Boolean;
    LTokenEndPos: PChar;
    LTokenFollowEndPos: PChar;
    LTokenFollowPos: PChar;
    LTokenFollowText: string;
    LTokenPos: PChar;
    LTokenText: string;
  begin
    Result := False;

    if LOpenTokenSkipFoldRangeList.Count <> 0 then
      Exit;
    if CharInSet(CaseUpper(LLinePos^), FHighlighter.FoldOpenKeyChars) then
    begin
      LRange := nil;
      if LOpenTokenFoldRangeList.Count > 0 then
        LRange := LOpenTokenFoldRangeList.Last;
      if Assigned(LRange) and LRange.RegionItem.NoSubs then
        Exit;

      for LIndex := 0 to LCurrentCodeFoldingRegion.Count - 1 do
      begin
        LRegionItem := LCurrentCodeFoldingRegion[LIndex];
        if (LRegionItem.OpenTokenBeginningOfLine and LBeginningOfLine) or (not LRegionItem.OpenTokenBeginningOfLine) then
        begin
          { Check if extra token found }
          if Assigned(LRange) then
          begin
            if LRange.RegionItem.BreakIfNotFoundBeforeNextRegion <> '' then
              if (LLinePos^ = LRange.RegionItem.BreakIfNotFoundBeforeNextRegion[1]) then { If first character match }
              begin
                LTokenText := LRange.RegionItem.BreakIfNotFoundBeforeNextRegion;
                if (LTokenText <> '') then
                begin
                  LTokenPos := @LTokenText[1];
                  LTokenEndPos := @LTokenText[Length(LTokenText)];
                  LPBookmarkText := LLinePos;
                  { Check if open keyword found }
                  while ((LLinePos <= LLineEndPos) and (LTokenPos <= LTokenEndPos)
                    and ((CaseUpper(LLinePos^) = LTokenPos^)
                      or CharInSet(LLinePos^, [BCEDITOR_NONE_CHAR, BCEDITOR_SPACE_CHAR, BCEDITOR_TAB_CHAR]))) do
                  begin
                    if (CharInSet(LTokenPos^, [BCEDITOR_NONE_CHAR, BCEDITOR_SPACE_CHAR, BCEDITOR_TAB_CHAR])
                      or not CharInSet(LLinePos^, [BCEDITOR_NONE_CHAR, BCEDITOR_SPACE_CHAR, BCEDITOR_TAB_CHAR])) then
                      Inc(LTokenPos);
                    Inc(LLinePos);
                  end;
                  if (LTokenPos > LTokenEndPos) then
                  begin
                    LRange.IsExtraTokenFound := True;
                    Continue;
                  end
                  else
                    LLinePos := LPBookmarkText; { Region not found, return pointer back }
                end;
              end;
          end;
          { First word after newline }
          if (CaseUpper(LLinePos^) = LRegionItem.OpenToken[1]) then { If first character match }
          begin
            LTokenText := LRegionItem.OpenToken;
            if (LTokenText <> '') then
            begin
              LTokenPos := @LTokenText[1];
              LTokenEndPos := @LTokenText[Length(LTokenText)];
              LPBookmarkText := LLinePos;
              { Check if open keyword found }
              while ((LLinePos <= LLineEndPos) and (LTokenPos <= LTokenEndPos)
                and (CaseUpper(LLinePos^) = LTokenPos^)) do
              begin
                Inc(LLinePos);
                Inc(LTokenPos);
              end;

              if ((LRegionItem.OpenTokenCanBeFollowedBy <> '')
                and (CaseUpper(LLinePos^) = LRegionItem.OpenTokenCanBeFollowedBy[1])) then
              begin
                LLineTempPos := LLinePos;
                LTokenFollowText := LRegionItem.OpenTokenCanBeFollowedBy;
                LTokenFollowPos := @LTokenFollowText[1];
                LTokenFollowEndPos := @LTokenFollowText[Length(LTokenFollowText)];
                while (LLineTempPos <= LLineEndPos) and (LTokenFollowPos <= LTokenFollowEndPos)
                  and (CaseUpper(LLineTempPos^) = LTokenFollowPos^) do
                begin
                  Inc(LLineTempPos);
                  Inc(LTokenFollowPos);
                end;
                if (LTokenFollowPos > LTokenFollowEndPos) then
                  LLinePos := LLineTempPos;
              end;

              if (LTokenPos > LTokenEndPos) then
              begin
                if ((not LRegionItem.BreakCharFollows or LRegionItem.BreakCharFollows and IsWholeWord(LPBookmarkText - 1, LLinePos))
                  and not EscapeChar(LPBookmarkText - 1)) then { Not interested in partial hits }
                begin
                  { Check if special rule found }
                  LSkipIfFoundAfterOpenToken := False;
                  if (LRegionItem.SkipIfFoundAfterOpenTokenArrayCount > 0) then
                    while (LLinePos <= LLineEndPos) do
                    begin
                      for LArrayIndex := 0 to LRegionItem.SkipIfFoundAfterOpenTokenArrayCount - 1 do
                      begin
                        LTokenText := LRegionItem.SkipIfFoundAfterOpenTokenArray[LArrayIndex];
                        LTokenPos := @LTokenText[1];
                        LTokenEndPos := @LTokenText[Length(LTokenText)];
                        LPBookmarkText2 := LLinePos;
                        if (CaseUpper(LLinePos^) = LTokenPos^) then { If first character match }
                        begin
                          while ((LLinePos <= LLineEndPos) and (LTokenPos <= LTokenEndPos)
                            and (CaseUpper(LLinePos^) = LTokenPos^)) do
                          begin
                            Inc(LLinePos);
                            Inc(LTokenPos);
                          end;
                          if (LTokenPos > LTokenEndPos) then
                          begin
                            LSkipIfFoundAfterOpenToken := True;
                            Break; { for }
                          end
                          else
                            LLinePos := LPBookmarkText2; { Region not found, return pointer back }
                        end;
                      end;
                      if LSkipIfFoundAfterOpenToken then
                        Break; { while }
                      Inc(LLinePos);
                    end;
                  if LSkipIfFoundAfterOpenToken then
                  begin
                    LLinePos := LPBookmarkText; { Skip found, return pointer back }
                    Continue;
                  end;

                  if Assigned(LRange) and (LRange.RegionItem.BreakIfNotFoundBeforeNextRegion <> '')
                    and not LRange.IsExtraTokenFound then
                  begin
                    LOpenTokenFoldRangeList.Remove(LRange);
                    Dec(LFoldCount);
                  end;

                  if LOpenTokenFoldRangeList.Count > 0 then
                    LFoldRanges := TBCEditorCodeFolding.TRanges.TRange(LOpenTokenFoldRangeList.Last).SubCodeFoldingRanges
                  else
                    LFoldRanges := FAllCodeFoldingRanges;

                  LRange := LFoldRanges.Add(FAllCodeFoldingRanges, LLine, GetLineIndentLevel(LLine),
                    LFoldCount, LRegionItem, LLine);
                  { Open keyword found }
                  LOpenTokenFoldRangeList.Add(LRange);
                  Inc(LFoldCount);
                  Dec(LLinePos); { The end of the while loop will increase }
                  Result := LRegionItem.OpenTokenBreaksLine;
                  Break;
                end
                else
                  LLinePos := LPBookmarkText; { Region not found, return pointer back }
              end
              else
                LLinePos := LPBookmarkText; { Region not found, return pointer back }
            end;
          end;
        end;
      end;
    end;
  end;

  function MultiHighlighterOpen: Boolean;
  var
    LChar: Char;
    LCodeFoldingRegion: TBCEditorCodeFolding.TRegion;
    LIndex: Integer;
    LTokenEndPos: PChar;
    LTokenPos: PChar;
    LTokenText: string;
  begin
    Result := False;
    if LOpenTokenSkipFoldRangeList.Count <> 0 then
      Exit;
    LChar := CaseUpper(LLinePos^);
    LPBookmarkText := LLinePos;
    for LIndex := 1 to Highlighter.CodeFoldingRangeCount - 1 do { First (0) is the default range }
    begin
      LCodeFoldingRegion := Highlighter.CodeFoldingRegions[LIndex];

      if (LChar = LCodeFoldingRegion.OpenToken[1]) then { If first character match }
      begin
        LTokenText := LCodeFoldingRegion.OpenToken;
        if (LTokenText <> '') then
        begin
          LTokenPos := @LTokenText[1];
          LTokenEndPos := @LTokenText[Length(LTokenText)];
          { Check if open keyword found }
          while ((LLinePos <= LLineEndPos) and (LTokenPos <= LTokenEndPos)
            and (CaseUpper(LLinePos^) = LTokenPos^)) do
          begin
            Inc(LLinePos);
            Inc(LTokenPos);
          end;
          LLinePos := LPBookmarkText; { Return pointer always back }
          if (LTokenPos > LTokenEndPos) then
          begin
            LCodeFoldingRangeIndexList.Add(Pointer(LIndex));
            LCurrentCodeFoldingRegion := Highlighter.CodeFoldingRegions[LIndex];
            Exit(True)
          end;
        end;
      end;
    end;
  end;

  procedure MultiHighlighterClose;
  var
    LChar: Char;
    LCodeFoldingRegion: TBCEditorCodeFolding.TRegion;
    LIndex: Integer;
    LTokenEndPos: PChar;
    LTokenPos: PChar;
    LTokenText: string;
  begin
    if (LOpenTokenSkipFoldRangeList.Count = 0) then
    begin
      LChar := CaseUpper(LLinePos^);
      LPBookmarkText := LLinePos;
      for LIndex := 1 to Highlighter.CodeFoldingRangeCount - 1 do { First (0) is the default range }
      begin
        LCodeFoldingRegion := Highlighter.CodeFoldingRegions[LIndex];

        if (LChar = LCodeFoldingRegion.CloseToken[1]) then { If first character match }
        begin
          LTokenText := LCodeFoldingRegion.CloseToken;
          if (LTokenText <> '') then
          begin
            LTokenPos := @LTokenText[1];
            LTokenEndPos := @LTokenText[Length(LTokenText)];
            { Check if close keyword found }
            while ((LLinePos <= LLineEndPos) and (LTokenPos <= LTokenEndPos)
              and (CaseUpper(LLinePos^) = LTokenEndPos^)) do
            begin
              Inc(LLinePos);
              Inc(LTokenPos);
            end;
            LLinePos := LPBookmarkText; { Return pointer always back }
            if (LTokenPos > LTokenEndPos) then
            begin
              if LCodeFoldingRangeIndexList.Count > 0 then
                LCodeFoldingRangeIndexList.Delete(LCodeFoldingRangeIndexList.Count - 1);
              if LCodeFoldingRangeIndexList.Count > 0 then
                LCurrentCodeFoldingRegion := Highlighter.CodeFoldingRegions[Integer(LCodeFoldingRangeIndexList.Last)]
              else
                LCurrentCodeFoldingRegion := Highlighter.CodeFoldingRegions[DEFAULT_CODE_FOLDING_RANGE_INDEX];
              Exit;
            end
          end;
        end;
      end;
    end;
  end;

  function TagFolds: Boolean;
  var
    LCodeFoldingRegion: TBCEditorCodeFolding.TRegion;
    LIndex: Integer;
  begin
    Result := False;
    for LIndex := 0 to Highlighter.CodeFoldingRangeCount - 1 do
    begin
      LCodeFoldingRegion := Highlighter.CodeFoldingRegions[LIndex];
      if LCodeFoldingRegion.FoldTags then
        Exit(True);
    end;
  end;

  procedure AddTagFolds;
  var
    LAdded: Boolean;
    LCloseToken: string;
    LOpenToken: string;
    LText: string;
    LTextBeginPos: PChar;
    LTextEndPos: PChar;
    LTextPos: PChar;
    LRegionItem: TBCEditorCodeFoldingRegionItem;
    LTokenAttributes: string;
    LTokenAttributesBeginPos: PChar;
    LTokenName: string;
    LRegion: TBCEditorCodeFolding.TRegion;
  begin
    LText := FLines.Text;
    LTextBeginPos := @LText[1];
    LTextEndPos := @LText[Length(LText)];
    LTextPos := LTextBeginPos;
    LAdded := False;
    while (LTextPos <= LTextEndPos) do
    begin
      if (LTextPos^ = '<') then
      begin
        Inc(LTextPos);
        if not CharInSet(LTextPos^, ['?', '!', '/']) then
        begin
          LTokenName := '';
          while ((LTextPos <= LTextEndPos) and not CharInSet(LTextPos^, [' ', '>'])) do
          begin
            LTokenName := LTokenName + CaseUpper(LTextPos^);
            Inc(LTextPos);
          end;
          if (LTextPos^ <> ' ') then
            LTokenAttributes := ''
          else
          begin
            LTokenAttributesBeginPos := LTextPos;
            while ((LTextPos <= LTextEndPos) and not CharInSet(LTextPos^, ['/', '>'])) do
            begin
              Inc(LTextPos);
              if (CharInSet(LTextPos^, ['"', ''''])) then
              begin
                Inc(LTextPos);
                while ((LTextPos <= LTextEndPos) and not CharInSet(LTextPos^, ['"', ''''])) do
                  Inc(LTextPos);
              end;
            end;
            LTokenAttributes := UpperCase(Copy(LText, 1 + LTokenAttributesBeginPos - LTextBeginPos, LTextPos - LTokenAttributesBeginPos));
          end;

          LOpenToken := '<' + LTokenName + LTokenAttributes + LTextPos^;
          LCloseToken := '</' + LTokenName + '>';

          if (LTextPos^ = '>') and (LTextPos^ <> '/') then
          begin
            LRegion := FHighlighter.CodeFoldingRegions[0];
            if not LRegion.Contains(LOpenToken, LCloseToken) then { First (0) is the default range }
            begin
              LRegionItem := LRegion.Add(LOpenToken, LCloseToken);
              LRegionItem.BreakCharFollows := False;
              LAdded := True;
            end;
          end;
        end;
      end;
      Inc(LTextPos);
    end;
    if (LAdded) then
    begin
      FHighlighter.AddKeyChar(ctFoldOpen, '<');
      FHighlighter.AddKeyChar(ctFoldClose, '<');
    end;
  end;

var
  LRange: TBCEditorCodeFolding.TRanges.TRange;
  LRow: Integer;
  LPreviousLine: Integer;
begin
  LFoldCount := 0;
  LOpenTokenSkipFoldRangeList := TList.Create;
  LOpenTokenFoldRangeList := TList.Create;
  LCodeFoldingRangeIndexList := TList.Create;
  try
    if TagFolds then
      AddTagFolds;

    { Go through the text line by line, character by character }
    LPreviousLine := -1;

    LCodeFoldingRangeIndexList.Add(Pointer(DEFAULT_CODE_FOLDING_RANGE_INDEX));

    if Highlighter.CodeFoldingRangeCount > 0 then
      LCurrentCodeFoldingRegion := Highlighter.CodeFoldingRegions[DEFAULT_CODE_FOLDING_RANGE_INDEX];

    for LRow := 0 to FRows.Count - 1 do
    begin
      LLine := FRows.Items[LRow].Line;
      LRange := TBCEditorCodeFolding.TRanges.TRange(FLines.Items[LLine].CodeFolding.BeginRange);
      if Assigned(LRange) and LRange.Collapsed then
      begin
        LPreviousLine := LLine;
        Continue;
      end;

      if ((LPreviousLine <> LLine) and (FLines.Items[LLine].Text <> '')) then
      begin
        LLinePos := @FLines.Items[LLine].Text[1];
        LLineEndPos := @FLines.Items[LLine].Text[Length(FLines.Items[LLine].Text)];
        LBeginningOfLine := True;
        while (LLinePos <= LLineEndPos) do
          if (not SkipEmptySpace()) then
          begin
            if Highlighter.MultiHighlighter then
              if not MultiHighlighterOpen then
                MultiHighlighterClose;

            if SkipRegionsClose then
              Continue; { while LTextPos <= LTextEndPos do }
            if SkipRegionsOpen then
              Break; { Line comment breaks }

            if SkipEmptySpace then
              Break;

            if LOpenTokenSkipFoldRangeList.Count = 0 then
            begin
              RegionItemsClose;
              if RegionItemsOpen then
                Break; { OpenTokenBreaksLine region item option breaks }
            end;

            if (LLinePos <= LLineEndPos) then
              Inc(LLinePos);

            { Skip rest of the word }
            while ((LLinePos <= LLineEndPos)
              and (LLinePos^.IsLower or LLinePos^.IsUpper or LLinePos^.IsNumber)) do
              Inc(LLinePos);

            LBeginningOfLine := False; { Not in the beginning of the line anymore }
          end;
      end;
      LPreviousLine := LLine;
    end;
    { Check the last not empty line }
    LLine := FLines.Count - 1;
    while (LLine >= 0) and (Trim(FLines.Items[LLine].Text) = '') do
      Dec(LLine);
    if ((LLine >= 0) and (FLines.Items[LLine].Text <> '')) then
    begin
      LLinePos := @FLines.Items[LLine].Text[1];
      LLineEndPos := @FLines.Items[LLine].Text[Length(FLines.Items[LLine].Text)];
      while LOpenTokenFoldRangeList.Count > 0 do
      begin
        LLastFoldRange := LOpenTokenFoldRangeList.Last;
        if Assigned(LLastFoldRange) then
        begin
          Inc(LLine);
          LLine := Min(LLine, FLines.Count - 1);
          if LLastFoldRange.RegionItem.OpenIsClose then
            LLastFoldRange.EndLine := LLine;
          LOpenTokenFoldRangeList.Remove(LLastFoldRange);
          Dec(LFoldCount);
          RegionItemsClose;
        end;
      end;
    end;
  finally
    LCodeFoldingRangeIndexList.Free;
    LOpenTokenSkipFoldRangeList.Free;
    LOpenTokenFoldRangeList.Free;
  end;
end;

procedure TCustomBCEditor.ScanMatchingPair();

  procedure ScanAt(const APosition: TBCEditorLinesPosition);
  var
    LDeep: Integer;
    LFoundLengthCloseToken: Integer;
    LFoundLengthOpenToken: Integer;
    LMatchingPair: Integer;
    LPosition: TBCEditorLinesPosition;
    LSearchCloseToken: TBCEditorLines.TSearch;
    LSearchOpenToken: TBCEditorLines.TSearch;
  begin
    for LMatchingPair := 0 to FHighlighter.MatchingPairs.Count - 1 do
      if (FCurrentMatchingPair.FState = mpsClear) then
      begin
        LSearchCloseToken := TBCEditorLines.TSearch.Create(FLines,
          LinesArea(LinesPosition(Max(0, APosition.Char + 1 - Length(FHighlighter.MatchingPairs[LMatchingPair].CloseToken)), APosition.Line),
            LinesPosition(Min(Length(FLines[APosition.Line]), APosition.Char + Length(FHighlighter.MatchingPairs[LMatchingPair].CloseToken) - 1), APosition.Line)),
          False, False, False, False, FHighlighter.MatchingPairs[LMatchingPair].CloseToken);
        FCurrentMatchingPair.CloseTokenArea.BeginPosition := LSearchCloseToken.Area.BeginPosition;
        if (LSearchCloseToken.Find(FCurrentMatchingPair.CloseTokenArea.BeginPosition, LFoundLengthCloseToken)) then
        begin
          FCurrentMatchingPair.CloseTokenArea.EndPosition := FLines.PositionOf(LFoundLengthCloseToken, FCurrentMatchingPair.CloseTokenArea.BeginPosition);

          LDeep := 0;

          LSearchOpenToken := TBCEditorLines.TSearch.Create(FLines,
            LinesArea(FLines.BOFPosition,
              FLines.PositionOf(- 1, FCurrentMatchingPair.CloseTokenArea.BeginPosition)),
            False, False, False, True, FHighlighter.MatchingPairs[LMatchingPair].OpenToken);
          FCurrentMatchingPair.OpenTokenArea.BeginPosition := LSearchOpenToken.Area.EndPosition;

          LPosition := LSearchOpenToken.Area.EndPosition;
          while ((FCurrentMatchingPair.FState = mpsClear)
            and LSearchOpenToken.Find(FCurrentMatchingPair.OpenTokenArea.BeginPosition, LFoundLengthOpenToken)) do
          begin
            LSearchCloseToken.Free();
            LSearchCloseToken := TBCEditorLines.TSearch.Create(FLines,
              LinesArea(LSearchOpenToken.Area.BeginPosition,
                FCurrentMatchingPair.CloseTokenArea.BeginPosition),
              False, False, False, True, FHighlighter.MatchingPairs[LMatchingPair].CloseToken);

            if (LSearchCloseToken.Find(LPosition, LFoundLengthCloseToken)
              and (LPosition > FCurrentMatchingPair.OpenTokenArea.BeginPosition)) then
            begin
              Inc(LDeep);
              LPosition := FLines.PositionOf(-1, LPosition);
              FCurrentMatchingPair.OpenTokenArea.BeginPosition := LPosition;
            end
            else if (LDeep > 0) then
            begin
              Dec(LDeep);
              LPosition := FLines.PositionOf(-1, FCurrentMatchingPair.OpenTokenArea.BeginPosition);
              FCurrentMatchingPair.OpenTokenArea.BeginPosition := LPosition;
            end
            else
            begin
              FCurrentMatchingPair.FState := mpsFound;
              FCurrentMatchingPair.OpenTokenArea.EndPosition := FLines.PositionOf(LFoundLengthOpenToken, FCurrentMatchingPair.OpenTokenArea.BeginPosition);
            end;
          end;
          LSearchOpenToken.Free();
        end;
        LSearchCloseToken.Free();
      end;

    for LMatchingPair := 0 to FHighlighter.MatchingPairs.Count - 1 do
      if (FCurrentMatchingPair.FState = mpsClear) then
      begin
        LSearchOpenToken := TBCEditorLines.TSearch.Create(FLines,
          LinesArea(LinesPosition(Max(0, APosition.Char + 1 - Length(FHighlighter.MatchingPairs[LMatchingPair].CloseToken)), APosition.Line),
            LinesPosition(Min(Length(FLines[APosition.Line]), Min(Length(FLines[APosition.Line]), APosition.Char + Length(FHighlighter.MatchingPairs[LMatchingPair].OpenToken) - 1)), APosition.Line)),
          False, False, False, False, FHighlighter.MatchingPairs[LMatchingPair].OpenToken);
        FCurrentMatchingPair.OpenTokenArea.BeginPosition := LSearchOpenToken.Area.BeginPosition;
        if (LSearchOpenToken.Find(FCurrentMatchingPair.OpenTokenArea.BeginPosition, LFoundLengthOpenToken)) then
        begin
          FCurrentMatchingPair.OpenTokenArea.EndPosition := FLines.PositionOf(1, FCurrentMatchingPair.OpenTokenArea.BeginPosition);

          LDeep := 0;

          LSearchCloseToken := TBCEditorLines.TSearch.Create(FLines,
            LinesArea(FCurrentMatchingPair.OpenTokenArea.EndPosition,
              FLines.EOFPosition),
            False, False, False, False, FHighlighter.MatchingPairs[LMatchingPair].CloseToken);
          FCurrentMatchingPair.CloseTokenArea.BeginPosition := LSearchCloseToken.Area.BeginPosition;

          LPosition := LSearchCloseToken.Area.BeginPosition;
          while ((FCurrentMatchingPair.FState = mpsClear)
            and LSearchCloseToken.Find(FCurrentMatchingPair.CloseTokenArea.BeginPosition, LFoundLengthCloseToken)) do
          begin
            LSearchOpenToken.Free();
            LSearchOpenToken := TBCEditorLines.TSearch.Create(FLines,
              LinesArea(LSearchCloseToken.Area.BeginPosition,
                FCurrentMatchingPair.CloseTokenArea.BeginPosition),
              False, False, False, False, FHighlighter.MatchingPairs[LMatchingPair].OpenToken);

            if (LSearchOpenToken.Find(LPosition, LFoundLengthOpenToken)
              and (LPosition < FCurrentMatchingPair.CloseTokenArea.BeginPosition)) then
            begin
              Inc(LDeep);
              LPosition := FLines.PositionOf(1, LPosition);
              FCurrentMatchingPair.CloseTokenArea.BeginPosition := LPosition;
            end
            else if (LDeep > 0) then
            begin
              Dec(LDeep);
              LPosition := FLines.PositionOf(1, FCurrentMatchingPair.CloseTokenArea.BeginPosition);
              FCurrentMatchingPair.CloseTokenArea.BeginPosition := LPosition;
            end
            else
            begin
              FCurrentMatchingPair.FState := mpsFound;
              FCurrentMatchingPair.CloseTokenArea.EndPosition := FLines.PositionOf(LFoundLengthCloseToken, FCurrentMatchingPair.CloseTokenArea.BeginPosition);
            end;
          end;
          LSearchCloseToken.Free();
        end;
        LSearchOpenToken.Free()
      end;
  end;

begin
  Assert(FCurrentMatchingPair.FState = mpsClear);

  if (FMatchingPair.Enabled) then
  begin
    if (FLines.ValidPosition(FLines.CaretPosition)) then
      ScanAt(FLines.CaretPosition);

    if ((FCurrentMatchingPair.FState = mpsClear)
      and (FLines.CaretPosition.Line < FLines.Count)
      and (0 < FLines.CaretPosition.Char) and (FLines.CaretPosition.Char <= Length(FLines[FLines.CaretPosition.Line]))) then
      ScanAt(LinesPosition(FLines.CaretPosition.Char - 1, FLines.CaretPosition.Line));
  end;

  if (FCurrentMatchingPair.FState = mpsClear) then
    FCurrentMatchingPair.FState := mpsNotFound;
end;

procedure TCustomBCEditor.ScrollChanged(ASender: TObject);
begin
  UpdateScrollBars;
  Invalidate;
end;

procedure TCustomBCEditor.ScrollTimerHandler(ASender: TObject);
var
  LCursorPoint: TPoint;
  LLinesPosition: TBCEditorLinesPosition;
  LRow: Integer;
  LRowsPosition: TBCEditorRowsPosition;
begin
  BeginUpdate();

  GetCursorPos(LCursorPoint);
  LCursorPoint := ScreenToClient(LCursorPoint);
  LRowsPosition := ClientToRows(LCursorPoint.X, LCursorPoint.Y);
  if FScrollDeltaX <> 0 then
    if GetKeyState(VK_SHIFT) < 0 then
      HorzTextPos := Max(0, HorzTextPos + Sign(FScrollDeltaX) * FTextWidth)
    else
      HorzTextPos := Max(0, HorzTextPos + FScrollDeltaX);
  if FScrollDeltaY <> 0 then
  begin
    if GetKeyState(VK_SHIFT) < 0 then
      TopRow := TopRow + FScrollDeltaY * VisibleRows
    else
      TopRow := TopRow + FScrollDeltaY;
    LRow := TopRow;
    if FScrollDeltaY > 0 then
      Inc(LRow, VisibleRows - 1);
    LRowsPosition.Row := MinMax(LRow, 0, FRows.Count - 1);
  end;

  if not FMouseMoveScrolling then
  begin
    LLinesPosition := RowsToLines(LRowsPosition);
    if (FLines.CaretPosition <> LLinesPosition) then
      MoveCaretAndSelection(FLines.SelArea.BeginPosition, LLinesPosition, inherited MouseCapture);
  end;

  EndUpdate();
  ComputeScroll(LCursorPoint);
end;

procedure TCustomBCEditor.ScrollToCaret(ACenterVertical: Boolean = False; AScrollAlways: Boolean = False);
var
  LClient: TPoint;
  LRow: Integer;
begin
  LClient := RowsToClient(FRows.CaretPosition);
  if (LClient.X - FLeftMarginWidth < 0) then
    HorzTextPos := LClient.X - FLeftMarginWidth + HorzTextPos
  else if ((LClient.X - FLeftMarginWidth + FSpaceWidth > FTextWidth) or AScrollAlways) then
    HorzTextPos := LClient.X - FLeftMarginWidth + HorzTextPos - FTextWidth + FSpaceWidth + 1;

  LRow := FRows.CaretPosition.Row;
  if (not ACenterVertical) then
  begin
    if (LRow < TopRow) then
      TopRow := LRow
    else if ((LRow >= TopRow + VisibleRows) or AScrollAlways) then
      TopRow := LRow - VisibleRows + 1;
  end
  else
  begin
    if (LRow < TopRow) then
      TopRow := Max(0, LRow - VisibleRows div 2)
    else if ((LRow >= TopRow + VisibleRows div 2) or AScrollAlways) then
      TopRow := LRow - VisibleRows div 2 + 1;
  end;
end;

procedure TCustomBCEditor.SearchChanged(AEvent: TBCEditorSearchEvent);
begin
  if (FSearchResults.Count > 0) then
    case (AEvent) of
      seChange:
        FindFirst();
    end;
  Invalidate();
end;

function TCustomBCEditor.SearchStatus: string;
begin
  Result := FSearchStatus;
end;

procedure TCustomBCEditor.SelectAll;
begin
  FLines.SelArea := FLines.Area;
  FLastSortOrder := soDesc;
end;

function TCustomBCEditor.SelectedText(): string;
begin
  Result := SelText;
end;

procedure TCustomBCEditor.SelectionChanged(ASender: TObject);
begin
  if (FOldSelectionAvailable or SelectionAvailable) then
    if (UpdateCount > 0) then
      Include(FState, esSelectionChanged)
    else
      Invalidate();

  if (Assigned(FOnSelectionChanged)) then
    FOnSelectionChanged(Self);
end;

procedure TCustomBCEditor.SetActiveLine(const AValue: TBCEditorActiveLine);
begin
  FActiveLine.Assign(AValue);
end;

procedure TCustomBCEditor.SetBookmark(const AIndex: Integer; const ALinesPosition: TBCEditorLinesPosition);
var
  LBookmark: TBCEditorMark;
begin
  if (ALinesPosition.Line >= 0) and (ALinesPosition.Line <= Max(0, FLines.Count - 1)) then
  begin
    LBookmark := FBookmarkList.Find(AIndex);
    if Assigned(LBookmark) then
      DeleteBookmark(LBookmark);

    LBookmark := TBCEditorMark.Create(Self);
    LBookmark.Line := ALinesPosition.Line;
    LBookmark.Char := ALinesPosition.Char;
    LBookmark.ImageIndex := Min(AIndex, BCEDITOR_BOOKMARKS - 1);
    LBookmark.Index := AIndex;
    LBookmark.Visible := True;
    FBookmarkList.Add(LBookmark);
    FBookmarkList.Sort(CompareLines);
    if Assigned(FOnAfterBookmarkPlaced) then
      FOnAfterBookmarkPlaced(Self);
    Invalidate();
  end;
end;

procedure TCustomBCEditor.SetBorderStyle(const AValue: TBorderStyle);
begin
  if FBorderStyle <> AValue then
  begin
    FBorderStyle := AValue;
    RecreateWnd;
  end;
end;

procedure TCustomBCEditor.SetCaretAndSelection(ACaretPosition: TBCEditorLinesPosition;
  ASelArea: TBCEditorLinesArea);
begin
  FLines.BeginUpdate();
  try
    FLines.CaretPosition := ACaretPosition;
    FLines.SelArea := ASelArea;
  finally
    FLines.EndUpdate();
  end;
end;

procedure TCustomBCEditor.SetCaretPos(AValue: TPoint);
begin
  FLines.CaretPosition := LinesPosition(AValue);
end;

procedure TCustomBCEditor.SetCodeFolding(const AValue: TBCEditorCodeFolding);
begin
  FCodeFolding.Assign(AValue);
  if AValue.Visible then
    InitCodeFolding;
end;

procedure TCustomBCEditor.SetColor(const AValue: TColor);
begin
  if FColor <> AValue then
  begin
    FColor := AValue;
    Color := AValue;
    Invalidate;
  end;
end;

procedure TCustomBCEditor.SetDefaultKeyCommands;
begin
  FKeyCommands.ResetDefaults;
end;

procedure TCustomBCEditor.SetHideScrollBars(AValue: Boolean);
begin
  if (AValue <> FHideScrollBars) then
  begin
    FHideScrollBars := AValue;
    if (UpdateCount > 0) then
      Include(FState, esScrolled)
    else
    begin
      UpdateScrollBars();
      Invalidate();
    end;
  end;
end;

procedure TCustomBCEditor.SetHideSelection(AValue: Boolean);
begin
  if (AValue <> FHideSelection) then
  begin
    FHideSelection := AValue;
    if (not Focused() and SelectionAvailable) then
      Invalidate();
  end;
end;

procedure TCustomBCEditor.SetKeyCommands(const AValue: TBCEditorKeyCommands);
begin
  if not Assigned(AValue) then
    FKeyCommands.Clear
  else
    FKeyCommands.Assign(AValue);
end;

procedure TCustomBCEditor.SetLeftMargin(const AValue: TBCEditorLeftMargin);
begin
  FLeftMargin.Assign(AValue);
end;

procedure TCustomBCEditor.SetHorzTextPos(AValue: Integer);
begin
  if (AValue <> FHorzTextPos) then
  begin
    InvalidateCaret();

    FHorzTextPos := AValue;

    if ((UpdateCount > 0) or (esUpdating in FState)) then
      Include(FState, esScrolled)
    else
    begin
      UpdateScrollBars();
      Invalidate();
    end;
  end;
end;

procedure TCustomBCEditor.SetInsertPos(AValue: TPoint);
var
  LClient: TPoint;
begin
  if (AValue <> FInsertPos) then
  begin
    if (HandleAllocated and Assigned(FInsertPosCache)) then
    begin
      if (FInsertPos <> InvalidCaretPos) then
      begin
        LClient := RowsToClient(LinesToRows(LinesPosition(FInsertPos)));
        if (FInsertPos.X = 0) then
          Canvas.CopyRect(
            Rect(LClient.X, LClient.Y, LClient.X + 2 * GLineWidth, LClient.Y + LineHeight),
            FInsertPosCache.Canvas,
            Rect(GLineWidth, 0, 3 * GLineWidth, LineHeight))
        else if (FInsertPos.X > 0) then
          Canvas.CopyRect(
            Rect(LClient.X - GLineWidth, LClient.Y, LClient.X + 2 * GLineWidth, LClient.Y + LineHeight),
            FInsertPosCache.Canvas,
            Rect(0, 0, 3 * GLineWidth, LineHeight));
      end;
      FInsertPosCache.Free();
      FInsertPosCache := nil;
    end;

    if (AValue.Y < 0) then
      FInsertPos := InvalidCaretPos
    else
    begin
      AValue.X := Max(AValue.X, 0);
      AValue.Y := Min(AValue.Y, Max(0, FLines.Count - 1));
      if (AValue.Y < FLines.Count) then
        AValue.X := Min(AValue.X, FRows.Items[AValue.Y].Length)
      else
        AValue.X := 0;

      FInsertPos := AValue;

      if (HandleAllocated
        and (FInsertPos <> InvalidCaretPos)
        and (LinesToRows(LinesPosition(FInsertPos)).Row in [TopRow .. TopRow + VisibleRows + 1])) then
      begin
        LClient := RowsToClient(LinesToRows(LinesPosition(FInsertPos)));
        FInsertPosCache := TBitmap.Create();
        FInsertPosCache.Handle := CreateCompatibleBitmap(Canvas.Handle, 3 * GLineWidth, LineHeight);

        if (AValue.X = 0) then
        begin
          FInsertPosCache.Canvas.CopyRect(
            Rect(GLineWidth, 0, 3 * GLineWidth, LineHeight),
            Canvas,
            Rect(LClient.X, LClient.Y, LClient.X + 2 * GLineWidth, LClient.Y + LineHeight));
          Canvas.BrushCopy(
            Rect(LClient.X, LClient.Y, LClient.X + 2 * GLineWidth, LClient.Y + LineHeight),
            FInsertPosBitmap,
            Rect(GLineWidth, 0, 3 * GLineWidth, LineHeight),
            FInsertPosBitmap.TransparentColor);
        end
        else
        begin
          FInsertPosCache.Canvas.CopyRect(
            Rect(0, 0, 3 * GLineWidth, LineHeight),
            Canvas,
            Rect(LClient.X - GLineWidth, LClient.Y, LClient.X + 2 * GLineWidth, LClient.Y + LineHeight));
          Canvas.BrushCopy(
            Rect(LClient.X - GLineWidth, LClient.Y, LClient.X + 2 * GLineWidth, LClient.Y + LineHeight),
            FInsertPosBitmap,
            Rect(0, 0, 3 * GLineWidth, LineHeight),
            FInsertPosBitmap.TransparentColor);
        end;
      end;
    end;
  end;
end;

procedure TCustomBCEditor.SetLineColor(const ALine: Integer; const AForegroundColor, ABackgroundColor: TColor);
begin
  if (ALine >= 0) and (ALine < FLines.Count) then
  begin
    FLines.SetForeground(ALine, AForegroundColor);
    FLines.SetBackground(ALine, ABackgroundColor);
    Invalidate;
  end;
end;

procedure TCustomBCEditor.SetLineColorToDefault(const ALine: Integer);
begin
  if (ALine >= 0) and (ALine < FLines.Count) then
    Invalidate;
end;

procedure TCustomBCEditor.SetMark(const AIndex: Integer; const ALinesPosition: TBCEditorLinesPosition;
  const AImageIndex: Integer);
var
  LMark: TBCEditorMark;
begin
  if (ALinesPosition.Line >= 0) and (ALinesPosition.Line <= Max(0, FLines.Count - 1)) then
  begin
    LMark := FMarkList.Find(AIndex);
    if Assigned(LMark) then
      DeleteMark(LMark);

    LMark := TBCEditorMark.Create(Self);
    with LMark do
    begin
      Line := ALinesPosition.Line;
      Char := ALinesPosition.Char + 1;
      ImageIndex := AImageIndex;
      Index := AIndex;
      Visible := True;
    end;
    if Assigned(FOnBeforeMarkPlaced) then
      FOnBeforeMarkPlaced(Self, LMark);
    FMarkList.Add(LMark);
    FMarkList.Sort(CompareLines);
    if Assigned(FOnAfterMarkPlaced) then
      FOnAfterMarkPlaced(Self);
  end;
end;

procedure TCustomBCEditor.SetModified(const AValue: Boolean);
begin
  FLines.Modified := AValue;
end;

procedure TCustomBCEditor.SetMouseCapture(const AValue: TMouseCapture);
begin
  if (AValue <> FMouseCapture) then
  begin
    FMouseCapture := AValue;

    inherited MouseCapture := FMouseCapture <> mcNone;
  end;
end;

procedure TCustomBCEditor.SetMouseMoveScrollCursors(const AIndex: Integer; const AValue: HCursor);
begin
  if (AIndex >= Low(FMouseMoveScrollCursors)) and (AIndex <= High(FMouseMoveScrollCursors)) then
    FMouseMoveScrollCursors[AIndex] := AValue;
end;

procedure TCustomBCEditor.SetName(const AValue: TComponentName);
var
  LTextToName: Boolean;
begin
  LTextToName := (ComponentState * [csDesigning, csLoading] = [csDesigning]) and (TrimRight(Text) = Name);
  inherited SetName(AValue);
  if LTextToName then
    Text := AValue;
end;

procedure TCustomBCEditor.SetOption(const AOption: TBCEditorOption; const AEnabled: Boolean);
begin
  if AEnabled then
    Include(FOptions, AOption)
  else
    Exclude(FOptions, AOption);
end;

procedure TCustomBCEditor.SetOptions(const AValue: TBCEditorOptions);
begin
  if (AValue <> FOptions) then
  begin
    FOptions := AValue;

    if (eoTrimTrailingLines in Options) then
      FLines.Options := FLines.Options + [loTrimTrailingLines]
    else
      FLines.Options := FLines.Options - [loTrimTrailingLines];
    if (eoTrimTrailingSpaces in Options) then
      FLines.Options := FLines.Options + [loTrimTrailingSpaces]
    else
      FLines.Options := FLines.Options - [loTrimTrailingSpaces];

    if (eoDropFiles in FOptions) <> (eoDropFiles in AValue) and not (csDesigning in ComponentState) and HandleAllocated then
      DragAcceptFiles(Handle, eoDropFiles in FOptions);

    Invalidate;
  end;
end;

procedure TCustomBCEditor.SetReadOnly(const AValue: Boolean);
begin
  FLines.ReadOnly := AValue;
end;

procedure TCustomBCEditor.SetScroll(const AValue: TBCEditorScroll);
begin
  FScroll.Assign(AValue);
end;

procedure TCustomBCEditor.SetSearch(const AValue: TBCEditorSearch);
begin
  FSearch.Assign(AValue);
end;

procedure TCustomBCEditor.SetSelectedWord();
begin
  SetWordBlock(FLines.CaretPosition);
end;

procedure TCustomBCEditor.SetSelection(const AValue: TBCEditorSelection);
begin
  FSelection.Assign(AValue);
end;

procedure TCustomBCEditor.SetSelectionBeginPosition(const AValue: TBCEditorLinesPosition);
begin
  FLines.SelArea := LinesArea(AValue, FLines.SelArea.EndPosition);
end;

procedure TCustomBCEditor.SetSelectionEndPosition(const AValue: TBCEditorLinesPosition);
begin
  FLines.SelArea := LinesArea(FLines.SelArea.BeginPosition, AValue);
end;

procedure TCustomBCEditor.SetSelLength(const AValue: Integer);
begin
  FLines.SelArea := LinesArea(FLines.SelArea.BeginPosition, FLines.PositionOf(AValue, Min(FLines.SelArea.BeginPosition, FLines.SelArea.EndPosition)));
end;

procedure TCustomBCEditor.SetSelStart(const AValue: Integer);
begin
  FLines.CaretPosition := FLines.PositionOf(AValue);
end;

procedure TCustomBCEditor.SetSelText(const AValue: string);
var
  LArea: TBCEditorLinesArea;
begin
  ClearCodeFolding();

  FLines.BeginUpdate();

  LArea.BeginPosition := Min(FLines.SelArea.BeginPosition, FLines.SelArea.EndPosition);

  if (AValue = '') then
    FLines.DeleteText(LinesArea(LArea.BeginPosition, Max(FLines.SelArea.BeginPosition, FLines.SelArea.EndPosition)))
  else
    FLines.SelArea := LinesArea(LArea.BeginPosition, FLines.ReplaceText(LinesArea(LArea.BeginPosition, Max(FLines.SelArea.BeginPosition, FLines.SelArea.EndPosition)), AValue));

  FLines.EndUpdate();

  InitCodeFolding();
end;

procedure TCustomBCEditor.SetSpecialChars(const AValue: TBCEditorSpecialChars);
begin
  SpecialChars.Assign(AValue);
end;

procedure TCustomBCEditor.SetSyncEdit(const AValue: TBCEditorSyncEdit);
begin
  FSyncEdit.Assign(AValue);
end;

procedure TCustomBCEditor.SetTabs(const AValue: TBCEditorTabs);
begin
  FTabs.Assign(AValue);
end;

procedure TCustomBCEditor.SetText(const AValue: string);
begin
  FLines.Text := AValue;
end;

procedure TCustomBCEditor.SetTopRow(const AValue: Integer);
var
  LValue: Integer;
begin
  LValue := AValue;
  if (not (soBeyondEndOfLine in FScroll.Options)) then
    LValue := Min(AValue, FRows.Count - VisibleRows + 1);
  LValue := Max(0, LValue);

  if (LValue <> FTopRow) then
  begin
    InvalidateCaret();

    FTopRow := LValue;

    if ((UpdateCount > 0) or (esUpdating in FState)) then
      Include(FState, esScrolled)
    else
    begin
      UpdateScrollBars();
      Invalidate();
    end;
  end;
end;

procedure TCustomBCEditor.SetUndoOption(const AOption: TBCEditorUndoOption; const AEnabled: Boolean);
begin
  case (AOption) of
    uoGroupUndo:
      if (AEnabled) then
        FLines.Options := FLines.Options + [loUndoGrouped]
      else
        FLines.Options := FLines.Options - [loUndoGrouped];
    uoUndoAfterLoad:
      if (AEnabled) then
        FLines.Options := FLines.Options + [loUndoAfterLoad]
      else
        FLines.Options := FLines.Options - [loUndoAfterLoad];
    uoUndoAfterSave:
      if (AEnabled) then
        FLines.Options := FLines.Options + [loUndoAfterSave]
      else
        FLines.Options := FLines.Options - [loUndoAfterSave];
  end;
end;

procedure TCustomBCEditor.SetUndoOptions(AOptions: TBCEditorUndoOptions);
var
  LLinesOptions: TBCEditorLines.TOptions;
begin
  LLinesOptions := FLines.Options;
  LLinesOptions := LLinesOptions - [loUndoGrouped, loUndoAfterLoad, loUndoAfterSave];
  if (uoGroupUndo in AOptions) then
    LLinesOptions := LLinesOptions + [loUndoGrouped];
  if (uoUndoAfterLoad in AOptions) then
    LLinesOptions := LLinesOptions + [loUndoAfterLoad];
  if (uoUndoAfterSave in AOptions) then
    LLinesOptions := LLinesOptions + [loUndoAfterSave];
  FLines.Options := LLinesOptions;
end;

procedure TCustomBCEditor.SetUpdateState(AUpdating: Boolean);
begin
  if (AUpdating) then
  begin
    Assert(not (esUpdating in FState));

    if (HandleAllocated and Visible and not (esHandlingMouse in FState)) then
      SendMessage(Handle, WM_SETREDRAW, WPARAM(FALSE), 0);
  end
  else
  begin
    Assert(FLines.UndoList.UpdateCount = 0);

    Include(FState, esUpdating);
    try
      if (FState * [esLinesCleared, esLinesDeleted, esLinesInserted] <> []) then
        LinesChanged();

      if (FState * [esLinesCleared] <> []) then
        InitCodeFolding();

      if (FState * [esCaretChanged, esRowsChanged, esLinesCleared, esLinesDeleted, esLinesInserted, esLinesUpdated] <> []) then
        if (FState * [esFindind, esReplacing] = []) then
          FSearchResults.Clear();

      if (HandleAllocated) then
      begin
        if (FState * [esCaretChanged, esRowsChanged] <> []) then
          ScrollToCaret();

        if (Visible) then
        begin
          if (not (esHandlingMouse in FState)) then
            SendMessage(Handle, WM_SETREDRAW, WPARAM(TRUE), 0);
          if (FState * [esRowsChanged, esLinesCleared, esLinesUpdated, esSizeChanged, esScrolled, esSelectionChanged] <> []) then
            Invalidate()
          else if (FState * [esCaretInvalid] <> []) then
          begin
            FCaretPos := RowsToClient(FRows.CaretPosition);
            UpdateCaret();
          end;
        end;

        if (FState * [esRowsChanged, esLinesCleared, esLinesUpdated, esScrolled] <> []) then
          UpdateScrollBars(False, True);

        FState := FState - [esScrolled];
      end;

      if ((FState * [esLinesCleared, esLinesDeleted, esLinesInserted, esLinesUpdated] <> [])
        and HandleAllocated) then
      begin
        Include(FState, esCodeFoldingInvalid);
        KillTimer(Handle, tiCodeFolding);
        SetTimer(Handle, tiCodeFolding, FCodeFolding.DelayInterval, nil);
      end;

      FState := FState - [esRowsChanged];

      if (Assigned(FOnCaretChanged) and (FState * [esCaretChanged] <> [])) then
        FOnCaretChanged(Self, CaretPos);
      FState := FState - [esCaretChanged];

      if (Assigned(OnChange)
        and (FState * [esLinesCleared, esLinesDeleted, esLinesInserted, esLinesUpdated] <> [])
        and not (csReading in ComponentState)) then
        OnChange(Self);
      FState := FState - [esLinesCleared, esLinesDeleted, esLinesInserted, esLinesUpdated];
    finally
      Exclude(FState, esUpdating);
    end;
  end;
end;

procedure TCustomBCEditor.SetWantReturns(const AValue: Boolean);
begin
  FWantReturns := AValue;
end;

procedure TCustomBCEditor.SetWordBlock(const ALinesPosition: TBCEditorLinesPosition);
var
  LArea: TBCEditorLinesArea;
  LLineTextLength: Integer;
begin
  if (ALinesPosition.Line < FLines.Count) then
  begin
    LLineTextLength := Length(FLines.Items[ALinesPosition.Line].Text);

    LArea.BeginPosition := LinesPosition(Min(ALinesPosition.Char, LLineTextLength), ALinesPosition.Line);
    while ((LArea.BeginPosition.Char > 0)
      and not IsWordBreakChar(FLines.Items[ALinesPosition.Line].Text[1 + LArea.BeginPosition.Char - 1])) do
      Dec(LArea.BeginPosition.Char);
    while ((LArea.BeginPosition.Char > 0)
      and not IsWordBreakChar(FLines.Items[ALinesPosition.Line].Text[1 + LArea.BeginPosition.Char - 1])) do
      Dec(LArea.BeginPosition.Char);
    if ((soExpandRealNumbers in FSelection.Options) and FLines.Items[ALinesPosition.Line].Text[1 + LArea.BeginPosition.Char - 1].IsNumber) then
      while ((LArea.BeginPosition.Char > 0)
        and (FLines.Items[ALinesPosition.Line].Text[1 + LArea.BeginPosition.Char].IsNumber or CharInSet(FLines.Items[ALinesPosition.Line].Text[1 + LArea.BeginPosition.Char - 1], BCEDITOR_REAL_NUMBER_CHARS))) do
        Dec(LArea.BeginPosition.Char);

    LArea.EndPosition := LArea.BeginPosition;
    while ((LArea.EndPosition.Char < LLineTextLength)
      and not IsWordBreakChar(FLines.Items[ALinesPosition.Line].Text[1 + LArea.EndPosition.Char])) do
      Inc(LArea.EndPosition.Char);
    if ((soExpandRealNumbers in FSelection.Options) and FLines.Items[ALinesPosition.Line].Text[1 + LArea.BeginPosition.Char + 1].IsNumber) then
      while ((LArea.EndPosition.Char + 1 < LLineTextLength)
        and (FLines.Items[ALinesPosition.Line].Text[1 + LArea.EndPosition.Char + 1].IsNumber or CharInSet(FLines.Items[ALinesPosition.Line].Text[1 + LArea.EndPosition.Char + 1], BCEDITOR_REAL_NUMBER_CHARS))) do
        Inc(LArea.EndPosition.Char);

    SetCaretAndSelection(LArea.EndPosition, LArea);
  end;
end;

procedure TCustomBCEditor.SetWordWrap(const AValue: TBCEditorWordWrap);
begin
  FWordWrap.Assign(AValue);
end;

procedure TCustomBCEditor.Sort(const ASortOrder: TBCEditorSortOrder = soAsc; const ACaseSensitive: Boolean = False);
var
  LBeginLine: Integer;
  LEndLine: Integer;
  LLine: Integer;
  LSelectionBeginPosition: TBCEditorLinesPosition;
  LSelectionEndPosition: TBCEditorLinesPosition;
begin
  for LLine := 0 to FLines.Count - 1 do
  begin
    FLines.SetCodeFoldingBeginRange(LLine, nil);
    FLines.SetCodeFoldingEndRange(LLine, nil);
    FLines.SetCodeFoldingTreeLine(LLine, False);
  end;

  if (SelectionAvailable) then
  begin
    LSelectionBeginPosition := SelectionEndPosition;
    LSelectionEndPosition := SelectionEndPosition;

    LBeginLine := LSelectionBeginPosition.Line;
    LEndLine := LSelectionEndPosition.Line;
    if ((LSelectionEndPosition.Char = 0) and (LSelectionEndPosition.Line > LSelectionBeginPosition.Line)) then
      Dec(LEndLine);
  end
  else
  begin
    LBeginLine := 0;
    LEndLine := FLines.Count - 1;
  end;

  FLines.CaseSensitive := ACaseSensitive;
  FLines.SortOrder := ASortOrder;
  FLines.Sort(LBeginLine, LEndLine);

  if (HandleAllocated) then
  begin
    Include(FState, esCodeFoldingInvalid);
    KillTimer(Handle, tiCodeFolding);
    SetTimer(Handle, tiCodeFolding, FCodeFolding.DelayInterval, nil);
  end;
end;

procedure TCustomBCEditor.SpecialCharsChanged(ASender: TObject);
begin
  FSpecialCharsNullText := '';
  FSpecialCharsSpaceText := '';
  if (UpdateCount > 0) then
    Include(FState, esScrolled)
  else
  begin
    UpdateScrollBars();
    Invalidate();
  end;
end;

function TCustomBCEditor.SplitTextIntoWords(AStringList: TStrings; const ACaseSensitive: Boolean): string;
var
  LSkipCloseKeyChars: TBCEditorCharSet;
  LSkipOpenKeyChars: TBCEditorCharSet;
  LSkipRegionItem: TBCEditorCodeFolding.TSkipRegions.TItem;

  procedure AddKeyChars();
  var
    LIndex: Integer;
    LTokenEndPos: PChar;
    LTokenPos: PChar;
    LTokenText: string;
  begin
    LSkipOpenKeyChars := [];
    LSkipCloseKeyChars := [];

    for LIndex := 0 to FHighlighter.CompletionProposalSkipRegions.Count - 1 do
    begin
      LSkipRegionItem := FHighlighter.CompletionProposalSkipRegions[LIndex];

      LTokenText := LSkipRegionItem.OpenToken;
      if (LTokenText <> '') then
      begin
        LTokenPos := @LTokenText[1];
        LTokenEndPos := @LTokenText[Length(LTokenText)];
        while (LTokenPos <= LTokenEndPos) do
          LSkipOpenKeyChars := LSkipOpenKeyChars + [LTokenPos^];

        LTokenText := LSkipRegionItem.CloseToken;
        LTokenPos := @LTokenText[1];
        LTokenEndPos := @LTokenText[Length(LTokenText)];
        while (LTokenPos <= LTokenEndPos) do
          LSkipCloseKeyChars := LSkipCloseKeyChars + [LTokenPos^];
      end;
    end;
  end;

var
  LIndex: Integer;
  LLine: Integer;
  LLineEndPos: PChar;
  LLinePos: PChar;
  LOpenTokenSkipFoldRangeList: TList;
  LPBookmarkText: PChar;
  LStringList: TStringList;
  LTokenEndPos: PChar;
  LTokenPos: PChar;
  LTokenText: string;
  LWord: string;
  LWordList: string;
begin
  Result := '';
  AddKeyChars;
  AStringList.Clear;
  LOpenTokenSkipFoldRangeList := TList.Create;
  try
    for LLine := 0 to FLines.Count - 1 do
      if (FLines.Items[LLine].Text <> '') then
      begin
        { Add document words }
        LLinePos := @FLines.Items[LLine].Text[1];
        LLineEndPos := @FLines.Items[LLine].Text[Length(FLines.Items[LLine].Text)];
        LWord := '';
        while (LLinePos <= LLineEndPos) do
        begin
          { Skip regions - Close }
          if (LOpenTokenSkipFoldRangeList.Count > 0) and CharInSet(LLinePos^, LSkipCloseKeyChars) then
          begin
            LTokenText := TBCEditorCodeFolding.TSkipRegions.TItem(LOpenTokenSkipFoldRangeList.Last).CloseToken;
            if (LTokenText <> '') then
            begin
              LTokenPos := @LTokenText[1];
              LTokenEndPos := @LTokenText[Length(LTokenText)];
              LPBookmarkText := LLinePos;
              { Check if the close keyword found }
              while ((LLinePos <= LLineEndPos) and (LTokenPos <= LTokenEndPos)
                and (LLinePos^ = LTokenPos^)) do
              begin
                Inc(LLinePos);
                Inc(LTokenPos);
              end;
              if (LTokenPos > LTokenEndPos) then { If found, pop skip region from the list }
              begin
                LOpenTokenSkipFoldRangeList.Delete(LOpenTokenSkipFoldRangeList.Count - 1);
                Continue;
              end
              else
                LLinePos := LPBookmarkText;
                { Skip region close not found, return pointer back }
            end;
          end;

          { Skip regions - Open }
          if (CharInSet(LLinePos^, LSkipOpenKeyChars)) then
          begin
            for LIndex := 0 to FHighlighter.CompletionProposalSkipRegions.Count - 1 do
            begin
              LSkipRegionItem := FHighlighter.CompletionProposalSkipRegions[LIndex];
              LTokenText := LSkipRegionItem.OpenToken;
              if ((LTokenText <> '') and (LLinePos^ = LTokenText[1])) then { If the first character is a match }
              begin
                LTokenPos := @LTokenText[1];
                LTokenEndPos := @LTokenText[Length(LTokenText)];
                LPBookmarkText := LLinePos;
                { Check if the open keyword found }
                while ((LLinePos <= LLineEndPos) and (LTokenPos <= LTokenEndPos)
                  and (LLinePos^ = LTokenPos^)) do
                begin
                  Inc(LLinePos);
                  Inc(LTokenPos);
                end;
                if (LTokenPos > LTokenEndPos) then { If found, skip single line comment or push skip region into stack }
                begin
                  if LSkipRegionItem.RegionType = ritSingleLineComment then
                    { Single line comment skip until next line }
                    LLinePos := LLineEndPos
                  else
                    LOpenTokenSkipFoldRangeList.Add(LSkipRegionItem);
                  Dec(LLinePos); { The end of the while loop will increase }
                  Break;
                end
                else
                  LLinePos := LPBookmarkText;
                { Skip region open not found, return pointer back }
              end;
            end;
          end;

          if LOpenTokenSkipFoldRangeList.Count = 0 then
          begin
            if ((LWord = '') and (LLinePos^.IsLower or LLinePos^.IsUpper or (LLinePos^ = BCEDITOR_UNDERSCORE))
              or (LWord <> '') and (LLinePos^.IsLower or LLinePos^.IsUpper or LLinePos^.IsNumber or (LLinePos^ = BCEDITOR_UNDERSCORE))) then
              LWord := LWord + LLinePos^
            else
            begin
              if (LWord <> '') and (Length(LWord) > 1) then
                if Pos(LWord + FLines.LineBreak, LWordList) = 0 then { No duplicates }
                  LWordList := LWordList + LWord + FLines.LineBreak;
              LWord := ''
            end;
          end;
          LLinePos := LLineEndPos;
        end;
        if (Length(LWord) > 1) then
          if Pos(LWord + FLines.LineBreak, LWordList) = 0 then { No duplicates }
            LWordList := LWordList + LWord + FLines.LineBreak;
      end;
    LStringList := TStringList.Create();
    LStringList.LineBreak := FLines.LineBreak;
    LStringList.Text := LWordList;
    LStringList.Sort();
    AStringList.Assign(LStringList);
    LStringList.Free();
  finally
    LOpenTokenSkipFoldRangeList.Free;
  end;
end;

procedure TCustomBCEditor.SyncEditChanged(ASender: TObject);
var
  LIndex: Integer;
  LIsWordSelected: Boolean;
  LSelectionAvailable: Boolean;
  LTextPosition: TBCEditorLinesPosition;
begin
  FSyncEdit.ClearSyncItems;
  if FSyncEdit.Active then
  begin
    WordWrap.Enabled := False;
    LSelectionAvailable := SelectionAvailable;
    LIsWordSelected := IsWordSelected;
    if LSelectionAvailable and LIsWordSelected then
    begin
      FLines.BeginUpdate();
      try
        FSyncEdit.InEditor := True;
        FSyncEdit.EditArea := FLines.SelArea;
        FSyncEdit.EditWidth := FSyncEdit.EditArea.EndPosition.Char - FSyncEdit.EditArea.BeginPosition.Char;
        FindWords(SelText, FSyncEdit.SyncItems, seCaseSensitive in FSyncEdit.Options, True);
        LIndex := 0;
        while LIndex < FSyncEdit.SyncItems.Count do
        begin
          LTextPosition := PBCEditorTextPosition(FSyncEdit.SyncItems.Items[LIndex])^;
          if (LTextPosition.Line = FSyncEdit.EditArea.BeginPosition.Line) and
            (LTextPosition.Char = FSyncEdit.EditArea.BeginPosition.Char) or FSyncEdit.BlockSelected and
            not FSyncEdit.BlockArea.Containts(LTextPosition) then
          begin
            Dispose(PBCEditorTextPosition(FSyncEdit.SyncItems.Items[LIndex]));
            FSyncEdit.SyncItems.Delete(LIndex);
          end
          else
            Inc(LIndex);
        end;
      finally
        FLines.EndUpdate();
      end;
    end
    else
    if LSelectionAvailable and not LIsWordSelected then
    begin
      FSyncEdit.BlockSelected := True;
      FSyncEdit.BlockArea := LinesArea(Min(FLines.SelArea.BeginPosition, FLines.SelArea.EndPosition), Max(FLines.SelArea.BeginPosition, FLines.SelArea.EndPosition));
      FSyncEdit.Abort;
      FLines.SelArea := LinesArea(FLines.CaretPosition, FLines.CaretPosition);
    end
    else
      FSyncEdit.Abort;
  end
  else
  begin
    FSyncEdit.BlockSelected := False;
    if FSyncEdit.InEditor then
    begin
      FSyncEdit.InEditor := False;
      FLines.EndUpdate;
    end;
  end;
  Invalidate;
end;

procedure TCustomBCEditor.TabsChanged(ASender: TObject);
begin
  if (WordWrap.Enabled) then
    FRows.Clear();
  InvalidateCaret();
  Invalidate();
end;

function TCustomBCEditor.TextCaretPosition(): TBCEditorLinesPosition;
begin
  Result := FLines.CaretPosition;
end;

function TCustomBCEditor.TokenWidth(const AText: PChar;
  const ALength: Integer; const AColumn: Integer;
  const AToken: TBCEditorHighlighter.TFind): Integer;
var
  LRect: TRect;
begin
  LRect := Rect(0, 0, MaxInt, MaxInt);
  ProcessToken(paNone, mbLeft, [], -1, -1, LRect,
    InvalidLinesPosition, RowsPosition(AColumn, -1),
    AText, ALength, @AToken);
  Result := LRect.Left;
end;

procedure TCustomBCEditor.ToggleBookmark(const AIndex: Integer = -1);
begin
  if (AIndex = -1) then
    DoToggleBookmark(FLines.CaretPosition)
  else if (not DeleteBookmark(FLines.CaretPosition.Line, AIndex)) then
    SetBookmark(AIndex, FLines.CaretPosition);
end;

procedure TCustomBCEditor.ToggleSelectedCase(const ACase: TBCEditorCase = cNone);
var
  LCommand: TBCEditorCommand;
  LSelArea: TBCEditorLinesArea;
begin
  if AnsiUpperCase(SelText) <> AnsiUpperCase(FSelectedCaseText) then
  begin
    FSelectedCaseCycle := cUpper;
    FSelectedCaseText := SelText;
  end;
  if ACase <> cNone then
    FSelectedCaseCycle := ACase;

  BeginUpdate();

  LSelArea := FLines.SelArea;
  LCommand := ecNone;
  case FSelectedCaseCycle of
    cUpper: { UPPERCASE }
      LCommand := ecUpperCase;
    cLower: { lowercase }
      LCommand := ecLowerCase;
    cOriginal: { Original text }
      SelText := FSelectedCaseText;
  end;
  if FSelectedCaseCycle <> cOriginal then
    CommandProcessor(LCommand, BCEDITOR_NONE_CHAR, nil);
  FLines.SelArea := LSelArea;

  EndUpdate();

  Inc(FSelectedCaseCycle);
  if FSelectedCaseCycle > cOriginal then
    FSelectedCaseCycle := cUpper;
end;

function TCustomBCEditor.TranslateKeyCode(const ACode: Word; const AShift: TShiftState; var AData: Pointer): TBCEditorCommand;
var
  LIndex: Integer;
begin
  LIndex := KeyCommands.FindKeycodes(FLastKey, FLastShiftState, ACode, AShift);
  if LIndex >= 0 then
    Result := KeyCommands[LIndex].Command
  else
  begin
    LIndex := KeyCommands.FindKeycode(ACode, AShift);
    if LIndex >= 0 then
      Result := KeyCommands[LIndex].Command
    else
      Result := ecNone;
  end;
  if (Result = ecNone) and (ACode >= VK_ACCEPT) and (ACode <= VK_SCROLL) then
  begin
    FLastKey := ACode;
    FLastShiftState := AShift;
  end
  else
  begin
    FLastKey := 0;
    FLastShiftState := [];
  end;
end;

procedure TCustomBCEditor.UMFreeCompletionProposalPopupWindow(var AMessage: TMessage);
begin
  if (Assigned(FCompletionProposalPopupWindow)) then
  begin
    FCompletionProposalPopupWindow.Free;
    FCompletionProposalPopupWindow := nil;
  end;
end;

procedure TCustomBCEditor.Undo();
begin
  FLines.Undo();
end;

procedure TCustomBCEditor.UnhookEditorLines;
var
  LOldWrap: Boolean;
begin
  Assert(not Assigned(FChainedEditor));
  if FLines = FOriginalLines then
    Exit;

  LOldWrap := WordWrap.Enabled;
  UpdateWordWrap(False);

  with FLines do
  begin
    OnCleared := FOnChainLinesCleared;
    OnDeleted := FOnChainLinesDeleted;
    OnInserted := FOnChainLinesInserted;
    OnUpdated := FOnChainLinesUpdated;
  end;

  FOnChainLinesCleared := nil;
  FOnChainLinesDeleted := nil;
  FOnChainLinesInserted := nil;
  FOnChainLinesUpdated := nil;

  FLines := FOriginalLines;
  LinesHookChanged;

  UpdateWordWrap(LOldWrap);
end;

procedure TCustomBCEditor.UnregisterCommandHandler(AHookedCommandEvent: TBCEditorHookedCommandEvent);
var
  LIndex: Integer;
begin
  if not Assigned(AHookedCommandEvent) then
    Exit;
  LIndex := FindHookedCommandEvent(AHookedCommandEvent);
  if LIndex > -1 then
    FHookedCommandHandlers.Delete(LIndex)
end;

function TCustomBCEditor.UpdateAction(Action: TBasicAction): Boolean;
begin
  Result := Focused;

  if (Result) then
    if Action is TEditCut then
      TEditCut(Action).Enabled := not ReadOnly and SelectionAvailable
    else if Action is TEditCopy then
      TEditCopy(Action).Enabled := SelectionAvailable
    else if Action is TEditPaste then
      TEditPaste(Action).Enabled := Focused() and CanPaste
    else if Action is TEditDelete then
      TEditDelete(Action).Enabled := not ReadOnly and SelectionAvailable
    else if Action is TEditSelectAll then
      TEditSelectAll(Action).Enabled := (FLines.Count > 0)
    else if Action is TEditUndo then
      TEditUndo(Action).Enabled := not ReadOnly and FLines.CanUndo
    else if Action is TSearchFindNext then
      // must be before TSearchFind, since TSearchFindNext is TSearchFind too
      TSearchFindNext(Action).Enabled := Search.Pattern <> ''
    else if Action is TSearchReplace then
      // must be before TSearchFind, since TSearchReplace is TSearchFind too
      TSearchReplace(Action).Enabled := (FLines.Count > 0)
    else if Action is TSearchFind then
      TSearchFind(Action).Enabled := (FLines.Count > 0)
    else
      Result := inherited;
end;

procedure TCustomBCEditor.UpdateCaret();
var
//  LCompForm: TCompositionForm;
  LRect: TRect;
//  LImc: HIMC;
begin
  if (HandleAllocated) then
  begin
    LRect := ClientRect;
    Inc(LRect.Left, FLeftMarginWidth);
    if (not LRect.Contains(FCaretPos)
      or not Focused() and not Assigned(FCompletionProposalPopupWindow)) then
    begin
      if (FCaretVisible) then
      begin
        HideCaret(Handle);
        FCaretVisible := False;
      end;
    end
    else
    begin
      Windows.SetCaretPos(FCaretPos.X, FCaretPos.Y);

//      LCompForm.dwStyle := CFS_POINT;
//      LCompForm.ptCurrentPos := FCaretPos;
//      LImc := ImmGetContext(Handle);
//      ImmSetCompositionWindow(LImc, @LCompForm);
//      ImmReleaseContext(Handle, LImc);

      if (not FCaretVisible) then
      begin
        ShowCaret(Handle);
        FCaretVisible := True;
      end;
    end;

    FState := FState - [esCaretInvalid];
  end;
end;

procedure TCustomBCEditor.UpdateLinesBeginRanges(const ALine: Integer);
var
  LLine: Integer;
  LRange: TBCEditorHighlighter.TRange;
  LToken: TBCEditorHighlighter.TFind;
begin
  Assert(ALine < FLines.Count);

  LLine := ALine;

  while (LLine < FLines.Count - 1) do
  begin
    if (FHighlighter.FindFirstToken(FLines.Items[LLine].BeginRange, FLines.Items[LLine].Text, LToken)) then
      LRange := FLines.Items[LLine].BeginRange
    else
      repeat
        LRange := LToken.Range;
      until (not FHighlighter.FindNextToken(LToken));

    if (LRange = FLines.Items[LLine + 1].BeginRange) then
      exit;

    FLines.SetBeginRange(LLine + 1, LRange);
    Inc(LLine);
  end;
end;

procedure TCustomBCEditor.UpdateRows(const ALine: Integer);
var
  LDeletedRows: Integer;
  LLine: Integer;
  LRow: Integer;
begin
  if (FRows.Count > 0) then
  begin
    LRow := FLines.Items[ALine].FirstRow;

    if (LRow >= 0) then
    begin
      LDeletedRows := 0;
      while ((LRow < FRows.Count) and (FRows.Items[LRow].Line = ALine)) do
      begin
        FRows.Delete(LRow);
        Inc(LDeletedRows);
      end;

      if (LDeletedRows > 0) then
        for LLine := ALine + 1 to FLines.Count - 1 do
          if (FLines.Items[LLine].FirstRow >= 0) then
            FLines.SetFirstRow(LLine, FLines.Items[LLine].FirstRow - LDeletedRows);

      InsertLineIntoRows(ALine, LRow);
    end;
  end;
end;

procedure TCustomBCEditor.UpdateScrollBars(const AUpdateRows: Boolean = True;
  const AUpdateWhileUpdating: Boolean = False);
var
  LHorzScrollInfo: TScrollInfo;
  LVertScrollInfo: TScrollInfo;
begin
  if (HandleAllocated
    and (not (esUpdating in FState) or AUpdateWhileUpdating)
    and not (esUpdatingScrollBars in FState)) then
  begin
    Include(FState, esUpdatingScrollBars);

    try
      LVertScrollInfo.cbSize := SizeOf(ScrollInfo);
      LVertScrollInfo.fMask := SIF_PAGE or SIF_POS or SIF_RANGE;
      LVertScrollInfo.nMin := 0;
      LVertScrollInfo.nMax := Max(FRows.CaretPosition.Row, FRows.Count - 1);
      LVertScrollInfo.nPage := VisibleRows;
      LVertScrollInfo.nPos := TopRow;
      LVertScrollInfo.nTrackPos := 0;
      SetScrollInfo(Handle, SB_VERT, LVertScrollInfo, TRUE);

      LHorzScrollInfo.cbSize := SizeOf(ScrollInfo);
      LHorzScrollInfo.fMask := SIF_PAGE or SIF_POS or SIF_RANGE;
      LHorzScrollInfo.nMin := 0;
      if ((FRows.CaretPosition.Row < FRows.Count)
        and (FRows.CaretPosition.Column <= FRows.Items[FRows.CaretPosition.Row].Columns)) then
        LHorzScrollInfo.nMax := FRows.MaxWidth
      else
        LHorzScrollInfo.nMax := RowsToClient(FRows.CaretPosition).X;
      if (FSpecialChars.Visible) then
        Inc(LHorzScrollInfo.nMax, FLineBreakSignWidth);
      LHorzScrollInfo.nPage := FTextWidth;
      LHorzScrollInfo.nPos := HorzTextPos;
      LHorzScrollInfo.nTrackPos := 0;
      SetScrollInfo(Handle, SB_HORZ, LHorzScrollInfo, TRUE);

      ShowScrollBar(Handle, SB_VERT, not HideScrollBars or (LVertScrollInfo.nMax >= INT(LVertScrollInfo.nPage)));
      ShowScrollBar(Handle, SB_HORZ, not HideScrollBars or (LHorzScrollInfo.nMax >= INT(LHorzScrollInfo.nPage)));
    finally
      Exclude(FState, esUpdatingScrollBars);
    end;
  end;
end;

procedure TCustomBCEditor.UpdateWordWrap(const AValue: Boolean);
var
  LOldCaretInView: Boolean;
  LOldTopRow: Integer;
begin
  if (AValue <> WordWrap.Enabled) then
  begin
    LOldCaretInView := ClientRect.Contains(FCaretPos);
    LOldTopRow := TopRow;
    if (AValue) then
      HorzTextPos := 0;
    TopRow := LOldTopRow;

    if (LOldCaretInView) then
      ScrollToCaret()
    else if (UpdateCount > 0) then
      Include(FState, esScrolled)
    else
    begin
      UpdateScrollBars();
      Invalidate();
    end;
  end;
end;

procedure TCustomBCEditor.WMCaptureChanged(var AMessage: TMessage);
begin
  FScrollTimer.Enabled := False;
  inherited;
end;

procedure TCustomBCEditor.WMChar(var AMessage: TWMChar);
begin
  DoKeyPressW(AMessage);
end;

procedure TCustomBCEditor.WMClear(var AMessage: TMessage);
begin
  if not ReadOnly then
    SelText := '';
end;

procedure TCustomBCEditor.WMCopy(var AMessage: TMessage);
begin
  CopyToClipboard;
  AMessage.Result := Ord(True);
end;

procedure TCustomBCEditor.WMCut(var AMessage: TMessage);
begin
  if not ReadOnly then
    CutToClipboard;
  AMessage.Result := Ord(True);
end;

procedure TCustomBCEditor.WMEraseBkgnd(var AMessage: TMessage);
begin
  AMessage.Result := 1;
end;

procedure TCustomBCEditor.WMGetDlgCode(var AMessage: TWMGetDlgCode);
begin
  inherited;
  AMessage.Result := AMessage.Result or DLGC_WANTARROWS or DLGC_WANTCHARS;
  if FTabs.WantTabs then
    AMessage.Result := AMessage.Result or DLGC_WANTTAB;
  if FWantReturns then
    AMessage.Result := AMessage.Result or DLGC_WANTALLKEYS;
end;

procedure TCustomBCEditor.WMGetText(var AMessage: TWMGetText);
begin
  StrLCopy(PChar(AMessage.Text), PChar(Text), AMessage.TextMax - 1);
  AMessage.Result := StrLen(PChar(AMessage.Text));
end;

procedure TCustomBCEditor.WMGetTextLength(var AMessage: TWMGetTextLength);
begin
  if (csDocking in ControlState) or (csDestroying in ComponentState) then
    AMessage.Result := 0
  else
    AMessage.Result := FLines.GetTextLength();
end;

procedure TCustomBCEditor.WMHScroll(var AMessage: TWMScroll);
begin
  AMessage.Result := 0;

  inherited;

  case (AMessage.ScrollCode) of
    SB_LEFT:
      HorzTextPos := 0;
    SB_RIGHT:
      HorzTextPos := Max(0, FRows.MaxWidth - FTextWidth);
    SB_LINELEFT:
      HorzTextPos := Max(0, FHorzTextPos - FMaxDigitWidth);
    SB_LINERIGHT:
      HorzTextPos := Min(Max(0, FRows.MaxWidth - FTextWidth), FHorzTextPos + FMaxDigitWidth);
    SB_PAGELEFT:
      HorzTextPos := Max(0, FHorzTextPos - FTextWidth);
    SB_PAGERIGHT:
      HorzTextPos := Min(Max(0, FRows.MaxWidth - FTextWidth), FHorzTextPos + FTextWidth);
    SB_THUMBPOSITION,
    SB_THUMBTRACK:
      HorzTextPos := AMessage.Pos;
  end;
end;

procedure TCustomBCEditor.WMIMEChar(var AMessage: TMessage);
begin
  { Do nothing here, the IME string is retrieved in WMIMEComposition
    Handling the WM_IME_CHAR message stops Windows from sending WM_CHAR messages while using the IME }
end;

procedure TCustomBCEditor.WMIMEComposition(var AMessage: TMessage);
var
  LImc: HIMC;
  LImeCount: Integer;
  LPBuffer: PChar;
begin
  if (AMessage.LParam and GCS_RESULTSTR) <> 0 then
  begin
    LImc := ImmGetContext(Handle);
    try
      LImeCount := ImmGetCompositionString(LImc, GCS_RESULTSTR, nil, 0);
      { ImeCount is always the size in bytes, also for Unicode }
      GetMem(LPBuffer, LImeCount + SizeOf(Char));
      try
        ImmGetCompositionString(LImc, GCS_RESULTSTR, LPBuffer, LImeCount);
        LPBuffer[LImeCount div SizeOf(Char)] := BCEDITOR_NONE_CHAR;
        CommandProcessor(ecImeStr, BCEDITOR_NONE_CHAR, LPBuffer);
      finally
        FreeMem(LPBuffer);
      end;
    finally
      ImmReleaseContext(Handle, LImc);
    end;
  end;

  inherited;
end;

procedure TCustomBCEditor.WMIMENotify(var AMessage: TMessage);
var
  LImc: HIMC;
  LLogFont: TLogFont;
begin
  case (AMessage.wParam) of
    IMN_SETOPENSTATUS:
      begin
        LImc := ImmGetContext(Handle);
        GetObject(Font.Handle, SizeOf(TLogFont), @LLogFont);
        ImmSetCompositionFont(LImc, @LLogFont);
        ImmReleaseContext(Handle, LImc);
      end;
  end;

  inherited;
end;

procedure TCustomBCEditor.WMMouseHWheel(var AMessage: TWMMouseWheel);
var
  LIsNeg: Boolean;
begin
  Inc(FHWheelAccumulator, AMessage.WheelDelta);
  while Abs(FHWheelAccumulator) >= WHEEL_DELTA do
  begin
    LIsNeg := FHWheelAccumulator < 0;
    FHWheelAccumulator := Abs(FHWheelAccumulator) - WHEEL_DELTA;
    if LIsNeg then
    begin
      if FHWheelAccumulator <> 0 then FHWheelAccumulator := -FHWheelAccumulator;
      HorzTextPos := Max(0, HorzTextPos - FSpaceWidth);
    end
    else
      HorzTextPos := Min(Max(0, FRows.MaxWidth - FTextWidth), HorzTextPos + FSpaceWidth);
  end;
end;

procedure TCustomBCEditor.WMNCPaint(var AMessage: TMessage);
var
  LRect: TRect;
  LRgn: HRGN;
begin
  if (StyleServices.Enabled
    and (GetWindowLong(Handle, GWL_EXSTYLE) and WS_EX_CLIENTEDGE <> 0)) then
  begin
    GetWindowRect(Handle, LRect);
    InflateRect(LRect, -GetSystemMetrics(SM_CXEDGE), -GetSystemMetrics(SM_CYEDGE));
    LRgn := CreateRectRgnIndirect(LRect);
    DefWindowProc(Handle, AMessage.Msg, WPARAM(LRgn), 0);
    DeleteObject(LRgn);
  end
  else
    DefaultHandler(AMessage);

  if (StyleServices.Enabled) then
    StyleServices.PaintBorder(Self, False);
end;

procedure TCustomBCEditor.WMKillFocus(var AMessage: TWMKillFocus);
begin
  inherited;

  if (not Assigned(FCompletionProposalPopupWindow)) then
    UpdateCaret();

  if (HideSelection and SelectionAvailable) then
    Invalidate();
end;

procedure TCustomBCEditor.WMPaint(var AMessage: TWMPaint);
var
  LCompatibleBitmap: HBITMAP;
  LCompatibleDC: HDC;
  LDC: HDC;
  LOldBitmap: HBITMAP;
  LPaintStruct: TPaintStruct;
begin
  if AMessage.DC <> 0 then
  begin
    if not (csCustomPaint in ControlState) and (ControlCount = 0) then
      inherited
    else
      PaintHandler(AMessage);
  end
  else
  begin
    LDC := GetDC(0);
    LCompatibleBitmap := CreateCompatibleBitmap(LDC, ClientWidth, ClientHeight);
    ReleaseDC(0, LDC);
    LCompatibleDC := CreateCompatibleDC(0);
    LOldBitmap := SelectObject(LCompatibleDC, LCompatibleBitmap);
    try
      LDC := BeginPaint(Handle, LPaintStruct);
      AMessage.DC := LCompatibleDC;
      WMPaint(AMessage);
      BitBlt(LDC, 0, 0, ClientRect.Right, ClientRect.Bottom, LCompatibleDC, 0, 0, SRCCOPY);
      EndPaint(Handle, LPaintStruct);
    finally
      SelectObject(LCompatibleDC, LOldBitmap);
      DeleteObject(LCompatibleBitmap);
      DeleteDC(LCompatibleDC);
    end;
  end;
end;

procedure TCustomBCEditor.WMPaste(var AMessage: TMessage);
begin
  if (not ReadOnly) then
    PasteFromClipboard();
  AMessage.Result := LRESULT(TRUE);
end;

procedure TCustomBCEditor.WMSetCursor(var AMessage: TWMSetCursor);
var
  LCursorIndex: Integer;
  LCursorPoint: TPoint;
  LNewCursor: TCursor;
  LSelectionAvailable: Boolean;
  LTextPosition: TBCEditorLinesPosition;
  LWidth: Integer;
begin
  if ((AMessage.HitTest <> HTCLIENT)
    or (AMessage.CursorWnd <> Handle)
    or (csDesigning in ComponentState)) then
    inherited
  else
  begin
    GetCursorPos(LCursorPoint);
    LCursorPoint := ScreenToClient(LCursorPoint);

    Inc(LCursorPoint.X, 4);

    LWidth := 0;

    if FMouseMoveScrolling then
    begin
      LCursorIndex := GetMouseMoveScrollCursorIndex;
      if LCursorIndex <> -1 then
        SetCursor(FMouseMoveScrollCursors[LCursorIndex])
      else
        SetCursor(0)
    end
    else
    if (LWidth < LCursorPoint.X) and (LCursorPoint.X < LWidth + FLeftMarginWidth) then
      SetCursor(Screen.Cursors[crDefault])
    else
    begin
      LSelectionAvailable := SelectionAvailable;
      if LSelectionAvailable then
        LTextPosition := ClientToLines(LCursorPoint.X, LCursorPoint.Y);
      LNewCursor := Cursor;
      FKeyboardHandler.ExecuteMouseCursor(Self, LTextPosition, LNewCursor);
      SetCursor(Screen.Cursors[LNewCursor]);
    end;
  end;
end;

procedure TCustomBCEditor.WMSetFocus(var AMessage: TWMSetFocus);
begin
  inherited;

  if (not Assigned(FCompletionProposalPopupWindow)) then
    UpdateCaret();

  if (HideSelection and SelectionAvailable) then
    Invalidate();

  if (Assigned(FCompletionProposalPopupWindow)) then
    PostMessage(Handle, UM_FREE_COMPLETIONPROPOSAL_POPUPWINDOW, 0, 0);
end;

procedure TCustomBCEditor.WMSetText(var AMessage: TWMSetText);
begin
  AMessage.Result := 1;
  Text := StrPas(AMessage.Text);
end;

procedure TCustomBCEditor.WMSettingChange(var AMessage: TWMSettingChange);
begin
  inherited;

  if (AMessage.Flag = SPI_SETWHEELSCROLLLINES) then
    if (not SystemParametersInfo(SPI_GETWHEELSCROLLLINES, 0, @FWheelScrollLines, 0)) then
      FWheelScrollLines := 3;
end;

procedure TCustomBCEditor.WMTimer(var Msg: TWMTimer);
begin
  case (Msg.TimerID) of
    tiCodeFolding:
      begin
        KillTimer(Handle, Msg.TimerID);
        DoScanCodeFoldingRanges();
      end;
    tiShowHint:
      begin
        KillTimer(Handle, Msg.TimerID);
        Process(paHint, mbLeft, [], FMouseMoveHintPoint.X, FMouseMoveHintPoint.Y);
      end;
  end;
end;

procedure TCustomBCEditor.WMUndo(var AMessage: TMessage);
begin
  Undo();
end;

procedure TCustomBCEditor.WMVScroll(var AMessage: TWMScroll);
var
  LScrollButtonHeight: Integer;
  LScrollHint: string;
  LScrollHintPoint: TPoint;
  LScrollHintRect: TRect;
  LScrollHintWindow: THintWindow;
  LScrollInfo: TScrollInfo;
begin
  Invalidate;
  AMessage.Result := 0;

  case AMessage.ScrollCode of
    SB_TOP:
      TopRow := 1;
    SB_BOTTOM:
      TopRow := FRows.Count;
    SB_LINEDOWN:
      TopRow := TopRow + 1;
    SB_LINEUP:
      TopRow := TopRow - 1;
    SB_PAGEDOWN:
      TopRow := TopRow + VisibleRows;
    SB_PAGEUP:
      TopRow := TopRow - VisibleRows;
    SB_THUMBPOSITION, SB_THUMBTRACK:
      begin
        if FRows.Count > BCEDITOR_MAX_SCROLL_RANGE then
          TopRow := MulDiv(VisibleRows + FRows.Count - 1, AMessage.Pos, BCEDITOR_MAX_SCROLL_RANGE)
        else
          TopRow := AMessage.Pos;

        if soShowVerticalScrollHint in FScroll.Options then
        begin
          LScrollHintWindow := GetScrollHint;
          if FScroll.Hint.Format = shFTopLineOnly then
            LScrollHint := Format(SBCEditorScrollInfoTopLine, [TopRow])
          else
            LScrollHint := Format(SBCEditorScrollInfo,
              [TopRow, TopRow + Min(VisibleRows, FRows.Count - TopRow)]);

          LScrollHintRect := LScrollHintWindow.CalcHintRect(200, LScrollHint, nil);

          if soHintFollows in FScroll.Options then
          begin
            LScrollButtonHeight := GetSystemMetrics(SM_CYVSCROLL);

            FillChar(LScrollInfo, SizeOf(LScrollInfo), 0);
            LScrollInfo.cbSize := SizeOf(LScrollInfo);
            LScrollInfo.fMask := SIF_ALL;
            GetScrollInfo(Handle, SB_VERT, LScrollInfo);

            LScrollHintPoint := ClientToScreen(Point(ClientWidth - LScrollHintRect.Right - 4,
              ((LScrollHintRect.Bottom - LScrollHintRect.Top) shr 1) + Round((LScrollInfo.nTrackPos / LScrollInfo.nMax)
              * (ClientHeight - LScrollButtonHeight * 2)) - 2));
          end
          else
            LScrollHintPoint := ClientToScreen(Point(ClientWidth - LScrollHintRect.Right - 4, 4));

          OffsetRect(LScrollHintRect, LScrollHintPoint.X, LScrollHintPoint.Y);
          LScrollHintWindow.ActivateHint(LScrollHintRect, LScrollHint);
          LScrollHintWindow.Update;
        end;
      end;
    SB_ENDSCROLL:
      begin
        if soShowVerticalScrollHint in FScroll.Options then
          ShowWindow(GetScrollHint.Handle, SW_HIDE);
      end;
  end;
  Update;
end;

procedure TCustomBCEditor.WndProc(var AMessage: TMessage);
begin
  case (AMessage.Msg) of
    WM_LBUTTONDOWN, WM_LBUTTONDBLCLK:
      if (not (csDesigning in ComponentState) and not Focused()) then
      begin
        Windows.SetFocus(Handle);
        if (not Focused) then Exit;
      end;
    WM_SYSCHAR:
      { Prevent Alt+Backspace from beeping }
      if ((AMessage.wParam = VK_BACK) and (AMessage.lParam and (1 shl 29) <> 0)) then
        AMessage.Msg := 0;
    WM_SETTEXT,
    WM_GETTEXT,
    WM_GETTEXTLENGTH:
      { Handle direct WndProc calls that could happen through VCL-methods like Perform }
      if (HandleAllocated) then
        if (FWindowProducedMessage) then
          FWindowProducedMessage := False
        else
        begin
          FWindowProducedMessage := True;
          with AMessage do
            Result := SendMessageA(Handle, Msg, wParam, LParam);
          Exit;
        end;
  end;

  inherited;
end;

function TCustomBCEditor.WordAtCursor(): string;
begin
  Result := GetWordAt(CaretPos);
end;

function TCustomBCEditor.WordBegin(const ALinesPosition: TBCEditorLinesPosition): TBCEditorLinesPosition;
begin
  Result := ALinesPosition;
  while ((Result.Char - 1 >= 0) and not IsWordBreakChar(FLines.Items[Result.Line].Text[1 + Result.Char - 1])) do
    Dec(Result.Char);
end;

function TCustomBCEditor.WordEnd(): TBCEditorLinesPosition;
begin
  Result := WordEnd(FLines.CaretPosition);
end;

function TCustomBCEditor.WordEnd(const ALinesPosition: TBCEditorLinesPosition): TBCEditorLinesPosition;
begin
  Result := ALinesPosition;
  while ((Result.Char + 1 < Length(FLines.Items[Result.Line].Text)) and not IsWordBreakChar(FLines.Items[Result.Line].Text[1 + Result.Char + 1])) do
    Inc(Result.Char);
end;

procedure TCustomBCEditor.WordWrapChanged(ASender: TObject);
begin
  FRows.Clear();
  InvalidateCaret();
  Invalidate();
end;

initialization
  GLineWidth := Round(Screen.PixelsPerInch / USER_DEFAULT_SCREEN_DPI);
  GPadding := Round(Screen.PixelsPerInch / USER_DEFAULT_SCREEN_DPI);

  OleCheck(OleInitialize(nil));
finalization
  OleUninitialize();
end.


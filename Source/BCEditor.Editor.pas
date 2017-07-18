unit BCEditor.Editor;

interface {********************************************************************}

uses
  Windows, Messages, ActiveX, GDIPAPI, GDIPObj,
  Classes, SysUtils, Contnrs, UITypes, StrUtils, Generics.Collections,
  Forms, StdActns, Controls, Graphics, StdCtrls, ExtCtrls, Dialogs, Consts,
  Menus,
  BCEditor.Consts, BCEditor.Editor.ActiveLine,
  BCEditor.Editor.Marks, BCEditor.Editor.CodeFolding,
  BCEditor.Types, BCEditor.Editor.CompletionProposal,
  BCEditor.Editor.CompletionProposal.PopupWindow,
  BCEditor.Editor.KeyCommands, BCEditor.Editor.LeftMargin, BCEditor.Editor.MatchingPair,
  BCEditor.Editor.Replace, BCEditor.Editor.Scroll, BCEditor.Editor.Search,
  BCEditor.Editor.Selection, BCEditor.Editor.SpecialChars,
  BCEditor.Editor.Tabs,
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

    TMouseCapture = (mcNone, mcSyncEditButton, mcMarks, mcLineNumbers,
      mcLineState, mcCodeFolding, mcText, mcScrolling);

    TState = set of (esFontChanged, esSizeChanged, esRowsChanged,
      esCaretInvalid, esCaretChanged, esSelectionChanged,
      esScrolled, esLinesCleared, esLinesDeleted, esLinesInserted,
      esLinesUpdated, esIgnoreNextChar, esSyncEditInvalid,
      esFindind, esReplacing, esScrolling, esUpdated,
      esPainting, esBuildingRows, esHandlingMouse,
      esCodeFoldingInvalid, esSysFontChanged, esHighlighterChanged,
      esWaitForDrag, esDragging, esMouseDouble, esScrollToCaret);

    TPendingJob = (pjBuildRows);
    TPendingJobs = set of TPendingJob;

    TOverlay = record
      Area: TBCEditorLinesArea;
      Style: (osRect, osUnderline, osWaveLine);
    end;

    TOverlays = class(TList<TOverlay>)
    private
      FEditor: TCustomBCEditor;
    public
      function Add(const AValue: TOverlay): Integer;
      constructor Create(const AEditor: TCustomBCEditor);
    end;

    PPaintVar = ^TPaintVar;
    TPaintVar = record
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
      Graphics: TGPGraphics;
      LeftMarginBorderBrush: TGPBrush;
      LineBackgroundColor: TColor;
      LineForegroundColor: TColor;
      OverlayIndex: Integer;
      OverlayRectBrush: TGPBrush;
      OverlayUnderlineBrush: TGPBrush;
      Parts: TList<TPart>;
      PreviousBackgroundColor: TColor;
      PreviousFontStyles: TFontStyles;
      PreviousUCC: Boolean;
      SearchResultIndex: Integer;
      SelArea: TBCEditorLinesArea;
      UCCBrush: TGPBrush;
    end;

    TProcessAction = (paNone, paPaint, paMouseDown, paMouseDouble, paMouseTriple,
      paMouseMove, paMouseUp, paHint, paScrolling);

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
      function GetFmtText(): string;
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
      property FmtText: string read GetFmtText;
      property MaxColumns: Integer read GetMaxColumns;
      property MaxWidth: Integer read GetMaxWidth;
      property RowArea[Row: Integer]: TBCEditorLinesArea read GetRowArea;
      property Text[Row: Integer]: string read GetText; default;
    end;

  private const
    DefaultOptions = [eoAutoIndent, eoDropFiles];
    DefaultUndoOptions = [uoGroupUndo];
    UM_FREE_COMPLETIONPROPOSAL_POPUPWINDOW = WM_USER;
    UM_IDLE = WM_USER + 1;
  private
    FActiveLine: TBCEditorActiveLine;
    FAllCodeFoldingRanges: TBCEditorCodeFolding.TAllRanges;
    FBookmarkBitmaps: array[0 .. BCEDITOR_BOOKMARKS - 1] of TGPCachedBitmap;
    FBookmarkList: TBCEditorMarkList;
    FBorderStyle: TBorderStyle;
    FCaretPos: TPoint; // Caret position in pixel - NOT the related to ClientPos
    FCaretVisible: Boolean;
    FChainedEditor: TCustomBCEditor;
    FCodeFolding: TBCEditorCodeFolding;
    FCodeFoldingCollapsedBitmap: TGPCachedBitmap;
    FCodeFoldingEndLineBitmap: TGPCachedBitmap;
    FCodeFoldingExpandedBitmap: TGPCachedBitmap;
    FCodeFoldingNoneBitmap: TGPCachedBitmap;
    FCodeFoldingLineBitmap: TGPCachedBitmap;
    FCodeFoldingWidth: Integer;
    FCodeFoldingCollapsedMarkWidth: Integer;
    FCompletionProposal: TBCEditorCompletionProposal;
    FCompletionProposalPopupWindow: TBCEditorCompletionProposalPopupWindow;
    FCompletionProposalTimer: TTimer;
    FCurrentMatchingPair: TMatchingPairResult;
    FDoubleClickTime: Cardinal;
    FFontPitchFixed: Boolean;
    FFormWnd: HWND;
    FHideSelectionBeforeSearch: Boolean;
    FHideScrollBars: Boolean;
    FHighlightedFoldRange: TBCEditorCodeFolding.TRanges.TRange;
    FHighlighter: TBCEditorHighlighter;
    FCursorPoint: TPoint;
    FDlgCtrlID: Integer;
    FFmtLines: Boolean;
    FHideSelection: Boolean;
    FHintWindow: THintWindow;
    FHookedCommandHandlers: TObjectList;
    FHorzTextPos: Integer;
    FHWheelAccumulator: Integer;
    FIMEStatus: LPARAM;
    FInsertPos: TPoint;
    FInsertPosBitmap: TGPCachedBitmap;
    FInsertPosCache: TBitmap;
    FItalicOffsetCache: array [AnsiChar] of Byte;
    FKeyboardHandler: TBCEditorKeyboardHandler;
    FKeyCommands: TBCEditorKeyCommands;
    FLastBuiltLine: Integer;
    FLastCursorPoint: TPoint;
    FLastDoubleClickTime: Cardinal;
    FLastKey: Word;
    FLastShiftState: TShiftState;
    FLeftMargin: TBCEditorLeftMargin;
    FLeftMarginBorderWidth: Integer;
    FLeftMarginWidth: Integer;
    FLineBreakSignWidth: Integer;
    FLineHeight: Integer;
    FLineNumbersWidth: Integer;
    FLines: TBCEditorLines;
    FLineStateWidth: Integer;
    FMarkList: TBCEditorMarkList;
    FMarksPanelPopupMenu: TPopupMenu;
    FMarksPanelWidth: Integer;
    FMatchingPair: TBCEditorMatchingPair;
    FMaxDigitWidth: Integer;
    FMouseCapture: TMouseCapture;
    FMouseDownPoint: TPoint;
    FOldSelectionAvailable: Boolean;
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
    FOnMarksPanelClick: TBCEditorMarksPanelClick;
    FOnModified: TNotifyEvent;
    FOnProcessCommand: TBCEditorProcessCommandEvent;
    FOnProcessUserCommand: TBCEditorProcessCommandEvent;
    FOnReplaceText: TBCEditorReplaceEvent;
    FOnRightMarginMouseUp: TNotifyEvent;
    FOnSelectionChanged: TNotifyEvent;
    FOptions: TBCEditorOptions;
    FOriginalLines: TBCEditorLines;
    FOverlays: TOverlays;
    FPaintHelper: TBCEditorPaintHelper;
    FParentWnd: HWND;
    FPendingJobs: TPendingJobs;
    FPopupMenu: HMENU;
    FReadOnly: Boolean;
    FReplace: TBCEditorReplace;
    FRows: TCustomBCEditor.TRows;
    FScroll: TBCEditorScroll;
    FScrollBars: UITypes.TScrollStyle;
    FScrollingBitmap: TGPCachedBitmap;
    FScrollingBitmapHeight: Integer;
    FScrollingBitmapWidth: Integer;
    FScrollingEnabled: Boolean;
    FScrollingPoint: TPoint;
    FScrollingRect: TRect;
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
    FSyncEditAvailable: Boolean;
    FSyncEditButtonHotBitmap: TGPCachedBitmap;
    FSyncEditButtonNormalBitmap: TGPCachedBitmap;
    FSyncEditButtonPressedBitmap: TGPCachedBitmap;
    FTabSignWidth: Integer;
    FTabs: TBCEditorTabs;
    FTextEntryMode: TBCEditorTextEntryMode;
    FTextRect: TRect;
    FTopRow: Integer;
    FUCCVisible: Boolean;
    FUpdateCount: Integer;
    FVisibleRows: Integer;
    FWantReturns: Boolean;
    FWantTabs: Boolean;
    FWheelScrollLines: UINT;
    FWindowProducedMessage: Boolean;
    FWordWrap: Boolean;
    procedure ActiveLineChanged(Sender: TObject);
    procedure AfterLinesUpdate(Sender: TObject);
    procedure BeforeLinesUpdate(Sender: TObject);
    procedure BookmarkListChange(Sender: TObject);
    procedure BuildRows(const ACanvas: TCanvas; const RequiredRow: Integer);
    procedure CheckIfAtMatchingKeywords;
    procedure ClearCodeFolding();
    function ClientToLines(const X, Y: Integer; const AForCaret: Boolean = False): TBCEditorLinesPosition; {$IFNDEF Debug} inline; {$ENDIF}
    function ClientToRows(const X, Y: Integer; const AForCaret: Boolean = False): TBCEditorRowsPosition;
    procedure CMCursorChanged(var AMessage: TMessage); message CM_CURSORCHANGED;
    procedure CMSysFontChanged(var AMessage: TMessage); message CM_SYSFONTCHANGED;
    function CodeFoldingCollapsableFoldRangeForLine(const ALine: Integer): TBCEditorCodeFolding.TRanges.TRange;
    function CodeFoldingFoldRangeForLineTo(const ALine: Integer): TBCEditorCodeFolding.TRanges.TRange;
    procedure CodeFoldingOnChange(AEvent: TBCEditorCodeFoldingChanges);
    procedure CollapseCodeFoldingRange(const ARange: TBCEditorCodeFolding.TRanges.TRange);
    procedure CompletionProposalTimerHandler(EndLine: TObject);
    function ComputeIndentText(const IndentCount: Integer): string;
    procedure ComputeMetrics();
    function ComputeTextColumns(const AText: PChar; const ALength, AColumn: Integer): Integer; {$IFNDEF Debug} inline; {$ENDIF}
    procedure Delay(const AAction: TPendingJob);
    procedure DeleteChar;
    procedure DeleteLastWordOrBeginningOfLine(const ACommand: TBCEditorCommand);
    procedure DeleteLine;
    procedure DeleteLineFromRows(const ALine: Integer);
    procedure DeleteWordOrEndOfLine(const ACommand: TBCEditorCommand);
    procedure DoBackspace();
    procedure DoBlockComment;
    procedure DoChar(const AChar: Char);
    procedure DoEditorBottom(const ACommand: TBCEditorCommand); {$IFNDEF Debug} inline; {$ENDIF}
    procedure DoEditorTop(const ACommand: TBCEditorCommand); {$IFNDEF Debug} inline; {$ENDIF}
    procedure DoEndKey(const ASelectionCommand: Boolean);
    procedure DoHomeKey(const ASelectionCommand: Boolean);
    procedure DoImeStr(AData: Pointer);
    procedure DoInsertText(const AText: string);
    procedure DoLineComment;
    procedure DoPageTopOrBottom(const ACommand: TBCEditorCommand);
    procedure DoPageUpOrDown(const ACommand: TBCEditorCommand);
    function DoReplaceText(): Integer;
    procedure DoReturnKey();
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
    procedure DoTabKey;
    procedure DoToggleSelectedCase(const ACommand: TBCEditorCommand);
    procedure DoWordLeft(const ACommand: TBCEditorCommand);
    procedure DoWordRight(const ACommand: TBCEditorCommand);
    procedure EMCanUndo(var AMessage: TMessage); message EM_CANUNDO;
    procedure EMCharFromPos(var AMessage: TMessage); message EM_CHARFROMPOS;
    procedure EMEmptyUndoBuffer(var AMessage: TMessage); message EM_EMPTYUNDOBUFFER;
    procedure EMFmtLines(var AMessage: TMessage); message EM_FMTLINES;
    procedure EMGetFirstVisible(var AMessage: TMessage); message EM_GETFIRSTVISIBLELINE;
    procedure EMGetHandle(var AMessage: TMessage); message EM_GETHANDLE;
    procedure EMGetIMEStatus(var AMessage: TMessage); message EM_GETIMESTATUS;
    procedure EMGetLine(var AMessage: TMessage); message EM_GETLINE;
    procedure EMGetLineCount(var AMessage: TMessage); message EM_GETLINECOUNT;
    procedure EMGetModify(var AMessage: TMessage); message EM_GETMODIFY;
    procedure EMGetRect(var AMessage: TMessage); message EM_GETRECT;
    procedure EMGetSel(var AMessage: TMessage); message EM_GETSEL;
    procedure EMGetThumb(var AMessage: TMessage); message EM_GETTHUMB;
    procedure EMLineFromChar(var AMessage: TMessage); message EM_LINEFROMCHAR;
    procedure EMLineIndex(var AMessage: TMessage); message EM_LINEINDEX;
    procedure EMLineLength(var AMessage: TMessage); message EM_LINELENGTH;
    procedure EMLineScroll(var AMessage: TMessage); message EM_LINESCROLL;
    procedure EMPosFromChar(var AMessage: TMessage); message EM_POSFROMCHAR;
    procedure EMReplaceSel(var AMessage: TMessage); message EM_REPLACESEL;
    procedure EMScroll(var AMessage: TMessage); message EM_SCROLL;
    procedure EMScrollCaret(var AMessage: TMessage); message EM_SCROLLCARET;
    procedure EMSetIMEStatus(var AMessage: TMessage); message EM_SETIMESTATUS;
    procedure EMSetModify(var AMessage: TMessage); message EM_SETMODIFY;
    procedure EMSetReadOnly(var AMessage: TMessage); message EM_SETREADONLY;
    procedure EMSetSel(var AMessage: TMessage); message EM_SETSEL;
    procedure EMSetTabStop(var AMessage: TMessage); message EM_SETTABSTOPS;
    procedure EMUndo(var AMessage: TMessage); message EM_UNDO;
    procedure ExpandCodeFoldingRange(const ARange: TBCEditorCodeFolding.TRanges.TRange);
    function FindHookedCommandEvent(const AHookedCommandEvent: TBCEditorHookedCommandEvent): Integer;
    procedure FontChanged(ASender: TObject);
    function GetCanPaste(): Boolean; {$IFNDEF Debug} inline; {$ENDIF}
    function GetCanRedo(): Boolean; {$IFNDEF Debug} inline; {$ENDIF}
    function GetCanUndo(): Boolean; {$IFNDEF Debug} inline; {$ENDIF}
    function GetCaretPos(): TPoint;
    function GetCharAt(APos: TPoint): Char; {$IFNDEF Debug} inline; {$ENDIF}
    function GetHookedCommandHandlersCount: Integer;
    function GetLeadingExpandedLength(const AStr: string; const ABorder: Integer = 0): Integer;
    function GetLineIndentLevel(const ALine: Integer): Integer;
    function GetModified(): Boolean; {$IFNDEF Debug} inline; {$ENDIF}
    function GetSearchResultCount(): Integer; {$IFNDEF Debug} inline; {$ENDIF}
    function GetSelectionBeginPosition(): TBCEditorLinesPosition;
    function GetSelectionEndPosition(): TBCEditorLinesPosition;
    function GetSelLength(): Integer; {$IFNDEF Debug} inline; {$ENDIF}
    function GetSelStart(): Integer; {$IFNDEF Debug} inline; {$ENDIF}
    function GetSelText(): string; {$IFNDEF Debug} inline; {$ENDIF}
    function GetText(): string; {$IFNDEF Debug} inline; {$ENDIF}
    function GetTextBetween(ABeginPosition, AEndPosition: TBCEditorLinesPosition): string;
    function GetUndoOptions(): TBCEditorUndoOptions;
    function GetVisibleArea(): TBCEditorLinesArea;
    function GetWordAt(ALinesPos: TPoint): string; {$IFNDEF Debug} inline; {$ENDIF}
    function GetWordAtLinesPosition(const ALinesPosition: TBCEditorLinesPosition): string;
    procedure HighlighterChanged(ASender: TObject);
    procedure Idle(); {$IFNDEF Debug} inline; {$ENDIF}
    procedure InitCodeFolding();
    procedure InsertLine();
    procedure InsertLineIntoRows(const ALine: Integer; const ANewLine: Boolean); overload;
    function InsertLineIntoRows(const ALine: Integer; const ARow: Integer): Integer; overload;
    procedure InvalidateCaret();
    procedure InvalidateRows();
    procedure InvalidateScrollBars();
    procedure InvalidateSyncEdit();
    function IsKeywordAtPosition(const APosition: TBCEditorLinesPosition;
      const APOpenKeyWord: PBoolean = nil): Boolean;
    function LeftSpaceCount(const AText: string; AWantTabs: Boolean = False): Integer;
    function LeftTrimLength(const AText: string): Integer;
    procedure LinesChanged();
    procedure MoveCaretAndSelection(ABeforeLinesPosition, AAfterLinesPosition: TBCEditorLinesPosition;
      const ASelectionCommand: Boolean);
    procedure MoveCaretHorizontally(const AColumns: Integer; const SelectionCommand: Boolean);
    procedure MoveCaretVertically(const ARows: Integer; const SelectionCommand: Boolean);
    function NextWordPosition(const ALinesPosition: TBCEditorLinesPosition): TBCEditorLinesPosition; overload;
    function PreviousWordPosition(const ALinesPosition: TBCEditorLinesPosition): TBCEditorLinesPosition; overload;
    function Process(const AAction: TProcessAction; const APaintVar: PPaintVar;
      const AButton: TMouseButton; const AShift: TShiftState; X, Y: Integer): Boolean;
    function ProcessToken(const AAction: TProcessAction;
      const APaintVar: PPaintVar;
      const AButton: TMouseButton; const AShift: TShiftState; const X, Y: Integer;
      var ARect: TRect;
      const ALinesPosition: TBCEditorLinesPosition;
      const ARowsPosition: TBCEditorRowsPosition;
      const AText: PChar; const ALength: Integer;
      const AToken: TBCEditorHighlighter.PFind = nil;
      const ARange: TBCEditorCodeFolding.TRanges.TRange = nil): Boolean;
    procedure ReplaceChanged(AEvent: TBCEditorReplaceChanges);
    function RowsToClient(ARowsPosition: TBCEditorRowsPosition): TPoint;
    function RowsToLines(const ARowsPosition: TBCEditorRowsPosition): TBCEditorLinesPosition;
    procedure ScrollChanged(ASender: TObject);
    procedure SearchChanged(AEvent: TBCEditorSearchEvent);
    procedure SelectionChanged(ASender: TObject);
    procedure SetActiveLine(const AValue: TBCEditorActiveLine);
    procedure SetBorderStyle(const AValue: TBorderStyle);
    procedure SetCaretPos(const AValue: TPoint);
    procedure SetCodeFolding(const AValue: TBCEditorCodeFolding);
    procedure SetDefaultKeyCommands;
    procedure SetHideScrollBars(AValue: Boolean);
    procedure SetHideSelection(AValue: Boolean); {$IFNDEF Debug} inline; {$ENDIF}
    procedure SetHorzTextPos(AValue: Integer);
    procedure SetInsertPos(AValue: TPoint);
    procedure SetKeyCommands(const AValue: TBCEditorKeyCommands);
    procedure SetLeftMargin(const AValue: TBCEditorLeftMargin);
    procedure SetModified(const AValue: Boolean);
    procedure SetMouseCapture(const AValue: TMouseCapture);
    procedure SetOptions(const AValue: TBCEditorOptions);
    procedure SetReadOnly(const AValue: Boolean); {$IFNDEF Debug} inline; {$ENDIF}
    procedure SetScroll(const AValue: TBCEditorScroll);
    procedure SetScrollBars(const AValue: UITypes.TScrollStyle);
    procedure SetSearch(const AValue: TBCEditorSearch);
    procedure SetSelectedWord;
    procedure SetSelection(const AValue: TBCEditorSelection);
    procedure SetSelectionBeginPosition(const AValue: TBCEditorLinesPosition);
    procedure SetSelectionEndPosition(const AValue: TBCEditorLinesPosition);
    procedure SetSelLength(AValue: Integer);
    procedure SetSelStart(AValue: Integer);
    procedure SetSelText(const AValue: string);
    procedure SetSpecialChars(const AValue: TBCEditorSpecialChars);
    procedure SetSyncEdit(const AValue: TBCEditorSyncEdit);
    procedure SetTabs(const AValue: TBCEditorTabs);
    procedure SetText(const AValue: string); {$IFNDEF Debug} inline; {$ENDIF}
    procedure SetTopRow(const AValue: Integer);
    procedure SetUndoOptions(AOptions: TBCEditorUndoOptions);
    procedure SetWantReturns(const AValue: Boolean); {$IFNDEF Debug} inline; {$ENDIF}
    procedure SetWordBlock(const ALinesPosition: TBCEditorLinesPosition);
    procedure SetWordWrap(const AValue: Boolean);
    procedure SpecialCharsChanged(ASender: TObject);
    procedure SyncEditChanged(ASender: TObject);
    procedure TabsChanged(ASender: TObject);
    function TokenWidth(const AText: PChar; const ALength: Integer;
      const AColumn: Integer; const AToken: TBCEditorHighlighter.TFind): Integer; // inline takes the double time. Why???
    procedure UMFreeCompletionProposalPopupWindow(var AMessage: TMessage); message UM_FREE_COMPLETIONPROPOSAL_POPUPWINDOW;
    procedure UMIdle(var AMessage: TMessage); message UM_IDLE;
    procedure UpdateLinesBeginRanges(const ALine: Integer);
    procedure UpdateRows(const ALine: Integer);
    procedure WMChar(var AMessage: TWMChar); message WM_CHAR;
    procedure WMClear(var AMessage: TMessage); message WM_CLEAR;
    procedure WMCommand(var AMessage: TWMCommand); message WM_COMMAND;
    procedure WMContextMenu(var AMessage: TWMContextMenu); message WM_CONTEXTMENU;
    procedure WMCopy(var AMessage: TWMCopy); message WM_COPY;
    procedure WMCut(var AMessage: TWMCut); message WM_CUT;
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
    procedure WMPaste(var AMessage: TWMPaste); message WM_PASTE;
    procedure WMSetCursor(var AMessage: TWMSetCursor); message WM_SETCURSOR;
    procedure WMSetFocus(var AMessage: TWMSetFocus); message WM_SETFOCUS;
    procedure WMSetText(var AMessage: TWMSetText); message WM_SETTEXT;
    procedure WMSettingChange(var AMessage: TWMSettingChange); message WM_SETTINGCHANGE;
    procedure WMStyleChanged(var AMessage: TWMStyleChanged); message WM_STYLECHANGED;
    procedure WMTimer(var Msg: TWMTimer); message WM_TIMER;
    procedure WMUndo(var AMessage: TMessage); message WM_UNDO;
    procedure WMVScroll(var AMessage: TWMScroll); message WM_VSCROLL;
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
    procedure Change(); virtual;
    procedure ChangeScale(M, D: Integer); override;
    procedure ClearBookmarks;
    procedure ClearMarks;
    procedure ClearUndo();
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
    procedure GotoBookmark(const AIndex: Integer);
    procedure GotoLineAndCenter(const ALine: Integer; const AChar: Integer = 1);
    procedure GotoNextBookmark;
    procedure GotoPreviousBookmark;
    procedure InvalidateMatchingPair();
    function IsCommentChar(const AChar: Char): Boolean;
    function IsEmptyChar(const AChar: Char): Boolean; {$IFNDEF Debug} inline; {$ENDIF}
    function IsWordBreakChar(const AChar: Char): Boolean; {$IFNDEF Debug} inline; {$ENDIF}
    procedure KeyDown(var AKey: Word; AShift: TShiftState); override;
    procedure KeyUp(var AKey: Word; AShift: TShiftState); override;
    procedure LeftMarginChanged(ASender: TObject);
    procedure LineDeleted(ASender: TObject; const ALine: Integer);
    procedure LineDeleting(ASender: TObject; const ALine: Integer);
    procedure LineInserted(ASender: TObject; const ALine: Integer);
    procedure LinesCaretChanged(ASender: TObject);
    procedure LinesCleared(ASender: TObject);
    procedure LinesHookChanged;
    procedure LinesLoaded(ASender: TObject);
    procedure LinesSyncEditChanged(ASender: TObject);
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
    procedure SetMark(const AIndex: Integer; const ALinesPosition: TBCEditorLinesPosition;
      const AImageIndex: Integer);
    procedure SetOption(const AOption: TBCEditorOption; const AEnabled: Boolean);
    procedure SetUndoOption(const AOption: TBCEditorUndoOption; const AEnabled: Boolean);
    procedure SetUpdateState(AUpdating: Boolean); virtual;
    procedure ScrollToCaret();
    procedure UpdateCaret();
    function WordBegin(const ALinesPosition: TBCEditorLinesPosition): TBCEditorLinesPosition; overload;
    function WordEnd(): TBCEditorLinesPosition; overload; {$IFNDEF Debug} inline; {$ENDIF}
    function WordEnd(const ALinesPosition: TBCEditorLinesPosition): TBCEditorLinesPosition; overload;
    property AllCodeFoldingRanges: TBCEditorCodeFolding.TAllRanges read FAllCodeFoldingRanges;
    property BorderStyle: TBorderStyle read FBorderStyle write SetBorderStyle default bsSingle;
    property HideScrollBars: Boolean read FHideScrollBars write SetHideScrollBars default True;
    property HideSelection: Boolean read FHideSelection write SetHideSelection default True;
    property HorzTextPos: Integer read FHorzTextPos write SetHorzTextPos;
    property LineHeight: Integer read FLineHeight;
    property MarksPanelPopupMenu: TPopupMenu read FMarksPanelPopupMenu write FMarksPanelPopupMenu;
    property MouseCapture: TMouseCapture read FMouseCapture write SetMouseCapture;
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
    property OnMarksPanelClick: TBCEditorMarksPanelClick read FOnMarksPanelClick;
    property OnModified: TNotifyEvent read FOnModified write FOnModified;
    property OnProcessCommand: TBCEditorProcessCommandEvent read FOnProcessCommand write FOnProcessCommand;
    property OnProcessUserCommand: TBCEditorProcessCommandEvent read FOnProcessUserCommand write FOnProcessUserCommand;
    property OnReplaceText: TBCEditorReplaceEvent read FOnReplaceText write FOnReplaceText;
    property OnRightMarginMouseUp: TNotifyEvent read FOnRightMarginMouseUp write FOnRightMarginMouseUp;
    property OnSelectionChanged: TNotifyEvent read FOnSelectionChanged write FOnSelectionChanged;
    property Options: TBCEditorOptions read FOptions write SetOptions default DefaultOptions;
    property PaintHelper: TBCEditorPaintHelper read FPaintHelper;
    property ParentColor default False;
    property ParentFont default False;
    property ScrollBars: UITypes.TScrollStyle read FScrollBars write SetScrollBars default ssBoth;
    property TabStop default True;
    property TopRow: Integer read FTopRow write SetTopRow;
    property UndoOptions: TBCEditorUndoOptions read GetUndoOptions write SetUndoOptions default DefaultUndoOptions;
    property VisibleArea: TBCEditorLinesArea read GetVisibleArea;
    property VisibleRows: Integer read FVisibleRows;
    property WantReturns: Boolean read FWantReturns write SetWantReturns default True;
    property WantTabs: Boolean read FWantTabs write FWantTabs default True;
    property WordWrap: Boolean read FWordWrap write SetWordWrap default False;
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
    procedure BeginUndoBlock(); deprecated 'Use Lines.EndUpdate()'; // 2017-07-12
    procedure BeginUpdate();
    procedure ChainEditor(AEditor: TCustomBCEditor);
    function CharIndexToPos(const ACharIndex: Integer): TPoint; {$IFNDEF Debug} inline; {$ENDIF}
    procedure Clear(); virtual;
    function ClientToPos(const X, Y: Integer): TPoint; {$IFNDEF Debug} inline; {$ENDIF}
    function ClientToText(const X, Y: Integer): TPoint; deprecated 'Use ClientToPos'; // 2017-05-13
    function CharAtCursor(): Char; deprecated 'Use CharAt[CaretPos]'; // 2017-04-05
    procedure CommandProcessor(ACommand: TBCEditorCommand; AChar: Char; AData: Pointer);
    procedure CopyToClipboard();
    constructor Create(AOwner: TComponent); override;
    procedure CutToClipboard();
    destructor Destroy(); override;
    procedure DoRedo(); {$IFNDEF Debug} inline; {$ENDIF} deprecated 'Use Redo()'; // 2017-02-12
    procedure DoUndo(); {$IFNDEF Debug} inline; {$ENDIF} deprecated 'Use Undo()'; // 2017-02-12
    procedure DragDrop(ASource: TObject; X, Y: Integer); override;
    procedure EndUndoBlock(); deprecated 'Use Lines.EndUpdate()'; // 2017-07-12
    procedure EndUpdate();
    function ExecuteAction(Action: TBasicAction): Boolean; override;
    procedure ExecuteCommand(ACommand: TBCEditorCommand; AChar: Char; AData: Pointer); virtual;
    procedure ExportToHTML(const AFileName: string; const ACharSet: string = ''; AEncoding: TEncoding = nil); overload;
    procedure ExportToHTML(AStream: TStream; const ACharSet: string = ''; AEncoding: TEncoding = nil); overload;
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
    procedure SelectAll();
    function SelectedText(): string; deprecated 'Use SelText'; // 2017-03-16
    function SelectionAvailable: Boolean; deprecated 'Use SelLength <> 0'; // 2017-07-16
    procedure SetFocus(); override;
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
    property CanPaste: Boolean read GetCanPaste;
    property CanRedo: Boolean read GetCanRedo;
    property CanUndo: Boolean read GetCanUndo;
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
    property ReadOnly: Boolean read FReadOnly write SetReadOnly default False;
    property Replace: TBCEditorReplace read FReplace write FReplace;
    property Scroll: TBCEditorScroll read FScroll write SetScroll;
    property Search: TBCEditorSearch read FSearch write SetSearch;
    property SearchResultCount: Integer read GetSearchResultCount;
    property Selection: TBCEditorSelection read FSelection write SetSelection;
    property SelectionBeginPosition: TBCEditorLinesPosition read GetSelectionBeginPosition write SetSelectionBeginPosition;
    property SelectionEndPosition: TBCEditorLinesPosition read GetSelectionEndPosition write SetSelectionEndPosition;
    property SelLength: Integer read GetSelLength write SetSelLength;
    property SelStart: Integer read GetSelStart write SetSelStart;
    property SelText: string read GetSelText write SetSelText;
    property SpecialChars: TBCEditorSpecialChars read FSpecialChars write SetSpecialChars;
    property SyncEdit: TBCEditorSyncEdit read FSyncEdit write SetSyncEdit;
    property Tabs: TBCEditorTabs read FTabs write SetTabs;
    property Text: string read GetText write SetText;
    property TextBetween[ALinesBeginPosition, ALinesEndPosition: TBCEditorLinesPosition]: string read GetTextBetween;
    property TextEntryMode: TBCEditorTextEntryMode read FTextEntryMode write FTextEntryMode default temInsert;
    property TextRect: TRect read FTextRect;
    property WordAt[ATextPos: TPoint]: string read GetWordAt;
    property UpdateCount: Integer read FUpdateCount;
  end;

  TBCEditor = class(TCustomBCEditor)
  public
    property Canvas;
  published
    property ActiveLine;
    property Align;
    property Anchors;
    property BorderStyle;
    property CodeFolding;
    property Color;
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
    property OnMarksPanelClick;
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
    property WantTabs;
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
  Clipbrd, Themes, ImgList,
  BCEditor.Language, BCEditor.Export.HTML;

resourcestring
  SBCEditorLineIsNotVisible = 'Line %d is not visible';
  SBCEditorOverlayInvalidArea = 'Overlay area invalid';
  SBCEditorOverlayOverlap = 'Overlay overlap';

type
  TUnprotectedWinControl = class(TWinControl);

const
  InvalidRect: TRect = ( Left: -1; Top: -1; Right: -1; Bottom: -1 );
  InvalidPos: TPoint = ( X: -1; Y: -1 );

  tiCodeFolding = 0;
  tiShowHint = 1;
  tiScrolling = 2;
  tiScroll = 3;
  tiIdle = 4;

  RowToInsert = -2;

var
  GLineWidth: Integer;
  GImmEnabled: Boolean;
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

{ TCustomBCEditor.TOverlays ***************************************************}

function TCustomBCEditor.TOverlays.Add(const AValue: TOverlay): Integer;
var
  LIndex: Integer;
begin
  LIndex := 0;
  while ((LIndex < Count) and (Items[LIndex].Area.BeginPosition < AValue.Area.BeginPosition)) do
    Inc(LIndex);

  if ((AValue.Area.BeginPosition.Line <> AValue.Area.EndPosition.Line)
    or (AValue.Area.BeginPosition.Char < 0)
    or (AValue.Area.EndPosition.Char < AValue.Area.BeginPosition.Char)
    or (AValue.Area.EndPosition.Char > Length(FEditor.FLines.Items[AValue.Area.EndPosition.Line].Text))) then
    raise ERangeError.Create(SBCEditorOverlayInvalidArea);
  if ((LIndex > 0) and (Items[LIndex - 1].Area.EndPosition > AValue.Area.BeginPosition)) then
    raise ERangeError.Create(SBCEditorOverlayOverlap);

  Insert(LIndex, AValue);
  Result := LIndex;
end;

constructor TCustomBCEditor.TOverlays.Create(const AEditor: TCustomBCEditor);
begin
  inherited Create();

  FEditor := AEditor;
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

function TCustomBCEditor.TRows.GetFmtText(): string;
var
  LRow: Integer;
  LStringBuilder: TStringBuilder;
begin
  LStringBuilder := TStringBuilder.Create();

  for LRow := 0 to Count - 1 do
  begin
    LStringBuilder.Append(FEditor.FLines.Items[Items[LRow].Line].Text, Items[LRow].Char, Items[LRow].Length);
    if (not (rfLastRowOfLine in Items[LRow].Flags)) then
      LStringBuilder.Append(#13#13#10)
    else if (LRow < Count - 1) then
      LStringBuilder.Append(#13#10);
  end;

  Result := LStringBuilder.ToString();

  LStringBuilder.Free();
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
  Assert((0 <= ARow) and (ARow < Count));

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

{ TCustomBCEditor *************************************************************}

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
  if (not (csReading in ComponentState)) then
    EndUpdate();
end;

procedure TCustomBCEditor.Assign(ASource: TPersistent);
begin
  if Assigned(ASource) and (ASource is TCustomBCEditor) then
    with ASource as TCustomBCEditor do
    begin
      Self.FActiveLine.Assign(FActiveLine);
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
    end
  else
    inherited Assign(ASource);
end;

procedure TCustomBCEditor.BeforeLinesUpdate(Sender: TObject);
begin
  if (not (csReading in ComponentState)) then
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

procedure TCustomBCEditor.BuildRows(const ACanvas: TCanvas;
   const RequiredRow: Integer);
var
  LCodeFolding: Integer;
  LLine: Integer;
  LRange: TBCEditorCodeFolding.TRanges.TRange;
  LRow: Integer;
begin
  Include(FState, esBuildingRows);
  FPaintHelper.BeginDrawing(ACanvas);
  try
    for LCodeFolding := 0 to FAllCodeFoldingRanges.AllCount - 1 do
    begin
      LRange := FAllCodeFoldingRanges[LCodeFolding];
      if (Assigned(LRange) and LRange.Collapsed) then
        for LLine := LRange.BeginLine + 1 to LRange.EndLine do
          FLines.SetFirstRow(LLine, -1);
    end;

    LRow := FRows.Count;
    LLine := FLastBuiltLine + 1;
    while ((LLine < FLines.Count)
      and ((LRow < RequiredRow) or (RequiredRow < 0))) do
    begin
      if (FLines.Items[LLine].FirstRow = RowToInsert) then
      begin
        Inc(LRow, InsertLineIntoRows(LLine, LRow));
        FLastBuiltLine := LLine;
      end;
      FLastBuiltLine := LLine;
      Inc(LLine);
    end;
  finally
    FPaintHelper.EndDrawing();
    Exclude(FState, esBuildingRows);
  end;

  if (LLine < FLines.Count) then
    Delay(pjBuildRows)
  else
  begin
    InitCodeFolding();

    if (UpdateCount > 0) then
      Include(FState, esRowsChanged)
    else
    begin
      InvalidateScrollBars();
      Invalidate();
    end;
  end;
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

procedure TCustomBCEditor.Change();
begin
  SendMessage(FParentWnd, WM_COMMAND, EN_CHANGE shl 16 + FDlgCtrlID and $FFFF, LPARAM(Handle));

  if (Assigned(FOnChange)) then
    FOnChange(Self);

  Include(FState, esUpdated);

  LinesChanged();
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
  Result := FLines.PositionOf(ACharIndex);
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
  InvalidateRows();
  ClearCodeFolding();
  InvalidateMatchingPair();
  ClearBookmarks;
  FMarkList.Clear;
  HorzTextPos := 0;
  TopRow := 0;
  InvalidateCaret();
  InvalidateScrollBars();
  Invalidate();

  inherited;
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
  Result := ClientToLines(X, Y);
end;

function TCustomBCEditor.ClientToText(const X, Y: Integer): TPoint;
begin
  Result := ClientToPos(X, Y);
end;

procedure TCustomBCEditor.CMCursorChanged(var AMessage: TMessage);
begin
  inherited;

  Perform(WM_SETCURSOR, Handle, HTCLIENT);
end;

procedure TCustomBCEditor.CMSysFontChanged(var AMessage: TMessage);
begin
  if (Assigned(FHintWindow)) then
    FreeAndNil(FHintWindow);

  FMarksPanelWidth := GetSystemMetrics(SM_CXSMICON) + GetSystemMetrics(SM_CXSMICON) div 4;
  FLineStateWidth := GetSystemMetrics(SM_CXSMICON) div 4;
  FLeftMarginBorderWidth := 2 * GLineWidth;
  ComputeMetrics();

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
  if (not FLines.SelArea.IsEmpty()) then
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
    if ((ARange.BeginLine + 1 <= FLines.CaretPosition.Line) and (FLines.CaretPosition.Line <= ARange.EndLine)) then
      MoveCaretAndSelection(FLines.BOLPosition[ARange.BeginLine], FLines.BOLPosition[ARange.BeginLine], False);

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
        ecClear, ecReturn, ecChar, ecString, ecImeStr, ecCut, ecPaste,
        ecBlockIndent, ecBlockUnindent, ecTab:
          if (not FLines.SelArea.IsEmpty()) then
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

procedure TCustomBCEditor.ComputeMetrics();
begin
  FLeftMarginWidth := 0;
  if (FLeftMargin.MarksPanel.Visible) then
    Inc(FLeftMarginWidth, FMarksPanelWidth);
  if (FLeftMargin.LineNumbers.Visible) then
    Inc(FLeftMarginWidth, FLineNumbersWidth);
  if (FLeftMargin.LineState.Visible) then
    Inc(FLeftMarginWidth, FLineStateWidth);
  if (FCodeFolding.Visible) then
    Inc(FLeftMarginWidth, FCodeFoldingWidth);
  if (FLeftMarginWidth > 0) then
    Inc(FLeftMarginWidth, FLeftMarginBorderWidth);
  FTextRect := ClientRect;
  FTextRect.Left := FLeftMarginWidth;
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
var
  LClipboardData: Pointer;
  LGlobal: HGLOBAL;
  LOpened: Boolean;
  LRetry: Integer;
  LText: string;
begin
  LRetry := 0;
  repeat
    LOpened := OpenClipboard(Handle);
    if (not LOpened) then
    begin
      Sleep(50);
      Inc(LRetry);
    end;
  until (LOpened or (LRetry = 10));

  if (not LOpened) then
    raise EClipboardException.CreateFmt(SCannotOpenClipboard, [SysErrorMessage(GetLastError)])
  else
  begin
    try
      EmptyClipboard();
      LText := SelText;
      LGlobal := GlobalAlloc(GMEM_MOVEABLE or GMEM_DDESHARE, (Length(LText) + 1) * SizeOf(Char));
      if (LGlobal <> 0) then
      try
        LClipboardData := GlobalLock(LGlobal);
        if (Assigned(LClipboardData)) then
        begin
          StrPCopy(LClipboardData, LText);
          SetClipboardData(CF_UNICODETEXT, LGlobal);
        end;
      finally
        GlobalUnlock(LGlobal);
      end;
    finally
      CloseClipboard();
    end;
  end;
end;

constructor TCustomBCEditor.Create(AOwner: TComponent);
var
  LIndex: Integer;
  LLogFont: TLogFont;
  LNonClientMetrics: TNonClientMetrics;
begin
  inherited;

  Color := clWindow;
  ControlStyle := ControlStyle + [csOpaque, csNeedsBorderPaint];
  TabStop := True;

  for LIndex := 0 to Length(FBookmarkBitmaps) - 1 do
    FBookmarkBitmaps[LIndex] := nil;
  FBorderStyle := bsSingle;
  FCaretVisible := False;
  FCursorPoint := Point(-1, -1);
  FCodeFoldingCollapsedBitmap := nil;
  FCodeFoldingExpandedBitmap := nil;
  FCodeFoldingLineBitmap := nil;
  FCodeFoldingEndLineBitmap := nil;
  FDoubleClickTime := GetDoubleClickTime();
  FFmtLines := False;
  FHideScrollBars := True;
  FHideSelection := True;
  FHintWindow := nil;
  FHWheelAccumulator := 0;
  FIMEStatus := 0;
  FInsertPos := InvalidPos;
  FInsertPosBitmap := nil;
  FInsertPosCache := nil;
  FMouseCapture := mcNone;
  FLastCursorPoint := Point(-1, -1);
  FLastBuiltLine := -1;
  FLineHeight := 0;
  FOldSelectionAvailable := False;
  FOnChange := nil;
  FOptions := DefaultOptions;
  FParentWnd := 0;
  FPendingJobs := [];
  FPopupMenu := 0;
  FReadOnly := False;
  FScrollBars := ssBoth;
  FScrollingBitmap := nil;
  FSearchFindDialog := nil;
  FSearchReplaceDialog := nil;
  FSelectedCaseText := '';
  FSyncEditButtonHotBitmap := nil;
  FSyncEditButtonNormalBitmap := nil;
  FSyncEditButtonPressedBitmap := nil;
  FTextEntryMode := temInsert;
  FTopRow := 0;
  FUCCVisible := False;
  FUpdateCount := 0;
  FVisibleRows := -1;
  FWantTabs := True;
  FWantReturns := True;
  if (not SystemParametersInfo(SPI_GETWHEELSCROLLLINES, 0, @FWheelScrollLines, 0)) then
    FWheelScrollLines := 3;
  FWordWrap := False;

  { Code folding }
  FAllCodeFoldingRanges := TBCEditorCodeFolding.TAllRanges.Create;
  FCodeFolding := TBCEditorCodeFolding.Create;
  FCodeFolding.OnChange := CodeFoldingOnChange;
  { Matching pair }
  FMatchingPair := TBCEditorMatchingPair.Create;
  { Special chars }
  FSpecialChars := TBCEditorSpecialChars.Create;
  FSpecialChars.OnChange := SpecialCharsChanged;
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
  FLines.OnLoaded := LinesLoaded;
  FLines.OnUpdated := LineUpdated;
  FLines.OnSelChange := SelectionChanged;
  FLines.OnSyncEditChange := LinesSyncEditChanged;
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
  FPaintHelper := TBCEditorPaintHelper.Create(Font);
  FOverlays := TOverlays.Create(Self);
  { Active line, selection }
  FSelection := TBCEditorSelection.Create();
  { Bookmarks }
  FBookmarkList := TBCEditorMarkList.Create(Self);
  FBookmarkList.OnChange := BookmarkListChange;
  { Marks }
  FMarkList := TBCEditorMarkList.Create(Self);
  FMarkList.OnChange := MarkListChange;
  { Tabs }
  FTabs := TBCEditorTabs.Create;
  FTabs.OnChange := TabsChanged;
  { Text }
  FKeyboardHandler := TBCEditorKeyboardHandler.Create;
  FKeyCommands := TBCEditorKeyCommands.Create(Self);
  SetDefaultKeyCommands;
  { Completion proposal }
  FCompletionProposal := TBCEditorCompletionProposal.Create(Self);
  FCompletionProposalTimer := TTimer.Create(Self);
  FCompletionProposalTimer.Enabled := False;
  FCompletionProposalTimer.OnTimer := CompletionProposalTimerHandler;
  { Search }
  FSearch := TBCEditorSearch.Create;
  FSearch.OnChange := SearchChanged;
  FSearchResults := TList<TBCEditorLinesArea>.Create();
  FReplace := TBCEditorReplace.Create;
  FReplace.OnChange := ReplaceChanged;
  { Scroll }
  FScroll := TBCEditorScroll.Create;
  FScroll.OnChange := ScrollChanged;
  { Active line }
  FActiveLine := TBCEditorActiveLine.Create;
  FActiveLine.OnChange := ActiveLineChanged;
  { Sync edit }
  FSyncEdit := TBCEditorSyncEdit.Create();
  FSyncEdit.OnChange := SyncEditChanged;
  FSyncEditAvailable := False;
  { FLeftMargin }
  FLeftMargin := TBCEditorLeftMargin.Create(Self);
  FLeftMargin.OnChange := LeftMarginChanged;
  { Do update character constraints }
  TabsChanged(nil);
  { Highlighter }
  FHighlighter := TBCEditorHighlighter.Create(Self);
  FHighlighter.OnChange := HighlighterChanged;
end;

function TCustomBCEditor.CreateLines(): BCEditor.Lines.TBCEditorLines;
begin
  Result := BCEditor.Lines.TBCEditorLines.Create();
end;

procedure TCustomBCEditor.CreateParams(var AParams: TCreateParams);
const
  LBorderStyles: array [TBorderStyle] of DWORD = (0, WS_BORDER);
begin
  inherited;

  with AParams do
  begin
    WindowClass.Style := WindowClass.Style and not CS_VREDRAW or CS_HREDRAW;
    Style := Style or LBorderStyles[FBorderStyle] or WS_CLIPCHILDREN or ES_AUTOHSCROLL or ES_AUTOVSCROLL;
    if (FReadOnly) then
      Style := Style or ES_READONLY;

    if (NewStyleControls and Ctl3D and (FBorderStyle = bsSingle)) then
    begin
      Style := Style and not WS_BORDER;
      ExStyle := ExStyle or WS_EX_CLIENTEDGE;
    end;
  end;
end;

procedure TCustomBCEditor.CreateWnd();
begin
  inherited;

  FFormWnd := GetParentForm(Self).Handle;
  FParentWnd := FParentWnd;
  FDlgCtrlID := GetDlgCtrlID(Handle);

  OleCheck(RegisterDragDrop(Handle, Self));

  EnableScrollBar(Handle, SB_VERT, ESB_ENABLE_BOTH);
  EnableScrollBar(Handle, SB_HORZ, ESB_ENABLE_BOTH);

  FState := FState + [esFontChanged, esSizeChanged, esScrolled];
end;

procedure TCustomBCEditor.CutToClipboard();
begin
  if (not ReadOnly and not FLines.SelArea.IsEmpty()) then
  begin
    CopyToClipboard();
    SelText := '';
  end;
end;

procedure TCustomBCEditor.Delay(const AAction: TPendingJob);
begin
  Include(FPendingJobs, AAction);

  SetTimer(Handle, tiIdle, 10, nil);
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
    FBookmarkList.Remove(ABookmark);
end;

procedure TCustomBCEditor.DeleteChar();
begin
  if (not FLines.SelArea.IsEmpty()) then
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
  if (not FLines.SelArea.IsEmpty()) then
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
    if (FLines.Items[ALine].FirstRow >= 0) then
    begin
      LLastRow := FLines.Items[ALine].FirstRow;
      while (not (rfLastRowOfLine in FRows.Items[LLastRow].Flags)) do
        Inc(LLastRow);

      LDeletedRows := LLastRow - FLines.Items[ALine].FirstRow + 1;

      if (FLines.CaretPosition.Line = ALine) then
        InvalidateCaret()
      else if (not (esCaretInvalid in FState)
        and (FLines.CaretPosition.Line > ALine)) then
      begin
        Dec(FCaretPos.Y, LDeletedRows * LineHeight);
        UpdateCaret();
      end;

      for LRow := LLastRow downto FLines.Items[ALine].FirstRow do
        FRows.Delete(LRow);

      for LLine := ALine to FLines.Count - 1 do
        FLines.SetFirstRow(LLine, FLines.Items[LLine].FirstRow - LDeletedRows);

      if (UpdateCount > 0) then
        Include(FState, esRowsChanged)
      else
      begin
        InvalidateMatchingPair();
        InvalidateScrollBars();
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
  FPaintHelper.Free;
  if (Assigned(FInsertPosCache)) then
    FInsertPosCache.Free();
  if (Assigned(FInsertPosBitmap)) then
    FInsertPosBitmap.Free();
  if (Assigned(FScrollingBitmap)) then
    FScrollingBitmap.Free();
  FOriginalLines.Free;
  FActiveLine.Free;
  FScroll.Free;
  FSearch.Free;
  FSearchResults.Free();
  FReplace.Free;
  FTabs.Free;
  FSpecialChars.Free;
  FMatchingPair.Free;
  FCompletionProposal.Free;
  FSyncEdit.Free;
  FOverlays.Free();
  if (Assigned(FSyncEditButtonHotBitmap)) then
    FSyncEditButtonHotBitmap.Free();
  if (Assigned(FSyncEditButtonNormalBitmap)) then
    FSyncEditButtonNormalBitmap.Free();
  if (Assigned(FSyncEditButtonPressedBitmap)) then
    FSyncEditButtonPressedBitmap.Free();
  FRows.Free();
  if (Assigned(FHintWindow)) then
    FHintWindow.Free();

  inherited;
end;

procedure TCustomBCEditor.DestroyWnd();
begin
  RevokeDragDrop(Handle);

  FParentWnd := 0;

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
    if (not FLines.SelArea.IsEmpty()) then
      SelText := ''
    else if (FLines.CaretPosition > FLines.BOFPosition) then
    begin
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

        FLines.CaretPosition := FLines.DeleteText(LinesArea(LNewCaretPosition, FLines.CaretPosition), True);
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

        FLines.DeleteText(LinesArea(LNewCaretPosition, FLines.CaretPosition), True);
      end
      else
        FLines.CaretPosition := FLines.BOFPosition;
    end;
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

      if (FLines.SelArea.IsEmpty()) then
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
  if (FWordWrap) then
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
    if (not FLines.SelArea.IsEmpty()) then
    begin
      BeginUpdate();
      SelText := AText;
      FLines.SelArea := LinesArea(FLines.SelArea.EndPosition, FLines.SelArea.EndPosition);
      EndUpdate();
    end
    else if ((FTextEntryMode = temOverwrite)
      and (FLines.CaretPosition.Line < FLines.Count)
      and (FLines.CaretPosition.Char < Length(FLines.Items[FLines.CaretPosition.Line].Text))) then
      FLines.ReplaceText(LinesArea(FLines.CaretPosition, LinesPosition(FLines.CaretPosition.Char + 1, FLines.CaretPosition.Line)), AText)
    else
      FLines.InsertText(FLines.CaretPosition, AText);
  finally
    EndUpdate();
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

        if (FLines.SelArea.IsEmpty()) then
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

procedure TCustomBCEditor.DoReturnKey();
var
  LInsertText: string;
begin
  if (not FLines.SelArea.IsEmpty()) then
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

  if (not FLines.SelArea.IsEmpty()) then
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
  if ((toSelectedBlockIndent in FTabs.Options) and not FLines.SelArea.IsEmpty()) then
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
  else if (not FLines.SelArea.IsEmpty() or
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

procedure TCustomBCEditor.DoToggleSelectedCase(const ACommand: TBCEditorCommand);
var
  LSelectedText: string;
begin
  if (not FLines.SelArea.IsEmpty()) then
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

  InsertPos := InvalidPos;
end;

procedure TCustomBCEditor.DragDrop(ASource: TObject; X, Y: Integer);
begin
  if (not ReadOnly) then
  begin
    inherited;

    InsertPos := InvalidPos;
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
  InsertPos := InvalidPos;

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
    InsertPos := InvalidPos;
    dwEffect := DROPEFFECT_NONE;
  end
  else
  begin
    LScreen := ScreenToClient(pt);
    LPosition := ClientToLines(LScreen.X, LScreen.Y);
    if (FLines.SelArea.Contains(LPosition)) then
    begin
      InsertPos := InvalidPos;
      dwEffect := DROPEFFECT_NONE;
    end
    else
    begin
      InsertPos := LPosition;
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
      FLines.CaretPosition := InsertPos;
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
        LOldPosition := InsertPos;
        LNewPosition := FLines.InsertFile(InsertPos, LFilename);
        SetCaretAndSelection(LNewPosition, LinesArea(LOldPosition, LNewPosition));
        InsertPos := InvalidPos;
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

procedure TCustomBCEditor.EMCanUndo(var AMessage: TMessage);
begin
  AMessage.Result := LRESULT(CanUndo);
end;

procedure TCustomBCEditor.EMCharFromPos(var AMessage: TMessage);
var
  LPosition: TBCEditorLinesPosition;
begin
  LPosition := ClientToLines(AMessage.LParamLo, AMessage.LParamHi, True);
  AMessage.ResultLo := LPosition.Char;
  AMessage.ResultHi := LPosition.Line;
end;

procedure TCustomBCEditor.EMEmptyUndoBuffer(var AMessage: TMessage);
begin
  FLines.ClearUndo();
end;

procedure TCustomBCEditor.EMFmtLines(var AMessage: TMessage);
begin
  FFmtLines := BOOL(AMessage.WParam);

  AMessage.Result := AMessage.WParam;
end;

procedure TCustomBCEditor.EMGetFirstVisible(var AMessage: TMessage);
begin
  if (FTopRow < FRows.Count) then
    AMessage.Result := FRows.Items[FTopRow].Line
  else
    AMessage.Result := FLines.Count - 1;
end;

procedure TCustomBCEditor.EMGetHandle(var AMessage: TMessage);
begin
  AMessage.Result := 0;
end;

procedure TCustomBCEditor.EMGetIMEStatus(var AMessage: TMessage);
begin
  if (AMessage.WParam <> EMSIS_COMPOSITIONSTRING) then
    AMessage.Result := 0
  else
    AMessage.Result := FIMEStatus;
end;

procedure TCustomBCEditor.EMGetLine(var AMessage: TMessage);
var
  LLine: Integer;
begin
  if (AMessage.WParam > 0) then
    LLine := Integer(AMessage.WParam)
  else if (FTopRow >= FRows.Count) then
    LLine := -1
  else
    LLine := FRows.Items[FTopRow].Line;
  if ((LLine < 0) or (AMessage.LParam = 0) or (Word(Pointer(AMessage.LParam)^) >= Length(FLines.Items[LLine].Text))) then
    AMessage.Result := 0
  else
    StrPCopy(PChar(AMessage.LParam), FLines.Items[LLine].Text);
end;

procedure TCustomBCEditor.EMGetLineCount(var AMessage: TMessage);
begin
  if (FLines.Count = 0) then
    AMessage.Result := 1
  else
    AMessage.Result := LPARAM(FLines.Count);
end;

procedure TCustomBCEditor.EMGetModify(var AMessage: TMessage);
begin
  AMessage.Result := LRESULT(Modified);
end;

procedure TCustomBCEditor.EMGetRect(var AMessage: TMessage);
begin
  if (AMessage.LParam <> 0) then
    Windows.PRect(AMessage.LParam)^ := TextRect;
end;

procedure TCustomBCEditor.EMGetSel(var AMessage: TMessage);
var
  LSelStart: Integer;
  LSelLength: Integer;
begin
  LSelStart := SelStart;
  LSelLength := SelLength;

  if (AMessage.WParam <> 0) then
    PDWORD(AMessage.WParam)^ := LSelStart;
  if (AMessage.WParam <> 0) then
    PDWORD(AMessage.WParam)^ := LSelStart + SelLength;

  if (LSelStart + LSelLength > 65535) then
    AMessage.Result := -1
  else
  begin
    AMessage.ResultLo := LSelStart;
    AMessage.ResultHi := LSelStart + LSelLength;
  end;
end;

procedure TCustomBCEditor.EMGetThumb(var AMessage: TMessage);
begin
  AMessage.Result := TopRow * FLineHeight;
end;

procedure TCustomBCEditor.EMLineFromChar(var AMessage: TMessage);
begin
  if (Integer(AMessage.WParam) <> -1) then
    if (AMessage.WParam <= WPARAM(FLines.TextLength)) then
      AMessage.Result := FLines.PositionOf(Integer(AMessage.WParam)).Line
    else
      AMessage.Result := 0
  else if (not FLines.SelArea.IsEmpty()) then
    AMessage.Result := Min(FLines.SelArea.BeginPosition, FLines.SelArea.EndPosition).Line
  else
    AMessage.Result := Min(Max(0, FLines.Count - 1), FLines.CaretPosition.Line);
end;

procedure TCustomBCEditor.EMLineIndex(var AMessage: TMessage);
var
  LLine: Integer;
begin
  if (Integer(AMessage.WParam) = - 1) then
    LLine := FLines.CaretPosition.Line
  else
    LLine := Integer(AMessage.WParam);

  AMessage.Result := LRESULT(FLines.CharIndexOf(FLines.BOLPosition[LLine]));
end;

procedure TCustomBCEditor.EMLineLength(var AMessage: TMessage);
var
  LLine: Integer;
begin
  if (Integer(AMessage.WParam) = -1) then
    AMessage.Result := FLines.CharIndexOf(FLines.SelArea.BeginPosition) - FLines.CharIndexOf(FLines.BOLPosition[FLines.SelArea.BeginPosition.Line])
  else if (Integer(AMessage.WParam) < FLines.TextLength) then
    AMessage.Result := 0
  else
  begin
    LLine := FLines.PositionOf(AMessage.WParam).Line;
    if (LLine < FLines.Count) then
      AMessage.Result := Length(FLines.Items[LLine].Text)
    else
      AMessage.Result := 0;
  end;
end;

procedure TCustomBCEditor.EMLineScroll(var AMessage: TMessage);
begin
  AMessage.Result := LRESULT(TRUE);
end;

procedure TCustomBCEditor.EMPosFromChar(var AMessage: TMessage);
var
  LClient: TPoint;
begin
  if (AMessage.WParam >= WPARAM(FLines.TextLength)) then
    AMessage.Result := -1
  else
  begin
    LClient := RowsToClient(LinesToRows(FLines.PositionOf(AMessage.WParam)));
    AMessage.ResultLo := LClient.X;
    AMessage.ResultHi := LClient.Y;
  end;
end;

procedure TCustomBCEditor.EMReplaceSel(var AMessage: TMessage);
begin
  SelText := StrPas(PChar(AMessage.LParam));
  if (not BOOL(AMessage.WParam)) then
    ClearUndo();
end;

procedure TCustomBCEditor.EMScroll(var AMessage: TMessage);
begin
  case (AMessage.WParam) of
    SB_LINEDOWN: MoveCaretVertically(1, False);
    SB_LINEUP: MoveCaretVertically(-1, False);
    SB_PAGEDOWN: MoveCaretVertically(FVisibleRows, False);
    SB_PAGEUP: MoveCaretVertically(- FVisibleRows, False);
  end;
end;

procedure TCustomBCEditor.EMScrollCaret(var AMessage: TMessage);
begin
  ScrollToCaret();
end;

procedure TCustomBCEditor.EMSetIMEStatus(var AMessage: TMessage);
begin
  if (AMessage.WParam <> EMSIS_COMPOSITIONSTRING) then
    AMessage.Result := 0
  else
  begin
    AMessage.Result := FIMEStatus;
    FIMEStatus := AMessage.LParam;
  end;
end;

procedure TCustomBCEditor.EMSetModify(var AMessage: TMessage);
begin
  Modified := BOOL(AMessage.WParam);
end;

procedure TCustomBCEditor.EMSetReadOnly(var AMessage: TMessage);
begin
  ReadOnly := BOOL(AMessage.WParam);
  AMessage.Result := LRESULT(TRUE);
end;

procedure TCustomBCEditor.EMSetSel(var AMessage: TMessage);
begin
  if (AMessage.wParam = WPARAM(-1)) then
    SelLength := 0
  else if (AMessage.lParam = LPARAM(-1)) then
    SetCaretAndSelection(FLines.EOFPosition, FLines.Area)
  else
  begin
    SelStart := Integer(AMessage.WParam);
    SelLength := Integer(AMessage.WParam) + Integer(AMessage.LParam);
  end;
end;

procedure TCustomBCEditor.EMSetTabStop(var AMessage: TMessage);
type
  PUNIT = ^UINT;
begin
  if (AMessage.WParam <> 1) then
    AMessage.Result := LRESULT(FALSE)
  else
  begin
    FTabs.Width := PUNIT(AMessage.LParam)^;
    AMessage.Result := LRESULT(TRUE);
  end;
end;

procedure TCustomBCEditor.EMUndo(var AMessage: TMessage);
begin
  AMessage.Result := LRESULT(CanUndo);
  Undo();
end;

procedure TCustomBCEditor.EndUndoBlock();
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
  if (not FLines.SelArea.IsEmpty()) then
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
      MoveCaretHorizontally(-1, ACommand = ecSelectionLeft);
    ecRight, ecSelectionRight:
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
    ecGotoNextBookmark:
      GotoNextBookmark;
    ecGotoPreviousBookmark:
      GotoPreviousBookmark;
    ecGotoBookmark1 .. ecGotoBookmark0:
      if FLeftMargin.Bookmarks.ShortCuts then
        GotoBookmark(ACommand - ecGotoBookmark1);
    ecSetBookmark1 .. ecSetBookmark0:
      if FLeftMargin.Bookmarks.ShortCuts then
        DoSetBookmark(ACommand, AData);
    ecWordLeft, ecSelectionWordLeft:
      DoWordLeft(ACommand);
    ecWordRight, ecSelectionWordRight:
      DoWordRight(ACommand);
    ecSelectionWord:
      SetSelectedWord;
    ecSelectAll:
      SelectAll();
    ecBackspace:
      if (not ReadOnly) then
        DoBackspace();
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
    ecReturn:
      if (not ReadOnly and FWantReturns) then
        DoReturnKey();
    ecTab:
      if (not ReadOnly and FWantTabs) then
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
      CutToClipboard();
    ecCopy:
      CopyToClipboard();
    ecPaste:
      PasteFromClipboard();
    ecScrollUp, ecScrollDown:
      DoScroll(ACommand);
    ecScrollLeft:
      HorzTextPos := HorzTextPos - 1;
    ecScrollRight:
      HorzTextPos := HorzTextPos + 1;
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

function TCustomBCEditor.FindFirst(): Boolean;
var
  LArea: TBCEditorLinesArea;
  LPosition: TBCEditorLinesPosition;
  LSearchResult: TBCEditorLinesArea;
begin
  if (not FLines.SelArea.IsEmpty() and FSearch.InSelection.Active) then
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

procedure TCustomBCEditor.FontChanged(ASender: TObject);
begin
  FState := FState + [esFontChanged];
  InvalidateScrollBars();
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

function TCustomBCEditor.GetCanPaste(): Boolean;
begin
  Result := not ReadOnly and (IsClipboardFormatAvailable(CF_TEXT) or IsClipboardFormatAvailable(CF_UNICODETEXT));
end;

function TCustomBCEditor.GetCanRedo(): Boolean;
begin
  Result := not ReadOnly and FLines.CanRedo;
end;

function TCustomBCEditor.GetCanUndo(): Boolean;
begin
  Result := not ReadOnly and FLines.CanUndo;
end;

function TCustomBCEditor.GetCaretPos(): TPoint;
begin
  Result := FLines.CaretPosition;
end;

function TCustomBCEditor.GetCharAt(APos: TPoint): Char;
begin
  Result := FLines.Char[APos];
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

function TCustomBCEditor.GetSearchResultCount: Integer;
begin
  Result := FSearchResults.Count;
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
  Result := FLines.CharIndexOf(Max(FLines.SelArea.BeginPosition, FLines.SelArea.EndPosition),
    Min(FLines.SelArea.BeginPosition, FLines.SelArea.EndPosition));
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
  Result := GetWordAtLinesPosition(ALinesPos);
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
  InvalidateRows();

  LElement := FHighlighter.Colors.GetElement(BCEDITOR_ATTRIBUTE_ELEMENT_EDITOR);
  if (Assigned(LElement) and (LElement^.Foreground <> clNone)) then
    Font.Color := LElement^.Foreground
  else
    Font.Color := clWindowText;
  if (Assigned(LElement) and (LElement^.Background <> clNone)) then
    Color := LElement^.Background
  else
    Color := clWindow;

  Include(FState, esHighlighterChanged);
  InvalidateCaret();
  Invalidate();
end;

procedure TCustomBCEditor.HookEditorLines(ALines: TBCEditorLines; AUndo, ARedo: TBCEditorLines.TUndoList);
var
  LOldWordWrap: Boolean;
begin
  Assert(not Assigned(FChainedEditor));
  Assert(FLines = FOriginalLines);

  LOldWordWrap := FWordWrap;
  WordWrap := False;

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

  WordWrap := LOldWordWrap;
end;

procedure TCustomBCEditor.Idle();
begin
  if (pjBuildRows in FPendingJobs) then
  begin
    Exclude(FPendingJobs, pjBuildRows);
    BuildRows(Canvas, FRows.Count + 1);
  end;
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
  if (not FLines.SelArea.IsEmpty()) then
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

    if (not (esCaretInvalid in FState)
      and (FLines.CaretPosition.Line >= ALine)) then
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
  FPaintHelper.BeginDrawing(Canvas);
  try
    if (not FWordWrap) then
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
      LMaxRowWidth := FTextRect.Width;
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
      InvalidateMatchingPair();
      InvalidateScrollBars();
      Invalidate();
    end;
end;

procedure TCustomBCEditor.InvalidateCaret();
begin
  FRows.CaretPosition := InvalidRowsPosition;
  FCaretPos := InvalidPos;
  SetInsertPos(InvalidPos);
  FState := FState + [esCaretInvalid];

  if (not (esPainting in FState)) then
    Invalidate();
end;

procedure TCustomBCEditor.InvalidateMatchingPair();
begin
  FCurrentMatchingPair.FState := mpsClear;

  if (not (esPainting in FState)) then
    Invalidate();
end;

procedure TCustomBCEditor.InvalidateRows();
var
  LLine: Integer;
begin
  FRows.Clear();
  for LLine := 0 to FLines.Count - 1 do
    FLines.SetFirstRow(LLine, RowToInsert);
  FLastBuiltLine := -1;
end;

procedure TCustomBCEditor.InvalidateScrollBars();
begin
  Include(FState, esScrolled);

  if (not (esPainting in FState)) then
    Invalidate();
end;

procedure TCustomBCEditor.InvalidateSyncEdit();
begin
  if (FLines.SyncEdit) then
    Include(FState, esSyncEditInvalid);

  if (not (esPainting in FState)) then
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

  if (AKey = 0) then
    Include(FState, esIgnoreNextChar)
  else if ((AKey = BCEDITOR_ESCAPE_KEY) and FLines.SyncEdit) then
  begin
    FLines.DeactivateSyncEdit();
    AKey := 0;
    Exit;
  end
  else if ((AKey = BCEDITOR_ESCAPE_KEY) and (esScrolling in FState)) then
  begin
    Process(paMouseDown, nil, mbMiddle, [], 0, 0);
    AKey := 0;
    Exit;
  end;

  if FSyncEdit.Enabled then
  begin
    ShortCutToKey(FSyncEdit.ShortCut, LShortCutKey, LShortCutShift);
    if (AShift = LShortCutShift) and (AKey = LShortCutKey) then
    begin
      if (not FLines.SyncEdit) then
        FLines.ActivateSyncEdit(FHighlighter)
      else
        FLines.DeactivateSyncEdit();
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
  UpdateMarks(FBookmarkList);
  UpdateMarks(FMarkList);

  if (ALine < FLines.Count - 1) then
    FLines.SetBeginRange(ALine + 1, FLines.Items[ALine].BeginRange);

  Modified := True;

  if (UpdateCount > 0) then
    Include(FState, esLinesDeleted)
  else
  begin
    Change();

    if (HandleAllocated) then
    begin
      Include(FState, esCodeFoldingInvalid);
      KillTimer(Handle, tiCodeFolding);
      SetTimer(Handle, tiCodeFolding, FCodeFolding.DelayInterval, nil);
    end;
  end;
end;

procedure TCustomBCEditor.LineDeleting(ASender: TObject; const ALine: Integer);
var
  LRow: Integer;
begin
  LRow := FLines.Items[ALine].FirstRow;
  if (LRow >= 0) then
  begin
    while (not (rfLastRowOfLine in FRows.Items[LRow].Flags)) do
      Inc(LRow);
    if (LRow = FRows.Count - 1) then
      LRow := -1
    else
      Inc(LRow);
  end
  else if (FRows.Count > 0) then
    repeat
      Inc(LRow);
    until (FRows.Items[LRow].Line > ALine);

  if (LRow >= 0) then
    while (LRow < FRows.Count) do
    begin
      Dec(FRows.List[LRow].Line);
      Inc(LRow);
    end;

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
  UpdateMarks(FBookmarkList);
  UpdateMarks(FMarkList);
  UpdateLinesBeginRanges(ALine);

  InsertLineIntoRows(ALine, True);

  Modified := True;

  if (UpdateCount > 0) then
    Include(FState, esLinesInserted)
  else
  begin
    Change();

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
  InvalidateMatchingPair();
  InvalidateCaret();
  InvalidateSyncEdit();

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
  ClearBookmarks;
  FMarkList.Clear;
  InvalidateRows();

  Modified := True;

  if (UpdateCount > 0) then
    Include(FState, esLinesCleared)
  else
  begin
    Change();
    InitCodeFolding();

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
  begin
    FLineNumbersWidth := 2 * GPadding + Max(2, Length(IntToStr(FLines.Count + VisibleRows + 1))) * FMaxDigitWidth;
    ComputeMetrics();
  end;

  InvalidateMatchingPair();
  InvalidateScrollBars();
  Invalidate();
end;

procedure TCustomBCEditor.LinesHookChanged;
begin
  InvalidateScrollBars();
  Invalidate();
end;

procedure TCustomBCEditor.LinesLoaded(ASender: TObject);
begin
  Loaded();
  Modified := False;
end;

procedure TCustomBCEditor.LinesSyncEditChanged(ASender: TObject);
begin
  InvalidateSyncEdit();
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
  else if (FLines.Items[ALinesPosition.Line].FirstRow < 0) then
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
      while (LLinePos < LLineEndPos) do
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
  UpdateRows(ALine);
  UpdateLinesBeginRanges(ALine);

  Modified := True;

  if (UpdateCount > 0) then
    Include(FState, esLinesUpdated)
  else
  begin
    Change();

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

procedure TCustomBCEditor.MouseDown(AButton: TMouseButton; AShift: TShiftState; X, Y: Integer);
var
  LAction: TProcessAction;
begin
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

  Include(FState, esHandlingMouse);
  Process(LAction, nil, AButton, AShift, X, Y);
  Exclude(FState, esHandlingMouse);
end;

procedure TCustomBCEditor.MouseMove(AShift: TShiftState; X, Y: Integer);
var
  LMsg: TMsg;
begin
  if (Assigned(FHintWindow)
    and (Point(X, Y) <> FCursorPoint)) then
    FreeAndNil(FHintWindow);
  if (MouseCapture = mcText) then
    KillTimer(Handle, tiScroll);

  FCursorPoint := Point(X, Y);

  inherited;

  if (PeekMessage(LMsg, Handle, WM_MOUSEMOVE, WM_MOUSEMOVE, PM_NOREMOVE)
    and (LMsg.Message = WM_MOUSEMOVE)
    and (KeysToShiftState(LMsg.wParam) = AShift)) then
    // Do nothing - handle this message within the next equal message
  else if (FLineHeight > 0) then
  begin
    Include(FState, esHandlingMouse);
    Process(paMouseMove, nil, mbLeft, AShift, X, Y);
    Exclude(FState, esHandlingMouse);

    if (not Assigned(FHintWindow)
      and (Point(X, Y) <> FLastCursorPoint)
      and (AShift * [ssLeft, ssRight, ssMiddle] = [])) then
      if (ClientRect.Contains(Point(X, Y))) then
        SetTimer(Handle, tiShowHint, Application.HintPause, nil)
      else
        KillTimer(Handle, tiShowHint);
    FLastCursorPoint := FCursorPoint;
  end;
end;

procedure TCustomBCEditor.MouseUp(AButton: TMouseButton; AShift: TShiftState; X, Y: Integer);
begin
  KillTimer(Handle, tiShowHint);
  if (MouseCapture = mcText) then
    KillTimer(Handle, tiScroll);

  inherited;

  FKeyboardHandler.ExecuteMouseUp(Self, AButton, AShift, X, Y);

  Include(FState, esHandlingMouse);
  Process(paMouseUp, nil, AButton, AShift, X, Y);
  Exclude(FState, esHandlingMouse);

  if (not (esScrolling in FState)) then
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
      if (not (soBeyondEndOfLine in FScroll.Options) or FWordWrap) then
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
    else if ((soBeyondEndOfLine in FScroll.Options) and not FWordWrap) then
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

  if (not (soBeyondEndOfLine in FScroll.Options) or FWordWrap) then
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

  procedure BuildBitmaps(const APaintVar: PPaintVar); {$IFNDEF Debug} inline; {$ENDIF}
  var
    LBackgroundColor: TGPColor;
    LBitmap: TGPBitmap;
    LBrush: TGPSolidBrush;
    LColor: TGPColor;
    LFont: TGPFont;
    LGraphics: TGPGraphics;
    LHDC: HDC;
    LHeight: Integer;
    LIcon: TGPBitmap;
    LIconId: Integer;
    LIndex: Integer;
    LPen: TGPPen;
    LPoints: array [0 .. 2] of TGPPoint;
    LRect: TRect;
    LRectF: TGPRectF;
    LResData: HGLOBAL;
    LResInfo: HRSRC;
    LResource: Pointer;
    LStringFormat: TGPStringFormat;
    LText: string;
    LWidth: Integer;
    LY: Integer;
  begin
    LBrush := TGPSolidBrush.Create(aclTransparent);
    LFont := TGPFont.Create(GetParentForm(Self).Font.Name, FLineHeight - 2 * GPadding - 2 * GLineWidth, FontStyleRegular, UnitPixel);
    LStringFormat := TGPStringFormat.Create();
    LStringFormat.SetAlignment(StringAlignmentCenter);
    LPen := TGPPen.Create(aclTransparent, GLineWidth);
    LWidth := Min(FLineHeight, GetSystemMetrics(SM_CXSMICON));
    LHeight := LWidth;

    // Bookmarks
    LBitmap := TGPBitmap.Create(LWidth, LHeight);
    LGraphics := TGPGraphics.Create(LBitmap);
    for LIndex := 0 to BCEDITOR_BOOKMARKS - 1 do
    begin
      if (Assigned(FBookmarkBitmaps[LIndex])) then FBookmarkBitmaps[LIndex].Free();
      LBrush.SetColor(aclTransparent);
      LGraphics.FillRectangle(LBrush, 0, 0, LWidth, LHeight);
      if (LIndex < BCEDITOR_BOOKMARKS - 1) then
        LText := IntToStr(LIndex + 1)
      else
        LText := '0';
      LRectF := MakeRect(GPadding + 2 * GLineWidth + 0.0,
        GPadding + GLineWidth,
        LWidth - 2 * GPadding - 3 * GLineWidth,
        LHeight - 2 * GPadding - 3 * GLineWidth);
      LBrush.SetColor(ColorRefToARGB(ColorToRGB(clBookmarkCover)));
      LGraphics.FillRectangle(LBrush, LRectF);
      LRectF := MakeRect(GPadding + 2 * GLineWidth + 0.0,
        GPadding + GLineWidth - 2,
        LWidth - 2 * GPadding - 3 * GLineWidth,
        LHeight - 2 * GPadding - GLineWidth);
      LBrush.SetColor(ColorRefToARGB(ColorToRGB(clBookmarkNumber)));
      LGraphics.DrawString(LText, -1, LFont, LRectF, LStringFormat, LBrush);
      LPen.SetColor(ColorRefToARGB(ColorToRGB(clBookmarkBorder)));
      LGraphics.DrawRectangle(LPen, GPadding + GLineWidth, GPadding, FLineHeight - 2 * GPadding - 2 * GLineWidth, FLineHeight - 3 * GPadding - GLineWidth);

      LY := GPadding + 2 * GLineWidth;
      repeat
        LBrush.SetColor(ColorRefToARGB(ColorToRGB(clBookmarkRingLeft)));
        LGraphics.FillRectangle(LBrush, GPadding, LY, GLineWidth, GLineWidth);
        LBrush.SetColor(ColorRefToARGB(ColorToRGB(clBookmarkRingMiddle)));
        LGraphics.FillRectangle(LBrush, GPadding + GLineWidth, LY, GLineWidth, GLineWidth);
        LBrush.SetColor(ColorRefToARGB(ColorToRGB(clBookmarkRingRight)));
        LGraphics.FillRectangle(LBrush, GPadding + 2 * GLineWidth, LY, GLineWidth, GLineWidth);
        Inc(LY, 2 * GLineWidth);
      until (LY >= FLineHeight - 2 * GPadding - 2 * GLineWidth);

      FBookmarkBitmaps[LIndex] := TGPCachedBitmap.Create(LBitmap, APaintVar^.Graphics);
    end;
    LGraphics.Free();
    LBitmap.Free();

    // CodeFoling

    if (FCodeFolding.Colors.Background <> clNone) then
      LBackgroundColor :=ColorRefToARGB(ColorToRGB(FCodeFolding.Colors.Background))
    else if (FLeftMargin.Colors.Background <> clNone) then
      LBackgroundColor :=ColorRefToARGB(ColorToRGB(FLeftMargin.Colors.Background))
    else
      LBackgroundColor :=ColorRefToARGB(ColorToRGB(Color));
    if (FLeftMargin.Colors.Foreground <> clNone) then
      LColor := ColorRefToARGB(ColorToRGB(FCodeFolding.Colors.Foreground))
    else
      LColor := ColorRefToARGB(ColorToRGB(Font.Color));
    LPen.SetColor(LColor);

    // CodeFolding None / Collapsed / Expanded
    if (Assigned(FCodeFoldingNoneBitmap)) then FCodeFoldingNoneBitmap.Free();
    if (Assigned(FCodeFoldingCollapsedBitmap)) then FCodeFoldingCollapsedBitmap.Free();
    if (Assigned(FCodeFoldingExpandedBitmap)) then FCodeFoldingExpandedBitmap.Free();
    LBitmap := TGPBitmap.Create(LWidth, LHeight);
    LGraphics := TGPGraphics.Create(LBitmap);
    LBrush.SetColor(LBackgroundColor);
    LGraphics.FillRectangle(LBrush, 0, 0, LWidth, LHeight);
    FCodeFoldingNoneBitmap := TGPCachedBitmap.Create(LBitmap, APaintVar^.Graphics);
    LBrush.SetColor(LColor);
    LGraphics.DrawRectangle(LPen, GPadding + 2 * GLineWidth, GPadding + 2 * GLineWidth, LWidth - 2 * GPadding - 6 * GLineWidth, LHeight - 2 * GPadding - 6 * GLineWidth);
    LGraphics.DrawLine(LPen, GPadding + 4 * GLineWidth, (2 * LHeight - GLineWidth) div 4, LWidth - GPadding - 6 * GLineWidth, (2 * LHeight - GLineWidth) div 4);
    FCodeFoldingCollapsedBitmap := TGPCachedBitmap.Create(LBitmap, APaintVar^.Graphics);
    LGraphics.DrawLine(LPen, (2 * LWidth - GLineWidth) div 4, GPadding + 4 * GLineWidth, (2 * LWidth - GLineWidth) div 4, LHeight - GPadding - 6 * GLineWidth);
    FCodeFoldingExpandedBitmap := TGPCachedBitmap.Create(LBitmap, APaintVar^.Graphics);
    LGraphics.Free();
    LBitmap.Free();

    // CodeFolding Line / EndLine
    if (Assigned(FCodeFoldingLineBitmap)) then FCodeFoldingLineBitmap.Free();
    if (Assigned(FCodeFoldingEndLineBitmap)) then FCodeFoldingEndLineBitmap.Free();
    LBitmap := TGPBitmap.Create(LWidth, LHeight);
    LGraphics := TGPGraphics.Create(LBitmap);
    LBrush.SetColor(LBackgroundColor);
    LGraphics.FillRectangle(LBrush, 0, 0, LWidth, LHeight);
    LGraphics.DrawLine(LPen, (2 * LWidth - GLineWidth) div 4, 0, (2 * LWidth - GLineWidth) div 4, LHeight - GLineWidth);
    FCodeFoldingLineBitmap := TGPCachedBitmap.Create(LBitmap, APaintVar^.Graphics);
    LGraphics.DrawLine(LPen, (2 * LWidth - GLineWidth) div 4, LHeight - GLineWidth, LWidth - GLineWidth, LHeight - GLineWidth);
    FCodeFoldingEndLineBitmap := TGPCachedBitmap.Create(LBitmap, APaintVar^.Graphics);
    LGraphics.Free();

    // InsertPos Mark
    if (Assigned(FInsertPosBitmap)) then FInsertPosBitmap.Free();
    LWidth := 3 * GLineWidth;
    LHeight := FLineHeight;
    LBitmap := TGPBitmap.Create(LWidth, LHeight);
    LGraphics := TGPGraphics.Create(LBitmap);
    LBrush.SetColor(aclTransparent);
    LGraphics.FillRectangle(LBrush, 0, 0, LWidth, LHeight);
    LBrush.SetColor(ColorRefToARGB(ColorToRGB(Font.Color)));
    LGraphics.FillRectangle(LBrush, GLineWidth, GPadding, GLineWidth, LineHeight - GLineWidth - GPadding);
    LGraphics.SetSmoothingMode(SmoothingModeHighQuality);
    LPen.SetColor(ColorRefToARGB(ColorToRGB(Font.Color)));
    LPoints[0] := MakePoint(0, GPadding);
    LPoints[1] := MakePoint(GLineWidth, GPadding + GLineWidth);
    LPoints[2] := MakePoint(GLineWidth, GPadding);
    LGraphics.DrawPolygon(LPen, PGPPoint(@LPoints[0]), 3);
    LGraphics.FillPolygon(LBrush, PGPPoint(@LPoints[0]), 3);
    LPoints[0] := MakePoint(2 * GLineWidth - 1, GPadding);
    LPoints[1] := MakePoint(2 * GLineWidth - 1, GPadding + GLineWidth);
    LPoints[2] := MakePoint(3 * GLineWidth - 1, GPadding);
    LGraphics.DrawPolygon(LPen, PGPPoint(@LPoints[0]), 3);
    LGraphics.FillPolygon(LBrush, PGPPoint(@LPoints[0]), 3);
    LPoints[0] := MakePoint(0, LineHeight - 1 - GPadding);
    LPoints[1] := MakePoint(GLineWidth, LineHeight - 1 - GLineWidth - GPadding);
    LPoints[2] := MakePoint(GLineWidth, LineHeight - 1 - GPadding);
    LGraphics.DrawPolygon(LPen, PGPPoint(@LPoints[0]), 3);
    LGraphics.FillPolygon(LBrush, PGPPoint(@LPoints[0]), 3);
    LPoints[0] := MakePoint(2 * GLineWidth - 1, LineHeight - 1 - GPadding);
    LPoints[1] := MakePoint(2 * GLineWidth - 1, LineHeight - 1 - GLineWidth - GPadding);
    LPoints[2] := MakePoint(3 * GLineWidth - 1, LineHeight - 1 - GPadding);
    LGraphics.DrawPolygon(LPen, PGPPoint(@LPoints[0]), 3);
    LGraphics.FillPolygon(LBrush, PGPPoint(@LPoints[0]), 3);
    FInsertPosBitmap := TGPCachedBitmap.Create(LBitmap, APaintVar^.Graphics);
    LGraphics.Free();


    if (esSysFontChanged in FState) then
    begin
      // Scrolling Anchor
      if (Assigned(FScrollingBitmap)) then FScrollingBitmap.Free();
      LWidth := 2 * GetSystemMetrics(SM_CXSMICON) - GetSystemMetrics(SM_CXSMICON) div 4;
      LHeight := LWidth;
      FScrollingBitmapWidth := LWidth;
      FScrollingBitmapHeight := LHeight;
      LBitmap := TGPBitmap.Create(LWidth, LHeight);
      LGraphics := TGPGraphics.Create(LBitmap);
      LBrush.SetColor(aclTransparent);
      LGraphics.FillRectangle(LBrush, 0, 0, LWidth, LHeight);
      LGraphics.SetSmoothingMode(SmoothingModeHighQuality);
      LBrush.SetColor(ColorRefToARGB(ColorToRGB(Color)));
      LGraphics.FillEllipse(LBrush, GLineWidth, GLineWidth, LWidth - GLineWidth - 1, LHeight - GLineWidth - 1);
      LPen.SetColor(ColorRefToARGB(ColorToRGB(Font.Color)));
      LGraphics.DrawEllipse(LPen, GLineWidth, GLineWidth, LWidth - GLineWidth - 1, LHeight - GLineWidth - 1);
      LBrush.SetColor(ColorRefToARGB(ColorToRGB(Font.Color)));
      LPoints[0].X := LWidth div 2;
      LPoints[0].Y := 4 * GLineWidth;
      LPoints[1].X := LWidth div 2 - 4 * GLineWidth;
      LPoints[1].Y := 8 * GLineWidth;
      LPoints[2].X := LWidth div 2 + 4 * GLineWidth;
      LPoints[2].Y := 8 * GLineWidth;
      LGraphics.DrawPolygon(LPen, PGPPoint(@LPoints[0]), 3);
      LGraphics.FillPolygon(LBrush, PGPPoint(@LPoints[0]), 3);
      LPoints[0].X := LWidth - 4 * GLineWidth;
      LPoints[0].Y := LHeight div 2;
      LPoints[1].X := LWidth - 8 * GLineWidth;
      LPoints[1].Y := LHeight div 2 - 4 * GLineWidth;
      LPoints[2].X := LWidth - 8 * GLineWidth;
      LPoints[2].Y := LHeight div 2 + 4 * GLineWidth;
      LGraphics.DrawPolygon(LPen, PGPPoint(@LPoints[0]), 3);
      LGraphics.FillPolygon(LBrush, PGPPoint(@LPoints[0]), 3);
      LPoints[0].X := LWidth div 2;
      LPoints[0].Y := LHeight - 4 * GLineWidth;
      LPoints[1].X := LWidth div 2 - 4 * GLineWidth;
      LPoints[1].Y := LHeight - 8 * GLineWidth;
      LPoints[2].X := LWidth div 2 + 4 * GLineWidth;
      LPoints[2].Y := LHeight - 8 * GLineWidth;
      LGraphics.DrawPolygon(LPen, PGPPoint(@LPoints[0]), 3);
      LGraphics.FillPolygon(LBrush, PGPPoint(@LPoints[0]), 3);
      LPoints[0].X := 4 * GLineWidth;
      LPoints[0].Y := LHeight div 2;
      LPoints[1].X := 8 * GLineWidth;
      LPoints[1].Y := LHeight div 2 - 4 * GLineWidth;
      LPoints[2].X := 8 * GLineWidth;
      LPoints[2].Y := LHeight div 2 + 4 * GLineWidth;
      LGraphics.DrawPolygon(LPen, PGPPoint(@LPoints[0]), 3);
      LGraphics.FillPolygon(LBrush, PGPPoint(@LPoints[0]), 3);
      LGraphics.DrawEllipse(LPen, LWidth div 2 - 2 * GLineWidth, LHeight div 2 - 2 * GLineWidth, 4 * GLineWidth, 4 * GLineWidth);
      LGraphics.FillEllipse(LBrush, LWidth div 2 - 2 * GLineWidth, LHeight div 2 - 2 * GLineWidth, 4 * GLineWidth, 4 * GLineWidth);
      FScrollingBitmap := TGPCachedBitmap.Create(LBitmap, APaintVar^.Graphics);
      LGraphics.Free();


      // SyncEdit Button
      LResInfo := FindResource(HInstance, BCEDITOR_SYNCEDIT, RT_GROUP_ICON);
      LResData := LoadResource(HInstance, LResInfo);
      LResource := LockResource(LResData);
      LIconId := LookupIconIdFromDirectoryEx(LResource, TRUE, GetSystemMetrics(SM_CXSMICON), GetSystemMetrics(SM_CYSMICON), LR_DEFAULTCOLOR);
      LResInfo := FindResource(HInstance, MAKEINTRESOURCE(LIconId), RT_ICON);
      LResData := LoadResource(HInstance, LResInfo);
      LIcon := TGPBitmap.Create(CreateIconFromResourceEx(
        LockResource(LResData), SizeOfResource(HInstance, LResInfo),
        TRUE, $00030000, GetSystemMetrics(SM_CXSMICON), GetSystemMetrics(SM_CYSMICON), LR_DEFAULTCOLOR));
      LWidth := GetSystemMetrics(SM_CXSMICON) + 2 * GetSystemMetrics(SM_CXEDGE);
      LHeight := GetSystemMetrics(SM_CYSMICON) + 2 * GetSystemMetrics(SM_CYEDGE);
      LRect := Rect(0, 0, LWidth, LHeight);
      LBrush.SetColor(clTransparent);
      LBitmap := TGPBitmap.Create(LWidth, LHeight);
      LGraphics := TGPGraphics.Create(LBitmap);
      if (Assigned(FSyncEditButtonNormalBitmap)) then FSyncEditButtonNormalBitmap.Free();
      if (not StyleServices.Enabled) then
      begin
        LBrush.SetColor(ColorRefToARGB(ColorToRGB(clBtnFace)));
        LGraphics.FillRectangle(LBrush, 0, 0, LWidth, LHeight);
        LHDC := LGraphics.GetHDC();
        DrawEdge(LHDC, LRect, BDR_RAISEDINNER, BF_RECT);
        LGraphics.ReleaseHDC(LHDC);
      end
      else
      begin
        LGraphics.FillRectangle(LBrush, 0, 0, LWidth, LHeight);
        LHDC := LGraphics.GetHDC();
        StyleServices.DrawElement(LHDC, StyleServices.GetElementDetails(tbPushButtonNormal), LRect);
        LGraphics.ReleaseHDC(LHDC);
      end;
      LGraphics.DrawImage(LIcon, GetSystemMetrics(SM_CXEDGE), GetSystemMetrics(SM_CYEDGE));
      FSyncEditButtonNormalBitmap := TGPCachedBitmap.Create(LBitmap, APaintVar^.Graphics);
      if (Assigned(FSyncEditButtonHotBitmap)) then FSyncEditButtonHotBitmap.Free();
      if (not StyleServices.Enabled) then
      begin
        LBrush.SetColor(ColorRefToARGB(ColorToRGB(clBtnFace)));
        LGraphics.FillRectangle(LBrush, 0, 0, LWidth, LHeight);
        LHDC := LGraphics.GetHDC();
        DrawEdge(LHDC, LRect, BDR_RAISED, BF_RECT);
        LGraphics.ReleaseHDC(LHDC);
      end
      else
      begin
        LGraphics.FillRectangle(LBrush, 0, 0, LWidth, LHeight);
        LHDC := LGraphics.GetHDC();
        StyleServices.DrawElement(LHDC, StyleServices.GetElementDetails(tbPushButtonHot), LRect);
        LGraphics.ReleaseHDC(LHDC);
      end;
      LGraphics.DrawImage(LIcon, GetSystemMetrics(SM_CXEDGE), GetSystemMetrics(SM_CYEDGE));
      FSyncEditButtonHotBitmap := TGPCachedBitmap.Create(LBitmap, APaintVar^.Graphics);
      if (Assigned(FSyncEditButtonPressedBitmap)) then FSyncEditButtonPressedBitmap.Free();
      if (not StyleServices.Enabled) then
      begin
        LBrush.SetColor(ColorRefToARGB(ColorToRGB(clBtnFace)));
        LGraphics.FillRectangle(LBrush, 0, 0, LWidth, LHeight);
        LHDC := LGraphics.GetHDC();
        DrawEdge(LHDC, LRect, BDR_SUNKENOUTER, BF_RECT);
        LGraphics.ReleaseHDC(LHDC);
      end
      else
      begin
        LGraphics.FillRectangle(LBrush, 0, 0, LWidth, LHeight);
        LHDC := LGraphics.GetHDC();
        StyleServices.DrawElement(LHDC, StyleServices.GetElementDetails(tbPushButtonPressed), LRect);
        LGraphics.ReleaseHDC(LHDC);
      end;
      LGraphics.DrawImage(LIcon, GetSystemMetrics(SM_CXEDGE), GetSystemMetrics(SM_CYEDGE));
      FSyncEditButtonPressedBitmap := TGPCachedBitmap.Create(LBitmap, APaintVar^.Graphics);
      LGraphics.Free();
      LBitmap.Free();
      LIcon.Free();
    end;

    LBrush.Free();
    LPen.Free();
  end;

  procedure BuildOverlaysFromSyncEdit(); {$IFNDEF Debug} inline; {$ENDIF}
  var
    LCurrentId: Integer;
    LCurrentIndex: Integer;
    LIndex: Integer;
    LOverlay: TOverlay;
  begin
    FOverlays.Clear();
    if (FLines.SyncEdit) then
    begin
      LCurrentIndex := -1;
      LCurrentId := -1;
      for LIndex := 0 to FLines.SyncEditItems.Count - 1 do
        if (FLines.SyncEditItems[LIndex].Area.Contains(FLines.CaretPosition)) then
        begin
          LCurrentIndex := LIndex;
          LCurrentId := FLines.SyncEditItems[LIndex].Id;
          break;
        end;

      for LIndex := 0 to FLines.SyncEditItems.Count - 1 do
        if (LIndex <> LCurrentIndex) then
        begin
          LOverlay.Area := FLines.SyncEditItems[LIndex].Area;
          if (FLines.SyncEditItems[LIndex].Id = LCurrentId) then
            LOverlay.Style := osRect
          else
            LOverlay.Style := osUnderline;
          FOverlays.Add(LOverlay);
        end;
      Invalidate();
    end;
  end;

  procedure UpdateScrollBars(); {$IFNDEF Debug} inline; {$ENDIF}
  var
    LHorzScrollInfo: TScrollInfo;
    LVertScrollInfo: TScrollInfo;
  begin
    LVertScrollInfo.cbSize := SizeOf(ScrollInfo);
    LVertScrollInfo.fMask := SIF_RANGE or SIF_PAGE or SIF_POS;
    LVertScrollInfo.nMin := 0;
    LVertScrollInfo.nMax := Max(FRows.CaretPosition.Row, FRows.Count - 1);
    LVertScrollInfo.nPage := VisibleRows;
    LVertScrollInfo.nPos := TopRow;
    LVertScrollInfo.nTrackPos := 0;
    SetScrollInfo(Handle, SB_VERT, LVertScrollInfo, TRUE);

    LHorzScrollInfo.cbSize := SizeOf(ScrollInfo);
    LHorzScrollInfo.fMask := SIF_RANGE or SIF_PAGE or SIF_POS;
    LHorzScrollInfo.nMin := 0;
    if ((FRows.CaretPosition.Row < FRows.Count)
      and (FRows.CaretPosition.Column <= FRows.Items[FRows.CaretPosition.Row].Columns)) then
      LHorzScrollInfo.nMax := FRows.MaxWidth
    else
      LHorzScrollInfo.nMax := RowsToClient(FRows.CaretPosition).X;
    if (FSpecialChars.Visible) then
      Inc(LHorzScrollInfo.nMax, FLineBreakSignWidth);
    LHorzScrollInfo.nPage := FTextRect.Width;
    LHorzScrollInfo.nPos := HorzTextPos;
    LHorzScrollInfo.nTrackPos := 0;
    SetScrollInfo(Handle, SB_HORZ, LHorzScrollInfo, TRUE);

    ShowScrollBar(Handle, SB_VERT, not FHideScrollBars or (FScrollBars in [ssVertical, ssBoth]) and (LVertScrollInfo.nMax >= INT(LVertScrollInfo.nPage)));
    ShowScrollBar(Handle, SB_HORZ, not FHideScrollBars or (FScrollBars in [ssHorizontal, ssBoth]) and (LHorzScrollInfo.nMax >= INT(LHorzScrollInfo.nPage)));
    FScrollingEnabled := (soMiddleClickMove in FScroll.Options)
      and ((LVertScrollInfo.nMax >= INT(LVertScrollInfo.nPage))
        or (LHorzScrollInfo.nMax >= INT(LHorzScrollInfo.nPage)));

    FState := FState - [esScrolled];
  end;

  function EnumFontsFamiliesProc(var lpelf: TEnumLogFont; var lpntm: TNewTextMetric;
    FontType: Integer; lParam: LPARAM): Integer; stdcall;
  begin;
    Result := Integer(lpelf.elfLogFont.lfPitchAndFamily and FIXED_PITCH <> 0);
  end;

var
  LIndex: Integer;
  LInsertPos: TPoint;
  LPaintVar: TPaintVar;
  LScrollBarInfo: TScrollBarInfo;
  LTextWidth: Integer;
  LVisibleRows: Integer;
  LWidth: Integer;
begin
  LPaintVar.Graphics := TGPGraphics.Create(Canvas.Handle);
  LPaintVar.LeftMarginBorderBrush := TGPSolidBrush.Create(ColorRefToARGB(ColorToRGB(Color)));
  LPaintVar.LineForegroundColor := clNone;
  LPaintVar.LineBackgroundColor := clNone;
  LPaintVar.OverlayIndex := 0;
  LPaintVar.Parts := TList<TPaintVar.TPart>.Create();
  LPaintVar.PreviousBackgroundColor := clNone;
  LPaintVar.PreviousFontStyles := [];
  LPaintVar.SearchResultIndex := 0;
  LPaintVar.SelArea.BeginPosition := Min(FLines.SelArea.BeginPosition, FLines.SelArea.EndPosition);
  LPaintVar.SelArea.EndPosition := Max(FLines.SelArea.BeginPosition, FLines.SelArea.EndPosition);
  if (FUCCVisible) then
    if (FSpecialChars.Color <> clNone) then
      LPaintVar.UCCBrush := TGPSolidBrush.Create(ColorRefToARGB(ColorToRGB(FSpecialChars.Color)))
    else
      LPaintVar.UCCBrush := TGPSolidBrush.Create(ColorRefToARGB(ColorToRGB(clSpecialChar)));

  Include(FState, esPainting);
  try
    if ((FState * [esFontChanged] <> [])
      or ((FState * [esSizeChanged] <> []) and FWordWrap)) then
      InvalidateRows();

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
        ComputeMetrics();

        BuildBitmaps(@LPaintVar);
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
        LTextWidth := ClientWidth - FLeftMarginWidth;

        LScrollBarInfo.cbSize := SizeOf(LScrollBarInfo);
        GetScrollBarInfo(Handle, Integer(OBJID_HSCROLL), LScrollBarInfo);
        LVisibleRows := Max(1, ClientHeight div LineHeight);

        FillChar(FItalicOffsetCache, SizeOf(FItalicOffsetCache), 0);

        if ((LTextWidth <> FTextRect.Width) or (LVisibleRows <> VisibleRows)) then
        begin
          if (FWordWrap and (LTextWidth <> FTextRect.Width)) then
            InvalidateRows();

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

    if (esUpdated in FState) then
    begin
      SendMessage(FParentWnd, WM_COMMAND, EN_UPDATE shl 16 + FDlgCtrlID and $FFFF, LPARAM(Handle));
      Exclude(FState, esUpdated);
    end;

    if (FCurrentMatchingPair.FState = mpsClear) then
      ScanMatchingPair();

    if ((FRows.Count = 0) and (FLines.Count > 0)) then
      BuildRows(Canvas, -1 {TopRow + VisibleRows});

    if (esScrollToCaret in FState) then
    begin
      ScrollToCaret();
      Exclude(FState, esScrollToCaret);
    end;

    if (esSyncEditInvalid in FState) then
    begin
      BuildOverlaysFromSyncEdit();
      Exclude(FState, esSyncEditInvalid);
    end;

    if (FOverlays.Count > 0) then
    begin
      LPaintVar.OverlayRectBrush := TGPSolidBrush.Create(ColorRefToARGB(ColorToRGB(SyncEdit.Colors.WordBorder)));
      LPaintVar.OverlayUnderlineBrush := TGPSolidBrush.Create(ColorRefToARGB(ColorToRGB(SyncEdit.Colors.WordBorder)));
    end;

    Process(paPaint, @LPaintVar, mbLeft, [], -1, -1);

    if (not InvalidPoint(FInsertPos)) then
    begin
      LInsertPos := FInsertPos;
      FInsertPos := InvalidPos;
      SetInsertPos(LInsertPos);
    end;

    if (FState * [esCaretInvalid] <> []) then
    begin
      if (InvalidPoint(FCaretPos)
        and ((FLines.CaretPosition.Line >= FLines.Count)
          or (FLines.CaretPosition.Char >= Length(FLines.Items[FLines.CaretPosition.Line].Text)))
        and (FRows.CaretPosition.Row in [TopRow .. TopRow + VisibleRows + 1])) then
        FCaretPos := RowsToClient(FRows.CaretPosition);

      UpdateCaret();
    end;

    if (FState * [esScrolled] <> []) then
    begin
      UpdateScrollBars();
      Exclude(FState, esScrolled);
    end;

    FState := FState - [esFontChanged, esSysFontChanged, esSizeChanged, esHighlighterChanged];
  finally
    LPaintVar.Graphics.Free();
    LPaintVar.LeftMarginBorderBrush.Free();
    if (FOverlays.Count > 0) then
    begin
      LPaintVar.OverlayRectBrush.Free();
      LPaintVar.OverlayUnderlineBrush.Free();
    end;
    LPaintVar.Parts.Free();
    if (FUCCVisible) then
      LPaintVar.UCCBrush.Free();

    Exclude(FState, esPainting);
  end;

  FOldSelectionAvailable := not FLines.SelArea.IsEmpty();
end;

function TCustomBCEditor.ProcessToken(const AAction: TProcessAction;
  const APaintVar: PPaintVar;
  const AButton: TMouseButton; const AShift: TShiftState; const X, Y: Integer;
  var ARect: TRect;
  const ALinesPosition: TBCEditorLinesPosition;
  const ARowsPosition: TBCEditorRowsPosition;
  const AText: PChar; const ALength: Integer;
  const AToken: TBCEditorHighlighter.PFind = nil;
  const ARange: TBCEditorCodeFolding.TRanges.TRange = nil): Boolean;
var
  LEndPosition: TBCEditorLinesPosition;

  procedure AddPart(const APartBeginPosition, APartEndPosition: TBCEditorLinesPosition;
    const APartType: TPaintVar.TPart.TPartType);
  var
    LIndex: Integer;
    LPart: TPaintVar.TPart;
  begin
    LIndex := APaintVar^.Parts.Count - 1;
    while (LIndex >= 0) do
    begin
      if (APaintVar^.Parts.List[LIndex].BeginPosition = APartBeginPosition) then
      begin
        if (APaintVar^.Parts.List[LIndex].BeginPosition = APartBeginPosition) then
        begin
          if (APaintVar^.Parts.List[LIndex].EndPosition = APartEndPosition) then
            APaintVar^.Parts.List[LIndex].PartType := APartType
          else if (APaintVar^.Parts.List[LIndex].EndPosition > APartEndPosition) then
          begin
            APaintVar^.Parts.List[LIndex].BeginPosition := APartEndPosition;

            LPart.BeginPosition := APartBeginPosition;
            LPart.EndPosition := APartEndPosition;
            LPart.PartType := APartType;
            APaintVar^.Parts.Insert(LIndex, LPart);
          end
          else
          begin
            APaintVar^.Parts.List[LIndex].EndPosition := APartEndPosition;
            APaintVar^.Parts.List[LIndex].PartType := APartType;
            while ((LIndex < APaintVar^.Parts.Count) and (APaintVar^.Parts.List[LIndex].EndPosition < APartEndPosition)) do
              APaintVar^.Parts.Delete(LIndex);
            if (LIndex < APaintVar^.Parts.Count) then
              APaintVar^.Parts.List[LIndex].BeginPosition := APartEndPosition;
          end;
          exit;
        end
      end
      else if (APaintVar^.Parts.List[LIndex].BeginPosition < APartBeginPosition) then
      begin
        while ((LIndex >= 0) and (APaintVar^.Parts.List[LIndex].BeginPosition > APartBeginPosition)) do
        begin
          APaintVar^.Parts.Delete(LIndex);
          Dec(LIndex);
        end;
        if ((LIndex > 0) and (APaintVar^.Parts.List[LIndex - 1].EndPosition > APartBeginPosition)) then
          APaintVar^.Parts.List[LIndex - 1].EndPosition := APartBeginPosition;
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
    if ((APaintVar^.Parts.Count > 0) and (LIndex < APaintVar^.Parts.Count)) then
      APaintVar^.Parts.Insert(LIndex, LPart)
    else
      APaintVar^.Parts.Add(LPart);
  end;

  procedure ApplyPart(const AArea: TBCEditorLinesArea; APartType: TPaintVar.TPart.TPartType);
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
      if (LIndex = APaintVar^.Parts.Count) then
      begin
        AddPart(LPosition, LEndPosition, ptNormal);
        exit;
      end
      else if (LPosition < APaintVar^.Parts.List[LIndex].BeginPosition) then
      begin
        AddPart(LPosition, APaintVar^.Parts.List[LIndex].BeginPosition, ptNormal);
        Inc(LIndex);
        LPosition := LinesPosition(APaintVar^.Parts.List[LIndex].EndPosition.Char, APaintVar^.Parts.List[LIndex].EndPosition.Line);
      end
      else
      begin
        LPosition := LinesPosition(APaintVar^.Parts.List[LIndex].EndPosition.Char, APaintVar^.Parts.List[LIndex].EndPosition.Line);
        Inc(LIndex);
      end;
  end;

var
  LAddOnColor: TColor;
  LBackgroundColor: TColor;
  LBorderColor: TColor;
  LChar: Integer;
  LCollapsedMarkRect: TRect;
  LCursorPosition: TBCEditorLinesPosition;
  LEffect: Longint;
  LFontStyles: TFontStyles;
  LForegroundColor: TColor;
  LHint: string;
  LIsLineBreakToken: Boolean;
  LIsTabToken: Boolean;
  LIsUCCToken: Boolean;
  LLeft: Integer;
  LLength: Integer;
  LLine: Integer;
  LOptions: Longint;
  LOverlayBeginChar: Integer;
  LOverlayEndChar: Integer;
  LPartBackgroundColor: TColor;
  LPartForegroundColor: TColor;
  LPartIndex: Integer;
  LPartLength: Integer;
  LPartText: PChar;
  LRect: TRect;
  LRight: Integer;
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
  LIsUCCToken := Assigned(AText) and (ALength = 1) and AText^.IsInArray(BCEditor_UCCs);

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
        if (not LIsLineBreakToken or (ALinesPosition.Line < FLines.Count - 1)) then
        begin
          LEndPosition := LinesPosition(ALinesPosition.Char + LLength, ALinesPosition.Line);


          if (not Assigned(APaintVar)) then
            LForegroundColor := clNone
          else if (APaintVar^.LineForegroundColor <> clNone) then
            LForegroundColor := APaintVar^.LineForegroundColor
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

          if (not Assigned(APaintVar)) then
            LBackgroundColor := clNone
          else if (APaintVar^.LineBackgroundColor <> clNone) then
            LBackgroundColor := APaintVar^.LineBackgroundColor
          else if (ActiveLine.Visible
            and (ALinesPosition.Line = FLines.CaretPosition.Line)) then
            LBackgroundColor := ActiveLine.Color
          else if (LIsLineBreakToken) then
            LBackgroundColor := clWindow
          else if (Assigned(AToken) and Assigned(AToken^.Attribute) and (AToken^.Attribute.Background <> clNone)) then
            LBackgroundColor := AToken^.Attribute.Background
          else
            LBackgroundColor := clWindow;

          if (FLines.SyncEdit
            and (FLines.SyncEditArea.BeginPosition < FLines.SyncEditArea.EndPosition)) then
            ApplyPart(FLines.SyncEditArea, ptSyncEdit);

          if (Assigned(FSearchResults) and not FSearch.InSelection.Active
            and (APaintVar^.SearchResultIndex < FSearchResults.Count)) then
            repeat
              if ((ALinesPosition <= FSearchResults[APaintVar^.SearchResultIndex].BeginPosition)
                or (FSearchResults[APaintVar^.SearchResultIndex].EndPosition < LEndPosition)) then
                ApplyPart(FSearchResults[APaintVar^.SearchResultIndex], ptSearchResult);

              if (FSearchResults[APaintVar^.SearchResultIndex].EndPosition <= LEndPosition) then
                Inc(APaintVar^.SearchResultIndex)
              else
                break;
            until ((APaintVar^.SearchResultIndex = FSearchResults.Count)
              or (FSearchResults[APaintVar^.SearchResultIndex].BeginPosition > LEndPosition));

          if (FCurrentMatchingPair.FState = mpsFound) then
          begin
            ApplyPart(FCurrentMatchingPair.OpenTokenArea, ptMatchingPair);
            ApplyPart(FCurrentMatchingPair.CloseTokenArea, ptMatchingPair);
          end;

          if (not APaintVar^.SelArea.IsEmpty()) then
            ApplyPart(APaintVar^.SelArea, ptSelection);

          if (Assigned(FSearchResults) and FSearch.InSelection.Active
            and (APaintVar^.SearchResultIndex < FSearchResults.Count)) then
            repeat
              if ((ALinesPosition <= FSearchResults[APaintVar^.SearchResultIndex].BeginPosition)
                or (FSearchResults[APaintVar^.SearchResultIndex].EndPosition < LEndPosition)) then
                ApplyPart(FSearchResults[APaintVar^.SearchResultIndex], ptSearchResult);

              if (FSearchResults[APaintVar^.SearchResultIndex].EndPosition <= LEndPosition) then
                Inc(APaintVar^.SearchResultIndex)
              else
                break;
            until ((APaintVar^.SearchResultIndex = FSearchResults.Count)
              or (FSearchResults[APaintVar^.SearchResultIndex].BeginPosition > LEndPosition));

          if (APaintVar^.Parts.Count > 0) then
            CompleteParts();


          LBorderColor := clNone;
          LAddOnColor := clNone;
          LPartForegroundColor := LForegroundColor;
          LPartBackgroundColor := LBackgroundColor;

          LPartIndex := 0;
          repeat
            if (APaintVar^.Parts.Count = 0) then
            begin
              LPartText := LText;
              LPartLength := LLength;
            end
            else
            begin
              if (LPartIndex > 0) then
                LRect.Left := LRect.Right;

              LPartText := @LText[APaintVar^.Parts[LPartIndex].BeginPosition.Char - ALinesPosition.Char];
              LPartLength := APaintVar^.Parts[LPartIndex].EndPosition.Char - APaintVar^.Parts[LPartIndex].BeginPosition.Char;

              case (APaintVar^.Parts[LPartIndex].PartType) of
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
                else raise ERangeError.Create('PartType: ' + IntToStr(Ord(APaintVar^.Parts[LPartIndex].PartType)));
              end;

              if (LIsTabToken) then
                // Tab-Tokens have one part only - and they are computed before
              else if (LIsLineBreakToken) then
                // LineBreak-Tokens have one part only - and they are computed before
              else if (not Assigned(AText)) then
                // ... rest of the line
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
            begin
              if (LIsTabToken) then
                FPaintHelper.ExtTextOut(LLeft + (LRect.Width - FTabSignWidth) div 2, LRect.Top,
                  LOptions + ETO_OPAQUE, LRect, LPartText, LPartLength, nil)
              else if (LIsLineBreakToken or not Assigned(AText)) then
                FPaintHelper.ExtTextOut(LLeft, LRect.Top,
                  LOptions + ETO_OPAQUE, LRect, LPartText, LPartLength, nil)
              else if (not (fsItalic in LFontStyles)) then
                FPaintHelper.ExtTextOut(LLeft, LRect.Top,
                  LOptions + ETO_OPAQUE, LRect, LPartText, LPartLength, nil)
              else if (not (fsItalic in APaintVar^.PreviousFontStyles)
                or (LPartBackgroundColor <> APaintVar^.PreviousBackgroundColor)
                or (APaintVar^.PreviousBackgroundColor = clNone)) then
                FPaintHelper.ExtTextOut(LLeft, LRect.Top,
                  LOptions + ETO_OPAQUE, Rect(LRect.Left, LRect.Top, ARect.Right, LRect.Bottom), LPartText, LPartLength, nil)
              else
                FPaintHelper.ExtTextOut(LLeft, LRect.Top,
                  LOptions, LRect, LPartText, LPartLength, nil);

              if (FUCCVisible and APaintVar^.PreviousUCC) then
              begin
                APaintVar^.Graphics.FillRectangle(APaintVar^.UCCBrush, LRect.Left, LRect.Top, GLineWidth, FLineHeight);
                if (LIsTabToken) then
                  FPaintHelper.ExtTextOut(LLeft + (LRect.Width - FTabSignWidth) div 2, LRect.Top,
                    0, LRect, LPartText, 1, nil)
                else
                  FPaintHelper.ExtTextOut(LLeft, LRect.Top,
                    0, LRect, LPartText, 1, nil);
              end;

              APaintVar^.PreviousBackgroundColor := LPartBackgroundColor;
            end;

            Inc(LPartIndex);
          until ((APaintVar^.Parts.Count = 0) or (LPartIndex = APaintVar^.Parts.Count));

          APaintVar^.PreviousFontStyles := LFontStyles;
          APaintVar^.PreviousUCC := False;
          APaintVar^.Parts.Clear();

          if (Assigned(LText) and (LLength > 0) and not LIsUCCToken) then
            while ((APaintVar^.OverlayIndex < FOverlays.Count)
              and ((FOverlays[APaintVar^.OverlayIndex].Area.BeginPosition.Line < ALinesPosition.Line)
                or (FOverlays[APaintVar^.OverlayIndex].Area.BeginPosition.Line = ALinesPosition.Line)
                  and (FOverlays[APaintVar^.OverlayIndex].Area.BeginPosition.Char <= ALinesPosition.Char + LLength))) do
            begin
              if (ALinesPosition.Char < FOverlays[APaintVar^.OverlayIndex].Area.EndPosition.Char) then
              begin
                LOverlayBeginChar := Max(FOverlays[APaintVar^.OverlayIndex].Area.BeginPosition.Char, ALinesPosition.Char);
                LOverlayEndChar := Min(FOverlays[APaintVar^.OverlayIndex].Area.EndPosition.Char, ALinesPosition.Char + LLength);
                if ((ALinesPosition.Char <= LOverlayBeginChar) and (LOverlayEndChar <= ALinesPosition.Char + LLength)) then
                begin
                  LLeft := LRect.Left + FPaintHelper.TextWidth(LText, LOverlayBeginChar - ALinesPosition.Char);
                  LRight := LRect.Left + FPaintHelper.TextWidth(LText, LOverlayEndChar - ALinesPosition.Char);
                  case (FOverlays[APaintVar^.OverlayIndex].Style) of
                    osRect:
                      begin
                        if ((FOverlays[APaintVar^.OverlayIndex].Area.BeginPosition.Char >= ALinesPosition.Char)
                          and (LLeft >= LRect.Left)) then
                          APaintVar^.Graphics.FillRectangle(APaintVar^.OverlayRectBrush, LLeft, LRect.Top, GLineWidth, FLineHeight);
                        APaintVar^.Graphics.FillRectangle(APaintVar^.OverlayRectBrush, LLeft, LRect.Top, LRight - LLeft, GLineWidth);
                        APaintVar^.Graphics.FillRectangle(APaintVar^.OverlayRectBrush, LLeft, LRect.Bottom - GLineWidth, LRight - LLeft, GLineWidth);
                        if (FOverlays[APaintVar^.OverlayIndex].Area.EndPosition.Char <= ALinesPosition.Char + LLength) then
                          APaintVar^.Graphics.FillRectangle(APaintVar^.OverlayRectBrush, LRight - GLineWidth, LRect.Top, GLineWidth, FLineHeight);
                      end;
                    osUnderline:
                      begin
                        APaintVar^.Graphics.FillRectangle(APaintVar^.OverlayUnderlineBrush, LLeft, LRect.Bottom - 2 * GLineWidth, LRight - LLeft, GLineWidth);
                      end;
  //                  osWaveLine:
  //                    begin
  //                      LStep := 0;
  //                      while LStep < ARect.Right - 4 do
  //                      begin
  //                        Canvas.MoveTo(ARect.Left + LStep, ARect.Bottom - 3);
  //                        Canvas.LineTo(ARect.Left + LStep + 2, ARect.Bottom - 1);
  //                        Canvas.LineTo(ARect.Left + LStep + 4, ARect.Bottom - 3);
  //                        Inc(LStep, 4);
  //                      end;
  //                    end;
                  end;
                end;
              end;
              if (FOverlays[APaintVar^.OverlayIndex].Area.EndPosition.Char <= ALinesPosition.Char + LLength) then
                Inc(APaintVar^.OverlayIndex)
              else
                break;
            end;

          if (Assigned(ARange)
            and (LCollapsedMarkRect.Right >= ARect.Left)
            and (LCollapsedMarkRect.Left < ARect.Right)) then
          begin
            FPaintHelper.FrameRect(LCollapsedMarkRect, FCodeFolding.Colors.Foreground);
            FPaintHelper.ForegroundColor := FCodeFolding.Colors.Foreground;
            FPaintHelper.ExtTextOut(LCollapsedMarkRect.Left, LCollapsedMarkRect.Top,
              0, LCollapsedMarkRect, BCEDITOR_CODEFOLDING_COLLAPSEDMARK, Length(BCEDITOR_CODEFOLDING_COLLAPSEDMARK), nil);
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

          if (Assigned(APaintVar)) then
            APaintVar^.PreviousUCC := LIsUCCToken;
        end;
      paMouseDown,
      paMouseMove,
      paMouseUp,
      paHint:
        if ((LRect.Left <= X) and (X < LRect.Right)
          and (LRect.Top <= Y) and (Y < LRect.Bottom)
          and (MouseCapture in [mcNone, mcText])
          and (not LIsLineBreakToken or (ALinesPosition.Line < FLines.Count - 1))) then
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
                if (FLines.SelArea.Contains(LCursorPosition)) then
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
                    if ((MouseCapture = mcText)
                      and not (esMouseDouble in FState)) then
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
                if ((AButton = mbLeft)
                  and (esWaitForDrag in FState)) then
                begin
                  FLines.CaretPosition := LCursorPosition;
                  Exclude(FState, esWaitForDrag);
                end;
                if (FCodeFolding.Visible) then
                  CheckIfAtMatchingKeywords();
              end;
            paHint:
              if (LCollapsedMarkRect.Contains(Point(X, Y))) then
                ActivateHint(X, Y + FLineHeight,
                  Format(SBCEditorCodeFoldingCollapsedMark, [ARange.EndLine - ARange.BeginLine]))
              else if (LRect.Contains(Point(X, Y))) then
                if (Assigned(FOnHint)) then
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
                  ActivateHint(LRect.Left, LRect.Top + FLineHeight, Trim(LHint));
                {$ENDIF}
                end;
          end;
          Result := True;
        end;
      paMouseDouble:
        if ((LRect.Left <= X) and (X < LRect.Right)
          and (not LIsLineBreakToken or (ALinesPosition.Line < FLines.Count - 1))) then
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

  if (Assigned(ARange)) then
    ARect.Left := Max(ARect.Left, LCollapsedMarkRect.Right)
  else if (not LIsLineBreakToken) then
    ARect.Left := Max(ARect.Left, LRect.Right)
  else if (FSpecialChars.Visible) then
    ARect.Left := Max(ARect.Left, LRect.Left + FLineBreakSignWidth);
end;

procedure TCustomBCEditor.PasteFromClipboard();
var
  LClipboardData: Pointer;
  LGlobal: HGLOBAL;
  LOpened: Boolean;
  LRetry: Integer;
  LText: string;
begin
  LRetry := 0;
  repeat
    LOpened := OpenClipboard(Handle);
    if (not LOpened) then
    begin
      Sleep(50);
      Inc(LRetry);
    end;
  until (LOpened or (LRetry = 10));

  if (not LOpened) then
    raise EClipboardException.CreateFmt(SCannotOpenClipboard, [SysErrorMessage(GetLastError)])
  else
  begin
    try
      LGlobal := GetClipboardData(CF_UNICODETEXT);
      if (LGlobal <> 0) then
      begin
        LClipboardData := GlobalLock(LGlobal);
        if (Assigned(LClipboardData)) then
          LText := StrPas(PChar(LClipboardData));
        GlobalUnlock(LGlobal);
      end;
    finally
      CloseClipboard();
    end;

    FLines.BeginUpdate();
    try
      FLines.UndoGroupBreak();
      DoInsertText(LText);
    finally
      FLines.EndUpdate();
    end;
  end;
end;

function TCustomBCEditor.PosToCharIndex(const APos: TPoint): Integer;
begin
  Result := FLines.CharIndexOf(APos);
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
  const APaintVar: PPaintVar;
  const AButton: TMouseButton; const AShift: TShiftState; X, Y: Integer): Boolean;
var
  LSyncEditButtonRect: TRect;

  function ProcessMarks(var ARect: TRect; const ALine, ARow: Integer): Boolean; {$IFNDEF Debug} inline; {$ENDIF}
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
              APaintVar^.Graphics.DrawCachedBitmap(FBookmarkBitmaps[LBookmark.Index], LLeft, LRect.Top);

            LMark := nil;
            for LIndex := FMarkList.Count - 1 downto 0 do
              if (FMarkList[LIndex].Line = ALine) then
                LBookmark := FMarkList[LIndex];
            if (Assigned(LMark)) then
            begin
              if (Assigned(LBookmark)) then
                Inc(LLeft, GetSystemMetrics(SM_CXSMICON) div 4);

              if (Assigned(LMark)) then
                FLeftMargin.Marks.Images.Draw(Canvas, LLeft, LRect.Top, LMark.ImageIndex);
            end;
          end;
        end;
      paMouseDown:
        if ((AButton = mbLeft)
          and LRect.Contains(Point(X, Y)) and not LSyncEditButtonRect.Contains(Point(X, Y))
          and (MouseCapture in [mcNone, mcMarks])) then
        begin
          MouseCapture := mcMarks;
          Result := True;
        end;
      paMouseMove:
        if (LRect.Contains(Point(X, Y)) and not LSyncEditButtonRect.Contains(Point(X, Y))
          and (MouseCapture in [mcNone, mcMarks])) then
        begin
          MouseCapture := mcMarks;
          Cursor := crDefault;
          Result := True;
        end
        else if (MouseCapture = mcMarks) then
        begin
          if (not LRect.Contains(Point(X, Y)) or LSyncEditButtonRect.Contains(Point(X, Y))) then
            MouseCapture := mcNone;
          Result := True;
        end;
      paMouseUp:
        if ((AButton = mbLeft)
          and LRect.Contains(Point(X, Y)) and not LSyncEditButtonRect.Contains(Point(X, Y))
          and (MouseCapture in [mcNone, mcMarks])) then
        begin
          if ((ALine <> -1) and Assigned(FOnMarksPanelClick)) then
            FOnMarksPanelClick(Self, ALine);
          MouseCapture := mcNone;
          Result := True;
        end;
    end;

    ARect.Left := LRect.Right;
  end;

  function ProcessLineNumber(var ARect: TRect; const ALine, ARow: Integer): Boolean; {$IFNDEF Debug} inline; {$ENDIF}
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

          if ((ARow = 0) and (FLines.Count = 0)) then
          begin
            LText := IntToStr(FLeftMargin.LineNumbers.StartFrom);
            FPaintHelper.Style := [];
          end
          else if ((ALine < 0) and not (lnoAfterLastLine in FLeftMargin.LineNumbers.Options)
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
        if (LRect.Contains(Point(X, Y)) and not LSyncEditButtonRect.Contains(Point(X, Y))
          and (MouseCapture in [mcNone, mcLineNumbers])) then
        begin
          Cursor := crDefault;
          Result := True;
        end;
    end;

    ARect.Left := LRect.Right;
  end;

  function ProcessLineState(var ARect: TRect; const ALine, ARow: Integer): Boolean; {$IFNDEF Debug} inline; {$ENDIF}
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
                if (FLeftMargin.Colors.LineStateLoaded <> clNone) then
                  FPaintHelper.BackgroundColor := FLeftMargin.Colors.LineStateLoaded
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
        if (LRect.Contains(Point(X, Y)) and not LSyncEditButtonRect.Contains(Point(X, Y))
          and (MouseCapture in [mcNone, mcLineState])) then
        begin
          Cursor := crDefault;
          Result := True;
        end;
    end;

    ARect.Left := LRect.Right;
  end;

  function ProcessCodeFolding(var ARect: TRect; const ALine, ARow: Integer): Boolean; {$IFNDEF Debug} inline; {$ENDIF}
  var
    LRange: TBCEditorCodeFolding.TRanges.TRange;
    LRect: TRect;
  begin
    Result := False;

    LRect := ARect;
    LRect.Right := LRect.Left + FCodeFoldingWidth;

    if (AAction = paMouseMove) then
    begin
      if (LRect.Contains(Point(X, Y)) and not LSyncEditButtonRect.Contains(Point(X, Y))
        and (MouseCapture in [mcNone, mcCodeFolding])) then
      begin
        Cursor := crDefault;
        Result := True;
      end;
    end
    else
    begin
      if (AAction = paPaint) then
      begin
        if (FCodeFolding.Colors.Background <> clNone) then
          FPaintHelper.BackgroundColor := FCodeFolding.Colors.Background
        else
          FPaintHelper.BackgroundColor := Color;
        FPaintHelper.FillRect(LRect);
      end;

      if (ALine < 0) then
        LRange := nil
      else
        LRange := CodeFoldingCollapsableFoldRangeForLine(ALine);

      if ((ALine >= 0)
        and not Assigned(LRange) and (cfoShowTreeLine in FCodeFolding.Options)) then
      begin
        if (FLines.Items[ALine].CodeFolding.TreeLine) then
          case (AAction) of
            paPaint:
              APaintVar^.Graphics.DrawCachedBitmap(FCodeFoldingLineBitmap, LRect.Left, LRect.Top);
          end
        else if (Assigned(FLines.Items[ALine].CodeFolding.EndRange)) then
          case (AAction) of
            paPaint:
              APaintVar^.Graphics.DrawCachedBitmap(FCodeFoldingEndLineBitmap, LRect.Left, LRect.Top);
          end;
      end
      else if (Assigned(LRange) and LRange.Collapsable) then
      begin
        if (not LRange.Collapsed) then
          case (AAction) of
            paPaint:
              APaintVar^.Graphics.DrawCachedBitmap(FCodeFoldingCollapsedBitmap, LRect.Left, LRect.Top);
            paMouseDown:
              if ((AButton = mbLeft)
                and LRect.Contains(Point(X, Y)) and not LSyncEditButtonRect.Contains(Point(X, Y))
                and (MouseCapture in [mcNone, mcCodeFolding])) then
              begin
                CollapseCodeFoldingRange(LRange);
                Result := True;
              end;
          end
        else
          case (AAction) of
            paPaint:
              APaintVar^.Graphics.DrawCachedBitmap(FCodeFoldingExpandedBitmap, LRect.Left, LRect.Top);
            paMouseDown:
              if ((AButton = mbLeft)
                and LRect.Contains(Point(X, Y)) and not LSyncEditButtonRect.Contains(Point(X, Y))
                and (MouseCapture in [mcNone, mcCodeFolding])) then
              begin
                ExpandCodeFoldingRange(LRange);
                Result := True;
              end;
          end;
      end
      else
        case (AAction) of
          paPaint:
            APaintVar^.Graphics.DrawCachedBitmap(FCodeFoldingNoneBitmap, LRect.Left, LRect.Top);
        end
    end;

    ARect.Left := LRect.Right;
  end;

  function ProcessLeftMarginBorder(var ARect: TRect; const ALine, ARow: Integer): Boolean; {$IFNDEF Debug} inline; {$ENDIF}
  var
    LRect: TRect;
  begin
    Result := False;

    LRect := ARect;
    LRect.Right := LRect.Left + FLeftMarginBorderWidth;

    case (AAction) of
      paPaint:
        APaintVar^.Graphics.FillRectangle(APaintVar^.LeftMarginBorderBrush, LRect.Left, LRect.Top, LRect.Width, LRect.Height);
      paMouseDown,
      paMouseDouble,
      paMouseTriple,
      paMouseUp,
      paHint:
        if (LRect.Contains(Point(X, Y))) then
          X := LRect.Right;
      paMouseMove:
        if (LRect.Contains(Point(X, Y))) then
          Cursor := crDefault;
    end;

    ARect.Left := LRect.Right;
  end;

  function ProcessSyncEditButton(): Boolean; {$IFNDEF Debug} inline; {$ENDIF}
  var
    LGraphics: TGPGraphics;
    LRow: Integer;
  begin
    Result := False;

    if (not FLines.SyncEdit and FSyncEditAvailable and not FLines.SelArea.IsEmpty()) then
      LRow := LinesToRows(SelectionEndPosition).Row
    else if (FLines.SyncEdit) then
    begin
      LRow := LinesToRows(FLines.SyncEditArea.BeginPosition).Row;
      LRow := Max(LRow, TopRow);
      LRow := Min(LRow, TopRow + VisibleRows);
    end
    else
      LRow := -1;

    if (LRow = -1) then
      LSyncEditButtonRect := InvalidRect
    else
    begin
      LSyncEditButtonRect.Left := 2 * GetSystemMetrics(SM_CXEDGE);
      LSyncEditButtonRect.Top := (LRow - TopRow) * LineHeight;
      LSyncEditButtonRect.Right := LSyncEditButtonRect.Left + GetSystemMetrics(SM_CXSMICON);
      LSyncEditButtonRect.Bottom := LSyncEditButtonRect.Top +  GetSystemMetrics(SM_CYSMICON);

      case (AAction) of
        paPaint:
          if (not FLines.SyncEdit) then
            APaintVar^.Graphics.DrawCachedBitmap(FSyncEditButtonNormalBitmap, LSyncEditButtonRect.Left, LSyncEditButtonRect.Top)
          else
            APaintVar^.Graphics.DrawCachedBitmap(FSyncEditButtonPressedBitmap, LSyncEditButtonRect.Left, LSyncEditButtonRect.Top);
        paMouseDown:
          if ((AButton = mbLeft)
            and LSyncEditButtonRect.Contains(Point(X, Y))) then
          begin
            LGraphics := TGPGraphics.Create(Canvas.Handle);
            if (not FLines.SyncEdit) then
              LGraphics.DrawCachedBitmap(FSyncEditButtonPressedBitmap, LSyncEditButtonRect.Left, LSyncEditButtonRect.Top)
            else
              LGraphics.DrawCachedBitmap(FSyncEditButtonPressedBitmap, LSyncEditButtonRect.Left, LSyncEditButtonRect.Top);
            LGraphics.Free();
            MouseCapture := mcSyncEditButton;
            Result := True;
          end;
        paMouseMove:
          if (LSyncEditButtonRect.Contains(Point(X, Y))) then
          begin
            if (MouseCapture <> mcSyncEditButton) then
            begin
              LGraphics := TGPGraphics.Create(Canvas.Handle);
              if (not FLines.SyncEdit) then
                LGraphics.DrawCachedBitmap(FSyncEditButtonHotBitmap, LSyncEditButtonRect.Left, LSyncEditButtonRect.Top)
              else
                LGraphics.DrawCachedBitmap(FSyncEditButtonPressedBitmap, LSyncEditButtonRect.Left, LSyncEditButtonRect.Top);
              LGraphics.Free();
              MouseCapture := mcSyncEditButton;
            end;
          end
          else if (MouseCapture = mcSyncEditButton) then
          begin
            LGraphics := TGPGraphics.Create(Canvas.Handle);
            if (not FLines.SyncEdit) then
              LGraphics.DrawCachedBitmap(FSyncEditButtonNormalBitmap, LSyncEditButtonRect.Left, LSyncEditButtonRect.Top)
            else
              LGraphics.DrawCachedBitmap(FSyncEditButtonPressedBitmap, LSyncEditButtonRect.Left, LSyncEditButtonRect.Top);
            LGraphics.Free();
            if (not LSyncEditButtonRect.Contains(Point(X, Y))) then
              MouseCapture := mcNone;
          end;
        paMouseUp:
          if ((AButton = mbLeft)
            and LSyncEditButtonRect.Contains(Point(X, Y))) then
          begin
            if (not FLines.SyncEdit) then
              FLines.ActivateSyncEdit(FHighlighter)
            else
              FLines.DeactivateSyncEdit();
            MouseCapture := mcNone;
            Result := True;
          end;
      end;
    end;
  end;

  procedure ProcessScroll(); {$IFNDEF Debug} inline; {$ENDIF}
  var
    LLinesPosition: TBCEditorLinesPosition;
  begin
    LLinesPosition := ClientToLines(FCursorPoint.X, FCursorPoint.Y);
    if (LLinesPosition <> FLines.CaretPosition) then
      MoveCaretAndSelection(FLines.SelArea.BeginPosition, LLinesPosition, True);
  end;

  function ProcessScrolling(): Boolean; {$IFNDEF Debug} inline; {$ENDIF}
  var
    LGraphics: TGPGraphics;
  begin
    Result := False;

    case (AAction) of
      paPaint:
        if (esScrolling in FState) then
          APaintVar^.Graphics.DrawCachedBitmap(FScrollingBitmap, FScrollingRect.Left, FScrollingRect.Top);
      paMouseDown:
        if (esScrolling in FState) then
        begin
          Exclude(FState, esScrolling);
          MouseCapture := mcNone;
          Invalidate();
          Result := True;
        end
        else if ((AButton = mbMiddle)
          and Rect(FLeftMarginWidth, 0, ClientWidth, ClientHeight).Contains(Point(X, Y))) then
        begin
          FScrollingPoint.X := X;
          FScrollingPoint.Y := Y;
          FScrollingRect.Left := FScrollingPoint.X - FScrollingBitmapWidth div 2;
          FScrollingRect.Top := FScrollingPoint.Y - FScrollingBitmapHeight div 2;
          FScrollingRect.Right := FScrollingPoint.X + FScrollingBitmapWidth div 2;
          FScrollingRect.Bottom := FScrollingPoint.Y + FScrollingBitmapHeight div 2;
          LGraphics := TGPGraphics.Create(Canvas.Handle);
          LGraphics.DrawCachedBitmap(FScrollingBitmap, FScrollingRect.Left, FScrollingRect.Top);
          LGraphics.Free();
          Cursor := crSizeAll;
          Include(FState, esScrolling);
          MouseCapture := mcScrolling;
          SetTimer(Handle, tiScrolling, 100, nil);
          Result := True;
        end;
      paScrolling:
        if (MouseCapture = mcScrolling) then
        begin
          if (FCursorPoint.Y < FScrollingPoint.Y) then
            TopRow := TopRow + Min(0, FCursorPoint.Y - FScrollingPoint.Y - GetSystemMetrics(SM_CXEDGE)) div FLineHeight
          else
            TopRow := TopRow + Max(0, FCursorPoint.Y - FScrollingPoint.Y - GetSystemMetrics(SM_CXEDGE)) div FLineHeight;
          if (FCursorPoint.X < FScrollingPoint.X) then
            HorzTextPos := HorzTextPos + Min(0, FCursorPoint.X - FScrollingPoint.X - GetSystemMetrics(SM_CXEDGE))
          else
            HorzTextPos := HorzTextPos + Max(0, FCursorPoint.X - FScrollingPoint.X - GetSystemMetrics(SM_CXEDGE));
        end;
    end;
  end;

var
  LCodeFoldingRange: TBCEditorCodeFolding.TRanges.TRange;
  LColumn: Integer;
  LLine: Integer;
  LRect: TRect;
  LRow: Integer;
  LToken: TBCEditorHighlighter.TFind;
begin
  Assert(FLineHeight > 0);

  Result := False;

  FPaintHelper.BeginDrawing(Canvas);
  try
    if ((AAction <> paPaint)
      and FSyncEdit.Enabled
      and (seoButton in FSyncEdit.Options)) then
      Result := Result or ProcessSyncEditButton()
    else
      LSyncEditButtonRect := InvalidRect;

    if ((AAction <> paPaint)
      and FScrollingEnabled) then
      Result := Result or ProcessScrolling();

    for LRow := TopRow to TopRow + VisibleRows + 1 do
      if ((AAction = paPaint)
        or (Y div FLineHeight = LRow - TopRow)) then
      begin
        if (LRow < FRows.Count) then
          LLine := FRows.Items[LRow].Line
        else
          LLine := -1;

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

        if (FLeftMarginWidth > 0) then
          Result := Result or ProcessLeftMarginBorder(LRect, LLine, LRow);

        Dec(LRect.Left, FHorzTextPos);

        if (not Result) then
          if (LRow >= FRows.Count) then
            Result := ProcessToken(AAction, APaintVar, AButton, AShift, X, Y, LRect,
              FRows.BORPosition[LRow], RowsPosition(0, LRow),
              nil, 0)
          else if (AAction = paMouseTriple) then
          begin
            if ((AButton = mbLeft)
              and (soTripleClickLineSelect in FSelection.Options)) then
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
                APaintVar^.LineForegroundColor := clNone
              else
                APaintVar^.LineForegroundColor := FLines.Items[LLine].Foreground;
              if ((LLine >= FLines.Count) or (FLines.Items[LLine].Background = clNone)) then
                APaintVar^.LineBackgroundColor := clNone
              else
                APaintVar^.LineBackgroundColor := FLines.Items[LLine].Background;
            end;

            LColumn := 0;

            if (FHighlighter.FindFirstToken(FRows.Items[LRow].BeginRange, FRows[LRow], LToken)) then
            begin
              if (Assigned(APaintVar)) then
              begin
                APaintVar^.PreviousFontStyles := [];
                APaintVar^.PreviousBackgroundColor := clNone;
                APaintVar^.PreviousUCC := False;
              end;

              repeat
                Result := Result or ProcessToken(AAction, APaintVar, AButton, AShift, X, Y, LRect,
                  LinesPosition(FRows.Items[LRow].Char + LToken.Char, LLine),
                  RowsPosition(LColumn, LRow),
                  LToken.Text, LToken.Length,
                  @LToken);

                if (LToken.Text^ <> BCEDITOR_TAB_CHAR) then
                  Inc(LColumn, LToken.Length)
                else
                  LColumn := FTabs.Width - LColumn mod FTabs.Width;
              until ((LRect.Left > ClientWidth)
                or not FHighlighter.FindNextToken(LToken));
            end;

            if (LRect.Left <= ClientWidth) then
            begin
              if (not FCodeFolding.Visible
                or not (rfLastRowOfLine in FRows.Items[LRow].Flags)) then
                LCodeFoldingRange := nil
              else
              begin
                LCodeFoldingRange := CodeFoldingCollapsableFoldRangeForLine(LLine);
                if (Assigned(LCodeFoldingRange) and (not LCodeFoldingRange.Collapsed or LCodeFoldingRange.ParentCollapsed)) then
                  LCodeFoldingRange := nil;
              end;
              Result := Result or ProcessToken(AAction, APaintVar, AButton, AShift, X, Y, LRect,
                FRows.EORPosition[LRow], RowsPosition(FRows.Items[LRow].Length, LRow),
                nil, 0, nil, LCodeFoldingRange);
            end;
          end;

        LRect.Left := 0;
        LRect.Right := Width;
      end;

    if ((AAction = paMouseMove)
      and not Result
      and (MouseCapture = mcText)) then
    begin
      ProcessScroll();
      SetTimer(Handle, tiScroll, 100, nil);
    end;

    if ((AAction = paPaint)
      and FSyncEdit.Enabled
      and (seoButton in FSyncEdit.Options)) then
      Result := Result or ProcessSyncEditButton();

    if ((AAction = paPaint)
      and FScrollingEnabled) then
      Result := ProcessScrolling() or Result;

  finally
    FPaintHelper.EndDrawing();
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
  if (not FLines.SelArea.IsEmpty() and (roSelectedOnly in Replace.Options)) then
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

  FState := FState + [esSizeChanged];
  InvalidateScrollBars();
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
  Assert((ARowsPosition.Column >= 0) and (ARowsPosition.Row >= 0));

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
  if (not (soBeyondEndOfLine in FScroll.Options)) then
    FLines.Options := FLines.Options - [loBeyondEndOfLine]
  else
    FLines.Options := FLines.Options + [loBeyondEndOfLine];
  if (not (soBeyondEndOfFile in FScroll.Options)) then
    FLines.Options := FLines.Options - [loBeyondEndOfFile]
  else
    FLines.Options := FLines.Options + [loBeyondEndOfFile];
  InvalidateScrollBars();
  Invalidate();
end;

procedure TCustomBCEditor.ScrollToCaret();
var
  LClient: TPoint;
begin
  if (FRows.Count = 0) then
  begin
    Include(FState, esScrollToCaret);
    Invalidate();
  end
  else
  begin
    if (GetWindowLong(Handle, GWL_STYLE) and ES_AUTOHSCROLL <> 0) then
    begin
      LClient := RowsToClient(FRows.CaretPosition);
      if (LClient.X - FLeftMarginWidth < 0) then
        HorzTextPos := LClient.X - FLeftMarginWidth + FHorzTextPos
      else if (LClient.X - FLeftMarginWidth + FSpaceWidth + 1 > FTextRect.Width) then
        HorzTextPos := LClient.X - FLeftMarginWidth + FHorzTextPos - FTextRect.Width + FSpaceWidth + 1;
    end;

    if (GetWindowLong(Handle, GWL_STYLE) and ES_AUTOVSCROLL <> 0) then
    begin
      if (FRows.CaretPosition.Row < FTopRow) then
        TopRow := FRows.CaretPosition.Row
      else if (FRows.CaretPosition.Row + 1 >= FTopRow + FVisibleRows) then
        TopRow := FRows.CaretPosition.Row - FVisibleRows + 1;
    end;
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

procedure TCustomBCEditor.SelectAll();
begin
  FLines.SelArea := FLines.Area;
end;

function TCustomBCEditor.SelectedText(): string;
begin
  Result := SelText;
end;

function TCustomBCEditor.SelectionAvailable: Boolean;
begin
  Result := not FLines.SelArea.IsEmpty();
end;

procedure TCustomBCEditor.SelectionChanged(ASender: TObject);
begin
  FSyncEditAvailable := FSyncEdit.Enabled
    and not FLines.SyncEdit
    and not FLines.SelArea.IsEmpty()
    and FLines.ScanSyncEdit(FHighlighter, True);

  if (FOldSelectionAvailable or not FLines.SelArea.IsEmpty()) then
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
    Invalidate();
  end;
end;

procedure TCustomBCEditor.SetBorderStyle(const AValue: TBorderStyle);
begin
  if (AValue <> FBorderStyle) then
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

procedure TCustomBCEditor.SetCaretPos(const AValue: TPoint);
begin
  FLines.CaretPosition := AValue;
end;

procedure TCustomBCEditor.SetCodeFolding(const AValue: TBCEditorCodeFolding);
begin
  FCodeFolding.Assign(AValue);
  if AValue.Visible then
    InitCodeFolding;
end;

procedure TCustomBCEditor.SetDefaultKeyCommands;
begin
  FKeyCommands.ResetDefaults;
end;

procedure TCustomBCEditor.SetFocus();
begin
  // Todo: Implement EIMES_CANCELCOMPSTRINGFOCUS and EIMES_COMPLETECOMPSTRKILLFOCUS

  inherited;
end;

procedure TCustomBCEditor.SetHideScrollBars(AValue: Boolean);
begin
  if (AValue <> FHideScrollBars) then
  begin
    FHideScrollBars := AValue;
    InvalidateScrollBars();
  end;
end;

procedure TCustomBCEditor.SetHideSelection(AValue: Boolean);
begin
  if (AValue <> FHideSelection) then
  begin
    FHideSelection := HideSelection;
    if (HandleAllocated) then
      if (not AValue) then
        SetWindowLong(Handle, GWL_STYLE, GetWindowLong(Handle, GWL_STYLE) or ES_NOHIDESEL)
      else
        SetWindowLong(Handle, GWL_STYLE, GetWindowLong(Handle, GWL_STYLE) and not ES_NOHIDESEL);
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
var
  LValue: Integer;
begin
  LValue := AValue;
  if (not (soBeyondEndOfLine in FScroll.Options)) then
    LValue := Min(AValue, FRows.MaxWidth - FTextRect.Width);
  LValue := Max(0, LValue);

  if (LValue <> FHorzTextPos) then
  begin
    if (Assigned(FHintWindow)) then
      FreeAndNil(FHintWindow);

    FHorzTextPos := LValue;

    SendMessage(FParentWnd, WM_COMMAND, EN_HSCROLL shl 16 + FDlgCtrlID and $FFFF, LPARAM(Handle));

    InvalidateCaret();
    InvalidateScrollBars();

    if (not (esPainting in FState)) then
      Invalidate();
  end;
end;

procedure TCustomBCEditor.SetInsertPos(AValue: TPoint);
var
  LClient: TPoint;
  LGraphics: TGPGraphics;
begin
  if (AValue <> FInsertPos) then
  begin
    if (HandleAllocated and Assigned(FInsertPosCache)) then
    begin
      if (not InvalidPoint(FInsertPos)) then
      begin
        LClient := RowsToClient(LinesToRows(FInsertPos));
        if (FInsertPos.X >= 0) then
          BitBlt(Canvas.Handle, LClient.X - GLineWidth, LClient.Y, 3 * GLineWidth, LineHeight,
            FInsertPosCache.Canvas.Handle, 0, 0,
            SRCCOPY);
      end;
      FInsertPosCache.Free();
      FInsertPosCache := nil;
    end;

    if (AValue.Y < 0) then
      FInsertPos := InvalidPos
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
        and (not InvalidPoint(FInsertPos))
        and (LinesToRows(FInsertPos).Row in [TopRow .. TopRow + VisibleRows + 1])) then
      begin
        LClient := RowsToClient(LinesToRows(FInsertPos));
        FInsertPosCache := TBitmap.Create();
        FInsertPosCache.Handle := CreateCompatibleBitmap(Canvas.Handle, 3 * GLineWidth, LineHeight);

        BitBlt(FInsertPosCache.Canvas.Handle, 0, 0, 3 * GLineWidth, LineHeight,
          Canvas.Handle, LClient.X - GLineWidth, LClient.Y,
          SRCCOPY);
        LGraphics := TGPGraphics.Create(Canvas.Handle);
        LGraphics.DrawCachedBitmap(FInsertPosBitmap, LClient.X - GLineWidth, LClient.Y);
        LGraphics.Free();
      end;
    end;
  end;
end;

procedure TCustomBCEditor.SetLineColor(const ALine: Integer; const AForegroundColor, ABackgroundColor: TColor);
begin
  if ((0 <= ALine) and (ALine < FLines.Count)
    and ((AForegroundColor <> FLines.Items[ALine].Foreground) or (ABackgroundColor <> FLines.Items[ALine].Background))) then
  begin
    FLines.SetForeground(ALine, AForegroundColor);
    FLines.SetBackground(ALine, ABackgroundColor);
    Invalidate();
  end;
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

procedure TCustomBCEditor.SetOption(const AOption: TBCEditorOption; const AEnabled: Boolean);
begin
  if (AEnabled) then
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

    Invalidate();
  end;
end;

procedure TCustomBCEditor.SetReadOnly(const AValue: Boolean);
begin
  if (AValue <> FReadOnly) then
  begin
    FReadOnly := AValue;
    if (not FReadOnly) then
      FLines.Options := FLines.Options - [loReadOnly]
    else
      FLines.Options := FLines.Options + [loReadOnly];
    if (HandleAllocated) then
      if (not FReadOnly) then
        SetWindowLong(Handle, GWL_STYLE, GetWindowLong(Handle, GWL_STYLE) and not ES_READONLY)
      else
        SetWindowLong(Handle, GWL_STYLE, GetWindowLong(Handle, GWL_STYLE) or ES_READONLY);
  end;
end;

procedure TCustomBCEditor.SetScroll(const AValue: TBCEditorScroll);
begin
  FScroll.Assign(AValue);
end;

procedure TCustomBCEditor.SetScrollBars(const AValue: UITypes.TScrollStyle);
begin
  if (AValue <> FScrollBars) then
  begin
    FScrollBars := AValue;
    InvalidateScrollBars();
  end;
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

procedure TCustomBCEditor.SetSelLength(AValue: Integer);
begin
  FLines.SelArea := LinesArea(FLines.SelArea.BeginPosition, FLines.PositionOf(AValue, Min(FLines.SelArea.BeginPosition, FLines.SelArea.EndPosition)));
end;

procedure TCustomBCEditor.SetSelStart(AValue: Integer);
begin
  FLines.CaretPosition := FLines.PositionOf(AValue);
end;

procedure TCustomBCEditor.SetSelText(const AValue: string);
var
  LArea: TBCEditorLinesArea;
begin
  if (not ReadOnly) then
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
  if (not (soBeyondEndOfFile in FScroll.Options)) then
    LValue := Min(AValue, FRows.Count - VisibleRows + 1);
  LValue := Max(0, LValue);

  if (LValue <> FTopRow) then
  begin
    if (Assigned(FHintWindow)) then
      FreeAndNil(FHintWindow);

    FTopRow := LValue;

    SendMessage(FParentWnd, WM_COMMAND, EN_VSCROLL shl 16 + FDlgCtrlID and $FFFF, LPARAM(Handle));

    InvalidateCaret();
    InvalidateScrollBars();

    if (not (esPainting in FState)) then
      Invalidate();
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
    if (HandleAllocated and Visible and not (esHandlingMouse in FState)) then
      SendMessage(Handle, WM_SETREDRAW, WPARAM(FALSE), 0);
  end
  else
  begin
    Assert(FLines.UndoList.UpdateCount = 0);

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

    if ((FState * [esLinesCleared, esLinesDeleted, esLinesInserted, esLinesUpdated] <> [])
      and not (csReading in ComponentState)) then
      Change();
    FState := FState - [esLinesCleared, esLinesDeleted, esLinesInserted, esLinesUpdated];
  end;
end;

procedure TCustomBCEditor.SetWantReturns(const AValue: Boolean);
begin
  if (AValue <> FWantReturns) then
  begin
    FWantReturns := AValue;
    if (HandleAllocated) then
      if (not AValue) then
        SetWindowLong(Handle, GWL_STYLE, GetWindowLong(Handle, GWL_STYLE) and not ES_WANTRETURN)
      else
        SetWindowLong(Handle, GWL_STYLE, GetWindowLong(Handle, GWL_STYLE) or ES_WANTRETURN);
  end;
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

procedure TCustomBCEditor.SetWordWrap(const AValue: Boolean);
var
  LOldCaretInView: Boolean;
  LOldTopRow: Integer;
begin
  if (AValue <> FWordWrap) then
  begin
    FWordWrap := AValue;

    if (HandleAllocated) then
    begin
      LOldCaretInView := ClientRect.Contains(FCaretPos);
      LOldTopRow := TopRow;
      if (AValue) then
        HorzTextPos := 0;
      TopRow := LOldTopRow;

      InvalidateRows();
      if (LOldCaretInView) then
        ScrollToCaret();

      InvalidateCaret();
      InvalidateScrollBars();
      Invalidate();
    end;
  end;
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

  if (not FLines.SelArea.IsEmpty()) then
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
  InvalidateScrollBars();
  Invalidate();
end;

function TCustomBCEditor.SplitTextIntoWords(AStringList: TStrings; const ACaseSensitive: Boolean): string;
var
  LSkipCloseKeyChars: TBCEditorAnsiCharSet;
  LSkipOpenKeyChars: TBCEditorAnsiCharSet;
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
begin
  if (seoCaseSensitive in FSyncEdit.Options) then
    FLines.Options := FLines.Options + [loSyncEditCaseSensitive]
  else
    FLines.Options := FLines.Options - [loSyncEditCaseSensitive];

  if (FLines.SyncEdit) then
  begin
    FLines.SelArea := FLines.SyncEditArea;
    FLines.ActivateSyncEdit(FHighlighter);
  end;
end;

procedure TCustomBCEditor.TabsChanged(ASender: TObject);
begin
  if (FWordWrap) then
    InvalidateRows();
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
  ProcessToken(paNone, nil, mbLeft, [], -1, -1, LRect,
    InvalidLinesPosition, RowsPosition(AColumn, -1),
    AText, ALength, @AToken);
  Result := LRect.Left;
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

procedure TCustomBCEditor.UMIdle(var AMessage: TMessage);
var
  LMsg: TMsg;
begin
  while (FPendingJobs <> []) do
    if (PeekMessage(LMsg, FFormWnd, 0, 0, PM_NOREMOVE)) then
    begin
      SetTimer(Handle, tiIdle, 10, nil);
      break;
    end
    else
      Idle();
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

  LOldWrap := FWordWrap;
  WordWrap := False;

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

  WordWrap := LOldWrap;
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
      TEditCut(Action).Enabled := not ReadOnly and not FLines.SelArea.IsEmpty()
    else if Action is TEditCopy then
      TEditCopy(Action).Enabled := not FLines.SelArea.IsEmpty()
    else if Action is TEditPaste then
      TEditPaste(Action).Enabled := Focused() and CanPaste
    else if Action is TEditDelete then
      TEditDelete(Action).Enabled := not ReadOnly and not FLines.SelArea.IsEmpty()
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
  LCompForm: TCompositionForm;
  LRect: TRect;
  LImc: HIMC;
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

      if (GImmEnabled) then
      begin
        LCompForm.dwStyle := CFS_POINT;
        LCompForm.ptCurrentPos := FCaretPos;
        LImc := ImmGetContext(Handle);
        ImmSetCompositionWindow(LImc, @LCompForm);
        ImmReleaseContext(Handle, LImc);
      end;

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

procedure TCustomBCEditor.WMChar(var AMessage: TWMChar);
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
  begin
    if not (esIgnoreNextChar in FState) then
    begin
      FKeyboardHandler.ExecuteKeyPress(Self, LKey);
      CommandProcessor(ecChar, LKey, nil);
    end
    else
      Exclude(FState, esIgnoreNextChar);
  end;
end;

procedure TCustomBCEditor.WMClear(var AMessage: TMessage);
begin
  SelText := '';
end;

procedure TCustomBCEditor.WMCommand(var AMessage: TWMCommand);
begin
  if ((AMessage.NotifyCode = 0) and (AMessage.Ctl = 0)) then
    case (AMessage.ItemID) of
      WM_UNDO,
      WM_CUT,
      WM_COPY,
      WM_PASTE,
      WM_CLEAR: Perform(AMessage.ItemID, 0, 0);
      EM_SETSEL: Perform(AMessage.ItemID, 0, -1);
      WM_APP + 0: { Right to left Reading order }
        MessageBox(Handle, 'Does it make sense to heave a right to left order this editor?' + #10
          + 'If yes, please explaint it to the developer of the BCEditor at: https://github.com/bonecode/BCEditor/',
          'Help wanted',
          MB_OK);
      WM_APP + 1: { Show Unicode control characters }
        begin
          FUCCVisible := not FUCCVisible;
          Invalidate();
        end;
      WM_APP + 2: Perform(WM_CHAR, Ord(BCEditor_UCC_ZWJ), 0); { ZWJ Unicode control character }
      WM_APP + 3: Perform(WM_CHAR, Ord(BCEditor_UCC_ZWNJ), 0); { ZWNJ Unicode control character }
      WM_APP + 4: Perform(WM_CHAR, Ord(BCEditor_UCC_LRM), 0); { LRM Unicode control character }
      WM_APP + 5: Perform(WM_CHAR, Ord(BCEditor_UCC_RLM), 0); { RLM Unicode control character }
      WM_APP + 6: Perform(WM_CHAR, Ord(BCEditor_UCC_LRE), 0); { LRE Unicode control character }
      WM_APP + 7: Perform(WM_CHAR, Ord(BCEditor_UCC_RLE), 0); { RLE Unicode control character }
      WM_APP + 8: Perform(WM_CHAR, Ord(BCEditor_UCC_LRO), 0); { LRO Unicode control character }
      WM_APP + 9: Perform(WM_CHAR, Ord(BCEditor_UCC_RLO), 0); { RLO Unicode control character }
      WM_APP + 10: Perform(WM_CHAR, Ord(BCEditor_UCC_PDF), 0); { PDF Unicode control character }
      WM_APP + 11: Perform(WM_CHAR, Ord(BCEditor_UCC_NADS), 0); { NADS Unicode control character }
      WM_APP + 12: Perform(WM_CHAR, Ord(BCEditor_UCC_NODS), 0); { NODS Unicode control character }
      WM_APP + 13: Perform(WM_CHAR, Ord(BCEditor_UCC_ASS), 0); { ASS Unicode control character }
      WM_APP + 14: Perform(WM_CHAR, Ord(BCEditor_UCC_ISS), 0); { ISS Unicode control character }
      WM_APP + 15: Perform(WM_CHAR, Ord(BCEditor_UCC_AAFS), 0); { AAFS Unicode control character }
      WM_APP + 16: Perform(WM_CHAR, Ord(BCEditor_UCC_IAFS), 0); { IAFS Unicode control character }
      WM_APP + 17: Perform(WM_CHAR, Ord(BCEditor_UCC_RS), 0); { RS Unicode control character }
      WM_APP + 18: Perform(WM_CHAR, Ord(BCEditor_UCC_US), 0); { US Unicode control character }
      WM_APP + 20: { Open IME }
        MessageBox(Handle, 'The developer of the BCEditor don''t know, how to implement this feature.' + #10
          + 'If you know it, please contact him at: https://github.com/bonecode/BCEditor/',
          'Help wanted',
          MB_OK);
      WM_APP + 21: ; { Reconversion }
      else
        inherited;
    end
  else
    inherited;
end;

procedure TCustomBCEditor.WMContextMenu(var AMessage: TWMContextMenu);
var
  LBuffer: array [0 .. 100] of Char;
  LClient: TPoint;
  LIndex: Integer;
  LIndex2: Integer;
  LInstance: THandle;
  LLen: Integer;
  LMenu: HMENU;
  LMenuItemInfo: MENUITEMINFO;
  LNewCaretPosition: TBCEditorLinesPosition;
begin
  LClient := ScreenToClient(Point(AMessage.XPos, AMessage.YPos));

  if (FLeftMargin.MarksPanel.Visible
    and (LClient.X <= FMarksPanelWidth)
    and Assigned(FMarksPanelPopupMenu)) then
  begin
    FMarksPanelPopupMenu.Popup(AMessage.XPos, AMessage.YPos);
    AMessage.Result := LRESULT(TRUE);
  end
  else if (LClient.X > FLeftMarginWidth) then
  begin
    LNewCaretPosition := ClientToLines(LClient.X, LClient.Y);

    if (Assigned(PopupMenu)) then
      inherited
    else
    begin
      if (FPopupMenu = 0) then
      begin
        LInstance := GetModuleHandle('User32.dll');
        LMenu := LoadMenu(LInstance, MAKEINTRESOURCE(1));
        if ((LMenu > 0) and (GetMenuItemCount(LMenu) = 1)) then
        begin
          LMenu := GetSubMenu(LMenu, 0);
          LMenuItemInfo.cbSize := SizeOf(LMenuItemInfo);
          LMenuItemInfo.fMask := MIIM_ID or MIIM_STATE or MIIM_SUBMENU;
          if ((LMenu > 0) and GetMenuItemInfo(LMenu, 0, TRUE, LMenuItemInfo)) then
          begin
            FPopupMenu := LMenu;
            for LIndex := 0 to GetMenuItemCount(FPopupMenu) - 1 do
              if (GetMenuItemInfo(FPopupMenu, LIndex, TRUE, LMenuItemInfo)) then
                case (LMenuItemInfo.wID) of
                  WM_APP + 1: { Show Unicode control characters }
                    begin
                      LMenuItemInfo.fState := LMenuItemInfo.fState and not MFS_DISABLED;
                      SetMenuItemInfo(FPopupMenu, LIndex, TRUE, LMenuItemInfo);
                    end;
                  WM_APP + 19: { Insert Unicode control character }
                    begin
                      LMenu := LMenuItemInfo.hSubMenu;
                      for LIndex2 := 0 to GetMenuItemCount(LMenu) - 1 do
                        if (GetMenuItemInfo(LMenu, LIndex2, TRUE, LMenuItemInfo)) then
                        begin
                          LMenuItemInfo.fState := LMenuItemInfo.fState and not MFS_DISABLED;
                          SetMenuItemInfo(LMenu, LIndex2, TRUE, LMenuItemInfo);
                        end;
                    end;
                end;
            LLen := LoadString(LInstance, 700, @LBuffer[0], Length(LBuffer));
            if (LLen > 0) then
            begin
              AppendMenu(FPopupMenu, MF_SEPARATOR, 0, nil);
              AppendMenu(FPopupMenu, MF_STRING, WM_APP + 20, @LBuffer[0]);
              LLen := LoadString(LInstance, 705, @LBuffer[0], Length(LBuffer));
              if (LLen > 0) then
                AppendMenu(FPopupMenu, MF_STRING, WM_APP + 21, @LBuffer[0]);
            end;
          end;
        end;
      end;
      if (FPopupMenu <> 0) then
      begin
        LMenuItemInfo.cbSize := SizeOf(LMenuItemInfo);
        LMenuItemInfo.fMask := MIIM_ID or MIIM_STATE;
        for LIndex := 0 to GetMenuItemCount(FPopupMenu) - 1 do
          if (GetMenuItemInfo(FPopupMenu, LIndex, TRUE, LMenuItemInfo)) then
          begin
            case (LMenuItemInfo.wID) of
              WM_UNDO:
                if (CanUndo) then
                  LMenuItemInfo.fState := LMenuItemInfo.fState and not MFS_DISABLED
                else
                  LMenuItemInfo.fState := LMenuItemInfo.fState or MFS_DISABLED;
              WM_CUT:
                if (not FLines.SelArea.IsEmpty() and not ReadOnly) then
                  LMenuItemInfo.fState := LMenuItemInfo.fState and not MFS_DISABLED
                else
                  LMenuItemInfo.fState := LMenuItemInfo.fState or MFS_DISABLED;
              WM_COPY:
                if (not FLines.SelArea.IsEmpty()) then
                  LMenuItemInfo.fState := LMenuItemInfo.fState and not MFS_DISABLED
                else
                  LMenuItemInfo.fState := LMenuItemInfo.fState or MFS_DISABLED;
              WM_PASTE:
                if (CanPaste) then
                  LMenuItemInfo.fState := LMenuItemInfo.fState and not MFS_DISABLED
                else
                  LMenuItemInfo.fState := LMenuItemInfo.fState or MFS_DISABLED;
              WM_CLEAR:
                if (not FLines.SelArea.IsEmpty() and not ReadOnly) then
                  LMenuItemInfo.fState := LMenuItemInfo.fState and not MFS_DISABLED
                else
                  LMenuItemInfo.fState := LMenuItemInfo.fState or MFS_DISABLED;
              EM_SETSEL:
                if ((FLines.SelArea <> FLines.Area) and (FLines.Count > 0)) then
                  LMenuItemInfo.fState := LMenuItemInfo.fState and not MFS_DISABLED
                else
                  LMenuItemInfo.fState := LMenuItemInfo.fState or MFS_DISABLED;
              WM_APP + 0: { Right to left Reading order }
                  LMenuItemInfo.fState := LMenuItemInfo.fState and not MFS_DISABLED;
//              WM_APP + 1: { Show Unicode control characters }
//                if (not FUCCVisible) then
//                  LMenuItemInfo.fState := LMenuItemInfo.fState and not MFS_CHECKED
//                else
//                  LMenuItemInfo.fState := LMenuItemInfo.fState or MFS_CHECKED;
//              WM_APP + 19: { Insert Unicode control character }
//                if (ReadOnly) then
//                  LMenuItemInfo.fState := LMenuItemInfo.fState or MFS_DISABLED
//                else
//                  LMenuItemInfo.fState := LMenuItemInfo.fState and not MFS_DISABLED;
              WM_APP + 20: { Open IME }
                LMenuItemInfo.fState := LMenuItemInfo.fState and not MFS_DISABLED;
              WM_APP + 21: { Reconversion }
                LMenuItemInfo.fState := LMenuItemInfo.fState or MFS_DISABLED
              else
                LMenuItemInfo.fState := LMenuItemInfo.fState or MFS_DISABLED;
            end;
            SetMenuItemInfo(FPopupMenu, LIndex, TRUE, LMenuItemInfo);
          end;

        TrackPopupMenu(FPopupMenu, 2, AMessage.XPos, AMessage.YPos, 0, Handle, nil);
      end;
    end;
  end;
end;

procedure TCustomBCEditor.WMCopy(var AMessage: TWMCopy);
begin
  if (FLines.SelArea.IsEmpty()) then
    AMessage.Result := LRESULT(FALSE)
  else
  begin
    CopyToClipboard();
    AMessage.Result := LRESULT(TRUE);
  end;
end;

procedure TCustomBCEditor.WMCut(var AMessage: TWMCut);
begin
  if (ReadOnly or FLines.SelArea.IsEmpty()) then
    AMessage.Result := LRESULT(FALSE)
  else
  begin
    CutToClipboard();
    AMessage.Result := LRESULT(TRUE);
  end;
end;

procedure TCustomBCEditor.WMEraseBkgnd(var AMessage: TMessage);
begin
  AMessage.Result := 1;
end;

procedure TCustomBCEditor.WMGetDlgCode(var AMessage: TWMGetDlgCode);
begin
  inherited;
  AMessage.Result := AMessage.Result or DLGC_WANTARROWS or DLGC_WANTCHARS;
  if FWantTabs then
    AMessage.Result := AMessage.Result or DLGC_WANTTAB;
  if FWantReturns then
    AMessage.Result := AMessage.Result or DLGC_WANTALLKEYS;
end;

procedure TCustomBCEditor.WMGetText(var AMessage: TWMGetText);
var
  LText: string;
begin
  if (FFmtLines) then
    LText := FRows.FmtText
  else
    LText := FLines.Text;
  StrLCopy(PChar(AMessage.Text), PChar(LText), AMessage.TextMax - 1);
  AMessage.Result := StrLen(PChar(AMessage.Text));
end;

procedure TCustomBCEditor.WMGetTextLength(var AMessage: TWMGetTextLength);
begin
  if ((csDocking in ControlState) or (csDestroying in ComponentState)) then
    AMessage.Result := 0
  else
    AMessage.Result := FLines.TextLength;
end;

procedure TCustomBCEditor.WMHScroll(var AMessage: TWMScroll);
begin
  AMessage.Result := 0;

  inherited;

  case (AMessage.ScrollCode) of
    SB_LEFT:
      HorzTextPos := 0;
    SB_RIGHT:
      HorzTextPos := FRows.MaxWidth - FTextRect.Width;
    SB_LINELEFT:
      HorzTextPos := FHorzTextPos - FMaxDigitWidth;
    SB_LINERIGHT:
      HorzTextPos := FHorzTextPos + FMaxDigitWidth;
    SB_PAGELEFT:
      HorzTextPos := FHorzTextPos - FTextRect.Width;
    SB_PAGERIGHT:
      HorzTextPos := FHorzTextPos + FTextRect.Width;
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
  if ((AMessage.LParam and GCS_RESULTSTR <> 0)
    and (FIMEStatus and EIMES_GETCOMPSTRATONCE = 0)) then
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
      HorzTextPos := HorzTextPos - FSpaceWidth;
    end
    else
      HorzTextPos := HorzTextPos + FSpaceWidth;
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
  if (Assigned(FHintWindow)) then
    FreeAndNil(FHintWindow);

  inherited;

  SendMessage(FParentWnd, WM_COMMAND, EN_KILLFOCUS shl 16 + FDlgCtrlID and $FFFF, LPARAM(Handle));

  if (not Assigned(FCompletionProposalPopupWindow)) then
    UpdateCaret();

  if (HideSelection and not FLines.SelArea.IsEmpty()) then
    Invalidate();
end;

procedure TCustomBCEditor.WMPaint(var AMessage: TWMPaint);
begin
  ControlState := ControlState + [csCustomPaint];
  inherited;
  ControlState := ControlState - [csCustomPaint];
end;

procedure TCustomBCEditor.WMPaste(var AMessage: TWMPaste);
begin
  if (ReadOnly or not IsClipboardFormatAvailable(CF_UNICODETEXT)) then
    AMessage.Result := LRESULT(FALSE)
  else
  begin
    PasteFromClipboard();
    AMessage.Result := LRESULT(TRUE);
  end;
end;

procedure TCustomBCEditor.WMSetCursor(var AMessage: TWMSetCursor);
var
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

    if (LWidth < LCursorPoint.X) and (LCursorPoint.X < LWidth + FLeftMarginWidth) then
      SetCursor(Screen.Cursors[crDefault])
    else
    begin
      LSelectionAvailable := not FLines.SelArea.IsEmpty();
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

  SendMessage(FParentWnd, WM_COMMAND, EN_SETFOCUS shl 16 + FDlgCtrlID and $FFFF, LPARAM(Handle));

  if (HideSelection and not FLines.SelArea.IsEmpty()) then
    Invalidate();

  if (Assigned(FCompletionProposalPopupWindow)) then
    PostMessage(Handle, UM_FREE_COMPLETIONPROPOSAL_POPUPWINDOW, 0, 0);
end;

procedure TCustomBCEditor.WMSetText(var AMessage: TWMSetText);
begin
  if (ReadOnly) then
    AMessage.Result := LPARAM(FALSE)
  else
  begin
    AMessage.Result := LPARAM(TRUE);
    Text := StrPas(AMessage.Text);
  end;
end;

procedure TCustomBCEditor.WMSettingChange(var AMessage: TWMSettingChange);
begin
  inherited;

  if ((AMessage.Flag = SPI_SETWHEELSCROLLLINES)
    and not SystemParametersInfo(SPI_GETWHEELSCROLLLINES, 0, @FWheelScrollLines, 0)) then
    FWheelScrollLines := 3;
end;

procedure TCustomBCEditor.WMStyleChanged(var AMessage: TWMStyleChanged);
begin
  inherited;

  FLines.ReadOnly := ReadOnly;
  if (not Focused() and not FLines.SelArea.IsEmpty()) then
    Invalidate();
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
        if (ShowHint
          and not (esScrolling in FState)) then
          Process(paHint, nil, mbLeft, [], FCursorPoint.X, FCursorPoint.Y);
      end;
    tiScrolling:
      Process(paScrolling, nil, mbLeft, [], FCursorPoint.X, FCursorPoint.Y);
    tiScroll:
      Process(paMouseMove, nil, mbLeft, [], FCursorPoint.X, FCursorPoint.Y);
    tiIdle:
      begin
        KillTimer(Handle, Msg.TimerID);
        Perform(UM_IDLE, 0, 0);
      end;
  end;
end;

procedure TCustomBCEditor.WMUndo(var AMessage: TMessage);
begin
  FLines.Undo();
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

initialization
  GImmEnabled := BOOL(GetSystemMetrics(SM_DBCSENABLED));
  GLineWidth := Round(Screen.PixelsPerInch / USER_DEFAULT_SCREEN_DPI);
  GPadding := Round(Screen.PixelsPerInch / USER_DEFAULT_SCREEN_DPI);

  OleCheck(OleInitialize(nil));
finalization
  OleUninitialize();
end.


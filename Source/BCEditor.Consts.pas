unit BCEditor.Consts;

interface {********************************************************************}

uses
  Graphics;

type
  TBCEditorAnsiCharSet = set of AnsiChar;

const
  BCEDITOR_BOOKMARKS = 10;
  BCEDITOR_CODEFOLDING_COLLAPSEDMARK: PChar = '...';
  { Characters }
  BCEDITOR_UNDERSCORE = '_';
  BCEDITOR_CODE_FOLDING_VALID_CHARACTERS = ['\', '@', '_'];
  BCEDITOR_REAL_NUMBER_CHARS = ['0' .. '9', 'e', 'E', '.'];
  BCEDITOR_NONE_CHAR = #0;
  BCEDITOR_BACKSPACE_CHAR = #8;
  BCEDITOR_TAB_CHAR = #9;
  BCEDITOR_LINEFEED = #10;
  BCEDITOR_CARRIAGE_RETURN = #13;
  BCEDITOR_CARRIAGE_RETURN_KEY = 13;
  BCEDITOR_ESCAPE_KEY = 27;
  BCEDITOR_SPACE_CHAR = #32;
  BCEDITOR_EXCLAMATION_MARK = #33;
  BCEDITOR_CTRL_BACKSPACE = #127;
  BCEDITOR_WORD_BREAK_CHARACTERS = ['.', ',', ';', ':', '"', '''', '!', '?', '[', ']', '(', ')', '{', '}', '^',
    '=', '+', '-', '*', '/', '\', '|', ' '];
  BCEDITOR_EXTRA_WORD_BREAK_CHARACTERS = ['´', '`', '°', '&', '$', '@', '§', '%', '#', '~', '<', '>'];
  BCEDITOR_EMPTY_CHARACTERS = [BCEDITOR_NONE_CHAR, BCEDITOR_TAB_CHAR, BCEDITOR_SPACE_CHAR];
  BCEDITOR_DEFAULT_DELIMITERS: TBCEditorAnsiCharSet = ['*', '/', '+', '-', '=', '\', '|', '&', '(', ')', '[', ']', '{', '}',
    '`', '~', '!', '@', ',', '$', '%', '^', '?', ':', ';', '''', '"', '.', '>', '<', '#'];
  BCEDITOR_ABSOLUTE_DELIMITERS: TBCEditorAnsiCharSet = [BCEDITOR_NONE_CHAR, BCEDITOR_TAB_CHAR, BCEDITOR_LINEFEED,
    BCEDITOR_CARRIAGE_RETURN, BCEDITOR_SPACE_CHAR];
  { Unicode control characters }
  BCEditor_UCC_RS = #$001E;
  BCEditor_UCC_US = #$001F;
  BCEditor_UCC_LRE = #$202A;
  BCEditor_UCC_RLE = #$202B;
  BCEditor_UCC_ZWNJ = #$200C;
  BCEditor_UCC_ZWJ = #$200D;
  BCEditor_UCC_LRM = #$200E;
  BCEditor_UCC_RLM = #$200F;
  BCEditor_UCC_ISS = #$206A;
  BCEditor_UCC_ASS = #$206B;
  BCEditor_UCC_PDF = #$202C;
  BCEditor_UCC_LRO = #$202D;
  BCEditor_UCC_RLO = #$202E;
  BCEditor_UCC_IAFS = #$206C;
  BCEditor_UCC_AAFS = #$206D;
  BCEditor_UCC_NADS = #$206E;
  BCEditor_UCC_NODS = #$206F;
  { Highlighter attribute elements }
  BCEDITOR_ATTRIBUTE_ELEMENT_EDITOR = 'Editor';
  BCEDITOR_ATTRIBUTE_ELEMENT_COMMENT = 'Comment';
  BCEDITOR_ATTRIBUTE_ELEMENT_STRING = 'String';
  { Resource file icons }
  BCEDITOR_SYNCEDIT = 'BCEDITORSYNCEDIT';

const
  { Command categories ********************************************************}

  eccUnknown = 0;
  eccState = 1;
  eccBookmark = 2;
  eccScroll = 3;
  eccMoveCaret = 4;
  eccText = 5;
  eccUndo = 6;
  eccClipboard = 7;
  eccShowDialog = 8;
  eccMacroRecorder = 9;

  eccLast = 9;
  eccUser = 100;

  { Commands ******************************************************************}

  ecNone = 0;

  ecCancel = 104;
  ecInsertTextMode = 1;
  ecOverwriteTextMode = 2;
  ecSyncEdit = 3;
  ecTerminate = 107;
  ecToggleTextMode = 4;

  ecGotoBookmark1 = 5;
  ecGotoBookmark2 = 6;
  ecGotoBookmark3 = 7;
  ecGotoBookmark4 = 8;
  ecGotoBookmark5 = 9;
  ecGotoBookmark6 = 10;
  ecGotoBookmark7 = 11;
  ecGotoBookmark8 = 12;
  ecGotoBookmark9 = 13;
  ecGotoBookmark0 = 14;
  ecGotoNextBookmark = 15;
  ecGotoPreviousBookmark = 16;
  ecSetBookmark1 = 17;
  ecSetBookmark2 = 18;
  ecSetBookmark3 = 19;
  ecSetBookmark4 = 20;
  ecSetBookmark5 = 21;
  ecSetBookmark6 = 22;
  ecSetBookmark7 = 23;
  ecSetBookmark8 = 24;
  ecSetBookmark9 = 25;
  ecSetBookmark0 = 26;

  ecScrollDown = 27;
  ecScrollLeft = 28;
  ecScrollRight = 29;
  ecScrollTo = 30;
  ecScrollUp = 31;

  ecBOF = 32;
  ecBOL = 33;
  ecDown = 35;
  ecEOF = 36;
  ecEOL = 37;
  ecFindBackward = 39;
  ecFindFirst = 40;
  ecFindForeward = 41;
  ecFindNext = 42;
  ecFindPrevious = 43;
  ecGotoMatchingPair = 108;
  ecLeft = 44;
  ecPageBottom = 38;
  ecPageDown = 45;
  ecPageTop = 34;
  ecPageUp = 46;
  ecPosition = 47;
  ecRight = 48;
  ecSel = 49;
  ecSelBOF = 50;
  ecSelBOL = 51;
  ecSelBOP = 52;
  ecSelDown = 53;
  ecSelectAll = 54;
  ecSelEOF = 55;
  ecSelEOL = 56;
  ecSelEOP = 57;
  ecSelLeft = 58;
  ecSelPageDown = 59;
  ecSelPageUp = 60;
  ecSelRight = 61;
  ecSelUp = 62;
  ecSelWord = 63;
  ecSelWordLeft = 64;
  ecSelWordRight = 65;
  ecUnselect = 66;
  ecUp = 67;
  ecWordLeft = 68;
  ecWordRight = 69;

  ecAcceptDrop = 105;
  ecBackspace = 70;
  ecBlockComment = 71;
  ecBlockIndent = 72;
  ecBlockUnindent = 73;
  ecChar = 74;
  ecClear = 75;
  ecDeleteToBOL = 76;
  ecDeleteChar = 77;
  ecDeleteToEOL = 78;
  ecDeleteLastWord = 79;
  ecDeleteLine = 80;
  ecDeleteWord = 81;
  ecDropOLE = 106;
  ecInsertLine = 82;
  ecLineComment = 83;
  ecLowerCase = 84;
  ecReturn = 85;
  ecReplace = 86;
  ecShiftTab = 87;
  ecTab = 88;
  ecText = 89;
  ecUpperCase = 90;

  ecRedo = 91;
  ecUndo = 92;

  ecCopyToClipboard = 93;
  ecCutToClipboard = 94;
  ecPasteFromClipboard = 95;

  ecShowCompletionProposal = 96;
  ecShowFind = 97;
  ecShowGotoLine = 98;
  ecShowReplace = 99;

  ecPlaybackMacro = 100;
  ecRecordMacro = 101;
  ecStepMacro = 102;
  ecStopMacro = 103;

  ecLast = 108;
  ecUser = 10000;

  { Colors ********************************************************************}

  aclBookmarkBorder = $FF977302;
  aclBookmarkCover = $FFFAE6B2;
  aclBookmarkNumber = $FF977302;
  aclBookmarkRingLeft = $FF929292;
  aclBookmarkRingMiddle = $FFFCFCFC;
  aclBookmarkRingRight = $FFBCBCBC;
  clCodeFoldingBackground = $00F4F4F4;
  clCodeFoldingForeground = $00CC9999;
  clActiveLineBackground = $00E6FFFA;
  clFoundTextBackground = $0078AAFF;
  clFoundTextForeground = clWindowText;
  clLineNumbersBackground = $00F4F4F4;
  clLineNumbersForeground = $00CC9999;
  clLineStateLoaded = $00F4F4F4;
  clLineStateModified = clYellow;
  clLineStateSaved = clLime;
  clMarksBackground = clBtnFace;
  clMatchingPairBackground = clAqua;
  clSelectionBackground = clHighlight;
  clSelectionForeground = clHighlightText;
  clSpecialCharsForeground = clGrayText;
  clSyncEditBackground = $00FCFDCD;
  clSyncEditOverlay = clHighlight;

var
  BCEditor_UCCs: array of Char;

implementation {***************************************************************}

initialization
  SetLength(BCEditor_UCCs, 17);
  BCEditor_UCCs[0] := BCEditor_UCC_RS;
  BCEditor_UCCs[1] := BCEditor_UCC_US;
  BCEditor_UCCs[2] := BCEditor_UCC_LRE;
  BCEditor_UCCs[3] := BCEditor_UCC_RLE;
  BCEditor_UCCs[4] := BCEditor_UCC_ZWNJ;
  BCEditor_UCCs[5] := BCEditor_UCC_ZWJ;
  BCEditor_UCCs[6] := BCEditor_UCC_LRM;
  BCEditor_UCCs[7] := BCEditor_UCC_RLM;
  BCEditor_UCCs[8] := BCEditor_UCC_ISS;
  BCEditor_UCCs[9] := BCEditor_UCC_ASS;
  BCEditor_UCCs[10] := BCEditor_UCC_PDF;
  BCEditor_UCCs[11] := BCEditor_UCC_LRO;
  BCEditor_UCCs[12] := BCEditor_UCC_RLO;
  BCEditor_UCCs[13] := BCEditor_UCC_IAFS;
  BCEditor_UCCs[14] := BCEditor_UCC_AAFS;
  BCEditor_UCCs[15] := BCEditor_UCC_NADS;
  BCEditor_UCCs[16] := BCEditor_UCC_NODS;
end.

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
  BCEDITOR_REAL_NUMBER_CHARS = ['e', 'E', '.'];
  BCEDITOR_NONE_CHAR = #0;
  BCEDITOR_BACKSPACE_CHAR = #8;
  BCEDITOR_TAB_CHAR = #9;
  BCEDITOR_LINEFEED = #10;
  BCEDITOR_CARRIAGE_RETURN = #13;
  BCEDITOR_CARRIAGE_RETURN_KEY = 13;
  BCEDITOR_ESCAPE = #27;
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
  { Default colors }
  clSelectionColor = clHighlight;
  clSearchHighlighter = $0078AAFF;
  clSearchInSelectionBackground = $00FCFDCD;
  clSpecialChar = clGrayText;
  clActiveLineBackground = $00E6FFFA;
  clLeftMarginBackground = $00F4F4F4;
  clLeftMarginForeground = $00CC9999;
  clSearchMapActiveLine = $00F4F4F4;
  clIndentHighlight = $00CC9999;
  clIndent = $00CC9999;
  clMatchingPair = clAqua;
  clWordWrapIndicatorArrow = clNavy;
  clWordWrapIndicatorLines = clBlack;
  clSyncEditBackground = $00FCFDCD;
  clBookmarkBorder = $00027397;
  clBookmarkCover = $00B2E6FA;
  clBookmarkRingLeft = $00929292;
  clBookmarkRingMiddle = $00FCFCFC;
  clBookmarkRingRight = $00BCBCBC;
  clBookmarkNumber = $00027397;
  clTransparent = clFuchsia;
  { Resource file bitmaps }
  BCEDITOR_SYNCEDIT = 'BCEDITORSYNCEDIT';

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

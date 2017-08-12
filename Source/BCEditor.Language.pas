unit BCEditor.Language;

interface {********************************************************************}

resourcestring

  { Messages }
  SBCEditorMessageConfirmation = 'Confirmation';
  SBCEditorMessageError = 'Error';
  SBCEditorMessageInformation = 'Information';
  SBCEditorMessageQuestion = 'Question';
  SBCEditorOk = 'Ok';
  SBCEditorCancel = 'Cancel';

  { BCEditor }
  SBCEditorCodeFoldingCollapsedMark = '%d Lines';
  SBCEditorScrollInfo = 'Line: %d';
  SBCEditorPatternIsEmpty = 'Pattern is empty';
  SBCEditorPatternContainsWordBreakChar = 'Pattern contains word break character';
  SBCEditorReplaceTextPrompt = 'Replace this occurrence of "%s"?';
  SBCEditorSearchNotFound = 'Search string "%s" not found';
  SBCEditorSearchWrapAroundTitle = 'Search match not found';
  SBCEditorSearchWrapAroundForwards = 'Restart search from the beginning of the file?';
  SBCEditorSearchWrapAroundBackwards = 'Restart search from the ending of the file?';
  SBCEditorTerminatedByUser = 'Terminated by user';

  { BCEditor.Print.Preview }
  SBCEditorPreviewScrollHint = 'Page: %d';

  { BCEditor.DGotoLine }
  SBCEditorGotoLineCaption = 'Go to Line Number';
  SBCEditorGotoLineLine = 'Enter new line number';

implementation {***************************************************************}

end.

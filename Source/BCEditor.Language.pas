unit BCEditor.Language;

interface

resourcestring

  { Messages }
  SBCEditorMessageConfirmation = 'Confirmation';
  SBCEditorMessageError = 'Error';
  SBCEditorMessageInformation = 'Information';
  SBCEditorMessageQuestion = 'Question';

  { BCEditor.Editor }
  SBCEditorCodeFoldingCollapsedMark = '%d Lines';
  SBCEditorScrollInfo = 'Line: %d';
  SBCEditorPatternIsEmpty = 'Pattern is empty';
  SBCEditorPatternContainsWordBreakChar = 'Pattern contains word break character';
  SBCEditorReplaceTextPrompt = 'Replace this occurrence of "%s"?';
  SBCEditorSearchNotFound = 'Search string "%s" not found';
  SBCEditorSearchWrapAroundTitle = 'Search match not found';
  SBCEditorSearchWrapAroundForwards = 'Restart search from the beginning of the file?';
  SBCEditorSearchWrapAroundBackwards = 'Restart search from the ending of the file?';

  { BCEditor.MacroRecorder }
  SBCEditorCannotRecord = 'Cannot record macro; already recording or playing';
  SBCEditorCannotPlay = 'Cannot playback macro; already playing or recording';
  SBCEditorCannotPause = 'Can only pause when recording';
  SBCEditorCannotResume = 'Can only resume when paused';

  { BCEditor.Print.Preview }
  SBCEditorPreviewScrollHint = 'Page: %d';

implementation

end.

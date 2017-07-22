unit BCEditor.Language;

interface

resourcestring

  { BCEditor.Editor }
  SBCEditorVersion = 'Версия';
  SBCEditorScrollInfo = 'Верхняя строка: %d';
  SBCEditorSearchStringNotFound = 'Строка ''%s'' не найдена';
  SBCEditorSearchMatchNotFound = 'Выражение не найдено. %s Повторить поиск с начала текста?';
  SBCEditorRightMarginPosition = 'Позиция: %d';

  { BCEditor.MacroRecorder }
  SBCEditorCannotRecord = 'Невозможно записать макрос - он уже записывается или проигрывается';
  SBCEditorCannotPlay = 'Невозможно воспроизвести макрос - он уже записывается или проигрывается';
  SBCEditorCannotPause = 'Можно приостановить запись';
  SBCEditorCannotResume = 'Можно продолжить после паузы';

  { BCEditor.Print.Preview }
  SBCEditorPreviewScrollHint = 'Страница: %d';

  { BCEditor.Search }
  SBCEditorPatternIsEmpty = 'Паттерн занят';

implementation

end.

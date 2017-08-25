unit BCEditor.CompletionProposal;

interface {********************************************************************}

uses
  Messages,
  Classes, Types,
  Forms, Controls, Graphics, StdCtrls,
  BCEditor.Types, BCEditor.Properties;

type
  TBCEditorCompletionProposalPopup = class(TCustomControl)
  strict private
    FAdjustCompletionStart: Boolean;
    FDoubleBufferBitmap: Graphics.TBitmap;
    FCaseSensitive: Boolean;
    FCompletionProposal: TBCEditorCompletionProposal;
    FCompletionStartChar: Integer;
    FCurrentString: string;
    FEditor: TCustomControl;
    FFiltered: Boolean;
    FItemHeight: Integer;
    FItemIndexArray: array of Integer;
    FItems: TStrings;
    FMargin: Integer;
    FOnClose: TBCEditorCompletionProposalCloseEvent;
    FOnValidate: TBCEditorCompletionProposalValidateEvent;
    FOriginalHeight: Integer;
    FOriginalWidth: Integer;
    FPopupParent: TCustomForm;
    FSelectedLine: Integer;
    FSendToEditor: Boolean;
    FTitleHeight: Integer;
    FTitleVisible: Boolean;
    FTopLine: Integer;
    FValueSet: Boolean;
    function GetItemHeight(): Integer;
    function GetItems(): TBCEditorCompletionProposalItems;
    function GetTitleHeight(): Integer;
    function GetVisibleLines(): Integer;
    procedure HandleDblClick(ASender: TObject);
    procedure HandleOnValidate(ASender: TObject; AShift: TShiftState; AEndToken: Char);
    procedure MoveSelectedLine(ALineCount: Integer);
    procedure SetCurrentString(const AValue: string);
    procedure SetPopupParent(Value: TCustomForm);
    procedure SetTopLine(const AValue: Integer);
    procedure UpdateScrollBar();
    procedure WMActivate(var Msg: TWMActivate); message WM_ACTIVATE;
    procedure WMEraseBkgnd(var AMessage: TMessage); message WM_ERASEBKGND;
    procedure WMVScroll(var AMessage: TWMScroll); message WM_VSCROLL;
  protected
    FActiveControl: TWinControl;
    procedure CreateParams(var Params: TCreateParams); override;
    procedure Hide();
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure KeyPress(var Key: Char); override;
    procedure MouseDown(AButton: TMouseButton; AShift: TShiftState; X, Y: Integer); override;
    procedure Paint(); override;
    procedure Show(Origin: TPoint);
    property OnClose: TBCEditorCompletionProposalCloseEvent read FOnClose write FOnClose;
    property OnValidate: TBCEditorCompletionProposalValidateEvent read FOnValidate write FOnValidate;
  public
    constructor Create(const AEditor: TCustomControl); reintroduce;
    destructor Destroy; override;
    procedure Assign(ASource: TPersistent); override;
    procedure Execute(const ACurrentString: string; const APoint: TPoint);
    function GetCurrentInput(): string;
    procedure IncSize(const AWidth: Integer; const AHeight: Integer);
    procedure MouseWheel(AShift: TShiftState; AWheelDelta: Integer; AMousePos: TPoint);
    procedure SetOriginalSize;
    property ActiveControl: TWinControl read FActiveControl;
    property CurrentString: string read FCurrentString write SetCurrentString;
    property Editor: TCustomControl read FEditor;
    property Items: TBCEditorCompletionProposalItems read GetItems;
    property TopLine: Integer read FTopLine write SetTopLine;
    property PopupParent: TCustomForm read FPopupParent write SetPopupParent;
  end;

implementation {***************************************************************}

uses
  Windows,
  SysUtils, UITypes, Math,
  Themes, Dialogs,
  BCEditor.Consts, BCEditor, BCEditor.Commands, BCEditor.Lines;

type
  TCustomBCEditor = class(BCEditor.TCustomBCEditor);
  TBCEditorLines = class(BCEditor.Lines.TBCEditorLines);

{ TBCEditorCompletionProposalPopupWindow **************************************}

procedure TBCEditorCompletionProposalPopup.Assign(ASource: TPersistent);
begin
  if ASource is TBCEditorCompletionProposal then
  begin
    FCompletionProposal := ASource as TBCEditorCompletionProposal;
    with FCompletionProposal do
    begin
      Self.FCaseSensitive := cpoCaseSensitive in Options;
      Self.FFiltered := cpoFiltered in Options;
      Self.Width := Width;
      Self.Constraints.Assign(Constraints);
    end
  end
  else
    inherited Assign(ASource);
end;

constructor TBCEditorCompletionProposalPopup.Create(const AEditor: TCustomControl);
begin
  inherited Create(AEditor);

  ControlStyle := ControlStyle + [csNoDesignVisible, csReplicatable];

  FEditor := AEditor;

  Ctl3D := False;
  FCaseSensitive := False;
  FFiltered := False;
  FItemHeight := 0;
  FMargin := 2;
  FOnClose := nil;
  FPopupParent := nil;
  FValueSet := False;
  ParentCtl3D := False;
  Visible := False;

  FItems := TStringList.Create;
  FDoubleBufferBitmap := Graphics.TBitmap.Create();

  FOnValidate := nil;
  OnDblClick := HandleDblClick;
end;

procedure TBCEditorCompletionProposalPopup.CreateParams(var Params: TCreateParams);
begin
  inherited;

  Params.Style := WS_POPUP or WS_BORDER;
  if cpoResizeable in FCompletionProposal.Options then
    Params.Style := Params.Style or WS_SIZEBOX;
  Params.WindowClass.Style := Params.WindowClass.Style or CS_DROPSHADOW;

  if (Assigned(PopupParent)) then
    Params.WndParent := PopupParent.Handle;
end;

destructor TBCEditorCompletionProposalPopup.Destroy();
var
  LSelectedItem: string;
begin
  if FItemHeight <> 0 then
    FCompletionProposal.VisibleLines := ClientHeight div FItemHeight;
  FCompletionProposal.Width := Width;

  if not FValueSet and Assigned(FOnClose) then
  begin
    LSelectedItem := '';
    FOnClose(FEditor, LSelectedItem);
  end;

  FDoubleBufferBitmap.Free;
  SetLength(FItemIndexArray, 0);
  FItems.Free;

  inherited Destroy;
end;

procedure TBCEditorCompletionProposalPopup.Execute(const ACurrentString: string; const APoint: TPoint);
var
  LPoint: TPoint;

  procedure CalculateFormPlacement;
  begin
    LPoint.X := APoint.X - FDoubleBufferBitmap.Canvas.TextWidth(ACurrentString);
    LPoint.Y := APoint.Y;

    ClientHeight := FItemHeight * FCompletionProposal.VisibleLines + FTitleHeight + 2;

    if LPoint.X + ClientWidth > Screen.DesktopWidth then
    begin
      LPoint.X := Screen.DesktopWidth - ClientWidth - 5;
      if LPoint.X < 0 then
        LPoint.X := 0;
    end;

    if LPoint.Y + ClientHeight > Screen.DesktopHeight then
    begin
      LPoint.Y := LPoint.Y - ClientHeight - TCustomBCEditor(Editor).LineHeight - 2;
      if LPoint.Y < 0 then
        LPoint.Y := 0;
    end;
  end;

  procedure CalculateColumnWidths();
  var
    LAutoWidthCount: Integer;
    LColumnIndex: Integer;
    LIndex: Integer;
    LItems: TBCEditorCompletionProposalItems;
    LMaxWidth: Integer;
    LProposalColumn: TBCEditorCompletionProposalColumns.TColumn;
    LTempWidth: Integer;
    LVisibleColumnCount: Integer;
    LWidthSum: Integer;
  begin
    LVisibleColumnCount := 0;
    for LColumnIndex := 0 to FCompletionProposal.Columns.Count - 1 do
      if FCompletionProposal.Columns[LColumnIndex].Visible then
        Inc(LVisibleColumnCount);

    if LVisibleColumnCount = 1 then
    begin
      LProposalColumn := nil; // Hide compiler warning only.
      for LColumnIndex := 0 to FCompletionProposal.Columns.Count - 1 do
        if FCompletionProposal.Columns[LColumnIndex].Visible then
          LProposalColumn := FCompletionProposal.Columns[LColumnIndex];
      if LProposalColumn.AutoWidth then
        LProposalColumn.Width := Width;
      Exit;
    end;

    LAutoWidthCount := 0;
    LWidthSum := 0;
    for LColumnIndex := 0 to FCompletionProposal.Columns.Count - 1 do
    begin
      LProposalColumn := FCompletionProposal.Columns[LColumnIndex];
      if LProposalColumn.Visible and LProposalColumn.AutoWidth then
      begin
        LItems := LProposalColumn.Items;
        LMaxWidth := 0;
        for LIndex := 0 to LItems.Count - 1 do
        begin
          LTempWidth := FDoubleBufferBitmap.Canvas.TextWidth(LItems[LIndex].Value);
          if LTempWidth > LMaxWidth then
            LMaxWidth := LTempWidth;
        end;
        LProposalColumn.Width := LMaxWidth;
        LWidthSum := LWidthSum + LMaxWidth;
        Inc(LAutoWidthCount);
      end;
    end;

    LMaxWidth := (Width - LWidthSum - GetSystemMetrics(SM_CYHSCROLL)) div LAutoWidthCount;
    if LMaxWidth > 0 then
    for LColumnIndex := 0 to FCompletionProposal.Columns.Count - 1 do
    begin
      LProposalColumn := FCompletionProposal.Columns[LColumnIndex];
      if LProposalColumn.Visible and LProposalColumn.AutoWidth then
        LProposalColumn.Width := LProposalColumn.Width + LMaxWidth;
    end;
  end;

  function GetTitleVisible: Boolean;
  var
    LColumn: TBCEditorCompletionProposalColumns.TColumn;
    LColumnIndex: Integer;
  begin
    Result := False;
    for LColumnIndex := 0 to FCompletionProposal.Columns.Count - 1 do
    begin
      LColumn := FCompletionProposal.Columns[LColumnIndex];
      if LColumn.Visible and LColumn.Title.Visible then
        Exit(True);
    end;
  end;

  procedure SetAutoConstraints;
  begin
    if cpoAutoConstraints in FCompletionProposal.Options then
    begin
      FCompletionProposal.Constraints.MinHeight := Height;
      FCompletionProposal.Constraints.MinWidth := Width;
      Constraints.Assign(FCompletionProposal.Constraints);
    end;
  end;

var
  LCount: Integer;
  LIndex: Integer;
begin
  LCount := GetItems.Count;
  SetLength(FItemIndexArray, 0);
  SetLength(FItemIndexArray, LCount);
  for LIndex := 0 to LCount - 1 do
    FItemIndexArray[LIndex] := LIndex;

  if Length(FItemIndexArray) > 0 then
  begin
    FTitleVisible := GetTitleVisible;
    FItemHeight := GetItemHeight;
    FTitleHeight := GetTitleHeight;
    CalculateFormPlacement;
    CalculateColumnWidths;
    SetAutoConstraints;
    CurrentString := ACurrentString;
    if Length(FItemIndexArray) > 0 then
    begin
      UpdateScrollBar;
      Show(LPoint);
    end;
  end;
end;

function TBCEditorCompletionProposalPopup.GetCurrentInput(): string;
var
  LChar: Integer;
  LLineText: string;
  LTextCaretPosition: TBCEditorLinesPosition;
begin
  Result := '';

  LTextCaretPosition := TCustomBCEditor(Editor).CaretPos;

  LLineText := TCustomBCEditor(Editor).Lines[Min(LTextCaretPosition.Line, TCustomBCEditor(Editor).Lines.Count - 1)];
  LChar := LTextCaretPosition.Char;
  if (LChar <= Length(LLineText)) then
  begin
    FAdjustCompletionStart := False;
    while ((LChar > 0) and not TCustomBCEditor(Editor).IsWordBreakChar(LLineText[1 + LChar - 1])) do
      Dec(LChar);

    FCompletionStartChar := LChar;
    Result := Copy(LLineText, 1 + FCompletionStartChar, LTextCaretPosition.Char - FCompletionStartChar);
  end
  else
  begin
    FAdjustCompletionStart := True;
    FCompletionStartChar := LTextCaretPosition.Char;
  end;
end;

function TBCEditorCompletionProposalPopup.GetItemHeight(): Integer;
var
  LColumn: TBCEditorCompletionProposalColumns.TColumn;
  LColumnIndex: Integer;
  LHeight: Integer;
begin
  Result := 0;
  for LColumnIndex := 0 to FCompletionProposal.Columns.Count - 1 do
  begin
    LColumn := FCompletionProposal.Columns[LColumnIndex];
    FDoubleBufferBitmap.Canvas.Font.Assign(LColumn.Font);
    LHeight := FDoubleBufferBitmap.Canvas.TextHeight('X');
    if LHeight > Result then
      Result := LHeight;
  end;
end;

function TBCEditorCompletionProposalPopup.GetItems(): TBCEditorCompletionProposalItems;
begin
  Result := nil;
  if FCompletionProposal.CompletionColumnIndex <  FCompletionProposal.Columns.Count then
    Result := FCompletionProposal.Columns[FCompletionProposal.CompletionColumnIndex].Items;
end;

function TBCEditorCompletionProposalPopup.GetTitleHeight(): Integer;
var
  LColumn: TBCEditorCompletionProposalColumns.TColumn;
  LColumnIndex: Integer;
  LHeight: Integer;
begin
  Result := 0;
  if FTitleVisible then
  for LColumnIndex := 0 to FCompletionProposal.Columns.Count - 1 do
  begin
    LColumn := FCompletionProposal.Columns[LColumnIndex];
    FDoubleBufferBitmap.Canvas.Font.Assign(LColumn.Title.Font);
    LHeight := FDoubleBufferBitmap.Canvas.TextHeight('X');
    if LHeight > Result then
      Result := LHeight;
  end;
end;

function TBCEditorCompletionProposalPopup.GetVisibleLines(): Integer;
begin
  Result := (ClientHeight - FTitleHeight) div FItemHeight;
end;

procedure TBCEditorCompletionProposalPopup.HandleDblClick(ASender: TObject);
begin
  if Assigned(FOnValidate) then
    FOnValidate(Self, [], BCEDITOR_NONE_CHAR);
  Hide;
end;

procedure TBCEditorCompletionProposalPopup.HandleOnValidate(ASender: TObject; AShift: TShiftState; AEndToken: Char);
begin
  with TCustomBCEditor(Editor) do
    if (FSelectedLine < Length(FItemIndexArray)) then
    begin
      Lines.BeginUpdate();
      try
        ProcessCommand(ecDeleteWord);
        ProcessCommand(ecText, TBCEditorCommandDataText.Create(GetItems[FItemIndexArray[FSelectedLine]].Value));
      finally
        Lines.EndUpdate();
      end;
    end;
end;

procedure TBCEditorCompletionProposalPopup.Hide();
begin
  SetWindowPos(Handle, 0, 0, 0, 0, 0, SWP_HIDEWINDOW or SWP_NOSIZE or SWP_NOMOVE or SWP_NOZORDER);
  Visible := False;
end;

procedure TBCEditorCompletionProposalPopup.IncSize(const AWidth: Integer; const AHeight: Integer);
var
  LHeight: Integer;
  LWidth: Integer;
begin
  LHeight := FOriginalHeight + AHeight;
  LWidth := FOriginalWidth + AWidth;

  if LHeight < Constraints.MinHeight then
    LHeight := Constraints.MinHeight;
  if (Constraints.MaxHeight > 0) and (LHeight > Constraints.MaxHeight) then
    LHeight := Constraints.MaxHeight;

  if LWidth < Constraints.MinWidth then
    LWidth := Constraints.MinWidth;
  if (Constraints.MaxWidth > 0) and (LWidth > Constraints.MaxWidth) then
    LWidth := Constraints.MaxWidth;

  SetBounds(Left, Top, LWidth, LHeight);
end;

procedure TBCEditorCompletionProposalPopup.KeyDown(var Key: Word; Shift: TShiftState);
var
  LChar: Char;
  LTextCaretPosition: TBCEditorLinesPosition;
begin
  FSendToEditor := True;
  case Key of
    VK_TAB,
    VK_RETURN:
      begin
        if (Assigned(FOnValidate)) then
          FOnValidate(Self, Shift, BCEDITOR_NONE_CHAR)
        else
          HandleOnValidate(Self, Shift, BCEDITOR_NONE_CHAR);
        FSendToEditor := False;
      end;
    VK_ESCAPE:
      begin
        Editor.SetFocus;
        FSendToEditor := False;
      end;
    VK_LEFT:
      begin
        if Length(FCurrentString) > 0 then
        begin
          CurrentString := Copy(FCurrentString, 1, Length(FCurrentString) - 1);
          TCustomBCEditor(Editor).ProcessCommand(ecLeft);
        end
        else
        begin
          TCustomBCEditor(Editor).ProcessCommand(ecLeft);
          Editor.SetFocus;
        end;
        FSendToEditor := False;
      end;
    VK_RIGHT:
      with TCustomBCEditor(Editor) do
      begin
        LTextCaretPosition := CaretPos;
        if LTextCaretPosition.Char < Length(Lines[LTextCaretPosition.Line]) then
          LChar := Lines[LTextCaretPosition.Line][1 + LTextCaretPosition.Char]
        else
          LChar := BCEDITOR_SPACE_CHAR;

        if not IsWordBreakChar(LChar) then
          CurrentString := FCurrentString + LChar
        else
          Editor.SetFocus;

        ProcessCommand(ecRight);
        FSendToEditor := False;
      end;
    VK_PRIOR:
      begin
        MoveSelectedLine(-GetVisibleLines);
        FSendToEditor := False;
      end;
    VK_NEXT:
      begin
        MoveSelectedLine(GetVisibleLines);
        FSendToEditor := False;
      end;
    VK_END:
      begin
        TopLine := Length(FItemIndexArray) - 1;
        FSendToEditor := False;
      end;
    VK_HOME:
      begin
        TopLine := 0;
        FSendToEditor := False;
      end;
    VK_UP:
      begin
        if ssCtrl in Shift then
          FSelectedLine := 0
        else
          MoveSelectedLine(-1);
        FSendToEditor := False;
      end;
    VK_DOWN:
      begin
        if ssCtrl in Shift then
          FSelectedLine := Length(FItemIndexArray) - 1
        else
          MoveSelectedLine(1);
        FSendToEditor := False;
      end;
    VK_BACK:
      if Shift = [] then
      begin
        if Length(FCurrentString) > 0 then
        begin
          CurrentString := Copy(FCurrentString, 1, Length(FCurrentString) - 1);

          TCustomBCEditor(Editor).ProcessCommand(ecBackspace);
        end
        else
        begin
          TCustomBCEditor(Editor).ProcessCommand(ecBackspace);
          Editor.SetFocus;
        end;
        FSendToEditor := False;
      end;
    VK_DELETE:
      begin
        TCustomBCEditor(Editor).ProcessCommand(ecDeleteChar);
        FSendToEditor := False;
      end;
  end;
  Key := 0;
  Invalidate;
end;

procedure TBCEditorCompletionProposalPopup.KeyPress(var Key: Char);
begin
  case Key of
    BCEDITOR_CARRIAGE_RETURN:
      begin
        Editor.SetFocus;
      end;
    BCEDITOR_SPACE_CHAR .. High(Char):
      begin
        if not (cpoAutoInvoke in FCompletionProposal.Options) then
          if TCustomBCEditor(Editor).IsWordBreakChar(Key) and Assigned(FOnValidate) then
            if Key = BCEDITOR_SPACE_CHAR then
              FOnValidate(Self, [], BCEDITOR_NONE_CHAR);
        CurrentString := FCurrentString + Key;
        if (cpoAutoInvoke in FCompletionProposal.Options) and (Length(FItemIndexArray) = 0) or
          (Pos(Key, FCompletionProposal.CloseChars) <> 0) then
          Editor.SetFocus
        else
        if Assigned(OnKeyPress) then
          OnKeyPress(Self, Key);
      end;
    BCEDITOR_BACKSPACE_CHAR:
      TCustomBCEditor(Editor).ProcessCommand(ecChar, TBCEditorCommandDataChar.Create(Key));
  end;
  if (FSendToEditor) then
    PostMessage(TCustomBCEditor(Editor).Handle, WM_CHAR, WParam(Key), 0);
  Invalidate;
end;

procedure TBCEditorCompletionProposalPopup.MouseDown(AButton: TMouseButton; AShift: TShiftState; X, Y: Integer);
begin
  FSelectedLine := Max(0, TopLine + ((Y - FTitleHeight) div FItemHeight));
  inherited MouseDown(AButton, AShift, X, Y);
  Refresh;
end;

procedure TBCEditorCompletionProposalPopup.MouseWheel(AShift: TShiftState; AWheelDelta: Integer; AMousePos: TPoint);
var
  LLinesToScroll: Integer;
begin
  if csDesigning in ComponentState then
    Exit;

  if ssCtrl in aShift then
    LLinesToScroll := GetVisibleLines
  else
    LLinesToScroll := 1;

  if AWheelDelta > 0 then
    TopLine := Max(0, TopLine - LLinesToScroll)
  else
    TopLine := Min(GetItems.Count - GetVisibleLines, TopLine + LLinesToScroll);

  Invalidate;
end;

procedure TBCEditorCompletionProposalPopup.MoveSelectedLine(ALineCount: Integer);
begin
  FSelectedLine := Min(Max(FSelectedLine + ALineCount, 0), Length(FItemIndexArray) - 1);
  if FSelectedLine >= TopLine + GetVisibleLines then
    TopLine := FSelectedLine - GetVisibleLines + 1;
  if FSelectedLine < TopLine then
    TopLine := FSelectedLine;
end;

procedure TBCEditorCompletionProposalPopup.Paint();
var
  LColumn: TBCEditorCompletionProposalColumns.TColumn;
  LColumnIndex: Integer;
  LColumnWidth: Integer;
  LIndex: Integer;
  LItemIndex: Integer;
  LLeft: Integer;
  LRect: TRect;
begin
  with FDoubleBufferBitmap do
  begin
    Canvas.Brush.Color := TCustomBCEditor(FEditor).Color;
    Height := 0;
    Width := ClientWidth;
    Height := ClientHeight;
    { Title }
    LRect := ClientRect;
    LRect.Height := FItemHeight;
    LColumnWidth := 0;
    if FTitleVisible then
    begin
      LRect.Height := FTitleHeight;
      for LColumnIndex := 0 to FCompletionProposal.Columns.Count - 1 do
      begin
        LColumn := FCompletionProposal.Columns[LColumnIndex];
        if (LColumn.Visible) then
        begin
          LColumn := FCompletionProposal.Columns[LColumnIndex];
          Canvas.Brush.Color := LColumn.Title.Colors.Background;
          LRect.Left := LColumnWidth;
          LRect.Right := LColumnWidth + LColumn.Width;
          ExtTextOut(Canvas.Handle, 0, 0, ETO_OPAQUE, LRect, '', 0, nil);
          Canvas.Font.Assign(LColumn.Title.Font);
          if LColumn.Title.Visible then
            Canvas.TextOut(FMargin + LColumnWidth, 0, LColumn.Title.Caption);
          Canvas.Pen.Color := LColumn.Title.Colors.BottomBorder;
          Canvas.MoveTo(LRect.Left, LRect.Bottom - 1);
          Canvas.LineTo(LRect.Right, LRect.Bottom - 1);
          Canvas.Pen.Color := LColumn.Title.Colors.RightBorder;
          Canvas.MoveTo(LRect.Right - 1, LRect.Top - 1);
          Canvas.LineTo(LRect.Right - 1, LRect.Bottom - 1);
          LColumnWidth := LColumnWidth + LColumn.Width;
        end;
        LRect.Right := ClientRect.Right;
        LRect.Left := 0;
        LRect.Top := LRect.Bottom;
        LRect.Bottom := LRect.Top + FItemHeight;
      end;
    end;
    { Items }
    for LIndex := 0 to Min(GetVisibleLines(), Length(FItemIndexArray) - 1) do
    begin
      if LIndex + TopLine = FSelectedLine then
        Canvas.Brush.Color := TCustomBCEditor(FEditor).Colors.Selection.Background
      else
        Canvas.Brush.Color := TCustomBCEditor(FEditor).Color;
      Canvas.FillRect(LRect);

      LColumnWidth := 0;
      for LColumnIndex := 0 to FCompletionProposal.Columns.Count - 1 do
      begin
        LItemIndex := FItemIndexArray[TopLine + LIndex];
        LColumn := FCompletionProposal.Columns[LColumnIndex];
        if (LColumn.Visible) then
        begin
          Canvas.Font.Assign(LColumn.Font);
          if LIndex + TopLine = FSelectedLine then
            Canvas.Font.Color := TCustomBCEditor(FEditor).Colors.Selection.Foreground
          else
            Canvas.Font.Color := TCustomBCEditor(FEditor).Font.Color;

          if LItemIndex < LColumn.Items.Count then
          begin
            LLeft := 0;
            if LColumn.Items[LItemIndex].ImageIndex <> -1 then
            begin
              FCompletionProposal.Images.Draw(Canvas, FMargin + LColumnWidth, LRect.Top, LColumn.Items[LItemIndex].ImageIndex);
              Inc(LLeft, FCompletionProposal.Images.Width + FMargin);
            end;
            Canvas.TextOut(FMargin + LColumnWidth + LLeft, LRect.Top, LColumn.Items[LItemIndex].Value);
          end;

          LColumnWidth := LColumnWidth + LColumn.Width;
        end;
      end;
      Inc(LRect.Top, FItemHeight);
      Inc(LRect.Bottom, FItemHeight);
    end;
  end;
  Canvas.Draw(0, 0, FDoubleBufferBitmap);
end;

procedure TBCEditorCompletionProposalPopup.SetCurrentString(const AValue: string);

  function MatchItem(AIndex: Integer): Boolean;
  var
    LCompareString: string;
  begin
    LCompareString := Copy(GetItems[AIndex].Value, 1, Length(AValue));

    if FCaseSensitive then
      Result := CompareStr(LCompareString, AValue) = 0
    else
      Result := AnsiCompareText(LCompareString, AValue) = 0;
  end;

  procedure RecalcList();
  var
    LIndex: Integer;
    LIndex2: Integer;
    LItemsCount: Integer;
  begin
    LIndex2 := 0;
    LItemsCount := GetItems.Count;
    SetLength(FItemIndexArray, 0);
    SetLength(FItemIndexArray, LItemsCount);
    for LIndex := 0 to LItemsCount - 1 do
      if MatchItem(LIndex) then
      begin
        FItemIndexArray[LIndex2] := LIndex;
        Inc(LIndex2);
      end;
    SetLength(FItemIndexArray, LIndex2);
  end;

var
  LIndex: Integer;
begin
  FCurrentString := AValue;

  if FFiltered then
  begin
    RecalcList;
    TopLine := 0;
    Repaint;
  end
  else
  begin
    LIndex := 0;
    while (LIndex < Items.Count) and (not MatchItem(LIndex)) do
      Inc(LIndex);

    if LIndex < Items.Count then
      TopLine := LIndex
    else
      TopLine := 0;
  end;
end;

procedure TBCEditorCompletionProposalPopup.SetOriginalSize();
begin
  FOriginalHeight := Height;
  FOriginalWidth := Width;
end;

procedure TBCEditorCompletionProposalPopup.SetPopupParent(Value: TCustomForm);
begin
  if (Value <> FPopupParent) then
  begin
    if FPopupParent <> nil then
      FPopupParent.RemoveFreeNotification(Self);
    FPopupParent := Value;
    if Value <> nil then
      Value.FreeNotification(Self);
    if HandleAllocated and not (csDesigning in ComponentState) then
      RecreateWnd;
  end;
end;

procedure TBCEditorCompletionProposalPopup.SetTopLine(const AValue: Integer);
begin
  if TopLine <> AValue then
  begin
    FTopLine := AValue;
    UpdateScrollBar;
    Invalidate;
  end;
end;

procedure TBCEditorCompletionProposalPopup.Show(Origin: TPoint);
var
  LScreen: TPoint;
begin
  LScreen := FEditor.ClientToScreen(Origin);
  SetBounds(LScreen.X, LScreen.Y, Width, Height);

  SetWindowPos(Handle, HWND_TOP, 0, 0, 0, 0, SWP_SHOWWINDOW or SWP_NOSIZE or SWP_NOMOVE or SWP_NOZORDER);

  Visible := True;
end;

procedure TBCEditorCompletionProposalPopup.UpdateScrollBar();
var
  LScrollInfo: TScrollInfo;
begin
  LScrollInfo.cbSize := SizeOf(ScrollInfo);
  LScrollInfo.fMask := SIF_ALL;
  LScrollInfo.fMask := LScrollInfo.fMask or SIF_DISABLENOSCROLL;

  if Visible then
    SendMessage(Handle, WM_SETREDRAW, 0, 0);

  LScrollInfo.nMin := 0;
  LScrollInfo.nMax := Max(0, GetItems.Count - 2);
  LScrollInfo.nPage := GetVisibleLines;
  LScrollInfo.nPos := TopLine;

  ShowScrollBar(Handle, SB_VERT, (LScrollInfo.nMin = 0) or (LScrollInfo.nMax > GetVisibleLines));
  SetScrollInfo(Handle, SB_VERT, LScrollInfo, True);

  if GetItems.Count <= GetVisibleLines then
    EnableScrollBar(Handle, SB_VERT, ESB_DISABLE_BOTH)
  else
  begin
    EnableScrollBar(Handle, SB_VERT, ESB_ENABLE_BOTH);
    if TopLine <= 0 then
      EnableScrollBar(Handle, SB_VERT, ESB_DISABLE_UP)
    else
    if TopLine + GetVisibleLines >= GetItems.Count then
      EnableScrollBar(Handle, SB_VERT, ESB_DISABLE_DOWN);
  end;

  if Visible then
    SendMessage(Handle, WM_SETREDRAW, -1, 0);
end;

procedure TBCEditorCompletionProposalPopup.WMActivate(var Msg: TWMActivate);
begin
  if ((Msg.Active <> WA_INACTIVE) and Assigned(PopupParent)) then
    SendMessage(PopupParent.Handle, WM_NCACTIVATE, WPARAM(TRUE), 0);

  inherited;

  if Msg.Active = WA_INACTIVE then
    Hide();
end;

procedure TBCEditorCompletionProposalPopup.WMEraseBkgnd(var AMessage: TMessage);
begin
  AMessage.Result := 1;
end;

procedure TBCEditorCompletionProposalPopup.WMVScroll(var AMessage: TWMScroll);
begin
  Invalidate;
  AMessage.Result := 0;

  case AMessage.ScrollCode of
    SB_TOP:
      TopLine := 0;
    SB_BOTTOM:
      TopLine := GetItems.Count - 1;
    SB_LINEDOWN:
      TopLine := Min(GetItems.Count - GetVisibleLines, TopLine + 1);
    SB_LINEUP:
      TopLine := Max(0, TopLine - 1);
    SB_PAGEDOWN:
      TopLine := Min(GetItems.Count - GetVisibleLines, TopLine + GetVisibleLines);
    SB_PAGEUP:
      TopLine := Max(0, TopLine - GetVisibleLines);
    SB_THUMBPOSITION, SB_THUMBTRACK:
      TopLine := AMessage.Pos;
  end;
  Invalidate;
end;

end.

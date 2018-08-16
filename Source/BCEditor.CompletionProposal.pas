unit BCEditor.CompletionProposal;

interface {********************************************************************}

uses
  Messages,
  Classes, Types, Generics.Collections,
  Forms, Controls, Graphics, StdCtrls,
  BCEditor.Properties, BCEditor.Types;

type
  TBCEditorCompletionProposalPopup = class(TCustomControl)
  strict private
    FCaseSensitive: Boolean;
    FCompletionStartChar: Integer;
    FCurrentString: string;
    FEditor: TCustomControl;
    FFiltered: Boolean;
    FMargin: Integer;
    FLineHeight: Integer;
    FOnHide: TBCEditorCompletionProposal.THideEvent;
    FPopupParent: TCustomForm;
    FSelectedLine: Integer;
    FSendKeyToEditor: Boolean;
    FTopLine: Integer;
    FVisibleItems: TList<Integer>;
    function GetItems(): TBCEditorCompletionProposal.TColumn.TItems;
    function GetVisibleLines(): Integer;
    procedure HandleDblClick(ASender: TObject);
    procedure MoveSelectedLine(ALineCount: Integer);
    procedure SetCurrentString(const AValue: string);
    procedure SetPopupParent(const AValue: TCustomForm);
    procedure SetTopLine(const AValue: Integer);
    procedure UpdateScrollBar();
    procedure WMActivate(var AMessage: TWMActivate); message WM_ACTIVATE;
    procedure WMEraseBkgnd(var AMessage: TMessage); message WM_ERASEBKGND;
    procedure WMVScroll(var AMessage: TWMScroll); message WM_VSCROLL;
  protected
    procedure CreateParams(var Params: TCreateParams); override;
    function GetCurrentInput(): string;
    procedure Hide();
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure KeyPress(var Key: Char); override;
    procedure MouseDown(AButton: TMouseButton; AShift: TShiftState; X, Y: Integer); override;
    procedure Paint(); override;
    procedure Show(Origin: TPoint);
    property OnClose: TBCEditorCompletionProposal.THideEvent read FOnHide write FOnHide;
    property Items: TBCEditorCompletionProposal.TColumn.TItems read GetItems;
    property PopupParent: TCustomForm read FPopupParent write SetPopupParent;
  public
    constructor Create(const AEditor: TCustomControl); reintroduce;
    destructor Destroy(); override;
    procedure Execute(const ACurrentString: string; const APoint: TPoint);
  end;

implementation {***************************************************************}

uses
  Windows,
  SysUtils, UITypes, Math,
  Themes,
  BCEditor, BCEditor.Consts, BCEditor.Commands;

type
  TCustomBCEditor = class(BCEditor.TCustomBCEditor);

{ TBCEditorCompletionProposalPopup ********************************************}

constructor TBCEditorCompletionProposalPopup.Create(const AEditor: TCustomControl);
begin
  inherited Create(AEditor);

  ControlStyle := ControlStyle + [csNoDesignVisible, csReplicatable];

  FEditor := AEditor;
  Width := TCustomBCEditor(FEditor).CompletionProposal.Width;

  Ctl3D := False;
  FCaseSensitive := False;
  FFiltered := False;
  FLineHeight := 0;
  FVisibleItems := TList<Integer>.Create();
  FMargin := GetSystemMetrics(SM_CXSIZEFRAME) div 2;
  FOnHide := nil;
  FPopupParent := nil;
  Visible := False;

  OnDblClick := HandleDblClick;
end;

procedure TBCEditorCompletionProposalPopup.CreateParams(var Params: TCreateParams);
begin
  inherited;

  Params.Style := WS_POPUP or WS_BORDER;
  if (cpoResizeable in TCustomBCEditor(FEditor).CompletionProposal.Options) then
    Params.Style := Params.Style or WS_SIZEBOX;
  Params.WindowClass.Style := Params.WindowClass.Style or CS_DROPSHADOW;
  if (TCustomBCEditor(FEditor).DoubleBuffered and not (csDesigning in ComponentState)) then
    Params.ExStyle := Params.ExStyle or WS_EX_COMPOSITED;

  if (Assigned(PopupParent)) then
    Params.WndParent := PopupParent.Handle;
end;

destructor TBCEditorCompletionProposalPopup.Destroy();
begin
  if (FLineHeight <> 0) then
  begin
    TCustomBCEditor(FEditor).CompletionProposal.Lines := ClientHeight div FLineHeight;
    TCustomBCEditor(FEditor).CompletionProposal.Width := Width;
  end;

  FVisibleItems.Free();

  inherited;
end;

procedure TBCEditorCompletionProposalPopup.Execute(const ACurrentString: string; const APoint: TPoint);
var
  LPoint: TPoint;

  procedure CalculateFormPlacement();
  begin
    Canvas.Font.Assign(TCustomBCEditor(FEditor).Font);
    LPoint.X := APoint.X - Canvas.TextWidth(ACurrentString);
    LPoint.Y := APoint.Y;

    ClientHeight := FLineHeight * TCustomBCEditor(FEditor).CompletionProposal.Lines + 2;

    if (LPoint.X + ClientWidth > Screen.DesktopWidth) then
      LPoint.X := Max(0, Screen.DesktopWidth - ClientWidth - 5);

    if (LPoint.Y + ClientHeight > Screen.DesktopHeight) then
      LPoint.Y := Max(0, LPoint.Y - ClientHeight - TCustomBCEditor(FEditor).RowHeight - 2);
  end;

  procedure CalculateColumnWidths();
  var
    LAutoWidthCount: Integer;
    LColumnIndex: Integer;
    LIndex: Integer;
    LItems: TBCEditorCompletionProposal.TColumn.TItems;
    LMaxWidth: Integer;
    LProposalColumn: TBCEditorCompletionProposal.TColumn;
    LTempWidth: Integer;
    LVisibleColumnCount: Integer;
    LWidthSum: Integer;
  begin
    LVisibleColumnCount := 0;
    for LColumnIndex := 0 to TCustomBCEditor(FEditor).CompletionProposal.Columns.Count - 1 do
      if (TCustomBCEditor(FEditor).CompletionProposal.Columns[LColumnIndex].Visible) then
        Inc(LVisibleColumnCount);

    if (LVisibleColumnCount = 1) then
    begin
      LProposalColumn := nil; // Hide compiler warning only.
      for LColumnIndex := 0 to TCustomBCEditor(FEditor).CompletionProposal.Columns.Count - 1 do
        if (TCustomBCEditor(FEditor).CompletionProposal.Columns[LColumnIndex].Visible) then
          LProposalColumn := TCustomBCEditor(FEditor).CompletionProposal.Columns[LColumnIndex];
      if (LProposalColumn.AutoWidth) then
        LProposalColumn.Width := Width;
      Exit;
    end;

    LAutoWidthCount := 0;
    LWidthSum := 0;
    for LColumnIndex := 0 to TCustomBCEditor(FEditor).CompletionProposal.Columns.Count - 1 do
    begin
      LProposalColumn := TCustomBCEditor(FEditor).CompletionProposal.Columns[LColumnIndex];
      Canvas.Font.Assign(LProposalColumn.Font);
      if (LProposalColumn.Visible and LProposalColumn.AutoWidth) then
      begin
        LItems := LProposalColumn.Items;
        LMaxWidth := 0;
        for LIndex := 0 to LItems.Count - 1 do
        begin
          LTempWidth := Canvas.TextWidth(LItems[LIndex].Value);
          if (LTempWidth > LMaxWidth) then
            LMaxWidth := LTempWidth;
        end;
        LProposalColumn.Width := LMaxWidth;
        LWidthSum := LWidthSum + LMaxWidth;
        Inc(LAutoWidthCount);
      end;
    end;

    LMaxWidth := (Width - LWidthSum - GetSystemMetrics(SM_CYHSCROLL)) div LAutoWidthCount;
    if (LMaxWidth > 0) then
      for LColumnIndex := 0 to TCustomBCEditor(FEditor).CompletionProposal.Columns.Count - 1 do
      begin
        LProposalColumn := TCustomBCEditor(FEditor).CompletionProposal.Columns[LColumnIndex];
        if (LProposalColumn.Visible and LProposalColumn.AutoWidth) then
          LProposalColumn.Width := LProposalColumn.Width + LMaxWidth;
      end;
  end;

  function CalculateLineHeight(): Integer;
  var
    LIndex: Integer;
  begin
    Result := 0;
    for LIndex := 0 to TCustomBCEditor(FEditor).CompletionProposal.Columns.Count - 1 do
    begin
      Canvas.Font.Assign(TCustomBCEditor(FEditor).CompletionProposal.Columns[LIndex].Font);
      Result := Max(Result, Canvas.TextHeight('X'));
    end;
  end;

  procedure SetAutoConstraints();
  begin
    if cpoAutoConstraints in TCustomBCEditor(FEditor).CompletionProposal.Options then
    begin
      TCustomBCEditor(FEditor).CompletionProposal.Constraints.MinHeight := Height;
      TCustomBCEditor(FEditor).CompletionProposal.Constraints.MinWidth := Width;
      Constraints.Assign(TCustomBCEditor(FEditor).CompletionProposal.Constraints);
    end;
  end;

var
  LCount: Integer;
  LIndex: Integer;
begin
  FCaseSensitive := cpoCaseSensitive in TCustomBCEditor(FEditor).CompletionProposal.Options;
  FFiltered := cpoFiltered in TCustomBCEditor(FEditor).CompletionProposal.Options;

  LCount := GetItems().Count;
  FVisibleItems.Clear();
  for LIndex := 0 to LCount - 1 do
    FVisibleItems.Add(LIndex);
  UpdateScrollBar();

  if (FVisibleItems.Count > 0) then
  begin
    FLineHeight := CalculateLineHeight();
    CalculateFormPlacement();
    CalculateColumnWidths();
    SetAutoConstraints();
    SetCurrentString(ACurrentString);
    if (FVisibleItems.Count > 0) then
    begin
      UpdateScrollBar();
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

  if (TCustomBCEditor(FEditor).Lines.Count > 0) then
  begin
    LTextCaretPosition := TCustomBCEditor(FEditor).CaretPos;

    LLineText := TCustomBCEditor(FEditor).Lines[Min(LTextCaretPosition.Line, TCustomBCEditor(FEditor).Lines.Count - 1)];
    LChar := LTextCaretPosition.Char;
    if (LChar <= Length(LLineText)) then
    begin
      while ((LChar > 0) and not TCustomBCEditor(FEditor).IsWordBreakChar(LLineText[1 + LChar - 1])) do
        Dec(LChar);

      FCompletionStartChar := LChar;
      Result := Copy(LLineText, 1 + FCompletionStartChar, LTextCaretPosition.Char - FCompletionStartChar);
    end
    else
    begin
      FCompletionStartChar := LTextCaretPosition.Char;
    end;
  end;
end;

function TBCEditorCompletionProposalPopup.GetItems(): TBCEditorCompletionProposal.TColumn.TItems;
begin
  Result := nil;
  if (TCustomBCEditor(FEditor).CompletionProposal.CompletionColumnIndex < TCustomBCEditor(FEditor).CompletionProposal.Columns.Count) then
    Result := TCustomBCEditor(FEditor).CompletionProposal.Columns[TCustomBCEditor(FEditor).CompletionProposal.CompletionColumnIndex].Items;
end;

function TBCEditorCompletionProposalPopup.GetVisibleLines(): Integer;
begin
  Result := ClientHeight div FLineHeight;
end;

procedure TBCEditorCompletionProposalPopup.HandleDblClick(ASender: TObject);
begin
  if (FSelectedLine < FVisibleItems.Count) then
  begin
    TCustomBCEditor(FEditor).Lines.BeginUpdate();
    try
      if (FCompletionStartChar < TCustomBCEditor(FEditor).CaretPos.X) then
      begin
        TCustomBCEditor(FEditor).ProcessCommand(ecWordLeft);
        TCustomBCEditor(FEditor).ProcessCommand(ecDeleteWord);
      end
      else if (TCustomBCEditor(FEditor).WordAt[TCustomBCEditor(FEditor).CaretPos] <> '') then
      begin
        TCustomBCEditor(FEditor).ProcessCommand(ecSelWordRight);
        TCustomBCEditor(FEditor).ProcessCommand(ecDeleteChar);
      end;
      if (FVisibleItems[FSelectedLine] < GetItems().Count) then
        TCustomBCEditor(FEditor).ProcessCommand(ecText, TBCEditorCommandDataText.Create(GetItems()[FVisibleItems[FSelectedLine]].Value));
    finally
      TCustomBCEditor(FEditor).Lines.EndUpdate();
    end;
  end;
end;

procedure TBCEditorCompletionProposalPopup.Hide();
begin
  if (Assigned(FOnHide)) then
    FOnHide(FEditor);

  SetWindowPos(Handle, 0, 0, 0, 0, 0, SWP_HIDEWINDOW or SWP_NOSIZE or SWP_NOMOVE or SWP_NOZORDER);
  Visible := False;
end;

procedure TBCEditorCompletionProposalPopup.KeyDown(var Key: Word; Shift: TShiftState);
var
  LChar: Char;
  LTextCaretPosition: TBCEditorLinesPosition;
begin
  FSendKeyToEditor := False;
  case (Key) of
    VK_TAB,
    VK_RETURN:
      HandleDblClick(Self);
    VK_ESCAPE:
      FEditor.SetFocus();
    VK_LEFT:
      if Length(FCurrentString) > 0 then
      begin
        SetCurrentString(Copy(FCurrentString, 1, Length(FCurrentString) - 1));
        TCustomBCEditor(FEditor).ProcessCommand(ecLeft);
      end
      else
      begin
        TCustomBCEditor(FEditor).ProcessCommand(ecLeft);
        FEditor.SetFocus();
      end;
    VK_RIGHT:
      begin
        LTextCaretPosition := TCustomBCEditor(FEditor).CaretPos;
        if LTextCaretPosition.Char < Length(TCustomBCEditor(FEditor).Lines[LTextCaretPosition.Line]) then
          LChar := TCustomBCEditor(FEditor).Lines[LTextCaretPosition.Line][1 + LTextCaretPosition.Char]
        else
          LChar := BCEDITOR_SPACE_CHAR;

        if (not TCustomBCEditor(FEditor).IsWordBreakChar(LChar)) then
          SetCurrentString(FCurrentString + LChar)
        else
          FEditor.SetFocus();

        TCustomBCEditor(FEditor).ProcessCommand(ecRight);
      end;
    VK_PRIOR:
      MoveSelectedLine(-GetVisibleLines());
    VK_NEXT:
      MoveSelectedLine(GetVisibleLines());
    VK_END:
      SetTopLine(FVisibleItems.Count - 1);
    VK_HOME:
      SetTopLine(0);
    VK_UP:
      if (ssCtrl in Shift) then
        FSelectedLine := 0
      else
        MoveSelectedLine(-1);
    VK_DOWN:
      if (ssCtrl in Shift) then
        FSelectedLine := FVisibleItems.Count - 1
      else
        MoveSelectedLine(1);
    VK_BACK:
      if (Shift = []) then
        if (Length(FCurrentString) > 0) then
        begin
          SetCurrentString(Copy(FCurrentString, 1, Length(FCurrentString) - 1));

          TCustomBCEditor(FEditor).ProcessCommand(ecBackspace);
        end
        else
        begin
          TCustomBCEditor(FEditor).ProcessCommand(ecBackspace);
          FEditor.SetFocus();
        end;
    VK_DELETE:
      TCustomBCEditor(FEditor).ProcessCommand(ecDeleteChar);
    else
      FSendKeyToEditor := True;
  end;
  Key := 0;
  Invalidate();
end;

procedure TBCEditorCompletionProposalPopup.KeyPress(var Key: Char);
begin
  case (Key) of
    BCEDITOR_CARRIAGE_RETURN:
      FEditor.SetFocus();
    BCEDITOR_SPACE_CHAR .. High(Char):
      begin
        SetCurrentString(FCurrentString + Key);
        if ((cpoAutoInvoke in TCustomBCEditor(FEditor).CompletionProposal.Options) and (FVisibleItems.Count = 0)
          or (Pos(Key, TCustomBCEditor(FEditor).CompletionProposal.CloseChars) <> 0)) then
          FEditor.SetFocus()
        else if (Assigned(OnKeyPress)) then
          OnKeyPress(Self, Key);
      end;
  end;
  if (FSendKeyToEditor) then
    TCustomBCEditor(FEditor).ProcessCommand(ecChar, TBCEditorCommandDataChar.Create(Key));
  Invalidate();
end;

procedure TBCEditorCompletionProposalPopup.MouseDown(AButton: TMouseButton; AShift: TShiftState; X, Y: Integer);
begin
  FSelectedLine := Max(0, FTopLine + Y div FLineHeight);

  inherited;

  Invalidate();
end;

procedure TBCEditorCompletionProposalPopup.MoveSelectedLine(ALineCount: Integer);
begin
  FSelectedLine := Min(Max(FSelectedLine + ALineCount, 0), FVisibleItems.Count - 1);
  if (FSelectedLine >= FTopLine + GetVisibleLines()) then
    SetTopLine(FSelectedLine - GetVisibleLines() + 1);
  if (FSelectedLine < FTopLine) then
    SetTopLine(FSelectedLine);
end;

procedure TBCEditorCompletionProposalPopup.Paint();
var
  LColumn: TBCEditorCompletionProposal.TColumn;
  LColumnIndex: Integer;
  LColumnWidth: Integer;
  LItemIndex: Integer;
  LLeft: Integer;
  LLineIndex: Integer;
  LRect: TRect;
begin
  LRect := ClientRect;
  LRect.Height := FLineHeight;
  for LLineIndex := 0 to Min(GetVisibleLines() - 1, FVisibleItems.Count - 1) do
  begin
    if (FTopLine + LLineIndex = FSelectedLine) then
      Canvas.Brush.Color := TCustomBCEditor(FEditor).Colors.Selection.Background
    else
      Canvas.Brush.Color := TCustomBCEditor(FEditor).Color;
    Canvas.FillRect(LRect);

    LColumnWidth := 0;
    for LColumnIndex := 0 to TCustomBCEditor(FEditor).CompletionProposal.Columns.Count - 1 do
    begin
      LColumn := TCustomBCEditor(FEditor).CompletionProposal.Columns[LColumnIndex];
      LItemIndex := FVisibleItems[FTopLine + LLineIndex];
      if (LColumn.Visible) then
      begin
        if (LItemIndex < LColumn.Items.Count) then
        begin
          Canvas.Font.Assign(LColumn.Font);
          if (FTopLine + LLineIndex = FSelectedLine) then
            Canvas.Font.Color := TCustomBCEditor(FEditor).Colors.Selection.Foreground
          else
            Canvas.Font.Color := TCustomBCEditor(FEditor).Font.Color;

          LLeft := 0;
          if (LColumn.Items[LItemIndex].ImageIndex >= 0) then
          begin
            TCustomBCEditor(FEditor).CompletionProposal.Images.Draw(Canvas, FMargin + LColumnWidth, LRect.Top, LColumn.Items[LItemIndex].ImageIndex);
            Inc(LLeft, TCustomBCEditor(FEditor).CompletionProposal.Images.Width + FMargin);
          end;
          Canvas.TextOut(FMargin + LColumnWidth + LLeft, LRect.Top, LColumn.Items[LItemIndex].Value);
        end;

        Inc(LColumnWidth, LColumn.Width);
      end;
    end;
    Inc(LRect.Top, FLineHeight);
    Inc(LRect.Bottom, FLineHeight);
  end;
  LRect.Bottom := Height;
  if (LRect.Height > 0) then
  begin
    Canvas.Brush.Color := TCustomBCEditor(FEditor).Color;
    Canvas.FillRect(LRect);
  end;
end;

procedure TBCEditorCompletionProposalPopup.SetCurrentString(const AValue: string);

  function MatchItem(AIndex: Integer): Boolean;
  var
    LCompareString: string;
  begin
    LCompareString := Copy(GetItems[AIndex].Value, 1, Length(AValue));

    if (FCaseSensitive) then
      Result := CompareStr(LCompareString, AValue) = 0
    else
      Result := AnsiCompareText(LCompareString, AValue) = 0;
  end;

  procedure RecalcList();
  var
    LIndex: Integer;
    LItemsCount: Integer;
  begin
    LItemsCount := GetItems().Count;
    FVisibleItems.Clear();
    for LIndex := 0 to LItemsCount - 1 do
      if (MatchItem(LIndex)) then
        FVisibleItems.Add(LIndex);
    UpdateScrollBar();
  end;

var
  LIndex: Integer;
begin
  FCurrentString := AValue;

  if (FFiltered) then
  begin
    RecalcList();
    SetTopLine(0);
  end
  else
  begin
    LIndex := 0;
    while (LIndex < Items.Count) and (not MatchItem(LIndex)) do
      Inc(LIndex);

    if (LIndex < Items.Count) then
      SetTopLine(LIndex)
    else
      SetTopLine(0);
  end;
end;

procedure TBCEditorCompletionProposalPopup.SetPopupParent(const AValue: TCustomForm);
begin
  if (AValue <> FPopupParent) then
  begin
    if (Assigned(FPopupParent)) then
      FPopupParent.RemoveFreeNotification(Self);
    FPopupParent := AValue;
    if (Assigned(AValue)) then
      AValue.FreeNotification(Self);
    if (HandleAllocated and not (csDesigning in ComponentState)) then
      RecreateWnd();
  end;
end;

procedure TBCEditorCompletionProposalPopup.SetTopLine(const AValue: Integer);
begin
  if (AValue <> FTopLine) then
  begin
    FTopLine := AValue;
    UpdateScrollBar();
    Invalidate();
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
  LScrollInfo: SCROLLINFO;
begin
  if (FLineHeight > 0) then
  begin
    if (Visible) then
      SendMessage(Handle, WM_SETREDRAW, 0, 0);

    LScrollInfo.cbSize := SizeOf(LScrollInfo);
    LScrollInfo.fMask := SIF_ALL;
    LScrollInfo.nMin := 0;
    LScrollInfo.nMax := Max(0, FVisibleItems.Count - 1);
    LScrollInfo.nPage := GetVisibleLines();
    LScrollInfo.nPos := FTopLine;
    SetScrollInfo(Handle, SB_VERT, LScrollInfo, True);
    EnableScrollBar(Handle, SB_VERT, ESB_ENABLE_BOTH);
    if (LScrollInfo.nPos = 0) then
      EnableScrollBar(Handle, SB_VERT, ESB_DISABLE_UP);
    if (LScrollInfo.nPos = LScrollInfo.nMax - GetVisibleLines() + 1) then
      EnableScrollBar(Handle, SB_VERT, ESB_DISABLE_DOWN);

    if (Visible) then
      SendMessage(Handle, WM_SETREDRAW, -1, 0);
  end;
end;

procedure TBCEditorCompletionProposalPopup.WMActivate(var AMessage: TWMActivate);
begin
  if ((AMessage.Active <> WA_INACTIVE) and Assigned(PopupParent)) then
    SendMessage(PopupParent.Handle, WM_NCACTIVATE, WPARAM(TRUE), 0);

  inherited;

  if (AMessage.Active = WA_INACTIVE) then
    Hide();
end;

procedure TBCEditorCompletionProposalPopup.WMEraseBkgnd(var AMessage: TMessage);
begin
  AMessage.Result := 1;
end;

procedure TBCEditorCompletionProposalPopup.WMVScroll(var AMessage: TWMScroll);
begin
  case AMessage.ScrollCode of
    SB_TOP:
      SetTopLine(0);
    SB_BOTTOM:
      SetTopLine(GetItems().Count - 1);
    SB_LINEDOWN:
      SetTopLine(Min(GetItems().Count - GetVisibleLines(), FTopLine + 1));
    SB_LINEUP:
      SetTopLine(Max(0, FTopLine - 1));
    SB_PAGEDOWN:
      SetTopLine(Min(GetItems().Count - GetVisibleLines(), FTopLine + GetVisibleLines()));
    SB_PAGEUP:
      SetTopLine(Max(0, FTopLine - GetVisibleLines()));
    SB_THUMBPOSITION, SB_THUMBTRACK:
      SetTopLine(AMessage.Pos);
  end;

  AMessage.Result := 0;
end;

end.

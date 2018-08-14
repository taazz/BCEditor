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
    FAdjustCompletionStart: Boolean;
    FDoubleBufferBitmap: Graphics.TBitmap;
    FCaseSensitive: Boolean;
    FCompletionProposal: TBCEditorCompletionProposal;
    FCompletionStartChar: Integer;
    FCurrentString: string;
    FEditor: TCustomControl;
    FFiltered: Boolean;
    FItemHeight: Integer;
    FVisibleItems: TList<Integer>;
    FItems: TStrings;
    FMargin: Integer;
    FOnClose: TBCEditorCompletionProposalCloseEvent;
    FOnValidate: TBCEditorCompletionProposalValidateEvent;
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
    property OnClose: TBCEditorCompletionProposalCloseEvent read FOnClose write FOnClose;
    property OnValidate: TBCEditorCompletionProposalValidateEvent read FOnValidate write FOnValidate;
    property Items: TBCEditorCompletionProposalItems read GetItems;
    property PopupParent: TCustomForm read FPopupParent write SetPopupParent;
  public
    constructor Create(const AEditor: TCustomControl); reintroduce;
    destructor Destroy(); override;
    procedure Assign(ASource: TPersistent); override;
    procedure Execute(const ACurrentString: string; const APoint: TPoint);
  end;

implementation {***************************************************************}

uses
  Windows,
  SysUtils, UITypes, Math,
  Themes, Dialogs,
  BCEditor, BCEditor.Consts, BCEditor.Commands, BCEditor.Lines;

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
  FVisibleItems := TList<Integer>.Create();
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

  FDoubleBufferBitmap.Free();
  FVisibleItems.Free();
  FItems.Free();

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
      LPoint.Y := LPoint.Y - ClientHeight - TCustomBCEditor(FEditor).RowHeight - 2;
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
    LProposalColumn: TBCEditorCompletionProposalColumn;
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

  function GetTitleVisible(): Boolean;
  var
    LColumn: TBCEditorCompletionProposalColumn;
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

  procedure SetAutoConstraints();
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
  LCount := GetItems().Count;
  FVisibleItems.Clear();
  for LIndex := 0 to LCount - 1 do
    FVisibleItems.Add(LIndex);
  UpdateScrollBar();

  if (FVisibleItems.Count > 0) then
  begin
    FTitleVisible := GetTitleVisible();
    FItemHeight := GetItemHeight();
    FTitleHeight := GetTitleHeight();
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
      FAdjustCompletionStart := False;
      while ((LChar > 0) and not TCustomBCEditor(FEditor).IsWordBreakChar(LLineText[1 + LChar - 1])) do
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
end;

function TBCEditorCompletionProposalPopup.GetItemHeight(): Integer;
var
  LColumn: TBCEditorCompletionProposalColumn;
  LColumnIndex: Integer;
  LHeight: Integer;
begin
  Result := 0;
  for LColumnIndex := 0 to FCompletionProposal.Columns.Count - 1 do
  begin
    LColumn := FCompletionProposal.Columns[LColumnIndex];
    FDoubleBufferBitmap.Canvas.Font.Assign(LColumn.Font);
    LHeight := FDoubleBufferBitmap.Canvas.TextHeight('X');
    if (LHeight > Result) then
      Result := LHeight;
  end;
end;

function TBCEditorCompletionProposalPopup.GetItems(): TBCEditorCompletionProposalItems;
begin
  Result := nil;
  if (FCompletionProposal.CompletionColumnIndex < FCompletionProposal.Columns.Count) then
    Result := FCompletionProposal.Columns[FCompletionProposal.CompletionColumnIndex].Items;
end;

function TBCEditorCompletionProposalPopup.GetTitleHeight(): Integer;
var
  LColumn: TBCEditorCompletionProposalColumn;
  LColumnIndex: Integer;
  LHeight: Integer;
begin
  Result := 0;
  if (FTitleVisible) then
  for LColumnIndex := 0 to FCompletionProposal.Columns.Count - 1 do
  begin
    LColumn := FCompletionProposal.Columns[LColumnIndex];
    FDoubleBufferBitmap.Canvas.Font.Assign(LColumn.Title.Font);
    LHeight := FDoubleBufferBitmap.Canvas.TextHeight('X');
    if (LHeight > Result) then
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
  Hide();
end;

procedure TBCEditorCompletionProposalPopup.HandleOnValidate(ASender: TObject; AShift: TShiftState; AEndToken: Char);
begin
  if (FSelectedLine < FVisibleItems.Count) then
  begin
    TCustomBCEditor(FEditor).Lines.BeginUpdate();
    try
      if (FCompletionStartChar < TCustomBCEditor(FEditor).CaretPos.X) then
      begin
        TCustomBCEditor(FEditor).ProcessCommand(ecSelWordLeft);
        TCustomBCEditor(FEditor).ProcessCommand(ecDeleteWord);
      end;
      TCustomBCEditor(FEditor).ProcessCommand(ecText, TBCEditorCommandDataText.Create(GetItems[FVisibleItems[FSelectedLine]].Value));
    finally
      TCustomBCEditor(FEditor).Lines.EndUpdate();
    end;
  end;
end;

procedure TBCEditorCompletionProposalPopup.Hide();
begin
  SetWindowPos(Handle, 0, 0, 0, 0, 0, SWP_HIDEWINDOW or SWP_NOSIZE or SWP_NOMOVE or SWP_NOZORDER);
  Visible := False;
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
        FEditor.SetFocus;
        FSendToEditor := False;
      end;
    VK_LEFT:
      begin
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
        FSendToEditor := False;
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
        FSendToEditor := False;
      end;
    VK_PRIOR:
      begin
        MoveSelectedLine(-GetVisibleLines());
        FSendToEditor := False;
      end;
    VK_NEXT:
      begin
        MoveSelectedLine(GetVisibleLines());
        FSendToEditor := False;
      end;
    VK_END:
      begin
        SetTopLine(FVisibleItems.Count - 1);
        FSendToEditor := False;
      end;
    VK_HOME:
      begin
        SetTopLine(0);
        FSendToEditor := False;
      end;
    VK_UP:
      begin
        if (ssCtrl in Shift) then
          FSelectedLine := 0
        else
          MoveSelectedLine(-1);
        FSendToEditor := False;
      end;
    VK_DOWN:
      begin
        if (ssCtrl in Shift) then
          FSelectedLine := FVisibleItems.Count - 1
        else
          MoveSelectedLine(1);
        FSendToEditor := False;
      end;
    VK_BACK:
      if (Shift = []) then
      begin
        if Length(FCurrentString) > 0 then
        begin
          SetCurrentString(Copy(FCurrentString, 1, Length(FCurrentString) - 1));

          TCustomBCEditor(FEditor).ProcessCommand(ecBackspace);
        end
        else
        begin
          TCustomBCEditor(FEditor).ProcessCommand(ecBackspace);
          FEditor.SetFocus;
        end;
        FSendToEditor := False;
      end;
    VK_DELETE:
      begin
        TCustomBCEditor(FEditor).ProcessCommand(ecDeleteChar);
        FSendToEditor := False;
      end;
  end;
  Key := 0;
  Invalidate();
end;

procedure TBCEditorCompletionProposalPopup.KeyPress(var Key: Char);
begin
  case Key of
    BCEDITOR_CARRIAGE_RETURN:
      begin
        FEditor.SetFocus();
      end;
    BCEDITOR_SPACE_CHAR .. High(Char):
      begin
        if not (cpoAutoInvoke in FCompletionProposal.Options) then
          if (TCustomBCEditor(FEditor).IsWordBreakChar(Key) and Assigned(FOnValidate) and (Key = BCEDITOR_SPACE_CHAR)) then
            FOnValidate(Self, [], BCEDITOR_NONE_CHAR);
        SetCurrentString(FCurrentString + Key);
        if ((cpoAutoInvoke in FCompletionProposal.Options) and (FVisibleItems.Count = 0)
          or (Pos(Key, FCompletionProposal.CloseChars) <> 0)) then
          FEditor.SetFocus()
        else if (Assigned(OnKeyPress)) then
          OnKeyPress(Self, Key);
      end;
  end;
  if (FSendToEditor) then
    PostMessage(TCustomBCEditor(FEditor).Handle, WM_CHAR, WParam(Key), 0);
  Invalidate();
end;

procedure TBCEditorCompletionProposalPopup.MouseDown(AButton: TMouseButton; AShift: TShiftState; X, Y: Integer);
begin
  FSelectedLine := Max(0, FTopLine + ((Y - FTitleHeight) div FItemHeight));

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
  LColumn: TBCEditorCompletionProposalColumn;
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
    if (FTitleVisible) then
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
    for LIndex := 0 to Min(GetVisibleLines() - 1, FVisibleItems.Count - 1) do
    begin
      if (LIndex + FTopLine = FSelectedLine) then
        Canvas.Brush.Color := TCustomBCEditor(FEditor).Colors.Selection.Background
      else
        Canvas.Brush.Color := TCustomBCEditor(FEditor).Color;
      Canvas.FillRect(LRect);

      LColumnWidth := 0;
      for LColumnIndex := 0 to FCompletionProposal.Columns.Count - 1 do
      begin
        LItemIndex := FVisibleItems[FTopLine + LIndex];
        LColumn := FCompletionProposal.Columns[LColumnIndex];
        if (LColumn.Visible) then
        begin
          Canvas.Font.Assign(LColumn.Font);
          if LIndex + FTopLine = FSelectedLine then
            Canvas.Font.Color := TCustomBCEditor(FEditor).Colors.Selection.Foreground
          else
            Canvas.Font.Color := TCustomBCEditor(FEditor).Font.Color;

          if (LItemIndex < LColumn.Items.Count) then
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

    if LIndex < Items.Count then
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
  if (FItemHeight > 0) then
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

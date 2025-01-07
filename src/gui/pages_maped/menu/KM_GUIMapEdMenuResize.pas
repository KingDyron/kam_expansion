unit KM_GUIMapEdMenuResize;
{$I KaM_Remake.inc}
interface
uses
   Classes, SysUtils,
   KM_Controls, KM_ControlsBase, KM_ControlsEdit;


type
  TKMMapEdMenuResize = class
  private
    fOnDone: TNotifyEvent;
    fOnPageChange: TNotifyEvent;
    fIsMultiplayer: Boolean;

    procedure ResizeRefresh(Sender: TObject);
    procedure PanelConfirm_Switch(Sender: TObject);
    procedure Resize_Click(Sender: TObject);
    procedure Menu_Click(Sender: TObject);
  protected
    Panel_Resize: TKMPanel;
      Panel_Resize_Edit: TKMPanel;
        NumEdit_Resize_Left, NumEdit_Resize_Right,
        NumEdit_Resize_Top, NumEdit_Resize_Bottom: TKMNumericEdit;
        Button_Resize: TKMButton;
        Button_Cancel: TKMButton;
        Label_CurrentMapSize, Label_NewMapSize: TKMLabel;
      Panel_Resize_Confirm: TKMPanel;
        Label_Resize_Confirm: TKMLabel;
        Button_Resize_Confirm_Yes, Button_Resize_Confirm_No: TKMButton;
  public
    constructor Create(aParent: TKMPanel; aOnDone, aOnPageChange: TNotifyEvent);

    procedure SetLoadMode(aMultiplayer: Boolean);
    function Visible: Boolean;
    procedure Show;
    procedure Hide;
  end;


implementation
uses
  KromUtils, Math, KM_Defaults, KM_GameApp, KM_Game, KM_GameParams, KM_Terrain,
  KM_InterfaceGame, KM_ResFonts, KM_RenderUI, KM_Points, KM_Maps, KM_ResTexts, KM_ResTilesetTypes;


{ TKMMapEdMenuSave }
constructor TKMMapEdMenuResize.Create(aParent: TKMPanel; aOnDone, aOnPageChange: TNotifyEvent);
const
  MAX_MAP_DIF = MAX_MAP_SIZE - MIN_MAP_SIZE;

var
  Y: Integer;
begin
  inherited Create;

  fOnDone := aOnDone;
  fOnPageChange := aOnPageChange;

  Panel_Resize := TKMPanel.Create(aParent, 0, 45, aParent.Width, aParent.Height - 45);
  Panel_Resize.Anchors := [anLeft, anTop, anBottom];

    Panel_Resize_Edit := TKMPanel.Create(Panel_Resize, 9, 0, Panel_Resize.Width - 9, Panel_Resize.Height);

      Y := PAGE_TITLE_Y;
      with TKMLabel.Create(Panel_Resize_Edit, 0, Y, Panel_Resize_Edit.Width, 45, gResTexts[TX_MAPED_MAP_RESIZE_TITLE], fntOutline, taCenter) do
        Anchors := [anLeft, anTop, anRight];
      Inc(Y, 45);
      with TKMLabel.Create(Panel_Resize_Edit, 0, Y, Panel_Resize_Edit.Width, 30, gResTexts[TX_MAPED_MAP_RESIZE_MOVE_BORDERS], fntGrey, taCenter) do
        Anchors := [anLeft, anTop, anRight];
      Inc(Y, 25);

      // Use left-top-right-bottom order of creation. Same order will be used for Tab focus change
      NumEdit_Resize_Left   := TKMNumericEdit.Create(Panel_Resize_Edit, 0,   Y+30, -MAX_MAP_DIF, MAX_MAP_DIF);
      NumEdit_Resize_Left.AutoFocusable := False;
      NumEdit_Resize_Top    := TKMNumericEdit.Create(Panel_Resize_Edit, (Panel_Resize_Edit.Width div 2) - 39, Y, -MAX_MAP_DIF, MAX_MAP_DIF);
      NumEdit_Resize_Top.AutoFocusable := False;
      NumEdit_Resize_Top.Anchors := [anLeft, anTop, anRight];
      NumEdit_Resize_Right  := TKMNumericEdit.Create(Panel_Resize_Edit, Panel_Resize_Edit.Width - 78, Y+30, -MAX_MAP_DIF, MAX_MAP_DIF);
      NumEdit_Resize_Right.AutoFocusable := False;
      NumEdit_Resize_Right.Anchors := [anLeft, anTop, anRight];
      NumEdit_Resize_Bottom := TKMNumericEdit.Create(Panel_Resize_Edit, (Panel_Resize_Edit.Width div 2) - 39, Y+60, -MAX_MAP_DIF, MAX_MAP_DIF);
      NumEdit_Resize_Bottom.AutoFocusable := False;
      NumEdit_Resize_Bottom.Anchors := [anLeft, anTop, anRight];

      NumEdit_Resize_Left.OnChange    := ResizeRefresh;
      NumEdit_Resize_Right.OnChange   := ResizeRefresh;
      NumEdit_Resize_Top.OnChange     := ResizeRefresh;
      NumEdit_Resize_Bottom.OnChange  := ResizeRefresh;

      Inc(Y, 90);

      Label_CurrentMapSize := TKMLabel.Create(Panel_Resize_Edit, 0, Y, TB_WIDTH, 30, '', fntOutline, taCenter);
      Inc(Y, 45);
      Label_NewMapSize := TKMLabel.Create(Panel_Resize_Edit, 0, Y, TB_WIDTH, 30, '', fntOutline, taCenter);
      Inc(Y, 45);

      Button_Resize := TKMButton.Create(Panel_Resize_Edit, 0, Y, Panel_Resize_Edit.Width, 45, gResTexts[TX_MAPED_MAP_RESIZE_AND_SAVE], bsGame);
      Button_Resize.Anchors := [anLeft, anTop, anRight];
      Button_Resize.Hint := gResTexts[TX_MAPED_MAP_RESIZE_AND_SAVE_HINT];
      Button_Resize.OnClick := PanelConfirm_Switch;
      Button_Resize.Disable;
      Inc(Y, 60);

      Button_Cancel := TKMButton.Create(Panel_Resize_Edit, 0, Y, Panel_Resize_Edit.Width, 30, gResTexts[TX_WORD_CANCEL], bsGame);
      Button_Cancel.Anchors := [anLeft, anTop, anRight];
      Button_Cancel.OnClick   := Menu_Click;

    Panel_Resize_Confirm := TKMPanel.Create(Panel_Resize, 9, 0, Panel_Resize.Width - 9, Panel_Resize.Height);
      Label_Resize_Confirm := TKMLabel.Create(Panel_Resize_Confirm, 0, 0, Panel_Resize_Confirm.Width, 20, gResTexts[TX_MAPED_MAP_RESIZE_CONFIRM_TITLE], fntOutline, taCenter);
      Label_Resize_Confirm.Anchors := [anLeft, anTop, anRight];
      Label_Resize_Confirm.WordWrap := True;

      Button_Resize_Confirm_Yes := TKMButton.Create(Panel_Resize_Confirm, 0, Max(150, Label_Resize_Confirm.TextSize.Y + 10),
                                                    Panel_Resize_Confirm.Width, 30, gResTexts[TX_WORD_YES], bsGame);
      Button_Resize_Confirm_Yes.Hint := gResTexts[TX_MAPED_MAP_RESIZE_AND_SAVE_HINT];
      Button_Resize_Confirm_No := TKMButton.Create(Panel_Resize_Confirm, 0, Max(190, Label_Resize_Confirm.TextSize.Y + 50),
                                                   Panel_Resize_Confirm.Width, 30, gResTexts[TX_WORD_NO], bsGame);
      Button_Resize_Confirm_No.Hint := gResTexts[TX_GO_PREV_MENU];

      Button_Resize_Confirm_Yes.OnClick := Resize_Click;
      Button_Resize_Confirm_No.OnClick := PanelConfirm_Switch;
end;


procedure TKMMapEdMenuResize.Menu_Click(Sender: TObject);
begin
  fOnDone(Self);
  fOnPageChange(Self);
end;


procedure TKMMapEdMenuResize.ResizeRefresh(Sender: TObject);
var
  newMapX, newMapY: SmallInt;
begin
  Button_Resize.Enabled := (NumEdit_Resize_Left.Value   <> 0)
                        or (NumEdit_Resize_Right.Value  <> 0)
                        or (NumEdit_Resize_Top.Value    <> 0)
                        or (NumEdit_Resize_Bottom.Value <> 0);

  // Calc num edits range restrictions
  NumEdit_Resize_Left.ValueMax := EnsureRange(MAX_MAP_SIZE - gTerrain.MapX - NumEdit_Resize_Right.Value, 0, MAX_MAP_SIZE);
  NumEdit_Resize_Left.ValueMin := EnsureRange(-(gTerrain.MapX - MIN_MAP_SIZE + NumEdit_Resize_Right.Value), -MAX_MAP_SIZE, 0);
  NumEdit_Resize_Right.ValueMax := EnsureRange(MAX_MAP_SIZE - gTerrain.MapX - NumEdit_Resize_Left.Value, 0, MAX_MAP_SIZE);
  NumEdit_Resize_Right.ValueMin := EnsureRange(-(gTerrain.MapX - MIN_MAP_SIZE + NumEdit_Resize_Left.Value), -MAX_MAP_SIZE, 0);

  NumEdit_Resize_Top.ValueMax := EnsureRange(MAX_MAP_SIZE - gTerrain.MapY - NumEdit_Resize_Bottom.Value, 0, MAX_MAP_SIZE);
  NumEdit_Resize_Top.ValueMin := EnsureRange(-(gTerrain.MapY - MIN_MAP_SIZE + NumEdit_Resize_Bottom.Value), -MAX_MAP_SIZE, 0);
  NumEdit_Resize_Bottom.ValueMax := EnsureRange(MAX_MAP_SIZE - gTerrain.MapY - NumEdit_Resize_Top.Value, 0, MAX_MAP_SIZE);
  NumEdit_Resize_Bottom.ValueMin := EnsureRange(-(gTerrain.MapY - MIN_MAP_SIZE + NumEdit_Resize_Top.Value), -MAX_MAP_SIZE, 0);

  gGame.MapEditor.ResizeMapRect.Left := Abs(Min(0, NumEdit_Resize_Left.Value)) + 1;
  gGame.MapEditor.ResizeMapRect.Top := Abs(Min(0, NumEdit_Resize_Top.Value)) + 1;
  gGame.MapEditor.ResizeMapRect.Right := gTerrain.MapX - Abs(Min(0, NumEdit_Resize_Right.Value)) - 1;
  gGame.MapEditor.ResizeMapRect.Bottom := gTerrain.MapY - Abs(Min(0, NumEdit_Resize_Bottom.Value)) - 1;

  newMapX := gTerrain.MapX + NumEdit_Resize_Left.Value + NumEdit_Resize_Right.Value;
  newMapY := gTerrain.MapY + NumEdit_Resize_Top.Value + NumEdit_Resize_Bottom.Value;

  Label_CurrentMapSize.Caption := Format(gResTexts[TX_MAPED_MAP_RESIZE_CURR_MAP_SIZE], [gTerrain.MapX, gTerrain.MapY]);
  Label_NewMapSize.Enabled := Button_Resize.Enabled;
  Label_NewMapSize.Caption := Format(gResTexts[TX_MAPED_MAP_RESIZE_NEW_MAP_SIZE],
                                    [EnsureRange(newMapX, MIN_MAP_SIZE, MAX_MAP_SIZE),
                                     EnsureRange(newMapY, MIN_MAP_SIZE, MAX_MAP_SIZE)]);
end;


procedure TKMMapEdMenuResize.Resize_Click(Sender: TObject);
type
  TDir4 = (dLeft, dTop, dRight, dBottom);
var
  saveNameFullPath: string;
  left, top, right, bot: Integer;
  DIR4: TDir4;
  rRect: array[TDir4] of TKMRect;
  isMultiplayer: Boolean;
begin
  left  := Max(0, NumEdit_Resize_Left.Value);
  top   := Max(0, NumEdit_Resize_Top.Value);
  right := Max(0, NumEdit_Resize_Right.Value);
  bot   := Max(0, NumEdit_Resize_Bottom.Value);

  gGame.TerrainPainter.FixTerrainKindInfoAtBorders(False);
  isMultiplayer := fIsMultiplayer;

  saveNameFullPath := TKMapsCollection.FullPath(gGameParams.Name, '.dat', fIsMultiplayer);
  gGame.SaveMapEditor(saveNameFullPath, KMRect(NumEdit_Resize_Left.Value,  NumEdit_Resize_Top.Value,
                                       NumEdit_Resize_Right.Value, NumEdit_Resize_Bottom.Value));
  FreeThenNil(gGame);
  gGameApp.NewMapEditor(saveNameFullPath, isMultiplayer);

  // Collect generated map areas
  rRect[dLeft]   := KMRect(1, top + 1, left, gTerrain.MapY - bot);
  rRect[dTop]    := KMRect(1, 1, gTerrain.MapX, top);
  rRect[dRight]  := KMRect(gTerrain.MapX - right, top + 1, gTerrain.MapX, gTerrain.MapY - bot);
  rRect[dBottom] := KMRect(1, gTerrain.MapY - bot, gTerrain.MapX, gTerrain.MapY);

  for DIR4 := Low(TDir4) to High(TDir4) do
  begin
    // Has to fix terrain info, since we have tkGrass on new tiles there
    gGame.TerrainPainter.FixTerrainKindInfo(rRect[DIR4], False);
    // Rebuild generated map areas with normal tiles
    gGame.TerrainPainter.RebuildMap(rRect[DIR4], True);
    // Apply magic brush at the end to fix rest of the transtions
    gGame.TerrainPainter.MagicBrush(rRect[DIR4], mkSoft1);
  end;

  // Save changes
  gGame.SaveMapEditor(saveNameFullPath);
end;


procedure TKMMapEdMenuResize.Hide;
begin
  Panel_Resize.Hide;
end;


procedure TKMMapEdMenuResize.Show;
begin
  Panel_Resize_Confirm.Hide;
  Panel_Resize_Edit.Show;
  Panel_Resize.Show;
  ResizeRefresh(nil);
  fOnPageChange(Self);
end;


function TKMMapEdMenuResize.Visible: Boolean;
begin
  Result := Panel_Resize.Visible;
end;


procedure TKMMapEdMenuResize.PanelConfirm_Switch(Sender: TObject);
begin
  if Sender = Button_Resize then
  begin
    Panel_Resize_Edit.Hide;
    Panel_Resize_Confirm.Show;
    if not gGame.MapEditor.IsNewMap then
    begin
      Label_Resize_Confirm.Caption := gResTexts[TX_MAPED_MAP_RESIZE_CONFIRM];
      Button_Resize_Confirm_Yes.Visible := True;
      Button_Resize_Confirm_No.Caption := gResTexts[TX_WORD_NO];
    end else begin
      Label_Resize_Confirm.Caption := gResTexts[TX_MAPED_MAP_RESIZE_NOT_AVAIL];
      Button_Resize_Confirm_Yes.Hide;
      Button_Resize_Confirm_No.Caption := gResTexts[TX_MENU_TAB_HINT_GO_BACK];
    end;
  end
  else
  if Sender = Button_Resize_Confirm_No then
  begin
    Panel_Resize_Confirm.Hide;
    Panel_Resize_Edit.Show;
  end;
end;


procedure TKMMapEdMenuResize.SetLoadMode(aMultiplayer: Boolean);
begin
  fIsMultiplayer := aMultiplayer;
end;


end.

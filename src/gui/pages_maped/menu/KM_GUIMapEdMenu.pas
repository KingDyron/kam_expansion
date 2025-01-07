unit KM_GUIMapEdMenu;
{$I KaM_Remake.inc}
interface
uses
  Classes, SysUtils,
  KM_Controls, KM_ControlsBase, KM_ControlsList, KM_ControlsPopUp,
  KM_InterfaceGame, KM_InterfaceDefaults,
  KM_GUIMapEdMenuResize,
  KM_GUIMapEdMenuQuickPlay,
  KM_GUIMapEdMenuLoad,
  KM_GUIMapEdMenuSave,
  KM_GUIMapEdMenuQuit,
  KM_GUICommonGameOptions,
  KM_CommonTypes;

type
  TKMMapEdMenu = class(TKMMapEdMenuPage)
  private
    fGuiMenuResize: TKMMapEdMenuResize;
    fGuiMenuQuickPlay: TKMMapEdMenuQuickPlay;
    fGuiMenuLoad: TKMMapEdMenuLoad;
    fGuiMenuSave: TKMMapEdMenuSave;
    fGuiMenuSettings: TKMGUICommonGameOptions;
    fGuiMenuQuit: TKMMapEdMenuQuit;
    procedure MenuClick(Sender: TObject);
    procedure MenuDone(Sender: TObject);
    procedure HidePopUp(Sender : TObject);
    procedure ConfirmDelete(Sender : TObject);
    procedure CancelDelete(Sender : TObject);
    procedure RefreshList;
  protected
    Panel_Menu: TKMPanel;
      Button_Resize: TKMButton;
      Button_Menu_Save: TKMButton;
      Button_Menu_Load: TKMButton;
      Button_QuickPlay: TKMButton;
      Button_Menu_Settings: TKMButton;
      Button_Menu_Quit: TKMButton;
      Button_Menu_Save_Layers: TKMButton;
      Button_Menu_Load_Layers: TKMButton;

    PopUp_Save : TKMPopUpMenu;
      Button_SaveLayers : array[0..4] of TKMButton;
    PopUp_Delete : TKMPopUpConfirm;

    PopUp_List : TKMPopUpMenu;
      ListBox_Load: TKMListBox;
      Button_Delete : TKMButton;
    procedure DoShowSubMenu(aIndex: Byte); override;
  public
    constructor Create(aParent: TKMPanel; aOnPageChange: TNotifyEvent; aOnMapTypChanged: TBooleanEvent; aOnKeysUpdated: TEvent);
    destructor Destroy; override;

    property GuiMenuResize: TKMMapEdMenuResize read fGuiMenuResize;
    property GuiMenuQuickPlay: TKMMapEdMenuQuickPlay read fGuiMenuQuickPlay write fGuiMenuQuickPlay;
    property GuiMenuSettings: TKMGUICommonGameOptions read fGuiMenuSettings;
    procedure SetLoadMode(aMultiplayer: Boolean);
    procedure Show;
    procedure Hide;
    procedure Resize;
    function Visible: Boolean; override;
    procedure UpdateHotkeys;
    procedure UpdateState;
  end;


implementation
uses
  KM_Maps,
  KM_ResTexts, KM_RenderUI, KM_Utils, KM_Terrain,
  KM_ResFonts, KM_ResTypes, KM_GameParams;

const
  BUTTON_CAPTION : array[0..4] of Integer = (371, 373, 372, 1396, 1855);
{ TKMMapEdMenu }
constructor TKMMapEdMenu.Create(aParent: TKMPanel; aOnPageChange: TNotifyEvent; aOnMapTypChanged: TBooleanEvent; aOnKeysUpdated: TEvent);

var I : Integer;
begin
  inherited Create;

  fGuiMenuResize := TKMMapEdMenuResize.Create(aParent, MenuDone, aOnPageChange);
  fGuiMenuLoad := TKMMapEdMenuLoad.Create(aParent, MenuDone);
  fGuiMenuSave := TKMMapEdMenuSave.Create(aParent, MenuDone, aOnMapTypChanged);
  fGuiMenuQuit := TKMMapEdMenuQuit.Create(aParent, MenuDone);
  fGuiMenuSettings := TKMGUICommonGameOptions.Create(aParent, gResTexts[TX_MENU_SETTINGS_MAPED], aOnKeysUpdated);

  Panel_Menu := TKMPanel.Create(aParent, 0, 45, aParent.Width, aParent.Height - 45);
  Panel_Menu.Anchors := [anLeft, anTop, anBottom];

  Button_Resize := TKMButton.Create(Panel_Menu, 9, 10, Panel_Menu.Width - 9, 30, gResTexts[TX_MAPED_MAP_RESIZE], bsGame);
  Button_Resize.Anchors := [anLeft, anTop, anRight];
  Button_Resize.OnClick := MenuClick;

  Button_QuickPlay := TKMButton.Create(Panel_Menu, 9, 50, Panel_Menu.Width - 9, 30, gResTexts[TX_MAPED_MAP_QUICK_PLAY], bsGame);
  Button_QuickPlay.Anchors := [anLeft, anTop, anRight];
  Button_QuickPlay.OnClick := MenuClick;

  Button_Menu_Load := TKMButton.Create(Panel_Menu, 9, 110, Panel_Menu.Width - 9, 30, gResTexts[TX_MAPED_LOAD_TITLE], bsGame);
  Button_Menu_Load.Anchors := [anLeft, anTop, anRight];
  Button_Menu_Load.OnClick := MenuClick;
  Button_Menu_Save := TKMButton.Create(Panel_Menu, 9, 150, Panel_Menu.Width - 9, 30, gResTexts[TX_MAPED_SAVE_TITLE], bsGame);
  Button_Menu_Save.Anchors := [anLeft, anTop, anRight];
  Button_Menu_Save.OnClick := MenuClick;
  Button_Menu_Settings := TKMButton.Create(Panel_Menu, 9, 190, TB_MAP_ED_WIDTH - 9, 30, gResTexts[TX_MENU_SETTINGS], bsGame);
  Button_Menu_Settings.OnClick := MenuClick;

  Button_Menu_Quit := TKMButton.Create(Panel_Menu, 9, 250, Panel_Menu.Width - 9, 30, gResTexts[TX_MENU_QUIT_MAPED], bsGame);
  Button_Menu_Quit.Anchors := [anLeft, anTop, anRight];
  Button_Menu_Quit.OnClick := MenuClick;

  Button_Menu_Save_Layers := TKMButton.Create(Panel_Menu, 9, 300, Panel_Menu.Width - 9, 30, gResTexts[1853], bsMenu);
  Button_Menu_Save_Layers.Anchors := [anLeft, anTop, anRight];
  Button_Menu_Save_Layers.OnClick := MenuClick;

  Button_Menu_Load_Layers := TKMButton.Create(Panel_Menu, 9, 340, Panel_Menu.Width - 9, 30, gResTexts[1854], bsMenu);
  Button_Menu_Load_Layers.Anchors := [anLeft, anTop, anRight];
  Button_Menu_Load_Layers.OnClick := MenuClick;

  PopUp_List := TKMPopUpMenu.Create(aParent, 400);

  PopUp_List.Height := 400;
  PopUp_List.Top := Panel_Menu.Top;
  PopUp_List.Left := Panel_Menu.Right;


  TKMImage.Create(PopUp_List, 0, 0, PopUp_List.Width, PopUp_List.Height, 15, rxGuiMain).ImageStretch;
  with TKMImage.Create(PopUp_List, PopUp_List.Width - 50, 7, 32, 32, 52, rxGui) do
  begin
    Tag := 0;
    OnClick := HidePopUp;
    HighlightOnMouseOver := true;
  end;

  ListBox_Load := TKMListBox.Create(PopUp_List, 10, 50, PopUp_List.Width - 20, PopUp_List.Height - 70 - 35, fntGrey, bsMenu);
  ListBox_Load.Anchors := [anLeft, anTop, anRight];
  ListBox_Load.ItemHeight := 18;
  ListBox_Load.AutoHideScrollBar := True;
  ListBox_Load.HintBackColor := TKMColor4f.New(87, 72, 37);
  ListBox_Load.SearchEnabled := True;
  ListBox_Load.OnDoubleClick := MenuClick;

  Button_Delete := TKMButton.Create(PopUp_List, 10, PopUp_List.Height - 35, PopUp_List.Width - 20, 30, gResTexts[168], bsGame);
  Button_Delete.OnClick := MenuClick;

  PopUp_Save := TKMPopUpMenu.Create(aParent, 250);

  PopUp_Save.Height := 250;
  PopUp_Save.Top := Panel_Menu.Top + 150;
  PopUp_Save.Left := Panel_Menu.Right;


  TKMImage.Create(PopUp_Save, 0, 0, PopUp_Save.Width, PopUp_Save.Height, 15, rxGuiMain).ImageStretch;
  with TKMImage.Create(PopUp_Save, PopUp_Save.Width - 50, 7, 32, 32, 52, rxGui) do
  begin
    Tag := 1;
    OnClick := HidePopUp;
    HighlightOnMouseOver := true;
  end;
  for I := 0 to High(Button_SaveLayers) do
  begin
    Button_SaveLayers[I] := TKMButton.Create(PopUp_Save, 9,40 + I * 35, PopUp_Save.Width - 18, 30, gResTexts[BUTTON_CAPTION[I]], bsGame );
    Button_SaveLayers[I].OnClick :=  MenuClick;
    Button_SaveLayers[I].Tag := I;
  end;

  PopUp_Delete := TKMPopUpConfirm.Create(PopUp_List, 20, PopUp_List.Height - 250, PopUp_List.Width - 20);
  PopUp_Delete.LabelTop.Caption := gResTexts[1581];
  PopUp_Delete.LabelCenter.Caption := gResTexts[1856];
  PopUp_Delete.OnConfirm := ConfirmDelete;
  PopUp_Delete.OnCancel := CancelDelete;
  PopUp_Delete.Button_Confirm.Width := 150;
  PopUp_Delete.Button_Cancel.Width := 150;
end;


destructor TKMMapEdMenu.Destroy;
begin
  fGuiMenuResize.Free;
  fGuiMenuLoad.Free;
  fGuiMenuSave.Free;
  fGuiMenuQuit.Free;
  fGuiMenuSettings.Free;

  inherited;
end;


procedure TKMMapEdMenu.DoShowSubMenu(aIndex: Byte);
begin
  inherited;

  case aIndex of
    0: Button_Resize.Click;
    1: Button_QuickPlay.Click;
    2: Button_Menu_Load.Click;
    3: Button_Menu_Save.Click;
    4: Button_Menu_Settings.Click;
    5: Button_Menu_Quit.Click;
  end;
end;


procedure TKMMapEdMenu.MenuClick(Sender: TObject);
  var I : Integer;
begin
  for I := 0 to High(Button_SaveLayers) do
    if Sender = Button_SaveLayers[I] then
    begin
      gTerrain.SaveSeperatedLayer(I);
      Exit;
    end;
  if Sender = Button_Delete then
  begin
    PopUp_Delete.Show;
    Exit;
  end else
  if Sender = Button_Menu_Save_Layers then
  begin
    PopUp_List.Hide;
    PopUp_Save.Show;
    Exit;
  end else
  if Sender = Button_Menu_Load_Layers then
  begin
    RefreshList;
    PopUp_List.Show;
    PopUp_Save.Hide;

    Exit;
  end;
  if Sender = ListBox_Load then
  begin
    gTerrain.LoadSeperatedLayer(ListBox_Load.Item[ListBox_Load.ItemIndex]);
    Exit;
  end;

  if (Sender <> Button_QuickPlay)
  and (Sender <> Button_Menu_Settings) then
    Hide;

  if Sender = Button_Resize then
    fGuiMenuResize.Show
  else
  if Sender = Button_QuickPlay then
    fGuiMenuQuickPlay.Show
  else
  if Sender = Button_Menu_Quit then
    fGuiMenuQuit.Show
  else
  if Sender = Button_Menu_Save then
    fGuiMenuSave.Show
  else
  if Sender = Button_Menu_Load then
    fGuiMenuLoad.Show
  else
  if Sender = Button_Menu_Settings then
    fGuiMenuSettings.Show;
end;


procedure TKMMapEdMenu.MenuDone(Sender: TObject);
begin
  fGuiMenuResize.Hide;
  fGuiMenuQuickPlay.Hide;
  fGuiMenuLoad.Hide;
  fGuiMenuSave.Hide;
  fGuiMenuQuit.Hide;
  fGuiMenuSettings.Hide;

  Show;
end;


procedure TKMMapEdMenu.Resize;
begin
  fGuiMenuQuickPlay.Resize;
end;


procedure TKMMapEdMenu.Hide;
begin
  Panel_Menu.Hide;
end;


procedure TKMMapEdMenu.Show;
begin
  Panel_Menu.Show;

end;


function TKMMapEdMenu.Visible: Boolean;
begin
  Result := Panel_Menu.Visible;
end;

procedure TKMMapEdMenu.HidePopUp(Sender: TObject);
begin
  if TKMImage(Sender).Tag = 0 then
    PopUp_List.Hide
  else
    PopUp_Save.Hide;
end;


procedure TKMMapEdMenu.ConfirmDelete(Sender: TObject);
var aPath : String;
begin
  if ListBox_Load.ItemIndex = -1 then Exit;

  aPath := TKMapsCollection.GetMapPath(Trim(gGameParams.Name)) + 'Layers' + PathDelim + ListBox_Load.Item[ListBox_Load.ItemIndex];
  if FileExists(aPath) then
    DeleteFile(aPath);
  RefreshList;
  PopUp_Delete.Hide;
end;

procedure TKMMapEdMenu.CancelDelete(Sender: TObject);
begin
  PopUp_Delete.Hide;
end;

procedure TKMMapEdMenu.RefreshList;
var aList : TStringList;
var I, J : Integer;
begin
  aList := TStringList.Create;
  J := ListBox_Load.ItemIndex;
  ListBox_Load.Clear;

  TKMapsCollection.GetMapLayersFiles(gGameParams.Name, aList);
  for I := 0 to aList.Count - 1 do
    ListBox_Load.Add(aList[I]);

  if J < ListBox_Load.Count - 1 then
    ListBox_Load.ItemIndex := J
  else
    ListBox_Load.ItemIndex := ListBox_Load.Count - 1;

end;


procedure TKMMapEdMenu.UpdateHotkeys;
begin
  Button_Resize.Hint        := GetHintWHotkey(TX_MAPED_MAP_RESIZE,          MAPED_SUBMENU_HOTKEYS[0]);
  Button_QuickPlay.Hint     := GetHintWHotkey(TX_MAPED_MAP_QUICK_PLAY_HINT, MAPED_SUBMENU_HOTKEYS[1]);
  Button_Menu_Load.Hint     := GetHintWHotkey(TX_MAPED_LOAD_TITLE,          MAPED_SUBMENU_HOTKEYS[2]);
  Button_Menu_Save.Hint     := GetHintWHotkey(TX_MAPED_SAVE_TITLE,          MAPED_SUBMENU_HOTKEYS[3]);
  Button_Menu_Settings.Hint := GetHintWHotkey(TX_MENU_SETTINGS,             MAPED_SUBMENU_HOTKEYS[4]);
  Button_Menu_Quit.Hint     := GetHintWHotkey(TX_MENU_QUIT_MAPED,           MAPED_SUBMENU_HOTKEYS[5]);
end;


procedure TKMMapEdMenu.UpdateState;
begin
  fGuiMenuLoad.UpdateState;
end;


procedure TKMMapEdMenu.SetLoadMode(aMultiplayer: Boolean);
begin
  fGuiMenuResize.SetLoadMode(aMultiplayer);
  fGuiMenuQuickPlay.SetLoadMode(aMultiplayer);
  fGuiMenuLoad.SetLoadMode(aMultiplayer);
  fGuiMenuSave.SetLoadMode(aMultiplayer);
end;


end.

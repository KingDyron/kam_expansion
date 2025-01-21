unit KM_GUIGameMessagesPopUp;
{$I KaM_Remake.inc}
interface
uses
  KM_Controls, KM_ControlsBase, KM_ControlsEdit,
  KM_CommonClasses,
  KM_ResTypes, KM_ControlsList,
  System.Classes;


type
  TKMGUIGameMessagesPopUp = class
  private
    fOnShow : TNotifyEvent;
    procedure ClosePanel(Sender : TObject);
    procedure RefreshList;
    procedure SelectListItem(Sender : TObject);
  protected
    Pin_Open : TKMButtonFlatPin;
    Panel_Messages : TKMPanel;
      Image_Background,
      Image_ClosePanel: TKMImage;
      Columnbox_Messages : TKMColumnBox;
      Label_Message: TKMLabel;
      Image_MessageKind: TKMImage;
  public
    constructor Create(aParent: TKMPanel; aOnShow : TNotifyEvent);

    procedure Show;
    procedure Hide;
    function Visible: Boolean;
  end;


implementation
uses  KM_HandsCollection,
      KM_Pics,
      KM_Game,
      KM_AIGoals, KM_MapTypes,
      KM_Defaults, KM_GameParams,
      KM_ResFonts, KM_ResTexts,
      KM_RenderUI, KM_GameTypes,
      KM_CommonUtils, SysUtils, Math;

constructor TKMGUIGameMessagesPopUp.Create(aParent: TKMPanel; aOnShow : TNotifyEvent);
begin
  inherited Create;
  fOnShow := aOnShow;
  Pin_Open := TKMButtonFlatPin.Create(aParent, 198, 120, 25, 32, 953);
  Pin_Open.OnClick := ClosePanel;
  Pin_Open.Hint := gResTexts[2116];
  Panel_Messages := TKMPanel.Create(aParent, 240, 0, 600, 500);

  Image_Background := TKMImage.Create(Panel_Messages, 0, 0, Panel_Messages.Width, Panel_Messages.Height, 18, rxGuiMain);
  Image_Background.ImageStretch;

  TKMBevel.Create(Panel_Messages, 20, 40, Panel_Messages.Width - 40, Panel_Messages.Height - 100);

  Image_ClosePanel := TKMImage.Create(Panel_Messages, Panel_Messages.Width - 60, 10, 30, 30, 52);
  Image_ClosePanel.OnClick := ClosePanel;

  Columnbox_Messages := TKMColumnBox.Create(Panel_Messages, 30, 50, Panel_Messages.Width - 60, 250, fntOutline, bsGame);
  Columnbox_Messages.SetColumns(fntMetal, ['Messages'], [0]);
  Columnbox_Messages.ShowHeader := true;
  Columnbox_Messages.AutoFocusable := false;
  Columnbox_Messages.ShowLines := true;
  Columnbox_Messages.OnClick := SelectListItem;
  Columnbox_Messages.OnChange := SelectListItem;
  Label_Message := TKMLabel.Create(Panel_Messages, 30, Columnbox_Messages.Bottom, Panel_Messages.Width - 60 - 40, Panel_Messages.Height - 250, '', fntMetal, taLeft);
  Label_Message.WordWrap := true;

  Image_MessageKind := TKMImage.Create(Panel_Messages, Label_Message.Right + 2, Label_Message.Top + 5, 40, 50, 0, rxGui);

  Hide;
end;


procedure TKMGUIGameMessagesPopUp.Show;
begin
  Panel_Messages.Show;
  if Assigned(fOnShow) then
    fOnShow(Self);
  RefreshList;
end;


procedure TKMGUIGameMessagesPopUp.Hide;
begin
  Panel_Messages.Hide;
end;


function TKMGUIGameMessagesPopUp.Visible: Boolean;
begin
  Result := Panel_Messages.Visible;
end;

procedure TKMGUIGameMessagesPopUp.ClosePanel(Sender: TObject);
begin
  if Sender = Pin_Open then
  begin
    if Visible then
      Hide
    else
      Show;

  end else
    Hide;
end;

procedure TKMGUIGameMessagesPopUp.RefreshList;
var I, ItemIndex, TopIndex : Integer;
  S : String;
begin
  ItemIndex := Columnbox_Messages.ItemIndex;
  TopIndex := Columnbox_Messages.TopIndex;

  Columnbox_Messages.Clear;

  for I := 0 to High(gMySpectator.Hand.MessageStack) do
  begin
    with gMySpectator.Hand.MessageStack[I] do
    begin
      S := Text;
      S := gGame.TextMission.ParseTextMarkup(Text);
      Columnbox_Messages.AddItem( MakeListRow([ Copy(S, 1, 30) + '...'], I ) );
    end;
  end;

  if ItemIndex >= Columnbox_Messages.RowCount then
    ItemIndex := Columnbox_Messages.RowCount - 1;

  Columnbox_Messages.ItemIndex := ItemIndex;
  Columnbox_Messages.TopIndex := TopIndex;
  Columnbox_Messages.JumpToSelected;
  SelectListItem(Columnbox_Messages);
end;

procedure TKMGUIGameMessagesPopUp.SelectListItem(Sender: TObject);
var aIndex : integer;
begin
  aIndex := Columnbox_Messages.ItemIndex;
  Label_Message.Caption := '';
  Image_MessageKind.TexID := 0;
  if aIndex = -1 then
    Exit;
  if aIndex > high(gMySpectator.Hand.MessageStack) then Exit;
  
  Label_Message.Caption := gGame.TextMission.ParseTextMarkup(gMySpectator.Hand.MessageStack[aIndex].Text);
  Image_MessageKind.TexID := MSG_ICON[gMySpectator.Hand.MessageStack[aIndex].Kind];
end;


end.



unit KM_GUIMapEdCampaignMap;
{$I KaM_Remake.inc}
interface
uses
  Classes, SysUtils, Math, VCL.Controls,
  KM_Controls, KM_ControlsBase, KM_ControlsPopUp, KM_ControlsEdit,
  KM_ControlsSwitch,
  KM_Campaigns,
  KM_InterfaceDefaults, KM_InterfaceTypes;


type
  TKMCampaignMapEditor = class(TKMMenuPageCommon)
    private
      fOnPageChange: TKMMenuChangeEventText; //will be in ancestor class
      fCampaign : TKMCampaign;
      fMapSelected : Byte;
      fDragingID : Byte;
      fDragginSubNode : Boolean;
      fCData : record
        MapsCount : Integer;
        Maps : TKMCampaignMapsInfo;//it's only this but maybe something will be added later
      end;
      procedure RefreshNodes;
      procedure Campaign_SelectMap(aMapID : Integer);
      procedure Campaign_Click(Sender : TObject);
      procedure Campaign_ClickShift(Sender: TObject; X,Y: Integer; Shift: TShiftState; Button: TMouseButton);
      procedure Campaign_MouseMove(Sender : TObject; X, Y : Integer; Shift : TShiftState);
      procedure Campaign_MouseDown(Sender : TObject; X, Y : Integer; Shift : TShiftState);
      procedure Campaign_SetFlagColor(Sender : TObject);

    protected
      Panel_Campaign : TKMPanel;
        Image_CampaignBG : TKMImage;
        Panel_Flags : TKMPanel;
          Image_CampaignFlags: array[0..MAX_CAMP_MAPS - 1] of TKMImage;
          Label_CampaignFlags: array[0..MAX_CAMP_MAPS - 1] of TKMLabel;
          Image_CampaignSubNode: array[0..MAX_CAMP_NODES - 1] of TKMImage;
          Image_Scroll: TKMImage;

        Panel_Settings : TKMPanel;
          Button_SetFlagColor : TKMButton;
          Edit_UnlockAfter : array[0..4] of TKMNumericEdit;
          Radio_TextPos : TKMRadioGroup;//left or right
          Label_MissionTitle : TKMLabel;
          Edit_NodeCount,
          Edit_FlagsCount : TKMNumericEdit;
        PopUpRGB : TKMPopUpPanelRGB;
        Button_Save,
        Button_Back : TKMButton;

    public
      constructor Create(aParent : TKMPanel; aOnPageChange: TKMMenuChangeEventText);
      function Visible : Boolean;
      procedure Hide;
      procedure Show(aCampaign : TKMCampaign);
      procedure Resize(X, Y: Word);
  end;

implementation
uses KM_ResTypes, KM_ResFonts, KM_Defaults, KM_ResTexts,
      KM_RenderUI;
const
  FLAG_LABEL_OFFSET_X = 10;
  FLAG_LABEL_OFFSET_Y = 3;
  CAMP_NODE_ANIMATION_PERIOD = 5;
  IMG_SCROLL_MAX_HEIGHT = 430;

constructor TKMCampaignMapEditor.Create(aParent : TKMPanel; aOnPageChange: TKMMenuChangeEventText);
var I : Integer;
begin
  Inherited Create(gpMapEdCampaign);
  fMapSelected:= 255;
  fOnPageChange := aOnPageChange;
  Panel_Campaign := TKMPanel.Create(aParent, 0, 0, aParent.Width, aParent.Height);
  Panel_Campaign.AnchorsStretch;


    Image_CampaignBG := TKMImage.Create(Panel_Campaign, 0, 0, Panel_Campaign.Width, Panel_Campaign.Height, 2, rxCustom);
    Image_CampaignBG.ImageStretch;
    Image_CampaignBG.Highlight := false;
    Image_CampaignBG.Hitable := false;

    Panel_Flags := TKMPanel.Create(Panel_Campaign, 0, 0, aParent.Width, aParent.Height);
    Panel_Flags.AnchorsStretch;
    Panel_Flags.Hitable := true;
    Panel_Flags.OnMouseMove := Campaign_MouseMove;
    Panel_Flags.OnClickPos := Campaign_ClickShift;
    Panel_Flags.OnMouseDown := Campaign_MouseDown;
    Panel_Flags.Scale := 1;
      for I := 0 to High(Image_CampaignFlags) do
      begin
        Image_CampaignFlags[I] := TKMImage.Create(Panel_Flags, 20, 20, 23, 29, 11, rxGuiMain);
        Image_CampaignFlags[I].Tag := I;
        Image_CampaignFlags[I].Hitable := false;
        Image_CampaignFlags[I].HighlightOnMouseOver := true;
        //Image_CampaignFlags[I].Highlight := true;

        Label_CampaignFlags[I] := TKMLabel.Create(Panel_Flags, 20, 20, 20, 20, IntToStr(I + 1), fntGrey, taCenter);
        Label_CampaignFlags[I].FontColor := icLightGray2;
        Label_CampaignFlags[I].Hitable := false;
      end;
      for I := 0 to High(Image_CampaignSubNode) do
      begin
        Image_CampaignSubNode[I] := TKMImage.Create(Panel_Flags, 50, 50, 11, 11, 16, rxGuiMain);
        Image_CampaignSubNode[I].ImageCenter; //Pivot at the center of the dot (width/height = 0)
        Image_CampaignSubNode[I].Hitable := false;
      end;

      Image_Scroll := TKMImage.Create(Panel_Flags, Panel_Flags.Width - 360, Panel_Flags.Height - 200, 360, 200, 410, rxGui);
      Image_Scroll.ClipToBounds := True;
      Image_Scroll.AnchorsStretch;
      Image_Scroll.ImageAnchors := [anLeft, anRight, anTop];
      Image_Scroll.Hide;

    Panel_Settings := TKMPanel.Create(Panel_Campaign, 10, Panel_Campaign.Height - 500, 214, 400);

      with TKMBevel.Create(Panel_Settings, 0, 0, Panel_Settings.Width, Panel_Settings.Height) do
        Hitable := false;


      TKMLabel.Create(Panel_Settings, 10, 10, Panel_Settings.Width - 20, 20, gResTexts[2172], fntOutline, taLeft);

      Edit_FlagsCount:= TKMNumericEdit.Create(Panel_Settings, 10, 30, 1, 64);
      Edit_FlagsCount.OnChange := Campaign_Click;

      Button_SetFlagColor := TKMButton.Create(Panel_Settings, 10, Panel_Settings.Height - 30 , Panel_Settings.Width - 20, 25, gResTexts[2178], bsGame);
      Button_SetFlagColor.OnClick := Campaign_Click;

      TKMLabel.Create(Panel_Settings, 10, Button_SetFlagColor.Top - 80, Panel_Settings.Width - 20, 20, gResTexts[2177], fntOutline, taLeft);

      for I := Low(Edit_UnlockAfter) to High(Edit_UnlockAfter) do
      begin
        Edit_UnlockAfter[I] := TKMNumericEdit.Create(Panel_Settings, 10 + (I mod 3) * 70, Button_SetFlagColor.Top - 60 + (I div 3) * 25, 0, 64);
        Edit_UnlockAfter[I].OnChange := Campaign_Click;
        Edit_UnlockAfter[I].Tag := I;
        Edit_UnlockAfter[I].AutoFocusable := false;
      end;

      Radio_TextPos := TKMRadioGroup.Create(Panel_Settings, 10, Edit_UnlockAfter[0].Top - 55, Panel_Settings.Width - 20, 40, fntMetal);
      Radio_TextPos.Add(gResTexts[2175]);
      Radio_TextPos.Add(gResTexts[2176]);
      Radio_TextPos.OnChange := Campaign_Click;

      TKMLabel.Create(Panel_Settings, 10, Radio_TextPos.Top - 20, Panel_Settings.Width - 20, 20, gResTexts[2174], fntOutline, taLeft);

      Edit_NodeCount:= TKMNumericEdit.Create(Panel_Settings, 10, Radio_TextPos.Top - 45, 0, 64);
      Edit_NodeCount.OnChange := Campaign_Click;

      TKMLabel.Create(Panel_Settings, 10, Edit_NodeCount.Top - 20, Panel_Settings.Width - 20, 20, gResTexts[2173], fntOutline, taLeft);
      Label_MissionTitle := TKMLabel.Create(Panel_Settings, 0, Edit_NodeCount.Top - 60, Panel_Settings.Width, 20, '', fntOutLine, taCenter);

  PopUpRGB := TKMPopUpPanelRGB.Create(Panel_Campaign);
  PopUpRGB.OnConfirm := Campaign_SetFlagColor;

  Button_Back := TKMButton.Create(Panel_Campaign, 10, Panel_Campaign.Height - 40, 150, 25, gResTexts[106], bsGame);
  Button_Back.OnClick := Campaign_Click;

  Button_Save := TKMButton.Create(Panel_Campaign, Button_Back.Right + 10, Panel_Campaign.Height - 40, 150, 25, gResTexts[343
  ], bsGame);
  Button_Save.OnClick := Campaign_Click;
end;

function TKMCampaignMapEditor.Visible : Boolean;
begin
  Result := Panel_Campaign.Visible;
end;

procedure TKMCampaignMapEditor.Hide;
begin
  if Assigned(fOnPageChange) then
    fOnPageChange(gpMapEditor);
  Panel_Campaign.Hide;
end;

procedure TKMCampaignMapEditor.Show(aCampaign : TKMCampaign);
var I : Integer;
begin
  fCampaign := aCampaign;
  fCData.Maps := fCampaign.Maps;
  fCData.MapsCount := fCampaign.MapCount;
  //SetLength(fCData.Maps, 64);
  Image_CampaignBG.TexID := fCampaign.BackGroundPic.ID;
  Image_CampaignBG.RX := fCampaign.BackGroundPic.RX;

  //Special rule to keep campaign flags pivoted at the right place (so the flagpole doesn't move when you resize)
  if fCampaign <> nil then
    for I := 0 to high(Image_CampaignFlags) do
      if I >= fCData.MapsCount then
      begin
        Image_CampaignFlags[I].Hide;
        Label_CampaignFlags[I].Hide;
      end
      else
        with Image_CampaignFlags[I] do
        begin
          //Pivot flags around Y=bottom X=middle, that's where the flag pole is
          Left := fCampaign.Maps[I].Flag.X - Round((Width/2)*(1-Panel_Flags.Scale));
          Top  := fCampaign.Maps[I].Flag.Y - Round(Height   *(1-Panel_Flags.Scale));
          Label_CampaignFlags[I].AbsLeft := AbsLeft;
          Label_CampaignFlags[I].AbsTop := AbsTop + FLAG_LABEL_OFFSET_Y;
          Label_CampaignFlags[I].Show;
          FlagColor := fCData.Maps[I].FlagColor;
          Show;
        end;

  Edit_FlagsCount.Value := fCData.MapsCount;
  Panel_Campaign.Show;
  Campaign_SelectMap(255);
end;

procedure TKMCampaignMapEditor.Resize(X, Y: Word);
begin
  if (fCampaign = nil) or not Visible then Exit;

  //Special rules for resizing the campaigns panel
  Panel_Flags.Scale := Min(768,Y) / 768;

  Panel_Flags.Left := Round(1024*(1-Panel_Flags.Scale) / 2);
  Image_CampaignBG.Left := Round(1024*(1-Panel_Flags.Scale) / 2);
  Image_CampaignBG.Height := Min(768,Y);
  Image_CampaignBG.Width := Round(1024*Panel_Flags.Scale);
end;

procedure TKMCampaignMapEditor.Campaign_Click(Sender : TObject);
var I : Integer;
begin

  if Sender = Edit_NodeCount then
  begin
    if fMapSelected = 255 then
      Exit;

    fCData.Maps[fMapSelected].NodeCount := Edit_NodeCount.Value;
    for I := 0 to 63 do
      if fCData.Maps[fMapSelected].Nodes[I].X = 0 then
      begin
        fCData.Maps[fMapSelected].Nodes[I].X := 20;
        fCData.Maps[fMapSelected].Nodes[I].Y := 20;
      end;

    Campaign_SelectMap(fMapSelected);
  end else
  if Sender = Edit_FlagsCount then
  begin
    fCData.MapsCount := Edit_FlagsCount.Value;
    if fCData.MapsCount >= length(fCData.Maps) then
    begin
      SetLength(fCData.Maps, fCData.MapsCount);
    end;

    for I := 0 to high(Image_CampaignFlags) do
      if I >= fCData.MapsCount then
      begin
        Image_CampaignFlags[I].Hide;
        Label_CampaignFlags[I].Hide;
      end
      else
      with Image_CampaignFlags[I] do
      begin
        Label_CampaignFlags[I].Show;

        if fCData.Maps[I].FlagColor = 0 then
          fCData.Maps[I].FlagColor := $FF0000EB;
          FlagColor := fCData.Maps[I].FlagColor;
        Show;
      end;

  end else
  if Sender = Button_Save then
  begin
    fCampaign.MapCount := fCData.MapsCount;
    fCampaign.Maps := fCData.Maps;
    fCampaign.SaveToFile(fCampaign.Path + 'info.cmp');
    //Hide;
  end else
  if Sender = Button_Back then
  begin
    Hide;
  end
  else
  if Sender = Button_SetFlagColor then
  begin
    if fMapSelected = 255 then
      Exit;

    PopUpRGB.Color := fCData.Maps[fMapSelected].FlagColor;
    PopUpRGB.Show;
  end else
  if Sender = Radio_TextPos then
  begin
    if fMapSelected = 255 then
      Exit;
    fCData.Maps[fMapSelected].TextPos := TKMBriefingCorner(Radio_TextPos.ItemIndex);
    Image_Scroll.Left := IfThen(fCData.Maps[fMapSelected].TextPos = bcBottomRight, Panel_Flags.Width - 360);
  end else
  if fMapSelected <> 255 then
    for I := 0 to High(Edit_UnlockAfter) do
      if Sender = Edit_UnlockAfter[I] then
      begin
        fCData.Maps[fMapSelected].UnlockAfter[I] := Edit_UnlockAfter[I].Value;
      end;

    

end;

procedure TKMCampaignMapEditor.RefreshNodes;
var I : Integer;
begin

  for I := 0 to high(Image_CampaignSubNode) do
    if I >= fCData.Maps[fMapSelected].NodeCount then
    begin
      Image_CampaignSubNode[I].Hide;
      Image_CampaignSubNode[I].Left := 50;
      Image_CampaignSubNode[I].Top  := 50;
    end
    else
    begin
      Image_CampaignSubNode[I].Show;
      Image_CampaignSubNode[I].Left := fCData.Maps[fMapSelected].Nodes[I].X - 11 div 2;
      Image_CampaignSubNode[I].Top  := fCData.Maps[fMapSelected].Nodes[I].Y - 11 div 2;
      Image_CampaignSubNode[I].FlagColor := fCData.Maps[fMapSelected].FlagColor;
    end;

end;

procedure TKMCampaignMapEditor.Campaign_SelectMap(aMapID : Integer);
var I : Integer;
begin
  //disable Everything first
  fMapSelected := aMapID;
  Label_MissionTitle.Caption := '';
  Image_Scroll.Hide;
  Button_SetFlagColor.Disable;
  Radio_TextPos.Disable;
  Edit_NodeCount.Disable;
  for I := 0 to Radio_TextPos.Count - 1 do
    Radio_TextPos.SetItemEnabled(I, false);

  for I := 0 to High(Image_CampaignSubNode) do
    Image_CampaignSubNode[I].Hide; //hide sub nodes
  for I := low(Edit_UnlockAfter) to high(Edit_UnlockAfter) do
    Edit_UnlockAfter[I].Disable;

  for I := 0 to High(Image_CampaignFlags) do
    Image_CampaignFlags[I].Highlight := fMapSelected = I;

  if fMapSelected = 255 then
    Exit;

  for I := low(Edit_UnlockAfter) to high(Edit_UnlockAfter) do
    Edit_UnlockAfter[I].Enable;

  Panel_Settings.Left := IfThen(Image_CampaignFlags[fMapSelected].Left <= Panel_Flags.Width div 2, Panel_Flags.Width - Panel_Settings.Width - 10, 10);

  for I := 0 to Radio_TextPos.Count - 1 do
    Radio_TextPos.SetItemEnabled(I, true);
  Radio_TextPos.Enable;

  Radio_TextPos.ItemIndex := byte(fCData.Maps[fMapSelected].TextPos);

  Button_SetFlagColor.Enable;
  Image_Scroll.Left := IfThen(fCData.Maps[fMapSelected].TextPos = bcBottomRight, Panel_Flags.Width - 360);
  Image_Scroll.Show;

  for I := low(Edit_UnlockAfter) to high(Edit_UnlockAfter) do
    Edit_UnlockAfter[I].Value := fCData.Maps[fMapSelected].UnlockAfter[I];

  Label_MissionTitle.Caption := fCampaign.ShortName + ' ' + IntToStr(fMapSelected + 1); {fCampaign.GetCampaignMissionTitle(fMapSelected)};
  Edit_NodeCount.Value := fCData.Maps[fMapSelected].NodeCount;
  Edit_NodeCount.Enabled := true;
  RefreshNodes;
end;

procedure TKMCampaignMapEditor.Campaign_MouseMove(Sender : TObject; X, Y : Integer; Shift : TShiftState);
var aX, aY : Integer;
begin
  if fDragingID = 255 then
    Exit;

  aX := X - Panel_Flags.AbsLeft;
  aY := Y - Panel_Flags.AbsTop;

  if fDragginSubNode and (fMapSelected <> 255) then
  begin
    aX := Max(aX, 0);
    aY := Max(aY, 0);
    With Image_CampaignSubNode[fDragingID] do
    begin
      Left := aX - 11 div 2;
      Top  := aY - 11 div 2;

      fCData.Maps[fMapSelected].Nodes[fDragingID].X := aX;
      fCData.Maps[fMapSelected].Nodes[fDragingID].Y := aY;
    end;

  end else
  begin
    if fMapSelected <> fDragingID then
      Exit;

    aX := aX - 23 div 2;
    aY := aY - 29;
    aX := Max(aX, 0);
    aY := Max(aY, 0);

    With Image_CampaignFlags[fDragingID] do
    begin
      Left := aX - Round((Width/2)*(1-Panel_Flags.Scale));
      Top  := aY - Round(Height   *(1-Panel_Flags.Scale));

      fCData.Maps[fDragingID].Flag.X := aX;
      fCData.Maps[fDragingID].Flag.Y := aY;

      Label_CampaignFlags[fDragingID].AbsLeft := AbsLeft;
      Label_CampaignFlags[fDragingID].AbsTop := AbsTop + FLAG_LABEL_OFFSET_Y;
    end;
  end;
end;

procedure TKMCampaignMapEditor.Campaign_ClickShift(Sender: TObject; X,Y: Integer; Shift: TShiftState; Button: TMouseButton);
var I : Integer;
  aX, aY : Integer;
begin

  if (ssRight in Shift) and (ssShift in Shift) then
  begin
    if fMapSelected <> 255 then
    begin
      aX := X - Panel_Flags.AbsLeft + 2;
      aY := Y - Panel_Flags.AbsTop + 2;

      I := fCData.Maps[fMapSelected].NodeCount;
      fCData.Maps[fMapSelected].NodeCount := fCData.Maps[fMapSelected].NodeCount + 1;
      fCData.Maps[fMapSelected].Nodes[I].X := aX;
      fCData.Maps[fMapSelected].Nodes[I].Y := aY;
      Edit_NodeCount.Value := I + 1;
      RefreshNodes;
    end;
    Exit;
  end;

  Image_Scroll.Hide;
  Button_SetFlagColor.Disable;
  for I := low(Edit_UnlockAfter) to high(Edit_UnlockAfter) do
    Edit_UnlockAfter[I].Disable;

  fDragingID := 255;
  if not fDragginSubNode then
    fMapSelected := 255;
  fDragginSubNode := false;

  Campaign_SelectMap(fMapSelected);
  for I := 0 to High(Image_CampaignFlags) do
    If Image_CampaignFlags[I].Visible then
      if Image_CampaignFlags[I].HitTest(X, Y, false, true) then
        Campaign_SelectMap(I);

end;

procedure TKMCampaignMapEditor.Campaign_MouseDown(Sender: TObject; X: Integer; Y: Integer; Shift: TShiftState);
var I: Integer;
begin
  if (ssRight in Shift) and (ssShift in Shift) then
  begin
    Exit;
  end;
  begin
    fDragingID := 255;
    fDragginSubNode := false;

    for I := 0 to High(Image_CampaignFlags) do
      If Image_CampaignFlags[I].Visible then
        if Image_CampaignFlags[I].HitTest(X, Y, false, true) then
          fDragingID := I;

    for I := 0 to High(Image_CampaignFlags) do
      if Image_CampaignSubNode[I].Visible then
        if Image_CampaignSubNode[I].HitTest(X{ - Panel_Flags.AbsLeft}, Y{ - Panel_Flags.AbsTop}, false, true) then
        begin
          fDragingID := I;
          fDragginSubNode := true;
        end;
    if fDragginSubNode then
      Image_CampaignSubNode[fDragingID].Highlight := true;
  end;

end;

procedure TKMCampaignMapEditor.Campaign_SetFlagColor(Sender : TObject);
var I : integer;
begin
  if fMapSelected = 255 then
    Exit;

  fCData.Maps[fMapSelected].FlagColor := PopUpRGB.Color;

  Image_CampaignFlags[fMapSelected].FlagColor := fCData.Maps[fMapSelected].FlagColor;

  for I := 0 to fCData.Maps[fMapSelected].NodeCount - 1 do
    Image_CampaignSubNode[I].FlagColor := fCData.Maps[fMapSelected].FlagColor;


end;

end.

unit KM_GUIMapEdMarkerReveal;
{$I KaM_Remake.inc}
interface
uses
   Classes, Math, StrUtils, SysUtils,
   KM_Controls, KM_ControlsBase, KM_ControlsTrackBar,
   KM_Defaults, KM_Pics, KM_MapEdTypes;


type
  TKMMapEdMarkerReveal = class
  private
    fOwner: TKMHandID;
    fIndex: Integer;
    fOnDone: TNotifyEvent;
    fType: TKMMapEdMarkerType;
    procedure Marker_Change(Sender: TObject);
  protected
    Panel_MarkerReveal: TKMPanel;
    Label_MarkerType: TKMLabel;
    Image_MarkerPic: TKMImage;
    TrackBar_RevealSize: TKMTrackBar;
    Button_RevealDelete: TKMButton;
    Button_RevealClose: TKMButton;
  public
    constructor Create(aParent: TKMPanel; aOnDone: TNotifyEvent);

    property Index: Integer read fIndex;
    property Owner: TKMHandID read fOwner;
    property MarkerType: TKMMapEdMarkerType read fType;

    procedure Show(aPlayer: TKMHandID; aIndex: Integer; aType : TKMMapEdMarkerType);
    procedure Hide;
    function Visible: Boolean;
  end;


implementation
uses
  KM_Game,
  KM_InterfaceGame,
  KM_HandsCollection,
  KM_ResTexts, KM_ResFonts, KM_ResTypes,
  KM_RenderUI,
  KM_CommonClasses;


{ TKMMapEdMarkerReveal }
constructor TKMMapEdMarkerReveal.Create(aParent: TKMPanel; aOnDone: TNotifyEvent);
begin
  inherited Create;

  fOnDone := aOnDone;

  Panel_MarkerReveal := TKMPanel.Create(aParent, TB_PAD, 50, TB_MAP_ED_WIDTH - TB_PAD, 400);

  Label_MarkerType := TKMLabel.Create(Panel_MarkerReveal, 0, 10, Panel_MarkerReveal.Width, 0, '', fntOutline, taCenter);
  Image_MarkerPic := TKMImage.Create(Panel_MarkerReveal, 0, 10, 32, 32, 338);

  TrackBar_RevealSize := TKMTrackBar.Create(Panel_MarkerReveal, 0, 45, Panel_MarkerReveal.Width, 1, 64);
  TrackBar_RevealSize.Caption := gResTexts[TX_MAPED_FOG_RADIUS];
  TrackBar_RevealSize.OnChange := Marker_Change;

  Button_RevealDelete := TKMButton.Create(Panel_MarkerReveal, 0, 100, 25, 25, 340, rxGui, bsGame);
  Button_RevealDelete.Hint := gResTexts[TX_MAPED_DELETE_REVEALER_HINT];
  Button_RevealDelete.OnClick := Marker_Change;

  Button_RevealClose := TKMButton.Create(Panel_MarkerReveal, Panel_MarkerReveal.Width-100, 100, 100, 25, gResTexts[TX_MAPED_CLOSE], bsGame);
  Button_RevealClose.Hint := gResTexts[TX_MAPED_CLOSE_REVEALER_HINT];
  Button_RevealClose.OnClick := Marker_Change;
end;


procedure TKMMapEdMarkerReveal.Marker_Change(Sender: TObject);
var
  rev: TKMPointTagList;
begin
  //Shortcut to structure we update
  rev := gGame.MapEditor.Revealers[fOwner];

  if Sender = TrackBar_RevealSize then
  begin

    case fType of
      mmtRevealFOW: rev.Tag[fIndex] := TrackBar_RevealSize.Position;
      mmtDefendPos: gHands[fOwner].AI.General.DefendPositions[fIndex].Radius := TrackBar_RevealSize.Position;
    end;
  end;

  if Sender = Button_RevealDelete then
  begin
    rev.Delete(fIndex);
    If rev.Count = 0 then
    begin
      Hide;
      fOnDone(Self);
    end else
    begin
      fIndex := Min(fIndex, rev.Count - 1);
      Show(fOwner, fIndex, fType);
    end;
  end;

  if Sender = Button_RevealClose then
  begin
    Hide;
    fOnDone(Self);
  end;
end;


procedure TKMMapEdMarkerReveal.Show(aPlayer: TKMHandID; aIndex: Integer; aType : TKMMapEdMarkerType);
begin
  fOwner := aPlayer;
  fIndex := aIndex;
  fType := aType;
  Image_MarkerPic.FlagColor := gHands[fOwner].FlagColor;
  Label_MarkerType.Caption := gResTexts[TX_MAPED_FOG];
  Image_MarkerPic.TexID := 393;
  case fType of
    mmtRevealFOW: TrackBar_RevealSize.Position := gGame.MapEditor.Revealers[fOwner].Tag[fIndex];
    mmtDefendPos: TrackBar_RevealSize.Position := gHands[fOwner].AI.General.DefendPositions[aIndex].Radius;
  end;


  Panel_MarkerReveal.Show;
end;


procedure TKMMapEdMarkerReveal.Hide;
begin
  Panel_MarkerReveal.Hide;
end;


function TKMMapEdMarkerReveal.Visible: Boolean;
begin
  Result := Panel_MarkerReveal.Visible;
end;


end.

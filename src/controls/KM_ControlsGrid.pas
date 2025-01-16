unit KM_ControlsGrid;
{$I KaM_Remake.inc}
interface
uses
  Classes, Vcl.Controls,
  KromOGLUtils,
  KM_Controls,
  KM_ResTypes,
  KM_ResFonts,
  KM_CommonTypes,
  KM_Points;

type
  TKMGridItem = record
    Left, Top, Width, Height : Integer;
    Checked,
    Enabled,
    Visible : Boolean;

    RX : TRXType;
    TexID : Word;
    Tag : Integer;
    Caption : String;
    IdX, IdY : Word;
  end;
  PKMGridItem = ^TKMGridItem;

  TGridItemEvent = procedure(Sender : TObject; aItem : PKMGridItem) of object;
  TGridItemEventPos = procedure(Sender : TObject; X, Y : Integer; aItem : PKMGridItem) of object;

  TKMGridCommon = class(TKMControl)
  private
    fSpaceX,
    fSpaceY : Integer;
    fItemSizeX, fItemSizeY,
    fSizeX, fSizeY : Word;
    fMouseDownItem,
    fMouseOverItem : PKMGridItem;
    fItems : array of array of TKMGridItem;
    function GetItem(aIndex, aIndex2 : Integer) : PKMGridItem;
    function GetItemAtPos(X, Y : Integer) : PKMGridItem;
    procedure RefreshItems;
    procedure SetSizeX(aX: Word);
    procedure SetSizeY(aY: Word);

    procedure SetSpaceX(aX: Integer);
    procedure SetSpaceY(aY: Integer);

    procedure DoItemDown(aItem : PKMGridItem); virtual;
    procedure DoItemMove(aItem : PKMGridItem); virtual;

    procedure PaintItem(aItem : PKMGridItem); virtual;

  public
    OnChange: TGridItemEvent;
    OnChangePos: TGridItemEventPos;
    OnMouseUp: TNotifyEvent;

    constructor Create(aParent : TKMPanel; aLeft, aTop, aSizeX, aSizeY : Integer); overload;
    procedure MouseDown (X,Y: Integer; Shift: TShiftState; Button: TMouseButton); override;
    procedure MouseMove (X,Y: Integer; Shift: TShiftState); override;
    procedure MouseUp   (X,Y: Integer; Shift: TShiftState; Button: TMouseButton); override;

    property Item[aIndex, aIndex2 : Integer] : PKMGridItem read GetItem;
    property ItemOver : PKMGridItem read fMouseOverItem;
    property ItemDown : PKMGridItem read fMouseDownItem;

    procedure SetSpace(aX, aY: Integer);
    property SpaceX : Integer read fSpaceX write SetSpaceX;
    property SpaceY : Integer read fSpaceY write SetSpaceY;

    procedure SetSize(aX, aY: Word);
    property SizeX : Word read fSizeX write SetSizeX;
    property SizeY : Word read fSizeY write SetSizeY;

    procedure SetItemSize(aWidth, aHeight : Word);

    procedure Paint; override;

  end;

  TKMCheckBoxGrid = class(TKMGridCommon)
    private
      procedure DoItemDown(aItem : PKMGridItem); override;
      procedure DoItemMove(aItem : PKMGridItem); override;
      procedure PaintItem(aItem : PKMGridItem); override;
  end;


implementation
uses
  SysUtils, Math,
  KM_RenderUI,
  KM_Resource,
  KM_Defaults;

constructor TKMGridCommon.Create(aParent: TKMPanel; aLeft: Integer; aTop: Integer; aSizeX: Integer; aSizeY: Integer);
begin
  Inherited Create(aParent, aLeft, aTop, 0, 0);

  fSizeX := aSizeX;
  fSizeY := aSizeY;

  fSpaceX := 20;
  fSpaceY := 20;

  fItemSizeX := 18;
  fItemSizeY := 18;

  RefreshItems;
end;


function TKMGridCommon.GetItemAtPos(X: Integer; Y: Integer): PKMGridItem;
var I, K : Integer;
  pX, pY : Integer;
begin
  Result := nil;
  If (fSizeX = 0) or (fSizeY = 0) then
    Exit;
  pX := AbsLeft;
  pY := AbsTop;
  for I := 0 to fSizeX - 1 do
    for K := 0 to fSizeY - 1 do
    if InRange(X, fItems[I, K].Left + pX, fItems[I, K].Left + pX + fItems[I, K].Width)
    and InRange(Y, fItems[I, K].Top + pY, fItems[I, K].Top + pY + fItems[I, K].Height)
    and fItems[I, K].Visible
    and fItems[I, K].Enabled then
    begin
      Result := @fItems[I, K];
      Exit;
    end;
end;

procedure TKMGridCommon.MouseDown(X: Integer; Y: Integer; Shift: TShiftState; Button: TMouseButton);
begin
  Inherited;

  if fMouseOverItem = nil then
    Exit;

  fMouseDownItem := fMouseOverItem;
  DoItemDown(fMouseDownItem);
end;

procedure TKMGridCommon.MouseUp(X: Integer; Y: Integer; Shift: TShiftState; Button: TMouseButton);
begin
  Inherited;
  fMouseDownItem := nil;
  If Assigned(OnMouseUp) then
    OnMouseUp(Self);
end;

procedure TKMGridCommon.MouseMove(X: Integer; Y: Integer; Shift: TShiftState);
begin
  Inherited;
  fMouseOverItem := GetItemAtPos(X, Y);
  DoItemMove(fMouseOverItem);

end;

procedure TKMGridCommon.DoItemDown(aItem: PKMGridItem);
begin
  //only for children types
end;

procedure TKMGridCommon.DOItemMove(aItem: PKMGridItem);
begin
  //only for children types
end;

function TKMGridCommon.GetItem(aIndex: Integer; aIndex2: Integer): PKMGridItem;
begin
  Result := @fItems[aIndex, aIndex2];
end;

procedure TKMGridCommon.RefreshItems;
var I, K : Integer;
begin
  SetLength(fItems, fSizeX, fSizeY);
  for I := 0 to fSizeX - 1 do
    for K := 0 to fSizeY - 1 do
    begin
      fItems[I, K].Left := I * fSpaceX;
      fItems[I, K].Top := K * fSpaceY;
      fItems[I, K].Width := fItemSizeX;
      fItems[I, K].Height := fItemSizeY;
      fItems[I, K].Enabled := true;
      fItems[I, K].Visible := true;
      fItems[I, K].IdX := I;
      fItems[I, K].IdY := K;
    end;
  Width := fSizeX * fSpaceX;
  Height := fSizeY * fSpaceY;
end;

procedure TKMGridCommon.SetSize(aX: Word; aY: Word);
begin
  fSizeX := aX;
  fSizeY := aY;
  RefreshItems;
end;

procedure TKMGridCommon.SetSizeX(aX: Word);
begin
  SetSize(aX, fSizeY);
end;

procedure TKMGridCommon.SetSizeY(aY: Word);
begin
  SetSize(fSizeX, aY);
end;

procedure TKMGridCommon.SetSpace(aX: Integer; aY: Integer);
begin
  fSpaceX := aX;
  fSpaceY := aY;
  RefreshItems;
end;

procedure TKMGridCommon.SetSpaceX(aX: Integer);
begin
  SetSpace(aX, fSpaceY);
end;

procedure TKMGridCommon.SetSpaceY(aY: Integer);
begin
  SetSpace(fSpaceX, aY);
end;

procedure TKMGridCommon.SetItemSize(aWidth: Word; aHeight: Word);
begin
  fItemSizeX := aWidth;
  fItemSizeX := aHeight;
  RefreshItems;
end;

procedure TKMGridCommon.PaintItem(aItem: PKMGridItem);
begin

end;

procedure TKMGridCommon.Paint;
var I, K : Integer;
begin
  Inherited;
  if not Visible then
    Exit;

  for I := 0 to fSizeX - 1 do
    for K := 0 to fSizeY - 1 do
      PaintItem(Item[I, K]);

end;

procedure TKMCheckBoxGrid.DoItemDown(aItem: PKMGridItem);
begin
  if fMouseDownItem = nil then
    Exit;
  fMouseDownItem.Checked := not fMouseDownItem.Checked;
  If Assigned(OnChange) then
    OnChange(self, fMouseDownItem);

  If Assigned(OnChangePos) then
    OnChangePos(self, fMouseDownItem.IdX, fMouseDownItem.IdY, fMouseOverItem);
end;

procedure TKMCheckBoxGrid.DoItemMove(aItem: PKMGridItem);
begin
  if fMouseDownItem = nil then
    Exit;
  if fMouseOverItem = nil then
    Exit;
  if fMouseDownItem.Checked <> fMouseOverItem.Checked then
  begin
    fMouseOverItem.Checked := fMouseDownItem.Checked;
    If Assigned(OnChange) then
      OnChange(self, fMouseOverItem);
    If Assigned(OnChangePos) then
      OnChangePos(self, fMouseOverItem.IdX, fMouseOverItem.IdY, fMouseOverItem);

  end;


end;

procedure TKMCheckBoxGrid.PaintItem(aItem: PKMGridItem);
begin
  if not aItem.Visible then
    Exit;

  TKMRenderUI.WriteBevel(AbsLeft + aItem.Left, AbsTop + aItem.Top, aItem.Width, aItem.Height,
                        1, 0.2 + Byte(aItem.Enabled and Enabled)*0.3 - 0.25 * byte(fMouseOverItem = aItem));

  if aItem.Checked then
    TKMRenderUI.WritePicture(AbsLeft + aItem.Left, AbsTop + aItem.Top, aItem.Width, aItem.Height,
                              [], rxGuiMain, 32, Enabled and aItem.Enabled);

end;



end.


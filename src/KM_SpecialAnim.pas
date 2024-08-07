unit KM_SpecialAnim;
{$I KaM_Remake.inc}
interface
uses
  KM_Units, KM_Houses, KM_ResTypes,
  KM_CommonClasses, KM_CommonTypes, KM_CommonGameTypes, KM_Points, KM_RenderTypes;


type
  //special animation which are shown only once
  //mainly made for boots dust while unit is walking
  TKMSpecialAnims = class
  private
      fItems: array of record //1..n
        Pos : TKMPointF;
        Anim : TKMAnimation;
        AnimStep : Byte;
        LoopTimes : Integer;
        Visible : Boolean;
        AlphaStep : Single;
        InFront : Boolean;
        RX : TRXType;
        FirstPaint : Boolean;
      end;
      function GetLengthOfImportant : Integer;
    public
      constructor Create;
      function GetLength : Integer;
      procedure UpdateState(aTick : Integer);
      procedure Paint(aTickLag: Single);
      Procedure Add(aAnimLoop: TKMAnimLoop; aLoc: TKMPointF; aLoopTimes: Integer; aRX : TRXType = rxTrees; aInFront : Boolean = false; aAlphaStep : Single = -1);overload;
      Procedure Add(aAnimLoop: TKMAnimation; aLoc: TKMPointF; aLoopTimes: Integer; aRX : TRXType = rxTrees; aInFront : Boolean = false; aAlphaStep : Single = -1);overload;
      Procedure RemoveAnim(aIndex : Integer);
      procedure Save(SaveStream: TKMemoryStream);
      procedure Load(LoadStream: TKMemoryStream);
  end;


var
  gSpecAnim: TKMSpecialAnims;


implementation
uses
  Math, KromUtils,
  //KM_Terrain,
  KM_RenderPool,
  KM_CommonUtils, KM_Defaults;

{ TKMProjectiles }


constructor TKMSpecialAnims.Create;
begin
  inherited Create;
end;

function TKMSpecialAnims.GetLength: Integer;
begin
  result := Length(fItems);
end;

procedure TKMSpecialAnims.Add(aAnimLoop: TKMAnimLoop; aLoc: TKMPointF; aLoopTimes: Integer; aRX : TRXType = rxTrees; aInFront : Boolean = false; aAlphaStep : Single = -1);
var J : Integer;
begin
  J := length(fItems);
  SetLength(fItems, J + 1);
  with fItems[J] do
  begin
    Pos := aLoc;
    Anim.Create(aAnimLoop);
    AnimStep := 0;
    LoopTimes := aLoopTimes - 1;
    Visible := true;
    RX := aRX;
    InFront := aInFront;
    AlphaStep := aAlphaStep;
    FirstPaint := true;
  end;
end;

procedure TKMSpecialAnims.Add(aAnimLoop: TKMAnimation; aLoc: TKMPointF; aLoopTimes: Integer; aRX : TRXType = rxTrees; aInFront : Boolean = false; aAlphaStep : Single = -1);
var J : Integer;
begin
  J := length(fItems);
  SetLength(fItems, J + 1);
  with fItems[J] do
  begin
    Pos := aLoc;
    Anim := aAnimLoop;
    AnimStep := 0;
    LoopTimes := aLoopTimes - 1;
    Visible := true;
    RX := aRX;
    InFront := aInFront;
    AlphaStep := aAlphaStep;
    FirstPaint := true;
  end;
end;
procedure TKMSpecialAnims.RemoveAnim(aIndex: Integer);
var I, J : Integer;
begin
  J := length(fItems);
  Assert(J > 0);
  Assert(aIndex < J);
  for I := aIndex + 1 to J - 1 do
    fItems[I-1] := fItems[I];

  SetLength(fItems, J - 1);
end;

//Update all items positions and kill some targets
procedure TKMSpecialAnims.UpdateState(aTick : Integer);
var I : integer;
  aRemoveArr: TKMWordArray;
begin
  //if aTick mod 2 <> 0 then Exit;
  aRemoveArr := [];

  for I := 0 to High(fItems) do
    with fItems[I] do
      if Visible then
      begin
        if not FirstPaint then
          Inc(AnimStep);

        if AnimStep = Anim.Count then
        begin
          if LoopTimes <= 0 then
          begin
            Visible := false;
            AnimStep := 0;
            //RemoveAnim(I);
            SetLength(aRemoveArr, length(aRemoveArr) + 1);
            aRemoveArr[high(aRemoveArr)] := I;

          end else
          begin
            Dec(LoopTimes);
            AnimStep := 0;
            Visible := True;
          end;
        end;
        FirstPaint := false;


      end;
  for I := High(aRemoveArr) downto 0 do
    RemoveAnim(aRemoveArr[I]);

end;

function TKMSpecialAnims.GetLengthOfImportant: Integer;
var I : Integer;
begin
  Result := 0;
  for I := 0 to High(fItems) do
    with fItems[I] do
      if Visible and (LoopTimes >= 5) then
        Inc(Result);


end;

procedure TKMSpecialAnims.Paint(aTickLag: Single);
var I : integer;
begin
  for I := 0 to High(fItems) do
    with fItems[I] do
      if Visible then
      begin
        if KMInRect(Pos, gRenderPool.ViewPort.GetClip) then
          gRenderPool.AddSpecAnim(Pos, Anim, AnimStep, RX, InFront, AlphaStep);
        if FirstPaint then
          FirstPaint := false;
      end;
end;


procedure TKMSpecialAnims.Save(SaveStream: TKMemoryStream);
var I, newCount : Integer;
begin
  SaveStream.PlaceMarker('SpecialAnim');
  newCount := GetLengthOfImportant;
  SaveStream.Write(newCount);

  for I := 0 to High(fItems) do
    with fItems[I] do
      if Visible and (LoopTimes >= 5) then
      begin
        SaveStream.Write(Pos);
        SaveStream.Write(Anim, SizeOf(Anim));
        SaveStream.Write(AnimStep);
        SaveStream.Write(InFront);
        SaveStream.Write(AlphaStep);
        SaveStream.Write(FirstPaint);
        SaveStream.Write(LoopTimes);
        SaveStream.Write(RX, SizeOf(RX));
      end;

end;


procedure TKMSpecialAnims.Load(LoadStream: TKMemoryStream);
var I, newCount : Integer;
  aLoc : TKMPointF;
  aAnim : TKMAnimLoop;
  aAnimStep : Byte;
  aLoopTimes : Integer;
  aInFront, aFirstPaint : Boolean;
  aAlphaStep : Single;
  aRX : TRXType;
begin
  LoadStream.CheckMarker('SpecialAnim');
  LoadStream.Read(newCount);
  for I := 0 to newCount - 1 do
  begin
    LoadStream.Read(aLoc);
    LoadStream.Read(aAnim, SizeOf(aAnim));
    LoadStream.Read(aAnimStep);
    LoadStream.Read(aInFront);
    LoadStream.Read(aAlphaStep);
    LoadStream.Read(aFirstPaint);
    LoadStream.Read(aLoopTimes);
    LoadStream.Read(aRX, SizeOf(aRX));
    Add(aAnim, aLoc, aLoopTimes,aRX, aInFront, aAlphaStep);
    fItems[high(fItems)].AnimStep :=  aAnimStep;
    fItems[high(fItems)].FirstPaint :=  aFirstPaint;
  end;
end;



end.

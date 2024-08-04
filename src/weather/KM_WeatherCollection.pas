unit KM_WeatherCollection;
{$I KaM_Remake.inc}
interface
uses
  KM_CommonTypes,
  KM_CommonClasses,
  KM_Points,
  KM_ResTypes,
  KM_WeatherTypes,
  KM_Weather;

  type
    TKMWeatherCollection = class
      private
        fList : array of TKMWeather;
        function GetCount : Word;
        procedure AddItem(aWeather : TKMWeather); overload;
      public
        constructor Create;
        destructor Destroy;
        procedure AddItem(aType : TKMWeatherType; aPos, aSpeed : TKMPointF; aLifeTime : Cardinal; aRX: TRXType; aAnimation : TKMAnimation); overload;

        property Count : Word read GetCount;

        procedure Save(SaveStream : TKMemoryStream);
        procedure Load(LoadStream : TKMemoryStream);
        procedure UpdateState;
        procedure Paint(aLag : Single; aClipRect : TKMRect);
    end;

implementation

constructor TKMWeatherCollection.Create;
begin
  Inherited;
  SetLength(fList, 0);
end;

destructor TKMWeatherCollection.Destroy;
var I : Integer;
begin
  for I := 0 to High(fList) do
    fList[I].Free;

  Inherited;
end;

function TKMWeatherCollection.GetCount: Word;
begin
  Result := length(fList);
end;

procedure TKMWeatherCollection.AddItem(aWeather: TKMWeather);
begin
  SetLength(fList, Count + 1);
  fList[Count - 1] := aWeather;
end;

procedure TKMWeatherCollection.AddItem(aType: TKMWeatherType; aPos: TKMPointF; aSpeed: TKMPointF; aLifeTime: Cardinal; aRX: TRXType; aAnimation: TKMAnimation);
begin
  SetLength(fList, Count + 1);

  fList[Count - 1] := TKMWeather.Create(aType, aPos, aSpeed, aLifeTime, aRX, aAnimation);
end;

procedure TKMWeatherCollection.Save(SaveStream: TKMemoryStream);
var I, newCount : Integer;
begin
  SaveStream.PlaceMarker('WeatherCollection');
  newCount := Count;
  SaveStream.Write(newCount);
  for I := 0 to Count - 1 do
    fList[I].Save(SaveStream);
end;

procedure TKMWeatherCollection.Load(LoadStream: TKMemoryStream);
var I, newCount : Integer;
  tmp : TKMWeather;
begin
  LoadStream.CheckMarker('WeatherCollection');
  LoadStream.Read(newCount);

  for I := 0 to newCount - 1 do
  begin
    tmp := TKMWeather.Load(LoadStream);
    if not tmp.Deleted then
      AddItem(tmp);
  end;
end;

procedure TKMWeatherCollection.UpdateState;
var I : Integer;
begin
  for I := 0 to Count - 1 do
    fList[I].UpdateState;
end;

procedure TKMWeatherCollection.Paint(aLag : Single; aClipRect : TKMRect);
var I : Integer;
begin
  for I := 0 to Count - 1 do
    fList[I].Paint(aLag, aClipRect);
end;

end.

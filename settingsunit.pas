unit SettingsUnit;

{$mode ObjFPC}{$H+}

interface

uses
  // system
  Classes, Graphics;
  // main

// end uses

type
  TTextPosition = (
    posTopLeft, posTopCenter, posTopRight,
    posBottomLeft, posBottomCenter, posBottomRight
  );

  { TAppSettings }

  TAppSettings = class
  private
    FFontName: string;
    FFontSize: Integer;
    FFontColor: TColor;
    FTextPosition: TTextPosition;
  public
    constructor Create();
    function GetPosition(ATextWidth: Integer; ATextHeight: Integer): TPoint;
    property FontName: string read FFontName;
    property FontSize: Integer read FFontSize;
    property FontColor: TColor read FFontColor;
    property TextPosition: TTextPosition read FTextPosition;
  end;

var
  AppSettings: TAppSettings;

implementation

uses
  // system
  Forms;
  // main

// end uses

{ TAppSettings }

constructor TAppSettings.Create();
begin
  FFontName := '';
  FFontSize := 24;
  FFontColor := TColor($6040FF);
  FTextPosition := posTopCenter;
end;

function TAppSettings.GetPosition(ATextWidth: Integer; ATextHeight: Integer): TPoint;
begin
  case FTextPosition of
    posTopLeft:
      Result := TPoint.Create(0, 0);
    posTopCenter:
      Result := TPoint.Create((Screen.Width - ATextWidth) div 2, 0);
    posTopRight:
      Result := TPoint.Create(Screen.Width - ATextWidth, 0);
    posBottomLeft:
      Result := TPoint.Create(0, Screen.Height - ATextHeight);
    posBottomCenter:
      Result := TPoint.Create((Screen.Width - ATextWidth) div 2, Screen.Height - ATextHeight);
    posBottomRight:
      Result := TPoint.Create(Screen.Width - ATextWidth, Screen.Height - ATextHeight);
  end;
end;

end.


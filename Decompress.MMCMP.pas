unit Decompress.MMCMP;

// MMCMP (Zirconia) decompressor.
// Taken from Mmcmp.cpp (ModPlug Tracker source code) and
// converted from C++ to C (to Pascal).
// libmodplug is public domain, so this file should be
// able to go under BSD 3-clause.

{$MODE OBJFPC}
{$H+}
{$R-}
{$MACRO ON}
{$COPERATORS ON}

interface

uses
	Classes, SysUtils;

const
	MMCMP_COMP  = $0001;
	MMCMP_DELTA = $0002;
	MMCMP_16BIT = $0004;
	MMCMP_ABS16 = $0200;


	function  UnpackMMCMP(Stream: TStream): Boolean;


implementation


function UnpackMMCMP(Stream: TStream): Boolean;
begin
	Result := False;
end;


end.


unit IT.AudioDriver.HQ;

{$MODE OBJFPC}
{$H+}
{$R-}
{$COPERATORS ON}
{$MACRO ON}

interface

uses
	Classes, SysUtils, Math,
	IT2play;

const
	UINT32_MAX = $FFFFFFFF;

type
	Float = Double;

	TMixFunction = procedure(sc: TSlaveChannel; MixBufPtr: PFloat; NumSamples: Cardinal) of object;

	TITAudioDriver_HQ = class(TITAudioDriver)
	const
		RAMPSPEED        = 8;  // slightly faster than SB16 MMX driver

		SINC_WIDTH       = 8;
		SINC_WIDTH_BITS  = 3;  // log2(SINC_WIDTH)
		SINC_PHASES      = 8192;
		SINC_PHASES_BITS = 13; // log2(SINC_PHASES)
		SINC_FSHIFT      = 32 - (SINC_PHASES_BITS + SINC_WIDTH_BITS);
		SINC_FMASK       = (SINC_WIDTH * SINC_PHASES) - SINC_WIDTH;

		// Higher is better. 14 is max for audio output rates of 44100Hz (and higher).
		FREQ_MUL_EXTRA_BITS = 14;

		BPM_FRAC_BITS  = 31; // absolute max for 32-bit arithmetics, don't change!
		BPM_FRAC_SCALE = Cardinal(1) << BPM_FRAC_BITS;
		BPM_FRAC_MASK  = BPM_FRAC_SCALE - 1;
	protected
		procedure UpdateNoLoop      (sc: TSlaveChannel; NumSamples: Cardinal); override;
		procedure UpdateForwardsLoop(sc: TSlaveChannel; NumSamples: Cardinal); override;
		procedure UpdatePingPongLoop(sc: TSlaveChannel; NumSamples: Cardinal); override;
	private
		SincLUT: array of Float;

		RealBytesToMix, FreqMulVal: Integer;
		BytesToMixFractional, CurrentFractional, RandSeed: Cardinal;
		SamplesPerTickInt, SamplesPerTickFrac: array [0..256-LOWEST_BPM_POSSIBLE-1] of Cardinal;
		LastValue, PrngState, LastClickRemoval: array[Boolean] of Float;

		MixBuffer: array of Float;

		MixFunctions: array [Boolean, Boolean, Boolean, Boolean] of TMixFunction;

		procedure Mix8Bit                       (sc: TSlaveChannel; MixBufPtr: PFloat; NumSamples: Cardinal);
		procedure Mix16Bit                      (sc: TSlaveChannel; MixBufPtr: PFloat; NumSamples: Cardinal);
		procedure MixSurround8Bit               (sc: TSlaveChannel; MixBufPtr: PFloat; NumSamples: Cardinal);
		procedure MixSurround16Bit              (sc: TSlaveChannel; MixBufPtr: PFloat; NumSamples: Cardinal);
		procedure Mix8BitStereo                 (sc: TSlaveChannel; MixBufPtr: PFloat; NumSamples: Cardinal);
		procedure Mix16BitStereo                (sc: TSlaveChannel; MixBufPtr: PFloat; NumSamples: Cardinal);
		procedure MixSurround8BitStereo         (sc: TSlaveChannel; MixBufPtr: PFloat; NumSamples: Cardinal);
		procedure MixSurround16BitStereo        (sc: TSlaveChannel; MixBufPtr: PFloat; NumSamples: Cardinal);
		procedure MixFiltered8Bit               (sc: TSlaveChannel; MixBufPtr: PFloat; NumSamples: Cardinal);
		procedure MixFiltered16Bit              (sc: TSlaveChannel; MixBufPtr: PFloat; NumSamples: Cardinal);
		procedure MixFilteredSurround8Bit       (sc: TSlaveChannel; MixBufPtr: PFloat; NumSamples: Cardinal);
		procedure MixFilteredSurround16Bit      (sc: TSlaveChannel; MixBufPtr: PFloat; NumSamples: Cardinal);
		procedure MixFiltered8BitStereo         (sc: TSlaveChannel; MixBufPtr: PFloat; NumSamples: Cardinal);
		procedure MixFiltered16BitStereo        (sc: TSlaveChannel; MixBufPtr: PFloat; NumSamples: Cardinal);
		procedure MixFilteredSurround8BitStereo (sc: TSlaveChannel; MixBufPtr: PFloat; NumSamples: Cardinal);
		procedure MixFilteredSurround16BitStereo(sc: TSlaveChannel; MixBufPtr: PFloat; NumSamples: Cardinal);

		procedure InitWindowedSincLUT;
	public
		procedure SetTempo(Tempo: Byte); override;
		procedure ResetMixer; override;
		procedure FixSamples; override;
		procedure MixSamples; override;
		procedure Mix(NumSamples: Integer; AudioOut: PInt16); override;

		function  PostMix(AudioOut16: PInt16; SamplesLeft: Integer; {%H-}SampleShiftValue: Byte = 0): Integer; override;

		constructor Create(AModule: TITModule; DriverType: TITAudioDriverType; MixingFrequency: Integer); override;
	end;

implementation

// Common macros for mixing functions

{$DEFINE SincInterpolationCommon :=
	((s[-3] * t[0]) + (s[-2] * t[1]) + (s[-1] * t[2]) + (s[+0] * t[3]) +
	 (s[+1] * t[4]) + (s[+2] * t[5]) + (s[+3] * t[6]) + (s[+4] * t[7])) }

{$DEFINE GetSamplePtrs :=
	base := sc.Sample.Data[False].Data;
	smp  := base + sc.SamplingPosition; }

{$DEFINE GetSamplePtrsStereo :=
	GetSamplePtrs;
	smpR := sc.Sample.Data[True].Data + sc.SamplingPosition; }

{$DEFINE GetSincLUT := @SincLUT[(sc.Frac64 >> SINC_FSHIFT and UINT32_MAX) and SINC_FMASK] }

{$DEFINE Get8BitWaveForm :=
	t := GetSincLUT;
	fSample := SincInterpolation8(smp, t); }

{$DEFINE Get16BitWaveForm :=
	t := GetSincLUT;
	fSample := SincInterpolation16(smp, t); }

{$DEFINE Get8BitStereoWaveForm :=
	t := GetSincLUT;
	fSample  := SincInterpolation8(smp,  t);
	fSampleR := SincInterpolation8(smpR, t); }

{$DEFINE Get16BitStereoWaveForm :=
	t := GetSincLUT;
	fSample  := SincInterpolation16(smp,  t);
	fSampleR := SincInterpolation16(smpR, t); }

{$DEFINE FilterSample :=
	fSample := (fSample * sc.fFiltera) + (sc.fOldSamples[0] * sc.fFilterb) + (sc.fOldSamples[1] * sc.fFilterc);
	fSample := EnsureRange(fSample, -2.0, +2.0);
	sc.fOldSamples[1] := sc.fOldSamples[0];
	sc.fOldSamples[0] := fSample; }

{$DEFINE FilterStereoSample :=
	fSample  := (fSample  * sc.fFiltera) + (sc.fOldSamples[0] * sc.fFilterb) + (sc.fOldSamples[1] * sc.fFilterc);
	fSampleR := (fSampleR * sc.fFiltera) + (sc.fOldSamples[2] * sc.fFilterb) + (sc.fOldSamples[3] * sc.fFilterc);
	fSample  := EnsureRange(fSample, -2.0, +2.0);
	fSampleR := EnsureRange(fSample, -2.0, +2.0);
	sc.fOldSamples[1] := sc.fOldSamples[0]; sc.fOldSamples[0] := fSample;
	sc.fOldSamples[3] := sc.fOldSamples[2]; sc.fOldSamples[2] := fSampleR; }

{$DEFINE RampCurrVolumeL := sc.fCurrVol[False] += (sc.fVolume[False] - sc.fCurrVol[False]) * ((1 << RAMPSPEED) / 16384); }
{$DEFINE RampCurrVolumeR := sc.fCurrVol[True]  += (sc.fVolume[True]  - sc.fCurrVol[True])  * ((1 << RAMPSPEED) / 16384); }
{$DEFINE RampCurrVolumeS := RampCurrVolumeL; RampCurrVolumeR; }

{$DEFINE MixSample :=
	LastValue[False] := fSample * sc.fCurrVol[False];
	LastValue[True]  := fSample * sc.fCurrVol[True];
	MixBufPtr^ += LastValue[False]; Inc(MixBufPtr);
	MixBufPtr^ += LastValue[True];  Inc(MixBufPtr); }

{$DEFINE MixSampleSurround :=
	LastValue[False] := fSample * sc.fCurrVol[False];
	MixBufPtr^ += LastValue[False]; Inc(MixBufPtr);
	MixBufPtr^ -= LastValue[False]; Inc(MixBufPtr); }

{$DEFINE MixStereoSample :=
	LastValue[False] := fSample  * sc.fCurrVol[False];
	LastValue[True]  := fSampleR * sc.fCurrVol[True];
	MixBufPtr^ += LastValue[False]; Inc(MixBufPtr);
	MixBufPtr^ += LastValue[True];  Inc(MixBufPtr); }

{$DEFINE MixStereoSampleSurround :=
	LastValue[False] := fSample  * sc.fCurrVol[False];
	LastValue[True]  := fSampleR * sc.fCurrVol[False];
	MixBufPtr^ += LastValue[False]; Inc(MixBufPtr);
	MixBufPtr^ -= LastValue[True];  Inc(MixBufPtr); }

{$DEFINE UpdatePos :=
	sc.Frac64 += Delta64;
	smp += Integer(sc.Frac64 >> 32);
	sc.Frac64 := sc.Frac64 and UINT32_MAX; }

{$DEFINE UpdatePosStereo :=
	sc.Frac64 += Delta64;
	WholeSamples := Integer(sc.Frac64 >> 32);
	smp  += WholeSamples;
	smpR += WholeSamples;
	sc.Frac64 := sc.Frac64 and UINT32_MAX; }

{$DEFINE MixIt :=
	GetSamplePtrs;
	for i := 1 to NumSamples do DoMix;
	sc.SamplingPosition := Int32(smp - base); }

{$DEFINE MixItStereo :=
	GetSamplePtrsStereo;
	for i := 1 to NumSamples do DoMix;
	sc.SamplingPosition := Int32(smp - base); }

function SincInterpolation8(s: PInt8; t: PFloat): Float; inline;
begin
	Result := (1.0 / 128) * SincInterpolationCommon;
end;

function SincInterpolation16(s: PInt16; t: PFloat): Float; inline;
begin
	Result := (1.0 / 32768) * SincInterpolationCommon;
end;

// ================================================================================================
// Mixing routines
// ================================================================================================

procedure TITAudioDriver_HQ.Mix8Bit
	(sc: TSlaveChannel; MixBufPtr: PFloat; NumSamples: Cardinal);
var
	i: Integer;
	t: PFloat;
	fSample: Float;
	base, smp: PInt8;

	procedure DoMix; inline;
	begin
		Get8BitWaveForm;
		MixSample;
		UpdatePos;
		RampCurrVolumeS;
	end;
begin
	MixIt;
end;

procedure TITAudioDriver_HQ.Mix16Bit
	(sc: TSlaveChannel; MixBufPtr: PFloat; NumSamples: Cardinal);
var
	i: Integer;
	t: PFloat;
	fSample: Float;
	base, smp: PInt16;

	procedure DoMix; inline;
	begin
		Get16BitWaveForm;
		MixSample;
		UpdatePos;
		RampCurrVolumeS;
	end;
begin
	MixIt;
end;

procedure TITAudioDriver_HQ.MixSurround8Bit
	(sc: TSlaveChannel; MixBufPtr: PFloat; NumSamples: Cardinal);
var
	i: Integer;
	t: PFloat;
	fSample: Float;
	base, smp: PInt8;

	procedure DoMix; inline;
	begin
		Get8BitWaveForm;
		MixSampleSurround;
		UpdatePos;
		RampCurrVolumeS;
	end;
begin
	MixIt;
	LastValue[True] := -LastValue[False];
end;

procedure TITAudioDriver_HQ.MixSurround16Bit
	(sc: TSlaveChannel; MixBufPtr: PFloat; NumSamples: Cardinal);
var
	i: Integer;
	t: PFloat;
	fSample: Float;
	base, smp: PInt16;

	procedure DoMix; inline;
	begin
		Get16BitWaveForm;
		MixSampleSurround;
		UpdatePos;
		RampCurrVolumeS;
	end;
begin
	MixIt;
	LastValue[True] := -LastValue[False];
end;

procedure TITAudioDriver_HQ.Mix8BitStereo
	(sc: TSlaveChannel; MixBufPtr: PFloat; NumSamples: Cardinal);
var
	i, WholeSamples: Integer;
	t: PFloat;
	fSample, fSampleR: Float;
	base, smp, smpR: PInt8;

	procedure DoMix; inline;
	begin
		Get8BitStereoWaveForm;
		MixStereoSample;
		UpdatePosStereo;
		RampCurrVolumeS;
	end;
begin
	MixItStereo;
end;

procedure TITAudioDriver_HQ.Mix16BitStereo
	(sc: TSlaveChannel; MixBufPtr: PFloat; NumSamples: Cardinal);
var
	i, WholeSamples: Integer;
	t: PFloat;
	fSample, fSampleR: Float;
	base, smp, smpR: PInt16;

	procedure DoMix; inline;
	begin
		Get16BitStereoWaveForm;
		MixStereoSample;
		UpdatePosStereo;
		RampCurrVolumeS;
	end;
begin
	MixItStereo;
end;

procedure TITAudioDriver_HQ.MixSurround8BitStereo
	(sc: TSlaveChannel; MixBufPtr: PFloat; NumSamples: Cardinal);
var
	i, WholeSamples: Integer;
	t: PFloat;
	fSample, fSampleR: Float;
	base, smp, smpR: PInt8;

	procedure DoMix; inline;
	begin
		Get8BitStereoWaveForm;
		MixStereoSampleSurround;
		UpdatePosStereo;
		RampCurrVolumeL;
	end;
begin
	MixItStereo;
	LastValue[True] := -LastValue[True];
end;

procedure TITAudioDriver_HQ.MixSurround16BitStereo
	(sc: TSlaveChannel; MixBufPtr: PFloat; NumSamples: Cardinal);
var
	i, WholeSamples: Integer;
	t: PFloat;
	fSample, fSampleR: Float;
	base, smp, smpR: PInt16;

	procedure DoMix; inline;
	begin
		Get16BitStereoWaveForm;
		MixStereoSampleSurround;
		UpdatePosStereo;
		RampCurrVolumeL;
	end;
begin
	MixItStereo;
	LastValue[True] := -LastValue[True];
end;

procedure TITAudioDriver_HQ.MixFiltered8Bit
	(sc: TSlaveChannel; MixBufPtr: PFloat; NumSamples: Cardinal);
var
	i: Integer;
	t: PFloat;
	fSample: Float;
	base, smp: PInt8;

	procedure DoMix; inline;
	begin
		Get8BitWaveForm;
		FilterSample;
		MixSample;
		UpdatePos;
		RampCurrVolumeS;
	end;
begin
	MixIt;
end;

procedure TITAudioDriver_HQ.MixFiltered16Bit
	(sc: TSlaveChannel; MixBufPtr: PFloat; NumSamples: Cardinal);
var
	i: Integer;
	t: PFloat;
	fSample: Float;
	base, smp: PInt16;

	procedure DoMix; inline;
	begin
		Get16BitWaveForm;
		FilterSample;
		MixSample;
		UpdatePos;
		RampCurrVolumeS;
	end;
begin
	MixIt;
end;

procedure TITAudioDriver_HQ.MixFilteredSurround8Bit
	(sc: TSlaveChannel; MixBufPtr: PFloat; NumSamples: Cardinal);
var
	i: Integer;
	t: PFloat;
	fSample: Float;
	base, smp: PInt8;

	procedure DoMix; inline;
	begin
		Get8BitWaveForm;
		FilterSample;
		MixSampleSurround;
		UpdatePos;
		RampCurrVolumeL;
	end;
begin
	MixIt;
	LastValue[True] := -LastValue[False];
end;

procedure TITAudioDriver_HQ.MixFilteredSurround16Bit
	(sc: TSlaveChannel; MixBufPtr: PFloat; NumSamples: Cardinal);
var
	i: Integer;
	t: PFloat;
	fSample: Float;
	base, smp: PInt16;

	procedure DoMix; inline;
	begin
		Get16BitWaveForm;
		FilterSample;
		MixSampleSurround;
		UpdatePos;
		RampCurrVolumeL;
	end;
begin
	MixIt;
	LastValue[True] := -LastValue[False];
end;

procedure TITAudioDriver_HQ.MixFiltered8BitStereo
	(sc: TSlaveChannel; MixBufPtr: PFloat; NumSamples: Cardinal);
var
	i, WholeSamples: Integer;
	t: PFloat;
	fSample, fSampleR: Float;
	base, smp, smpR: PInt8;

	procedure DoMix; inline;
	begin
		Get8BitStereoWaveForm;
		FilterStereoSample;
		MixStereoSample;
		UpdatePosStereo;
		RampCurrVolumeS;
	end;
begin
	MixItStereo;
end;

procedure TITAudioDriver_HQ.MixFiltered16BitStereo
	(sc: TSlaveChannel; MixBufPtr: PFloat; NumSamples: Cardinal);
var
	i, WholeSamples: Integer;
	t: PFloat;
	fSample, fSampleR: Float;
	base, smp, smpR: PInt16;

	procedure DoMix; inline;
	begin
		Get16BitStereoWaveForm;
		FilterStereoSample;
		MixStereoSample;
		UpdatePosStereo;
		RampCurrVolumeS;
	end;
begin
	MixItStereo;
end;

procedure TITAudioDriver_HQ.MixFilteredSurround8BitStereo
	(sc: TSlaveChannel; MixBufPtr: PFloat; NumSamples: Cardinal);
var
	i, WholeSamples: Integer;
	t: PFloat;
	fSample, fSampleR: Float;
	base, smp, smpR: PInt8;

	procedure DoMix; inline;
	begin
		Get8BitStereoWaveForm;
		FilterStereoSample;
		MixStereoSampleSurround;
		UpdatePosStereo;
		RampCurrVolumeL;
	end;
begin
	MixItStereo;
	LastValue[True] := -LastValue[True];
end;

procedure TITAudioDriver_HQ.MixFilteredSurround16BitStereo
	(sc: TSlaveChannel; MixBufPtr: PFloat; NumSamples: Cardinal);
var
	i, WholeSamples: Integer;
	t: PFloat;
	fSample, fSampleR: Float;
	base, smp, smpR: PInt16;

	procedure DoMix; inline;
	begin
		Get16BitStereoWaveForm;
		FilterStereoSample;
		MixStereoSampleSurround;
		UpdatePosStereo;
		RampCurrVolumeL;
	end;
begin
	MixItStereo;
	LastValue[True] := -LastValue[True];
end;

// ================================================================================================
// Routines for changing sample data before mixing (and reverting after mix) for
// interpolation taps to be read the correctly. This is a bit messy...
// ================================================================================================

type
	SampleDataFixMethod = ( FixNoLoop, FixFwdLoop, FixPingPongLoop, Unfix );

generic procedure FixSampleNoLoop<T>(S: TITSample; sc: TSlaveChannel);
var
	Chan: Boolean;
	Data, DataEnd: T;
begin
	for Chan := False to (S.Data[True].Data <> nil) do
	begin
		Data := S.Data[Chan].Data;
		DataEnd := Data + sc.LoopEnd;

		Data[-1]   := Data[0];
		Data[-2]   := Data[0];
		Data[-3]   := Data[0];
		DataEnd[0] := DataEnd[-1];
		DataEnd[1] := DataEnd[-1];
		DataEnd[2] := DataEnd[-1];
		DataEnd[3] := DataEnd[-1];
	end;
end;

generic procedure FixSampleFwdLoop<T>(S: TITSample; sc: TSlaveChannel);
var
	Chan: Boolean;
	ptr, LoopBegin, LoopEnd: T;
begin
	for Chan := False to (S.Data[True].Data <> nil) do
	begin
		ptr := S.Data[Chan].Data;
		LoopBegin := ptr + sc.LoopBegin;
		LoopEnd   := ptr + sc.LoopEnd;

		if sc.HasLooped then
		begin
			sc.leftTmpSamples[Chan,0] := LoopBegin[-1];
			sc.leftTmpSamples[Chan,1] := LoopBegin[-2];
			sc.leftTmpSamples[Chan,2] := LoopBegin[-3];
			LoopBegin[-1] := LoopEnd[-1];
			LoopBegin[-2] := LoopEnd[-2];
			LoopBegin[-3] := LoopEnd[-3];
		end
		else
		begin
			ptr[-1] := ptr[0];
			ptr[-2] := ptr[0];
			ptr[-3] := ptr[0];
		end;

		sc.rightTmpSamples[Chan,0] := LoopEnd[0];
		sc.rightTmpSamples[Chan,1] := LoopEnd[1];
		sc.rightTmpSamples[Chan,2] := LoopEnd[2];
		sc.rightTmpSamples[Chan,3] := LoopEnd[3];
		LoopEnd[0] := LoopEnd[-1];
		LoopEnd[1] := LoopEnd[-2];
		LoopEnd[2] := LoopEnd[-3];
		LoopEnd[3] := LoopEnd[-4];
	end;
end;

generic procedure FixSamplePingpong<T>(S: TITSample; sc: TSlaveChannel);
var
	Chan: Boolean;
	ptr, LoopBegin, LoopEnd: T;
begin
	for Chan := False to (S.Data[True].Data <> nil) do
	begin
		ptr := S.Data[Chan].Data;
		LoopBegin := ptr + sc.LoopBegin;
		LoopEnd   := ptr + sc.LoopEnd;

		if sc.HasLooped then
		begin
			sc.leftTmpSamples[Chan,0] := LoopBegin[-1];
			sc.leftTmpSamples[Chan,1] := LoopBegin[-2];
			sc.leftTmpSamples[Chan,2] := LoopBegin[-3];
			LoopBegin[-1] := LoopBegin[0];
			LoopBegin[-2] := LoopBegin[1];
			LoopBegin[-3] := LoopBegin[2];
		end
		else
		begin
			ptr[-1] := ptr[0];
			ptr[-2] := ptr[0];
			ptr[-3] := ptr[0];
		end;

		sc.rightTmpSamples[Chan,0] := LoopEnd[0];
		sc.rightTmpSamples[Chan,1] := LoopEnd[1];
		sc.rightTmpSamples[Chan,2] := LoopEnd[2];
		sc.rightTmpSamples[Chan,3] := LoopEnd[3];
		LoopEnd[0] := LoopEnd[-1];
		LoopEnd[1] := LoopEnd[-2];
		LoopEnd[2] := LoopEnd[-3];
		LoopEnd[3] := LoopEnd[-4];
	end;
end;

generic procedure UnfixSample<T>(S: TITSample; sc: TSlaveChannel);
var
	Chan: Boolean;
	ptr, LoopBegin, LoopEnd: T;
begin
	for Chan := False to (S.Data[True].Data <> nil) do
	begin
		ptr := S.Data[Chan].Data;

		LoopEnd := ptr + sc.LoopEnd;
		LoopEnd[0] := sc.rightTmpSamples[Chan,0];
		LoopEnd[1] := sc.rightTmpSamples[Chan,1];
		LoopEnd[2] := sc.rightTmpSamples[Chan,2];
		LoopEnd[3] := sc.rightTmpSamples[Chan,3];

		if sc.HasLooped then
		begin
			LoopBegin := ptr + sc.LoopBegin;
			LoopBegin[-1] := sc.leftTmpSamples[Chan,0];
			LoopBegin[-2] := sc.leftTmpSamples[Chan,1];
			LoopBegin[-3] := sc.leftTmpSamples[Chan,2];
		end;
	end;
end;

procedure FixSampleData(Method: SampleDataFixMethod; S: TITSample; sc: TSlaveChannel);
var
	Is16Bit: Boolean;
begin
	// This requires complicated logic. As this is rare, don't bother.
	if (sc.LoopEnd - sc.LoopBegin) <= 4 then Exit; // LoopLength

	Is16Bit := sc.SmpIs16Bit;

	case Method of
		FixNoLoop:
			if Is16Bit then
				specialize FixSampleNoLoop<PInt16>(S, sc)
			else
				specialize FixSampleNoLoop<PInt8>(S, sc);
		FixFwdLoop:
			if Is16Bit then
				specialize FixSampleFwdLoop<PInt16>(S, sc)
			else
				specialize FixSampleFwdLoop<PInt8>(S, sc);
		FixPingPongLoop:
			if Is16Bit then
				specialize FixSamplePingpong<PInt16>(S, sc)
			else
				specialize FixSamplePingpong<PInt8>(S, sc);
		Unfix:
			if Is16Bit then
				specialize UnfixSample<PInt16>(S, sc)
			else
				specialize UnfixSample<PInt8>(S, sc);
	end;
end;

// ================================================================================================
// Zero-vol routines for HQ driver
// ================================================================================================

procedure TITAudioDriver_HQ.UpdateNoLoop(sc: TSlaveChannel; NumSamples: Cardinal);
var
	SamplingPosition, IntSamples: Cardinal;
	FracSamples: Word;
	Delta: UInt64;
begin
	SamplingPosition := sc.SamplingPosition;

	Delta := sc.Delta64 * NumSamples;
	IntSamples  := Delta >> 32;
	FracSamples := Delta and UINT32_MAX;

	sc.Frac64 += FracSamples;
	SamplingPosition += sc.Frac64 >> 32;
	SamplingPosition += IntSamples;
	sc.Frac64 := sc.Frac64 and UINT32_MAX;

	if SamplingPosition >= sc.LoopEnd then
	begin
		sc.Flags.WordAccess := 0;
		sc.Flags.SF_NOTE_STOP := True;
		if (sc.HostChnNum and CHN_DISOWNED) = 0 then
		begin
			sc.HostChannel.Flags.HF_CHAN_ON := False; // Signify channel off
			Exit;
		end;
	end;

	sc.SamplingPosition := SamplingPosition;
end;

procedure TITAudioDriver_HQ.UpdateForwardsLoop(sc: TSlaveChannel; NumSamples: Cardinal);
var
	IntSamples, LoopLength: Cardinal;
	FracSamples: Word;
	Delta: UInt64;
begin
	Delta := sc.Delta64 * NumSamples;
	IntSamples  := Delta >> 32;
	FracSamples := Delta and UINT32_MAX;

	sc.Frac64 += FracSamples;
	sc.SamplingPosition += sc.Frac64 >> 32;
	sc.SamplingPosition += IntSamples;
	sc.Frac64 := sc.Frac64 and UINT32_MAX;

	if sc.SamplingPosition >= sc.LoopEnd then // Reset position...
	begin
		LoopLength := sc.LoopEnd - sc.LoopBegin;
		if LoopLength = 0 then
			sc.SamplingPosition := 0
		else
			sc.SamplingPosition := sc.LoopBegin + ((sc.SamplingPosition - sc.LoopEnd) div LoopLength);
	end;
end;

procedure TITAudioDriver_HQ.UpdatePingPongLoop(sc: TSlaveChannel; NumSamples: Cardinal);
var
	IntSamples, LoopLength, NewLoopPos: Cardinal;
	FracSamples: Word;
	Delta: UInt64;
begin
	Delta := sc.Delta64 * NumSamples;
	IntSamples  := Delta >> 32;
	FracSamples := Delta and UINT32_MAX;
	LoopLength  := sc.LoopEnd - sc.LoopBegin;

	if sc.LoopDirection = DIR_BACKWARDS then
	begin
		sc.Frac64 -= FracSamples;
		sc.SamplingPosition += Integer(sc.Frac64 >> 32);
		sc.SamplingPosition -= IntSamples;
		sc.Frac64 := sc.Frac64 and UINT32_MAX;

		if sc.SamplingPosition <= sc.LoopBegin then
		begin
			NewLoopPos := (sc.LoopBegin - sc.SamplingPosition) mod (LoopLength * 1);
			if NewLoopPos >= LoopLength then
			begin
				sc.SamplingPosition := (sc.LoopEnd - 1) - (NewLoopPos - LoopLength);

				if sc.SamplingPosition = sc.LoopBegin then
				begin
					sc.LoopDirection := DIR_FORWARDS;
					sc.Frac64 := (-sc.Frac64) and UINT32_MAX;
				end;
			end
			else
			begin
				sc.LoopDirection := DIR_FORWARDS;
				sc.SamplingPosition := sc.LoopBegin + NewLoopPos;
				sc.Frac64 := (-sc.Frac64) and UINT32_MAX;
			end;
			sc.HasLooped := True;
		end;
	end
	else // forwards
	begin
		sc.Frac64 += FracSamples;
		sc.SamplingPosition += Integer(sc.Frac64 >> 32);
		sc.SamplingPosition += IntSamples;
		sc.Frac64 := sc.Frac64 and UINT32_MAX;

		if sc.SamplingPosition >= sc.LoopEnd then
		begin
			NewLoopPos := (sc.SamplingPosition - sc.LoopEnd) mod (LoopLength * 2);
			if NewLoopPos >= LoopLength then
				sc.SamplingPosition := sc.LoopBegin + (NewLoopPos - LoopLength)
			else
			begin
				sc.SamplingPosition := (sc.LoopEnd - 1) - NewLoopPos;
				if sc.SamplingPosition <> sc.LoopBegin then
				begin
					sc.LoopDirection := DIR_BACKWARDS;
					sc.Frac64 := (-sc.Frac64) and UINT32_MAX;
				end;
			end;
			sc.HasLooped := True;
		end;
	end;
end;


// ================================================================================================
// TITAudioDriver_HQ
// ================================================================================================

procedure TITAudioDriver_HQ.MixSamples;
var
	i: Integer;
	Chan: Boolean;
	SamplesToMix, MixBlockSize: Cardinal;
	LoopLength, NewLoopPos: Cardinal;
	iLoopLength: Int32 absolute LoopLength;
	MixBufferPtr: PFloat;
	B, FilterActive: Boolean;
	filterCutOff, filterQ: Byte;
	FilterFreqValue: Word;
	r, p, d, e: Float;
	Vol: Integer;
	sc: TSlaveChannel;
	Sam: TITSample;
	MixFunc: TMixFunction;

	procedure GetSamplesToMix; inline;
	var
		S64: UInt64;
	begin
		S64 := (UInt64(SamplesToMix << 32) or
			((sc.Frac64 xor UINT32_MAX) and UINT32_MAX)) div sc.Delta64 + 1;
		SamplesToMix := S64 and UINT32_MAX;
	end;

	procedure DoMix; inline;
	begin
		if SamplesToMix > MixBlockSize then
			SamplesToMix := MixBlockSize;

		if SamplesToMix > 0 then
		begin
			MixFunc(sc, MixBufferPtr, SamplesToMix);

			MixBufferPtr += SamplesToMix * 2;
			MixBlockSize -= SamplesToMix;
		end;
	end;

	procedure ClearFlags;
	begin
		sc.Flags.SF_RECALC_PAN      := False;
		sc.Flags.SF_RECALC_VOL      := False;
		sc.Flags.SF_FREQ_CHANGE     := False;
		sc.Flags.SF_UPDATE_MIXERVOL := False;
		sc.Flags.SF_NEW_NOTE        := False;
		sc.Flags.SF_NOTE_STOP       := False;
		sc.Flags.SF_LOOP_CHANGED    := False;
		sc.Flags.SF_PAN_CHANGED     := False;
	end;

	procedure SetSamplingPosition(NewPos: Cardinal); inline;
	begin
		sc.SamplingPosition := Int32(NewPos);
	end;

begin
	RealBytesToMix := BytesToMix;
	CurrentFractional += BytesToMixFractional;
	if CurrentFractional >= BPM_FRAC_SCALE  then
	begin
		CurrentFractional := CurrentFractional and BPM_FRAC_MASK;
		RealBytesToMix += 1;
	end;

	// click removal (also clears buffer)
	MixBufferPtr := @MixBuffer[0];
	for i := 0 to RealBytesToMix-1 do
	begin
		for B in Boolean do
		begin
			MixBufferPtr^ := LastClickRemoval[B];
			Inc(MixBufferPtr);
			LastClickRemoval[B] -= LastClickRemoval[B]  * (1.0 / 4096.0);
		end;
	end;

	MixTransferOffset := 0;

	for i := 0 to NumChannels-1 do
	begin
		sc := Module.SlaveChannels[i];
		if (not sc.Flags.SF_CHAN_ON) or (sc.Smp = 100) then
			Continue;

		Sam := sc.Sample;
		Assert(Sam <> nil);

		if sc.Flags.SF_NOTE_STOP then // note cut
		begin
			sc.Flags.SF_CHAN_ON := False;
			sc.FinalVol32768 := 0;
			sc.Flags.SF_UPDATE_MIXERVOL := True;
		end;

		if sc.Flags.SF_FREQ_CHANGE then
		begin
			if sc.Frequency >= (MaxLongInt div 2) then // non-IT2 limit, but required for safety
			begin
				sc.Flags.WordAccess := 0;
				sc.Flags.SF_NOTE_STOP := True;
				if (sc.HostChnNum and CHN_DISOWNED) = 0 then
					sc.HostChannel.Flags.HF_CHAN_ON := False; // Turn off channel
				Continue;
			end;

			// mixer delta (32.32fp)
			sc.Delta64 := SarInt64(Int64(sc.Frequency * FreqMulVal), FREQ_MUL_EXTRA_BITS);
		end;

		if sc.Flags.SF_NEW_NOTE then
		begin
			for Chan in Boolean do
			begin
				sc.fOldVolume[Chan] := 0;
				// ramp in current voice (old note is ramped out in another voice)
				sc.fCurrVol[Chan] := 0;
			end;

			// clear filter state and filter coeffs
			sc.fOldSamples[0] := 0; sc.fOldSamples[1] := 0;
			sc.fOldSamples[2] := 0; sc.fOldSamples[3] := 0;
			sc.fFiltera := 1.0;
			sc.fFilterb := 0;
			sc.fFilterc := 0;
		end;

		if (sc.Flags.SF_UPDATE_MIXERVOL) or (sc.Flags.SF_LOOP_CHANGED) or (sc.Flags.SF_PAN_CHANGED) then
		begin
			if (sc.HostChnNum and CHN_DISOWNED) <> 0 then
				FilterQ := sc.MIDIBank >> 8 // if disowned, use channel filters
			else
			begin
				filterCutOff := FilterParameters[sc.HostChnNum];
				filterQ      := FilterParameters[sc.HostChnNum+64];

				sc.VolEnvState.CurNode := (filterCutOff * 256) + (sc.VolEnvState.CurNode and $00FF);
				sc.MIDIBank := (filterQ * 256) + (sc.MIDIBank and $00FF);
			end;

			// FilterEnvVal (0..255) * CutOff (0..127)
			FilterFreqValue := (sc.MIDIBank and $00FF) * (sc.VolEnvState.CurNode div 256);
			if (filterQ <> 0) or (FilterFreqValue <> 127*255) then
			begin
				Assert((FilterFreqValue <= 127*255) and (filterQ <= 127));

				r := Power(2.0, FilterFreqValue * FreqParameterMultiplier) * FreqMultiplier;
				p := QualityFactorTable[filterQ];
				d := (p * r) + (p - 1.0);
				e := r * r;

				sc.fFiltera := 1.0 / (1.0 + d + e);
				sc.fFilterb := (d + e + e) * sc.fFiltera;
				sc.fFilterc := 1.0 - sc.fFiltera - sc.fFilterb;
			end;

			if sc.Flags.SF_CHN_MUTED then
			begin
				sc.fVolume[False] := 0;
				sc.fVolume[True]  := 0;
			end
			else
			begin
				Vol := sc.FinalVol32768 * MixVolume;

				if not Module.Header.Flags.ITF_STEREO then // mono?
				begin
					sc.fVolume[False] := Vol * (0.5 / (32768 * 128));
					sc.fVolume[True]  := sc.fVolume[False];
				end
				else
				if sc.FinalPan = PAN_SURROUND then
				begin
					sc.fVolume[False] := Vol * (0.5 / (32768 * 128));
					sc.fVolume[True]  := sc.fVolume[False];
				end
				else // normal (panned)
				begin
					sc.fVolume[False] := (Integer(64-sc.FinalPan) * Vol) * (1.0 / (64 * 32768 * 128));
					sc.fVolume[True]  := (Integer(   sc.FinalPan) * Vol) * (1.0 / (64 * 32768 * 128));
				end;
			end;
		end;

		if sc.Delta64 = 0 then // just in case
			Continue;

		MixBlockSize := RealBytesToMix;
		FilterActive := (sc.fFilterb > 0.0) or (sc.fFilterc > 0.0);

		if (not FilterActive) and
		   (sc.fVolume[False] = 0.0) and (sc.fVolume[True] = 0.0) and
		   (sc.fOldVolume[False] <= 0.000001) and (sc.fOldVolume[True] <= 0.000001) then
		begin
			// use position update routine (zero voice volume and no filter)

			LoopLength := sc.LoopEnd - sc.LoopBegin; // also length for non-loopers
			if iLoopLength > 0 then
			begin
				if sc.LoopMode = LOOP_PINGPONG then
					UpdatePingPongLoop(sc, MixBlockSize)
				else
				if sc.LoopMode = LOOP_FORWARDS then
					UpdateForwardsLoop(sc, MixBlockSize)
				else
					UpdateNoLoop(sc, MixBlockSize);
			end;
		end
		else // 8bb: regular mixing
		begin
			// MixFunctions[FilterActive, Stereo, Surround, Is16Bit]
			MixFunc := MixFunctions[FilterActive, Sam.Flags.SMPF_STEREO, sc.FinalPan = PAN_SURROUND, sc.SmpIs16Bit];
			Assert(MixFunc <> nil);

			LoopLength := sc.LoopEnd - sc.LoopBegin; // also actual length for non-loopers

			if iLoopLength > 0 then
			begin
				MixBufferPtr := @MixBuffer[0];

				if sc.LoopMode = LOOP_PINGPONG then
				begin
					// pingpong loop
					while MixBlockSize > 0 do
					begin
						if sc.LoopDirection = DIR_BACKWARDS then
						begin
							if sc.SamplingPosition = sc.LoopBegin then
							begin
								sc.LoopDirection := DIR_FORWARDS;
								sc.Frac64 := (-sc.Frac64) and UINT32_MAX;
								SamplesToMix := (sc.LoopEnd - 1) - sc.SamplingPosition;
								GetSamplesToMix;
								Delta64 := sc.Delta64;
							end
							else
							begin
								SamplesToMix := sc.SamplingPosition - (sc.LoopBegin + 1);
								SamplesToMix := (Uint64(SamplesToMix << 32) or
									(sc.Frac64 and UINT32_MAX)) div sc.Delta64 + 1; // no xor!
								Delta64 := -sc.Delta64;
							end;
						end
						else // forwards
						begin
							SamplesToMix := (sc.LoopEnd - 1) - sc.SamplingPosition;
							GetSamplesToMix;
							Delta64 := sc.Delta64;
						end;

						FixSampleData(FixPingPongLoop, Sam, sc); // for interpolation taps
						DoMix;
						FixSampleData(Unfix, Sam, sc);

						if sc.LoopDirection = DIR_BACKWARDS then
						begin
							if sc.SamplingPosition <= sc.LoopBegin then
							begin
								NewLoopPos := (sc.LoopBegin - sc.SamplingPosition) mod (LoopLength * 2);
								if NewLoopPos >= LoopLength then
								begin
									SetSamplingPosition((sc.LoopEnd - 1) - (NewLoopPos - LoopLength));

									if sc.SamplingPosition = sc.LoopBegin then
									begin
										sc.LoopDirection := DIR_FORWARDS;
										sc.Frac64 := (-sc.Frac64) and UINT32_MAX;
									end;
								end
								else
								begin
									sc.LoopDirection := DIR_FORWARDS;
									SetSamplingPosition(sc.LoopBegin + NewLoopPos);
									sc.Frac64 := (-sc.Frac64) and UINT32_MAX;
								end;
								sc.HasLooped := True;
							end;
						end
						else // forwards
						begin
							if sc.SamplingPosition >= sc.LoopEnd then
							begin
								NewLoopPos := (sc.SamplingPosition - sc.LoopEnd) mod (LoopLength * 2);
								if NewLoopPos >= LoopLength then
									SetSamplingPosition(sc.LoopBegin + (NewLoopPos - LoopLength))
								else
								begin
									SetSamplingPosition((sc.LoopEnd - 1) - NewLoopPos);
									if sc.SamplingPosition <> sc.LoopBegin then
									begin
										sc.LoopDirection := DIR_BACKWARDS;
										sc.Frac64 := (-sc.Frac64) and UINT32_MAX;
									end;
								end;
								sc.HasLooped := True;
							end;
						end;
					end;
				end
				else
				if sc.LoopMode = LOOP_FORWARDS then
				begin
					while MixBlockSize > 0 do
					begin
						SamplesToMix := (sc.LoopEnd - 1) - sc.SamplingPosition;
						GetSamplesToMix;
						Delta64 := sc.Delta64;

						FixSampleData(FixFwdLoop, Sam, sc); // for interpolation taps
						DoMix;
						FixSampleData(Unfix, Sam, sc);      // for interpolation taps

						if sc.SamplingPosition >= sc.LoopEnd then
						begin
							SetSamplingPosition(sc.LoopBegin + ((sc.SamplingPosition - sc.LoopEnd) mod LoopLength));
							sc.HasLooped := True;
						end;
					end;
				end
				else  // no loop
				begin
					while MixBlockSize > 0 do
					begin
						SamplesToMix := (sc.LoopEnd - 1) - sc.SamplingPosition;
						GetSamplesToMix;
						Delta64 := sc.Delta64;

						FixSampleData(FixNoLoop, Sam, sc); // for interpolation taps
						DoMix;

						if sc.SamplingPosition >= sc.LoopEnd then
						begin
							sc.Flags.WordAccess := 0;
							sc.Flags.SF_NOTE_STOP := True;
							if (sc.HostChnNum and CHN_DISOWNED) = 0 then
								sc.HostChannel.Flags.HF_CHAN_ON := False;

							// sample ended, ramp out very last sample point for the remaining samples
							while MixBlockSize > 0 do
							begin
								for Chan in Boolean do
								begin
									MixBufferPtr^ += LastValue[Chan];
									Inc(MixBufferPtr);
									LastValue[Chan] -= LastValue[Chan] * (1.0 / 4096.0);
								end;
								Dec(MixBlockSize);
							end;

							// update anti-click value for next mixing session
							for Chan in Boolean do
								LastClickRemoval[Chan] += LastValue[Chan];

							Break;
						end;
					end;
				end;
			end;

			for Chan := False to (sc.FinalPan = PAN_SURROUND) do
				sc.fOldVolume[Chan] := sc.fCurrVol[Chan];
		end;

		ClearFlags;
	end;
end;

procedure TITAudioDriver_HQ.Mix(NumSamples: Integer; AudioOut: PInt16);
var
	SamplesToTransfer: Integer;
begin
	WaitFor;
	Busy := True;

	while NumSamples > 0 do
	begin
		if MixTransferRemaining = 0 then
		begin
			Module.Update;
			MixSamples;
			MixTransferRemaining := RealBytesToMix;
		end;

		SamplesToTransfer := NumSamples;
		if SamplesToTransfer > MixTransferRemaining then
			SamplesToTransfer := MixTransferRemaining;

		PostMix(AudioOut, SamplesToTransfer);
		AudioOut += SamplesToTransfer * 2;

		MixTransferRemaining -= SamplesToTransfer;
		NumSamples -= SamplesToTransfer;
	end;

	Busy := False;
end;

function TITAudioDriver_HQ.PostMix(AudioOut16: PInt16; SamplesLeft: Integer; SampleShiftValue: Byte): Integer;

	function Random32: Integer; inline;
	begin
		// LCG 32-bit random
		RandSeed *= 134775813;
		RandSeed += 1;
		Result := RandSeed;
	end;

var
	i, Sample: Integer;
	fOut, fPrng: Float;
	Chan, HasClipped: Boolean;
begin
	If SamplesLeft = 0 then
		SamplesLeft := RealBytesToMix;

	HasClipped := False;

	for i := 0 to SamplesLeft-1 do
	for Chan in Boolean do
	begin
		// 1-bit triangular dithering
		fPrng := Random32 * (0.5 / MaxLongInt); // -0.5f .. 0.5f
		fOut  := MixBuffer[MixTransferOffset] * 32768;
		Inc(MixTransferOffset);
		fOut := (fOut + fPrng) - PrngState[Chan];
		PrngState[Chan] := fPrng;
		Sample := Trunc(fOut);

		// fast 32-bit -> 16-bit clamp
		// if ((int16_t)i <> i)
		if (Int16(Sample) <> Sample) then
		begin
			// i := INT16_MAX ^ ((Integer)i >> 31)
			Sample := MaxSmallInt xor (SarLongInt(Sample, 31));
			HasClipped := True;
		end;

		AudioOut16^ := Sample;
		Inc(AudioOut16);
	end;

	if HasClipped then
		Clipped;

	Result := SamplesLeft;
end;

procedure TITAudioDriver_HQ.FixSamples;
begin
	// not used, we do it in realtime instead for accuracy
end;

procedure TITAudioDriver_HQ.InitWindowedSincLUT;

	// zeroth-order modified Bessel function of the first kind (series approximation)
	//
	function besselI0(z: double): Double; inline;
	var
		s, ds, d, zz: Double;
	begin
		s := 1.0; ds := 1.0; d := 2.0;
		zz := z * z;
		repeat
			ds *= zz / (d * d);
			s += ds;
			d += 2.0;
		until (ds <= s*(1E-12));
		Result := s;
	end;

	function sinc(x: Double): Double; inline;
	begin
		if x = 0.0 then
			Result := 1.0
		else
		begin
			x *= PI;
			Result := Sin(x) / x;
		end;
	end;

var
	x, n,
	kaiserBeta, besselI0Beta,
	window: Double;
	i: Integer;
begin
	SetLength(SincLUT, SINC_PHASES * SINC_WIDTH);

	// sinc with Kaiser-Bessel window
	kaiserBeta := 9.6377; // lower beta results in audible ringing in some cases
	besselI0Beta := 1.0 / besselI0(kaiserBeta);

	for i := 0 to SINC_PHASES * SINC_WIDTH - 1 do
	begin
		x := ((i and (SINC_WIDTH-1)) - ((SINC_WIDTH / 2) - 1)) - ((i >> SINC_WIDTH_BITS) * (1.0 / SINC_PHASES));

		// Kaiser-Bessel window
		n := x * (1.0 / (SINC_WIDTH / 2));
		window := besselI0(kaiserBeta * sqrt(1.0 - n * n)) * besselI0Beta;

		SincLUT[i] := sinc(x) * window;
	end;
end;

procedure TITAudioDriver_HQ.SetTempo(Tempo: Byte);
var
	i: Cardinal;
begin
	if Tempo < LOWEST_BPM_POSSIBLE then
		Tempo := LOWEST_BPM_POSSIBLE;

	i := Tempo - LOWEST_BPM_POSSIBLE;
	BytesToMix           := SamplesPerTickInt [i];
	BytesToMixFractional := SamplesPerTickFrac[i];

	if (Module <> nil) and (Assigned(Module.OnTempoChange)) then
		Module.OnTempoChange(Module);
end;

procedure TITAudioDriver_HQ.ResetMixer;
var
	Chan: Boolean;
begin
	inherited ResetMixer;

	CurrentFractional := 0;
	RandSeed := $12345000;

	for Chan in Boolean do
	begin
		LastClickRemoval[Chan] := 0;
		PrngState[Chan] := 0;
	end;
end;

constructor TITAudioDriver_HQ.Create(AModule: TITModule; DriverType: TITAudioDriverType; MixingFrequency: Integer);
var
	i, MaxSamplesToMix: Integer;
	index: Cardinal;
	dHz, dInt, dFrac, dSamplesPerTick: Double;
begin
	Flags.DF_SUPPORTS_MIDI        := True;
	Flags.DF_USES_VOLRAMP         := True;
	Flags.DF_HAS_RESONANCE_FILTER := True;

	NumChannels := 256;

	// 32769Hz is absolute lowest (for FreqMulVal to fit in INT32_MAX)
	MixingFrequency := EnsureRange(MixingFrequency, 32769, 768000);

	inherited Create(AModule, DriverType, MixingFrequency);

	FreqMulVal := Round(Cardinal(1) << (32 + FREQ_MUL_EXTRA_BITS) / MixingFrequency);

	MaxSamplesToMix := Ceil((MixingFrequency * 2.5) / LOWEST_BPM_POSSIBLE) + 1;
	SetLength(MixBuffer, MaxSamplesToMix * 2);

	// calculate samples-per-tick tables
	for i := LOWEST_BPM_POSSIBLE to 255 do
	begin
		dHz := i * (1.0 / 2.5);
		dSamplesPerTick := MixingFrequency / dHz;

		// break into int/frac parts
		dInt  := Int (dSamplesPerTick);
		dFrac := Frac(dSamplesPerTick);

		index := i - LOWEST_BPM_POSSIBLE;
		SamplesPerTickInt [index] := Trunc(dInt);
		SamplesPerTickFrac[index] := Trunc(dFrac * BPM_FRAC_SCALE + 0.5);
	end;

	// MixFunctions[FilterActive, Stereo, Surround, Is16Bit]
	//
	MixFunctions[False, False, False, False] := @Mix8Bit;
	MixFunctions[False, False, False, True ] := @Mix16Bit;
	MixFunctions[False, False, True,  False] := @MixSurround8Bit;
	MixFunctions[False, False, True,  True ] := @MixSurround16Bit;
	MixFunctions[False, True,  False, False] := @Mix8BitStereo;
	MixFunctions[False, True,  False, True ] := @Mix16BitStereo;
	MixFunctions[False, True,  True,  False] := @MixSurround8BitStereo;
	MixFunctions[False, True,  True,  True ] := @MixSurround16BitStereo;
	MixFunctions[True,  False, False, False] := @MixFiltered8Bit;
	MixFunctions[True,  False, False, True ] := @MixFiltered16Bit;
	MixFunctions[True,  False, True,  False] := @MixFilteredSurround8Bit;
	MixFunctions[True,  False, True,  True ] := @MixFilteredSurround16Bit;
	MixFunctions[True,  True,  False, False] := @MixFiltered8BitStereo;
	MixFunctions[True,  True,  False, True ] := @MixFiltered16BitStereo;
	MixFunctions[True,  True,  True,  False] := @MixFilteredSurround8BitStereo;
	MixFunctions[True,  True,  True,  True ] := @MixFilteredSurround16BitStereo;

	InitWindowedSincLUT;
end;


end.

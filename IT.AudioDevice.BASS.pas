unit IT.AudioDevice.BASS;

// BASS driver

{$MODE OBJFPC}
{$H+}
{$COPERATORS ON}
{$MACRO ON}

interface

uses
	Classes, SysUtils,
	BASS,
	IT2play,
	IT.AudioDevice;

type
	TITAudioDevice_BASS = class (TITAudioDevice)
	protected
		Stream: HSTREAM;

		// mixer callbacks
		function  OpenMixer ({%H-}Module: TITModule; MixingFrequency, {%H-}MixingBufferSize: Cardinal): Boolean; override;
		procedure LockMixer ({%H-}Module: TITModule; {%H-}Value: Boolean); override;
		procedure CloseMixer({%H-}Module: TITModule); override;
		procedure Playback  ({%H-}Module: TITModule; {%H-}Value: Boolean); override;
	public
		procedure   Lock;   override;
		procedure   Unlock; override;

		constructor Create(Module: TITModule; SampleRate: Word = 44100); override;
		destructor  Destroy; override;
	end;


implementation

var
	MixerOpened, MixerBusy, MixerLocked: Boolean;

function AudioCallback({%H-}Handle: HSTREAM; Buffer: Pointer;
	BufLen: DWord; PAudioDeviceBuffer: Pointer): DWord; stdcall;
var
	Info:   TITAudioDeviceBuffer;
	Module: TITModule;
	OK:     Boolean;
begin
	BufLen := BufLen div 4; // -> stereo samples
	Result := BufLen * 4;   // bytes written
	if Buffer = nil then Exit;

	Info := TITAudioDeviceBuffer(PAudioDeviceBuffer);
	OK := (MixerOpened) and (Info <> nil) and (Info.Module <> nil);
	if OK then
		Module := Info.Module
	else
		Module := nil;

	MixerBusy := True;

	if (OK) and (not MixerLocked) and (Module.Playing) and (not Module.MixerLocked) then
		Module.FillAudioBuffer(Buffer, BufLen)
	else
		FillDWord(Buffer^, BufLen, 0);

	if Info <> nil then
	begin
		BufLen *= 4; // stereo samples -> bytes
		if BufLen > Length(Info.Data) then
			SetLength(Info.Data, BufLen);
		// copy the filled audio buffer for visualization
		Move(Buffer^, Info.Data[0], BufLen);
	end;

	MixerBusy := False;
end;

{ TITAudioDevice }

function TITAudioDevice_BASS.OpenMixer(Module: TITModule;
	MixingFrequency, MixingBufferSize: Cardinal): Boolean;
begin
	Result := False;
	if MixerOpened then Exit;

	Frequency := MixingFrequency;

	Buffer.Module := Module;
	Buffer.SampleRate := Frequency;

	MixerOpened := BASS_Init(-1, MixingFrequency,
		BASS_DEVICE_16BITS or BASS_DEVICE_STEREO,
		0, nil);

	if MixerOpened then
	begin
		Stream := BASS_StreamCreate(MixingFrequency, 2, 0, @AudioCallback, Buffer);
		if Stream <> 0 then
		begin
			// minimize buffering for faster updates to output visualization
			// remove if audio dropouts occur
			BASS_ChannelSetAttribute(Stream, BASS_ATTRIB_BUFFER, 0);
			BASS_ChannelPlay(Stream, False);
		end;
	end;

	Result := MixerOpened;
end;

procedure TITAudioDevice_BASS.CloseMixer(Module: TITModule);
begin
	MixerOpened := False;
	MixerBusy   := False;

	BASS_StreamFree(Stream);
	BASS_Free;
end;

procedure TITAudioDevice_BASS.Lock;
begin
	if MixerLocked then Exit;

	MixerLocked := True;
	while MixerBusy do;
end;

procedure TITAudioDevice_BASS.Unlock;
begin
	MixerBusy   := False;
	MixerLocked := False;
end;

procedure TITAudioDevice_BASS.LockMixer(Module: TITModule; Value: Boolean);
begin
	if Value then
		Lock
	else
		Unlock;
end;

procedure TITAudioDevice_BASS.Playback(Module: TITModule; Value: Boolean);
begin
	Buffer.Module := Module;
end;

constructor TITAudioDevice_BASS.Create(Module: TITModule; SampleRate: Word = 44100);
begin
	inherited Create(Module, SampleRate);

	Module.OnOpenMixer  := @OpenMixer;
	Module.OnCloseMixer := @CloseMixer;
	Module.OnLockMixer  := @LockMixer;
	Module.OnPlayback   := @Playback;
end;

destructor TITAudioDevice_BASS.Destroy;
begin
	CloseMixer(nil);

	inherited Destroy;
end;

end.


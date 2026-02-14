unit IT.AudioDevice.SDL3;

{$MODE OBJFPC}
{$H+}
{$COPERATORS ON}
{$MACRO ON}

interface

uses
	Classes, SysUtils,
	SDL3,
	IT2play,
	IT.AudioDevice;

type
	TITAudioDevice_SDL3 = class (TITAudioDevice)
	protected
		Audio: PSDL_AudioStream;

		// mixer callbacks
		function  OpenMixer (Module: TITModule; MixingFrequency, MixingBufferSize: Cardinal): Boolean; override;
		procedure LockMixer (Module: TITModule; Value: Boolean); override;
		procedure CloseMixer(Module: TITModule); override;
		procedure Playback  (Module: TITModule; Value: Boolean); override;
	public
		procedure   Lock;   override;
		procedure   Unlock; override;

		constructor Create(Module: TITModule; SampleRate: Word = 44100); override;
		destructor  Destroy; override;
	end;


implementation

var
	MixerBusy:    Boolean = False;
	EnableMixing: Boolean = False;

procedure AudioCallback(userdata: Pointer; stream: PSDL_AudioStream; additional_amount, total_amount: Longint); cdecl;
var
	Info: TITAudioDeviceBuffer;
begin
	if not EnableMixing then
	begin
		MixerBusy := False;
		Exit;
	end;

	Info := TITAudioDeviceBuffer(userdata);
	if (Info = nil) or (Info.Module = nil) then Exit;

	while MixerBusy do; // safety - the buffer may be accessed by another thread at the moment

	MixerBusy := True;

	if Length(Info.Data) < total_amount then
		SetLength(Info.Data, total_amount);

	Info.Module.FillAudioBuffer(@Info.Data[0], total_amount div 4); // stereo 16-bit signed samples
	SDL_PutAudioStreamData(stream, @Info.Data[0], total_amount);

	MixerBusy := False;
end;

{ TITAudioDevice }

procedure TITAudioDevice_SDL3.Lock;
begin
	if not EnableMixing then Exit;

	EnableMixing := False;
	while MixerBusy do;
end;

procedure TITAudioDevice_SDL3.Unlock;
begin
	MixerBusy := False;
	EnableMixing := True;
end;

function TITAudioDevice_SDL3.OpenMixer(Module: TITModule;
	MixingFrequency, MixingBufferSize: Cardinal): Boolean;
var
	AudioSpec: TSDL_AudioSpec;
begin
	Audio := nil;
	Frequency := MixingFrequency;

	if SDL_Init(SDL_INIT_AUDIO) then
	begin
		// note that MixingBufferSize will be zero if we didn't specify it in the Init() call!
		if MixingBufferSize > Length(Buffer.Data) then
			SetLength(Buffer.Data, MixingBufferSize);
		Buffer.SampleRate := MixingFrequency;
		Buffer.Module := Module;

		AudioSpec.Channels := 2;
		AudioSpec.Freq     := Frequency;
		AudioSpec.Format   := SDL_AUDIO_S16LE;

		Audio := SDL_OpenAudioDeviceStream(SDL_AUDIO_DEVICE_DEFAULT_PLAYBACK,
			@AudioSpec, @AudioCallback, Buffer);
	end;

	Result := (Audio <> nil);
	EnableMixing := Result;
end;

procedure TITAudioDevice_SDL3.LockMixer(Module: TITModule; Value: Boolean);
begin
	case Value of
		// wait for the current mixing block to finish and disable further mixing
		True:  begin Lock;   SDL_LockAudioStream(Audio); end;
		// enable mixing again
		False: begin Unlock; SDL_UnlockAudioStream(Audio); end;
	end;
end;

procedure TITAudioDevice_SDL3.CloseMixer(Module: TITModule);
begin
	SDL_DestroyAudioStream(Audio);
	Audio := nil;
end;

procedure TITAudioDevice_SDL3.Playback(Module: TITModule; Value: Boolean);
begin
	if Value then
		SDL_ResumeAudioStreamDevice(Audio)
	else
		SDL_PauseAudioStreamDevice(Audio);
end;

constructor TITAudioDevice_SDL3.Create(Module: TITModule; SampleRate: Word = 44100);
begin
	inherited Create(Module, SampleRate);

	Module.OnOpenMixer  := @OpenMixer;
	Module.OnCloseMixer := @CloseMixer;
	Module.OnLockMixer  := @LockMixer;
	Module.OnPlayback   := @Playback;
end;

destructor TITAudioDevice_SDL3.Destroy;
begin
	CloseMixer(nil);
	SDL_Quit;

	inherited Destroy;
end;

end.


unit IT.AudioDevice.WinMM;

// Windows Waveform Audio/WinMM driver

{$MODE OBJFPC}
{$H+}
{$COPERATORS ON}
{$MACRO ON}

interface

uses
	Classes, SysUtils,
	MMSystem, Windows,
	IT2play,
	IT.AudioDevice;

type
	TITAudioDevice_WinMM = class (TITAudioDevice)
	protected
		// mixer callbacks
		function  OpenMixer (Module: TITModule; MixingFrequency, MixingBufferSize: Cardinal): Boolean; override;
		procedure LockMixer (Module: TITModule; Value: Boolean); override;
		procedure CloseMixer(Module: TITModule); override;
		procedure Playback  (Module: TITModule; Value: Boolean); override;
	public
		constructor Create(Module: TITModule; SampleRate: Word = 44100); override;
		destructor  Destroy; override;
	end;


implementation

const
	MIX_BUF_NUM = 4;

var
	MixerOpened, MixerBusy, MixerLocked: Boolean;
	CurrBuffer: Byte;
	AudioBuffer: array[0..MIX_BUF_NUM-1] of array of Byte;
	BufferSize: Integer;
	hThread, hAudioSem: THandle;
	WaveBlocks: array [0..MIX_BUF_NUM-1] of WAVEHDR;
	hWave: HWAVEOUT;

procedure AudioCallback(hwo: HWAVEOUT; uMsg: UINT; dwInstance, dwParam1, dwParam2: DWORD_PTR); stdcall;
begin
	if uMsg = WOM_DONE then
		ReleaseSemaphore(hAudioSem, 1, nil);
end;

function MixThread(Buf: Pointer): Ptrint;
var
	Info: TITAudioDeviceBuffer;
begin
	Result := 0;
	SetThreadPriority(GetCurrentThread, THREAD_PRIORITY_TIME_CRITICAL);

	while MixerOpened do
	begin
		Info := TITAudioDeviceBuffer(Buf);

		if Info = nil then Exit;
		if Info.Module = nil then Exit;

		if not MixerLocked then
		begin
			Info.Updating := True;
			MixerBusy := True;

			if Info.Module.Playing then
				Info.Module.FillAudioBuffer(@AudioBuffer[CurrBuffer][0], BufferSize)
			else
				FillDWord(AudioBuffer[CurrBuffer][0], BufferSize, 0);

			MixerBusy := False;
			Info.Updating := False;
		end;

		// copy the filled audio buffer for visualization
		CopyMemory(@Info.Data[0], @AudioBuffer[CurrBuffer][0], BufferSize*4);

		WaveOutWrite(hWave, @WaveBlocks[CurrBuffer], SizeOf(WAVEHDR));

		CurrBuffer += 1;
		if CurrBuffer >= MIX_BUF_NUM then CurrBuffer := 0;

		WaitForSingleObject(hAudioSem, INFINITE); // wait for buffer fill request
	end;
end;

{ TITAudioDevice }

function TITAudioDevice_WinMM.OpenMixer(Module: TITModule;
	MixingFrequency, MixingBufferSize: Cardinal): Boolean;
var
	i: Integer;
	ThreadID: DWord;
	WaveFormat: WAVEFORMATEX;
label
	omError;
begin
	Result := False;

	Frequency := MixingFrequency;

	CurrBuffer := 0;

	// note that MixingBufferSize will be zero if we didn't specify it in the Init() call!
	if MixingBufferSize = 0 then
		MixingBufferSize := 1024*4; // !!!
	if MixingBufferSize > Length(Buffer.Data) then
		SetLength(Buffer.Data, MixingBufferSize);

	Buffer.Module := Module;
	Buffer.SampleRate := Frequency;

	// don't unprepare headers on error
	for i := 0 to MIX_BUF_NUM-1 do
		WaveBlocks[i].dwUser := $FFFF;

	CloseMixer(Module);
	BufferSize := MixingBufferSize div 4;

	with WaveFormat do
	begin
		wFormatTag      := WAVE_FORMAT_PCM;
		nChannels       := 2;
		wBitsPerSample  := 16;
		nSamplesPerSec  := Frequency;
		nBlockAlign     := nChannels * (wBitsPerSample div 8);
		nAvgBytesPerSec := nSamplesPerSec * nBlockAlign;
		cbSize          := 0;
	end;

	if (WaveOutOpen(@hWave, WAVE_MAPPER, @WaveFormat,
	    DWORD_PTR(@AudioCallback), 0, CALLBACK_FUNCTION)) <> MMSYSERR_NOERROR then
		goto omError;

	// create semaphore for buffer fill requests
	hAudioSem := CreateSemaphore(nil, MIX_BUF_NUM-1, MIX_BUF_NUM, nil);
	if hAudioSem = 0 then goto omError;

	// allocate WinMM mix buffers
	for i := 0 to MIX_BUF_NUM-1 do
		SetLength(AudioBuffer[i], MixingBufferSize);

	// initialize WinMM mix headers
	for i := 0 to MIX_BUF_NUM-1 do
	begin
		WaveBlocks[i] := Default(WAVEHDR);
		WaveBlocks[i].lpData := @AudioBuffer[i,0];
		WaveBlocks[i].dwBufferLength := MixingBufferSize;
		WaveBlocks[i].dwFlags := WHDR_DONE;

		if WaveOutPrepareHeader(hWave, @WaveBlocks[i], SizeOf(WAVEHDR)) <> MMSYSERR_NOERROR then
			goto omError;
	end;

	MixerOpened := True;

	hThread := BeginThread(@MixThread, Pointer(Buffer), ThreadID);
	if hThread = 0 then goto omError;

	Exit(True);

omError:
	CloseMixer(Module);
end;

procedure TITAudioDevice_WinMM.LockMixer(Module: TITModule; Value: Boolean);
begin
	if Value then
	begin
		MixerLocked := True;
		while MixerBusy do;
	end
	else
	begin
		MixerBusy   := False;
		MixerLocked := False;
	end;

end;

procedure TITAudioDevice_WinMM.CloseMixer(Module: TITModule);
var
	i: Integer;
begin
	MixerOpened := False;
	MixerBusy   := False;

	if hAudioSem <> 0 then
		ReleaseSemaphore(hAudioSem, 1, nil);

	if hThread <> 0 then
	begin
		WaitForSingleObject(hThread, INFINITE);
		CloseHandle(hThread);
		hThread := 0;
	end;

	if hAudioSem <> 0 then
	begin
		CloseHandle(hAudioSem);
		hAudioSem := 0;
	end;

	if hWave <> 0 then
	begin
		waveOutReset(hWave);

		for i := 0 to MIX_BUF_NUM-1 do
		begin
			if WaveBlocks[i].dwUser <> $FFFF then
				WaveOutUnprepareHeader(hWave, @waveBlocks[i], SizeOf(WAVEHDR));
		end;

		WaveOutClose(hWave);
		hWave := 0;
	end;
end;

procedure TITAudioDevice_WinMM.Playback(Module: TITModule; Value: Boolean);
begin
	Buffer.Module := Module;
end;

constructor TITAudioDevice_WinMM.Create(Module: TITModule; SampleRate: Word = 44100);
begin
	inherited Create(Module, SampleRate);

	Module.OnOpenMixer  := @OpenMixer;
	Module.OnCloseMixer := @CloseMixer;
	Module.OnLockMixer  := @LockMixer;
	Module.OnPlayback   := @Playback;

	hThread := 0;
	hAudioSem := 0;
	hWave := 0;
end;

destructor TITAudioDevice_WinMM.Destroy;
begin
	CloseMixer(nil);

	inherited Destroy;
end;

end.


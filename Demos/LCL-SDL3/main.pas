unit Main;

{$DEFINE AUDIO}

{$MODE DELPHI}
{$H+}

interface

uses
	Classes, SysUtils, Forms, Controls, Graphics, Dialogs,
	LCLType, StdCtrls, ExtCtrls,
	{$IFDEF AUDIO}SDL3,{$ENDIF}
	IT2play;

type
	TForm1 = class(TForm)
		bPlay: TButton;
		Timer: TTimer;
		pb: TPaintBox;
		ListBox1: TListBox;
		Memo: TMemo;
		pbSample: TPaintBox;
		procedure FormShow(Sender: TObject);
		procedure FormDestroy(Sender: TObject);
		procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
		procedure bPlayClick(Sender: TObject);
		procedure TimerTimer(Sender: TObject);
		procedure pbPaint(Sender: TObject);
		procedure ListBox1Click(Sender: TObject);
		procedure pbSamplePaint(Sender: TObject);
		procedure FormDropFiles(Sender: TObject; const FileNames: array of string);
	private
		procedure LoadModule(const Filename: String);
	public
	end;

var
	Form1: TForm1;
	Module: TITModule;
	{$IFDEF AUDIO}
	Audio: PSDL_AudioStream;
	{$ENDIF}

	Buffer: array of Byte;
	BufferUpdating: Boolean;


implementation

{$R *.lfm}


{$IFDEF AUDIO}
procedure AudioCallback(userdata: Pointer; stream: PSDL_AudioStream; additional_amount, total_amount: Longint); cdecl;
begin
	while BufferUpdating do;
	BufferUpdating := True;

	if Length(Buffer) <= total_amount then
		SetLength(Buffer, total_amount+1);

	Module.FillAudioBuffer(@Buffer[0], total_amount div 4); // stereo 16-bit signed samples
	SDL_PutAudioStreamData(stream, @Buffer[0], total_amount);

	BufferUpdating := False;
end;
{$ENDIF}

procedure TForm1.FormShow(Sender: TObject);
const
	AudioFreq = 44100;
var
	{$IFDEF AUDIO}
	AudioSpec: TSDL_AudioSpec;
	{$ENDIF}
begin
	OnShow := nil;

	{$IFDEF AUDIO}
	AudioSpec.Channels := 2;
	AudioSpec.Freq   := AudioFreq;
	AudioSpec.Format := SDL_AUDIO_S16LE;
	{$ELSE}
	SetLength(Buffer, 1024*4);
	{$ENDIF}

	Module := TITModule.Create(AudioFreq);

	{$IFDEF AUDIO}
	if SDL_Init(SDL_INIT_AUDIO) then
	begin
		Audio := SDL_OpenAudioDeviceStream(
			SDL_AUDIO_DEVICE_DEFAULT_PLAYBACK,
			@AudioSpec, AudioCallback, nil);
	end
	else
		ShowMessage('ERROR: Could not init audio!');
	{$ENDIF}
end;

procedure TForm1.LoadModule(const Filename: String);
var
	i: Integer;
	S: TSample;
begin
	ListBox1.Items.Clear;

	if Module.Playing then
	begin
		bPlayClick(Self);
		while BufferUpdating do;
	end;

	if not Module.LoadFromFile(Filename)  then // ins
	begin
		ShowMessage('ERROR: Could not load module!');
		Exit;
	end
	else
	begin
		bPlayClick(Self);
	end;

	for i := 0 to Length(Module.Samples)-1 do
	begin
		S := Module.Samples[i];
		if S <> nil then
			ListBox1.items.AddObject('%.2d. %s', [i+1, S.SampleName], S);
	end;

	ListBox1.ItemIndex := 0;
	ListBox1Click(Self);
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
	{$IFDEF AUDIO}
	SDL_DestroyAudioStream(Audio);
	SDL_Quit;
	{$ENDIF}

	Module.Free;
end;

procedure TForm1.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
	if Key = VK_ESCAPE then Close;
end;

procedure TForm1.bPlayClick(Sender: TObject);
begin
	if Module.Playing then
	begin
		bPlay.Caption := 'Play';
		{$IFDEF AUDIO}
		SDL_PauseAudioStreamDevice(Audio);
		{$ENDIF}
		Module.Stop;
	end
	else
	if Module.Play() then
	begin
		bPlay.Caption := 'Stop';
		{$IFDEF AUDIO}
		SDL_ResumeAudioStreamDevice(Audio);
		{$ENDIF}
	end;

	Timer.Enabled := Module.Playing;
end;

procedure TForm1.TimerTimer(Sender: TObject);
begin
	Caption := Format('Order: %d, Pattern: %d, Row: %d', [Module.CurrentOrder, Module.CurrentPattern, Module.CurrentRow]);

	pb.Invalidate;
end;

procedure TForm1.pbPaint(Sender: TObject);
var
	X, Y, W, H: Integer;
	P: PInt16;
begin
	pb.Canvas.Clear;

	if not Module.Loaded then Exit;

	W := pb.ClientWidth;
	H := pb.ClientHeight;

	pb.Canvas.PenPos := Point(0, H div 2);
	pb.Canvas.Pen.Color := clWhite;

	if Module.Playing then
	begin
		if BufferUpdating then Exit;

		{$IFNDEF AUDIO}
		Module.FillAudioBuffer(@Buffer[0], 1024);
		{$ENDIF}

		BufferUpdating := True;

		P := @Buffer[0];

		for X := 0 to W-1 do
		begin
			Y := Trunc((P^ / 65536) * (H-1)) + (H div 2);
			pb.Canvas.LineTo(X, H-Y);
			Inc(P, 2);
		end;

		BufferUpdating := False;
	end;
end;

procedure TForm1.pbSamplePaint(Sender: TObject);
var
	X, Y, W, H: Integer;
	F, D: Double;
	S: TSample;
	P16: PInt16;
	P8:  PInt8;
begin
	pbSample.Canvas.Clear;

	if not Module.Loaded then Exit;

	W := pbSample.ClientWidth;
	H := pbSample.ClientHeight;
	F := 0;

	pbSample.Canvas.PenPos := Point(0, H div 2);
	pbSample.Canvas.Pen.Color := clWhite;

	S := TSample(ListBox1.Items.Objects[ListBox1.ItemIndex]);
	if (S = nil) or (S.Length < 2) or (not S.Flags.SMPF_ASSOCIATED_WITH_HEADER) then Exit;

	if S.Flags.SMPF_16BIT then
		D := (S.Length-1) / W
	else
		D := (S.Length-1) / W;

	if S.Flags.SMPF_16BIT then
	begin
		P16 := S.Data[False].Data;
		for X := 0 to W-1 do
		begin
			Y := Trunc(P16[Trunc(F)] / 65536 * (H-1)) + (H div 2);
			F += D;
			pbSample.Canvas.LineTo(X, H-Y);
		end;
	end
	else
	begin
		P8  := S.Data[False].Data;
		for X := 0 to W-1 do
		begin
			Y := Trunc(P8[Trunc(F)] / 256 * (H-1)) + (H div 2);
			F += D;
			pbSample.Canvas.LineTo(X, H-Y);
		end;
	end;
end;

procedure TForm1.FormDropFiles(Sender: TObject; const FileNames: array of string);
var
	S: String;
begin
	S := FileNames[0];
	if (S = '') or (not FileExists(S)) then Exit;

	LoadModule(S);
end;

procedure TForm1.ListBox1Click(Sender: TObject);

	function IfThen(B: Boolean; const sYes, sNo: String): String; inline;
	begin
		if B then Result := sYes else Result := sNo;
	end;

var
	S: TSample;
begin
	pbSample.Invalidate;
	Memo.Lines.Clear;

	S := TSample(ListBox1.Items.Objects[ListBox1.ItemIndex]);
	if S = nil then Exit;

	Memo.Lines.Add('[Module Flags]');
	Memo.Lines.Add(Format('Channels:       %s', [IfThen(Module.Header.Flags.ITF_STEREO,      'Stereo', 'Mono')]));
	Memo.Lines.Add(Format('Ins/Smp:        %s', [IfThen(Module.Header.Flags.ITF_INSTR_MODE,  'Instruments', 'Samples')]));
	Memo.Lines.Add(Format('Pitch slides:   %s', [IfThen(Module.Header.Flags.ITF_LINEAR_FRQ,  'Linear', 'Amiga')]));
	Memo.Lines.Add(Format('Old effects:    %s', [IfThen(Module.Header.Flags.ITF_OLD_EFFECTS, 'Yes', 'No')]));
	Memo.Lines.Add(Format('Compatible Gxx: %s', [IfThen(Module.Header.Flags.ITF_COMPAT_GXX,  'Yes', 'No')]));
	Memo.Lines.Add('');

	Memo.Lines.Add('[Sample %d]', [ListBox1.ItemIndex+1]);

	Memo.Lines.Add(Format('Length:     %d samples (%d bytes)', [S.Length, S.Length*(BoolToInt[S.Flags.SMPF_16BIT]+1)]));
	Memo.Lines.Add(Format('Volume:     %d   (Glob: %d)', [S.Vol, S.GlobVol]));
	Memo.Lines.Add(Format('Speed:      %d', [S.C5Speed]));
	Memo.Lines.Add(Format('Pan:        %d', [S.DefPan]));
	Memo.Lines.Add(Format('Loop:       %s (%d-%d) %s', [
		IfThen(S.Flags.SMPF_USE_LOOP, 'Enabled ', 'Disabled'),
		S.LoopBegin, S.LoopEnd,
		IfThen(S.Flags.SMPF_LOOP_PINGPONG, '(Pingpong)', '')
	]));
	Memo.Lines.Add(Format('Offset:     %d', [S.OffsetInFile]));
	Memo.Lines.Add(Format('Format:     %s', [IfThen((S.Cvt and 1) <> 0, 'Signed', 'Unsigned')]));
	Memo.Lines.Add(Format('Bitness:    %s', [IfThen(S.Flags.SMPF_16BIT, '16-bit', '8-bit')]));
	Memo.Lines.Add(Format('Channels:   %s', [IfThen(S.Flags.SMPF_STEREO, 'Stereo', 'Mono')]));
	Memo.Lines.Add(Format('SusLoop:    %s', [IfThen(S.Flags.SMPF_USE_SUSTAINLOOP, 'Yes', 'No')]));
	Memo.Lines.Add(Format('Compressed: %s %s', [
		IfThen(S.Flags.SMPF_COMPRESSED, 'Yes', 'No'),
		IfThen((S.Cvt and 4) <> 0, '(Delta encoded)', '')
	]));
	Memo.Lines.Add(Format('Associated with header: %s', [IfThen(S.Flags.SMPF_ASSOCIATED_WITH_HEADER, 'Yes', 'No')]));
end;

end.


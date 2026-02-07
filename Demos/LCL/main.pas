unit Main;

{$MODE DELPHI}
{$H+}
{$MACRO ON}

// on Windows, use WinMM for audio output
// on other systems, require SDL3 for this example
// NOTE! Add SDL3_package to required packages in IDE if using SDL3
// TODO: Show waveform while playing with WinMM driver

{$IFDEF WINDOWS}
	{$DEFINE AudioDeviceUnit  := IT.AudioDevice.WinMM}
	{$DEFINE TAudioDeviceType := TITAudioDevice_WinMM}
{$ELSE}
	{$DEFINE AudioDeviceUnit  := IT.AudioDevice.SDL3}
	{$DEFINE TAudioDeviceType := TITAudioDevice_SDL3}
{$ENDIF}

interface

uses
	Classes, SysUtils, Forms, Controls, Graphics, Dialogs,
	LCLType, StdCtrls, ExtCtrls,
	AudioDeviceUnit, // <- macro!
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
		procedure bPlayClick(Sender: TObject);
		procedure TimerTimer(Sender: TObject);
		procedure pbPaint(Sender: TObject);
		procedure ListBox1Click(Sender: TObject);
		procedure pbSamplePaint(Sender: TObject);
		procedure FormDropFiles(Sender: TObject; const FileNames: array of string);
	private
		procedure LoadModule(const Filename: String);
	public
		Module: TITModule;
		Output: TAudioDeviceType; // <- macro!
	end;

var
	Form1: TForm1;


implementation

{$R *.lfm}


procedure TForm1.FormShow(Sender: TObject);
begin
	OnShow := nil;

	// create the it2play instance
	Module := TITModule.Create;

	// initialize audio output device
	Output := TAudioDeviceType.Create(Module, 44100);

	// initialize the module with defaults
	Module.Init(DRIVER_DEFAULT, Output.Frequency);
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
	Module.Free;
	Output.Free;
end;

procedure TForm1.bPlayClick(Sender: TObject);
begin
	if Module.Playing then
	begin
		Module.Stop;
		bPlay.Caption := 'Play';
	end
	else
	if Module.Play then
	begin
		bPlay.Caption := 'Stop';
	end;

	Timer.Enabled := Module.Playing;
end;

procedure TForm1.TimerTimer(Sender: TObject);
begin
	// display song progress
	Caption := Format('Order: %d, Pattern: %d, Row: %d',
		[Module.CurrentOrder, Module.CurrentPattern, Module.CurrentRow]);

	// paint a waveform of outgoing audio
	pb.Invalidate;
end;

procedure TForm1.FormDropFiles(Sender: TObject; const FileNames: array of string);
var
	S: String;
begin
	S := FileNames[0];
	if (S <> '') and (FileExists(S)) then
		LoadModule(S);
end;

procedure TForm1.LoadModule(const Filename: String);
var
	i: Integer;
	S: TSample;
begin
	ListBox1.Items.Clear;

	if Module.Playing then
		while Output.Buffer.Updating do;

	if not Module.LoadFromFile(Filename) then
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

	// ugly!
	if Module.Playing then
	begin
		if Output.Buffer.Updating then Exit;

		Output.Buffer.Updating := True;

		P := @Output.Buffer.Data[0];

		for X := 0 to W-1 do
		begin
			Y := Trunc((P^ / 65536) * (H-1)) + (H div 2);
			pb.Canvas.LineTo(X, H-Y);
			Inc(P, 2);
		end;

		Output.Buffer.Updating := False;
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


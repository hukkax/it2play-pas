unit Main;

{$MODE DELPHI}
{$H+}
{$MACRO ON}

// on Windows, use WinMM for audio output
// on other systems, require SDL3 for this example
// NOTE! Add SDL3_package to required packages in IDE if using SDL3

{$IFDEF WINDOWS}
	{$DEFINE AudioDeviceUnit  := IT.AudioDevice.WinMM}
	{$DEFINE TAudioDeviceType := TITAudioDevice_WinMM}
{$ELSE}
	{$DEFINE AudioDeviceUnit  := IT.AudioDevice.SDL3}
	{$DEFINE TAudioDeviceType := TITAudioDevice_SDL3}
{$ENDIF}

{$WARN 5026 off : Value parameter "$1" is assigned but never used}
{$WARN 5024 off : Parameter "$1" not used}

interface

uses
	Classes, SysUtils, Forms, Controls, Graphics, Dialogs,
	LCLType, StdCtrls, ExtCtrls, ComCtrls,
	AudioDeviceUnit, // <- macro!
	IT2play;

type
	TMainForm = class(TForm)
		bPlay: TButton;
		pb: TPaintBox;
		lbSamples: TListBox;
		Memo: TMemo;
		pbSample: TPaintBox;
		PageControl: TPageControl;
		tsSamples: TTabSheet;
		tsInstruments: TTabSheet;
		tsPatterns: TTabSheet;
		lbInstruments: TListBox;
		lbPatterns: TListBox;
		bPrevOrder: TButton;
		bNextOrder: TButton;
		lOrder: TLabel;
		lTempo: TLabel;
		lPattern: TLabel;
		TabSheet1: TTabSheet;
		GroupBox1: TGroupBox;
		MemoMod: TMemo;
		sbMixVol: TScrollBar;
		Label4: TLabel;
		lMixVol: TLabel;
		lVoices: TLabel;
		cbStereo: TCheckBox;
		sbPanSep: TScrollBar;
		cmbDriver: TComboBox;
		Label1: TLabel;
		Button1: TButton;

		procedure FormShow(Sender: TObject);
		procedure FormDestroy(Sender: TObject);
		procedure bPlayClick(Sender: TObject);
		procedure pbPaint(Sender: TObject);
		procedure lbSamplesClick(Sender: TObject);
		procedure pbSamplePaint(Sender: TObject);
		procedure FormDropFiles(Sender: TObject; const FileNames: array of string);
		procedure lbPatternsClick(Sender: TObject);
		procedure bPrevOrderClick(Sender: TObject);
		procedure bNextOrderClick(Sender: TObject);
		procedure sbMixVolChange(Sender: TObject);
		procedure sbMixVolScroll(Sender: TObject; ScrollCode: TScrollCode; var ScrollPos: Integer);
		procedure cbStereoChange(Sender: TObject);
		procedure sbPanSepScroll(Sender: TObject; ScrollCode: TScrollCode; var ScrollPos: Integer);
		procedure cmbDriverChange(Sender: TObject);
		procedure Button1Click(Sender: TObject);
	private
		procedure LoadModule(const Filename: String);

		// callback handlers
		procedure OnRowChange(M: TITModule);
		procedure OnBufferFilled(M: TITModule);
	public
		Module: TITModule;
		Output: TAudioDeviceType; // <- macro!
	end;

var
	MainForm: TMainForm;


implementation

{$R *.lfm}


function IfThen(B: Boolean; const sYes, sNo: String): String; inline;
begin
	if B then Result := sYes else Result := sNo;
end;

procedure TMainForm.FormShow(Sender: TObject);
const
	DefaultDriver = TITAudioDriverType.HighQuality;
var
	dr: TITAudioDriverType;
begin
	OnShow := nil;
	PageControl.Enabled := False;

	cmbDriver.Items.Clear;
	for dr in TITAudioDriverType do
		cmbDriver.Items.Add(AudioDriverTypeNames[dr]);
	cmbDriver.ItemIndex := Ord(DefaultDriver);

	// create the it2play instance
	Module := TITModule.Create;

	// initialize audio output device
	Output := TAudioDeviceType.Create(Module, 44100);

	// initialize the module with defaults
	Module.Init(DefaultDriver, Output.Frequency);

	Module.Options.AutoCropOrderList    := True;
	Module.Options.AutoReduceVolume     := True;
	Module.Options.AutoCalculateLength  := True;
	Module.Options.AutoGetOptimumVolume := False;

	// set up callbacks to display waveform/playback info
	Module.OnBufferFilled := OnBufferFilled;
	Module.OnRowChange    := OnRowChange;
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
	Module.Free;
	Output.Free;
end;

procedure TMainForm.bPlayClick(Sender: TObject);
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
end;

procedure TMainForm.FormDropFiles(Sender: TObject; const FileNames: array of string);
var
	S: String;
begin
	S := FileNames[0];
	if (S <> '') and (FileExists(S)) then
		LoadModule(S);
end;

procedure TMainForm.LoadModule(const Filename: String);
var
	i: Integer;
	S: String;
	Sam: TITSample;
	Ins: TITInstrument;
	Pat: TITPattern;
begin
	lbSamples.Items.Clear;
	lbInstruments.Items.Clear;
	lbPatterns.Items.Clear;
	Memo.Lines.Clear;

	// wait until audio output has finished processing

	if Module.Playing then
		Output.Lock;

	// load in new module

	if (Module.LoadFromFile(Filename)) and (Module.Header.PatNum > 0) then
	begin
		S := TrimRight(Module.Header.SongName);
		if S <> '' then S := S + ' ';
		S := S + '(' + ExtractFileName(Filename) + ')';
		Caption := 'IT2play - ' + S;

		// if the orderlist is empty generate one from the patterns
		if Module.Header.OrdNum = 0 then
			Module.Header.OrdNum := 1;
		if Module.Orders[0] = 255 then
		begin
			Module.Header.OrdNum := Module.Header.PatNum;
			for i := 0 to Module.Header.OrdNum-1 do
				Module.Orders[0] := i;
		end;

		// populate sample list
		//
		for i := 0 to Length(Module.Samples)-1 do
		begin
			Sam := Module.Samples[i];
			if Sam <> nil then
			begin
				if (Sam.Length > 0) or (Sam.SampleName <> '') then
					lbSamples.Items.AddObject('%.2d. %s', [i+1, Sam.SampleName], Sam);
			end;
		end;
		if lbSamples.Items.Count > 0 then
			lbSamples.ItemIndex := 0;

		// populate instrument list
		//
		if Module.Header.Flags.ITF_INSTR_MODE then
		begin
			for i := 0 to Length(Module.Instruments)-1 do
			begin
				Ins := Module.Instruments[i];
				if Ins <> nil then
					lbInstruments.Items.AddObject('%.2d. %s', [i+1, Ins.InstrumentName], Ins);
			end;
			if lbInstruments.Items.Count > 0 then
				lbInstruments.ItemIndex := 0;
			tsInstruments.TabVisible := True;
		end
		else
			tsInstruments.TabVisible := False;

		// populate pattern list
		//
		for i := 0 to Module.Header.PatNum-1 do
		begin
			Pat := Module.Patterns[i];
			if Pat <> nil then
				lbPatterns.Items.AddObject('%.2d. Channels:%d Rows:%d', [i, Pat.GetChannelCount, Pat.Rows], Pat);
		end;
		if lbPatterns.Items.Count > 0 then
			lbPatterns.ItemIndex := 0;

		with MemoMod do
		begin
			Lines.Clear;
			Lines.Add(Format('Filename:       %s', [ExtractFilename(Filename)]));
			Lines.Add(Format('Song title:     %s', [Module.Header.SongName]));
			Lines.Add(Format('Duration:       %d:%.2d.%.2d',
				[Module.SongDuration.Hours, Module.SongDuration.Minutes, Module.SongDuration.Seconds]));
			Lines.Add(Format('Total channels: %d', [Module.ChannelsUsed]));
			Lines.Add(Format('Channels:       %s', [IfThen(Module.Header.Flags.ITF_STEREO,      'Stereo', 'Mono')]));
			Lines.Add(Format('Ins/Smp:        %s', [IfThen(Module.Header.Flags.ITF_INSTR_MODE,  'Instruments', 'Samples')]));
			Lines.Add(Format('Pitch slides:   %s', [IfThen(Module.Header.Flags.ITF_LINEAR_FRQ,  'Linear', 'Amiga')]));
			Lines.Add(Format('Old effects:    %s', [IfThen(Module.Header.Flags.ITF_OLD_EFFECTS, 'Yes', 'No')]));
			Lines.Add(Format('Compatible Gxx: %s', [IfThen(Module.Header.Flags.ITF_COMPAT_GXX,  'Yes', 'No')]));

			if Module.Header.MessageLength > 0 then
			begin
				Lines.Add('Song message:');
				Lines.AddStrings(Module.SongMessage);
			end;
		end;

		sbMixVol.Position := Module.MixingVolume;
		sbPanSep.Position := Module.PanSeparation;
		cbStereo.Checked  := Module.Header.Flags.ITF_STEREO;

		// load success, start playback
		bPlay.Enabled := True;
		PageControl.Enabled := True;
		bPlayClick(Self);
	end
	else
	begin
		bPlay.Caption := '-';
		bPlay.Enabled := False;
		PageControl.Enabled := False;
		ShowMessage('Load failed: ' + Module.ErrorMessage);
	end;

	Output.Unlock;
end;

procedure TMainForm.OnRowChange(M: TITModule);
begin
	// display song progress
	lOrder.Caption := Format('Order %d / %d ',
		[Module.CurrentOrder, Module.Header.OrdNum-1]);
	lPattern.Caption := Format('Pattern: %d.%.2d / %d ',
		[Module.CurrentPattern, Module.CurrentRow, Module.Header.PatNum-1]);
	lTempo.Caption := Format('Tempo/Speed: %d / %d ',
		[Module.Tempo, Module.CurrentSpeed]);
	lVoices.Caption := Format('Active voices: %d ', [Module.GetActiveVoices]);

	if Module.MixingVolume <> sbMixVol.Position then
		sbMixVol.Position := Module.MixingVolume;
end;

procedure TMainForm.OnBufferFilled(M: TITModule);
begin
	// paint a waveform of outgoing audio
	pb.Invalidate;
end;

procedure TMainForm.pbPaint(Sender: TObject);
var
	X, Y, W, H: Integer;
	P: PInt16;
begin
	pb.Canvas.Clear;

	if not Module.Loaded then Exit;

	W := pb.ClientWidth;
	H := pb.ClientHeight;

	pb.Canvas.PenPos := Point(0, H div 2);
	pb.Canvas.Pen.Color := RGBToColor(255, 210, 120);

	// ugly!
	if Module.Playing then
	begin
		Output.Lock;

		P := @Output.Buffer.Data[0];

		for X := 0 to W-1 do
		begin
			Y := Trunc((P^ / 65536) * (H-1)) + (H div 2);
			pb.Canvas.LineTo(X, H-Y);
			Inc(P, 2);
		end;

		Output.Unlock;
	end;
end;

procedure TMainForm.pbSamplePaint(Sender: TObject);
var
	X, Y, W, H: Integer;
	F, D: Double;
	S: TITSample;
	P16: PInt16;
	P8:  PInt8;
begin
	pbSample.Canvas.Clear;

	if not Module.Loaded then Exit;

	W := pbSample.ClientWidth;
	H := pbSample.ClientHeight;
	F := 0;

	pbSample.Canvas.PenPos := Point(0, H div 2);
	pbSample.Canvas.Pen.Color := clBtnText;

	S := TITSample(lbSamples.Items.Objects[lbSamples.ItemIndex]);
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

procedure TMainForm.lbSamplesClick(Sender: TObject);
var
	S: TITSample;
begin
	Memo.Lines.Clear;
	if not Module.Loaded then Exit;

	// draw waveform
	pbSample.Invalidate;

	Memo.Lines.BeginUpdate;

	S := TITSample(lbSamples.Items.Objects[lbSamples.ItemIndex]);
	if S = nil then Exit;

	Memo.Lines.Add('[Sample %d]', [lbSamples.ItemIndex+1]);

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

	Memo.Lines.EndUpdate;
end;

procedure TMainForm.lbPatternsClick(Sender: TObject);
const
	NoteNames  = 'CCDDEFFGGAAB';
	NoteSharp  = '-#-#--#-#-#-';
	NoteOctave   = '0123456789';
var
	Pat: TITPattern;
	P: TITUnpackedPattern;
	N: PUnpackedNote;
	i, Chan, Row, NumChans, NumRows: Integer;
	S, sNote: String;
begin
	Memo.Lines.Clear;
	if not Module.Loaded then Exit;

	Memo.Lines.BeginUpdate;

	Pat := TITPattern(lbPatterns.Items.Objects[lbPatterns.ItemIndex]);
	if Pat = nil then Exit;

	P := Pat.Unpack;

	NumChans := Pat.GetChannelCount;
	NumRows  := Pat.Rows;

	Memo.Lines.Add('[Pattern %d]', [lbPatterns.ItemIndex]);
	Memo.Lines.Add('');

	for Row := 0 to NumRows-1 do
	begin
		S := '';
		for Chan := 0 to NumChans-1 do
		begin
			sNote := '---';
			N := @P.Notes[Chan, Row];

			case N.Note of
				1..120:
				begin
					i := (N.Note-1) mod 12;
					sNote[1] := NoteNames[i+1];
					sNote[2] := NoteSharp[i+1];
					sNote[3] := NoteOctave[(N.Note-1) div 12 + 1];
				end;
				254: sNote := '^^^';
				255: sNote := '===';
			end;

			S := S + sNote + ' | ';
		end;

		Memo.Lines.Add(S);
	end;

	Memo.Lines.EndUpdate;
end;

procedure TMainForm.bPrevOrderClick(Sender: TObject);
begin
	if Module.Playing then
		Module.PreviousOrder;
end;

procedure TMainForm.bNextOrderClick(Sender: TObject);
begin
	if Module.Playing then
		Module.NextOrder;
end;

procedure TMainForm.cbStereoChange(Sender: TObject);
begin
	if not Module.Loaded then Exit;

	Module.Driver.WaitFor;
	Module.Header.Flags.ITF_STEREO := cbStereo.Checked;
end;

procedure TMainForm.sbMixVolChange(Sender: TObject);
begin
	lMixVol.Caption := Module.MixingVolume.ToString;
end;

procedure TMainForm.sbMixVolScroll(Sender: TObject; ScrollCode: TScrollCode;
	var ScrollPos: Integer);
begin
	Module.MixingVolume := ScrollPos;
end;

procedure TMainForm.sbPanSepScroll(Sender: TObject; ScrollCode: TScrollCode;
	var ScrollPos: Integer);
begin
	Module.PanSeparation := ScrollPos;
end;

procedure TMainForm.cmbDriverChange(Sender: TObject);
begin
	if (Module <> nil) and (Module.Loaded) and (cmbDriver.ItemIndex >= 0) then
		Module.DriverType := TITAudioDriverType(cmbDriver.ItemIndex);
end;

procedure TMainForm.Button1Click(Sender: TObject);
var
	WasPlaying: Boolean;
begin
	WasPlaying := Module.Playing;
	if WasPlaying then
		Module.Stop;

	Module.GetOptimumVolume;

	if WasPlaying then
		Module.Play;
end;


end.


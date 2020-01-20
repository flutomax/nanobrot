unit uFloatExp;

{

  Nanobrot
  Copyright (C) 2019 - 2020 Vasily Makarov

  Original C++ source codes:
  Copyright (C) 2013-2017 Karl Runmo
  Copyright (C) 2017-2018 Claude Heiland-Allen

  This program is free software: you can redistribute it and/or modify
  it under the terms of the GNU Affero General Public License as
  published by the Free Software Foundation, either version 3 of the
  License, or (at your option) any later version.

  This program is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU Affero General Public License for more details.

  You should have received a copy of the GNU Affero General Public License
  along with this program.  If not, see <https://www.gnu.org/licenses/>.

}

interface

{$I Nanobrot.inc}

uses
  Windows, Classes, uDecNumber;

const
  MAX_PREC = 1020;
  EXP_MIN: int64 = -$80000000000000;
  FIXEDFLOAT_ENTRIES = DECNUMDIGITS div 8 + 16;
  FIXEDFLOAT_PARTMAX = 100000000;
  FIXEDFLOAT_DIGITS = 8;

type

  TFloatExp = record
  private
    fVal: Double;
    fExp: Int64;
    procedure Align; {$IFDEF DOINLINE} inline; {$ENDIF}
    procedure Init(const aVal: Double); overload; {$IFDEF DOINLINE} inline; {$ENDIF}
    procedure Init(const aVal: Double; const aExp: Int64); overload; {$IFDEF DOINLINE} inline; {$ENDIF}
    procedure Init(const aVal: Extended); overload; {$IFDEF DOINLINE} inline; {$ENDIF}
    procedure Init(const aVal: AnsiString); overload; {$IFDEF DOINLINE} inline; {$ENDIF}
    class function SetExp(const aVal: Double; const aExp: Int64): Double; static; {$IFDEF DOINLINE} inline; {$ENDIF}
  public
    function ToString: AnsiString; {$IFDEF DOINLINE} inline; {$ENDIF}
    function ToDisplay: AnsiString; {$IFDEF DOINLINE} inline; {$ENDIF}
    function Abs: TFloatExp; {$IFDEF DOINLINE} inline; {$ENDIF}
    function Sqrt: TFloatExp; {$IFDEF DOINLINE} inline; {$ENDIF}
    class operator Implicit(const Value: Double): TFloatExp; overload; {$IFDEF DOINLINE} inline; {$ENDIF}
    class operator Implicit(const Value: Extended): TFloatExp; overload; {$IFDEF DOINLINE} inline; {$ENDIF}
    class operator Implicit(const Value: Integer): TFloatExp; overload; {$IFDEF DOINLINE} inline; {$ENDIF}
    class operator Implicit(const Value: AnsiString): TFloatExp; overload; {$IFDEF DOINLINE} inline; {$ENDIF}
    class operator Implicit(const Value: TCDecNumber): TFloatExp; overload; {$IFDEF DOINLINE} inline; {$ENDIF}
    class operator Implicit(const Value: TFloatExp): Double; overload; {$IFDEF DOINLINE} inline; {$ENDIF}
    class operator Implicit(const Value: TFloatExp): Extended; overload; {$IFDEF DOINLINE} inline; {$ENDIF}
    class operator Negative(const Value: TFloatExp): TFloatExp; {$IFDEF DOINLINE} inline; {$ENDIF}
    class operator Positive(const Value: TFloatExp): TFloatExp; {$IFDEF DOINLINE} inline; {$ENDIF}
    // Addition
    class operator Add(const Left, Right: TFloatExp): TFloatExp; {$IFDEF DOINLINE} inline; {$ENDIF}
    // Subtraction
    class operator Subtract(const Left, Right: TFloatExp): TFloatExp; {$IFDEF DOINLINE} inline; {$ENDIF}
    // Multiplication
    class operator Multiply(const Left, Right: TFloatExp): TFloatExp; {$IFDEF DOINLINE} inline; {$ENDIF}
    // Division
    class operator Divide(const Left, Right: TFloatExp): TFloatExp; {$IFDEF DOINLINE} inline; {$ENDIF}
    // Modulus
    class operator Modulus(const Left, Right: TFloatExp): TFloatExp; {$IFDEF DOINLINE} inline; {$ENDIF}
    // Comparison
    class operator Equal(const Left, Right: TFloatExp): Boolean; {$IFDEF DOINLINE} inline; {$ENDIF}
    class operator NotEqual(const Left, Right: TFloatExp): Boolean; {$IFDEF DOINLINE} inline; {$ENDIF}
    class operator GreaterThan(const Left, Right: TFloatExp): Boolean; {$IFDEF DOINLINE} inline; {$ENDIF}
    class operator GreaterThanOrEqual(const Left, Right: TFloatExp): Boolean; {$IFDEF DOINLINE} inline; {$ENDIF}
    class operator LessThan(const Left, Right: TFloatExp): Boolean; {$IFDEF DOINLINE} inline; {$ENDIF}
    class operator LessThanOrEqual(const Left, Right: TFloatExp): Boolean; {$IFDEF DOINLINE} inline; {$ENDIF}
  end;

  PFloatExpArray = ^TFloatExpArray;
  TFloatExpArray = array[0..0] of TFloatExp;

  PFloatExpComplex = ^TFloatExpComplex;
  TFloatExpComplex = record
  public
    Re: TFloatExp;
    Im: TFloatExp;
    constructor Create(const aRe: Double); overload;
    constructor Create(const aRe, aIm: Double); overload;
    constructor Create(const aRe: TFloatExp); overload;
    constructor Create(const aRe, aIm: TFloatExp); overload;
    function ToString: AnsiString; {$IFDEF DOINLINE} inline; {$ENDIF}
    function Abs: TFloatExp; {$IFDEF DOINLINE} inline; {$ENDIF}
    function Norm: TFloatExp; {$IFDEF DOINLINE} inline; {$ENDIF}
    class operator Explicit(const Value: Double): TFloatExpComplex; overload; {$IFDEF DOINLINE} inline; {$ENDIF}
    class operator Explicit(const Value: TFloatExp): TFloatExpComplex; overload; {$IFDEF DOINLINE} inline; {$ENDIF}
    class operator Implicit(const Value: Double): TFloatExpComplex; overload; {$IFDEF DOINLINE} inline; {$ENDIF}
    class operator Implicit(const Value: TFloatExp): TFloatExpComplex; overload; {$IFDEF DOINLINE} inline; {$ENDIF}
    class operator Implicit(const Value: TCDecNumberComplex): TFloatExpComplex; overload; {$IFDEF DOINLINE} inline; {$ENDIF}
    class operator Negative(const Value: TFloatExpComplex): TFloatExpComplex; {$IFDEF DOINLINE} inline; {$ENDIF}
    class operator Add(const Left, Right: TFloatExpComplex): TFloatExpComplex; {$IFDEF DOINLINE} inline; {$ENDIF}
    class operator Subtract(const Left, Right: TFloatExpComplex): TFloatExpComplex; {$IFDEF DOINLINE} inline; {$ENDIF}
    class operator Multiply(const Left, Right: TFloatExpComplex): TFloatExpComplex; {$IFDEF DOINLINE} inline; {$ENDIF}
    class operator Divide(const Left, Right: TFloatExpComplex): TFloatExpComplex; {$IFDEF DOINLINE} inline; {$ENDIF}
  end;

  PFloatExpComplexArray = ^TFloatExpComplexArray;
  TFloatExpComplexArray = array[0..0] of TFloatExpComplex;
  TFloatExpComplexArray128 = array[0..127] of TFloatExpComplex;
  TFloatExpComplexMatrix128 = array[0..127, 0..127] of TFloatExpComplex;

  function sprintf(buf: PAnsiChar; format: PAnsiChar): integer; cdecl varargs;
    external 'MSVCRT.DLL';

implementation

uses
  Types, Math, AnsiStrings, uMisc;


{ TFloatExp }


procedure TFloatExp.Align;
var
  IntCst: Int64 absolute fVal;
begin
  if fVal <> 0 then begin
    fExp := fExp + ((IntCst and $7FF0000000000000) shr 52) - 1023;
    IntCst := (IntCst and $800FFFFFFFFFFFFF) or $3FF0000000000000;
  end else begin
    fVal := 0;
		fExp := EXP_MIN;
  end;
end;

class function TFloatExp.SetExp(const aVal: Double; const aExp: Int64): Double;
var
  IntCst: Int64 absolute Result;
begin
  Result := aVal;
  IntCst := (IntCst and $800FFFFFFFFFFFFF) or ((aExp + 1023) shl 52);
end;

procedure TFloatExp.Init(const aVal: Double);
begin
  fVal := aVal;
  fExp := 0;
  Align;
end;

procedure TFloatExp.Init(const aVal: Double; const aExp: Int64);
begin
  fVal := aVal;
  fExp := aExp;
  Align;
end;

procedure TFloatExp.Init(const aVal: Extended);
var
  m: Extended;
  e: Integer;
begin
  Frexp(aVal, m, e);
  Init(m, Int64(e));
end;

procedure TFloatExp.Init(const aVal: AnsiString);
var
  Values: TInt64DynArray;
  sz, sn, e: PAnsiChar;
  i, n, nexp, nnew, count,
  nmax, err: Integer;
  sign: Boolean;
  startexp, partmax: TFloatExp;
begin
  SetLength(Values, FIXEDFLOAT_ENTRIES);
  FillChar(Values[0], FIXEDFLOAT_ENTRIES * sizeof(Int64), 0);
  count := 0;
  sn := nil;
  sz := PAnsiChar(aVal);
  while sz^ in [#9, #10, #13, #32] do
    Inc(sz);
	if sz^ = '-' then begin
		sign := true;
		Inc(sz);
	end
	else
		sign := false;
  e := AnsiStrScan(sz, 'E');
	if e = nil then
		e := AnsiStrScan(sz,'e');
	if Assigned(e) then begin
		Inc(e);
    Val(e, nexp, err);
    if err <> 0 then
      Exit;
		if nexp < 0 then begin
			Inc(nexp);
			nnew := StrLen(sz) - nexp + 10;
			sn := AnsiStrAlloc(nnew + 256);
			n := 2;
			StrCopy(sn, '0.');
			while (nexp < 0) and (n < nNew) do begin
				sn[n] := '0';
        Inc(n);
				Inc(nexp);
			end;
			while (sz^ <> #0) and (n < nNew) do begin
				if sz^ = '.' then begin
					Inc(sz);
					continue;
				end
				else
          if not (sz^ in ['0'..'9']) then
					  break;
				sn[n] := sz^;
        Inc(n);
				Inc(sz);
			end;
			sn[n] := #0;
			sz := sn;
		end;
	end;
	while sz^ in ['0'..'9'] do begin
		Count := 1;
		Values[0] := 10 * Values[0] + Ord(sz^) - Ord('0');
		Inc(sz);
	end;
	if sz^ = '.' then begin
		Inc(sz);
		nmax := FIXEDFLOAT_ENTRIES;
		if Count=0 then begin
			Values[0] := 0;
			Inc(Count);
		end;
    while Count<nmax do begin
			Values[Count] := 0;
			i := 0;
			while (sz^ in ['0'..'9']) and (i < FIXEDFLOAT_DIGITS) do begin
				Values[Count] := 10 * Values[Count] + Ord(sz^) - Ord('0');
				Inc(sz);
				Inc(i);
			end;
			if i < FIXEDFLOAT_DIGITS then begin
				while i < FIXEDFLOAT_DIGITS do begin
          Values[Count] := 10 * Values[Count];
					Inc(i);
        end;
				Inc(Count);
				break;
			end;
      Inc(Count);
	  end;
	end;
	if Assigned(sn) then
		StrDispose(sn);
  // convert
  fVal := 0;
  fExp := 0;
  startexp := 1;
  partmax := FIXEDFLOAT_PARTMAX;
  for i := 0 to count - 1 do begin
    if Values[i] <> 0 then
      break;
    startexp := startexp / partmax;
	end;
  i := Min(i, count);
	n := i + 24 div FIXEDFLOAT_DIGITS;
  i := Min(i, count - 1);

  while (n >= i) and (n >= 0) do begin
    fVal := fVal / FIXEDFLOAT_PARTMAX + Values[n];
    Dec(n);
  end;
  Values := nil;
	Align;
	self := self * startexp;
	if sign then
		fVal := -fVal;
end;

function TFloatExp.Abs: TFloatExp;
begin
  result := self;
  if result.fVal < 0 then
			result.fVal := -result.fVal;
end;

function TFloatExp.Sqrt: TFloatExp;
var
  b: Boolean;
  v: double;
  e: Int64;
begin
  result.Init(
    System.Sqrt(IfThen(Odd(fExp), 2 * fVal, fVal)),
    IfThen(Odd(fExp), (fExp - 1) div 2, fExp div 2)
  );
end;

function TFloatExp.ToDisplay: AnsiString;
begin
  result := Format('%1.5f@%d',[fVal,fExp]);
end;

function TFloatExp.ToString: AnsiString;
const
  BUF_LEN = 150000;
var
  buf: PAnsiChar;
  partmin, partmax,
  tmp: TFloatExp;
  i, b: Integer;
  Neg: Boolean;
begin
  buf := AnsiStrAlloc(BUF_LEN);
  try
    ZeroMemory(buf, BUF_LEN * SizeOf(AnsiChar));
    partmin := 1/FIXEDFLOAT_PARTMAX;
		partmax := FIXEDFLOAT_PARTMAX;
    tmp := self;
    Neg := false;
    if tmp < 0 then begin
      Neg := true;
      tmp := -tmp;
    end;
    while tmp < partmin do begin
			if buf^ = #0 then begin
				if Neg then
					AnsiStrings.StrCopy(buf,'-0.0000000')
				else
					AnsiStrings.StrCopy(buf,'0.0000000');
			end
			else
        AnsiStrings.StrCat(buf,'00000000');
			tmp := tmp * partmax;
		end;
    sprintf(buf + AnsiStrings.StrLen(buf), '%.20f', Double(tmp));
    b:=6;
    i:=6;
    while buf[i] <> #0 do begin
      if buf[i] <> '.' then begin
				buf[b] := buf[i];
        Inc(b);
      end;
      Inc(i);
    end;
		buf[b]:=#0;
    SetString(result, buf, AnsiStrings.StrLen(buf));
  finally
    AnsiStrings.StrDispose(buf);
  end;
  if Neg then
    Insert('-', result, 1);
end;

class operator TFloatExp.Implicit(const Value: Double): TFloatExp;
begin
  Result.Init(Value);
end;

class operator TFloatExp.Implicit(const Value: Integer): TFloatExp;
begin
  Result.Init(Value);
end;

class operator TFloatExp.Implicit(const Value: AnsiString): TFloatExp;
begin
  Result.Init(Value);
end;

class operator TFloatExp.Implicit(const Value: Extended): TFloatExp;
begin
  Result.Init(Value);
end;

class operator TFloatExp.Implicit(const Value: TCDecNumber): TFloatExp;
begin
  Result.Init(Value.ToString);
end;

class operator TFloatExp.Implicit(const Value: TFloatExp): Double;
begin
  if (Value.fExp < -MAX_PREC) or (Value.fExp > MAX_PREC) then
    result := 0
  else
		result := SetExp(Value.fVal, Value.fExp);
end;

class operator TFloatExp.Implicit(const Value: TFloatExp): Extended;
var
  tmp: Extended;
begin
  if Value.fVal = 0 then
    Exit(0);
  if Value.fExp >= MaxInt then
    Exit(Infinity);
  if Value.fExp <= Low(integer) then
    Exit(0);
  tmp := Value.fVal;
	result := Ldexp(tmp, Value.fExp);
end;

class operator TFloatExp.Negative(const Value: TFloatExp): TFloatExp;
begin
  result := value;
  result.fVal := -result.fVal;
end;

class operator TFloatExp.Positive(const Value: TFloatExp): TFloatExp;
begin
  result := value;
end;

class operator TFloatExp.Add(const Left, Right: TFloatExp): TFloatExp;
var
  diff: Int64;
  aval: Double;
begin
  if Left.fExp > Right.fExp then begin
    diff := Left.fExp - Right.fExp;
    result.fExp := Left.fExp;
    if diff > MAX_PREC then
      result.fVal := Left.fVal
    else begin
			aval := SetExp(Right.fVal, -diff);
			result.fVal := Left.fVal + aval;
    end;
  end else begin
    diff := Right.fExp - Left.fExp;
    result.fExp := Right.fExp;
    if diff > MAX_PREC then
      result.fVal := Right.fVal
    else begin
			aval := SetExp(Left.fVal, -diff);
      result.fVal := Right.fVal + aval;
    end;
	end;
	result.Align;
end;

class operator TFloatExp.Subtract(const Left, Right: TFloatExp): TFloatExp;
var
  diff: Int64;
  aval: Double;
begin
	if Left.fExp>Right.fExp then begin
		diff := Left.fExp-Right.fExp;
		result.fExp := Left.fExp;
		if diff > MAX_PREC then
			result.fVal := Left.fVal
		else begin
			aval := SetExp(Right.fVal, -diff);
			result.fVal := Left.fVal - aval;
		end;
	end else begin
		diff := Right.fExp - Left.fExp;
		result.fExp := Right.fExp;
		if diff > MAX_PREC then
			result.fVal := -Right.fVal
		else begin
			aval := SetExp(Left.fVal, -diff);
			result.fVal := aval - Right.fVal;
		end;
	end;
  result.Align;
end;

class operator TFloatExp.Multiply(const Left, Right: TFloatExp): TFloatExp;
begin
  result.fVal := Left.fVal * Right.fVal;
	result.fExp := Left.fExp + Right.fExp;
	result.Align;
end;

class operator TFloatExp.Divide(const Left, Right: TFloatExp): TFloatExp;
begin
  if (Right.fVal = 0) then
    ZeroDivideError;
  result.fVal := Left.fVal / Right.fVal;
	result.fExp := Left.fExp - Right.fExp;
	result.Align;
end;

class operator TFloatExp.Modulus(const Left, Right: TFloatExp): TFloatExp;
var
  x: TFloatExp;
  n: Int64;
begin
  if Math.IsZero(Right.fVal) then
    ZeroDivideError;
  x := Left / Right;
  n := Floor(x.fVal);
  result := (Left - Right * n);
end;

class operator TFloatExp.Equal(const Left, Right: TFloatExp): Boolean;
begin
  result:=(Left.fExp = Right.fExp) and (Left.fVal = Right.fVal);
end;

class operator TFloatExp.NotEqual(const Left, Right: TFloatExp): Boolean;
begin
  result:=(Left.fExp <> Right.fExp) or (Left.fVal <> Right.fVal);
end;

class operator TFloatExp.GreaterThan(const Left, Right: TFloatExp): Boolean;
begin
  if Left.fVal > 0 then begin
    if Right.fVal < 0 then
			Exit(true);
    if Left.fExp > Right.fExp then
			Exit(true)
    else
      if Left.fExp < Right.fExp then
				Exit(false);
  end else begin
    if Right.fVal > 0 then
      Exit(false);
    if Left.fExp > Right.fExp then
			Exit(false)
    else
      if Left.fExp < Right.fExp then
				Exit(true);
	end;
  result:=Left.fVal > Right.fVal;
end;

class operator TFloatExp.GreaterThanOrEqual(const Left,
  Right: TFloatExp): Boolean;
begin
  result := (Left > Right) or (Left = Right);
end;

class operator TFloatExp.LessThan(const Left, Right: TFloatExp): Boolean;
begin
  if Left.fVal > 0 then begin
		if Right.fVal < 0 then
			Exit(false);
    if Left.fExp > Right.fExp then
			Exit(false)
    else
      if Left.fExp < Right.fExp then
				Exit(true);
  end else begin
    if Right.fVal > 0 then
			Exit(true);
    if Left.fExp > Right.fExp then
			Exit(true)
    else
      if Left.fExp < Right.fExp then
				Exit(false);
	end;
  result := Left.fVal < Right.fVal;
end;

class operator TFloatExp.LessThanOrEqual(const Left, Right: TFloatExp): Boolean;
begin
  result := (Left < Right) or (Left = Right);
end;



{ TFloatExpComplex }

constructor TFloatExpComplex.Create(const aRe: Double);
begin
  Re := aRe;
  Im := 0.0;
end;

constructor TFloatExpComplex.Create(const aRe, aIm: Double);
begin
  Re := aRe;
  Im := aIm;
end;

constructor TFloatExpComplex.Create(const aRe: TFloatExp);
begin
  Re := aRe;
  Im := 0.0;
end;

constructor TFloatExpComplex.Create(const aRe, aIm: TFloatExp);
begin
  Re := aRe;
  Im := aIm;
end;

function TFloatExpComplex.ToString: AnsiString;
begin
  result:=Format('(%s,%s)',[Re.ToDisplay, Im.ToDisplay]);
end;

function TFloatExpComplex.Abs: TFloatExp;
begin
  Result := Norm.Sqrt;
end;

function TFloatExpComplex.Norm: TFloatExp;
begin
  Result := Re * Re + Im * Im;
end;

class operator TFloatExpComplex.Explicit(const Value: Double): TFloatExpComplex;
begin
  Result.Create(Value);
end;

class operator TFloatExpComplex.Explicit(const Value: TFloatExp): TFloatExpComplex;
begin
  Result.Create(Value);
end;

class operator TFloatExpComplex.Implicit(const Value: Double): TFloatExpComplex;
begin
  Result.Create(Value);
end;

class operator TFloatExpComplex.Implicit(const Value: TFloatExp): TFloatExpComplex;
begin
  Result.Create(Value);
end;

class operator TFloatExpComplex.Implicit(const Value: TCDecNumberComplex): TFloatExpComplex;
begin
  Result.Create(Value.Re, Value.Im);
end;

class operator TFloatExpComplex.Negative(const Value: TFloatExpComplex): TFloatExpComplex;
begin
  Result.Create(-Value.Re, -Value.Im);
end;

class operator TFloatExpComplex.Add(const Left, Right: TFloatExpComplex): TFloatExpComplex;
begin
  Result.Create(Left.Re + Right.Re, Left.Im + Right.Im);
end;

class operator TFloatExpComplex.Subtract(const Left, Right: TFloatExpComplex): TFloatExpComplex;
begin
  Result.Create(Left.Re - Right.Re, Left.Im - Right.Im);
end;

class operator TFloatExpComplex.Multiply(const Left, Right: TFloatExpComplex): TFloatExpComplex;
begin
  Result.Create((Left.Re * Right.Re) - (Left.Im * Right.Im), (Left.Re * Right.Im) + (Left.Im * Right.Re));
end;

class operator TFloatExpComplex.Divide(const Left, Right: TFloatExpComplex): TFloatExpComplex;
var
  Denom: TFloatExp;
begin
  Denom := (Right.Re * Right.Re) + (Right.Im * Right.Im);
  if Denom = 0 then
    ZeroDivideError;
  Result.Create(((Left.Re * Right.Re) + (Left.Im * Right.Im)) / Denom,
    ((Left.Im * Right.Re) - (Left.Re * Right.Im)) / Denom);
end;



end.

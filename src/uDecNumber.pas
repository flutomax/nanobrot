unit uDecNumber;

{
  Translate original C++ source codes to Object Pascal Vasily Makarov

			Decimal number arithmetic module for the decNumber C Library.
			   Copyright (C) 2005-2013 Free Software Foundation, Inc.
			   Contributed by IBM Corporation.  Author Mike Cowlishaw.

			   This file is part of GCC.

			   GCC is free software; you can redistribute it and/or modify it under
			   the terms of the GNU General Public License as published by the Free
			   Software Foundation; either version 3, or (at your option) any later
			   version.

			   GCC is distributed in the hope that it will be useful, but WITHOUT ANY
			   WARRANTY; without even the implied warranty of MERCHANTABILITY or
			   FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
			   for more details.

			Under Section 7 of GPL version 3, you are granted additional
			permissions described in the GCC Runtime Library Exception, version
			3.1, as published by the Free Software Foundation.

			You should have received a copy of the GNU General Public License and
			a copy of the GCC Runtime Library Exception along with this program;
			see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see
			<http://www.gnu.org/licenses/>.

			 ------------------------------------------------------------------
			 Decimal Number arithmetic module
			 ------------------------------------------------------------------
			 This module comprises the routines for arbitrary-precision General
			 Decimal Arithmetic as defined in the specification which may be
			 found on the General Decimal Arithmetic pages.  It implements both
			 the full ('extended') arithmetic and the simpler ('subset')
			 arithmetic.

			 Usage notes:

			 1. This code is ANSI C89 except:

			    a) C99 line comments (double forward slash) are used.  (Most C
				 compilers accept these.  If yours does not, a simple script
				 can be used to convert them to ANSI C comments.)

			    b) Types from C99 stdint.h are used.  If you do not have this
				 header file, see the User's Guide section of the decNumber
				 documentation; this lists the necessary definitions.

			    c) If DECDPUN>4 or DECUSE64=1, the C99 64-bit int64_t and
				 uint64_t types may be used.  To avoid these, set DECUSE64=0
				 and DECDPUN<=4 (see documentation).

			    The code also conforms to C99 restrictions; in particular,
			    strict aliasing rules are observed.

			 2. The decNumber format which this library uses is optimized for
			    efficient processing of relatively short numbers; in particular
			    it allows the use of fixed sized structures and minimizes copy
			    and move operations.  It does, however, support arbitrary
			    precision (up to 999,999,999 digits) and arbitrary exponent
			    range (Emax in the range 0 through 999,999,999 and Emin in the
			    range -999,999,999 through 0).  Mathematical functions (for
			    example decNumberExp) as identified below are restricted more
			    tightly: digits, emax, and -emin in the context must be <=
			    DEC_MAX_MATH (999999), and their operand(s) must be within
			    these bounds.

			 3. Logical functions are further restricted; their operands must
			    be finite, positive, have an exponent of zero, and all digits
			    must be either 0 or 1.  The result will only contain digits
			    which are 0 or 1 (and will have exponent=0 and a sign of 0).

			 4. Operands to operator functions are never modified unless they
			    are also specified to be the result number (which is always
			    permitted).  Other than that case, operands must not overlap.

			 5. Error handling: the type of the error is ORed into the status
			    flags in the current context (decContext structure).  The
			    SIGFPE signal is then raised if the corresponding trap-enabler
			    flag in the decContext is set (is 1).

			    It is the responsibility of the caller to clear the status
			    flags as required.

			    The result of any routine which returns a number will always
			    be a valid number (which may be a special value, such as an
			    Infinity or NaN).

			 6. The decNumber format is not an exchangeable concrete
			    representation as it comprises fields which may be machine-
			    dependent (packed or unpacked, or special length, for example).
			    Canonical conversions to and from strings are provided; other
			    conversions are available in separate modules.

			 7. Normally, input operands are assumed to be valid.  Set DECCHECK
			    to 1 for extended operand checking (including NULL operands).
			    Results are undefined if a badly-formed structure (or a NULL
			    pointer to a structure) is provided, though with DECCHECK
			    enabled the operator routines are protected against exceptions.
			    (Except if the result pointer is NULL, which is unrecoverable.)

			    However, the routines will never cause exceptions if they are
			    given well-formed operands, even if the value of the operands
			    is inappropriate for the operation and DECCHECK is not set.
			    (Except for SIGFPE, as and where documented.)

			 8. Subset arithmetic is available only if DECSUBSET is set to 1.
			 ------------------------------------------------------------------
			 Implementation notes for maintenance of this module:

			 1. Storage leak protection:	Routines which use malloc are not
			    permitted to use return for fastpath or error exits (i.e.,
			    they follow strict structured programming conventions).
			    Instead they have a do while(0); construct surrounding the
			    code which is protected -- break may be used to exit this.
			    Other routines can safely use the return statement inline.

			    Storage leak accounting can be enabled using DECALLOC.

			 2. All loops use the for(;;) construct.  Any do construct does
			    not loop; it is for allocation protection as just described.

			 3. Setting status in the context must always be the very last
			    action in a routine, as non-0 status may raise a trap and hence
			    the call to set status may not return (if the handler uses long
			    jump).  Therefore all cleanup must be done first.  In general,
			    to achieve this status is accumulated and is only applied just
			    before return by calling decContextSetStatus (via decStatus).

			    Routines which allocate storage cannot, in general, use the
			    'top level' routines which could cause a non-returning
			    transfer of control.  The decXxxxOp routines are safe (do not
			    call decStatus even if traps are set in the context) and should
			    be used instead (they are also a little faster).

			 4. Exponent checking is minimized by allowing the exponent to
			    grow outside its limits during calculations, provided that
			    the decFinalize function is called later.  Multiplication and
			    division, and intermediate calculations in exponentiation,
			    require more careful checks because of the risk of 31-bit
			    overflow (the most negative valid exponent is -1999999997, for
			    a 999999999-digit number with adjusted exponent of -999999999).

			 5. Rounding is deferred until finalization of results, with any
			    'off to the right' data being represented as a single digit
			    residue (in the range -1 through 9).  This avoids any double-
			    rounding when more than one shortening takes place (for
			    example, when a result is subnormal).

			 6. The digits count is allowed to rise to a multiple of DECDPUN
			    during many operations, so whole Units are handled and exact
			    accounting of digits is not needed.  The correct digits value
			    is found by decGetDigits, which accounts for leading zeros.
			    This must be called before any rounding if the number of digits
			    is not known exactly.

			 7. The multiply-by-reciprocal 'trick' is used for partitioning
			    numbers up to four digits, using appropriate constants.  This
			    is not useful for longer numbers because overflow of 32 bits
			    would lead to 4 multiplies, which is almost as expensive as
			    a divide (unless a floating-point or 64-bit multiply is
			    assumed to be available).

			 8. Unusual abbreviations that may be used in the commentary:
				lhs -- left hand side (operand, of an operation)
				lsd -- least significant digit (of coefficient)
				lsu -- least significant Unit (of coefficient)
				msd -- most significant digit (of coefficient)
				msi -- most significant item (in an array)
				msu -- most significant Unit (of coefficient)
				rhs -- right hand side (operand, of an operation)
				+ve -- positive
				-ve -- negative
				**  -- raise to the power
			 ------------------------------------------------------------------

}

interface

uses
  Types, Windows, Classes, SysUtils, SysConst, Math, AnsiStrings;

const
  DECNUMDIGITS = 1010;

{$DEFINE TRIMING}


const

  DECUSE64 = 1;

  // Maxima and Minima for context settings
  DEC_MAX_DIGITS = 999999999;
  DEC_MIN_DIGITS = 1;
  DEC_MAX_EMAX = 999999999;
  DEC_MIN_EMAX = 0;
  DEC_MAX_EMIN = 0;
  DEC_MIN_EMIN = -999999999;
  DEC_MAX_MATH = 999999; // max emax, etc., for math funcs.

  // Bit settings for decNumber.bits
  DECNEG    = $80;      // Sign; 1=negative, 0=positive or zero
	DECINF    = $40;      // 1=Infinity
	DECNAN    = $20;      // 1=NaN
	DECSNAN   = $10;      // 1=sNaN
  // The remaining bits are reserved; they must be 0
	DECSPECIAL = (DECINF or DECNAN or DECSNAN); // any special value
  DECDPUN = 9;	      // DECimal Digits Per UNit [must be >0

  DECNUMUNITS = ((DECNUMDIGITS+DECDPUN-1) div DECDPUN);

  DEC_Overflow = $00000008;
  DEC_Clamped = $00000000;
  DEC_Rounded = $00000000;
  DEC_Subnormal = $00000000;
  DEC_Underflow = $00000004;

  DEC_Conversion_syntax = $00000001;
  DEC_Division_by_zero = $00000002;
  DEC_Division_impossible = $00000004;
  DEC_Division_undefined = $00000008; //  [when malloc fails]
  DEC_Insufficient_storage = $00000010;
  DEC_Inexact = $00000020;
  DEC_Invalid_context = $00000040;
  DEC_Invalid_operation = $00000080;

  DEC_IEEE_754_Division_by_zero = DEC_Division_by_zero;

  DEC_IEEE_754_Invalid_operation = ((((DEC_Conversion_syntax or DEC_Division_impossible) or DEC_Division_undefined) or DEC_Insufficient_storage) or DEC_Invalid_context) or DEC_Invalid_operation;
  DEC_IEEE_754_Overflow = DEC_Overflow;
  DEC_IEEE_754_Underflow = DEC_Underflow;
  // flags which are normally errors (result is qNaN, infinite, or 0)
  DEC_Errors = ((DEC_IEEE_754_Division_by_zero or DEC_IEEE_754_Invalid_operation) or DEC_IEEE_754_Overflow) or DEC_IEEE_754_Underflow;
  // flags which cause a result to become qNaN
  DEC_NaNs = DEC_IEEE_754_Invalid_operation;
  // flags which are normally for information only (finite results)

  DEC_Condition_OV = 'Overflow';
  DEC_Condition_PA = 'Clamped';
  DEC_Condition_RO = 'Rounded';
  DEC_Condition_SU = 'Subnormal';
  DEC_Condition_UN = 'Underflow';
  DEC_Condition_ZE = 'No status';
  DEC_Condition_MU = 'Multiple status';
  // length of the longest string,
  DEC_Condition_Length = 21;
  // including terminator
  // Initialization descriptors, used by decContextDefault
  DEC_INIT_BASE = 0;
  DEC_INIT_DECIMAL32 = 32;
  DEC_INIT_DECIMAL64 = 64;
  DEC_INIT_DECIMAL128 = 128;
  // Synonyms
  DEC_INIT_DECSINGLE = DEC_INIT_DECIMAL32;
  DEC_INIT_DECDOUBLE = DEC_INIT_DECIMAL64;
  DEC_INIT_DECQUAD = DEC_INIT_DECIMAL128;

  DEC_sNaN = $40000000;    // local status: sNaN signal
	BADINT: Integer = $80000000;    // most-negative Int; error indicator
	// Next two indicate an integer >= 10**6, and its parity (bottom bit)
	BIGEVEN: Integer = $80000002;
	BIGODD: Integer = $80000003;

  DECDPUNMAX = 999999999;
  DECDPUNMAXQ: Int64 = 1000000000000000000;
  DECBUFFER = 36;

type
  TRounding = (DEC_ROUND_CEILING,DEC_ROUND_UP,DEC_ROUND_HALF_UP,
    DEC_ROUND_HALF_EVEN,DEC_ROUND_HALF_DOWN,DEC_ROUND_DOWN,DEC_ROUND_FLOOR,
    DEC_ROUND_05UP,DEC_ROUND_MAX);

  TDecClass = (DEC_CLASS_SNAN,DEC_CLASS_QNAN,DEC_CLASS_NEG_INF,
    DEC_CLASS_NEG_NORMAL,DEC_CLASS_NEG_SUBNORMAL,DEC_CLASS_NEG_ZERO,
    DEC_CLASS_POS_ZERO,DEC_CLASS_POS_SUBNORMAL,DEC_CLASS_POS_NORMAL,
    DEC_CLASS_POS_INF);

  TDecContext = record
    digits: LongInt;
    emax: LongInt;
    emin: LongInt;
    round: TRounding;
    traps: LongWord;
    status: LongWord;
    clamp: Byte;
    extended: Byte;
  end;

  PDecNumber = ^TDecNumber;
  TDecNumber = record
    digits: LongInt; // Count of digits in the coefficient; >0
    exponent: LongInt; // Unadjusted exponent, unbiased, in
    bits: LongInt; // Indicator bits (see above)
    lsu: array[0..DECNUMUNITS-1] of LongInt; // Coefficient, from least significant unit
  end;

  TStrBuffer = array[0..DECNUMDIGITS+139] of AnsiChar;

  TCDecNumber = record
  private
    fDec: TDecNumber;
    fStop: Boolean;
    procedure Init;
  public
    constructor Create(const Value: TCDecNumber); overload;
    constructor Create(const Value: Extended); overload;
    constructor Create(const Value: Double); overload;
    constructor Create(const Value: Single); overload;
    constructor Create(const Value: Int64); overload;
    constructor Create(const Value: UInt64); overload;
    constructor Create(const Value: Integer); overload;
    constructor Create(const Value: Cardinal); overload;
    constructor Create(const Value: AnsiString); overload;
    // Assign functions
    procedure Assign(const Value: TCDecNumber); overload;
    procedure Assign(const Value: AnsiString); overload;
    procedure Assign(const Value: Extended); overload;
    procedure Assign(const Value: Double); overload;
    procedure Assign(const Value: Single); overload;
    procedure Assign(const Value: Int64); overload;
    procedure Assign(const Value: UInt64); overload;
    procedure Assign(const Value: Integer); overload;
    procedure Assign(const Value: Cardinal); overload;
    // Specific
    procedure Reduce;
    function IsZero: Boolean;
    function GetExponent: Integer;
    // Output functions
    function ToString: AnsiString;
    function ToText: AnsiString;
    function ToExtended: Extended;
    function ToDouble: Double;
    function ToSingle: Single;
    function ToInt64: Int64;
    function ToInteger: Integer;
    function ToCardinal: Cardinal;
    // Operators
    class operator Implicit(const Value: string): TCDecNumber; overload;
    class operator Implicit(const Value: AnsiString): TCDecNumber; overload;
    class operator Implicit(const Value: Extended): TCDecNumber; overload;
    class operator Implicit(const Value: Double): TCDecNumber; overload;
    class operator Implicit(const Value: Single): TCDecNumber; overload;
    class operator Implicit(const Value: Int64): TCDecNumber; overload;
    class operator Implicit(const Value: UInt64): TCDecNumber; overload;
    class operator Implicit(const Value: Integer): TCDecNumber; overload;
    class operator Implicit(const Value: Cardinal): TCDecNumber; overload;
    class operator Negative(const Value: TCDecNumber): TCDecNumber;
    class operator Positive(const Value: TCDecNumber): TCDecNumber;
    // Addition
    class operator Add(const Left, Right: TCDecNumber): TCDecNumber; overload;
    class operator Add(const Left: TCDecNumber; const Right: Extended): TCDecNumber; overload;
    class operator Add(const Left: TCDecNumber; const Right: Double): TCDecNumber; overload;
    class operator Add(const Left: TCDecNumber; const Right: Single): TCDecNumber; overload;
    class operator Add(const Left: TCDecNumber; const Right: Int64): TCDecNumber; overload;
    class operator Add(const Left: TCDecNumber; const Right: UInt64): TCDecNumber; overload;
    class operator Add(const Left: TCDecNumber; const Right: Integer): TCDecNumber; overload;
    class operator Add(const Left: TCDecNumber; const Right: Cardinal): TCDecNumber; overload;
    class operator Add(const Left: Extended; const Right: TCDecNumber): TCDecNumber; overload;
    class operator Add(const Left: Double; const Right: TCDecNumber): TCDecNumber; overload;
    class operator Add(const Left: Single; const Right: TCDecNumber): TCDecNumber; overload;
    class operator Add(const Left: Int64; const Right: TCDecNumber): TCDecNumber; overload;
    class operator Add(const Left: UInt64; const Right: TCDecNumber): TCDecNumber; overload;
    class operator Add(const Left: Integer; const Right: TCDecNumber): TCDecNumber; overload;
    class operator Add(const Left: Cardinal; const Right: TCDecNumber): TCDecNumber; overload;
    // Subtraction
    class operator Subtract(const Left, Right: TCDecNumber): TCDecNumber; overload;
    class operator Subtract(const Left: TCDecNumber; const Right: Extended): TCDecNumber; overload;
    class operator Subtract(const Left: TCDecNumber; const Right: Double): TCDecNumber; overload;
    class operator Subtract(const Left: TCDecNumber; const Right: Single): TCDecNumber; overload;
    class operator Subtract(const Left: TCDecNumber; const Right: Int64): TCDecNumber; overload;
    class operator Subtract(const Left: TCDecNumber; const Right: UInt64): TCDecNumber; overload;
    class operator Subtract(const Left: TCDecNumber; const Right: Integer): TCDecNumber; overload;
    class operator Subtract(const Left: TCDecNumber; const Right: Cardinal): TCDecNumber; overload;
    class operator Subtract(const Left: Extended; const Right: TCDecNumber): TCDecNumber; overload;
    class operator Subtract(const Left: Double; const Right: TCDecNumber): TCDecNumber; overload;
    class operator Subtract(const Left: Single; const Right: TCDecNumber): TCDecNumber; overload;
    class operator Subtract(const Left: Int64; const Right: TCDecNumber): TCDecNumber; overload;
    class operator Subtract(const Left: UInt64; const Right: TCDecNumber): TCDecNumber; overload;
    class operator Subtract(const Left: Integer; const Right: TCDecNumber): TCDecNumber; overload;
    class operator Subtract(const Left: Cardinal; const Right: TCDecNumber): TCDecNumber; overload;
    // Multiplication
    class operator Multiply(const Left, Right: TCDecNumber): TCDecNumber; overload;
    class operator Multiply(const Left: TCDecNumber; const Right: Extended): TCDecNumber; overload;
    class operator Multiply(const Left: TCDecNumber; const Right: Double): TCDecNumber; overload;
    class operator Multiply(const Left: TCDecNumber; const Right: Single): TCDecNumber; overload;
    class operator Multiply(const Left: TCDecNumber; const Right: Int64): TCDecNumber; overload;
    class operator Multiply(const Left: TCDecNumber; const Right: UInt64): TCDecNumber; overload;
    class operator Multiply(const Left: TCDecNumber; const Right: Integer): TCDecNumber; overload;
    class operator Multiply(const Left: TCDecNumber; const Right: Cardinal): TCDecNumber; overload;
    class operator Multiply(const Left: Extended; const Right: TCDecNumber): TCDecNumber; overload;
    class operator Multiply(const Left: Double; const Right: TCDecNumber): TCDecNumber; overload;
    class operator Multiply(const Left: Single; const Right: TCDecNumber): TCDecNumber; overload;
    class operator Multiply(const Left: Int64; const Right: TCDecNumber): TCDecNumber; overload;
    class operator Multiply(const Left: UInt64; const Right: TCDecNumber): TCDecNumber; overload;
    class operator Multiply(const Left: Integer; const Right: TCDecNumber): TCDecNumber; overload;
    class operator Multiply(const Left: Cardinal; const Right: TCDecNumber): TCDecNumber; overload;
    // Division
    class operator Divide(const Left, Right: TCDecNumber): TCDecNumber; overload;
    class operator Divide(const Left: TCDecNumber; const Right: Extended): TCDecNumber; overload;
    class operator Divide(const Left: TCDecNumber; const Right: Double): TCDecNumber; overload;
    class operator Divide(const Left: TCDecNumber; const Right: Single): TCDecNumber; overload;
    class operator Divide(const Left: TCDecNumber; const Right: Int64): TCDecNumber; overload;
    class operator Divide(const Left: TCDecNumber; const Right: UInt64): TCDecNumber; overload;
    class operator Divide(const Left: TCDecNumber; const Right: Integer): TCDecNumber; overload;
    class operator Divide(const Left: TCDecNumber; const Right: Cardinal): TCDecNumber; overload;
    class operator Divide(const Left: Extended; const Right: TCDecNumber): TCDecNumber; overload;
    class operator Divide(const Left: Double; const Right: TCDecNumber): TCDecNumber; overload;
    class operator Divide(const Left: Single; const Right: TCDecNumber): TCDecNumber; overload;
    class operator Divide(const Left: Int64; const Right: TCDecNumber): TCDecNumber; overload;
    class operator Divide(const Left: UInt64; const Right: TCDecNumber): TCDecNumber; overload;
    class operator Divide(const Left: Integer; const Right: TCDecNumber): TCDecNumber; overload;
    class operator Divide(const Left: Cardinal; const Right: TCDecNumber): TCDecNumber; overload;
    // Comparison
    // Equal
    class operator Equal(const Left, Right: TCDecNumber): Boolean; overload;
    class operator Equal(const Left: TCDecNumber; const Right: Extended): Boolean; overload;
    class operator Equal(const Left: TCDecNumber; const Right: Double): Boolean; overload;
    class operator Equal(const Left: TCDecNumber; const Right: Single): Boolean; overload;
    class operator Equal(const Left: TCDecNumber; const Right: Int64): Boolean; overload;
    class operator Equal(const Left: TCDecNumber; const Right: UInt64): Boolean; overload;
    class operator Equal(const Left: TCDecNumber; const Right: Integer): Boolean; overload;
    class operator Equal(const Left: TCDecNumber; const Right: Cardinal): Boolean; overload;
    class operator Equal(const Left: Extended; const Right: TCDecNumber): Boolean; overload;
    class operator Equal(const Left: Double; const Right: TCDecNumber): Boolean; overload;
    class operator Equal(const Left: Single; const Right: TCDecNumber): Boolean; overload;
    class operator Equal(const Left: Int64; const Right: TCDecNumber): Boolean; overload;
    class operator Equal(const Left: UInt64; const Right: TCDecNumber): Boolean; overload;
    class operator Equal(const Left: Integer; const Right: TCDecNumber): Boolean; overload;
    class operator Equal(const Left: Cardinal; const Right: TCDecNumber): Boolean; overload;
    // Not-Equal
    class operator NotEqual(const Left, Right: TCDecNumber): Boolean; overload;
    class operator NotEqual(const Left: TCDecNumber; const Right: Extended): Boolean; overload;
    class operator NotEqual(const Left: TCDecNumber; const Right: Double): Boolean; overload;
    class operator NotEqual(const Left: TCDecNumber; const Right: Single): Boolean; overload;
    class operator NotEqual(const Left: TCDecNumber; const Right: Int64): Boolean; overload;
    class operator NotEqual(const Left: TCDecNumber; const Right: UInt64): Boolean; overload;
    class operator NotEqual(const Left: TCDecNumber; const Right: Integer): Boolean; overload;
    class operator NotEqual(const Left: TCDecNumber; const Right: Cardinal): Boolean; overload;
    class operator NotEqual(const Left: Extended; const Right: TCDecNumber): Boolean; overload;
    class operator NotEqual(const Left: Double; const Right: TCDecNumber): Boolean; overload;
    class operator NotEqual(const Left: Single; const Right: TCDecNumber): Boolean; overload;
    class operator NotEqual(const Left: Int64; const Right: TCDecNumber): Boolean; overload;
    class operator NotEqual(const Left: UInt64; const Right: TCDecNumber): Boolean; overload;
    class operator NotEqual(const Left: Integer; const Right: TCDecNumber): Boolean; overload;
    class operator NotEqual(const Left: Cardinal; const Right: TCDecNumber): Boolean; overload;
    // GreaterThan
    class operator GreaterThan(const Left, Right: TCDecNumber): Boolean; overload;
    class operator GreaterThan(const Left: TCDecNumber; const Right: Extended): Boolean; overload;
    class operator GreaterThan(const Left: TCDecNumber; const Right: Double): Boolean; overload;
    class operator GreaterThan(const Left: TCDecNumber; const Right: Single): Boolean; overload;
    class operator GreaterThan(const Left: TCDecNumber; const Right: Int64): Boolean; overload;
    class operator GreaterThan(const Left: TCDecNumber; const Right: UInt64): Boolean; overload;
    class operator GreaterThan(const Left: TCDecNumber; const Right: Integer): Boolean; overload;
    class operator GreaterThan(const Left: TCDecNumber; const Right: Cardinal): Boolean; overload;
    class operator GreaterThan(const Left: Extended; const Right: TCDecNumber): Boolean; overload;
    class operator GreaterThan(const Left: Double; const Right: TCDecNumber): Boolean; overload;
    class operator GreaterThan(const Left: Single; const Right: TCDecNumber): Boolean; overload;
    class operator GreaterThan(const Left: Int64; const Right: TCDecNumber): Boolean; overload;
    class operator GreaterThan(const Left: UInt64; const Right: TCDecNumber): Boolean; overload;
    class operator GreaterThan(const Left: Integer; const Right: TCDecNumber): Boolean; overload;
    class operator GreaterThan(const Left: Cardinal; const Right: TCDecNumber): Boolean; overload;
    // GreaterThanOrEqual
    class operator GreaterThanOrEqual(const Left, Right: TCDecNumber): Boolean; overload;
    class operator GreaterThanOrEqual(const Left: TCDecNumber; const Right: Extended): Boolean; overload;
    class operator GreaterThanOrEqual(const Left: TCDecNumber; const Right: Double): Boolean; overload;
    class operator GreaterThanOrEqual(const Left: TCDecNumber; const Right: Single): Boolean; overload;
    class operator GreaterThanOrEqual(const Left: TCDecNumber; const Right: Int64): Boolean; overload;
    class operator GreaterThanOrEqual(const Left: TCDecNumber; const Right: UInt64): Boolean; overload;
    class operator GreaterThanOrEqual(const Left: TCDecNumber; const Right: Integer): Boolean; overload;
    class operator GreaterThanOrEqual(const Left: TCDecNumber; const Right: Cardinal): Boolean; overload;
    class operator GreaterThanOrEqual(const Left: Extended; const Right: TCDecNumber): Boolean; overload;
    class operator GreaterThanOrEqual(const Left: Double; const Right: TCDecNumber): Boolean; overload;
    class operator GreaterThanOrEqual(const Left: Single; const Right: TCDecNumber): Boolean; overload;
    class operator GreaterThanOrEqual(const Left: Int64; const Right: TCDecNumber): Boolean; overload;
    class operator GreaterThanOrEqual(const Left: UInt64; const Right: TCDecNumber): Boolean; overload;
    class operator GreaterThanOrEqual(const Left: Integer; const Right: TCDecNumber): Boolean; overload;
    class operator GreaterThanOrEqual(const Left: Cardinal; const Right: TCDecNumber): Boolean; overload;
    // LessThan
    class operator LessThan(const Left, Right: TCDecNumber): Boolean; overload;
    class operator LessThan(const Left: TCDecNumber; const Right: Extended): Boolean; overload;
    class operator LessThan(const Left: TCDecNumber; const Right: Double): Boolean; overload;
    class operator LessThan(const Left: TCDecNumber; const Right: Single): Boolean; overload;
    class operator LessThan(const Left: TCDecNumber; const Right: Int64): Boolean; overload;
    class operator LessThan(const Left: TCDecNumber; const Right: UInt64): Boolean; overload;
    class operator LessThan(const Left: TCDecNumber; const Right: Integer): Boolean; overload;
    class operator LessThan(const Left: TCDecNumber; const Right: Cardinal): Boolean; overload;
    class operator LessThan(const Left: Extended; const Right: TCDecNumber): Boolean; overload;
    class operator LessThan(const Left: Double; const Right: TCDecNumber): Boolean; overload;
    class operator LessThan(const Left: Single; const Right: TCDecNumber): Boolean; overload;
    class operator LessThan(const Left: Int64; const Right: TCDecNumber): Boolean; overload;
    class operator LessThan(const Left: UInt64; const Right: TCDecNumber): Boolean; overload;
    class operator LessThan(const Left: Integer; const Right: TCDecNumber): Boolean; overload;
    class operator LessThan(const Left: Cardinal; const Right: TCDecNumber): Boolean; overload;
    // LessThanOrEqual
    class operator LessThanOrEqual(const Left, Right: TCDecNumber): Boolean; overload;
    class operator LessThanOrEqual(const Left: TCDecNumber; const Right: Extended): Boolean; overload;
    class operator LessThanOrEqual(const Left: TCDecNumber; const Right: Double): Boolean; overload;
    class operator LessThanOrEqual(const Left: TCDecNumber; const Right: Single): Boolean; overload;
    class operator LessThanOrEqual(const Left: TCDecNumber; const Right: Int64): Boolean; overload;
    class operator LessThanOrEqual(const Left: TCDecNumber; const Right: UInt64): Boolean; overload;
    class operator LessThanOrEqual(const Left: TCDecNumber; const Right: Integer): Boolean; overload;
    class operator LessThanOrEqual(const Left: TCDecNumber; const Right: Cardinal): Boolean; overload;
    class operator LessThanOrEqual(const Left: Extended; const Right: TCDecNumber): Boolean; overload;
    class operator LessThanOrEqual(const Left: Double; const Right: TCDecNumber): Boolean; overload;
    class operator LessThanOrEqual(const Left: Single; const Right: TCDecNumber): Boolean; overload;
    class operator LessThanOrEqual(const Left: Int64; const Right: TCDecNumber): Boolean; overload;
    class operator LessThanOrEqual(const Left: UInt64; const Right: TCDecNumber): Boolean; overload;
    class operator LessThanOrEqual(const Left: Integer; const Right: TCDecNumber): Boolean; overload;
    class operator LessThanOrEqual(const Left: Cardinal; const Right: TCDecNumber): Boolean; overload;
  end;

  TCDecNumberComplex = record
  public
    Re: TCDecNumber;
    Im: TCDecNumber;
    constructor Create(const aRe: TCDecNumber); overload;
    constructor Create(const aRe, aIm: TCDecNumber); overload;
    constructor Create(const aRe: Extended); overload;
    constructor Create(const aRe, aIm: Extended); overload;
    constructor Create(const aRe: Double); overload;
    constructor Create(const aRe, aIm: Double); overload;
    constructor Create(const aRe: Single); overload;
    constructor Create(const aRe, aIm: Single); overload;
    constructor Create(const aRe: Int64); overload;
    constructor Create(const aRe, aIm: Int64); overload;
    constructor Create(const aRe: UInt64); overload;
    constructor Create(const aRe, aIm: UInt64); overload;
    constructor Create(const aRe: Integer); overload;
    constructor Create(const aRe, aIm: Integer); overload;
    constructor Create(const aRe: Cardinal); overload;
    constructor Create(const aRe, aIm: Cardinal); overload;
    constructor Create(const aRe: AnsiString); overload;
    constructor Create(const aRe, aIm: AnsiString); overload;
    class operator Implicit(const Value: Integer): TCDecNumberComplex;
    class operator Add(const Left, Right: TCDecNumberComplex): TCDecNumberComplex;
    class operator Subtract(const Left, Right: TCDecNumberComplex): TCDecNumberComplex;
    class operator Multiply(const Left, Right: TCDecNumberComplex): TCDecNumberComplex;
    class operator Divide(const Left, Right: TCDecNumberComplex): TCDecNumberComplex;
  end;


implementation

const
  SIGFPE = 8;       // floating point exception
  SPECIAL_CASE: Cardinal = $80000000;

  DECPOWERS: array[0..9] of Cardinal = (1, 10, 100, 1000, 10000, 100000,
    1000000, 10000000, 100000000, 1000000000);
  DECNUMMAXP = 999999999;	// maximum precision code can handle
  DECNUMMAXE = 999999999;	// maximum adjusted exponent ditto
  DECNUMMINE =-999999999; // minimum adjusted exponent ditto
  DECMAXD2U = 49;
  D2UTABLE: array[0..DECMAXD2U] of Byte = (
    0,1,1,1,1,1,1,1,1,1,2,2,2,2,2,2,2,2,2,3,3,3,
    3,3,3,3,3,3,4,4,4,4,4,4,4,4,4,5,5,5,5,5,5,5,
    5,5,6,6,6,6
  );

   DIVIDE	    = $80;	   // Divide operators
	 REMAINDER  = $40;	   // ..
	 DIVIDEINT  = $20;	   // ..
	 REMNEAR    = $10;	   // ..
   COMPARE    = $01;	   // Compare operators */
	 COMPMAX    = $02;	   // .. */
	 COMPMIN    = $03;	   // .. */
	 COMPTOTAL  = $04;	   // .. */
	 COMPNAN    = $05;	   // .. [NaN processing] */
	 COMPSIG    = $06;	   // .. [signaling COMPARE] */
	 COMPMAXMAG = $07;	   // .. */
	 COMPMINMAG = $08;	   // .. */

var
  g_set: TDecContext;

procedure ZeroDivideError;
begin
  raise EZeroDivide.Create(SDivByZero);
end;


function _raise(a: Integer): Integer;
begin
  result:=0;
end;

function SD2U(const d: Integer): Integer; inline;
begin
  result:=(d+DECDPUN-1) div DECDPUN;
end;

function D2U(const d: Integer): Integer; inline;
begin
  result:=IfThen(d<=DECMAXD2U,D2UTABLE[d],SD2U(d));
end;


// X10 and X100 -- multiply integer i by 10 or 100
// [shifts are usually faster than multiply; could be conditional]
function X10(const i: Integer): Integer; inline;
begin
  result:=(i shl 1)+(i shl 3);
end;

function X100(const i: Integer): Integer; inline;
begin
  result:=(i shl 2)+(i shl 5)+(i shl 6);
end;

function MSUDigits(const d: Integer): Integer; inline;
begin
  result:=d-(D2U(d)-1)*DECDPUN;
end;

function DecBiStr(targ, str1, str2: PAnsiChar): Boolean; inline;
begin
  result:=false;
  while targ^<>#0 do begin
    if (targ^<>str1^) and (targ^<>str2^) then
      Exit;
    Inc(targ);
    Inc(str1);
    Inc(str2);
  end;
  result:=true;
end;

function DecNumberIsZero(const dn: PDecNumber): Boolean; inline;
begin
  result:=(dn.lsu[0]=0) and (dn.digits=1) and ((dn.bits and DECSPECIAL)=0);
end;

function DecNumberIsNegative(const dn: PDecNumber): Boolean; inline;
begin
  result:=(dn.bits and DECNEG)<>0;
end;

function DecNumberIsInfinite(const dn: PDecNumber): Boolean; inline;
begin
  result:=(dn.bits and DECINF)<>0;
end;

function DecNumberIsNaN(const dn: PDecNumber): Boolean; inline;
begin
  result:=(dn.bits and (DECNAN or DECSNAN))<>0;
end;

function DecNumberIsQNaN(const dn: PDecNumber): Boolean; inline;
begin
  result:=(dn.bits and DECNAN)<>0;
end;

function DecNumberIsSNaN(const dn: PDecNumber): Boolean; inline;
begin
  result:=(dn.bits and DECSNAN)<>0;
end;

function DecUnitAddSub(a: PCardinal; alength: Integer; b: PCardinal; blength,
  bshift: Integer; c: PCardinal; m: Integer): Integer;
label
  f1,f2;
var
  alsu,clsu,minC,
  maxC,hold: PCardinal;
  carry: Int64; // carry integer (could be Long)
  add: Integer;
begin
  carry:=0;
  alsu:=a;
	clsu:=c;
  {$POINTERMATH ON}
  maxC:=c+alength;		   // A is usually the longer
	minC:=c+blength;		   // .. and B the shorter
	if bshift<>0 then begin		   // B is shifted; low As copy across
	  Inc(minC,bshift);
	  // if in place [common], skip copy unless there's a gap [rare]
	  if (a=c) and (bshift<=alength) then begin
	    Inc(c,bshift);
	    Inc(a,bshift);
	  end
	  else
      while c<clsu+bshift do begin // copy needed
        if a<alsu+alength then
          c^:=a^
	      else
          c^:=0;
	      Inc(a);
        Inc(c);
      end;
  end;
	if minC>maxC then begin // swap
	  hold:=minC;
	  minC:=maxC;
	  maxC:=hold;
	end;
  // For speed, do the addition as two loops; the first where both A
	// and B contribute, and the second (if necessary) where only one or
	// other of the numbers contribute.
	// Carry handling is the same (i.e., duplicated) in each case.
  while c<minC do begin
    Inc(carry,a^);
	  Inc(a);
  	Inc(carry,Int64(b^)*m);		// [special-casing m=1/-1
	  Inc(b);				// here is not a win]
	  // here carry is new Unit of digits; it could be +ve or -ve

	  if UInt64(carry)<=DECDPUNMAX then begin	// fastpath 0-DECDPUNMAX
      c^:=Cardinal(carry);
      carry:=0;
			goto f1;
		end;
    // remainder operator is undefined if negative, so must test
    if UInt64(carry)<(DECDPUNMAX+1)*2 then begin  // fastpath carry +1
		  c^:=Cardinal(carry-(DECDPUNMAX+1));     // [helps additions]
		  carry:=1;
		  goto f1;
		end;
    if carry>=0 then begin
		  c^:=Cardinal(carry mod (DECDPUNMAX+1));
      carry:=carry div (DECDPUNMAX+1);
      goto f1;
		end;
		// negative case
		carry:=carry+DECDPUNMAXQ; //  make positive
		c^:=Cardinal(carry mod (DECDPUNMAX+1));
		carry:=carry div (DECDPUNMAX+1)-(DECDPUNMAX+1);
    f1:
      Inc(c);
  end; // c

	// now may have one or other to complete
	// [pretest to avoid loop setup/shutdown]
	if c<maxC then
    while c<maxC do begin
	    if a<alsu+alength then begin		// still in A
	      Inc(carry,a^);
	      Inc(a);
	    end else begin				// inside B
	      Inc(carry,Int64(b^)*m);
	      Inc(b);
	    end;
	    // here carry is new Unit of digits; it could be +ve or -ve and
	    // magnitude up to DECDPUNMAX squared
	    if UInt64(carry)<=DECDPUNMAX then begin	// fastpath 0-DECDPUNMAX
				c^:=Cardinal(carry);
				carry:=0;
				goto f2;
			end;
      if UInt64(carry)<(DECDPUNMAX+1)*2 then begin // fastpath carry 1
				c^:=Cardinal(carry-(DECDPUNMAX+1));
				carry:=1;
				goto f2;
			end;
      // remainder operator is undefined if negative, so must test
      if carry>=0 then begin
				c^:=Cardinal(carry mod (DECDPUNMAX+1));
				carry:=carry div (DECDPUNMAX+1);
				goto f2;
			end;
			// negative case
			carry:=carry+DECDPUNMAXQ; // make positive
      c^:=Cardinal(carry mod (DECDPUNMAX+1));
			carry:=carry div (DECDPUNMAX+1)-(DECDPUNMAX+1);
			f2:
        Inc(c);
    end; // c

	// OK, all A and B processed; might still have carry or borrow
	// return number of Units in the result, negated if a borrow
	if carry=0 then
    exit(c-clsu);	   // no carry, so no more to do
	if carry>0 then begin		   // positive carry
    c^:=Cardinal(carry); // place as new unit
	  Inc(c);			   // ..
	  exit(c-clsu);
	end;
	// -ve carry: it's a borrow; complement needed
	add:=1;			   // temporary carry...
  c:=clsu;
	while c<maxC do begin
	  add:=DECDPUNMAX+add-c^;
	  if add<=DECDPUNMAX then begin
	    c^:=Cardinal(add);
	    add:=0;
	  end else begin
	    c^:=0;
	    add:=1;
	  end;
    Inc(c);
	end;
	// add an extra unit iff it would be non-zero
	if (add-carry-1)<>0 then begin
	  c^:=Cardinal(add-carry-1);
	  Inc(c);	// interesting, include it
	end;
	result:=clsu-c;
  {$POINTERMATH OFF}
end;

function DecUnitCompare(a: PCardinal; alength: Integer; b: PCardinal;
  blength, exp: Integer): Integer;
var
  acc,l,r,u,
  allocacc: PCardinal;
  accbuff: TCardinalDynArray; // local buffer
  accunits,need,
  expunits,exprem: Integer; // units in use or needed for acc
begin
  SetLength(accbuff,SD2U(DECBUFFER*2+1));
  allocacc:=nil;
  if exp=0 then begin 		   // aligned; fastpath
    if alength>blength then
      Exit(1);
    if alength<blength then
      Exit(-1);
    // same number of units in both -- need unit-by-unit compare
    {$POINTERMATH ON}
    l:=a+alength-1;
    r:=b+alength-1;
    while l>=a do begin
      if l^>r^ then
        Exit(1);
      if l^<r^ then
        Exit(-1);
      Dec(l);
      Dec(r);
    end;
    Exit(0); // all units match
  end; // aligned

	// Unaligned.  If one is >1 unit longer than the other, padded
	// approximately, then can return easily
  if alength>blength+D2U(exp) then
    Exit(1);
  if alength+1<blength+D2U(exp) then
    Exit(-1);

  // Need to do a real subtract.  For this, a result buffer is needed
  // even though only the sign is of interest.	Its length needs
  // to be the larger of alength and padded blength, +2
  need:=blength+D2U(exp);		// maximum real length of B
  if need<alength then
    need:=alength;
  Inc(need,2);
	acc:=@accbuff[0];				// assume use local buffer
  if need>Length(accbuff) then begin
    GetMem(allocacc,need*sizeof(Cardinal));
    if allocacc=nil then
      Exit(BADINT);	// hopeless -- abandon
    acc:=allocacc;
	end;
	// Calculate units and remainder from exponent.
	expunits:=exp div DECDPUN;
	exprem:=exp mod DECDPUN;
	// subtract [A+B*(-m)]
	accunits:=DecUnitAddSub(a, alength, b, blength, expunits, acc, -DECPOWERS[exprem]);
	// [UnitAddSub result may have leading zeros, even on zero]
	if accunits<0 then
    result:=-1		// negative result
  else begin				// non-negative result
	  // check units of the result before freeing any storage
    u:=acc;
    while (u<acc+accunits-1) and (u^=0) do
      Inc(u);
	  result:=IfThen(u^=0,0,1);
	end;
  {$POINTERMATH OFF}
	// clean up and return the result
  if allocacc<>nil then
  	FreeMem(allocacc);	// drop any storage used
end;

function DecCompare(lhs, rhs: PDecNumber; const flag: Byte): Integer;
var
  compare,sigr: Integer;
  temp: PDecNumber;
begin
	result:=1;				     // assume signum(lhs)
	if DecNumberIsZero(lhs) then
    result:=0;
	if flag<>0  then begin
	  if DecNumberIsZero(rhs) then
      exit;	     // LHS wins or both 0
	  // RHS is non-zero
	  if result=0 then
      exit(-1);		     // LHS is 0; RHS wins
	  // [here, both non-zero, result=1]
	end else begin				     // signs matter
	  if (result<>0) and DecNumberIsNegative(lhs) then
      result:=-1;
	  sigr:=1;				     // compute signum(rhs)
	  if DecNumberIsZero(rhs) then
      sigr:=0
    else if DecNumberIsNegative(rhs) then
      sigr:=-1;
	  if result>sigr then
      Exit(1);	     // L > R, return 1
	  if result<sigr then
      Exit(-1);	     // L < R, return -1
	  if result=0 then
      Exit;		       // both 0
	end;

	// signums are the same; both are non-zero
	if ((lhs.bits or rhs.bits) and DECINF)<>0 then begin   // one or more infinities
	  if DecNumberIsInfinite(rhs) then begin
	    if DecNumberIsInfinite(lhs) then
        result:=0 // both infinite
	    else
        result:=-result;		     // only rhs infinite
	  end;
	  Exit;
	end;
	// must compare the coefficients, allowing for exponents
	if lhs.exponent>rhs.exponent then begin	     // LHS exponent larger
	  // swap sides, and sign
	  temp:=lhs;
	  lhs:=rhs;
	  rhs:=temp;
	  result:=-result;
	end;
	compare:=DecUnitCompare(@lhs.lsu[0], D2U(lhs.digits), @rhs.lsu[0], D2U(rhs.digits),
			 rhs.exponent-lhs.exponent);
	if compare<>BADINT then
    compare:=compare*result;      // comparison succeeded
  result:=compare;
end;

function SpecialArg(const bits: Integer): Integer;
begin
  result:=bits and DECSPECIAL;
end;

function SpecialArgs(const lbits, rbits: Integer): Integer;
begin
  result:=((lbits or rbits) and DECSPECIAL);
end;

procedure ToDigit(var u: Cardinal; cut: Integer; c: PAnsiChar);
var
  pow: Cardinal;
begin
    //IntToStr(
  c^:='0';
  pow:=DECPOWERS[cut]*2;
  if u>pow then begin
    pow:=pow*4;
    if u>=pow then begin
      Dec(u,pow);
      Inc(c^,8);
    end;
    pow:=pow shr 1;
    if u>=pow then begin
      Dec(u,pow);
      Inc(c^,4);
    end;
    pow:=pow shr 1;
  end;
  if u>=pow then begin
    Dec(u,pow);
    Inc(c^,2);
  end;
  pow:=pow shr 1;
  if u>=pow then begin
    Dec(u,pow);
    Inc(c^);
  end;
end;

procedure DecContextSetStatus(var context: TDecContext; status: Cardinal);
begin
  context.status:=context.status or status;
  if (status>0) and (context.traps>0) then
    _raise(SIGFPE);
end;

procedure DecContextDefault(var context: TDecContext; kind: Integer);
begin
  FillChar(context,SizeOf(TDecContext),0);
  context.digits:=9;			     // 9 digits
  context.emax:=DEC_MAX_EMAX;		     // 9-digit exponents
  context.emin:=DEC_MIN_EMIN;		     // .. balanced
  context.round:=DEC_ROUND_HALF_UP;	 // 0.5 rises
  context.traps:=DEC_Errors;		     // all but informational
  case kind of
    DEC_INIT_BASE:; // use defaults
    DEC_INIT_DECIMAL32:
    begin
      context.digits:=7;		     // digits
      context.emax:=96; 		     // Emax
      context.emin:=-95;		     // Emin
      context.round:=DEC_ROUND_HALF_EVEN;    // 0.5 to nearest even
      context.traps:=0; 		     // no traps set
      context.clamp:=1; 		     // clamp exponents
    end;
    DEC_INIT_DECIMAL64:
    begin
      context.digits:=16;		     // digits
      context.emax:=384; 		     // Emax
      context.emin:=-383;		     // Emin
      context.round:=DEC_ROUND_HALF_EVEN;    // 0.5 to nearest even
      context.traps:=0; 		     // no traps set
      context.clamp:=1; 		     // clamp exponents
    end;
    DEC_INIT_DECIMAL128:
    begin
      context.digits:=34;		     // digits
      context.emax:=6144; 		     // Emax
      context.emin:=-6143;		     // Emin
      context.round:=DEC_ROUND_HALF_EVEN;    // 0.5 to nearest even
      context.traps:=0; 		     // no traps set
      context.clamp:=1; 		     // clamp exponents
    end;
    else
      DecContextSetStatus(context, DEC_Invalid_operation); // trap
  end;
end;

procedure DecNumberZero(dn: PDecNumber);
begin
  dn.bits:=0;
	dn.exponent:=0;
	dn.digits:=1;
	FillChar(dn.lsu,SizeOf(dn.lsu),0);
end;

function DecGetDigits(uar: PCardinal; len: Integer): Integer;
label
  fin;
var
  up,pow: PCardinal;
begin
  //up:=PCardinal(IntPtr(uar)+(len-1));
  {$POINTERMATH ON}
  up:=uar+(len-1);
  result:=(len-1)*DECDPUN+1;
  while up>=uar do begin
    if up^=0 then begin		    // unit is all 0s
      if result=1 then
        break;	              // a zero has one digit
      Dec(result,DECDPUN);		// adjust for 0 unit
      goto fin;
    end;
    if up^<10 then
      break;
    Inc(result);		   // is 1-9
    if up^<100 then
      break;
    Inc(result);       // is 10-99
    if up^<1000 then
      break;
    Inc(result);       // 100-999
		// count the rest ...
    pow:=PCardinal(@DECPOWERS[4]);
    while up^>=pow^ do begin
      Inc(result);
      Inc(pow);
    end;
    break;
    fin:
      Dec(up);
  end;
  {$POINTERMATH OFF}
end;

procedure DecNumberFromUInt32(dn: PDecNumber; input: Cardinal);
var
  up: PCardinal;
begin
  DecNumberZero(dn);			// clean
  if input=0 then
    Exit;		// [or decGetDigits bad call]
  up:=@dn.lsu[0];
  while input>0 do begin
    up^:=Cardinal(input mod (DECDPUNMAX+1));
    input:=input div (DECDPUNMAX+1);
    Inc(up);
  end;

  {$POINTERMATH ON}
  dn.digits:=DecGetDigits(@dn.lsu[0],up-PCardinal(@dn.lsu[0]));
  {$POINTERMATH OFF}
end;

procedure DecNumberFromInt32(dn: PDecNumber; input: Integer);
var
  unsig: Cardinal;
begin
  if input>=0 then
    unsig:=input
  else begin				// negative (possibly BADINT)
    if input=BADINT then
      unsig:=SPECIAL_CASE // special case
    else
      unsig:=-input;			// invert
	end;
  // in is now positive
  DecNumberFromUInt32(dn, unsig);
  if input<0 then
    dn.bits:=DECNEG;		// sign needed
end;

procedure DecSetCoeff(dn: PDecNumber; const _set: TDecContext;
  lsu: PCardinal; len: Integer; var residue: Integer; var status: Cardinal);
const
  resmap: array[0..9] of Byte = (0, 3, 3, 3, 3, 5, 7, 7, 7, 7);
var
  discard, count: Integer;
  cut,half,quot,rem,
  discard1: Cardinal;
  up,target: PCardinal;

begin
  discard:=len-_set.digits;    // digits to discard
  if discard<=0 then begin	      // no digits are being discarded
    if @dn.lsu[0]<>lsu then begin       // copy needed
      // copy the coefficient array to the result number; no shift needed
			count:=len;	      // avoids D2U
			up:=lsu;
      target:=@dn.lsu[0];
      while count>0 do begin
        target^:=up^;
        Inc(target);
        Inc(up);
        Dec(count,DECDPUN);
      end;
      dn.digits:=len;	      // set the new length
		end;
    // dn.exponent and residue are unchanged, record any inexactitude
    if residue<>0 then
      status:=status or (DEC_Inexact or DEC_Rounded);
		exit;
  end;

	// some digits must be discarded ...
	Inc(dn.exponent,discard);      // maintain numerical value
	status:=status or DEC_Rounded;       // accumulate Rounded status
	if residue>1 then
    residue:=1; // previous residue now to right, so reduce
  if discard>len then begin	      // everything, +1, is being discarded
    // guard digit is 0
    // residue is all the number [NB could be all 0s]
    if residue<=0 then begin		      // not already positive
      count:=len;	      // avoids D2U
      up:=lsu;
      while count>0 do begin
        if up^<>0 then begin  // found non-0
          residue:=1;
          break;
          // no need to check any others
        end;
        Inc(up);
        Dec(count,DECDPUN);
      end; // while count>0
    end;
    if residue<>0 then
      status:=status or DEC_Inexact; // record inexactitude
    FillChar(dn.lsu[0],SizeOf(dn.lsu),0); // coefficient will now be 0
    dn.digits:=1;
    exit;
  end; // total discard

	// partial discard [most common case]
	// here, at least the first (most significant) discarded digit exists

	// spin up the number, noting residue during the spin, until get to
	// the Unit with the first discarded digit.  When reach it, extract
	// it and remember its position
	count:=0;
  up:=lsu;
  while true do begin
    Inc(count,DECDPUN);
    if count>=discard then
      break; // full ones all checked
    if up^<>0 then
      residue:=1;
    Inc(up);
  end;
  // here up . Unit with first discarded digit
	cut:=discard-(count-DECDPUN)-1;
	if cut=DECDPUN-1 then begin       // unit-boundary case (fast)
	  half:=DECPOWERS[DECDPUN] shr 1;
	  // set residue directly
	  if up^>=half then begin
	    if up^>half then
        residue:=7
	    else
        Inc(residue,5);       // add sticky bit
	  end else begin // <half
	    if up^<>0 then
        residue:=3; // [else is 0, leave as sticky bit]
	  end;
	  if _set.digits<=0 then begin    // special for Quantize/Subnormal :-(
	    FillChar(dn.lsu[0],SizeOf(dn.lsu),0); // coefficient will now be 0
		  dn.digits:=1;
	  end else begin		      // shift to least
	    count:=_set.digits;      // now digits to end up with
	    dn.digits:=count;       // set the new length
	    Inc(up);		      // move to next
	    // on unit boundary, so shift-down copy loop is simple
      target:=@dn.lsu[0];
      while count>0 do begin
        target^:=up^;
        Inc(target);
        Inc(up);
        Dec(count,DECDPUN);
      end
	  end
	end // unit-boundary case
  else begin // discard digit is in low digit(s), and not top digit
    if cut=0 then
      quot:=up^	   // is at bottom of unit
    else begin // cut>0
      // it's not at bottom of unit
      rem:=up^ mod DECPOWERS[cut];
			quot:=up^ div DECPOWERS[cut];
      if rem<>0 then
        residue:=1;
		end;
    // discard digit is now at bottom of quot
    discard1:=quot mod 10;
    quot:=quot div 10;
    // here, discard1 is the guard digit, and residue is everything
    // else [use mapping array to accumulate residue safely]
    Inc(residue,resmap[discard1]);
		Inc(cut); // update cut
		// here: up . Unit of the array with bottom digit
		//	     cut is the division point for each Unit
		//	     quot holds the uncut high-order digits for the current unit
		if _set.digits<=0 then begin	   // special for Quantize/Subnormal :-(
			FillChar(dn.lsu[0],SizeOf(dn.lsu),0); // coefficient will now be 0
		  dn.digits:=1;		   // ..
		end else begin			   // shift to least needed
			count:=_set.digits;	   // now digits to end up with
			dn.digits:=count; 	   // set the new length
      // shift-copy the coefficient array to the result number
      target:=@dn.lsu[0];
      while true do begin
        target^:=quot;
        Dec(count,DECDPUN-cut);
			  if count<=0 then
          break;
			  Inc(up);
			  quot:=up^;
        rem:=quot mod DECPOWERS[cut];
			  quot:=quot div DECPOWERS[cut];
        target^:=target^+rem*DECPOWERS[DECDPUN-cut];
        Dec(count,cut);
			  if count<=0 then
          break;
        Inc(target);
      end; // shift-copy loop
		end; // shift to least
	end; // not unit boundary

  if residue<>0 then
    status:=status or DEC_Inexact; // record inexactitude
end;

procedure DecSetMaxValue(dn: PDecNumber; const _set: TDecContext);
var
  up: PCardinal;
  count: Cardinal;
begin
  count:=_set.digits;	   // nines to add
	dn.digits:=count;
	// fill in all nines to set maximum value
  up:=@dn.lsu[0];
  while true do begin
    if count>DECDPUN then
      up^:=DECDPUNMAX	// unit full o'nines
	  else begin				// this is the msu
	    up^:=DECPOWERS[count]-1;
	    break;
    end;
    Dec(count,DECDPUN);
    Inc(up);
  end;
  dn.bits:=0;			   // + sign
  dn.exponent:=_set.emax-_set.digits+1;
end;

procedure DecSetOverflow(dn: PDecNumber; const _set: TDecContext;
  var status: Cardinal);
var
  sign: Byte;
  needmax: Boolean;
  emax: Integer;
begin
  needmax:=false;		   // result is maximum finite value
	sign:=dn.bits and DECNEG;
  if DecNumberIsZero(dn) then begin	   // zero does not overflow magnitude
		emax:=_set.emax; 		     // limit value
    if _set.clamp<>0 then
      Dec(emax,_set.digits-1);     // lower if clamping
    if dn.exponent>emax then begin		     // clamp required
      dn.exponent:=emax;
			status:=status or DEC_Clamped;
		end;
		exit;
	end;

  DecNumberZero(dn);
  case _set.round of
    DEC_ROUND_DOWN,
    DEC_ROUND_05UP: needmax:=true;
    DEC_ROUND_CEILING: if sign<>0 then needmax:=true;
    DEC_ROUND_FLOOR: if sign=0 then needmax:=true;
  end;
  if needmax then begin
		DecSetMaxValue(dn,_set);
    dn.bits:=sign;		   // set sign
	end
  else
    dn.bits:=sign or DECINF;	   // Value is +/-Infinity
	status:=status or DEC_Overflow or DEC_Inexact or DEC_Rounded;
end;

function DecShiftToMost(uar: PCardinal; const digits, shift: Integer): Integer;
var
  target, source,
  first: PCardinal;
  next,rem: Cardinal;
  cut: Integer;
begin
  if shift=0 then
    Exit(digits);	   // [fastpath] nothing to do
	if (digits+shift)<=DECDPUN then begin   // [fastpath] single-unit case
	  uar^:=uar^*DECPOWERS[shift];
    Exit(digits+shift);
	end;
	next:=0;			   // all paths
  {$POINTERMATH ON}
	source:=uar+D2U(digits)-1;	   // where msu comes from
	target:=source+D2U(shift);	   // where upper part of first cut goes
	cut:=DECDPUN-MSUDigits(shift);    // where to slice
	if cut=0 then begin		   // unit-boundary case
    while source>=uar do begin
      target^:=source^;
      Dec(source);
      Dec(target);
    end;
	end else begin
	  first:=uar+D2U(digits+shift)-1; // where msu of source will end up
    while source>=uar do begin
      rem:=source^ mod DECPOWERS[cut];
      next:=next+source^ div DECPOWERS[cut];
      if target<=first then
        target^:=next;   // write to target iff valid
	    next:=rem*DECPOWERS[DECDPUN-cut];	// save remainder for next Unit
      Dec(source);
      Dec(target);
    end;
	end; // shift-move

	// propagate any partial unit to one below and clear the rest
  while target>=uar do begin
    target^:=next;
    next:=0;
    Dec(target);
  end;
  {$POINTERMATH OFF}
	result:=digits+shift;
end;

function DecShiftToLeast(uar: PCardinal; const units, shift: Integer): Integer;
var
  target, up: PCardinal;		   // work */
	cut, count: Integer;		   // work */
  quot, rem: Integer;		   // for division */
begin
  if shift=0 then
    Exit(units);	   // [fastpath] nothing to do */
	if shift=(units*DECDPUN) then begin	   // [fastpath] little to do */
	  uar^:=0;			   // all digits cleared gives zero */
	  Exit(1);			   // leaves just the one */
	end;
  {$POINTERMATH ON}
	target:=uar;			   // both paths */
	cut:=MSUDigits(shift);
	if cut=DECDPUN then begin			   // unit-boundary case; easy */
	  up:=uar+D2U(shift);
    while up<uar+units do begin
      target^:=up^;
      Inc(target);
      Inc(up);
    end;
	  Exit(target-uar);
	end;

	// messier */
	up:=uar+D2U(shift-cut);	   // source; correct to whole Units */
	count:=units*DECDPUN-shift;	   // the maximum new length */
	quot:=up^ div DECPOWERS[cut];
  while true do begin
	  target^:=Cardinal(quot);
	  Dec(count,DECDPUN-cut);
	  if count<=0 then
      break;
	  Inc(up);
	  quot:=up^;
	  rem:=quot mod DECPOWERS[cut];
	  quot:=quot div DECPOWERS[cut];
	  target^:=Cardinal(target^+rem*DECPOWERS[DECDPUN-cut]);
	  Dec(count,cut);
	  if count<=0 then
      break;
    Inc(target);
  end;
	result:=Integer(target-uar+1);
  {$POINTERMATH OFF}
end;

procedure DecTrim(dn: PDecNumber;  const _set: TDecContext; const all,
	noclamp: Boolean; var dropped: Integer);
var
	d, exp, maxd: Integer; 		   // work */
	cut: Cardinal;			   // .. */
	up: PCardinal;			   // . current Unit */
begin
  dropped:=0;				// assume no zeros dropped */
	if ((dn.bits and DECSPECIAL)<>0)		// fast exit if special .. */
  or ((dn.lsu[0] and $01)<>0) then
    Exit;	// .. or odd */
	if DecNumberIsZero(dn) then begin		// .. or 0 */
	  dn.exponent:=0;			// (sign is preserved) */
	  Exit;
	end;

	// have a finite number which is even */
	exp:=dn.exponent;
	cut:=1;			   // digit (1-DECDPUN) in Unit */
	up:=PCardinal(@dn.lsu[0]);			   // . current Unit */
  d:=0;
  while d<dn.digits-1 do begin // [don't strip the final digit]
	// slice by powers */

	  if up^ mod DECPOWERS[cut]<>0 then
      break;	     // found non-0 digit */

	  // have a trailing 0 */
	  if not all then begin 		   // trimming */
	    // [if exp>0 then all trailing 0s are significant for trim] */
	    if exp<=0 then begin 		   // if digit might be significant */
	      if exp=0 then
          break;	   // then quit */
	      Inc(exp);			   // next digit might be significant */
      end;
    end;
	  Inc(cut);			   // next power */
	  if cut>DECDPUN then begin	   // need new Unit */
	    Inc(up);
	    cut:=1;
	  end;
    Inc(d);
	end; // d */
	if d=0 then
    Exit;		   // none to drop */

	// may need to limit drop if clamping */
	if (_set.clamp<>0) and (not noclamp) then begin
	  maxd:=_set.emax-_set.digits+1-dn.exponent;
	  if maxd<=0 then
      Exit;	   // nothing possible */
	  if d>maxd then
      d:=maxd;
	end;

	// effect the drop */
	DecShiftToLeast(PCardinal(@dn.lsu[0]), D2U(dn.digits), d);
	Inc(dn.exponent,d);		   // maintain numerical value */
	Dec(dn.digits,d);		   // new length */
	dropped:=d;			   // report the count */
end;

procedure DecApplyRound(dn: PDecNumber; const _set: TDecContext;
  residue: Integer; var status: Cardinal);
var
  bump,lsd5: Integer; // 1 if coefficient needs to be incremented
  up,sup: PCardinal;
  count: Cardinal;
begin
  if residue=0 then
    exit;   // nothing to apply
  bump:=0;	// assume a smooth ride
  // now decide whether, and how, to round, depending on mode
  case _set.round of
    DEC_ROUND_05UP:
    begin
      lsd5:=dn.lsu[0] mod 5;     // get lsd and quintate
      if (residue<0) and (lsd5<>1) then
        bump:=-1
      else
      if (residue>0) and (lsd5=0) then
        bump:=1;
			// [bump==1 could be applied directly; use common path for clarity]
    end;
    DEC_ROUND_DOWN: if residue<0 then bump:=-1;
    DEC_ROUND_HALF_DOWN: if residue>5 then bump:=1;
    DEC_ROUND_HALF_EVEN:
    begin
      if residue>5 then
        bump:=1		// >0.5 goes up
      else if residue=5 then begin		// exactly 0.5000...
				// 0.5 goes up iff [new] lsd is odd
				if (dn.lsu[0] and $01)<>0 then
          bump:=1;
			end;
    end;
    DEC_ROUND_HALF_UP: if residue>=5 then bump:=1;
    DEC_ROUND_UP: if residue>0 then bump:=1;
    DEC_ROUND_CEILING:
    begin
      if DecNumberIsNegative(dn) then begin
				if residue<0 then
          bump:=-1;
      end else begin
				if residue>0 then
          bump:=1;
			end
    end;
    DEC_ROUND_FLOOR:
    begin
      if not DecNumberIsNegative(dn) then begin
				if residue<0 then
          bump:=-1;
			end else begin
				if residue>0 then
          bump:=1;
			end
    end;
    // default
    else
      status:=status or DEC_Invalid_context;
  end; // case
  if bump=0 then
    exit;	// no action required
  if bump>0 then begin
    count:=dn.digits;		     // digits to be checked
    up:=@dn.lsu[0];
    while true do begin
      if count<=DECDPUN then begin
				// this is the last Unit (the msu)
				if up^<>(DECPOWERS[count]-1) then
          break;     // not still 9s
				// here if it, too, is all nines
				up^:=DECPOWERS[count-1];	     // here 999 . 100 etc.
        {$POINTERMATH ON}
        up:=up-1;
        while up>=PCardinal(@dn.lsu[0]) do begin
          up^:=0; // others all to 0
          Dec(up);
        end;
        {$POINTERMATH OFF}
				Inc(dn.exponent); 		     // and bump exponent
				// [which, very rarely, could cause Overflow...]
				if (dn.exponent+dn.digits)>_set.emax+1 then
				  DecSetOverflow(dn, _set, status);
        exit;
      end;
      // a full unit to check, with more to come
      if up^<>DECDPUNMAX then
        break;	     // not still 9s
      Dec(count,DECDPUN);
      Inc(up);
    end;
  end else begin  //  -1
    // here checking for a pre-bump of 1000... (leading 1, all
    // other digits zero)
		count:=dn.digits;		     // digits to be checked
    up:=@dn.lsu[0];
    while true do begin
      if count<=DECDPUN then begin
				// this is the last Unit (the msu)
				if up^<>DECPOWERS[count-1] then
          break;     // not 100..
				// here if have the 1000... case
				sup:=up; 			     // save msu pointer
				up^:=DECPOWERS[count]-1;	     // here 100 in msu . 999
				// others all to all-nines, too
        {$POINTERMATH ON}
        up:=up-1;
        while up>=PCardinal(@dn.lsu[0]) do begin
        {$POINTERMATH OFF}
          up^:=DECPOWERS[DECDPUN]-1;
          Dec(up);
        end;
				Dec(dn.exponent); 		     // and bump exponent

				if dn.exponent+1=_set.emin-_set.digits+1 then begin
				  if (count=1) and (dn.digits=1) then
            sup^:=0  // here 9 . 0[.9]
          else begin
            sup^:=DECPOWERS[count-1]-1; // here 999.. in msu . 99..
            Dec(dn.digits);
					end;
				  Inc(dn.exponent);
				  status:=status or DEC_Underflow or DEC_Subnormal or DEC_Inexact or DEC_Rounded;
				end;
				exit; 			     // done
			end;

      // a full unit to check, with more to come
      if up^=0 then
        break;	     // not still 0s
      Dec(count,DECDPUN);
      Inc(up);
		end // up
  end
end;

procedure DecSetSubnormal(dn: PDecNumber; const _set: TDecContext;
  var residue: Integer; var status: Cardinal);
var
  workset: TDecContext;
  etiny, adjust: Integer;
begin
  etiny:=_set.emin-(_set.digits-1);	// smallest allowed exponent
  if DecNumberIsZero(dn) then begin			// value is zero
    if dn.exponent<etiny then begin		// clamp required
			dn.exponent:=etiny;
			status:=status or DEC_Clamped;
		end;
		exit;
	end;
  status:=status or DEC_Subnormal;		// have a non-zero subnormal
	adjust:=etiny-dn.exponent;		// calculate digits to remove
  if adjust<=0 then begin		// not out of range; unrounded
		// residue can never be non-zero here, except in the Nmin-residue
		// case (which is a subnormal result), so can take fast-path here
		// it may already be inexact (from setting the coefficient)
		if (status and DEC_Inexact)<>0 then
      status:=status or DEC_Underflow;
		exit;
  end;

	// adjust>0, so need to rescale the result so exponent becomes Etiny
	// [this code is similar to that in rescale]
	workset:=_set; 			// clone rounding, etc.
	workset.digits:=dn.digits-adjust;	// set requested length
	Dec(workset.emin,adjust); 		// and adjust emin to match
	// [note that the latter can be <1, here, similar to Rescale case]
	DecSetCoeff(dn, workset, @dn.lsu, dn.digits, residue, status);
	DecApplyRound(dn, workset, residue, status);

	// Use 754 default rule: Underflow is set iff Inexact
	// [independent of whether trapped]
	if (status and DEC_Inexact)<>0 then
    status:=status or DEC_Underflow;

	// if rounded up a 999s case, exponent will be off by one; adjust
	// back if so [it will fit, because it was shortened earlier]
	if dn.exponent>etiny then begin
	  dn.digits:=DecShiftToMost(@dn.lsu[0], dn.digits, 1);
	  Dec(dn.exponent);			// (re)adjust the exponent.
	end;

	// if rounded to zero, it is by definition clamped...
	if DecNumberIsZero(dn) then
    status:=status or DEC_Clamped;
end;

procedure DecFinish(dn: PDecNumber; const _set: TDecContext;
  var residue: Integer; var status: Cardinal);
var
  shift,tinyexp,
  comp: Integer;
  nmin: TDecNumber;
begin
  tinyexp:=_set.emin-dn.digits+1;
  if dn.exponent<=tinyexp then begin		// prefilter
		// A very nasty case here is dn == Nmin and residue<0
		if dn.exponent<tinyexp then begin
		  // Go handle subnormals; this will apply round if needed.
		  decSetSubnormal(dn, _set, residue, status);
		  exit;
		end;
		// Equals case: only subnormal if dn=Nmin and negative residue
		DecNumberZero(@nmin);
		nmin.lsu[0]:=1;
		nmin.exponent:=_set.emin;
		comp:=DecCompare(dn, @nmin, 1);		  // (signless compare)
		if comp=BADINT then begin			  // oops
		  status:=status or DEC_Insufficient_storage;	  // abandon...
		  exit;
		end;
		if (residue<0) and (comp=0) then begin	  // neg residue and dn==Nmin
		  DecApplyRound(dn, _set, residue, status);   // might force down
		  decSetSubnormal(dn, _set, residue, status);
		  exit;
		end;
	end;

	// now apply any pending round (this could raise overflow).
	if residue<>0 then
    decApplyRound(dn, _set, residue, status);
	// Check for overflow [redundant in the 'rare' case] or clamp
	if dn.exponent<=_set.emax-_set.digits+1 then
    exit;   // neither needed
	// here when might have an overflow or clamp to do
	if dn.exponent>_set.emax-dn.digits+1 then begin      // too big
	  decSetOverflow(dn, _set, status);
	  exit;
	end;
	// here when the result is normal but in clamp range
	if _set.clamp=0 then
    exit;

	// here when need to apply the IEEE exponent clamp (fold-down)
	shift:=dn.exponent-(_set.emax-_set.digits+1);
	// shift coefficient (if non-zero)
	if not DecNumberIsZero(dn) then
	  dn.digits:=DecShiftToMost(@dn.lsu[0], dn.digits, shift);

	Dec(dn.exponent,shift);	 // adjust the exponent to match
	status:=status or DEC_Clamped;  // and record the dirty deed
end;

procedure DecStatus(dn: PDecNumber; status: Cardinal; var _set: TDecContext);
begin
  if (status and DEC_NaNs)<>0 then begin	// error status . NaN
		//if cause was an sNaN, clear and propagate [NaN is already set up]
    if (status and DEC_sNaN)<>0 then
      status:=status and not DEC_sNaN
    else begin
			DecNumberZero(dn);		// other error: clean throughout
      dn.bits:=DECNAN;			// and make a quiet NaN
		end;
	end;
	DecContextSetStatus(_set, status);	// [may not return]
end;

procedure DecNumberFromString(dn: PDecNumber; input: AnsiString;
  var _set: TDecContext);
label
  f1,f2,f3;
var
  resbuff: TCardinalDynArray;
  c,f,cfirst,last,firstexp,
  dotchar,chars: PAnsiChar;
  res,allocres,up: PCardinal;
  bits: Byte;
  nege: Boolean;
  d,residue,cut,exponent,
  _out,need: Integer;
  status: Cardinal;
begin
  d:=0;
  c:=nil;
  f:=nil;
  last:=nil;
  dotchar:=nil;
  firstexp:=nil;
  need:=0;
  bits:=0;
  status:=0;
  exponent:=0;
  chars:=PAnsiChar(input);
  cfirst:=chars;
  res:=nil;
  allocres:=nil;
  SetLength(resbuff,SD2U(DECBUFFER+9));
  {$POINTERMATH ON}
  repeat
    // status & malloc protection
    c:=chars; // . input character
    while (c^<>#0) do begin
      if c^ in ['0'..'9'] then begin    // test for Arabic digit
				last:=c;
				Inc(d);			   // count of real digits
				goto f1;		   // still in decimal part
			end;
      if (c^='.') and (dotchar=nil) then begin   // first '.'
				dotchar:=c;		   // record offset into decimal part
				if c=cfirst then
          Inc(cfirst);   // first digit must follow
        goto f1;
      end;
      if c=chars then begin	   // first in string...
				if c^='-' then begin		   // valid - sign
				  Inc(cfirst);
				  bits:=DECNEG;
          goto f1;
        end;
				if c^ in ['+',' '] then begin		   // valid + sign or space
				  Inc(cfirst);
          goto f1;
        end;
			end;
      // c^ is not a digit, or a valid +, -, or '.'
      break;

      f1:
        Inc(c);
    end;

    if last=nil then begin		   // no digits yet
      if c^=#0 then
        break;
      status:=DEC_Conversion_syntax; // assume the worst
      // Infinities and NaNs are possible, here
      if dotchar<>nil then
        break;    // .. unless had a dot
			DecNumberZero(dn);	   // be optimistic
      if DecBiStr(c, 'infinity', 'INFINITY')
			or DecBiStr(c,'inf','INF') then begin
				dn.bits:=bits or DECINF;
				status:=0;		   // is OK
				break; // all done
		  end;
      // a NaN expected
			// 2003.09.10 NaNs are now permitted to have a sign
      dn.bits:=bits or DECNAN;	   // assume simple NaN
      if (c^='s') or (c^='S') then begin  // looks like an sNaN
				Inc(c);
				dn.bits:=bits or DECSNAN;
			end;
      if (c^<>'n') and (c^<>'N') then
        break;	// check caseless "NaN"
			Inc(c);
      if (c^<>'a') and (c^<>'A') then
        break;
			Inc(c);
      if (c^<>'n') and (c^<>'N') then
        break;
			Inc(c);
      // now either nothing, or nnnn payload, expected
      // . start of integer and skip leading 0s [including plain 0]
      cfirst:=c;
      while cfirst^='0' do
        inc(cfirst);
      if cfirst^=#0 then begin	   // "NaN" or "sNaN", maybe with all 0s
				status:=0;		   // it's good
				break;
			end;
      // something other than 0s; setup last and d as usual [no dots]
      cfirst:=c;
      while true do begin
        if (c^<'0') or (c^>'9') then
          break; // test for Arabic digit
				last:=c;
        Inc(c);
        Inc(d);
      end;
      if c^<>#0 then
        break;	   // not all digits
      if d>_set.digits-1 then begin
        // [NB: payload in a decNumber can be full length unless
				// clamped, in which case can only be digits-1]
				if (_set.clamp>0) or (d>_set.digits) then
          break;
      end;  // too many digits?
      // good; drop through to convert the integer to coefficient
      status:=0; 		   // syntax is OK
      bits:=dn.bits;	// for copy-back
    end // last=nil
    else
    if c^<>#0 then begin	   // more to process...
      // had some digits; exponent is only valid sequence now
      status:=DEC_Conversion_syntax; // assume the worst
      if (c^<>'e') and (c^<>'E') then
        break;
      // Found 'e' or 'E' -- now process Implicit exponent
      // sign no longer required
      nege:=false;
			Inc(c);			   // to (possible) sign
      if c^='-' then begin
        nege:=true;
        Inc(c);
      end
			else
      if c^='+' then
        Inc(c);
      if c^=#0 then
        break;
      while (c^='0') and ((c+1)^<>#0) do
        Inc(c);  // strip insignificant zeros
      firstexp:=c; // save exponent digit place
      while c^ in ['0'..'9'] do begin
        exponent:=X10(exponent)+Ord(c^)-Ord('0');
        Inc(c);
      end;
      // if not now on a #0, c^ must not be a digit
      if c^<>#0 then
        break;
      f:=firstexp+9+1;
      if c>=f then begin
				if (c>f) or (firstexp^>'1') then
          exponent:=DECNUMMAXE*2;
				// [up to 1999999999 is OK, for example 1E-1000000998]
			end;
      if nege then
        exponent:=-exponent;	// was negative
      status:=0; 			// is OK

    end; //  stuff after digits
    // Here when whole string has been inspected; syntax is good
		// cfirst.first digit (never dot), last.last digit (ditto)

  	// strip leading zeros/dot [leave final 0 if all 0's]
    if cfirst^='0' then begin  // [cfirst has stepped over .]
      c:=cfirst;
      while c<last do begin
        if c^='.' then
          goto f2; // ignore dots
        if c^<>'0' then
          break;		// non-zero found
        Dec(d); // 0 stripped
        f2:
          Inc(c);
          Inc(cfirst);
      end;
		end; // at least one leading 0
    // Handle decimal point...
    if (dotchar<>nil) and (dotchar<last) then	// non-trailing '.' found?
      exponent:=exponent-(last-dotchar); 	// adjust exponent
		// [we can now ignore the .]

    // OK, the digits string is good.  Assemble in the decNumber, or in
		// a temporary units array if rounding is needed
    if d<=_set.digits then
      res:=@dn.lsu[0]	// fits into supplied decNumber
    else begin				// rounding needed
      need:=D2U(d); // bytes needed
      res:=@resbuff[0];			// assume use local buffer
      if need>Length(resbuff) then begin  // too big for local
				allocres:=AllocMem(need*SizeOf(Cardinal));
				if allocres=nil then begin
          status:=status or DEC_Insufficient_storage;
          break;
        end;
				res:=allocres;
      end;
		end;
    // res now . number lsu, buffer, or allocated storage for Unit array

    // Place the coefficient into the selected Unit array
    // [this is often 70% of the cost of this function when DECDPUN>1]
    _out:=0;			   // accumulator
    up:=res+D2U(d)-1;		   // . msu
    cut:=d-(up-res)*DECDPUN;	 // digits in top unit
    c:=cfirst; // along the digits
    while c^<>#0 do begin
      if c^='.' then
        goto f3; // ignore '.' [don't decrement cut]
      _out:=X10(_out)+Ord(c^)-Ord('0');
      if c=last then
        break;	   // done [never get to trailing '.']
      Dec(cut);
      if cut>0 then
        goto f3; // more for this unit
      up^:=Cardinal(_out);		   // write unit
      Dec(up); // prepare for unit below..
      cut:=DECDPUN;
      _out:=0;
      f3:
        Inc(c);
    end;
    up^:=Cardinal(_out);	 // write lsu

    dn.bits:=bits;
    dn.exponent:=exponent;
    dn.digits:=d;

    // if not in number (too long) shorten into the number
    if d>_set.digits then begin
			residue:=0;
			DecSetCoeff(dn, _set, res, d, residue, status);
			/// always check for overflow or subnormal and round as needed
			DecFinish(dn, _set, residue, status);
		end else begin // no rounding, but may still have overflow or subnormal
      // [these tests are just for performance; finalize repeats them]
      if ((dn.exponent-1<_set.emin-dn.digits)
				or (dn.exponent-1>_set.emax-_set.digits)) then begin
				residue:=0;
				DecFinish(dn, _set, residue, status);
			end;
    end;
  until true;
  if allocres<>nil then
    FreeMem(allocres);	// drop any storage used
  if status<>0 then
    DecStatus(dn, status, _set);
  {$POINTERMATH OFF}
end;

function DecNumberToInt32(const dn: PDecNumber; var _set: TDecContext): Integer;
var
  d, i: Integer;			   // work */
	up: PCardinal;		   // .. */
	hi, lo: Cardinal;		   // .. */
begin
  result:=0;
  hi:=0;
  // special or too many digits, or bad exponent */
  if ((dn.bits and DECSPECIAL)<>0) or (dn.digits>10) or (dn.exponent<>0) then // bad */
  else begin // is a finite integer with 10 or fewer digits */
		up:=PCardinal(@dn.lsu[0]); 		   // . lsu */
		lo:=up^;			   // get 1 to 9 digits */
		// split to higher */
		hi:=lo div 10;
		lo:=lo mod 10;
		Inc(up);
    // collect remaining Units, if any, into hi */
    d:=DECDPUN;
    while d<dn.digits do begin
      Inc(hi,up^*DECPOWERS[d-1]);
      Inc(up);
      Inc(d,DECDPUN);
    end;
		// now low has the lsd, hi the remainder */
		if (hi>214748364) or ((hi=214748364) and (lo>7)) then begin // out of range? */
		  // most-negative is a reprieve */
		  if ((dn.bits and DECNEG)<>0) and (hi=214748364) and (lo=8) then
        Exit($80000000);
		  // bad -- drop through */
    end else begin // in-range always */
		  i:=X10(hi)+lo;
		  if (dn.bits and DECNEG)<>0 then
        Exit(-i);
		  Exit(i);
    end;
  end; // integer */
  DecContextSetStatus(_set, DEC_Invalid_operation); // [may not return] */
end;

procedure DecNumberToString(const dn: PDecNumber; const str: PAnsiChar);
const
  eng = false;
var
  c: PAnsiChar; // work [output pointer]
  e,exp,// local copy
  pre, // digits before the '.'
  cut, //for counting digits in a Unit
  adj,n: Integer;
  u: Cardinal;
  up: PCardinal; // . msu [input pointer]
  had: Boolean;
begin
  c:=str;
  exp:=dn.exponent;
  {$POINTERMATH ON}
  up:=PCardinal(@dn.lsu[0])+D2U(dn.digits)-1;
  if DecNumberIsNegative(dn) then begin   // Negatives get a minus
	  c^:='-';
	  Inc(c);
	end;
	if (dn.bits and DECSPECIAL)<>0 then begin 	   // Is a special value
	  if DecNumberIsInfinite(dn) then begin
      AnsiStrings.StrPCopy(c,'Inf');
      exit;
    end;
    // a NaN
	  if (dn.bits and DECSNAN)<>0 then begin 	   // signalling NaN
	    c^:='s';
	    Inc(c);
	  end;
	  AnsiStrings.StrPCopy(c,'NaN');
	  Inc(c,3);			   // step past
	  // if not a clean non-zero coefficient, that's all there is in a
	  // NaN string
	  if (exp<>0) or ((dn.lsu[0]=0) and (dn.digits=1)) then
      exit;
	    // [drop through to add integer]
	end;

  // calculate how many digits in msu, and hence first cut
	cut:=MSUDigits(dn.digits);	   // [faster than remainder]
	Dec(cut);			   // power of ten for digit

	if exp=0 then begin  		   // simple integer [common fastpath]
    while up>=PCardinal(@dn.lsu[0]) do begin // each Unit from msu
	    u:=up^;			   // contains DECDPUN digits to lay out
      while cut>=0 do begin
        ToDigit(u, cut, c);
        Inc(c);
        Dec(cut);
      end;
	    cut:=DECDPUN-1;		   // next Unit has all digits
      Dec(up);
	  end;
	  c^:=#0;			   // terminate the string
	  exit;
  end;

	// non-0 exponent -- assume plain form
	pre:=dn.digits+exp;		   // digits before '.'
	e:=0;				   // no E
	if (exp>0) or (pre<-5) then begin   // need exponential form
	  e:=exp+dn.digits-1; 	   // calculate E value
	  pre:=1;			   // assume one digit before '.'
	  if eng and (e<>0) then begin	   // engineering: may need to adjust
	  // The C remainder operator is undefined for negative numbers, so
	  // a positive remainder calculation must be used here
	    if e<0 then begin
	      adj:=(-e) mod 3;
	      if adj<>0 then
          adj:=3-adj;
	    end else begin // e>0
	      adj:=e mod 3;
	    end;
      e:=e-adj;
			// if dealing with zero still produce an exponent which is a
			// multiple of three, as expected, but there will only be the
			// one zero before the E, still.	Otherwise note the padding.
      if not DecNumberIsZero(dn) then
        Inc(pre,adj)
      else begin  // is zero
				if (adj<>0) then begin		   // 0.00Esnn needed
				  e:=e+3;
				  Dec(pre,(2-adj));
				end;
			end; // zero
		end; // eng
	end; // need exponent

	// lay out the digits of the coefficient, adding 0s and . as needed
	u:=up^;
	if pre>0 then begin			   // xxx.xxx or xx00 (engineering) form
	  n:=pre;
    while pre>0 do begin
      if cut<0 then begin 		   // need new Unit
	      if up=@dn.lsu[0] then
          break;    // out of input digits (pre>digits)
	      Dec(up);
	      cut:=DECDPUN-1;
	      u:=up^;
      end;
      ToDigit(u, cut, c);
      Dec(pre);
      Inc(c);
      Dec(cut);
    end; // while

    if n<dn.digits then begin  	   // more to come, after '.'
      c^:='.';
      Inc(c);
      while true do begin
				if cut<0 then begin 		   // need new Unit
				  if up=@dn.lsu[0] then
            break;   // out of input digits
				  Dec(up);
	        cut:=DECDPUN-1;
	        u:=up^;
				end;
				ToDigit(u, cut, c);
        Inc(c);
        Dec(cut);
			end; // while
		end
		else
      while pre>0 do begin
        c^:='0'; // 0 padding (for engineering) needed
        Dec(pre);
        Inc(c);
      end;
  end else begin			   // 0.xxx or 0.000xxx form
		c^:='0'; Inc(c);
		c^:='.'; Inc(c);
    while pre<0 do begin
      c^:='0'; // add any 0's after '.'
      Inc(pre);
      Inc(c);
    end;
    while true do begin
		//for (; ; c++, cut--)
		  if cut<0 then begin		   // need new Unit
		    if up=@dn.lsu[0] then
          break;  // out of input digits
        Dec(up);
        cut:=DECDPUN-1;
        u:=up^;
      end;
      ToDigit(u, cut, c);
      Inc(c);
      Dec(cut);
		end;
  end;

	{ Finally add the E-part, if needed.  It will never be 0, has a
		 base maximum and minimum of +999999999 through -999999999, but
		 could range down to -1999999998 for anormal numbers }
  if e<>0 then begin
		had:=false; 	      // 1=had non-zero
		c^:='E'; Inc(c);
		c^:='+'; Inc(c);	      // assume positive
		u:=e;		      // ..
		if e<0 then begin
		  (c-1)^:='-';	      // oops, need -
		  u:=-e;		      // uInt, please
    end;
		// lay out the exponent [_itoa or equivalent is not ANSI C]
		for cut:=9 downto 0 do begin
		  ToDigit(u, cut, c);
		  if (c^='0') and (not had) then
        continue;	// skip leading zeros
		  had:=true;				// had non-0
		  Inc(c);				// step for next
		end; // cut
  end;
  {$POINTERMATH OFF}
	c^:=#0;	    // terminate the string (all paths)
end;

procedure DecNumberCopy(dest: PDecNumber; const src: PDecNumber);
var
  d, s, smsup: PCardinal;
begin
  if @src=@dest then begin
    dest:=src;
    exit;
  end;
  // Use Implicit assignments here as structure assignment could copy
	// more than just the lsu (for small DECDPUN).  This would not affect
	// the value of the results, but could disturb test harness spill
	// checking.
	dest.bits:=src.bits;
	dest.exponent:=src.exponent;
	dest.digits:=src.digits;
	dest.lsu[0]:=src.lsu[0];
	if (src.digits>DECDPUN) then begin		     // more Units to come

	// memcpy for the remaining Units would be safe as they cannot
	// overlap.  However, this Implicit loop is faster in short cases.
    s:=@src.lsu[1];
	  d:=@dest.lsu[1];			     // . first destination
	  smsup:=@src.lsu[D2U(src.digits)];	     // . source msu+1
    {$POINTERMATH ON}
    while s<smsup do begin
    {$POINTERMATH OFF}
      d^:=s^;
      Inc(s);
      Inc(d);
    end;
  end;
end;

procedure DecCopyFit(dest: PDecNumber; const src: PDecNumber;
  const _set: TDecContext; var residue: Integer; var status: Cardinal);
begin
	dest.bits:=src.bits;
	dest.exponent:=src.exponent;
	DecSetCoeff(dest, _set, PCardinal(@src.lsu[0]), src.digits, residue, status);
end;

procedure DecDecap(dn: PDecNumber; drop: Integer);
var
  msu: PCardinal; // . target cut point
	cut: Integer;				// work
begin
  if drop>=dn.digits then begin		// losing the whole thing
		dn.lsu[0]:=0;
		dn.digits:=1;
		Exit;
	end;
  {$POINTERMATH ON}
  msu:=PCardinal(@dn.lsu[0])+D2U(dn.digits-drop)-1;	// . likely msu
  cut:=MSUDigits(dn.digits-drop);	// digits to be in use in msu
  if cut<>DECDPUN then
    msu^:=msu^ mod DECPOWERS[cut];	// clear left digits
  // that may have left leading zero digits, so do a proper count...
  dn.digits:=DecGetDigits(@dn.lsu[0],msu-PCardinal(@dn.lsu[0])+1);
  {$POINTERMATH OFF}
end;

procedure DecNaNs(res, lhs, rhs: PDecNumber;
  const _set: TDecContext; var status: Cardinal);
var
  ul, ur, uresp1: PCardinal;
begin
  // This decision tree ends up with LHS being the source pointer,
	// and status updated if need be
	if (lhs.bits and DECSNAN)<>0 then
	  status:=status or DEC_Invalid_operation or DEC_sNaN
  else
  if rhs=nil then //nothing
  else
  if (rhs.bits and DECSNAN)<>0 then begin
	  lhs:=rhs;
	  status:=status or DEC_Invalid_operation or DEC_sNaN;
	end
  else
  if (lhs.bits and DECNAN)<>0 then //nothing
  else
    lhs:=rhs;

	// propagate the payload
	if lhs.digits<=_set.digits then
    DecNumberCopy(res, lhs) // easy
  else begin
    // too long
  	// copy safe number of units, then decapitate
	  res.bits:=lhs.bits;		// need sign etc.
    {$POINTERMATH ON}
	  uresp1:=PCardinal(@res.lsu[0])+D2U(_set.digits);
    ur:=@res.lsu[0];
    ul:=@lhs.lsu[0];
    while ur<uresp1 do begin
      ur^:=ul^;
      Inc(ur);
      Inc(ul);
    end;
    {$POINTERMATH OFF}
	  res.digits:=D2U(_set.digits)*DECDPUN;
	  // maybe still too long
	  if res.digits>_set.digits then
    DecDecap(res, res.digits-_set.digits);
	end;

	res.bits:=res.bits and not DECSNAN;	      // convert any sNaN to NaN, while
	res.bits:=res.bits or DECNAN;	      // .. preserving sign
	res.exponent:=0;	      // clean exponent
  // [coefficient was copied/decapitated]
end;


procedure DecAddOp(res, lhs, rhs: PDecNumber;
  const _set: TDecContext; const negate: Byte; var status: Cardinal);
type
  TLocalBuffer = array[0..(((DECBUFFER*2+20)+DECDPUN-1) div DECDPUN)-1] of Cardinal;  // local buffer [*2+20 reduces many
var
  rhsshift: Integer;		   // working shift (in Units)
  maxdigits: Integer;		   // longest logical length
  mult: Integer;			   // multiplier
  residue: Integer;		   // rounding accumulator
	bits: Byte;			   // result bits
	diffsign: Boolean;		   // non-0 if arguments have different sign
	acc: PCardinal;			   // accumulator for result
  accbuff: TLocalBuffer; // allocations when called from other operations, notable exp]
	allocacc: PCardinal; 	   // . allocated acc buffer, iff allocated
	reqdigits: Integer;	   // local copy; requested DIGITS
	padding: Integer;		   // work
  adjust: Integer;       // work
  lexp: Integer;		// save in case LHS==RES
  rexp: Integer;		// save in case RHS==RES
  partial: Integer;
  swapped: Boolean;
  t: PDecNumber;  // temporary
  shift: Integer; // left shift needed
  need: Integer;
  dropped: Integer;
begin
  allocacc:=nil;
  dropped:=0;
  FillChar(accbuff,SizeOf(accbuff),0);
  reqdigits:=_set.digits;
  repeat // protect allocated storage
    diffsign:=((lhs.bits xor rhs.bits xor negate) and DECNEG)<>0;
    // handle infinities and NaNs
    if SpecialArgs(lhs.bits,rhs.bits)<>0 then begin 		// a special bit set
      if (SpecialArgs(lhs.bits,rhs.bits) and (DECSNAN or DECNAN))<>0 then  // a NaN
				DecNaNs(res, lhs, rhs, _set, status)
      else begin // one or two infinities
				if DecNumberIsInfinite(lhs) then begin // LHS is infinity
				  // two infinities with different signs is invalid
				  if DecNumberIsInfinite(rhs) and diffsign then begin
					  status:=status or DEC_Invalid_operation;
					  break;
					end;
        bits:=lhs.bits and DECNEG;	// get sign from LHS
				end
        else
          bits:=(rhs.bits xor negate) and DECNEG; // RHS must be Infinity
				bits:=bits or DECINF;
				DecNumberZero(res);
				res.bits:=bits; 		// set +/- infinity
      end; // an infinity
      break;
    end;
    // Quick exit for add 0s; return the non-0, modified as need be
    if DecNumberIsZero(lhs) then begin
		  bits:=lhs.bits;			// ..
      residue:=0;			// clear accumulator
			DecCopyFit(res, rhs, _set, residue, status); // copy (as needed)
      res.bits:=res.bits xor negate;		// flip if rhs was negated
      // exponent will be the lower of the two
      adjust:=lexp-res.exponent;	// adjustment needed [if -ve]
      if DecNumberIsZero(res) then begin // both 0: special IEEE 754 rules
        if adjust<0 then
          res.exponent:=lexp;  // set exponent
        // 0-0 gives +0 unless rounding to -infinity, and -0-0 gives -0
				if diffsign then begin
				  if _set.round<>DEC_ROUND_FLOOR then
            res.bits:=0
				  else
            res.bits:=DECNEG;	// preserve 0 sign
				end;
      end
      else begin // non-0 res
			  if adjust<0 then begin    // 0-padding needed
				  if (res.digits-adjust)>_set.digits then begin
				    adjust:=res.digits-_set.digits;	  // to fit exactly
				    status:=status or DEC_Rounded;		  // [but exact]
				  end;
				  res.digits:=DecShiftToMost(PCardinal(@res.lsu[0]), res.digits, -adjust);
				  Inc(res.exponent,adjust);		  // set the exponent.
				end;
			end; // non-0 res
      DecFinish(res, _set, residue, status);	  // clean and finalize
      break;
    end;

    if DecNumberIsZero(rhs) then begin			// [lhs is non-zero]
			rexp:=rhs.exponent;		// save in case RHS==RES
      bits:=rhs.bits;			// be clean
      residue:=0;			// clear accumulator
			DecCopyFit(res, lhs, _set, residue, status); // copy (as needed)
      // exponent will be the lower of the two
			// [0-0 case handled above]
			adjust:=rexp-res.exponent;	// adjustment needed [if -ve]
			if adjust<0 then begin     // 0-padding needed
			  if (res.digits-adjust)>_set.digits then begin
				  adjust:=res.digits-_set.digits;	// to fit exactly
				  status:=status or DEC_Rounded;		// [but exact]
				end;
			  res.digits:=DecShiftToMost(PCardinal(@res.lsu[0]), res.digits, -adjust);
			  Inc(res.exponent,adjust);		// set the exponent.
      end;
      DecFinish(res, _set, residue, status);	  // clean and finalize
      break;
    end;

    // [NB: both fastpath and mainpath code below assume these cases
    // (notably 0-0) have already been handled]

		// calculate the padding needed to align the operands
		padding:=rhs.exponent-lhs.exponent;

    // Fastpath cases where the numbers are aligned and normal, the RHS
    // is all in one unit, no operand rounding is needed, and no carry,
    // lengthening, or borrow is needed
    if (padding=0)
		and (rhs.digits<=DECDPUN)
		and (rhs.exponent>=_set.emin)	// [some normals drop through]
		and (rhs.exponent<=_set.emax-_set.digits+1) // [could clamp]
		and (rhs.digits<=reqdigits)
		and (lhs.digits<=reqdigits) then begin

			partial:=lhs.lsu[0];
      if not diffsign then begin			// adding
        Inc(partial,rhs.lsu[0]);
			  if (partial<=DECDPUNMAX)	// result fits in unit
			  and ((lhs.digits>=DECDPUN)  // .. and no digits-count change
        or (partial<DECPOWERS[lhs.digits])) then begin // ..
			    if res<>lhs then
            decNumberCopy(res, lhs);  // not in place
			    res.lsu[0]:=Cardinal(partial);	// [copy could have overwritten RHS]
			    break;
			  end;
			// else drop out for careful add
			end else begin				// signs differ
        Dec(partial,rhs.lsu[0]);
			  if partial>0 then begin // no borrow needed, and non-0 result
          if res<>lhs then
            decNumberCopy(res, lhs);  // not in place
          res.lsu[0]:=Cardinal(partial);
			    // this could have reduced digits [but result>0]
			    res.digits:=DecGetDigits(PCardinal(@res.lsu[0]), D2U(res.digits));
			    break;
			  end;
			  // else drop out for careful subtract
			end;
    end;

    // Now align (pad) the lhs or rhs so they can be added or
		// subtracted, as necessary.  If one number is much larger than
		// the other (that is, if in plain form there is a least one
		// digit between the lowest digit of one and the highest of the
		// other) padding with up to DIGITS-1 trailing zeros may be
		// needed; then apply rounding (as exotic rounding modes may be
		// affected by the residue).
		rhsshift:=0; 	      // rhs shift to left (padding) in Units
		bits:=lhs.bits;	      // assume sign is that of LHS
		mult:=1;		      // likely multiplier

		// [if padding==0 the operands are aligned; no padding is needed]
		if padding<>0 then begin
		  // some padding needed; always pad the RHS, as any required
		  // padding can then be effected by a simple combination of
		  // shifts and a multiply
		  swapped:=false;
		  if padding<0 then begin		// LHS needs the padding
		    padding:=-padding;		// will be +ve
		    bits:=Byte(rhs.bits xor negate); // assumed sign is now that of RHS
		    t:=lhs;
        lhs:=rhs;
        rhs:=t;
		    swapped:=true;
		  end;
      // If, after pad, rhs would be longer than lhs by digits+1 or
		  // more then lhs cannot affect the answer, except as a residue,
		  // so only need to pad up to a length of DIGITS+1.
			if rhs.digits+padding>lhs.digits+reqdigits+1 then begin
			  // The RHS is sufficient
			  // for residue use the relative sign indication...
			  shift:=reqdigits-rhs.digits;     // left shift needed
			  residue:=1;			     // residue for rounding
			  if diffsign then
          residue:=-residue;      // signs differ
			  // copy, shortening if necessary
			  DecCopyFit(res, rhs, _set, residue, status);
			  // if it was already shorter, then need to pad with zeros
			  if shift>0 then begin
			    res.digits:=decShiftToMost(PCardinal(@res.lsu[0]), res.digits, shift);
			    Dec(res.exponent,shift); 	     // adjust the exponent.
			  end;
			  // flip the result sign if unswapped and rhs was negated
			  if not swapped then
          res.bits:=res.bits or negate;
			  DecFinish(res, _set, residue, status);	  // done
			  break;
      end;

      // LHS digits may affect result
      rhsshift:=D2U(padding+1)-1;	// this much by Unit shift ..
      mult:=DECPOWERS[padding-(rhsshift*DECDPUN)]; // .. this by multiplication
		end; // padding needed

		if diffsign then
      mult:=-mult;		// signs differ

		// determine the longer operand
		maxdigits:=rhs.digits+padding;	// virtual length of RHS
		if lhs.digits>maxdigits then
      maxdigits:=lhs.digits;

		// Decide on the result buffer to use; if possible place directly
		// into result.
		acc:=PCardinal(@res.lsu[0]);			// assume add direct to result
		// If destructive overlap, or the number is too long, or a carry or
		// borrow to DIGITS+1 might be possible, a buffer must be used.
		// [Might be worth more sophisticated tests when maxdigits==reqdigits]
		if (maxdigits>=reqdigits)		// is, or could be, too large
		or ((res=rhs) and (rhsshift>0)) then begin	// destructive overlap
		  // buffer needed, choose it; units for maxdigits digits will be
		  // needed, +1 Unit for carry or borrow
		  need:=D2U(maxdigits)+1;
		  acc:=PCardinal(@accbuff[0]);			// assume use local buffer
		  if need*sizeof(Cardinal)>sizeof(accbuff) then begin
		    // printf("malloc add %ld %ld\n", need, sizeof(accbuff));
		    ReallocMem(allocacc,need*sizeof(Cardinal));
		    if allocacc=nil then begin		// hopeless -- abandon
		      status:=status or DEC_Insufficient_storage;
		      break;
        end;
		    acc:=allocacc;
		  end;
    end;

		res.bits:=Byte(bits and DECNEG);	// it's now safe to overwrite..
		res.exponent:=lhs.exponent;	// .. operands (even if aliased)

    // add [A+B*m] or subtract [A+B*(-m)]
    res.digits:=DecUnitAddSub(PCardinal(@lhs.lsu[0]), D2U(lhs.digits),
			PCardinal(@rhs.lsu[0]), D2U(rhs.digits), rhsshift, acc, mult)*DECDPUN;	   // [units . digits]
    if res.digits<0 then begin	   // borrowed...
      res.digits:=-res.digits;
      res.bits:=res.bits xor DECNEG;	   // flip the sign
		end;

    // If a buffer was used the result must be copied back, possibly
		// shortening.  (If no buffer was used then the result must have
		// fit, so can't need rounding and residue must be 0.)
		residue:=0;			   // clear accumulator
		if acc<>PCardinal(@res.lsu[0]) then begin
			// remove leading zeros that were added due to rounding up to
			// integral Units -- before the test for rounding.
			if res.digits>reqdigits then
			  res.digits:=DecGetDigits(acc, D2U(res.digits));
			DecSetCoeff(res, _set, acc, res.digits, residue, status);
    end; // used buffer

		// strip leading zeros [these were left on in case of subset subtract]
		res.digits:=decGetDigits(PCardinal(@res.lsu[0]), D2U(res.digits));

		// apply checks and rounding
		DecFinish(res, _set, residue, status);
    {$IFDEF TRIMING}
    DecTrim(res, _set, true, false, dropped);
    {$ENDIF}
    // "When the sum of two operands with opposite signs is exactly
		// zero, the sign of that sum shall be '+' in all rounding modes
		// except round toward -Infinity, in which mode that sign shall be
		// '-'."  [Subset zeros also never have '-', set by decFinish.]
		if DecNumberIsZero(res) and diffsign
    and ((status and DEC_Inexact)=0) then begin
		  if _set.round=DEC_ROUND_FLOOR then
        res.bits:=res.bits or DECNEG   // sign -
      else
        res.bits:=res.bits and not DECNEG;  // sign +
		end;
		// end protected
  until true;
  if Assigned(allocacc) then
    FreeMem(allocacc);	     // drop any storage used
end;

procedure DecMultiplyOp(res, lhs, rhs: PDecNumber;
  var _set: TDecContext; var status: Cardinal);
type
  TLocalBuffer = array[0..(((DECBUFFER*4+1)+DECDPUN-1) div DECDPUN)-1] of Cardinal;
var
  accunits: Integer;		   // Units of accumulator in use
	exponent: Integer;		   // work
	residue: Integer;		   // rounding residue
	bits: Byte;			   // result sign
	acc: PCardinal;			   // . accumulator Unit array
	needbytes: Integer;		   // size calculator
	allocacc: Pointer; 	   // . allocated accumulator, iff allocated
	accbuff: TLocalBuffer; // buffer (+1 for DECBUFFER==0, *4 for calls from other operations)
	mer, mermsup: PCardinal;	   // work
	madlength: Integer;		   // Units in multiplicand
	shift: Integer;			   // Units to shift multiplicand by
  temp: Integer;
  hold: PDecNumber;
  dropped: Integer;
begin
  residue:=0;
  allocacc:=nil;
  dropped:=0;
  FillChar(accbuff,SizeOf(accbuff),0);
  // precalculate result sign
	bits:=Byte((lhs.bits xor rhs.bits) and DECNEG);

	// handle infinities and NaNs
  temp:=SpecialArgs(lhs.bits,rhs.bits);
	if temp<>0 then begin		   // a special bit set
	  if (temp and (DECSNAN or DECNAN))<>0 then begin	 // one or two NaNs
	    DecNaNs(res, lhs, rhs, _set, status);
	    Exit;
    end;
	  // one or two infinities; Infinity * 0 is invalid
	  if (((lhs.bits and DECINF)=0) and DecNumberIsZero(lhs))
	  or (((rhs.bits and DECINF)=0) and DecNumberIsZero(rhs)) then begin
	    status:=status or DEC_Invalid_operation;
	    Exit;
    end;
	  DecNumberZero(res);
	  res.bits:=bits or DECINF;	   // infinity
	  Exit;
  end;

	// For best speed, as in DMSRCN [the original Rexx numerics
	// module], use the shorter number as the multiplier (rhs) and
	// the longer as the multiplicand (lhs) to minimise the number of
	// adds (partial products)
	if lhs.digits<rhs.digits then begin   // swap...
	  hold:=lhs;
	  lhs:=rhs;
	  rhs:=hold;
	end;

  repeat
    // if accumulator will be too long for local storage, then allocate
    acc:=PCardinal(@accbuff[0]);		   // . assume buffer for accumulator
    needbytes:=(D2U(lhs.digits)+D2U(rhs.digits))*SizeOf(Cardinal);
    if needbytes>SizeOf(accbuff) then begin
      allocacc:=AllocMem(needbytes);
      if allocacc=nil then begin
        status:=status or DEC_Insufficient_storage;
        break;
      end;
      acc:=PCardinal(allocacc);		     // use the allocated space
    end;
    // Now the main long multiplication loop
		// Unlike the equivalent in the IBM Java implementation, there
		// is no advantage in calculating from msu to lsu.  So, do it
		// by the book, as it were.
		// Each iteration calculates ACC=ACC+MULTAND*MULT
		accunits:=1;		   // accumulator starts at '0'
		acc^:=0;			   // .. (lsu=0)
		shift:=0;			   // no multiplicand shift at first
		madlength:=D2U(lhs.digits);  // this won't change
    {$POINTERMATH ON}
		mermsup:=PCardinal(@rhs.lsu[0])+D2U(rhs.digits); // . msu+1 of multiplier
    mer:=PCardinal(@rhs.lsu[0]);
    while mer<mermsup do begin
			// Here, *mer is the next Unit in the multiplier to use
			// If non-zero [optimization] add it...
			if mer^<>0 then
        accunits:=DecUnitAddSub(@acc[shift], accunits-shift,
          PCardinal(@lhs.lsu[0]), madlength, 0,	@acc[shift], mer^) + shift
      else begin // extend acc with a 0; it will be used shortly
			  (acc+accunits)^:=0;	   // [this avoids length of <=0 later]
			  Inc(accunits);
			end;
			// multiply multiplicand by 10**DECDPUN for next Unit to left
			Inc(shift);		   // add this for 'logical length'
      Inc(mer);
		end; // n
    {$POINTERMATH OFF}
    // acc now contains the exact result of the multiplication,
		// possibly with a leading zero unit; build the decNumber from
		// it, noting if any residue
		res.bits:=bits;			     // set sign
		res.digits:=DecGetDigits(acc, accunits); // count digits exactly

		// There can be a 31-bit wrap in calculating the exponent.
		// This can only happen if both input exponents are negative and
		// both their magnitudes are large.  If there was a wrap, set a
		// safe very negative exponent, from which decFinalize() will
		// raise a hard underflow shortly.
		exponent:=lhs.exponent+rhs.exponent;    // calculate exponent
		if (lhs.exponent<0) and (rhs.exponent<0) and (exponent>0) then
		  exponent:=-2*DECNUMMAXE;		     // force underflow
		res.exponent:=exponent;		     // OK to overwrite now
		// Set the coefficient.  If any rounding, residue records
		DecSetCoeff(res, _set, acc, res.digits, residue, status);
		DecFinish(res, _set, residue, status);   // final cleanup
    {$IFDEF TRIMING}
    DecTrim(res, _set, true, false, dropped);
    {$ENDIF}
  until true;
  if Assigned(allocacc) then
    FreeMem(allocacc);
end;

procedure DecDivideOp(res, lhs, rhs: PDecNumber;
  var _set: TDecContext; op: Byte; var status: Cardinal);
type
  TLocalBuffer = array[0..(((DECBUFFER+DECDPUN+10)+DECDPUN-1) div DECDPUN)-1] of Cardinal;
  TVarBuffer = array[0..(((DECBUFFER*2+DECDPUN)+DECDPUN-1) div DECDPUN)-1] of Cardinal;
var
  accbuff: TLocalBuffer;
  acc: PCardinal;          // . accumulator array for result
  allocacc: PCardinal; 	   // . allocated buffer, iff allocated */
	accnext: PCardinal;		   // . where next digit will go */
	acclength: Integer;		   // length of acc needed [Units] */
	accunits: Integer;		   // count of units accumulated */
	accdigits: Integer;		   // count of digits accumulated */
  varbuff: TVarBuffer;  // buffer for var1 */
	var1: PCardinal;			   // . var1 array for long subtraction */
	varalloc: PCardinal; 	   // . allocated buffer, iff used */
	msu1: PCardinal;			   // . msu of var1 */
	var2: PCardinal;		   // . var2 array */
	msu2: PCardinal;		   // . msu of var2 */
	msu2plus: Integer;		   // msu2 plus one [does not vary] */
	msu2pair: Int64;		   // msu2 pair plus one [does not vary] */
	var1units, var2units: Integer;	   // actual lengths */
	var2ulen: Integer;		   // logical length (units) */
	var1initpad: Integer;		   // var1 initial padding (digits) */
	maxdigits: Integer;		   // longest LHS or required acc length */
	mult: Integer;			   // multiplier for subtraction */
	thisunit: Cardinal;		   // current unit being accumulated */
	residue: Integer;		   // for rounding */
	reqdigits: Integer;	   // requested DIGITS */
	exponent: Integer;		   // working exponent */
	maxexponent: Integer;		   // DIVIDE maximum exponent if unrounded */
	bits: Byte;			   // working sign */
	target: PCardinal;		   // work */
	source: PCardinal;		   // .. */
	pow: PCardinal;		   // .. */
  u: PCardinal;		   // .. */
	shift, cut, temp: Integer;		   // .. */
  pv1, pv2: PCardinal;   // ..
	v2: Cardinal;			     // units to compare
  lsu: Cardinal;         // work */
  drop: Integer;         // work */
  postshift: Integer;			     // work */
  wasodd: Boolean;			     // integer was odd */
	quotlsu: PCardinal;			     // for save */
  quotdigits: Integer;		     // .. */
  exp: Integer;	     // save min(exponents) */
  up: PCardinal;     // work
  compare, tarunits: Integer;	// work
  half: Integer;		   // half to add to lower unit */
  expunits, exprem: Integer; // work
  allnines: Boolean; // True if quotient all nines
  dropped: Integer;
begin
  allocacc:=nil;
  varalloc:=nil;
  acc:=PCardinal(@accbuff[0]);
  var1:=PCardinal(@varbuff[0]);
  var1initpad:=0;
  maxexponent:=0;
  dropped:=0;
  reqdigits:=_set.digits;
  repeat  // protect allocated storage
    // [following code does not require input rounding] */
		bits:=(lhs.bits xor rhs.bits) and DECNEG;	// assumed sign for divisions */
    // handle infinities and NaNs */
    temp:=SpecialArgs(lhs.bits,rhs.bits);
  	if temp<>0 then begin		   // a special bit set
  	  if (temp and (DECSNAN or DECNAN))<>0 then begin	 // one or two NaNs
  	    DecNaNs(res, lhs, rhs, _set, status);
  	    break;
      end;
  	  // one or two infinities */
      if DecNumberIsInfinite(lhs) then begin	// LHS (dividend) is infinite */
  		  if DecNumberIsInfinite(rhs)  // two infinities are invalid .. */
  			or ((op and (REMAINDER or REMNEAR))<>0) then begin // as is remainder of infinity */
  		    status:=status or DEC_Invalid_operation;
  		    break;
  		  end;
  		  // [Note that infinity/0 raises no exceptions] */
  		  DecNumberZero(res);
  		  res.bits:=bits or DECINF;		// set +/- infinity */
  		  break;
  		end else begin
        // RHS (divisor) is infinite */
        residue:=0;
        if (op and (REMAINDER or REMNEAR))<>0 then begin
  				  // result is [finished clone of] lhs */
          DecCopyFit(res, lhs, _set, residue, status);
  			end else begin  // a division */
          DecNumberZero(res);
  				res.bits:=bits;		// set +/- zero */
  				// for DIVIDEINT the exponent is always 0.  For DIVIDE, result */
  				// is a 0 with infinitely negative exponent, clamped to minimum */
  				if (op and DIVIDE)<>0 then begin
  				  res.exponent:=_set.emin-_set.digits+1;
  				  status:=status or DEC_Clamped;
  				end;
        end;
        DecFinish(res, _set, residue, status);
        break;
      end;
    end;

    // handle 0 rhs (x/0) */
    if DecNumberIsZero(rhs) then begin			// x/0 is always exceptional */
      if DecNumberIsZero(lhs) then begin
				DecNumberZero(res);		// [after lhs test] */
				status:=status or DEC_Division_undefined; // 0/0 will become NaN */
      end else begin
				decNumberZero(res);
				if (op and (REMAINDER or REMNEAR))<>0 then
          status:=status or DEC_Invalid_operation
        else begin
				  status:=status or DEC_Division_by_zero; // x/0 */
				  res.bits:=bits or DECINF;	 // .. is +/- Infinity */
        end;
      end;
      break;
    end;

    // handle 0 lhs (0/x) */
    if DecNumberIsZero(lhs) then begin			// 0/x [x<>0] */
			if (op and DIVIDE)<>0 then begin
				residue:=0;
				exponent:=lhs.exponent-rhs.exponent; // ideal exponent */
				DecNumberCopy(res, lhs);	// [zeros always fit] */
				res.bits:=bits;		// sign as computed */
				res.exponent:=exponent;	// exponent, too */
				DecFinish(res, _set, residue, status);   // check exponent */
      end else
      if (op and DIVIDEINT)<>0 then begin
			  DecNumberZero(res);		// integer 0 */
			  res.bits:=bits;		// sign as computed */
			end else begin 			// a remainder */
			  exponent:=rhs.exponent;	// [save in case overwrite] */
			  DecNumberCopy(res, lhs);	// [zeros always fit] */
			  if exponent<res.exponent then
          res.exponent:=exponent; // use lower */
      end;
      break;
    end;

		// Precalculate exponent.  This starts off adjusted (and hence fits */
		// in 31 bits) and becomes the usual unadjusted exponent as the */
		// division proceeds.  The order of evaluation is important, here, */
		// to avoid wrap. */
		exponent:=(lhs.exponent+lhs.digits)-(rhs.exponent+rhs.digits);

		// If the working exponent is -ve, then some quick exits are */
		// possible because the quotient is known to be <1 */
		// [for REMNEAR, it needs to be < -1, as -0.5 could need work] */
		if (exponent<0) and (not(op=DIVIDE)) then begin
		  if (op and DIVIDEINT)<>0 then begin
		    DecNumberZero(res);		     // integer part is 0 */
			  res.bits:=bits;		     // set +/- zero */
		    break;
      end;
      // fastpath remainders so long as the lhs has the smaller */
      // (or equal) exponent */
			if lhs.exponent<=rhs.exponent then begin
			  if ((op and REMAINDER)<>0) or (exponent<-1) then begin
			    // It is REMAINDER or safe REMNEAR; result is [finished */
			    // clone of] lhs  (r = x - 0*y) */
			    residue:=0;
			    DecCopyFit(res, lhs, _set, residue, status);
			    DecFinish(res, _set, &residue, status);
			    break;
         end;
			// [unsafe REMNEAR drops through] */
			end;
    end; // fastpaths */

    // Long (slow) division is needed; roll up the sleeves... */
		// The accumulator will hold the quotient of the division. */
		// If it needs to be too long for stack storage, then allocate. */
		acclength:=D2U(reqdigits+DECDPUN);	// in Units */
		if (acclength*SizeOf(Cardinal)>SizeOf(accbuff)) then begin
		  // printf("malloc dvacc %ld units\n", acclength); */
		  allocacc:=AllocMem(acclength*SizeOf(Cardinal));
		  if allocacc=nil then begin		// hopeless -- abandon */
		    status:=status or DEC_Insufficient_storage;
		    break;
      end;
		  acc:=allocacc;			// use the allocated space */
    end;

		// var1 is the padded LHS ready for subtractions. */
		// If it needs to be too long for stack storage, then allocate. */
		// The maximum units needed for var1 (long subtraction) is: */
		// Enough for */
		//	   (rhs.digits+reqdigits-1) -- to allow full slide to right */
		// or  (lhs.digits)	     -- to allow for long lhs */
		// whichever is larger */
		//	 +1		   -- for rounding of slide to right */
		//	 +1		   -- for leading 0s */
		//	 +1		   -- for pre-adjust if a remainder or DIVIDEINT */
		// [Note: unused units do not participate in decUnitAddSub data] */
		maxdigits:=rhs.digits+reqdigits-1;
		if lhs.digits>maxdigits then
      maxdigits:=lhs.digits;
		var1units:=D2U(maxdigits)+2;
		// allocate a guard unit above msu1 for REMAINDERNEAR */
		if (op and DIVIDE)=0 then
      Inc(var1units);
		if (var1units+1)*SizeOf(Cardinal)>SizeOf(varbuff) then begin
		  // printf("malloc dvvar %ld units\n", var1units+1); */
		  varalloc:=AllocMem((var1units+1)*SizeOf(Cardinal));
      if varalloc=nil then begin		// hopeless -- abandon */
		    status:=status or DEC_Insufficient_storage;
		    break;
      end;
		  var1:=varalloc;			// use the allocated space */
    end;

    // Extend the lhs and rhs to full long subtraction length.	The lhs */
		// is truly extended into the var1 buffer, with 0 padding, so a */
		// subtract in place is always possible.  The rhs (var2) has */
		// virtual padding (implemented by decUnitAddSub). */
		// One guard unit was allocated above msu1 for rem=rem+rem in */
		// REMAINDERNEAR. */
    {$POINTERMATH ON}
		msu1:=var1+var1units-1;		// msu of var1 */
		source:=PCardinal(@lhs.lsu[0])+D2U(lhs.digits)-1; // msu of input array */
    target:=msu1;
    while source>=PCardinal(@lhs.lsu[0]) do begin
      target^:=source^;
      Dec(source);
      Dec(target);
    end;
    while target>=var1 do begin
      target^:=0;
      Dec(target);
    end;
		// rhs (var2) is left-aligned with var1 at the start */
		var2ulen:=var1units; 		// rhs logical length (units) */
		var2units:=D2U(rhs.digits); 	// rhs actual length (units) */
		var2:=PCardinal(@rhs.lsu[0]);			// . rhs array */
		msu2:=var2+var2units-1;		// . msu of var2 [never changes] */
		// now set up the variables which will be used for estimating the */
		// multiplication factor.  If these variables are not exact, add */
		// 1 to make sure that the multiplier is never overestimated. */
		msu2plus:=msu2^;			// it's value .. */
		if var2units>1 then
      Inc(msu2plus);	// .. +1 if any more */
		msu2pair:=Int64(msu2^)*(DECDPUNMAX+1);// top two pair .. */
		if var2units>1 then begin		// .. [else treat 2nd as 0] */
		  Inc(msu2pair,(msu2-1)^);		// .. */
		  if var2units>2 then
        Inc(msu2pair);	// .. +1 if any more */
		end;

		// The calculation is working in units, which may have leading zeros, */
		// but the exponent was calculated on the assumption that they are */
		// both left-aligned.  Adjust the exponent to compensate: add the */
		// number of leading zeros in var1 msu and subtract those in var2 msu. */
		// [This is actually done by counting the digits and negating, as */
		// lead1=DECDPUN-digits1, and similarly for lead2.] */
    pow:=@DECPOWERS[1];
    while msu1^>=pow^ do begin
      Dec(exponent);
      Inc(pow);
    end;
    pow:=@DECPOWERS[1];
    while msu2^>=pow^ do begin
      Inc(exponent);
      Inc(pow);
    end;

		// Now, if doing an integer divide or remainder, ensure that */
		// the result will be Unit-aligned.  To do this, shift the var1 */
		// accumulator towards least if need be.  (It's much easier to */
		// do this now than to reassemble the residue afterwards, if */
		// doing a remainder.)  Also ensure the exponent is not negative. */
		if (op and DIVIDE)=0 then begin
		  // save the initial 'false' padding of var1, in digits */
		  var1initpad:=(var1units-D2U(lhs.digits))*DECDPUN;
		  // Determine the shift to do. */
		  if exponent<0 then
        cut:=-exponent
      else
        cut:=DECDPUN-exponent mod DECDPUN;
		  DecShiftToLeast(var1, var1units, cut);
		  Inc(exponent,cut);			// maintain numerical value */
		  Dec(var1initpad,cut); 		// .. and reduce padding */
		  // clean any most-significant units which were just emptied */
      u:=msu1;
      while cut>=DECDPUN do begin
        Dec(cut,DECDPUN);
        u^:=0;
        Dec(u);
      end;
    end else begin
		  // is DIVIDE */
		  maxexponent:=lhs.exponent-rhs.exponent;	  // save */
		  // optimization: if the first iteration will just produce 0, */
		  // preadjust to skip it [valid for DIVIDE only] */
		  if msu1^<msu2^ then begin
        Dec(var2ulen);			// shift down */
		    Dec(exponent,DECDPUN);		// update the exponent */
		  end;
    end;

    // ---- start the long-division loops ------------------------------ */
		accunits:=0; 			// no units accumulated yet */
		accdigits:=0;			// .. or digits */
		accnext:=acc+acclength-1;		// . msu of acc [NB: allows digits+1] */
		while true do begin				// outer forever loop */
      thisunit:=0;			// current unit assumed 0 */
				  // find the next unit */
			while true do begin				// inner forever loop */
				// strip leading zero units [from either pre-adjust or from */
				// subtract last time around].	Leave at least one unit. */
        while (msu1^=0) and (msu1>var1) do begin
          Dec(var1units);
          Dec(msu1);
        end;
				if var1units<var2ulen then
          break;	     // var1 too low for subtract */
				if var1units=var2ulen then begin	     // unit-by-unit compare needed */
				  // compare the two numbers, from msu */
				  pv2:=msu2;			     // . msu */
          pv1:=msu1;
          while true do begin
            // v1:=pv1^ -- always OK */
            v2:=0;			     // assume in padding */
					  if pv2>=var2 then
              v2:=pv2^;	     // in range */
					  if pv1^<>v2 then
              break;	     // no longer the same */
					  if pv1=var1 then
              break;	     // done; leave pv1 as is */
            Dec(pv1);
            Dec(pv2);
          end;
				  // here when all inspected or a difference seen */
				  if pv1^<v2 then
            break;		     // var1 too low to subtract */
				  if pv1^=v2 then	begin	     // var1 == var2 */
					  // reach here if var1 and var2 are identical; subtraction */
					  // would increase digit by one, and the residue will be 0 so */
					  // the calculation is done; leave the loop with residue=0. */
					  Inc(thisunit); 		     // as though subtracted */
					  var1^:=0;			     // set var1 to 0 */
					  var1units:=1;		     // .. */
					  break;  // from inner */
					end; // var1 = var2 */
				  // pv1^>v2.  Prepare for real subtraction; the lengths are equal */
				  // Estimate the multiplier (there's always a msu1-1)... */
				  // Bring in two units of var2 to provide a good estimate. */
				  mult:=Integer((Int64(msu1^)*(DECDPUNMAX+1)+(msu1-1)^) div msu2pair);
				  // lengths the same */
        end else begin // var1units > var2ulen, so subtraction is safe */
				  // The var2 msu is one unit towards the lsu of the var1 msu, */
				  // so only one unit for var2 can be used. */
				  mult:=Integer((Int64(msu1^)*(DECDPUNMAX+1)+(msu1-1)^) div msu2plus);
        end;
				if mult=0 then
          mult:=1;		     // must always be at least 1 */
				// subtraction needed; var1 is > var2 */
				thisunit:=Cardinal(thisunit+mult);      // accumulate */
				// subtract var1-var2, into var1; only the overlap needs */
				// processing, as this is an in-place calculation */
				shift:=var2ulen-var2units;
				DecUnitAddSub(PCardinal(@var1[shift]), var1units-shift, var2, var2units,
          0, PCardinal(@var1[shift]), -mult);
				// var1 now probably has leading zeros; these are removed at the */
				// top of the inner loop. */
      end;// inner loop */

      // The next unit has been calculated in full; unless it's a */
      // leading zero, add to acc */
      if (accunits<>0) or (thisunit<>0) then begin      // is first or non-zero */
				accnext^:=thisunit;		     // store in accumulator */
				// account exactly for the new digits */
				if accunits=0 then begin
				  Inc(accdigits);			     // at least one */
          pow:=@DECPOWERS[1];
          while thisunit>=pow^ do begin
            Inc(accdigits);
            Inc(pow);
          end;
				end else
          Inc(accdigits,DECDPUN);
				Inc(accunits);			     // update count */
				Dec(accnext);			     // ready for next */
				if accdigits>reqdigits then
          break;      // have enough digits */
      end;

      // if the residue is zero, the operation is done (unless divide */
      // or divideInteger and still not enough digits yet) */
      if (var1^=0) and (var1units=1) then begin 	     // residue is 0 */
				if (op and (REMAINDER or REMNEAR))<>0 then
          break;
				if ((op and DIVIDE)<>0) and (exponent<=maxexponent) then
          break;
				// [drop through if divideInteger] */
      end;
			// also done enough if calculating remainder or integer */
			// divide and just did the last ('units') unit */
			if (exponent=0) and ((op and DIVIDE)=0) then
        break;

			// to get here, var1 is less than var2, so divide var2 by the per- */
			// Unit power of ten and go for the next digit */
			Dec(var2ulen);			     // shift down */
			Dec(exponent,DECDPUN);		     // update the exponent */
    end; // outer loop */

		// ---- division is complete --------------------------------------- */
		// here: acc      has at least reqdigits+1 of good results (or fewer */
		//		      if early stop), starting at accnext+1 (its lsu) */
		//	     var1     has any residue at the stopping point */
		//	     accunits is the number of digits collected in acc */
		if accunits=0 then begin		   // acc is 0 */
		  accunits:=1;		   // show have a unit .. */
		  accdigits:=1;		   // .. */
		  accnext^:=0;		   // .. whose value is 0 */
    end else
      Inc(accnext);		   // back to last placed */
		// accnext now . lowest unit of result */

		residue:=0;			   // assume no residue */
		if (op and DIVIDE)<>0 then begin
		  // record the presence of any residue, for rounding */
		  if (var1^<>0) or (var1units>1) then
        residue:=1
      else begin // no residue */
		    // Had an exact division; clean up spurious trailing 0s. */
		    // There will be at most DECDPUN-1, from the final multiply, */
		    // and then only if the result is non-0 (and even) and the */
		    // exponent is 'loose'. */

		    lsu:=accnext^;
		    if ((lsu and $01)=0) and (lsu<>0) then begin
		      // count the trailing zeros */
		      drop:=0;
          while true do begin // [will terminate because lsu<>0] */
  		      if exponent>=maxexponent then
              break;	  // don't chop real 0s */
    			  if (lsu mod DECPOWERS[drop+1])<>0 then
              break;   // found non-0 digit */
      			Inc(exponent);
            Inc(drop);
          end;
          if drop>0 then begin
			      accunits:=decShiftToLeast(accnext, accunits, drop);
			      accdigits:=decGetDigits(accnext, accunits);
			      accunits:=D2U(accdigits);
			      // [exponent was adjusted in the loop] */
			    end;
        end; // neither odd nor 0 */
      end;  // exact divide
    end  // divide */
    else begin // op<>DIVIDE */
		  // check for coefficient overflow */
		  if accdigits+exponent>reqdigits then begin
		    status:=status or DEC_Division_impossible;
		    break;
		  end;
		  if (op and (REMAINDER or REMNEAR))<>0 then begin
		    // [Here, the exponent will be 0, because var1 was adjusted */
		    // appropriately.] */
        wasodd:=false;
    		bits:=lhs.bits; 		     // remainder sign is always as lhs */
    		// Fastpath when residue is truly 0 is worthwhile [and */
    		// simplifies the code below] */
    		if (var1^=0) and (var1units=1) then begin      // residue is 0 */
		      exp:=lhs.exponent;	     // save min(exponents) */
		      if rhs.exponent<exp then
            exp:=rhs.exponent;
		      DecNumberZero(res);		     // 0 coefficient */
		      res.exponent:=exp;		     // .. with proper exponent */
		      res.bits:=Byte(bits and DECNEG);	   // [cleaned] */
		      DecFinish(res, _set, residue, status);   // might clamp */
		      break;
		    end;
		    // note if the quotient was odd */
		    if (accnext^ and $01)<>0 then
          wasodd:=true;	     // acc is odd */
		    quotlsu:=accnext;		     // save in case need to reinspect */
		    quotdigits:=accdigits;		     // .. */

		    // treat the residue, in var1, as the value to return, via acc */
		    // calculate the unused zero digits.  This is the smaller of: */
		    //   var1 initial padding (saved above) */
		    //   var2 residual padding, which happens to be given by: */
		    postshift:=var1initpad+exponent-lhs.exponent+rhs.exponent;
		    // [the 'exponent' term accounts for the shifts during divide] */
		    if var1initpad<postshift then
          postshift:=var1initpad;

    		// shift var1 the requested amount, and adjust its digits */
    		var1units:=DecShiftToLeast(var1, var1units, postshift);
    		accnext:=var1;
    		accdigits:=decGetDigits(var1, var1units);
    		accunits:=D2U(accdigits);

    		exponent:=lhs.exponent; 	// exponent is smaller of lhs & rhs */
    		if rhs.exponent<exponent then
          exponent:=rhs.exponent;

    		// Now correct the result if doing remainderNear; if it */
    		// (looking just at coefficients) is > rhs/2, or == rhs/2 and */
    		// the integer was odd then the result should be rem-rhs. */
    		if (op and REMNEAR)<>0 then begin
    		  // calculate remainder*2 into the var1 buffer (which has */
    		  // 'headroom' of an extra unit and hence enough space) */
    		  // [a dedicated 'double' loop would be faster, here] */
    		  tarunits:=DecUnitAddSub(accnext, accunits, accnext, accunits,
            0, accnext, 1);
    		  // decDumpAr('r', accnext, tarunits); */

    		  // Here, accnext (var1) holds tarunits Units with twice the */
    		  // remainder's coefficient, which must now be compared to the */
    		  // RHS.  The remainder's exponent may be smaller than the RHS's. */
    		  compare:=DecUnitCompare(accnext, tarunits, PCardinal(@rhs.lsu[0]),
            D2U(rhs.digits), rhs.exponent-exponent);
    		  if compare=BADINT then begin	     // deep trouble */
    			  status:=status or DEC_Insufficient_storage;
    			  break;
          end;

      		  // now restore the remainder by dividing by two; the lsu */
      		  // is known to be even. */
          up:=accnext;
          while up<accnext+tarunits do begin
            repeat
              half:=up^ and $01;
        			up^:=up^ shr 1;		   // [shift] */
        			if half=0 then
                break;
      			  Inc((up-1)^,(DECDPUNMAX+1) shr 1);
            until true;
            Inc(up);
          end;

    		  // [accunits still describes the original remainder length] */
    		  if (compare>0) or ((compare=0) and wasodd) then begin // adjustment needed */
      			// This is effectively causing round-up of the quotient, */
      			// so if it was the rare case where it was full and all */
      			// nines, it would overflow and hence division-impossible */
      			// should be raised */
      			allnines:=false;		     // 1 if quotient all nines */
      			if quotdigits=reqdigits then begin      // could be borderline */
              up:=quotlsu;
      			  while true do begin
                if quotdigits>DECDPUN then begin
      			      if up^<>DECDPUNMAX then
                    break;// non-nines */
      			    end else begin 		     // this is the last Unit */
    			        if up^=DECPOWERS[quotdigits]-1 then
                    allnines:=true;
                  break;
    			      end;
    			      Dec(quotdigits,DECDPUN);	     // checked those digits */
                Inc(up);
              end; // up */
            end; // borderline check */
    			  if allnines then begin
    			    status:=status or DEC_Division_impossible;
    			    break;
            end;

    			  // rem-rhs is needed; the sign will invert.  Again, var1 */
    			  // can safely be used for the working Units array. */
    			  exp:=rhs.exponent-exponent;      // RHS padding needed */
    			  // Calculate units and remainder from exponent. */
      			expunits:=exp div DECDPUN;
      			exprem:=exp mod DECDPUN;
      			// subtract [A+B*(-m)]; the result will always be negative */
      			accunits:=-DecUnitAddSub(accnext, accunits,
              PCardinal(@rhs.lsu[0]), D2U(rhs.digits),
              expunits, accnext, -Integer(DECPOWERS[exprem]));
            accdigits:=decGetDigits(accnext, accunits); // count digits exactly */
      			accunits:=D2U(accdigits);	// and recalculate the units for copy */
      			// [exponent is as for original remainder] */
      			bits:=bits xor DECNEG;		// flip the sign */
          end;
        end; // REMNEAR */
      end; // REMAINDER or REMNEAR */
    end; // not DIVIDE */

		// Set exponent and bits */
		res.exponent:=exponent;
		res.bits:=Byte(bits and DECNEG);	     // [cleaned] */
    {$POINTERMATH OFF}
		// Now the coefficient. */
		DecSetCoeff(res, _set, accnext, accdigits, residue, status);
		DecFinish(res, _set, residue, status);   // final cleanup */
    {$IFDEF TRIMING}
    DecTrim(res, _set, true, false, dropped);
    {$ENDIF}
  until true;
  if Assigned(allocacc) then
    FreeMem(allocacc);
  if Assigned(varalloc) then
    FreeMem(varalloc);
end;

procedure DecCompareOp(res, lhs, rhs: PDecNumber;
  var _set: TDecContext; op: Byte; var status: Cardinal);
var
  merged: Byte; 		   // work */
  result: SmallInt;
  residue: Integer;
  choice: PDecNumber;
  slhs: Byte;
  srhs: Byte;
begin
  repeat				   // protect allocated storage */
    // [following code does not require input rounding] */
		// If total ordering then handle differing signs 'up front' */
		if op=COMPTOTAL then begin		// total ordering */
		  if DecNumberIsNegative(lhs) and not DecNumberIsNegative(rhs) then begin
		    result:=-1;
		    break;
		  end;
		  if not DecNumberIsNegative(lhs) and DecNumberIsNegative(rhs) then begin
		    result:=+1;
		    break;
		  end;
    end;

		// handle NaNs specially; let infinities drop through */
		// This assumes sNaN (even just one) leads to NaN. */
		merged:=(lhs.bits or rhs.bits) and (DECSNAN or DECNAN);
		if merged<>0 then begin			// a NaN bit set */
		  if op=COMPARE then 		// result will be NaN */
      else if op=COMPSIG then		// treat qNaN as sNaN */
		    status:=status or DEC_Invalid_operation or DEC_sNaN
      else if op=COMPTOTAL then begin		// total ordering, always finite */
		    // signs are known to be the same; compute the ordering here */
		    // as if the signs are both positive, then invert for negatives */
		    if not DecNumberIsNaN(lhs) then
          result:=-1
		    else if not DecNumberIsNaN(rhs) then
          result:=+1
		    // here if both NaNs */
		    else if DecNumberIsSNaN(lhs) and decNumberIsQNaN(rhs) then
          result:=-1
		    else if DecNumberIsQNaN(lhs) and decNumberIsSNaN(rhs) then
          result:=+1
        else begin // both NaN or both sNaN */
		      // now it just depends on the payload */
		      result:=DecUnitCompare(PCardinal(@lhs.lsu[0]), D2U(lhs.digits),
					  PCardinal(@rhs.lsu[0]), D2U(rhs.digits), 0);
		      // [Error not possible, as these are 'aligned'] */
		    end; // both same NaNs */
		    if DecNumberIsNegative(lhs) then
          result:=-result;
	      break;
      end // total order */
      else if (merged and DECSNAN)<>0 then	     // sNaN . qNaN */
      else begin // here if MIN or MAX and one or two quiet NaNs */
		    // min or max -- 754 rules ignore single NaN */
		    if not DecNumberIsNaN(lhs) or not DecNumberIsNaN(rhs) then begin
		      // just one NaN; force choice to be the non-NaN operand */
		      op:=COMPMAX;
		      if (lhs.bits and DECNAN)<>0 then
            result:=-1 // pick rhs */
          else
            result:=+1; // pick lhs */
		      break;
		    end;
		  end; // max or min */
		  op:=COMPNAN;			     // use special path */
		  DecNaNs(res, lhs, rhs, _set, status);   // propagate NaN */
		  break;
    end;
		// have numbers */
		if (op=COMPMAXMAG) or (op=COMPMINMAG) then
      result:=decCompare(lhs, rhs, 1)
    else
      result:=decCompare(lhs, rhs, 0);    // sign matters */
  until true; 			     // end protected */

  if result=BADINT then
    status:=status or DEC_Insufficient_storage // rare */
  else begin
		if (op=COMPARE) or (op=COMPSIG) or (op=COMPTOTAL) then begin // returning signum */
		  if (op=COMPTOTAL) and (result=0) then begin
		    // operands are numerically equal or same NaN (and same sign, */
		    // tested first); if identical, leave result 0 */
		    if lhs.exponent<>rhs.exponent then begin
		      if lhs.exponent<rhs.exponent then
            result:=-1
		      else
            result:=+1;
		      if DecNumberIsNegative(lhs) then
            result:=-result;
        end; // lexp!=rexp */
		  end; // total-order by exponent */
      DecNumberZero(res);		// [always a valid result] */
      if result<>0 then	begin		// must be -1 or +1 */
		    res.lsu[0]:=1;
		    if result<0 then
          res.bits:=DECNEG;
		  end;
	  end
    else if op=COMPNAN then		// special, drop through */
    else begin				// MAX or MIN, non-NaN result */
      residue:=0;			// rounding accumulator */
      // choose the operand for the result */
      if result=0 then begin // operands are numerically equal */
		    // choose according to sign then exponent (see 754) */
        slhs:=(lhs.bits and DECNEG);
        srhs:=(rhs.bits and DECNEG);
        if slhs<>srhs then begin	   // signs differ */
          if slhs<>0 then
            result:=-1	   // rhs is max */
          else
            result:=+1;	   // lhs is max */
        end
        else if (slhs and srhs)<>0 then begin // both negative */
          if lhs.exponent<rhs.exponent then
            result:=+1
          else
            result:=-1;
				    // [if equal, use lhs, technically identical] */
        end else begin 		   // both positive */
          if lhs.exponent>rhs.exponent then
            result:=+1
          else
            result:=-1;
				    // [ditto] */
        end;
      end; // numerically equal */
      // here result will be non-0; reverse if looking for MIN */
      if (op=COMPMIN) or (op=COMPMINMAG) then
        result:=-result;
      if result>0 then
        choice:=lhs
      else
        choice:=rhs;	// choose */
      // copy chosen to result, rounding if need be */
      DecCopyFit(res, choice, _set, residue, status);
      DecFinish(res, _set, residue, status);
	  end;
	end;
end;

procedure DecNumberAdd(res: PDecNumber; const lhs, rhs: PDecNumber;
  var _set: TDecContext);
var
  status: Cardinal;
begin
  status:=0;
  DecNumberZero(res);
  DecAddOp(res, lhs, rhs, _set, 0, status);
  if status<>0 then
    DecStatus(res, status, _set);
end;

procedure DecNumberSubtract(res: PDecNumber; const lhs, rhs: PDecNumber;
  var _set: TDecContext);
var
  status: Cardinal;
begin
  status:=0;
  DecNumberZero(res);
  DecAddOp(res, lhs, rhs, _set, DECNEG, status);
  if status<>0 then
    DecStatus(res, status, _set);
end;

procedure DecNumberMultiply(res: PDecNumber; const lhs, rhs: PDecNumber;
  var _set: TDecContext);
var
  status: Cardinal;
begin
  status:=0;
  DecMultiplyOp(res, lhs, rhs, _set, status);
  if status<>0 then
    DecStatus(res, status, _set);
end;

procedure DecNumberDivide(res: PDecNumber; const lhs, rhs: PDecNumber;
  var _set: TDecContext);
var
  status: Cardinal;
begin
  status:=0;
  DecNumberZero(res);
  DecDivideOp(res, lhs, rhs, _set, DIVIDE, status);
  if status<>0 then
    DecStatus(res, status, _set);
end;

procedure DecNumberReduce(res: PDecNumber; const rhs: PDecNumber;
  var _set: TDecContext);
var
  status: Cardinal;
  residue: Integer;
  dropped: Integer;
begin
  status:=0;
  residue:=0;
  repeat
    // [following code does not require input rounding] */
		// Infinities copy through; NaNs need usual treatment */
		if DecNumberIsNaN(rhs) then begin
		  DecNaNs(res, rhs, nil, _set, status);
		  break;
    end;
		// reduce result to the requested length and copy to result */
		DecCopyFit(res, rhs, _set, residue, status); // copy & round */
		DecFinish(res, _set, residue, status);	  // cleanup/set flags */
    {$IFDEF TRIMING}
		DecTrim(res, _set, true, false, dropped);		  // normalize in place */
    {$ENDIF}
    // [may clamp] */
  until true;
  if status<>0 then
    DecStatus(res, status, _set); // then report status
end;

procedure DecNumberCompare(res: PDecNumber; const lhs, rhs: PDecNumber;
  var _set: TDecContext);
var
  status: Cardinal;
begin
  status:=0;
  DecCompareOp(res, lhs, rhs, _set, COMPARE, status);
  if status<>0 then
    DecStatus(res, status, _set);
end;

function DecNumberCompareInt(const lhs, rhs: PDecNumber;
  var _set: TDecContext): Integer;
var
  status: Cardinal;
  res: TDecNumber;
begin
  status:=0;
  DecCompareOp(@res, lhs, rhs, _set, COMPARE, status);
  if status<>0 then
    DecStatus(@res, status, _set);
	result:=DecNumberToInt32(@res,g_set);
end;

{ TCDecNumber }

constructor TCDecNumber.Create(const Value: TCDecNumber);
begin
  Init;
  Assign(Value);
end;

constructor TCDecNumber.Create(const Value: Integer);
begin
  Init;
  Assign(Value);
end;

constructor TCDecNumber.Create(const Value: Double);
begin
  Init;
  Assign(Value);
end;

constructor TCDecNumber.Create(const Value: Single);
begin
  Init;
  Assign(Value);
end;

constructor TCDecNumber.Create(const Value: Extended);
begin
  Init;
  Assign(Value);
end;

constructor TCDecNumber.Create(const Value: Int64);
begin
  Init;
  Assign(Value);
end;

constructor TCDecNumber.Create(const Value: Cardinal);
begin
  Init;
  Assign(Value);
end;

constructor TCDecNumber.Create(const Value: UInt64);
begin
  Init;
  Assign(Value);
end;

constructor TCDecNumber.Create(const Value: AnsiString);
begin
  Init;
  Assign(Value);
end;

procedure TCDecNumber.Init;
begin
  FillChar(fDec,SizeOf(fDec),0);
  fStop:=false;
end;

function TCDecNumber.IsZero: Boolean;
begin
  result:=DecNumberIsZero(@fDec);
end;

function TCDecNumber.GetExponent: Integer;
begin
  result:=fDec.exponent+fDec.digits;
  Dec(result);
end;

procedure TCDecNumber.Assign(const Value: TCDecNumber);
begin
  DecNumberCopy(@fDec, @Value.fDec);
end;

procedure TCDecNumber.Assign(const Value: AnsiString);
begin
  DecNumberFromString(@fDec, Value, g_set);
end;

procedure TCDecNumber.Assign(const Value: Extended);
var
  s: AnsiString;
begin
  s:=Format('%.22g',[Value]);
  DecNumberFromString(@fDec, s, g_set);
end;

procedure TCDecNumber.Assign(const Value: Double);
var
  s: AnsiString;
begin
  Str(Value,s);
  DecNumberFromString(@fDec, s, g_set);
end;

procedure TCDecNumber.Assign(const Value: Single);
var
  s: AnsiString;
begin
  Str(Value,s);
  DecNumberFromString(@fDec, s, g_set);
end;

procedure TCDecNumber.Assign(const Value: Integer);
begin
  DecNumberFromInt32(@fDec,Value);
end;

procedure TCDecNumber.Assign(const Value: Int64);
var
  s: AnsiString;
begin
  Str(Value,s);
  DecNumberFromString(@fDec, s, g_set);
end;

procedure TCDecNumber.Assign(const Value: UInt64);
var
  s: AnsiString;
begin
  Str(Value,s);
  DecNumberFromString(@fDec, s, g_set);
end;

procedure TCDecNumber.Assign(const Value: Cardinal);
begin
  DecNumberFromUInt32(@fDec,Value);
end;

procedure TCDecNumber.Reduce;
begin
  DecNumberReduce(@fDec,@fDec, g_set);
end;

function TCDecNumber.ToString: AnsiString;
var
  buf: PAnsiChar;
begin
  Reduce;
  buf:=AnsiStrAlloc(DECNUMDIGITS+139);
  try
  	DecNumberToString(@fDec, buf);
    SetString(result,buf,AnsiStrings.StrLen(buf));
  finally
    AnsiStrings.StrDispose(buf);
  end;
end;

function TCDecNumber.ToText: AnsiString;
var
  buf,sexp: PAnsiChar;
  nexp, e, i: Integer;
begin
  buf:=AnsiStrAlloc(DECNUMDIGITS+140);
  try
  	DecNumberToString(@fDec, buf);
    if (AnsiStrings.StrPos(buf,'.')<>nil) and (AnsiStrings.StrPos(buf,'E')=nil) then begin
		  while buf[AnsiStrings.StrLen(buf)-1]='0' do
			  buf[AnsiStrings.StrLen(buf)-1]:=#0;
		  if buf[AnsiStrings.StrLen(buf)-1]='.' then
			  buf[AnsiStrings.StrLen(buf)-1]:=#0;
	  end
    else if (AnsiStrings.StrLComp(buf,'0E',2)=0) then begin
		  buf[1]:=#0;
	  end
    else if AnsiStrings.StrPos(buf,'E')<>nil then begin
      sexp:=AnsiStrings.StrPos(buf,'E');
      Inc(sexp);
		  Val(sexp,nexp,e);
		  if (nexp<DECNUMDIGITS-3) and (nexp>0) then begin
        i:=0;
        while (buf[i]<>#0) and (buf[i]<>'.') and (buf[i+1]<>'E') do
          Inc(i);
        while nexp<>0 do begin
  				if buf[i+1]='E' then begin
            if i=0 then
						  Inc(i);
				    break;
				  end;
				  buf[i]:=buf[i+1];
				  Dec(nexp);
				  Inc(i);
			  end;
			  while nexp<>0 do begin
				  buf[i]:='0';
				  Dec(nexp);
				  Inc(i);
			  end;
			  buf[i]:=#0;
  		end;
		  sExp:=AnsiStrings.StrScan(buf,'E');
		  while (sexp<>nil) and (sexp<>buf) and (((sexp-1)^='0') or ((sexp-1)^='.')) do begin
			  AnsiStrings.StrCopy(sexp-1,sexp);
			  Dec(sexp);
		  end;
	  end;
    SetString(result,buf,AnsiStrings.StrLen(buf));
  finally
    AnsiStrings.StrDispose(buf);
  end;
end;

function TCDecNumber.ToExtended: Extended;
var
  e, p: Integer;
  s, ex: AnsiString;
begin
  s := ToString;
  p := Pos('E',s);
  if p > 0 then
    ex:=Copy(s, p, MaxInt);
  s := Copy(s, 1, 22) + ex;
  Val(s, result, e);
end;

function TCDecNumber.ToDouble: Double;
begin
  result := ToExtended;
end;

function TCDecNumber.ToSingle: Single;
begin
  result := ToDouble;
end;

function TCDecNumber.ToInt64: Int64;
begin
  result := Round(ToExtended);
end;

function TCDecNumber.ToInteger: Integer;
begin
  result := Round(ToExtended);
end;

function TCDecNumber.ToCardinal: Cardinal;
begin
  result := Round(ToExtended);
end;

class operator TCDecNumber.Implicit(const Value: AnsiString): TCDecNumber;
begin
  Result.Create(Value);
end;

class operator TCDecNumber.Implicit(const Value: string): TCDecNumber;
begin
  Result.Create(AnsiString(Value));
end;

class operator TCDecNumber.Implicit(const Value: Extended): TCDecNumber;
begin
  Result.Create(Value);
end;

class operator TCDecNumber.Implicit(const Value: Double): TCDecNumber;
begin
  Result.Create(Value);
end;

class operator TCDecNumber.Implicit(const Value: Single): TCDecNumber;
begin
  Result.Create(Value);
end;

class operator TCDecNumber.Implicit(const Value: Int64): TCDecNumber;
begin
  Result.Create(Value);
end;

class operator TCDecNumber.Implicit(const Value: UInt64): TCDecNumber;
begin
  Result.Create(Value);
end;

class operator TCDecNumber.Implicit(const Value: Integer): TCDecNumber;
begin
  Result.Create(Value);
end;

class operator TCDecNumber.Implicit(const Value: Cardinal): TCDecNumber;
begin
  Result.Create(Value);
end;

class operator TCDecNumber.Negative(const Value: TCDecNumber): TCDecNumber;
begin
  result:=0-Value;
end;

class operator TCDecNumber.Positive(const Value: TCDecNumber): TCDecNumber;
begin
  Result.Create(Value);
end;

class operator TCDecNumber.Add(const Left, Right: TCDecNumber): TCDecNumber;
begin
  DecNumberAdd(@Result.fDec, @Left.fDec, @Right.fDec, g_set);
end;

class operator TCDecNumber.Add(const Left: TCDecNumber;
  const Right: Extended): TCDecNumber;
var
  t: TCDecNumber;
begin
  t:=TCDecNumber.Create(Right);
  DecNumberAdd(@Result.fDec, @Left.fDec, @t.fDec, g_set);
end;

class operator TCDecNumber.Add(const Left: TCDecNumber;
  const Right: Double): TCDecNumber;
var
  t: TCDecNumber;
begin
  t:=TCDecNumber.Create(Right);
  DecNumberAdd(@Result.fDec, @Left.fDec, @t.fDec, g_set);
end;

class operator TCDecNumber.Add(const Left: TCDecNumber;
  const Right: Single): TCDecNumber;
var
  t: TCDecNumber;
begin
  t:=TCDecNumber.Create(Right);
  DecNumberAdd(@Result.fDec, @Left.fDec, @t.fDec, g_set);
end;

class operator TCDecNumber.Add(const Left: TCDecNumber;
  const Right: Int64): TCDecNumber;
var
  t: TCDecNumber;
begin
  t:=TCDecNumber.Create(Right);
  DecNumberAdd(@Result.fDec, @Left.fDec, @t.fDec, g_set);
end;

class operator TCDecNumber.Add(const Left: TCDecNumber;
  const Right: Cardinal): TCDecNumber;
var
  t: TCDecNumber;
begin
  t:=TCDecNumber.Create(Right);
  DecNumberAdd(@Result.fDec, @Left.fDec, @t.fDec, g_set);
end;

class operator TCDecNumber.Add(const Left: TCDecNumber;
  const Right: Integer): TCDecNumber;
var
  t: TCDecNumber;
begin
  t:=TCDecNumber.Create(Right);
  DecNumberAdd(@Result.fDec, @Left.fDec, @t.fDec, g_set);
end;

class operator TCDecNumber.Add(const Left: TCDecNumber;
  const Right: UInt64): TCDecNumber;
var
  t: TCDecNumber;
begin
  t:=TCDecNumber.Create(Right);
  DecNumberAdd(@Result.fDec, @Left.fDec, @t.fDec, g_set);
end;

class operator TCDecNumber.Add(const Left: Single;
  const Right: TCDecNumber): TCDecNumber;
var
  t: TCDecNumber;
begin
  t:=TCDecNumber.Create(Left);
  DecNumberAdd(@Result.fDec, @t.fDec, @Right.fDec, g_set);
end;

class operator TCDecNumber.Add(const Left: Double;
  const Right: TCDecNumber): TCDecNumber;
var
  t: TCDecNumber;
begin
  t:=TCDecNumber.Create(Left);
  DecNumberAdd(@Result.fDec, @t.fDec, @Right.fDec, g_set);
end;

class operator TCDecNumber.Add(const Left: Extended;
  const Right: TCDecNumber): TCDecNumber;
var
  t: TCDecNumber;
begin
  t:=TCDecNumber.Create(Left);
  DecNumberAdd(@Result.fDec, @t.fDec, @Right.fDec, g_set);
end;

class operator TCDecNumber.Add(const Left: Int64;
  const Right: TCDecNumber): TCDecNumber;
var
  t: TCDecNumber;
begin
  t:=TCDecNumber.Create(Left);
  DecNumberAdd(@Result.fDec, @t.fDec, @Right.fDec, g_set);
end;

class operator TCDecNumber.Add(const Left: Cardinal;
  const Right: TCDecNumber): TCDecNumber;
var
  t: TCDecNumber;
begin
  t:=TCDecNumber.Create(Left);
  DecNumberAdd(@Result.fDec, @t.fDec, @Right.fDec, g_set);
end;

class operator TCDecNumber.Add(const Left: Integer;
  const Right: TCDecNumber): TCDecNumber;
var
  t: TCDecNumber;
begin
  t:=TCDecNumber.Create(Left);
  DecNumberAdd(@Result.fDec, @t.fDec, @Right.fDec, g_set);
end;

class operator TCDecNumber.Add(const Left: UInt64;
  const Right: TCDecNumber): TCDecNumber;
var
  t: TCDecNumber;
begin
  t:=TCDecNumber.Create(Left);
  DecNumberAdd(@Result.fDec, @t.fDec, @Right.fDec, g_set);
end;

class operator TCDecNumber.Subtract(const Left,
  Right: TCDecNumber): TCDecNumber;
begin
  DecNumberSubtract(@Result.fDec, @Left.fDec, @Right.fDec, g_set);
end;

class operator TCDecNumber.Subtract(const Left: TCDecNumber;
  const Right: UInt64): TCDecNumber;
var
  t: TCDecNumber;
begin
  t:=TCDecNumber.Create(Right);
  DecNumberSubtract(@Result.fDec, @Left.fDec, @t.fDec, g_set);
end;

class operator TCDecNumber.Subtract(const Left: TCDecNumber;
  const Right: Integer): TCDecNumber;
var
  t: TCDecNumber;
begin
  t:=TCDecNumber.Create(Right);
  DecNumberSubtract(@Result.fDec, @Left.fDec, @t.fDec, g_set);
end;

class operator TCDecNumber.Subtract(const Left: TCDecNumber;
  const Right: Cardinal): TCDecNumber;
var
  t: TCDecNumber;
begin
  t:=TCDecNumber.Create(Right);
  DecNumberSubtract(@Result.fDec, @Left.fDec, @t.fDec, g_set);
end;

class operator TCDecNumber.Subtract(const Left: TCDecNumber;
  const Right: Int64): TCDecNumber;
var
  t: TCDecNumber;
begin
  t:=TCDecNumber.Create(Right);
  DecNumberSubtract(@Result.fDec, @Left.fDec, @t.fDec, g_set);
end;

class operator TCDecNumber.Subtract(const Left: TCDecNumber;
  const Right: Extended): TCDecNumber;
var
  t: TCDecNumber;
begin
  t:=TCDecNumber.Create(Right);
  DecNumberSubtract(@Result.fDec, @Left.fDec, @t.fDec, g_set);
end;

class operator TCDecNumber.Subtract(const Left: TCDecNumber;
  const Right: Double): TCDecNumber;
var
  t: TCDecNumber;
begin
  t:=TCDecNumber.Create(Right);
  DecNumberSubtract(@Result.fDec, @Left.fDec, @t.fDec, g_set);
end;

class operator TCDecNumber.Subtract(const Left: TCDecNumber;
  const Right: Single): TCDecNumber;
var
  t: TCDecNumber;
begin
  t:=TCDecNumber.Create(Right);
  DecNumberSubtract(@Result.fDec, @Left.fDec, @t.fDec, g_set);
end;

class operator TCDecNumber.Subtract(const Left: UInt64;
  const Right: TCDecNumber): TCDecNumber;
var
  t: TCDecNumber;
begin
  t:=TCDecNumber.Create(Left);
  DecNumberSubtract(@Result.fDec, @t.fDec, @Right.fDec, g_set);
end;

class operator TCDecNumber.Subtract(const Left: Integer;
  const Right: TCDecNumber): TCDecNumber;
var
  t: TCDecNumber;
begin
  t:=TCDecNumber.Create(Left);
  DecNumberSubtract(@Result.fDec, @t.fDec, @Right.fDec, g_set);
end;

class operator TCDecNumber.Subtract(const Left: Cardinal;
  const Right: TCDecNumber): TCDecNumber;
var
  t: TCDecNumber;
begin
  t:=TCDecNumber.Create(Left);
  DecNumberSubtract(@Result.fDec, @t.fDec, @Right.fDec, g_set);
end;

class operator TCDecNumber.Subtract(const Left: Int64;
  const Right: TCDecNumber): TCDecNumber;
var
  t: TCDecNumber;
begin
  t:=TCDecNumber.Create(Left);
  DecNumberSubtract(@Result.fDec, @t.fDec, @Right.fDec, g_set);
end;

class operator TCDecNumber.Subtract(const Left: Extended;
  const Right: TCDecNumber): TCDecNumber;
var
  t: TCDecNumber;
begin
  t:=TCDecNumber.Create(Left);
  DecNumberSubtract(@Result.fDec, @t.fDec, @Right.fDec, g_set);
end;

class operator TCDecNumber.Subtract(const Left: Double;
  const Right: TCDecNumber): TCDecNumber;
var
  t: TCDecNumber;
begin
  t:=TCDecNumber.Create(Left);
  DecNumberSubtract(@Result.fDec, @t.fDec, @Right.fDec, g_set);
end;

class operator TCDecNumber.Subtract(const Left: Single;
  const Right: TCDecNumber): TCDecNumber;
var
  t: TCDecNumber;
begin
  t:=TCDecNumber.Create(Left);
  DecNumberSubtract(@Result.fDec, @t.fDec, @Right.fDec, g_set);
end;

class operator TCDecNumber.Multiply(const Left, Right: TCDecNumber): TCDecNumber;
begin
  DecNumberMultiply(@Result.fDec, @Left.fDec, @Right.fDec, g_set);
end;

class operator TCDecNumber.Multiply(const Left: TCDecNumber;
  const Right: Int64): TCDecNumber;
var
  t: TCDecNumber;
begin
  t:=TCDecNumber.Create(Right);
  DecNumberMultiply(@Result.fDec, @Left.fDec, @t.fDec, g_set);
end;

class operator TCDecNumber.Multiply(const Left: TCDecNumber;
  const Right: UInt64): TCDecNumber;
var
  t: TCDecNumber;
begin
  t:=TCDecNumber.Create(Right);
  DecNumberMultiply(@Result.fDec, @Left.fDec, @t.fDec, g_set);
end;

class operator TCDecNumber.Multiply(const Left: TCDecNumber;
  const Right: Integer): TCDecNumber;
var
  t: TCDecNumber;
begin
  t:=TCDecNumber.Create(Right);
  DecNumberMultiply(@Result.fDec, @Left.fDec, @t.fDec, g_set);
end;

class operator TCDecNumber.Multiply(const Left: TCDecNumber;
  const Right: Single): TCDecNumber;
var
  t: TCDecNumber;
begin
  t:=TCDecNumber.Create(Right);
  DecNumberMultiply(@Result.fDec, @Left.fDec, @t.fDec, g_set);
end;

class operator TCDecNumber.Multiply(const Left: TCDecNumber;
  const Right: Extended): TCDecNumber;
var
  t: TCDecNumber;
begin
  t:=TCDecNumber.Create(Right);
  DecNumberMultiply(@Result.fDec, @Left.fDec, @t.fDec, g_set);
end;

class operator TCDecNumber.Multiply(const Left: TCDecNumber;
  const Right: Double): TCDecNumber;
var
  t: TCDecNumber;
begin
  t:=TCDecNumber.Create(Right);
  DecNumberMultiply(@Result.fDec, @Left.fDec, @t.fDec, g_set);
end;

class operator TCDecNumber.Multiply(const Left: TCDecNumber;
  const Right: Cardinal): TCDecNumber;
var
  t: TCDecNumber;
begin
  t:=TCDecNumber.Create(Right);
  DecNumberMultiply(@Result.fDec, @Left.fDec, @t.fDec, g_set);
end;

class operator TCDecNumber.Multiply(const Left: UInt64;
  const Right: TCDecNumber): TCDecNumber;
var
  t: TCDecNumber;
begin
  t:=TCDecNumber.Create(Left);
  DecNumberMultiply(@Result.fDec, @t.fDec, @Right.fDec, g_set);
end;

class operator TCDecNumber.Multiply(const Left: Integer;
  const Right: TCDecNumber): TCDecNumber;
var
  t: TCDecNumber;
begin
  t:=TCDecNumber.Create(Left);
  DecNumberMultiply(@Result.fDec, @t.fDec, @Right.fDec, g_set);
end;

class operator TCDecNumber.Multiply(const Left: Cardinal;
  const Right: TCDecNumber): TCDecNumber;
var
  t: TCDecNumber;
begin
  t:=TCDecNumber.Create(Left);
  DecNumberMultiply(@Result.fDec, @t.fDec, @Right.fDec, g_set);
end;

class operator TCDecNumber.Multiply(const Left: Int64;
  const Right: TCDecNumber): TCDecNumber;
var
  t: TCDecNumber;
begin
  t:=TCDecNumber.Create(Left);
  DecNumberMultiply(@Result.fDec, @t.fDec, @Right.fDec, g_set);
end;

class operator TCDecNumber.Multiply(const Left: Extended;
  const Right: TCDecNumber): TCDecNumber;
var
  t: TCDecNumber;
begin
  t:=TCDecNumber.Create(Left);
  DecNumberMultiply(@Result.fDec, @t.fDec, @Right.fDec, g_set);
end;

class operator TCDecNumber.Multiply(const Left: Double;
  const Right: TCDecNumber): TCDecNumber;
var
  t: TCDecNumber;
begin
  t:=TCDecNumber.Create(Left);
  DecNumberMultiply(@Result.fDec, @t.fDec, @Right.fDec, g_set);
end;

class operator TCDecNumber.Multiply(const Left: Single;
  const Right: TCDecNumber): TCDecNumber;
var
  t: TCDecNumber;
begin
  t:=TCDecNumber.Create(Left);
  DecNumberMultiply(@Result.fDec, @t.fDec, @Right.fDec, g_set);
end;

class operator TCDecNumber.Divide(const Left,
  Right: TCDecNumber): TCDecNumber;
begin
  DecNumberDivide(@Result.fDec, @Left.fDec, @Right.fDec, g_set);
end;

class operator TCDecNumber.Divide(const Left: TCDecNumber;
  const Right: UInt64): TCDecNumber;
var
  t: TCDecNumber;
begin
  t:=TCDecNumber.Create(Right);
  DecNumberDivide(@Result.fDec, @Left.fDec, @t.fDec, g_set);
end;

class operator TCDecNumber.Divide(const Left: TCDecNumber;
  const Right: Integer): TCDecNumber;
var
  t: TCDecNumber;
begin
  t:=TCDecNumber.Create(Right);
  DecNumberDivide(@Result.fDec, @Left.fDec, @t.fDec, g_set);
end;

class operator TCDecNumber.Divide(const Left: TCDecNumber;
  const Right: Cardinal): TCDecNumber;
var
  t: TCDecNumber;
begin
  t:=TCDecNumber.Create(Right);
  DecNumberDivide(@Result.fDec, @Left.fDec, @t.fDec, g_set);
end;

class operator TCDecNumber.Divide(const Left: TCDecNumber;
  const Right: Int64): TCDecNumber;
var
  t: TCDecNumber;
begin
  t:=TCDecNumber.Create(Right);
  DecNumberDivide(@Result.fDec, @Left.fDec, @t.fDec, g_set);
end;

class operator TCDecNumber.Divide(const Left: TCDecNumber;
  const Right: Extended): TCDecNumber;
var
  t: TCDecNumber;
begin
  t:=TCDecNumber.Create(Right);
  DecNumberDivide(@Result.fDec, @Left.fDec, @t.fDec, g_set);
end;

class operator TCDecNumber.Divide(const Left: TCDecNumber;
  const Right: Double): TCDecNumber;
var
  t: TCDecNumber;
begin
  t:=TCDecNumber.Create(Right);
  DecNumberDivide(@Result.fDec, @Left.fDec, @t.fDec, g_set);
end;

class operator TCDecNumber.Divide(const Left: TCDecNumber;
  const Right: Single): TCDecNumber;
var
  t: TCDecNumber;
begin
  t:=TCDecNumber.Create(Right);
  DecNumberDivide(@Result.fDec, @Left.fDec, @t.fDec, g_set);
end;

class operator TCDecNumber.Divide(const Left: UInt64;
  const Right: TCDecNumber): TCDecNumber;
var
  t: TCDecNumber;
begin
  t:=TCDecNumber.Create(Left);
  DecNumberDivide(@Result.fDec, @t.fDec, @Right.fDec, g_set);
end;

class operator TCDecNumber.Divide(const Left: Integer;
  const Right: TCDecNumber): TCDecNumber;
var
  t: TCDecNumber;
begin
  t:=TCDecNumber.Create(Left);
  DecNumberDivide(@Result.fDec, @t.fDec, @Right.fDec, g_set);
end;

class operator TCDecNumber.Divide(const Left: Cardinal;
  const Right: TCDecNumber): TCDecNumber;
var
  t: TCDecNumber;
begin
  t:=TCDecNumber.Create(Left);
  DecNumberDivide(@Result.fDec, @t.fDec, @Right.fDec, g_set);
end;

class operator TCDecNumber.Divide(const Left: Int64;
  const Right: TCDecNumber): TCDecNumber;
var
  t: TCDecNumber;
begin
  t:=TCDecNumber.Create(Left);
  DecNumberDivide(@Result.fDec, @t.fDec, @Right.fDec, g_set);
end;

class operator TCDecNumber.Divide(const Left: Extended;
  const Right: TCDecNumber): TCDecNumber;
var
  t: TCDecNumber;
begin
  t:=TCDecNumber.Create(Left);
  DecNumberDivide(@Result.fDec, @t.fDec, @Right.fDec, g_set);
end;

class operator TCDecNumber.Divide(const Left: Double;
  const Right: TCDecNumber): TCDecNumber;
var
  t: TCDecNumber;
begin
  t:=TCDecNumber.Create(Left);
  DecNumberDivide(@Result.fDec, @t.fDec, @Right.fDec, g_set);
end;

class operator TCDecNumber.Divide(const Left: Single;
  const Right: TCDecNumber): TCDecNumber;
var
  t: TCDecNumber;
begin
  t:=TCDecNumber.Create(Left);
  DecNumberDivide(@Result.fDec, @t.fDec, @Right.fDec, g_set);
end;

class operator TCDecNumber.Equal(const Left, Right: TCDecNumber): Boolean;
begin
  result:=DecNumberCompareInt(@Left.fDec,@Right.fDec,g_set)=0;
end;

class operator TCDecNumber.Equal(const Left: TCDecNumber;
  const Right: UInt64): Boolean;
var
  t: TCDecNumber;
begin
  t:=TCDecNumber.Create(Right);
  result:=DecNumberCompareInt(@Left.fDec,@t.fDec,g_set)=0;
end;

class operator TCDecNumber.Equal(const Left: TCDecNumber;
  const Right: Integer): Boolean;
var
  t: TCDecNumber;
begin
  t:=TCDecNumber.Create(Right);
  result:=DecNumberCompareInt(@Left.fDec,@t.fDec,g_set)=0;
end;

class operator TCDecNumber.Equal(const Left: TCDecNumber;
  const Right: Cardinal): Boolean;
var
  t: TCDecNumber;
begin
  t:=TCDecNumber.Create(Right);
  result:=DecNumberCompareInt(@Left.fDec,@t.fDec,g_set)=0;
end;

class operator TCDecNumber.Equal(const Left: TCDecNumber;
  const Right: Int64): Boolean;
var
  t: TCDecNumber;
begin
  t:=TCDecNumber.Create(Right);
  result:=DecNumberCompareInt(@Left.fDec,@t.fDec,g_set)=0;
end;

class operator TCDecNumber.Equal(const Left: TCDecNumber;
  const Right: Extended): Boolean;
var
  t: TCDecNumber;
begin
  t:=TCDecNumber.Create(Right);
  result:=DecNumberCompareInt(@Left.fDec,@t.fDec,g_set)=0;
end;

class operator TCDecNumber.Equal(const Left: TCDecNumber;
  const Right: Double): Boolean;
var
  t: TCDecNumber;
begin
  t:=TCDecNumber.Create(Right);
  result:=DecNumberCompareInt(@Left.fDec,@t.fDec,g_set)=0;
end;

class operator TCDecNumber.Equal(const Left: TCDecNumber;
  const Right: Single): Boolean;
var
  t: TCDecNumber;
begin
  t:=TCDecNumber.Create(Right);
  result:=DecNumberCompareInt(@Left.fDec,@t.fDec,g_set)=0;
end;

class operator TCDecNumber.Equal(const Left: UInt64;
  const Right: TCDecNumber): Boolean;
var
  t: TCDecNumber;
begin
  t:=TCDecNumber.Create(Left);
  result:=DecNumberCompareInt(@t.fDec,@Right.fDec,g_set)=0;
end;

class operator TCDecNumber.Equal(const Left: Integer;
  const Right: TCDecNumber): Boolean;
var
  t: TCDecNumber;
begin
  t:=TCDecNumber.Create(Left);
  result:=DecNumberCompareInt(@t.fDec,@Right.fDec,g_set)=0;
end;

class operator TCDecNumber.Equal(const Left: Cardinal;
  const Right: TCDecNumber): Boolean;
var
  t: TCDecNumber;
begin
  t:=TCDecNumber.Create(Left);
  result:=DecNumberCompareInt(@t.fDec,@Right.fDec,g_set)=0;
end;

class operator TCDecNumber.Equal(const Left: Int64;
  const Right: TCDecNumber): Boolean;
var
  t: TCDecNumber;
begin
  t:=TCDecNumber.Create(Left);
  result:=DecNumberCompareInt(@t.fDec,@Right.fDec,g_set)=0;
end;

class operator TCDecNumber.Equal(const Left: Extended;
  const Right: TCDecNumber): Boolean;
var
  t: TCDecNumber;
begin
  t:=TCDecNumber.Create(Left);
  result:=DecNumberCompareInt(@t.fDec,@Right.fDec,g_set)=0;
end;

class operator TCDecNumber.Equal(const Left: Double;
  const Right: TCDecNumber): Boolean;
var
  t: TCDecNumber;
begin
  t:=TCDecNumber.Create(Left);
  result:=DecNumberCompareInt(@t.fDec,@Right.fDec,g_set)=0;
end;

class operator TCDecNumber.Equal(const Left: Single;
  const Right: TCDecNumber): Boolean;
var
  t: TCDecNumber;
begin
  t:=TCDecNumber.Create(Left);
  result:=DecNumberCompareInt(@t.fDec,@Right.fDec,g_set)=0;
end;

class operator TCDecNumber.NotEqual(const Left, Right: TCDecNumber): Boolean;
begin
  result:=DecNumberCompareInt(@Left.fDec,@Right.fDec,g_set)<>0;
end;


class operator TCDecNumber.NotEqual(const Left: TCDecNumber;
  const Right: UInt64): Boolean;
var
  t: TCDecNumber;
begin
  t:=TCDecNumber.Create(Right);
  result:=DecNumberCompareInt(@Left.fDec,@t.fDec,g_set)<>0;
end;

class operator TCDecNumber.NotEqual(const Left: TCDecNumber;
  const Right: Integer): Boolean;
var
  t: TCDecNumber;
begin
  t:=TCDecNumber.Create(Right);
  result:=DecNumberCompareInt(@Left.fDec,@t.fDec,g_set)<>0;
end;

class operator TCDecNumber.NotEqual(const Left: TCDecNumber;
  const Right: Cardinal): Boolean;
var
  t: TCDecNumber;
begin
  t:=TCDecNumber.Create(Right);
  result:=DecNumberCompareInt(@Left.fDec,@t.fDec,g_set)<>0;
end;

class operator TCDecNumber.NotEqual(const Left: TCDecNumber;
  const Right: Int64): Boolean;
var
  t: TCDecNumber;
begin
  t:=TCDecNumber.Create(Right);
  result:=DecNumberCompareInt(@Left.fDec,@t.fDec,g_set)<>0;
end;

class operator TCDecNumber.NotEqual(const Left: TCDecNumber;
  const Right: Extended): Boolean;
var
  t: TCDecNumber;
begin
  t:=TCDecNumber.Create(Right);
  result:=DecNumberCompareInt(@Left.fDec,@t.fDec,g_set)<>0;
end;

class operator TCDecNumber.NotEqual(const Left: TCDecNumber;
  const Right: Double): Boolean;
var
  t: TCDecNumber;
begin
  t:=TCDecNumber.Create(Right);
  result:=DecNumberCompareInt(@Left.fDec,@t.fDec,g_set)<>0;
end;

class operator TCDecNumber.NotEqual(const Left: TCDecNumber;
  const Right: Single): Boolean;
var
  t: TCDecNumber;
begin
  t:=TCDecNumber.Create(Right);
  result:=DecNumberCompareInt(@Left.fDec,@t.fDec,g_set)<>0;
end;

class operator TCDecNumber.NotEqual(const Left: UInt64;
  const Right: TCDecNumber): Boolean;
var
  t: TCDecNumber;
begin
  t:=TCDecNumber.Create(Left);
  result:=DecNumberCompareInt(@t.fDec,@Right.fDec,g_set)=0;
end;

class operator TCDecNumber.NotEqual(const Left: Integer;
  const Right: TCDecNumber): Boolean;
var
  t: TCDecNumber;
begin
  t:=TCDecNumber.Create(Left);
  result:=DecNumberCompareInt(@t.fDec,@Right.fDec,g_set)=0;
end;

class operator TCDecNumber.NotEqual(const Left: Cardinal;
  const Right: TCDecNumber): Boolean;
var
  t: TCDecNumber;
begin
  t:=TCDecNumber.Create(Left);
  result:=DecNumberCompareInt(@t.fDec,@Right.fDec,g_set)=0;
end;

class operator TCDecNumber.NotEqual(const Left: Int64;
  const Right: TCDecNumber): Boolean;
var
  t: TCDecNumber;
begin
  t:=TCDecNumber.Create(Left);
  result:=DecNumberCompareInt(@t.fDec,@Right.fDec,g_set)=0;
end;

class operator TCDecNumber.NotEqual(const Left: Extended;
  const Right: TCDecNumber): Boolean;
var
  t: TCDecNumber;
begin
  t:=TCDecNumber.Create(Left);
  result:=DecNumberCompareInt(@t.fDec,@Right.fDec,g_set)=0;
end;

class operator TCDecNumber.NotEqual(const Left: Double;
  const Right: TCDecNumber): Boolean;
var
  t: TCDecNumber;
begin
  t:=TCDecNumber.Create(Left);
  result:=DecNumberCompareInt(@t.fDec,@Right.fDec,g_set)=0;
end;

class operator TCDecNumber.NotEqual(const Left: Single;
  const Right: TCDecNumber): Boolean;
var
  t: TCDecNumber;
begin
  t:=TCDecNumber.Create(Left);
  result:=DecNumberCompareInt(@t.fDec,@Right.fDec,g_set)=0;
end;

class operator TCDecNumber.GreaterThan(const Left, Right: TCDecNumber): Boolean;
begin
  result:=DecNumberCompareInt(@Left.fDec,@Right.fDec,g_set)>0;
end;

class operator TCDecNumber.GreaterThan(const Left: TCDecNumber;
  const Right: UInt64): Boolean;
var
  t: TCDecNumber;
begin
  t:=TCDecNumber.Create(Right);
  result:=DecNumberCompareInt(@Left.fDec,@t.fDec,g_set)>0;
end;

class operator TCDecNumber.GreaterThan(const Left: TCDecNumber;
  const Right: Integer): Boolean;
var
  t: TCDecNumber;
begin
  t:=TCDecNumber.Create(Right);
  result:=DecNumberCompareInt(@Left.fDec,@t.fDec,g_set)>0;
end;

class operator TCDecNumber.GreaterThan(const Left: TCDecNumber;
  const Right: Cardinal): Boolean;
var
  t: TCDecNumber;
begin
  t:=TCDecNumber.Create(Right);
  result:=DecNumberCompareInt(@Left.fDec,@t.fDec,g_set)>0;
end;

class operator TCDecNumber.GreaterThan(const Left: TCDecNumber;
  const Right: Int64): Boolean;
var
  t: TCDecNumber;
begin
  t:=TCDecNumber.Create(Right);
  result:=DecNumberCompareInt(@Left.fDec,@t.fDec,g_set)>0;
end;

class operator TCDecNumber.GreaterThan(const Left: TCDecNumber;
  const Right: Extended): Boolean;
var
  t: TCDecNumber;
begin
  t:=TCDecNumber.Create(Right);
  result:=DecNumberCompareInt(@Left.fDec,@t.fDec,g_set)>0;
end;

class operator TCDecNumber.GreaterThan(const Left: TCDecNumber;
  const Right: Double): Boolean;
var
  t: TCDecNumber;
begin
  t:=TCDecNumber.Create(Right);
  result:=DecNumberCompareInt(@Left.fDec,@t.fDec,g_set)>0;
end;

class operator TCDecNumber.GreaterThan(const Left: TCDecNumber;
  const Right: Single): Boolean;
var
  t: TCDecNumber;
begin
  t:=TCDecNumber.Create(Right);
  result:=DecNumberCompareInt(@Left.fDec,@t.fDec,g_set)>0;
end;

class operator TCDecNumber.GreaterThan(const Left: UInt64;
  const Right: TCDecNumber): Boolean;
var
  t: TCDecNumber;
begin
  t:=TCDecNumber.Create(Left);
  result:=DecNumberCompareInt(@t.fDec,@Right.fDec,g_set)>0;
end;

class operator TCDecNumber.GreaterThan(const Left: Integer;
  const Right: TCDecNumber): Boolean;
var
  t: TCDecNumber;
begin
  t:=TCDecNumber.Create(Left);
  result:=DecNumberCompareInt(@t.fDec,@Right.fDec,g_set)>0;
end;

class operator TCDecNumber.GreaterThan(const Left: Cardinal;
  const Right: TCDecNumber): Boolean;
var
  t: TCDecNumber;
begin
  t:=TCDecNumber.Create(Left);
  result:=DecNumberCompareInt(@t.fDec,@Right.fDec,g_set)>0;
end;

class operator TCDecNumber.GreaterThan(const Left: Int64;
  const Right: TCDecNumber): Boolean;
var
  t: TCDecNumber;
begin
  t:=TCDecNumber.Create(Left);
  result:=DecNumberCompareInt(@t.fDec,@Right.fDec,g_set)>0;
end;

class operator TCDecNumber.GreaterThan(const Left: Extended;
  const Right: TCDecNumber): Boolean;
var
  t: TCDecNumber;
begin
  t:=TCDecNumber.Create(Left);
  result:=DecNumberCompareInt(@t.fDec,@Right.fDec,g_set)>0;
end;

class operator TCDecNumber.GreaterThan(const Left: Double;
  const Right: TCDecNumber): Boolean;
var
  t: TCDecNumber;
begin
  t:=TCDecNumber.Create(Left);
  result:=DecNumberCompareInt(@t.fDec,@Right.fDec,g_set)>0;
end;

class operator TCDecNumber.GreaterThan(const Left: Single;
  const Right: TCDecNumber): Boolean;
var
  t: TCDecNumber;
begin
  t:=TCDecNumber.Create(Left);
  result:=DecNumberCompareInt(@t.fDec,@Right.fDec,g_set)>0;
end;

class operator TCDecNumber.GreaterThanOrEqual(const Left,
  Right: TCDecNumber): Boolean;
begin
  result:=DecNumberCompareInt(@Left.fDec,@Right.fDec,g_set)>=0;
end;

class operator TCDecNumber.GreaterThanOrEqual(const Left: TCDecNumber;
  const Right: UInt64): Boolean;
var
  t: TCDecNumber;
begin
  t:=TCDecNumber.Create(Right);
  result:=DecNumberCompareInt(@Left.fDec,@t.fDec,g_set)>=0;
end;

class operator TCDecNumber.GreaterThanOrEqual(const Left: TCDecNumber;
  const Right: Integer): Boolean;
var
  t: TCDecNumber;
begin
  t:=TCDecNumber.Create(Right);
  result:=DecNumberCompareInt(@Left.fDec,@t.fDec,g_set)>=0;
end;

class operator TCDecNumber.GreaterThanOrEqual(const Left: TCDecNumber;
  const Right: Cardinal): Boolean;
var
  t: TCDecNumber;
begin
  t:=TCDecNumber.Create(Right);
  result:=DecNumberCompareInt(@Left.fDec,@t.fDec,g_set)>=0;
end;

class operator TCDecNumber.GreaterThanOrEqual(const Left: TCDecNumber;
  const Right: Int64): Boolean;
var
  t: TCDecNumber;
begin
  t:=TCDecNumber.Create(Right);
  result:=DecNumberCompareInt(@Left.fDec,@t.fDec,g_set)>=0;
end;

class operator TCDecNumber.GreaterThanOrEqual(const Left: TCDecNumber;
  const Right: Extended): Boolean;
var
  t: TCDecNumber;
begin
  t:=TCDecNumber.Create(Right);
  result:=DecNumberCompareInt(@Left.fDec,@t.fDec,g_set)>=0;
end;

class operator TCDecNumber.GreaterThanOrEqual(const Left: TCDecNumber;
  const Right: Double): Boolean;
var
  t: TCDecNumber;
begin
  t:=TCDecNumber.Create(Right);
  result:=DecNumberCompareInt(@Left.fDec,@t.fDec,g_set)>=0;
end;

class operator TCDecNumber.GreaterThanOrEqual(const Left: TCDecNumber;
  const Right: Single): Boolean;
var
  t: TCDecNumber;
begin
  t:=TCDecNumber.Create(Right);
  result:=DecNumberCompareInt(@Left.fDec,@t.fDec,g_set)>=0;
end;

class operator TCDecNumber.GreaterThanOrEqual(const Left: UInt64;
  const Right: TCDecNumber): Boolean;
var
  t: TCDecNumber;
begin
  t:=TCDecNumber.Create(Left);
  result:=DecNumberCompareInt(@t.fDec,@Right.fDec,g_set)>=0;
end;

class operator TCDecNumber.GreaterThanOrEqual(const Left: Integer;
  const Right: TCDecNumber): Boolean;
var
  t: TCDecNumber;
begin
  t:=TCDecNumber.Create(Left);
  result:=DecNumberCompareInt(@t.fDec,@Right.fDec,g_set)>=0;
end;

class operator TCDecNumber.GreaterThanOrEqual(const Left: Cardinal;
  const Right: TCDecNumber): Boolean;
var
  t: TCDecNumber;
begin
  t:=TCDecNumber.Create(Left);
  result:=DecNumberCompareInt(@t.fDec,@Right.fDec,g_set)>=0;
end;

class operator TCDecNumber.GreaterThanOrEqual(const Left: Int64;
  const Right: TCDecNumber): Boolean;
var
  t: TCDecNumber;
begin
  t:=TCDecNumber.Create(Left);
  result:=DecNumberCompareInt(@t.fDec,@Right.fDec,g_set)>=0;
end;

class operator TCDecNumber.GreaterThanOrEqual(const Left: Extended;
  const Right: TCDecNumber): Boolean;
var
  t: TCDecNumber;
begin
  t:=TCDecNumber.Create(Left);
  result:=DecNumberCompareInt(@t.fDec,@Right.fDec,g_set)>=0;
end;

class operator TCDecNumber.GreaterThanOrEqual(const Left: Double;
  const Right: TCDecNumber): Boolean;
var
  t: TCDecNumber;
begin
  t:=TCDecNumber.Create(Left);
  result:=DecNumberCompareInt(@t.fDec,@Right.fDec,g_set)>=0;
end;

class operator TCDecNumber.GreaterThanOrEqual(const Left: Single;
  const Right: TCDecNumber): Boolean;
var
  t: TCDecNumber;
begin
  t:=TCDecNumber.Create(Left);
  result:=DecNumberCompareInt(@t.fDec,@Right.fDec,g_set)>=0;
end;

class operator TCDecNumber.LessThan(const Left,
  Right: TCDecNumber): Boolean;
begin
  result:=DecNumberCompareInt(@Left.fDec,@Right.fDec,g_set)<0;
end;

class operator TCDecNumber.LessThan(const Left: TCDecNumber;
  const Right: UInt64): Boolean;
var
  t: TCDecNumber;
begin
  t:=TCDecNumber.Create(Right);
  result:=DecNumberCompareInt(@Left.fDec,@t.fDec,g_set)<0;
end;

class operator TCDecNumber.LessThan(const Left: TCDecNumber;
  const Right: Integer): Boolean;
var
  t: TCDecNumber;
begin
  t:=TCDecNumber.Create(Right);
  result:=DecNumberCompareInt(@Left.fDec,@t.fDec,g_set)<0;
end;

class operator TCDecNumber.LessThan(const Left: TCDecNumber;
  const Right: Cardinal): Boolean;
var
  t: TCDecNumber;
begin
  t:=TCDecNumber.Create(Right);
  result:=DecNumberCompareInt(@Left.fDec,@t.fDec,g_set)<0;
end;

class operator TCDecNumber.LessThan(const Left: TCDecNumber;
  const Right: Int64): Boolean;
var
  t: TCDecNumber;
begin
  t:=TCDecNumber.Create(Right);
  result:=DecNumberCompareInt(@Left.fDec,@t.fDec,g_set)<0;
end;

class operator TCDecNumber.LessThan(const Left: TCDecNumber;
  const Right: Extended): Boolean;
var
  t: TCDecNumber;
begin
  t:=TCDecNumber.Create(Right);
  result:=DecNumberCompareInt(@Left.fDec,@t.fDec,g_set)<0;
end;

class operator TCDecNumber.LessThan(const Left: TCDecNumber;
  const Right: Double): Boolean;
var
  t: TCDecNumber;
begin
  t:=TCDecNumber.Create(Right);
  result:=DecNumberCompareInt(@Left.fDec,@t.fDec,g_set)<0;
end;

class operator TCDecNumber.LessThan(const Left: TCDecNumber;
  const Right: Single): Boolean;
var
  t: TCDecNumber;
begin
  t:=TCDecNumber.Create(Right);
  result:=DecNumberCompareInt(@Left.fDec,@t.fDec,g_set)<0;
end;

class operator TCDecNumber.LessThan(const Left: UInt64;
  const Right: TCDecNumber): Boolean;
var
  t: TCDecNumber;
begin
  t:=TCDecNumber.Create(Left);
  result:=DecNumberCompareInt(@t.fDec,@Right.fDec,g_set)<0;
end;

class operator TCDecNumber.LessThan(const Left: Integer;
  const Right: TCDecNumber): Boolean;
var
  t: TCDecNumber;
begin
  t:=TCDecNumber.Create(Left);
  result:=DecNumberCompareInt(@t.fDec,@Right.fDec,g_set)<0;
end;

class operator TCDecNumber.LessThan(const Left: Cardinal;
  const Right: TCDecNumber): Boolean;
var
  t: TCDecNumber;
begin
  t:=TCDecNumber.Create(Left);
  result:=DecNumberCompareInt(@t.fDec,@Right.fDec,g_set)<0;
end;

class operator TCDecNumber.LessThan(const Left: Int64;
  const Right: TCDecNumber): Boolean;
var
  t: TCDecNumber;
begin
  t:=TCDecNumber.Create(Left);
  result:=DecNumberCompareInt(@t.fDec,@Right.fDec,g_set)<0;
end;

class operator TCDecNumber.LessThan(const Left: Extended;
  const Right: TCDecNumber): Boolean;
var
  t: TCDecNumber;
begin
  t:=TCDecNumber.Create(Left);
  result:=DecNumberCompareInt(@t.fDec,@Right.fDec,g_set)<0;
end;

class operator TCDecNumber.LessThan(const Left: Double;
  const Right: TCDecNumber): Boolean;
var
  t: TCDecNumber;
begin
  t:=TCDecNumber.Create(Left);
  result:=DecNumberCompareInt(@t.fDec,@Right.fDec,g_set)<0;
end;

class operator TCDecNumber.LessThan(const Left: Single;
  const Right: TCDecNumber): Boolean;
var
  t: TCDecNumber;
begin
  t:=TCDecNumber.Create(Left);
  result:=DecNumberCompareInt(@t.fDec,@Right.fDec,g_set)<0;
end;

class operator TCDecNumber.LessThanOrEqual(const Left,
  Right: TCDecNumber): Boolean;
begin
  result:=DecNumberCompareInt(@Left.fDec,@Right.fDec,g_set)<=0;
end;

class operator TCDecNumber.LessThanOrEqual(const Left: TCDecNumber;
  const Right: UInt64): Boolean;
var
  t: TCDecNumber;
begin
  t:=TCDecNumber.Create(Right);
  result:=DecNumberCompareInt(@Left.fDec,@t.fDec,g_set)<=0;
end;

class operator TCDecNumber.LessThanOrEqual(const Left: TCDecNumber;
  const Right: Integer): Boolean;
var
  t: TCDecNumber;
begin
  t:=TCDecNumber.Create(Right);
  result:=DecNumberCompareInt(@Left.fDec,@t.fDec,g_set)<=0;
end;

class operator TCDecNumber.LessThanOrEqual(const Left: TCDecNumber;
  const Right: Cardinal): Boolean;
var
  t: TCDecNumber;
begin
  t:=TCDecNumber.Create(Right);
  result:=DecNumberCompareInt(@Left.fDec,@t.fDec,g_set)<=0;
end;

class operator TCDecNumber.LessThanOrEqual(const Left: TCDecNumber;
  const Right: Int64): Boolean;
var
  t: TCDecNumber;
begin
  t:=TCDecNumber.Create(Right);
  result:=DecNumberCompareInt(@Left.fDec,@t.fDec,g_set)<=0;
end;

class operator TCDecNumber.LessThanOrEqual(const Left: TCDecNumber;
  const Right: Extended): Boolean;
var
  t: TCDecNumber;
begin
  t:=TCDecNumber.Create(Right);
  result:=DecNumberCompareInt(@Left.fDec,@t.fDec,g_set)<=0;
end;

class operator TCDecNumber.LessThanOrEqual(const Left: TCDecNumber;
  const Right: Double): Boolean;
var
  t: TCDecNumber;
begin
  t:=TCDecNumber.Create(Right);
  result:=DecNumberCompareInt(@Left.fDec,@t.fDec,g_set)<=0;
end;

class operator TCDecNumber.LessThanOrEqual(const Left: TCDecNumber;
  const Right: Single): Boolean;
var
  t: TCDecNumber;
begin
  t:=TCDecNumber.Create(Right);
  result:=DecNumberCompareInt(@Left.fDec,@t.fDec,g_set)<=0;
end;

class operator TCDecNumber.LessThanOrEqual(const Left: UInt64;
  const Right: TCDecNumber): Boolean;
var
  t: TCDecNumber;
begin
  t:=TCDecNumber.Create(Left);
  result:=DecNumberCompareInt(@t.fDec,@Right.fDec,g_set)<=0;
end;

class operator TCDecNumber.LessThanOrEqual(const Left: Integer;
  const Right: TCDecNumber): Boolean;
var
  t: TCDecNumber;
begin
  t:=TCDecNumber.Create(Left);
  result:=DecNumberCompareInt(@t.fDec,@Right.fDec,g_set)<=0;
end;

class operator TCDecNumber.LessThanOrEqual(const Left: Cardinal;
  const Right: TCDecNumber): Boolean;
var
  t: TCDecNumber;
begin
  t:=TCDecNumber.Create(Left);
  result:=DecNumberCompareInt(@t.fDec,@Right.fDec,g_set)<=0;
end;

class operator TCDecNumber.LessThanOrEqual(const Left: Int64;
  const Right: TCDecNumber): Boolean;
var
  t: TCDecNumber;
begin
  t:=TCDecNumber.Create(Left);
  result:=DecNumberCompareInt(@t.fDec,@Right.fDec,g_set)<=0;
end;

class operator TCDecNumber.LessThanOrEqual(const Left: Extended;
  const Right: TCDecNumber): Boolean;
var
  t: TCDecNumber;
begin
  t:=TCDecNumber.Create(Left);
  result:=DecNumberCompareInt(@t.fDec,@Right.fDec,g_set)<=0;
end;

class operator TCDecNumber.LessThanOrEqual(const Left: Double;
  const Right: TCDecNumber): Boolean;
var
  t: TCDecNumber;
begin
  t:=TCDecNumber.Create(Left);
  result:=DecNumberCompareInt(@t.fDec,@Right.fDec,g_set)<=0;
end;

class operator TCDecNumber.LessThanOrEqual(const Left: Single;
  const Right: TCDecNumber): Boolean;
var
  t: TCDecNumber;
begin
  t:=TCDecNumber.Create(Left);
  result:=DecNumberCompareInt(@t.fDec,@Right.fDec,g_set)<=0;
end;

{ TCDecComplex }

constructor TCDecNumberComplex.Create(const aRe: TCDecNumber);
begin
  Re.Create(aRe);
  Im.Create(0);
end;

constructor TCDecNumberComplex.Create(const aRe, aIm: TCDecNumber);
begin
  Re.Create(aRe);
  Im.Create(aIm);
end;

constructor TCDecNumberComplex.Create(const aRe: Extended);
begin
  Re.Create(aRe);
  Im.Create(0);
end;

constructor TCDecNumberComplex.Create(const aRe, aIm: Extended);
begin
  Re.Create(aRe);
  Im.Create(aIm);
end;

constructor TCDecNumberComplex.Create(const aRe: Double);
begin
  Re.Create(aRe);
  Im.Create(0);
end;

constructor TCDecNumberComplex.Create(const aRe, aIm: Double);
begin
  Re.Create(aRe);
  Im.Create(aIm);
end;

constructor TCDecNumberComplex.Create(const aRe: Single);
begin
  Re.Create(aRe);
  Im.Create(0);
end;

constructor TCDecNumberComplex.Create(const aRe, aIm: Single);
begin
  Re.Create(aRe);
  Im.Create(aIm);
end;

constructor TCDecNumberComplex.Create(const aRe: Int64);
begin
  Re.Create(aRe);
  Im.Create(0);
end;

constructor TCDecNumberComplex.Create(const aRe, aIm: Int64);
begin
  Re.Create(aRe);
  Im.Create(aIm);
end;

constructor TCDecNumberComplex.Create(const aRe: UInt64);
begin
  Re.Create(aRe);
  Im.Create(0);
end;

constructor TCDecNumberComplex.Create(const aRe, aIm: UInt64);
begin
  Re.Create(aRe);
  Im.Create(aIm);
end;

constructor TCDecNumberComplex.Create(const aRe: Integer);
begin
  Re.Create(aRe);
  Im.Create(0);
end;

constructor TCDecNumberComplex.Create(const aRe, aIm: Integer);
begin
  Re.Create(aRe);
  Im.Create(aIm);
end;

constructor TCDecNumberComplex.Create(const aRe: Cardinal);
begin
  Re.Create(aRe);
  Im.Create(0);
end;

constructor TCDecNumberComplex.Create(const aRe, aIm: Cardinal);
begin
  Re.Create(aRe);
  Im.Create(aIm);
end;

constructor TCDecNumberComplex.Create(const aRe: AnsiString);
begin
  Re.Create(aRe);
  Im.Create(0);
end;

constructor TCDecNumberComplex.Create(const aRe, aIm: AnsiString);
begin
  Re.Create(aRe);
  Im.Create(aIm);
end;

class operator TCDecNumberComplex.Implicit(const Value: Integer): TCDecNumberComplex;
begin
  Result.Create(Value);
end;

class operator TCDecNumberComplex.Add(const Left, Right: TCDecNumberComplex): TCDecNumberComplex;
begin
  Result.Create(Left.Re + Right.Re, Left.Im + Right.Im);
end;

class operator TCDecNumberComplex.Subtract(const Left, Right: TCDecNumberComplex): TCDecNumberComplex;
begin
  Result.Create(Left.Re - Right.Re, Left.Im - Right.Im);
end;

class operator TCDecNumberComplex.Multiply(const Left, Right: TCDecNumberComplex): TCDecNumberComplex;
begin
  Result.Create((Left.Re * Right.Re) - (Left.Im * Right.Im), (Left.Re * Right.Im) + (Left.Im * Right.Re));
end;

class operator TCDecNumberComplex.Divide(const Left, Right: TCDecNumberComplex): TCDecNumberComplex;
var
  Denom: TCDecNumber;
begin
  Denom := (Right.Re * Right.Re) + (Right.Im * Right.Im);
  if Denom.IsZero then
    ZeroDivideError;
  Result.Create(((Left.Re * Right.Re) + (Left.Im * Right.Im)) / Denom,
    ((Left.Im * Right.Re) - (Left.Re * Right.Im)) / Denom);
end;



initialization
  decContextDefault(g_set, DEC_INIT_BASE);
  g_set.digits:=DECNUMDIGITS;
  g_set.round:=DEC_ROUND_DOWN;

end.


unit uLZMA;

{
Translate original C++ source codes to Object Pascal Vasily Makarov

LZMA SDK   Copyright (C) 1999-2006 Igor Pavlov

LZMA SDK provides the documentation, samples, header files, libraries,
and tools you need to develop applications that use LZMA compression.

LZMA is default and general compression method of 7z format
in 7-Zip compression program (www.7-zip.org). LZMA provides high
compression ratio and very fast decompression.

LZMA is an improved version of famous LZ77 compression algorithm.
It was improved in way of maximum increasing of compression ratio,
keeping high decompression speed and low memory requirements for
decompressing.



LICENSE
-------

LZMA SDK is available under any of the following licenses:

1) GNU Lesser General Public License (GNU LGPL)
2) Common Public License (CPL)
3) Simplified license for unmodified code (read SPECIAL EXCEPTION)
4) Proprietary license

It means that you can select one of these four options and follow rules of that license.


1,2) GNU LGPL and CPL licenses are pretty similar and both these
licenses are classified as
 - "Free software licenses" at http://www.gnu.org/
 - "OSI-approved" at http://www.opensource.org/


3) SPECIAL EXCEPTION

Igor Pavlov, as the author of this code, expressly permits you
to statically or dynamically link your code (or bind by name)
to the files from LZMA SDK without subjecting your linked
code to the terms of the CPL or GNU LGPL.
Any modifications or additions to files from LZMA SDK, however,
are subject to the GNU LGPL or CPL terms.

SPECIAL EXCEPTION allows you to use LZMA SDK in applications with closed code,
while you keep LZMA SDK code unmodified.


SPECIAL EXCEPTION #2: Igor Pavlov, as the author of this code, expressly permits
you to use this code under the same terms and conditions contained in the License
Agreement you have for any previous version of LZMA SDK developed by Igor Pavlov.

SPECIAL EXCEPTION #2 allows owners of proprietary licenses to use latest version
of LZMA SDK as update for previous versions.


SPECIAL EXCEPTION #3: Igor Pavlov, as the author of this code, expressly permits
you to use code of the following files:
BranchTypes.h, LzmaTypes.h, LzmaTest.c, LzmaStateTest.c, LzmaAlone.cpp,
LzmaAlone.cs, LzmaAlone.java
as public domain code.


4) Proprietary license

LZMA SDK also can be available under a proprietary license which
can include:

1) Right to modify code without subjecting modified code to the
terms of the CPL or GNU LGPL
2) Technical support for code

To request such proprietary license or any additional consultations,
send email message from that page:
http://www.7-zip.org/support.html


You should have received a copy of the GNU Lesser General Public
License along with this library; if not, write to the Free Software
Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

You should have received a copy of the Common Public License
along with this library.

}

interface

uses Classes;

const
  kNumRepDistances = 4;
  kNumStates = 12;
  kNumOpts = 1 shl 12;
  kNumPosSlotBits = 6;
  kPropSize = 5;
  kDicLogSizeMin = 0;
  kNumLenToPosStatesBits = 2; // it's for speed optimization
  kNumLenToPosStates = 1 shl kNumLenToPosStatesBits;
  kMatchMinLen = 2;
  kNumAlignBits = 4;
  kAlignTableSize = 1 shl kNumAlignBits;
  kAlignMask = (kAlignTableSize - 1);
  kStartPosModelIndex = 4;
  kEndPosModelIndex = 14;
  kNumPosModels = kEndPosModelIndex - kStartPosModelIndex;
  kNumFullDistances = 1 shl (kEndPosModelIndex div 2);
  kNumLitPosStatesBitsEncodingMax = 4;
  kNumLitContextBitsMax = 8;
  kNumPosStatesBitsMax = 4;
  kNumPosStatesMax = (1 shl kNumPosStatesBitsMax);
  kNumPosStatesBitsEncodingMax = 4;
  kNumPosStatesEncodingMax = (1 shl kNumPosStatesBitsEncodingMax);
  kNumLowLenBits = 3;
  kNumMidLenBits = 3;
  kNumHighLenBits = 8;
  kNumLowLenSymbols = 1 shl kNumLowLenBits;
  kNumMidLenSymbols = 1 shl kNumMidLenBits;
  kNumLenSymbols = kNumLowLenSymbols + kNumMidLenSymbols + (1 shl kNumHighLenBits);
  kMatchMaxLen = kMatchMinLen + kNumLenSymbols - 1;
  kPropertiesSize = 5;
  kNumBitModelTotalBits = 11;
  kBitModelTotal = (1 shl kNumBitModelTotalBits);
  kNumMoveReducingBits = 2;
  CodeProgressInterval = 50;
  kTopMask = not ((1 shl 24) - 1);
  kNumMoveBits = 5;
  kNumBitPriceShiftBits = 6;
  kHash2Size = 1 shl 10;
  kHash3Size = 1 shl 16;
  kBT2HashSize = 1 shl 16;
  kStartMaxLen = 1;
  kHash3Offset = kHash2Size;
  kEmptyHashValue = 0;
  kMaxValForNormalize = (1 shl 30) - 1;
  kNumFastBytesDefault = $20;
  kDefaultDictionaryLogSize = 22;
  kIfinityPrice: integer = $FFFFFFF;
  EMatchFinderTypeBT2 = 0;
  EMatchFinderTypeBT4 = 1;

type

  TLZMAProgressAction = (lpAMax, lpAPos);
  TLZMAProgress = procedure(const Action: TLZMAProgressAction;
    const Value: int64) of object;
  TReps = array[0..kNumStates - 1] of smallint;
  TMatchs = array[0..kNumStates shl kNumPosStatesBitsMax - 1] of smallint;
  TPoss = array[0..kNumFullDistances - kEndPosModelIndex - 1] of smallint;
  TChoice = array[0..1] of smallint;

  TLZMAProperties = packed array[0..4] of byte;

  TRangeDecoder = class
  public
    Range, Code: integer;
    Stream: TStream;
    procedure SetStream(const Stream: TStream); inline;
    procedure ReleaseStream; inline;
    procedure Init; inline;
    function DecodeDirectBits(const numTotalBits: integer): integer; inline;
    function DecodeBit(var probs: array of smallint; const index: integer): integer;
  end;

  TBitTreeDecoder = class
  public
    Models: array of smallint;
    NumBitLevels: integer;
    constructor Create(const numBitLevels: integer);
    procedure Init; inline;
    function Decode(const rangeDecoder: TRangeDecoder): integer; inline;
    function ReverseDecode(const rangeDecoder: TRangeDecoder): integer; overload; inline;
  end;

  TLZOutWindow = class
  public
    buffer: array of byte;
    pos: integer;
    windowSize: integer;
    streamPos: integer;
    stream: TStream;
    procedure _Create(const windowSize: integer); inline;
    procedure SetStream(const stream: TStream); inline;
    procedure ReleaseStream; inline;
    procedure Init(const solid: boolean); inline;
    procedure Flush; inline;
    procedure CopyBlock(const distance: integer; len: integer); inline;
    procedure PutByte(const b: byte); inline;
    function GetByte(const distance: integer): byte; inline;
  end;

  TLZMALenDecoder = class;
  TLZMALiteralDecoder = class;

  TBitTreeDecoders = array[0..kNumLenToPosStates - 1] of TBitTreeDecoder;
  TCoders = array[0..kNumPosStatesMax - 1] of TBitTreeDecoder;

  TLZMADecoder = class
  private
    fOutWindow: TLZOutWindow;
    fRangeDecoder: TRangeDecoder;
    fIsMatchDecoders: TMatchs;
    fIsRepDecoders: TReps;
    fIsRepG0Decoders: TReps;
    fIsRepG1Decoders: TReps;
    fIsRepG2Decoders: TReps;
    fIsRep0LongDecoders: TMatchs;
    fPosSlotDecoder: TBitTreeDecoders;
    fPosDecoders: TPoss;
    fPosAlignDecoder: TBitTreeDecoder;
    fLenDecoder: TLZMALenDecoder;
    fRepLenDecoder: TLZMALenDecoder;
    fLiteralDecoder: TLZMALiteralDecoder;
    fDictionarySize: integer;
    fDictionarySizeCheck: integer;
    fPosStateMask: integer;
    fOnProgress: TLZMAProgress;
    procedure DoProgress(const Action: TLZMAProgressAction; const Value: integer); inline;
  public
    constructor Create;
    destructor Destroy; override;
    function SetDictionarySize(const dictionarySize: integer): boolean; inline;
    function SetLcLpPb(const lc, lp, pb: integer): boolean; inline;
    procedure Init; inline;
    function Code(const inStream, outStream: TStream; outSize: int64): boolean; inline;
    function SetDecoderProperties(const properties: array of byte): boolean;
    property OnProgress: TLZMAProgress read FOnProgress write FOnProgress;
  end;

  TLZMALenDecoder = class
  private
    fChoice: TChoice;
    fLowCoder: TCoders;
    fMidCoder: TCoders;
    fHighCoder: TBitTreeDecoder;
    fNumPosStates: integer;
  public
    constructor Create;
    destructor Destroy; override;
    procedure _Create(const numPosStates: integer); inline;
    procedure Init; inline;
    function Decode(const rangeDecoder: TRangeDecoder;
      const posState: integer): integer; inline;
  end;

  TLZMADecoder2 = class
  private
    fDecoders: array[0..$300 - 1] of smallint;
  public
    procedure Init; inline;
    function DecodeNormal(const rangeDecoder: TRangeDecoder): byte; inline;
    function DecodeWithMatchByte(const rangeDecoder: TRangeDecoder;
      matchByte: byte): byte; inline;
  end;

  TLZMALiteralDecoder = class
  private
    fCoders: array of TLZMADecoder2;
    fNumPrevBits: integer;
    fNumPosBits: integer;
    fPosMask: integer;
  public
    procedure _Create(const numPosBits, numPrevBits: integer); inline;
    procedure Init; inline;
    function GetDecoder(const pos: integer; const prevByte: byte): TLZMADecoder2; inline;
    destructor Destroy; override;
  end;

  TRangeEncoder = class
  private
    ProbPrices: array[0..kBitModelTotal shr kNumMoveReducingBits - 1] of integer;
  public
    Stream: TStream;
    Low, Position: int64;
    Range, cacheSize, cache: integer;
    procedure SetStream(const stream: TStream); inline;
    procedure ReleaseStream; inline;
    procedure Init; inline;
    procedure FlushData; inline;
    procedure FlushStream; inline;
    procedure ShiftLow; inline;
    procedure EncodeDirectBits(const v, numTotalBits: integer); inline;
    function GetProcessedSizeAdd: int64; inline;
    procedure Encode(var probs: array of smallint; const index, symbol: integer);
    constructor Create;
    function GetPrice(const Prob, symbol: integer): integer; inline;
    function GetPrice0(const Prob: integer): integer; inline;
    function GetPrice1(const Prob: integer): integer; inline;
  end;

  TBitTreeEncoder = class
  public
    Models: array of smallint;
    NumBitLevels: integer;
    constructor Create(const numBitLevels: integer);
    procedure Init; inline;
    procedure Encode(const rangeEncoder: TRangeEncoder; const symbol: integer); inline;
    procedure ReverseEncode(const rangeEncoder: TRangeEncoder; symbol: integer); inline;
    function GetPrice(const symbol: integer): integer; inline;
    function ReverseGetPrice(symbol: integer): integer; overload; inline;
  end;

  TLZInWindow = class
  public
    bufferBase: array of byte;// pointer to buffer with data
    stream: TStream;
    posLimit: integer;
    // offset (from _buffer) of first byte when new block reading must be done
    streamEndWasReached: boolean;
    // if (true) then _streamPos shows real end of stream

    pointerToLastSafePosition: integer;

    bufferOffset: integer;

    blockSize: integer;  // Size of Allocated memory block
    pos: integer;             // offset (from _buffer) of curent byte
    keepSizeBefore: integer;  // how many BYTEs must be kept in buffer before _pos
    keepSizeAfter: integer;   // how many BYTEs must be kept buffer after _pos
    streamPos: integer;   // offset (from _buffer) of first not read byte from Stream

    procedure MoveBlock; inline;
    procedure ReadBlock; inline;
    procedure _Free; inline;
    procedure _Create(const keepSizeBefore, keepSizeAfter, keepSizeReserv: integer);
      virtual;
    procedure SetStream(const stream: TStream); inline;
    procedure ReleaseStream; inline;
    procedure Init; virtual;
    procedure MovePos; virtual;
    function GetIndexByte(const index: integer): byte; inline;
    // index + limit have not to exceed _keepSizeAfter;
    function GetMatchLen(const index: integer; distance, limit: integer): integer; inline;
    function GetNumAvailableBytes: integer; inline;
    procedure ReduceOffsets(const subValue: integer); inline;
  end;

  TLZBinTree = class(TLZInWindow)
  public
    cyclicBufferPos: integer;
    cyclicBufferSize: integer;
    matchMaxLen: integer;
    son: array of integer;
    hash: array of integer;
    cutValue: integer;
    hashMask: integer;
    hashSizeSum: integer;
    HASH_ARRAY: boolean;
    kNumHashDirectBytes: integer;
    kMinMatchCheck: integer;
    kFixHashSize: integer;
    constructor Create;
    procedure SetType(const numHashBytes: integer); inline;
    procedure Init; override;
    procedure MovePos; override;
    function _Create(const historySize, keepAddBufferBefore,
      matchMaxLen, keepAddBufferAfter: integer): boolean; reintroduce;
    function GetMatches(var distances: array of integer): integer;
    procedure Skip(num: integer); inline;
    procedure NormalizeLinks(var items: array of integer;
      const numItems, subValue: integer);
    procedure Normalize; inline;
    procedure SetCutValue(const cutValue: integer); inline;
  end;

  TLZMAEncoder2 = class;
  TLZMALiteralEncoder = class;
  TLZMAOptimal = class;
  TLZMALenPriceTableEncoder = class;

  TLZMAEncoder = class
  private
    fOnProgress: TLZMAProgress;
    procedure DoProgress(const Action: TLZMAProgressAction; const Value: integer); inline;
  public
    g_FastPos: array[0..1 shl 11 - 1] of byte;
    _state: integer;
    _previousByte: byte;
    _repDistances: array[0..kNumRepDistances - 1] of integer;

    _optimum: array[0..kNumOpts - 1] of TLZMAOptimal;
    _matchFinder: TLZBinTree;
    _rangeEncoder: TRangeEncoder;

    _isMatch: TMatchs;
    _isRep: TReps;
    _isRepG0: TReps;
    _isRepG1: TReps;
    _isRepG2: TReps;
    _isRep0Long: TMatchs;
    _posSlotEncoder: array[0..kNumLenToPosStates - 1] of TBitTreeEncoder; // kNumPosSlotBits
    _posEncoders: TPoss;
    _posAlignEncoder: TBitTreeEncoder;
    _lenEncoder: TLZMALenPriceTableEncoder;
    _repMatchLenEncoder: TLZMALenPriceTableEncoder;
    _literalEncoder: TLZMALiteralEncoder;
    _matchDistances: array[0..kMatchMaxLen * 2 + 1] of integer;
    _numFastBytes: integer;
    _longestMatchLength: integer;
    _numDistancePairs: integer;
    _additionalOffset: integer;
    _optimumEndIndex: integer;
    _optimumCurrentIndex: integer;
    _longestMatchWasFound: boolean;
    _posSlotPrices: array[0..1 shl (kNumPosSlotBits + kNumLenToPosStatesBits) - 1] of integer;
    _distancesPrices: array[0..kNumFullDistances shl kNumLenToPosStatesBits - 1] of integer;
    _alignPrices: array[0..kAlignTableSize - 1] of integer;
    _alignPriceCount: integer;
    _distTableSize: integer;
    _posStateBits: integer;
    _posStateMask: integer;
    _numLiteralPosStateBits: integer;
    _numLiteralContextBits: integer;
    _dictionarySize: integer;
    _dictionarySizePrev: integer;
    _numFastBytesPrev: integer;
    nowPos64: int64;
    _finished: boolean;
    _inStream: TStream;
    _matchFinderType: integer;
    _writeEndMark: boolean;
    _needReleaseMFStream: boolean;
    reps: array[0..kNumRepDistances - 1] of integer;
    repLens: array[0..kNumRepDistances - 1] of integer;
    backRes: integer;
    processedInSize: int64;
    processedOutSize: int64;
    finished: boolean;
    properties: array[0..kPropSize] of byte;
    tempPrices: array[0..kNumFullDistances - 1] of integer;
    _matchPriceCount: integer;
    constructor Create;
    destructor Destroy; override;
    function GetPosSlot(const pos: integer): integer; inline;
    function GetPosSlot2(const pos: integer): integer; inline;
    procedure BaseInit; inline;
    procedure _Create; inline;
    procedure SetWriteEndMarkerMode(const writeEndMarker: boolean); inline;
    procedure Init; inline;
    function ReadMatchDistances: integer; inline;
    procedure MovePos(const num: integer); inline;
    function GetRepLen1Price(const state, posState: integer): integer; inline;
    function GetPureRepPrice(const repIndex, state, posState: integer): integer; inline;
    function GetRepPrice(const repIndex, len, state, posState: integer): integer; inline;
    function GetPosLenPrice(const pos, len, posState: integer): integer; inline;
    function Backward(cur: integer): integer; inline;
    function GetOptimum(position: integer): integer; inline;
    function ChangePair(const smallDist, bigDist: integer): boolean; inline;
    procedure WriteEndMarker(const posState: integer); inline;
    procedure Flush(const nowPos: integer); inline;
    procedure ReleaseMFStream; inline;
    procedure CodeOneBlock(var inSize, outSize: int64; var finished: boolean); inline;
    procedure FillDistancesPrices; inline;
    procedure FillAlignPrices; inline;
    procedure SetOutStream(const outStream: TStream); inline;
    procedure ReleaseOutStream; inline;
    procedure ReleaseStreams; inline;
    procedure SetStreams(const inStream, outStream: TStream;
      const inSize, outSize: int64); inline;
    procedure Code(const inStream, outStream: TStream; const inSize, outSize: int64); inline;
    procedure WriteCoderProperties(const outStream: TStream); inline;
    function SetAlgorithm(const algorithm: integer): boolean; inline;
    function SetDictionarySize(dictionarySize: integer): boolean; inline;
    function SeNumFastBytes(const numFastBytes: integer): boolean; inline;
    function SetMatchFinder(const matchFinderIndex: integer): boolean; inline;
    function SetLcLpPb(const lc, lp, pb: integer): boolean; inline;
    procedure SetEndMarkerMode(const endMarkerMode: boolean); inline;
    property OnProgress: TLZMAProgress read fOnProgress write fOnProgress;
  end;

  TLZMALiteralEncoder = class
  public
    m_Coders: array of TLZMAEncoder2;
    m_NumPrevBits: integer;
    m_NumPosBits: integer;
    m_PosMask: integer;
    procedure _Create(const numPosBits, numPrevBits: integer); inline;
    destructor Destroy; override;
    procedure Init; inline;
    function GetSubCoder(const pos: integer; const prevByte: byte): TLZMAEncoder2; inline;
  end;

  TLZMAEncoder2 = class
  public
    m_Encoders: array[0..$300 - 1] of smallint;
    procedure Init; inline;
    procedure Encode(const rangeEncoder: TRangeEncoder; const symbol: byte); inline;
    procedure EncodeMatched(const rangeEncoder: TRangeEncoder;
      const matchByte, symbol: byte); inline;
    function GetPrice(const matchMode: boolean; const matchByte, symbol: byte): integer; inline;
  end;

  TLZMALenEncoder = class
  public
    _choice: TChoice;
    _lowCoder: array[0..kNumPosStatesEncodingMax - 1] of TBitTreeEncoder;
    _midCoder: array[0..kNumPosStatesEncodingMax - 1] of TBitTreeEncoder;
    _highCoder: TBitTreeEncoder;
    constructor Create;
    destructor Destroy; override;
    procedure Init(const numPosStates: integer); inline;
    procedure Encode(const rangeEncoder: TRangeEncoder; symbol: integer;
      const posState: integer); virtual;
    procedure SetPrices(const posState, numSymbols: integer;
      var prices: array of integer; const st: integer);
  end;

  TLZMALenPriceTableEncoder = class(TLZMALenEncoder)
  public
    _prices: array[0..kNumLenSymbols shl kNumPosStatesBitsEncodingMax - 1] of integer;
    _tableSize: integer;
    _counters: array[0..kNumPosStatesEncodingMax - 1] of integer;
    procedure SetTableSize(const tableSize: integer);  inline;
    function GetPrice(const symbol, posState: integer): integer; inline;
    procedure UpdateTable(const posState: integer); inline;
    procedure UpdateTables(const numPosStates: integer); inline;
    procedure Encode(const rangeEncoder: TRangeEncoder; symbol: integer;
      const posState: integer); override;
  end;

  TLZMAOptimal = class
  public
    State: integer;
    Prev1IsChar: boolean;
    Prev2: boolean;
    PosPrev2: integer;
    BackPrev2: integer;
    Price: integer;
    PosPrev: integer;
    BackPrev: integer;
    Backs0: integer;
    Backs1: integer;
    Backs2: integer;
    Backs3: integer;
    procedure MakeAsChar; inline;
    procedure MakeAsShortRep; inline;
    function IsShortRep: boolean; inline;
  end;

  function ReadByte(const stream: TStream): byte; inline;
  procedure WriteByte(const stream: TStream; const b: byte); inline;
  procedure InitBitModels(var probs: array of smallint);
  function StateInit: integer; inline;
  function StateIsCharState(const index: integer): boolean; inline;
  function StateUpdateChar(const index: integer): integer; inline;
  function StateUpdateShortRep(const index: integer): integer; inline;
  function StateUpdateMatch(const index: integer): integer; inline;
  function StateUpdateRep(const index: integer): integer; inline;
  function GetLenToPosState(len: integer): integer; inline;
  function ReverseDecode(var Models: array of smallint;
    const startIndex: integer; const rangeDecoder: TRangeDecoder;
    const NumBitLevels: integer): integer;
  procedure ReverseEncode(var Models: array of smallint; const startIndex: integer;
    const rangeEncoder: TRangeEncoder; const NumBitLevels: integer; symbol: integer);
  function ReverseGetPrice(var Models: array of smallint;
    const startIndex, NumBitLevels: integer; symbol: integer): integer;

var
  RangeEncoder: TRangeEncoder;
  CRCTable: array[Byte] of integer;

implementation

uses
  Math;


function ReadByte(const stream: TStream): byte;
begin
  stream.Read(Result, 1);
end;

procedure WriteByte(const stream: TStream; const b: byte);
begin
  stream.Write(b, 1);
end;

function StateInit: integer;
begin
  Result := 0;
end;

function StateUpdateChar(const index: integer): integer;
begin
  if (index < 4) then
    Result := 0
  else
  if (index < 10) then
    Result := index - 3
  else
    Result := index - 6;
end;

function StateUpdateMatch(const index: integer): integer;
begin
  if index < 7 then
    Result := 7
  else
    Result := 10;
end;

function StateUpdateRep(const index: integer): integer;
begin
  if index < 7 then
    Result := 8
  else
    Result := 11;
end;

function StateUpdateShortRep(const index: integer): integer;
begin
  if index < 7 then
    Result := 9
  else
    Result := 11;
end;

function StateIsCharState(const index: integer): boolean;
begin
  Result := index < 7;
end;

function GetLenToPosState(len: integer): integer;
begin
  len := len - kMatchMinLen;
  if (len < kNumLenToPosStates) then
    Result := len
  else
    Result := (kNumLenToPosStates - 1);
end;

procedure InitBitModels(var probs: array of smallint);
var
  i: integer;
begin
  for i := 0 to length(probs) - 1 do
    probs[i] := kBitModelTotal shr 1;
end;

function ReverseDecode(var Models: array of smallint;
  const startIndex: integer; const rangeDecoder: TRangeDecoder;
  const NumBitLevels: integer): integer;
var
  m, symbol, bitindex, bit: integer;
begin
  m := 1;
  symbol := 0;
  for bitindex := 0 to numbitlevels - 1 do
  begin
    bit := rangeDecoder.DecodeBit(Models, startIndex + m);
    m := (m shl 1) + bit;
    symbol := symbol or bit shl bitindex;
  end;
  Result := symbol;
end;


function ReverseGetPrice(var Models: array of smallint;
  const startIndex, NumBitLevels: integer; symbol: integer): integer;
var
  price, m, i, bit: integer;
begin
  price := 0;
  m := 1;
  for i := NumBitLevels downto 1 do
  begin
    bit := symbol and 1;
    symbol := symbol shr 1;
    price := price + RangeEncoder.GetPrice(Models[startIndex + m], bit);
    m := (m shl 1) or bit;
  end;
  Result := price;
end;

procedure ReverseEncode(var Models: array of smallint; const startIndex: integer;
  const rangeEncoder: TRangeEncoder; const NumBitLevels: integer; symbol: integer);
var
  m, i, bit: integer;
begin
  m := 1;
  for i := 0 to NumBitLevels - 1 do
  begin
    bit := symbol and 1;
    rangeEncoder.Encode(Models, startIndex + m, bit);
    m := (m shl 1) or bit;
    symbol := symbol shr 1;
  end;
end;

procedure InitCRC;
var
  i, r, j: integer;
begin
  for i := 0 to 255 do
  begin
    r := i;
    for j := 0 to 7 do
      if ((r and 1) <> 0) then
        r := (r shr 1) xor integer($EDB88320)
      else
        r := r shr 1;
    CrcTable[i] := r;
  end;
end;

procedure InitModule;
begin
  RangeEncoder := TRangeEncoder.Create;
  InitCRC;
end;

procedure FinalModule;
begin
  RangeEncoder.Free;
end;

{-----------------------------------  Decoder ---------------------------------}

{ TRangeDecoder }

procedure TRangeDecoder.SetStream(const Stream: TStream);
begin
  self.Stream := Stream;
end;

procedure TRangeDecoder.ReleaseStream;
begin
  stream := nil;
end;

procedure TRangeDecoder.Init;
var
  i: integer;
begin
  code := 0;
  Range := -1;
  for i := 0 to 4 do
  begin
    code := (code shl 8) or byte(ReadByte(stream));
  end;
end;

function TRangeDecoder.DecodeDirectBits(const numTotalBits: integer): integer;
var
  i, t: integer;
begin
  Result := 0;
  for i := numTotalBits downto 1 do
  begin
    range := range shr 1;
    t := ((Code - Range) shr 31);
    Code := Code - Range and (t - 1);
    Result := (Result shl 1) or (1 - t);
    if ((Range and kTopMask) = 0) then
    begin
      Code := (Code shl 8) or ReadByte(stream);
      Range := Range shl 8;
    end;
  end;
end;

function TRangeDecoder.DecodeBit(var probs: array of smallint;
  const index: integer): integer;
var
  prob, newbound: integer;
begin
  prob := probs[index];
  newbound := (Range shr kNumBitModelTotalBits) * prob;
  if (integer((integer(Code) xor integer($80000000))) < integer(
    (integer(newBound) xor integer($80000000)))) then
  begin
    Range := newBound;
    probs[index] := (prob + ((kBitModelTotal - prob) shr kNumMoveBits));
    if ((Range and kTopMask) = 0) then
    begin
      Code := (Code shl 8) or ReadByte(stream);
      Range := Range shl 8;
    end;
    Result := 0;
  end
  else
  begin
    Range := Range - newBound;
    Code := Code - newBound;
    probs[index] := (prob - ((prob) shr kNumMoveBits));
    if ((Range and kTopMask) = 0) then
    begin
      Code := (Code shl 8) or ReadByte(stream);
      Range := Range shl 8;
    end;
    Result := 1;
  end;
end;

{  TBitTreeDecoder }

constructor TBitTreeDecoder.Create(const numBitLevels: integer);
begin
  self.NumBitLevels := numBitLevels;
  setlength(Models, 1 shl numBitLevels);
end;

procedure TBitTreeDecoder.Init;
begin
  InitBitModels(Models);
end;

function TBitTreeDecoder.Decode(const rangeDecoder: TRangeDecoder): integer;
var
  m, bitIndex: integer;
begin
  m := 1;
  for bitIndex := NumBitLevels downto 1 do
  begin
    m := m shl 1 + rangeDecoder.DecodeBit(Models, m);
  end;
  Result := m - (1 shl NumBitLevels);
end;

function TBitTreeDecoder.ReverseDecode(const rangeDecoder: TRangeDecoder): integer;
var
  m, symbol, bitindex, bit: integer;
begin
  m := 1;
  symbol := 0;
  for bitindex := 0 to numbitlevels - 1 do
  begin
    bit := rangeDecoder.DecodeBit(Models, m);
    m := (m shl 1) + bit;
    symbol := symbol or (bit shl bitIndex);
  end;
  Result := symbol;
end;

{ TLZOutWindow }

procedure TLZOutWindow._Create(const windowSize: integer);
begin
  if (length(buffer) = 0) or (self.windowSize <> windowSize) then
    setlength(buffer, windowSize);
  self.windowSize := windowSize;
  pos := 0;
  streamPos := 0;
end;

procedure TLZOutWindow.SetStream(const stream: TStream);
begin
  ReleaseStream;
  self.stream := stream;
end;

procedure TLZOutWindow.ReleaseStream;
begin
  flush;
  self.stream := nil;
end;

procedure TLZOutWindow.Init(const solid: boolean);
begin
  if not solid then
  begin
    streamPos := 0;
    Pos := 0;
  end;
end;

procedure TLZOutWindow.Flush;
var
  size: integer;
begin
  size := pos - streamPos;
  if (size = 0) then
    exit;
  stream.Write(buffer[streamPos], size);
  if (pos >= windowSize) then
    pos := 0;
  streamPos := pos;
end;

procedure TLZOutWindow.CopyBlock(const distance: integer; len: integer);
var
  pos: integer;
begin
  pos := self.pos - distance - 1;
  if pos < 0 then
    pos := pos + windowSize;
  while len <> 0 do
  begin
    if pos >= windowSize then
      pos := 0;
    buffer[self.pos] := buffer[pos];
    Inc(self.pos);
    Inc(pos);
    if self.pos >= windowSize then
      Flush();
    Dec(len);
  end;
end;

procedure TLZOutWindow.PutByte(const b: byte);
begin
  buffer[pos] := b;
  Inc(pos);
  if (pos >= windowSize) then
    Flush();
end;

function TLZOutWindow.GetByte(const distance: integer): byte;
var
  pos: integer;
begin
  pos := self.pos - distance - 1;
  if (pos < 0) then
    pos := pos + windowSize;
  Result := buffer[pos];
end;

{ TLZMALenDecoder }

constructor TLZMALenDecoder.Create;
begin
  fHighCoder := TBitTreeDecoder.Create(kNumHighLenBits);
  fNumPosStates := 0;
end;

destructor TLZMALenDecoder.Destroy;
var
  i: integer;
begin
  fHighCoder.Free;
  for i := low(fLowCoder) to high(fLowCoder) do
  begin
    if fLowCoder[i] <> nil then
      fLowCoder[i].Free;
    if fMidCoder[i] <> nil then
      fMidCoder[i].Free;
  end;
  inherited;
end;

procedure TLZMALenDecoder._Create(const numPosStates: integer);
begin
  while fNumPosStates < numPosStates do
  begin
    fLowCoder[fNumPosStates] := TBitTreeDecoder.Create(kNumLowLenBits);
    fMidCoder[fNumPosStates] := TBitTreeDecoder.Create(kNumMidLenBits);
    Inc(fNumPosStates);
  end;
end;

procedure TLZMALenDecoder.Init;
var
  posState: integer;
begin
  InitBitModels(fChoice);
  for posState := 0 to fNumPosStates - 1 do
  begin
    fLowCoder[posState].Init;
    fMidCoder[posState].Init;
  end;
  fHighCoder.Init;
end;

function TLZMALenDecoder.Decode(const rangeDecoder: TRangeDecoder;
  const posState: integer): integer;
var
  symbol: integer;
begin
  if (rangeDecoder.DecodeBit(fChoice, 0) = 0) then
  begin
    Result := fLowCoder[posState].Decode(rangeDecoder);
    exit;
  end;
  symbol := kNumLowLenSymbols;
  if (rangeDecoder.DecodeBit(fChoice, 1) = 0) then
    symbol := symbol + fMidCoder[posState].Decode(rangeDecoder)
  else
    symbol := symbol + kNumMidLenSymbols + fHighCoder.Decode(rangeDecoder);
  Result := symbol;
end;

{ TLZMADecoder2 }

procedure TLZMADecoder2.Init;
begin
  InitBitModels(fDecoders);
end;

function TLZMADecoder2.DecodeNormal(const rangeDecoder: TRangeDecoder): byte;
var
  symbol: integer;
begin
  symbol := 1;
  repeat
    symbol := (symbol shl 1) or rangeDecoder.DecodeBit(fDecoders, symbol);
  until not (symbol < $100);
  Result := symbol;
end;

function TLZMADecoder2.DecodeWithMatchByte(const rangeDecoder: TRangeDecoder;
  matchByte: byte): byte;
var
  symbol: integer;
  matchbit: integer;
  bit: integer;
begin
  symbol := 1;
  repeat
    matchBit := (matchByte shr 7) and 1;
    matchByte := matchByte shl 1;
    bit := rangeDecoder.DecodeBit(fDecoders, ((1 + matchBit) shl 8) + symbol);
    symbol := (symbol shl 1) or bit;
    if (matchBit <> bit) then
    begin
      while (symbol < $100) do
      begin
        symbol := (symbol shl 1) or rangeDecoder.DecodeBit(fDecoders, symbol);
      end;
      break;
    end;
  until not (symbol < $100);
  Result := symbol;
end;

{ TLZMALiteralDecoder }

procedure TLZMALiteralDecoder._Create(const numPosBits, numPrevBits: integer);
var
  numStates, i: integer;
begin
  if (length(fCoders) <> 0) and (fNumPrevBits = numPrevBits) and
    (fNumPosBits = numPosBits) then
    exit;
  fNumPosBits := numPosBits;
  fPosMask := (1 shl numPosBits) - 1;
  fNumPrevBits := numPrevBits;
  numStates := 1 shl (fNumPrevBits + fNumPosBits);
  setlength(fCoders, numStates);
  for i := 0 to numStates - 1 do
    fCoders[i] := TLZMADecoder2.Create;
end;

destructor TLZMALiteralDecoder.Destroy;
var
  i: integer;
begin
  for i := low(fCoders) to high(fCoders) do
    if fCoders[i] <> nil then
      fCoders[i].Free;
  inherited;
end;

procedure TLZMALiteralDecoder.Init;
var
  numStates, i: integer;
begin
  numStates := 1 shl (fNumPrevBits + fNumPosBits);
  for i := 0 to numStates - 1 do
    fCoders[i].Init;
end;

function TLZMALiteralDecoder.GetDecoder(const pos: integer;
  const prevByte: byte): TLZMADecoder2;
begin
  Result := fCoders[((pos and fPosMask) shl fNumPrevBits) +
    ((prevByte and $FF) shr (8 - fNumPrevBits))];
end;


{ TLZMADecoder }

constructor TLZMADecoder.Create;
var
  i: integer;
begin
  fOnProgress := nil;
  fOutWindow := TLZOutWindow.Create;
  fRangeDecoder := TRangeDecoder.Create;
  fPosAlignDecoder := TBitTreeDecoder.Create(kNumAlignBits);
  fLenDecoder := TLZMALenDecoder.Create;
  fRepLenDecoder := TLZMALenDecoder.Create;
  fLiteralDecoder := TLZMALiteralDecoder.Create;
  fDictionarySize := -1;
  fDictionarySizeCheck := -1;
  for i := 0 to kNumLenToPosStates - 1 do
    fPosSlotDecoder[i] := TBitTreeDecoder.Create(kNumPosSlotBits);
end;

destructor TLZMADecoder.Destroy;
var
  i: integer;
begin
  fOutWindow.Free;
  fRangeDecoder.Free;
  fPosAlignDecoder.Free;
  fLenDecoder.Free;
  fRepLenDecoder.Free;
  fLiteralDecoder.Free;
  for i := 0 to kNumLenToPosStates - 1 do
    fPosSlotDecoder[i].Free;
end;

function TLZMADecoder.SetDictionarySize(const dictionarySize: integer): boolean;
begin
  if dictionarySize < 0 then
    Result := False
  else
  begin
    if fDictionarySize <> dictionarySize then
    begin
      fDictionarySize := dictionarySize;
      fDictionarySizeCheck := max(fDictionarySize, 1);
      fOutWindow._Create(max(fDictionarySizeCheck, (1 shl 12)));
    end;
    Result := True;
  end;
end;

function TLZMADecoder.SetLcLpPb(const lc, lp, pb: integer): boolean;
var
  numPosStates: integer;
begin
  if (lc > kNumLitContextBitsMax) or (lp > 4) or
    (pb > kNumPosStatesBitsMax) then
  begin
    Result := False;
    exit;
  end;
  fLiteralDecoder._Create(lp, lc);
  numPosStates := 1 shl pb;
  fLenDecoder._Create(numPosStates);
  fRepLenDecoder._Create(numPosStates);
  fPosStateMask := numPosStates - 1;
  Result := True;
end;

procedure TLZMADecoder.Init;
var
  i: integer;
begin
  fOutWindow.Init(False);

  InitBitModels(fIsMatchDecoders);
  InitBitModels(fIsRep0LongDecoders);
  InitBitModels(fIsRepDecoders);
  InitBitModels(fIsRepG0Decoders);
  InitBitModels(fIsRepG1Decoders);
  InitBitModels(fIsRepG2Decoders);
  InitBitModels(fPosDecoders);

  fLiteralDecoder.Init();
  for i := 0 to kNumLenToPosStates - 1 do
    fPosSlotDecoder[i].Init;
  fLenDecoder.Init;
  fRepLenDecoder.Init;
  fPosAlignDecoder.Init;
  fRangeDecoder.Init;
end;

function TLZMADecoder.Code(const inStream, outStream: TStream; outSize: int64): boolean;
var
  state, rep0, rep1, rep2, rep3: integer;
  nowPos64: int64;
  prevByte: byte;
  posState: integer;
  decoder2: TLZMADecoder2;
  len, distance, posSlot, numDirectBits: integer;
  lpos: int64;
  progint: int64;
begin
  DoProgress(LPAMax, outSize);
  fRangeDecoder.SetStream(inStream);
  fOutWindow.SetStream(outStream);
  Init;

  state := StateInit;
  rep0 := 0;
  rep1 := 0;
  rep2 := 0;
  rep3 := 0;

  nowPos64 := 0;
  prevByte := 0;
  progint := outsize div CodeProgressInterval;
  lpos := progint;
  while (outSize < 0) or (nowPos64 < outSize) do
  begin
    if (nowPos64 >= lpos) then
    begin
      DoProgress(LPAPos, nowPos64);
      lpos := lpos + progint;
    end;
    posState := nowPos64 and fPosStateMask;
    if (fRangeDecoder.DecodeBit(fIsMatchDecoders,
      (state shl kNumPosStatesBitsMax) + posState) = 0) then
    begin
      decoder2 := fLiteralDecoder.GetDecoder(nowPos64, prevByte);
      if not StateIsCharState(state) then
        prevByte := decoder2.DecodeWithMatchByte(fRangeDecoder,
          fOutWindow.GetByte(rep0))
      else
        prevByte := decoder2.DecodeNormal(fRangeDecoder);
      fOutWindow.PutByte(prevByte);
      state := StateUpdateChar(state);
      Inc(nowPos64);
    end
    else
    begin
      if (fRangeDecoder.DecodeBit(fIsRepDecoders, state) = 1) then
      begin
        len := 0;
        if (fRangeDecoder.DecodeBit(fIsRepG0Decoders, state) = 0) then
        begin
          if (fRangeDecoder.DecodeBit(fIsRep0LongDecoders,
            (state shl kNumPosStatesBitsMax) + posState) = 0) then
          begin
            state := StateUpdateShortRep(state);
            len := 1;
          end;
        end
        else
        begin
          if fRangeDecoder.DecodeBit(fIsRepG1Decoders, state) = 0 then
            distance := rep1
          else
          begin
            if (fRangeDecoder.DecodeBit(fIsRepG2Decoders,
              state) = 0) then
              distance := rep2
            else
            begin
              distance := rep3;
              rep3 := rep2;
            end;
            rep2 := rep1;
          end;
          rep1 := rep0;
          rep0 := distance;
        end;
        if len = 0 then
        begin
          len := fRepLenDecoder.Decode(fRangeDecoder, posState) +
            kMatchMinLen;
          state := StateUpdateRep(state);
        end;
      end
      else
      begin
        rep3 := rep2;
        rep2 := rep1;
        rep1 := rep0;
        len := kMatchMinLen + fLenDecoder.Decode(fRangeDecoder, posState);
        state := StateUpdateMatch(state);
        posSlot :=
          fPosSlotDecoder[GetLenToPosState(len)].Decode(fRangeDecoder);
        if posSlot >= kStartPosModelIndex then
        begin
          numDirectBits := (posSlot shr 1) - 1;
          rep0 := ((2 or (posSlot and 1)) shl numDirectBits);
          if posSlot < kEndPosModelIndex then
            rep0 :=
              rep0 + ReverseDecode(fPosDecoders,
              rep0 - posSlot - 1, fRangeDecoder, numDirectBits)
          else
          begin
            rep0 :=
              rep0 + (fRangeDecoder.DecodeDirectBits(
              numDirectBits - kNumAlignBits) shl kNumAlignBits);
            rep0 :=
              rep0 + fPosAlignDecoder.ReverseDecode(fRangeDecoder);
            if rep0 < 0 then
            begin
              if rep0 = -1 then
                break;
              Result := False;
              exit;
            end;
          end;
        end
        else
          rep0 := posSlot;
      end;
      if (rep0 >= nowPos64) or (rep0 >= fDictionarySizeCheck) then
      begin
        fOutWindow.Flush();
        Result := False;
        exit;
      end;
      fOutWindow.CopyBlock(rep0, len);
      nowPos64 := nowPos64 + len;
      prevByte := fOutWindow.GetByte(0);
    end;
  end;
  fOutWindow.Flush();
  fOutWindow.ReleaseStream();
  fRangeDecoder.ReleaseStream();
  DoProgress(LPAPos, nowPos64);
  Result := True;
end;

function TLZMADecoder.SetDecoderProperties(const properties: array of byte): boolean;
var
  val, lc, remainder, lp, pb, dictionarysize, i: integer;
begin
  if length(properties) < 5 then
  begin
    Result := False;
    exit;
  end;
  val := properties[0] and $FF;
  lc := val mod 9;
  remainder := val div 9;
  lp := remainder mod 5;
  pb := remainder div 5;
  dictionarySize := 0;
  for i := 0 to 3 do
    dictionarySize := dictionarysize + ((properties[1 + i]) and $FF) shl (i * 8);
  if (not SetLcLpPb(lc, lp, pb)) then
  begin
    Result := False;
    exit;
  end;
  Result := SetDictionarySize(dictionarySize);
end;

procedure TLZMADecoder.DoProgress(const Action: TLZMAProgressAction; const Value: integer);
begin
  if assigned(fOnProgress) then
    fOnProgress(action, Value);
end;

{-----------------------------------  Encoder ---------------------------------}

{ TRangeEncoder }

procedure TRangeEncoder.SetStream(const stream: TStream);
begin
  self.Stream := Stream;
end;

procedure TRangeEncoder.ReleaseStream;
begin
  stream := nil;
end;

procedure TRangeEncoder.Init;
begin
  position := 0;
  Low := 0;
  Range := -1;
  cacheSize := 1;
  cache := 0;
end;

procedure TRangeEncoder.FlushData;
var
  i: integer;
begin
  for i := 0 to 4 do
    ShiftLow();
end;

procedure TRangeEncoder.FlushStream;
begin
  //stream.flush;
end;

procedure TRangeEncoder.ShiftLow;
var
  LowHi: integer;
  temp: integer;
begin
  LowHi := (Low shr 32);
  if (LowHi <> 0) or (Low < int64($FF000000)) then
  begin
    position := position + cacheSize;
    temp := cache;
    repeat
      WriteByte(stream, temp + LowHi);
      temp := $FF;
      Dec(cacheSize);
    until (cacheSize = 0);
    cache := (Low shr 24);
  end;
  Inc(cacheSize);
  Low := (Low and integer($FFFFFF)) shl 8;
end;

procedure TRangeEncoder.EncodeDirectBits(const v, numTotalBits: integer);
var
  i: integer;
begin
  for i := numTotalBits - 1 downto 0 do
  begin
    Range := Range shr 1;
    if (((v shr i) and 1) = 1) then
      Low := Low + Range;
    if ((Range and kTopMask) = 0) then
    begin
      Range := range shl 8;
      ShiftLow;
    end;
  end;
end;

function TRangeEncoder.GetProcessedSizeAdd: int64;
begin
  Result := cacheSize + position + 4;
end;

procedure TRangeEncoder.Encode(var probs: array of smallint; const index, symbol: integer);
var
  prob, newbound: integer;
begin
  prob := probs[index];
  newBound := (Range shr kNumBitModelTotalBits) * prob;
  if (symbol = 0) then
  begin
    Range := newBound;
    probs[index] := (prob + ((kBitModelTotal - prob) shr kNumMoveBits));
  end
  else
  begin
    Low := Low + (newBound and int64($FFFFFFFF));
    Range := Range - newBound;
    probs[index] := (prob - ((prob) shr kNumMoveBits));
  end;
  if ((Range and kTopMask) = 0) then
  begin
    Range := Range shl 8;
    ShiftLow;
  end;
end;

constructor TRangeEncoder.Create;
var
  kNumBits: integer;
  i, j, start, _end: integer;
begin
  kNumBits := (kNumBitModelTotalBits - kNumMoveReducingBits);
  for i := kNumBits - 1 downto 0 do
  begin
    start := 1 shl (kNumBits - i - 1);
    _end := 1 shl (kNumBits - i);
    for j := start to _end - 1 do
      ProbPrices[j] := (i shl kNumBitPriceShiftBits) +
        (((_end - j) shl kNumBitPriceShiftBits) shr (kNumBits - i - 1));
  end;
end;

function TRangeEncoder.GetPrice(const Prob, symbol: integer): integer;
begin
  Result := ProbPrices[(((Prob - symbol) xor ((-symbol))) and (kBitModelTotal - 1)) shr
    kNumMoveReducingBits];
end;

function TRangeEncoder.GetPrice0(const Prob: integer): integer;
begin
  Result := ProbPrices[Prob shr kNumMoveReducingBits];
end;

function TRangeEncoder.GetPrice1(const Prob: integer): integer;
begin
  Result := ProbPrices[(kBitModelTotal - Prob) shr kNumMoveReducingBits];
end;

{ TBitTreeEncoder }


constructor TBitTreeEncoder.Create(const numBitLevels: integer);
begin
  self.NumBitLevels := numBitLevels;
  setlength(Models, 1 shl numBitLevels);
end;

procedure TBitTreeEncoder.Init;
begin
  InitBitModels(Models);
end;

procedure TBitTreeEncoder.Encode(const rangeEncoder: TRangeEncoder; const symbol: integer);
var
  m, bitindex, bit: integer;
begin
  m := 1;
  for bitIndex := NumBitLevels - 1 downto 0 do
  begin
    bit := (symbol shr bitIndex) and 1;
    rangeEncoder.Encode(Models, m, bit);
    m := (m shl 1) or bit;
  end;
end;

procedure TBitTreeEncoder.ReverseEncode(const rangeEncoder: TRangeEncoder; symbol: integer);
var
  m, i, bit: integer;
begin
  m := 1;
  for i := 0 to NumBitLevels - 1 do
  begin
    bit := symbol and 1;
    rangeEncoder.Encode(Models, m, bit);
    m := (m shl 1) or bit;
    symbol := symbol shr 1;
  end;
end;

function TBitTreeEncoder.GetPrice(const symbol: integer): integer;
var
  price, m, bitindex, bit: integer;
begin
  price := 0;
  m := 1;
  for bitIndex := NumBitLevels - 1 downto 0 do
  begin
    bit := (symbol shr bitIndex) and 1;
    price := price + RangeEncoder.GetPrice(Models[m], bit);
    m := (m shl 1) + bit;
  end;
  Result := price;
end;

function TBitTreeEncoder.ReverseGetPrice(symbol: integer): integer;
var
  price, m, i, bit: integer;
begin
  price := 0;
  m := 1;
  for i := NumBitLevels downto 1 do
  begin
    bit := symbol and 1;
    symbol := symbol shr 1;
    price := price + RangeEncoder.GetPrice(Models[m], bit);
    m := (m shl 1) or bit;
  end;
  Result := price;
end;

{ TLZInWindow }

procedure TLZInWindow.MoveBlock;
var
  offset, numbytes, i: integer;
begin
  offset := bufferOffset + pos - keepSizeBefore;
  // we need one additional byte, since MovePos moves on 1 byte.
  if (offset > 0) then
    Dec(offset);

  numBytes := bufferOffset + streamPos - offset;

  // check negative offset ????
  for i := 0 to numBytes - 1 do
    bufferBase[i] := bufferBase[offset + i];
  bufferOffset := bufferOffset - offset;
end;

procedure TLZInWindow.ReadBlock;
var
  size, numreadbytes, pointerToPostion: integer;
begin
  if streamEndWasReached then
    exit;
  while (True) do
  begin
    size := (0 - bufferOffset) + blockSize - streamPos;
    if size = 0 then
      exit;
    numReadBytes := stream.Read(bufferBase[bufferOffset + streamPos], size);
    if (numReadBytes = 0) then
    begin
      posLimit := streamPos;
      pointerToPostion := bufferOffset + posLimit;
      if (pointerToPostion > pointerToLastSafePosition) then
        posLimit := pointerToLastSafePosition - bufferOffset;
      streamEndWasReached := True;
      exit;
    end;
    streamPos := streamPos + numReadBytes;
    if (streamPos >= pos + keepSizeAfter) then
      posLimit := streamPos - keepSizeAfter;
  end;
end;

procedure TLZInWindow._Free;
begin
  setlength(bufferBase, 0);
end;

procedure TLZInWindow._Create(
  const keepSizeBefore, keepSizeAfter, keepSizeReserv: integer);
var
  blocksize: integer;
begin
  self.keepSizeBefore := keepSizeBefore;
  self.keepSizeAfter := keepSizeAfter;
  blockSize := keepSizeBefore + keepSizeAfter + keepSizeReserv;
  if (length(bufferBase) = 0) or (self.blockSize <> blockSize) then
  begin
    _Free;
    self.blockSize := blockSize;
    setlength(bufferBase, self.blockSize);
  end;
  pointerToLastSafePosition := self.blockSize - keepSizeAfter;
end;

procedure TLZInWindow.SetStream(const stream: TStream);
begin
  self.stream := stream;
end;

procedure TLZInWindow.ReleaseStream;
begin
  stream := nil;
end;

procedure TLZInWindow.Init;
begin
  bufferOffset := 0;
  pos := 0;
  streamPos := 0;
  streamEndWasReached := False;
  ReadBlock;
end;

procedure TLZInWindow.MovePos;
var
  pointerToPostion: integer;
begin
  Inc(pos);
  if pos > posLimit then
  begin
    pointerToPostion := bufferOffset + pos;
    if pointerToPostion > pointerToLastSafePosition then
      MoveBlock;
    ReadBlock;
  end;
end;

function TLZInWindow.GetIndexByte(const index: integer): byte;
begin
  Result := bufferBase[bufferOffset + pos + index];
end;

function TLZInWindow.GetMatchLen(const index: integer; distance, limit: integer): integer;
var
  pby, i: integer;
begin
  if streamEndWasReached then
    if (pos + index) + limit > streamPos then
      limit := streamPos - (pos + index);
  Inc(distance);
  // Byte *pby = _buffer + (size_t)_pos + index;
  pby := bufferOffset + pos + index;

  i := 0;
  while (i < limit) and (bufferBase[pby + i] = bufferBase[pby + i - distance]) do
  begin
    Inc(i);
  end;
  Result := i;
end;

function TLZInWindow.GetNumAvailableBytes: integer;
begin
  Result := streamPos - pos;
end;

procedure TLZInWindow.ReduceOffsets(const subvalue: integer);
begin
  bufferOffset := bufferOffset + subValue;
  posLimit := posLimit - subValue;
  pos := pos - subValue;
  streamPos := streamPos - subValue;
end;

{ TLZBinTree }

constructor TLZBinTree.Create;
begin
  inherited Create;
  cyclicBufferSize := 0;
  cutValue := $FF;
  hashSizeSum := 0;
  HASH_ARRAY := True;
  kNumHashDirectBytes := 0;
  kMinMatchCheck := 4;
  kFixHashsize := kHash2Size + kHash3Size;
end;

procedure TLZBinTree.SetType(const numHashBytes: integer);
begin
  HASH_ARRAY := (numHashBytes > 2);
  if HASH_ARRAY then
  begin
    kNumHashDirectBytes := 0;
    kMinMatchCheck := 4;
    kFixHashSize := kHash2Size + kHash3Size;
  end
  else
  begin
    kNumHashDirectBytes := 2;
    kMinMatchCheck := 2 + 1;
    kFixHashSize := 0;
  end;
end;

procedure TLZBinTree.Init;
var
  i: integer;
begin
  inherited init;
  for i := 0 to hashSizeSum - 1 do
    hash[i] := kEmptyHashValue;
  cyclicBufferPos := 0;
  ReduceOffsets(-1);
end;

procedure TLZBinTree.MovePos;
begin
  Inc(cyclicBufferPos);
  if cyclicBufferPos >= cyclicBufferSize then
    cyclicBufferPos := 0;
  inherited MovePos;
  if pos = kMaxValForNormalize then
    Normalize;
end;

function TLZBinTree._Create(
  const historySize, keepAddBufferBefore, matchMaxLen, keepAddBufferAfter: integer): boolean;
var
  windowReservSize: integer;
  cyclicBufferSize: integer;
  hs: integer;
begin
  if (historySize > kMaxValForNormalize - 256) then
  begin
    Result := False;
    exit;
  end;
  cutValue := 16 + (matchMaxLen shr 1);

  windowReservSize := (historySize + keepAddBufferBefore + matchMaxLen +
    keepAddBufferAfter) div 2 + 256;

  inherited _Create(historySize + keepAddBufferBefore, matchMaxLen +
    keepAddBufferAfter, windowReservSize);

  self.matchMaxLen := matchMaxLen;

  cyclicBufferSize := historySize + 1;
  if self.cyclicBufferSize <> cyclicBufferSize then
  begin
    self.cyclicBufferSize := cyclicBufferSize;
    setlength(son, cyclicBufferSize * 2);
  end;

  hs := kBT2HashSize;

  if HASH_ARRAY then
  begin
    hs := historySize - 1;
    hs := hs or (hs shr 1);
    hs := hs or (hs shr 2);
    hs := hs or (hs shr 4);
    hs := hs or (hs shr 8);
    hs := hs shr 1;
    hs := hs or $FFFF;
    if (hs > (1 shl 24)) then
      hs := hs shr 1;
    hashMask := hs;
    Inc(hs);
    hs := hs + kFixHashSize;
  end;
  if (hs <> hashSizeSum) then
  begin
    hashSizeSum := hs;
    setlength(hash, hashSizeSum);
  end;
  Result := True;
end;

function TLZBinTree.GetMatches(var distances: array of integer): integer;
var
  lenLimit: integer;
  offset, matchMinPos, cur, maxlen, hashvalue, hash2value, hash3value: integer;
  temp, curmatch, curmatch2, curmatch3, ptr0, ptr1, len0, len1, Count: integer;
  delta, cyclicpos, pby1, len: integer;
begin
  if pos + matchMaxLen <= streamPos then
    lenLimit := matchMaxLen
  else
  begin
    lenLimit := streamPos - pos;
    if lenLimit < kMinMatchCheck then
    begin
      MovePos();
      Result := 0;
      exit;
    end;
  end;

  offset := 0;
  if (pos > cyclicBufferSize) then
    matchMinPos := (pos - cyclicBufferSize)
  else
    matchMinPos := 0;
  cur := bufferOffset + pos;
  maxLen := kStartMaxLen; // to avoid items for len < hashSize;
  hash2Value := 0;
  hash3Value := 0;

  if HASH_ARRAY then
  begin
    temp := CrcTable[bufferBase[cur] and $FF] xor (bufferBase[cur + 1] and $FF);
    hash2Value := temp and (kHash2Size - 1);
    temp := temp xor ((bufferBase[cur + 2] and $FF) shl 8);
    hash3Value := temp and (kHash3Size - 1);
    hashValue := (temp xor (CrcTable[bufferBase[cur + 3] and $FF] shl 5)) and hashMask;
  end
  else
    hashValue := ((bufferBase[cur] and $FF) xor
      ((bufferBase[cur + 1] and $FF) shl 8));

  curMatch := hash[kFixHashSize + hashValue];
  if HASH_ARRAY then
  begin
    curMatch2 := hash[hash2Value];
    curMatch3 := hash[kHash3Offset + hash3Value];
    hash[hash2Value] := pos;
    hash[kHash3Offset + hash3Value] := pos;
    if curMatch2 > matchMinPos then
      if bufferBase[bufferOffset + curMatch2] = bufferBase[cur] then
      begin
        maxLen := 2;
        distances[offset] := maxLen;
        Inc(offset);
        distances[offset] := pos - curMatch2 - 1;
        Inc(offset);
      end;
    if curMatch3 > matchMinPos then
      if bufferBase[bufferOffset + curMatch3] = bufferBase[cur] then
      begin
        if curMatch3 = curMatch2 then
          offset := offset - 2;
        maxLen := 3;
        distances[offset] := maxlen;
        Inc(offset);
        distances[offset] := pos - curMatch3 - 1;
        Inc(offset);
        curMatch2 := curMatch3;
      end;
    if (offset <> 0) and (curMatch2 = curMatch) then
    begin
      offset := offset - 2;
      maxLen := kStartMaxLen;
    end;
  end;

  hash[kFixHashSize + hashValue] := pos;

  ptr0 := (cyclicBufferPos shl 1) + 1;
  ptr1 := (cyclicBufferPos shl 1);

  len0 := kNumHashDirectBytes;
  len1 := len0;

  if kNumHashDirectBytes <> 0 then
  begin
    if (curMatch > matchMinPos) then
    begin
      if (bufferBase[bufferOffset + curMatch + kNumHashDirectBytes] <>
        bufferBase[cur + kNumHashDirectBytes]) then
      begin
        maxLen := kNumHashDirectBytes;
        distances[offset] := maxLen;
        Inc(offset);
        distances[offset] := pos - curMatch - 1;
        Inc(offset);
      end;
    end;
  end;

  Count := cutValue;

  while (True) do
  begin
    if (curMatch <= matchMinPos) or (Count = 0) then
    begin
      son[ptr1] := kEmptyHashValue;
      son[ptr0] := son[ptr1];
      break;
    end;
    Dec(Count);
    delta := pos - curMatch;
    if delta <= cyclicBufferPos then
      cyclicpos := (cyclicBufferPos - delta) shl 1
    else
      cyclicpos := (cyclicBufferPos - delta + cyclicBufferSize) shl 1;

    pby1 := bufferOffset + curMatch;
    len := min(len0, len1);
    if bufferBase[pby1 + len] = bufferBase[cur + len] then
    begin
      Inc(len);
      while (len <> lenLimit) do
      begin
        if (bufferBase[pby1 + len] <> bufferBase[cur + len]) then
          break;
        Inc(len);
      end;
      if maxLen < len then
      begin
        maxLen := len;
        distances[offset] := maxlen;
        Inc(offset);
        distances[offset] := delta - 1;
        Inc(offset);
        if (len = lenLimit) then
        begin
          son[ptr1] := son[cyclicPos];
          son[ptr0] := son[cyclicPos + 1];
          break;
        end;
      end;
    end;
    if (bufferBase[pby1 + len] and $FF) < (bufferBase[cur + len] and $FF) then
    begin
      son[ptr1] := curMatch;
      ptr1 := cyclicPos + 1;
      curMatch := son[ptr1];
      len1 := len;
    end
    else
    begin
      son[ptr0] := curMatch;
      ptr0 := cyclicPos;
      curMatch := son[ptr0];
      len0 := len;
    end;
  end;
  MovePos;
  Result := offset;
end;

procedure TLZBinTree.Skip(num: integer);
var
  lenLimit, matchminpos, cur, hashvalue, temp, hash2value, hash3value, curMatch: integer;
  ptr0, ptr1, len, len0, len1, Count, delta, cyclicpos, pby1: integer;
begin
  repeat
    if pos + matchMaxLen <= streamPos then
      lenLimit := matchMaxLen
    else
    begin
      lenLimit := streamPos - pos;
      if lenLimit < kMinMatchCheck then
      begin
        MovePos();
        Dec(num);
        continue;
      end;
    end;

    if pos > cyclicBufferSize then
      matchminpos := (pos - cyclicBufferSize)
    else
      matchminpos := 0;
    cur := bufferOffset + pos;

    if HASH_ARRAY then
    begin
      temp := CrcTable[bufferBase[cur] and $FF] xor (bufferBase[cur + 1] and $FF);
      hash2Value := temp and (kHash2Size - 1);
      hash[hash2Value] := pos;
      temp := temp xor ((bufferBase[cur + 2] and $FF) shl 8);
      hash3Value := temp and (kHash3Size - 1);
      hash[kHash3Offset + hash3Value] := pos;
      hashValue := (temp xor (CrcTable[bufferBase[cur + 3] and $FF] shl 5)) and hashMask;
    end
    else
      hashValue := ((bufferBase[cur] and $FF) xor
        ((bufferBase[cur + 1] and $FF) shl 8));

    curMatch := hash[kFixHashSize + hashValue];
    hash[kFixHashSize + hashValue] := pos;

    ptr0 := (cyclicBufferPos shl 1) + 1;
    ptr1 := (cyclicBufferPos shl 1);

    len0 := kNumHashDirectBytes;
    len1 := kNumHashDirectBytes;

    Count := cutValue;
    while True do
    begin
      if (curMatch <= matchMinPos) or (Count = 0) then
      begin
        son[ptr1] := kEmptyHashValue;
        son[ptr0] := son[ptr1];
        break;
      end
      else
        Dec(Count);

      delta := pos - curMatch;
      if (delta <= cyclicBufferPos) then
        cyclicpos := (cyclicBufferPos - delta) shl 1
      else
        cyclicpos := (cyclicBufferPos - delta + cyclicBufferSize) shl 1;

      pby1 := bufferOffset + curMatch;
      len := min(len0, len1);
      if bufferBase[pby1 + len] = bufferBase[cur + len] then
      begin
        Inc(len);
        while (len <> lenLimit) do
        begin
          if bufferBase[pby1 + len] <> bufferBase[cur + len] then
            break;
          Inc(len);
        end;
        if len = lenLimit then
        begin
          son[ptr1] := son[cyclicPos];
          son[ptr0] := son[cyclicPos + 1];
          break;
        end;
      end;
      if ((bufferBase[pby1 + len] and $FF) < (bufferBase[cur + len] and $FF)) then
      begin
        son[ptr1] := curMatch;
        ptr1 := cyclicPos + 1;
        curMatch := son[ptr1];
        len1 := len;
      end
      else
      begin
        son[ptr0] := curMatch;
        ptr0 := cyclicPos;
        curMatch := son[ptr0];
        len0 := len;
      end;
    end;
    MovePos;
    Dec(num);
  until num = 0;
end;

procedure TLZBinTree.NormalizeLinks(var items: array of integer;
  const numItems, subValue: integer);
var
  i, Value: integer;
begin
  for i := 0 to NumItems - 1 do
  begin
    Value := items[i];
    if Value <= subValue then
      Value := kEmptyHashValue
    else
      Value := Value - subValue;
    items[i] := Value;
  end;
end;

procedure TLZBinTree.Normalize;
var
  subvalue: integer;
begin
  subValue := pos - cyclicBufferSize;
  NormalizeLinks(son, cyclicBufferSize * 2, subValue);
  NormalizeLinks(hash, hashSizeSum, subValue);
  ReduceOffsets(subValue);
end;

procedure TLZBinTree.SetCutValue(const cutvalue: integer);
begin
  self.cutValue := cutValue;
end;

{ TLZMAEncoder }

constructor TLZMAEncoder.Create;
var
  kFastSlots, c, slotFast, j, k: integer;
begin
  kFastSlots := 22;
  c := 2;
  g_FastPos[0] := 0;
  g_FastPos[1] := 1;
  for slotFast := 2 to kFastSlots - 1 do
  begin
    k := (1 shl ((slotFast shr 1) - 1));
    for j := 0 to k - 1 do
    begin
      g_FastPos[c] := slotFast;
      Inc(c);
    end;
  end;
  _state := StateInit();
  _matchFinder := nil;
  _rangeEncoder := TRangeEncoder.Create;
  _posAlignEncoder := TBitTreeEncoder.Create(kNumAlignBits);
  _lenEncoder := TLZMALenPriceTableEncoder.Create;
  _repMatchLenEncoder := TLZMALenPriceTableEncoder.Create;
  _literalEncoder := TLZMALiteralEncoder.Create;
  _numFastBytes := kNumFastBytesDefault;
  _distTableSize := (kDefaultDictionaryLogSize * 2);
  _posStateBits := 2;
  _posStateMask := (4 - 1);
  _numLiteralPosStateBits := 0;
  _numLiteralContextBits := 3;

  _dictionarySize := (1 shl kDefaultDictionaryLogSize);
  _dictionarySizePrev := -1;
  _numFastBytesPrev := -1;
  _matchFinderType := EMatchFinderTypeBT4;
  _writeEndMark := False;

  _needReleaseMFStream := False;
end;

destructor TLZMAEncoder.Destroy;
var
  i: integer;
begin
  _rangeEncoder.Free;
  _posAlignEncoder.Free;
  _lenEncoder.Free;
  _repMatchLenEncoder.Free;
  _literalEncoder.Free;
  if _matchFinder <> nil then
    _matchFinder.Free;
  for i := 0 to kNumOpts - 1 do
    _optimum[i].Free;
  for i := 0 to kNumLenToPosStates - 1 do
    _posSlotEncoder[i].Free;
end;

procedure TLZMAEncoder._Create;
var
  bt: TLZBinTree;
  numHashBytes, i: integer;
begin
  if _matchFinder = nil then
  begin
    bt := TLZBinTree.Create;
    numHashBytes := 4;
    if _matchFinderType = EMatchFinderTypeBT2 then
      numHashBytes := 2;
    bt.SetType(numHashBytes);
    _matchFinder := bt;
  end;
  _literalEncoder._Create(_numLiteralPosStateBits, _numLiteralContextBits);

  if (_dictionarySize = _dictionarySizePrev) and (_numFastBytesPrev = _numFastBytes) then
    exit;
  _matchFinder._Create(_dictionarySize, kNumOpts, _numFastBytes, kMatchMaxLen + 1);
  _dictionarySizePrev := _dictionarySize;
  _numFastBytesPrev := _numFastBytes;

  for i := 0 to kNumOpts - 1 do
    _optimum[i] := TLZMAOptimal.Create;
  for i := 0 to kNumLenToPosStates - 1 do
    _posSlotEncoder[i] := TBitTreeEncoder.Create(kNumPosSlotBits);
end;

function TLZMAEncoder.GetPosSlot(const pos: integer): integer;
begin
  if (pos < (1 shl 11)) then
    Result := g_FastPos[pos]
  else if (pos < (1 shl 21)) then
    Result := (g_FastPos[pos shr 10] + 20)
  else
    Result := (g_FastPos[pos shr 20] + 40);
end;

function TLZMAEncoder.GetPosSlot2(const pos: integer): integer;
begin
  if (pos < (1 shl 17)) then
    Result := (g_FastPos[pos shr 6] + 12)
  else if (pos < (1 shl 27)) then
    Result := (g_FastPos[pos shr 16] + 32)
  else
    Result := (g_FastPos[pos shr 26] + 52);
end;

procedure TLZMAEncoder.BaseInit;
var
  i: integer;
begin
  _state := StateInit;
  _previousByte := 0;
  for i := 0 to kNumRepDistances - 1 do
    _repDistances[i] := 0;
end;

procedure TLZMAEncoder.SetWriteEndMarkerMode(const writeEndMarker: boolean);
begin
  _writeEndMark := writeEndMarker;
end;

procedure TLZMAEncoder.Init;
var
  i: integer;
begin
  BaseInit;
  _rangeEncoder.Init;

  InitBitModels(_isMatch);
  InitBitModels(_isRep0Long);
  InitBitModels(_isRep);
  InitBitModels(_isRepG0);
  InitBitModels(_isRepG1);
  InitBitModels(_isRepG2);
  InitBitModels(_posEncoders);


  _literalEncoder.Init();
  for i := 0 to kNumLenToPosStates - 1 do
    _posSlotEncoder[i].Init;

  _lenEncoder.Init(1 shl _posStateBits);
  _repMatchLenEncoder.Init(1 shl _posStateBits);

  _posAlignEncoder.Init;

  _longestMatchWasFound := False;
  _optimumEndIndex := 0;
  _optimumCurrentIndex := 0;
  _additionalOffset := 0;
end;

function TLZMAEncoder.ReadMatchDistances: integer;
var
  lenRes: integer;
begin
  lenRes := 0;
  _numDistancePairs := _matchFinder.GetMatches(_matchDistances);

  if _numDistancePairs > 0 then
  begin
    lenRes := _matchDistances[_numDistancePairs - 2];
    if lenRes = _numFastBytes then
      lenRes := lenRes + _matchFinder.GetMatchLen(lenRes - 1,
        _matchDistances[_numDistancePairs - 1], kMatchMaxLen - lenRes);
  end;
  Inc(_additionalOffset);
  Result := lenRes;
end;

procedure TLZMAEncoder.MovePos(const num: integer);
begin
  if num > 0 then
  begin
    _matchFinder.Skip(num);
    _additionalOffset := _additionalOffset + num;
  end;
end;

function TLZMAEncoder.GetRepLen1Price(const state, posState: integer): integer;
begin
  Result := RangeEncoder.GetPrice0(_isRepG0[state]) +
    RangeEncoder.GetPrice0(_isRep0Long[(state shl kNumPosStatesBitsMax) +
    posState]);
end;

function TLZMAEncoder.GetPureRepPrice(const repIndex, state, posState: integer): integer;
var
  price: integer;
begin
  if repIndex = 0 then
  begin
    price := RangeEncoder.GetPrice0(_isRepG0[state]);
    price := price + RangeEncoder.GetPrice1(
      _isRep0Long[(state shl kNumPosStatesBitsMax) + posState]);
  end
  else
  begin
    price := RangeEncoder.GetPrice1(_isRepG0[state]);
    if repIndex = 1 then
      price := price + RangeEncoder.GetPrice0(_isRepG1[state])
    else
    begin
      price := price + RangeEncoder.GetPrice1(_isRepG1[state]);
      price := price + RangeEncoder.GetPrice(_isRepG2[state], repIndex - 2);
    end;
  end;
  Result := price;
end;

function TLZMAEncoder.GetRepPrice(const repIndex, len, state, posState: integer): integer;
var
  price: integer;
begin
  price := _repMatchLenEncoder.GetPrice(len - kMatchMinLen, posState);
  Result := price + GetPureRepPrice(repIndex, state, posState);
end;

function TLZMAEncoder.GetPosLenPrice(const pos, len, posState: integer): integer;
var
  price, lenToPosState: integer;
begin
  lenToPosState := GetLenToPosState(len);
  if pos < kNumFullDistances then
    price := _distancesPrices[(lenToPosState * kNumFullDistances) + pos]
  else
    price := _posSlotPrices[(lenToPosState shl kNumPosSlotBits) +
      GetPosSlot2(pos)] + _alignPrices[pos and kAlignMask];
  Result := price + _lenEncoder.GetPrice(len - kMatchMinLen, posState);
end;

function TLZMAEncoder.Backward(cur: integer): integer;
var
  posMem, backMem, posPrev, backCur: integer;
begin
  _optimumEndIndex := cur;
  posMem := _optimum[cur].PosPrev;
  backMem := _optimum[cur].BackPrev;
  repeat
    if _optimum[cur].Prev1IsChar then
    begin
      _optimum[posMem].MakeAsChar;
      _optimum[posMem].PosPrev := posMem - 1;
      if _optimum[cur].Prev2 then
      begin
        _optimum[posMem - 1].Prev1IsChar := False;
        _optimum[posMem - 1].PosPrev := _optimum[cur].PosPrev2;
        _optimum[posMem - 1].BackPrev := _optimum[cur].BackPrev2;
      end;
    end;
    posPrev := posMem;
    backCur := backMem;

    backMem := _optimum[posPrev].BackPrev;
    posMem := _optimum[posPrev].PosPrev;

    _optimum[posPrev].BackPrev := backCur;
    _optimum[posPrev].PosPrev := cur;
    cur := posPrev;
  until not (cur > 0);
  backRes := _optimum[0].BackPrev;
  _optimumCurrentIndex := _optimum[0].PosPrev;
  Result := _optimumCurrentIndex;
end;

function TLZMAEncoder.GetOptimum(position: integer): integer;
var
  lenRes, lenMain, numDistancePairs, numAvailableBytes, repMaxIndex, i: integer;
  matchPrice, repMatchPrice, shortRepPrice, lenEnd, len, repLen, price: integer;
  curAndLenPrice, normalMatchPrice, Offs, distance, cur, newLen: integer;
  posPrev, state, pos, curPrice, curAnd1Price, numAvailableBytesFull: integer;
  lenTest2, t, state2, posStateNext, nextRepMatchPrice, offset: integer;
  startLen, repIndex, lenTest, lenTestTemp, curAndLenCharPrice: integer;
  nextMatchPrice, curBack: integer;
  optimum, opt, nextOptimum: TLZMAOptimal;
  currentByte, matchByte, posState: byte;
  nextIsChar: boolean;
begin
  if (_optimumEndIndex <> _optimumCurrentIndex) then
  begin
    lenRes := _optimum[_optimumCurrentIndex].PosPrev - _optimumCurrentIndex;
    backRes := _optimum[_optimumCurrentIndex].BackPrev;
    _optimumCurrentIndex := _optimum[_optimumCurrentIndex].PosPrev;
    Result := lenRes;
    exit;
  end;//if optimumendindex
  _optimumCurrentIndex := 0;
  _optimumEndIndex := 0;

  if not _longestMatchWasFound then
  begin
    lenMain := ReadMatchDistances();
  end
  else
  begin //if not longest
    lenMain := _longestMatchLength;
    _longestMatchWasFound := False;
  end;//if not longest else
  numDistancePairs := _numDistancePairs;

  numAvailableBytes := _matchFinder.GetNumAvailableBytes + 1;
  if numAvailableBytes < 2 then
  begin
    backRes := -1;
    Result := 1;
    exit;
  end;//if numavailable
{if numAvailableBytes > kMatchMaxLen then
   numAvailableBytes := kMatchMaxLen;}

  repMaxIndex := 0;
  for i := 0 to kNumRepDistances - 1 do
  begin
    reps[i] := _repDistances[i];
    repLens[i] := _matchFinder.GetMatchLen(0 - 1, reps[i], kMatchMaxLen);
    if repLens[i] > repLens[repMaxIndex] then
      repMaxIndex := i;
  end;//for i
  if repLens[repMaxIndex] >= _numFastBytes then
  begin
    backRes := repMaxIndex;
    lenRes := repLens[repMaxIndex];
    MovePos(lenRes - 1);
    Result := lenRes;
    exit;
  end;//if replens[]

  if lenMain >= _numFastBytes then
  begin
    backRes := _matchDistances[numDistancePairs - 1] + kNumRepDistances;
    MovePos(lenMain - 1);
    Result := lenMain;
    exit;
  end;//if lenMain

  currentByte := _matchFinder.GetIndexByte(0 - 1);
  matchByte := _matchFinder.GetIndexByte(0 - _repDistances[0] - 1 - 1);

  if (lenMain < 2) and (currentByte <> matchByte) and (repLens[repMaxIndex] < 2) then
  begin
    backRes := -1;
    Result := 1;
    exit;
  end;//if lenmain<2

  _optimum[0].State := _state;

  posState := (position and _posStateMask);

  _optimum[1].Price := RangeEncoder.GetPrice0(
    _isMatch[(_state shl kNumPosStatesBitsMax) + posState]) +
    _literalEncoder.GetSubCoder(position, _previousByte).GetPrice(not
    StateIsCharState(_state), matchByte, currentByte);
  _optimum[1].MakeAsChar();

  matchPrice := RangeEncoder.GetPrice1(
    _isMatch[(_state shl kNumPosStatesBitsMax) + posState]);
  repMatchPrice := matchPrice + RangeEncoder.GetPrice1(_isRep[_state]);

  if matchByte = currentByte then
  begin
    shortRepPrice := repMatchPrice + GetRepLen1Price(_state, posState);
    if shortRepPrice < _optimum[1].Price then
    begin
      _optimum[1].Price := shortRepPrice;
      _optimum[1].MakeAsShortRep;
    end;//if shortrepprice
  end;//if matchbyte

  if lenMain >= repLens[repMaxIndex] then
    lenEnd := lenMain
  else
    lenEnd := repLens[repMaxIndex];

  if lenEnd < 2 then
  begin
    backRes := _optimum[1].BackPrev;
    Result := 1;
    exit;
  end;//if lenend<2

  _optimum[1].PosPrev := 0;

  _optimum[0].Backs0 := reps[0];
  _optimum[0].Backs1 := reps[1];
  _optimum[0].Backs2 := reps[2];
  _optimum[0].Backs3 := reps[3];

  len := lenEnd;
  repeat
    _optimum[len].Price := kIfinityPrice;
    Dec(len);
  until not (len >= 2);

  for i := 0 to kNumRepDistances - 1 do
  begin
    repLen := repLens[i];
    if repLen < 2 then
      continue;
    price := repMatchPrice + GetPureRepPrice(i, _state, posState);
    repeat
      curAndLenPrice := price + _repMatchLenEncoder.GetPrice(repLen - 2, posState);
      optimum := _optimum[repLen];
      if curAndLenPrice < optimum.Price then
      begin
        optimum.Price := curAndLenPrice;
        optimum.PosPrev := 0;
        optimum.BackPrev := i;
        optimum.Prev1IsChar := False;
      end;//if curandlenprice
      Dec(replen);
    until not (repLen >= 2);
  end;//for i

  normalMatchPrice := matchPrice + RangeEncoder.GetPrice0(_isRep[_state]);

  if repLens[0] >= 2 then
    len := repLens[0] + 1
  else
    len := 2;

  if len <= lenMain then
  begin
    offs := 0;
    while len > _matchDistances[offs] do
      offs := offs + 2;
    while (True) do
    begin
      distance := _matchDistances[offs + 1];
      curAndLenPrice := normalMatchPrice + GetPosLenPrice(distance, len, posState);
      optimum := _optimum[len];
      if curAndLenPrice < optimum.Price then
      begin
        optimum.Price := curAndLenPrice;
        optimum.PosPrev := 0;
        optimum.BackPrev := distance + kNumRepDistances;
        optimum.Prev1IsChar := False;
      end;//if curlenandprice
      if len = _matchDistances[offs] then
      begin
        offs := offs + 2;
        if offs = numDistancePairs then
          break;
      end;//if len=_match
      Inc(len);
    end;//while (true)
  end;//if len<=lenmain

  cur := 0;

  while (True) do
  begin
    Inc(cur);
    if cur = lenEnd then
    begin
      Result := Backward(cur);
      exit;
    end;//if cur=lenEnd
    newLen := ReadMatchDistances;
    numDistancePairs := _numDistancePairs;
    if newLen >= _numFastBytes then
    begin
      _longestMatchLength := newLen;
      _longestMatchWasFound := True;
      Result := Backward(cur);
      exit;
    end;//if newlen=_numfast
    Inc(position);
    posPrev := _optimum[cur].PosPrev;
    if _optimum[cur].Prev1IsChar then
    begin
      Dec(posPrev);
      if _optimum[cur].Prev2 then
      begin
        state := _optimum[_optimum[cur].PosPrev2].State;
        if _optimum[cur].BackPrev2 < kNumRepDistances then
          state := StateUpdateRep(state)
        else
          state := StateUpdateMatch(state);
      end//if _optimum[cur].Prev2
      else
        state := _optimum[posPrev].State;
      state := StateUpdateChar(state);
    end//if _optimum[cur].Prev1IsChar
    else
      state := _optimum[posPrev].State;
    if posPrev = cur - 1 then
    begin
      if _optimum[cur].IsShortRep then
        state := StateUpdateShortRep(state)
      else
        state := StateUpdateChar(state);
    end //if posPrev = cur - 1
    else
    begin
      if _optimum[cur].Prev1IsChar and _optimum[cur].Prev2 then
      begin
        posPrev := _optimum[cur].PosPrev2;
        pos := _optimum[cur].BackPrev2;
        state := StateUpdateRep(state);
      end//if _optimum[cur].Prev1IsChar
      else
      begin
        pos := _optimum[cur].BackPrev;
        if pos < kNumRepDistances then
          state := StateUpdateRep(state)
        else
          state := StateUpdateMatch(state);
      end;//if else  _optimum[cur].Prev1IsChar
      opt := _optimum[posPrev];
      if pos < kNumRepDistances then
      begin
        if pos = 0 then
        begin
          reps[0] := opt.Backs0;
          reps[1] := opt.Backs1;
          reps[2] := opt.Backs2;
          reps[3] := opt.Backs3;
        end//if pos=0
        else if pos = 1 then
        begin
          reps[0] := opt.Backs1;
          reps[1] := opt.Backs0;
          reps[2] := opt.Backs2;
          reps[3] := opt.Backs3;
        end //if pos=1
        else if pos = 2 then
        begin
          reps[0] := opt.Backs2;
          reps[1] := opt.Backs0;
          reps[2] := opt.Backs1;
          reps[3] := opt.Backs3;
        end//if pos=2
        else
        begin
          reps[0] := opt.Backs3;
          reps[1] := opt.Backs0;
          reps[2] := opt.Backs1;
          reps[3] := opt.Backs2;
        end;//else if pos=
      end// if pos < kNumRepDistances
      else
      begin
        reps[0] := (pos - kNumRepDistances);
        reps[1] := opt.Backs0;
        reps[2] := opt.Backs1;
        reps[3] := opt.Backs2;
      end;//if else pos < kNumRepDistances
    end;//if else posPrev = cur - 1
    _optimum[cur].State := state;
    _optimum[cur].Backs0 := reps[0];
    _optimum[cur].Backs1 := reps[1];
    _optimum[cur].Backs2 := reps[2];
    _optimum[cur].Backs3 := reps[3];
    curPrice := _optimum[cur].Price;

    currentByte := _matchFinder.GetIndexByte(0 - 1);
    matchByte := _matchFinder.GetIndexByte(0 - reps[0] - 1 - 1);

    posState := (position and _posStateMask);

    curAnd1Price := curPrice + RangeEncoder.GetPrice0(
      _isMatch[(state shl kNumPosStatesBitsMax) + posState]) +
      _literalEncoder.GetSubCoder(position, _matchFinder.GetIndexByte(0 - 2)).GetPrice(not StateIsCharState(state), matchByte, currentByte);

    nextOptimum := _optimum[cur + 1];

    nextIsChar := False;
    if curAnd1Price < nextOptimum.Price then
    begin
      nextOptimum.Price := curAnd1Price;
      nextOptimum.PosPrev := cur;
      nextOptimum.MakeAsChar;
      nextIsChar := True;
    end;//if curand1price

    matchPrice := curPrice + RangeEncoder.GetPrice1(
      _isMatch[(state shl kNumPosStatesBitsMax) + posState]);
    repMatchPrice := matchPrice + RangeEncoder.GetPrice1(_isRep[state]);

    if (matchByte = currentByte) and
      (not ((nextOptimum.PosPrev < cur) and (nextOptimum.BackPrev = 0))) then
    begin
      shortRepPrice := repMatchPrice + GetRepLen1Price(state, posState);
      if shortRepPrice <= nextOptimum.Price then
      begin
        nextOptimum.Price := shortRepPrice;
        nextOptimum.PosPrev := cur;
        nextOptimum.MakeAsShortRep;
        nextIsChar := True;
      end;//if shortRepPrice <= nextOptimum.Price
    end;//if (matchByte = currentByte) and

    numAvailableBytesFull := _matchFinder.GetNumAvailableBytes + 1;
    numAvailableBytesFull := min(kNumOpts - 1 - cur, numAvailableBytesFull);
    numAvailableBytes := numAvailableBytesFull;

    if numAvailableBytes < 2 then
      continue;
    if numAvailableBytes > _numFastBytes then
      numAvailableBytes := _numFastBytes;
    if (not nextIsChar) and (matchByte <> currentByte) then
    begin
      // try Literal + rep0
      t := min(numAvailableBytesFull - 1, _numFastBytes);
      lenTest2 := _matchFinder.GetMatchLen(0, reps[0], t);
      if lenTest2 >= 2 then
      begin
        state2 := StateUpdateChar(state);

        posStateNext := (position + 1) and _posStateMask;
        nextRepMatchPrice :=
          curAnd1Price + RangeEncoder.GetPrice1(
          _isMatch[(state2 shl kNumPosStatesBitsMax) + posStateNext]) +
          RangeEncoder.GetPrice1(_isRep[state2]);
        begin
          offset := cur + 1 + lenTest2;
          while lenEnd < offset do
          begin
            Inc(lenEnd);
            _optimum[lenEnd].Price := kIfinityPrice;
          end;//while lenend
          curAndLenPrice :=
            nextRepMatchPrice + GetRepPrice(0, lenTest2, state2, posStateNext);
          optimum := _optimum[offset];
          if curAndLenPrice < optimum.Price then
          begin
            optimum.Price := curAndLenPrice;
            optimum.PosPrev := cur + 1;
            optimum.BackPrev := 0;
            optimum.Prev1IsChar := True;
            optimum.Prev2 := False;
          end;//if curandlenprice
        end;//none
      end;//if lentest
    end;//if not nextischar and ...

    startLen := 2; // speed optimization

    for repIndex := 0 to kNumRepDistances - 1 do
    begin
      lenTest := _matchFinder.GetMatchLen(0 - 1, reps[repIndex], numAvailableBytes);
      if lenTest < 2 then
        continue;
      lenTestTemp := lenTest;
      repeat
        while lenEnd < cur + lenTest do
        begin
          Inc(lenEnd);
          _optimum[lenEnd].Price := kIfinityPrice;
        end;//while lenEnd
        curAndLenPrice := repMatchPrice + GetRepPrice(repIndex,
          lenTest, state, posState);
        optimum := _optimum[cur + lenTest];
        if curAndLenPrice < optimum.Price then
        begin
          optimum.Price := curAndLenPrice;
          optimum.PosPrev := cur;
          optimum.BackPrev := repIndex;
          optimum.Prev1IsChar := False;
        end;//if curandlen
        Dec(lenTest);
      until not (lenTest >= 2);
      lenTest := lenTestTemp;

      if repIndex = 0 then
        startLen := lenTest + 1;

      // if (_maxMode)
      if lenTest < numAvailableBytesFull then
      begin
        t := min(numAvailableBytesFull - 1 - lenTest, _numFastBytes);
        lenTest2 := _matchFinder.GetMatchLen(lenTest, reps[repIndex], t);
        if lenTest2 >= 2 then
        begin
          state2 := StateUpdateRep(state);

          posStateNext := (position + lenTest) and _posStateMask;
          curAndLenCharPrice :=
            repMatchPrice + GetRepPrice(repIndex, lenTest, state, posState) +
            RangeEncoder.GetPrice0(
            _isMatch[(state2 shl kNumPosStatesBitsMax) + posStateNext]) +
            _literalEncoder.GetSubCoder(position + lenTest,
            _matchFinder.GetIndexByte(lenTest - 1 - 1)).GetPrice(True,
            _matchFinder.GetIndexByte(lenTest - 1 - (reps[repIndex] + 1)),
            _matchFinder.GetIndexByte(lenTest - 1));
          state2 := StateUpdateChar(state2);
          posStateNext := (position + lenTest + 1) and _posStateMask;
          nextMatchPrice :=
            curAndLenCharPrice + RangeEncoder.GetPrice1(
            _isMatch[(state2 shl kNumPosStatesBitsMax) + posStateNext]);
          nextRepMatchPrice :=
            nextMatchPrice + RangeEncoder.GetPrice1(_isRep[state2]);

          // for(; lenTest2 >= 2; lenTest2--)
          begin
            offset := lenTest + 1 + lenTest2;
            while lenEnd < cur + offset do
            begin
              Inc(lenEnd);
              _optimum[lenEnd].Price := kIfinityPrice;
            end;//while lenEnd
            curAndLenPrice :=
              nextRepMatchPrice + GetRepPrice(0, lenTest2, state2, posStateNext);
            optimum := _optimum[cur + offset];
            if curAndLenPrice < optimum.Price then
            begin
              optimum.Price := curAndLenPrice;
              optimum.PosPrev := cur + lenTest + 1;
              optimum.BackPrev := 0;
              optimum.Prev1IsChar := True;
              optimum.Prev2 := True;
              optimum.PosPrev2 := cur;
              optimum.BackPrev2 := repIndex;
            end;//if curAndLenPrice < optimum.Price
          end;//none
        end;//if lenTest2 >= 2
      end;//if lenTest < numAvailableBytesFull
    end;//for repIndex

    if newLen > numAvailableBytes then
    begin
      newLen := numAvailableBytes;
      numDistancePairs := 0;
      while newLen > _matchDistances[numDistancePairs] do
        numDistancePairs := numDistancePairs + 2;
      _matchDistances[numDistancePairs] := newLen;
      numDistancePairs := numDistancePairs + 2;
    end;//if newLen > numAvailableBytes
    if newLen >= startLen then
    begin
      normalMatchPrice := matchPrice + RangeEncoder.GetPrice0(_isRep[state]);
      while lenEnd < cur + newLen do
      begin
        Inc(lenEnd);
        _optimum[lenEnd].Price := kIfinityPrice;
      end;//while lenEnd

      offs := 0;
      while startLen > _matchDistances[offs] do
        offs := offs + 2;

      lenTest := startLen;
      while (True) do
      begin
        curBack := _matchDistances[offs + 1];
        curAndLenPrice :=
          normalMatchPrice + GetPosLenPrice(curBack, lenTest, posState);
        optimum := _optimum[cur + lenTest];
        if curAndLenPrice < optimum.Price then
        begin
          optimum.Price := curAndLenPrice;
          optimum.PosPrev := cur;
          optimum.BackPrev := curBack + kNumRepDistances;
          optimum.Prev1IsChar := False;
        end;//if curAndLenPrice < optimum.Price

        if lenTest = _matchDistances[offs] then
        begin
          if lenTest < numAvailableBytesFull then
          begin
            t := min(numAvailableBytesFull - 1 - lenTest, _numFastBytes);
            lenTest2 := _matchFinder.GetMatchLen(lenTest, curBack, t);
            if lenTest2 >= 2 then
            begin
              state2 := StateUpdateMatch(state);

              posStateNext := (position + lenTest) and _posStateMask;
              curAndLenCharPrice :=
                curAndLenPrice + RangeEncoder.GetPrice0(
                _isMatch[(state2 shl kNumPosStatesBitsMax) + posStateNext]) +
                _literalEncoder.GetSubCoder(position +
                lenTest, _matchFinder.GetIndexByte(lenTest - 1 - 1)).GetPrice(True,
                _matchFinder.GetIndexByte(lenTest - (curBack + 1) - 1),
                _matchFinder.GetIndexByte(lenTest - 1));
              state2 := StateUpdateChar(state2);
              posStateNext := (position + lenTest + 1) and _posStateMask;
              nextMatchPrice :=
                curAndLenCharPrice + RangeEncoder.GetPrice1(
                _isMatch[(state2 shl kNumPosStatesBitsMax) + posStateNext]);
              nextRepMatchPrice :=
                nextMatchPrice + RangeEncoder.GetPrice1(_isRep[state2]);

              offset := lenTest + 1 + lenTest2;
              while lenEnd < cur + offset do
              begin
                Inc(lenEnd);
                _optimum[lenEnd].Price := kIfinityPrice;
              end;//while lenEnd
              curAndLenPrice :=
                nextRepMatchPrice + GetRepPrice(0, lenTest2, state2, posStateNext);
              optimum := _optimum[cur + offset];
              if curAndLenPrice < optimum.Price then
              begin
                optimum.Price := curAndLenPrice;
                optimum.PosPrev := cur + lenTest + 1;
                optimum.BackPrev := 0;
                optimum.Prev1IsChar := True;
                optimum.Prev2 := True;
                optimum.PosPrev2 := cur;
                optimum.BackPrev2 := curBack + kNumRepDistances;
              end;//if curAndLenPrice < optimum.Price
            end;//if lenTest2 >= 2
          end;//lenTest < numAvailableBytesFull
          offs := offs + 2;
          if offs = numDistancePairs then
            break;
        end;//if lenTest = _matchDistances[offs]
        Inc(lenTest);
      end;//while(true)
    end;//if newLen >= startLen
  end;//while (true)
end;

function TLZMAEncoder.ChangePair(const smallDist, bigDist: integer): boolean;
var
  kDif: integer;
begin
  kDif := 7;
  Result := (smallDist < (1 shl (32 - kDif))) and (bigDist >= (smallDist shl kDif));
end;

procedure TLZMAEncoder.WriteEndMarker(const posState: integer);
var
  len, posSlot, lenToPosState, footerBits, posReduced: integer;
begin
  if not _writeEndMark then
    exit;

  _rangeEncoder.Encode(_isMatch, (_state shl kNumPosStatesBitsMax) +
    posState, 1);
  _rangeEncoder.Encode(_isRep, _state, 0);
  _state := StateUpdateMatch(_state);
  len := kMatchMinLen;
  _lenEncoder.Encode(_rangeEncoder, len - kMatchMinLen, posState);
  posSlot := (1 shl kNumPosSlotBits) - 1;
  lenToPosState := GetLenToPosState(len);
  _posSlotEncoder[lenToPosState].Encode(_rangeEncoder, posSlot);
  footerBits := 30;
  posReduced := (1 shl footerBits) - 1;
  _rangeEncoder.EncodeDirectBits(posReduced shr kNumAlignBits,
    footerBits - kNumAlignBits);
  _posAlignEncoder.ReverseEncode(_rangeEncoder, posReduced and kAlignMask);
end;

procedure TLZMAEncoder.Flush(const nowPos: integer);
begin
  ReleaseMFStream;
  WriteEndMarker(nowPos and _posStateMask);
  _rangeEncoder.FlushData();
  _rangeEncoder.FlushStream();
end;

procedure TLZMAEncoder.CodeOneBlock(var inSize, outSize: int64; var finished: boolean);
var
  progressPosValuePrev: int64;
  posState, len, pos, complexState, distance, i, posSlot, lenToPosState: integer;
  footerBits, baseVal, posReduced: integer;
  curByte, matchByte: byte;
  subcoder: TLZMAEncoder2;
begin
  inSize := 0;
  outSize := 0;
  finished := True;

  if _inStream <> nil then
  begin
    _matchFinder.SetStream(_inStream);
    _matchFinder.Init;
    _needReleaseMFStream := True;
    _inStream := nil;
  end;

  if _finished then
    exit;
  _finished := True;

  progressPosValuePrev := nowPos64;
  if nowPos64 = 0 then
  begin
    if _matchFinder.GetNumAvailableBytes = 0 then
    begin
      Flush(nowPos64);
      exit;
    end;

    ReadMatchDistances;
    posState := integer(nowPos64) and _posStateMask;
    _rangeEncoder.Encode(_isMatch, (_state shl kNumPosStatesBitsMax) +
      posState, 0);
    _state := StateUpdateChar(_state);
    curByte := _matchFinder.GetIndexByte(0 - _additionalOffset);
    _literalEncoder.GetSubCoder(integer(nowPos64), _previousByte).Encode(
      _rangeEncoder, curByte);
    _previousByte := curByte;
    Dec(_additionalOffset);
    Inc(nowPos64);
  end;
  if _matchFinder.GetNumAvailableBytes = 0 then
  begin
    Flush(integer(nowPos64));
    exit;
  end;
  while True do
  begin
    len := GetOptimum(integer(nowPos64));
    pos := backRes;
    posState := integer(nowPos64) and _posStateMask;
    complexState := (_state shl kNumPosStatesBitsMax) + posState;
    if (len = 1) and (pos = -1) then
    begin
      _rangeEncoder.Encode(_isMatch, complexState, 0);
      curByte := _matchFinder.GetIndexByte(0 - _additionalOffset);
      subCoder := _literalEncoder.GetSubCoder(integer(nowPos64), _previousByte);
      if not StateIsCharState(_state) then
      begin
        matchByte := _matchFinder.GetIndexByte(0 - _repDistances[0] -
          1 - _additionalOffset);
        subCoder.EncodeMatched(_rangeEncoder, matchByte, curByte);
      end
      else
        subCoder.Encode(_rangeEncoder, curByte);
      _previousByte := curByte;
      _state := StateUpdateChar(_state);
    end
    else
    begin
      _rangeEncoder.Encode(_isMatch, complexState, 1);
      if pos < kNumRepDistances then
      begin
        _rangeEncoder.Encode(_isRep, _state, 1);
        if pos = 0 then
        begin
          _rangeEncoder.Encode(_isRepG0, _state, 0);
          if len = 1 then
            _rangeEncoder.Encode(_isRep0Long, complexState, 0)
          else
            _rangeEncoder.Encode(_isRep0Long, complexState, 1);
        end
        else
        begin
          _rangeEncoder.Encode(_isRepG0, _state, 1);
          if pos = 1 then
            _rangeEncoder.Encode(_isRepG1, _state, 0)
          else
          begin
            _rangeEncoder.Encode(_isRepG1, _state, 1);
            _rangeEncoder.Encode(_isRepG2, _state, pos - 2);
          end;
        end;
        if len = 1 then
          _state := StateUpdateShortRep(_state)
        else
        begin
          _repMatchLenEncoder.Encode(_rangeEncoder,
            len - kMatchMinLen, posState);
          _state := StateUpdateRep(_state);
        end;
        distance := _repDistances[pos];
        if pos <> 0 then
        begin
          for i := pos downto 1 do
            _repDistances[i] := _repDistances[i - 1];
          _repDistances[0] := distance;
        end;
      end
      else
      begin
        _rangeEncoder.Encode(_isRep, _state, 0);
        _state := StateUpdateMatch(_state);
        _lenEncoder.Encode(_rangeEncoder, len -
          kMatchMinLen, posState);
        pos := pos - kNumRepDistances;
        posSlot := GetPosSlot(pos);
        lenToPosState := GetLenToPosState(len);
        _posSlotEncoder[lenToPosState].Encode(_rangeEncoder, posSlot);

        if posSlot >= kStartPosModelIndex then
        begin
          footerBits := integer((posSlot shr 1) - 1);
          baseVal := ((2 or (posSlot and 1)) shl footerBits);
          posReduced := pos - baseVal;

          if posSlot < kEndPosModelIndex then
            ReverseEncode(_posEncoders, baseVal - posSlot - 1, _rangeEncoder,
              footerBits, posReduced)
          else
          begin
            _rangeEncoder.EncodeDirectBits(posReduced shr
              kNumAlignBits, footerBits - kNumAlignBits);
            _posAlignEncoder.ReverseEncode(_rangeEncoder,
              posReduced and kAlignMask);
            Inc(_alignPriceCount);
          end;
        end;
        distance := pos;
        for i := kNumRepDistances - 1 downto 1 do
          _repDistances[i] := _repDistances[i - 1];
        _repDistances[0] := distance;
        Inc(_matchPriceCount);
      end;
      _previousByte := _matchFinder.GetIndexByte(len - 1 - _additionalOffset);
    end;
    _additionalOffset := _additionalOffset - len;
    nowPos64 := nowPos64 + len;
    if _additionalOffset = 0 then
    begin
      // if (!_fastMode)
      if _matchPriceCount >= (1 shl 7) then
        FillDistancesPrices;
      if _alignPriceCount >= kAlignTableSize then
        FillAlignPrices;
      inSize := nowPos64;
      outSize := _rangeEncoder.GetProcessedSizeAdd;
      if _matchFinder.GetNumAvailableBytes = 0 then
      begin
        Flush(integer(nowPos64));
        exit;
      end;

      if (nowPos64 - progressPosValuePrev >= (1 shl 12)) then
      begin
        _finished := False;
        finished := False;
        exit;
      end;
    end;
  end;
end;

procedure TLZMAEncoder.ReleaseMFStream;
begin
  if (_matchFinder <> nil) and _needReleaseMFStream then
  begin
    _matchFinder.ReleaseStream;
    _needReleaseMFStream := False;
  end;
end;

procedure TLZMAEncoder.SetOutStream(const outStream: TStream);
begin
  _rangeEncoder.SetStream(outStream);
end;

procedure TLZMAEncoder.ReleaseOutStream;
begin
  _rangeEncoder.ReleaseStream;
end;

procedure TLZMAEncoder.ReleaseStreams;
begin
  ReleaseMFStream;
  ReleaseOutStream;
end;

procedure TLZMAEncoder.SetStreams(const inStream, outStream: TStream;
  const inSize, outSize: int64);
begin
  _inStream := inStream;
  _finished := False;
  _Create();
  SetOutStream(outStream);
  Init();

  // if (!_fastMode)
  FillDistancesPrices;
  FillAlignPrices;

  _lenEncoder.SetTableSize(_numFastBytes + 1 - kMatchMinLen);
  _lenEncoder.UpdateTables(1 shl _posStateBits);
  _repMatchLenEncoder.SetTableSize(_numFastBytes + 1 - kMatchMinLen);
  _repMatchLenEncoder.UpdateTables(1 shl _posStateBits);

  nowPos64 := 0;
end;

procedure TLZMAEncoder.Code(const inStream, outStream: TStream;
  const inSize, outSize: int64);
var
  lpos: int64;
  progint: int64;
  inputsize: int64;
begin
  if insize = -1 then
    inputsize := instream.Size - instream.Position
  else
    inputsize := insize;
  progint := inputsize div CodeProgressInterval;
  lpos := progint;

  _needReleaseMFStream := False;
  DoProgress(LPAMax, inputsize);
  try
    SetStreams(inStream, outStream, inSize, outSize);
    while True do
    begin
      CodeOneBlock(processedInSize, processedOutSize, finished);
      if finished then
      begin
        DoProgress(LPAPos, inputsize);
        exit;
      end;
      if (processedInSize >= lpos) then
      begin
        DoProgress(LPAPos, processedInSize);
        lpos := lpos + progint;
      end;
    end;
  finally
    ReleaseStreams();
  end;
end;

procedure TLZMAEncoder.WriteCoderProperties(const outStream: TStream);
var
  i: integer;
begin
  properties[0] := (_posStateBits * 5 + _numLiteralPosStateBits) * 9 +
    _numLiteralContextBits;
  for i := 0 to 3 do
    properties[1 + i] := (_dictionarySize shr (8 * i));
  outStream.Write(properties, kPropSize);
end;

procedure TLZMAEncoder.FillDistancesPrices;
var
  i, posSlot, footerBits, baseVal, lenToPosState, st, st2: integer;
  encoder: TBitTreeEncoder;
begin
  for i := kStartPosModelIndex to kNumFullDistances - 1 do
  begin
    posSlot := GetPosSlot(i);
    footerBits := integer((posSlot shr 1) - 1);
    baseVal := (2 or (posSlot and 1)) shl footerBits;
    tempPrices[i] := ReverseGetPrice(_posEncoders, baseVal -
      posSlot - 1, footerBits, i - baseVal);
  end;

  for lenToPosState := 0 to kNumLenToPosStates - 1 do
  begin
    encoder := _posSlotEncoder[lenToPosState];

    st := (lenToPosState shl kNumPosSlotBits);
    for posSlot := 0 to _distTableSize - 1 do
      _posSlotPrices[st + posSlot] := encoder.GetPrice(posSlot);
    for posSlot := kEndPosModelIndex to _distTableSize - 1 do
      _posSlotPrices[st + posSlot] :=
        _posSlotPrices[st + posSlot] + ((((posSlot shr 1) - 1) - kNumAlignBits) shl
        kNumBitPriceShiftBits);

    st2 := lenToPosState * kNumFullDistances;
    for i := 0 to kStartPosModelIndex - 1 do
      _distancesPrices[st2 + i] := _posSlotPrices[st + i];
    for i := kStartPosModelIndex to kNumFullDistances - 1 do
      _distancesPrices[st2 + i] := _posSlotPrices[st + GetPosSlot(i)] + tempPrices[i];
  end;
  _matchPriceCount := 0;
end;

procedure TLZMAEncoder.FillAlignPrices;
var
  i: integer;
begin
  for i := 0 to kAlignTableSize - 1 do
    _alignPrices[i] := _posAlignEncoder.ReverseGetPrice(i);
  _alignPriceCount := 0;
end;

function TLZMAEncoder.SetAlgorithm(const algorithm: integer): boolean;
begin
{
    _fastMode = (algorithm == 0);
    _maxMode = (algorithm >= 2);
}
  Result := True;
end;

function TLZMAEncoder.SetDictionarySize(dictionarySize: integer): boolean;
var
  kDicLogSizeMaxCompress, dicLogSize: integer;
begin
  kDicLogSizeMaxCompress := 29;
  if (dictionarySize < (1 shl kDicLogSizeMin)) or
    (dictionarySize > (1 shl kDicLogSizeMaxCompress)) then
  begin
    Result := False;
    exit;
  end;
  _dictionarySize := dictionarySize;
  dicLogSize := 0;
  while dictionarySize > (1 shl dicLogSize) do
    Inc(dicLogSize);
  _distTableSize := dicLogSize * 2;
  Result := True;
end;

function TLZMAEncoder.SeNumFastBytes(const numFastBytes: integer): boolean;
begin
  if (numFastBytes < 5) or (numFastBytes > kMatchMaxLen) then
  begin
    Result := False;
    exit;
  end;
  _numFastBytes := numFastBytes;
  Result := True;
end;

function TLZMAEncoder.SetMatchFinder(const matchFinderIndex: integer): boolean;
var
  matchFinderIndexPrev: integer;
begin
  if (matchFinderIndex < 0) or (matchFinderIndex > 2) then
  begin
    Result := False;
    exit;
  end;
  matchFinderIndexPrev := _matchFinderType;
  _matchFinderType := matchFinderIndex;
  if (_matchFinder <> nil) and (matchFinderIndexPrev <> _matchFinderType) then
  begin
    _dictionarySizePrev := -1;
    _matchFinder := nil;
  end;
  Result := True;
end;

function TLZMAEncoder.SetLcLpPb(const lc, lp, pb: integer): boolean;
begin
  if (lp < 0) or (lp > kNumLitPosStatesBitsEncodingMax) or
    (lc < 0) or (lc > kNumLitContextBitsMax) or (pb < 0) or
    (pb > kNumPosStatesBitsEncodingMax) then
  begin
    Result := False;
    exit;
  end;
  _numLiteralPosStateBits := lp;
  _numLiteralContextBits := lc;
  _posStateBits := pb;
  _posStateMask := ((1) shl _posStateBits) - 1;
  Result := True;
end;

procedure TLZMAEncoder.SetEndMarkerMode(const endMarkerMode: boolean);
begin
  _writeEndMark := endMarkerMode;
end;

{ TLZMAEncoder2 }


procedure TLZMAEncoder2.Init;
begin
  InitBitModels(m_Encoders);
end;

procedure TLZMAEncoder2.Encode(const rangeEncoder: TRangeEncoder; const symbol: byte);
var
  context: integer;
  bit, i: integer;
begin
  context := 1;
  for i := 7 downto 0 do
  begin
    bit := ((symbol shr i) and 1);
    rangeEncoder.Encode(m_Encoders, context, bit);
    context := (context shl 1) or bit;
  end;
end;

procedure TLZMAEncoder2.EncodeMatched(const rangeEncoder: TRangeEncoder;
  const matchByte, symbol: byte);
var
  context, i, bit, state, matchbit: integer;
  same: boolean;
begin
  context := 1;
  same := True;
  for i := 7 downto 0 do
  begin
    bit := ((symbol shr i) and 1);
    state := context;
    if same then
    begin
      matchBit := ((matchByte shr i) and 1);
      state := state + ((1 + matchBit) shl 8);
      same := (matchBit = bit);
    end;
    rangeEncoder.Encode(m_Encoders, state, bit);
    context := (context shl 1) or bit;
  end;
end;

function TLZMAEncoder2.GetPrice(const matchMode: boolean;
  const matchByte, symbol: byte): integer;
var
  price, context, i, matchbit, bit: integer;
begin
  price := 0;
  context := 1;
  i := 7;
  if matchMode then
    while i >= 0 do
    begin
      matchBit := (matchByte shr i) and 1;
      bit := (symbol shr i) and 1;
      price := price + RangeEncoder.GetPrice(
        m_Encoders[((1 + matchBit) shl 8) + context], bit);
      context := (context shl 1) or bit;
      if (matchBit <> bit) then
      begin
        Dec(i);
        break;
      end;
      Dec(i);
    end;
  while i >= 0 do
  begin
    bit := (symbol shr i) and 1;
    price := price + RangeEncoder.GetPrice(m_Encoders[context], bit);
    context := (context shl 1) or bit;
    Dec(i);
  end;
  Result := price;
end;

{ TLZMALiteralEncoder }

procedure TLZMALiteralEncoder._Create(const numPosBits, numPrevBits: integer);
var
  numstates: integer;
  i: integer;
begin
  if (length(m_Coders) <> 0) and (m_NumPrevBits = numPrevBits) and
    (m_NumPosBits = numPosBits) then
    exit;
  m_NumPosBits := numPosBits;
  m_PosMask := (1 shl numPosBits) - 1;
  m_NumPrevBits := numPrevBits;
  numStates := 1 shl (m_NumPrevBits + m_NumPosBits);
  setlength(m_coders, numStates);
  for i := 0 to numStates - 1 do
    m_Coders[i] := TLZMAEncoder2.Create;
end;

destructor TLZMALiteralEncoder.Destroy;
var
  i: integer;
begin
  for i := low(m_Coders) to high(m_Coders) do
    if m_Coders[i] <> nil then
      m_Coders[i].Free;
  inherited;
end;

procedure TLZMALiteralEncoder.Init;
var
  numstates, i: integer;
begin
  numStates := 1 shl (m_NumPrevBits + m_NumPosBits);
  for i := 0 to numStates - 1 do
    m_Coders[i].Init;
end;

function TLZMALiteralEncoder.GetSubCoder(const pos: integer;
  const prevByte: byte): TLZMAEncoder2;
begin
  Result := m_Coders[((pos and m_PosMask) shl m_NumPrevBits) +
    ((prevByte and $FF) shr (8 - m_NumPrevBits))];
end;

{ TLZMALenEncoder }


constructor TLZMALenEncoder.Create;
var
  posState: integer;
begin
  _highCoder := TBitTreeEncoder.Create(kNumHighLenBits);
  for posState := 0 to kNumPosStatesEncodingMax - 1 do
  begin
    _lowCoder[posState] := TBitTreeEncoder.Create(kNumLowLenBits);
    _midCoder[posState] := TBitTreeEncoder.Create(kNumMidLenBits);
  end;
end;

destructor TLZMALenEncoder.Destroy;
var
  posState: integer;
begin
  _highCoder.Free;
  for posState := 0 to kNumPosStatesEncodingMax - 1 do
  begin
    _lowCoder[posState].Free;
    _midCoder[posState].Free;
  end;
  inherited;
end;

procedure TLZMALenEncoder.Init(const numPosStates: integer);
var
  posState: integer;
begin
  InitBitModels(_choice);

  for posState := 0 to numPosStates - 1 do
  begin
    _lowCoder[posState].Init;
    _midCoder[posState].Init;
  end;
  _highCoder.Init;
end;

procedure TLZMALenEncoder.Encode(const rangeEncoder: TRangeEncoder; symbol: integer;
  const posState: integer);
begin
  if (symbol < kNumLowLenSymbols) then
  begin
    rangeEncoder.Encode(_choice, 0, 0);
    _lowCoder[posState].Encode(rangeEncoder, symbol);
  end
  else
  begin
    symbol := symbol - kNumLowLenSymbols;
    rangeEncoder.Encode(_choice, 0, 1);
    if symbol < kNumMidLenSymbols then
    begin
      rangeEncoder.Encode(_choice, 1, 0);
      _midCoder[posState].Encode(rangeEncoder, symbol);
    end
    else
    begin
      rangeEncoder.Encode(_choice, 1, 1);
      _highCoder.Encode(rangeEncoder, symbol - kNumMidLenSymbols);
    end;
  end;
end;

procedure TLZMALenEncoder.SetPrices(const posState, numSymbols: integer;
  var prices: array of integer; const st: integer);
var
  a0, a1, b0, b1, i: integer;
begin
  a0 := RangeEncoder.GetPrice0(_choice[0]);
  a1 := RangeEncoder.GetPrice1(_choice[0]);
  b0 := a1 + RangeEncoder.GetPrice0(_choice[1]);
  b1 := a1 + RangeEncoder.GetPrice1(_choice[1]);
  i := 0;
  while i < kNumLowLenSymbols do
  begin
    if i >= numSymbols then
      exit;
    prices[st + i] := a0 + _lowCoder[posState].GetPrice(i);
    Inc(i);
  end;
  while i < kNumLowLenSymbols + kNumMidLenSymbols do
  begin
    if i >= numSymbols then
      exit;
    prices[st + i] := b0 + _midCoder[posState].GetPrice(i -
      kNumLowLenSymbols);
    Inc(i);
  end;
  while i < numSymbols do
  begin
    prices[st + i] := b1 + _highCoder.GetPrice(i - kNumLowLenSymbols -
      kNumMidLenSymbols);
    Inc(i);
  end;
end;

{ TLZMALenPriceTableEncoder }


procedure TLZMALenPriceTableEncoder.SetTableSize(const tableSize: integer);
begin
  _tableSize := tableSize;
end;

function TLZMALenPriceTableEncoder.GetPrice(const symbol, posState: integer): integer;
begin
  Result := _prices[posState * kNumLenSymbols + symbol];
end;

procedure TLZMALenPriceTableEncoder.UpdateTable(const posState: integer);
begin
  SetPrices(posState, _tableSize, _prices, posState * kNumLenSymbols);
  _counters[posState] := _tableSize;
end;

procedure TLZMALenPriceTableEncoder.UpdateTables(const numPosStates: integer);
var
  posState: integer;
begin
  for posState := 0 to numPosStates - 1 do
    UpdateTable(posState);
end;

procedure TLZMALenPriceTableEncoder.Encode(const rangeEncoder: TRangeEncoder;
  symbol: integer;
  const posState: integer);
begin
  inherited Encode(rangeEncoder, symbol, posState);
  Dec(_counters[posState]);
  if (_counters[posState] = 0) then
    UpdateTable(posState);
end;

{ TLZMAOptimal }


procedure TLZMAOptimal.MakeAsChar;
begin
  BackPrev := -1;
  Prev1IsChar := False;
end;

procedure TLZMAOptimal.MakeAsShortRep;
begin
  BackPrev := 0;
  Prev1IsChar := False;
end;

function TLZMAOptimal.IsShortRep: boolean;
begin
  Result := BackPrev = 0;
end;

procedure TLZMAEncoder.DoProgress(const Action: TLZMAProgressAction; const Value: integer);
begin
  if assigned(fOnProgress) then
    fOnProgress(action, Value);
end;

initialization
  InitModule;

finalization
  FinalModule;


end.

{-# LANGUAGE CPP #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
module Crisis.Util where

import Control.Monad (liftM)
import Data.Bits
import qualified Data.Vector.Generic.Mutable as VGM
import qualified Data.Vector.Generic as VG
import Data.Vector.Unboxed (Vector)
import qualified Data.Vector.Unboxed as V
import Data.Word

data Board = Board !Word64 !Word64
           deriving (Eq,Show)

instance Bits Board where
  Board l1 h1 .&. Board l2 h2 = Board (l1.&.l2) (h1.&.h2)
  Board l1 h1 .|. Board l2 h2 = Board (l1.|.l2) (h1.|.h2)
  Board l1 h1 `xor` Board l2 h2 = Board (l1`xor`l2) (h1`xor`h2)
  complement (Board l h) = Board (complement l) (complement h)
  popCount (Board l h) = popCount l + popCount h
  testBit (Board l h) n = if n < 64 then testBit l n else testBit h (n - 64)
  bit n = if n < 64 then Board (bit n) 0 else Board 0 (bit (n - 64))
#ifdef GHC78
  bitSizeMaybe _ = Just 128
#endif
  bitSize _ = 128
  isSigned _ = False
  shift _ _ = undefined
  rotate _ _ = undefined

newtype instance V.MVector s Board = MV_Board (V.MVector s (Word64, Word64))
newtype instance V.Vector    Board = V_Board (V.Vector (Word64, Word64))

instance VGM.MVector V.MVector Board where
  {-# INLINE basicLength #-}
  basicLength (MV_Board v) = VGM.basicLength v
  {-# INLINE basicUnsafeSlice #-}
  basicUnsafeSlice i j (MV_Board v) = MV_Board (VGM.basicUnsafeSlice i j v)
  {-# INLINE basicUnsafeNew #-}
  basicUnsafeNew i = liftM MV_Board (VGM.basicUnsafeNew i)
  {-# INLINE basicUnsafeRead #-}
  basicUnsafeRead (MV_Board v) i = liftM (uncurry Board) (VGM.basicUnsafeRead v i)
  {-# INLINE basicUnsafeWrite #-}
  basicUnsafeWrite (MV_Board v) i (Board x y) = VGM.basicUnsafeWrite v i (x, y)
  {-# INLINE basicOverlaps #-}
  basicOverlaps (MV_Board v) (MV_Board w) = VGM.basicOverlaps v w

instance VG.Vector V.Vector Board where
  {-# INLINE basicUnsafeFreeze #-}
  basicUnsafeFreeze (MV_Board v) = liftM V_Board (VG.basicUnsafeFreeze v)
  {-# INLINE basicUnsafeThaw #-}
  basicUnsafeThaw (V_Board v) = liftM MV_Board (VG.basicUnsafeThaw v)
  {-# INLINE basicLength #-}
  basicLength (V_Board v) = VG.basicLength v
  {-# INLINE basicUnsafeSlice #-}
  basicUnsafeSlice i j (V_Board v) = V_Board (VG.basicUnsafeSlice i j v)
  {-# INLINE basicUnsafeIndexM #-}
  basicUnsafeIndexM (V_Board v) i = liftM (uncurry Board) (VG.basicUnsafeIndexM v i)

instance V.Unbox Board

data Ship =
  Destroyer  |
  Cruiser    |
  Battleship |
  Hovercraft |
  Carrier
  deriving Enum

allPositionsOf :: Ship -> Vector Board
allPositionsOf Destroyer = V.fromList[Board 65 0,Board 3 0,Board 130 0,Board 6 0,Board 260 0,Board 12 0,Board 520 0,Board 24 0,Board 1040 0,Board 48 0,Board 2080 0,Board 4160 0,Board 192 0,Board 8320 0,Board 384 0,Board 16640 0,Board 768 0,Board 33280 0,Board 1536 0,Board 66560 0,Board 3072 0,Board 133120 0,Board 266240 0,Board 12288 0,Board 532480 0,Board 24576 0,Board 1064960 0,Board 49152 0,Board 2129920 0,Board 98304 0,Board 4259840 0,Board 196608 0,Board 8519680 0,Board 17039360 0,Board 786432 0,Board 34078720 0,Board 1572864 0,Board 68157440 0,Board 3145728 0,Board 136314880 0,Board 6291456 0,Board 272629760 0,Board 12582912 0,Board 545259520 0,Board 1090519040 0,Board 50331648 0,Board 2181038080 0,Board 100663296 0,Board 4362076160 0,Board 201326592 0,Board 8724152320 0,Board 402653184 0,Board 17448304640 0,Board 805306368 0,Board 34896609280 0,Board 69793218560 0,Board 3221225472 0,Board 139586437120 0,Board 6442450944 0,Board 279172874240 0,Board 12884901888 0,Board 558345748480 0,Board 25769803776 0,Board 1116691496960 0,Board 51539607552 0,Board 2233382993920 0,Board 281543696187392 0,Board 206158430208 0,Board 563087392374784 0,Board 412316860416 0,Board 1126174784749568 0,Board 824633720832 0,Board 2252349569499136 0,Board 1649267441664 0,Board 4504699138998272 0,Board 3298534883328 0,Board 9009398277996544 0,Board 6597069766656 0,Board 18018796555993088 0,Board 13194139533312 0,Board 36037593111986176 0,Board 26388279066624 0,Board 72075186223972352 0,Board 52776558133248 0,Board 144150372447944704 0,Board 105553116266496 0,Board 288300744895889408 0,Board 211106232532992 0,Board 576601489791778816 0,Board 1153202979583557632 0,Board 844424930131968 0,Board 2306405959167115264 0,Board 1688849860263936 0,Board 4612811918334230528 0,Board 3377699720527872 0,Board 9225623836668461056 0,Board 6755399441055744 0,Board 4503599627370496 1,Board 13510798882111488 0,Board 9007199254740992 2,Board 27021597764222976 0,Board 18014398509481984 4,Board 54043195528445952 0,Board 36028797018963968 8,Board 108086391056891904 0,Board 72057594037927936 16,Board 216172782113783808 0,Board 144115188075855872 32,Board 432345564227567616 0,Board 288230376151711744 64,Board 864691128455135232 0,Board 576460752303423488 128,Board 1152921504606846976 256,Board 3458764513820540928 0,Board 2305843009213693952 512,Board 6917529027641081856 0,Board 4611686018427387904 1024,Board 13835058055282163712 0,Board 9223372036854775808 2048,Board 9223372036854775808 1,Board 0 4097,Board 0 3,Board 0 8194,Board 0 6,Board 0 16388,Board 0 12,Board 0 32776,Board 0 24,Board 0 65552,Board 0 48,Board 0 131104,Board 0 96,Board 0 262208,Board 0 192,Board 0 524416,Board 0 1048832,Board 0 768,Board 0 2097664,Board 0 1536,Board 0 4195328,Board 0 3072,Board 0 8390656,Board 0 6144,Board 0 16781312,Board 0 12288,Board 0 33562624,Board 0 24576,Board 0 67125248,Board 0 49152,Board 0 134250496,Board 0 98304,Board 0 268500992,Board 0 196608,Board 0 537001984,Board 0 393216,Board 0 1074003968,Board 0 786432,Board 0 2148007936,Board 0 4296015872,Board 0 3145728,Board 0 8592031744,Board 0 6291456,Board 0 17184063488,Board 0 12582912,Board 0 34368126976,Board 0 25165824,Board 0 68736253952,Board 0 50331648,Board 0 137472507904,Board 0 100663296,Board 0 274945015808,Board 0 201326592,Board 0 549890031616,Board 0 402653184,Board 0 1099780063232,Board 0 805306368,Board 0 2199560126464,Board 0 1610612736,Board 0 4399120252928,Board 0 3221225472,Board 0 8798240505856,Board 0 12884901888,Board 0 25769803776,Board 0 51539607552,Board 0 103079215104,Board 0 206158430208,Board 0 412316860416,Board 0 824633720832,Board 0 1649267441664,Board 0 3298534883328,Board 0 6597069766656,Board 0 13194139533312]
allPositionsOf Cruiser = V.fromList [Board 4161 0,Board 7 0,Board 8322 0,Board 14 0,Board 16644 0,Board 28 0,Board 33288 0,Board 56 0,Board 66576 0,Board 133152 0,Board 266304 0,Board 448 0,Board 532608 0,Board 896 0,Board 1065216 0,Board 1792 0,Board 2130432 0,Board 3584 0,Board 4260864 0,Board 8521728 0,Board 17043456 0,Board 28672 0,Board 34086912 0,Board 57344 0,Board 68173824 0,Board 114688 0,Board 136347648 0,Board 229376 0,Board 272695296 0,Board 545390592 0,Board 1090781184 0,Board 1835008 0,Board 2181562368 0,Board 3670016 0,Board 4363124736 0,Board 7340032 0,Board 8726249472 0,Board 14680064 0,Board 17452498944 0,Board 34904997888 0,Board 69809995776 0,Board 117440512 0,Board 139619991552 0,Board 234881024 0,Board 279239983104 0,Board 469762048 0,Board 558479966208 0,Board 939524096 0,Board 1116959932416 0,Board 2233919864832 0,Board 281544769929216 0,Board 7516192768 0,Board 563089539858432 0,Board 15032385536 0,Board 1126179079716864 0,Board 30064771072 0,Board 2252358159433728 0,Board 60129542144 0,Board 4504716318867456 0,Board 9009432637734912 0,Board 1153203048303034368 0,Board 481036337152 0,Board 2306406096606068736 0,Board 962072674304 0,Board 4612812193212137472 0,Board 1924145348608 0,Board 9225624386424274944 0,Board 3848290697216 0,Board 4504699138998272 1,Board 7696581394432 0,Board 9009398277996544 2,Board 15393162788864 0,Board 18018796555993088 4,Board 30786325577728 0,Board 36037593111986176 8,Board 61572651155456 0,Board 72075186223972352 16,Board 123145302310912 0,Board 144150372447944704 32,Board 246290604621824 0,Board 288300744895889408 64,Board 576601489791778816 128,Board 1153202979583557632 256,Board 1970324836974592 0,Board 2306405959167115264 512,Board 3940649673949184 0,Board 4612811918334230528 1024,Board 7881299347898368 0,Board 9225623836668461056 2048,Board 15762598695796736 0,Board 4503599627370496 4097,Board 31525197391593472 0,Board 9007199254740992 8194,Board 63050394783186944 0,Board 18014398509481984 16388,Board 126100789566373888 0,Board 36028797018963968 32776,Board 252201579132747776 0,Board 72057594037927936 65552,Board 504403158265495552 0,Board 144115188075855872 131104,Board 1008806316530991104 0,Board 288230376151711744 262208,Board 576460752303423488 524416,Board 1152921504606846976 1048832,Board 8070450532247928832 0,Board 2305843009213693952 2097664,Board 16140901064495857664 0,Board 4611686018427387904 4195328,Board 13835058055282163712 1,Board 9223372036854775808 8390656,Board 9223372036854775808 3,Board 0 16781313,Board 0 7,Board 0 33562626,Board 0 14,Board 0 67125252,Board 0 28,Board 0 134250504,Board 0 56,Board 0 268501008,Board 0 112,Board 0 537002016,Board 0 224,Board 0 1074004032,Board 0 2148008064,Board 0 4296016128,Board 0 1792,Board 0 8592032256,Board 0 3584,Board 0 17184064512,Board 0 7168,Board 0 34368129024,Board 0 14336,Board 0 68736258048,Board 0 28672,Board 0 137472516096,Board 0 57344,Board 0 274945032192,Board 0 114688,Board 0 549890064384,Board 0 229376,Board 0 1099780128768,Board 0 458752,Board 0 2199560257536,Board 0 917504,Board 0 4399120515072,Board 0 8798241030144,Board 0 7340032,Board 0 14680064,Board 0 29360128,Board 0 58720256,Board 0 117440512,Board 0 234881024,Board 0 469762048,Board 0 939524096,Board 0 1879048192,Board 0 3758096384,Board 0 30064771072,Board 0 60129542144,Board 0 120259084288,Board 0 240518168576,Board 0 481036337152,Board 0 962072674304,Board 0 1924145348608,Board 0 3848290697216,Board 0 7696581394432,Board 0 15393162788864]
allPositionsOf Battleship = V.fromList [Board 266305 0,Board 15 0,Board 532610 0,Board 30 0,Board 1065220 0,Board 60 0,Board 2130440 0,Board 4260880 0,Board 8521760 0,Board 17043520 0,Board 960 0,Board 34087040 0,Board 1920 0,Board 68174080 0,Board 3840 0,Board 136348160 0,Board 272696320 0,Board 545392640 0,Board 1090785280 0,Board 61440 0,Board 2181570560 0,Board 122880 0,Board 4363141120 0,Board 245760 0,Board 8726282240 0,Board 17452564480 0,Board 34905128960 0,Board 69810257920 0,Board 3932160 0,Board 139620515840 0,Board 7864320 0,Board 279241031680 0,Board 15728640 0,Board 558482063360 0,Board 1116964126720 0,Board 2233928253440 0,Board 281544786706432 0,Board 251658240 0,Board 563089573412864 0,Board 503316480 0,Board 1126179146825728 0,Board 1006632960 0,Board 2252358293651456 0,Board 4504716587302912 0,Board 9009433174605824 0,Board 1153203049376776192 0,Board 16106127360 0,Board 2306406098753552384 0,Board 32212254720 0,Board 4612812197507104768 0,Board 64424509440 0,Board 9225624395014209536 0,Board 4504716318867456 1,Board 9009432637734912 2,Board 1153203048303034368 256,Board 1030792151040 0,Board 2306406096606068736 512,Board 2061584302080 0,Board 4612812193212137472 1024,Board 4123168604160 0,Board 9225624386424274944 2048,Board 8246337208320 0,Board 4504699138998272 4097,Board 16492674416640 0,Board 9009398277996544 8194,Board 32985348833280 0,Board 18018796555993088 16388,Board 65970697666560 0,Board 36037593111986176 32776,Board 131941395333120 0,Board 72075186223972352 65552,Board 263882790666240 0,Board 144150372447944704 131104,Board 288300744895889408 262208,Board 576601489791778816 524416,Board 1153202979583557632 1048832,Board 4222124650659840 0,Board 2306405959167115264 2097664,Board 8444249301319680 0,Board 4612811918334230528 4195328,Board 16888498602639360 0,Board 9225623836668461056 8390656,Board 33776997205278720 0,Board 4503599627370496 16781313,Board 67553994410557440 0,Board 9007199254740992 33562626,Board 135107988821114880 0,Board 18014398509481984 67125252,Board 270215977642229760 0,Board 36028797018963968 134250504,Board 540431955284459520 0,Board 72057594037927936 268501008,Board 1080863910568919040 0,Board 144115188075855872 537002016,Board 288230376151711744 1074004032,Board 576460752303423488 2148008064,Board 1152921504606846976 4296016128,Board 17293822569102704640 0,Board 2305843009213693952 8592032256,Board 16140901064495857664 1,Board 4611686018427387904 17184064512,Board 13835058055282163712 3,Board 9223372036854775808 34368129024,Board 9223372036854775808 7,Board 0 68736258049,Board 0 15,Board 0 137472516098,Board 0 30,Board 0 274945032196,Board 0 60,Board 0 549890064392,Board 0 120,Board 0 1099780128784,Board 0 240,Board 0 2199560257568,Board 0 4399120515136,Board 0 8798241030272,Board 0 3840,Board 0 7680,Board 0 15360,Board 0 30720,Board 0 61440,Board 0 122880,Board 0 245760,Board 0 491520,Board 0 983040,Board 0 15728640,Board 0 31457280,Board 0 62914560,Board 0 125829120,Board 0 251658240,Board 0 503316480,Board 0 1006632960,Board 0 2013265920,Board 0 4026531840,Board 0 64424509440,Board 0 128849018880,Board 0 257698037760,Board 0 515396075520,Board 0 1030792151040,Board 0 2061584302080,Board 0 4123168604160,Board 0 8246337208320,Board 0 16492674416640]
allPositionsOf Hovercraft = V.fromList [Board 8645 0,Board 17290 0,Board 34580 0,Board 24774 0,Board 69160 0,Board 49548 0,Board 99096 0,Board 198192 0,Board 553280 0,Board 1106560 0,Board 2213120 0,Board 1585536 0,Board 4426240 0,Board 3171072 0,Board 6342144 0,Board 12684288 0,Board 35409920 0,Board 12675 0,Board 70819840 0,Board 25350 0,Board 141639680 0,Board 50700 0,Board 20930 0,Board 101474304 0,Board 283279360 0,Board 101400 0,Board 41860 0,Board 202948608 0,Board 83720 0,Board 405897216 0,Board 167440 0,Board 811794432 0,Board 2266234880 0,Board 811200 0,Board 4532469760 0,Board 1622400 0,Board 9064939520 0,Board 3244800 0,Board 1339520 0,Board 6494355456 0,Board 18129879040 0,Board 6489600 0,Board 2679040 0,Board 12988710912 0,Board 5358080 0,Board 25977421824 0,Board 10716160 0,Board 51954843648 0,Board 145039032320 0,Board 51916800 0,Board 290078064640 0,Board 103833600 0,Board 580156129280 0,Board 207667200 0,Board 85729280 0,Board 415638749184 0,Board 1160312258560 0,Board 415334400 0,Board 171458560 0,Board 831277498368 0,Board 342917120 0,Board 1662554996736 0,Board 685834240 0,Board 3325109993472 0,Board 563436358467584 0,Board 3322675200 0,Board 1126872716935168 0,Board 6645350400 0,Board 2253745433870336 0,Board 13290700800 0,Board 5486673920 0,Board 1689062461145088 0,Board 4507490867740672 0,Board 26581401600 0,Board 10973347840 0,Board 3378124922290176 0,Board 21946695680 0,Board 6756249844580352 0,Board 43893391360 0,Board 13512499689160704 0,Board 2307813677648052224 0,Board 212651212800 0,Board 4615627355296104448 0,Board 425302425600 0,Board 9231254710592208896 0,Board 850604851200 0,Board 351147130880 0,Board 6918373864888074240 0,Board 15765347474866176 1,Board 1701209702400 0,Board 702294261760 0,Board 13836747729776148480 0,Board 31530694949732352 2,Board 1404588523520 0,Board 9226751385842745344 1,Board 63061389899464704 4,Board 2809177047040 0,Board 6758697975939072 3,Board 126122779798929408 8,Board 13517395951878144 6,Board 252245559597858816 16,Board 27034791903756288 12,Board 504491119195717632 32,Board 54069583807512576 24,Board 1008982238391435264 64,Board 108139167615025152 48,Board 216278335230050304 96,Board 432556670460100608 192,Board 8071857907131482112 512,Board 844840468217856 0,Board 16143715814262964224 1024,Board 1689680936435712 0,Board 13840687554816376832 2049,Board 3379361872871424 0,Board 1407858067374080 0,Board 3460453363680804864 1536,Board 9234631035923202048 4099,Board 6758723745742848 0,Board 2815716134748160 0,Board 6920906727361609728 3072,Board 22517998136852480 8199,Board 13517447491485696 0,Board 5631432269496320 0,Board 13841813454723219456 6144,Board 45035996273704960 16398,Board 11262864538992640 0,Board 9236882835736887296 12289,Board 90071992547409920 32796,Board 22525729077985280 0,Board 27021597764222976 24579,Board 180143985094819840 65592,Board 54043195528445952 49158,Board 360287970189639680 131184,Board 108086391056891904 98316,Board 720575940379279360 262368,Board 216172782113783808 196632,Board 432345564227567616 393264,Board 864691128455135232 786528,Board 5764607523034234880 2098944,Board 3460453569839235072 0,Board 11529215046068469760 4197888,Board 6920907139678470144 0,Board 4611686018427387904 8395777,Board 13841814279356940288 0,Board 5766577985310162944 0,Board 6917529027641081856 6292224,Board 9223372036854775808 16791554,Board 9236884485004328960 1,Board 11533155970620325888 0,Board 13835058055282163712 12584448,Board 0 33583109,Board 27024896299106304 3,Board 4619567867531100160 1,Board 9223372036854775808 25168897,Board 0 67166218,Board 54049792598212608 6,Board 9239135735062200320 2,Board 0 50337795,Board 0 134332436,Board 108099585196425216 12,Board 31527396414849024 5,Board 0 100675590,Board 0 268664872,Board 216199170392850432 24,Board 63054792829698048 10,Board 0 201351180,Board 0 537329744,Board 432398340785700864 48,Board 126109585659396096 20,Board 0 402702360,Board 0 1074659488,Board 864796681571401728 96,Board 252219171318792192 40,Board 0 805404720,Board 504438342637584384 80,Board 0 1610809440,Board 1008876685275168768 160,Board 0 3221618880,Board 0 8597275904,Board 6918373452571213824 768,Board 0 17194551808,Board 13836746905142427648 1536,Board 0 34389103616,Board 9226749736575303680 3073,Board 8071013482201350144 1280,Board 0 25772951040,Board 0 68778207232,Board 6755399441055744 6147,Board 16142026964402700288 2560,Board 0 51545902080,Board 0 137556414464,Board 13510798882111488 12294,Board 13837309855095848960 5121,Board 0 103091804160,Board 0 275112828928,Board 27021597764222976 24588,Board 9227875636482146304 10243,Board 0 206183608320,Board 0 550225657856,Board 54043195528445952 49176,Board 9007199254740992 20487,Board 0 412367216640,Board 0 1100451315712,Board 108086391056891904 98352,Board 18014398509481984 40974,Board 0 824734433280,Board 0 2200902631424,Board 216172782113783808 196704,Board 36028797018963968 81948,Board 0 1649468866560,Board 0 4401805262848,Board 432345564227567616 393408,Board 72057594037927936 163896,Board 0 3298937733120,Board 144115188075855872 327792,Board 0 6597875466240,Board 288230376151711744 655584,Board 0 13195750932480,Board 3458764513820540928 3147264,Board 6917529027641081856 6294528,Board 13835058055282163712 12589056,Board 2305843009213693952 5244672,Board 9223372036854775808 25178113,Board 4611686018427387904 10489344,Board 0 50356227,Board 9223372036854775808 20978688,Board 0 100712454,Board 0 41957377,Board 0 201424908,Board 0 83914754,Board 0 402849816,Board 0 167829508,Board 0 805699632,Board 0 335659016,Board 0 1611399264,Board 0 671318032,Board 0 1342636064,Board 0 2685272128,Board 0 12891194112,Board 0 25782388224,Board 0 51564776448,Board 0 21482177024,Board 0 103129552896,Board 0 42964354048,Board 0 206259105792,Board 0 85928708096,Board 0 412518211584,Board 0 171857416192,Board 0 825036423168,Board 0 343714832384,Board 0 1650072846336,Board 0 687429664768,Board 0 3300145692672,Board 0 1374859329536,Board 0 6600291385344,Board 0 2749718659072,Board 0 5499437318144,Board 0 10998874636288]
allPositionsOf Carrier = V.fromList [Board 5057 0,Board 10114 0,Board 20228 0,Board 532615 0,Board 1065230 0,Board 2130460 0,Board 4260920 0,Board 323648 0,Board 647296 0,Board 1294592 0,Board 34087360 0,Board 68174720 0,Board 136349440 0,Board 272698880 0,Board 20713472 0,Board 41426944 0,Board 82853888 0,Board 2181591040 0,Board 33736 0,Board 4363182080 0,Board 67472 0,Board 8726364160 0,Board 134944 0,Board 17452728320 0,Board 1325662208 0,Board 1843330 0,Board 2651324416 0,Board 3686660 0,Board 5302648832 0,Board 7373320 0,Board 139621826560 0,Board 14746640 0,Board 2159104 0,Board 279243653120 0,Board 4318208 0,Board 558487306240 0,Board 8636416 0,Board 1116974612480 0,Board 84842381312 0,Board 117973120 0,Board 169684762624 0,Board 235946240 0,Board 339369525248 0,Board 471892480 0,Board 563089657298944 0,Board 943784960 0,Board 138182656 0,Board 1126179314597888 0,Board 276365312 0,Board 2252358629195776 0,Board 552730624 0,Board 4504717258391552 0,Board 282506842603520 0,Board 7550279680 0,Board 565013685207040 0,Board 15100559360 0,Board 1130027370414080 0,Board 30201118720 0,Board 2306406104122261504 0,Board 2260054740828160 0,Board 60402237440 0,Board 8843689984 0,Board 4612812208244523008 0,Board 4520109481656320 0,Board 17687379968 0,Board 9225624416489046016 0,Board 9040218963312640 0,Board 35374759936 0,Board 4504759268540416 1,Board 1157143697976983552 0,Board 483217899520 0,Board 2314287395953967104 0,Board 966435799040 0,Board 4628574791907934208 0,Board 1932871598080 0,Board 2306406440203452416 512,Board 9257149583815868416 0,Board 3865743196160 0,Board 565996158976 0,Board 4612812880406904832 1024,Board 67555093922185216 1,Board 7731486392320 0,Board 1131992317952 0,Board 9225625760813809664 2048,Board 135110187844370432 2,Board 2263984635904 0,Board 4507447918067712 4097,Board 270220375688740864 4,Board 9014895836135424 8194,Board 540440751377481728 8,Board 18029791672270848 16388,Board 1080881502754963456 16,Board 36059583344541696 32776,Board 72119166689083392 65552,Board 144238333378166784 131104,Board 288476666756333568 262208,Board 17294104044079415296 256,Board 1970464456966144 0,Board 16141464014449278976 513,Board 3940928913932288 0,Board 13836183955189006336 1027,Board 7881857827864576 0,Board 2307813334050668544 2097664,Board 9225623836668461056 2055,Board 15763715655729152 0,Board 2252839195770880 0,Board 4615626668101337088 4195328,Board 4503599627370496 4111,Board 31527431311458304 0,Board 4505678391541760 0,Board 9231253336202674176 8390656,Board 9007199254740992 8222,Board 9011356783083520 0,Board 15762598695796736 16781313,Board 18014398509481984 16444,Board 31525197391593472 33562626,Board 36028797018963968 32888,Board 63050394783186944 67125252,Board 72057594037927936 65776,Board 126100789566373888 134250504,Board 252201579132747776 268501008,Board 504403158265495552 537002016,Board 1008806316530991104 1074004032,Board 1152921504606846976 1052416,Board 8071013621787787264 0,Board 2305843009213693952 2104832,Board 16142027243575574528 0,Board 4611686018427387904 4209664,Board 13837310413441597440 1,Board 8070450532247928832 8592032256,Board 9223372036854775808 8419328,Board 9227876753173643264 3,Board 9227594711261249536 0,Board 16140901064495857664 17184064512,Board 0 16838657,Board 9009432637734912 7,Board 8445348812947456 1,Board 13835058055282163712 34368129025,Board 0 33677314,Board 16890697625894912 2,Board 9223372036854775808 68736258051,Board 0 67354628,Board 33781395251789824 4,Board 0 137472516103,Board 0 134709256,Board 67562790503579648 8,Board 0 274945032206,Board 0 269418512,Board 135125581007159296 16,Board 0 549890064412,Board 270251162014318592 32,Board 0 1099780128824,Board 540502324028637184 64,Board 0 2199560257648,Board 1081004648057274368 128,Board 0 4399120515296,Board 0 4310696192,Board 2306406096606068736 1792,Board 0 8621392384,Board 4612812193212137472 3584,Board 0 17242784768,Board 9225624386424274944 7168,Board 0 34485569536,Board 4504699138998272 14337,Board 17296074368916389888 2048,Board 0 68971139072,Board 9009398277996544 28674,Board 16145404664123228160 4097,Board 0 137942278144,Board 18018796555993088 57348,Board 13844065254536904704 8195,Board 0 275884556288,Board 36037593111986176 114696,Board 9241386435364257792 16391,Board 0 551769112576,Board 72075186223972352 229392,Board 36028797018963968 32783,Board 0 1103538225152,Board 144150372447944704 458784,Board 72057594037927936 65566,Board 288300744895889408 917568,Board 144115188075855872 131132,Board 288230376151711744 262264,Board 576460752303423488 524528,Board 2306405959167115264 7340544,Board 4612811918334230528 14681088,Board 9225623836668461056 29362176,Board 4503599627370496 58724353,Board 9223372036854775808 8392448,Board 9007199254740992 117448706,Board 0 16784897,Board 18014398509481984 234897412,Board 0 33569794,Board 36028797018963968 469794824,Board 0 67139588,Board 72057594037927936 939589648,Board 0 134279176,Board 144115188075855872 1879179296,Board 0 268558352,Board 288230376151711744 3758358592,Board 0 537116704,Board 0 1074233408,Board 0 2148466816,Board 2305843009213693952 30066868736,Board 4611686018427387904 60133737472,Board 9223372036854775808 120267474944,Board 0 240534949889,Board 0 34375469056,Board 0 481069899778,Board 0 68750938112,Board 0 962139799556,Board 0 137501876224,Board 0 1924279599112,Board 0 275003752448,Board 0 3848559198224,Board 0 550007504896,Board 0 7697118396448,Board 0 1100015009792,Board 0 15394236792896,Board 0 2200030019584,Board 0 4400060039168,Board 0 8800120078336]
